#!/usr/bin/env Rscript
# GRU comparator arm: GRU-seq (sequences only) and GRU-full (sequences + clinical),
# 3 seeds each, evaluated on the same temporal test set as XGBoost (03_model.R).
# Early stopping on validation AUROC (patience 8, max 60 epochs).
# Usage: Rscript 13_gru_train.R [bench|full]
# Outputs: /workspace/gru/out/{metrics_gru.csv, preds_gru.csv, *.pt}

suppressPackageStartupMessages({
  library(torch)
  library(data.table)
  library(pROC)
  library(PRROC)
})

mode <- if (length(commandArgs(trailingOnly = TRUE)) > 0)
  commandArgs(trailingOnly = TRUE)[1] else "full"

g <- readRDS("/workspace/gru/gru_data.rds")
OUT <- "/workspace/gru/out"; dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

X <- g$X; lens <- g$lens; S <- g$S; y <- g$y
stopifnot(!any(is.na(X)), !any(is.na(S)))
sub_idx <- g$sub_idx; va_idx <- g$va_idx
te_idx <- which(g$split == "test")

Xt <- torch_tensor(X, dtype = torch_float())
St <- torch_tensor(S, dtype = torch_float())
yt <- torch_tensor(y, dtype = torch_float())
len_t <- torch_tensor(lens, dtype = torch_long())
rm(X, S); gc()

HID <- 64L; DRO <- 0.3; BS <- 128L; LR <- 1e-3; WD <- 1e-5
MAX_EP <- if (mode == "bench") 2L else 60L
PATIENCE <- 8L

GRUSeq <- nn_module(
  "GRUSeq",
  initialize = function(hidden = HID, dropout = DRO) {
    self$gru <- nn_gru(input_size = 10, hidden_size = hidden, batch_first = TRUE)
    self$drop <- nn_dropout(dropout)
    self$head <- nn_linear(hidden, 1)
  },
  forward = function(x, len) {
    h <- self$gru(x)[[1]]
    last <- h$gather(2, len$view(c(-1, 1, 1))$expand(c(-1, 1, h$size(3))))$squeeze(2)
    self$head(self$drop(last))$squeeze(2)
  }
)

GRUFull <- nn_module(
  "GRUFull",
  initialize = function(hidden = HID, sdim, dropout = DRO) {
    self$gru <- nn_gru(input_size = 10, hidden_size = hidden, batch_first = TRUE)
    self$mlp <- nn_sequential(
      nn_linear(hidden + sdim, 32), nn_relu(), nn_dropout(dropout),
      nn_linear(32, 1))
  },
  forward = function(x, len, s) {
    h <- self$gru(x)[[1]]
    last <- h$gather(2, len$view(c(-1, 1, 1))$expand(c(-1, 1, h$size(3))))$squeeze(2)
    z <- torch_cat(list(last, s), dim = 2)
    self$mlp(z)$squeeze(2)
  }
)

make_batches <- function(idx, lens, bs = BS) {
  o <- idx[order(lens[idx])]
  chunks <- split(o, ceiling(seq_along(o) / bs))
  sample(chunks)
}

predict_ids <- function(model, variant, ids, chunk = 512L) {
  model$eval()
  out <- numeric(length(ids))
  with_no_grad({
    for (s in split(ids, ceiling(seq_along(ids) / chunk))) {
      logit <- if (variant == "seq") model(Xt[s, , ], len_t[s])
      else model(Xt[s, , ], len_t[s], St[s, ])
      out[match(s, ids)] <- as.numeric(torch_sigmoid(logit))
    }
  })
  out
}

train_one <- function(variant, seed) {
  torch_manual_seed(seed)
  model <- if (variant == "seq") GRUSeq() else GRUFull(sdim = St$size(2))
  opt <- optim_adam(model$parameters, lr = LR, weight_decay = WD)
  best_auc <- -Inf; best_state <- NULL; bad <- 0L; best_ep <- 0L
  for (epoch in seq_len(MAX_EP)) {
    t0 <- Sys.time()
    model$train()
    for (b in make_batches(sub_idx, lens)) {
      opt$zero_grad()
      logit <- if (variant == "seq") model(Xt[b, , ], len_t[b])
      else model(Xt[b, , ], len_t[b], St[b, ])
      loss <- nnf_binary_cross_entropy_with_logits(logit, yt[b])
      loss$backward()
      opt$step()
    }
    pv <- predict_ids(model, variant, va_idx)
    auc_v <- as.numeric(auc(roc(y[va_idx], pv, quiet = TRUE)))
    if (auc_v > best_auc) {
      best_auc <- auc_v; best_ep <- epoch; bad <- 0L
      best_state <- lapply(model$state_dict(), function(t) t$clone())
    } else bad <- bad + 1L
    cat(sprintf("%s seed%d epoch %d valAUC %.4f best %.4f (ep%d) %.1fs\n",
                variant, seed, epoch, auc_v, best_auc, best_ep,
                as.numeric(difftime(Sys.time(), t0, units = "secs"))), flush = TRUE)
    if (bad >= PATIENCE) break
  }
  model$load_state_dict(best_state)
  list(model = model, val_auc = best_auc, best_ep = best_ep)
}

eval_binary <- function(y01, p, label) {
  roc_obj <- roc(y01, p, quiet = TRUE)
  pr <- pr.curve(scores.class0 = p[y01 == 1], scores.class1 = p[y01 == 0],
                 curve = FALSE)
  lp <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  data.frame(model = label, AUROC = as.numeric(auc(roc_obj)),
             AUPRC = pr$auc.integral, Brier = mean((p - y01)^2),
             cal_intercept = unname(coef(glm(y01 ~ offset(lp), family = binomial))[1]),
             cal_slope = unname(coef(glm(y01 ~ lp, family = binomial))[2]))
}

if (mode == "bench") {
  cat("benchmark: 2 epochs GRU-seq\n")
  invisible(train_one("seq", 1L))
  cat("BENCH_DONE\n")
  quit(save = "no")
}

seeds <- c(20250817L, 20250818L, 20250819L)
all_metrics <- list()
all_preds <- data.table(caseid = g$caseid[te_idx], outcome = y[te_idx])

for (variant in c("seq", "full")) {
  probs <- matrix(NA_real_, nrow = length(te_idx), ncol = length(seeds))
  for (si in seq_along(seeds)) {
    tag <- sprintf("gru_%s_s%d", variant, si)
    fit <- train_one(variant, seeds[si])
    torch_save(fit$model$state_dict(), file.path(OUT, paste0(tag, ".pt")))
    pv <- predict_ids(fit$model, variant, va_idx)
    pt <- predict_ids(fit$model, variant, te_idx)
    probs[, si] <- pt
    all_preds[[paste0(tag)]] <- pt
    m <- eval_binary(y[te_idx], pt, tag)
    m$val_AUROC <- fit$val_auc; m$best_epoch <- fit$best_ep
    all_metrics[[tag]] <- m
    saveRDS(list(metrics = all_metrics, preds = all_preds),
            file.path(OUT, "checkpoint.rds"))
    rm(fit); gc()
  }
  ens <- rowMeans(probs)
  all_preds[[sprintf("gru_%s_ens", variant)]] <- ens
  m <- eval_binary(y[te_idx], ens, sprintf("gru_%s_ens", variant))
  m$val_AUROC <- NA; m$best_epoch <- NA
  all_metrics[[sprintf("gru_%s_ens", variant)]] <- m
  saveRDS(list(metrics = all_metrics, preds = all_preds),
          file.path(OUT, "checkpoint.rds"))
}

metrics <- rbindlist(all_metrics, fill = TRUE)
fwrite(metrics, file.path(OUT, "metrics_gru.csv"))
fwrite(all_preds, file.path(OUT, "preds_gru.csv"))

# DeLong vs XGB_full on the same test set
tp <- fread("/workspace/gru/test_predictions.csv")
stopifnot(identical(tp$caseid, all_preds$caseid))
roc_xgb <- roc(tp$outcome, tp$XGB_full, quiet = TRUE)
for (v in c("seq", "full")) {
  roc_g <- roc(all_preds$outcome, all_preds[[sprintf("gru_%s_ens", v)]], quiet = TRUE)
  dl <- roc.test(roc_xgb, roc_g, method = "delong", paired = TRUE)
  cat(sprintf("DeLong XGB_full vs gru_%s_ens: diff %.4f, p = %.4g\n",
              v, as.numeric(auc(roc_xgb)) - as.numeric(auc(roc_g)), dl$p.value))
}
cat("DONE\n")
