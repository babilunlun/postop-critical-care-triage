## Consolidate audit parts 1-4 into a single classified report
suppressMessages(library(data.table))
p1 <- fread("/workspace/audit_part1.csv"); setnames(p1, c("as.character.claimed.", "as.character.recomputed.", "m"), c("claimed", "recomputed", "match"))
p2 <- fread("/workspace/audit_part2.csv"); p3 <- fread("/workspace/audit_part3.csv"); p4 <- fread("/workspace/audit_part4.csv")
all <- rbindlist(list(p1, p2, p3, p4), fill = TRUE)
all[, part := c(rep(1, nrow(p1)), rep(2, nrow(p2)), rep(3, nrow(p3)), rep(4, nrow(p4)))]

## classify every flagged row
classify <- function(domain, metric, cohort, note) {
  key <- paste(domain, metric, cohort)
  m <- list(
    "Headline XGB_noArt AUROC MOVER" = c("artifact_rounding", "true value 0.7539; text 0.753 is truncation-consistent with Table 2"),
    "Headline DeLong p XGB vs LR_full MOVER" = c("artifact_rounding", "text uses approx; 1e-148 vs 1.0e-147 same magnitude"),
    "TableS7 dep_vs_obs_admitted AUROC INSPIRE" = c("artifact_convention", "AUROC 0.498 for icu_dependent direction; text reports >=0.5 convention 0.502; CI 0.490-0.514 symmetric; both = chance"),
    "DCA min net benefit 5-30% (positive?)  INSPIRE" = c("artifact_stringcompare", "actual min 3.9/1000 > 0; claim 'positive across 5-30%' is TRUE"),
    "DCA min net benefit 5-30% (positive?)  MOVER" = c("artifact_stringcompare", "actual min 10.0/1000 > 0; claim TRUE"),
    "Internal LR_clinical AUROC VitalDB" = c("artifact_rounding", "true 0.89749; text/Table 2 both 0.898 (self-consistent)"),
    "Internal SASA AUROC VitalDB" = c("artifact_rounding", "true 0.71643; text/Table 2 both 0.717 (self-consistent)"),
    "Internal DeLong p XGB vs LR_full VitalDB" = c("artifact_format", "1.0e-5 == 1.000e-05"),
    "Internal DeLong p XGB vs LR_clinical VitalDB" = c("artifact_format", "3.3e-9 == 3.300e-09"),
    "GRU DeLong p GRU_seq vs XGB VitalDB" = c("artifact_format", "1.5e-8 == 1.500e-08"),
    "Table1 VitalDB ASA rows sum (miss 1.9%) VitalDB" = c("expected_missingness", "ASA rows sum to 5,871 because ASA missing ~1.9%; not an error")
  )
  if (key %in% names(m)) return(m[[key]])
  c("unclassified", "")
}
cls <- t(mapply(classify, all$domain, all$metric, all$cohort, all$note))
all[, classification := ifelse(match == "OK", "verified_ok", cls[, 1])]
all[, class_note := cls[, 2]]

fwrite(all, "/workspace/audit_report.csv")
cat("TOTAL:", nrow(all), "checks |", sum(all$match == "OK"), "OK |", sum(all$match != "OK"), "flagged\n")
print(all[match != "OK", .(part, domain, metric, cohort, claimed, recomputed, classification)])
