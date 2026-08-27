import pandas as pd
df = pd.read_csv("/mnt/results/06_icu_course/refined_categories_inspire.csv")
ei = (df.early_icu == 1); di = (df.delayed_icu == 1); dth = (df.death_idx == 1)

early = int(ei.sum())
dep_obs = int(df.category.isin(["icu_dependent", "observational_icu"]).sum())
deaths_no_early = int((dth & ~ei).sum())
delayed_only = int((di & ~ei & ~dth).sum())
composite = early + deaths_no_early

print("early_icu (admitted <=24h):", early)
print("  icu_dependent+observational:", dep_obs, "(match:", early == dep_obs, ")")
print("deaths without early ICU:", deaths_no_early)
print("delayed-only (delayed & ~early & survived):", delayed_only)
print("missed_escalation total:", int((df.category == "missed_escalation").sum()),
      "= delayed_only + deaths =", delayed_only, "+", deaths_no_early)
print()
print("COMPOSITE = early + deaths_no_early =", composite, "(matches 10,370:", composite == 10370, ")")
print("observational/composite =", round(5958 / composite * 100, 1), "%")
print("9927+1080 =", 9927 + 1080, "-> overcounts by", 9927 + 1080 - 10370, "(= the 637 non-composite delayed-only)")
print()
# MOVER
m = pd.read_csv("/mnt/results/06_icu_course/refined_categories_mover.csv")
print("MOVER category counts:", m.category.value_counts().to_dict())
mc = int(m.category.isin(["icu_dependent", "observational_icu", "missed_escalation"]).sum())
print("MOVER composite =", mc, "(matches 21,740:", mc == 21740, ")")
print("MOVER observational/composite =", round(15821 / mc * 100, 1), "%")
