###############################################################################
# DE base-rate concern — Study 1 extension
#
# Wu asked whether our gender/race-feedback finding could instead reflect that
# "people use these other measures appropriately in their initial selections."
# For Studies 3A-4B we tested this by subsetting on below-pool vs at-or-above-pool
# initial picks (see revision-analysis/de_4b_concern_analysis.R).
#
# Study 1 has no participant-initial selections to subset — the "initial
# portfolio" is a fixed stimulus describing 10 past NPR expert picks. The
# stimulus is varied between-subjects via `set_num` (set 1 vs set 2 carry
# different prior-selection percentages).
#
# The natural adaptation for Study 1: test whether each feedback effect
# depends on stimulus baseline (feedback × set_num). Wu's alternative
# predicts that comparison-attribute feedback would help where the stimulus
# baseline is low (room to move), and be null where the baseline is high.
# Our theory predicts null interactions for comparison attributes regardless
# of baseline, and an interaction for gender only via diminishing returns.
###############################################################################

rm(list = ls())
setwd("C:/Users/jcerv/Jose/diversity_feedback")

suppressPackageStartupMessages({
  library(dplyr)
  library(sandwich)
  library(lmtest)
})

robust_tab <- function(model) {
  vc <- vcovHC(model, type = "HC3")
  ct <- coeftest(model, vcov. = vc)
  ci <- coefci(model, vcov. = vc)
  data.frame(
    term   = rownames(ct),
    est    = round(ct[, 1], 4),
    se     = round(ct[, 2], 4),
    t      = round(ct[, 3], 2),
    p      = round(ct[, 4], 4),
    ci_lo  = round(ci[, 1], 4),
    ci_hi  = round(ci[, 2], 4),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

# Reusable runner — Study 1 flavor.
# feedback_var = the feedback indicator for the attribute
# dv_var       = attr_proportion DV
run_study1 <- function(d, feedback_var, dv_var, label) {
  d <- d[!is.na(d[[feedback_var]]) & !is.na(d[[dv_var]]) & !is.na(d$set_num), ]

  # Main effect (pre-registered spec: includes set_num control)
  f1 <- as.formula(paste0(dv_var, " ~ ", feedback_var, " + set_num"))
  m1 <- lm(f1, data = d)
  main <- robust_tab(m1)
  main_eff <- main[main$term == feedback_var, , drop = FALSE]

  # Interaction with set_num (stimulus-baseline proxy)
  f2 <- as.formula(paste0(dv_var, " ~ ", feedback_var, " * set_num"))
  m2 <- lm(f2, data = d)
  inter <- robust_tab(m2)
  interaction_row <- inter[grepl(":", inter$term), , drop = FALSE]

  cat("\n--------------------------------------------------------------\n")
  cat(label, "\n")
  cat("--------------------------------------------------------------\n")
  cat(sprintf("  N total: %d\n", nrow(d)))
  cat(sprintf("  Main effect of feedback (controlling for set_num):  %+0.2f pp (p=%.3f)\n",
              100 * main_eff$est, main_eff$p))
  if (nrow(interaction_row) > 0) {
    cat(sprintf("  feedback x set_num interaction:                     %+0.3f, p=%.3f\n",
                interaction_row$est[1], interaction_row$p[1]))
  }

  data.frame(
    attribute     = label,
    n_total       = nrow(d),
    main_eff_pp   = round(100 * main_eff$est, 2),
    main_p        = main_eff$p,
    inter_est     = if (nrow(interaction_row) > 0) round(interaction_row$est[1], 4) else NA,
    inter_p       = if (nrow(interaction_row) > 0) interaction_row$p[1]            else NA,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# STUDY 1 (gender — NPR experts)
# =============================================================================

cat("\n\n==============================================================\n")
cat("STUDY 1  (gender, NPR experts, between-subjects stimulus sets)\n")
cat("==============================================================\n")

d1 <- read.csv("Study-1/Study1.csv", check.names = FALSE)

rows <- list()
rows[[length(rows) + 1]] <- run_study1(d1, "women_feedback",     "women_proportion",
                                       "1: women feedback -> women pick (TARGET)")
rows[[length(rows) + 1]] <- run_study1(d1, "age_feedback",       "age_proportion",
                                       "1: age feedback -> age pick")
rows[[length(rows) + 1]] <- run_study1(d1, "location_feedback",  "location_proportion",
                                       "1: location feedback -> location pick")
rows[[length(rows) + 1]] <- run_study1(d1, "university_feedback", "university_proportion",
                                       "1: university feedback -> university pick")

res1 <- do.call(rbind, rows)
res1$study <- "Study 1 (gender, NPR)"
res1 <- res1[, c("study", "attribute", "n_total",
                 "main_eff_pp", "main_p", "inter_est", "inter_p")]

cat("\n\n==============================================================\n")
cat("STUDY 1 SUMMARY\n")
cat("==============================================================\n")
print(res1, row.names = FALSE)

write.csv(res1, "revision-analysis/de_study1_concern_results.csv",
          row.names = FALSE)
cat("\nSaved: revision-analysis/de_study1_concern_results.csv\n")
