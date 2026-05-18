###############################################################################
# Race-only vs gender-only zero-initial pooled effects.
#
# WHY: Aneesh asked in the response letter whether we can report the +22.50pp
# pooled effect broken out as race-only vs gender-only. Also clarify clustering.
#
# Race cells: Study 3A (gender = race feedback -> race_pick), Study 4A.
# Gender cells: Study 2, Study 3B, Study 4B.
###############################################################################

rm(list = ls())
setwd("C:/Users/jcerv/Jose/diversity_feedback")

suppressPackageStartupMessages({
  library(sandwich)
  library(lmtest)
  library(metafor)
})

long <- read.csv("revision-analysis/_r1_review/de_zero_benchmark_long.csv",
                 stringsAsFactors = FALSE)
res  <- read.csv("revision-analysis/_r1_review/de_zero_benchmark_results.csv",
                 stringsAsFactors = FALSE)

# Mark race vs gender among the target rows
res$tgt_kind <- NA
res$tgt_kind[res$is_target & res$study %in% c("Study 3A", "Study 4A")] <- "Race"
res$tgt_kind[res$is_target & res$study %in% c("Study 2", "Study 3B", "Study 4B")] <- "Gender"

cat("\n========================================\n")
cat("Per-cell zero-initial estimates (target only)\n")
cat("========================================\n")
keep <- res$is_target == TRUE
print(res[keep, c("study","tgt_kind","n_zero","zero_est_pp","zero_se_pp","zero_p")],
      row.names = FALSE)

# --- REML pool: race vs gender separately --------------------------------
pool_one <- function(est, se, label) {
  ok <- !is.na(est) & !is.na(se) & is.finite(se) & se > 0
  est_ok <- est[ok]; se_ok <- se[ok]
  if (length(est_ok) < 2) {
    return(data.frame(group=label, k=length(est_ok),
                      pooled=est_ok, se=se_ok,
                      ci_lo=est_ok - 1.96*se_ok, ci_hi=est_ok + 1.96*se_ok,
                      p=2*pnorm(-abs(est_ok/se_ok)), i2=NA))
  }
  fit <- metafor::rma(yi = est_ok, sei = se_ok, method = "REML")
  data.frame(group = label,
             k     = fit$k,
             pooled= round(as.numeric(fit$b), 3),
             se    = round(fit$se, 3),
             ci_lo = round(fit$ci.lb, 3),
             ci_hi = round(fit$ci.ub, 3),
             p     = round(fit$pval, 4),
             i2    = round(fit$I2, 1))
}

race_rows   <- res[res$tgt_kind == "Race"   & !is.na(res$tgt_kind), ]
gender_rows <- res[res$tgt_kind == "Gender" & !is.na(res$tgt_kind), ]

cat("\n=== REML pools (separate race vs gender) ===\n")
re_split <- rbind(
  pool_one(race_rows$zero_est_pp,   race_rows$zero_se_pp,   "Race only -- initial=0"),
  pool_one(gender_rows$zero_est_pp, gender_rows$zero_se_pp, "Gender only -- initial=0")
)
print(re_split, row.names = FALSE)

# --- Long-format FE pooled regressions for the same split ----------------
# Long format: each row = participant x attribute. Target rows: each
# participant contributes exactly 1 row (their study's target attribute).
# Comparison rows: each participant contributes up to 3 rows (3 comparison
# attributes per study). Thus clustering matters for the comparison and
# combined pools, less for the target-only pool, but we cluster everywhere
# for consistency.

long_target_zero <- long[long$is_target == TRUE & long$zero_initial == 1, ]
long_target_zero$cell <- droplevels(factor(long_target_zero$cell))
long_target_zero$kind <- ifelse(long_target_zero$study %in% c("Study3A","Study4A"),
                                "Race","Gender")

cat("\n=== FE regression with cell FE + cluster-robust SE (target zero-initial) ===\n")
cat(sprintf("Target rows at initial=0: N=%d obs, %d cells, %d participants\n",
            nrow(long_target_zero), nlevels(long_target_zero$cell),
            length(unique(long_target_zero$pid))))
cat("(In target-only data each participant has exactly one row, so cluster-robust = HC1.)\n")

run_fe <- function(dat, label) {
  if (nrow(dat) == 0) { cat(sprintf("  %s: empty\n", label)); return(NULL) }
  dat$cell <- droplevels(factor(dat$cell))
  if (nlevels(dat$cell) >= 2) {
    m  <- lm(pick ~ feedback + cell, data = dat)
  } else {
    m  <- lm(pick ~ feedback, data = dat)
  }
  vc <- sandwich::vcovCL(m, cluster = dat$pid, type = "HC1")
  ct <- lmtest::coeftest(m, vcov. = vc)
  ci <- lmtest::coefci(m, vcov. = vc)
  i  <- which(rownames(ct) == "feedback")
  cat(sprintf("  %-40s %+0.2f pp  SE %.3f  p=%.4g  CI [%.2f, %.2f]  (N=%d, cells=%d, pid=%d)\n",
              label, 100*ct[i,1], 100*ct[i,2], ct[i,4],
              100*ci[i,1], 100*ci[i,2],
              nrow(dat), nlevels(dat$cell), length(unique(dat$pid))))
}

run_fe(long_target_zero,                                  "All target (race+gender):")
run_fe(long_target_zero[long_target_zero$kind=="Race",],   "Race only:")
run_fe(long_target_zero[long_target_zero$kind=="Gender",], "Gender only:")

# Drop Study 4A (per Katy's comment 8)
cat("\n=== Same split, dropping Study 4A ===\n")
no4a <- long_target_zero[long_target_zero$study != "Study4A", ]
run_fe(no4a,                                  "All target (race+gender), no 4A:")
run_fe(no4a[no4a$kind=="Race",],              "Race only, no 4A:")
run_fe(no4a[no4a$kind=="Gender",],            "Gender only, no 4A:")

# --- Clarify clustering: how many rows per participant on the comparison side?
cat("\n=== Clustering diagnostics (long-format data) ===\n")
rows_per_pid_target  <- table(table(long$pid[long$is_target == TRUE]))
rows_per_pid_compare <- table(table(long$pid[long$is_target == FALSE]))
cat("Target-only long data, rows per participant:\n");  print(rows_per_pid_target)
cat("Comparison-only long data, rows per participant:\n"); print(rows_per_pid_compare)

# For the published contrast (target vs comparison at initial=0) the same
# participant can show up as 1 target row + up to 3 comparison rows. Confirm.
zero_long <- long[long$zero_initial == 1, ]
cat("\nAt initial=0 (stacked target+comparison): rows per participant\n")
print(table(table(zero_long$pid)))
