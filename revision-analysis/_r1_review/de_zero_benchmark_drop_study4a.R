###############################################################################
# Zero-benchmark analysis with STUDY 4A dropped.
#
# WHY: Study 4A is a race-only study. In the zero-initial restriction it is
# the single largest target cell (N_zero = 358 of 1,222), yet it contributes
# ZERO comparison cells to the n_zero >= 20 filter (all three 4A comparison
# attributes -- Books, Classic, Poets -- have pool prevalence 52-60%, so
# initial = 0 is rare). That asymmetry could drive the target vs comparison
# contrast. Re-run dropping Study 4A entirely to check robustness.
#
# Outputs:
#   - per-cell results without Study 4A
#   - REML pooled effects (target, comparison) without Study 4A
#   - long-format FE regression: target vs comparison contrast at initial=0
###############################################################################

rm(list = ls())
setwd("C:/Users/jcerv/Jose/diversity_feedback")

suppressPackageStartupMessages({
  library(dplyr)
  library(sandwich)
  library(lmtest)
  library(metafor)
})

# Reuse the per-cell results and long-format file from the original run.
all_res  <- read.csv("revision-analysis/_r1_review/de_zero_benchmark_results.csv",
                     stringsAsFactors = FALSE)
long_all <- read.csv("revision-analysis/_r1_review/de_zero_benchmark_long.csv",
                     stringsAsFactors = FALSE)

cat("=== Sanity check: rows in original run ===\n")
cat(sprintf("  per-cell rows: %d\n", nrow(all_res)))
cat(sprintf("  long-format rows: %d\n", nrow(long_all)))
cat(sprintf("  studies in long-format: %s\n",
            paste(sort(unique(long_all$study)), collapse = ", ")))

# --------- Filter out Study 4A ----------------------------------------------
res_no4a  <- all_res[all_res$study != "Study 4A", ]
long_no4a <- long_all[long_all$study != "Study4A", ]
long_no4a$cell <- factor(long_no4a$cell)

cat("\n=== After dropping Study 4A ===\n")
cat(sprintf("  per-cell rows: %d\n", nrow(res_no4a)))
cat(sprintf("  long-format rows: %d\n", nrow(long_no4a)))
cat(sprintf("  remaining studies: %s\n",
            paste(sort(unique(long_no4a$study)), collapse = ", ")))

# =============================================================================
# Random-effects pool (REML) -- per-cell, no Study 4A
# =============================================================================
pool_one <- function(est, se, label) {
  ok <- !is.na(est) & !is.na(se) & is.finite(se) & se > 0
  est_ok <- est[ok]; se_ok <- se[ok]
  if (length(est_ok) < 2) {
    return(data.frame(group=label, k=length(est_ok), pooled=NA, se=NA,
                      ci_lo=NA, ci_hi=NA, p=NA, i2=NA))
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

target_rows     <- res_no4a[res_no4a$is_target == TRUE, ]
comparison_rows <- res_no4a[res_no4a$is_target == FALSE, ]

cat("\n\n===========================================================\n")
cat("REML pool (no Study 4A), percentage points\n")
cat("===========================================================\n")
re_pools <- rbind(
  pool_one(target_rows$all_est_pp,      target_rows$all_se_pp,      "Target -- overall"),
  pool_one(target_rows$zero_est_pp,     target_rows$zero_se_pp,     "Target -- initial = 0"),
  pool_one(target_rows$pos_est_pp,      target_rows$pos_se_pp,      "Target -- initial >= 1"),
  pool_one(comparison_rows$all_est_pp,  comparison_rows$all_se_pp,  "Comparison -- overall"),
  pool_one(comparison_rows$zero_est_pp, comparison_rows$zero_se_pp, "Comparison -- initial = 0"),
  pool_one(comparison_rows$pos_est_pp,  comparison_rows$pos_se_pp,  "Comparison -- initial >= 1")
)
print(re_pools, row.names = FALSE)
write.csv(re_pools, "revision-analysis/_r1_review/de_zero_benchmark_no4a_re_pools.csv",
          row.names = FALSE)

# =============================================================================
# Long-format FE pool (cell FE, participant-clustered SE) -- TARGET vs COMPARISON
# =============================================================================
cluster_se <- function(model, cluster_var) {
  vc <- sandwich::vcovCL(model, cluster = cluster_var, type = "HC1")
  list(coef = lmtest::coeftest(model, vcov. = vc),
       ci   = lmtest::coefci(model, vcov. = vc))
}

run_fe_pool <- function(dat, label) {
  dat$cell <- droplevels(factor(dat$cell))
  cat(sprintf("\n--- FE pool: %s (N=%d obs, %d cells, %d participants) ---\n",
              label, nrow(dat), nlevels(dat$cell), length(unique(dat$pid))))

  mA <- lm(pick ~ feedback + cell, data = dat)
  rA <- cluster_se(mA, dat$pid)
  iA <- which(rownames(rA$coef) == "feedback")
  cat(sprintf("  A. feedback (main, cell FE):       %+0.2f pp  SE %.3f  p=%.4g  CI [%.2f, %.2f]\n",
              100*rA$coef[iA,1], 100*rA$coef[iA,2], rA$coef[iA,4],
              100*rA$ci[iA,1], 100*rA$ci[iA,2]))

  dat_zero <- dat[dat$zero_initial == 1, ]
  dat_zero$cell <- droplevels(dat_zero$cell)
  if (nrow(dat_zero) >= 20 && length(unique(dat_zero$feedback)) == 2) {
    mC <- lm(pick ~ feedback + cell, data = dat_zero)
    rC <- cluster_se(mC, dat_zero$pid)
    iC <- which(rownames(rC$coef) == "feedback")
    cat(sprintf("  C. feedback within initial=0:       %+0.2f pp  SE %.3f  p=%.4g  CI [%.2f, %.2f]   (N=%d, cells=%d)\n",
                100*rC$coef[iC,1], 100*rC$coef[iC,2], rC$coef[iC,4],
                100*rC$ci[iC,1], 100*rC$ci[iC,2], nrow(dat_zero), nlevels(dat_zero$cell)))
  } else {
    cat("  C. feedback within initial=0:       <too few obs>\n")
  }
  invisible(NULL)
}

cat("\n\n===========================================================\n")
cat("Long-format FE pool, NO Study 4A\n")
cat("===========================================================\n")
run_fe_pool(long_no4a[long_no4a$is_target == TRUE,  ], "TARGET (race/gender)")
run_fe_pool(long_no4a[long_no4a$is_target == FALSE, ], "COMPARISON (other)")

# =============================================================================
# Target vs Comparison contrast at initial = 0 -- NO Study 4A
# =============================================================================
long_zero <- long_no4a[long_no4a$zero_initial == 1, ]
long_zero$cell <- droplevels(factor(long_zero$cell))

cat("\n\n===========================================================\n")
cat("Contrast at initial=0 :  target vs comparison (NO Study 4A)\n")
cat("===========================================================\n")
cat(sprintf("Stacked sample at initial=0: %d obs over %d cells, %d participants\n",
            nrow(long_zero), nlevels(long_zero$cell), length(unique(long_zero$pid))))
cat(sprintf("  Target cells:     %d obs across %d cells\n",
            sum(long_zero$is_target), length(unique(long_zero$cell[long_zero$is_target]))))
cat(sprintf("  Comparison cells: %d obs across %d cells\n",
            sum(!long_zero$is_target), length(unique(long_zero$cell[!long_zero$is_target]))))

m_contrast <- lm(pick ~ feedback * is_target + cell, data = long_zero)
vc_contrast <- sandwich::vcovCL(m_contrast, cluster = long_zero$pid, type = "HC1")
ct <- lmtest::coeftest(m_contrast, vcov. = vc_contrast)
ci <- lmtest::coefci(m_contrast, vcov. = vc_contrast)

show_row <- function(name, label) {
  i <- which(rownames(ct) == name)
  if (length(i) == 0) { cat(sprintf("  %-45s <missing>\n", label)); return(NULL) }
  cat(sprintf("  %-45s  %+6.2f pp   SE %5.3f   p=%-9.4g   CI [%+0.2f, %+0.2f]\n",
              label, 100*ct[i,1], 100*ct[i,2], ct[i,4], 100*ci[i,1], 100*ci[i,2]))
}
show_row("feedback",                  "Comparison effect at initial=0:")
show_row("feedback:is_targetTRUE",    "Target - Comparison contrast:")

# Reparameterise so the feedback coef is the TARGET cell main effect at initial=0
long_zero$is_comparison <- !long_zero$is_target
m_contrast_b <- lm(pick ~ feedback * is_comparison + cell, data = long_zero)
vc_b <- sandwich::vcovCL(m_contrast_b, cluster = long_zero$pid, type = "HC1")
ct_b <- lmtest::coeftest(m_contrast_b, vcov. = vc_b)
ci_b <- lmtest::coefci(m_contrast_b, vcov. = vc_b)
i_t <- which(rownames(ct_b) == "feedback")
cat(sprintf("  %-45s  %+6.2f pp   SE %5.3f   p=%-9.4g   CI [%+0.2f, %+0.2f]\n",
            "Target effect at initial=0:",
            100*ct_b[i_t,1], 100*ct_b[i_t,2], ct_b[i_t,4], 100*ci_b[i_t,1], 100*ci_b[i_t,2]))

# =============================================================================
# Side comparison: drop ONLY Study 4A's target row (keep its comparison nothing-cells)
# This isolates the target-side N=358 cell instead of removing the whole study.
# =============================================================================
cat("\n\n===========================================================\n")
cat("Side test: drop only Study 4A's TARGET cell from REML target pool\n")
cat("===========================================================\n")
tgt_no4a <- all_res[all_res$is_target == TRUE & all_res$study != "Study 4A", ]
print(pool_one(tgt_no4a$zero_est_pp, tgt_no4a$zero_se_pp,
               "Target initial=0 (no Study 4A target row)"), row.names = FALSE)

cat("\nSaved:\n")
cat("  revision-analysis/_r1_review/de_zero_benchmark_no4a_re_pools.csv\n")
