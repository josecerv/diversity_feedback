###############################################################################
# Cross-study meta-analysis for the DE base-rate concern
#
# Goal: produce two pooled views.
#
#   (1) Overall pools — target vs comparison main effects pooled across the
#       five participant-driven studies (2, 3A, 3B, 4A, 4B). Used for the
#       Discussion paragraph and as a sanity check.
#
#   (2) CRITICAL-CELL pools — the pools that directly answer the DE's
#       "people use these other measures appropriately at baseline"
#       concern:
#         (a) Target effect among At/Above-Pool participants (no
#             underrepresentation gap to close), k = 5.
#         (b) Comparison effect among Below-Pool participants (clear gap
#             to close), k = 15.
#         (c) The contrast between (a) and (b).
#
# Inputs:
#   revision-analysis/de_4b_concern_results.csv  (Studies 2, 3A, 3B, 4A, 4B
#                                                  — all five participant-
#                                                  driven initial portfolios)
#
# Outputs:
#   revision-analysis/de_meta_by_attribute_type_rows.csv  — per-row long
#   revision-analysis/de_meta_by_attribute_type_pools.csv — overall pools
#   revision-analysis/de_meta_critical_cells.csv          — critical-cell pools
#   revision-analysis/de_meta_forest.pdf                  — forest plot
###############################################################################

rm(list = ls())
setwd("C:/Users/jcerv/Jose/diversity_feedback")

suppressPackageStartupMessages({
  library(dplyr)
  library(metafor)
  library(ggplot2)
})

# ---------------------------------------------------------------------------
# Load and tidy
# ---------------------------------------------------------------------------

bulk <- read.csv("revision-analysis/de_4b_concern_results.csv", stringsAsFactors = FALSE)

bulk <- bulk %>% mutate(is_target = grepl("TARGET", attribute))

# Harmonize column sets. `se` is not stored in the CSVs — derive it from
# main_eff_pp and main_p using the two-sided normal approximation (robust HC3
# SEs in the source models mean this approximation is close to exact for
# these N~1000 samples). We back out SE from |z| = qnorm(1 - p/2).
derive_se <- function(est_pp, p) {
  p_clamped <- pmin(pmax(p, 1e-10), 1 - 1e-10)
  z <- qnorm(1 - p_clamped / 2)
  # Avoid division by zero when p ~ 1 (z ~ 0).
  ifelse(z < 1e-4, NA_real_, abs(est_pp) / z)
}

bulk_tidy <- bulk %>%
  transmute(
    study,
    attribute_raw = attribute,
    attribute_label = sub("^[0-9AB]+:\\s*", "", attribute),
    attribute_label = sub("\\s*->.*", "", attribute_label),
    is_target,
    # Overall (main) effect
    n = n_total,
    eff_pp = main_eff_pp,
    p = main_p,
    se_pp = derive_se(main_eff_pp, main_p),
    # Below-Pool subgroup
    below_n = below_n,
    below_eff_pp = below_eff_pp,
    below_p = below_p,
    below_se_pp = derive_se(below_eff_pp, below_p),
    # At/Above-Pool subgroup
    above_n = above_n,
    above_eff_pp = above_eff_pp,
    above_p = above_p,
    above_se_pp = derive_se(above_eff_pp, above_p)
  )

all_rows <- bulk_tidy

# ---------------------------------------------------------------------------
# Pool by attribute type (target vs comparison), using random-effects meta.
# Effect size = percentage-point change in pick probability; SE derived above.
# ---------------------------------------------------------------------------

pool_group <- function(estimates, ses, label) {
  ok <- !is.na(estimates) & !is.na(ses) & is.finite(ses) & ses > 0
  est_ok <- estimates[ok]
  se_ok  <- ses[ok]
  if (length(est_ok) < 2) {
    return(tibble(
      group = label,
      k = length(est_ok),
      pooled_est = NA_real_,
      pooled_se  = NA_real_,
      pooled_p   = NA_real_,
      ci_lo      = NA_real_,
      ci_hi      = NA_real_,
      i2         = NA_real_
    ))
  }
  fit <- rma(yi = est_ok, sei = se_ok, method = "REML")
  tibble(
    group = label,
    k = fit$k,
    pooled_est = round(as.numeric(fit$b), 2),
    pooled_se  = round(fit$se, 3),
    pooled_p   = round(fit$pval, 4),
    ci_lo      = round(fit$ci.lb, 2),
    ci_hi      = round(fit$ci.ub, 2),
    i2         = round(fit$I2, 1)
  )
}

# ---- Overall pools (main effects) ----
target_rows     <- all_rows %>% filter(is_target)
comparison_rows <- all_rows %>% filter(!is_target)

target_pool     <- pool_group(target_rows$eff_pp,     target_rows$se_pp,     "Target (gender / race)")
comparison_pool <- pool_group(comparison_rows$eff_pp, comparison_rows$se_pp, "Comparison attributes")
pools <- bind_rows(target_pool, comparison_pool)

cat("\n==============================================================\n")
cat("OVERALL POOLED ESTIMATES (main effects, random-effects REML)\n")
cat("==============================================================\n")
print(pools, row.names = FALSE)

# ---- Critical-cell pools ----
# The pools that directly answer Wu's concern:
#   (a) Target effect among At/Above-Pool participants — no gap to close
#   (b) Comparison effect among Below-Pool participants — gap to close
target_above_pool    <- pool_group(target_rows$above_eff_pp,    target_rows$above_se_pp,
                                   "Target — At/Above-Pool subgroup")
comparison_below_pool <- pool_group(comparison_rows$below_eff_pp, comparison_rows$below_se_pp,
                                    "Comparison — Below-Pool subgroup")

# Also compute the "complementary" cells for context:
target_below_pool    <- pool_group(target_rows$below_eff_pp,    target_rows$below_se_pp,
                                   "Target — Below-Pool subgroup")
comparison_above_pool <- pool_group(comparison_rows$above_eff_pp, comparison_rows$above_se_pp,
                                    "Comparison — At/Above-Pool subgroup")

critical <- bind_rows(target_above_pool, comparison_below_pool,
                      target_below_pool, comparison_above_pool)

cat("\n==============================================================\n")
cat("CRITICAL-CELL POOLED ESTIMATES (subgroup-conditional, REML)\n")
cat("==============================================================\n")
print(critical, row.names = FALSE)

# ---------------------------------------------------------------------------
# Target - comparison contrast.
# Pool within each group (as above) and then compute the difference in
# pooled estimates, with SE = sqrt(SE1^2 + SE2^2) (independence assumption —
# effects come from the same studies but different attributes, so not quite
# independent; this is a conservative first pass).
# ---------------------------------------------------------------------------

contrast_print <- function(t_pool, c_pool, label) {
  if (is.na(t_pool$pooled_est) || is.na(c_pool$pooled_est)) {
    cat(sprintf("\n[%s — missing pooled estimate]\n", label)); return(invisible(NULL))
  }
  delta <- t_pool$pooled_est - c_pool$pooled_est
  se_delta <- sqrt(t_pool$pooled_se^2 + c_pool$pooled_se^2)
  z <- delta / se_delta
  p_delta <- 2 * pnorm(-abs(z))
  cat(sprintf(
    "\n%-50s %+0.2f pp (SE %.2f, z = %.2f, p = %.4f)\n",
    label, delta, se_delta, z, p_delta
  ))
  invisible(list(delta = delta, se = se_delta, z = z, p = p_delta))
}

cat("\n==============================================================\n")
cat("CONTRASTS\n")
cat("==============================================================")
overall_contrast <- contrast_print(target_pool, comparison_pool,
                                   "Overall: Target - Comparison")
critical_contrast <- contrast_print(target_above_pool, comparison_below_pool,
                                    "Critical: Target(At/Above) - Comparison(Below)")

# ---------------------------------------------------------------------------
# Persist
# ---------------------------------------------------------------------------

all_rows_out <- all_rows %>%
  mutate(attribute_label = if_else(is_target,
                                   paste0(attribute_label, " (TARGET)"),
                                   attribute_label)) %>%
  select(study, attribute_label, n, eff_pp, p, se_pp,
         below_n, below_eff_pp, below_p, below_se_pp,
         above_n, above_eff_pp, above_p, above_se_pp)

write.csv(all_rows_out, "revision-analysis/de_meta_by_attribute_type_rows.csv",
          row.names = FALSE)
write.csv(pools, "revision-analysis/de_meta_by_attribute_type_pools.csv",
          row.names = FALSE)

# Save critical-cell pools + overall contrast + critical contrast for use by
# the table builder and response-letter prose.
critical_with_contrasts <- bind_rows(
  critical,
  if (!is.null(overall_contrast))
    tibble(group = "Contrast: Overall Target - Overall Comparison",
           k = NA_integer_,
           pooled_est = round(overall_contrast$delta, 2),
           pooled_se  = round(overall_contrast$se, 3),
           pooled_p   = round(overall_contrast$p, 4),
           ci_lo = NA_real_, ci_hi = NA_real_, i2 = NA_real_),
  if (!is.null(critical_contrast))
    tibble(group = "Contrast: Target(At/Above) - Comparison(Below)",
           k = NA_integer_,
           pooled_est = round(critical_contrast$delta, 2),
           pooled_se  = round(critical_contrast$se, 3),
           pooled_p   = round(critical_contrast$p, 4),
           ci_lo = NA_real_, ci_hi = NA_real_, i2 = NA_real_)
)
write.csv(critical_with_contrasts, "revision-analysis/de_meta_critical_cells.csv",
          row.names = FALSE)

cat("\nSaved:\n")
cat("  revision-analysis/de_meta_by_attribute_type_rows.csv\n")
cat("  revision-analysis/de_meta_by_attribute_type_pools.csv\n")
cat("  revision-analysis/de_meta_critical_cells.csv\n")

# ---------------------------------------------------------------------------
# Forest plot
# ---------------------------------------------------------------------------

plot_rows <- all_rows %>%
  mutate(
    group = if_else(is_target, "Target (gender / race)", "Comparison attribute"),
    label = paste0(study, " · ", attribute_label),
    ci_lo = eff_pp - 1.96 * se_pp,
    ci_hi = eff_pp + 1.96 * se_pp
  ) %>%
  arrange(group, study, attribute_label)

pool_rows <- pools %>%
  mutate(
    label = paste0("Pooled (", group, ")"),
    group = if_else(group == "Target (gender / race)", "Target (gender / race)",
                    "Comparison attribute"),
    eff_pp = pooled_est,
    ci_lo  = ci_lo,
    ci_hi  = ci_hi
  )

combined <- bind_rows(
  plot_rows %>% select(label, group, eff_pp, ci_lo, ci_hi) %>% mutate(shape = "study"),
  pool_rows %>% select(label, group, eff_pp, ci_lo, ci_hi) %>% mutate(shape = "pool")
)

combined$label <- factor(combined$label, levels = rev(combined$label))

p <- ggplot(combined, aes(x = eff_pp, y = label, color = group, shape = shape)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.25) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Target (gender / race)" = "#990000",
                                "Comparison attribute"   = "#011F5B")) +
  scale_shape_manual(values = c("study" = 19, "pool" = 18)) +
  labs(x = "Feedback effect on final selection (percentage points)",
       y = NULL,
       title = "Cross-study meta-analysis of feedback effects",
       subtitle = "Target (gender/race) vs comparison attributes, per study + pooled (REML)") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("revision-analysis/de_meta_forest.pdf", p, width = 9, height = 7)
cat("  revision-analysis/de_meta_forest.pdf\n")
