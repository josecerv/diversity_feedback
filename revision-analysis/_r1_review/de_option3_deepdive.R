###############################################################################
# Option 3 deep dive
#
# Goal: produce, for Katy, a fully interpretable read on the centered-at-pool
# interaction model.
#
#   final_pick = b0 + b1*feedback + b2*gap + b3*feedback*gap
#       gap = initial_rate - pool_rate
#
#   b1: feedback effect when initial_rate == pool_rate (the "no gap" anchor)
#   b3: how the feedback effect changes with each unit increase in gap
#       (negative b3 means feedback effects shrink as initial rate rises -
#       feedback nudges toward the pool)
#
# We produce:
#   (a) Predicted feedback effect at gap = -0.20, -0.10, 0, +0.10, +0.20 per
#       study & for the cross-study pool, with HC3 SEs and CIs.
#   (b) The gap value at which the predicted feedback effect crosses zero
#       (the "reversal point"), per target study.
#   (c) A formal contrast: target interaction vs comparison interaction.
#   (d) A forest-style plot of predicted feedback effects across gap values.
###############################################################################

rm(list = ls())
setwd("C:/Users/jcerv/Jose/diversity_feedback")

suppressPackageStartupMessages({
  library(dplyr)
  library(sandwich)
  library(lmtest)
  library(metafor)
  library(ggplot2)
})

# ---------- HC3 helpers ----------
robust_vcov <- function(model) vcovHC(model, type = "HC3")
robust_coef <- function(model) {
  ct <- coeftest(model, vcov. = robust_vcov(model))
  ci <- coefci(model, vcov. = robust_vcov(model))
  data.frame(term = rownames(ct),
             est  = unname(ct[, 1]),
             se   = unname(ct[, 2]),
             p    = unname(ct[, 4]),
             ci_lo = unname(ci[, 1]),
             ci_hi = unname(ci[, 2]),
             stringsAsFactors = FALSE)
}

# Predicted feedback effect at a given gap value, with delta-method SE on
# beta_feedback + g * beta_(feedback:gap)
predicted_effect_at_gap <- function(model, feedback_var, g) {
  vc <- robust_vcov(model)
  cf <- coef(model)
  inter_term <- paste0(feedback_var, ":gap")
  if (!(feedback_var %in% names(cf))) stop("feedback term not in model")
  if (!(inter_term %in% names(cf)))   stop("interaction term not in model")
  est <- as.numeric(cf[feedback_var] + g * cf[inter_term])
  v   <- as.numeric(vc[feedback_var, feedback_var]
                    + g^2 * vc[inter_term, inter_term]
                    + 2 * g * vc[feedback_var, inter_term])
  se  <- sqrt(v)
  z   <- est / se
  p   <- 2 * pnorm(-abs(z))
  data.frame(gap = g,
             est_pp   = round(100 * est, 2),
             se_pp    = round(100 * se, 3),
             ci_lo_pp = round(100 * (est - 1.96 * se), 2),
             ci_hi_pp = round(100 * (est + 1.96 * se), 2),
             p        = round(p, 4))
}

# Reversal point: g* such that beta_feedback + g* * beta_(feedback:gap) = 0
#   g* = -beta_feedback / beta_(feedback:gap)
reversal_gap <- function(model, feedback_var) {
  cf <- coef(model)
  inter_term <- paste0(feedback_var, ":gap")
  b1 <- as.numeric(cf[feedback_var])
  b3 <- as.numeric(cf[inter_term])
  if (abs(b3) < 1e-9) return(NA_real_)
  -b1 / b3
}

add_base <- function(d, choice_cols, item_list, new_name) {
  d[[new_name]] <- rowSums(sapply(choice_cols, function(cc) d[[cc]] %in% item_list))
  d
}

# ---------- Per-attribute single-study analysis ----------
fit_one <- function(d, feedback_var, pick_var, base_var, pool_rate, n_initial,
                    label, study_label) {
  d <- d[!is.na(d[[feedback_var]]) & !is.na(d[[pick_var]]) & !is.na(d[[base_var]]), ]
  d$init_rate <- d[[base_var]] / n_initial
  d$gap <- d$init_rate - pool_rate

  f <- as.formula(paste0(pick_var, " ~ ", feedback_var, " * gap"))
  m <- lm(f, data = d)
  cf <- robust_coef(m)
  inter_term <- paste0(feedback_var, ":gap")
  b1_row <- cf[cf$term == feedback_var, , drop = FALSE]
  b3_row <- cf[cf$term == inter_term, , drop = FALSE]

  preds <- do.call(rbind, lapply(c(-0.20, -0.10, 0, 0.10, 0.20),
                                 predicted_effect_at_gap,
                                 model = m, feedback_var = feedback_var))
  preds$gap_label <- paste0("gap = ", sprintf("%+0.2f", preds$gap))

  rev_gap <- reversal_gap(m, feedback_var)
  rev_init_rate <- if (is.na(rev_gap)) NA else rev_gap + pool_rate

  list(
    summary = data.frame(
      study     = study_label,
      attribute = label,
      pool_rate = pool_rate,
      init_min  = round(min(d$init_rate), 3),
      init_max  = round(max(d$init_rate), 3),
      b1_at_pool_pp = round(100 * b1_row$est, 2),
      b1_se_pp      = round(100 * b1_row$se, 3),
      b1_p          = b1_row$p,
      b3_inter      = round(b3_row$est, 4),
      b3_se         = round(b3_row$se, 4),
      b3_p          = b3_row$p,
      reversal_init_rate = if (is.na(rev_init_rate)) NA else round(rev_init_rate, 3),
      stringsAsFactors = FALSE
    ),
    preds = cbind(study = study_label, attribute = label, preds, stringsAsFactors = FALSE),
    model = m,
    feedback_var = feedback_var,
    is_target = grepl("(TARGET)", label, fixed = TRUE)
  )
}

# =============================================================================
# Load every study (matches de_4b_concern_analysis.R verbatim)
# =============================================================================

# STUDY 2
d2 <- read.csv("Study-2/Study2.csv", check.names = FALSE)
pages_2       <- c('Tina Fey','Keith Richards','Andre Agassi','Henry Winkler','Willie Nelson',
                   'Michael J. Fox','John Stamos')
year_2        <- c('Steve Jobs','J. Robert Oppenheimer','Mark Twain','Anthony Bourdain',
                   'Andrew Jackson','Tina Fey','Johnny Cash','Jackie Kennedy',
                   'Theodore Roosevelt','Che Guevara','Tennessee Williams','Keith Richards',
                   'Napoleon Bonaparte','Andre Agassi','Henry Winkler','Robin Williams',
                   'Willie Nelson','Michael J. Fox','John Stamos')
entertainer_2 <- c('Anthony Bourdain','Tina Fey','Johnny Cash','Jim Carroll','Keith Richards',
                   'Henry Winkler','Robin Williams','Willie Nelson','Michael J. Fox',
                   'John Stamos','Barbra Streisand')
choice_initial_2 <- paste0("choice-", 1:6)
d2 <- add_base(d2, choice_initial_2, pages_2,       "base_pages")
d2 <- add_base(d2, choice_initial_2, year_2,        "base_year")
d2 <- add_base(d2, choice_initial_2, entertainer_2, "base_entertainer")

# STUDY 3A
d3a <- read.csv("Study-3A/Study3A.csv", check.names = FALSE)
budget_3a <- c('Oppenheimer', 'Moneyball', 'Ali', 'Braveheart', 'A Beautiful Mind',
               'The Aviator', "The King's Speech", 'Rocketman', 'The Greatest Showman', 'Walk the Line')
year_3a <- c('A Beautiful Day in the Neighborhood', 'Oppenheimer', 'Salem', 'Moneyball',
             'The Imitation Game', 'Tolkien', 'Jobs', 'J. Edgar', 'Hitchcock', 'LBJ',
             'On The Basis of Sex', 'The Founder', 'Chappaquiddick', 'Rocketman',
             'The Greatest Showman', 'Walk the Line', 'Harriet')
duration_3a <- c('Moneyball', 'Chaplin', 'W. A Life Misunderstood', 'The Aviator', 'J. Edgar',
                 'Oppenheimer', 'Selma', 'The Doors', 'A Beautiful Mind', 'Harriet', 'Nixon',
                 'On The Basis of Sex', 'Jobs', 'Braveheart', 'Ali', '42', 'Patton',
                 'Rocketman', 'Walk the Line')
choice_initial_3a <- paste0("choice-", 1:7)
d3a <- add_base(d3a, choice_initial_3a, budget_3a,   "base_budget")
d3a <- add_base(d3a, choice_initial_3a, year_3a,     "base_year")
d3a <- add_base(d3a, choice_initial_3a, duration_3a, "base_duration")

# STUDY 3B
d3b <- read.csv("Study-3B/Study3B.csv", check.names = FALSE)
budget_3b <- c('Oppenheimer', 'Moneyball', 'JFK', 'Braveheart', 'Lincoln',
               'A Beautiful Mind', 'The Aviator', 'Marie Antoinette')
year_3b <- c('A Beautiful Day in the Neighborhood', 'Oppenheimer', 'Moneyball',
             'The Imitation Game', 'Tolkien', 'Jobs', 'J. Edgar', 'Hitchcock',
             'Lincoln', 'The Darkest Hour', 'Judy', 'The Iron Lady', 'On The Basis of Sex')
poli_3b <- c('JFK', 'Braveheart', 'J. Edgar', 'Nixon', 'Lincoln',
             'W. A Life Misunderstood', 'The Darkest Hour', 'The Iron Lady',
             'On The Basis of Sex', 'Marie Antoinette')
choice_initial_3b <- paste0("choice-", 1:6)
d3b <- add_base(d3b, choice_initial_3b, budget_3b, "base_budget")
d3b <- add_base(d3b, choice_initial_3b, year_3b,   "base_year")
d3b <- add_base(d3b, choice_initial_3b, poli_3b,   "base_poli")

# STUDY 4A
d4a <- read.csv("Study-4A/Study4A.csv", check.names = FALSE)
books_list_4a  <- c("Agatha Christie","Alice Walker","Charles Dickens","Herman Melville",
                    "Isabel Allende","Jack London","John Steinbeck","Joyce Carol Oates",
                    "Jorge Luis Borges","JRR Tolkien","Louisa May Alcott","Lucy Maud",
                    "Michael Crichton","Sandra Cisneros","Toni Morrison")
oldies_list_4a <- c("Agatha Christie","Charles Dickens","Emily Bronte","Ernest Hemingway",
                    "Herman Melville","Jack London","Jane Austen","Jorge Luis Borges",
                    "JRR Tolkien","Louisa May Alcott","Lucy Maud","Nathaniel Hawthorne","WEB Du Bois")
poets_list_4a  <- c("Alice Walker","Charles Dickens","Emily Bronte","George Orwell",
                    "Jack London","James Baldwin","Joyce Carol Oates","Jorge Luis Borges",
                    "Louisa May Alcott","Lucy Maud","Sandra Cisneros","Sylvia Plath","Toni Morrison")
choice_initial_4a <- paste0("selection_", 1:6)
d4a <- add_base(d4a, choice_initial_4a, books_list_4a,  "base_books")
d4a <- add_base(d4a, choice_initial_4a, oldies_list_4a, "base_oldies")
d4a <- add_base(d4a, choice_initial_4a, poets_list_4a,  "base_poets")

# STUDY 4B
d4b <- read.csv("Study-4B/Study4B.csv", check.names = FALSE)
multi_genre   <- c('Herman Melville','Joyce Carol Oates','Cormac McCarthy','Ray Bradbury',
                   'Kurt Vonnegut','George Orwell','Isabel Allende','Gabriel Garcia Marquez',
                   'Lucy Maud','Neil Gaiman','J.R.R. Tolkien')
sold_30m      <- c('Charles Dickens','Gabriel Garcia Marquez','George Orwell','Isabel Allende',
                   'J.D. Salinger','J.R.R. Tolkien','Lucy Maud','F. Scott Fitzgerald')
classic_50plus<- c('Jane Austen','Charles Dickens','Herman Melville','Nathaniel Hawthorne',
                   'Jack London','J.D. Salinger','F. Scott Fitzgerald','Ernest Hemingway',
                   'John Steinbeck','George Orwell','J.R.R. Tolkien','Lucy Maud','Ray Bradbury',
                   'Kurt Vonnegut','Gabriel Garcia Marquez','Michael Crichton','Sylvia Plath')
choice_initial_4b <- paste0("choice-", 1:6)
d4b <- add_base(d4b, choice_initial_4b, multi_genre,    "base_forms")
d4b <- add_base(d4b, choice_initial_4b, sold_30m,       "base_sold30m")
d4b <- add_base(d4b, choice_initial_4b, classic_50plus, "base_classic")

# =============================================================================
# Run the deep-dive on every (study, attribute)
# =============================================================================
fits <- list()
A <- function(...) fits[[length(fits)+1]] <<- fit_one(...)

A(d2, "gender_feedback",   "female_pick",     "base_gender",     5/25, 6,
  "gender feedback -> female pick (TARGET)", "Study 2 (gender)")
A(d2, "pages_shown",       "pages_pick",      "base_pages",     length(pages_2)/25, 6,
  "pages feedback -> pages pick", "Study 2 (gender)")
A(d2, "year_shown",        "year_pick",       "base_year",      length(year_2)/25, 6,
  "year feedback -> year pick", "Study 2 (gender)")
A(d2, "entertainer_shown", "entertainer_pick","base_entertainer", length(entertainer_2)/25, 6,
  "entertainer feedback -> entertainer pick", "Study 2 (gender)")

A(d3a, "race_feedback",  "race_pick",     "base_race",     4/25, 7,
  "race feedback -> race pick (TARGET)", "Study 3A (race)")
A(d3a, "budget_shown",   "budget_pick",   "base_budget",   length(budget_3a)/25, 7,
  "budget feedback -> budget pick", "Study 3A (race)")
A(d3a, "year_shown",     "year_pick",     "base_year",     length(year_3a)/25, 7,
  "year feedback -> year pick", "Study 3A (race)")
A(d3a, "duration_shown", "duration_pick", "base_duration", length(duration_3a)/25, 7,
  "duration feedback -> duration pick", "Study 3A (race)")

A(d3b, "gender_feedback","female_pick", "base_gender",  6/25, 6,
  "gender feedback -> female pick (TARGET)", "Study 3B (gender)")
A(d3b, "budget_shown",   "budget_pick", "base_budget",  length(budget_3b)/25, 6,
  "budget feedback -> budget pick", "Study 3B (gender)")
A(d3b, "year_shown",     "year_pick",   "base_year",    length(year_3b)/25, 6,
  "year feedback -> year pick", "Study 3B (gender)")
A(d3b, "poli_shown",     "poli_pick",   "base_poli",    length(poli_3b)/25, 6,
  "political feedback -> political pick", "Study 3B (gender)")

A(d4a, "race_feedback", "race_pick",   "base_race",   8/25, 6,
  "race feedback -> race pick (TARGET)", "Study 4A (race)")
A(d4a, "books",         "book_pick",   "base_books",  length(books_list_4a)/25, 6,
  "books feedback -> book pick", "Study 4A (race)")
A(d4a, "oldies",        "oldies_pick", "base_oldies", length(oldies_list_4a)/25, 6,
  "oldies feedback -> oldies pick", "Study 4A (race)")
A(d4a, "poets",         "poets_pick",  "base_poets",  length(poets_list_4a)/25, 6,
  "poets feedback -> poets pick", "Study 4A (race)")

A(d4b, "gender_feedback","female_pick",   "base_gender",  6/25, 6,
  "gender feedback -> female pick (TARGET)", "Study 4B (gender)")
A(d4b, "forms_shown",    "forms_pick",    "base_forms",   length(multi_genre)/25, 6,
  "forms feedback -> forms pick", "Study 4B (gender)")
A(d4b, "sold30m_shown",  "sold30m_pick",  "base_sold30m", length(sold_30m)/25, 6,
  "sold30m feedback -> sold30m pick", "Study 4B (gender)")
A(d4b, "classic_shown",  "classic_pick",  "base_classic", length(classic_50plus)/25, 6,
  "classic feedback -> classic pick", "Study 4B (gender)")

summary_df <- do.call(rbind, lapply(fits, function(f) f$summary))
summary_df$is_target <- grepl("(TARGET)", summary_df$attribute, fixed = TRUE)
preds_df <- do.call(rbind, lapply(fits, function(f) f$preds))
preds_df$is_target <- grepl("(TARGET)", preds_df$attribute, fixed = TRUE)

cat("\n=================================================\n")
cat("Per-study Option 3 summary (b1 at pool, b3 interaction, reversal point)\n")
cat("=================================================\n")
print(summary_df, row.names = FALSE)

cat("\n=================================================\n")
cat("Predicted feedback effects at gap = -0.20 / -0.10 / 0 / +0.10 / +0.20 (pp)\n")
cat("=================================================\n")
print(preds_df, row.names = FALSE)

# =============================================================================
# Pool b3 across studies, separately for target vs comparison
# =============================================================================
pool_b3 <- function(df, label) {
  ok <- !is.na(df$b3_inter) & !is.na(df$b3_se) & is.finite(df$b3_se) & df$b3_se > 0
  fit <- rma(yi = df$b3_inter[ok], sei = df$b3_se[ok], method = "REML")
  data.frame(group = label, k = fit$k,
             pooled_b3 = round(as.numeric(fit$b), 4),
             se = round(fit$se, 4),
             ci_lo = round(fit$ci.lb, 4),
             ci_hi = round(fit$ci.ub, 4),
             p = round(fit$pval, 4),
             i2 = round(fit$I2, 1))
}

target_b3 <- pool_b3(summary_df[summary_df$is_target, ], "Target — pooled feedback x gap")
comp_b3   <- pool_b3(summary_df[!summary_df$is_target, ], "Comparison — pooled feedback x gap")

cat("\n=================================================\n")
cat("Pooled Option-3 interactions (random-effects REML)\n")
cat("=================================================\n")
print(rbind(target_b3, comp_b3), row.names = FALSE)

# Contrast: target b3 - comparison b3 (independence assumption between groups)
delta <- target_b3$pooled_b3 - comp_b3$pooled_b3
se_delta <- sqrt(target_b3$se^2 + comp_b3$se^2)
z_delta <- delta / se_delta
p_delta <- 2 * pnorm(-abs(z_delta))
cat(sprintf("\nContrast (target b3 minus comparison b3): %+0.4f  SE %.4f  z = %.2f  p = %.5f\n",
            delta, se_delta, z_delta, p_delta))

# =============================================================================
# Pool predicted feedback effects at each gap value, target vs comparison
# =============================================================================
pool_preds_at_gap <- function(df, label) {
  ok <- !is.na(df$est_pp) & !is.na(df$se_pp) & is.finite(df$se_pp) & df$se_pp > 0
  fit <- rma(yi = df$est_pp[ok], sei = df$se_pp[ok], method = "REML")
  data.frame(group = label, k = fit$k,
             gap = unique(df$gap),
             pooled_pp = round(as.numeric(fit$b), 2),
             se_pp = round(fit$se, 3),
             ci_lo_pp = round(fit$ci.lb, 2),
             ci_hi_pp = round(fit$ci.ub, 2),
             p = round(fit$pval, 4),
             i2 = round(fit$I2, 1))
}

pred_pools <- list()
for (g in unique(preds_df$gap)) {
  sub_t <- preds_df[preds_df$is_target  & preds_df$gap == g, ]
  sub_c <- preds_df[!preds_df$is_target & preds_df$gap == g, ]
  pred_pools[[length(pred_pools)+1]] <- pool_preds_at_gap(sub_t, "Target (gender/race)")
  pred_pools[[length(pred_pools)+1]] <- pool_preds_at_gap(sub_c, "Comparison")
}
pred_pools_df <- do.call(rbind, pred_pools)

cat("\n=================================================\n")
cat("Pooled predicted feedback effect across gap values (pp)\n")
cat("Negative gap means initial rate below pool; positive means above.\n")
cat("=================================================\n")
print(pred_pools_df, row.names = FALSE)

# Persist
write.csv(summary_df,  "revision-analysis/_r1_review/de_option3_summary.csv", row.names = FALSE)
write.csv(preds_df,    "revision-analysis/_r1_review/de_option3_predicted_effects.csv", row.names = FALSE)
write.csv(pred_pools_df, "revision-analysis/_r1_review/de_option3_pooled_predictions.csv", row.names = FALSE)

# =============================================================================
# Plot: pooled predicted feedback effect by gap, target vs comparison
# =============================================================================
plot_df <- pred_pools_df %>%
  mutate(group = factor(group, levels = c("Target (gender/race)", "Comparison")))

p <- ggplot(plot_df, aes(x = gap, y = pooled_pp, color = group, fill = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  geom_ribbon(aes(ymin = ci_lo_pp, ymax = ci_hi_pp), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Target (gender/race)" = "#990000",
                                "Comparison"           = "#011F5B")) +
  scale_fill_manual(values  = c("Target (gender/race)" = "#990000",
                                "Comparison"           = "#011F5B")) +
  labs(x = "Gap (initial selection rate − pool base rate)",
       y = "Predicted feedback effect on final pick (pp)",
       title = "Option 3: feedback effect by distance from pool",
       subtitle = "Pooled across 5 participant-driven studies (REML).\nTarget = gender/race feedback (k=5). Comparison = other binary attributes (k=15).",
       color = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("revision-analysis/_r1_review/de_option3_predicted_effects.pdf",
       p, width = 8, height = 5)
cat("\nSaved:\n")
cat("  revision-analysis/_r1_review/de_option3_summary.csv\n")
cat("  revision-analysis/_r1_review/de_option3_predicted_effects.csv\n")
cat("  revision-analysis/_r1_review/de_option3_pooled_predictions.csv\n")
cat("  revision-analysis/_r1_review/de_option3_predicted_effects.pdf\n")
