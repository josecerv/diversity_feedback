###############################################################################
# Two new participant-level analyses to address Katy's critique of Table R1.
#
# Why we need new analyses:
#   The existing Below-Pool / At-Above-Pool subgroup framing has two problems
#   Katy flagged: (i) the candidate-pool base rate isn't a normative
#   "appropriate" benchmark, just a directional reference, and (ii) bundling
#   "at pool" and "above pool" hides the symmetric prediction that above-pool
#   participants should be pushed DOWN by feedback, not unaffected.
#
# Two replacements that use base rates as a DIRECTIONAL threshold (not a
# normative benchmark):
#
# OPTION 2 — Movement-toward-pool outcome.
#   For each (participant, attribute), classify the participant by the sign
#   of (pool_rate - initial_rate):
#     - Below-pool (pool > initial_rate)  -> "toward pool" means PICK the
#       attribute on the final selection.  We score 1 if picked, else 0.
#     - Above-pool (pool < initial_rate)  -> "toward pool" means DO NOT PICK
#       the attribute on the final selection.  We score 1 if not picked.
#     - Exactly-at-pool (pool == initial_rate, rare)  -> drop.
#
#   Outcome variable: move_toward_pool (binary).
#   Regress: move_toward_pool ~ feedback (HC3 robust SEs).
#   Treatment effect = increase in pool-matching from receiving feedback.
#
# OPTION 3 — Centered-at-pool interaction.
#   Regress: final_pick ~ feedback * (initial_rate - pool_rate)  (HC3).
#     - Main effect of `feedback`: predicted feedback effect at exactly
#       initial_rate == pool_rate (the "no gap" anchor).
#     - Interaction `feedback : (initial_rate - pool_rate)`: how the
#       feedback effect varies with distance from the pool.  A NEGATIVE
#       interaction means below-pool participants get bigger positive
#       effects and above-pool participants get smaller (or negative)
#       effects, consistent with feedback nudging toward the pool.
#
# Both options use the pool only as a directional reference and do not
# require any "appropriate use" claim.
###############################################################################

rm(list = ls())
setwd("C:/Users/jcerv/Jose/diversity_feedback")

suppressPackageStartupMessages({
  library(dplyr)
  library(sandwich)
  library(lmtest)
  library(metafor)
})

# ---------- HC3 robust extraction ----------
robust_row <- function(model, term) {
  vc <- vcovHC(model, type = "HC3")
  ct <- coeftest(model, vcov. = vc)
  ci <- coefci(model, vcov. = vc)
  i  <- which(rownames(ct) == term)
  if (length(i) == 0) return(NULL)
  list(est = unname(ct[i, 1]),
       se  = unname(ct[i, 2]),
       p   = unname(ct[i, 4]),
       ci  = unname(ci[i, ]))
}

add_base <- function(d, choice_cols, item_list, new_name) {
  d[[new_name]] <- rowSums(sapply(choice_cols, function(cc) d[[cc]] %in% item_list))
  d
}

# ---------- Per-attribute runner ----------
run_pool_movement <- function(d, feedback_var, pick_var, base_var,
                              pool_rate, n_initial, label, study_label) {
  d <- d[!is.na(d[[feedback_var]]) & !is.na(d[[pick_var]]) & !is.na(d[[base_var]]), ]

  # Per-participant initial rate and gap
  d$init_rate <- d[[base_var]] / n_initial
  d$gap_from_pool <- d$init_rate - pool_rate
  d$pool_dir <- sign(pool_rate - d$init_rate)  # +1 if below pool, -1 if above, 0 if at

  # ===================== OPTION 2: movement-toward-pool ====================
  # Below-pool (dir == +1):  move = picked
  # Above-pool (dir == -1):  move = 1 - picked
  # At-pool   (dir ==  0):   drop
  d2 <- d[d$pool_dir != 0, ]
  d2$move_toward_pool <- ifelse(d2$pool_dir == 1, d2[[pick_var]], 1 - d2[[pick_var]])

  f_opt2 <- as.formula(paste0("move_toward_pool ~ ", feedback_var))
  m_opt2 <- lm(f_opt2, data = d2)
  opt2 <- robust_row(m_opt2, feedback_var)

  # Diagnostic: how the sample splits
  n_below <- sum(d2$pool_dir == 1)
  n_above <- sum(d2$pool_dir == -1)
  n_at    <- sum(d$pool_dir == 0)

  # ============== OPTION 3: centered-at-pool interaction =================
  d$gap <- d$init_rate - pool_rate  # gap = 0 means at pool
  f_opt3 <- as.formula(paste0(pick_var, " ~ ", feedback_var, " * gap"))
  m_opt3 <- lm(f_opt3, data = d)

  main_at_pool <- robust_row(m_opt3, feedback_var)              # feedback effect at pool
  inter_term   <- paste0(feedback_var, ":gap")
  inter_eff    <- robust_row(m_opt3, inter_term)               # how effect varies with gap

  cat(sprintf("\n--- %s : %s ---\n", study_label, label))
  cat(sprintf("  N=%d (below=%d, above=%d, at=%d)  pool=%.1f%%  init mean=%.1f%%\n",
              nrow(d), n_below, n_above, n_at,
              100*pool_rate, 100*mean(d$init_rate)))
  cat(sprintf("  OPT2  move_toward_pool ~ feedback :  %+0.2f pp  SE %.3f  p=%.4f  CI [%.2f, %.2f]\n",
              100*opt2$est, opt2$se, opt2$p, 100*opt2$ci[1], 100*opt2$ci[2]))
  cat(sprintf("  OPT3  feedback at pool :              %+0.2f pp  SE %.3f  p=%.4f  CI [%.2f, %.2f]\n",
              100*main_at_pool$est, main_at_pool$se, main_at_pool$p,
              100*main_at_pool$ci[1], 100*main_at_pool$ci[2]))
  cat(sprintf("  OPT3  feedback x gap (interaction) :  %+0.4f     SE %.4f  p=%.4f  CI [%.4f, %.4f]\n",
              inter_eff$est, inter_eff$se, inter_eff$p, inter_eff$ci[1], inter_eff$ci[2]))

  data.frame(
    study              = study_label,
    attribute          = label,
    n_total            = nrow(d),
    n_below            = n_below,
    n_above            = n_above,
    n_at               = n_at,
    pool_rate          = pool_rate,
    init_mean          = round(mean(d$init_rate), 4),
    # Option 2
    opt2_est_pp        = round(100 * opt2$est, 2),
    opt2_se_pp         = round(100 * opt2$se, 3),
    opt2_p             = opt2$p,
    opt2_ci_lo_pp      = round(100 * opt2$ci[1], 2),
    opt2_ci_hi_pp      = round(100 * opt2$ci[2], 2),
    # Option 3
    opt3_main_pp       = round(100 * main_at_pool$est, 2),
    opt3_main_se_pp    = round(100 * main_at_pool$se, 3),
    opt3_main_p        = main_at_pool$p,
    opt3_main_ci_lo    = round(100 * main_at_pool$ci[1], 2),
    opt3_main_ci_hi    = round(100 * main_at_pool$ci[2], 2),
    opt3_inter_est     = round(inter_eff$est, 4),
    opt3_inter_se      = round(inter_eff$se, 4),
    opt3_inter_p       = inter_eff$p,
    opt3_inter_ci_lo   = round(inter_eff$ci[1], 4),
    opt3_inter_ci_hi   = round(inter_eff$ci[2], 4),
    is_target          = grepl("(TARGET)", label, fixed = TRUE),
    stringsAsFactors   = FALSE
  )
}

# =============================================================================
# Load every study with the same stimulus lists used in de_4b_concern_analysis.R
# =============================================================================

# ---- STUDY 2 ----
d2 <- read.csv("Study-2/Study2.csv", check.names = FALSE)
women_2       <- c('Anne Frank','Tina Fey','Jackie Kennedy','Helen Keller','Barbra Streisand')
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

# ---- STUDY 3A ----
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

# ---- STUDY 3B ----
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

# ---- STUDY 4A ----
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

# ---- STUDY 4B ----
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
# Run Option 2 + Option 3 for each (study, attribute)
# =============================================================================
rows <- list()

# Study 2
rows[[length(rows)+1]] <- run_pool_movement(d2, "gender_feedback",   "female_pick",     "base_gender",     5/25, 6,
                                            "gender feedback -> female pick (TARGET)", "Study 2 (gender)")
rows[[length(rows)+1]] <- run_pool_movement(d2, "pages_shown",       "pages_pick",      "base_pages",     length(pages_2)/25, 6,
                                            "pages feedback -> pages pick", "Study 2 (gender)")
rows[[length(rows)+1]] <- run_pool_movement(d2, "year_shown",        "year_pick",       "base_year",      length(year_2)/25, 6,
                                            "year feedback -> year pick", "Study 2 (gender)")
rows[[length(rows)+1]] <- run_pool_movement(d2, "entertainer_shown", "entertainer_pick","base_entertainer", length(entertainer_2)/25, 6,
                                            "entertainer feedback -> entertainer pick", "Study 2 (gender)")

# Study 3A
rows[[length(rows)+1]] <- run_pool_movement(d3a, "race_feedback",  "race_pick",     "base_race",     4/25, 7,
                                            "race feedback -> race pick (TARGET)", "Study 3A (race)")
rows[[length(rows)+1]] <- run_pool_movement(d3a, "budget_shown",   "budget_pick",   "base_budget",   length(budget_3a)/25, 7,
                                            "budget feedback -> budget pick", "Study 3A (race)")
rows[[length(rows)+1]] <- run_pool_movement(d3a, "year_shown",     "year_pick",     "base_year",     length(year_3a)/25, 7,
                                            "year feedback -> year pick", "Study 3A (race)")
rows[[length(rows)+1]] <- run_pool_movement(d3a, "duration_shown", "duration_pick", "base_duration", length(duration_3a)/25, 7,
                                            "duration feedback -> duration pick", "Study 3A (race)")

# Study 3B
rows[[length(rows)+1]] <- run_pool_movement(d3b, "gender_feedback",  "female_pick",     "base_gender",   6/25, 6,
                                            "gender feedback -> female pick (TARGET)", "Study 3B (gender)")
rows[[length(rows)+1]] <- run_pool_movement(d3b, "budget_shown", "budget_pick", "base_budget",  length(budget_3b)/25, 6,
                                            "budget feedback -> budget pick", "Study 3B (gender)")
rows[[length(rows)+1]] <- run_pool_movement(d3b, "year_shown",   "year_pick",   "base_year",    length(year_3b)/25, 6,
                                            "year feedback -> year pick", "Study 3B (gender)")
rows[[length(rows)+1]] <- run_pool_movement(d3b, "poli_shown",   "poli_pick",   "base_poli",    length(poli_3b)/25, 6,
                                            "political feedback -> political pick", "Study 3B (gender)")

# Study 4A
rows[[length(rows)+1]] <- run_pool_movement(d4a, "race_feedback", "race_pick",   "base_race",   8/25, 6,
                                            "race feedback -> race pick (TARGET)", "Study 4A (race)")
rows[[length(rows)+1]] <- run_pool_movement(d4a, "books",         "book_pick",   "base_books",  length(books_list_4a)/25, 6,
                                            "books feedback -> book pick", "Study 4A (race)")
rows[[length(rows)+1]] <- run_pool_movement(d4a, "oldies",        "oldies_pick", "base_oldies", length(oldies_list_4a)/25, 6,
                                            "oldies feedback -> oldies pick", "Study 4A (race)")
rows[[length(rows)+1]] <- run_pool_movement(d4a, "poets",         "poets_pick",  "base_poets",  length(poets_list_4a)/25, 6,
                                            "poets feedback -> poets pick", "Study 4A (race)")

# Study 4B
rows[[length(rows)+1]] <- run_pool_movement(d4b, "gender_feedback", "female_pick",   "base_gender",  6/25, 6,
                                            "gender feedback -> female pick (TARGET)", "Study 4B (gender)")
rows[[length(rows)+1]] <- run_pool_movement(d4b, "forms_shown",     "forms_pick",    "base_forms",   length(multi_genre)/25, 6,
                                            "forms feedback -> forms pick", "Study 4B (gender)")
rows[[length(rows)+1]] <- run_pool_movement(d4b, "sold30m_shown",   "sold30m_pick",  "base_sold30m", length(sold_30m)/25, 6,
                                            "sold30m feedback -> sold30m pick", "Study 4B (gender)")
rows[[length(rows)+1]] <- run_pool_movement(d4b, "classic_shown",   "classic_pick",  "base_classic", length(classic_50plus)/25, 6,
                                            "classic feedback -> classic pick", "Study 4B (gender)")

all_res <- do.call(rbind, rows)

cat("\n\n===========================================================\n")
cat("Pool-movement analysis: COMBINED RESULTS\n")
cat("===========================================================\n")
print(all_res, row.names = FALSE)

# Persist
write.csv(all_res, "revision-analysis/_r1_review/de_pool_movement_results.csv", row.names = FALSE)

# =============================================================================
# Pool across studies via random-effects meta-analysis (REML), separately for
# target attributes and comparison attributes, for both Option 2 and Option 3
# (interaction-coefficient pool).
# =============================================================================
pool_one <- function(est, se, label) {
  ok <- !is.na(est) & !is.na(se) & is.finite(se) & se > 0
  est_ok <- est[ok]; se_ok <- se[ok]
  if (length(est_ok) < 2) {
    return(data.frame(group=label, k=length(est_ok), pooled=NA, se=NA, ci_lo=NA, ci_hi=NA, p=NA, i2=NA))
  }
  fit <- rma(yi = est_ok, sei = se_ok, method = "REML")
  data.frame(group=label, k=fit$k,
             pooled=round(as.numeric(fit$b), 4),
             se=round(fit$se, 4),
             ci_lo=round(fit$ci.lb, 4),
             ci_hi=round(fit$ci.ub, 4),
             p=round(fit$pval, 4),
             i2=round(fit$I2, 1))
}

target_rows     <- all_res[all_res$is_target, ]
comparison_rows <- all_res[!all_res$is_target, ]

cat("\n\n===========================================================\n")
cat("OPTION 2  (movement-toward-pool, percentage-point pp)\n")
cat("===========================================================\n")
opt2_pools <- rbind(
  pool_one(target_rows$opt2_est_pp,     target_rows$opt2_se_pp,     "Target — movement toward pool"),
  pool_one(comparison_rows$opt2_est_pp, comparison_rows$opt2_se_pp, "Comparison — movement toward pool")
)
print(opt2_pools, row.names = FALSE)

cat("\n\n===========================================================\n")
cat("OPTION 3  (interaction coefficient: feedback x gap)\n")
cat("Interaction is on the proportion scale (gap = init_rate - pool_rate)\n")
cat("===========================================================\n")
opt3_inter_pools <- rbind(
  pool_one(target_rows$opt3_inter_est,     target_rows$opt3_inter_se,     "Target — interaction (feedback x gap)"),
  pool_one(comparison_rows$opt3_inter_est, comparison_rows$opt3_inter_se, "Comparison — interaction (feedback x gap)")
)
print(opt3_inter_pools, row.names = FALSE)

cat("\n\n===========================================================\n")
cat("OPTION 3  (feedback effect AT the pool rate, pp)\n")
cat("This is the predicted feedback effect when initial rate == pool.\n")
cat("===========================================================\n")
opt3_main_pools <- rbind(
  pool_one(target_rows$opt3_main_pp,     target_rows$opt3_main_se_pp,     "Target — feedback effect at pool"),
  pool_one(comparison_rows$opt3_main_pp, comparison_rows$opt3_main_se_pp, "Comparison — feedback effect at pool")
)
print(opt3_main_pools, row.names = FALSE)

write.csv(rbind(
  cbind(option = "Option 2 (move-toward-pool, pp)",            opt2_pools),
  cbind(option = "Option 3 main (effect at pool, pp)",          opt3_main_pools),
  cbind(option = "Option 3 interaction (feedback x gap)",       opt3_inter_pools)
), "revision-analysis/_r1_review/de_pool_movement_pools.csv", row.names = FALSE)

cat("\nSaved:\n")
cat("  revision-analysis/_r1_review/de_pool_movement_results.csv\n")
cat("  revision-analysis/_r1_review/de_pool_movement_pools.csv\n")
