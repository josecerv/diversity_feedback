###############################################################################
# DE base-rate concern: Below-Pool / At-Above-Pool subgroup analysis
#
# The DE's worry: the gender/race feedback effect is larger than the
# comparison-attribute feedback effect NOT because gender/race is special,
# but because participants already use the comparison attributes
# appropriately in their initial selections (so feedback is redundant / has
# no room to move them).
#
# Test: for each attribute in each study, estimate the feedback effect
# separately within Below-Pool (initial proportion < pool rate) and
# At/Above-Pool (initial proportion >= pool rate) subgroups.
#
# If target effects survive in At/Above-Pool (no underrepresentation gap to
# close) AND comparison effects fail in Below-Pool (a gap exists, but
# feedback fails to close it), the DE's alternative loses its bite.
#
# Studies used: 2, 3A, 3B, 4A, 4B (all five generate participant-driven
# initial selections). Study 1 is excluded because initial counts there
# are stimulus-sampled (not participant behavior); the Study 1 analog is
# the feedback x stimulus-set interaction, computed separately.
###############################################################################

rm(list = ls())
setwd("C:/Users/jcerv/Jose/diversity_feedback")

suppressPackageStartupMessages({
  library(dplyr)
  library(sandwich)
  library(lmtest)
})

# ---------- robust SEs helper (same as base-rate-analysis) ----------
robust_tab <- function(model) {
  vc <- vcovHC(model, type = "HC3")
  ct <- coeftest(model, vcov. = vc)
  ci <- coefci(model, vcov. = vc)
  out <- data.frame(
    term   = rownames(ct),
    est    = round(ct[, 1], 4),
    se     = round(ct[, 2], 4),
    t      = round(ct[, 3], 2),
    p      = round(ct[, 4], 4),
    ci_lo  = round(ci[, 1], 4),
    ci_hi  = round(ci[, 2], 4),
    row.names = NULL, stringsAsFactors = FALSE
  )
  out
}

# ---------- reusable runner ----------
# d:               study data frame
# feedback_var:    name of the treatment indicator for this attribute
# pick_var:        name of the binary final-pick indicator for this attribute
# base_var:        name of the initial-count variable for this attribute
# pool_rate:       numeric, the attribute's base rate in the candidate pool
# n_initial:       number of picks in initial portfolio (to express base_var as a proportion)
# label:           pretty label
run_one <- function(d, feedback_var, pick_var, base_var, pool_rate, n_initial, label) {
  d <- d[!is.na(d[[feedback_var]]) & !is.na(d[[pick_var]]) & !is.na(d[[base_var]]), ]

  # Main effect (unconditional)
  f1 <- as.formula(paste0(pick_var, " ~ ", feedback_var))
  m1 <- lm(f1, data = d)
  main <- robust_tab(m1)
  main_eff <- main[main$term == feedback_var, , drop = FALSE]

  # Interaction with initial count (centered so the main effect stays interpretable
  # at the average initial count)
  d$base_c <- d[[base_var]] - mean(d[[base_var]])
  f2 <- as.formula(paste0(pick_var, " ~ ", feedback_var, " * base_c"))
  m2 <- lm(f2, data = d)
  inter <- robust_tab(m2)

  # Subset: participants whose initial proportion was BELOW the pool base rate
  # (i.e., room to move upward). Interpretable test of the DE's ceiling story.
  below <- d[(d[[base_var]] / n_initial) < pool_rate, ]
  if (nrow(below) >= 20) {
    m3 <- lm(f1, data = below)
    below_tab <- robust_tab(m3)
    below_eff <- below_tab[below_tab$term == feedback_var, , drop = FALSE]
  } else {
    below_eff <- data.frame(term = feedback_var, est = NA, se = NA, t = NA,
                            p = NA, ci_lo = NA, ci_hi = NA)
  }

  # Subset: participants whose initial proportion was AT OR ABOVE the pool rate
  above <- d[(d[[base_var]] / n_initial) >= pool_rate, ]
  if (nrow(above) >= 20) {
    m4 <- lm(f1, data = above)
    above_tab <- robust_tab(m4)
    above_eff <- above_tab[above_tab$term == feedback_var, , drop = FALSE]
  } else {
    above_eff <- data.frame(term = feedback_var, est = NA, se = NA, t = NA,
                            p = NA, ci_lo = NA, ci_hi = NA)
  }

  cat("\n--------------------------------------------------------------\n")
  cat(label, "\n")
  cat("--------------------------------------------------------------\n")
  cat(sprintf("  N total: %d | pool rate: %.1f%% | initial proportion mean: %.1f%% (SD %.1f)\n",
              nrow(d), 100*pool_rate,
              100 * mean(d[[base_var]] / n_initial),
              100 * sd(d[[base_var]] / n_initial)))
  cat(sprintf("  Main effect of feedback:      %+0.2f pp (p=%.3f)\n",
              100 * main_eff$est, main_eff$p))
  cat(sprintf("  Below pool rate (N=%4d):     %+0.2f pp (p=%.3f)\n",
              nrow(below), 100 * below_eff$est, below_eff$p))
  cat(sprintf("  At/above pool rate (N=%4d): %+0.2f pp (p=%.3f)\n",
              nrow(above), 100 * above_eff$est, above_eff$p))

  interaction_row <- inter[grepl(":", inter$term), , drop = FALSE]
  if (nrow(interaction_row) > 0) {
    cat(sprintf("  Interaction feedback x initial_count (centered): est=%+0.3f, p=%.3f\n",
                interaction_row$est[1], interaction_row$p[1]))
  }

  out <- data.frame(
    study            = NA_character_,
    attribute        = label,
    n_total          = nrow(d),
    pool_rate        = pool_rate,
    initial_mean_pct = round(100 * mean(d[[base_var]] / n_initial), 2),
    main_eff_pp      = round(100 * main_eff$est, 2),
    main_p           = main_eff$p,
    below_n          = nrow(below),
    below_eff_pp     = round(100 * below_eff$est, 2),
    below_p          = below_eff$p,
    above_n          = nrow(above),
    above_eff_pp     = round(100 * above_eff$est, 2),
    above_p          = above_eff$p,
    inter_est        = if (nrow(interaction_row) > 0) round(interaction_row$est[1], 4) else NA,
    inter_p          = if (nrow(interaction_row) > 0) interaction_row$p[1] else NA,
    stringsAsFactors = FALSE
  )
  out
}

add_base <- function(d, choice_cols, item_list, new_name) {
  d[[new_name]] <- rowSums(sapply(choice_cols, function(cc) d[[cc]] %in% item_list))
  d
}

# =============================================================================
# STUDY 2 (gender — biographies, field study)
# =============================================================================
cat("\n\n==============================================================\n")
cat("STUDY 2  (gender, biographies, 25 candidates, 5 women = 20%)\n")
cat("==============================================================\n")
d2 <- read.csv("Study-2/Study2.csv", check.names = FALSE)

# Stimulus lists from Study-2/Study-2.Rmd
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

pool_rate_pages_2       <- length(pages_2)       / 25
pool_rate_year_2        <- length(year_2)        / 25
pool_rate_entertainer_2 <- length(entertainer_2) / 25
cat(sprintf("Pool rates: pages=%.1f%%, year=%.1f%%, entertainer=%.1f%%\n",
            100*pool_rate_pages_2, 100*pool_rate_year_2, 100*pool_rate_entertainer_2))

rows <- list()
rows[[length(rows)+1]] <- run_one(d2, "gender_feedback",   "female_pick",      "base_gender",
                                  5/25, 6, "2: gender feedback -> female pick (TARGET)")
rows[[length(rows)+1]] <- run_one(d2, "pages_shown",       "pages_pick",       "base_pages",
                                  pool_rate_pages_2, 6, "2: pages feedback -> pages pick")
rows[[length(rows)+1]] <- run_one(d2, "year_shown",        "year_pick",        "base_year",
                                  pool_rate_year_2, 6, "2: year feedback -> year pick")
rows[[length(rows)+1]] <- run_one(d2, "entertainer_shown", "entertainer_pick", "base_entertainer",
                                  pool_rate_entertainer_2, 6, "2: entertainer feedback -> entertainer pick")
res2 <- do.call(rbind, rows)
res2$study <- "Study 2 (gender)"

# =============================================================================
# STUDY 3A (race — films)
# =============================================================================
cat("\n\n==============================================================\n")
cat("STUDY 3A  (race, films, 25 films, 4 URM protagonists = 16%)\n")
cat("==============================================================\n")
d3a <- read.csv("Study-3A/Study3A.csv", check.names = FALSE)

race_list_3a <- c('Salem','Harriet', 'Ali', '42')
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

# Pool size is 25, initial portfolio 7, pool_rate for each attribute:
pool_rate_budget_3a   <- length(budget_3a)   / 25
pool_rate_year_3a     <- length(year_3a)     / 25
pool_rate_duration_3a <- length(duration_3a) / 25
cat(sprintf("Pool rates: budget=%.1f%%, year=%.1f%%, duration=%.1f%%\n",
            100*pool_rate_budget_3a, 100*pool_rate_year_3a, 100*pool_rate_duration_3a))

rows <- list()
rows[[length(rows)+1]] <- run_one(d3a, "race_feedback",     "race_pick",     "base_race",
                                  4/25, 7, "3A: race feedback -> race pick (TARGET)")
rows[[length(rows)+1]] <- run_one(d3a, "budget_shown",      "budget_pick",   "base_budget",
                                  pool_rate_budget_3a,   7, "3A: budget feedback -> budget pick")
rows[[length(rows)+1]] <- run_one(d3a, "year_shown",        "year_pick",     "base_year",
                                  pool_rate_year_3a,     7, "3A: year feedback -> year pick")
rows[[length(rows)+1]] <- run_one(d3a, "duration_shown",    "duration_pick", "base_duration",
                                  pool_rate_duration_3a, 7, "3A: duration feedback -> duration pick")
res3a <- do.call(rbind, rows)
res3a$study <- "Study 3A (race)"

# =============================================================================
# STUDY 3B (gender — films)
# =============================================================================
cat("\n\n==============================================================\n")
cat("STUDY 3B  (gender, films, 25 films, 6 women = 24%)\n")
cat("==============================================================\n")
d3b <- read.csv("Study-3B/Study3B.csv", check.names = FALSE)

women_3b <- c('On The Basis of Sex', 'Marie Antoinette', 'The Iron Lady', 'Judy',
              'Coco Before Chanel', 'Rooney')
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

pool_rate_budget_3b <- length(budget_3b) / 25
pool_rate_year_3b   <- length(year_3b)   / 25
pool_rate_poli_3b   <- length(poli_3b)   / 25
cat(sprintf("Pool rates: budget=%.1f%%, year=%.1f%%, political=%.1f%%\n",
            100*pool_rate_budget_3b, 100*pool_rate_year_3b, 100*pool_rate_poli_3b))

rows <- list()
rows[[length(rows)+1]] <- run_one(d3b, "gender_feedback", "female_pick", "base_gender",
                                  6/25, 6, "3B: gender feedback -> female pick (TARGET)")
rows[[length(rows)+1]] <- run_one(d3b, "budget_shown",    "budget_pick", "base_budget",
                                  pool_rate_budget_3b, 6, "3B: budget feedback -> budget pick")
rows[[length(rows)+1]] <- run_one(d3b, "year_shown",      "year_pick",   "base_year",
                                  pool_rate_year_3b,   6, "3B: year feedback -> year pick")
rows[[length(rows)+1]] <- run_one(d3b, "poli_shown",      "poli_pick",   "base_poli",
                                  pool_rate_poli_3b,   6, "3B: political feedback -> political pick")
res3b <- do.call(rbind, rows)
res3b$study <- "Study 3B (gender)"

# =============================================================================
# STUDY 4A (race — authors)
# =============================================================================
cat("\n\n==============================================================\n")
cat("STUDY 4A  (race, authors, 25 authors, 8 URM = 32%)\n")
cat("==============================================================\n")
d4a <- read.csv("Study-4A/Study4A.csv", check.names = FALSE)

race_list_4a   <- c("Alice Walker","Gabriel Garcia Marquez","Isabel Allende","James Baldwin",
                    "Sandra Cisneros","Toni Morrison","WEB Du Bois","Jorge Luis Borges")
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

# In Study 4A the feedback indicators for comparison attributes are `books`, `oldies`, `poets`
pool_rate_books_4a  <- length(books_list_4a)  / 25
pool_rate_oldies_4a <- length(oldies_list_4a) / 25
pool_rate_poets_4a  <- length(poets_list_4a)  / 25
cat(sprintf("Pool rates: books=%.1f%%, oldies=%.1f%%, poets=%.1f%%\n",
            100*pool_rate_books_4a, 100*pool_rate_oldies_4a, 100*pool_rate_poets_4a))

rows <- list()
rows[[length(rows)+1]] <- run_one(d4a, "race_feedback", "race_pick",   "base_race",
                                  8/25, 6, "4A: race feedback -> race pick (TARGET)")
rows[[length(rows)+1]] <- run_one(d4a, "books",         "book_pick",   "base_books",
                                  pool_rate_books_4a,  6, "4A: books feedback -> book pick")
rows[[length(rows)+1]] <- run_one(d4a, "oldies",        "oldies_pick", "base_oldies",
                                  pool_rate_oldies_4a, 6, "4A: oldies feedback -> oldies pick")
rows[[length(rows)+1]] <- run_one(d4a, "poets",         "poets_pick",  "base_poets",
                                  pool_rate_poets_4a,  6, "4A: poets feedback -> poets pick")
res4a <- do.call(rbind, rows)
res4a$study <- "Study 4A (race)"

# =============================================================================
# STUDY 4B (gender — authors)
# =============================================================================
cat("\n\n==============================================================\n")
cat("STUDY 4B  (gender, authors, 25 authors, 6 women = 24%)\n")
cat("==============================================================\n")
d4b <- read.csv("Study-4B/Study4B.csv", check.names = FALSE)

women_4b      <- c('Zadie Smith','Isabel Allende','Jane Austen','Joyce Carol Oates','Lucy Maud','Sylvia Plath')
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

pool_rate_forms_4b   <- length(multi_genre)    / 25
pool_rate_sold30m_4b <- length(sold_30m)       / 25
pool_rate_classic_4b <- length(classic_50plus) / 25
cat(sprintf("Pool rates: forms=%.1f%%, sold30m=%.1f%%, classic=%.1f%%\n",
            100*pool_rate_forms_4b, 100*pool_rate_sold30m_4b, 100*pool_rate_classic_4b))

rows <- list()
rows[[length(rows)+1]] <- run_one(d4b, "gender_feedback", "female_pick",  "base_gender",
                                  6/25, 6, "4B: gender feedback -> female pick (TARGET)")
rows[[length(rows)+1]] <- run_one(d4b, "forms_shown",     "forms_pick",   "base_forms",
                                  pool_rate_forms_4b,   6, "4B: forms feedback -> forms pick")
rows[[length(rows)+1]] <- run_one(d4b, "sold30m_shown",   "sold30m_pick", "base_sold30m",
                                  pool_rate_sold30m_4b, 6, "4B: sold30m feedback -> sold30m pick")
rows[[length(rows)+1]] <- run_one(d4b, "classic_shown",   "classic_pick", "base_classic",
                                  pool_rate_classic_4b, 6, "4B: classic feedback -> classic pick")
res4b <- do.call(rbind, rows)
res4b$study <- "Study 4B (gender)"

# =============================================================================
# COMBINE & SAVE
# =============================================================================
all_res <- rbind(res2, res3a, res3b, res4a, res4b)
all_res <- all_res[, c("study","attribute","n_total","pool_rate","initial_mean_pct",
                       "main_eff_pp","main_p","below_n","below_eff_pp","below_p",
                       "above_n","above_eff_pp","above_p","inter_est","inter_p")]

cat("\n\n==============================================================\n")
cat("COMBINED SUMMARY\n")
cat("==============================================================\n")
print(all_res, row.names = FALSE)

write.csv(all_res, "revision-analysis/de_4b_concern_results.csv", row.names = FALSE)

cat("\n\nSaved: revision-analysis/de_4b_concern_results.csv\n")

# =============================================================================
# STRICT robustness check on the At/Above-Pool definition
#
# The lenient definition is: initial proportion >= pool rate (i.e.,
# c / n_initial >= p). This admits participants who could fall BELOW the
# pool rate after a non-target final choice (because total selections
# becomes n_initial + 1).
#
# The strict definition is: c / (n_initial + 1) >= p, i.e., the participant
# would remain at-or-above the pool rate EVEN IF the final choice is
# non-target. For Studies 2, 3A, 3B, 4B this turns out to be identical to
# the lenient definition (because the integer count threshold rounds the
# same way). Only Study 4A differs (lenient: c >= 2; strict: c >= 3).
# We rerun Study 4A target's At/Above-Pool effect with the strict subset.
# =============================================================================
cat("\n\n==============================================================\n")
cat("STRICT ROBUSTNESS — At/Above-Pool with c/(n_initial+1) >= p\n")
cat("==============================================================\n")

check_strict <- function(d, n_initial, pool_rate, base_var, label) {
  lenient_thresh <- ceiling(pool_rate * n_initial)
  strict_thresh  <- ceiling(pool_rate * (n_initial + 1))
  cat(sprintf("  %-35s pool=%.0f%%  n0=%d  lenient c>=%d  strict c>=%d  identical=%s\n",
              label, 100*pool_rate, n_initial, lenient_thresh, strict_thresh,
              ifelse(lenient_thresh == strict_thresh, "YES", "NO")))
}

cat("\nThresholds (target attributes only):\n")
check_strict(d2,  6, 5/25, "base_gender", "Study 2 women")
check_strict(d3a, 7, 4/25, "base_race",   "Study 3A race")
check_strict(d3b, 6, 6/25, "base_gender", "Study 3B gender")
check_strict(d4a, 6, 8/25, "base_race",   "Study 4A race")
check_strict(d4b, 6, 6/25, "base_gender", "Study 4B gender")

# Study 4A target with strict subset
cat("\n\nStudy 4A race feedback -> race pick, STRICT At/Above (c >= 3):\n")
d4a_strict <- d4a[d4a$base_race >= 3, ]
m4a_strict <- lm(race_pick ~ race_feedback, data = d4a_strict)
strict_tab <- robust_tab(m4a_strict)
strict_eff <- strict_tab[strict_tab$term == "race_feedback", ]
cat(sprintf("  N (strict) = %d (was %d in lenient At/Above)\n",
            nrow(d4a_strict),
            sum((d4a$base_race / 6) >= (8/25), na.rm = TRUE)))
cat(sprintf("  Effect: %+0.2f pp  (SE %.3f, p = %.4f, 95%% CI [%.2f, %.2f])\n",
            100 * strict_eff$est, strict_eff$se, strict_eff$p,
            100 * strict_eff$ci_lo, 100 * strict_eff$ci_hi))

# Save the strict result so build_table_r1.py can pick it up via footnote
strict_out <- data.frame(
  study      = "Study 4A (race)",
  attribute  = "4A: race feedback -> race pick (STRICT At/Above)",
  n_strict   = nrow(d4a_strict),
  eff_pp     = round(100 * strict_eff$est, 2),
  se         = round(strict_eff$se, 4),
  p          = strict_eff$p,
  ci_lo_pp   = round(100 * strict_eff$ci_lo, 2),
  ci_hi_pp   = round(100 * strict_eff$ci_hi, 2),
  stringsAsFactors = FALSE
)
write.csv(strict_out, "revision-analysis/de_4a_strict_robustness.csv", row.names = FALSE)
cat("\nSaved: revision-analysis/de_4a_strict_robustness.csv\n")
