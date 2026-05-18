###############################################################################
# Zero-benchmark version of the Table R1 subgroup analysis.
#
# WHY: Our theory says race/gender feedback has a *normative* benchmark (people
# carry a target above zero), but comparison attributes (multi-genre, classic,
# 30M+ copies, etc.) do not. So zero is the cleanest cutoff: a participant
# whose initial selections contained ZERO of attribute X has maximum headroom
# to be moved by attribute-X feedback. If even the zero-initial subgroup
# refuses to update on a comparison attribute -- but updates on race/gender --
# the effect cannot be attributed to "appropriate use" of base rates.
#
# Replaces "Below-Pool" vs "At/Above-Pool" with "Initial = 0" vs "Initial >= 1".
#
# For each (study, attribute), reports:
#   - Overall feedback effect on final pick
#   - Effect among participants with base_count == 0
#   - Effect among participants with base_count >= 1
#
# Pools across cells two ways:
#   (a) Random-effects meta-analysis (REML), same as original Table R1 row
#   (b) Long-format fixed-effects regression with cell FEs and participant-
#       clustered SEs -- "fancy way with statistics" the user asked about.
###############################################################################

rm(list = ls())
setwd("C:/Users/jcerv/Jose/diversity_feedback")

suppressPackageStartupMessages({
  library(dplyr)
  library(sandwich)
  library(lmtest)
  library(metafor)
})

# ---------- HC3 robust extraction --------------------------------------------
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

# ---------- Per-cell runner --------------------------------------------------
run_zero_cell <- function(d, feedback_var, pick_var, base_var,
                          pool_rate, n_initial, label, study_label,
                          is_target = FALSE) {
  d <- d[!is.na(d[[feedback_var]]) & !is.na(d[[pick_var]]) & !is.na(d[[base_var]]), ]

  d$zero_initial <- as.integer(d[[base_var]] == 0)

  n_total <- nrow(d)
  n_zero  <- sum(d$zero_initial == 1)
  n_pos   <- sum(d$zero_initial == 0)

  # Overall (full sample) effect
  f_all <- as.formula(paste0(pick_var, " ~ ", feedback_var))
  m_all <- lm(f_all, data = d)
  r_all <- robust_row(m_all, feedback_var)

  # Zero-subgroup effect
  d_zero <- d[d$zero_initial == 1, ]
  if (nrow(d_zero) >= 20 && length(unique(d_zero[[feedback_var]])) == 2) {
    m_zero <- lm(f_all, data = d_zero)
    r_zero <- robust_row(m_zero, feedback_var)
  } else {
    r_zero <- list(est = NA, se = NA, p = NA, ci = c(NA, NA))
  }

  # Positive-subgroup effect
  d_pos <- d[d$zero_initial == 0, ]
  if (nrow(d_pos) >= 20 && length(unique(d_pos[[feedback_var]])) == 2) {
    m_pos <- lm(f_all, data = d_pos)
    r_pos <- robust_row(m_pos, feedback_var)
  } else {
    r_pos <- list(est = NA, se = NA, p = NA, ci = c(NA, NA))
  }

  cat(sprintf("\n--- %s : %s ---\n", study_label, label))
  cat(sprintf("  N=%d (initial=0: %d, initial>=1: %d)  pool=%.1f%%\n",
              n_total, n_zero, n_pos, 100*pool_rate))
  cat(sprintf("  Overall  feedback -> pick :         %+0.2f pp  SE %.3f  p=%.4f  CI [%.2f, %.2f]\n",
              100*r_all$est, 100*r_all$se, r_all$p, 100*r_all$ci[1], 100*r_all$ci[2]))
  if (!is.na(r_zero$est)) {
    cat(sprintf("  Zero-init (base==0) :              %+0.2f pp  SE %.3f  p=%.4f  CI [%.2f, %.2f]\n",
                100*r_zero$est, 100*r_zero$se, r_zero$p, 100*r_zero$ci[1], 100*r_zero$ci[2]))
  } else {
    cat(sprintf("  Zero-init (base==0) :              <insufficient cell size: n=%d>\n", n_zero))
  }
  if (!is.na(r_pos$est)) {
    cat(sprintf("  Positive-init (base>=1) :          %+0.2f pp  SE %.3f  p=%.4f  CI [%.2f, %.2f]\n",
                100*r_pos$est, 100*r_pos$se, r_pos$p, 100*r_pos$ci[1], 100*r_pos$ci[2]))
  } else {
    cat(sprintf("  Positive-init (base>=1) :          <insufficient cell size: n=%d>\n", n_pos))
  }

  data.frame(
    study             = study_label,
    attribute         = label,
    is_target         = is_target,
    n_total           = n_total,
    n_zero            = n_zero,
    n_pos             = n_pos,
    pool_rate         = pool_rate,
    pct_zero          = round(100 * n_zero / n_total, 1),
    # Overall
    all_est_pp        = round(100 * r_all$est, 2),
    all_se_pp         = round(100 * r_all$se, 3),
    all_p             = r_all$p,
    all_ci_lo_pp      = round(100 * r_all$ci[1], 2),
    all_ci_hi_pp      = round(100 * r_all$ci[2], 2),
    # Zero subgroup
    zero_est_pp       = ifelse(is.na(r_zero$est), NA, round(100 * r_zero$est, 2)),
    zero_se_pp        = ifelse(is.na(r_zero$se),  NA, round(100 * r_zero$se,  3)),
    zero_p            = r_zero$p,
    zero_ci_lo_pp     = ifelse(is.na(r_zero$ci[1]), NA, round(100 * r_zero$ci[1], 2)),
    zero_ci_hi_pp     = ifelse(is.na(r_zero$ci[2]), NA, round(100 * r_zero$ci[2], 2)),
    # Positive subgroup
    pos_est_pp        = ifelse(is.na(r_pos$est), NA, round(100 * r_pos$est, 2)),
    pos_se_pp         = ifelse(is.na(r_pos$se),  NA, round(100 * r_pos$se,  3)),
    pos_p             = r_pos$p,
    pos_ci_lo_pp      = ifelse(is.na(r_pos$ci[1]), NA, round(100 * r_pos$ci[1], 2)),
    pos_ci_hi_pp      = ifelse(is.na(r_pos$ci[2]), NA, round(100 * r_pos$ci[2], 2)),
    stringsAsFactors  = FALSE
  )
}

# =============================================================================
# Load all studies (stimulus lists copied from de_pool_movement_analysis.R)
# =============================================================================

# ---- STUDY 2 ----------------------------------------------------------------
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

# ---- STUDY 3A ---------------------------------------------------------------
d3a <- read.csv("Study-3A/Study3A.csv", check.names = FALSE)
budget_3a   <- c('Oppenheimer','Moneyball','Ali','Braveheart','A Beautiful Mind',
                 'The Aviator',"The King's Speech",'Rocketman','The Greatest Showman','Walk the Line')
year_3a     <- c('A Beautiful Day in the Neighborhood','Oppenheimer','Salem','Moneyball',
                 'The Imitation Game','Tolkien','Jobs','J. Edgar','Hitchcock','LBJ',
                 'On The Basis of Sex','The Founder','Chappaquiddick','Rocketman',
                 'The Greatest Showman','Walk the Line','Harriet')
duration_3a <- c('Moneyball','Chaplin','W. A Life Misunderstood','The Aviator','J. Edgar',
                 'Oppenheimer','Selma','The Doors','A Beautiful Mind','Harriet','Nixon',
                 'On The Basis of Sex','Jobs','Braveheart','Ali','42','Patton',
                 'Rocketman','Walk the Line')
choice_initial_3a <- paste0("choice-", 1:7)
d3a <- add_base(d3a, choice_initial_3a, budget_3a,   "base_budget")
d3a <- add_base(d3a, choice_initial_3a, year_3a,     "base_year")
d3a <- add_base(d3a, choice_initial_3a, duration_3a, "base_duration")

# ---- STUDY 3B ---------------------------------------------------------------
d3b <- read.csv("Study-3B/Study3B.csv", check.names = FALSE)
budget_3b <- c('Oppenheimer','Moneyball','JFK','Braveheart','Lincoln',
               'A Beautiful Mind','The Aviator','Marie Antoinette')
year_3b   <- c('A Beautiful Day in the Neighborhood','Oppenheimer','Moneyball',
               'The Imitation Game','Tolkien','Jobs','J. Edgar','Hitchcock',
               'Lincoln','The Darkest Hour','Judy','The Iron Lady','On The Basis of Sex')
poli_3b   <- c('JFK','Braveheart','J. Edgar','Nixon','Lincoln',
               'W. A Life Misunderstood','The Darkest Hour','The Iron Lady',
               'On The Basis of Sex','Marie Antoinette')
choice_initial_3b <- paste0("choice-", 1:6)
d3b <- add_base(d3b, choice_initial_3b, budget_3b, "base_budget")
d3b <- add_base(d3b, choice_initial_3b, year_3b,   "base_year")
d3b <- add_base(d3b, choice_initial_3b, poli_3b,   "base_poli")

# ---- STUDY 4A ---------------------------------------------------------------
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

# ---- STUDY 4B ---------------------------------------------------------------
d4b <- read.csv("Study-4B/Study4B.csv", check.names = FALSE)
multi_genre    <- c('Herman Melville','Joyce Carol Oates','Cormac McCarthy','Ray Bradbury',
                    'Kurt Vonnegut','George Orwell','Isabel Allende','Gabriel Garcia Marquez',
                    'Lucy Maud','Neil Gaiman','J.R.R. Tolkien')
sold_30m       <- c('Charles Dickens','Gabriel Garcia Marquez','George Orwell','Isabel Allende',
                    'J.D. Salinger','J.R.R. Tolkien','Lucy Maud','F. Scott Fitzgerald')
classic_50plus <- c('Jane Austen','Charles Dickens','Herman Melville','Nathaniel Hawthorne',
                    'Jack London','J.D. Salinger','F. Scott Fitzgerald','Ernest Hemingway',
                    'John Steinbeck','George Orwell','J.R.R. Tolkien','Lucy Maud','Ray Bradbury',
                    'Kurt Vonnegut','Gabriel Garcia Marquez','Michael Crichton','Sylvia Plath')
choice_initial_4b <- paste0("choice-", 1:6)
d4b <- add_base(d4b, choice_initial_4b, multi_genre,    "base_forms")
d4b <- add_base(d4b, choice_initial_4b, sold_30m,       "base_sold30m")
d4b <- add_base(d4b, choice_initial_4b, classic_50plus, "base_classic")

# =============================================================================
# Per-cell runs.  Target (race/gender) rows are flagged is_target = TRUE.
# =============================================================================
rows <- list()

# Study 2
rows[[length(rows)+1]] <- run_zero_cell(d2, "gender_feedback",   "female_pick",     "base_gender",     5/25, 6,
                                        "Gender (TARGET)",                 "Study 2",  is_target = TRUE)
rows[[length(rows)+1]] <- run_zero_cell(d2, "entertainer_shown", "entertainer_pick","base_entertainer", length(entertainer_2)/25, 6,
                                        "Featured an Entertainer",         "Study 2")
rows[[length(rows)+1]] <- run_zero_cell(d2, "pages_shown",       "pages_pick",      "base_pages",      length(pages_2)/25, 6,
                                        "Over 500 Pages",                  "Study 2")
rows[[length(rows)+1]] <- run_zero_cell(d2, "year_shown",        "year_pick",       "base_year",       length(year_2)/25, 6,
                                        "Written in Past 25 Years",        "Study 2")

# Study 3A
rows[[length(rows)+1]] <- run_zero_cell(d3a, "race_feedback",  "race_pick",     "base_race",     4/25, 7,
                                        "Race (TARGET)",                   "Study 3A", is_target = TRUE)
rows[[length(rows)+1]] <- run_zero_cell(d3a, "budget_shown",   "budget_pick",   "base_budget",   length(budget_3a)/25, 7,
                                        "High Budget",                     "Study 3A")
rows[[length(rows)+1]] <- run_zero_cell(d3a, "duration_shown", "duration_pick", "base_duration", length(duration_3a)/25, 7,
                                        "Long Duration",                   "Study 3A")
rows[[length(rows)+1]] <- run_zero_cell(d3a, "year_shown",     "year_pick",     "base_year",     length(year_3a)/25, 7,
                                        "Recent Release",                  "Study 3A")

# Study 3B
rows[[length(rows)+1]] <- run_zero_cell(d3b, "gender_feedback",  "female_pick",  "base_gender",  6/25, 6,
                                        "Gender (TARGET)",                 "Study 3B", is_target = TRUE)
rows[[length(rows)+1]] <- run_zero_cell(d3b, "budget_shown",     "budget_pick",  "base_budget",  length(budget_3b)/25, 6,
                                        "High Budget",                     "Study 3B")
rows[[length(rows)+1]] <- run_zero_cell(d3b, "poli_shown",       "poli_pick",    "base_poli",    length(poli_3b)/25, 6,
                                        "Political Leader",                "Study 3B")
rows[[length(rows)+1]] <- run_zero_cell(d3b, "year_shown",       "year_pick",    "base_year",    length(year_3b)/25, 6,
                                        "Recent Release",                  "Study 3B")

# Study 4A
rows[[length(rows)+1]] <- run_zero_cell(d4a, "race_feedback", "race_pick",   "base_race",   8/25, 6,
                                        "Race (TARGET)",                   "Study 4A", is_target = TRUE)
rows[[length(rows)+1]] <- run_zero_cell(d4a, "books",         "book_pick",   "base_books",  length(books_list_4a)/25, 6,
                                        "Wrote Books in 500-Most-Common List", "Study 4A")
rows[[length(rows)+1]] <- run_zero_cell(d4a, "oldies",        "oldies_pick", "base_oldies", length(oldies_list_4a)/25, 6,
                                        "Wrote Classic Works (50+ Years Old)", "Study 4A")
rows[[length(rows)+1]] <- run_zero_cell(d4a, "poets",         "poets_pick",  "base_poets",  length(poets_list_4a)/25, 6,
                                        "Wrote Poetry",                    "Study 4A")

# Study 4B
rows[[length(rows)+1]] <- run_zero_cell(d4b, "gender_feedback", "female_pick",  "base_gender",  6/25, 6,
                                        "Gender (TARGET)",                 "Study 4B", is_target = TRUE)
rows[[length(rows)+1]] <- run_zero_cell(d4b, "classic_shown",   "classic_pick", "base_classic", length(classic_50plus)/25, 6,
                                        "In Continuous Print 50+ Years",   "Study 4B")
rows[[length(rows)+1]] <- run_zero_cell(d4b, "sold30m_shown",   "sold30m_pick", "base_sold30m", length(sold_30m)/25, 6,
                                        "Sold 30M+ Copies",                "Study 4B")
rows[[length(rows)+1]] <- run_zero_cell(d4b, "forms_shown",     "forms_pick",   "base_forms",   length(multi_genre)/25, 6,
                                        "Spanning Multiple Genres",        "Study 4B")

all_res <- do.call(rbind, rows)

cat("\n\n===========================================================\n")
cat("Zero-benchmark analysis: per-cell results\n")
cat("===========================================================\n")
print(all_res, row.names = FALSE)

write.csv(all_res, "revision-analysis/_r1_review/de_zero_benchmark_results.csv", row.names = FALSE)

# =============================================================================
# Random-effects meta-analysis (REML) across cells -- matches original Table R1
# =============================================================================
pool_one <- function(est, se, label) {
  ok <- !is.na(est) & !is.na(se) & is.finite(se) & se > 0
  est_ok <- est[ok]; se_ok <- se[ok]
  if (length(est_ok) < 2) {
    return(data.frame(group=label, k=length(est_ok), pooled=NA, se=NA,
                      ci_lo=NA, ci_hi=NA, p=NA, i2=NA))
  }
  fit <- rma(yi = est_ok, sei = se_ok, method = "REML")
  data.frame(group = label,
             k     = fit$k,
             pooled= round(as.numeric(fit$b), 3),
             se    = round(fit$se, 3),
             ci_lo = round(fit$ci.lb, 3),
             ci_hi = round(fit$ci.ub, 3),
             p     = round(fit$pval, 4),
             i2    = round(fit$I2, 1))
}

target_rows     <- all_res[all_res$is_target, ]
comparison_rows <- all_res[!all_res$is_target, ]

cat("\n\n===========================================================\n")
cat("Random-effects pool (REML), percentage points\n")
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
write.csv(re_pools, "revision-analysis/_r1_review/de_zero_benchmark_re_pools.csv", row.names = FALSE)

# =============================================================================
# Fixed-effects pooled regression (the "fancy" alternative).
#
# Long format: each row = one (participant, study, attribute). Each
# participant contributes one row per attribute (target row for their
# gender/race condition + 3 comparison-attribute rows in their study).
# Model: pick ~ feedback * zero_initial + cell_FE, with SEs clustered by
# participant id (CR1 sandwich). Run separately for target vs comparison cells.
# =============================================================================

build_long <- function(d, study_label, cell_specs, pid_prefix) {
  d$.pid <- paste0(pid_prefix, "_", seq_len(nrow(d)))
  out <- lapply(cell_specs, function(spec) {
    pick  <- d[[spec$pick]]
    fb    <- d[[spec$feedback]]
    base  <- d[[spec$base]]
    ok    <- !is.na(pick) & !is.na(fb) & !is.na(base)
    data.frame(
      pid         = d$.pid[ok],
      study       = study_label,
      cell        = paste0(study_label, ":", spec$label),
      attribute   = spec$label,
      is_target   = spec$is_target,
      feedback    = as.integer(fb[ok]),
      zero_initial= as.integer(base[ok] == 0),
      pick        = as.integer(pick[ok]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

cells_2 <- list(
  list(feedback="gender_feedback",   pick="female_pick",      base="base_gender",      label="Gender",                    is_target=TRUE),
  list(feedback="entertainer_shown", pick="entertainer_pick", base="base_entertainer", label="Entertainer",               is_target=FALSE),
  list(feedback="pages_shown",       pick="pages_pick",       base="base_pages",       label="Pages500",                  is_target=FALSE),
  list(feedback="year_shown",        pick="year_pick",        base="base_year",        label="Past25",                    is_target=FALSE)
)
cells_3a <- list(
  list(feedback="race_feedback",  pick="race_pick",     base="base_race",     label="Race",         is_target=TRUE),
  list(feedback="budget_shown",   pick="budget_pick",   base="base_budget",   label="Budget",       is_target=FALSE),
  list(feedback="duration_shown", pick="duration_pick", base="base_duration", label="Duration",     is_target=FALSE),
  list(feedback="year_shown",     pick="year_pick",     base="base_year",     label="Year",         is_target=FALSE)
)
cells_3b <- list(
  list(feedback="gender_feedback",  pick="female_pick",  base="base_gender",  label="Gender",       is_target=TRUE),
  list(feedback="budget_shown",     pick="budget_pick",  base="base_budget",  label="Budget",       is_target=FALSE),
  list(feedback="poli_shown",       pick="poli_pick",    base="base_poli",    label="PoliLeader",   is_target=FALSE),
  list(feedback="year_shown",       pick="year_pick",    base="base_year",    label="Year",         is_target=FALSE)
)
cells_4a <- list(
  list(feedback="race_feedback", pick="race_pick",   base="base_race",   label="Race",         is_target=TRUE),
  list(feedback="books",         pick="book_pick",   base="base_books",  label="Books",        is_target=FALSE),
  list(feedback="oldies",        pick="oldies_pick", base="base_oldies", label="Oldies",       is_target=FALSE),
  list(feedback="poets",         pick="poets_pick",  base="base_poets",  label="Poets",        is_target=FALSE)
)
cells_4b <- list(
  list(feedback="gender_feedback", pick="female_pick",  base="base_gender",  label="Gender",       is_target=TRUE),
  list(feedback="classic_shown",   pick="classic_pick", base="base_classic", label="Classic",      is_target=FALSE),
  list(feedback="sold30m_shown",   pick="sold30m_pick", base="base_sold30m", label="Sold30M",      is_target=FALSE),
  list(feedback="forms_shown",     pick="forms_pick",   base="base_forms",   label="Forms",        is_target=FALSE)
)

long_all <- rbind(
  build_long(d2,  "Study2",  cells_2,  "S2"),
  build_long(d3a, "Study3A", cells_3a, "S3A"),
  build_long(d3b, "Study3B", cells_3b, "S3B"),
  build_long(d4a, "Study4A", cells_4a, "S4A"),
  build_long(d4b, "Study4B", cells_4b, "S4B")
)
long_all$cell <- factor(long_all$cell)

# Cluster-robust SEs (CR1) by participant id
cluster_se <- function(model, cluster_var) {
  vc <- sandwich::vcovCL(model, cluster = cluster_var, type = "HC1")
  list(coef = coeftest(model, vcov. = vc),
       ci   = coefci(model, vcov. = vc))
}

run_fe_pool <- function(dat, label) {
  cat(sprintf("\n--- Fixed-effects pool: %s (N = %d obs across %d cells, %d participants) ---\n",
              label, nrow(dat), nlevels(droplevels(dat$cell)), length(unique(dat$pid))))

  # Model A: main effect of feedback only
  mA <- lm(pick ~ feedback + cell, data = dat)
  rA <- cluster_se(mA, dat$pid)
  iA <- which(rownames(rA$coef) == "feedback")
  cat(sprintf("  A. feedback (main, cell FE):           %+0.2f pp  SE %.3f  p=%.4g  CI [%.2f, %.2f]\n",
              100*rA$coef[iA,1], 100*rA$coef[iA,2], rA$coef[iA,4],
              100*rA$ci[iA,1], 100*rA$ci[iA,2]))

  # Model B: feedback x zero_initial, cell FE
  mB <- lm(pick ~ feedback * zero_initial + cell, data = dat)
  rB <- cluster_se(mB, dat$pid)
  iB_main <- which(rownames(rB$coef) == "feedback")
  iB_int  <- which(rownames(rB$coef) == "feedback:zero_initial")
  cat(sprintf("  B. feedback @ initial>=1:               %+0.2f pp  SE %.3f  p=%.4g  CI [%.2f, %.2f]\n",
              100*rB$coef[iB_main,1], 100*rB$coef[iB_main,2], rB$coef[iB_main,4],
              100*rB$ci[iB_main,1], 100*rB$ci[iB_main,2]))
  cat(sprintf("     feedback x zero_initial (interact): %+0.2f pp  SE %.3f  p=%.4g  CI [%.2f, %.2f]\n",
              100*rB$coef[iB_int,1], 100*rB$coef[iB_int,2], rB$coef[iB_int,4],
              100*rB$ci[iB_int,1], 100*rB$ci[iB_int,2]))

  # Reparameterise: pooled feedback effect WITHIN zero-initial subgroup
  dat_zero <- dat[dat$zero_initial == 1, ]
  dat_zero$cell <- droplevels(dat_zero$cell)
  if (nrow(dat_zero) > 0 && nlevels(dat_zero$cell) >= 1 &&
      length(unique(dat_zero$feedback)) == 2) {
    mC <- lm(pick ~ feedback + cell, data = dat_zero)
    rC <- cluster_se(mC, dat_zero$pid)
    iC <- which(rownames(rC$coef) == "feedback")
    cat(sprintf("  C. feedback within initial=0 only:     %+0.2f pp  SE %.3f  p=%.4g  CI [%.2f, %.2f]    (N=%d, cells=%d)\n",
                100*rC$coef[iC,1], 100*rC$coef[iC,2], rC$coef[iC,4],
                100*rC$ci[iC,1], 100*rC$ci[iC,2], nrow(dat_zero), nlevels(dat_zero$cell)))
  } else {
    cat("  C. feedback within initial=0 only:     <too few obs>\n")
  }

  invisible(list(A = list(model = mA, robust = rA),
                 B = list(model = mB, robust = rB)))
}

cat("\n\n===========================================================\n")
cat("Fixed-effects pooled regressions (cell FE; participant-clustered SE)\n")
cat("===========================================================\n")
fe_target     <- run_fe_pool(long_all[long_all$is_target == TRUE, ],  "TARGET (race/gender)")
fe_comparison <- run_fe_pool(long_all[long_all$is_target == FALSE, ], "COMPARISON (other attributes)")

# Save the long format for any follow-up
write.csv(long_all, "revision-analysis/_r1_review/de_zero_benchmark_long.csv", row.names = FALSE)

# =============================================================================
# REFRAME: formal target vs comparison contrast among initial = 0 rows only.
# Stack every (participant, cell) row where the participant had base_count == 0
# for that attribute, then fit:
#   pick ~ feedback * is_target + cell_FE         (CR1 SEs clustered by pid)
# Coefficients:
#   feedback                  -- comparison-cell feedback effect at initial = 0
#   feedback:is_targetTRUE    -- DIFFERENTIAL: target - comparison effect at 0
#   feedback + interaction    -- target-cell feedback effect at initial = 0
# =============================================================================
long_zero <- long_all[long_all$zero_initial == 1, ]
long_zero$cell <- droplevels(long_zero$cell)

cat("\n\n===========================================================\n")
cat("Contrast at initial = 0 :  target vs comparison\n")
cat("===========================================================\n")
cat(sprintf("Stacked sample at initial=0: %d obs over %d cells, %d participants\n",
            nrow(long_zero), nlevels(long_zero$cell), length(unique(long_zero$pid))))
cat(sprintf("  Target cells:     %d obs across %d cells\n",
            sum(long_zero$is_target), length(unique(long_zero$cell[long_zero$is_target]))))
cat(sprintf("  Comparison cells: %d obs across %d cells\n",
            sum(!long_zero$is_target), length(unique(long_zero$cell[!long_zero$is_target]))))

m_contrast <- lm(pick ~ feedback * is_target + cell, data = long_zero)
vc_contrast <- sandwich::vcovCL(m_contrast, cluster = long_zero$pid, type = "HC1")
ct <- coeftest(m_contrast, vcov. = vc_contrast)
ci <- coefci(m_contrast, vcov. = vc_contrast)

show_row <- function(name, label) {
  i <- which(rownames(ct) == name)
  if (length(i) == 0) { cat(sprintf("  %-45s <missing>\n", label)); return(NULL) }
  cat(sprintf("  %-45s  %+6.2f pp   SE %5.3f   p=%-9.4g   CI [%+0.2f, %+0.2f]\n",
              label, 100*ct[i,1], 100*ct[i,2], ct[i,4], 100*ci[i,1], 100*ci[i,2]))
}
show_row("feedback",                  "Comparison effect at initial=0:")
show_row("feedback:is_targetTRUE",    "Target - Comparison contrast:")

# Reparameterise to get the target-cell main effect at initial=0
long_zero$is_comparison <- !long_zero$is_target
m_contrast_b <- lm(pick ~ feedback * is_comparison + cell, data = long_zero)
vc_b <- sandwich::vcovCL(m_contrast_b, cluster = long_zero$pid, type = "HC1")
ct_b <- coeftest(m_contrast_b, vcov. = vc_b)
ci_b <- coefci(m_contrast_b, vcov. = vc_b)
i_t <- which(rownames(ct_b) == "feedback")
cat(sprintf("  %-45s  %+6.2f pp   SE %5.3f   p=%-9.4g   CI [%+0.2f, %+0.2f]\n",
            "Target effect at initial=0:",
            100*ct_b[i_t,1], 100*ct_b[i_t,2], ct_b[i_t,4], 100*ci_b[i_t,1], 100*ci_b[i_t,2]))

# =============================================================================
# Compact per-cell display for the (potential) revised Table R1
# =============================================================================
display <- all_res[, c("study","attribute","is_target","n_zero",
                       "zero_est_pp","zero_se_pp","zero_p",
                       "zero_ci_lo_pp","zero_ci_hi_pp",
                       "all_est_pp","all_p","n_total")]
display <- display[display$n_zero >= 20, ]
display$Type <- ifelse(display$is_target, "Target", "Comparison")
display <- display[order(!display$is_target, display$study, display$attribute), ]
display$is_target <- NULL
cat("\n\n===========================================================\n")
cat("Compact per-cell table (cells with n_initial=0 >= 20)\n")
cat("===========================================================\n")
print(display, row.names = FALSE)
write.csv(display, "revision-analysis/_r1_review/de_zero_benchmark_display.csv", row.names = FALSE)

# Summary text
cat("\n\n===========================================================\n")
cat("Saved:\n")
cat("  revision-analysis/_r1_review/de_zero_benchmark_results.csv     (per-cell)\n")
cat("  revision-analysis/_r1_review/de_zero_benchmark_re_pools.csv    (REML pools)\n")
cat("  revision-analysis/_r1_review/de_zero_benchmark_long.csv        (long format)\n")
cat("  revision-analysis/_r1_review/de_zero_benchmark_display.csv     (compact table)\n")
cat("===========================================================\n")
