# Load libraries
library(dplyr)
library(mediation)
library(sandwich)

# Read data
d0 <- read.csv('Study-4-pilot.csv', check.names = FALSE)

# Set seed
set.seed(123)

cat('==========================================================\n')
cat('CONDITIONAL MEDIATION: FAIRNESS SCALE INDIRECT EFFECTS\n')
cat('==========================================================\n\n')

# ============================================
# MEN POOL (Women Underrepresented)
# ============================================

d_men <- d0 %>% filter(pool == 'men')

cat('----------------------------------------------------------\n')
cat('MEN POOL (Women Underrepresented in Pool)\n')
cat('----------------------------------------------------------\n\n')

# Fairness - Men Pool
med.fit.fairness.men <- lm(fairness ~ gender_feedback, data = d_men)
out.fit.fairness.men <- lm(female_pick ~ gender_feedback + fairness, data = d_men)
med.out.fairness.men <- mediate(med.fit.fairness.men, out.fit.fairness.men, boot = TRUE,
                                treat = 'gender_feedback', boot.ci.type = 'perc',
                                mediator = 'fairness', sims = 5000)

cat('FAIRNESS SCALE:\n')
cat('  Indirect Effect (ACME): ', sprintf('%.4f', med.out.fairness.men$d0), '\n')
cat('  95% CI: [', sprintf('%.4f', med.out.fairness.men$d0.ci[1]), ', ',
    sprintf('%.4f', med.out.fairness.men$d0.ci[2]), ']\n', sep='')
cat('  p-value: ', sprintf('%.4f', med.out.fairness.men$d0.p), '\n\n')

# ============================================
# WOMEN POOL (Women Overrepresented)
# ============================================

d_women <- d0 %>% filter(pool == 'women')

cat('----------------------------------------------------------\n')
cat('WOMEN POOL (Women Overrepresented in Pool)\n')
cat('----------------------------------------------------------\n\n')

# Fairness - Women Pool
med.fit.fairness.women <- lm(fairness ~ gender_feedback, data = d_women)
out.fit.fairness.women <- lm(female_pick ~ gender_feedback + fairness, data = d_women)
med.out.fairness.women <- mediate(med.fit.fairness.women, out.fit.fairness.women, boot = TRUE,
                                  treat = 'gender_feedback', boot.ci.type = 'perc',
                                  mediator = 'fairness', sims = 5000)

cat('FAIRNESS SCALE:\n')
cat('  Indirect Effect (ACME): ', sprintf('%.4f', med.out.fairness.women$d0), '\n')
cat('  95% CI: [', sprintf('%.4f', med.out.fairness.women$d0.ci[1]), ', ',
    sprintf('%.4f', med.out.fairness.women$d0.ci[2]), ']\n', sep='')
cat('  p-value: ', sprintf('%.4f', med.out.fairness.women$d0.p), '\n\n')

cat('==========================================================\n')
