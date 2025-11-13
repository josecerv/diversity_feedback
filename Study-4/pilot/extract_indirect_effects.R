# Load libraries
library(dplyr)
library(mediation)
library(sandwich)

# Read data
d0 <- read.csv('Study-4-pilot.csv', check.names = FALSE)

# Set seed
set.seed(123)

cat('==========================================================\n')
cat('CONDITIONAL MEDIATION: INDIRECT EFFECTS BY POOL\n')
cat('==========================================================\n\n')

# ============================================
# MEN POOL (Women Underrepresented)
# ============================================

d_men <- d0 %>% filter(pool == 'men')

cat('----------------------------------------------------------\n')
cat('MEN POOL (Women Underrepresented in Pool)\n')
cat('----------------------------------------------------------\n\n')

# Internal Motivation - Men Pool
med.fit.internal.men <- lm(internal ~ gender_feedback, data = d_men)
out.fit.internal.men <- lm(female_pick ~ gender_feedback + internal, data = d_men)
med.out.internal.men <- mediate(med.fit.internal.men, out.fit.internal.men, boot = TRUE,
                                treat = 'gender_feedback', boot.ci.type = 'perc',
                                mediator = 'internal', sims = 5000)

cat('INTERNAL MOTIVATION:\n')
cat('  Indirect Effect (ACME): ', sprintf('%.4f', med.out.internal.men$d0), '\n')
cat('  95% CI: [', sprintf('%.4f', med.out.internal.men$d0.ci[1]), ', ',
    sprintf('%.4f', med.out.internal.men$d0.ci[2]), ']\n', sep='')
cat('  p-value: ', sprintf('%.4f', med.out.internal.men$d0.p), '\n\n')

# External Motivation - Men Pool
med.fit.external.men <- lm(external ~ gender_feedback, data = d_men)
out.fit.external.men <- lm(female_pick ~ gender_feedback + external, data = d_men)
med.out.external.men <- mediate(med.fit.external.men, out.fit.external.men, boot = TRUE,
                                treat = 'gender_feedback', boot.ci.type = 'perc',
                                mediator = 'external', sims = 5000)

cat('EXTERNAL MOTIVATION:\n')
cat('  Indirect Effect (ACME): ', sprintf('%.4f', med.out.external.men$d0), '\n')
cat('  95% CI: [', sprintf('%.4f', med.out.external.men$d0.ci[1]), ', ',
    sprintf('%.4f', med.out.external.men$d0.ci[2]), ']\n', sep='')
cat('  p-value: ', sprintf('%.4f', med.out.external.men$d0.p), '\n\n')

# ============================================
# WOMEN POOL (Women Overrepresented)
# ============================================

d_women <- d0 %>% filter(pool == 'women')

cat('----------------------------------------------------------\n')
cat('WOMEN POOL (Women Overrepresented in Pool)\n')
cat('----------------------------------------------------------\n\n')

# Internal Motivation - Women Pool
med.fit.internal.women <- lm(internal ~ gender_feedback, data = d_women)
out.fit.internal.women <- lm(female_pick ~ gender_feedback + internal, data = d_women)
med.out.internal.women <- mediate(med.fit.internal.women, out.fit.internal.women, boot = TRUE,
                                  treat = 'gender_feedback', boot.ci.type = 'perc',
                                  mediator = 'internal', sims = 5000)

cat('INTERNAL MOTIVATION:\n')
cat('  Indirect Effect (ACME): ', sprintf('%.4f', med.out.internal.women$d0), '\n')
cat('  95% CI: [', sprintf('%.4f', med.out.internal.women$d0.ci[1]), ', ',
    sprintf('%.4f', med.out.internal.women$d0.ci[2]), ']\n', sep='')
cat('  p-value: ', sprintf('%.4f', med.out.internal.women$d0.p), '\n\n')

# External Motivation - Women Pool
med.fit.external.women <- lm(external ~ gender_feedback, data = d_women)
out.fit.external.women <- lm(female_pick ~ gender_feedback + external, data = d_women)
med.out.external.women <- mediate(med.fit.external.women, out.fit.external.women, boot = TRUE,
                                  treat = 'gender_feedback', boot.ci.type = 'perc',
                                  mediator = 'external', sims = 5000)

cat('EXTERNAL MOTIVATION:\n')
cat('  Indirect Effect (ACME): ', sprintf('%.4f', med.out.external.women$d0), '\n')
cat('  95% CI: [', sprintf('%.4f', med.out.external.women$d0.ci[1]), ', ',
    sprintf('%.4f', med.out.external.women$d0.ci[2]), ']\n', sep='')
cat('  p-value: ', sprintf('%.4f', med.out.external.women$d0.p), '\n\n')

cat('==========================================================\n')
