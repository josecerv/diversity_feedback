library(dplyr)
library(lmtest)
library(sandwich)

# Read data
d0 <- read.csv('Study3B.csv', check.names = FALSE)

# Robust summary function
robust_summary <- function(model) {
    robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
    model_summary <- summary(model)
    model_summary$coefficients[, "Std. Error"] <- robust_se
    model_summary$coefficients[, "t value"] <- model_summary$coefficients[, "Estimate"] / robust_se
    model_summary$coefficients[, "Pr(>|t|)"] <- 2 * pt(-abs(model_summary$coefficients[, "t value"]), df = model_summary$df[2], lower.tail = TRUE)
    return(model_summary)
}

# Independent regression equations
model1 <- lm(female_pick ~ gender_feedback, data=d0)
model2 <- lm(forms_pick ~ forms_shown, data=d0)
model3 <- lm(sold30m_pick ~ sold30m_shown, data=d0)
model4 <- lm(classic_pick ~ classic_shown, data=d0)

# Simultaneous system of equations (all predictors, no constant)
model5 <- lm(female_pick ~ gender_feedback + forms_shown + sold30m_shown + classic_shown - 1, data=d0)
model6 <- lm(forms_pick ~ gender_feedback + forms_shown + sold30m_shown + classic_shown - 1, data=d0)
model7 <- lm(sold30m_pick ~ gender_feedback + forms_shown + sold30m_shown + classic_shown - 1, data=d0)
model8 <- lm(classic_pick ~ gender_feedback + forms_shown + sold30m_shown + classic_shown - 1, data=d0)

# Print results
cat("MODEL 1: Female Pick ~ Gender Feedback\n")
print(robust_summary(model1))
cat("R-squared:", summary(model1)$r.squared, "\n\n")

cat("MODEL 2: Forms Pick ~ Forms Shown\n")
print(robust_summary(model2))
cat("R-squared:", summary(model2)$r.squared, "\n\n")

cat("MODEL 3: Sold30M Pick ~ Sold30M Shown\n")
print(robust_summary(model3))
cat("R-squared:", summary(model3)$r.squared, "\n\n")

cat("MODEL 4: Classic Pick ~ Classic Shown\n")
print(robust_summary(model4))
cat("R-squared:", summary(model4)$r.squared, "\n\n")

cat("MODEL 5: Female Pick ~ All (no intercept)\n")
print(robust_summary(model5))
cat("R-squared:", summary(model5)$r.squared, "\n\n")

cat("MODEL 6: Forms Pick ~ All (no intercept)\n")
print(robust_summary(model6))
cat("R-squared:", summary(model6)$r.squared, "\n\n")

cat("MODEL 7: Sold30M Pick ~ All (no intercept)\n")
print(robust_summary(model7))
cat("R-squared:", summary(model7)$r.squared, "\n\n")

cat("MODEL 8: Classic Pick ~ All (no intercept)\n")
print(robust_summary(model8))
cat("R-squared:", summary(model8)$r.squared, "\n\n")
