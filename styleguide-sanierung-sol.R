# Exercise 3: Linear Models and Dummy Coding

errors <- read.table("read.txt", header = TRUE)
str(errors)
summary(errors)

# variable HowOftenRead coding:
# ordinal, with category 1 = often, category 5 = almost never

table(errors[["HowOftenRead"]])
errors[["HowOftenRead"]][errors[["HowOftenRead"]] == 5] <- 4
# convert to factors
errors[["HowOftenRead"]] <- as.factor(errors[["HowOftenRead"]])


lm_read_dummy <- lm(NumErrors ~ HowOftenRead, data = errors)
coefs_dummy <- coefficients(lm_read_dummy)
summary(lm_read_dummy)

# Intercept: E-value in group 1 (students that read often)
boxplot(NumErrors ~ HowOftenRead, data = errors)

errors[["sex"]] <- factor(errors[["sex"]],
  levels = c(0, 1), labels = c("female", "male")
)
head(errors[["sex"]])

# fit a model with just sex as explanatory
lm_sex <- lm(NumErrors ~ sex, data = errors)
summary(lm_sex)

# being male has a significant effect on the number of errors
# question: what happens if I change reference category?
errors[["sex"]] <- relevel(errors[["sex"]], ref = "male")

lm_errors <- lm(NumErrors ~ ReadTimeMin + sex + HowOftenRead, data = errors)
summary(lm_errors)

# add interaction between sex and readtimemin
lm_errors <- lm(NumErrors ~ ReadTimeMin + sex +
  HowOftenRead + sex * ReadTimeMin, data = errors)
summary(lm_errors)
