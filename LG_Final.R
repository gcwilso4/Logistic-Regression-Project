rm(list=ls())


install.packages("ROCR")
install.packages("DescTools")
install.packages("Hmisc")

## All libraries given to us in code snippets in case we need them
library(haven)
library(MASS)
library(visreg)
library(brglm)
library(tidyverse)
library(ROCR) 
library(DescTools)
library(Hmisc)
library(mgcv)
library(car)


setwd("C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Logistic Regression\\Final Project")

path <- "C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Logistic Regression\\Final Project\\"

input.file <- "construction.sas7bdat"

construction <- read_sas(paste(path, input.file,sep = ""))


########  DATA EXPLORATION  ################
rs_table <- with(lowbwt,                                                            ##Example exploration stuff from class code
                 table(factor(smoke, labels = c("non-smoker", "smoker")),
                       race))
addmargins(rs_table) # add row and column totals
hist(lowbwt$age, breaks = 20, col = "gray", xlab = "age", main = "")
hist(lowbwt$lwt, breaks = 25, col = "gray", xlab = "lwt", main = "")

########  REGRESSION ANALYSIS  #############
fit <- glm(y ~ x1 + x2 
           data = construction, family = binomial(link = "logit"))
summary(fit)

exp(confint(fit))               ## Get likelihood confidence intervals (Mass Library) --> exponentiated --> CI for odds ratio

## predictions
newdata <- data.frame(age = c(30, 30),
                      lwt = c(130, 130),
                      race = c("black", "white"),
                      smoke = c(1, 0))
# type = "link" will return the predicted log(odds) for each of these subjects
predict(fit, newdata = newdata, type = "link")       ## Scores the new data

## Likelihood Ratio Test
fit2 <- glm(low ~ age + lwt, data = lowbwt, family = binomial)
anova(fit, fit2, test = "LRT")

# can also check for separation using separation.detection()
# each column shows convergence of standard errors for betas
# if any keep changing, then it didn't converge
separation.detection(fit)

influence.measures(fit)

### plot Cook's distance
plot(fit, 4, n.id = 5) # n.id = #points identified on the plot

### dfbetas plots
# age:
dfbetasPlots(fit, terms = "age", id.n = 5,
             col = ifelse(fit$y == 1, "red", "blue"))

### partial residuals ###
# age:
visreg(fit, "age", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for age",
       x = "age", y = "partial (deviance) residuals")

### GAMs ###
# fit model as a GAM:
fit.gam <- gam(low ~ s(age) + lwt + smoke + race,
               data = lowbwt, family = binomial, method = "REML")
summary(fit.gam)

# plot estimated effect of age
plot(fit.gam, ylab = "f(age)", shade = TRUE, main = "effect of age", jit = TRUE,
     seWithMean = TRUE)

#############  Model Assessment  ###############

