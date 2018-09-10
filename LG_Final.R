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

########  DIVIDE INTO TRAINING AND VALIDATION SETS   ############

ctraining <-

cval <-

########  DATA EXPLORATION  ################
rs_table <- with(lowbwt,                                                            ##Example exploration stuff from class code
                 table(factor(smoke, labels = c("non-smoker", "smoker")),
                       race))
addmargins(rs_table) # add row and column totals
hist(lowbwt$age, breaks = 20, col = "gray", xlab = "age", main = "")
hist(lowbwt$lwt, breaks = 25, col = "gray", xlab = "lwt", main = "")

########  REGRESSION ANALYSIS  #############
fit <- glm(Win_Bid ~ x1 + x2 
           data = ctraining, family = binomial(link = "logit"))
summary(fit)

exp(confint(fit))               ## Get likelihood confidence intervals (Mass Library) --> exponentiated --> CI for odds ratio



## Likelihood Ratio Test
fit2 <- glm(Win_Bid ~ x1 + x2, data = ctraining, family = binomial)
anova(fit, fit2, test = "LRT")

# can also check for separation using separation.detection()
# each column shows convergence of standard errors for betas
# if any keep changing, then it didn't converge
separation.detection(fit)

influence.measures(fit)

### plot Cook's distance
plot(fit, 4, n.id = 5) # n.id = #points identified on the plot

### dfbetas plots
# some variable:
dfbetasPlots(fit, terms = "soem variable", id.n = 5,
             col = ifelse(fit$y == 1, "red", "blue"))

### partial residuals ###
# soem variable:
visreg(fit, "some variable", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for some variable",
       x = "some variable", y = "partial (deviance) residuals")

### GAMs ###
# fit model as a GAM:
fit.gam <- gam(Win_bid ~ s(x1) + x2,
               data = ctraining, family = binomial, method = "REML")
summary(fit.gam)

# plot estimated effect of some variable
plot(fit.gam, ylab = "f(some variable)", shade = TRUE, main = "effect of soem variable", jit = TRUE,
     seWithMean = TRUE)

#############  Model Assessment  ###############
# AIC and BIC
AIC(fit)
BIC(fit)

PseudoR2(fit, which = c("Cox", "Nagelkerke", "McFadden"))

### Brier score function ###
brier_score <- function(obj, new_x = NULL, new_y = NULL){
  # computes [scaled] brier score
  #
  # inputs:
  # 1. obj: either a model from glm() or a data frame.
  #         the data frame must have a vector responses "y" and a vector of
  #         either probabilities "p" or linear predictor "lp".
  # 2. new_x: specify new dataset to get predicted probabilities for new obs.
  #             if NULL, the estimated probabilities from original obs will
  #             be used.
  # 3. new_y: use new responses. if NULL, original ones will be used.
  #
  # output:
  #   brier score, scaled brier score
  
  if(is.null(new_y)){
    y <- obj$y
  } else {
    y <- new_y
  }
  
  p_obs <- mean(y)
  
  if(any(class(obj) == "glm")){
    if(is.null(new_x)){
      p <- predict(obj, newdata = new_x, type = "response")
      lp <- predict(obj, newdata = new_x, type = "link")
    } else {
      lp <- obj$linear
      p <- fitted(obj)
    }
  } else if(is.null(obj$p)) {
    lp <- obj$lp
    p <- fitted(obj)
  } else {
    p <- obj$p
    lp <- obj$linear
  }
  
  # brier score
  brier_score <- mean((y - p)^2)
  
  # max brier score is just the observed proportion
  brier_max <- p_obs*((1 - p_obs)^2) + (1 - p_obs)*(p_obs^2)
  
  # scaled brier score
  # ranges from 0 to 1---lower is better
  brier_scaled <- brier_score/brier_max
  # essentially, 1 - brier_scaled is the %improvement over null model
  
  res <- data.frame(brier_score = brier_score,
                    brier_max = brier_max,
                    brier_scaled = brier_scaled)
  res
}

brier_score(fit)


### discrimination slope = mean(p1) - mean(p0) ###
D <- mean(fitted(fit)[fit$y == 1]) - mean(fitted(fit)[fit$y == 0])
# alternatively:
# D <- diff(aggregate(fitted(fit), by = list(fit$y == 0, fit$y == 1), FUN = mean)$x)

# histogram of predicted probabilities by outcome
# create data frame of outcome and predicted probabilities
df <- data.frame(y = fit$y,
                 phat = fitted(fit))
ggplot(df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")

### c-statistic and Somers' D ###
# predicted prob goes first, outcome second
rcorr.cens(fitted(fit), fit$y)[-c(5, 6, 9)] # ignoring output i don't need

## predictions

# type = "link" will return the predicted log(odds) for each of these subjects
predict(fit, newdata = cval, type = "link")       ## Scores the new data