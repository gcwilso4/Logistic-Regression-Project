rm(list=ls())

# install.packages("pastecs")
# install.packages('corrplot')
# install.packages("ROCR")
# install.packages("DescTools")
# install.packages("Hmisc")
# install.packages('visreg')
# install.packages('car')
# install.packages("caret")


## All libraries given to us in code snippets in case we need them
library(haven)
library(pastecs)
library(caret)
library(tidyverse)
library(corrplot)
# 
library(MASS)
library(visreg)
# library(brglm)
# library(ROCR) 
# library(DescTools)
# library(Hmisc)
library(mgcv)
library(car)


setwd("C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Logistic Regression\\Final Project")
#setwd("C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\lab and hw\\Logistic\\Project\\")

path <- "C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Logistic Regression\\Final Project\\"
#path <- "C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\data\\Logistic Data\\"
#path <- "C:\\Users\\gavin\\Desktop\\Logisitic_Regression_Data\\"

input.file <- "construction.sas7bdat"

construction <- read_sas(paste(path, input.file,sep = ""))

############################################
########  CLEANING   #######################
############################################

# ADDING METRICS OF INTEREST
# of competitors
construction <- construction %>%
  mutate(comp.count = select(., Competitor_A:Competitor_J) %>% apply(1, sum, na.rm=TRUE)) %>%
  mutate(profit = Bid_Price__Millions_ - Estimated_Cost__Millions_) %>%
  mutate(perc_over_bid = (Bid_Price__Millions_ - Winning_Bid_Price__Millions_)/Winning_Bid_Price__Millions_) %>%
  mutate(Win = as.numeric(as.factor(Win_Bid))-1) #converts to factor levels 1 and 2 then subtracts 1 for binary
#needed 0,1 for regression modelling later

#DIVIDE INTO TRAINING / VALIDATION SETS
set.seed(123)
train_ind <- createDataPartition(construction$Estimated_Cost__Millions_, p=0.70, list=FALSE)

ctrain <<- construction[train_ind,]
cval <- construction[-train_ind,]

#DIVIDE TRAINIG INTO CONTINUOUS, CATEGORICAL
continuous <- c(
  "Estimated_Cost__Millions_",
  "Estimated_Years_to_Complete",
  "Bid_Price__Millions_",
  "Number_of_Competitor_Bids",
  "Winning_Bid_Price__Millions_",
  "Cost_After_Engineering_Estimate_",
  "profit",
  "perc_over_bid",
  "comp.count")

ctrain_cont <- select(ctrain, continuous)
ctrain_cat <- select(ctrain, -continuous)
ctrain_cat <- as.data.frame(sapply(ctrain_cat, as.character)) #converting binary dbls to factors
ctrain_cont2 <- select(ctrain_cont, -c("Estimated_Cost__Millions_", "Winning_Bid_Price__Millions_"))#reduced to get rid of some redundant variables

############################################
########  DATA EXPLORATION  ################
############################################

#bar charts & histos
ggplot(data=ctrain, aes(Sector, fill=Win_Bid)) + geom_bar()
ggplot(data=ctrain, aes(Region_of_Country, fill=Win_Bid)) + geom_bar()
ggplot(data=ctrain, aes(Competitor_A, fill=Win_Bid)) + geom_bar()
ggplot(data=ctrain, aes(factor(comp.count), fill=Win_Bid)) + geom_bar()
ggplot(data=ctrain, aes(Winning_Bid_Price__Millions_, fill=Win_Bid)) + geom_histogram()
ggplot(data=ctrain, aes(Estimated_Years_to_Complete, fill=Win_Bid)) + geom_histogram()
ggplot(data=ctrain, aes(Number_of_Competitor_Bids, fill=Win_Bid)) + geom_histogram()
hist(ctrain$Number_of_Competitor_Bids, breaks=25)
plot(ctrain_cont2)

#Correlation matrix of continuous variables APP A?
c.cor <- cor(ctrain_cont) 
corrplot(c.cor, method = "number") #Some are perfectly correlated, comp.count not that strong with number of competitor bids!?

#summary statistics of continuous, APP A?
options(scipen=100)
options(digits=2)
stat.desc(ctrain_cont) 

# basic frequency tables 
table1 <- round(prop.table(with(ctrain, table(Region_of_Country, Win_Bid))),4)
table2 <- addmargins(round(prop.table(with(ctrain, table(Sector, Win_Bid))),4))
table3 <- with(ctrain, table(Sector, Region_of_Country, Win_Bid))
round(prop.table(table3),4)
round(addmargins(prop.table(table3)),4)

#difference in freq created by competitors
freq.expec <- function(var){
  freq.tab <- addmargins(prop.table(with(ctrain, table(var, Win_Bid))))
  diff <- round((freq.tab - chisq.test(freq.tab)$expected),4)
  print(diff)
}
competitors <- select(ctrain, Competitor_A:Competitor_J)
lapply(competitors, freq.expec) # Max diff from expected frequency: 4% with competitor H and F


############################################
########  REGRESSION ANALYSIS  #############
############################################
fit1 <- glm(Win ~ comp.count + Number_of_Competitor_Bids + Region_of_Country + Estimated_Cost__Millions_, 
           data = ctrain, family = binomial(link = "logit"))
summary(fit1)  ## AIC - 185

fit2 <- glm(Win ~ Region_of_Country + Number_of_Competitor_Bids, 
           data = ctrain, family = binomial(link = "logit"))
summary(fit2) ## AIC - 189.9

fit3 <- glm(Win ~ Estimated_Years_to_Complete + Estimated_Cost__Millions_ +
              Competitor_D + comp.count + Number_of_Competitor_Bids + Region_of_Country,
            data = ctrain, family = binomial(link = "logit"))
fit.step <- stepAIC(fit, direction=c('both'))
summary(fit.step)  ## AIC - 183

fit4 <- glm(Win ~ comp.count + Number_of_Competitor_Bids + Region_of_Country + Estimated_Cost__Millions_, 
            data = ctrain, family = binomial(link = "logit"))
summary(fit4)  ## AIC - 


exp(confint(fit))               ## Get likelihood confidence intervals (Mass Library) --> exponentiated --> CI for odds ratio



## Likelihood Ratio Test
fit2 <- glm(Win ~ x1 + x2, data = ctrain, family = binomial)
anova(fit, fit2, test = "LRT")

# can also check for separation using separation.detection()
# each column shows convergence of standard errors for betas
# if any keep changing, then it didn't converge
separation.detection(fit)

influence.measures(fit)

### plot Cook's distance
plot(fit2, 4, n.id = 5) # n.id = #points identified on the plot

### dfbetas plots
# some variable:
dfbetasPlots(fit, terms = "Number_of_Competitor_Bids", id.n = 5,
             col = ifelse(fit$y == 1, "red", "blue"))

### partial residuals ###
# Number of Competitor Bids:
visreg(fit, "Number_of_Competitor_Bids", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "Partial Residual Plot for Number of Competitor Bids",
       x = "some variable", y = "partial (deviance) residuals")

### GAMs ###
# fit model as a GAM:
fit.gam <- gam(Win ~ s(x1) + x2,
               data = ctrain, family = binomial, method = "REML")
summary(fit.gam)

# plot estimated effect of some variable
plot(fit.gam, ylab = "f(some variable)", shade = TRUE, main = "effect of some variable", jit = TRUE,
     seWithMean = TRUE)

################################################
#############  Model Assessment  ###############
################################################

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