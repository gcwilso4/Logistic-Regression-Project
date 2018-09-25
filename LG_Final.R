############################################
########  LOGISTIC PROJECT  ################
############################################


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
library(corrplot)
library(MASS)
library(visreg)
library(brglm)
library(ROCR) 
# library(DescTools)
library(Hmisc)
library(mgcv)
library(car)
library(tidyverse)


#setwd("C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Logistic Regression\\Final Project")
setwd("C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\lab and hw\\Logistic\\Project\\")
#setwd("C:\\Users\\Grant\\Documents\\MSA\\Fall\\Logistic Regression")
#setwd("C:\\Users\\molly\\OneDrive\\Documents\\Logistic Regression\\MSA2019LogisticData\\data\\")

#path <- "C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Logistic Regression\\Final Project\\"
path <- "C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\data\\Logistic Data\\"
#path <- "C:\\Users\\gavin\\Desktop\\Logisitic_Regression_Data\\"
#path <- "C:\\Users\\Grant\\Documents\\MSA\\Fall\\Logistic Regression\\"
#path <- "C:\\Users\\molly\\OneDrive\\Documents\\Logistic Regression\\MSA2019LogisticData\\data\\"


input.file <- "construction.sas7bdat"
construction <- read_sas(paste(path, input.file,sep = ""))


############################################
########  EXPLORATION OF NON-COMPETITVE PROJECTS #############
############################################
#M. Rubin
#Question: Should we consider bids where there were no competitors?
#Answer: probably doesn't change our results much, only .5% of population.

#View(construction)
dim(construction)
#543 obs

#frequency table number of competitor bids
mytable <- table(construction$Number_of_Competitor_Bids) 
mytable 
#only 3 with 0 bids

#crosstab of bid numbers and how many wins
mytable1 <- table(construction$Win_Bid, construction$Number_of_Competitor_Bids) 
mytable1
#obviously won all with 0. won 90% between 1-3 bids

mean(construction$Number_of_Competitor_Bids)
#12 average bids

3/543
#only .5% of the bids with 0 competitors


#if we decide to remove this population, use table below for analysis
bid0_remove <- subset(construction, Number_of_Competitor_Bids = 0)
#View(bid0_remove)



############################################
########  CLEANING   #######################
############################################
#by S.Powell

# ADDING METRICS OF INTEREST
# of competitors
construction <- construction %>%
  mutate(comp.count = select(., Competitor_A:Competitor_J) %>% apply(1, sum, na.rm=TRUE)) %>%
  mutate(Est_profit = Bid_Price__Millions_ - Estimated_Cost__Millions_) %>%
  mutate(Win_profit = Winning_Bid_Price__Millions_  - Estimated_Cost__Millions_) %>%
  mutate(perc_over_bid = (Bid_Price__Millions_ - Winning_Bid_Price__Millions_)/Winning_Bid_Price__Millions_) %>%
  mutate(Eng_Cost.per.Est_Cost = (Cost_After_Engineering_Estimate_*0.001/Estimated_Cost__Millions_)) %>%
  mutate(Win = as.numeric(as.factor(Win_Bid))-1) #converts to factor levels 1 and 2 then subtracts 1 for binary
# needed 0,1 for regression modelling later
# Cost_After_Engineering_Estimate_ not define in data dictionary. Value is tiny relative to other prices. What does it mean?
# TODO Ask Matt

#DEFINING SECTORS
Sec.names <- c("Transportation",
  "Lodging",
  "Multi-family residential",
  "Amusement/recreation",
  "Highway/street",
  "Education",
  "Healthcare",
  "Manufacturing",
  "Power",
  "Military") #assumed order based on PDF. No actual key provided
Sector <- as.character(c(1:10))
sector.names <- as.data.frame(cbind(Sector, Sec.names))
construction$Sector <- as.character(construction$Sector) 
construction <- left_join(construction, sector.names, by="Sector")
construction <- construction[,c(1:4,26,5:25)] #reordering to check merge success
# TODO Fix hardcoding in the above line 

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
  "Est_profit",
  "perc_over_bid",
  "comp.count")

ctrain_cont <- dplyr::select(ctrain, continuous)
ctrain_cat <- dplyr::select(ctrain, setdiff(colnames(ctrain), continuous))
ctrain_cat <- as.data.frame(sapply(ctrain_cat, as.character)) #converting binary dbls to factors
ctrain_cont2 <- dplyr::select(ctrain_cont, setdiff(colnames(ctrain_cont), c("Estimated_Cost__Millions_", "Winning_Bid_Price__Millions_"))) #reduced to get rid of some redundant variables


############################################
########  DATA EXPLORATION  ################
############################################
#By G.Wilson and S.Powell

#BAR CHARTS AND HISTOS
#Note that the histograms are stacked, not overlapping
ggplot(data=ctrain, aes(Sector, fill=Win_Bid)) + geom_bar()
ggplot(data=ctrain, aes(Region_of_Country, fill=Win_Bid)) + geom_bar()
ggplot(data=ctrain, aes(Competitor_A, fill=Win_Bid)) + geom_bar()
ggplot(data=ctrain, aes(factor(Number_of_Competitor_Bids), fill=Win_Bid)) + geom_bar()
ggplot(data=ctrain, aes(factor(comp.count), fill=Win_Bid)) + geom_bar()
ggplot(data=ctrain, aes(Winning_Bid_Price__Millions_, fill=Win_Bid)) + geom_histogram()
ggplot(data=ctrain, aes(Estimated_Years_to_Complete, fill=Win_Bid)) + geom_histogram()
ggplot(data=ctrain, aes(Number_of_Competitor_Bids, fill=Win_Bid)) + geom_histogram()
ggplot(data=ctrain, aes(Est_profit, fill=Win_Bid)) + geom_histogram()
ggplot(data=ctrain)+geom_point(aes(x=Winning_Bid_Price__Millions_, y=Win_profit, color=as.factor(Win_Bid)))
ggplot(data=ctrain)+geom_point(aes(x=Estimated_Years_to_Complete, y=perc_over_bid, color=as.factor(Sec.names)))
ggplot(data=ctrain)+geom_point(aes(x=Estimated_Years_to_Complete, y=perc_over_bid, color=as.factor(Region_of_Country)))

# Q: Does Engineering cost have relationship with overbid? 
ggplot(data=ctrain)+geom_point(aes(x=Eng_Cost.per.Est_Cost, y=perc_over_bid, color=as.factor(Region_of_Country)))
ggplot(data=ctrain)+geom_density(aes(x=Eng_Cost.per.Est_Cost, color=as.factor(Win))) # Doesn't properly display low counts, histogram does
ggplot(data=ctrain, aes(x=Eng_Cost.per.Est_Cost)) + 
  geom_histogram(data=filter(ctrain, Win==0), fill="red", alpha=0.2) + 
  geom_histogram(data=filter(ctrain, Win==1), fill="blue", alpha=0.2) # 2 histograms necessary for Overlapping
# A: no apparent relationship

# Q: Does IAA Construction overbid in certain regions/sectors
#focusing on only losing bids
ggplot(data=filter(ctrain, Win==0)) +
  geom_density(aes(x=perc_over_bid, color=as.factor(Region_of_Country)))
ggplot(data=filter(ctrain, Win==0)) +
  geom_density(aes(x=perc_over_bid, color=as.factor(Sec.names)))
# A: no clear overbidding issues in any region or sector

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
# Warning: 
  # prop.table() determines table values as proportion of sum of ALL values in table
  # addmargins() sums rows and columns to give "sum" values on ends
  # Thus, use prop.table() within addmargins() to avoid counting sums in proportion calcs
table1 <- round(prop.table(with(ctrain, table(Region_of_Country, Win_Bid))),4)
table2 <- addmargins(round(prop.table(with(ctrain, table(Sector, Win_Bid))),4))
table2
table3 <- with(ctrain, table(Sector, Region_of_Country, Win_Bid))
round(prop.table(table3),4)
round(addmargins(prop.table(table3)),4)

options(digits=5) # Resetting from 2 decimal points, otherwise lots problems in later wt calculations 

#difference in freq created by competitors
freq.expec <- function(var){
  #calcs normal frequency table
  freq.tab <- addmargins(prop.table(with(ctrain, table(var, Win_Bid))))
  #calcs difference b/w freq table and expected freq table
  diff <- round((freq.tab - chisq.test(freq.tab)$expected),4)
  print(diff)
}
competitors <- select(ctrain, Competitor_A:Competitor_J)
lapply(competitors, freq.expec) # Max diff from expected frequency: 4% with competitor H and F


############################################
########  CANDIDATE MODELS     #############
############################################

# BUILDING CANDIDATE MODELS by Team during 2018.09.13 Meeting
fit1 <- glm(Win ~ comp.count + Number_of_Competitor_Bids + Region_of_Country + Estimated_Cost__Millions_, 
           data = ctrain, family = binomial(link = "logit"))
summary(fit1)  ## AIC - 185

fit2 <- glm(Win ~ Region_of_Country + Number_of_Competitor_Bids, 
           data = ctrain, family = binomial(link = "logit"))

summary(fit2) ## AIC - 189.9

(fit1$aic)/(fit2$aic)

fit3 <- glm(Win ~ Estimated_Years_to_Complete + Estimated_Cost__Millions_ +
              Competitor_D + comp.count + Number_of_Competitor_Bids + Region_of_Country,
            data = ctrain, family = binomial(link = "logit"))
summary(fit3)
fit3.step <- stepAIC(fit3, direction=c('both'))

summary(fit3.step)  ## AIC - 183

fit4 <- glm(Win ~ comp.count + Number_of_Competitor_Bids + Region_of_Country + 
              Estimated_Cost__Millions_ + Sec.names, 
            data = ctrain, family = binomial(link = "logit"))
summary(fit4) ## AIC - 173
# Ran stepAIC() on fit4, but it just removed Estimated_Cost var for AIC of 171.6

# CANDIDATE MODELS
fit1
fit2
fit3.step
fit4

############################################
#############  DIAGNOSTICS  ################
############################################

fit1$aic
fit2$aic
fit3.step$aic
fit4$aic

exp(confint(fit1))               ## Get likelihood confidence intervals (Mass Library) --> exponentiated --> CI for odds ratio


###### Likelihood Ratio Test ######  -> not used to select models since AIC used instead
#fit2 <- glm(Win ~ x1 + x2, data = ctrain, family = binomial)
#anova(fit, fit2, test = "LRT")

# can also check for separation using separation.detection() from brglm library
# each column shows convergence of standard errors for betas
# if any keep changing, then it didn't converge

separation.detection(fit1)
separation.detection(fit2)
separation.detection(fit3.step)
separation.detection(fit4)
influence.measures(fit1)



### Molly ###

### Cook's Distance for each model ###
#Estimate of the influence of a single data point. 
#Measures the effect of deleting this given observation. 
#Points with a large Cook's distance are considered to merit closer examination in the analysis.

#model 1
cooks.distance(fit1)
max(cooks.distance(fit1))
plot(cooks.distance(fit1))
plot(fit1, 4, n.id = 5) 
#top 3 IDs: 374 (cook's D .11), 343, 115

#model 2
cooks.distance(fit2)
max(cooks.distance(fit2))
plot(cooks.distance(fit2))
plot(fit2, 4, n.id = 5) 
#top 3 IDs: 374 (cook's D .089), 1, 343

#model 3
cooks.distance(fit3.step)
max(cooks.distance(fit3.step))
plot(cooks.distance(fit3.step))
plot(fit3.step, 4, n.id = 5)
#top 3 IDs: 374 (cook's D .099), 343, 1

#model 4
cooks.distance(fit4)
max(cooks.distance(fit4))
plot(cooks.distance(fit4))
plot(fit4, 4, n.id = 5)
#top 3 IDs: 30 (cook's D .08), 374, 343

# points 1, 343, and 374 may warrant further exploration (unexpected victories)
oddballs <- c(1,270, 343,374)

plot(fit1, n.id=8)


############################################
########  DF BETAS ANALYSIS  ###############
############################################
## By B.Jenista

#############   Model 1 (fit1)   ###########

# comp.count:
dfbetasPlots(fit1, terms = "comp.count", id.n = 5,
             col = ifelse(fit1$y == 1, "red", "blue"))

# Number of Competitor Bids:
dfbetasPlots(fit1, terms = "Number_of_Competitor_Bids", id.n = 5,
             col = ifelse(fit1$y == 1, "red", "blue"))

# Region:
dfbetasPlots(fit1, terms = "Region_of_Country", id.n = 5,
             col = ifelse(fit1$y == 1, "red", "blue"))

# Estimated Cost:
dfbetasPlots(fit1, terms = "Estimated_Cost__Millions_", id.n = 5,
             col = ifelse(fit1$y == 1, "red", "blue"))


#############   Model 2   ##################

# Number of Competitor Bids:
dfbetasPlots(fit2, terms = "Number_of_Competitor_Bids", id.n = 5,
             col = ifelse(fit2$y == 1, "red", "blue"))

# Region:
dfbetasPlots(fit2, terms = "Region_of_Country", id.n = 5,
             col = ifelse(fit2$y == 1, "red", "blue"))


#############   Model 3   ##################

# comp.count:
dfbetasPlots(fit3.step, terms = "comp.count", id.n = 5,
             col = ifelse(fit3.step$y == 1, "red", "blue"))

# Number of Competitor Bids:
dfbetasPlots(fit3.step, terms = "Number_of_Competitor_Bids", id.n = 5,
             col = ifelse(fit3.step$y == 1, "red", "blue"))

# Region:
dfbetasPlots(fit3.step, terms = "Region_of_Country", id.n = 5,
             col = ifelse(fit3$y == 1, "red", "blue"))


#############   Model 4   ##################
# comp.count:
dfbetasPlots(fit4, terms = "comp.count", id.n = 5,
             col = ifelse(fit4$y == 1, "red", "blue"))

# Number of Competitor Bids:
dfbetasPlots(fit4, terms = "Number_of_Competitor_Bids", id.n = 5,
             col = ifelse(fit4$y == 1, "red", "blue"))

# Region:
dfbetasPlots(fit4, terms = "Region_of_Country", id.n = 5,
             col = ifelse(fit4$y == 1, "red", "blue"))

# Estimated Cost:
dfbetasPlots(fit4, terms = "Estimated_Cost__Millions_", id.n = 5,
             col = ifelse(fit4$y == 1, "red", "blue"))

# Sector:
dfbetasPlots(fit4, terms = "Sector", id.n = 5,
             col = ifelse(fit4$y == 1, "red", "blue"))

###  The only influential point consistent across all 4 models is ID 374 with regards to Region (threshold ~ >1.0) ###
###  The recommended threshold of > 2*sqrt(1/n) is ~ 0.1 when n=382 which results in too many points to be useful  ###
###  If you have any concerns/questions, ask Bill for more info  ###


### Partial residual Plots ###
### Grant F, 09/16/2018
## Fit 1:

visreg(fit1, "Number_of_Competitor_Bids", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "Partial Residual Plot for Number of Competitor Bids",
       x = "some variable", y = "partial (deviance) residuals") 
visreg(fit1, "Estimated_Cost__Millions_", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "Partial Residual Plot for Estimated Cost in Millions",
       x = "some variable", y = "partial (deviance) residuals")
## Fit 2
visreg(fit2, "Number_of_Competitor_Bids", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "Partial Residual Plot for Number of Competitor Bids",
       x = "some variable", y = "partial (deviance) residuals")

## Fit 3
visreg(fit3, "Number_of_Competitor_Bids", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "Partial Residual Plot for Number of Competitor Bids",
       x = "some variable", y = "partial (deviance) residuals")
visreg(fit3, "Estimated_Years_to_Complete", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "Partial Residual Plot for Estimated Years to Complete",
       x = "some variable", y = "partial (deviance) residuals") # this one doesn't look good
visreg(fit3, "Estimated_Cost__Millions_", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "Partial Residual Plot for Estimated Cost in Millions",
       x = "some variable", y = "partial (deviance) residuals")
## Fit 4
visreg(fit4, "Number_of_Competitor_Bids", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "Partial Residual Plot for Number of Competitor Bids",
       x = "some variable", y = "partial (deviance) residuals")
visreg(fit4, "Estimated_Cost__Millions_", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "Partial Residual Plot for Estimated Cost in Millions",
       x = "some variable", y = "partial (deviance) residuals")

### End Grant Code

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

### Beginning Gavins Code ###
library(DescTools)
#install.packages("DescTools")

### Getting the AIC and BIC of each Fitted Model ###

AIC(fit1)
BIC(fit1)

AIC(fit2)
BIC(fit2)

AIC(fit3.step)
BIC(fit3.step)

AIC(fit4)
BIC(fit4)

### Finding the "R Squared" for each Fitted Model ###

PseudoR2(fit1, which = c("Cox", "Nagelkerke", "McFadden"))
PseudoR2(fit2, which = c("Cox", "Nagelkerke", "McFadden"))
PseudoR2(fit3.step, which = c("Cox", "Nagelkerke", "McFadden"))
PseudoR2(fit4, which = c("Cox", "Nagelkerke", "McFadden"))

### Matt has a personal preference for McFadden and according to the output fit4 has the highest "R squared" ###

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
  #   brier score, scaled brier score , max brier score
  
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

### Calculating the Brier Score for each Fitted Model ###

brier_score(fit1)
brier_score(fit2)
brier_score(fit3.step)
brier_score(fit4)

### Finding the discrimination of each fitted Model ###
### discrimination slope = mean(p1) - mean(p0) ###

fit1D <- mean(fitted(fit1)[fit1$y == 1]) - mean(fitted(fit1)[fit1$y == 0])
fit2D <- mean(fitted(fit2)[fit2$y == 1]) - mean(fitted(fit2)[fit2$y == 0])
fit3D <- mean(fitted(fit3.step)[fit3.step$y == 1]) - mean(fitted(fit3.step)[fit3.step$y == 0])
fit4D <- mean(fitted(fit4)[fit4$y == 1]) - mean(fitted(fit4)[fit4$y == 0])

### Seeing how well our Models fit the 1's and )'s ###
# histogram of predicted probabilities by outcome
# create data frame of outcome and predicted probabilities
fit1df <- data.frame(y = fit1$y,
                     phat = fitted(fit1))
ggplot(fit1df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")

fit2df <- data.frame(y = fit2$y,
                     phat = fitted(fit2))
ggplot(fit2df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")

fit3df <- data.frame(y = fit3.step$y,
                     phat = fitted(fit3.step))
ggplot(fit1df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")

fit4df <- data.frame(y = fit4$y,
                     phat = fitted(fit4))
ggplot(fit1df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")

### c-statistic and Somers' D ###
# predicted prob goes first, outcome second
# this line of code does not work! (for Grant)
rcorr.cens(fitted(fit), fit$y)[-c(5, 6, 9)] # ignoring output i don't need



## predictions

### Scroring the new data based on Each Model Fit ###
# type = "link" will return the predicted log(odds) for each of these subjects
fit1scores <- predict(fit1, newdata = cval, type = "link")       ## Scores the new data
fit2scores <- predict(fit2, newdata = cval, type = "link") 
fit3scroes <- predict(fit3.step, newdata = cval, type = "link") 
fit4scores <- predict(fit4, newdata = cval, type = "link") 




############################################
############   ASSESSMENT  #################
############################################
#By S.Powell

final.model <- fit4


# SCORING
scores <- predict(final.model, newdata = cval, type = "response") 
summary(scores)
c_scores <- cbind(scores, cval)

# ADDING PREDICTED PROFIT, and Exploring relationship
# since we really care about money not wins
c_scores <- c_scores %>%
  mutate(pred_profit=scores*Est_profit)%>%
  mutate(realized_profit=Est_profit*Win)
# Plot of predicted profit, based on probabilities
ggplot(data= c_scores) +
  geom_point(aes(x=scores, y=pred_profit, color=as.factor(Win)))
# Plot of realized profit, knowing wins
ggplot(data= c_scores) +
  geom_point(aes(x=scores, y=realized_profit, color=as.factor(Win)))
# Plot of comparing predictions vs realized
ggplot(data= c_scores) +
  geom_point(aes(x=pred_profit, y=realized_profit, color=as.factor(Sec.names)), alpha=0.4)

#Brier score
brier_score1 <- mean((c_scores$Win - c_scores$scores)^2, na.rm=TRUE)
brier_score1

# COEFFICIENT OF DISCRIMINATION
D <- mean(scores[cval$Win == 1], na.rm = TRUE) - mean(scores[cval$Win == 0], na.rm = TRUE)
D <- round(D, 5) #rounding for plot labelling later
D

# PLOT OF DISCRIMINATION
ggplot(c_scores, aes(scores, fill = Win_Bid)) +
  geom_density(alpha = 0.2) +
  labs(title="Density Plot of Predicted Probabilities",
       subtitle=paste("Coef. of Discrimination = ",D,sep=""),
       x = "Predicted Probability of Winning Bid",
       y= "Density",
       fill = "Bid \nWon") +
  theme_minimal()

ggplot(c_scores) +
  geom_point(aes(x=Estimated_Cost__Millions_, y=scores , color = Win_Bid), alpha = 0.4) +
  labs(title="Scatter Plot of Predicted Probabilities",
       y = "Predicted Probability of Winning Bid",
       fill = "Bid \nWon") +
  theme_minimal()
ggplot(c_scores) +
  geom_point(aes(x=Number_of_Competitor_Bids, y=scores , color = Win_Bid), alpha = 0.4) +
  labs(title="Scatter Plot of Predicted Probabilities",
       y = "Predicted Probability of Winning Bid",
       fill = "Bid \nWon") +
  theme_minimal()
# Flat, even distribution for Winning bids, Great for Non-wins 
# TODO Ask Matt

# c-STATISTIC
plot(scores, cval$Win)
c.stat <- rcorr.cens(scores, cval$Win)[1]
c.stat #0.92, among all possbile pairs of bids, our model correctly assigned the higher probability of Win 92% of the time 

# BRIER SCORE
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

brier_score_garbage <- brier_score(final.model, new_x=cval, new_y = cval$Win)
brier_score_garbage #Do not trust this number, b/c is gave inconsistent results during previous Logistic HWs
brier_score1 #is more reliable

### ROC CURVES ###
# actual outcomes must be a factor (b/c considered classification?)
pred <- prediction(scores, factor(cval$Win))

# then in performance, "measure" is the y-axis, and "x.measure" is the x-axis
# for a roc curve, we want tpr vs. fpr. ("sens" and "spec" also work)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# Plot of ROC and 45-degree line to reference random guessing
plot(perf, colorize = TRUE)
abline(a = 0, b = 1, lty = 2)

# AUC
auc <- performance(pred, measure = "auc")@y.values
auc
c.stat # Equals c.stat. YAY!

### classification table ###
classif_table <- data.frame(threshold = perf@alpha.values[[1]],
                            tpr = perf@y.values[[1]],
                            tnr = 1 - perf@x.values[[1]],
                            fpr = perf@x.values[[1]])
# TODO I want to simplify this from 162 values for the report. But whatever.

#colnames(classif_table) <- c("Threshold", "True Positive Rate", "True Negative Rate", "False Positive Rate")

# YOUDEN'S INDEX: 
# added hypothetical weights for tpr (sens) and tnr (spec), but not req'd in HW
avg_profit <- mean(construction$Win_profit) #assumes all winner profits will be representative of IAA Construction profit
avg_profit
pos.value <- avg_profit # value of won bid: potential profit (in millions $) not realized 
neg.value <- .005  # cost of failed bid: $5k of marketing cost wasted
wt <- pos.value/(pos.value+neg.value) # Need to confirm math of wt...
wt

classif_table$youdenJ <- with(classif_table, (2*(wt*tpr + (1-wt)*tnr) - 1))

# find row with max
classif_table[which.max(classif_table$youdenJ),]$threshold
ideal_threshold <- round(classif_table[which.max(classif_table$youdenJ),]$threshold,3)
max.tpr <- classif_table[which.max(classif_table$youdenJ),]$tpr
max.tnr <- classif_table[which.max(classif_table$youdenJ),]$tnr
with(classif_table, plot(threshold, youdenJ)) 

# MEDIOCRE ROC PLOT
# plot(perf, colorize = TRUE)
# abline(a = 0, b = 1, lty = 2)
# points((1-max.tnr), max.tpr, type = "p", cex=1.5, col = "red", lwd = 2)
# title("ROC Curve of Validation Data")
# legend("bottomright", inset = 0.03, type(), col = c("grey","red"),
#        legend = c("Random guessing", "Ideal with assumed costs"),
#        lty = c(2,NA), pch= c(NA, 1), lwd = c(1,2))

# FINAL ROC PLOT w/ ideal threshold
roc_curve <- ggplot(classif_table) +
  geom_line(aes(x=fpr, y=tpr, color=threshold), size=2) +
  scale_color_gradient(low="green", high="red") +
  labs(x="False Positive Rate", y= "False Negative Rate", color="Pred. Prob.\nThreshold ",
       title="ROC Curve of Validation Data") +
  theme_minimal()
roc_curve

roc_curve + geom_point(x= (1-max.tnr), y= max.tpr, size=3, shape=17) +
  geom_text(x= .02+(1-max.tnr), y= -.02+max.tpr, hjust=0, vjust=1, 
            label=paste("Ideal threshold for \nassumed costs (",ideal_threshold,")"))

