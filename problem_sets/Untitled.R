getwd()

# Set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

blah <- read.csv("incumbents_subset.csv")

# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("readr", "ggplot2", "dplyr", "viridis", "foreign"),  pkgTest)

# Get working directory
getwd()

# Set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Incumbent <- read.csv("../../datasets/incumbents_subset.csv")
View(Incumbent)
#Question 1:
VoteSharexDiffLog <- lm(Incumbent$voteshare ~ Incumbent$difflog)
plot(Incumbent$difflog, Incumbent$voteshare,
     col = "turquoise",
     main = "Difference in Campagain Spending x Vote Share",
     xlab = "Difference in Campaign Spending", ylab = "Incumbent's Vote Share"
     ) + abline(VoteSharexDiffLog)
ResidualsQ1 <- resid(VoteSharexDiffLog)
residual1 <- VoteSharexDiffLog$residuals
VoteSharexDiffLog
residual1
ResidualsQ1



#Question 2
PresvoteXDiffLogg <- lm(Incumbent$presvote ~ Incumbent$difflog)
plot(Incumbent$difflog, Incumbent$presvote,
     col = "pink",
     main = "Difference in Campagain Spending x Pres Vote Share",
     xlab = "Difference in Campaign Spending", ylab = "Presidential Canidate's Vote Share") + abline(PresvoteXDiffLogg)
ResidualsQ2 <- resid(PresvoteXDiffLogg)
residuals2 <- PresvoteXDiffLogg$residuals
PresvoteXDiffLogg

#Question 3
PresvoteXVoteShare <- lm(Incumbent$voteshare ~ Incumbent$presvote)
plot(Incumbent$presvote, Incumbent$voteshare,
     col = "lightgreen",
     main = "Difference in Campagain Spending x Pres Vote Share",
     xlab = "Difference in Campaign Spending", ylab = "Presidential Canidate's Vote Share"
) + abline(PresvoteXVoteShare)
PresvoteXVoteShare
#Prediction Equation:

#Question 4:
Residuals <- lm(residual1 ~ residuals2)
plot(residuals2, residual1,
     col = "lightblue",
     main = "Title",
     xlab = "Residual from Question 2", ylab = "Residual from Question 1"
) + abline(Residuals)
Residuals



#Question 5:
q5 <- lm(Incumbent$voteshare ~ Incumbent$difflog + Incumbent$presvote)
q5
#in the question 5 mutlivariate regression the coeffcient for presidential popularity and 