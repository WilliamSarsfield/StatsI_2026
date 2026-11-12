#####################
# load libraries
# set wd
# clear global .envir
#####################


# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")
#Question 1:
VoteSharexDiffLog <- lm(inc.sub$voteshare ~ inc.sub$difflog)
stargazer(VoteSharexDiffLog)
pdf("Question1Plot.pdf")
plot(inc.sub$difflog, inc.sub$voteshare,
     col = "turquoise",
     main = "Scatterplot of Incumbent's Vote Share and Difference in Campaign Spending",
     xlab = "Logarithmic Difference in Campaign Spending", 
     ylab = "Incumbent's Vote Share"
) + abline(VoteSharexDiffLog)
dev.off()
residual1 <- VoteSharexDiffLog$residuals

#Question 2
PresvoteXDiffLogg <- lm(inc.sub$presvote ~ inc.sub$difflog)
stargazer(PresvoteXDiffLogg)
pdf("Question2Plot.pdf")
plot(inc.sub$difflog, inc.sub$presvote,
     col = "pink",
     main = "Scatterplot of Pres Candidate Vote Share and Difference in Campaign Spending",
     xlab = "Logarithmic Difference in Campaign Spending", 
     ylab = "Presidential Candidate's Vote Share") + abline(PresvoteXDiffLogg)
dev.off() 
residuals2 <- PresvoteXDiffLogg$residuals

#Question 3
PresvoteXVoteShare <- lm(inc.sub$voteshare ~ inc.sub$presvote)
stargazer(PresvoteXVoteShare)
pdf("Question3Plot.pdf")
plot(inc.sub$presvote, inc.sub$voteshare,
     col = "lightgreen",
     main = "Scatterplot of Incumbent's Vote Share and Pres Candidate Vote Share",
     xlab = "Presidential Candidate's Vote Share",
     ylab = "Incumbent's Vote Share")  + abline(PresvoteXVoteShare)
dev.off()
library(stargazer)

#Question 4:
Residuals <- lm(residual1 ~ residuals2)
stargazer(Residuals)
sprintf("%f", 2.569e-01)
sprintf("%f", -1.942e-18)
pdf("Question4Plot.pdf")
plot(residuals2, residual1,
     col = "lightblue",
     main = "Scatterplot of the Residuals from Q1 and Q2",
     xlab = "Residual from Question 2", 
     ylab = "Residual from Question 1") + abline(Residuals)
dev.off()


View(inc.sub)
#Question 5:
MReg<- lm(inc.sub$voteshare ~ inc.sub$difflog + inc.sub$presvote)
stargazer(MReg)


