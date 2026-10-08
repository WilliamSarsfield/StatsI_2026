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

lapply(c("stringr"),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
meanY <- mean(y)
stdevY <- sd(y)
students <- length(y)
seY <- stdevY/sqrt(students)
TStat <- qt(0.95, 24)
lowerbound <- meanY - TStat * seY 
upperbound <- meanY + TStat * seY
CI <- c(lowerbound, upperbound)
CI
qt(.95, 24)
qt(p = .05, 24)
#Null Hypothesis Student IQ =< 100
#Alternative Hypothesis Student IQ > 100
tteststat <- (meanY - 100) / seY
tteststat
df <- length(y) - 1
pvalue <- 2*pt(abs(tteststat), df = df, lower.tail=FALSE)
pvalue
#I fail to Reject the Null Hypothesis as the p-value is greater than .05 
t.test(y, mu = 100)
#We Fail To Reject the Null Hypothesis

#####################
# Problem 2
#####################


#Question 1
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)
library(ggplot2)
#Question 1
pdf("Scatterplot of the Variables.pdf") 
pairs(~ Y + X1 + X2 + X3, data = expenditure, col = "blue",
      upper.panel = NULL) +
  title("Scatterplot of the Variables", sub = NULL, xlab = NULL, line = 3)
dev.off()




#Question 2
#Renaming
library(dplyr)
expenditure$Region = case_when(
  expenditure$Region == 1 ~ "Northeast",
  expenditure$Region == 2 ~ "Northcenteral",
  expenditure$Region == 3 ~ "South",
  expenditure$Region == 4 ~ "West",
  )
View(expenditure)
pdf("Boxplot.pdf")
ggplot(expenditure, aes(x=factor(Region), y=Y, colour = factor(Region))) +
  geom_boxplot() +
  scale_color_manual(values = c("red", "violet", "blue", "orange")) +
  labs(title = "Expenditure on Housing and Shelter by Region", x = "Region", y = "Expenditure per Capita") 
dev.off()
library(tidyverse)
#Question 3
pdf("Shelter.pdf")
ggplot(expenditure, aes(x=Y, y=X1)) +
  geom_point(alpha = .5, fill = "pink") +
  labs(x = "Per Capita Expenditure on Shelters And Housing Assistance In State", y = "Per Capita Personal Income In State") +
  geom_smooth(method = lm, se = FALSE, colour = "purple")
dev.off()
pdf("ShelterRegion.pdf")
ggplot(expenditure, aes(x=Y, y=X1, colour = factor(Region), shape = factor(Region))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "violet", "lightblue", "cyan")) 
  labs(
    x = "Per Captia Expenditure on Shelters and Housing Assistance in a State", 
    y = "Per Capita Personal Income in State",
    colour = "Region",
    shape = "Region",
    title = "Expenditure on Shelters and Housing by Personal Income in a State") +
  theme_minimal()
dev.off()
 
