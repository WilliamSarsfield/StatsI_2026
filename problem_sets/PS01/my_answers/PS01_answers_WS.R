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
TStat * seY
meanY
lowerbound <- meanY - TStat * seY 
upperbound <- meanY + TStat * seY
CI <- c(lowerbound, upperbound)
cat("Confidence Interval is:", CI, "Mean is:", meanY, "Standard Error is:", seY, "Critical Value is:", TStat)
#Null Hypothesis Student IQ =< 100
#Alternative Hypothesis Student IQ > 100
tteststat <- (meanY - 100) / seY
tteststat
df <- length(y) - 1
pvalue <- pt(abs(tteststat), df = df, lower.tail=TRUE)
pvalue
cat("PValue is:", pvalue, "T-test statistic is:", tteststat)
#I fail to Reject the Null Hypothesis as the p-value is much greater than .05 
#We Fail To Reject the Null Hypothesis

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)
library(ggplot2)
View(expenditure)
#Question 1
pdf("Scatterplot.pdf")
pairs(~ Y + X1 + X2 + X3, data = expenditure, col = "blue", upper.panel = NULL) +
  title(main = "Scatterplot of the Relationship Between Variables", line = 3)
dev.off()



#Question 2
#Renaming
library(dplyr)
expenditure$NamedRegions = case_when(
  expenditure$Region == 1 ~ "Northeast",
  expenditure$Region == 2 ~ "North Central",
  expenditure$Region == 3 ~ "South",
  expenditure$Region == 4 ~ "West", 
)

pdf("Boxplot.pdf")
ggplot(expenditure, aes(x=factor(Region), y=Y, colour = factor(NamedRegions))) +
  geom_boxplot() +
  scale_color_manual(values = c("red", "violet", "blue", "orange")) +
  labs(title = "Expenditure on Housing and Shelter by Region", x = "Region", y = "Expenditure per Capita", colour = "Region") 
dev.off()



library(tidyverse)
#Question 3
pdf("Shelter.pdf")
ggplot(expenditure, aes(x=Y, y=X1)) +
  geom_point(alpha = .5) +
  labs(title = "Expenditure on Shelters and Housing by Personal Income in a State",
       x = "Per Capita Expenditure on Shelters And Housing Assistance In State", 
       y = "Per Capita Personal Income In State") +
  geom_smooth(method = lm, se = FALSE, colour = "purple")
dev.off()
pdf("ShelterRegion.pdf")
ggplot(expenditure, aes(x = Y, y= X1, colour = factor(NamedRegions), shape = factor(NamedRegions))) +
  geom_point(size = 3) +
  labs(
    x = "Per Captia Expenditure on Shelters and Housing Assistance in a State", 
    y = "Per Capita Personal Income in State",
    colour = "NamedRegions",
    shape = "NamedRegions",
    title = "Expenditure on Assistance x Personal Income x Region") + 
  scale_color_manual(values = c("red", "violet", "blue", "orange"))
  theme_minimal()
dev.off()

