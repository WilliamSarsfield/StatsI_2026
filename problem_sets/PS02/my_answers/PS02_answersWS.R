#Part 1
observed <- c(14,7,6,7,7,1)
total <- sum(observed)
m <- matrix(c(14,7,6,7,7,1), nrow = 2)
sum(m)
SumofRow <- rowSums(m)
SumofRow[1]
SumofCol <- colSums(m)
expectedvalues = vector("double", length = length(observed))
expectedvalues <- c(((SumofRow[1]/total)*SumofCol[1]), ((SumofRow[2]/total)*SumofCol[1]), 
                       ((SumofRow[1]/total)*SumofCol[2]), ((SumofRow[2]/total)*SumofCol[2]),
                       ((SumofRow[1]/total)*SumofCol[3]), ((SumofRow[2]/total)*SumofCol[3]))
xsquared <- c((((observed[1] - expectedvalues[1])^2)/expectedvalues[1]), 
                  (((observed[2] - expectedvalues[2])^2)/expectedvalues[2]),
                  (((observed[3] - expectedvalues[3])^2)/expectedvalues[3]),
                  (((observed[4] - expectedvalues[4])^2)/expectedvalues[4]),
                  (((observed[5] - expectedvalues[5])^2)/expectedvalues[5]), 
                  (((observed[6] - expectedvalues[6])^2)/expectedvalues[6]))
xstat <- sum(xsquared)
cat("The X^2 statistic is:", xstat)

#Part 2
df <- 2 * 1
Pvalue <- pchisq(xstat, df, lower.tail=FALSE)
cat("The p-value from the test stat is:", Pvalue)


#Part 3 
standardizedresiduals <- c((observed[1] - expectedvalues[1]) / 
                             (sqrt((expectedvalues[1])*(1-(SumofRow[1]/total))*(1-(SumofCol[1]/total)))),
                           (observed[2] - expectedvalues[2]) / 
                             (sqrt((expectedvalues[2])*(1-(SumofRow[2]/total))*(1-(SumofCol[1]/total)))),
                           (observed[3] - expectedvalues[3]) / 
                             (sqrt((expectedvalues[3])*(1-(SumofRow[1]/total))*(1-(SumofCol[2]/total)))),
                           (observed[4] - expectedvalues[4]) / 
                             (sqrt((expectedvalues[4])*(1-(SumofRow[2]/total))*(1-(SumofCol[2]/total)))),
                           (observed[5] - expectedvalues[5]) / 
                             (sqrt((expectedvalues[5])*(1-(SumofRow[1]/total))*(1-(SumofCol[3]/total)))),
                           (observed[6] - expectedvalues[6]) / 
                             (sqrt((expectedvalues[6])*(1-(SumofRow[2]/total))*(1-(SumofCol[3]/total)))))
cat("the standardized residuals are:", standardizedresiduals)


data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
data$GP <- NULL
data$village <- NULL
data$female <- NULL
data$irrigation <- NULL
pdf("FirstPlot.pdf")
plot(data$reserved, data$water,
     main = "Scatterplot of the Relationship", 
     xlab = "Reservation Policy", ylab = "Repaired or New Drinking Water Facilities")
dev.off()
#to get initial numbers:
model <- lm(data$water ~ data$reserved)
model
pdf("SecondPlot.pdf")
plot(data$reserved, data$water,
     main = "Scatterplot with Regression Line", 
     xlab = "Reservation Policy", ylab = "Repaired or New Drinking Water Facilities") +
  abline(model)
dev.off()
#correlation test:
r <- cov(data)[1,2] / (sd(data[,1])*sd(data[,2]))
cor.test(data[,1], data[,2])
correlationpvalue <- .0197
cat("the correlation coefficient is found to be:", r, 
    "\n", "the correlations p-value is:", correlationpvalue, 
    "\n", "allowing me to reject the null that reservation policy and", 
    "\n", "repaired or new drinking water facilities are not correlated")
#manual testing:
slope <- sum((data$water - mean(data$water)) *
               (data$reserved - mean(data$reserved))) /
                 sum((data$reserved - mean(data$reserved))^2)
reserved <- data$reserved
water <- data$water
intercept <- mean(water) - slope * mean(reserved)
cat("Manually I found the slope and the intercept. The slope is:", slope, "\n", "and the intercept is:", intercept)
sd_estimate <- sqrt(sum(resid(model)^2)/(dim(data)[1]-2))
sigma <- sigma(model)
cat("sd estimate is:", sd_estimate, "double checked with sigma(model) which got", sigma)
slopese <- sd_estimate/sqrt(sum((reserved - mean(reserved))^2))
interceptse <- sd_estimate * sqrt((1/dim(data)[1]) + 
                                    (mean(reserved)^2/sum((reserved-mean(reserved))^2)))
statsforSlope <- 2*pt((slope - 0) / slopese, dim(data)[1]-2, lower.tail = F)
statsforInt <- 2*pt((intercept - 0) / interceptse, dim(data) [1]-2, lower.tail = F)
cat("Manually I found the pvalue for the slope and the intercept", 
    "\n", "the B1 p-value is:", statsforSlope, 
    "\n", "and the B0 or alpha's p-value is:", statsforInt)
#double checking 
summary(model)
#confidence interval for the slope 
tscore <- qt(.95, 320)
CIupper <- slope + tscore * slopese
CIlower <- slope - tscore * slopese
CI <- c(CIlower, CIupper)
cat("The confidence interval for the slope is:", CI)
rsquared <- r * r
cat("The amount of variability in repaired or new drinking water", 
    "\n", "facilities explained by reservation policy is:", rsquared,
    "\n", "or better stated as 1.7%")


