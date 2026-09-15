# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

#############
# Basic stats
#############

# create vector y
y <- c(0, 4, 4, 5, 7, 10)

# (1) find sum of y using the built-in R function
sumofy <- sum(c(y))
sumofy
# (2) find mean of y using your "own" function
# now do the same thing, but faster using the built-in R function
mean(sumofy)
# (3) find sum of demeaned values
demeanedSum <- NULL
for(i in 1:length(y)){
  demeanedSum[i] <- y[i] - mean(y)
}
demeanedSumSimple <- y - mean(y)
sum(demeanedSumSimple)
# (4) calculate sum of squared error

squareError <- demeanedSum^2
sum(squareError)

###########
# Quantiles
###########

# create vector
quantilesVec <- c(55, 84, 65, 54, 61, 67, 80, 59, 81, 82)

# (1) calculate median 
median(quantilesVec)
# (2) calculate quantiles
quantile(quantilesVec)
boxplot(quantilesVec, col = "blue")
# (3) make a histogram of state median income
pdf("HistogramofState.pdf", width = 10, height = 10)
hist(state.x77[,2], main = "Histogram of state", xlab = "States", ylab = "Frequency", col = "lightblue")
dev.off()


# remember to save your plot as a pdf