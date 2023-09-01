library(MASS)
library(ISLR2)
rm(list = ls())
setwd("C:/Users/39327/Desktop/Uni/Tesi/Dataframes/Data Science Salaries 2023")
df <- read.table("ds_salaries.csv", header = TRUE, sep = ",")
sum(is.na(df$salary)) #just a rapid check

###Splitting data in three parts: model estimation, calibration set and validation set
splitting <- c(rep(1, 1500), rep(2, 1500), rep(3, 755))
set.seed(1)
splitting.sampled <- sample(splitting)
model.est <- df[splitting.sampled == 1, ]
calibration.set <- df[splitting.sampled == 2, ]
validation.set <- df[splitting.sampled == 3, ]

###Model Estimation: we will estimate a classification model (LDA) using as response the variable experience_level and
#as predictor the salary_in_usd variable
levels(factor(df$experience_level)) #check how many levels there are

#plot(model.est$salary_in_usd, col = 1:4, type = "p", pch = 19)

lda.fit <- lda(experience_level ~ salary_in_usd, data = model.est) #model est.

lda.pred <- predict(lda.fit, newdata = validation.set) #estimating over the validation data
lda.pred$class #the classes estimated
lda.pred$posterior #what we need to run conformal prediction

#Checking the error rate:
mean(lda.pred$class != validation.set$experience_level) #0.3258
#Confusion matrix:
out <- table(estimated = lda.pred$class, true = validation.set$experience_level); out
addmargins(out)

###Conformal Prediction###

#Computation of predictions over the calibration set:
lda.pred.cal <- predict(lda.fit, calibration.set)[[2]]

#Extracting the probabilities of the true class only, because we will use as score function
#s(x, y) = 1 - f(x):
lda.pred.cal <- as.data.frame(lda.pred.cal)
lda.pred.cal$trueclass <- calibration.set$experience_level
lda.pred.cal.true <- NULL
for (i in 1:nrow(calibration.set)) {
    
    if (lda.pred.cal$trueclass[i] == "EN") {
        lda.pred.cal.true[i] <- lda.pred.cal[i, 1]
    }
    else if (lda.pred.cal$trueclass[i] == "EX") {
        lda.pred.cal.true[i] <- lda.pred.cal[i, 2]
    }
    else if (lda.pred.cal$trueclass[i] == "MI") {
        lda.pred.cal.true[i] <- lda.pred.cal[i, 3]
    }
    else if (lda.pred.cal$trueclass[i] == "SE") {
        lda.pred.cal.true[i] <- lda.pred.cal[i, 4]
    }
    
}

#Scores computation:
scores <- 1 - lda.pred.cal.true

#Quantile with alpha = 0.05:
q.hat <- quantile(scores,
                  probs = ceiling((nrow(calibration.set) + 1)*(1 - 0.05))/nrow(calibration.set))

#Finally, we compute the prediction sets over the validation set:
prediction.sets <- predict(lda.fit, validation.set)[[2]]
prediction.sets <- prediction.sets >= 1 - q.hat #the logic vector of the classes in the pred. sets for each X

prediction.sets.final <- as.data.frame(prediction.sets)
prediction.sets.final[prediction.sets.final[, 1] == TRUE, 1] <- "EN"
prediction.sets.final[prediction.sets.final[, 2] == TRUE, 2] <- "EX"
prediction.sets.final[prediction.sets.final[, 3] == TRUE, 3] <- "MI"
prediction.sets.final[prediction.sets.final[, 4] == TRUE, 4] <- "SE"
colnames(prediction.sets.final) <- 1:4
View(prediction.sets.final) #final prediction sets (not logical)

#Checking the coverage on this particular sample:
sum(validation.set$experience_level == prediction.sets.final)/nrow(validation.set) #0.9364

#Let's remake the above check for R trials:
R <- 100
C_j <- NULL
for (i in 1:R) {
    
    #splitting
    splitting <- c(rep(1, 1500), rep(2, 1500), rep(3, 755))
    set.seed(i)
    splitting.sampled <- sample(splitting)
    model.est <- df[splitting.sampled == 1, ]
    calibration.set <- df[splitting.sampled == 2, ]
    validation.set <- df[splitting.sampled == 3, ]
    
    #model estimation
    lda.fit <- lda(experience_level ~ salary_in_usd, data = model.est)
    
    #computation of predictions over the calibration set:
    lda.pred.cal <- predict(lda.fit, calibration.set)[[2]]
    
    #extracting the probabilities of the true class only:
    lda.pred.cal <- as.data.frame(lda.pred.cal)
    lda.pred.cal$trueclass <- calibration.set$experience_level
    lda.pred.cal.true <- NULL
    for (j in 1:nrow(calibration.set)) {
        
        if (lda.pred.cal$trueclass[j] == "EN") {
            lda.pred.cal.true[j] <- lda.pred.cal[j, 1]
        }
        else if (lda.pred.cal$trueclass[j] == "EX") {
            lda.pred.cal.true[j] <- lda.pred.cal[j, 2]
        }
        else if (lda.pred.cal$trueclass[j] == "MI") {
            lda.pred.cal.true[j] <- lda.pred.cal[j, 3]
        }
        else if (lda.pred.cal$trueclass[j] == "SE") {
            lda.pred.cal.true[j] <- lda.pred.cal[j, 4]
        }
        
    }
    
    #scores computation:
    scores <- 1 - lda.pred.cal.true
    
    #quantile with alpha = 0.05:
    q.hat <- quantile(scores,
                      probs = ceiling((nrow(calibration.set) + 1)*(1 - 0.05))/nrow(calibration.set))
    
    #prediction sets:
    prediction.sets <- predict(lda.fit, validation.set)[[2]]
    prediction.sets <- prediction.sets >= 1 - q.hat #the logic vector of the classes in the pred. sets for each X
    
    prediction.sets.final <- as.data.frame(prediction.sets)
    prediction.sets.final[prediction.sets.final[, 1] == TRUE, 1] <- "EN"
    prediction.sets.final[prediction.sets.final[, 2] == TRUE, 2] <- "EX"
    prediction.sets.final[prediction.sets.final[, 3] == TRUE, 3] <- "MI"
    prediction.sets.final[prediction.sets.final[, 4] == TRUE, 4] <- "SE"
    
    C_j[i] <- sum(validation.set$experience_level == prediction.sets.final)/nrow(validation.set)
}
#Let's now compute the average to see the final results:
mean(C_j) #0.9480; it works!

###FSC Metric###
FSC <- function() {
i <- 0
FSC <- NULL
class <- NULL
number.obs <- NULL
for (p in levels(factor(validation.set$experience_level))) {
    i <- i + 1
    set <- as.data.frame(prediction.sets.final[validation.set$experience_level == p, ])
    FSC[i] <- sum(validation.set$experience_level[validation.set$experience_level == p] == set)/nrow(set)
    number.obs[i] <- nrow(set)
    class[i] <- p
}
results <<- data.frame(FSC, number.obs, class)
}
FSC()

###SSC Metric###
SSC <- function() {
    bins <- NULL
    for (i in 1:nrow(prediction.sets)) {
        bins[i] <- sum(prediction.sets[i, ]) #it sums the TRUE values for each row
    }
    bins <- factor(bins)
    j <- 0
    number.obs <- NULL
    actual.bin <- NULL
    SSC <- NULL
    for(p in levels(bins)) {
        j <- j + 1
        set <- as.data.frame(prediction.sets.final[bins == p, ])
        SSC[j] <- sum(validation.set$experience_level[bins == p] == set)/nrow(set)
        number.obs[j] <- nrow(set)
        actual.bin[j] <- p
    }
    
results <<- data.frame(SSC, number.obs, actual.bin)
}
SSC()

#####Exporting to Latex#####
library(xtable)
print(xtable(prediction.sets[1:10, ], type = "latex"), file = "prediction_sets.tex")
print(xtable(prediction.sets.final[1:10, ], type = "latex"), file = "prediction_sets_final.tex")
print(xtable(results, type = "latex"), file = "FSC_results.tex")
print(xtable(results, type = "latex"), file = "SSC_results.tex")
###
lda.fit$prior #we see that some populations are way smaller than others: we must adjust for this
#to reduce the error rate

#Weighting:
weights <- 1 - lda.fit$prior
lda.fit <- lda(experience_level ~ salary_in_usd, data = model.est, prior = c(0.25, 0.25, 0.25, 0.25))

lda.pred <- predict(lda.fit, newdata = validation.set) #estimating over the validation data
lda.pred$class #the classes estimated
lda.pred$posterior
lda.pred$x

#Checking the error rate:
mean(lda.pred$class != validation.set$experience_level) #0.3258
#Confusion matrix:
out <- table(estimated = lda.pred$class, true = validation.set$experience_level); out
addmargins(out)
