library(quantreg)
rm(list = ls())
setwd("C:/Users/39327/Desktop/Uni/Tesi/Dataframes/Diamonds")
df <- read.table("diamonds.csv", header = TRUE, sep = ",")

###Splitting data in three parts: model estimation, calibration set and validation set
splitting <- c(rep(1, 25000), rep(2, 25000), rep(3, 3940))
set.seed(1)
splitting.sampled <- sample(splitting)
model.est <- df[splitting.sampled == 1, ]
calibration.set <- df[splitting.sampled == 2, ]
validation.set <- df[splitting.sampled == 3, ]

###Fitting the model###
alpha <- 0.05
qregr <- rq(price ~ ., data = model.est, tau = c(alpha/2, (1 - alpha/2)))
summary(qregr) #regression is quite good for both quantiles
qregr$fitted.values #is it ok that some of the 0.05 quantile are negative???

###Score function###
calibration.pred <- predict(qregr, calibration.set) #predicting on the calib. set
scores <- NULL
for (i in 1:nrow(calibration.set)) {
scores[i] <- max(calibration.pred[i, 1] - calibration.set$price, calibration.set$price - calibration.pred[i, 2])
}

###Quantile computing###
q.hat <- quantile(scores,
                  ceiling((nrow(calibration.set) + 1) * (1 - alpha))/nrow(calibration.set))

###Computing prediction intervals###
validation.pred <- predict(qregr, validation.set) #predicting on the valid. set
prediction.intervals <- matrix(NA, nrow(validation.set), 2)
prediction.intervals[, 1] <- validation.pred[, 1] - q.hat
prediction.intervals[, 2] <- q.hat + validation.pred[, 2]

###Coverage check###
check <- data.frame(validation.pred[, 1], validation.set$price, validation.pred[, 2])
colnames(check) <- c("lower_bound", "real_values", "upper_bound")
counter <- NULL
for (i in 1:nrow(check)) {
    if (check$real_values[i] >= check$lower_bound[i] & 
        check$real_values[i] <= check$upper_bound[i]) {
        counter[i] <- TRUE
    }
    else {
        counter[i] <- FALSE
    }
}
sum(counter)/nrow(check) #0.9503

###Let's remake the above check for R trials: IT TAKES AROUND 10 MINUTES WITH R = 100
R <- 100
C_j <- NULL
for (j in 1:R) {
    
    #splitting
    splitting <- c(rep(1, 25000), rep(2, 25000), rep(3, 3940))
    set.seed(j)
    splitting.sampled <- sample(splitting)
    model.est <- df[splitting.sampled == 1, ]
    calibration.set <- df[splitting.sampled == 2, ]
    validation.set <- df[splitting.sampled == 3, ]
    
    #model fitting
    alpha <- 0.05
    qregr <- rq(price ~ ., data = model.est, tau = c(alpha/2, (1 - alpha/2)))
    
    #scores
    calibration.pred <- predict(qregr, calibration.set)
    scores <- NULL
    for (i in 1:nrow(calibration.set)) {
        scores[i] <- max(calibration.pred[i, 1] - calibration.set$price, calibration.set$price - calibration.pred[i, 2])
    }
    
    #quantile
    q.hat <- quantile(scores,
                      ceiling((nrow(calibration.set) + 1) * (1 - alpha))/nrow(calibration.set))
    
    #prediction intervals
    validation.pred <- predict(qregr, validation.set)
    prediction.intervals <- matrix(NA, nrow(validation.set), 2)
    prediction.intervals[, 1] <- validation.pred[, 1] - q.hat
    prediction.intervals[, 2] <- q.hat + validation.pred[, 2]
    
    #checking coverage
    check <- data.frame(validation.pred[, 1], validation.set$price, validation.pred[, 2])
    colnames(check) <- c("lower_bound", "real_values", "upper_bound")
    counter <- NULL
    for (i in 1:nrow(check)) {
        if (check$real_values[i] >= check$lower_bound[i] & 
            check$real_values[i] <= check$upper_bound[i]) {
            counter[i] <- TRUE
        }
        else {
            counter[i] <- FALSE
        }
    }
    C_j[j] <- sum(counter)/nrow(check)
}
#Let's now compute the average to see the final results:
mean(C_j) #0.9487; it works!!!

###FSC Metric###
range(validation.set$price)
#we divide the vector price into different categories based on the vector elements dimension
price.cut <- cut(validation.set$price, c(0, 500, seq(1000, 19000, by = 1000)))

FSC <- function() {
    j <- 0
    FSC <- NULL
    class <- NULL
    number.obs <- NULL
    
    for (p in levels(price.cut)) {
        j <- j + 1
        check <- data.frame(validation.pred[price.cut == p, 1],
                            validation.set$price[price.cut == p],
                            validation.pred[price.cut == p, 2])
        colnames(check) <- c("lower_bound", "real_values", "upper_bound")
        counter <- NULL
        
        for (i in 1:nrow(check)) {
            if (check$real_values[i] >= check$lower_bound[i] & 
                check$real_values[i] <= check$upper_bound[i]) {
                counter[i] <- TRUE
            }
            else {
                counter[i] <- FALSE
            }
        }
        
        FSC[j] <- sum(counter)/nrow(check)
        class[j] <- p
        number.obs[j] <- nrow(check)
    }
    results <<- data.frame(FSC, number.obs, class)
}

FSC()

#####Exporting to Latex#####
library(xtable)
print(xtable(results, type = "latex"), file = "FSC_continuous.tex")


