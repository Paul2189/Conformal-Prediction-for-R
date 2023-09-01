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



###Estimating the model: I'm using logistic regression on the employee_residence variable
#creating dummy variable:
model.est$residence <- ifelse(model.est$employee_residence == "US", "US", "abroad")
validation.set$residence <- ifelse(validation.set$employee_residence == "US", "US", "abroad")
#estimating the model
library(glmnet)
model <- glm(I(residence) == "US" ~ salary_in_usd, data = model.est, family = "binomial")
summary(model) #p-value is stat. sign.!
model$coefficients
#let's check the accuracy on the validation set:
accuracy.check <- rep("abroad", nrow(validation.set))
model.prob <- predict(model, newdata = validation.set, type = "response")
accuracy.check[model.prob > 0.5] <- "US"
out <- table(estimated = accuracy.check, true = validation.set$residence); out #results
addmargins(out)
mean(accuracy.check != validation.set$residence) #overall error rate: 14.17%
81/161 #I type error: 50.31%
27/594 #II type error: 4.55%
library(pROC)
roc <- roc(validation.set$residence, model.prob, plot = T) #ROC curve plot
auc(roc) #0.8619



###Conformal Prediction Application###

#Score Function setting: I will use s(x,y) = 1 - f(x), where f(x) is the
#predicted probability of the true class.
model.prob #These are the probabilities that the residence is US for each observation.
#We will compute them again but using the calibration set:
calibration.prob <- predict(model, newdata = calibration.set, type = "response")
#Now i create the score function vector:
score.function <- 1 - calibration.prob #higher values mean lower confidence of the model and viceversa

#Quantile calculation; i choose alpha = 0.05.
q.hat <- quantile(score.function, probs = 
             ceiling((nrow(calibration.set) + 1)*(1 - 0.05))/nrow(calibration.set))

#Now we can finally form prediction sets (using validation data):
validation.probs.us <- predict(model, newdata = validation.set, type = "response")
validation.probs.abroad <- 1 - validation.probs.us
prediction.sets <- matrix(NA, nrow = nrow(validation.set), ncol = 2)
colnames(prediction.sets) <- c("US", "abroad")
prediction.sets[, 1] <- validation.probs.us >= 1 - q.hat 
prediction.sets[, 2] <- validation.probs.abroad >= 1 - q.hat
View(prediction.sets) #These are the prediction sets; when both columns in a certain row
#have TRUE, it means that both probabilities for each class are bigger than
#the threshold required to be in the predicted set.

#Check the coverage for this particular sample:
prediction.sets2 <- prediction.sets
prediction.sets2[prediction.sets2[, 1] == TRUE, 1] <- "US"
prediction.sets2[prediction.sets2[, 2] == TRUE, 2] <- "abroad"
View(prediction.sets2)

sum(validation.set$residence == prediction.sets2)/nrow(validation.set) #0.9272% coverage with this
#particular sample.

#Let's remake the above check for R trials:
R <- 100
C_j <- NULL
for (i in 1:R) {
    
    #Sampling:
    splitting <- c(rep(1, 1500), rep(2, 1500), rep(3, 755))
    set.seed(i)
    splitting.sampled <- sample(splitting)
    model.est <- df[splitting.sampled == 1, ]
    calibration.set <- df[splitting.sampled == 2, ]
    validation.set <- df[splitting.sampled == 3,]
    
    #dummy creation:
    model.est$residence <- ifelse(model.est$employee_residence == "US", "US", "abroad")
    validation.set$residence <- ifelse(validation.set$employee_residence == "US", "US", "abroad")
    
    #estimating the model:
    library(glmnet)
    model <- glm(I(residence) == "US" ~ salary_in_usd, data = model.est, family = "binomial")
    
    #calibration probabilities to be used for the score function:
    calibration.prob <- predict(model, newdata = calibration.set, type = "response")
    
    #Now i create the score function vector:
    score.function <- 1 - calibration.prob
    
    #Quantile calculation; i choose alpha = 0.05.
    q.hat <- quantile(score.function, probs = 
                          ceiling((nrow(calibration.set) + 1)*(1 - 0.05))/nrow(calibration.set))
    
    #Now we can finally form prediction sets (using validation data):
    validation.probs.us <- predict(model, newdata = validation.set, type = "response")
    validation.probs.abroad <- 1 - validation.probs.us
    prediction.sets <- matrix(NA, nrow = nrow(validation.set), ncol = 2)
    colnames(prediction.sets) <- c("US", "abroad")
    prediction.sets[, 1] <- validation.probs.us >= 1 - q.hat 
    prediction.sets[, 2] <- validation.probs.abroad >= 1 - q.hat
    
    #Check the coverage for this particular sample:
    prediction.sets2 <- prediction.sets
    prediction.sets2[prediction.sets2[, 1] == TRUE, 1] <- "US"
    prediction.sets2[prediction.sets2[, 2] == TRUE, 2] <- "abroad"
    
    C_j[i] <- sum(validation.set$residence == prediction.sets2)/nrow(validation.set)
}
mean(C_j) #mean of all C_js; it should be around 0.95, in fact it is 0.9466!

