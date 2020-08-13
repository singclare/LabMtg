#directory
getwd()
setwd("G:/LabMeeting")
getwd()

#Data
#install.packages("car")
library(car)
head(Duncan)

##ggplot##
#install.packages("ggplot2")
library(ggplot2)
ggplot(Duncan, aes(x = education, y = income)) + 
  geom_point() + stat_smooth(method = "lm", col = "red") + theme_minimal() + 
  ggtitle("Income vs. education")

##Cook's barplot##
#install.packages("bindrcpp")
#install.packages("olsrr")
library(bindrcpp)
library(olsrr)
fitLS <- lm(income ~ education, data = Duncan)
ols_plot_cooksd_bar(fitLS)

##Robust Method Creation##
install.packages("quantreg")
library(MASS)

#LMS#
fitLMS <- lqs(income ~ education, data = Duncan, method = "lms")
fitLTS <- lqs(income ~ education, data = Duncan, method = "lts")
fitLAD <- rq(income ~ education, data = Duncan, tau = 0.5) #tau = desired quantile

##Plot##
plot(Duncan$education, Duncan$income, 
     xlab = "education", ylab = "income", type = "p",
     pch = 20, cex = .8)
abline(fitLS, col = 1) #Ordinal Least Squares
abline(fitLMS, col = 2) #Least Median Squares
abline(fitLTS, col = 3) #Least Trimmed Squares 
abline(fitLAD, clo = 4) #Least Absolute Squares
legend(0, 80, c("LS", "LMS", "LTS", "LAD" ),
       lty = rep(2,7), bty = "n",
       col = c(1, 2, 3, 4))

##Precision of the methods##
training_rows <- sample(1:nrow(Duncan), 0.8*nrow(Duncan))
training_data <- Duncan[training_rows, ]
test_data <- Duncan[-training_rows, ]

lm_Predicted <- predict(fitLS, test_data)
rob_Predicted <- predict(fitLTS, test_data)

lm_actuals_pred <- cbind(lm_Predicted, test_data$income)
rob_actuals_pred <- cbind(rob_Predicted, test_data$income)

mean(apply(lm_actuals_pred, 1, min)/apply(lm_actuals_pred, 1, max))

##Election of the best robust regression method##
#install.packages("robustbase")
library(robustbase)
OLS_R2 <- summary(fitLS)$r.squared #0.5249182
summary(fitLS)$coefficients[,4]
fitLTS <- ltsReg(Duncan$education, Duncan$income)
LTS_R2 <- summary(fitLTS)$r.squared #0.7578587
summary(fitLTS)$coefficients[,4]
LTS_R2 / OLS_R2 #1.443765
LTS_R2 - OLS_R2 #0.2329406
#The robust method improves by a 23%(R^2 = 0.75)
