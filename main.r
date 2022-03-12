setwd("/Users/zhaojiaqi/Desktop/Prev course/STAT306/project")
getwd()
D <- read.csv("data5.csv",sep=",",header=T)
library(leaps)
### CHANGE Market_capitalization_of_companies to Market !!!
s <- regsubsets(Market~.+Income*Education, data=D, method="exhaustive")
ss <- summary(s)
ss$which
ss$adjr2
ss$cp


#adj_R2 gives size 4, Agriculture + Education + Income + Education*Income,
#cp (closet to p) gives size 4
#cp (smallest) gives size 4

fit4 <- lm(Market~. + Education*Income - Industry,data=D)
summary(fit4)
AIC(fit4)

#check models by a different method
s <- regsubsets(Market~.+Income*Education, data=D, method="forward")
ss <- summary(s)
ss$which
ss$adjr2
ss$cp
#same result, size 4

s <- regsubsets(Market~.+Industry*Agriculture+Industry*Education+
                  Industry*Income+Agriculture*Education+
                  Agriculture*Income+Education*Income, data=D, method="exhaustive")
ss <- summary(s)
ss$which
ss$adjr2
ss$cp
#adj_R2 gives size 5, Agriculture + Education + Income + Education*Income + Agriculture*Income
#cp (closet to p) gives size 8, Industry*Agriculture+Industry*Education+Industry*Income+Agriculture*Education+Agriculture*Income+Education*Income
#cp (smallest) gives size 3, Agriculture + Income + Agriculture*Income

#now compare above 4 models.
#size 4, size 5, size 8, size 3

error_model4 <-0
for(i in 1:nrow(D)){
  test <- D[i,]
  train <- D[-i, ]
  model4 <- lm(Market~. + Education*Income - Industry,data=train)
  error_model4 <- error_model4 + sum((test$Market - predict(model4, test))^2)/nrow(D)
}
error_model4

error_model5 <-0
for(i in 1:nrow(D)){
  test <- D[i,]
  train <- D[-i, ]
  model5 <- lm(Market ~ Agriculture + Education + Income + Education*Income + Agriculture*Income, data=train)
  error_model5 <- error_model5 + sum((test$Market - predict(model5, test))^2)/nrow(D)
}
error_model5

error_model3 <-0
for(i in 1:nrow(D)){
  test <- D[i,]
  train <- D[-i, ]
  model3 <- lm(Market ~ Agriculture + Income + Agriculture*Income, data=train)
  error_model3 <- error_model3 + sum((test$Market - predict(model3, test))^2)/nrow(D)
}
error_model3

error_model8 <-0
for(i in 1:nrow(D)){
  test <- D[i,]
  train <- D[-i, ]
  model8 <- lm(Market ~ Industry*Agriculture+Industry*Education+Industry*Income+Agriculture*Education+Agriculture*Income+Education*Income, data=train)
  error_model8 <- error_model8 + sum((test$Market - predict(model8, test))^2)/nrow(D)
}
error_model8

#model3 better 

#additional diagnoses

ls.diag(model3)$std.res
ls.diag(model3)$cooks


#QQ plot
qqnorm(model3$residuals,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(model3$residuals)

#should take log of y

model3_log <- lm(log(Market) ~ Agriculture + Income + Agriculture*Income, data=train)
summary(model3_log)
AIC(model3_log)
#adjR2 of old model3 is 0.4911, AIC of the old is 2360.676, new model3 is better

s <- regsubsets(log(Market)~.+Income*Education, data=D, method="exhaustive")
ss <- summary(s)
ss$which
ss$adjr2
ss$cp
#log_model4 is better...

s <- regsubsets(log(Market)~.+Income*Education, data=D, method="forward")
ss <- summary(s)
ss$which
ss$adjr2
ss$cp
#different method but results are the same, log_model4 is better...

#compare model3_log and model4_log
error_model3_log <-0
for(i in 1:nrow(D)){
  test <- D[i,]
  train <- D[-i, ]
  model3_log <- lm(log(Market) ~ Agriculture + Income + Agriculture*Income, data=train)
  error_model3_log <- error_model3_log + sum((test$Market - predict(model3_log, test))^2)/nrow(D)
}
error_model3_log

error_model4_log <-0
for(i in 1:nrow(D)){
  test <- D[i,]
  train <- D[-i, ]
  model4_log <- lm(log(Market) ~ Agriculture + Education + Income + Education*Income, data=train)
  error_model4_log <- error_model4_log + sum((test$Market - predict(model5, test))^2)/nrow(D)
}
error_model4_log
#model4_log better 

#check QQ plot of model4_log
qqnorm(model4_log$residuals, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(model4_log$residuals)
#looks OK


plot(model4_log$fitted.values, model4_log$residuals, xlab="Fitted values", ylab="Residuals",main=" ")
#two clusters in fitted vs residual plot; indication of some underlying and unaccounted for variable?

plot(log(D[-1, ]$Market), model4_log$residuals, xlab="log_Y values", ylab="Residuals",main=" ")
#looks OK

#SO "log(Market) ~ Agriculture + Education + Income + Education*Income" IS THE FINAL MODEL

         