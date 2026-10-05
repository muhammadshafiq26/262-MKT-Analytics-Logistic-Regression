myData <- read.csv("marketing_logistic_data.csv")
str(myData)
sum(is.na(myData))


lpmModel <- lm(buy ~ spend + visits + age + tenure,
               data = myData)
flextable::as_flextable(lpmModel)


logModel <- glm(buy ~ spend + visits + age + tenure,
                data = myData,
                family = 'binomial')
exp(coef(logModel))
(1.01480459-1)*100
(1.59267145  -1)*100
(0.97285641  -1)*100
(1.00336848 -1)*100


new_data <- data.frame(
  spend = c(90,120),
  visits = c(10,12),
  age = c(46,32),
  tenure = c(3,4)
)
new_data


predict(lpmModel,
        newdata = new_data,
        type = 'response')

predict(logModel,
        newdata = new_data,
        type = 'response')




pHatLin <- predict(lpmModel, 
                   type = 'response')
yHatLin <- ifelse(pHatLin > .50, 1, 0) 

pHatLog<- predict(logModel,
                  type = 'response')
yHatLog <- ifelse(pHatLog > .50, 1, 0)


mean(myData$buy == yHatLin)*100
mean(myData$buy == yHatLog)*100



training_data <- myData[1:350, ]
validation_data <- myData[351:499, ]


Model_1 <- glm(buy ~ spend + visits + age + tenure,
               data = training_data,
               family = binomial)
flextable::as_flextable(Model_1)
Model_2 <- glm(buy ~ spend + visits,
               data = training_data,
               family = binomial)

pHatLogModel_1 <- predict(Model_1,
                          newdata = validation_data,
                          type = 'response')
yHatLogModel_1 <- ifelse(pHatLogModel_1 > .50, 1, 0)

pHatLogModel_2 <- predict(Model_2,
                          newdata = validation_data,
                          type = 'response')
yHatLogModel_2 <- ifelse(pHatLogModel_2 > .50, 1, 0)


mean(validation_data$buy == yHatLogModel_1)*100
mean(validation_data$buy == yHatLogModel_2)*100


AIC(Model_1, Model_2)

