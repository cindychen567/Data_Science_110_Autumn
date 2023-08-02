library(readr)
library(ggplot2)
library(magrittr)
library(car)
library(np)
library(dplyr)
library(rpart)
library(rpart.plot)
library(ggthemes)

## read my flie
setwd("/Users/cindychen/Desktop/數據科學概論/HW/")
data <- read_csv("insurance.csv")
str(data)

## sample from the data
set.seed(122)
taken <- sample(1:nrow(data),500)
test1 <- data[taken,]

## make regression of age and charges
model1 <- lm(charges~age,test1)
model1_without <- lm(charges~age -1 , test1)
summary(model1)
summary(model1_without)

plot(test1$age,test1$charges)
abline(model1 ,lty = 1 ,col = "red")
abline(model1_without,lty = 2, col = "blue")

## make regression of bmi and charges
model2 <- lm(charges~bmi, test1)
model2_without <- lm(charges~bmi-1, test1)
summary(model2)
summary(model2_without)

plot(test1$bmi , test1$charges)
abline(model2 ,lty = 1 ,col = "red")
abline(model2_without,lty = 2, col = "blue")

ggplot(test1 , aes(x = bmi, y = charges ))+
  geom_point(aes(colour = smoker))+
  scale_color_brewer(palette = "Set2")

ggplot(test1 , aes(x = bmi, y = charges ))+
  geom_point(aes(colour = sex))+
  scale_color_brewer(palette = "Set2")

## and split the dataset by variable smoker
NS <- split(data,data$smoker)

## run the regression line on two different dataset above
## With nonsmoker
model3_1 <- lm(charges~bmi,NS$no)
model3_1W <- lm(charges~bmi-1,NS$no)
summary(model3_1)
summary(model3_1W)

predicted_bmi <- data.frame(charges = predict(model3_1,NS$no),
                            bmi = NS$no$bmi)
predicted_bmiw <- data.frame(charges = predict(model3_1W,NS$no),
                             bmi = NS$no$bmi)

ggplot(NS$no,aes(x = bmi,y = charges))+
  geom_point(col = "pink")+
  geom_line(data = predicted_bmi , aes(x = bmi, y = charges,color = "regression") , lty = 2)+
  geom_line(data = predicted_bmiw , aes(x = bmi, y = charges ,color = "regression without B0" ), lty = 2)+
  scale_colour_manual("", breaks = c("regression","regression without B0"),
                     values = c("blue","purple"))

residuals(model3_1)
residuals(model3_1W)

## with smoker
model3_2 <- lm(charges~bmi,NS$yes)
model3_2W <- lm(charges~bmi-1,NS$yes)
summary(model3_2)
summary(model3_2W)

predicted_2bmi <- data.frame(charges = predict(model3_2,NS$yes),
                            bmi = NS$yes$bmi)
predicted_2bmiw <- data.frame(charges = predict(model3_2W,NS$yes),
                             bmi = NS$yes$bmi)

ggplot(NS$yes,aes(x = bmi,y = charges))+
  geom_point(col = "pink")+
  geom_line(data = predicted_2bmi , aes(x = bmi, y = charges,color = "regression") , lty = 2)+
  geom_line(data = predicted_2bmiw , aes(x = bmi, y = charges,color = "regression without B0" ), lty = 2)
  scale_colour_manual("", breaks = c("regression","regression without B0"),
                      values = c("blue","purple"))

residuals(model3_2)
residuals(model3_2W)

## check the residual of model3_2 is normal distribution with mean 0 and variance 1
ks.test(residuals(model3_2),"pnorm")
plot(model3_2)

## run the regression of bmi and charges with square bmi and cube bmi
b2 <- NS$yes$bmi^2
model3_3 <- lm(charges~bmi+b2,NS$yes)
b3 <- NS$yes$bmi^3
model3_4 <- lm(charges~bmi+b2+b3,NS$yes)
summary(model3_3)
summary(model3_4)

predict_3bmi <- data.frame(charges = predict(model3_3,NS$yes),
                           bmi = NS$yes$bmi)
predict_4bmi <- data.frame(charges = predict(model3_4,NS$yes),
                           bmi = NS$yes$bmi)

ggplot(NS$yes,aes(x = bmi , y = charges))+
  geom_point(col = "darkgrey")+
  geom_line(data = predict_3bmi,aes(x = bmi,y = charges,col = "regression with square bmi"))+
  geom_line(data = predict_4bmi,aes(x = bmi,y = charges,col = "regression with cube bmi"),lty =2)+
  geom_line(data = predicted_2bmi , aes(x = bmi, y = charges,color = "regression") , lty = 2)+
  scale_color_manual("",breaks = c("regression with square bmi","regression with cube bmi","regression"),
                     values = c("#FC9F66","#FAC357","#97C5D8"))+
  theme_clean()

## see the regression line which is more fit
list("Sum of square error with intercept" = sum(residuals(model3_2)^2),
     "Sum of square error without intercept" = sum(residuals(model3_2W)^2),
     "Sum of square error with quadratic" = sum(residuals(model3_3)^2),
     "Sum of square error with cube" = sum(residuals(model3_4)^2))

## change the variable of smoker into 0 and 1 (nonsmoker and smoker) 
datacopy <- data
datacopy$Idsmoker <- ifelse(data$smoker == "yes" ,1, 0)
datacopy <- datacopy[,c(1,2,3,4,8,6,7)]

## make regression of charges with children
model4 <- lm(charges~children,datacopy)
model4w <- lm(charges~children-1,datacopy)
summary(model4)
summary(model4w)

predict_1ch <- data.frame(charges = predict(model4,datacopy),
                          children = datacopy$children)

predict_1chw <- data.frame(charges = predict(model4w,datacopy),
                           children = datacopy$children)

ggplot(datacopy,aes(x=children,y = charges))+
  geom_jitter(alpha = 0.5,col = "lightblue")+
  geom_line(data = predict_1ch,aes(x = children , y = charges),col = "blue")+
  geom_line(data = predict_1chw,aes(x = children , y = charges),col = "#8B47B5")

by(datacopy$charges,datacopy$children,mean)

## make regression of charges with region
model5 <- lm(charges~region,datacopy)
summary(model5)
model.matrix(model5)

by(datacopy$charges,datacopy$region,mean)

## multiple regression
full_model <- lm(charges~. , datacopy)
summary(full_model)

## the variable of sex is not significant in the last model
## so we exclude it and see the regression again
model_wsex <- lm(charges~.-sex , datacopy)
summary(model_wsex)

predict(model_wsex)

## using log-linear regression
log_model <- lm(log(charges)~.-sex , datacopy)
summary(log_model)

## loess regression
lossreg <- loess(charges ~ bmi, NS$yes)
logreg <- lm(charges~ log(bmi),NS$yes)

ggplot(NS$yes,aes(x = bmi , y = charges))+
  geom_point(col = "pink")+
  geom_line(data = data.frame(charges = predict(lossreg , NS$yes) , bmi = NS$yes$bmi),
            aes(x = bmi , y = charges, colour = "Locally weighted regression" ))+
  geom_line(data = data.frame(charges = predict(logreg , NS$yes), bmi = NS$yes$bmi),
            aes(x = bmi , y = charges, colour = "Log-linear regression"))+
  scale_color_manual("",breaks = c("Locally weighted regression","Log-linear regression"),
                     values = c("red","#7E1037"))+
  theme(legend.position = "bottom")


lossreg_l <- loess(charges ~ age,NS$yes )
logreg_l <- lm(charges ~ log(age) , NS$yes)

ggplot(NS$yes, aes(x = age , y = charges))+
  geom_point(col = "#88CDF6")+
  geom_line(data = data.frame(charges = predict(lossreg_l,NS$yes), age = NS$yes$age),
            aes(x = age, y = charges , colour = "Locally weighted regression"))+
  geom_line(data = data.frame(charges = predict(logreg_l,NS$yes), age = NS$yes$age),
            aes(x = age , y = charges , colour = "Log-linear regression"))+
  scale_color_manual("" , breaks = c("Locally weighted regression","Log-linear regression"),
                     values = c("#73CD88","#23503A"))+
  theme(legend.position = "bottom")

## kernel regression
model.np <- npreg(charges ~ bmi , ckertype = "gaussian" , ckerorder = 2 , data = NS$yes)
model.np

series <- seq(10,20)
fit <- predict(model.np , series)

plot(NS$yes$charges[order(NS$yes$bmi)],fit$fit[order(NS$yes$bmi)],col = "blue", type = "l",
     xlab = "BMI" , ylab = "Charges")

## regression tree
regTree <- rpart(charges ~ bmi,data = NS$yes)
regTree

plot(regTree,uniform = T)
text(regTree,digits = 6)

rpart.plot(regTree ,digits = 3, type = 2 , roundint = F)

regTree_1 <- rpart(charges ~ age,data = NS$yes)
regTree_1

plot(regTree_1,uniform = T)
text(regTree_1,digits = 6)

rpart.plot(regTree_1, digits = 3) 

## regression tree - prune tree
plotcp(regTree)
regTree_prune <- prune(regTree , cp =0.11)
plot(regTree_prune,uniform = T)
text(regTree_prune,digits =6)

plotcp(regTree_1)
regTree_prune1 <- prune(regTree_1 , cp =0.05)
plot(regTree_prune1,uniform = T)
text(regTree_prune1,digits =6)

## compare before and after pruning tree
NS$yes %>% 
  mutate(pred = predict(regTree,NS$yes)) %>%
  mutate(pred2 = predict(regTree_prune,NS$yes)) %>%
  ggplot(aes(x = bmi , y = charges))+
  geom_point(col = "#53A7D8")+
  geom_line(aes(y = pred , colour = "decision tree"))+
  geom_line(aes(y = pred2 , colour = "pruned decision tree"))+
  scale_color_manual("",breaks = c("decision tree", "pruned decision tree"),
                     values = c("#A5678E","#33539E"))+
  theme_clean()+
  theme(legend.position = "bottom")

NS$yes %>% 
  mutate(pred = predict(regTree_1,NS$yes)) %>%
  mutate(pred2 = predict(regTree_prune1,NS$yes)) %>%
  ggplot(aes(x = age , y = charges))+
  geom_point(col = "#FFDD94")+
  geom_line(aes(y = pred , colour = "decision tree"))+
  geom_line(aes(y = pred2 , colour = "pruned decision tree"))+
  scale_color_manual("",breaks = c("decision tree", "pruned decision tree"),
                     values = c("#FA897B","#CCABDB"))+
  theme_clean()+
  theme(legend.position = "bottom")

