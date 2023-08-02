library(readr)
library(magrittr)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(reshape2)
library(rpart)
library(rpart.plot)

setwd("/Users/cindychen/Desktop/數據科學概論/HW/")
data <- read_csv("insurance.csv")

## check data format
head(data)
summary(data)

## plot the variables
ggplot(data , aes(x = sex , fill = sex))+
  geom_bar(width = 0.6)+
  scale_fill_brewer(palette = "Pastel1")+
  theme_clean()

## count of age
ggplot(data , aes(x = age , fill = sex))+
  geom_histogram(stat = "count" , position = "dodge")+
  scale_x_binned()+
  scale_fill_brewer(palette = "Pastel1")+
  theme_clean()+
  theme(plot.title = element_text(hjust = 0.5))

## distribrion of bmi of different gender
sex <- split(data , data$sex)
female <- sex$female
male <- sex$male

ggplot(data , aes(x = bmi , fill = sex))+
  geom_histogram(position = "identity" , alpha = 0.5)+
  theme_clean()

## distribution of children
ggplot(data , aes(x = children ))+
  geom_histogram(aes(fill = "pink"), stat = "bin" , binwidth = 0.5 , show.legend = F)+
  scale_fill_calc()+
  theme_clean()

## distribution of different region
ggplot(data , aes(x = region))+
  geom_histogram(aes(fill = region) , stat = "count" , show.legend = F , width = 0.6)+
  scale_fill_brewer(palette = "Pastel2")+
  theme_clean()

## distribution of smoker with different gender
ggplot(data , aes(x = smoker , fill = sex))+
  geom_histogram(stat = "count" , position = "dodge")+
  scale_fill_brewer(palette = "Pastel1")+
  theme_clean()

## distribution of charges
ggplot(data , aes(x = charges))+
  geom_histogram(aes(fill = "#E8DDCB"),stat = "bin", binwidth = 3500 , show.legend = F)+
  scale_fill_manual(values = c("#036564"))+
  theme_clean()

## correlation table of variable
correlation <- data
correlation$sex <- ifelse(data$sex == "female" , 1, 0)
correlation$smoker <- ifelse(data$smoker == "yes" , 1, 0)

cormat <- round(cor(correlation[,-6]),3)
cormat[lower.tri(cormat)] <- NA
melted_cormat <- melt(cormat , na.rm = T)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#FFE98A" , high = "#D74177" , space = "Lab" , name = "Correlation"
                       ,limit = c(-0.5,1))+
  theme_minimal()+
  coord_fixed()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

## charges with different variable
## charges with nonsmoker and smoker
ggplot(data , aes(x=charges , fill = smoker))+
  geom_density(alpha = 0.6)+
  facet_grid(smoker~.)+
  scale_fill_manual(values = c("#DBB7AB" , "#3F4D39"))+
  theme_clean()

## charges in different sex and smoker
ggplot(data , aes(x = sex , y = charges , fill = smoker))+
  geom_violin(scale = "area" , draw_quantiles = c(0.25, 0.5, 0.75))+
  scale_fill_manual(values = c("#702242" , "#B8728E" ))

## charges with nonsmoker and smoker for female
ggplot(female , aes(x = charges , fill = smoker))+
  geom_boxplot(alpha = 0.5)+
  labs(title = "Boxplot for Female")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_clean()

## charges with nonsmoker and smoker for male
ggplot(male , aes(x = charges , fill = smoker))+
  geom_boxplot(alpha = 0.5)+
  labs(title = "Boxplot for Male")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_clean()
  
## charges in different bmi values
ggplot(data , aes(x = bmi , y = charges , col = smoker))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#034C65" , "#EF8C86"))+
  theme_clean()

## charges in different age
ggplot(data , aes(x = age , y = charges , col = smoker))+
  geom_point()+
  scale_color_manual(values = c("#034C65" , "#EF8C86"))+
  theme_clean()

smoker <- data[which(data$smoker == "yes"),]

ggplot(smoker , aes(x = age , y = charges , col = bmi))+
  geom_point()+
  geom_smooth(method = "lm" , col = "#FC8884")+
  scale_color_gradient(low = "#EFE98A" , high = "#D74177" , space = "Lab")+
  theme_clean()

## regression model
library(caret)
library(e1071)

set.seed(20211218)
train.id <- createDataPartition(data$smoker , p = 0.7 , list = F)
train <- data[train.id,]
test <- data[-train.id,]

table(data$smoker)
table(train$smoker)
table(test$smoker)

## run the regression of bmi and charges with square bmi and cube bmi
smoker <- train[which(train$smoker == "yes"),]
model_bmi <- lm(charges~bmi , data = smoker)
summary(model_bmi)

bmi2 <- smoker$bmi^2
model_bmi2 <-lm(charges~bmi+bmi2 , data = smoker)
summary(model_bmi2)

bmi3 <- smoker$bmi^3
model_bmi3 <- lm(charges~bmi+bmi2+bmi3 , data = smoker)
summary(model_bmi3)

list("Sum of square error with intercept" = sum(residuals(model_bmi)^2),
     "Sum of square error with quadratic" = sum(residuals(model_bmi2)^2),
     "Sum of square error with cube" = sum(residuals(model_bmi3)^2))

ggplot(smoker,aes(x = bmi , y = charges))+
  geom_point(col = "#034c65")+
  geom_line(data = data.frame(charges = predict(model_bmi , smoker), bmi = smoker$bmi),
            aes(x = bmi,y = charges,color = "regression"))+
  geom_line(data = data.frame(charges = predict(model_bmi2 , smoker), bmi = smoker$bmi),
            aes(x = bmi, y = charges, color = "regression with square bmi"))+
  geom_line(data = data.frame(charges = predict(model_bmi3 , smoker), bmi = smoker$bmi),
            aes(x = bmi, y = charges, color = "regression with cube bmi") )+
  scale_color_manual("",breaks = c("regression","regression with square bmi","regression with cube bmi"),
                     values = c("#FC9F66","#FAC357","#97C5D8"))+
  theme_clean()

### Multivariate regression line
model1 <- lm(charges~. , data = train) 
summary(model1)

model1_p <- data.frame(realdata = test$charges , 
                       predictdata = predict(model1 , test))
cor(model1_p)
ggplot(model1_p , aes(x = realdata, y = predictdata))+
  geom_point(col = "#034c65")+
  theme_clean()

model2 <- lm(charges~. -region-sex, data = train)
summary(model2)

model2_p <- data.frame(realdata = test$charges,
                       predictdata = predict(model2 , test))
cor(model2_p)
ggplot(model2_p , aes(x = realdata, y = predictdata))+
  geom_point(col = "#034c65")+
  theme_clean()

## try to train better model 
train_sb <- train
train_sb$smoker_bmi <- ifelse(train_sb$smoker == "yes", 1 , 0)*train_sb$bmi

model_smoker_bmi <- lm(charges~age+children+smoker_bmi+bmi , train_sb)
summary(model_smoker_bmi)

test_sb <- test
test_sb$smoker_bmi <- ifelse(test_sb$smoker == "yes", 1 , 0)*test_sb$bmi
model_smoker_bmip <- data.frame(realdata = test_sb$charges , 
                                predictdata = predict(model_smoker_bmi, test_sb))
cor(model_smoker_bmip)
ggplot(model_smoker_bmip , aes(x = realdata, y = predictdata))+
  geom_point(col = "#034c65")+
  theme_clean()

## consider overfat person and the relationship with charges
train_sb$overweight <- ifelse(train_sb$bmi > 30 , train_sb$bmi , 0)
train_sb$s_over <- train_sb$overweight*ifelse(train_sb$smoker == "yes" , 1 ,0)

model_overweight <- lm(charges~age+children+s_over+smoker_bmi+ bmi , data = train_sb)
summary(model_overweight)

vif(model_overweight)

test_sb$overweight <- ifelse(test_sb$bmi > 30 , test_sb$bmi , 0)
test_sb$s_over <- test_sb$overweight*ifelse(test_sb$smoker == "yes" , 1 ,0)
model_overweightp <- data.frame(realdata = test_sb$charges , 
                                predictdata = predict(model_overweight, test_sb))
cor(model_overweightp)
ggplot(model_overweightp , aes(x = realdata, y = predictdata))+
  geom_point(col = "#034c65")+
  theme_clean()

## try different training model
## charges and bmi
lossreg <- loess(charges ~ bmi, train)
logreg <- lm(charges~ log(bmi), train)
summary(lossreg)

ggplot(train,aes(x = bmi , y = charges))+
  geom_point(col = "#034c65")+
  geom_line(data = data.frame(charges = predict(lossreg , train) , bmi = train$bmi),
            aes(x = bmi , y = charges, colour = "Locally weighted regression" ))+
  geom_line(data = data.frame(charges = predict(logreg , train), bmi = train$bmi),
            aes(x = bmi , y = charges, colour = "Log-linear regression"))+
  scale_color_manual("",breaks = c("Locally weighted regression","Log-linear regression"),
                     values = c("#ef8c86","#fcb677"))+
  theme(legend.position = "bottom")

lossreg_s <- loess(charges ~ bmi, smoker)
logreg_s <- lm(charges~ log(bmi), smoker)


ggplot(smoker,aes(x = bmi , y = charges))+
  geom_point(col = "#034c65")+
  geom_line(data = data.frame(charges = predict(lossreg_s , smoker) , bmi = smoker$bmi),
            aes(x = bmi , y = charges, colour = "Locally weighted regression" ), size =1)+
  geom_line(data = data.frame(charges = predict(logreg_s , smoker), bmi = smoker$bmi),
            aes(x = bmi , y = charges, colour = "Log-linear regression"), size =1)+
  scale_color_manual("",breaks = c("Locally weighted regression","Log-linear regression"),
                     values = c("#ef8c86","#fcb677"))+
  theme(legend.position = "bottom")

## charges with age
lossreg2 <- loess(charges ~ age, train)
logreg2 <- lm(charges ~ log(age) , train)

ggplot(train, aes(x = age , y = charges))+
  geom_point(col = "#582957")+
  geom_line(data = data.frame(charges = predict(lossreg2, train), age = train$age),
            aes(x = age, y = charges , colour = "Locally weighted regression"), size = 1)+
  geom_line(data = data.frame(charges = predict(logreg2, train), age = train$age),
            aes(x = age , y = charges , colour = "Log-linear regression"), size = 1)+
  scale_color_manual("" , breaks = c("Locally weighted regression","Log-linear regression"),
                     values = c("#b69088","#e1d9ef"))+
  theme(legend.position = "bottom")

lossreg2_s <- loess(charges ~ age, smoker)
logreg2_s <- lm(charges ~ log(age) , smoker)

ggplot(smoker, aes(x = age , y = charges))+
  geom_point(col = "#582957")+
  geom_line(data = data.frame(charges = predict(lossreg2_s,smoker), age = smoker$age),
            aes(x = age, y = charges , colour = "Locally weighted regression"),size = 1)+
  geom_line(data = data.frame(charges = predict(logreg2_s, smoker), age = smoker$age),
            aes(x = age , y = charges , colour = "Log-linear regression"), size = 1)+
  scale_color_manual("" , breaks = c("Locally weighted regression","Log-linear regression"),
                     values = c("#b69088","#e1d9ef"))+
  theme(legend.position = "bottom")

## regression tree
regTree <- rpart(charges~age+children+smoker+bmi,data = train)
regTree

rpart.plot(regTree ,digits = 3, type = 2 , roundint = F)

plotcp(regTree)
regTree_prune <- prune(regTree , cp =0.2)
regTree_prune

rpart.plot(regTree_prune ,digits = 3, type = 2 , roundint = F)

tab.decision <- data.frame("predict" = predict(regTree, test) , "real" = test$charges)
tab.decision
cor(tab.decision)

tab.cp <- data.frame("predict" = predict(regTree_prune, test) , "real" = test$charges)
tab.cp %>% head()
cor(tab.cp)

## using the training model above
regTree_s <- rpart(charges~age+children+smoker_bmi+s_over , data = train_sb)
regTree_s

rpart.plot(regTree_s ,digits = 3, type = 2 , roundint = F)

plotcp(regTree_s)
regTree_sprune <- prune(regTree_s , cp =0.025)
regTree_sprune

tab.s <- data.frame("predict" = predict(regTree_s, test_sb) , "real" = test_sb$charges)
tab.s %>% head()
cor(tab.s)




