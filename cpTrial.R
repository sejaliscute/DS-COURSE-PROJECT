library(tidyverse)
library(GGally)
setwd("D:/Data Science Lab/course project")
car<- read.csv("car-data.csv")
glimpse(car)
car<- car %>% mutate(Mileage = as.numeric(str_replace(Mileage," kmpl| km/kg","")),Engine = as.numeric(str_replace(Engine," CC","")),
                     Power = as.numeric(str_replace(Power," bhp","")),Fuel_Type = as.factor(Fuel_Type),Owner_Type = as.factor(Owner_Type),Transmission = as.factor(Transmission))%>% select(-c(X,Name,New_Price,Location))
str(car)
car<- drop_na(car)
car<-log(car$Power,car$Engine)
car
RNGkind(sample.kind = "Rounding")
set.seed(123)
intrain <- sample(x=nrow(car), size = nrow(car)*0.8)
car_train <- car[intrain,]
car_test <- car[-intrain,]
q<-ggcorr(car_train, label = T)
print(q)

l1<-lm(Price~.,car_train)
s1<-summary(l1)
print(s1)

l2<-lm(Price~ Power+Engine,car_train)
summary(l2)
# model_back <- step(l1,direction = "backward", trace = 0)
# summary(model_back)
# 
# 
# 
# model0 <- lm(Price~1, car_train)
# model_forward <- step(model0, direction = "forward", scope = list(lower =model0,  upper = l1), trace = 0)
# summary(model_forward)
# 
# model_both <- step(model0, direction = "both", scope = list(lower = model0, upper = l1), trace = 0)
# summary(model_both)
# 
# compare_performance(l1,model_back,model_forward,model_both)
# 
# l2<-lm(Price~ Power,car_train)
# s2<-summary(l2)
# print(s2)

view(car_test)
Price<- car_test$Price
car_test<- car_test %>% select(- Price)

pred<-predict(l2,car_test)
data<-cbind(car_test,pred)
print(data)
err<-data$pred -Price
print(err)
m<-mean(abs(err))
print(m)
sum(abs(err)<1)

total<-nrow(car_test)
print(total)

acc= (295/1175)*100
print(acc)

# eucd<-sqrt((car_test$Power-car_train$Power)**2+(car_test$Engine-car_train$Engine)**2)
# data<-cbind(,eucd)
# data<-data[order(data$eucd),]

# p<-ggplot(car_train,aes(Power,Price))+geom_point()+geom_smooth(method="lm",formula=y~x,col="red",se=F)
# print(p)
# r<-ggplot(car_train,aes(Year,Price))+geom_point()+geom_smooth(method="lm",formula=y~x,col="red",se=F)
# print(r)
# t<-ggplot(car_train,aes(Transmission,Price))+geom_point()+geom_smooth(method="lm",formula=y~x,col="red",se=F)
# print(t)