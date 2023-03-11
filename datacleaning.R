
# f<- read.csv("train-data.csv")
library(tidyverse)
# f %>% drop_na(Engine,Power,Mileage,Year,Kilometers_Driven)
# f$Engine<-gsub("CC"," ",as.character(f$Engine))
# f$Power<-gsub("bhp"," ",as.character(f$Power))
# f$Mileage<-gsub("kmpl"," ",as.character(f$Mileage))
# f$Mileage<-gsub("km/kg"," ",as.character(f$Mileage))
# view(f)
# na.omit(f)
# f$Engine= as.numeric(f$Engine)
# f$Power= as.numeric(f$Power)
# f$Mileage= as.numeric(f$Mileage)
# write.csv(f,"train-data.csv")

car<- read.csv("car-data.csv")
car<- car %>% mutate(Mileage = as.numeric(str_replace(Mileage," kmpl| km/kg","")),Engine = as.numeric(str_replace(Engine," CC","")),
                     Power = as.numeric(str_replace(Power," bhp","")),Fuel_Type = as.factor(Fuel_Type),Owner_Type = as.factor(Owner_Type),Transmission = as.factor(Transmission))%>% select(-c(X,Name,New_Price,Location))
write.csv(car,"car-data.csv")