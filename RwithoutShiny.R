library(ggplot2)
library(forcats)
library(dplyr)



myData<-read.csv("assignment-02-data-formated.csv")
myData

myData$value <- (as.numeric(sub("%", "", myData$value)))*0.01
head(myData)

#All Types of Corals
newData <- myData[order(myData$latitude),]
newData$location <- fct_reorder(newData$location,newData$latitude,.desc=TRUE)
ggplot(newData,aes(year,value))+
  geom_point(aes(color=location)) +
  facet_grid(~location~coralType)+
  theme(axis.text.x = element_text(angle=90))+
  geom_smooth(method='loess',color='black',size=0.5)+
  ggtitle("Five Types of Corals")


#Blue Corals 
blue_coral <- select(filter(newData,newData$coralType == "blue corals"),c(location,longitude,latitude,year,value))
new_blue_coral <-na.exclude(blue_coral)

new_blue_coral <- new_blue_coral[order(new_blue_coral$latitude),]
new_blue_coral$location <- fct_reorder(new_blue_coral$location,new_blue_coral$latitude,.desc=TRUE)
ggplot(new_blue_coral,aes(year,value))+
  geom_point(aes(color=location)) +
  facet_grid(~new_blue_coral$location)+
  theme(axis.text.x = element_text(angle=90))+
  geom_smooth(method='loess',color='black',size=0.5)+
  ggtitle("Blue Corals")


#Hard Corals
hard_coral <- select(filter(newData,newData$coralType == "hard corals"),c(location,longitude,latitude,year,value))
new_hard_coral <-na.exclude(hard_coral)

new_hard_coral <- new_hard_coral[order(new_hard_coral$latitude),]
new_hard_coral$location <- fct_reorder(new_hard_coral$location,new_hard_coral$latitude,.desc=TRUE)
ggplot(new_hard_coral,aes(year,value))+
  geom_point(aes(color=location)) +
  facet_grid(~new_hard_coral$location)+
  theme(axis.text.x = element_text(angle=90))+
  geom_smooth(method='loess',color='black',size=0.5)+
  ggtitle("Hard Corals")


#Sea Fans
sea_fans <- select(filter(newData,newData$coralType == "sea fans"),c(location,longitude,latitude,year,value))
new_sea_fans <-na.exclude(sea_fans)

new_sea_fans <- new_sea_fans[order(new_sea_fans$latitude),]
new_sea_fans$location <- fct_reorder(new_sea_fans$location,new_sea_fans$latitude,.desc=TRUE)
ggplot(new_sea_fans,aes(year,value))+
  geom_point(aes(color=location)) +
  facet_grid(~new_sea_fans$location)+
  theme(axis.text.x = element_text(angle=90))+
  geom_smooth(method='loess',color='black',size=0.5)+
  ggtitle("Sea Fans")

#Sea Pens
sea_pens <- select(filter(newData,newData$coralType == "sea pens"),c(location,longitude,latitude,year,value))
new_sea_pens <-na.exclude(sea_pens)

new_sea_pens <- new_sea_pens[order(new_sea_pens$latitude),]
new_sea_pens$location <- fct_reorder(new_sea_pens$location,new_sea_pens$latitude,.desc=TRUE)
ggplot(new_sea_pens,aes(year,value))+
  geom_point(aes(color=location)) +
  facet_grid(~new_sea_pens$location)+
  theme(axis.text.x = element_text(angle=90))+
  geom_smooth(method='loess',color='black',size=0.5)+
  ggtitle("Sea Pens")


#Soft Corals 
soft_coral <- select(filter(newData,newData$coralType == "soft corals"),c(location,longitude,latitude,year,value))
new_soft_coral <-na.exclude(soft_coral)

new_soft_coral <- new_soft_coral[order(new_soft_coral$latitude),]
new_soft_coral$location <- fct_reorder(new_soft_coral$location,new_soft_coral$latitude,.desc=TRUE)
ggplot(new_soft_coral,aes(year,value))+
  geom_point(aes(color=location)) +
  facet_grid(~new_soft_coral$location)+
  theme(axis.text.x = element_text(angle=90))+
  geom_smooth(method='loess',color='black',size=0.5)+
  ggtitle("Soft Corals")

