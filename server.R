library(leaflet)
shinyServer(function(input,output){
library(dplyr)
library(forcats)
library(ggplot2)


  output$mymap <- renderLeaflet({
    
    myData<- read.csv("assignment-02-data-formated.csv")
    newData <- myData[order(myData$latitude),]
    newData$location <- fct_reorder(newData$location,newData$latitude,.desc=TRUE) 
    pal<-colorFactor(palette = c("tomato","goldenrod3","olivedrab3","green3","cyan3","deepskyblue","darkorchid1","deeppink"),domain=newData$location)
    leaflet() %>%
      addTiles() %>%
      addProviderTiles("Esri.NatGeoWorldMap")%>%
      addCircleMarkers(data=newData,lng = ~longitude, lat = ~latitude,color = ~pal(location),popup=paste("Longitude:",newData$longitude,"<br>",
                                                                                                         "Latitude:",newData$latitude),label =~location,labelOptions = labelOptions(noHide=TRUE,direction = "right",textOnly = T,
                                                                                                                                          textsize = "14px",
                                                                                                                                          style = list(color="black",
                                                                                                                                                       "font-family"="serif",
                                                                                                                                                       "font-style"="italic")
                                                                                                
                                                                                                                                            ))  
    
      
    
    
    
  })
  
  
  
  output$myplot <-renderPlot({
    
  myData<- read.csv("assignment-02-data-formated.csv")  
  myData$value <- (as.numeric(sub("%", "", myData$value)))*0.01
    
    if(input$Type =="Blue corals"){
     
      blue_coral <- select(filter(newData,newData$coralType == "blue corals"),c(location,longitude,latitude,year,value))
      new_blue_coral <-na.exclude(blue_coral)
      
      new_blue_coral <- new_blue_coral[order(new_blue_coral$latitude),]
      new_blue_coral$location <- fct_reorder(new_blue_coral$location,new_blue_coral$latitude,.desc=TRUE)
      ggplot(new_blue_coral,aes(year,value))+
        geom_point(aes(color=location)) +
        facet_grid(~new_blue_coral$location)+
        theme(axis.text.x = element_text(angle=90))+
        geom_smooth(method=input$smoothers,color='black',size=0.5,se=FALSE)+
        ggtitle("Blue Corals")
      
    }
    else if(input$Type =="Hard corals"){
      
      hard_coral <- select(filter(newData,newData$coralType == "hard corals"),c(location,longitude,latitude,year,value))
      new_hard_coral <-na.exclude(hard_coral)
      
      new_hard_coral <- new_hard_coral[order(new_hard_coral$latitude),]
      new_hard_coral$location <- fct_reorder(new_hard_coral$location,new_hard_coral$latitude,.desc=TRUE)
      ggplot(new_hard_coral,aes(year,value))+
        geom_point(aes(color=location)) +
        facet_grid(~new_hard_coral$location)+
        theme(axis.text.x = element_text(angle=90))+
        geom_smooth(method=input$smoothers,color='black',size=0.5,se=FALSE)+
        ggtitle("Hard Corals")
      
    }
  
  
    else if(input$Type =="Sea fans"){
      
      
      sea_fans <- select(filter(newData,newData$coralType == "sea fans"),c(location,longitude,latitude,year,value))
      new_sea_fans <-na.exclude(sea_fans)
      
      new_sea_fans <- new_sea_fans[order(new_sea_fans$latitude),]
      new_sea_fans$location <- fct_reorder(new_sea_fans$location,new_sea_fans$latitude,.desc=TRUE)
      ggplot(new_sea_fans,aes(year,value))+
        geom_point(aes(color=location)) +
        facet_grid(~new_sea_fans$location)+
        theme(axis.text.x = element_text(angle=90))+
        geom_smooth(method=input$smoothers,color='black',size=0.5,se=FALSE)+
        ggtitle("Sea Fans")
      
      
    }
  
    else if(input$Type =="Sea pens"){
      
     
      sea_pens <- select(filter(newData,newData$coralType == "sea pens"),c(location,longitude,latitude,year,value))
      new_sea_pens <-na.exclude(sea_pens)
      
      new_sea_pens <- new_sea_pens[order(new_sea_pens$latitude),]
      new_sea_pens$location <- fct_reorder(new_sea_pens$location,new_sea_pens$latitude,.desc=TRUE)
      ggplot(new_sea_pens,aes(year,value))+
        geom_point(aes(color=location)) +
        facet_grid(~new_sea_pens$location)+
        theme(axis.text.x = element_text(angle=90))+
        geom_smooth(method=input$smoothers,color='black',size=0.5,se=FALSE)+
        ggtitle("Sea Pens")
      
      
      
      
    }
  
   else if(input$Type =="Soft corals"){
     
     soft_coral <- select(filter(newData,newData$coralType == "soft corals"),c(location,longitude,latitude,year,value))
     new_soft_coral <-na.exclude(soft_coral)
     
     new_soft_coral <- new_soft_coral[order(new_soft_coral$latitude),]
     new_soft_coral$location <- fct_reorder(new_soft_coral$location,new_soft_coral$latitude,.desc=TRUE)
     ggplot(new_soft_coral,aes(year,value))+
       geom_point(aes(color=location)) +
       facet_grid(~new_soft_coral$location)+
       theme(axis.text.x = element_text(angle=90))+
       geom_smooth(method=input$smoothers,color='black',size=0.5,se=FALSE)+
       ggtitle("Soft Corals")
     
  
   }
    
   else{
     
     newData <- myData[order(myData$latitude),]
     newData$location <- fct_reorder(newData$location,newData$latitude,.desc=TRUE)
     ggplot(newData,aes(year,value))+
       geom_point(aes(color=location)) +
       facet_grid(~location~coralType)+
       theme(axis.text.x = element_text(angle=90))+
       geom_smooth(method=input$smoothers,color='black',size=0.5,se=FALSE)+
       ggtitle("Five Types of Corals")
     
     
     
     
   }
  

   
   
      
    }
    
    
  )
  
  
  
})

