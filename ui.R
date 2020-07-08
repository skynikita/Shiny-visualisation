library(leaflet)
shinyUI(
  
  pageWithSidebar(
    
    titlePanel("Different kinds of Coral"),
    
    sidebarPanel(
      
      selectInput("Type","Please enter the type of the coral",choices = c("Blue corals","Hard corals",
                                                                          "Sea fans","Sea pens",
                                                                          "Soft corals","Show All")),
      selectInput("smoothers","Please choose your Smoother",
                  c("Lm"="lm","Loess"="loess","Glm"="glm","Gam"="gam"))
      
      
    ),
    
    mainPanel(
      
      leafletOutput("mymap"),
      
      plotOutput("myplot")
      
      
      
    )
    
    
    
    
  )
)
