##install.packages('shiny')
library(shiny)
library(sf)
library(ggplot2)

##runExample("01_hello")

stockpoly<-st_read('N:/data/CAFF/data/shapefiles/stocks/stockpoly.shp')

wcm<-st_read('N:/data/shapefiles/naturalearthdata_ne_50m_land_poly/ne_50m_land.shp')


# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("CAFF Stock Areas"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

  selectInput("selectedregion", 
              label="Region:", 
              choices = unique(stockpoly$region)),
  selectInput("selectedstock", 
              label="Stock:", 
              choices = unique(stockpoly$stock))
              
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput("distPlot")
    )
  )
)



# Define server logic required 
server <- function(input, output) {



currentregion<-reactive({input$selectedregion})
currentstock<-reactive({input$selectedstock})

output$distPlot <- renderPlot({
  
df<-subset(stockpoly,region %in% currentregion() &
                     stock %in% currentstock())  


xlm<-c(-145,-42)
ylm<-c(35,80)
lw<-0.5

ggplot()+
    geom_sf(data=df,fill='firebrick1',inherit.aes=FALSE,color=NA,lwd=0.5)+
##coord_sf(lims_method="geometry_bbox")
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE,lwd=0.001)+
  scale_x_continuous(expand=c(0,0),limits=c(-145,-42))+
  scale_y_continuous(expand=c(0,0),limits=c(35,80))+
   theme(axis.line=element_blank(),
         axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        strip.text.x = element_text(size = 9),
        panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
  ylab('')
    })
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)

   observeEvent(input$selectedregion,{
   updateSelectInput(session,'selectedstock',
           choices=unique(subset(stockpoly,region %in% input$selectedregion))$stock)
 }) 









a<-stockpoly[input$objet,]

spl<-as(stockpoly,'Spatial')


a<-filter(stockpoly,speciesg=='Salmonids')
ggplot()+
    geom_sf(data=df,fill='firebrick3',inherit.aes=FALSE,color=NA,lwd=0.5)+
##  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE,lwd=0.001)+
  scale_x_continuous(expand=c(0,0),limits=c(-145,-42))+
  scale_y_continuous(expand=c(0,0),limits=c(35,80))+
   theme(axis.line=element_blank(),
         axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        strip.text.x = element_text(size = 9),
        panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
  ylab('')


setwd("C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/code/shiny/")

runApp('App-1')