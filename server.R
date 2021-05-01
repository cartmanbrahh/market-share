library("shiny")
library("leaflet")
library("leaflet.extras")
library("DT")

path<-"/cg2021/shared/group3/deployment"
path <- getwd()
#Load environments
source(paste(path,"/www/Token.R",sep=""),local = TRUE)
load(paste(path,"/www/trade_network_63.RData",sep=""))
load(paste(path,"/www/rf.RData",sep=""))
load(paste(path,"/www/candidates.RData",sep=""))
load(paste(path,"/www/final_data_frame.RData",sep=""))
load(paste(path,"/www/final_candidate_market_zones.RData",sep=""))
load(paste(path,"/www/candidates_IRIS.RData",sep=""))
load(paste(path,"/www/best.RData",sep=""))
load(paste(path,"/www/best_cand.RData",sep=""))
load(paste(path,"/www/worst_cand.RData",sep=""))

server<-function(input, output, session) {
  
  filtered<-reactive({
    subset(best@data,best@data$Competitors>=input$Competitors[1]&
             best@data$Competitors<=input$Competitors[2]&
             best@data$`Total Schools`>=input$`Total Schools`[1]&
             best@data$`Total Schools`<=input$`Total Schools`[2]&
             best@data$`Total Pistes`>=input$`Total Pistes`[1]&
             best@data$`Bike Tracks/Skate Parks`<=input$`TBike Tracks/Skate Parks`[2]&
             best@data$`Sports shops`>=input$`Sports shops`[1]&
             best@data$`Sports shops`<=input$`Sports shops`[2]&
             best@data$`Population constraint` == input$`Population constraint`&
             best@data$`School constraint` == input$`School constraint`)
  })
  
  
  output$Map <- renderLeaflet({candidates_leaflet %>%
      addCircleMarkers(lng = candidate_IRIS$lon, lat = candidate_IRIS$lat,label = final_data_frame$posid, clusterOptions =  markerClusterOptions(), clusterId = "final_data_frame$posid",
                       labelOptions = labelOptions(noHide = FALSE,direction = 'top',offset=c(0,0),textOnly = TRUE,
                                                   style=list('color'='rgba(0,0,0,1)','font-family'= 'Arial Black','font-style'= 'bold',
                                                              'box-shadow' = '0px 0px rgba(0,0,0,0.25)','font-size' = '12px',
                                                              'background-color'='rgba(255,255,255,0.7)','border-color' = 'rgba(0,0,0,0)')))%>%
      addCircleMarkers(data = best_cand,color="gold", weight = 10,  group = "best_worst")%>%
      addCircleMarkers(data = worst_cand,color="purple", weight = 10,  group = "best_worst") })
  
  observe({
    leafletProxy("Map",data=filtered()) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = candidate_IRIS$lon, lat = candidate_IRIS$lat,label = final_candidate_market_zones$posid, clusterOptions =  leaflet::markerClusterOptions(), clusterId = "final_candidate_market_zones$posid",
                       labelOptions = labelOptions(noHide = FALSE,direction = 'top',offset=c(0,0),textOnly = TRUE,
                                                   style=list('color'='rgba(0,0,0,1)','font-family'= 'Arial Black','font-style'= 'bold',
                                                              'box-shadow' = '0px 0px rgba(0,0,0,0.25)','font-size' = '12px',
                                                              'background-color'='rgba(255,255,255,0.7)','border-color' = 'rgba(0,0,0,0)')))%>%
      addCircleMarkers(data = best_cand,color="gold", weight = 10,  group = "best_worst")%>%
      addCircleMarkers(data = worst_cand,color="purple", weight = 10,  group = "best_worst")
    
    
    output$table <-renderDT({
      datatable(final_data_frame, options = list(
        deferRender = TRUE,
        rownames = FALSE,
        filter = "top",
        scrollY = 400,
        scrollX = TRUE,
        scroller = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(width = '10%', targets = c(2,3,4)))
      ))  # %>% formatStyle(columns = c(2,3), width='20px')
      
      
    })
    
    output$table_1 <-renderDT({
      datatable(final_candidate_market_zones, options = list(
        deferRender = TRUE,
        rownames = FALSE,
        filter = "top",
        scrollY = 400,
        scrollX = TRUE,
        scroller = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(width = '10%', targets = c(2,3,4)))
      ))})
    
    
  })
  
  
}
