library("shiny")
library("leaflet")
library("leaflet.extras")
library("DT")

path<-"/cg2021/shared/group3/deployment"
path <- getwd()
load(paste(path,"/www/trade_network_63.RData",sep=""))
load(paste(path,"/www/rf.RData",sep=""))
load(paste(path,"/www/final_data_frame.RData",sep=""))
load(paste(path,"/www/final_candidate_market_zones.RData",sep=""))
load(paste(path,"/www/candidates.RData",sep=""))
load(paste(path,"/www/candidates_IRIS.RData",sep=""))
load(paste(path,"/www/best.RData",sep=""))
load(paste(path,"/www/best_cand.RData",sep=""))
load(paste(path,"/www/worst_cand.RData",sep=""))

library("leaflet")
library("shiny")
library("shinythemes")

ui <- shinyUI(navbarPage(theme = shinytheme("united"),windowTitle = "geomarketing.tse.fr",
                         title=div(img(src="logo_tse.png",width = 100),"Candidate Selection"),
                         tabPanel(h5("Current network of shops"),
                                  bootstrapPage(title = "Existing shops",
                                                div(class="outer",
                                                    shiny::tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                                    leafletOutput("Map1", width="100%", height="100%")))),
                         tabPanel(h5("Candidates market share"),
                                  bootstrapPage(title = "Candidate Selection", 
                                                div(class="outer",
                                                    shiny::tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                                    leafletOutput("Map", width="100%", height="100%"),
                                                    absolutePanel(id = 'panel',
                                                                  bottom = 20, left = 20, style="z-index:500; opacity:0.95; background-color:rgba(192,192,192,.5)", draggable = T,
                                                                  h4("Ranking Criteria for the 10 new shops"),
                                                                  sliderInput("Competitors", label="Number of competitors:",
                                                                              min=0, max=max(final_data_frame$Competitors), value=c(0,7574), step=100, width=280),
                                                                  checkboxInput("best@data$`Population constraint`", "Population constraint | Densely populated with over 50 % of the population residing in the INSEE carreaux", TRUE),
                                                                  sliderInput("Total Schools",label="Schools Constraint | Number of schools in the vicinity: ",
                                                                              min=0, max=max(final_data_frame$`Total Schools`), value=c(0,1697), step=100, width=280),
                                                                  sliderInput("Total Pistes",label="Number of biking paths/skate parks  in the perimeter:",
                                                                              min=0, max=max(final_data_frame$`Bike Tracks/Skate Parks`), value=c(0,69), step=4, width=280),
                                                                  sliderInput("Sports shops",label="Number of sports shops in the perimeter:",
                                                                              min=0, max=max(final_data_frame$`Sports shops`), value=c(0,424), step=50, width=280),
                                                                  h5('The best candidate is marked in golden '),
                                                                  h5('The worst candidate is marked in purple ')
                                                                  
                                                    )))),
                         tabPanel(h5("Condensed Dataframe"),bootstrapPage(div(class="outer",
                                                                              shiny::tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                                                              DTOutput("table")
                         ))),
                         tabPanel(h5("Detailed Dataframe"),bootstrapPage(div(class="outer",
                                                                             shiny::tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                                                             DTOutput("table_1"))))))
