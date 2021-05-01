# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# GEOMARKETING FINAL PROJECT - MARKET SHARE PREDICTION                      #
# ------------------------------------------------------------------------- #
# Authors : Ankita BHATTACHARYA, Sungyun HWANG, Amaratou MAHMADOU           #
# Date Created : 19.03.2021                                                 #
# Date Modified : 14.04.2021                                                #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                    I. Loading the libraries                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


library("data.table")
library("spdep")
library("sp")
library("PerformanceAnalytics")
library("parallel")
library("rjson")
library("leaflet")
library("rgdal")
library("rgeos")
library("spatialEco")
library("readxl")
library("glmnet")
library("caret")
library("MASS")
library("leaps")
library("FNN")
library("MCI")
library("geosphere")
library("shiny")
library("dplyr")
library("stringr")
library("leaflet.extras")

# Projections definition
WGS84<-"+proj=longlat +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +no_defs"
LIIE<-"+proj=lcc +lat_1=46.8 +lat_0=46.8 +lon_0=0 +k_0=0.99987742 +x_0=600000 +y_0=2200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs"


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                   II. Loading the data                                      #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

load("/cg2021/shared/data/in/ign/CONTOURS-IRIS_2-1__SHP17__FRA_2019-01-01.7z.001_data_INSEE_IRIS.RData")
load("/cg2021/shared/group3/data/tr/final_project/market_zone.RData")
load("/cg2021/shared/group3/data/client_shops.RData")
load("/cg2021/shared/group3/data/siren_competitors.RData")
load("/cg2021/shared/group3/data/client_customers.RData")
load("/cg2021/shared/group3/data/market_potential.RData")
load("/cg2021/shared/group3/data/blocs200m_iris_geometrie_name_geo2_data.RData")
load("/cg2021/shared/group3/data/cand.RData")
names(market_zones@data)

# Loading other open source variables
pistes_cyclabes <- read_excel("/cg2021/shared/group3/data/equip-sport-loisir-socio-infra-2019.xlsx", skip = 5)
sports_shops <- read_excel("/cg2021/shared/group3/data/equip-serv-commerce-infra-2019.xlsx", skip = 5)
high_schools <- read_excel("/cg2021/shared/group3/data/equip-serv-ens-2eme-degre-infra-2019.xlsx", skip = 5)
univs <- read_excel("/cg2021/shared/group3/data/equip-serv-ens-sup-form-serv-infra-2019.xlsx", skip = 5)
incomes <- read_excel("/cg2021/shared/group3/data/BASE_TD_FILO_DEC_IRIS_2017.xlsx", skip = 5)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                 III. Calculating the market share                           #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
order_<-match(market_zones@data$IRIS,market_potential$IRIS)
market_zones@data$mp<-market_potential[order_]$mp
market_zones@data$ms<-market_zones@data$sales/market_zones@data$mp
market_zones@data$ms[which(!is.finite(market_zones@data$ms))]<-0
market_zones@data$sales<-market_zones@data$ms*market_zones@data$mp
market_zones@data$cssales <-market_zones@data$sales/sum(market_zones@data$sales)

# Calculating the current market share by shop
market_share_by_shop <- aggregate(market_zones@data$ms, by = list(market_zones@data$pos_id), mean)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                 IV. Cleaning the INSEE Open Source Data                     #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 2. The INSEE IRIS sociodemographic data 
# We continue with a subset of the INSEE that we believe could go into predicting 
# the market share. Found through visual inspection
variables <- c( "P17_RP", "P17_RPMAISON", "P17_RP_3P", "P17_RP_4P", "P17_RP_5PP", 
                "P17_RP_CCIND", "C17_MENCOUPAENF", "P17_POP1524", 
                "C17_POP15P_CS3", "P17_POP", "C17_MEN",  "P17_LOG")

# To check if the variables are in the marketzones data imported from the
# previous session
variables[!(variables %in%  names(geo1)) ]
market_zones@data <- market_zones@data[c('IRIS', 'lon', 'lat', 'pos_id', 'distance', 'ms')]
market_zones@data <- merge(market_zones@data,
                           data.table(geo1@data[,c("IRIS", variables)]),
                           by="IRIS")

# Other INSEE open source data : # https://www.insee.fr/fr/statistiques/3568599?sommaire=3568656
# NB_B307 : Other sports shops in the region
market_zones@data$sports_shops <- sports_shops$NB_B307[match(market_zones@data$IRIS,sports_shops$IRIS )]
market_zones@data$sports_shops[is.na(market_zones@data$sports_shops)] <- 0
pistes_cyclabes <- pistes_cyclabes[, c("IRIS", "NB_F117",	"NB_F117_NB_AIREJEU",	
                                       "NB_F117_NB_COU",	"NB_F117_NB_ECL",
                                       "NB_F104_NB_COU",	"NB_F104_NB_ECL")]
# Roller-Skate-Vélo bicross ou freestyle - nombre de pistes
# NB_F117_NB_AIREJEU		
# Roller-Skate-Vélo bicross ou freestyle avec au moins un équipement couvert
# NB_F117_NB_COU
# Roller-Skate-Vélo bicross ou freestyle avec au moins un équipement éclairé
# NB_F117_NB_ECL
# Équipement de cyclisme
# NB_F104
# Équipement de cyclisme - nombre de pistes
# NB_F104_NB_AIREJEU
# Équipement de cyclisme avec au moins une piste couverte
# NB_F104_NB_COU
pistes_cyclabes$total_pistes <- rowSums(pistes_cyclabes[2:7])
market_zones@data$total_pistes <- pistes_cyclabes$total_pistes[match(market_zones@data$IRIS,pistes_cyclabes$IRIS )]
market_zones@data$total_pistes[is.na(market_zones@data$total_pistes)] <- 0

# Schools : collège, lycée & univs
high_schools$hs_total <- rowSums(high_schools[7:30]) 
univs$univs_total <- rowSums(univs[7:26])

# No mismatching IRIS
high_schools$IRIS[!(high_schools$IRIS %in% univs$IRIS)]
univs$IRIS[!(univs$IRIS %in% high_schools$IRIS)]

# Creating a new dataframe
schools <- cbind.data.frame(high_schools$IRIS, univs$univs_total+high_schools$hs_total)
names(schools) <- c("IRIS", "Total Schools")
market_zones@data$Total_Schools <- schools$`Total Schools`[match(market_zones@data$IRIS,schools$IRIS )]
market_zones@data$Total_Schools[is.na(market_zones@data$Total_Schools)] <- 0

market_zones@data$median_income <- incomes$DEC_MED17[match(market_zones@data$IRIS,incomes$IRIS )]
market_zones@data$median_income[is.na(market_zones@data$median_income)] <- median(market_zones@data$median_income, na.rm = T)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                   V. Computing the number of competitors                    #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
head(siren_competitors)

# We do this using the over function, by finding the SIRENs in the trade zone of
# our client:
# Following methods seen in class : we use the long-lat data to obtain the IRIS from geo1.  
spatial_siren <- SpatialPointsDataFrame(coords = siren_competitors[,c("longitude","latitude")], data = siren_competitors, proj4string = CRS(WGS84))
geo1_proj <- spTransform(geo1, CRS(WGS84))
over_siren_in_geo1 <- over(spatial_siren, geo1_proj)
# Check if we got the IRIS
"IRIS" %in% names(over_siren_in_geo1)
# Getting the number of competitors by IRIS
total_comeptitors <- aggregate(over_siren_in_geo1, by=list(over_siren_in_geo1$IRIS), FUN=length)
competitors <- total_comeptitors[,c('Group.1', 'IRIS')]
colnames(competitors) <- c('IRIS', 'nb_competitors')
market_zones@data <- merge(market_zones@data, competitors, by="IRIS")
rm(geo1_proj); rm(over_siren_in_geo1)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#               VI. Spation Interaction Model : Linear-log                    #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Model preparation: building some new variables of interest
# Caractéristiques des logements
market_zones@data$prop_RPMAISON <- market_zones@data$P17_RPMAISON / market_zones@data$P17_RP
# Caractéristiques des résidences principales
market_zones@data$prop_RP_3PP <-(market_zones@data$P17_RP_3P + market_zones@data$P17_RP_4P + market_zones@data$P17_RP_5PP) / market_zones@data$P17_RP
# Caractéristiques des logements avec un chauffage individuel
market_zones@data$prop_P17_RP_CCIND <- market_zones@data$P17_RP_CCIND / market_zones@data$P17_RP
# Caractéristiques des ménages
market_zones@data$prop_MENCOUPAENF<- market_zones@data$C17_MENCOUPAENF / market_zones@data$C17_MEN
# Caractéristiques des professions
market_zones@data$prop_POP15P_CS3 <-market_zones@data$C17_POP15P_CS3 / market_zones@data$P17_POP
# Caractéristiques des personnes : jeunes
market_zones@data$prop_P17_POP1524 <-market_zones@data$P17_POP1524 / market_zones@data$P17_POP

# Other variables from INSEE open sources
market_zones@data$prop_schools <-market_zones@data$Total_Schools / sum(market_zones@data$Total_Schools)
market_zones@data$prop_sports_shops <- market_zones@data$sports_shops/sum(market_zones@data$sports_shops)
market_zones@data$prop_pistes_cyclabes <- market_zones@data$total_pistes/sum(market_zones@data$total_pistes)
market_zones@data$prop_median_income <- market_zones@data$median_income/sum(market_zones@data$median_income)



# Performing the Stepwise Regression 
model_vars <- c('pos_id', 'ms', 'distance', 'nb_competitors',
                'prop_RPMAISON', 'prop_MENCOUPAENF', 'prop_P17_RP_CCIND',
                'prop_POP15P_CS3',  'prop_P17_POP1524', 'prop_RP_3PP', 
                'prop_median_income',
                'prop_schools', 'prop_sports_shops', 'prop_pistes_cyclabes')

# Check if all vars are included
setdiff(model_vars, names(market_zones@data))
model_dataset <- market_zones@data[model_vars]
model_dataset <- na.omit(model_dataset)

# Using Stepwise regression 
# As the data set contains only 10 predictors, we’ll vary nvmax from 1 to 5 resulting to the identification of the 5 best models with different sizes: the best 1-variable model, the best 2-variables model, …, the best 5-variables model.
# 
# We’ll use 10-fold cross-validation to estimate the average prediction error (RMSE) of each of the 5 models 
# The RMSE statistical metric is used to compare the 5 models and to automatically choose the best one, where best is defined as the model that minimize the RMSE.

train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(ms ~ log(distance) + ., data = model_dataset[, c( 'ms', 'distance', 'nb_competitors',
                                                                      'prop_RPMAISON', 'prop_MENCOUPAENF', 'prop_P17_RP_CCIND',
                                                                      'prop_POP15P_CS3',  'prop_P17_POP1524', 'prop_RP_3PP', 
                                                                      'prop_median_income',
                                                                      'prop_schools', 'prop_sports_shops', 'prop_pistes_cyclabes')],
                    method = "leapSeq", # Stepwise
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control)
step.model$results
# The output above shows different metrics and their standard deviation for comparing the accuracy of the 5 best models. Columns are:

# nvmax: the number of variable in the model. For example nvmax = 2, specify the best 2-variables model

step.model$bestTune
summary(step.model$finalModel)
# Running the regression on the selected 5 variables
final_ols <- lm(ms ~ log(distance) + prop_MENCOUPAENF+  prop_P17_POP1524, data = model_dataset[, c( 'ms', 'distance', 'nb_competitors',
                'prop_RPMAISON', 'prop_MENCOUPAENF', 'prop_P17_RP_CCIND',
                'prop_POP15P_CS3',  'prop_P17_POP1524', 'prop_RP_3PP', 
                'prop_median_income',
                'prop_schools', 'prop_sports_shops', 'prop_pistes_cyclabes')])
summary(final_ols)
plot(final_ols)


# Check the fit ! 
model_dataset$fitted <-  fitted(final_ols)
model_dataset$residuals <- residuals(final_ols)
aggregate(model_dataset$fitted, by = list(model_dataset$pos_id), mean)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                 VII. Finding the new Points of Sales                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# We choose our candidates based on our competitors database.
set.seed(8765) 
# candidates = 10
# # Sampling 1O points from the competitors existing locations
# candidate_positions <- spatial_siren[sample(1:length(spatial_siren), candidates),]

competitor_trade_zone <- function(x) {
  
  competitor_position <- coordinates(spTransform(candidate_positions[x,], CRS(LIIE)))
  # We get all geo1 variables of the 500 closest (euclid dist) IRIS to the ith_competitor_position.
  # This creates a market zone
  IRIS_positions <- data.frame(coordinates(spTransform(SpatialPoints(coords = geo1@data[,c("lon","lat")], 
                                                                     proj4string = CRS(WGS84)), CRS(LIIE))))
  colnames(IRIS_positions) <- c("x","y")
  # To build the market zone, we need the variables from the INSEE geo1 database. 
  # We use the FNN library to obtain the 1000 nearest neighbours
  # get.knn is a fast NN searching function
  # nn.index is a n x k matrix for the nearest neighbor indices
  candidate_market_zone <- geo1@data[c(get.knnx(data = IRIS_positions,
                                                query = competitor_position, k = 1000)$nn.index),]
  
  # Getting variables from the INSEE geo1 database
  candidate_market_zone <- candidate_market_zone[,c('IRIS', 'lon', 'lat', variables)]
  
  # Merge this new market zones with the competitors to obtain the competitors of our new database
  candidate_market_zone <- merge(candidate_market_zone, competitors, by="IRIS")
  
  # Here we re-use professor's code to calculate the travel time from origin to destination
  # the origins are the customers
  origin <- candidate_market_zone[,c("IRIS","lon","lat")]
  setnames(origin, c("id","x","y"))
  # the destination is the new point of sales
  destination <- data.frame(id=paste("candidate_pos_",x,sep=""), coordinates(candidate_positions[x,]))
  setnames(destination,c("id","x","y"))
  # Getting the Haversine distance 
  candidate_market_zone$distance <-distHaversine(origin[,c("x","y")],destination[,c("x","y")]) 
  
  
  # Create an index for the candidate position 
  candidate_pos_x <- paste("candidate_pos_", x, sep="")
  candidate_market_zone$posid <- candidate_pos_x
  
  return(candidate_market_zone)
}


final_candidate_market_zones <- data.frame()
for (x in 1:length(candidate_positions)) {
  print(x)
  final_candidate_market_zones <- rbind(final_candidate_market_zones, competitor_trade_zone(x))
}
dim(final_candidate_market_zones)

# Check for duplicates - none found ! 
dim(final_candidate_market_zones)
dim(final_candidate_market_zones %>% distinct())

# However our database is incomplete. We need to further develop our database by adding the other variables 
# Ménages avec enfant
final_candidate_market_zones$prop_MENCOUPAENF<- final_candidate_market_zones$C17_MENCOUPAENF / final_candidate_market_zones$C17_MEN
# Population entre 15 et 24
final_candidate_market_zones$prop_P17_POP1524 <-final_candidate_market_zones$P17_POP1524 / final_candidate_market_zones$P17_POP
# Personnes dans le ménage exerçant une profession supérieure/intellectuelle
final_candidate_market_zones$prop_POP15P_CS3 <-final_candidate_market_zones$C17_POP15P_CS3 / final_candidate_market_zones$P17_POP
# Maisons qui sont aussi les résidences principales
final_candidate_market_zones$prop_RPMAISON <- final_candidate_market_zones$P17_RPMAISON / final_candidate_market_zones$P17_RP
# Logements plus que 3 pièces 
final_candidate_market_zones$prop_RP_3PP <-(final_candidate_market_zones$P17_RP_3P + final_candidate_market_zones$P17_RP_4P + final_candidate_market_zones$P17_RP_5PP) / final_candidate_market_zones$P17_RP
# Logements avec un chauffage individuel
final_candidate_market_zones$prop_P17_RP_CCIND <- final_candidate_market_zones$P17_RP_CCIND / final_candidate_market_zones$P17_RP
# Sports shops in the IRIS of the shop
final_candidate_market_zones$sports_shops <- sports_shops$NB_B307[match(final_candidate_market_zones$IRIS,sports_shops$IRIS )]
# Setting NAs to 0
final_candidate_market_zones$sports_shops[is.na(final_candidate_market_zones$sports_shops)] <- 0
# Schools in the IRIS of the shop
final_candidate_market_zones$Total_Schools <- schools$`Total Schools`[match(final_candidate_market_zones$IRIS,schools$IRIS )]
# Setting NAs to 0
final_candidate_market_zones$Total_Schools[is.na(final_candidate_market_zones$Total_Schools)] <- 0
# Aménagements sportifs lié à notre client 
final_candidate_market_zones$total_pistes <- pistes_cyclabes$total_pistes[match(final_candidate_market_zones$IRIS,pistes_cyclabes$IRIS )]
# Setting NAs to 0
final_candidate_market_zones$total_pistes[is.na(final_candidate_market_zones$total_pistes)] <- 0
# Median income
final_candidate_market_zones$median_income <- incomes$DEC_MED17[match(final_candidate_market_zones$IRIS,incomes$IRIS )]
# Median imputation of missing values
final_candidate_market_zones$median_income[is.na(final_candidate_market_zones$median_income)] <- median(final_candidate_market_zones$median_income, na.rm = T)
# Calculating the proportion of schools relative to total
final_candidate_market_zones$prop_schools <- final_candidate_market_zones$Total_Schools / sum(final_candidate_market_zones$Total_Schools)
# Calculating the proportion of sports shops relative to total
final_candidate_market_zones$prop_sports_shops <- final_candidate_market_zones$sports_shops/sum(final_candidate_market_zones$sports_shops)
# Calculating the proportion of sports facilities relative to total
final_candidate_market_zones$prop_pistes_cyclabes <- final_candidate_market_zones$total_pistes/sum(final_candidate_market_zones$total_pistes)
# Calculating the proportion of median income relative to total
final_candidate_market_zones$prop_median_income <- final_candidate_market_zones$median_income/sum(final_candidate_market_zones$median_income)

# Calculating the predicted market shares 
final_candidate_market_zones$market_share_predicted <- predict(final_ols, newdata = final_candidate_market_zones)
# Calculate sales 
final_candidate_market_zones$mp <- market_potential$mp[match(final_candidate_market_zones$IRIS, market_potential$IRIS)]
final_candidate_market_zones$mp[is.na(final_candidate_market_zones$mp)] <- 0
final_candidate_market_zones$sales <- final_candidate_market_zones$market_share_predicted*final_candidate_market_zones$mp
final_candidate_market_zones$sales[is.na(final_candidate_market_zones$sales)] <- 0

# Competitors (subtracting itself)
final_candidate_market_zones$nb_competitors <- final_candidate_market_zones$nb_competitors-1
final_candidate_market_zones$cssales <- cumsum(final_candidate_market_zones$sales)/sum(final_candidate_market_zones$sales) #Apply a cumulative sales function

# Finding IRIS of the shop
candidate_IRIS <- over(candidate_positions, geo1)
posid <- unique(final_candidate_market_zones$posid)
final_data_frame <- cbind.data.frame(posid, candidate_IRIS[, c("IRIS", 'lon', 'lat', variables)])
final_candidate_market_zones$shop_IRIS <- final_data_frame$IRIS[match( final_candidate_market_zones$posid, final_data_frame$posid)]

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#           VIII. Creation of condensed data frame                            #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# We try to add several characteristics
# Constraint 1 : Schools
agg <-  aggregate(final_candidate_market_zones[, c("Total_Schools", "sports_shops", 
                                                   "total_pistes", "median_income",
                                                   "sales", "mp", "nb_competitors",
                                                   "C17_MENCOUPAENF", "P17_POP1524")],
                by = list(final_candidate_market_zones$posid),
                FUN = sum)
final_data_frame$sports_shops <- agg$sports_shops[match(final_data_frame$posid,agg$Group.1 )]
final_data_frame$prop_sports_shops <- final_data_frame$sports_shops/sum(final_data_frame$sports_shops)

# Schools in the IRIS of the shop
final_data_frame$Total_Schools <- agg$Total_Schools[match(final_data_frame$posid,agg$Group.1 )]
final_data_frame$prop_total_schools <- final_data_frame$Total_Schools/sum(final_data_frame$Total_Schools)

# Aménagements sportifs lié à notre client 
final_data_frame$total_pistes <- agg$total_pistes[match(final_data_frame$posid,agg$Group.1 )]
final_data_frame$prop_total_pistes <- final_data_frame$total_pistes/sum(final_data_frame$total_pistes)

# Median income
final_data_frame$median_income <- mean(agg$median_income)[match(final_data_frame$posid,agg$Group.1 )]
# Median imputation of missing values
final_data_frame$median_income[is.na(final_data_frame$median_income)] <- median(final_data_frame$median_income, na.rm = T)
final_data_frame$prop_median_income <- final_data_frame$median_income/sum(final_data_frame$median_income)

# Youth proportion
final_data_frame$Youth <- agg$P17_POP1524[match(final_data_frame$posid,agg$Group.1 )]
final_data_frame$prop_youth <- final_data_frame$Youth/sum(final_data_frame$Youth)

# Youth proportion
final_data_frame$menenfant <- agg$C17_MENCOUPAENF[match(final_data_frame$posid,agg$Group.1 )]
final_data_frame$prop_menenfant <- final_data_frame$menenfant/sum(final_data_frame$menenfant)


#Replacing all NA -> 0
final_data_frame[is.na(final_data_frame)] <- 0
# Add number of competitors
final_data_frame$competitors <-  agg$nb_competitors[match(final_data_frame$posid,agg$Group.1 )]
# Add market share and sales
final_data_frame$mp <- agg$mp[match(final_data_frame$posid,agg$Group.1 )]
final_data_frame$sales <- agg$sales[match(final_data_frame$posid,agg$Group.1 )]
final_data_frame$ms <- final_data_frame$sales/final_data_frame$mp

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#          IX. Constraints for our Candidate Points of Sales                  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Constraint 1: Schools in the vicinity
# Let's find a good threshold
ggplot(data =final_data_frame, aes(x = `Total Schools`))+
      geom_density(color="forestgreen", fill ="forestgreen", alpha=0.4)

final_data_frame$first_constraint <- ifelse(final_data_frame$Total_Schools>500, T, F)
table(final_data_frame$first_constraint)

# Population constraint : We obtained data from the geo2 dataset on the
# population by carreau and the idea was to rank the shops according to the
# population of the tradezone. We do so by imposing a constraint of having more
# than the median population of the carreau as we are looking for medium to
# densely populated areas atleast.

# We ran this first, and then saved the results under cand.RData as the data
# would occupy a lot of space on the server. For verification please uncomment
# these lines and run them (they take a very long time and a lot of space on the
# server


# geo2_data_sp<-SpatialPointsDataFrame(coords = geo2_data[,c("lon","lat")],data = geo2_data,proj4string = CRS(WGS84))
# over_geo2_data_sp_in_iris_position<-over(geo2_data_sp,market_zones)
# Constraint assumption, more than the median population in the carreau : 
# subset_geo2_data <- subset(geo2_data, ind_c > median(geo2_data$ind_c))
# geo2_data_sp <- SpatialPointsDataFrame(coords = subset_geo2_data[,c("lon","lat")],data = subset_geo2_data, proj4string = CRS(WGS84))
# geo2_data_sp_LIIE <- spTransform(geo2_data_sp,CRS(LIIE))
# geo2_data_sp_LIIE_blocs <- gBuffer(spgeom = geo2_data_sp_LIIE,byid = TRUE,width = 100,capStyle = "SQUARE")
# rm(subset_geo2_data)
# rm(geo2_data_sp)
# rm(geo2_data_sp_LIIE)
# blocs_WGS84 <- spTransform(gBuffer(spgeom = geo2_data_sp_LIIE_blocs,byid = TRUE,width = 100,capStyle = "SQUARE"),CRS(WGS84))
# rm(geo2_data_sp_LIIE_blocs)
# blocs_WGS84 <- spChFIDs(blocs_WGS84,as.character(blocs_WGS84$idgeo2))
# coord_points <- SpatialPointsDataFrame(data = market_zones@data[,c('lon','lat')],coords = market_zones@data[,c('lon','lat')],
#                                        proj4string = CRS(WGS84))
# 
# coord_points@data
# res <- over(coord_points, blocs_WGS84)
# res_new <- over(candidate_positions[,c('longitude','latitude')], blocs_WGS84)
# w_new <- which(!is.na(res_new$idgeo2))
# length(w_new)
# candidate_positions$second_constraint <- FALSE
# candidate_positions$second_constraint[w_new] <- TRUE
# candidate_positions@data$second_constraint
# save(candidate_positions, file = "cand.RData")

final_data_frame <- cbind.data.frame(final_data_frame, candidate_positions@data$second_constraint)
final_data_frame$second_constraint <- final_data_frame$`candidate_positions@data$second_constraint`
# Adding these constraints to the final df
final_candidate_market_zones$first_constraint <- final_data_frame$first_constraint[match(final_candidate_market_zones$posid, final_data_frame$posid)]
final_candidate_market_zones$second_constraint <- final_data_frame$second_constraint[match(final_candidate_market_zones$posid, final_data_frame$posid)]


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                X. Further Refinement : Huff Model                           #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

huff <- market_zones@data[market_zones@data$distance<2000,]
# huff <- market_zones@data
names(huff)
huff_numerator <- function(destinations_attractiveness, alpha, distance, beta){
  return((destinations_attractiveness ^ alpha) / (distance ^ beta))
}

# Composite measure of Attractiveness
huff$attractiveness <- 0.25*huff$median_income + 0.05*huff$Total_Schools + 0.05*huff$total_pistes + 0.25*huff$sports_shops + 0.20*huff$C17_MENCOUPAENF + 0.25*huff$P17_POP1524


# Parameter Optimization

# First let us construct the dataframe
huff_params <- huff[, c("IRIS", "pos_id" , "ms", "distance", "attractiveness")]
# Number of competitors for each IRIS
huff_params$competitor_count <- competitors$nb_competitors[match(huff_params$IRIS, competitors$IRIS)]
origins_name <- huff_params$IRIS

# Now we will calculate empirical frequencies of the distance, market share and
# a composite store attractiveness measure
tilde_dist <-  aggregate(huff_params$distance, by = list(origins_name), prod)
huff_params$tilde_dist <- tilde_dist$x[match(huff_params$IRIS, tilde_dist$Group.1)]
tilde_dist_count <-  aggregate(huff_params$distance, by = list(origins_name), length)
huff_params$tilde_dist_count <- tilde_dist_count$x[match(huff_params$IRIS, tilde_dist$Group.1)]
huff_params$tilde_dist <- huff_params$tilde_dist^(1/(huff_params$tilde_dist_count+huff_params$competitor_count))

tilde_ms <-  aggregate(huff_params$ms, by = list(origins_name), prod)
huff_params$tilde_ms <- tilde_ms$x[match(huff_params$IRIS, tilde_ms$Group.1)]
tilde_ms_count <-  aggregate(huff_params$ms, by = list(origins_name), length)
huff_params$tilde_ms_count <- tilde_ms_count$x[match(huff_params$IRIS, tilde_ms$Group.1)]
huff_params$tilde_ms <- huff_params$tilde_ms^(1/(huff_params$tilde_ms_count+huff_params$competitor_count))

tilde_attractiveness <-  aggregate(huff_params$attractiveness, by = list(origins_name), prod)
huff_params$tilde_attractiveness <- tilde_attractiveness$x[match(huff_params$IRIS, tilde_attractiveness$Group.1)]
tilde_attractiveness_count <-  aggregate(huff_params$attractiveness, by = list(origins_name), length)
huff_params$tilde_attractiveness_count <- tilde_attractiveness_count$x[match(huff_params$IRIS, tilde_attractiveness$Group.1)]
huff_params$tilde_attractiveness <- huff_params$tilde_attractiveness^(1/(huff_params$tilde_attractiveness_count+huff_params$competitor_count))

# Removing NAs
huff_params$y <- ifelse(is.na(log(huff_params$ms/huff_params$tilde_ms)) , 0,log(huff_params$ms/huff_params$tilde_ms))
huff_params$x1 <- ifelse(is.na(log(huff_params$distance/huff_params$tilde_dist)) , 0, log(huff_params$distance/huff_params$tilde_dist))
huff_params$x2 <- ifelse(is.na(log(huff_params$attractiveness/huff_params$tilde_attractiveness)) , 0, log(huff_params$attractiveness/huff_params$tilde_attractiveness))

# Model Calibration
d = lm(y~x1+x2, data = huff_params)
summary(d)

# Here we assign the optimized parameters from the OLS
distance_param <- coef(d)[2]
attrac_param <- coef(d)[3]

final_candidates_huff <- final_candidate_market_zones[, c("posid", "IRIS", 
                                                          "distance", 
                                                          "Total_Schools", 
                                                          "sports_shops", 
                                                          "median_income", 
                                                          "total_pistes", 
                                                          "C17_MENCOUPAENF", 
                                                          "P17_POP1524" )] 
final_candidates_huff$attractiveness <- 0.25*final_candidates_huff$median_income + 0.05*final_candidates_huff$Total_Schools + 0.05*final_candidates_huff$total_pistes + 0.25*final_candidates_huff$sports_shops + 0.20*final_candidates_huff$C17_MENCOUPAENF + 0.25*final_candidates_huff$P17_POP1524


# Current Market Share
huffoutput <- mapply(huff_numerator, huff$attractiveness, 
                     attrac_param, huff$distance, distance_param)

# Assuming we have the origin location names as vector in R (origins_name)
origins_name <- huff$IRIS
sum_huff_location <- aggregate(huffoutput, by = list(origins_name), sum)
# Rename the fields of the data frame
names(sum_huff_location) <- c("origins_name", "sum_huff")
# Merge numerator and denominator
destinations_name <- huff$pos_id
distance <- huff$distance
out <- merge(data.frame(origins_name, destinations_name, distance, huffoutput), sum_huff_location, by="origins_name") 
# Calculate the huff probabilities
out$huff_probability <- with(out, huffoutput / sum_huff)
head(out)
out$mp <- market_potential$mp[match(out$origins_name, market_potential$IRIS)]
out$penetration <- out$huff_probability/out$mp

aggregate(out$penetration, by = list(out$destinations_name), sum)


#  Predicting for the 10 new POS

huffoutput <- mapply(huff_numerator, final_candidates_huff$attractiveness, 
                     attrac_param, final_candidates_huff$distance, distance_param)

# Assuming we have the origin location names as vector in R (origins_name)
origins_name <- final_candidates_huff$IRIS
sum_huff_location <- aggregate(huffoutput, by = list(origins_name), sum)
# Rename the fields of the data frame
names(sum_huff_location) <- c("origins_name", "sum_huff")
# Merge numerator and denominator
destinations_name <- final_candidates_huff$posid
distance <- final_candidates_huff$distance
out <- merge(data.frame(origins_name, destinations_name, distance, huffoutput), sum_huff_location, by="origins_name") 
# Calculate the huff probabilities
out$huff_probability <- with(out, huffoutput / sum_huff)
out$mp <- market_potential$mp[match(out$origins_name, market_potential$IRIS)]
out$penetration <- out$huff_probability/out$mp

# Aggregating the rates by the shop
aggregate(out$penetration, by = list(out$destinations_name), mean)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                XI. Final cleaning !                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
head(final_data_frame)
final_data_frame <- final_data_frame[, c("posid", "IRIS", "lon", "lat", "ms", 
                                         "sales", "competitors", "sports_shops",
                                         "Total_Schools", "total_pistes",
                                          "prop_youth", "prop_menenfant",
                                          "first_constraint", "second_constraint")]

names(final_data_frame) <- c("posid", " Shop IRIS", "lon", "lat", "Predicted Market Share", 
                             "Predicted Sales", "Competitors", "Sports shops",
                             "Total Schools", "Bike Tracks/Skate Parks",
                             "% Youth, approximation", "% Families w children", 
                             "Schools Constraint", "Population Constraint")


final_data_frame$`Predicted Market Share` <- round(final_data_frame$`Predicted Market Share`, 4)
final_data_frame$`Predicted Sales` <- round(final_data_frame$`Predicted Sales`, 2)
final_data_frame$`% Families w children` <- round(final_data_frame$`% Families w children`,2)
final_data_frame$`% Youth, approximation` <- round(final_data_frame$`% Youth`, 2)

head(final_data_frame)

final_candidate_market_zones[is.na(final_candidate_market_zones)] <- 0
final_candidate_market_zones <- final_candidate_market_zones[, c("posid", "IRIS", 
                                                                 "lon", "lat","shop_IRIS",
                                          "distance", "market_share_predicted",
                                          "sales", "cssales", "nb_competitors", 
                                          "prop_MENCOUPAENF", "prop_P17_POP1524", 
                                          "Total_Schools", "total_pistes", 
                                          "sports_shops", "first_constraint", 
                                          "second_constraint")]



names(final_candidate_market_zones) <- c("posid", "IRIS", 
                             "lon", "lat","shop IRIS",
                             "distance (metres)", "Predicted Market Share",
                             "Predicted Sales", "Cum. Sales", "Competitors", 
                             "% families with children", "% Youth", 
                             "Total Schools", "Bike Tracks/Skate Parks", 
                             "Sports shops", "School constraint", 
                             "Population constraint")


final_candidate_market_zones$`Predicted Market Share` <- round(final_candidate_market_zones$`Predicted Market Share`, 4)
final_candidate_market_zones$`Predicted Sales` <- round(final_candidate_market_zones$`Predicted Sales`, 2)
final_candidate_market_zones$`Cum. Sales` <- round(final_candidate_market_zones$`Cum. Sales`, 2)
final_candidate_market_zones$`% families with children` <- round(final_candidate_market_zones$`% families with children`, 2)
final_candidate_market_zones$`% Youth` <- round(final_candidate_market_zones$`% Youth`, 2)
head(final_candidate_market_zones)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#              XII.  Leaflet plots !                                          #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Finding the best and worst candidates 
best_cand <- final_data_frame[which.max(final_data_frame$`Predicted Market Share`),]
worst_cand <- final_data_frame[which.min(final_data_frame$`Predicted Market Share`),]


# best_cl <- final_candidate_market_zones[posid == final_data_frame$posid[which.max(final_data_frame$ms)], c("shop_IRIS", "sales", "lon", "lat")]
best_cl <- final_candidate_market_zones[, c("shop IRIS", "IRIS", "Cum. Sales", "lon", "lat", "Competitors", "Total Schools", "Bike Tracks/Skate Parks", 
                                         "Sports shops","School constraint", 
                                         "Population constraint")]
best <- geo1[na.omit(unique(best_cl$IRIS)),]
# worst <- final_data_frame$posid[which.min(final_data_frame$ms)]
best@data <- best_cl
best@data$zone<-cut(best@data$`Cum. Sales`,breaks = c(-3,-1,0,1),right = TRUE,include.lowest = TRUE,labels = c("zone1","zone2","zone3"))
best@data$color<-cut(best@data$`Cum. Sales`,breaks = c(-3,-1,0,1),right = TRUE,include.lowest = TRUE,labels = c("firebrick","orange","khaki"))
pal <- colorQuantile("YlOrRd", domain = best@data$`Cum. Sales`,n=4)

candidates_leaflet <- leaflet() %>%  addTiles(urlTemplate = Token_map_box,attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
  addPolygons(data = best, fillColor = ~pal(best@data$`Cum. Sales`) , color='firebrick',
              fillOpacity = 0.5, weight = 3, label = paste("IRIS : ",best@data$IRIS," // ",round((best@data$`Cum. Sales`*100), 2)," %",sep=""),
              highlightOptions = highlightOptions(color = "white", weight = 7,bringToFront = FALSE,fillOpacity = 0.5),
              labelOptions = labelOptions(noHide = FALSE,direction = 'top',offset=c(0,0),textOnly = TRUE,
                                          style=list('color'='rgba(0,0,0,1)','font-family'= 'Arial Black','font-style'= 'bold',
                                                     'box-shadow' = '0px 0px rgba(0,0,0,0.25)','font-size' = '14px',
                                                     'background-color'='rgba(255,255,255,0.7)','border-color' = 'rgba(0,0,0,0)')))%>%
  addCircleMarkers(data = best_cand,color="gold", weight = 30)%>%
  addCircleMarkers(data = worst_cand,color="purple", weight = 30)


# Optimal POS : candidate 6
# ---------------------------------------------------------------------------- #

# Map for the 63 locations and trade area : could not implement on shiny due to
# memory constraints

# Let us reload the market zones data
load("/cg2021/shared/group3/data/tr/final_project/market_zone.RData")
order_<-match(market_zones@data$IRIS,market_potential$IRIS)
market_zones@data$mp<-market_potential[order_]$mp
market_zones@data$ms<-market_zones@data$sales/market_zones@data$mp
market_zones@data$ms[which(!is.finite(market_zones@data$ms))]<-0
market_zones@data$sales<-market_zones@data$ms*market_zones@data$mp
market_zones@data$cssales <-market_zones@data$sales/sum(market_zones@data$sales)

# Converting this df into a spatial polygons df
df <- as.data.frame(market_zones@data[1:8000,])
rf <- market_zones
rf@data <- df 
names(rf@data)

pal <- colorQuantile("YlOrRd", domain = rf@data$cssales,n=5) 
trade_network_63 <- leaflet() %>% addTiles(urlTemplate = Token_map_box)%>%
  addPolygons(data= rf, stroke = TRUE,color ="black",weight = 1.5, fillOpacity = 0.6, smoothFactor = 0.1,fillColor=~pal(rf@data$cssales),
              label = paste("IRIS : ",rf@data$IRIS," // ",round((rf@data$cssales*100),4)," %",sep=""),
              highlightOptions = highlightOptions(color = "white", weight = 7,bringToFront = FALSE,fillOpacity = 0.5),
              labelOptions = labelOptions(noHide = FALSE,direction = 'top',offset=c(0,0),textOnly = TRUE,
                                          style=list('color'='rgba(0,0,0,1)','font-family'= 'Arial Black','font-style'= 'bold',
                                                     'box-shadow' = '0px 0px rgba(0,0,0,0.25)','font-size' = '14px',
                                                     'background-color'='rgba(255,255,255,0.7)','border-color' = 'rgba(0,0,0,0)')))


path <- "/cg2021/shared/group3"
save(trade_network_63, file=paste(path, "/deployment/www/trade_network_63.RData", sep=''))
save(rf, file=paste(path, "/deployment/www/rf.RData", sep=''))
save(candidates_leaflet, file=paste(path, "/deployment/www/candidates.RData", sep=''))
save(final_candidate_market_zones, file=paste(path, "/deployment/www/final_candidate_market_zones.RData", sep=''))
save(final_data_frame, file=paste(path, "/deployment/www/final_data_frame.RData", sep=''))
save(candidate_IRIS, file=paste(path, "/deployment/www/candidates_IRIS.RData", sep=''))
save(best, file=paste(path, "/deployment/www/best.RData", sep=''))
save(best_cand, file=paste(path, "/deployment/www/best_cand.RData", sep=''))
save(worst_cand, file=paste(path, "/deployment/www/worst_cand.RData", sep=''))
save(huff_final, file=paste(path, "/deployment/www/huff_final.RData", sep=''))
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #



