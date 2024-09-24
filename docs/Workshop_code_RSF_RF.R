##RSF workshop - OTN Symposium 2024
#Authors: Jonathan Rodemann, Lucas Griffin, Robert Lennox
#Source Data - Rodemann et al., 2024 (In Review)

# Examples of acoustic telemetry (AT) with the use of resource selection functions (RSFs):

# Selby et al., 2019. Juvenile hawksbill residency and habitat use within a Caribbean marine protected area. Endangered Species Research.
# First known RSF approach with AT.

# Griffin et al., 2021. Space use and relative habitat selection for immature green turtles within a Caribbean marine protected area. Animal Biotelemetry.
# Similar to Selby et al., 2019 with GLMMs.

# Griffin et al., 2021. A novel framework to predict relative habitat selection in aquatic systems: Applying machine learning and resource selection functions to acoustic telemetry data from multiple shark species. Frontiers in Marine Science.
# RSF worked examples with random forest (code included).

# Brownscombe et al., 2022. Applications of telemetry to fish habitat science and management. Canadian Journal of Fisheries and Aquatic Sciences.
# Advocates for the inclusion of AT-based habitat selection models into management.

# Bangley et al., 2022. Modeling the probability of overlap between marine fish distributions and marine renewable energy infrastructure using acoustic telemetry data.
# Similar approach to RSFs with background points but 'SDMs,' grid approach, and with boosted regression trees.

# Landovskis et al., 2024. Habitat and movement selection processes of American lobster within a restricted bay in the Bras d’Or Lake, Nova Scotia, Canada. Movement Ecology.
# RSFs and VPS AT data to analyze lobster movements.

# Kressler et al., 2024. Habitat selection and spatial behavior of vulnerable juvenile lemon sharks: Implications for conservation. Ecological Indicators.
# RSFs and juvenile lemon sharks.

# van Zinnicq Bergmann et al., 2024. Intraguild processes drive space-use patterns in a large-bodied marine predator community. Journal of Animal Ecology.
# RSFs to study predator space-use patterns.

###RSFs using Random Forest models####

# Additional details on machine learning algorithms at "Machine Learning with Telemetry Data -Jake Brownscombe" on Thursday!

#Load required libraries
library(terra, exclude = 'resample') #work with spatial data - rasters
library(raster) #work with spatial data - rasters
library(sf) #Work with spatial data - shapefiles
library(sfheaders) #work with spatial data
library(chron) #visualization of acoustic data
library(splitstackshape) #break up data into smaller columns
library(scales)#visualization of acoustic data
library(mlr3verse) # Machine learning
library(mlr3spatial) #spatial machine learning
library(randomForest) #Machine learning
library(iml) #result interpretation
library(ranger) #Machine learning
library(tidyverse) #organize and visualize data
library(ggmap) #plotting onto map
library(beepr) #beeps when code is done running

#Load in data
setwd('~/Desktop/OTN_RSF_Workshop-main/') #set your working directory
dt <- read.csv('data/Acoustic_data.csv') #Acoustic data
tags <- read.csv('data/Tag_Metadata.csv') #tag metadata
stations <- read.csv('data/Stations.csv') #station metadata

#look at dataframes
dim(dt)
str(dt)

#organize detection data, correct for time, visualize abacus plot
dt <- dt %>% dplyr::select(-c(Transmitter.Name, Transmitter.Serial, Sensor.Value, Sensor.Unit, Latitude, Longitude, Transmitter.Type, Sensor.Precision))

#change Date_time into posixct
dt$Date_time <- as.POSIXct(dt$Date.and.Time..UTC., format='%Y-%m-%d %H:%M:%S', tz='UTC')

#abacus plot of detections
trout_det <- ggplot(dt, aes(Date_time, Transmitter, col = Receiver)) + 
  geom_point() +
  scale_x_datetime(labels = date_format("%Y-%m-%d"),
                   date_breaks = "3 months", limits = as.POSIXct(c('2020-01-27 00:00:00', '2021-01-01 00:00:00')))+
  labs(x = "Date",  y = "Transmitter") +
  # Change the angle and text size of x-axis tick labels to make more readable in final plots
  theme_bw()+
  theme(axis.text.x=element_text(angle= 50, size=10, vjust = 1, hjust=1),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.y=element_text(size=8))+
  labs(title = "Trout Detections")
trout_det

#Now that we have visualized the data, let's merge it with the station data to get lat and long
head(dt)
unique(dt$Station.Name)
head(stations)
dat <- merge(dt, stations, by="Station.Name")
dat <- dat %>% select(-c(X, Receiver.x, Receiver.y, SAV))
str(dat)

#Ok, we have the acoustic data merged with station data so we have locations. Let's calculate Center of Activities (COAs)
#split up data into time chunks - want the right balance of time based on your tag timing to reduce autocorrelation but also create enough data
ex <- seq(from = trunc(min(dat$Date_time, na.rm = TRUE), "day"), 
          to = trunc(max(dat$Date_time, na.rm = TRUE), "day") + 
            86400, by = 3600) #We split this data up into 1 hour time bins
dat$DateTime <- cut(dat$Date_time, breaks = ex) #cut up the data to calculate COAs
str(dat)

#Calculation of COAs
coadat <- dat %>% group_by(DateTime, Transmitter) %>% mutate(n = n()) %>% filter(n >= 5) %>% #Take out any time bin/fish combination with less than 5 detections (COAs will be comprised of at least 5 detections)
  group_by(DateTime, Transmitter) %>% mutate(lat.coa = mean(Latitude), long.coa = mean(Longitude)) %>% #calculate COAs
  select(-c(Date_time, Date.and.Time..UTC., Latitude, Longitude, Station.Name, Date_Established)) %>% distinct() #remove uneeded columns and take out repeated columns
head(coadat)

#So our COA lat and long are in the dataframe. Lets now take out fish with less than 50 COAs
coadat1 <- coadat %>% as.data.frame() %>% group_by(Transmitter) %>%
  mutate(count = n()) %>% filter(count >= 50) %>% select(-c(n, count)) %>% distinct()
str(coadat1)

#First, let's plot this in ggmap to get our bearings LG
# Note,a API key from Google Maps is required to load the map and run the below code. Visit https://rpubs.com/ktcross/1154003 for instructions on  how to install and run ggamps in R
# For now, we will use the already saved maps but the code to generate the maps if you have an existing API code is below as well.

# FLmap_zoom_out <- get_googlemap(center = c(lon=mean(coadat1$long.coa), lat=mean(coadat1$lat.coa)),
#                        zoom = 6,
#                        maptype = c("satellite"))
load("data/FLmap_zoom_out.RData")

ggmap(FLmap_zoom_out, extent='normal') + 
  geom_point(data = coadat1,
             aes(x = long.coa, y = lat.coa,), col ="yellow") 

# FLmap_zoom_in <- get_googlemap(center = c(lon=mean(coadat1$long.coa), lat=mean(coadat1$lat.coa)),
#                                 zoom = 13,
#                                 maptype = c("satellite"))
load("data/FLmap_zoom_in.RData")

ggmap(FLmap_zoom_in, extent='normal') + 
  geom_point(data = coadat1,
             aes(x = long.coa, y = lat.coa,), col ="yellow") +
  facet_wrap(~Transmitter)

#now we have a dataframe with our transmitter, datetime bin, and coa lat and long. Let's look at the COAs on a map
#create a spatial object in sf
sfdat <- coadat1 %>% 
  st_as_sf(coords = c('long.coa', 'lat.coa')) %>% #set up the coordinates
  st_set_crs(4326) %>%  # using 4326 for lat/lon decimal
  st_transform(2958) #Transform data into projected coordinate system

#We can now graph this in ggplot to confirm
ggplot() +
  geom_sf(data = sfdat, size = 3)

#this is now projected data, so revert it back!
coor <- as.data.frame(do.call('rbind', sfdat$geometry)) %>% rename(x = V1, y = V2)

coadat1 <- cbind(coadat1, coor) %>% select(-c(lat.coa, long.coa))

#As you can see, the data is limited to our grid of receivers. This is a downside to acoustic telemetry vs positioning solvers

#So we have the the real detections. Now we need to create pseudo-absences to compare the presences to.
#For Random Forest, we do a 1 to 1 ratio of presence and pseudo-absence points. Barbet-Massin et al., 2012 Meth Ecol Evol
set.seed(19) #repeatability of randomness

#we are going to break up the data by individual, diel period, and season. This is for GLMM and GAM calcs, not for RF
#set up data for break-up
head(coadat1)

coadat1$DateTime2 <- coadat1$DateTime

coadat1 <- cSplit(coadat1, "DateTime2", sep = " ", type.convert = F)

head(coadat1)


# Rename columns so they make sense, it'll just be your last 2 column numbers, in this case the 15th and 16th column s
colnames(coadat1)[5:6]<-c("Date", "Time")
head(coadat1)

# Then I repeat this and parse the date into year, month, and day (then hour, minute, second), so I can easily subset by year
# Copy date to parse out, maintain original date
coadat1$Date2 <- coadat1$Date


coadat1<-cSplit(coadat1, "Date2", sep = "-", type.convert = FALSE)
head(coadat1)
colnames(coadat1)[7:9]<-c("Year", "Month", "Day")
head(coadat1)

coadat1$Time2 <- coadat1$Time
head(coadat1)

coadat1<-cSplit(coadat1, "Time2", sep = ":", type.convert = FALSE)
head(coadat1)
colnames(coadat1)[10:12]<-c("Hour", "Minute", "Second")
head(coadat1)

coadat1$Date <- as.Date(coadat1$Date)
coadat1$Time <- as.times(coadat1$Time)
coadat1$Year <- as.numeric(coadat1$Year)
coadat1$Month <- as.numeric(coadat1$Month)
coadat1$Day <- as.numeric(coadat1$Day)
coadat1$Hour <- as.numeric(coadat1$Hour)
coadat1$Minute <- as.numeric(coadat1$Minute)
coadat1$Second <- as.numeric(coadat1$Second)

coadat1[is.na(coadat1)] <- 0

str(coadat1)

#diel period
for (i in 1:nrow(coadat1)){
  if (coadat1$Hour[i] >= 0 & coadat1$Hour[i] < 6){
    coadat1$period[i] <- 'Night'
  }else if (coadat1$Hour[i] >= 6 & coadat1$Hour[i] < 12){
    coadat1$period[i] <- 'Dawn'
  }else if (coadat1$Hour[i] >= 12 & coadat1$Hour[i] < 18){
    coadat1$period[i] <- 'Day'
  }else if (coadat1$Hour[i] >= 18 & coadat1$Hour[i] <= 24){
    coadat1$period[i] <- 'Dusk'
  }
}

head(coadat1)


#season
for (i in 1:nrow(coadat1)){
  if (coadat1$Month[i] >= 8 && coadat1$Month[i] <= 10){
    coadat1$periody[i] <- 'ew'
  }else if (coadat1$Month[i] >= 2 && coadat1$Month[i] <= 4){
    coadat1$periody[i] <- 'ed'
  }else if (coadat1$Month[i] >= 5 && coadat1$Month[i] <= 7){
    coadat1$periody[i] <- 'd'
  }else{
    coadat1$periody[i] <- 'w'
  }
}

#create list with number of occurences with each combination
COA_list <- coadat1 %>% as.data.frame() %>%
  group_by(Transmitter, period, periody) %>%
  # Calculate the number of occurrences with this transmitter, year, diel combination.
  dplyr::summarise(count = n()) %>% 
  ungroup() %>%
  # Combine the columns as unique ID.
  mutate(TYD = paste0(Transmitter, "_", period, "_", periody)) %>% 
  filter(count >= 5) %>% #keep combinations with only 5 or above observations
  # Select only 2013.
  #filter(Year == 2013) %>% 
  # Make into list based on TYD.
  group_split(TYD) 

extent <- st_read('data/trainr2021_mask.shp')
#create list to put results into
rand_list <- list()


for (i in 1:length(COA_list)) {
  # For reproducibility.
  set.seed(19) 
  
  # Distribute x number of points across defined available resource unit for this particular transmitter, year, diel period combination.
  randLocs <- sf::st_sample(extent, size = COA_list[[i]]$count, type = 'random') %>% st_transform(2958) %>% sfc_to_df()
  
  set.seed(19)
  #  Get and randomize coordinates.
  xcoor <- as.data.frame(randLocs[,3])
  ycoor <- as.data.frame(randLocs[,4])
  # Randomize coordinates.
  x.random <- as.data.frame(xcoor[sample(1:nrow(xcoor)), ])
  y.random <- as.data.frame(ycoor[sample(1:nrow(ycoor)), ])
  coords.random <- as.data.frame(c(x.random, y.random))
  names(coords.random) <- c('x', 'y')
  # Make a data frame that matches the number of COAs for that individual.
  df <- do.call('rbind', COA_list[i]) 
  # Replicate the info to match the observed.
  df2 <- rbind(df, df[rep(1, (df$count - 1)), ]) 
  # Delete row names.
  rownames(df2) <- NULL 
  df2$x <- coords.random[, 1]
  # Put the coordinates from the random sample into the data frame.
  df2$y <- coords.random[, 2] 
  #convert df2 into dataframe
  #df2 <- df2 %>% st_to_sf()
  # Label these detections are background points (0) (opposed to observed COAs (1)).
  df2$RealDets <- 0
  
  
  
  # Place completed iteration into a list.
  rand_list[[i]] <- df2
}

RandomPts <- as.data.frame(do.call("rbind", rand_list)) #make the list into a dataframe

#combine real COAs with pseudo-absences
coadat1$RealDets <- 1
coadat1 <- coadat1 %>% mutate(TYD = paste0(Transmitter, "_", period, "_", periody))
#remove data that we do not have random points for (combinations with less than 5 COAs)
coadat1 <- coadat1 %>% filter(TYD %in% RandomPts$TYD)

#make dataframes have same columns
head(RandomPts)
head(coadat1)
coadat1 <- coadat1 %>% select(-c(DateTime, Date, Time, Year, Month, Day, Hour, Minute, Second))
head(coadat1)
RandomPts <- RandomPts %>% select(-count)
head(RandomPts)

alldat <- rbind(coadat1, RandomPts)
head(alldat)

#now we have our full dataset with presences and pseudo-absences!!! 
#Now we can extract environmental variables to model habitat selection
#Load in rasters - all rasters are interpolated maps from either surveys performed by Rodemann et al. or by FWRI as part of the Fisheries Habitat Assessment Program (FHAP) in Florida Bay
cov_2020 <- rast('data/cov2020.tif') #percent SAV cover
sdcov_2020 <- rast('data/sdcov2020.tif') #standard deviation of cover
numsp_2020 <- rast('data/num2020.tif') #number of SAV species
hw_2020 <- rast('data/hw2020.tif') #Halodule wrightii cover
tt_2020 <- rast('data/tt2020.tif') #Thalassia testudinum cover

#crop all rasters to same extent
#load in shapefile for extent
extent <- st_read('data/trainr2021_mask.shp')

cov2020 <- terra::crop(cov_2020, extent)
sdcov2020 <- terra::crop(sdcov_2020, extent)
num2020 <- terra::crop(numsp_2020, extent)
hw2020 <- terra::crop(hw_2020, extent)
tt2020 <- terra::crop(tt_2020, extent)


#We have all the rasters at the same spatial extent. Now stack them to get ready for extraction
rastdat <- c(cov2020, sdcov2020, num2020, hw2020, tt2020)
rastdat <- terra::project(rastdat, 'epsg:2958')

#extract habitat data at each point - need to put the points into a spatial format first
datcoor <- alldat %>% 
  st_as_sf(coords = c('x', 'y')) %>% #set up the coordinates
  st_set_crs(2958) # using 2958 for projected

datextract <- terra::extract(rastdat, datcoor) #extract data at each point

datrf <- cbind(datextract, alldat) %>% drop_na() #combine dataframes and remove NAs (only happens if cell is not kept within cropped raster)
head(datrf)
#datrf is the dataset that we will use for all of our models. We have temporal components we can put into GLMM and GAMs as well as individual data for random effects
#let's get into modelling this with rf!

#need to remove all columns that we are not using for now
datrf1 <- datrf %>% dplyr::select(-c(Transmitter, period, periody, TYD))

#Set seed for replications
set.seed(19)

datrf1$RealDets <- as.factor(datrf$RealDets)

# Randomly select 70% of the data frame for the training dataset
RSF_ar.train <- datrf1[sample(1:nrow(datrf), nrow(datrf) * 0.7, replace = FALSE), ]
# Remainder (i.e., 30%) becomes the test dataset.
RSF_ar.test <- datrf1[!(datrf1$ID %in% RSF_ar.train$ID), ] 

head(RSF_ar.test)

#take out ID column
RSF_ar.test <- RSF_ar.test %>% dplyr::select(-ID) %>% drop_na()
RSF_ar.train <- RSF_ar.train %>% dplyr::select(-ID) %>% drop_na()

#turn datasets into sf objects for spatial classification
RSF_ar.train1 <- RSF_ar.train %>% as_tibble() %>% 
  st_as_sf(coords = c('x', 'y')) %>% #set up the coordinates
  st_set_crs(2958) 

RSF_ar.test1 <- RSF_ar.test %>% 
  st_as_sf(coords = c('x', 'y')) %>% #set up the coordinates
  st_set_crs(2958)

head(RSF_ar.train1)

# Set tasks for training and test datasets.
task_trout.train <- as_task_classif_st(
  RSF_ar.train1, target = "RealDets", positive = '1',
)
str(task_trout.train)
task_trout.train

task_trout.test <- as_task_classif_st(
  x = RSF_ar.test1, target = "RealDets",
  positive = "1"
)

# Make learner.
learner <-lrn(
  "classif.ranger",
  predict_type = "prob",
  mtry  = to_tune(1, ncol(RSF_ar.train) - 4),
  sample.fraction = to_tune(0.2, 0.9),
  min.node.size = to_tune(1,10),
  importance = 'impurity'
)

#tune hyperparameters
instance = ti(
  task = task_trout.train,
  learner = learner,
  resampling = rsmp("cv", folds = 5),
  measures = msr("classif.ce"),
  terminator = trm("none")
)

tuner = mlr3tuning::tnr("grid_search", resolution = 5, batch_size = 5)
tuner$optimize(instance) # Takes ~ 4 minutes on my relatively fast computer
beep(1)

#store tuned hyperparameters in learner
learner$param_set$values <- instance$result_learner_param_vals

#finally! We can train our model with the train function
learner$train(task_trout.train)

#let's quickly look at the model
learner$model

#Accuracy of model - first on training data, then testing data
measures <- msrs(c('classif.acc'))

pred_train <- learner$predict(task_trout.train)
pred_train$confusion
pred_train$score(measures)


pred_test <- learner$predict(task_trout.test)
pred_test$confusion
pred_test$score(measures)


#importance with iml package - this is looking at the most influencial predictors in the model
x_trout <- RSF_ar.train %>% dplyr::select(-RealDets) 
# Create "Predictor" object to interpret findings via the iml package.
predictor_trout <- Predictor$new(learner, data = x_trout, y = RSF_ar.train$RealDets) 

imp_trout <- FeatureImp$new(predictor_trout, loss = "ce") # Calculate importance.

imp_df_trout <- imp_trout$results 
imp_trout$plot()

#so halodule cover is the most important variable. Let's investigate how it impacts the presence of trout

effect_hw <- FeatureEffect$new(predictor_trout, feature = c('hw2020'), method = 'pdp')
effect_hw$plot()


#we can see as halodule cover goes up, probability of presence goes up. What about thalassia?
effect_tt <- FeatureEffect$new(predictor_trout, feature = c('tt2020'), method = 'pdp')
effect_tt$plot()


#opposite effect. Makes sense. How about cover?
effect_cov <- FeatureEffect$new(predictor_trout, feature = c('cov2020'), method = 'pdp')
effect_cov$plot()

#polynomial. Makes sense with low cover as it is often associated with Halodule beds. High cover is more interesting. Sd cover?
effect_sdcov <- FeatureEffect$new(predictor_trout, feature = c('sdcov2020'), method = 'pdp')
effect_sdcov$plot()

#Number of species?
effect_num <- FeatureEffect$new(predictor_trout, feature = c('num2020'), method = 'pdp')
effect_sdcov$plot()


#let's now create a spatial representation of presence/absence from the model!
plot(rastdat)

#need to do it kind of manually because mlr3spatial does not support probability surfaces
newdata <- as.data.table(as.data.frame(rastdat)) %>% mutate(tt2020 = ifelse(is.na(tt2020)==T, 0, tt2020)) %>% mutate(hw2020=ifelse(is.na(hw2020)==T, 0, hw2020))

pred = learner$predict_newdata(newdata)
pred$prob

walk(task_trout.train$class_names, function(class) {
  raster = rast(
    ext(rastdat), 
    resolution = res(rastdat), 
    crs = crs(rastdat), 
    vals = pred$data$prob[, class],
    names = class)
  add(rastdat) = raster 
})

plot(rastdat)
plot(rastdat$`1`) #the prediction of presence!

# Looks messy, but that is because of the nature of the data (and the small amount we are using)
# ANY COOL FINDINGS TO PULL OUT FROM IT

# Congratulations, you've now worked through an example of going from detections to implementing RSFs through Random Forest.
# Now we will implement the same approach with a Generalized Linear Mixed-Effect Model (GLMM) using the glmmTMB package.
# GLMMs offer easier interpretability, handle random effects (i.e., individual variation), and can better account for autocorrelation compared to Random Forest.

# Load required libraries for GLMM and visualization
library(glmmTMB)   # for fitting GLMMs
library(cowplot)   # for plotting multiple panels together via plot_grid function
library(ggeffects) # for extracting the marginal effects of each covariate in the model
library(MuMIn)   # for AIC
library(performance) # for R2

# Ensure the dataset has the proper format with predictors and the response variable (RealDets)
# As before, assemble our dataset remove NAs
datglmm <- cbind(datextract, alldat) %>% drop_na() %>% mutate(RealDets = as.factor(RealDets), Transmitter = as.factor(Transmitter))

# Derive the training and testing dataset
set.seed(19)

# Add a unique ID column to each row
datglmm <- datglmm %>%
  mutate(ID = seq_len(nrow(datglmm)))

# Split the data: 70% training, 30% testing
RSF_ar.train <- datglmm %>%
  group_by(Transmitter) %>%
  sample_frac(0.7) %>% 
  ungroup() %>% 
  as.data.frame()

RSF_ar.test <- anti_join(datglmm, RSF_ar.train, by = "ID")

table(RSF_ar.train$Transmitter)
table(RSF_ar.test$Transmitter)

# Fit a GLMM with glmmTMB
glmmTMB_model_lin <- glmmTMB(RealDets ~  hw2020 + tt2020 + cov2020 + num2020 + sdcov2020 + (1 | Transmitter),
                         data = RSF_ar.train, family = binomial)

# Should we consider a polynomial?
cowplot::plot_grid(effect_hw$plot(), effect_tt$plot(), effect_cov$plot(), labels = "auto", ncol = 3)

glmmTMB_model_poly <- glmmTMB(RealDets ~  hw2020 + tt2020 + poly(cov2020,2) + num2020 + sdcov2020 +(1 | Transmitter),
                         data = RSF_ar.train, family = binomial)

AICc(glmmTMB_model_lin, glmmTMB_model_poly)

glmmTMB_model = glmmTMB_model_poly
# Summarize the model results
summary(glmmTMB_model)
r2(glmmTMB_model) 
# Marginal R2 focuses on the explanatory power of the fixed effects alone.
# Conditional R2 considers both fixed effects and random effects. Only slightly improves variance explained.

# Calculate marginal effects for all covariates
effects_hw2020 <- ggpredict(glmmTMB_model, terms = "hw2020 [all]")
effects_tt2020 <- ggpredict(glmmTMB_model, terms = "tt2020 [all]")
effects_cov2020 <- ggpredict(glmmTMB_model, terms = "cov2020 [all]")
effects_sdcov2020 <- ggpredict(glmmTMB_model, terms = "sdcov2020 [all]")
effects_num2020 <- ggpredict(glmmTMB_model, terms = "num2020 [all]")

# Plot marginal effects for each covariate
hh_marg <- ggplot(effects_hw2020, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Halodule wrightii Cover", y = "Predicted Probability") +
  theme_minimal()

tt_marg <- ggplot(effects_tt2020, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Thalassia testudinum Cover", y = "Predicted Probability") +
  theme_minimal()

cov_marg <- ggplot(effects_cov2020, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Percent SAV Cover", y = "Predicted Probability") +
  theme_minimal()

sdcov_marg <- ggplot(effects_sdcov2020, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Standard Deviation in SAV Cover", y = "Predicted Probability") +
  theme_minimal()

num_marg <- ggplot(effects_num2020, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Number of Species", y = "Predicted Probability") +
  theme_minimal()

# Combine the five plots into one using cowplot
cowplot::plot_grid(hh_marg, tt_marg, cov_marg, sdcov_marg, num_marg, labels = "auto", ncol = 5)


# Accuracy of the model on the training data
RSF_ar.train$predicted_probs_glmm <- predict(glmmTMB_model, newdata = RSF_ar.train, type = "response")
RSF_ar.train$predicted_class_glmm <- ifelse(RSF_ar.train$predicted_probs_glmm > 0.5, 1, 0)
train_conf_matrix <- table(RSF_ar.train$RealDets, RSF_ar.train$predicted_class_glmm)
train_conf_matrix

# Accuracy of the model on the test data
RSF_ar.test$predicted_probs_glmm <- predict(glmmTMB_model, newdata = RSF_ar.test, type = "response")
RSF_ar.test$predicted_class_glmm <- ifelse(RSF_ar.test$predicted_probs_glmm > 0.5, 1, 0)
test_conf_matrix <- table(RSF_ar.test$RealDets, RSF_ar.test$predicted_class_glmm)
test_conf_matrix

# Calculate accuracy metrics for both training and test datasets
(train_accuracy <- sum(diag(train_conf_matrix)) / sum(train_conf_matrix))
(test_accuracy <- sum(diag(test_conf_matrix)) / sum(test_conf_matrix))


# Predict onto the spatial grid for GLMM (similar to Random Forest)
newdata <- as.data.table(as.data.frame(rastdat)) %>% 
  mutate(Transmitter = "place-holder") # have to include the column of Transmitter, even though we will ignore it in predictions.

# Predict probabilities of presence for the grid, excluding random effects
newdata$predicted_probs_glmm <- predict(glmmTMB_model, newdata = newdata, type = "response", 
                                        re.form = NA) # Ignore random effects.

# Map the predicted probabilities
new_raster <- rast(ext(rastdat), resolution = res(rastdat), crs = crs(rastdat))
new_raster[] <- newdata$predicted_probs_glmm

# Plot the predicted probabilities
plot(new_raster, main = "GLMM Predicted Probabilities of Presence")


# Convert rasters to data frames
df_glmm <- as.data.frame(new_raster, xy = TRUE)
colnames(df_glmm)[3] <- "GLMM_Prob"

# Create the GLMM plot
(glmm_plot <- ggplot(df_glmm, aes(x = x, y = y, fill = GLMM_Prob)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Probability") + 
  coord_equal() +
  labs(title = "GLMM Predicted Probabilities of Presence") +
  theme_minimal())

# Generalized Additive Models (GAMs) are flexible extensions of generalized linear models (GLMs), 
# allowing for smooth, non-linear relationships between predictors and the response variable. 
# They use smooth terms (represented as s()) to capture these complex relationships without requiring predefined functional forms.

# Many more details to come on GAMs and acoustic telemetry data at "Using GAMs for analyzing movement data - Eric Pedersen" on Thursday! 

library(mgcv) # For fitting Generalized Additive Mixed Models (GAMMs)

# As before, assemble our dataset remove NAs
datgam <- cbind(datextract, alldat) %>% drop_na() %>% mutate(RealDets = as.factor(RealDets), Transmitter = as.factor(Transmitter))

# Fit a GAM model with spatial and temporal autocorrelation (x, y, and Date). Stay tuned for 
gam_model <- gam(RealDets ~ s(hw2020, k = 4) + s(tt2020, k = 4) + s(cov2020, k = 4) +
                   s(Transmitter, bs = "re") + # ID
                   s(x, y, bs = "gp"), # Spatial component
                 family = binomial, data = datgam, method = "REML")

# Summarize the model
summary(gam_model)

# Visualize smooth effects for each covariate
plot(gam_model, pages = 1, all.terms = TRUE)

# Predict probabilities for the full dataset and evaluate accuracy. Let's skip the test vs. training - it's similar to GLMMs. 
datgam$predicted_probs_gam <- predict(gam_model, newdata = datgam, type = "response")
datgam$predicted_class_gam <- ifelse(datgam$predicted_probs_gam > 0.5, 1, 0)

# Confusion matrix and accuracy for the GAM
conf_matrix_gam <- table(datgam$RealDets, datgam$predicted_class_gam)
conf_matrix_gam
accuracy_gam <- sum(diag(conf_matrix_gam)) / sum(conf_matrix_gam)
accuracy_gam

# Predict probabilities on spatial grid using GAMs
# Convert rasters into a data frame and predict onto spatial data
newdata_gam <- as.data.table(as.data.frame(rastdat)) %>% 
  mutate(Transmitter = "place-holder")# Use rastdat from earlier
newdata_gam$predicted_probs_gam <- predict(gam_model, newdata = newdata_gam, type = "response")

# Create a raster from predicted probabilities
pred_raster_gam <- rast(ext(rastdat), resolution = res(rastdat), crs = crs(rastdat))
pred_raster_gam[] <- newdata_gam$predicted_probs_gam

# Plot the spatial predictions
plot(pred_raster_gam, main = "GAM Predicted Probabilities of Presence")

# Convert rasters to data frames
df_gam <- as.data.frame(pred_raster_gam, xy = TRUE)
colnames(df_gam)[3] <- "GAM_Prob" 

# Create the GAM plot
(gam_plot <- ggplot(df_gam, aes(x = x, y = y, fill = GAM_Prob)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Probability") + 
  coord_equal() +
  labs(title = "GAM Predicted Probabilities of Presence") +
  theme_minimal())

plot_grid(gam_plot, glmm_plot, ncol = 2)

