#Answer the following questions using R and the example data provided.
install.packages("dplyr")
install.packages("ggplot2")
install.packages("olsrr")
install.packages("PerformanceAnalytics")
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)
# read in greenhouse gas data from reservoirs
ghg <- read.csv("/cloud/project/activity07/Deemer_GHG_Data.csv")
ghg <- read.csv("/cloud/project/activity07/ETdata.csv")

#----InClassPrompts----
#----Prompt1----

#1. Based on the tutorial, lecture, and any online research (cite sources), develop a list of
#drivers that you predict will influence methane production in reservoirs. Indicate how you
#would expect each driver to influence methane.

#Runoff - the total amount of water from precipitation that flows into a drainage basin 
  # The more runoff, the more vegetative area covered by water and soil for microbes  
#Reservoir Age - can influence the availability of carbon and oxygen newly flooded vegetation and soils for microbes
  # The older the reservior the less emission at that point
#AirTemp - influences the conditions for microbial and aquatic organisms as well as water chemistry
  # The higher the airtemp the better the condition for methane emissions as microbial production is increased
#dissolved inorganic phosphorous(DIP) - influences the growth of microbes and photosynthetic organisms.
  #The more DIP the higher the growth of microbes and photosynthetic organisms so higher methane release 


#2. Using the map and description in section 6.3.1, discuss limitations in the Deemer
#data set that need to be considered in data analysis and interpretation.

# Limitations that need to be considered in the data analysis and interpretation wold be:
#The synthesis often had to merge different observation approaches and some data may be missing if the original study did not collect it.
#Different groups of people are take the data, so they may have likly done it differently so it may be hard to merge
#There is only a select amount of locations so some places may be left out (lots of Africa and norther Asia)

#3. You are asked to compile preliminary results from the Deemer data for climate policy
#makers interested in understanding whether increased reservoir creation for
#hydroelectric power would be expected to affect methane release. Develop a linear
#model that improves on the model results from the tutorial. Ensure your model meets all
#regression assumptions.

#Pretty much the same as the tutorial 
ghg$log.ch4 <- log(ghg$ch4 + 1)
ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP + 1)
ghg$log.precip <- log(ghg$precipitation)
ghg$log.chl <- log(ghg$chlorophyll.a + 1)

ghg$BorealV <- ifelse(ghg$Region == "Boreal", 1, 0)
ghg$TropicalV <- ifelse(ghg$Region == "Tropical", 1, 0)

# Improved model
mod.improved <- lm(log.ch4 ~ airTemp + log.age + mean.depth +
                     log.DIP + log.precip + BorealV + TropicalV + log.chl,
                   data = ghg)
summary(mod.improved)

res.imp <- rstandard(mod.improved)
fit.imp <- fitted.values(mod.improved)
qqnorm(res.imp, pch = 19)
qqline(res.imp)
shapiro.test(res.imp)
plot(fit.imp, res.imp, pch = 19)
abline(h = 0)

imp.step <- ols_step_forward_aic(mod.improved)
imp.step
plot(imp.step)

#To improve on the tutorial model I added two new variables 
#log-transformed chlorophyll a (log.chl) as an indicator of algal productivity, 
#and a binary variable for tropical regions (TropicalV), 
#since tropical reservoirs are known to produce more methane. 
#All the same transformations and binary variables from the tutorial were kept. 
#The full model had an adjusted R² of 0.37 and overall F-statistic p-value of 0.024. 
#Checking the assumptions, the residuals were approximately normally distributed 
#(Shapiro-Wilk p=0.31) and the residual plot showed no major patterns, 
#so the regression assumptions were met. However, only 28 observations were available 
#after removing missing data, mostly due to the many missing chlorophyll measurements in the dataset. 
#The stepwise selection identified log.chl and mean.depth as the only two variables that meaningfully reduced AIC, 
#dropping it from 114.5 down to 98.6 with an adjusted R² of 0.47.

#4. Compile a narrative interpretation and table of the results from the question three.
#Write your narrative so that the policy makers can best understand what drives high
#methane release from reservoirs and how reservoirs affect methane fluxes.

#Based on the model results, the two strongest predictors of methane release from reservoirs are chlorophyll 
#a concentration and mean reservoir depth. Higher chlorophyll a — meaning more algae and photosynthetic 
#organisms in the water — was strongly associated with higher methane emissions (p=0.0142). 
#This makes sense because algal growth adds organic matter to the water, 
#giving methane-producing bacteria more food. Shallower reservoirs also tended to produce more methane, 
#likely because shallow water warms more easily and has less oxygen mixing, 
#creating better conditions for the bacteria that produce methane. 
#The other variables like air temperature, region, and precipitation that were 
#important in the tutorial model did not show up as significant here, 
#likely because the much smaller sample size (only 28 reservoirs had complete data) 
#reduced the statistical power to detect those relationships. 
#Overall, for policy makers considering new hydroelectric reservoirs, 
#these results suggest that reservoirs with high algal productivity and shallow depths 
#are likely to produce the most methane, and this should be factored into environmental 
#impact assessments for proposed dam projects.

#----Prompt2----
#Run the tutorial code for the Almond orchard evapotranspiration time series data
ETdat <- read.csv("/cloud/project/activity07/ETdata.csv")
unique(ETdat$crop)
library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)
# average fields for each month for almonds
almond <- ETdat %>% # ET data
  filter(crop == "Almonds") %>% # only use almond fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")
# almond ET time series
almond_ts <- ts(almond$ET.in, # data
                start = c(2016,1), #start year 2016, month 1
                #first number is unit of time and second is observations within a unit
                frequency= 12) # frequency of observations in a unit
# decompose almond ET time series
almond_dec <- decompose(almond_ts)
# plot decomposition
plot(almond_dec)
almondTrend <- almond_dec$trend
almondSeason <- almond_dec$seasonal
acf(na.omit(almond_ts), # remove missing data
    lag.max = 24) # look at 2 years (24 months)
pacf.plot <- pacf(na.omit(almond_ts))
almond_y <- na.omit(almond_ts)
model1 <- arima(almond_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1
model4 <- arima(almond_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4
# calculate fit
AR_fit1 <- almond_y - residuals(model1) 
AR_fit4 <- almond_y - residuals(model4)
#plot data
plot(almond_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")
newAlmond <- forecast(model4)
newAlmond
#make dataframe for plotting
newAlmondF <- data.frame(newAlmond)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))
# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+  # Plotting original data
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")


#----Homework7----
#----Question1----
CO2transformed <- 1/(ghg$co2 + 1000)

ghg$BorealV <- ifelse(ghg$Region == "Boreal", 1, 0)
ghg$TropicalV <- ifelse(ghg$Region == "Tropical", 1, 0)

CO2transformedmodel_test <- lm(CO2transformed ~ airTemp + log.age + mean.depth +
                     log.DIP + log.precip + BorealV + TropicalV + log.chl + surface.area + volume ,
                   data = ghg)
summary(CO2transformedmodel_test)

# Keeping the variables that have the highest absolute t-values and the ones that have a fair amount of data points/observations
CO2transformedmodel_final <- lm(CO2transformed ~ log.age +
                                 log.DIP + log.precip + TropicalV + surface.area,
                               data = ghg)
summary(CO2transformedmodel_final)

#Question 2 



