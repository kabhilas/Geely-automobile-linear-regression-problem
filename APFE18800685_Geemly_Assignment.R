
# Load required libraries.
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(MASS)
library(car)


# DATA DICTIONARY
#	1	  Car_ID				    Unique id of each observation (Interger)		
#	2	  Symboling 				Its assigned insurance risk rating, A value of +3 indicates that the auto is risky, -3 that it is probably pretty safe.(Categorical) 		
#	3	  carCompany				Name of car company (Categorical)	 	
#	4	  fueltype				  Car fuel type i.e gas or diesel (Categorical)		2 levels 
#	5	  aspiration				Aspiration used in a car (Categorical)		2 levels
#	6	  doornumber				Number of doors in a car (Categorical)		2 levels
#	7	  carbody				    body of car (Categorical)	              	5 levels
#	8	  drivewheel				type of drive wheel (Categorical)		      3 levels
#	9	  enginelocation		Location of car engine (Categorical)      2 levels	    	
#	10	wheelbase				  Weelbase of car (Numeric)		
#	11	carlength				  Length of car (Numeric)		
#	12	carwidth				  Width of car (Numeric)		
#	13	carheight				  height of car (Numeric)		
#	14	curbweight				The weight of a car without occupants or baggage. (Numeric)		
#	15	enginetype				Type of engine. (Categorical)		          7 levels
#	16	cylindernumber		cylinder placed in the car (Categorical)	7 levels	
#	17	enginesize				Size of car (Numeric)		
#	18	fuelsystem				Fuel system of car (Categorical)          8 levels		
#	19	boreratio				  Boreratio of car (Numeric)		
#	20	stroke				    Stroke or volume inside the engine (Numeric)		
#	21	compressionratio	compression ratio of car (Numeric)		
#	22	horsepower				Horsepower (Numeric)		
#	23	peakrpm				    car peak rpm (Numeric)		
#	24	citympg				    Mileage in city (Numeric)		
#	25	highwaympg				Mileage on highway (Numeric)		
#	26	price(Dependent variable)				Price of car (Numeric)		

#*********************************** Data Sourcing ***************************************************
# Read the dataset using read.csv function
geely <- geely.cars <- read.csv("CarPrice_Assignment.csv")
str(geely)
head(geely)
summary(geely)
# There are 205 obs. of  26 variables

################################################  EDA ##############################################
# There are no nulls in the data set
sum(is.null(geely))
colnames(geely)
# There are no column with value as ""
sapply(colnames(geely), function(x) length(which(geely[,x] == "")))

# Check if car_ID is unique or not
sum(duplicated(geely$car_ID))  # The car_IDs are unique

### Univariate Analysis
#check numerical data for outliers
# wheelbase
boxplot(geely$wheelbase)
quantile(geely$wheelbase)
summary(geely$wheelbase)

# carlength
boxplot(geely$carlength)
quantile(geely$carlength)
summary(geely$carlength)

# carwidth
boxplot(geely$carwidth)
quantile(geely$carwidth)
summary(geely$carwidth)

# carheight
boxplot(geely$carheight)
quantile(geely$carheight)
summary(geely$carheight)

# curbweight
boxplot(geely$curbweight)
quantile(geely$curbweight)
summary(geely$curbweight)

# enginesize
boxplot(geely$enginesize)
quantile(geely$enginesize)
summary(geely$enginesize)

# boreratio
boxplot(geely$boreratio)
quantile(geely$boreratio)
summary(geely$boreratio)

# stroke
boxplot(geely$stroke)
quantile(geely$stroke)
summary(geely$stroke)

# compressionratio
boxplot(geely$compressionratio)
quantile(geely$compressionratio)
summary(geely$compressionratio)

# horsepower
boxplot(geely$horsepower)
quantile(geely$horsepower)
summary(geely$horsepower)

# peakrpm
boxplot(geely$peakrpm)
quantile(geely$peakrpm)
summary(geely$peakrpm)

# citympg
boxplot(geely$citympg)
quantile(geely$citympg)
summary(geely$citympg)

# highwaympg
boxplot(geely$highwaympg)
quantile(geely$highwaympg)
summary(geely$highwaympg)

# As per above univariate analysis on continuous data, there did not seem to be any issue for outliers.
# Also, as the number of observations are less i.e. 205 only, there is no critical outliers in the continuous variables.

# Converting 2 levels of categorical data as numeric

# 3. carName : Name of car company (Categorical)
# Seperate the car names as carCompany and carModel
geely$CarName = as.character(geely$CarName)
CarName_split = str_split_fixed(geely$CarName, "[ ]", 2)
CarName_split = as.data.frame(CarName_split)
#View(CarName_split)
geely$CarCompany = CarName_split$V1
geely$CarModel = CarName_split$V2 
geely[,c("CarModel","CarName","car_ID")] <- list(NULL)
#View(geely)

# Correcting the car names
levels(as.factor(geely$CarCompany))
company_name <- mapvalues(geely$CarCompany, from = c("maxda", "porcshce", "vokswagen", 
                                                     "vw", "Nissan", "toyouta"), to = c("mazda", 
                                                                                      "porsche", "volkswagen", "volkswagen", "nissan", "toyota"))
geely <- cbind(geely[,-25],company_name)
geely$company_name <- as.factor(geely$company_name)
summary(geely$company_name)

# company_name : company names of cars (Categorical)          28 levels
summary(geely$company_name)
dummy_companyName <- data.frame(model.matrix( ~company_name, data = geely))
#View(dummy_companyName)
dummy_companyName <- dummy_companyName[,-1]

# 4. fueltype : Car fuel type i.e gas or diesel (Categorical)
summary(geely$fueltype)
levels(geely$fueltype) <- c(0,1) # diesel (0), gas(1)
geely$fueltype <- as.numeric(levels(geely$fueltype))[geely$fueltype]
table(geely$fueltype)

# 5. aspiration : Aspiration used in a car (Categorical)		
summary(geely$aspiration)
levels(geely$aspiration) <- c(0,1) # gas(0), turbo(1)
geely$aspiration <- as.numeric(levels(geely$aspiration))[geely$aspiration]
table(geely$aspiration)

# 6. doornumber :	Number of doors in a car (Categorical)
summary(geely$doornumber)
levels(geely$doornumber) <- c(0,1) # four(0), two(1)
geely$doornumber <- as.numeric(levels(geely$doornumber))[geely$doornumber]
table(geely$doornumber)

# 9. enginelocation :	Location of car engine (Categorical)
summary(geely$enginelocation)
levels(geely$enginelocation) <- c(0,1) # front(0), rear(1)
geely$enginelocation <- as.numeric(levels(geely$enginelocation))[geely$enginelocation]
table(geely$enginelocation) 

### Converting multi level of categorical data as numeric
# 2. symboling : converting it to factor
levels(as.factor(geely$symboling))
geely$symboling <- as.factor(geely$symboling)
summary(geely$symboling)
dummy_symboling <- data.frame(model.matrix( ~symboling, data = geely))
#View(dummy_symboling)
dummy_symboling <- dummy_symboling[,-1]

# 7. carbody : body of car (Categorical)	  5 levels
summary(geely$carbody)
dummy_carBody <- data.frame(model.matrix( ~carbody, data = geely))
#View(dummy_carBody)
dummy_carBody <- dummy_carBody[,-1]

# 8. drivewheel	: type of drive wheel (Categorical)		3 levels
summary(geely$drivewheel)
dummy_driveWheel <- data.frame(model.matrix( ~drivewheel, data = geely))
#View(dummy_driveWheel)
dummy_driveWheel <- dummy_driveWheel[,-1]

# 15.	enginetype : Type of engine. (Categorical)       7 levels
summary(geely$enginetype)
dummy_engType <- data.frame(model.matrix( ~enginetype, data = geely))
#View(dummy_engType)
dummy_engType <- dummy_engType[,-1]

# 16.	cylindernumber : cylinder placed in the car (Categorical)	  7 levels
summary(geely$cylindernumber)
dummy_cylNumber <- data.frame(model.matrix( ~cylindernumber, data = geely))
#View(dummy_cylNumber)
dummy_cylNumber <- dummy_cylNumber[,-1]

# 18. fuelsystem : Fuel system of car (Categorical)          8 levels
summary(geely$fuelsystem)
dummy_fuelSys <- data.frame(model.matrix( ~fuelsystem, data = geely))
#View(dummy_fuelSys)
dummy_fuelSys <- dummy_fuelSys[,-1]


# Replacing multi level categorical columns with dummy variables
dummy_geely <- data.frame(dummy_carBody, dummy_driveWheel, dummy_engType, dummy_cylNumber, dummy_fuelSys, dummy_companyName, dummy_symboling)
geely[,c("carbody","drivewheel","enginetype","cylindernumber","fuelsystem", "company_name", "symboling")] <- list(NULL)
geely <- cbind(geely, dummy_geely)

## Derived metrices
# Average mpg
geely$Totalmpg <- round(mean(geely$citympg + geely$highwaympg),2)

# Following guidline has been used for model building :
#1. Build a model containing all variables
#2. Run stepAIC on a model containing all variables
#3. Take the last model call from the step function after the variables were reduced, and take the remaining variables in another model - model_2
#4. Proceed as you did in backward selection
#5. Remove variables with high VIF (>2 generally) and which are insignificant (p>0.05), one by one
#6. If the model has variables which have high VIF and are significant, check and remove other insignificant variables
#7. After removing the insignificant variables, the VIFs should decline
#8. If some variables still have a high VIF, remove the variable which is relatively less significant
#9. Now variables must be significant. If the number of variables is still high, remove them in order of insignificance until you arrive at a limited number of variables, that explain the model well.

## Setting seed to achieve reproducibility
set.seed(100)

## seperating Training and test datasets
trainindices= sample(1:nrow(geely), 0.7*nrow(geely))
train = geely[trainindices,]
test = geely[-trainindices,]

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)

## using stepAIC to estimate the model
step <- stepAIC(model_1, direction = "both")
step
## using last step of AIC for finalisation of our model
model_2 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + stroke + peakrpm + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd + enginetypedohcv + 
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberthree + fuelsystem2bbl + 
                company_namebmw + company_namebuick + company_namedodge + 
                company_namehonda + company_namejaguar + company_namemazda + 
                company_namemercury + company_namemitsubishi + company_namenissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen + symboling.1 + 
                symboling0 + symboling3, data = train)

summary(model_2)
vif(model_2)

# remove enginetypedohcv
model_3 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + stroke + peakrpm + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd +  
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberthree + fuelsystem2bbl + 
                company_namebmw + company_namebuick + company_namedodge + 
                company_namehonda + company_namejaguar + company_namemazda + 
                company_namemercury + company_namemitsubishi + company_namenissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen + symboling.1 + 
                symboling0 + symboling3, data = train)

summary(model_3)
vif(model_3)

# remove fuelsystem2bbl
model_4 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + stroke + peakrpm + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd +  
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberthree +  
                company_namebmw + company_namebuick + company_namedodge + 
                company_namehonda + company_namejaguar + company_namemazda + 
                company_namemercury + company_namemitsubishi + company_namenissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen + symboling.1 + 
                symboling0 + symboling3, data = train)

summary(model_4)
vif(model_4)

# remove symboling0
model_5 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + stroke + peakrpm + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd +  
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberthree +  
                company_namebmw + company_namebuick + company_namedodge + 
                company_namehonda + company_namejaguar + company_namemazda + 
                company_namemercury + company_namemitsubishi + company_namenissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen + symboling.1 + 
                symboling3, data = train)

summary(model_5)
vif(model_5)

# remove symboling.1
model_6 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + stroke + peakrpm + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd +  
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberthree +  
                company_namebmw + company_namebuick + company_namedodge + 
                company_namehonda + company_namejaguar + company_namemazda + 
                company_namemercury + company_namemitsubishi + company_namenissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen +  
                symboling3, data = train)

summary(model_6)
vif(model_6)

#remove company_namemercury
model_7 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + stroke + peakrpm + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd +  
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberthree +  
                company_namebmw + company_namebuick + company_namedodge + 
                company_namehonda + company_namejaguar + company_namemazda + 
                company_namemitsubishi + company_namenissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen +  
                symboling3, data = train)

summary(model_7)
vif(model_7)

#remove symboling3
model_8 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + stroke + peakrpm + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd +  
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberthree +  
                company_namebmw + company_namebuick + company_namedodge + 
                company_namehonda + company_namejaguar + company_namemazda + 
                company_namemitsubishi + company_namenissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen, data = train)

summary(model_8)
vif(model_8)

#remove carbodyhardtop
model_9 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + stroke + peakrpm + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd +  
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberthree +  
                company_namebmw + company_namebuick + company_namedodge + 
                company_namehonda + company_namejaguar + company_namemazda + 
                company_namemitsubishi + company_namenissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen, data = train)

summary(model_9)
vif(model_9)

#remove carbodysedan
model_10 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + carbodyhatchback + 
                 carbodywagon + drivewheelrwd +  
                 enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberthree +  
                 company_namebmw + company_namebuick + company_namedodge + 
                 company_namehonda + company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_namenissan + 
                 company_nameplymouth + company_namerenault + company_namesaab + 
                 company_nametoyota + company_namevolkswagen, data = train)

summary(model_10)
vif(model_10)

# remove carbodyhatchback
model_11 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + carbodywagon + drivewheelrwd +  
                 enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberthree +  
                 company_namebmw + company_namebuick + company_namedodge + 
                 company_namehonda + company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_namenissan + 
                 company_nameplymouth + company_namerenault + company_namesaab + 
                 company_nametoyota + company_namevolkswagen, data = train)

summary(model_11)
vif(model_11)

# remove carbodywagon
model_12 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + drivewheelrwd +  
                 enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberthree +  
                 company_namebmw + company_namebuick + company_namedodge + 
                 company_namehonda + company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_namenissan + 
                 company_nameplymouth + company_namerenault + company_namesaab + 
                 company_nametoyota + company_namevolkswagen, data = train)

summary(model_12)
vif(model_12)

#remove curbweight
model_13 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd +  
                 enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberthree +  
                 company_namebmw + company_namebuick + company_namedodge + 
                 company_namehonda + company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_namenissan + 
                 company_nameplymouth + company_namerenault + company_namesaab + 
                 company_nametoyota + company_namevolkswagen, data = train)

summary(model_13)
vif(model_13)

# remove cylindernumberthree
model_14 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd +  
                 enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + company_namebmw + company_namebuick + company_namedodge + 
                 company_namehonda + company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_namenissan + 
                 company_nameplymouth + company_namerenault + company_namesaab + 
                 company_nametoyota + company_namevolkswagen, data = train)

summary(model_14)
vif(model_14)

# remove cylindernumberfive
model_15 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd +  
                 enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 company_namebmw + company_namebuick + company_namedodge + 
                 company_namehonda + company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_namenissan + 
                 company_nameplymouth + company_namerenault + company_namesaab + 
                 company_nametoyota + company_namevolkswagen, data = train)

summary(model_15)
vif(model_15)

# remove peakrpm
model_16 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + drivewheelrwd +  
                 enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 company_namebmw + company_namebuick + company_namedodge + 
                 company_namehonda + company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_namenissan + 
                 company_nameplymouth + company_namerenault + company_namesaab + 
                 company_nametoyota + company_namevolkswagen, data = train)

summary(model_16)
vif(model_16)

# remove company_namesaab
model_17 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + drivewheelrwd +  
                 enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 company_namebmw + company_namebuick + company_namedodge + 
                 company_namehonda + company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_namenissan + 
                 company_nameplymouth + company_namerenault +  
                 company_nametoyota + company_namevolkswagen, data = train)

summary(model_17)
vif(model_17)

# remove stroke
model_18 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + drivewheelrwd +  enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 company_namebmw + company_namebuick + company_namedodge + 
                 company_namehonda + company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_namenissan + 
                 company_nameplymouth + company_namerenault +  
                 company_nametoyota + company_namevolkswagen, data = train)

summary(model_18)
vif(model_18)

# remove drivewheelrwd
model_19 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 company_namebmw + company_namebuick + company_namedodge + 
                 company_namehonda + company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_namenissan + 
                 company_nameplymouth + company_namerenault +  
                 company_nametoyota + company_namevolkswagen, data = train)

summary(model_19)
vif(model_19)

# remove enginetyperotor
model_20 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf +
                 company_namebmw + company_namebuick + company_namedodge + 
                 company_namehonda + company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_namenissan + 
                 company_nameplymouth + company_namerenault +  
                 company_nametoyota + company_namevolkswagen, data = train)

summary(model_20)
vif(model_20)

#remove aspiration
model_21 <- lm(price ~ enginelocation + carwidth + 
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf +
                 company_namebmw + company_namebuick + company_namedodge + 
                 company_namehonda + company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_namenissan + 
                 company_nameplymouth + company_namerenault +  
                 company_nametoyota + company_namevolkswagen, data = train)

summary(model_21)
vif(model_21)

# remove company_namejaguar
model_22 <- lm(price ~ enginelocation + carwidth + 
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf +
                 company_namebmw + company_namebuick + company_namedodge + 
                 company_namehonda + company_namemazda + 
                 company_namemitsubishi + company_namenissan + 
                 company_nameplymouth + company_namerenault +  
                 company_nametoyota + company_namevolkswagen, data = train)

summary(model_22)
vif(model_22)

# remove company_namedodge
model_23 <- lm(price ~ enginelocation + carwidth + 
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf +
                 company_namebmw + company_namebuick + 
                 company_namehonda + company_namemazda + 
                 company_namemitsubishi + company_namenissan + 
                 company_nameplymouth + company_namerenault +  
                 company_nametoyota + company_namevolkswagen, data = train)

summary(model_23)
vif(model_23)

# remove company_namehonda
model_24 <- lm(price ~ enginelocation + carwidth + 
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf +
                 company_namebmw + company_namebuick +  
                 company_namemazda + company_namemitsubishi + company_namenissan + 
                 company_nameplymouth + company_namerenault +  
                 company_nametoyota + company_namevolkswagen, data = train)

summary(model_24)
vif(model_24)

# remove company_nameplymouth
model_25 <- lm(price ~ enginelocation + carwidth + 
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf +
                 company_namebmw + company_namebuick +  
                 company_namemazda + company_namemitsubishi + company_namenissan + 
                 company_namerenault +  company_nametoyota + company_namevolkswagen, data = train)

summary(model_25)
vif(model_25)

# remove company_namevolkswagen
model_26 <- lm(price ~ enginelocation + carwidth + 
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf +
                 company_namebmw + company_namebuick +  
                 company_namemazda + company_namemitsubishi + company_namenissan + 
                 company_namerenault +  company_nametoyota, data = train)

summary(model_26)
vif(model_26)

# remove company_namemazda
model_27 <- lm(price ~ enginelocation + carwidth + 
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf +
                 company_namebmw + company_namebuick +  
                 company_namemitsubishi + company_namenissan + 
                 company_namerenault +  company_nametoyota, data = train)

summary(model_27)
vif(model_27)

# remove company_namenissan
model_27 <- lm(price ~ enginelocation + carwidth + 
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf +
                 company_namebmw + company_namebuick +  
                 company_namemitsubishi + company_namerenault +  company_nametoyota, data = train)

summary(model_27)
vif(model_27)

# remove company_namemitsubishi
model_28 <- lm(price ~ enginelocation + carwidth + 
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf +
                 company_namebmw + company_namebuick +  
                 company_namerenault +  company_nametoyota, data = train)

summary(model_28)
vif(model_28)

# remove company_namerenault
model_29 <- lm(price ~ enginelocation + carwidth + 
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf +
                 company_namebmw + company_namebuick +  
                 company_nametoyota, data = train)

summary(model_29)
vif(model_29)

# remove company_nametoyota
model_30 <- lm(price ~ enginelocation + carwidth + 
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf +
                 company_namebmw + company_namebuick, data = train)

summary(model_30)
vif(model_30)

### The below variables are considered a the most significant for the model
#enginelocation
#carwidth
#enginesize
#enginetypel
#enginetypeohc
#enginetypeohcf
#company_namebmw
#company_namebuick

### Let's predict the model on test data

# Predict the car prices in the testing dataset
Predict_1 <- predict(model_30,test[,-1])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared

# Train data r squared : 0.929      Adjusted r squared : 0.9248
# Test data r squared : 0.8454
# difference : 0.07