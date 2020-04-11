# Loading Libraries

library("MASS")
library("car")

# Loading the dataset

carprice <- read.csv("CarPrice_Assignment.csv",stringsAsFactors = F)

View(carprice)

# Creating independent variable Car Company from Car Name variable

carprice$carcompany<-word(carprice$CarName,1)

# Let us examine the structure of the dataset

str(carprice)

# Changing the type of Categorical variables to factor. Using the as.factor() command

carprice$symboling <- as.factor(carprice$symboling)
carprice$fueltype <- as.factor(carprice$fueltype)
carprice$aspiration <- as.factor(carprice$aspiration)
carprice$doornumber <- as.factor(carprice$doornumber)
carprice$carbody <- as.factor(carprice$carbody)
carprice$drivewheel <- as.factor(carprice$drivewheel)
carprice$enginelocation <- as.factor(carprice$enginelocation)
carprice$enginetype <- as.factor(carprice$enginetype)
carprice$cylindernumber <- as.factor(carprice$cylindernumber)
carprice$fuelsystem <- as.factor(carprice$fuelsystem)
carprice$carcompany <- as.factor(carprice$carcompany )

# Removing duplicate values (if any) in the dataset. Using the unique() command

unique(carprice)

# We observe that the number of observations doesn't change thus no duplicates are found in the dataset

# Checking for missing values and treat if any. Using sum(is.na()) to check if there are any missing values

sum(is.na(carprice))

# We observe that there are no missing values in the dataset

# Treating the outliers (if any)

quantile(carprice$wheelbase,seq(0,1,0.01))
quantile(carprice$carlength,seq(0,1,0.01))
quantile(carprice$carwidth,seq(0,1,0.01))
quantile(carprice$carheight,seq(0,1,0.01))
quantile(carprice$curbweight,seq(0,1,0.01))

# Note that there is a jump between 98% and 99%. So, cap all values above 3768.40 (98%) to 3768.40

carprice$curbweight[which(carprice$curbweight>3768.40)]<-3768.40 

# Note that there is a jump between 0% and 1%. So, floor all values below 1819.72 (1%) to 1819.72

carprice$curbweight[which(carprice$curbweight<1819.72)]<-1819.72

quantile(carprice$enginesize,seq(0,1,0.01))

# Note that there is a jump between 98% and 99%. So, cap all values above 256.08 (98%) to 256.08

carprice$enginesize[which(carprice$enginesize>256.08)]<-256.08

quantile(carprice$boreratio,seq(0,1,0.01))
quantile(carprice$stroke,seq(0,1,0.01))
quantile(carprice$compressionratio,seq(0,1,0.01))
quantile(carprice$horsepower,seq(0,1,0.01))

# Note that there is a jump between 99% and 100%. So, cap all values above 207.00 (99%) to 207.00

carprice$horsepower[which(carprice$horsepower>207.00)]<-207.00

quantile(carprice$peakrpm,seq(0,1,0.01))

# Note that there is a jump between 99% and 100%. So, cap all values above 6000 (99%) to 6000

carprice$peakrpm[which(carprice$peakrpm>6000)]<-6000

quantile(carprice$citympg,seq(0,1,0.01))
quantile(carprice$highwaympg,seq(0,1,0.01))

# Checking levels for various categorical variables

summary(carprice$symboling)
summary(carprice$fueltype)
summary(carprice$aspiration)
summary(carprice$doornumber)
summary(carprice$carbody)
summary(carprice$drivewheel)
summary(carprice$enginelocation)
summary(carprice$enginetype)
summary(carprice$cylindernumber)
summary(carprice$fuelsystem)
summary(carprice$carcompany)

# Identified issues in carcompany variable levels. Now resolving them

carprice$carcompany[carprice$carcompany == "maxda"] <- "mazda"
carprice$carcompany[carprice$carcompany == "Nissan"] <- "nissan"
carprice$carcompany[carprice$carcompany == "porcshce"] <- "porsche"
carprice$carcompany[carprice$carcompany == "toyouta"] <- "toyota"
carprice$carcompany[carprice$carcompany == "vokswagen" | carprice$carcompany == "vw" ] <- "volkswagen"

levels(carprice$carcompany)[10] <- "mazda"
levels(carprice$carcompany)[14] <- "nissan"
levels(carprice$carcompany)[17] <- "porcshce"
levels(carprice$carcompany)[21] <- "toyota"
levels(carprice$carcompany)[21] <- "volkswagen"
levels(carprice$carcompany)[23] <- "volkswagen"

# Creating dummy variables to convert the categorical variables to numerical. Using model.matrix()

summary(carprice$symboling)
dummy<-data.frame(model.matrix(~symboling,data=carprice))
dummy<-dummy[,-1]
carprice<-cbind(carprice,dummy)

summary(carprice$fueltype)
dummy<-data.frame(model.matrix(~fueltype,data=carprice))
dummy<-dummy[,-1]
carprice<-cbind(carprice,dummy)

colnames(carprice)[33]<-"fueltype"

summary(carprice$aspiration)
dummy<-data.frame(model.matrix(~aspiration,data=carprice))
dummy<-dummy[,-1]
carprice<-cbind(carprice,dummy)

colnames(carprice)[34]<-"aspiration"

summary(carprice$doornumber)
dummy<-data.frame(model.matrix(~doornumber,data=carprice))
dummy<-dummy[,-1]
carprice<-cbind(carprice,dummy)

colnames(carprice)[35]<-"doornumber"

summary(carprice$carbody)
dummy<-data.frame(model.matrix(~carbody,data=carprice))
dummy<-dummy[,-1]
carprice<-cbind(carprice,dummy)

summary(carprice$drivewheel)
dummy<-data.frame(model.matrix(~drivewheel,data=carprice))
dummy<-dummy[,-1]
carprice<-cbind(carprice,dummy)

summary(carprice$enginelocation)
dummy<-data.frame(model.matrix(~enginelocation,data=carprice))
dummy<-dummy[,-1]
carprice<-cbind(carprice,dummy)

colnames(carprice)[42]<-"enginelocation"

summary(carprice$enginetype)
dummy<-data.frame(model.matrix(~enginetype,data=carprice))
dummy<-dummy[,-1]
carprice<-cbind(carprice,dummy)

summary(carprice$cylindernumber)
dummy<-data.frame(model.matrix(~cylindernumber,data=carprice))
dummy<-dummy[,-1]
carprice<-cbind(carprice,dummy)

summary(carprice$fuelsystem)
dummy<-data.frame(model.matrix(~fuelsystem,data=carprice))
dummy<-dummy[,-1]
carprice<-cbind(carprice,dummy)

summary(carprice$carcompany)
dummy<-data.frame(model.matrix(~carcompany,data=carprice))
dummy<-dummy[,-1]
carprice<-cbind(carprice,dummy)

# Scaling of continuous variables

carprice$wheelbase_scaled<-scale(carprice$wheelbase)
carprice$carlength_scaled<-scale(carprice$carlength)
carprice$carwidth_scaled<-scale(carprice$carwidth)
carprice$carheight_scaled<-scale(carprice$carheight)
carprice$curbweight_scaled<-scale(carprice$curbweight)
carprice$enginesize_scaled<-scale(carprice$enginesize)
carprice$boreratio_scaled<-scale(carprice$borerati)
carprice$stroke_scaled<-scale(carprice$stroke)
carprice$compressionratio_scaled<-scale(carprice$compressionratio)
carprice$horsepower_scaled<-scale(carprice$horsepower)
carprice$peakrpm_scaled<-scale(carprice$peakrpm)
carprice$citympg_scaled<-scale(carprice$citympg)
carprice$highwaympg_scaled<-scale(carprice$highwaympg)

carprice_copy<-carprice

# Preparing the dataset for modeling

carprice<-carprice[,-1:-25]
carprice<-carprice[,-2]

# Dividing into training and test data set

set.seed(100)

# Randomly generating row indices for train dataset

trainindices = sample(1:nrow(carprice), 0.7*nrow(carprice))

# Generating the train data set

train = carprice[trainindices,]
  
# Similarly storing the rest of the observations into an object "test".

test = carprice[-trainindices,]

# Executing the first model model_1 in the training set 

model_1<-lm(price~.,data=train)
summary(model_1) # Adjusted R-squared:  0.9652 

# Variable selection using stepwise AIC algorithm

model_2<-stepAIC(model_1, direction="both") # AIC=2138.53 ------> AIC=2107.54
summary(model_2) # Adjusted R-squared:  0.9703

# Checking VIF

vif(model_2)

# Removing compressionratio_scaled variable based on high VIF and insignificance (p>0.05)
# Making a model without compressionratio_scaled variable

model_3 <- lm(formula = price ~ symboling1 + fueltype + aspiration + carbodyhatchback + 
     carbodysedan + carbodywagon + drivewheelrwd + enginelocation + 
     enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
     cylindernumberfive + cylindernumberfour + cylindernumbersix + 
     fuelsystem2bbl + fuelsystemmpfi + carcompanybmw + carcompanybuick + 
     carcompanydodge + carcompanyhonda + carcompanyisuzu + carcompanyjaguar + 
     carcompanymazda + carcompanymercury + carcompanymitsubishi + 
     carcompanynissan + carcompanyplymouth + carcompanyrenault + 
     carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
     carcompanyvolvo + wheelbase_scaled + carwidth_scaled + curbweight_scaled + 
     enginesize_scaled + stroke_scaled + peakrpm_scaled + citympg_scaled, data = train)

summary(model_3) # Adjusted R-squared:  0.9701
vif(model_3)

# Removing enginetyperotor variable based on high VIF and insignificance (p>0.05)
# Making a model without enginetyperotor variable

model_4 <- lm(formula = price ~ symboling1 + fueltype + aspiration + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd + enginelocation + 
                enginetypel + enginetypeohc + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + fuelsystemmpfi + carcompanybmw + carcompanybuick + 
                carcompanydodge + carcompanyhonda + carcompanyisuzu + carcompanyjaguar + 
                carcompanymazda + carcompanymercury + carcompanymitsubishi + 
                carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + wheelbase_scaled + carwidth_scaled + curbweight_scaled + 
                enginesize_scaled + stroke_scaled + peakrpm_scaled + citympg_scaled, data = train)

summary(model_4) # Adjusted R-squared:  0.9699
vif(model_4)

# Removing enginetypeohc variable based on high VIF and insignificance (p>0.05)
# Making a model without enginetypeohc variable

model_5 <- lm(formula = price ~ symboling1 + fueltype + aspiration + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd + enginelocation + 
                enginetypel + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + fuelsystemmpfi + carcompanybmw + carcompanybuick + 
                carcompanydodge + carcompanyhonda + carcompanyisuzu + carcompanyjaguar + 
                carcompanymazda + carcompanymercury + carcompanymitsubishi + 
                carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + wheelbase_scaled + carwidth_scaled + curbweight_scaled + 
                enginesize_scaled + stroke_scaled + peakrpm_scaled + citympg_scaled, data = train)

summary(model_5) # Adjusted R-squared:  0.9692
vif(model_5)

# Removing fuelsystemmpfi variable based on high VIF and insignificance (p>0.05)
# Making a model without fuelsystemmpfi variable

model_6 <- lm(formula = price ~ symboling1 + fueltype + aspiration + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd + enginelocation + 
                enginetypel + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + carcompanybmw + carcompanybuick + 
                carcompanydodge + carcompanyhonda + carcompanyisuzu + carcompanyjaguar + 
                carcompanymazda + carcompanymercury + carcompanymitsubishi + 
                carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + wheelbase_scaled + carwidth_scaled + curbweight_scaled + 
                enginesize_scaled + stroke_scaled + peakrpm_scaled + citympg_scaled, data = train)

summary(model_6) # Adjusted R-squared:  0.9693
vif(model_6)

# Removing citympg_scaled variable based on high VIF and insignificance (p>0.05)
# Making a model without citympg_scaled variable

model_7 <- lm(formula = price ~ symboling1 + fueltype + aspiration + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd + enginelocation + 
                enginetypel + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + carcompanybmw + carcompanybuick + 
                carcompanydodge + carcompanyhonda + carcompanyisuzu + carcompanyjaguar + 
                carcompanymazda + carcompanymercury + carcompanymitsubishi + 
                carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + wheelbase_scaled + carwidth_scaled + curbweight_scaled + 
                enginesize_scaled + stroke_scaled + peakrpm_scaled , data = train)

summary(model_7) # Adjusted R-squared:  0.9695
vif(model_7)

# Removing wheelbase_scaled variable based on high VIF and insignificance (p>0.05)
# Making a model without wheelbase_scaled variable

model_8 <- lm(formula = price ~ symboling1 + fueltype + aspiration + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd + enginelocation + 
                enginetypel + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + carcompanybmw + carcompanybuick + 
                carcompanydodge + carcompanyhonda + carcompanyisuzu + carcompanyjaguar + 
                carcompanymazda + carcompanymercury + carcompanymitsubishi + 
                carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carwidth_scaled + curbweight_scaled + 
                enginesize_scaled + stroke_scaled + peakrpm_scaled , data = train)

summary(model_8) # Adjusted R-squared:  0.9689
vif(model_8)

# Removing carbodyhatchback variable based on high VIF and insignificance (p>0.05)
# Making a model without carbodyhatchback variable

model_9 <- lm(formula = price ~ symboling1 + fueltype + aspiration +  
                carbodysedan + carbodywagon + drivewheelrwd + enginelocation + 
                enginetypel + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + carcompanybmw + carcompanybuick + 
                carcompanydodge + carcompanyhonda + carcompanyisuzu + carcompanyjaguar + 
                carcompanymazda + carcompanymercury + carcompanymitsubishi + 
                carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carwidth_scaled + curbweight_scaled + 
                enginesize_scaled + stroke_scaled + peakrpm_scaled , data = train)

summary(model_9) # Adjusted R-squared:  0.9687
vif(model_9)

# Removing fuelsystem2bbl variable based on high VIF and insignificance (p>0.05)
# Making a model without fuelsystem2bbl variable

model_10 <- lm(formula = price ~ symboling1 + fueltype + aspiration +  
                carbodysedan + carbodywagon + drivewheelrwd + enginelocation + 
                enginetypel + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                carcompanybmw + carcompanybuick + 
                carcompanydodge + carcompanyhonda + carcompanyisuzu + carcompanyjaguar + 
                carcompanymazda + carcompanymercury + carcompanymitsubishi + 
                carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carwidth_scaled + curbweight_scaled + 
                enginesize_scaled + stroke_scaled + peakrpm_scaled , data = train)

summary(model_10) # Adjusted R-squared:  0.9689
vif(model_10)

# Removing fueltype variable based on insignificance (p>0.05)
# Making a model without fueltype variable

model_11 <- lm(formula = price ~ symboling1 + aspiration +  
                 carbodysedan + carbodywagon + drivewheelrwd + enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanyisuzu + carcompanyjaguar + 
                 carcompanymazda + carcompanymercury + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                 carcompanyvolvo + carwidth_scaled + curbweight_scaled + 
                 enginesize_scaled + stroke_scaled + peakrpm_scaled , data = train)

summary(model_11) # Adjusted R-squared:  0.9691
vif(model_11)

# Removing carcompanyvolvo variable based on high VIF and insignificance (p>0.05)
# Making a model without carcompanyvolvo variable

model_12 <- lm(formula = price ~ symboling1 + aspiration +  
                 carbodysedan + carbodywagon + drivewheelrwd + enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanyisuzu + carcompanyjaguar + 
                 carcompanymazda + carcompanymercury + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled + curbweight_scaled + 
                 enginesize_scaled + stroke_scaled + peakrpm_scaled , data = train)

summary(model_12) # Adjusted R-squared:  0.9686
vif(model_12)

# Removing carbodysedan variable based on insignificance (p>0.05)
# Making a model without carbodysedan variable

model_13 <- lm(formula = price ~ symboling1 + aspiration +  
                 carbodywagon + drivewheelrwd + enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanyisuzu + carcompanyjaguar + 
                 carcompanymazda + carcompanymercury + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled + curbweight_scaled + 
                 enginesize_scaled + stroke_scaled + peakrpm_scaled , data = train)

summary(model_13) # Adjusted R-squared:  0.9689
vif(model_13)

# Removing carcompanyisuzu variable based on insignificance (p>0.05)
# Making a model without carcompanyisuzu variable

model_14 <- lm(formula = price ~ symboling1 + aspiration +  
                 carbodywagon + drivewheelrwd + enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanyjaguar + 
                 carcompanymazda + carcompanymercury + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled + curbweight_scaled + 
                 enginesize_scaled + stroke_scaled + peakrpm_scaled , data = train)

summary(model_14) # Adjusted R-squared:  0.9688
vif(model_14)

# Removing carbodywagon variable based on insignificance (p>0.05)
# Making a model without carbodywagon variable

model_15 <- lm(formula = price ~ symboling1 + aspiration +  
                 drivewheelrwd + enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanyjaguar + 
                 carcompanymazda + carcompanymercury + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled + curbweight_scaled + 
                 enginesize_scaled + stroke_scaled + peakrpm_scaled , data = train)

summary(model_15) # Adjusted R-squared:  0.9685
vif(model_15)

# Removing carcompanysaab variable based on insignificance (p>0.05)
# Making a model without carcompanysaab variable

model_16 <- lm(formula = price ~ symboling1 + aspiration +  
                 drivewheelrwd + enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanyjaguar + 
                 carcompanymazda + carcompanymercury + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled + curbweight_scaled + 
                 enginesize_scaled + stroke_scaled + peakrpm_scaled , data = train)

summary(model_16) # Adjusted R-squared:  0.9681
vif(model_16)

# Removing carcompanymercury variable based on insignificance (p>0.05)
# Making a model without carcompanymercury variable

model_17 <- lm(formula = price ~ symboling1 + aspiration +  
                 drivewheelrwd + enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled + curbweight_scaled + 
                 enginesize_scaled + stroke_scaled + peakrpm_scaled , data = train)

summary(model_17) # Adjusted R-squared:  0.9676
vif(model_17)

# Removing symboling1 variable based on insignificance (p>0.05)
# Making a model without symboling1 variable

model_18 <- lm(formula = price ~ aspiration +  
                 drivewheelrwd + enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled + curbweight_scaled + 
                 enginesize_scaled + stroke_scaled + peakrpm_scaled , data = train)

summary(model_18) # Adjusted R-squared:  0.9671
vif(model_18)

# Predicting the car prices in the testing dataset

predict_1 <- predict(model_18,test[,-1])
test$test_price <- predict_1

# Accuracy of the predictions

# Calculating correlation
r <- cor(test$price,test$test_price)

# Calculating R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# Checking R-squared
rsquared

# We find 84.91% accuracy for model_18 with all the independent variables as significant in explaining the car price
# model_18 is the final model

# Significant variables are
# aspiration  
# drivewheelrwd
# enginelocation 
# enginetypel
# enginetypeohcf 
# cylindernumberfive 
# cylindernumberfour 
# cylindernumbersix  
# carcompanybmw
# carcompanybuick 
# carcompanydodge
# carcompanyhonda
# carcompanyjaguar
# carcompanymazda
# carcompanymitsubishi
# carcompanynissan
# carcompanyplymouth
# carcompanyrenault 
# carcompanytoyota
# carcompanyvolkswagen
# carwidth_scaled
# curbweight_scaled
# enginesize_scaled
# stroke_scaled
# peakrpm_scaled

# Removing curbweight_scaled variable based on high VIF
# Making a model without curbweight_scaled variable

model_19 <- lm(formula = price ~ aspiration +  
                 drivewheelrwd + enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled + 
                 enginesize_scaled + stroke_scaled + peakrpm_scaled , data = train)

summary(model_19) # Adjusted R-squared:  0.9649
vif(model_19)

# Removing enginesize_scaled variable based on high VIF
# Making a model without enginesize_scaled variable

model_20 <- lm(formula = price ~ aspiration +  
                 drivewheelrwd + enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled + 
                 stroke_scaled + peakrpm_scaled , data = train)

summary(model_20) # Adjusted R-squared:  0.9464
vif(model_20)

# Removing stroke_scaled variable based on insignificance (p>0.05)
# Making a model without stroke_scaled variable

model_21 <- lm(formula = price ~ aspiration +  
                 drivewheelrwd + enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled + 
                 peakrpm_scaled , data = train)

summary(model_21) # Adjusted R-squared:  0.9469
vif(model_21)

# Removing peakrpm_scaled variable based on insignificance (p>0.05)
# Making a model without peakrpm_scaled variable

model_22 <- lm(formula = price ~ aspiration +  
                 drivewheelrwd + enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled
                  , data = train)

summary(model_22) # Adjusted R-squared:  0.9472
vif(model_22)

# Removing drivewheelrwd variable based on insignificance (p>0.05)
# Making a model without drivewheelrwd variable

model_23 <- lm(formula = price ~ aspiration +  
                 enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled
               , data = train)

summary(model_23) # Adjusted R-squared:  0.9475
vif(model_23)

# Removing cylindernumberfour variable based on high VIF
# Making a model without cylindernumberfour variable

model_24 <- lm(formula = price ~ aspiration +  
                 enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled
               , data = train)

summary(model_24) # Adjusted R-squared:  0.926
vif(model_24)

# Removing aspiration variable based on insignificance (p>0.05)
# Making a model without aspiration variable

model_25 <- lm(formula = price ~   
                 enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled
               , data = train)

summary(model_25) # Adjusted R-squared:  0.9259
vif(model_25)

# Predicting the car prices in the testing dataset

predict_1 <- predict(model_25,test[,-1])
test$test_price <- predict_1

# Accuracy of the predictions

# Calculating correlation
r <- cor(test$price,test$test_price)

# Calculating R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# Checking R-squared
rsquared # 0.8070

# Difference is almost 12% on Test data hence the model is overfitting

# Removing carcompanydodge variable based on lesser significance
# Making a model without carcompanydodge variable

model_26 <- lm(formula = price ~   
                 enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                carcompanyhonda + carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled
               , data = train)

summary(model_26) # Adjusted R-squared:  0.9199
sort(vif(model_26))

# Removing carcompanyplymouth variable based on lesser significance
# Making a model without carcompanyplymouth variable

model_27 <- lm(formula = price ~   
                 enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanyhonda + carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanynissan  + carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled
               , data = train)

summary(model_27) # Adjusted R-squared:  0.9139
sort(vif(model_27))

# Removing carcompanyhonda variable based on lesser significance
# Making a model without carcompanyhonda variable

model_28 <- lm(formula = price ~   
                 enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                   carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanynissan  + carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled
               , data = train)

summary(model_28) # Adjusted R-squared:  0.9094
sort(vif(model_28))

# Removing carcompanynissan variable based on lesser significance
# Making a model without carcompanynissan variable

model_29 <- lm(formula = price ~   
                 enginelocation + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                  carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled
               , data = train)

summary(model_29) # Adjusted R-squared:  0.9072
sort(vif(model_29))

# Removing enginetypel variable based on lesser significance
# Making a model without enginetypel variable

model_30 <- lm(formula = price ~   
                 enginelocation + 
                 enginetypeohcf + 
                 cylindernumberfive + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen + 
                 carwidth_scaled
               , data = train)

summary(model_30) # Adjusted R-squared:  0.9048
sort(vif(model_30))

# Removing carcompanyvolkswagen variable based on insignificance (p>0.05)
# Making a model without carcompanyvolkswagen variable

model_31 <- lm(formula = price ~   
                 enginelocation + 
                 enginetypeohcf + 
                 cylindernumberfive + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanyrenault + 
                 carcompanytoyota  + 
                 carwidth_scaled
               , data = train)

summary(model_31) # Adjusted R-squared:  0.903
sort(vif(model_31))

# Removing enginetypeohcf variable based on insignificance (p>0.05)
# Making a model without enginetypeohcf variable

model_32 <- lm(formula = price ~   
                 enginelocation + 
                 cylindernumberfive + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanyrenault + 
                 carcompanytoyota  + 
                 carwidth_scaled
               , data = train)

summary(model_32) # Adjusted R-squared:  0.9015
sort(vif(model_32))

# Removing carcompanytoyota variable based on insignificance (p>0.05)
# Making a model without carcompanytoyota variable

model_33 <- lm(formula = price ~   
                 enginelocation + 
                 cylindernumberfive + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanyrenault + 
                 carwidth_scaled
               , data = train)

summary(model_33) # Adjusted R-squared:  0.9007
sort(vif(model_33))

# Removing carcompanymitsubishi variable based on insignificance (p>0.05)
# Making a model without carcompanymitsubishi variable

model_34 <- lm(formula = price ~   
                 enginelocation + 
                 cylindernumberfive + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + 
                 carcompanymazda  + 
                 carcompanyrenault + 
                 carwidth_scaled
               , data = train)

summary(model_34) # Adjusted R-squared:  0.899
sort(vif(model_34))

# Removing carcompanyrenault variable based on insignificance (p>0.05)
# Making a model without carcompanyrenault variable

model_35 <- lm(formula = price ~   
                 enginelocation + 
                 cylindernumberfive + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + 
                 carcompanymazda  + 
                 carwidth_scaled
               , data = train)

summary(model_35) # Adjusted R-squared:  0.8969
sort(vif(model_35))

# Removing carcompanymazda variable based on insignificance (p>0.05)
# Making a model without carcompanymazda variable

model_36 <- lm(formula = price ~   
                 enginelocation + 
                 cylindernumberfive + cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + 
                 carwidth_scaled
               , data = train)

summary(model_36) # Adjusted R-squared:  0.8951
sort(vif(model_36))

# Removing cylindernumberfive variable based on insignificance (p>0.05)
# Making a model without cylindernumberfive variable

model_37 <- lm(formula = price ~   
                 enginelocation + 
                 cylindernumbersix + 
                 carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + 
                 carwidth_scaled
               , data = train)

summary(model_37) # Adjusted R-squared:  0.8932
sort(vif(model_37))

# Predicting the car prices in the testing dataset

predict_1 <- predict(model_37,test[,-1])
test$test_price <- predict_1

# Accuracy of the predictions

# Calculating correlation
r <- cor(test$price,test$test_price)

# Calculating R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# Checking R-squared
rsquared # 0.8352

# Difference is less than 6% on Test data

# Removing cylindernumbersix variable based on insignificance (p>0.05)
# Making a model without cylindernumbersix variable

model_38 <- lm(formula = price ~   
                 enginelocation + 
                 carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + 
                 carwidth_scaled
               , data = train)

summary(model_38) # Adjusted R-squared:  0.8853
sort(vif(model_38))

# Predicting the car prices in the testing dataset

predict_1 <- predict(model_38,test[,-1])
test$test_price <- predict_1

# Accuracy of the predictions

# Calculating correlation
r <- cor(test$price,test$test_price)

# Calculating R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# Checking R-squared
rsquared # 0.8200

# Difference is more than 6% on Test data hence the model_37 is the final model selected

summary(model_37)

# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)        11486.7      254.8  45.087  < 2e-16 ***
#   enginelocation     21838.1     1794.5  12.169  < 2e-16 ***
#   cylindernumbersix   3095.3      930.9   3.325  0.00114 ** 
#   carcompanybmw       9490.7     1647.1   5.762 5.31e-08 ***
#   carcompanybuick    11529.4     1277.0   9.028 1.49e-15 ***
#   carcompanyjaguar   11622.3     2093.5   5.552 1.43e-07 ***
#   carwidth_scaled     4471.2      284.1  15.739  < 2e-16 ***

#Engine location and number of cylinders seems to be a factor in deciding prices 
#Brand name particularly BMW, Buick and Jaguar can garner bigger prices
#Car width is also significant pointing to the usage of the car