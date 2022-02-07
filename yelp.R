rm(list=ls())
library(mice)

restaurants_IL2 <-read.csv("~/Desktop/Marketing Masters/Customer model/assignment1/restaurants_IL.csv")

# what to do with None?
# Replace with NA's
restaurants_IL2[restaurants_IL2=="None"]=NA


# make the factors as real factors:
restaurants_IL2$Open7days=as.factor(restaurants_IL2$Open7days)
restaurants_IL2$OpenSundays=as.factor(restaurants_IL2$OpenSundays)
restaurants_IL2$BusinessAcceptsCreditCards=as.factor(as.character(restaurants_IL2$BusinessAcceptsCreditCards))
restaurants_IL2$RestaurantsAttire=as.factor(as.character(restaurants_IL2$RestaurantsAttire))
restaurants_IL2$GoodForKids=as.factor(as.character(restaurants_IL2$GoodForKids))
restaurants_IL2$RestaurantsReservations=as.factor(as.character(restaurants_IL2$RestaurantsReservations))
restaurants_IL2$Caters=as.factor(as.character(restaurants_IL2$Caters))
restaurants_IL2$NoiseLevel=as.factor(as.character(restaurants_IL2$NoiseLevel))
restaurants_IL2$RestaurantsTakeOut=as.factor(as.character(restaurants_IL2$RestaurantsTakeOut))
restaurants_IL2$RestaurantsPriceRange2=as.factor(as.character(restaurants_IL2$RestaurantsPriceRange2))
restaurants_IL2$OutdoorSeating=as.factor(as.character(restaurants_IL2$OutdoorSeating))
restaurants_IL2$WiFi=as.factor(as.character(restaurants_IL2$WiFi))
restaurants_IL2$RestaurantsDelivery=as.factor(as.character(restaurants_IL2$RestaurantsDelivery))
restaurants_IL2$dessert=as.factor(as.character(restaurants_IL2$dessert))
restaurants_IL2$latenight=as.factor(as.character(restaurants_IL2$latenight))
restaurants_IL2$lunch=as.factor(as.character(restaurants_IL2$lunch))
restaurants_IL2$dinner=as.factor(as.character(restaurants_IL2$dinner))
restaurants_IL2$brunch=as.factor(as.character(restaurants_IL2$brunch))
restaurants_IL2$breakfast=as.factor(as.character(restaurants_IL2$breakfast))

# what to do with NA in pos and neg reviews?
# Replacee with 0
restaurants_IL2$posreview[is.na(restaurants_IL2$posreview)]=0
restaurants_IL2$posreview2016[is.na(restaurants_IL2$posreview2016)]=0
restaurants_IL2$posreview2017[is.na(restaurants_IL2$posreview2017)]=0
restaurants_IL2$posreview2018[is.na(restaurants_IL2$posreview2018)]=0

restaurants_IL2$negreview[is.na(restaurants_IL2$negreview)]=0
restaurants_IL2$negreview2016[is.na(restaurants_IL2$negreview2016)]=0
restaurants_IL2$negreview2017[is.na(restaurants_IL2$negreview2017)]=0
restaurants_IL2$negreview2018[is.na(restaurants_IL2$negreview2018)]=0

# what to do with NA's in checked-ins?
# it seems like the restaurants where closed at >2016. Replace the NA's with 0
restaurants_IL2$checkedin100[is.na(restaurants_IL2$checkedin100)]=0
restaurants_IL2$checkedin2016[is.na(restaurants_IL2$checkedin2016)]=0
restaurants_IL2$checkedin2017[is.na(restaurants_IL2$checkedin2017)]=0
restaurants_IL2$checkedin2018[is.na(restaurants_IL2$checkedin2018)]=0


# dealing with missings
a=md.pattern(restaurants_IL2) # what is the missing pattern? (look at the last row of a)
print(a[nrow(a),])

#sum of posreview and negreview
sumposreview <- rowSums(restaurants_IL2[,c(36,38,40)]) 
restaurants_IL23 <-cbind(restaurants_IL2, sumposreview) 
sumnegreview <- rowSums(restaurants_IL2[,c(37,39,41)]) 
restaurants_IL22 <- cbind(restaurants_IL23,sumnegreview)
View(restaurants_IL22)

# do we need all variables? 
# no, some can be excluded from the data-set.

restaurants_IL2a=subset(restaurants_IL22,select=c("city","state","postal_code","latitude","longitude",
                                                  "stars","review_count","categories","OpenSundays",
                                                  "Open7days","checkedin100","BusinessAcceptsCreditCards",
                                                  "RestaurantsTakeOut","RestaurantsPriceRange2",
                                                  "RestaurantsReservations","RestaurantsDelivery",
                                                  "GoodForKids","OutdoorSeating","RestaurantsAttire",
                                                  "garage","street","validated","lot","valet","WiFi",
                                                  "NoiseLevel","Caters","dessert","latenight","lunch",
                                                  "dinner","brunch","breakfast","sumnegreview","sumposreview"))
# now double check the NA's
a=md.pattern(restaurants_IL2a) # what is the missing pattern? (look at the last row of a)
print(a[nrow(a),])


predictorMatrix <- matrix(0,nrow = ncol(restaurants_IL2a), ncol = ncol(restaurants_IL2a)) # Make a matrix of zeros
colnames(predictorMatrix)=colnames(restaurants_IL2a)
rownames(predictorMatrix)=colnames(restaurants_IL2a)

containingNA=colnames(a)[a[nrow(a),]>0]
containingNA=containingNA[-length(containingNA)] # columns containing NA's
usedforprediction=c("city","state","postal_code","latitude","longitude",
                    "categories","OpenSundays", "Open7days",containingNA)

predictorMatrix[usedforprediction,containingNA] <- 1 
diag(predictorMatrix) <- 0 #diagonal must be zero

#impute data
restaurants_IL2a_data_imputed <- mice(restaurants_IL2a, predictorMatrix = predictorMatrix, m=1, maxit = 50, seed = 500)
summary(restaurants_IL2a_data_imputed)


#get one of the complete data sets
restaurants_IL2a_data_imputed_complete <- complete(restaurants_IL2a_data_imputed,1)

#the complete data sets can be used to estimate your model of choice
#and the results of all m models can be combined as in the earlier example
write.csv(restaurants_IL2a_data_imputed_complete, file="restaurants_IL2a_imputed.csv")
colSums(is.na(restaurants_IL2a_data_imputed_complete))


restaurants_imputed <-read.csv("~/Downloads/restaurants_IL2a_imputed.csv")


# outliers
e <- c(restaurants_imputed[,6:35])
e <- as.data.frame(e)
lower_bound <- quantile(restaurants_imputed$review_count, 0.0125)
upper_bound <- quantile(restaurants_imputed$review_count, 0.9875)


# count outliers 
# review count
lower_bound <- quantile(restaurants_imputed$review_count, 0.0125)
lower_bound
upper_bound <- quantile(restaurants_imputed$review_count, 0.9875)
upper_bound
f <- sum(restaurants_imputed$review_count < lower_bound)
g <- sum(restaurants_imputed$review_count >= upper_bound)
outlier_num <- f+g
outlier_num
outlier_perc <-0
as.data.frame(outlier_perc)
outlier_perc$review_count <- as.data.frame(outlier_perc$review_count <- outlier_num/4000)
outlier_perc$review_count <- outlier_num/4000


# Stars
lower_bound <- quantile(restaurants_imputed$stars, 0.0125)
lower_bound
upper_bound <- quantile(restaurants_imputed$stars, 0.9875)
upper_bound
f <- sum(restaurants_imputed$stars < lower_bound)
g <- sum(restaurants_imputed$stars >= upper_bound)
outlier_num <- f+g
outlier_num
outlier_perc$stars <- outlier_num/4000
outlier_perc
# Sum neg review
lower_bound <- quantile(restaurants_imputed$sumnegreview, 0.0125)
lower_bound
upper_bound <- quantile(restaurants_imputed$sumnegreview, 0.9875)
upper_bound
f <- sum(restaurants_imputed$sumnegreview < lower_bound)
g <- sum(restaurants_imputed$sumnegreview >= upper_bound)
outlier_num <- f+g
outlier_num
outlier_perc$sumnegreview <- outlier_num/4000
outlier_perc
# Sum pos review
lower_bound <- quantile(restaurants_imputed$sumposreview, 0.0125)
lower_bound
upper_bound <- quantile(restaurants_imputed$sumposreview, 0.9875)
upper_bound
f <- sum(restaurants_imputed$sumposreview < lower_bound)
g <- sum(restaurants_imputed$sumposreview >= upper_bound)
outlier_num <- f+g
outlier_num
outlier_perc$sumposreview <- outlier_num/4000
outlier_perc
# Outliers as table
outlier_perc <- as.data.frame(outlier_perc, row.names = NULL)
outlier_percentage <- t(outlier_perc)
outlier_percentage <- as.data.frame(outlier_percentage)
names <- rownames(outlier_percentage)
rownames(outlier_percentage) <- NULL
data <- cbind(names,outlier_percentage)
outlier_percentage <- data
outlier_percentage <- outlier_percentage[2:5,]
library(tidyverse)
outlier_percentage <- outlier_percentage %>% 
  rename(
    Variable = names,
    Outlier.count = V1
  )
outlier_percentage$Outlier.count <-as.numeric(outlier_percentage$Outlier.count)
outlier_percentage$Outlier.count <-  outlier_percentage$Outlier.count *100
outlier_percentage$Outlier.count <- round(outlier_percentage$Outlier.count, digits=1)

outlier_percentage
ggplot(outlier_percentage) +
  geom_bar(aes(x=Variable, y= Outlier.count), 
           stat = "Identity", fill="indianred3", 
           alpha=2,width = 0.6) + xlab("Variables") + ylab("Outlier count") + ggtitle('% of outliers')



options(scipen = 999)
library(ggplot2)
library(ggalt)

lower_bound <- quantile(restaurants_imputed$review_count, 0.0125)
lower_bound #3
upper_bound <- quantile(restaurants_imputed$review_count, 0.9875)
upper_bound #455
lower_bound <- quantile(restaurants_imputed$stars, 0.0125)
lower_bound #1.5
upper_bound <- quantile(restaurants_imputed$stars, 0.9875)
upper_bound #5

#outliers_select_high <- restaurants_imputed[restaurants_imputed$review_count >= 1000 | 
#    restaurants_imputed$stars >= 5, 6:7]
#outliers_select_low <- restaurants_imputed[restaurants_imputed$review_count <= 3 | 
#   restaurants_imputed$stars <= 1.5, 6:7]

outliers_select_high <- restaurants_imputed[restaurants_imputed$review_count >= 750, 6:7]
outliers_select_low <- restaurants_imputed[restaurants_imputed$review_count <= 50, 6:7]

max(restaurants_imputed$review_count)

# Plot
ggplot(restaurants_imputed, aes(x=stars, y=review_count)) + 
  geom_point(aes(col=categories, size=RestaurantsPriceRange2)) +   # draw points
  geom_smooth(method="loess", se=F) + 
  xlim(c(1,5)) + 
  ylim(c(0,1572)) +   # draw smoothing line
  geom_encircle(aes(x=stars, y=review_count), 
                data=outliers_select_high, 
                color="red", 
                size=1, 
                expand=0.02) + 
  geom_encircle(aes(x=stars, y=review_count), 
                data=outliers_select_low, 
                color="red", 
                size=1, 
                expand=0.02) +  # encircle
  labs(subtitle="Review Vs Stars", 
       y="Reviews", 
       x="Stars", 
       title="Scatterplot & circle of outliers", 
       caption="Source: outliers")


#####
summary(restaurants_imputed)
summarytable <- 0
summarytable$meal_type <- c("dessert","breakfast", "brunch", "lunch", "dinner")
summarytable <- as.data.frame(summarytable)
summarytable$serves[summarytable2$meal_type == "dessert"] <- 273/40
summarytable$serves[summarytable2$meal_type == "breakfast"] <- 475/40
summarytable$serves[summarytable2$meal_type == "brunch"] <- 367/40
summarytable$serves[summarytable2$meal_type == "lunch"] <- 2384/40
summarytable$serves[summarytable2$meal_type == "dinner"] <- 2002/40
summarytable$serves <- round(summarytable$serves, digits=1)
summarytable <- summarytable[,2:3]

ggplot(data=summarytable, aes(x=meal_type, y=serves, fill=meal_type)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("palevioletred", "palegreen2", "mediumpurple2", "rosybrown2", "slategray2")) +
  labs(y="Serving %", 
       x="Meal type", 
       fill="Meal type", 
       title="Barplot of restaurants that serve these meals", 
       caption="Source: summary table")


summary(restaurants_imputed)
summarytable2 <- 0
summarytable2$Amenities <- c("BusinessAcceptsCreditCards",
                             "OutdoorSeating", "RestaurantsReservations", 
                             "WiFi", "garage", "street", "validated", 
                             "lot", "valet", "Caters")
summarytable2 <- as.data.frame(summarytable2)
summarytable2$Provided[summarytable2$Amenities == "BusinessAcceptsCreditCards"] <- 3913/40
summarytable2$Provided[summarytable2$Amenities == "OutdoorSeating"] <- 2016/40
summarytable2$Provided[summarytable2$Amenities == "RestaurantsReservations"] <- 1086 /40
summarytable2$Provided[summarytable2$Amenities == "WiFi"] <- 2253/40
summarytable2$Provided[summarytable2$Amenities == "garage"] <- 257/40
summarytable2$Provided[summarytable2$Amenities == "street"] <- 915/40
summarytable2$Provided[summarytable2$Amenities == "validated"] <- 51/40
summarytable2$Provided[summarytable2$Amenities == "lot"] <- 1970/40
summarytable2$Provided[summarytable2$Amenities == "valet"] <- 73/40
summarytable2$Provided[summarytable2$Amenities == "Caters"] <- 2293/40
summarytable2$Provided <- round(summarytable2$Provided, digits=1)
summarytable2 <- summarytable[,2:3]
summarytable2$Amenities[summarytable2$Amenities == "RestaurantsReservations"] <- "Reservations"
summarytable2$Amenities[summarytable2$Amenities == "BusinessAcceptsCreditCards"] <- "Credit cards"
summarytable2$Amenities[summarytable2$Amenities == "Wifi"] <- "Free wifi"

ggplot(data=summarytable2, aes(x=Amenities, y=Provided, fill=Amenities)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("palevioletred", 
                                                         "palegreen2", 
                                                         "mediumpurple2", 
                                                         "rosybrown2", 
                                                         "slategray2", 
                                                         "paleturquoise3", 
                                                         "orchid2", 
                                                         "skyblue3", 
                                                         "cadetblue3", 
                                                         "lightcoral")) +
  labs(y="Providing amenity%", 
       x="Amenities", 
       fill="Amenities", 
       title="Barplot of restaurants that provide these amenities", 
       caption="Source: summary table")

head(restaurants_imputed)
ggplot(restaurants_imputed) + geom_boxplot(aes(review_count, color = categories))
ggplot(restaurants_imputed) + geom_bar(aes(categories, color = categories))
ggplot(restaurants_imputed) + geom_boxplot(aes(stars, color = categories))


ggplot(restaurants_imputed) + geom_bar(aes(RestaurantsPriceRange2, color = categories))
ggplot(restaurants_imputed) + geom_bar(aes(NoiseLevel, color = categories))


ggplot(restaurants_imputed) + geom_bar(aes(NoiseLevel, color = state))
ggplot(restaurants_imputed) + geom_bar(aes(RestaurantsPriceRange2, color = state))




#Testing for multicollinearity to verify whether some of the cvariables overlap
multitest <- lm(checkedin100 ~ categories + GoodForKids + RestaurantsReservations + OutdoorSeating + review_count + Open7days +
                  stars + NoiseLevel + RestaurantsAttire + OutdoorSeating + WiFi + RestaurantsPriceRange2 + sumposreview + sumnegreview, data=restaurants_imputed) 
library(car)
#create vector of VIF values and a vector of tolerance values
vif_values <- vif(multitest)
tolerance <- 1/vif_values
# the closer to 1, the lower the level of multicollinearity. Result - no multicol
vif_values
#The closer to 0, the higher the level of multicollinearity. Result - no multicol
tolerance

#combining parking option variables
restaurants_imputed$street=as.numeric(as.factor(restaurants_imputed$street))
restaurants_imputed$validated=as.numeric(as.factor(restaurants_imputed$validated))
restaurants_imputed$lot=as.numeric(as.factor(restaurants_imputed$lot))
restaurants_imputed$garage=as.numeric(as.factor(restaurants_imputed$garage))
restaurants_imputed$valet=as.numeric(as.factor(restaurants_imputed$valet))

restaurants_imputed$street[restaurants_imputed$street == 1] <- 0
restaurants_imputed$street[restaurants_imputed$street == 2] <- 1

restaurants_imputed$validated[restaurants_imputed$validated == 1] <- 0
restaurants_imputed$validated[restaurants_imputed$validated == 2] <- 2

restaurants_imputed$lot[restaurants_imputed$lot == 1] <- 0
restaurants_imputed$lot[restaurants_imputed$lot == 2] <- 3

restaurants_imputed$garage[restaurants_imputed$garage == 1] <- 0
restaurants_imputed$garage[restaurants_imputed$garage == 2] <- 4

restaurants_imputed$valet[restaurants_imputed$valet == 1] <- 0
restaurants_imputed$valet[restaurants_imputed$valet == 2] <- 5

str(restaurants_imputed)
View(restaurants_imputed)

library(dplyr)

#modeling binominal model (frequency of restaurant visits)
library(mlogit) 
library(gmnl)
#all variables
logitmodel <- glm(checkedin100 ~ categories + GoodForKids + RestaurantsReservations + OutdoorSeating + review_count + Open7days +
                    stars+ NoiseLevel+ RestaurantsAttire+ OutdoorSeating + WiFi+ RestaurantsPriceRange2 + sumposreview+ sumnegreview + parkingCom, family=binomial(link="logit"), data=restaurants_imputed)
summary(logitmodel)
#AIC: 2444.2
#comparing different models
logitmodel2 <- glm(checkedin100 ~ categories + GoodForKids + RestaurantsReservations + OutdoorSeating + Open7days + NoiseLevel+ RestaurantsAttire + WiFi+ RestaurantsPriceRange2 + parkingCom, family=binomial(link="logit"), data=restaurants_imputed) 
summary(logitmodel2) 
#AIC 4531.3

TheBestModel <- glm(checkedin100 ~  categories + RestaurantsReservations + OutdoorSeating +  review_count +  Open7days + parkingCom + stars + WiFi + RestaurantsPriceRange2, family=binomial(link="logit"), data=restaurants_imputed) 
summary(TheBestModel)
#AIC 2507.8

logitmodel3 <- glm(checkedin100 ~ categories + review_count + stars+ RestaurantsPriceRange2 + sumposreview+ sumnegreview, family=binomial(link="logit"), data=restaurants_imputed) 
summary(logitmodel3)
#AIC 2649.4

logitmodel4 <- glm(checkedin100 ~ categories + review_count + stars + RestaurantsPriceRange2 + sumposreview+ sumnegreview + RestaurantsAttire + parkingCom + Open7days, family=binomial(link="logit"), data=restaurants_imputed) 
summary(logitmodel4)
#AIC 2512.4

logitmodel5 <- glm(checkedin100 ~ categories + RestaurantsReservations + NoiseLevel + RestaurantsAttire + RestaurantsPriceRange2 + parkingCom + sumnegreview + sumposreview, family=binomial(link="logit"), data=restaurants_imputed)
summary(logitmodel5)
#AIC 4577.3

#######Pseudo R2######
library(rms)
mod2b <- lrm(checkedin100 ~ categories + GoodForKids + RestaurantsReservations + OutdoorSeating + Open7days + NoiseLevel+ RestaurantsAttire + WiFi+ RestaurantsPriceRange2 + parkingCom, data=restaurants_imputed)
print(mod2b)
#R2:0.292

TheBestModelb <- lrm(checkedin100 ~ categories + RestaurantsReservations + OutdoorSeating + review_count + Open7days + parkingCom + stars+  WiFi + RestaurantsPriceRange2, data=restaurants_imputed) 
print(TheBestModelb)
#R2:0.707

mod3b <- lrm(checkedin100 ~ categories + review_count + stars+ RestaurantsPriceRange2 + sumposreview+ sumnegreview,data=restaurants_imputed) 
print(mod3b)
#R2:0.683

mod4b <- lrm(checkedin100 ~ categories + review_count + stars + RestaurantsPriceRange2 + sumposreview+ sumnegreview + RestaurantsAttire + parkingCom + Open7days, data=restaurants_imputed)
print(mod4b)
#R2:0.707

mod5b <- lrm(checkedin100 ~ categories + RestaurantsReservations + NoiseLevel + RestaurantsAttire + RestaurantsPriceRange2 + parkingCom + sumnegreview + sumposreview, data=restaurants_imputed)
print(mod5b)
#R2: 0.278

#The Best Model is the statistical most validate model

#predictive validity

#hitrate 
restaurants_imputed$predvals <- predict(TheBestModel, type= "response") 
restaurants_imputed$predchoice <- ifelse(restaurants_imputed$predvals > 0.5, 1, 0) 
table(restaurants_imputed$checkedin100, restaurants_imputed$predchoice) 

#hitrate 0.8643
(2062+1395)/(2062+1395+342+201)
#good model

##predictions
restaurants_imputed$predvalues4 <- predict(TheBestModel, type = 'response')

makeLiftPlot <- function(Prediction, Evaluate, ModelName){
  iPredictionsSorted <- sort(Prediction,index.return=T,decreasing=T)[2]$ix #extract the index order according to predicted 1's
  CustomersSorted <- Evaluate[iPredictionsSorted] #sort the true behavior of customers according to predictions
  SumChurnReal<- sum(Evaluate == 1) #total number of real 1's in the evaluation set
  CustomerCumulative=seq(length(Evaluate))/length(Evaluate) #cumulative fraction of customers
  ChurnCumulative=apply(matrix(CustomersSorted==1),2,cumsum)/SumChurnReal #cumulative fraction of 1's
  ProbTD = sum(CustomersSorted[1:floor(length(Evaluate)*.1)]==1)/floor(length(Evaluate)*.1) #probability of 1 in 1st decile
  ProbOverall = SumChurnReal / length(Evaluate) #overall probability of 1's
  TDL = ProbTD / ProbOverall
  GINI = sum((ChurnCumulative-CustomerCumulative)/(t(matrix(1,1,length(Evaluate))-CustomerCumulative)),na.rm=T)/length(Evaluate)
  plot(CustomerCumulative,ChurnCumulative,type="l",main=paste("Lift curve of", ModelName),xlab="Cumulative fraction of customers (sorted by predicted probability of 1's)",ylab="Cumulative fraction of real 1's")
  grid()
  lines(c(0,1),c(0,1),col="blue",type="l",pch=22, lty=2)
  legend(.66,.2,c("According to model","Random selection"),cex=0.8,  col=c("black","blue"), lty=1:2)
  text(0.15,1,paste("TDL = ",round(TDL,2), "; GINI = ", round(GINI,2) ))
  return(data.frame(TDL,GINI))
}

#Liftplot #GINI 0.68/TDL 2.3
makeLiftPlot(restaurants_imputed$predvals,restaurants_imputed$checkedin100, "Logit")
#good fitting model

#lr test
TheBestModel <- glm(checkedin100 ~  categories + RestaurantsReservations + OutdoorSeating +  review_count +  Open7days + parkingCom + stars + WiFi + RestaurantsPriceRange2, family=binomial(link="logit"), data=restaurants_imputed) 
summary(TheBestModel)

nullmodel <- glm(checkedin100 ~  1,  family=binomial(link="logit"), data=restaurants_imputed) 
summary(nullmodel)

anova(nullmodel, TheBestModel, test = "Chisq")
#significant, our model is better than the null model

#marginal effects of best fitting model
library(mfx)
melogitmodel <- logitmfx(formula = checkedin100 ~ categories + RestaurantsReservations + OutdoorSeating + review_count + Open7days + parkingCom + stars+  WiFi + RestaurantsPriceRange2, data=restaurants_imputed)
print(melogitmodel)

#multinominal logit model (customer ratings)
restaurantstars <- restaurants_imputed[,c("stars", "review_count", "categories", "RestaurantsPriceRange2","RestaurantsAttire", "postal_code", "Open7days", "checkedin100", "OutdoorSeating", "parkingCom", "WiFi", "BusinessAcceptsCreditCards", "RestaurantsReservations", "NoiseLevel", "latitude", "longitude", "RestaurantsDelivery", "RestaurantsTakeOut", "GoodForKids", "sumposreview", "sumnegreview")]
View(restaurantstars)
df1mlogit <- mlogit.data(data=restaurantstars, choice="stars", shape ="wide")

restaurants_imputed$stars <- as.factor(as.character(restaurants_imputed$stars))
restaurants_imputed$checkedin100 <- as.factor(as.character(restaurants_imputed$checkedin100))
restaurants_imputed$categories <- as.factor(as.character(restaurants_imputed$categories))
restaurants_imputed$RestaurantsPriceRange2 <- as.factor(as.character(restaurants_imputed$RestaurantsPriceRange2))
restaurants_imputed$RestaurantsAttire <- as.factor(as.character(restaurants_imputed$RestaurantsAttire))
restaurants_imputed$postal_code <- as.factor(as.character(restaurants_imputed$postal_code))
restaurants_imputed$Open7days <- as.factor(as.character(restaurants_imputed$Open7days))
restaurants_imputed$OutdoorSeating <- as.factor(as.character(restaurants_imputed$OutdoorSeating))
restaurants_imputed$parkingCom <- as.factor(as.character(restaurants_imputed$parkingCom))
restaurants_imputed$WiFi <- as.factor(as.character(restaurants_imputed$WiFi))
restaurants_imputed$BusinessAcceptsCreditCards <- as.factor(as.character(restaurants_imputed$BusinessAcceptsCreditCards))
restaurants_imputed$RestaurantsReservations <- as.factor(as.character(restaurants_imputed$RestaurantsReservations))
restaurants_imputed$NoiseLevel <- as.factor(as.character(restaurants_imputed$NoiseLevel))
restaurants_imputed$RestaurantsDelivery <- as.factor(as.character(restaurants_imputed$RestaurantsDelivery))


#LL -6976.3, R2: 0.047377, AIC(14095.77)
m1 <- mlogit(stars ~ 0 | RestaurantsAttire + Open7days + OutdoorSeating + parkingCom + BusinessAcceptsCreditCards, reflevel = "1", data = df1mlogit)
summary(m1)
exp(coef(m1))
AIC(m1)

# LL -6976.3, R2: 0.047377, AIC(14096.65)
m2 <- mlogit(stars ~ 0 | RestaurantsAttire + Open7days + OutdoorSeating + parkingCom + BusinessAcceptsCreditCards + WiFi, reflevel = "1", data = df1mlogit)
summary(m2)
exp(coef(m2))
AIC(m2)

#LL -6847.7, R2 0.064946, AIC(13919.32)
m3 <- mlogit(stars ~ 0 |categories +RestaurantsAttire + Open7days + OutdoorSeating + parkingCom + BusinessAcceptsCreditCards + WiFi, reflevel = "1", data = df1mlogit)
summary(m3)
exp(coef(m3))
AIC(m3)

#LL -6818.8, R2: 0.06889, AIC(13877.5)
m4 <- mlogit(stars ~ 0 |categories + RestaurantsPriceRange2 + RestaurantsAttire + Open7days + OutdoorSeating + parkingCom + BusinessAcceptsCreditCards + WiFi, reflevel = "1", data = df1mlogit)
summary(m4)
exp(coef(m4))
AIC(m4)

#LL -6939.3, R2: 0.052439, AIC(14038.51)
m5 <- mlogit(stars ~ 0 |RestaurantsPriceRange2 + RestaurantsAttire + Open7days + OutdoorSeating + parkingCom + BusinessAcceptsCreditCards + WiFi, reflevel = "1", data = df1mlogit)
summary(m5)
exp(coef(m5))
AIC(m5)

#LL -6757.1, R2: 0.077306, AIC(13802.29)
m6 <- mlogit(stars ~ 0 |categories + RestaurantsPriceRange2 + RestaurantsAttire + Open7days + OutdoorSeating + parkingCom + BusinessAcceptsCreditCards + WiFi + NoiseLevel, reflevel = "1", data = df1mlogit)
summary(m6)
exp(coef(m6))
AIC(m6)

#LL -7034.1, R2:0.039492, AIC(14276.14)
m7 <- mlogit(stars ~ 0 |categories + RestaurantsPriceRange2 + RestaurantsAttire + OutdoorSeating + BusinessAcceptsCreditCards + WiFi, reflevel = "1", data = df1mlogit)
summary(m7)
exp(coef(m7))
AIC(m7)

#LL -7067.3, R2: 0.034951, AIC(14294.64)
m8 <- mlogit(stars ~ 0 |categories + RestaurantsPriceRange2 + RestaurantsAttire + OutdoorSeating, reflevel = "1", data = df1mlogit)
summary(m8)
exp(coef(m8))
AIC(m8)

#all models not good, but m6 the best fitted model

#LR test
m6 <-mlogit(stars ~ 0 |categories + RestaurantsPriceRange2 + RestaurantsAttire + Open7days + OutdoorSeating + parkingCom + BusinessAcceptsCreditCards + WiFi + NoiseLevel, reflevel = "1", data = df1mlogit)
summary(m6)

nullmodel1 <-mlogit(stars ~ 0 | 1, data=df1mlogit)
summary(nullmodel1)

lrtest(nullmodel1, m6)

#significant, HO is rejected
#our model m6 is better than the null model

#market shares
m6$fitted=apply(fitted(m6, type="probabilities"),2,mean)
rbind(m6$fitted, m6$freq/sum(m6$freq))

library(effects)
library(plm)
library(dplyr)

effects(m1,covariate="RestaurantsPriceRange2",data = df1mlogit)

df1mlogit$Price1 <- ifelse(df1mlogit$RestaurantsPriceRange2 == '2', 1, 0)
df1mlogit$Price2 <- ifelse(df1mlogit$RestaurantsPriceRange2 == '3', 1, 0)
df1mlogit$Price3 <- ifelse(df1mlogit$RestaurantsPriceRange2 == '4', 1, 0) 


summary(df1mlogit$RestaurantsPriceRange2)

m2 <- mlogit(stars ~ 0 | Price1 + Price2 + Price3 + parkingCom, reflevel = "1", data = df1mlogit)
summary(m2)
exp(coef(m2))

effects(m2,covariate="Price2",data = df1mlogit)
effects

#test for IIA
m6 <- mlogit(stars ~ 0 |RestaurantsPriceRange2 + parkingCom + review_count, reflevel = "1", data = df1mlogit)
summary(m6)

m6sub <- mlogit(stars ~ 0 |RestaurantsPriceRange2 + parkingCom + review_count, reflevel = "1", alt.subset = c("1","2","2.5","3","3.5","4","4.5","5"), data = df1mlogit)

#test
hmftest(m6,m6sub)
#IAA is rejected -> alternatives differ systematically

#try probit or nested logit model
#probit model
library(dfidx)
library(mlogit)
#AIC: 14013.59
orderedlogitM <- polr(stars ~ categories + RestaurantsPriceRange2 + RestaurantsAttire + Open7days + OutdoorSeating + parkingCom + WiFi + BusinessAcceptsCreditCards + NoiseLevel, data = restaurants_imputed, Hess = TRUE)
summary(orderedlogitM)

#AIC: 14387.38 
orderedlogitM1 <- polr(stars ~ categories + RestaurantsPriceRange2 + RestaurantsAttire + OutdoorSeating + WiFi + NoiseLevel + RestaurantsReservations, data = restaurants_imputed, Hess = TRUE)
summary(orderedlogitM1)

#AIC:14270.17
orderedlogitM2 <- polr(stars ~ categories + RestaurantsPriceRange2 + RestaurantsAttire + OutdoorSeating + WiFi + NoiseLevel + RestaurantsReservations + review_count +RestaurantsDelivery , data = restaurants_imputed, Hess = TRUE)
summary(orderedlogitM2)

#AIC: 14524.88
orderedlogitM3 <- polr(stars ~ categories + RestaurantsPriceRange2 + RestaurantsAttire, data = restaurants_imputed, Hess = TRUE)
summary(orderedlogitM3)

#lowest AIC with 13862.39
orderedlogitM4 <- polr(stars ~ categories + checkedin100 + RestaurantsPriceRange2 + RestaurantsAttire + Open7days + OutdoorSeating + RestaurantsReservations + review_count + 
                         GoodForKids + parkingCom + postal_code, data = restaurants_imputed, Hess = TRUE)
summary(orderedlogitM4)

#comparing AIC of best model and null model
orderedlogitM4 <- polr(stars ~ categories + checkedin100 + RestaurantsPriceRange2 + RestaurantsAttire + Open7days + OutdoorSeating + RestaurantsReservations + review_count + 
                         GoodForKids + parkingCom + postal_code, data = restaurants_imputed, Hess = TRUE)

summary(orderedlogitM4)

nullmodel2 <- polr(stars ~ 1, data = restaurants_imputed, Hess = TRUE)
summary(nullmodel2)
#AIC 14662.56

#Our model has a smaller AIC than nullmodel
#our model is better than nullmodel

#nested model

#R2: 0.011344 AIC: 14518.41
nl1 <- mlogit(stars ~ 0 |RestaurantsPriceRange2,
              data = df1mlogit, nests = list(low = c("1","1.5","2"), medium = c("2.5","3","3.5"), high = c("4","4.5","5")))
summary(nl1)
AIC(nl1)

#R2: 0.059462  AIC:  13845.64
nl2 <- mlogit(stars ~ 0 |RestaurantsPriceRange2 + parkingCom + review_count,
              data = df1mlogit, nests = list(low = c("1","1.5","2"), medium = c("2.5","3","3.5"), high = c("4","4.5","5")))
summary(nl2)
AIC(nl2)

###changing the nests is not working for me
nl3 <- mlogit(stars ~ 0 |RestaurantsPriceRange2 + parkingCom + review_count,
              data = df1mlogit, nests = list(low = c("1","2"), medium = c("3","4"), high = c("5")))
summary(nl3)
AIC(nl3)

#R2:  0.059902 AIC:  13855.2
nl4 <- mlogit(stars ~ 0 |RestaurantsPriceRange2 + parkingCom + review_count + postal_code,
              data = df1mlogit, nests = list(low = c("1","1.5","2"), medium = c("2.5","3","3.5"), high = c("4","4.5","5")))
summary(nl4)
AIC(nl4)

#R2:  0.062456 AIC:  13833.8
nl5 <- mlogit(stars ~ 0 |RestaurantsPriceRange2 + parkingCom + review_count + postal_code + OutdoorSeating,
              data = df1mlogit, nests = list(low = c("1","1.5","2"), medium = c("2.5","3","3.5"), high = c("4","4.5","5")))
summary(nl5)
AIC(nl5)

#R2:  0.070506  AIC:  13763.88
nl6 <- mlogit(stars ~ 0 |RestaurantsPriceRange2 + parkingCom + review_count + postal_code + OutdoorSeating + NoiseLevel ,
              data = df1mlogit, nests = list(low = c("1","1.5","2"), medium = c("2.5","3","3.5"), high = c("4","4.5","5")))
summary(nl6)
AIC(nl6)


#R2:  0.073872   AIC:  13730.59
nl7 <- mlogit(stars ~ 0 |RestaurantsPriceRange2 + parkingCom + review_count + postal_code + OutdoorSeating + NoiseLevel + RestaurantsDelivery,
              data = df1mlogit, nests = list(low = c("1","1.5","2"), medium = c("2.5","3","3.5"), high = c("4","4.5","5")))
summary(nl7)
AIC(nl7)

#R2:  0.082183    AIC:  13624.85
nl8 <- mlogit(stars ~ 0 |RestaurantsPriceRange2 + parkingCom + review_count + postal_code + OutdoorSeating + NoiseLevel + RestaurantsDelivery + GoodForKids,
              data = df1mlogit, nests = list(low = c("1","1.5","2"), medium = c("2.5","3","3.5"), high = c("4","4.5","5")))
summary(nl8)
AIC(nl8)

#R2:  0.097794     AIC:  13428.21
nl9 <- mlogit(stars ~ 0 |RestaurantsPriceRange2 + parkingCom + review_count + postal_code + OutdoorSeating + NoiseLevel + RestaurantsDelivery + GoodForKids + sumposreview + sumnegreview,
              data = df1mlogit, nests = list(low = c("1","1.5","2"), medium = c("2.5","3","3.5"), high = c("4","4.5","5")))
summary(nl9)
AIC(nl9)


#R2:  0.097794     AIC:  13428.21
nl10 <- mlogit(stars ~ 0 |RestaurantsPriceRange2 + parkingCom + review_count + postal_code + OutdoorSeating + NoiseLevel + RestaurantsDelivery + GoodForKids + sumposreview + sumnegreview,
               data = df1mlogit, nests = list(low = c("1"), medium = c("3","4"), high = c("5")))
summary(nl10)
AIC(nl10)
#error for different nests

#market shares
nl9$fitted=apply(fitted(nl9, type="probabilities"),2,mean)
rbind(nl9$fitted, nl9$freq/sum(nl9$freq))


