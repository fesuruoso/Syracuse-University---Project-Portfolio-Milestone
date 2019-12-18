# Final Project Import


#install the package
install.packages("jsonlite")
library(jsonlite) #for access to internet data
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("caret")
library(caret)
install.packages("lattice")
library(lattice)
install.packages("devtools")
library(devtools)
install.packages("dplyr")
library(dplyr)
install.packages("jitter")
library(jitter)
install.packages("qplot")
library(qplot)

mydata.list <-jsonlite::fromJSON("/Volumes/STORE N GO/00 - Graduate School/00 - SYR In Session/00 - Summer 2019/Intensive Course - IST 687 - Applied Data Science/Final Project - Team 3/projectsurvey.json")
#mydata.list <-jsonlite::fromJSON("E:/00 - Graduate School/00 - SYR In Session/00 - Summer 2019/Intensive Course - IST 687 - Applied Data Science/Final Project - Team 3/projectsurvey.json")


#create data frame
projectsurvey <- data.frame(mydata.list)
View(projectsurvey)

#Now we want to take a look at our dataset
#Glimpse 
glimpse(projectsurvey)

#Cleanup dataset
#cleanthedatafile
#o Remove any row with the Likelihood to recommend column as NA
Newpsurvey<-projectsurvey[!is.na(projectsurvey$Likelihood.to.recommend),]
#Newpsurvey1 <- projectsurvey[na.omit(projectsurvey$Likelihood.to.recommend),] #somehow taking the business class out
#Newpsurvey2 <- na.omit(projectsurvey) remove na from all the rows
Newpsurvey 
View(Newpsurvey)

#
Newpsurvey$Detractor <-as.factor(Newpsurvey$Likelihood.to.recommend<7)

# ----------------------------------------- mean ------------------
 #Begin explaratory
#mean based on Airline Status    
Newpsurvey %>%
  group_by (Airline.Status) %>%
  summarise(mean = mean(Likelihood.to.recommend))  #summarise because of categ varia
#two way table
mytable <- table(Newpsurvey$Detractor,Newpsurvey$Airline.Status)
mytable
prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages

#crosstab function created at university of Liverpool
# Load function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
# Calculate proportions rather than percentages...
#2 way table Frequency Table  "Airline Status"
crosstab(Newpsurvey, row.vars = "Detractor", col.vars = "Airline.Status", type = "t", percentages = FALSE)

#2 way table Frequency Table  "Origin State"
crosstab(Newpsurvey, row.vars = "Detractor", col.vars = "Eating.and.Drinking.at.Airport", type = "t", percentages = FALSE)
crosstab(Newpsurvey, row.vars = "Detractor", col.vars = "Gender", type = "t", percentages = FALSE)
crosstab(Newpsurvey, row.vars = "Detractor", col.vars = "Destination.City", type = "t", percentages = FALSE)

crosstab(Newpsurvey, row.vars = "Detractor", col.vars = "Airline.Status", type = "t", percentages = FALSE)


crosstab(Newpsurvey, row.vars = "Detractor", col.vars = "Class", type = "t", percentages = FALSE)

# Joint percentages (sums to 100 within final two table dimensions)
crosstab(Newpsurvey, row.vars = c("Class", "Airline.Status"), col.vars = "Detractor", type = "j")
crosstab(Newpsurvey, row.vars = c("Destination.City", "Origin.City"), col.vars = "Detractor", type = "j")

#mean based on Year of First Flight    
Newpsurvey %>%
  group_by (Year.of.First.Flight) %>%
  summarise(mean = mean(Likelihood.to.recommend))
#two way table
mytable1 <- table(Newpsurvey$Detractor,Newpsurvey$Year.of.First.Flight)
mytable1
prop.table(mytable1) # cell percentages

crosstab(Newpsurvey, row.vars = "Detractor", col.vars = "Year.of.First.Flight", type = "t", percentages = FALSE)
crosstab(Newpsurvey, row.vars = "Detractor", col.vars = "Day.of.Month", type = "t", percentages = FALSE)
crosstab(Newpsurvey, row.vars = "Detractor", col.vars = "Scheduled.Departure.Hour", type = "t", percentages = FALSE)

#mean based on Flights Per Year    
Newpsurvey %>%
  group_by (Flights.Per.Year) %>%
  summarise(mean = mean(Likelihood.to.recommend))


#mean based on Class  
Newpsurvey %>%
  group_by (Class) %>%
  summarise(mean = mean(Likelihood.to.recommend))
glimpse(Newpsurvey$Class)

crosstab(Newpsurvey, row.vars = "Detractor", col.vars = "Class", type = "t", percentages = FALSE)
crosstab(Newpsurvey, row.vars = "Detractor", col.vars = "Departure.Delay.in.Minutes", type = "t", percentages = FALSE)

#mean based on Day of the month  
Newpsurvey %>%
  group_by (Day.of.Month) %>%
  summarise(mean = mean(Likelihood.to.recommend))


#mean based on Day of the Flight Date  
Newpsurvey %>%
  group_by (Flight.date) %>%
  summarise(mean = mean(Likelihood.to.recommend))


#mean based on Scheduled Departure Hour  
Newpsurvey %>%
  group_by (Scheduled.Departure.Hour) %>%
  summarise(mean = mean(Likelihood.to.recommend))

glimpse(Newpsurvey$Scheduled.Departure.Hour)


# ---------------------------------------------------- PLOT ---------------------


View(Newsurvey)

# Generate a boxplot of Likelihood.to.Recommend
boxplot(Newpsurvey$Likelihood.to.recommend)
boxplot <- boxplot(Newpsurvey$Likelihood.to.recommend)


#boxplot
Newpsurvey %>% ggplot() +
  aes(x = Airline.Status, y = Likelihood.to.recommend) + 
  geom_boxplot() +
  ggtitle("Likelihood.to.recommend by Airline Status")


#Generate barchart based on Airline Status
Newpsurvey %>%
  group_by(Airline.Status) %>%
  summarise(nps = calcNPS(Likelihood.to.recommend)) %>%
  ggplot() + aes(y = nps, x = Airline.Status) + geom_col()

#Generate barchart based on Class
Newpsurvey %>%
  group_by(Class) %>%
  summarise(nps = calcNPS(Likelihood.to.recommend)) %>%
  ggplot() + aes(y = nps, x = Class) + geom_col()


#NPS
calcNPS <- function(v){
  v <- na.omit(v) # if there are any na's it removes it (not neccesary)
  total<-length(v)
  numPromoters<-length(v[v>8])
  numDetractors<-sum(v<7)
  
  #nps formula
  nps<-(numPromoters/total - numDetractors/total) * 100
  return(nps)
}

calcNPS()


# Scatterplot
# Where x is the Airline Status
#      y is Likelihood.to.recommend
ggplot(Newpsurvey) +
  aes(x = Airline.Status, y = Likelihood.to.recommend) + 
  geom_jitter(aes(color=Likelihood.to.recommend)) 

# ------------------------------- Linear Model -------------------------

#4b. Build a linear model
# try to predict Likelihood.to.recommend
# based on the eating and drinking and
# Type.of.Travel

#linear model
linearModel <- lm(formula = Likelihood.to.recommend ~ Airline.Status + Year.of.First.Flight + Class + Day.of.Month + Flight.date + Scheduled.Departure.Hour,
                  data = Newpsurvey) 
summary(linearModel)   #view the summary

#Group
linearModel <- lm(formula = Likelihood.to.recommend ~ Airline.Status + Eating.and.Drinking.at.Airport + Class + Age + Gender + Flight.cancelled + Partner.Code, 
      data = Newpsurvey)
summary(linearModel)   #view the summary
install.packages(Rcmdr)
library(Rcmdr)


write.csv(Newpsurvey, file = "Newpsurvey.csv")


#Correlation
cor -> cor(Newpsurvey[,c("Age","Arrival.Delay.in.Minutes","Day.of.Month","Departure.Delay.in.Minutes","dlat","dlong",
                              "Eating.and.Drinking.at.Airport","Flight.Distance","Flight.time.in.minutes","Flights.Per.Year","Likelihood.to.recommend",
                           "Loyalty","olat","olong","Price.Sensitivity","Scheduled.Departure.Hour","Shopping.Amount.at.Airport","Total.Freq.Flyer.Accts",
                         "Year.of.First.Flight")], use="complete")

linearModel <- lm(formula = Likelihood.to.recommend ~ Airline.Status + Eating.and.Drinking.at.Airport + Loyalty + Total.Freq.Flyer.Accts + Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes + Flight.Distance + Eating.and.Drinking.at.Airport
, 
                  data = Newpsurvey)
summary(linearModel)   #view the summary

#Code
linearModel <- lm(formula = Likelihood.to.recommend ~ Airline.Status + Gender + Class + Eating.and.Drinking.at.Airport + Loyalty + Total.Freq.Flyer.Accts + Age + Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes+ Eating.and.Drinking.at.Airport
                  , 
                  data = Newpsurvey)
summary(linearModel)   #view the summary
