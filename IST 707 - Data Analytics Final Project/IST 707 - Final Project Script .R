#load the required packages
library(dplyr)
library(tidyr)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
require(rpart)
library(RWeka)
library(tidyverse)
library(stringr)
library(arules)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("devtools")
library(devtools)
install.packages("jitter")
library(jitter)
install.packages("qplot")
library(qplot)
library(e1071)
 library(randomForest)

#load data 
datafifteen <- read.csv("/Project/Data/2015.csv")
str(datafifteen)
datasixteen <- read.csv("/Project/Data/2016.csv")
str(datasixteen)
dataseventeen <- read.csv("/Project/Data/2017.csv")
str(dataseventeen)

#Data Preprocessing
#add the years corresponding to each observation
datafifteen$Year<-2015
datasixteen$Year<-2016
dataseventeen$Year<-2017

#add a new column, "continent". We will use this instead of region for our dataset
datafifteen$continent <- NA
datasixteen$continent <- NA
dataseventeen$continent <- NA

#delete columns
#delete the "Region" columns from years 2015 and 2016
datafifteen$Region <- NULL 
datasixteen$Region <- NULL 

#delete the "Lower & Upper .Confidence.Interval"columns from year 2016. We can create another boxplot if we need it. 
datasixteen$Lower.Confidence.Interval <- NULL 
datasixteen$Upper.Confidence.Interval <- NULL 

#delete the "Whisker High" and Whisker Low" columns from year 2017. The datadic does not specify what this column means (we don't want to assume).We can create another boxplot if we need it. 
dataseventeen$Whisker.high <- NULL 
dataseventeen$Whisker.low <- NULL 

#Now lets fix the column headings, so they are consistent when we bind our rows
colnames(datafifteen)
#Our column names for 2015 are as follows: 
"Country"                          country
"Happiness.Rank"                   change to happinessrank
"Happiness.Score"                  change to happinessscore
"Standard.Error"                   change to standarderror
"Economy..GDP.per.Capita."         change to GDPpercap
"Family"                           family
"Health..Life.Expectancy."         change to healthlifeexpectancy
"Freedom"                          freedom
"Trust..Government.Corruption."    change to govtcorruption
"Generosity"                       generosity
"Dystopia.Residual"                dystopiaresidual
"Year"                             year
"continent"                       continent

#create new column names
colnames (datafifteen) <- c("country", "happinessrank", "happinessscore", "standarderror",
                       "GDPpercap", "family", "healthlifeexpectancy", "freedom", "govtcorruption", "generosity",
                       "dystopiaresidual", "year", "continent")

colnames(datasixteen)
#Our column names for 2016 are as follows: 
"Country"                       country
"Happiness.Rank"                change to happinessrank
"Happiness.Score"               change to happinessscore
"Lower.Confidence.Interval"     we should delete this column. We can build our own box plot     #deleted
"Upper.Confidence.Interval"      we should delete this column. We can build our own box plot    #deleted
"Economy..GDP.per.Capita."       GDPpercap
"Family"                         family
"Health..Life.Expectancy."       healthlifeexpectancy
"Freedom"                        freedom
"Trust..Government.Corruption."  govtcorruption
"Generosity"                     generosity
"Dystopia.Residual"              dystopiaresidual
"Year"                           year
"continent"                       continent

#create new column names
colnames (datasixteen) <- c("country", "happinessrank", "happinessscore",
                            "GDPpercap", "family", "healthlifeexpectancy", "freedom", "govtcorruption", "generosity",
                            "dystopiaresidual", "year", "continent")

colnames(dataseventeen)
"Country"                       country
"Happiness.Rank"                change to happinessrank
"Happiness.Score"               change to happinessscore
"Whisker.high"                  I believe this is the conf interval, but we arent certain (the datadic does not specify). We should delete and build our own box plot. #deleted
"Whisker.low"                   I believe this is the conf interval, but we arent certain (the datadic does not specify). We should delete and build our own box plot.  #deleted
"Economy..GDP.per.Capita."      GDPpercap
"Family"                        family
"Health..Life.Expectancy."      healthlifeexpectancy
"Freedom"                       freedom
"Generosity"                    generosity
"Trust..Government.Corruption." govtcorruption
"Dystopia.Residual"             dystopiaresidual
"Year"                          year
colnames (dataseventeen) <- c("country", "happinessrank", "happinessscore",
                            "GDPpercap", "family", "healthlifeexpectancy", "freedom", "generosity", "govtcorruption",
                            "dystopiaresidual", "year", "continent")

#Continents
#I now want to add data into the continent column. This function will give us the continents of each country for 2015
datafifteen$continent[which(datafifteen$country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                             "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                             "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                             "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                             "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                             "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                             "Cambodia", "Afghanistan", "Yemen", "Syria", "Taiwan", "Hong Kong", "Laos", "Oman"))] <- "Asia"
datafifteen$continent[which(datafifteen$country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                             "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                             "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                             "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                             "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                             "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                             "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                             "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                             "Bulgaria", "Albania", "Ukraine"))] <- "Europe"
datafifteen$continent[which(datafifteen$country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                             "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                             "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                             "Haiti"))] <- "North America"
datafifteen$continent[which(datafifteen$country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                             "Colombia", "Ecuador", "Bolivia", "Peru",
                                             "Paraguay", "Venezuela", "Suriname"))] <- "South America"
datafifteen$continent[which(datafifteen$country %in% c("New Zealand", "Australia", "Papua New Guinea"))] <- "Australia"
datafifteen$continent[which(datafifteen$country %in% c("Libya", "Algeria", "Mauritius", "Nigeria", "Ghana", "Somalia", "Burundi", "South Africa", "Niger", "Togo",
                                                       "Congo (Kinshasa)", "Zambia", "Morocco", "Mozambique", "Lesotho", "Swaziland", "Tunisia", "Zimbabwe", "Liberia", "Sudan",
                                                       "Ethiopia", "Sierra Leone", "Mauritania", "Kenya", "Djibouti", "Botswana", "Malawi", "Cameroon", "Egypt", 
                                                       "Angola", "Mali", "Congo (Brazzaville)", "Uganda", "Senegal", "Gabon", "Tanzania", "Guinea", "Madagascar", "Central African Republic", "Chad", "Ivory Coast", 
                                                       "Burkina Faso", "Rwanda", "Benin", "Burundi", "Comoros","Somaliland region" ))] <- "Africa"

#LETS DO THE SAME FOR 2016
datasixteen$continent[which(datasixteen$country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                                       "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                                       "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                                       "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                                       "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                                       "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                                       "Cambodia", "Afghanistan", "Yemen", "Syria", "Taiwan", "Hong Kong", "Laos", "Oman"))] <- "Asia"
datasixteen$continent[which(datasixteen$country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                                       "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                                       "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                                       "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                                       "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                                       "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                                       "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                                       "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                                       "Bulgaria", "Albania", "Ukraine"))] <- "Europe"
datasixteen$continent[which(datasixteen$country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                                       "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                                       "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                                       "Haiti", "Puerto Rico"))] <- "North America"
datasixteen$continent[which(datasixteen$country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                                       "Colombia", "Ecuador", "Bolivia", "Peru",
                                                       "Paraguay", "Venezuela", "Suriname"))] <- "South America"
datasixteen$continent[which(datasixteen$country %in% c("New Zealand", "Australia", "Papua New Guinea"))] <- "Australia"
datasixteen$continent[which(datasixteen$country %in% c("Libya", "Algeria", "Mauritius", "Nigeria", "Ghana", "Somalia", "Burundi", "South Africa", "Niger", "Togo",
                                                       "Congo (Kinshasa)", "Zambia", "Morocco", "Mozambique", "Lesotho", "Swaziland", "Tunisia", "Zimbabwe", "Liberia", "Sudan",
                                                       "Ethiopia", "Sierra Leone", "Mauritania", "Kenya", "Djibouti", "Botswana", "Malawi", "Cameroon", "Egypt", 
                                                       "Angola", "Mali", "Congo (Brazzaville)", "Uganda", "Senegal", "Gabon", "Tanzania", "Guinea", "Madagascar", "Central African Republic", "Chad", "Ivory Coast", 
                                                       "Burkina Faso", "Rwanda", "Benin", "Burundi", "Comoros","Somaliland region", "Somaliland Region", "Namibia", "South Sudan"))] <- "Africa"

#Now for 2017
dataseventeen$continent[which(dataseventeen$country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                                       "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                                       "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                                       "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                                       "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                                       "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                                       "Cambodia", "Afghanistan", "Yemen", "Syria", "Taiwan", "Hong Kong", "Laos", "Oman"))] <- "Asia"
dataseventeen$continent[which(dataseventeen$country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                                       "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                                       "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                                       "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                                       "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                                       "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                                       "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                                       "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                                       "Bulgaria", "Albania", "Ukraine"))] <- "Europe"
dataseventeen$continent[which(dataseventeen$country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                                       "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                                       "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                                       "Haiti", "Puerto Rico"))] <- "North America"
dataseventeen$continent[which(dataseventeen$country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                                       "Colombia", "Ecuador", "Bolivia", "Peru",
                                                       "Paraguay", "Venezuela", "Suriname"))] <- "South America"
dataseventeen$continent[which(dataseventeen$country %in% c("New Zealand", "Australia", "Papua New Guinea"))] <- "Australia"
dataseventeen$continent[which(dataseventeen$country %in% c("Libya", "Algeria", "Mauritius", "Nigeria", "Ghana", "Somalia", "Burundi", "South Africa", "Niger", "Togo",
                                                       "Congo (Kinshasa)", "Zambia", "Morocco", "Mozambique", "Lesotho", "Swaziland", "Tunisia", "Zimbabwe", "Liberia", "Sudan",
                                                       "Ethiopia", "Sierra Leone", "Mauritania", "Kenya", "Djibouti", "Botswana", "Malawi", "Cameroon", "Egypt", 
                                                       "Angola", "Mali", "Congo (Brazzaville)", "Uganda", "Senegal", "Gabon", "Tanzania", "Guinea", "Madagascar", "Central African Republic", "Chad", "Ivory Coast", 
                                                       "Burkina Faso", "Rwanda", "Benin", "Burundi", "Comoros","Somaliland region", "Somaliland Region", "Namibia", "South Sudan"))] <- "Africa"

#move the continent column to be the second column, and the year to be the third 
datafifteen <- datafifteen %>% select(country,continent,year, everything())
datasixteen <- datasixteen %>% select(country,continent,year, everything())
dataseventeen <- dataseventeen %>% select(country,continent,year, everything())

#bind the three (3) datasets together and create a master dataset
masterdata<-bind_rows(datafifteen,datasixteen, dataseventeen)

#I noticed that there is no standard error for 2016 and 2017, we might have to either delete this column allogether OR we can find the SE of the two other years. 
#move the continent column to be the second column, and the year to be the third 
data <- masterdata %>% select(country,continent,year, everything())

#export master dataset
write.csv(data, file="/Project/master data.csv", row.names = FALSE)
#export new 2015 dataset
write.csv(datafifteen, file="/Project/datafifteen.csv", row.names = FALSE)
#export new 2016 dataset
write.csv(datasixteen, file="/Project/datasixteen.csv", row.names = FALSE)
#export new 2017 dataset
write.csv(dataseventeen, file="/Project/dataseventeen.csv", row.names = FALSE)


#begin exploratory analysis
#import
data <- read.csv("/Project/master data.csv")
datafifteen <- read.csv("/Project/datafifteen.csv")
datasixteen <- read.csv("/Project/datasixteen.csv")
dataseventeen <- read.csv("/Project/dataseventeen.csv")


#Now we want to take a look at our dataset
#Glimpse 
glimpse(data)

#convert data types
#converts "country" into a factor data type
data$country=factor(data$country) 
str(data)   #view structure

#Convert "happiness rank" into ordinal data as "ordered factors"
data$happinessrank=ordered(data$happinessrank)

#remove years from dataframes 
datafifteen <- datafifteen[,-3]
datasixteen <- datasixteen[,-3]
dataseventeen <- dataseventeen[,-3]

datafifteen
#MEAN HAPPINESS SCORES

#mean happiness score based on continent (2015)   
datafifteen %>%
  group_by (continent) %>%
  summarise(mean = mean(happinessscore))

#mean happiness score based on continent (2016)   
datasixteen %>%
  group_by (continent) %>%
  summarise(mean = mean(happinessscore))

#mean happiness score based on continent (2017)   
dataseventeen %>%
  group_by (continent) %>%
  summarise(mean = mean(happinessscore))

#mean happiness score based on continent (whole dataset)   
data %>%
  group_by (continent, year) %>%
  summarise(mean = mean(happinessscore))


#based on country
datafifteen %>%
  group_by (country) %>%
  summarise(mean = mean(happinessscore))

#Visualize
#Generate barchart based on continent & Year
data %>%
  group_by(continent) %>%
  summarise(happinessscore)) %>%
  ggplot() + aes(x = continent, y = happinessscore) + geom_col()

#bar graph (void)
data %>%
  group_by(happinessscore) %>%
  summarise(continent) %>%
  ggplot() + aes(x = continent, y = happinessscore) + geom_col()

#bar graph (this works)
data %>%
  group_by(happinessscore) %>%
  ggplot() + aes(x = continent, y = happinessscore) + geom_col()

#scatterplot of 2015 - Continent/Happiness Score (This was printed out)
ggplot(datafifteen) +
  aes(x = continent, y = happinessscore) + 
  geom_jitter(aes(color=happinessscore)) 

#scatterplot of 2016 - Continent/Happiness Score (This was printed out)
ggplot(datasixteen) +
  aes(x = continent, y = happinessscore) + 
  geom_jitter(aes(color=happinessscore)) 

#scatterplot of 2017 - Continent/Happiness Score (This was printed out)
ggplot(dataseventeen) +
  aes(x = continent, y = happinessscore) + 
  geom_jitter(aes(color=happinessscore)) 

#scatterplot of entire dataset - Continent/Happiness Score (This was printed out)
ggplot(data) +
  aes(x = continent, y = happinessscore) + 
  geom_jitter(aes(color=happinessscore)) 

# Relation between Happiness score and GDP
ggplot(datafifteen,aes(x=happinessscore,y=GDPpercap,color=continent,size=healthlifeexpectancy))+geom_point(alpha=0.6)+labs(title="Happiness Score with Health Life Expectancy - 2015")
ggplot(datasixteen,aes(x=happinessscore,y=GDPpercap,color=continent,size=healthlifeexpectancy))+geom_point(alpha=0.6)+labs(title="Happiness Score with Health Life Expectancy - 2016")
ggplot(dataseventeen,aes(x=happinessscore,y=GDPpercap,color=continent,size=healthlifeexpectancy))+geom_point(alpha=0.6)+labs(title="Happiness Score with Health Life Expectancy - 2017")

#CORRELATION MATRICES 

#I want to run some correlation analysis on our varibles 
cor.test(data$happinessscore,data$GDPpercap)

#output: Pearson's product-moment correlation
data:  data$happinessscore and data$GDPpercap
t = 27.454, df = 468, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.7481519 0.8177994
sample estimates:
      cor 
0.7854496 

#correlation
cor.test(data$happinessscore,data$family)
#Pearson's product-moment correlation
data:  data$happinessscore and data$family
t = 17.855, df = 468, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
  0.5794440 0.6874043
sample estimates:
  cor 
0.636532

#correlation
cor.test(data$happinessscore,data$freedom)
#output:Pearson's product-moment correlation
data:  data$happinessscore and data$freedom
t = 14.636, df = 468, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.4949927 0.6194083
sample estimates:
      cor 
0.5603534 

#correlation
cor.test(data$happinessscore,data$healthlifeexpectancy)
#output: Pearson's product-moment correlation
data:  data$happinessscore and data$healthlifeexpectancy
t = 24.384, df = 468, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.7053127 0.7853526
sample estimates:
      cor 
0.7480404 


#cor
cor.test(data$happinessscore,data$govtcorruption)

#cor
cor.test(data$happinessscore,data$dystopiaresidual)
#output: Pearson's product-moment correlation
data:  data$happinessscore and data$dystopiaresidual
t = 12.152, df = 468, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.4178062 0.5555850
sample estimates:
      cor 
0.4897472 

cor.test(data$happinessscore,data$generosity)
#output: Pearson's product-moment correlation
data:  data$happinessscore and data$generosity
t = 3.5867, df = 468, p-value = 0.0003702
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.07421104 0.25030705
sample estimates:
      cor 
0.1635616 

#we see the following results 
GDP(0.79),healthlifeexpectancy(75%),
freedom (56%), family(64%), GDPpercap(79%),  dystopiaresidual (49%), govtcorruption (41%), 

#CORRELATION
install.packages("ellipse")
library(ellipse)
install.packages("RColorBrewer")
eldata=cor(data[, 3:12])
my_colors <- brewer.pal(7, "Greens")
my_colors=colorRampPalette(my_colors)(100)

# Order the correlation matrix
ord <- order(eldata[1, ])
data_ord = eldata[ord, ord]
plotcorr(data_ord , col=my_colors[data_ord*50+50] , mar=c(1,1,1,1)  )

glimpse(data)

#3D Scatter 
install.packages("plot3D")
library(plot3D)
scatter3D(datafifteen$freedom, datafifteen$healthlifeexpectancy, datafifteen$happinessscore, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed",
          main = "3D Plot of Happiness Score - 2015", xlab = "Freedom",
          ylab ="Health-Life Expectancy", zlab = "Happiness Score")


scatter3D(data$GDPpercap, data$healthlifeexpectancy, data$happinessscore, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed",
          main = "3D Plot of Happiness Data", xlab = "GDP Per Capital",
          ylab ="Health-Life Expectancy", zlab = "Happiness Score")

#additional Visualization 

#Time Series Visualization
install.packages("ggalt")
library(ggalt)
d15<-datafifteen %>% select(country,continent,HS15=happinessscore)
d16<-datasixteen %>% select(country,continent,HS16=happinessscore)
d17<-dataseventeen %>% select(country,continent,HS17=happinessscore)
glimpse(datafifteen)

#2015 vs. 2016
score<-inner_join(d15,d16)%>% mutate(score_diff= HS16-HS15)%>% filter(score_diff>0)

score$country <- factor(score$country, levels=as.character(score$country))
gg <- ggplot(score, aes(x=HS15, xend=HS16, y=country, group=country)) + 
  geom_dumbbell(size=2, color="#e3e2e1", 
                colour_x = "#5b8124", colour_xend = "#bad744",
                dot_guide=TRUE, dot_guide_size=0.25) + 
  labs(x=NULL, 
       y=NULL, 
       
       title=" Country Happiness Scores Increase: 2015 vs. 2016"
  ) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)

#2016 vs. 2017
score<-inner_join(d16,d17)%>% mutate(score_diff= HS17-HS16)%>% filter(score_diff>0)

score$country <- factor(score$country, levels=as.character(score$country))
gg <- ggplot(score, aes(x=HS16, xend=HS17, y=country, group=country)) + 
  geom_dumbbell(size=2, color="#e3e2e1", 
                colour_x = "#5b8124", colour_xend = "#bad744",
                dot_guide=TRUE, dot_guide_size=0.25) + 
  labs(x=NULL, 
       y=NULL, 
       
       title=" Country Happiness Scores Increase: 2016 vs. 2017"
  ) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)

#linear model
linearModel <- lm(formula = happinessscore ~ GDPpercap + family + healthlifeexpectancy + freedom + dystopiaresidual + govtcorruption, data = data) 
summary(linearModel)   #view the summary

#Predictions

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(115)
dataset <- happiness2[4:11]
split = sample.split(dataset$happinessscore, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Lets start with a Decision Tree Regression
# Fitting Decision Tree Regression to the dataset
library(rpart)
regressor_dt = rpart(formula = happinessscore ~ .,
                     data = happiness2,
                     control = rpart.control(minsplit = 10))

# Predicting a new result with Decision Tree Regression
y_pred_dt = predict(regressor_dt, newdata = test_set)

Pred_Actual_dt <- as.data.frame(cbind(Prediction = y_pred_dt, Actual = test_set$happinessscore))

gg.dt <- ggplot(Pred_Actual_dt, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Decision Tree Regression", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))
gg.dt

#It seems that Decision Tree Regression is not an excellent choice for this dataset (linear)

#Build decision tree
# Plotting the tree
library(rpart.plot)
prp(regressor_dt)

#BUILD DECISION TREE - TRAINING/TEST, ETC. 
install.packages('rJava', type='source')
dyn.load("/Library/Java/JavaVirtualMachines/jdk-11.0.5.jdk/Contents/Home/lib/server/libjvm.dylib")
library(rJava)
library(RWeka)

#discretizing - imported master dataset
library(arules)
library(arulesViz)
happiness <- data
str(happiness)

#Please note that several models were built and compared for optimization
#des tree - discretisize data 
glimpse(happiness2)
happiness2$happinessrank <- discretize(happiness2$happinessrank, method="frequency", breaks = 3, labels = c("very happy", "happy", "less happy"), order=T)
happiness2$GDPpercap <- discretize(happiness2$GDPpercap, method = "frequency", breaks = 3, 
                                   labels = c("low", "medium", "high"), order = T)
happiness2$family <- discretize(happiness2$family, method = "frequency", breaks = 3, 
                                labels = c("low", "medium", "high"), order = T)
happiness2$healthlifeexpectancy <- discretize(happiness2$healthlifeexpectancy, method = "frequency", breaks = 3, 
                                              labels = c("low", "medium", "high"), order = T)
happiness2$freedom <- discretize(happiness2$freedom, method = "frequency", breaks = 3, 
                                 labels = c("low", "medium", "high"), order = T)
happiness2$govtcorruption <- discretize(happiness2$govtcorruption, method = "frequency", breaks = 3, 
                                        labels = c("low", "medium", "high"), order = T)
happiness2$generosity <- discretize(happiness2$generosity, method = "frequency", breaks = 3, 
                                    labels = c("low", "medium", "high"), order = T)
str(happiness)
glimpse(happiness2)

#create test and train data
happiness2 <- happiness2[,-1]
happiness2 <- happiness2[,-2]
n <- nrow(happiness2)
n_train <- round(0.8 * n) 
set.seed(123)
train_indices <- sample(1:n, n_train)
train <- happiness2[train_indices, ]  
test <- happiness2[-train_indices, ]

model <- rpart(formula = happinessrank ~ GDPpercap + family + healthlifeexpectancy + freedom + govtcorruption + generosity + dystopiaresidual,
               data = happiness2,
               method = "class")

rpart.plot(model)   #view the model

#Performance Evaluation
test$pred <- predict(object = model,  
                     newdata = test,   
                     type = "class")

#Confusion Matrix
library(caret)
confusionMatrix(data = test$pred,       
                reference = test$happinessrank)

#des tree - undiscretisize data 
glimpse(data)
data3 <- data3
str(happiness)

n <- nrow(data2)
n_train <- round(0.8 * n) 
set.seed(123)
train_indices <- sample(1:n, n_train)
train <- data2[train_indices, ]  
test <- data2[-train_indices, ]

model <- rpart(formula = happinessrank ~ GDPpercap + family + healthlifeexpectancy + freedom + govtcorruption + generosity,
               data = data2,
               method = "class")

rpart.plot(model)   #view the model

#Performance Evaluation
test$pred <- predict(object = model,  
                     newdata = test,   
                     type = "class")

#Confusion Matrix
library(caret)
confusionMatrix(data = test$pred,       
                reference = test$Survived)

#undesc (numeric) DATA 3 
data2
data3 <- data2
glimpse(data3)
data3$happinessrank <- discretize(data3$happinessrank, method="frequency", breaks = 3, labels = c("very happy", "happy", "less happy"), order=T)
data3 <- data3[,-8]
data3 <- data3[,-3]
n <- nrow(data3)
n_train <- round(0.8 * n) 
set.seed(123)
train_indices <- sample(1:n, n_train)
train <- data3[train_indices, ]  
test <- data3[-train_indices, ]

model <- rpart(formula = happinessrank ~ GDPpercap + family + healthlifeexpectancy + freedom + govtcorruption + generosity,
               data = data3,
               method = "class")

rpart.plot(model)   #view the model

#Performance Evaluation
test$pred <- predict(object = model,  
                     newdata = test,   
                     type = "class")
pred

#Confusion Matrix
library(caret)
confusionMatrix(data = test$pred,       
                reference = test$happinessrank)

#DESCISION TREE (Undescritized)
data3 <- data
NN <- make_Weka_filter("weka/filters/unsupervised/attribute/NumericToNominal") # build a function using RWeka filter interface

#split train and test
n <- nrow(data3)
n_train <- round(0.8 * n) 
set.seed(123)
train_indices <- sample(1:n, n_train)
trainset <- data3[train_indices, ]  
testset <- data3[-train_indices, ]

#Apply the filter function to both training and test datasets.
trainset <- NN(data=trainset, control= Weka_control(R="1-3"), na.action = NULL) 
testset <- NN(data=testset, control= Weka_control(R="1,3"), na.action = NULL)

MS <- make_Weka_filter("weka/filters/unsupervised/attribute/ReplaceMissingValues") #missing values
#Apply the filter function to both training and test datasets.
trainset <-MS(data=trainset, na.action = NULL)
testset <-MS(data=testset, na.action = NULL)
str(trainset)

#Apply J48 Algorithm
#Build decision tree model
m=J48(happinessrank~., data = trainset)
m=J48(happinessrank~., data = trainset, control=Weka_control(U=FALSE, M=2, C=0.5))
WOW("J48")   #view parameters 

#use 10 fold cross-validation to evaluate the model
e <- evaluate_Weka_classifier(m,
                  numFolds = 10,
                  seed = 1, class = TRUE)
e   #view

#APPLY THE MODEL WITH THE TEST DATASET 
pred=predict (m, newdata = testset, type = c("class"))

#export prediction in csv file 
write.csv(pred, file="path/filename.csv")
#export new undescr dataset
write.csv(pred, file="/Project/undescpred.csv", row.names = FALSE)
#import to view
undescpred <- read.csv("/Project/undescpred.csv")
glimpse(undescpred)

#build the DT with the (Descritized data) 
#remove country, continent, year, happiness score and standard error 
glimpse(happiness)
happiness <- happiness[,-1]
happiness <- happiness[,-2]
happiness <- happiness[,-1]
#remove some rows 
NN <- make_Weka_filter("weka/filters/unsupervised/attribute/NumericToNominal") # build a function using RWeka filter interface

#split train and test
n <- nrow(happiness)
n_train <- round(0.8 * n) 
set.seed(123)
train_indices <- sample(1:n, n_train)
trainset <- happiness[train_indices, ]  
testset <- happiness[-train_indices, ]

#Apply the filter function to both training and test datasets.
trainset <- NN(data=trainset, control= Weka_control(R="1-3"), na.action = NULL) 
testset <- NN(data=testset, control= Weka_control(R="1,3"), na.action = NULL)

MS <- make_Weka_filter("weka/filters/unsupervised/attribute/ReplaceMissingValues") #missing values
#Apply the filter function to both training and test datasets.
trainset <-MS(data=trainset, na.action = NULL)
testset <-MS(data=testset, na.action = NULL)
str(trainset)

#Apply J48 Algorithm
#Build decision tree model
m=J48(happinessrank~., data = trainset)
m=J48(happinessrank~., data = trainset, control=Weka_control(U=FALSE, M=2, C=0.5))
WOW("J48")   #view parameters 

#use 10 fold cross-validation to evaluate the model
e <- evaluate_Weka_classifier(m,
                              numFolds = 10,
                              seed = 1, class = TRUE)
e   #view
summary(e)
#APPLY THE MODEL WITH THE TEST DATASET 
pred=predict (m, newdata = testset, type = c("class"))
pred

#export prediction in new dataset (Descritized)
write.csv(pred, file="/Project/descritized Pred.csv", row.names = FALSE)

#import to view
undescpred <- read.csv("/Project/undescpred.csv")
glimpse(undescpred)
glimpse(happiness)