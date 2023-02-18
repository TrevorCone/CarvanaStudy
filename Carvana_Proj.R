#Dataset from 
#https://www.kaggle.com/datasets/ravishah1/carvana-predict-car-prices
# Libraries used
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# read in .CSV file.
carvana <- read.csv("carvana.csv")
################## Data Cleaning/Manipulation #######################
head(carvana)
tail(carvana)
#in the head and tail calls of the dataframe I saw that there 
# some years listed with more than 4 digits. 
# Cars sold on carvana cannot be newer than 2022 from this list.
max(carvana$Year) # There is no car from the year 20223500
min(carvana$Year) # Minimum seems to be fine at 2009.
# Checking for NA values
# These lines go through the column and add up number of N/A entries
# All return 0. 
sum(is.na(carvana$Year))
sum(is.na(carvana$Miles))
sum(is.na(carvana$Price))

# Since 22000 objects is a lot to go through by hand I take 
# every year and use the function substr which changes the col into 
# a string with arguments 1 and 4 where it starts at the first character and stops at the 4th
# dropping the rest. 
# take the Year variable created to hold the characters for year and switch them
# into our main dataframe as numeric. 

Year <- substr(carvana$Year, 1, 4)
carvana$Year <- as.numeric(Year)
head(carvana)

# I also want to separate the name column into Make and Model.
# Since when we are looking for cars online you pick the make and then model separetly

#Get rid of first space
carvana$Name <- sub(" ", "", carvana$Name)
# Create two new col with Make (ie Chevrolet, GMC, etc) and Model
carvana[c("Make", "Model")] <- str_split_fixed(carvana$Name, ' ', 2)
#Reorganize columns to make sense
head(carvana)
colnames(carvana)
carvana <- carvana[,-1] # remove name column to make room for Make and Model columns
carvana <- carvana[,c(4,5,1,2,3)] # reorders Columns
head(carvana)
#Reorder so rownames are in order based on the new alphabetized
# order.
carvana <-carvana[order(carvana$Make),]
head(carvana)
rownames(carvana) <- NULL
# clened Data frame. 
head(carvana)

###################### Data Exploration ################################################

# Here we use the table function to see the count of each make
#
table(carvana$Make)
MakeCount <- ggplot(carvana, aes(y = Make, color = Make, fill = Make)) + 
  geom_bar() + 
  theme(legend.position = "none") + ggtitle("Distribution of Car Makes", subtitle = "Data from Carvana")
MakeCount
# The distribution of makes in the dataset makes sense to me
# This graph shows the makes with the most inventory are
# Volkswagen, Toyota, Nissan, Kia, Hyundai, Honda, Ford,
# Chevrolet. 
# The suprising count in the set would be Saturn, & Pontiac.
# Saturn and Pontiac haven't made any cars for a long time
max(carvana$Year)
min(carvana$Year)

YearDist <- ggplot(carvana, aes(x = Year)) + 
  geom_histogram(binwidth = 1.5, fill = "blue", col = "white") +
  xlab("Year (2009 to 2023)") + ggtitle("Distribution of Model Years (2009 - 2023)")
YearDist
# Looking at this graph we can see the majority of cars sold
# on carvana in this data set are between 2011 to 2020 although the range is from 2009 to 2023.

#Here we see the price range is large from $10,990 to $102,990
min(carvana$Price)
max(carvana$Price)
mean(carvana$Price) # Average Price is $20,707
# with this low average and large range we can infer there are a lot of outliers
# lets see what brands will have those outliers.
# i have also added a dotted line representing the mean of the prices
PricebyMake <- ggplot(carvana, aes(x = Price, y = Make, color = Make)) + 
  geom_point() +
  theme(legend.position = "none") + 
  geom_vline(xintercept = mean(carvana$Price), color = "black", linetype = 3) + ggtitle("Prices by Make")
# This graph is not the best way to display the prices becuase it looks like the average price should be way higher than $20,707
PricebyMake

PricebyMakeBOX <- ggplot(carvana, aes(x = Price, y = Make, color = Make)) + 
  geom_boxplot() + 
  theme(legend.position = "none") +
  geom_vline(xintercept = mean(carvana$Price), color = "black", linetype = 3) + ggtitle("Price by Make (Box Plot)", subtitle = "Average price ($20,707) represented with Grey dashed line")
PricebyMakeBOX
# This graph shows the average price per each make and shows outliers a little better to show that 
# makes like Toyota has a low average but have a few higher priced vehicles
# another interesting look is at the make Nissan the vehicles in the box for their line is mostly below but if we recall
# the Count of each make Nissan had a large number of vehicles in the set. 
# Others that have the box close to the vertical average line with large population of cars are Toyota, VW, Chevrolet, Honda, Hyundai
# I feel that this is why our Average Price is so low.

# Next we will look at the mileage
min(carvana$Miles)
max(carvana$Miles)
#This range is also pretty large from 53 miles all the way to 120,167 miles
mean(carvana$Miles)
# Average mileage is 54,445.7 miles
# This average seems to be more in the middle but lets visualize it
MilesVis <- ggplot(carvana, aes(x = Miles)) + geom_histogram(fill = "orange", col = "white") + ggtitle("Histogram of Milage")
MilesVis

############################### Specify what cars I would use so I can separate and predict prices on more specific models ########################
############################## Pick a popular model that I would realisticly buy in the real world.################
# in the dplyr package we use arrange and group_by to count the number of car models in arrange we use -n because it shows
# the models with the highest counts first becuase we want the most popular models
ModelTibble <- carvana %>% 
  group_by(Model) %>% 
  count()  %>% arrange(-n)
# make the tibble elements into their own variables to make a new dataframe of Popular models. I picked the top 25
n <- ModelTibble$n[1:25]
Model <- ModelTibble$Model[1:25]
PopularModel <- data.frame(Model, n)
PopularModel

PopmodelGraph <- ggplot(PopularModel, aes(x = n, y = Model, color = Model, fill = Model)) + 
  geom_col() + 
  xlab("Inventory count of Model") + 
  theme(legend.position = "none") + ggtitle("Popular Models")

PopmodelGraph
### In the graph we can see that the corolla is the most popular model in the set. 
## usually with cars if its popular its for a good reason. 
## A Toyota corolla would be a safe bet for my next purchase. So lets look into Toyota Corolla's 

#We will filter out all of the Toyota corolla's in the Carvana data frame
# and create a new Corolladf
Corolladf <- filter(carvana, Make == "Toyota", Model == "Corolla")

# Making sure it looks correct

head(Corolladf)
# Reorganized the dataframe so it shows older corollas first (2009) and increases in year
Corolladf <- Corolladf[order(Corolladf$Year),]
rownames(Corolladf) <- NULL
head(Corolladf)

#Averages for each category outside of make and model. 

mean(Corolladf$Miles) # 49,703.03 miles
mean(Corolladf$Price) # $18,333.16

# Ranges for each variable are as follows
min(Corolladf$Miles) # 1,584 miles
max(Corolladf$Miles) # 104,984 miles

min(Corolladf$Price) # $12,990
max(Corolladf$Price) # $27,990

min(Corolladf$Year) # Model year 2009
max(Corolladf$Year) # Model year 2022

# Median year is 2015
median(Corolladf$Year) # 2015


# this shows the relation ship between price and miles (x and y respectivley)
# The higher the price the lower the miles and when we go towards a lower price the miles go up. 
# This is pretty typical when shopping for cars
# Also I highlighted what each car Model year is and its also 
# Pretty typical with the newwer models being more expensive and lower miles
# and older models are on the lower price and higher miles.

Scatter_Price_Miles <- ggplot(Corolladf, aes(y = Price, x = Miles, color = Year)) + 
  geom_point() +
  ggtitle("Toyota Corolla Prices by Mile", subtitle = "Via Carvana") + 
  xlab("Miles") 
Scatter_Price_Miles
# We can see there is a linear relationship between Price Miles and Year. 
# The Lower the miles the higher the price and that is where a lot of the newer models sit. 
# On the other hand, The older models have higher miles and lower prices. 
# 


## Modelling
#### Linear Modeling
Year <- as.factor(Corolladf$Year)
# using lm() we will make 2 models one with just price and miles 
# and a second one that adds the factor year to its linear model.
Model1 <- lm(Price ~ Miles, data = Corolladf) 
Model2 <- lm(Price ~ Miles + Year, data = Corolladf)
summary(Model1)
summary(Model2)

#Create two new dataframes from each model
model1_df <- data.frame(price_prediction = predict(Model1, Corolladf), Miles = Corolladf$Miles, Year = Year)
head(model1_df)
# Plot model 1 dataframe in comparison to the Corolladf
Model1_plot <- ggplot(Corolladf,aes(y = Price, x = Miles, color = Year)) + 
  geom_point() +
  geom_line(color = "red", data = model1_df, aes(y = price_prediction, x = Miles, linetype = "Predicted Price")) +
  ggtitle("Predicted Price by miles - Model 1", subtitle = "Over actual Carvana Price")
Model1_plot # Singular regression
# with only the two variables we can see the linear regression of the lower the miles to higher the price. 
# The negative slope shows the greater the miles the lower the price.

model2_df <- data.frame(price_prediction = predict(Model2, Corolladf), Miles = Corolladf$Miles, Year = Year)
head(model2_df)
model2_plot <- ggplot(Corolladf,aes(y = Price, x = Miles, color = Year)) + 
  geom_point() +
  geom_line(color = "red", data = model2_df, aes(y = price_prediction, x = Miles, linetype = "Predicted Price")) +
  ggtitle("Predicted Price by miles - Model 2", subtitle = "Over actual Carvana Price")
model2_plot # Multiple Regression
# Model 2 adds the variable Year. I changed it to a factor for easier use.
# This one shows more specifically where the prices go. There is a lot more movement in the Predicted Price but overall it 
# still has that negative trend for the more miles the lower the price. 

## So If I wanted to get a cheaper Corolla I would need to look at higher mileage models. 
## But if mileage needed to be lower I would expect to pay more for a newer one. 




