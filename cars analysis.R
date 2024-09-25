# Problem 2 - Linear Regression 

# a. rename the columns of the data ("cars.csv")

cars <- read.csv("cars.csv")

#rename the column to more reasonable length 
colnames(cars) <- c("height", "length", "width", "wheeldrive", "cylinder", "hybrid",                      "gears", "transmission_full", "city_mpg", "fuel", "highway_mpg",                      "transmission_class", "id", "maker", "model_year", "year", "hp",                      "torque")    
colnames(cars) 
nrow(cars)

#b.Restrict the data to cars whose Fuel Type is “Gasoline”.

cars <- cars[cars$fuel == "Gasoline", ] # restrict the data to cars with "Gasoline"

#c.Examine the distribution of highway gas mileage

attach(cars) 

# plot between MPG on highway and Torque
plot(torque, highway_mpg, main = "The Relationship between MPG on highways and Torque", xlab = "Torque", ylab = "MPG on highways")

# Check the number of outlier whose highway_mpg is greater than 50
sum(highway_mpg > 50)

# Because there is only one car whose highway_mpg is greater than 50 as 223 MPG, restric the data to cars whose highway_mpg is leass than 50. 

plot(torque[highway_mpg <= 50], highway_mpg[highway_mpg <= 50], main = "The Relationship between MPG on highways and Torque", xlab = "Torque", ylab = "MPG on highways")

# The relationship between MPG on highways and Torque is not linear. The MPG decreases exponentially with Torque. Therefore, a log transformation of Torque is helpful.

plot(log(torque[highway_mpg <= 50]), highway_mpg[highway_mpg <= 50], main = "The Relationship between MPG on highway and Torque", xlab = "log(Torque)", ylab = "MPG on highway")

cars <- cars[cars$highway_mpg <= 50, ] # restrict the data without the outlier
cars$log_torque <- log(cars$torque) # add the transformation variable

# d. Fit a linear regression model predicting MPG on the highway
# There is negative linear relationshp between log(torque) and highway MPG
# The estimated coefficients on torque for each year are as follows: 2009 = -13.88094, 2010 = -16.65208, 2011 = -16.67382, and 2012 = -18.52437.

cars$year <- as.factor(cars$year) # transform year into a categorical variable
table(cars$year)

mod <- lm(highway_mpg~log_torque*year+height+length+width+hp, data = cars)
summary(mod)

c("2009" = mod$coef[2], "2010" = mod$coef[2] + mod$coef[10], "2011" = mod$coef[2] + mod$coef[11], "2012" = mod$coef[2] + mod$coef[12])


