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
# The estimated coefficient for log_torque is -17.09. This means that a car with a log_torque that is 1 unit higher is expected to have a highway MPG that is 17.09 units lower than another car with the same height, length, width, horsepower and year, but a different log_torque.

sum(cars$height == 0) # the number of unknown height values
sum(cars$length == 0) # the number of unknown length values
sum(cars$width == 0) # the number of unknown width values


cars$year <- as.factor(cars$year) # transform year into a categorical variable
table(cars$year)

# linear regression model predicting MPG on the highway
mod <- lm(highway_mpg~log_torque+height+length+width+hp+year, data = cars)
summary(mod)

#mod1 <- lm(highway_mpg~log_torque*year+height+length+width+hp, data = cars)
#summary(mod1)
#anova(mod,mod1)
#c("2009" = mod1$coef[2], "2010" = mod1$coef[2] + mod1$coef[10], "2011" = mod1$coef[2] + mod1$coef[11], "2012" = mod1$coef[2] + mod1$coef[12])


# e. Refit the model with an interaction terms and generate a plot,

# linear regression model with an interation between torque and horsepower
mod1 <- lm(highway_mpg~log_torque*hp+height+length+width+year, data = cars)
summary(mod1)

table(year) # the number of 2011 is the largest among years. 
summary(cars$hp) # the distribution of horsepower (the mean of horsepower : 267.5)
sd(cars$hp) # the standard error of horsepwoer (97.43795)
summary(cars$log_torque) # the distribution of log_torque (min : 4.585, max: 6.652)
hist(cars$log_torque) # the histogram of log_torque

library(interactions)
# interaction plot for the year 2011
interact_plot(mod1, pred = log_torque, modx = hp, at = list(year = "2011")) 

# The highway MPG decreases less with larger log_torque as the horsepower increases, for log_torque values between 4.585 and 6.652. 

# f. Calculate beta_hat from d. manually

xmat <- model.matrix(mod, data = cars) # design matrix 
beta_hat <- solve(t(xmat)%*%xmat)%*%t(xmat)%*%cars$highway_mpg # matrix algebra
beta_hat # beta_hat result
mod # result from lm function of d. 
beta_hat - mod$coeff # the coefficients are same
