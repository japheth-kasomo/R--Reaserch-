death_rates <- read.csv("C:/Users/Japheth/Desktop/Regression Analysis/japheth_kasomo_RA1/death_rates.csv")
View(death_rates)



attach(death_rates) #extracting columns from the dataset


x  <- Age
y  <- DeathRate
n <- length(x)
n


#Question 1 Descriptive  analysis of the variables 

#general code for descriptive analyais 
summary(death_rates)

x_bar     <-  sum(x)/n    #mean  of Age
x_bar
y_bar  <-  sum(y)/n       #mean of Death death rate
y_bar



var_Age  <- sum((y-y_bar)^2)/(n-1)   # variance of Age 
var_Age

var_death  <- sum((y-y_bar)^2)/(n-1)   #variance of Death rate 
var_death

s_d_Age  <- sqrt(var_age)              # standard deviation of Age
s_d_Age

s_d_death  <- sqrt(var_death)          # standard deviation of death rate
s_d_death


#cov_Age_death  <-  sum((y-y_bar)*(x-x_bar))/(n-1)
#cov_Age_death

#corr_Age_death  <- cov_Age_death/sqrt((var_age)*(var_death))
#corr_Age_death



#General plots

hist(Age,col = ("green"))
hist(DeathRate,col = ("green"))
boxplot(death_rates[,2:3])
plot(Age,DeathRate, main = "The graph of Age against Death Rate", col="blue",xlab = "Age ", ylab = "Death rate" )



#Question 2

#Separating data for males and females

data_male     <-  death_rates[1:31,]
data_female   <-  death_rates[32:62,]


#ASsociativity for males

summary(data_male)

x_a <-  data_male$Age
y_d <-  data_male$DeathRate


mean(x_a)

var_a   <-  sum((x_a-mean(x_a))^2)/(length(x_a)-1)   # variance of Age of males 
var_a

var_d   <-  sum((y_d-mean(y_d))^2)/(length(y_d)-1)  #variance of death of males
var_d

cov_males  <- sum((x_a-mean(x_a))*(y_d-mean(y_d)))/((length(x_a)-1))
cov_males  

cor_males  <- (cov_males)/(sqrt((var_d)*(var_a)))
cor_males

plot(data_male$Age,male$DeathRate, main = ("Age versus death rate for males only"), col=("blue"), xlab = ("Age of males"),ylab=("Death rate of Males"))


#Associativity of females
summary(data_female)

X_fa <- data_female$Age        # renaming the data for females

y_fd <- data_female$DeathRate
y_fd

y_fd_bar <- sum(y_fd)/(length(y_fd)) # mean of death rate for females
y_fd_bar

X_fa_bar <- sum(X_fa)/ length(X_fa)
X_fa_bar

var_fa  <-  sum((X_fa-mean(X_fa))^2) /(length(X_fa)-1)  # variance of age of females
var_fa
var_fd  <- sum((y_fd-mean(y_fd))^2)/(length(y_fd)-1)     # variance of death rate for females
var_fd

cov_females <- sum((X_fa-mean(X_fa))*(y_fd-mean(y_fd)))/(length(y_fd)-1)
cov_females

cor_females  <- (cov_females)/(sqrt((var_fa)*(var_fd)))
cor_females


plot(data_female$Age,data_female$DeathRate, main = ("Age versus death rate for females only"), col=("blue"), xlab = ("Age of females"),ylab=("Death rate of females"))



#question 3  simple regression model for females

beta_1_hat  <- sum((y_fd)*(X_fa-mean(X_fa)))/(sum((X_fa-mean(X_fa))^2))
beta_1_hat

beta_0_hat <- y_fd_bar - (beta_1_hat*X_fa_bar)  
beta_0_hat

y_hat <- beta_0_hat + beta_1_hat * X_fa   #a simple linear regression model  
y_hat





#Question 4

# Using the model to predict death rate for femeles at aged 51

y_hat <- beta_0_hat + beta_1_hat * 51   #using the model to predict  
y_hat


# Question 5


# using  residuals to check if the model quality of model

errors <- y_fd - y_hat    # getting residuals which is give by difference between response variable and predicted variable 
errors
plot(errors,y_hat, xlab=("Predicted"),ylab=("Residuals"), main = ("Residuals versus Fitted(Predicted)"))


