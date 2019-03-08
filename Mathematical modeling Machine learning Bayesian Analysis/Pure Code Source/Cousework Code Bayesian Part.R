#---- ## first task ## Bayesian Statistics Task --------------------
library(ggplot2)
library(readr)
library(scales)
setwd <- "C:/Users/Charis/Desktop/Courseworks Assignements Second Semester/MATH501/MATH501 Coursework-20180221"
shop_data <- read_csv("C:/Users/Charis/Desktop/Courseworks Assignements Second Semester/MATH501/MATH501 Coursework-20180221/Shops_Data.csv")
shop_data
names(shop_data)
attach(shop_data)

library(dplyr)
library(tidyr)

# ----Bayesian Statistics Part (a),(b)-----------######

# Build long data with gather from tidyr library to prepare
# the scater plot.

shop_data_long <- gather(shop_data,"Type" , "x" ,2:3)
shop_data_long

# Here we represent the data in a scater plot and and fit the  
# linear model line
ggplot(shop_data_long, aes(x = x, y = Sales, col = Type))+
  geom_point()+
  facet_grid(~Type, scales = "free")+
  geom_smooth(method = "lm",se = FALSE )+
  theme(legend.position = "bottom")+
  labs(y = "Sales (in £'000s)",x = "", col = "Type of Sales")

# Build 3D plot with rgl to have better visualazation
# of the plot
library(rgl)

#Fit the linear model in order to find the coef and build the 3D plane
m = lm(Sales ~ Advertising + Population, data = shop_data)


coef(m)
beta_0 <- m$coefficients[1]
beta_1 <- m$coefficients[2]
beta_2 <- m$coefficients[3]

plot3d(Advertising, Population, Sales, 
       type = "s", 
       col = "red", 
       size = 2) 
#
# Show the plane y = beta_0 + beta_1 x1 + beta_2 x2
# This can be written as beta_1 x1 + beta_2 x2 - y + beta_0 = 0
#
planes3d(beta_1, beta_2, -1, beta_0 , alpha = 0.5)
#

library(modelr)
#
df_with_fitted_values <- shop_data %>%
  add_predictions(m)

df_with_fitted_values_long <- gather(df_with_fitted_values,"Type" , "x" ,2:3)


ggplot(df_with_fitted_values_long,aes(x = x  ,y = Sales,col = Type))+
  geom_point()+
  facet_grid(~Type, scales = "free")+
  geom_smooth(method = "lm",se = FALSE )+
  geom_segment(aes(x = x , y = Sales, xend = x, yend = pred), col = "red")+
  labs(x = "City",y = "Sales (thousands)",col = "Types")

############################################################################################################
#---- Bayesian Statistics Part (c)#####

# We fit the model in the frequentist framework using the lm function
# The parameters beta_0, beta_1 and sigma^2 can be estimated by
# maximizing this likelihood
# R's function lm finds the maximum likelihood estimates of beta_0 and beta_1 (amongst other things)
# The estimate of sigma that is reported is, in fact, not a maximum likelihood estimate
# The details are not important
m <- lm(Sales ~ Advertising + Population, data = shop_data)

# Estimates of the parameters beta_0 and beta_1, with p-values
#
summary(m)
#
# Confidence intervals for beta_0 and beta_1
#
confint(m)
#
# Estimate of sigma (not maximum likelihood, in fact; an unbiased estimator is used)
#
summary(m)$sigma
#
# The p-value associate with the F-statistics allows us to test
# H_0: beta_1 = beta_2 = 0 (model is not useful) against
# H_1: not H_0 (model is useful)
#
# The values under Pr(>|t|) allow us to test
# H_0: beta_0 = 0 against H_1: beta_0 is not 0
# H_0: beta_1 = 0 against H_1: beta_1 is not 0
# H_0: beta_2 = 0 against H_1: beta_2 is not 0
#
# Recall that in general for a test at the 5% level of significance,
# we'd reject H_0 in favour of H_1,
# if the p-value < 0.05
#
#
# What do you conclude about how sales depends on avertising and on population?
#


############################################################################################################
#---- Bayesian Statistics Part (d) ############

Bayesian_regression_model <- function(){
  #
  # Data model or likelihood part
  #
  for(i in 1:n){ # n is the sample size (number of data points)
    #
    y[i] ~ dnorm(mu[i], tau) # Parametrized by the precision tau = 1 / sigma^2
    mu[i] = beta_0 + beta_1 * x1[i] + beta_2 * x2[i]
    #
  }
  #
  # Priors
  #
  beta_0 ~ dnorm(0.0, 1.0E-4) # Prior on beta_0 is normal with low precision
  beta_1 ~ dnorm(0.0, 1.0E-4) # Prior on beta_1 is normal with low precision
  beta_2 ~ dnorm(0.0, 1.0E-4) # Prior on beta_2 is normal with low precision
  tau ~ dgamma(1.0E-3, 1.0E-3) # Prior on tau is gamma with small shape and rate parameters
  #
  # Definition of sigma: it's completely determied by tau
  #
  sigma <- 1.0 / sqrt(tau)
}

# Load R2jags
#
library(R2jags)
citation("R2jags") # Always give credit

# Prepare the variable to input to the model
n<- 10
y <- shop_data$Sales
x1 <- shop_data$Advertising
x2 <- shop_data$Population

data_Regression <- list("x1","x2","y","n")

Bayesian_Regression <- jags(data = data_Regression, 
                            parameters.to.save = c("beta_0", 
                                                   "beta_1",
                                                   "beta_2",
                                                   "tau", 
                                                   "sigma"), 
                            n.iter = 1000000, # (Related to) size of sample required from the posterior
                            n.chains = 6, # Here, we repeat our sampling from the posterior three times
                            # The Gibbs sampler algorithm is iterative and
                            # requires a starting point.  By default, jags specifies the starting point for you.
                            # Here, jags runs the algorith three times, from three different starting points.
                            model.file = Bayesian_regression_model)

print(Bayesian_Regression, intervals = c(0.025, 0.5, 0.975))


plot(Bayesian_Regression)

# The fitnets of how bad is the model is not requires because we dont have 
# another moder to compare
Bayesian_Regression$BUGSoutput$DIC

# Transoform the to mcmc file to have better visualization
Bayesian_Regression.mcmc <- as.mcmc(Bayesian_Regression)


#
# Create a ggs object
#
library(ggmcmc) # You may need to install this package on your own machine
#
Bayesian_Regression.ggs <- ggs(Bayesian_Regression.mcmc)
#
# Traceplots
#
ggs_traceplot(Bayesian_Regression.ggs) 


ggs_density(Bayesian_Regression.ggs)

ggs_density(Bayesian_Regression.ggs, family = "^beta")

############################################################################################################
#---- Bayesian Statistics Part (e) ------
#' Question: Include a graphical presentation of the posterior distributions
#' of b0, b1 and b2 in your report and very briefly discuss these posterior
#'  distributions.
#
# Create a ggs object
#
library(ggmcmc) # You may need to install this package on your own machine
#
Bayesian_Regression.ggs <- ggs(Bayesian_Regression.mcmc)
#
# Traceplots
#
ggs_traceplot(Bayesian_Regression.ggs) 


ggs_density(Bayesian_Regression.ggs)

#' This is required for the second part and briefly talk about 
#' posteriors
ggs_density(Bayesian_Regression.ggs, family = "^beta")



############################################################################################################
#---- Bayesian Statistics Part (f) ------
#' Question: Include a graphical presentation and the numerical values
#' of a 95% credible interval for b1 in your report and briefly comment on it.Very briefly
#' compare this credible interval with the 95% confidence interval found above.

# grafical presentation of the b1
ggs_caterpillar(Bayesian_Regression.ggs,thick_ci = c(0.05, 0.95),family = "^beta_1")

# numeric representation
say <- print(Bayesian_Regression, intervals = c(0.025, 0.5, 0.95))

say$summary # all the summary table
say$summary[2,] # only the beta_1



############################################################################################################
#---- Bayesian Statistics Part (g) ------
#' The overall manager of the chain of shops is thinking of opening another shop in a similar
#' city. The city has a population of 200,000 people and the manager is proposing spending
#' £30,000 on advertising in order to make a good start.
#' Bayesian Statistics Part 
#' 
#' Question: Modify your BUGS/jags code to produce a credible
#' interval for the mean sale value and a Bayesian prediction interval for the sales that may be
#' achieved by a shop in a city of 200,000 people when £30,000 is spent on adversiting. Report
#' these intervals and provide a critical discussion about them for the overall manager

Bayesian_regression_model_with_prediction <- function(){
  #
  # Data model or likelihood part
  #
  for(i in 1:n){ # n is the sample size (number of data points)
    #
    y[i] ~ dnorm(mu[i], tau) # Parametrized by the precision tau = 1 / sigma^2
    mu[i] = beta_0 + beta_1 * x1[i] + beta_2 * x2[i]
    #
  }
  #
  # Priors
  #
  beta_0 ~ dnorm(0.0, 1.0E-4) # Prior on beta_0 is normal with low precision
  beta_1 ~ dnorm(0.0, 1.0E-4) # Prior on beta_1 is normal with low precision
  beta_2 ~ dnorm(0.0, 1.0E-4) # Prior on beta_2 is normal with low precision
  tau ~ dgamma(1.0E-3, 1.0E-3) # Prior on tau is gamma with small shape and rate parameters
  #
  # Definition of sigma: it's completely determied by tau
  #
  sigma <- 1.0 / sqrt(tau)
  #
  # We add x new to give us the power to make the prediction
  #
  mu_new <- beta_0 + beta_1 * x1_new + beta_2 * x2_new # Value of mu at x_new
  #
  y_new ~ dnorm(mu_new, tau) # New value of y at x_new
  #
}

#
# # Prepare the variable to input to the model x1_new and x2_new
#
n<- 10
y <- shop_data$Sales
x1 <- shop_data$Advertising
x2 <- shop_data$Population
x1_new <- 200
x2_new <- 30
#
data_Regression_predict <- list("x1","x2","y","n", "x1_new","x2_new")
#
# Perform Bayesian inference
#
Bayesian_Regression_predict <- jags(data = data_Regression_predict, 
                                    parameters.to.save = c("beta_0", "beta_1","beta_2","tau", "sigma","mu_new","y_new"), 
                                    n.iter = 100000, 
                                    n.chains = 3,
                                    model.file = Bayesian_regression_model_with_prediction)




# The fitnets of how bad is the model is not requires because we dont have 
# another moder to compare
Bayesian_Regression_predict$BUGSoutput$DIC

# Transoform the to mcmc file to have better visualization
Bayesian_Regression_predict.mcmc <- as.mcmc(Bayesian_Regression_predict)


#
# Create a ggs object
#
library(ggmcmc) # You may need to install this package on your own machine
#
Bayesian_Regression_predict.ggs <- ggs(Bayesian_Regression_predict.mcmc)
#
# Traceplots
#
ggs_traceplot(Bayesian_Regression_predict.ggs) 


ggs_density(Bayesian_Regression_predict.ggs)

ggs_density(Bayesian_Regression_predict.ggs, family = "^y_new")
# This gives the credible interval for y_new which is the sales spread and 
# mu_new which is the mean average
library(gridExtra)

gg_caterpillar_mu <- ggs_caterpillar(Bayesian_Regression_predict.ggs,family = "^mu_new")

gg_caterpillar_ynew <- ggs_caterpillar(Bayesian_Regression_predict.ggs,family = "^y_new")

grid.arrange(gg_caterpillar_mu , gg_caterpillar_ynew, nrow = 2)

#
m <- lm(Sales ~ Advertising + Population, data = shop_data)
#
# Estimate beta_0 and beta_1
#
coef(m)
#
# 95% confidence intervals for beta_0 and beta_1
#
confint(m)
#
# Estimate and 95% confidence interval for mu_new
#
predict(m, newdata = data.frame(Advertising = x1_new,Population = x2_new), interval = "confidence")
#
# Estimate and 95% prediction interval for y_new
#
predict(m, newdata = data.frame(Advertising = x1_new,Population = x2_new), interval = "confidence")
#
#
# Here are the above Bayesian results again:
#
print(Bayesian_Regression_predict, intervals = c(0.025, 0.5, 0.95))

#
# Accessing the jags output for future use-------------------------------------------
#
# *** The printed output ***
#
Bayesian_Regression_predict
#
# Accessing a part of it
#
Bayesian_Regression_predict$BUGSoutput$summary[c("beta_0", "beta_1"), # Rows 
                                               c("2.5%", "97.5%")] # Columns
#
# *** Sampled values ***
#
# The sampled values can be most easily accesses using the mcmc form of the output
#
# Here we extract the sampled values of beta_0 from the first chain
#
Bayesian_Regression_predict.mcmc[[1]][,"beta_0"]
#
# We can obtain the *** narrowest credible interval ***
# as shown in the caterpillar plots
# The narrowest credible interval is referred to as a *** highest posterior density (HPD) interval ***
# This is because the narrowest interval must be associated with the highest posterior density
# to maintain the 90% or 95% area
#
# Let's obtain the 90% and 95% HPDs for beta_0:
#
HPDinterval(Bayesian_Regression_predict.mcmc[[1]][,"y_new"], prob = 0.9)
HPDinterval(Bayesian_Regression_predict.mcmc[[1]][,"y_new"], prob = 0.95)
#
# Compare with the cruder summaries
#
print(Bayesian_Regression_predict, intervals = c(0.025, 0.05, 0.5, 0.95, 0.975))
#
# Compare the widths of the 95% intervals
#
diff(HPDinterval(Bayesian_Regression_predict.mcmc[[1]][,"y_new"], prob = 0.95)[1:2])
diff(Bayesian_Regression_predict$BUGSoutput$summary["y_new", c("2.5%", "97.5%")])
# The cruder interval is wider!
#
############################################################################################################
#---- ## Second task ## Bayesian Statistics Part (h) ------
#'
#'Some years ago a survey was conducted about the economy. Twenty people were interviewed
#'in Plymouth city centre and asked whether they thought the UK economy would be stronger
#'at the end of the year than it was at the start. The responses are given in the table below
#'together with the age of each respondent:
#'
# Insert the data from the Brief
responce <- c("Yes","Yes","No","No","No","Yes","No",
              "Yes","Yes","Yes","Yes","Yes","No","No",
              "No","Yes","Yes","Yes","Yes","No")
age <- c(38,42,56,68,40,41,74,37,50,40,33,42,54,
         48,60,35,37,43,44,65)


# Create a numeric vector containing the 0 if responce is NO and 
# 1 if responce is Yes
responce_prob <- rep(1,20)
for(i in 1:20) if(responce[i] == "No")responce_prob[i]<-0

# Create a data frame for the data 
survey <- data.frame(responce,age,responce_prob)
survey

responce_prob[i] ~ dbin(p[i], 1)





############################################################################################################
#---- Bayesian Statistics Part (i) ------
# BAGS Code for the model
Bayesian_binary_logistic_model <- function(){
  #
  # Define the likelihood part of the model
  #
  for(i in 1:n_obs){
    #
    y[i] ~ dbin(p[i], n[i]) 
    #
    # Link function
    #
    # logit(p) in BUGS give log(p / (1 - p))
    #
    # Linear predictor
    #
    logit(p[i]) <- eta[i]
    #
    eta[i] <- beta_0 + beta_1 * x[i]
  }
  #
  # Specify the prior distributions on the unknown parameters
  #
  beta_0 ~ dnorm(0.0, 1.0E-4) # Prior on beta_0 is normal with low precision
  beta_1 ~ dnorm(0.0, 1.0E-4) # Prior on beta_1 is normal with low precision
  #
  # Evaluate the linear predictor at the new value of Dose, called x_new
  #
  eta_new <- beta_0 + beta_1 * x_new
  #
  # Convert to the probability scale
  #
  p_new <- exp(eta_new) / (1 + exp(eta_new))
}

############################################################################################################
#---- Bayesian Statistics Part (j) ------
# we include the function beta_inits in order to give the initial values for the 
# slope and intecept
beta_inits <- function(){
  list("beta_0"=0.0001,"beta_1"=0.0001)
}
# *** Data Preparation ***
#
n_obs <- length(survey$age)
#
y <- survey$responce_prob

#
x <- survey$age

#
x_new <- c(18,50)

data_binary_logistic2 <- list("n_obs", "y", "x", "x_new")
#

Bayesian_binary_logistic_predict <- jags(data = data_binary_logistic2, 
                                         parameters.to.save = c("beta_0", 
                                                                "beta_1", 
                                                                "p", 
                                                                "p_new"), 
                                         n.iter = 100000, 
                                         n.chains = 3,
                                         model.file = Bayesian_binary_logistic_model_predict)


# Convert the jags output to an MCMC object
#
Bayesian_binary_logistic_predict.mcmc <- as.mcmc(Bayesian_binary_logistic_predict)
#
# Convert this to a ggs object
#
Bayesian_binary_logistic_predict.ggs <- ggs(Bayesian_binary_logistic_predict.mcmc)
#
# Inference about beta_0
# pi(beta_0 | data)
#

ggs_caterpillar(Bayesian_binary_logistic_predict.ggs, family = "^p_new")
Bayesian_binary_logistic_predict$BUGSoutput$summary[c("p_new[1]","p_new[2]"),
                                                    c("mean","sd","2.5%","97.5%")]
#
#
Bayesian_binary_logistic_predict_xnew.mcmc <- as.mcmc(Bayesian_binary_logistic_predict_xnew)


############################################################################################################
#---- Bayesian Statistics Part (k) ------

#
# Convert this to a ggs object
#
#
Bayesian_binary_logistic_predict_xnew.ggs <- ggs(Bayesian_binary_logistic_predict_xnew.mcmc)

ggs_caterpillar(Bayesian_binary_logistic_predict_xnew.ggs,thick_ci = c(0.05, 0.95),family = "^x_new")


############################################################################################################