#PS13 Laura Escher Prob and Stats

#1
month_return <- c(3.60,14.86,-6.07,-10.82,4.29,3.98,3.74,6.62,5.42,-11.83,1.21,-0.94)
monthsd <- sd(month_return)
monthmean <- mean(month_return)
monthvar <- var(month_return)

print(monthvar)
print(monthsd)

#4a
halloween <-read.csv("Problem 11.6 - Halloween.csv")
View(halloween)
mean_hal <- mean(halloween$Dollars.Spent, na.rm = TRUE)
print(mean_hal)
#mean is 41

#4b
sd_hal <- sd(halloween$Dollars.Spent, na.rm = TRUE)
print(sd_hal)

#4c
halloween_clean <- na.omit(halloween$Dollars.Spent)

halconf_sd <- function(x, conf.level = 0.95)
  {
  x <- na.omit(halloween$Dollars.Spent)
  n <- length(x)          
  df <- n - 1             
  s2 <- var(x)
  
  alpha <- 1 - conf.level
  halchi2_upper <- qchisq(alpha/2, df)
  halchi2_lower <- qchisq(1 - alpha/2, df)
  
  lower_var <- (df * s2) / halchi2_lower
  upper_var <- (df * s2) / halchi2_upper
  
  lower_sd <- sqrt(lower_var)
  upper_sd <- sqrt(upper_var)
  
  return(c(lower_sd, upper_sd))
}

halconf_sd(halloween_clean)

#5a
costco <- read.csv("Problem 11.10 - Costco.csv")
View(costco)

mean_costco <- mean(costco$Satisfaction.Score, na.rm = TRUE)
print(mean_costco) #84

#5b
costco_var <- var(costco$Satisfaction.Score, na.rm = TRUE)
print(costco_var) #118.7143

#5c
costco_sd <-sqrt(costco_var)
print(costco_sd) #10.89561

#5d
n_costco <- length(na.omit(costco$Satisfaction.Score))
print(n_costco)

#Ho: σ2= 144
#H1: σ2!= 144


costco_chisqtest <- ((n_costco-1)*(costco_var)) / 144
#11.54
#Conclusion: At the 5% significance level, we fail to reject the null hypothesis.
#We do not find sufficient evidence that the population standard deviation is different from 12.