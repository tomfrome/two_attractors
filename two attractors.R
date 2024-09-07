library(MASS)
library(ggplot2)
library(jmuOutlier)

n = 200
s = sample(1:10,n,replace=TRUE)
v = sample(1:10,n,replace=TRUE)
c = sample(1:10,n,replace=TRUE)

data = data.frame(cbind(c,v,s)) 

updater = function(i,data){
  n = dim(data)[1]
  datum = data
  for (k in 1:i){
    option = sample(1:3,1)
    if (option==1){
      firms = sample(1:n,2,replace=F)
      firm1 = firms[1]
      firm2 = firms[2]
      rate = (datum$s[firm1]+datum$s[firm2])/(datum$c[firm1]+datum$v[firm1]+datum$c[firm2]+datum$v[firm2])
      datum$s[firm1] = rate*(datum$c[firm1]+datum$v[firm1])
      datum$s[firm2] = rate*(datum$c[firm2]+datum$v[firm2])
    } else if (option==2){
      firm = sample(1:n,1)
      datum$c[firm] = datum$c[firm]*.9
      firm = sample(1:n,1)
      datum$v[firm] = datum$v[firm]*.9
      firm = sample(1:n,1)
      datum$s[firm] = datum$s[firm]*.9
    } else if (option==3){
      firms = sample(1:n,2,replace=F)
      firm1 = firms[1]
      firm2 = firms[2]
      rate = (datum$v[firm1]+datum$v[firm2])/(datum$v[firm1]+datum$v[firm2]+datum$s[firm1]+datum$s[firm2])
      datum$v[firm1] = rate*(datum$v[firm1]+datum$s[firm1])
      datum$v[firm2] = rate*(datum$v[firm2]+datum$s[firm2])
      datum$s[firm1] = (1-rate)*(datum$v[firm1]+datum$s[firm1])
      datum$s[firm2] = (1-rate)*(datum$v[firm2]+datum$s[firm2])
    }
  }
  datum$prices = datum$s+datum$c+datum$v
  datum$surplus_value = datum$s/datum$v
  datum$profit = datum$s/(datum$c+datum$v)
  datum$wage_share = 1/(datum$surplus_value+1)
  datum$OCC = datum$c/datum$v
  return(datum)
}

datum = updater(10000,data)

View(datum)

#set below to profit or surplus value or whatever
var_of_interest = datum$profit

#simple histogram
hist(var_of_interest,breaks=25)
abline(v=mean(var_of_interest))

#check for lognormal fit
fit_params = fitdistr(var_of_interest,"lognormal")
quants = seq(0,1,length=81)[2:80]
fit_quants = qlnorm(quants,fit_params$estimate['meanlog'], fit_params$estimate['sdlog'])
data_quants = quantile(var_of_interest,quants)
plot(fit_quants, data_quants, xlab="Theoretical Quantiles", ylab="Sample Quantiles")
title(main = "Q-Q plot of lognormal fit against data")
abline(0,1)
df = data.frame(x = var_of_interest)
ggplot(df, aes(x)) +
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(
    fun = dlnorm, 
    args = list(meanlog=fit_params$estimate['meanlog'], sdlog = fit_params$estimate['sdlog']), 
    lwd = 1, 
    col = "green"
  )


#check for laplace fit
dlaplace <- function(x, mu=0, b=1){
  if(b<=0) return(NA)
  exp(-abs(x-mu)/b) / (2*b)
}
m = median(var_of_interest)
t = mean(abs(var_of_interest-m))
quants = seq(0,1,length=81)[2:80]
fit_quants = qlaplace(quants, mean = m, sd = t)
data_quants = quantile(var_of_interest,quants)
plot(fit_quants, data_quants, xlab="Theoretical Quantiles", ylab="Sample Quantiles")
title(main = "Q-Q plot of Laplace fit against data")
abline(0,1)
df = data.frame(x = var_of_interest)
ggplot(df, aes(x)) +
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(
    fun = dlaplace, 
    args = list(mu = m, b = t), 
    lwd = 1, 
    col = "green"
  )

#visualizations of cockshott's first two correlations
plot(datum$OCC,datum$profit)
cor(datum$OCC,datum$profit)

plot(datum$OCC,datum$surplus_value)
cor(datum$OCC,datum$surplus_value)

