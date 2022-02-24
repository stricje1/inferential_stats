---
title: "The Central Limit Theorem"
author: "Jeffrey Strickland"
date: "1/11/2022"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE, fig.width=6.5, fig.height=4, scipen = 1000000)
```
## Overview
In this analysis, we compare the distribution of 1000 random samples from an exponential distribution and and the distribution of averages of 40 exponentials.

This problem is an Application of the Central Limit Theorem (CLT). Simply, the CLT states that when a sufficient number of independent random variables are summed or averaged, their result forms a distribution of sums or averages that are approximately normally distributed. Hence, the 1000 averages of 40 random uniform variates appear in the shape of a normal distribution (40 is considred adequate by most authors for application of the CLT).

## Simulations
### The Exponential Distribution
While samples have statistics, like the sample mean, population distributions have parameters, like the population mean. The exponential(lambda) distribution is defined by one parameters lambda, the rate. For our distribution, we take lambda to equal 0.2, with the mean being mu = 1/lambda = 5 and standard deviation, sigma = 1/lambda = 5.

### Simulation of the Exponential(lambda) distribution
First, generate the distribution of 1000 random exponentials, using the stats-package, which provides the random uniform distribution function, rexp. To see the documentation of the stats-package, we use the command, library(help = 'stats').

```{r}
library(help = "stats") # Opens the stats-package documentation in the Editor window/view.
library("stats")
```

### Set up the Simulation
The documentation for the random uniform random variate provides the functions:
* rexp generates random deviates
* n_sim the number of simulations
* lambda is the rate of growth or decay
* 1/lambda is the population mean (parameter)

Below, we generate 1000 random uniform distributions, calculate the sample means and plot a histogram. First, we construct the histogram.

```{r}
n_sim = 1000
lambda = 0.2
sim_exp = rexp(n_sim,lambda)
hist(sim_exp, col="powderblue")
```
Now, we calculate the sample mean.

```{r}
cat("The sample mean is ", mean(sim_exp))  
```

## Aplication of the CLT
Now, we take our exponential distributions, and using their sample means, we generate as distribution of 40 of those means.

### The Normal Distribution, Normal(mu,sigma)
The standard normal distribution is defined by two parameters, population mean = mu, the population standard deviation = sigma or Normal(mu,sigma).

### Normal Random Vaiates 
The sample MNS is approximately normal (due to the CLT) and has a sample mean near 5 and a sample standard deviation near calculated as (1/lambda)^2/n_dist. So, now we form the random distribution from the means of 40 Uniform[0,1] random variates.

```{r}
lambda=0.2
mu = 1/lambda
n_dist = 40
n_sim = 1000
mns_u = NULL
d =  data.frame(mean=numeric())
for (i in 1 : n_sim){ 
  mns_u = c(mns_u, mean(rexp(n_dist, lambda)))
  d[i,1] = mean(mns_u)
}

cat("The mean value of the exponetials is:", mean(d$mean), "which is the mean of the normal distribution.")
```
## The Mean Value
As we have seen, the mean value of the exponential distribution is 1/lambda and in our example it is equal to 5. The random variates drawn from this distribution have means near 5 and when we take form a distribution of these sample means, the new distribution also has a sample mean of 5.

```{r}
mu = 1/lambda
x_bar = mean(d$mean)
cat("The mean value, mu, of the normal distro is ", mu,"and the sample mean is ", x_bar)
```
So, we see that the sample mean of the normal distribution of exponential means is approximately 5, and the parameter mu is 5.

## The Variance 
The variance of the population parameter, sigma^2, is given by (1/lambda)^2/n_dist. Here, we calculate its value and compare to the sample variance of our normal distribution.

```{r}
samp_var = round(var(mns_u),4)
sigma_sq=(1/lambda)^2/n_dist
cat("The population variance is ", sigma_sq, "and the sample variance is ", samp_var)
```
So, we see that the sample variance approximates the population parameter, sigma^2. 

## Historgam Approximation of the Normal Distribution
Here, we construct the histogram of our normal distribution overlaid by horizonatl lines representing the sample mean and the population (theoretical) mean.

```{r}
hist(mns_u, col="dodgerblue", xlab = "rnorm(x_bar,s)", main = "Histogram of an Approximate Normal(mu,sigma) Distribution")
abline(v=mean(d$mean), col = "green", lwd=4) # Sample mean
abline(v=1/lambda, col = "red", lwd=2) # Theoretical mean
```

Looking at the histogram, the distribution appears to be approximately normal, as expected. We now calculate the population parameters and sample statistics.

```{r}
df <- data.frame(
  Metric=rep(c('mean', 'variance', 'stdev', 'minimum', 'maximum'), each=1),
                 Statistic=rep(c(
                   sprintf("%.6f",mean(mns_u)), 
                   sprintf("%.6f", var(mns_u)),
                   sprintf("%.6f",  sd(mns_u)),
                   sprintf("%.6f", min(mns_u)),
                   sprintf("%.6f", max(mns_u))), times=1),
                 Parameter=rep(c(
                   sprintf("%.6f", 1/lambda), 
                   sprintf("%.6f",(1/lambda)^2/n_dist), 
                   sprintf("%.6f", sqrt((1/lambda)^2/n_dist)), 
                   "-infinity",
                   "+infinity"), times=1))
knitr::kable(df)
```

## Boxplot and Sample Statistics
The boxplot aids us in seeing the min, max, mean, quartiles, and outliers.

```{r}
boxplot(mns_u, col="dodgerblue", horizontal=TRUE, las=2, main="Boxplot for MNS", len=2,
        boxcol="navyblue", medcol="orange", whiskcol="green4", staplecol="blue", outcol="purple")
summary(mns_u)
```

## Summary
In this analysis we investigated an application of the CLT, using the means of 40 exponential random variates, approximating a standard normal distribution. It is important to state that the only definitive way of knowing that our distribution of sample mean is approximately normal is due to the CLT, and this hold true regardless of the underlying sample distributions. Note, we could also apply this to the sums of our exponential.
