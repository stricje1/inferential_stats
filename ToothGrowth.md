---
title: "Effects of Dosage levels and Supplements on Tooth Growth"
author: "Jeffrey Strickland"
date: "1/10/2022"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE, fig.width=6, fig.height=3)
```

## 1. Introduction
The purpose of this report is to analyze elements affecting tooth growth, specifically supplement type (VC or OJ) and dose in milligrams/day. In this analysis we examine whether various combinations of supplments and dose significantly impact tooth growth. We do this by performing hypothesis test to compare make these comparisons.

## 2. Assumptions
This kind of analysis requires the assumptions of a two-sample t−test, which are as follows:
  
1. The data are continuous (not discrete).
2. The data (tooth lengh) follow the normal probability distribution.
3. The variances of the two populations are equal. (If not, the Aspin-Welch Unequal-Variance test is used.)
4. The two samples are independent (according to supplement types or level of dose). There is no relationship between the individuals in one sample as compared to the other (as there is in the paired t-test).
5. Both samples are simple random samples from their respective populations. Each individual in the population has an equal probability of being selected in the sample.

## 3. Exploratory Data Analysis (EDA)
Before delving into the heart of the analysis, it is appropriate perform some basic exploratory data analysis.

### 3.1. ToothGrowth Description
ToothGrowth is an R dataframe comprised of 60 observations and 3 variables.
Length (len) is a numerical variable from 4.20 to 3.90
Supplement (supp) is a factor with 2 levels, "OJ" (orange juice) and "VC" (vitamin C)
Dose (dose) is a factor with 3 levels, "0.5", "1", and "2"

```{r}
data(ToothGrowth)
? ToothGrowth # To read the document on ToothGrowth
str(ToothGrowth)

tg <- ToothGrowth
dim(tg)
```
### 3.2. ToothGrowth Summary

```{r}
library(dplyr)
group_data = group_by(tg, supp, dose)
dplyr::summarise(group_data, mean(len))
```

### 3.3. Data Visulization

First, we construct a scatterplot of tooth length according to supplement type (Vitamin C and Orange Juice) by dosage levels, using ggplot() with geom() functions.

```{r}
library(ggplot2)
p = ggplot(tg, aes(x = dose, y = len))
p = p + geom_point(aes(color = supp, shape = supp), size = 2.0)
p = p + labs(x = "Dose in milligrams/day", y = "Tooth Length")
p = p + labs(title = "Tooth Growth")
axis_size = element_text(face= "bold", size = rep(1.5))
axis_title = element_text(face = "bold.italic", color = "slateblue", size = rel(1))
main_title = element_text(size = rel(2.0), color = "slateblue")
# title and labels
p = p + theme(axis.text = axis_size, axis.title = axis_title, plot.title = main_title)
# legend title
p = p + theme(legend.title = element_text(colour="slateblue", size=rel(1), face="bold"))
# legend labels
p = p + theme(legend.text = element_text(colour="black", size=rel(1), face="bold"))
p
```

### 3.4. Boxplot Tooth Growth Factors
Boxplots can provide a visual aid in determining if one distribution's parameters are significantly different than another. This can help us determine whether a hypothesis test is unnecessary. We use three sets of boxplots as follows:

1. First, we examine the boxplots of the supplements, OJ and VC
2. Second, we explor the boxplots of the three levels of doses
3. Third, the dosage levels for each of the two different supplements

#### Boxplot Comparison of Orange Juice vs Vitamin C

```{r}
library(ggplot2)
p = ggplot(tg, aes(x = supp, y = len))
p = p + geom_boxplot(aes(fill = supp))
p = p + labs(x = "Supplement type (VC or OJ)", y = "Tooth Length")
p = p + labs(title = "Comparisons")
axis_size = element_text(face= "bold", size = rel(1.0))
axis_title = element_text(face = "bold.italic", color = "blue2", size = rel(1.25))
main_title = element_text(size = rel(2.0), color = "blue2")
# title and labels
p = p + theme(axis.text = axis_size, axis.title = axis_title, plot.title = main_title)
# legend title
p = p + theme(legend.title = element_text(colour="blue2", size=rel(1.25), face="bold"))
# legend labels
p = p + theme(legend.text = element_text(colour="black", size=rel(1.25), face="bold"))
p
```

The first set of boxplots demonstrate distributions whose means may be significantly different warrant analysis using hypothesis testing.

For the remaining boxplots, we omit the code using echo=FALSE in the markdown to save space. They are all similar to the previous.

#### Boxplot Comparison of Three Dosage Levels

```{r echo=FALSE}
library(ggplot2)
tg$dose = as.factor(tg$dose)
p = ggplot(tg, aes(x = dose, y = len))
p = p + geom_boxplot(aes(fill = dose)) 
p = p + labs(x = "Dose in milligrams/day", y = "Tooth Length")
p = p + labs(title = "Comparisons")
axis_size = element_text(face= "bold", size = rel(1.0))
axis_title = element_text(face = "bold.italic", color = "royalblue", size = rel(1.25))
main_title = element_text(size = rel(2.0), color = "royalblue")
# title and labels
p = p + theme(axis.text = axis_size, axis.title = axis_title, plot.title = main_title)
# legend title
p = p + theme(legend.title = element_text(colour="royalblue", size=rel(1.25), face="bold"))
# legend labels
p = p + theme(legend.text = element_text(colour="black", size=rel(1.25), face="bold"))
p
```

This set of boxplots also reveal distributions whose means may be significally differet warrent anaysis using hypothesis testing.

#### Boxplot Comparison of Orange Juice vs Vitamin C at Three Dodage Levels

```{r echo=FALSE}
library(ggplot2)
tg$dose = as.factor(tg$dose)
p = ggplot(tg, aes(x = dose, y = len))
p = p + geom_boxplot(aes(fill = dose)) 
p = p + facet_wrap(~ supp)
p = p + labs(x = "Dose in milligrams/day", y = "Tooth Length")
p = p + labs(title = "Multiple Comparisons")
axis_size = element_text(face= "bold", size = rel(1.0))
axis_title = element_text(face = "bold.italic", color = "dodgerblue", size = rel(1.25))
main_title = element_text(size = rel(2), color = "dodgerblue")
# title and labels
p = p + theme(axis.text = axis_size, axis.title = axis_title, plot.title = main_title)
p = p + theme(strip.text = element_text(face = "bold", size = rel(1.25), colour = "blue"))
# legend title
p = p + theme(legend.title = element_text(colour="dodgerblue", size=rel(1.25), face="bold"))
# legend labels
p = p + theme(legend.text = element_text(colour="black", size=rel(1.25), face="bold"))
p
```

This set of boxplots shows that at dosage-level 0.5, the means of OJ and VC may be significantly different, as does the means of OJ and VC at dosage-level 1. However, the distributions of OJ and VC at dosage-level 2 appear not to be significantly different. Hence, we can perform the following hypothesis tests.

## 4. Hypothesis Test
Plots are good visual aids in proving insights to the data we analyze. However, only hypothesis testing can provide the rigor of statistical significance. Even when things appear to be unequal, the differences we see may not be statistically significant.

### 4.1. Hypothesis Test on the Effect of Supplement Types
To perform this hypothesis test we have to assume these two samples (OJ and VC) are taken from populations with a Gaussian distribution. Moreover, it is necessary to check that these samples have equal variances, by performing a t-test as follows

```{r}
var.test(len ~ supp, data = tg)
```

Examining the p-value (0.2331), we see that it is greater than 0.05. Hence, we fail to reject the null hypothesis that two variances are equal.

Now, we test the following hypothesis.

H0 : There is no significant difference between the means supplement type (VC or OJ). That is, the two supplements have similar effects on tooth length.

HA : Supplement types OJ and VC have different means and hence different effects on tooth length.

```{r}
t.test(len ~ supp, paired = F, var.equal = T, data = tg)
```

The p-value (0.06039) is greater than 0.05, which indicates that we cannot reject the null hypothesis . Thus, we conclude that the two supplement types have a similar effect on the tooth growth. We also see that the confidence interval for the difference of means is [−0.167,7.567]. Since this interval contains zero, we conclude that there is no significant difference between the effect of these two supplement types on the growth of tooth.

### 4.2. Hypothesis Test on the Effect of Dose Types

In keeping with class techniques, we will use pair by pair (F-paired) tests for our evaluating these hypotheses.

H10: The means of dosage levels "0.5" and "1" are the same.
H20: The means of dosage levels "0.5" and "2" are the same.
H30: The means of dosage levels "1" and "2" are the same.

```{r}
subset1 = subset(tg, dose %in% c(0.5, 1.0))
subset2 = subset(tg, dose %in% c(0.5, 2.0))
subset3 = subset(tg, dose %in% c(1.0, 2.0))
```

Again, we first test for equal variances using a t−test.

```{r}
var.test(len ~ dose, data = subset1)
```

Since these these samples have equal variance, we choose var.equal=TRUE in the following t−test.

```{r}
t.test(len ~ dose, paired = FALSE, var.equal = TRUE, data = subset1)
```

The p−value is less than 0.05. Hence, we reject the null hypothesis conclude there is a difference in the effect of these two dosage levels. Also the confidence interval for the difference of the means is [−11.9837,−6.2763]. This implies the first level (dose="0.5") is not effective as the second level (dose="1").

Similarly, we can conduct the above procedure to mydata2 and mydata3.

### 4.3. Suplement as a Factor within Dose Levels

Similarly, we need to get the following three sub-group data.

```{r}
subset4 = subset(tg, dose == 0.5)
subset5 = subset(tg, dose == 1.0)
subset6 = subset(tg, dose == 2.0)
```

Now, we assume that the variances of two supplement types are different. So we choose var.equal=F in the fllowing t−test.

```{r}
t.test(len ~ supp, paired = F, var.equal = F, data = subset4)
```

The p−value (0.006359) is much less than 0.05. Hence, we reject the null hypothesis. This the difference in means is not equal to zero, and we conclude that there is a difference between orange juice and vitamin C as supplements for teeth growth. Also, the associated confidence interval for the difference of means is [1.719,8.781]. This implies the supplement type of orange juice (OJ) is much more effective than vitamin C (VC) at the level of dose equal to 0.5.


Also, similarly, we can conduct the above procedure to subset5 and subset6.

## 5. Summary

We concluded that the two supplement types have a similar effect on the tooth growth when taken at a aggregate of dose-levels. We also concluded there is a difference in the effect of these two dosage levels apart from supplement type. Finally, we learned there is a difference between orange juice and vitamin C as supplements for teeth growth.

