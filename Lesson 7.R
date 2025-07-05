library(BayesFactor) # for bayesian analyses
library(car) # to explore pair scatterplot matrices, data explorations,  box-cox transformations
library(corrplot)# for correlation graphs
library(gplots)# for including colors in correlation graphs
library(lattice)# for segment visualizations
library(binom)# for confidence intervals in binomial tests
library(multcomp) # for plotting the means and ci of multigroup comparisons
library(psych)
library(ggformula) # also for plots
library(tidyverse)

# Reading the dataset
sat.df<- read.csv("http://goo.gl/HKn174")

#Exploring the dataset
sat.df<- read.csv("rintro-chapter7.csv")
summary(sat.df)
str(sat.df)
sat.df$weekend<- factor(sat.df$weekend)
describe(sat.df)
scatterplotMatrix(sat.df)
corrplot.mixed(cor(sat.df[, c(2:8)]),upper="ellipse")

# Bivariate correlation
gf_point(overall~rides, data=sat.df, main= "Predicting General Satisfaction with Rides Satisfaction", 
         xlab= "Ride's Satisfaction",
         ylab= "General Satisfaction")%>%
         gf_smooth(overall~rides, data=sat.df)
#model with a single predictor
m1<-lm(overall~rides, data=sat.df
  )
m1
gf_point(overall~rides, data=sat.df, main= "Predicting General Satisfaction with Rides Satisfaction", 
         xlab= "Ride's Satisfaction",
         ylab= "General Satisfaction",
         main="Relation Between Rides Satisfaction and General Satisfaction")%>%
  abline(m1, col="red")
