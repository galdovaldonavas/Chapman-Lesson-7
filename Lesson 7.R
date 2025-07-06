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
library(coefplot)# to graph confidence intervals of regression coefficients
library(MCMCpack)

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
gf_point(overall ~ rides, data = sat.df,
         main = "Relation Between Rides Satisfaction and General Satisfaction", 
         xlab = "Ride's Satisfaction",
         ylab = "General Satisfaction") %>%
  gf_smooth(overall ~ rides, method = "lm", color = "red", se = FALSE)

#model with a single predictor
m1<-lm(overall~rides, data=sat.df
  )
m1
str(m1)
summary(m1)
confint(m1)
cor<-with(sat.df, cor(overall, rides))
cor^2

#Checking the fit of the model

par(mfrow =c(2,2))
plot(m1)

#we check the cases pointed by the resuduals vs leverage graph

sat.df[c(57,129,295), ]



# 7.3. FITTING LINEAR MODELS WITH MULTIPLE PREDICTORS

m2<-lm(overall~rides + games+ wait+ clean, data= sat.df)
m2
summary(m2)
plot(m2)
hist(m2[["residuals"]])

#plotting the confidence intervals of the regression coefficients

coefplot(m2, intercept=FALSE, outerCI= 1.96, lwOuter=1.5, ylab= "Feature Rated",
         xlab="Association with Overall Satisfaction")

#Comparing Models

summary(m1)
summary(m2)



# Agrega residuos y valores ajustados al data frame

sat.df$fitted_m1 <- fitted(m1)
sat.df$fitted_m2 <- fitted(m2)

# Crea gráfico con facetas
gf_point(overall ~ fitted_m1, data = sat.df, color = ~"Modelo 1") %>%
  gf_point(overall ~ fitted_m2, data = sat.df, color = ~"Modelo 2") %>%
  gf_labs(
    title = "Comparison of Fitted vs Actual Values",
    x = "Fitted",
    y = "Satisfaction"
  )

#Making Predictions from the model

#we can use a matrix of the scores we expect for a person, 
#for example a person who scores 100 in all 4 of satisfaction predictors

coef(m2) %*% c(1,100,100,100,100) 

#we can also calculate and save the predicted value for each subject in the sample, as we did earlier for the graph
sat.df$predicted <- predict(m2, sat.df)
sat.df$fitted_m2 <- fitted(m2) # otra manera

#STANDARDIZING THE PREDICTORS TO INCLUDE PREDICTORS ON DIFFERENT UNITS

sat.df$logdist<- log(sat.df$distance)
sat.df

sat.std<- sat.df[, c(1:2,4:8,14)] #to remove distance without the logarithmic transformation
sat.std[,3:8]<- scale(sat.std[,3:8])
head(sat.std)
summary(sat.std)
describe(sat.std)

#7.4 USING FACTORS AS PREDICTORS

m3<-lm(overall~rides + games+ wait+ clean+ weekend+ logdist+ num.child, data= sat.std)
m3
summary(m3)
plot(m3)

sat.std$num.child.factor <- factor(sat.std$num.child)
m4<-lm(overall~rides + games+ wait+ clean+ weekend+ logdist+ num.child.factor, data= sat.std)
m4
summary(m4)
plot(m4)

sat.std$has.child <- factor(ifelse(sat.std$num.child>=1,1,0))
m5<-lm(overall~rides + games+ wait+ clean+ logdist+ has.child, data= sat.std)
m5
summary(m5)
plot(m5)


m6<-lm(overall~rides + games+ wait+ clean+ weekend+ logdist+  has.child + 
         rides:has.child + games:has.child+ wait:has.child+ clean:has.child +
         rides:weekend + games:weekend+ wait:weekend+ clean:weekend,
          data= sat.std)
m6
summary(m6)
plot(m6)



m7<-lm(overall~rides + games+ wait+ clean+ logdist+  has.child + 
      wait:has.child,
       data= sat.std)
m7
summary(m7)
plot(m7)

#plotting the confidence intervals of the regression coefficients

coefplot(m7, intercept=FALSE, outerCI= 1.96, lwOuter=1.5, ylab= "Feature Rated",
         xlab="Association with Overall Satisfaction")


#7.5.4 BAYESIAN APPROACH

m7.bayes<-MCMCregress(overall~rides + games+ wait+ clean+ logdist+  has.child + 
         wait:has.child,
       data= sat.std)
summary(m7.bayes)
