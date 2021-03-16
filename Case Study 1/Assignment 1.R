library(data.table)
library(RSQLite)
library(DBI)
library(readr)
#connect to DataBase using SQLite & DBI Library function as below:
con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge3.db')
#Read Table wage1 as required - 
wdata<-DBI::dbReadTable(con,'wage1')
wdata<-data.table(wdata)
#View Lables for the columns with their data types and column descriptions as labels
print(DBI::dbReadTable(con,paste('wage1','labels',sep='_')))
#View wag1 data
wdata
#Disconnect to free DB 
dbDisconnect(con)

#Question 1 -
#1. Find the average education level in the sample. What are the lowest and highest years of education?
summary(wdata$educ)

#2. Find the average hourly wage in the sample. Does it seem high or low?
mean(wdata$wage)
summary(wdata$wage)

#3. The wage data are reported in 1976 dollars. Using the Economic Report of the President (2011 or
#later), obtain and report the Consumer Price Index (CPI) for the years 1976 and 2010
lv_cpi<-read.csv("CPI.csv")
lv_cpi
summary(CPI)

#4. Use the CPI values from above to find the average hourly wage in 2010 dollars. Now does the average
#hourly wage seem reasonable?

#Import file with CPI data of the year 2010
lv_cpi2010<-read.csv("CPI2010.csv")
lv_cpi2010

#Create a data frame for columns pertaining to monthly data
df<-data.frame(y=lv_cpi2010[2:13])
df

#Transpose from row to column
df1<-t(df)
df1
#Derive mean of wages for all the months
mean(df1)

#5.How many women are in the sample? How many men?
#Females
sum(wdata$female=='1')
#Males
sum(wdata$female=='0')


#Question 2

#connect to DataBase using SQLite & DBI Library function as below:
con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge3.db')
#Read Table wage1 as required - 
wdata<-DBI::dbReadTable(con,'meap01')
wdata<-data.table(wdata)
#View Lables for the columns with their data types and column descriptions as labels
print(DBI::dbReadTable(con,paste('meap01','labels',sep='_')))
#View wag1 data
wdata
#Disconnect to free DB 
dbDisconnect(con)

#The data in meap01 table are for the state of Michigan in the year 2001. Use these data to answer the
#following questions.
#1. Find the largest and smallest values of math4. Does the range make sense? Explain.

min(wdata$math4)
max(wdata$math4)
summary(wdata$math4)

#2. How many schools have a perfect pass rate on the math test? What percentage is this of the total sample?

summary(wdata$math4)
df<-data.frame(y=wdata$math4)
df
df1<-data.table(df)
df1
perfectpass<-data.frame()
perfectpass<-0
count(df1)
for (i in 1:1823) {
  if (df1$y[i]=='100') {
    perfectpass<-perfectpass + 1
  }
}
perfectpass

perfectpass_perc <- (perfectpass/1823)*100
perfectpass_perc

#3. How many schools have math pass rates of exactly 50%?
pass50<-0
for (i in 1:1823) {
  if (df1$y[i]=='50') {
    pass50<-pass50 + 1
  }
}

pass50

#4.Compare the average pass rates for the math and reading scores. Which test is harder to pass?

mean(wdata$math4)
mean(wdata$read4)


#5. Find the correlation between math4 and read4. What do you conclude?

relation<-cor.test(wdata$math4,wdata$read4, method = 'pearson')
relation

#6. The variable exppp is expenditure per pupil. Find the average of exppp along with its standard
#deviation. Would you say there is wide variation in per pupil spending?

#Average
mean(wdata$exppp)
#Review max,min,median,mode
summary(wdata$exppp)

#Find std deviation
var1<-var(wdata$exppp)
Std_dev<-sqrt(var1)
Std_dev

#7. Suppose School A spends $6000 per student and School B spends $5500 per student. By what percentage
#does School A's spending exceed School B's? Compare this to 100x[ln(6000) ??? ln(5500)], which is the
#approximation percentage difference based on the difference in the natural logs.

v1<-((6000-5500)/5500)*100
v1
log(5500)
log(6000)
v1<-0
v1<-log(6000)-log(5500)
100*v1

9.090909-8.701138


#Question3
#connect to DataBase using SQLite & DBI Library function as below:
con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge3.db')
#Read Table wage1 as required - 
wdata<-DBI::dbReadTable(con,'401k')
wdata<-data.table(wdata)
#View Lables for the columns with their data types and column descriptions as labels
print(DBI::dbReadTable(con,paste('401k','labels',sep='_')))
#View wag1 data
wdata
#Disconnect to free DB 
dbDisconnect(con)

#1. Find the average participation rate and the average match rate in the sample of plans.
mean(wdata$prate)
mean((wdata$mrate))

#2. Now, estimate the simple regression equation:
#prate(CAP) = beta0(CAP) + beta1(CAP)*mrate
#and report the results along with the sample size and R-squared.

model1 <- lm((wdata$prate)~wdata$mrate,data=wdata)
summary(model1)

nrow(wdata)

# trainingRowIndex <- sample(1:nrow(wdata), 0.8*nrow(wdata))  # row indices for training data
# trainingData <- wdata[trainingRowIndex, ]  # model training data
# testData  <- wdata[-trainingRowIndex, ]   # test data
# testData
# 
# length(trainingData)
# 
# # Build the model on training data -
# lmMod <- lm((wdata$prate)~wdata$mrate, data=trainingData)  # build the model
# distPred <- predict(lmMod, testData)  # predict distance
# 
# summary (lmMod)


83.0755 + (5.8611*3.5)
 
# The data set in ceosal2 contains information on chief executive officers for U.S. corporations. The variable
# salary is annual compensation, in thousands of dollars, and ceoten is prior number of years as company CEO.

#connect to DataBase using SQLite & DBI Library function as below:
con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge3.db')
#Read Table wage1 as required - 
wdata<-DBI::dbReadTable(con,'ceosal2')
wdata<-data.table(wdata)
#View Lables for the columns with their data types and column descriptions as labels
print(DBI::dbReadTable(con,paste('ceosal2','labels',sep='_')))
#View wag1 data
wdata
#Disconnect to free DB 
dbDisconnect(con)


# 1. Find the average salary and the average tenure in the sample.

mean(wdata$salary)
mean(wdata$comten)

# 2. How many CEOs are in their first year as CEO (that is, ceoten = 0)? What is the longest tenure as a
# CEO?

# Part1
count<-0
counter<-nrow(wdata)
for (i in 1:counter) {
  if (wdata$ceoten[i]==0) {
    count<-count + 1
  }
}

count

# Part2

max(wdata$ceoten)

# 3. Estimate the simple regression model
# ln[salary] = Beta0 + Beta1ceoten + u
# and report your results in the usual form. What is the (approximate) predicted percentage increase in
# salary given one more year as a CEO?

wsal<-log(wdata$salary)
model1 <- lm(wsal~wdata$ceoten,data=wdata)
summary(model1)



# Question5

#connect to DataBase using SQLite & DBI Library function as below:
con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge3.db')
#Read Table wage2 as required - 
wdata<-DBI::dbReadTable(con,'wage2')
wdata<-data.table(wdata)
#View Lables for the columns with their data types and column descriptions as labels
print(DBI::dbReadTable(con,paste('wage2','labels',sep='_')))
#View wag1 data
wdata
#Disconnect to free DB 
dbDisconnect(con)

# Use the data in wage2 to estimate a simple regression explaining monthly salary (wage) in terms of IQ score
# (IQ).
# 1. Find the average salary and average IQ in the sample. What is the sample standard deviation of IQ?
#   (IQ scores are standardized so that the average in the population is 100 with a standard deviation equal
#    to 15.)

mean(wdata$wage)
mean(wdata$IQ)

sqrt(var(wdata$IQ))

# 2.Estimate a simple regression model where a one-point increase in IQ changes wage by a constant dollar
# amount. Use this model to find the predicted increase in wage for an increase in IQ of 15 points. Does
# IQ explain most of the variation in wage?

model1 <- lm(wage~IQ,data=wdata)
summary(model1)
model1

predict(model1,data.frame(IQ=15))

# 3. Now, estimate a model where each one-point increase in IQ has the same percentage effect on wage. If
# IQ increases by 15 points, what is the approximate percentage increase in predicted wage?

model1<-lm(log(wage)~IQ,data = wdata)
summary(model1)

predict(model1,data.frame(IQ=c(1,15)))

# Question 6
# Using the meap93 data, we want to explore the relationship between the math pass rate (math10) and spending
# per student (expend).

con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge3.db')
#Read Table meap93 as required - 
wdata<-DBI::dbReadTable(con,'meap93')
wdata<-data.table(wdata)
#View Lables for the columns with their data types and column descriptions as labels
print(DBI::dbReadTable(con,paste('meap93','labels',sep='_')))
#View meap93 data
wdata
#Disconnect to free DB 
dbDisconnect(con)

# 1. Do you think each additional dollar spent has the same effect on the pass rate, or does a diminishing
# effect seem more appropriate? Explain.

library(tidyverse)
library(broom)
summary(wdata$expend)
model1<-lm(gradrate~expend,wdata)
summary(model1)

tidy(model1)
glance(model1)

predict(model1,data.frame(expend=c(1000,2000)))


# 2. In the population model,
# math10 = Beta0 + Beta1 ln[expend] + u
# argue that Beta1/10 is the percentage point change in math10 given a 10% increase in expend.


model1<-lm(math10~log(expend), wdata)
summary(model1)

predict(model1,data.frame(expend=c(1,1.1)))

math1=-69.341+(11.164*1)
math1

math02=-69.341+(11.164*1.1)
math02

# 3. Estimate this model. Report the estimated equation in the usual way, including the sample size and
# R-squared.

nrow(wdata)
summary(model1)

# 4. How big is the estimated spending effect? Namely, if spending increases by 10%, what is the estimated
# percentage point increase in math10?

# Ans. it is not phenomenal. Spending increases by 10% percentage point increase in math10 is .01 only

# 5. One might worry that regression analysis can produce fitted values for math10 that are greater than
# 100. Why is this not much of a worry in this data set?

summary(wdata$math10)

# As per summary indication above for Math marks. The maximum math value here in the dataset is 66.7 which is 
# below 100. So our linear regression model shall not produce over-fitting results

# Question 7

con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge3.db')
#Read Table hprice1 as required - 
wdata<-DBI::dbReadTable(con,'hprice1')
wdata<-data.table(wdata)
#View Lables for the columns with their data types and column descriptions as labels
print(DBI::dbReadTable(con,paste('hprice1','labels',sep='_')))
#View hprice1 data
wdata
#Disconnect to free DB 
dbDisconnect(con)

# Use the data in hprice1 to estimate the model
# price = Beta0 + Beta1sqrft + Beta2bdrms + u,
# where price is the house price measured in thousands of dollars.
# 
# 1. Write out the results in equation form.

model1<-lm(price~sqrft+bdrms,wdata)
summary(model1)

# Price= -19.31500 + (0.12844)sqrft + (15.19819)bdrms

# 2. What is the estimated increase in price for a house with one more bedroom, holding square footage
# constant?

model1<-lm(price~bdrms,wdata)
summary(model1)

predict(model1,data.frame(bdrms=c(1,2)))

# 3. What is the estimated increase in price for a house with an additional bedroom that is 140 square feet
# in size? Compare this to your answer from above.

model1<-lm(price~bdrms+sqrft,wdata)
summary(model1)

predict(model1,data.frame(bdrms=c(1,2),sqrft=c(140)))

# 4. What percentage of the variation in price is explained by square footage and number of bedrooms?

# Multiple R-squared:  0.6319 


# 5. The first house in the sample has sqrft = 2438 and bdrms = 4. Find the predicted selling price for this
# house from the OLS regression line.


model1<-lm(price~bdrms+sqrft,wdata)

predict(model1,data.frame(bdrms=c(4),sqrft=c(2348)))


library(ggplot2)
sp<-ggplot(wdata,aes(x=bdrms,y=price))
sp+geom_point()
sp+geom_point(col='red',size=3)
sp+geom_point(col='red',size=3)+stat_smooth(method = 'lm')

# 6. The actual selling price of the first house in the sample was $300000 (so price = 300). Find the residual
# for this house. Does it suggest that the buyer underpaid or overpaid for the house?


model1<-lm(price~bdrms,wdata)
summary(model1)


# Question 8
# The file ceosal2 contains data on 177 chief executive officers and can be used to examine the effects of firm
# performance on CEO salary.

con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge3.db')
#Read Table ceosal2 as required - 
wdata<-DBI::dbReadTable(con,'ceosal2')
wdata<-data.table(wdata)
#View Lables for the columns with their data types and column descriptions as labels
print(DBI::dbReadTable(con,paste('ceosal2','labels',sep='_')))
#View ceosal2 data
wdata
#Disconnect to free DB 
dbDisconnect(con)

# 1. Estimate a model relating annual salary to firm sales and market value. Make the model of the constant
# elasticity variety for both independent variables. Write the results out in equation form.

# ANS
# The "constant elasticity variety" means a model that is linear in elasticities. Elasticities
# are percentage changes. So a constant elasticity model would be:
#   log(salary) = ??0 + ??1log(sales) + ??2log(mktval) + u.

model1<-lm(log(salary)~log(sales)+log(mktval),wdata)
summary(model1)

# log(salary)=4.62092+0.16213*Log(sales)+0.10671*log(mktval)

# 2. Add profits to the model. Why can this variable not be included in logarithmic form? Would you say
# that these firm performance variables explain most of the variation in CEO salaries?

# PART1
model1<-lm(log(salary)~log(sales)+log(mktval)+log(profits),wdata)
summary(model1)

# Because of addition of Profits in our model, we are getting -ve values. 

# PART2
model1<-lm(log(salary)~log(sales)+log(mktval)+profits,wdata)
summary(model1)
# Now we dont get -ve value for Profits.However since Rsquare is 30% which is <70%, hence we cannot say that 
# Profits has hugely impacted our model

#   3. Now also add the variable ceoten to the model. What is the estimated percentage return for another
# year of CEO tenure, holding other factors fixed?

model1<-lm(log(salary)~log(sales)+log(mktval)+profits+ceoten,wdata)
summary(model1)

predict(model1,data.frame(sales=c(100),mktval=c(100),profits=c(100),ceoten=c(10,11)))
# If we run a prediction on our model, then we will see that the difference in pay from 10th year to 11th year is just
# 1.16%

#   4. Find the sample correlation coefficient between the variables log(mktval) and profits. Are these variables
# highly correlated? What does this say about the OLS estimators?

marketvalue<-log(wdata$mktval)
marketvalue
profits<-wdata$profits
profits

cor(marketvalue,profits)


# Question 9
# Use the data in attend for this exercise. Create the variable atndrte which is attend/32 because there were
# 32 classes.

con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge3.db')
#Read Table attend as required - 
wdata<-DBI::dbReadTable(con,'attend')
wdata<-data.table(wdata)
#View Lables for the columns with their data types and column descriptions as labels
print(DBI::dbReadTable(con,paste('attend','labels',sep='_')))
#View attend data
wdata
#Disconnect to free DB 
dbDisconnect(con)


# 1. Obtain the minimum, maximum, and average values for the variables atndrte, priGPA, and ACT.
summary(wdata$atndrte)
summary(wdata$priGPA)
summary(wdata$ACT)

# 2. Estimate the model
# atndrte = Beta0 + Beta1GPA + Beta2ACT + u
# and write the results in equation form. Interpret the intercept. Does it have a useful meaning?
  
model1<-lm(atndrte~priGPA+ACT,wdata)
summary(model1)

# Intercept is the max attendence a student gets when other factors are 0 in out linear equation. 
# In our case it is 77.7702

predict(model1,data.frame(priGPA=c(3.0,3.2),ACT=c(20,22)))
# atndrte= 77.7702 + (14.7331)priGPA + (-1.5274)ACT

# When we increase one point value for priGPA & ACT scores then Attendence seems to deprecite in decimals
# When we increase two point value for priGPA and keep ACT value constant then attendence seems to increase in decimal points

# To investigate this, I checked the correlation between priGPA & ACT scores, and seems like there is no correlation
cor(wdata$priGPA,wdata$ACT)
# [1] 0.3537694

# 3. Discuss the estimated slope coefficients. Are there any surprises?

# priGPA estimated value is 17.261 which means an increase in priGPA will increase attendance by 17.261%
# However ACT score is -1.717, which seems to decrease the attendance. This is a surprising

# 4. What is the predicted atndrte if priGPA = 3.65 and ACT = 20? What do you make of this result?
# Are there any students in the sample with these values of the explanatory variables?

predict(model1,data.frame(priGPA=c(3.65),ACT=c(20)))
# Attendance Rate comes out to be 104.3705 which is >100%. This cannot be true

max(wdata$atndrte)

# 5. If Student A has priGPA = 3.1 and ACT = 21 and Student B has priGPA = 2.1 and ACT = 26,
# what is the predicted difference in their attendance rates?

predict(model1,data.frame(priGPA=c(3.1),ACT=c(21)))
predict(model1,data.frame(priGPA=c(2.1),ACT=c(26)))

# 1 
# 93.16063 
# > predict(model1,data.frame(priGPA=c(2.1),ACT=c(26)))
# 1 
# 67.31727 
# > 93.16063-67.31727
# [1] 25.84336

# Question 10
# Use the data in htv to answer this question. The data set includes information on wages, education, parents'
# education, and several other variables for 1, 230 working men in 1991.

con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge3.db')
#Read Table htv as required - 
wdata<-DBI::dbReadTable(con,'htv')
wdata<-data.table(wdata)
#View Lables for the columns with their data types and column descriptions as labels
print(DBI::dbReadTable(con,paste('htv','labels',sep='_')))
#View htv data
wdata
#Disconnect to free DB 
dbDisconnect(con)

# 1. What is the range of the educ variable in the sample? What percentage of men completed 12th grade
# but no higher grade? Do the men or their parents have, on average, higher levels of education?
summary(wdata$educ)
#Range [6-20]

Men<-nrow(wdata[wdata$educ==12])
MenPerc<-(Men/nrow(wdata))*100
MenPerc

mean(wdata$educ)
mean(wdata$fatheduc)
mean(wdata$motheduc)

# Yes Men and their Father as a parent have higher education levels as compared to their Mother as a parent

#   2. Estimate the regression model
# educ = Beta0 + Beta1motheduc + Beta2fatheduc + u

model1<-lm(educ~motheduc+fatheduc,wdata)
summary(model1)


# by OLS and report the results in the usual form. How much sample variation in educ is explained by
# parents' education? Interpret the coefficient on motheduc.

# Sample variation Rsquare = 0.2493. This is not high enough

sp<-0
library(ggplot2)
sp<-ggplot(model1,aes(y=educ,x=fatheduc))
sp+geom_point()
sp+geom_point(col='red',size=3)
sp+geom_point(col='red',size=3)+stat_smooth(method = 'lm')

sp1<-0
library(ggplot2)
sp<-ggplot(model1,aes(y=educ,x=motheduc))
sp+geom_point()
sp+geom_point(col='red',size=3)
sp+geom_point(col='red',size=3)+stat_smooth(method = 'lm')

# For a point increase in education grade, mother's education increases by 30.4% as per summary(model1)

# 3. Add the variable abil (a measure of cognitive ability) to the regression above, and report the results in
# equation form. Does ability help to explain variations in education, even after controlling for parents'
# education? Explain.

model1<-lm(educ~motheduc+fatheduc+abil,wdata)
summary(model1)

#As per results, one point increase in education increases 50.2% increase in cognitive ability of parents
#this is a good number

# 4. Now estimate an equation where abil appears in quadratic form:
#   educ = Beta0 + Beta1motheduc + Beta2fatheduc + Beta3abil + Beta4abil2 + u.
abil2<-wdata$abil^2
model1<-lm(educ~motheduc+fatheduc+abil+abil2,wdata)
summary(model1)


# With the estimated coefficients on ability, use calculus to find the value of abil where educ is minimized.
# (The other coefficients and values of parents' education variables have no effect; we are holding parents'
#   education fixed.) Notice that abil is measured so that negative values are permissible. You might also
# verify that the second derivative is positive so that you do indeed have a minimum.

# Educ = 8.24 + .190motheduc + .109fatheduc +.401abil + .050abil2 + u
# d(Educ)/d(abil) = .401 + .10abil = 0
# abil* = -0.401/0.10 = -4.01
# d2(Educ)/d2(abil) = 0.10 > 0 and therefore we have a minimum.

# 5. Argue that only a small fraction of men in the sample have ability less than the value calculated
# above. Why is this important?
wdata[abil<'-4.01']
nrow(wdata[abil<'-4.01'])
100*(nrow(wdata[abil<'-4.01'])/nrow(wdata))

# Only 14 men out of the 1,230 have an ability level below -4.01. 

# This is important because only 14 individuals will have a higher expected education
# level than individuals slightly to the right of -4.01. That is, only 14 fitted values of the
# above equation are counter-intuitive.

#   6. Use the estimates above to plot the relationship between the predicted education and abil. Let motheduc
# and fatheduc have their average values in the sample, 12.18 and 12.45, respectively.


sp1<-0
library(ggplot2)
wdata1<-0
wdata1<-wdata
wdata1$motheduc = mean(wdata$motheduc)
wdata1$fatheduc = mean(wdata$fatheduc)
model2<-lm(educ~motheduc+fatheduc+abil+I(abil^2),wdata1)

sp<-ggplot(model1,aes(y=educ,x=abil2))
sp+geom_point()+geom_line(aes(y=predict(model2,wdata1),color='blue'))



mean(wdata$motheduc)



