library(data.table)
library(RSQLite)
library(DBI)
library(readr)
library(tidyverse)


# Question 2

#connect to DataBase using SQLite & DBI Library function as below:
con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge3.db')
#Read Table wage1 as required - 
wdata<-DBI::dbReadTable(con,'wage2')
wdata<-data.table(wdata)
#View Lables for the columns with their data types and column descriptions as labels
print(DBI::dbReadTable(con,paste('wage2','labels',sep='_')))
#View wag1 data
view(wdata)
#Disconnect to free DB 
dbDisconnect(con)

# Use the data in the lawsch85 table for this exercise.
# 1. Using the model:
#   ln[salary] = Beta0 + Beta1LSAT + Beta2GPA + beta3 ln[libvol] + Beta4 ln[cost] + Beta5rank + u,
# state and test the null hypothesis that the rank of law schools has no ceteris paribus effect on median
# starting salary.

model1<-lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank, data = wdata)
summary(model1)

# Let H0 = 0 ; ie. Rank has no impact on salary
# Since as per summary rank is having value in -ve, it indicates that it has no relevant affect on Salary
# Hence our null hypothesis is true

# 2. Are features of the incoming class of students-namely, LSAT and GPA-individually or jointly
# significant for explaining salary? (Be sure to account for missing data on LSAT and GPA.)

model2<-0
wdata1<-wdata[(!is.na(LSAT))&(!is.na(GPA))]
model2<-lm(log(salary)~log(libvol)+log(cost)+rank, data = wdata1)
summary(model1)
anova(model1,model2)


# 2.3
model3 <- lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank+clsize+faculty,data=wdata)
summary(model3)



# Question 10
model4 <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms,data=wdata)
summary(model4)

predict(model4,data.frame(lotsize=20000,sqrft=2500,bdrms=4))
exp(5.992899)

model5 <- lm(price~log(lotsize)+log(sqrft)+bdrms,data=wdata)
summary(model5)

# questtio 7
model6 <- lm(log(wage)~educ+exper+I(exper^2),data=wdata)
summary(model6)


coeffs<-coef(model6)
coeffs
returns <- function(year){
  return((coeffs[3]+2*coeffs[4]*year))
}
c(returns(5),returns(20))

-coeffs[3]/(2*coeffs[4])

coeffs[4]

predict(model6,data.frame(educ=10,exper=28.77))
predict(model6,data.frame(educ=10,exper=28.90))

experience_count<-0
experience_count <- (wdata$exper>=28.77049)
experience_count
c(sum(experience_count),100*mean(experience_count))
sum(experience_count)
count(wdata)
121/526


# question 5
nrow(wdata[fsize==1])

model8 <- lm(nettfa~inc+age,data=wdata[fsize==1])
summary(model8)


library(broom)
mat <- tidy(model8)
mat

tstat <- (mat[3,2]-1)/mat[3,3]
tstat
c(tstat,pnorm(tstat))

#H0: b2=1
#H1: b2<1
model9<-lm(nettfa-age~inc+age,data = wdata[fsize==1])
summary(model9)
confint(model9,level = .99)
#p value 8%, so we do not reject H0 at 5% and 1% significance level.

model10<-lm(nettfa~inc,data = wdata[fsize==1])
summary(model10)
#No the value of coeffienct has not changed much - new value=0.8207, old vlaue=0.7993
#this is because income is not dependent age


model11<-lm(log(price)~log(dist),data = wdata[year==1981])
summary(model11)

model12 <- lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age,data=wdata)
summary(model12)

model13 <- lm(log(price)~log(dist)+log(intst)+I(log(intst)^2)+log(area)+log(land)+rooms+baths+age,data=wdata)
summary(model13)

model14 <- lm(log(price)~log(dist)+I(log(dist)^2)+log(intst)+log(area)+log(land)+rooms+baths+age,data=wdata)
summary(model14)


# question 9
model15<-0
model15 <- lm(sat~hsize+I(hsize^2),data=wdata)
summary(model15)

#Optimal high school size can be calulated by differentiating the equation
model15_coef<-coef(model15)
model15_coef
b1=model15_coef[2]
b2=model15_coef[3]
hsize1=-b1/(2*b2)
hsize1
#465 student is the optimal class size. This is the maximum value as the second derivative is negative.

model16 <- lm(log(sat)~hsize+I(hsize^2),data=wdata)
summary(model16)

model16_coef<-coef(model16)
model16_coef
b1=model16_coef[2]
b2=model16_coef[3]
hsize1=-b1/(2*b2)
hsize1


# Question 4
model17 <- lm(log(wage)~educ+exper+tenure,data=wdata)
summary(model17)

#H0: b2=b3
model18<-lm(log(wage)~educ+exper+I(exper+tenure),data = wdata)
summary(model18)

confint(model18)

# Question 1

model19<-lm(voteA~log(expendA)+log(expendB)+prtystrA,data = wdata)
summary(model19)

#With every 1% increase in expendA, voteA increases by 6.08136 units


model20 <- lm(voteA~log(expendA)+log(expendB)+prtystrA,data=wdata)
summary(model20)


model21<-lm(voteA~log(expendA)+I(log(expendA)-log(expendB))+prtystrA,data = wdata)
summary(model21)
tidyw(model21)


model22 <- lm(log(price)~sqrft+bdrms,data=wdata)
summary(model22)


confint(model22)
coefficients <- coef(model22)
theta<-150*coefficients[2]+coefficients[3]
theta

model23 <- lm(log(price)~I(sqrft-150*bdrms)+I(theta*bdrms),data=wdata)
summary(model23)

# question 8

model24 <- lm(log(wage)~educ+exper+educ:exper,data=wdata)
summary(model24)

predict(model24,data.frame(educ=c(12,13,14),exper=c(10)))

##Question8.3
model25<-lm(log(wage)~educ+exper+I(educ*exper),data = wdata)
summary(model25)
#p value for (educ*exper) is 3%, making the variable significant at 5% significance level, so we reject the null hypothesis, but not at 1% significance level

##Question8.4
model26<-lm(log(wage)~educ+exper+I(educ*exper-10*educ),data = wdata)
summary(model26)
#Theta is 0.07608, having p value lower than 1%, making it significant at both 1% and 5% significance level
confint(model26)
#lower and upper limits of theta are 0.06309 and 0.08906

















