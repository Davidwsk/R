#https://www.learnbymarketing.com/tutorials/explaining-the-lm-summary-in-r/
#Anscombe's Quartet Q1 Data
y=c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68)
x1=c(10,8,13,9,11,14,6,4,12,7,5)
#Some fake data, set the seed to be reproducible.
set.seed(15)
x2=sqrt(y)+rnorm(length(y))

model=lm(y~x1+x2)
summary(model)

summary(y-model$fitted.values)

#Residual Standard error (Like Standard Deviation)
k=length(model$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(model$residuals**2)
n=length(model$residuals)
sqrt(SSE/(n-(1+k))) #Residual Standard Error

#Multiple R-Squared (Coefficient of Determination)
SSyy=sum((y-mean(y))**2)
SSE=sum(model$residuals**2)
(SSyy-SSE)/SSyy
#Alternatively
1-SSE/SSyy

#Adjusted R-Squared
n=length(y)
k=length(model$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(model$residuals**2)
SSyy=sum((y-mean(y))**2)
1-(SSE/SSyy)*(n-1)/(n-(k+1))

#F-Statistic
#Ho: All coefficients are zero
#Ha: At least one coefficient is nonzero
#Compare test statistic to F Distribution table
n<-length(y)
SSE<-sum(model$residuals**2)
SSyy<-sum((y-mean(y))**2)
k<-length(model$coefficients)-1
((SSyy-SSE)/k) / (SSE/(n-(k+1)))
