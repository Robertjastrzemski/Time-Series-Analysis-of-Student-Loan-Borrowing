require(plyr)
require(caret)
require(forecast)
require(ggplot2)
require(graphics)
require(xts)
require(chron)
require(fGarch)
require(tseries)
require(fractal)
require(rugarch)
require(astsa)
require(BNPTSclust)
require(gdata)
require(locits)
require(ltsa)
require(season)
require(timeSeries)
require(stats)

values<-read.csv("C:/Users/robj_01/Desktop/PRED_413/Case_1/wk6/change_in_student_loans.csv", header=T, dec=".", na.strings="0")

save(values, file='C:/Users/robj_01/Desktop/PRED_413/Case_1/wk6/StudentLoans.rda')
load('StudentLoans.rda')
typeof(StudentLoans)
is.data.frame(StudentLoans)

head(values)


StudentLoans.ts = ts(data=StudentLoans$total_increase_in_debt, frequency = 4, 
                     start=c(2006,1), end=c(2015,2))

print(StudentLoans.ts)

plot(StudentLoans.ts, ann=FALSE, col="blue", type='o')
title(main="Total Debt Increase by Quarters/Years (in Millions)") 
title(xlab="Year") 
title(ylab="Change in Debt")

class(StudentLoans.ts)

StudLoanTrain.ts = ts(data=StudentLoans.ts, start= 1, end=9, deltat=1/4)

print(StudLoanTrain.ts)

class(StudLoanTrain.ts)

g_range <- range(0, StudentLoans$total_increase_in_debt)

plot(StudentLoans.ts, type="o", ylim=g_range, 
     ann=FALSE, lwd=3, col="blue")
box()
title(main="Total Debt Increase by Quarter (in Millions)") 
title(xlab="Year") 
title(ylab="Change in Debt")

truehist(StudentLoans.ts, prob=TRUE, col="yellow", main="Histograph of Increase in Debt", 
         xlab="Dollar Value of Increase in Debt (in MM)", 
         ylab="Frequency of Occurrences")
lines(density(StudentLoans.ts), col="red")
lines(density(StudentLoans.ts, adjust=2), lty=3)

legend(1, g_range[2], c("Actual","Auto Forecast"), cex=0.8, 
       col=c("blue","red"), pch=21:22, lty=1:2)

qqnorm(StudentLoans.ts)
qqline(StudentLoans.ts, col="red")

lag1.plot(StudentLoans.ts, 9)

acf<-acf(StudentLoans.ts)
plot(acf, main="ACF- Total Debt Increase by Quarter")

pacf<-pacf(StudentLoans.ts)
plot(pacf, main="PACF- Total Debt Increase by Quarter")

pp.test(StudentLoans.ts)
adf.test(StudentLoans.ts)

autoarma<-auto.arima(StudentLoans$total_increase_in_debt)

print(autoarma)

autoarimaDebt<-auto.arima(StudLoanTrain.ts)
StudentLoants.grph<- StudentLoans.ts

print(StudentLoants.grph)

print(autoarimaDebt)

g_range1 <- range(0, StudentLoants.grph)


plot.ts(forecast(autoarimaDebt, h=20)$fitted, type="o", ylim=g_range1, ann=FALSE, 
        lwd=3, pch=22, lty=2, col="red")
lines(StudentLoans$observation_quarter, StudentLoans$total_increase_in_debt, ylim=g_range1, type="o", lwd=3, col="blue")
lines(forecast(autoarma, h=20)$fitted, ylim=g_range1, type="o", lwd=3, pch=21, lty=3, 
      col="green")
box()
title(main="Total Debt Increase by Quarter (in Millions)") 
title(xlab="Quarter") 
title(ylab="Change in Debt")
legend(1, g_range[2], c("Actual","ARIMA Forecast", "ARMA Forecast"), cex=0.8, 
       col=c("blue","red", "green"), pch=21:22, lty=1:2)

x<-StudentLoans.ts
y<- StudentLoans$observation_number
bc<-boxcox(x~1)
lam<-bc$x[which.max(bc$y)]
lam

print(bc)

truehist(BoxCox(x,lam), main="True Histograph of Increase in Debt (Box-Cox Transform)", xlab="Dollar Value of Increase in Debt (in MM, Box-Cox)", ylab="Frequency of Occurrences")
lines(density(BoxCox(x,lam)), col="red")
lines(density((BoxCox(x,lam)), adjust=2), lty="dotted")


BoxCoxPlot<-ts.plot(BoxCox(x,lam), main="Total Debt Increase by Quarter (in Millions, Box-Cox Transform)", 
                    xlab="Year", ylab="Change in Debt (Box-Cox)", col="blue")

sqrtStudLoans<-sqrt(StudentLoans$total_increase_in_debt)

StudentLoansBC.ts = ts(data=sqrtStudLoans, frequency = 4, 
                       start=c(2006,1), end=c(2015,2)) 

plot(StudentLoansBC.ts)


truehist(StudentLoansBC.ts, prob=TRUE, main="Histograph of Increase in Debt", 
         xlab="Dollar Value of Increase in Debt (in MM)", 
         ylab="Frequency of Occurrences")
lines(density(StudentLoansBC.ts))

qqnorm(StudentLoansBC.ts)
qqline(StudentLoansBC.ts)

lag1.plot(StudentLoansBC.ts, 9)

acf<-acf(StudentLoansBC.ts)
plot(acf, main="ACF- Total Debt Increase by Quarter")

pacf<-pacf(StudentLoansBC.ts)
plot(pacf, main="PACF- Total Debt Increase by Quarter")

pp.test(StudentLoansBC.ts)
adf.test(StudentLoansBC.ts)

plot(SLBC <- stl(StudentLoansBC.ts, "per"))  

autoarimaBC<-auto.arima(StudentLoansBC.ts )

print(autoarimaBC)

plot(StudentLoansBC.ts, type="o", col="blue", ann=FALSE)
lines(forecast(autoarimaBC, h=5)$fitted, type="o", col="red", pch=21, lty=2)
box()
title(main="Total Debt Increase by Quarter (in Millions)") 
title(xlab="Year") 
title(ylab="Change in Debt (Box-Cox)")
legend(1, 1, c("Actual","ARIMA"), cex=0.8, 
       col=c("blue","red"), pch=21, lty=1:2)

arimaBCgraph<- StudentLoansBC.ts(10, m=5)

t.test(sqrtStudLoans, lag=30)
acfsrtStudLoans<- acf((sqrtStudLoans), main="ACF- Student Loans")
acfsrtStudLoans<- acf(sqrtStudLoans, lag=30, main="ACF- Student Loans Lag 30")
pacf(sqrtStudLoans, lag=30, main="Partial ACF- Student Loans Lag 30")
Box.test(sqrtStudLoans, type='Ljung')

#ARCH


?axis

?plot

y=sqrtStudLoans-mean(sqrtStudLoans)
Box.test(y^2, lag=30, type='Ljung')
y1=(StudentLoansBC.ts-mean(StudentLoansBC.ts))^2
T=length(StudentLoansBC.ts)
atsq=y1[(m+1):T]
x=matrix(0,(T-m),m)
for (i in 1:m){
  x[,i]=y1[(m+1-i):(T-i)]
}
md=lm(atsq~x)
summary(md)


??adjust

dfsqrtLoans=as.data.frame(sqrtStudLoans) 

help(resid)

?lm

z<-as.character(StudentLoans$observation_quarter)

reg<-lm(sqrtStudLoans~values$quarter, na.action=NULL)
summary(reg)



res<-residuals(reg, standardize=TRUE)
ressq<-res^2
print(ressq)
plot.ts(ressq, type="o", col="blue", xaxt='no', ann=FALSE)
axis(1, at=1:38, lablels=c('2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015'))
title(main="Volatility based on Residuals Squared") 
title(xlab="Period") 
title(ylab="Residuals Squared")

summary(res)
hist(res[,"xu100"], breaks=100, main="Histogram of Estimated Coefficients", xlab="Value")
plot(res, main="Student Loans Residuals", ylab="Residuals", col="blue", type="o", adjust=2)
acf(res^2, lag=90, na.action = na.omit)
pacf(res^2, lag=90, na.action = na.omit)
Box.test(res, type='Ljung')
sqres<-res^2
print(sqres)

plot(sqres, col="blue", pch=19, lty=1, type="o", ann=FALSE)
title(main="Volatility based on residuals Squared") 
title(xlab="Year") 
title(ylab="Residuals Squared")

legend(100, 11, c("Actual","Auto Forecast"))

acf(sqres, na.action = na.omit)
pacf(sqres, na.action = na.omit)


ttest<-t.test(sqrtStudLoans, lag=90)
print(ttest)
mean(sqrtStudLoans)
sd(sqrtStudLoans)
skewness(sqrtStudLoans)
kurtosis(sqrtStudLoans)

z=sqrtStudLoans-mean(sqrtStudLoans)
Box.test(z, type='Ljung')

archTestStLoan1<- archTest(~1+garch(0,1), data=sqrtStudLoans, trace=FALSE)
archFITStLoan2<- archTest(~1+garch(0,2), data=sqrtStudLoans, trace=FALSE)
archFITStLoan3<- archTest(~1+garch(0,3), data=sqrtStudLoans, trace=FALSE)


sumarchFITLoan1<- summary(archFITStLoan1)
sumarchFITLoan2<- summary(archFITStLoan2)
sumarchFITLoan3<- summary(archFITStLoan3)




garchFITStLoan11<- garchFit(~garch(1,1), data=sqrtStudLoans, trace=FALSE)
garchFITStLoan12<- garchFit(~garch(1,2), data=sqrtStudLoans, trace=FALSE)
garchFITStLoan22<- garchFit(~garch(2,2), data=sqrtStudLoans, trace=FALSE)
garchFITStLoan13<- garchFit(~garch(1,3), data=sqrtStudLoans, trace=FALSE)
garchFITStLoan21<- garchFit(~garch(2,1), data=sqrtStudLoans, trace=FALSE)
garchFITStLoan20<- garchFit(~garch(2,0), data=sqrtStudLoans, trace=FALSE)
garchFITStLoan10<- garchFit(~garch(1,0), data=sqrtStudLoans, trace=FALSE)
garmaFITStLoan2110<- garchFit(~arma(2,1)+garch(1,0), data=sqrtStudLoans, trace=FALSE)
garmaFITStLoan1110<- garchFit(~arma(1,1)+garch(1,0), data=sqrtStudLoans, trace=FALSE)
garmaFITStLoan1210<- garchFit(~arma(1,2)+garch(1,0), data=sqrtStudLoans, trace=FALSE)


sumgarchFITLoan11<- summary(garchFITStLoan11)
sumgarchFITLoan12<- summary(garchFITStLoan12)
sumgarchFITLoan22<- summary(garchFITStLoan22)
sumgarchFITLoan13<- summary(garchFITStLoan13)
sumgarchFITLoan21<- summary(garchFITStLoan21)
sumgarchFITLoan20<- summary(garchFITStLoan20)
sumgarchFITLoan10<- summary(garchFITStLoan10)
sumgarmaFITLoan2110<-summary(garmaFITStLoan2110)
sumgarmaFITLoan1110<-summary(garmaFITStLoan1110)
sumgarmaFITLoan1210<-summary(garmaFITStLoan1210)

plot(garmaFITStLoan1210, which=10, ann=FALSE)
title(main="Student Loans Residuals")
plot(garmaFITStLoan1210, which=1)
plot(garmaFITStLoan1210, which=2)
plot(garmaFITStLoan1210, which=3)
plot(garmaFITStLoan1210, which=4)
plot(garmaFITStLoan1210, which=5)
plot(garmaFITStLoan1210, which=6)
plot(garmaFITStLoan1210, which=7)
plot(garmaFITStLoan1210, which=8)
plot(garmaFITStLoan1210, which=9)
plot(garmaFITStLoan1210, which=11, main="Student Loans Residuals")
plot(garmaFITStLoan1210, which=12)
plot(garmaFITStLoan1210, which=13, ann=FALSE)
title(main="Student Loans Residuals")

pacf(garmaFITStLoan1210, na.action = na.omit)

fittedSLoan1210<-fitted(sumgarmaFITLoan1210)

yline
plot(sumgarmaFITLoan1210)

garchLoan10<-garch(StudentLoansBC.ts, order= c(1,0))
sumgarchLoan10<-garch(StudentLoansBC.ts, order= c(1,0))



hist(garmaFITStLoan1210, breaks=100, main="Histogram of Estimated Coefficients", xlab="Value")
plot(StudentLoans$observation_quarter, garchLoan10, type='l')

truehist(garmaFITStLoan1210, prob=TRUE, main="Histograph of Increase in Debt", 
         xlab="Dollar Value of Increase in Debt (in MM)", 
         ylab="Frequency of Occurrences")

qqnorm(garmaFITStLoan1210)
qqline(garmaFITStLoan1210, col="blue")

knn(arimaBCgraph)
?legend


warnings()

