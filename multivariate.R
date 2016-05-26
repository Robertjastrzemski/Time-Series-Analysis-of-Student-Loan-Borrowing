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
require(vars)
require(timeSeries)
require(stats)

values<-read.csv("C:/Users/robj_01/Desktop/PRED_413/Case_1/wk8/change_in_student_loans.csv", header=T, dec=".", na.strings="0")

save(values, file='C:/Users/robj_01/Desktop/PRED_413/Case_1/wk8/StudentLoans.rda')
load('StudentLoans.rda')
StudentLoans<-values
typeof(StudentLoans)
is.data.frame(StudentLoans)

values2<-read.csv("C:/Users/robj_01/Desktop/PRED_413/Case_1/wk8/Unemployment_25_to_34_yos.csv", header=T, dec=".", na.strings="0")

save(values2, file='C:/Users/robj_01/Desktop/PRED_413/Case_1/wk8/Unemployment.rda')
dir()
load('values2')
Unemployment<-values2
typeof(Unemployment)
is.data.frame(Unemployment)

head(StudentLoans)
head(Unemployment)

StudentLoans.ts = ts(data=StudentLoans$total_increase_in_debt, frequency = 4, 
                     start=c(2006,1), end=c(2015,2))

print(StudentLoans.ts)

total_increase_in_debt<-StudentLoans$total_increase_in_debt

Unemployment.ts = ts(data=Unemployment$Percent_Change, frequency = 4, 
                     start=c(2006,1), end=c(2015,2))

print(Unemployment.ts)



plot(Unemployment.ts, type="o",  
      ann=FALSE, lwd=3, col="red")
title(main="Total Change in Unemployment by Quarter") 
title(xlab="Year") 
title(ylab="Change in Unemployment")
plot(StudentLoans.ts, type="o",  
      ann=FALSE, lwd=3, col="blue")
title(main="Total Debt Increase by Quarter (in Millions)") 
title(xlab="Year") 
title(ylab="Change in Debt")

adf.test(Unemployment$Percent_Change, k=1)

ndiffs(StudentLoans$total_increase_in_debt,alpha=0.05,test=c("adf"))

ndiffs(Unemployment$Percent_Change,alpha=0.05,test=c("adf"))

d.StudentLoans = diff(StudentLoans$total_increase_in_debt)

d.Unemployment = diff(Unemployment$Percent_Change)

Students.ts = cbind(d.StudentLoans, d.Unemployment)


var = VAR(Students.ts, p=1)

VARselect(Students.ts, type = "const")$selection

var1 = VAR(Students.ts, p=1)
serial.test(var1, lags.pt=10, type="PT.asymptotic")

var3 = VAR(Students.ts, p=3)
serial.test(var3, lags.pt=10, type="PT.asymptotic")

var5 = VAR(Students.ts, p=5)
serial.test(var5, lags.pt=10, type="PT.asymptotic")

var4 = VAR(Students.ts, p=4)
serial.test(var4, lags.pt=10, type="PT.asymptotic")

var3L90 = VAR(Students.ts, p=3)
serial.test(var3, lags.pt=90, type="PT.asymptotic")

varStudents<-VAR(y = Students.ts, p = 1)
print(varStudents)

summary(var1, equation="d.StudentLoans")
summary(var3, equation="d.StudentLoans")
summary(var5, equation="d.StudentLoans")
summary(var4, equation="d.StudentLoans")
summary(var3L90, equation="d.StudentLoans")

?grangertest

grangertest(d.StudentLoans ~ d.Unemployment, order=1)
grangertest(d.Unemployment~ d.StudentLoans, order=1, na.action=na.omit)

predict(var1, n.ahead=6, ci=0.95)
predict(var3L90, n.ahead=6, ci=0.95)


sineStudentLoans<-sin(StudentLoans.ts)
plot(sineStudentLoans, type="o",  
     ann=FALSE, lwd=3, col="red")
title(main="Total Sine Debt Change by Quarter") 
title(xlab="Year") 
title(ylab="Change in Debt")


ndiffs(sineStudentLoans.ts,alpha=0.05,test=c("adf"))

ndiffs(Unemployment$Percent_Change,alpha=0.05,test=c("adf"))

mxStudentLoans<-as.matrix(as.data.frame(sineStudentLoans.ts))

d.sinStudentLoans = diff(sineStudentLoans.ts)

d.Unemployment = diff(Unemployment$Percent_Change)

sineStudents.ts = cbind(d.sinStudentLoans, d.Unemployment)

var0sin = VAR(sineStudents.ts, p=0)
serial.test(var0sin, lags.pt=10, type="PT.asymptotic")

var1sin = VAR(sineStudents.ts, p=1)
serial.test(var1sin, lags.pt=10, type="PT.asymptotic")

summary(var0sin, equation="d.sinStudentLoans")
summary(var1sin, equation="d.sinStudentLoans")



fcst=forecast(var1)
plot(fcst)
