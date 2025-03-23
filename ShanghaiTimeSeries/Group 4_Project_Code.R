#load data
shanghai <- read.csv('/Users/meghananekkanti/Desktop/Spring /DNSC_6319_TSF/Project/Shanghai license plate price.csv')
head(shanghai)
nrow(shanghai)

#creating month column
n_rows <- nrow(shanghai)
shanghai$Month <- rep(1:12, length.out = n_rows)
shanghai
head(shanghai)
tail(shanghai)
attach(shanghai)



#SECTION 1
#create df with just avg price and month
new_df = data.frame(avg.price = shanghai$avg.price, Month = shanghai$Month)
head(new_df)


#time series plot
avg_price = ts(data=new_df, frequency=12) #creating data for time series 
ts.plot(avg_price,col="violetred4",lwd="2",ylab="Average Plate Price", xlab="Month",
        main="Monthly Average Plate Price from January 2002-December 2018")


#box plot
boxplot(shanghai$avg.price~shanghai$Month,ylab="Average Plate Price", xlab="Month",
        main="Monthly Average Plate Price from January 2002-December 2018", 
        col="palegreen4")


#periodogram
library(TSA)
time1<-seq(1, length(shanghai$avg.price))
detrend<-lm(shanghai$avg.price~time1)


#frequencies
prdgrm=periodogram(detrend$residuals,col="mediumblue")
frequency=prdgrm$freq
amplitude=prdgrm$spec


#periods
period=1/prdgrm$freq
par(mfrow=c(1,2))
plot(period,prdgrm$spec, type="h",col="mistyrose4",ylab="Peridogram",lwd=2)


#harmonics
all=cbind(period,frequency,amplitude)
all <- cbind(harmonic = 1:nrow(all), all)
all
head(all[order(-all[, "amplitude"]), ],7)



#SECTION 2
n_avgprice <- avg.price
n_month <- Month
time=seq(1, length(n_avgprice))
n=length(n_avgprice)
time=c(1:n)


#create dummy variables for drops which represent introduction of additional license plates
d1=rep(0,length(n_avgprice))
d2=rep(0,length(n_avgprice))
d3=rep(0,length(n_avgprice))
d4=rep(0,length(n_avgprice))
d5=rep(0,length(n_avgprice))
for (i in 1:length(n_avgprice)){
  if (time[i]<29) d1[i]=1 else d1[i]=0
  if (time[i]>28 & time[i]<73) d2[i]=1 else d2[i]=0
  if (time[i]==107) d3[i]=1 else d3[i]=0
  if (time[i]==134) d4[i]=1 else d4[i]=0
  if (time[i]==141) d5[i]=1 else d5[i]=0
}


#Model without trend

#using a training sample of 180
nT=180
nd1=d1[1:nT]
nd2=d2[1:nT]
nd3=d3[1:nT]
nd4=d4[1:nT]
nd5=d5[1:nT]
n_avgprice = avg.price[1:180]
n_Month = Month[1:180]
time = c(1:180)

#fit
fit_woT = lm(n_avgprice~poly(time, 20)+nd1+nd2+nd3+nd4+nd5)
summary(fit_woT)
ts.plot(n_avgprice,col="slateblue3")
lines(fitted.values(fit_woT),col="red4")
sd(fit_woT$residuals)

#predict
pred_woT <- predict(fit_woT, data.frame(n_avgprice = avg.price[181:204],
                                      time =c(181:204), nd1 = d1[181:204], nd2 = d2[181:204], 
                                      nd3 = d3[181:204],
                                      nd4 = d4[181:204],
                                      nd5 = d5[181:204]))
mean_woT = mean(abs(avg.price[181:204] - pred_woT)/ avg.price[181:204])
mean_woT


#Model with trend

#using a training sample of 180
nT=180
nd1=d1[1:nT]
nd2=d2[1:nT]
nd3=d3[1:nT]
nd4=d4[1:nT]
nd5=d5[1:nT]
n_avgprice = avg.price[1:180]
n_Month = Month[1:180]
time = c(1:180)

#fit
fit_wT = lm(n_avgprice~time+as.factor(n_Month)+nd1+nd2+nd3+nd4+nd5)
summary(fit_wT)
ts.plot(n_avgprice,col="slateblue3")
lines(fitted.values(fit_wT),col="red4")
sd(fit_wT$residuals)

#predict
pred_wT <- predict(fit_wT, data.frame(n_avgprice = avg.price[181:204], n_Month = Month[181:204],
                                      time =c(181:204), nd1 = d1[181:204], nd2 = d2[181:204], 
                                      nd3 = d3[181:204],
                                      nd4 = d4[181:204],
                                      nd5 = d5[181:204]))
mean_wT = mean(abs(avg.price[181:204] - pred_wT )/ avg.price[181:204])
mean_wT


#Cyclic model
nT=180
nd1=d1[1:nT]
nd2=d2[1:nT]
nd3=d3[1:nT]
nd4=d4[1:nT]
nd5=d5[1:nT]

n_avgprice = avg.price[1:180]
n_Month = Month[1:180]
time = c(1:180)

cos1=cos(2*pi*(1/12)*time)
sin1=sin(2*pi*(1/12)*time)
cos2=cos(2*pi*(2/12)*time)
cos4=cos(2*pi*(4/12)*time)
sin4=sin(2*pi*(4/12)*time)
cos6=cos(2*pi*(6/12)*time)
sin6=sin(2*pi*(6/12)*time)
cos9=cos(2*pi*(9/12)*time)
sin9=sin(2*pi*(9/12)*time)
cos3=cos(2*pi*(3/12)*time)
sin3=sin(2*pi*(3/12)*time)

#fit
fit_cyc <- lm(n_avgprice~time+cos1+sin1+cos2+cos4+sin4+cos6+sin6+cos9+
                sin9+cos3+sin3+nd1+nd2+nd3+nd4+nd5)
summary(fit_cyc)
ts.plot(n_avgprice,col="slateblue3")
lines(fitted.values(fit_cyc),col="red4")
sd(fit_cyc$residuals)

#predict
pred_cyc <- predict(fit_cyc, data.frame(n_avgprice = avg.price[181:204],
                                      time =c(181:204), nd1 = d1[181:204], nd2 = d2[181:204], 
                                      nd3 = d3[181:204],
                                      nd4 = d4[181:204],
                                      nd5 = d5[181:204],cos1=cos(2*pi*(1/12)*c(181:204)),
                                      cos1=cos(2*pi*(1/12)*c(181:204)),
                                      sin1=sin(2*pi*(1/12)*c(181:204)), cos2=cos(2*pi*(2/12)*c(181:204)),
                                      cos4=cos(2*pi*(4/12)*c(181:204)), sin4=sin(2*pi*(4/12)*c(181:204)),
                                      cos6=cos(2*pi*(6/12)*c(181:204)), sin6=sin(2*pi*(6/12)*c(181:204)),
                                      cos9=cos(2*pi*(9/12)*c(181:204)),
                                      sin9=sin(2*pi*(9/12)*c(181:204)), cos3=cos(2*pi*(3/12)*c(181:204)),
                                      sin3=sin(2*pi*(3/12)*c(181:204))))
mean_cyc = mean(abs(avg.price[181:204] - pred_cyc )/ avg.price[181:204])
mean_cyc


#2.3

#residuals for model w trend
acf(fit_wT$resid, main="ACF of the residuals from the Model with Trend Model",col="plum4")
Box.test(fit_wT$resid,lag=20) 

#residuals for cyclic model
acf(fit_cyc$resid, main="ACF of the residuals from the Cyclic Model",col="plum4")
Box.test(fit_cyc$resid,lag=20) 



#SECTION 3

#scatter plots
plot(shanghai$Total.number.of.license.issued, shanghai$avg.price, 
     main = "Scatter Plot of Average Price vs. Total No. of Plates Issues", xlab = "Total No. of Plates Issues", ylab = "Average Price")
plot(shanghai$lowest.price, shanghai$avg.price, main = "Scatter Plot of Average Price vs. Lowest Price", 
     xlab = "Lowest Price", ylab = "Average Price")
plot(shanghai$Total.number.of.applicants, shanghai$avg.price, main = "Scatter Plot of Average Price vs. Total No. of Applicants", 
     xlab = "Total No. of Applicants", ylab = "Average Price")

#log transformation
shanghai$log_Total_number_of_applicants <- log(shanghai$Total.number.of.applicants)
head(shanghai)
plot(shanghai$log_Total_number_of_applicants, shanghai$avg.price, main = "Scatter Plot of Average Price vs. Total No. of Applicants", 
     xlab = "Total No. of Applicants(Log Transformed)", ylab = "Average Price")

#correlation
cor(shanghai)

attach(shanghai)

#variables
n_avg = avg.price[1:180]
n_month = Month[1:180]
n_issued = Total.number.of.license.issued[1:180]
n_lowest = lowest.price[1:180]
n_applicants = log_Total_number_of_applicants[1:180]


#mlr - all 3
fit1 <- lm(n_avg~n_issued+n_lowest+n_applicants)
summary(fit1)

#fit 
ts.plot(n_avgprice,col="violetred3",lwd =3)
lines(fitted.values(fit1),col="lightblue3", lwd=2)
sd(fit1$residuals)

#predict
ppp <- predict(fit1, data.frame(n_avg = avg.price[181:204],
                                   n_issued = Total.number.of.license.issued[181:204],
                                   n_lowest = lowest.price[181:204],
                                   n_applicants = log_Total_number_of_applicants[181:204]))
mape=mean(abs(avg.price[181:204]-ppp)/avg.price[181:204])
mape


#mlr - w/o lowest price
fit2 <- lm(n_avg~n_month+n_issued+n_applicants)
summary(fit2)

#fit 
ts.plot(n_avgprice,col="violetred3",lwd =3)
lines(fitted.values(fit2),col="lightblue3", lwd=2)
sd(fit2$residuals)

#predict
ppp2 <- predict(fit2, data.frame(n_avg = avg.price[181:204], n_month = Month[181:204],
                                n_issued = Total.number.of.license.issued[181:204],
                                n_lowest = lowest.price[181:204],
                                n_applicants = log_Total_number_of_applicants[181:204]))
mape=mean(abs(avg.price[181:204]-ppp2)/avg.price[181:204])
mape

#acf
#residuals for cyclic model
acf(fit2$resid, main="ACF of the residuals from the MLR model with the 3 Independent variables",col="darkorange2")
Box.test(fit2$resid,lag=20) 



#SECTION 4.1
acf(fit_wT$resid, main="ACF of the Model with Trend",col="thistle3", lwd = 3) 
pacf(fit_wT$resid, main="PACF of the Model with Trend",col="chartreuse4", lwd = 2)

n_avgprice <- avg.price

#ar process
library(forecast)
four.1_ar <- Arima(n_avgprice, order = c(2, 0, 0))
summary(four.1_ar)
four.1_ar$coef
phi=four.1_ar$coef
mu=phi[2]
C=mu*(1-phi[1])
C

#coeffs
se_phi=rep(0,4)
se_phi[1]=(four.1_ar$var.coef[1,1])^0.5
se_phi
t_stat=phi/se_phi
t_stat

#acf + box test
acf(four.1_ar$resid, main="ACF of the residuals from the AR(2) model",col="cadetblue", lwd =3)
Box.test(four.1_ar$resid,lag=20)



#SECTION 4.2

acf(fit1$resid, main="ACF of the Regression Model",col="thistle3", lwd = 3) 
pacf(fit1$resid, main="PACF of the Regression Mode",col="chartreuse4", lwd = 2) 

#differencing 1
x = cbind(Total.number.of.license.issued, lowest.price, Total.number.of.applicants)
regdif <- arima(n_avgprice, order = c(0,1,0), xreg = x) 
acf(regdif$residuals, main="ACF of the differenced series", col = 'firebrick1', lwd = 2)
pacf(regdif$residuals, main="PACF of the differenced series", col = 'firebrick1', lwd = 2)

#corrected model
regarima <- arima(n_avgprice, order = c(0,1,1), xreg = x) 
regarima

#acf + box test
acf(regarima$resid, main="ACF of the residuals from the corrected regression model",col="cadetblue", lwd =3)
Box.test(regarima$resid,lag=20)



#SECTION 4.3
#create df with just avg price and month
new_df = data.frame(avg.price = avg.price, Month = Month)
head(new_df)

#ts data
avg_price = ts(data=new_df, frequency=12) 

#ts plot
ts.plot(avg_price,col="slategrey",lwd="2",ylab="Average Plate Price",
        main="Monthly Average Plate Price from January 2002-January 2019")

#acf of regular series
acf(avg.price, lwd = 2)

#acf & pacf of di
par(mfrow=c(1,2))
acf(diff(avg.price),lag=24,col="thistle3", lwd = 2)
pacf(diff(avg.price),lag=24,col="chartreuse4", lwd =2)

#arima model
library(forecast)
four.3_arima <- Arima(avg.price, order = c(1, 1, 1))
summary(four.3_arima)

#acf + box test
par(mfrow=c(1,1))
acf(four.3_arima$residuals,col="steelblue3", lwd =2)
Box.test(four.3_arima$residuals,lag=20)
