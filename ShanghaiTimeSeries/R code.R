#load data
shangai <- read.csv('/Users/meghananekkanti/Desktop/Spring /DNSC_6319_TSF/Project/Shanghai license plate price.csv')
head(shangai)
nrow(shangai)

#creating month column
n_rows <- nrow(shangai)
shangai$Month <- rep(1:12, length.out = n_rows)
shangai
head(shangai)
tail(shangai)



#SECTION 1
#create df with just avg price and month
new_df = data.frame(avg.price = shangai$avg.price, Month = shangai$Month)
head(new_df)


#time series plot
avg_price = ts(data=new_df, frequency=12) #creating data for time series 
ts.plot(avg_price,col="violetred4",lwd="2",ylab="Average Plate Price", xlab="Month",
        main="Monthly Average Plate Price from January 2002-December 2018")


#box plot
boxplot(shangai$avg.price~shangai$Month,ylab="Average Plate Price", xlab="Month",
        main="Monthly Average Plate Price from January 2002-December 2018", 
        col="palegreen4")


#periodogram
library(TSA)
time1<-seq(1, length(shangai$avg.price))
detrend<-lm(shangai$avg.price~time1)


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
n_avgprice <- shangai$avg.price
n_month <- shangai$Month
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

#using a training sample of 180
nT=204
nd1=d1[1:204]
nd2=d2[1:204]
nd3=d3[1:204]
nd4=d4[1:204]
nd5=d5[1:204]


#Model without trend
fit_woT = lm(n_avgprice[1:180]~time[1:180]+nd1[1:180]+nd2[1:180]+nd3[1:180]+nd4[1:180]+nd5[1:180])
summary(fit_woT)
ts.plot(n_avgprice,col="slateblue3")
lines(fitted.values(fit_woT),col="red4")

#mape w holdout
newdata <- data.frame(
  n_month = shangai$Month[181:204],
  time = c(181:204),
  nd1 = d1[181:204],
  nd2 = d2[181:204],
  nd3 = d3[181:204],
  nd4 = d4[181:204],
  nd5 = d5[181:204]
)
#predict using the newdata
pred_woT <- predict(fit_woT, newdata, interval="prediction")
