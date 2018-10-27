#loading necessary libraries
library("ggplot2")
library("forecast")
require("graphics")

# Loading the data
Gstore<- read.csv("Global Superstore.csv")
str(Gstore)

#Check for missing values
sum(is.na(Gstore))

# Finding which column has NA values
list.NA<-""
for (i in c(1:ncol(Gstore)))
{
  len<-length(grep("TRUE",is.na(Gstore[,i])))
  if(len > 0){
    list.NA<-paste(colnames(Gstore[i]),":",len,list.NA)
  }
}
list.NA # Postal.Code has 41296 NA values

subset(Gstore, is.na(Postal.Code)) 
# It is observed that postal code is not available for non-USA addresses# check duplicated

#Check duplicate
which(duplicated(Gstore))
# no duplicate values

#Data Preparation
str(Gstore)

# Data types update
# 1. Change Order.Date from factor to Date type
# 2. Change Ship.Date from factor to Date type
# 3. Change Postal.Code from int to factor

Gstore$Order.Date<-as.Date(Gstore$Order.Date,"%d-%m-%Y")
Gstore$Ship.Date<-as.Date(Gstore$Ship.Date,"%d-%m-%Y")
Gstore$Postal.Code<-as.factor(Gstore$Postal.Code)

# Sustetting the data based on Market and segment

levels(Gstore$Segment) # 3 Segments:"Consumer" "Corporate" "Home Office"
levels(Gstore$Market) # 7 Markets:"Africa" "APAC" "Canada" "EMEA" "EU" "LATAM" "US" 

# Analysis of TotalProfit, AvgSalesAmount, TotalSalesAmount, NumOfSales on Market and Segment
w<-aggregate(Gstore$Profit, by=list(Gstore$Market,Gstore$Segment), FUN=sum)
names(w)<-list("Market","Segment","TotalProfit")
x<-aggregate(Gstore$Sales, by=list(Gstore$Market,Gstore$Segment), FUN=mean)
names(x)<-list("Market","Segment","AvgSalesAmount")
y<-aggregate(Gstore$Sales, by=list(Gstore$Market,Gstore$Segment), FUN=sum)
names(y)<-list("Market","Segment","TotalSalesAmount")
z<-aggregate(Gstore$Sales, by=list(Gstore$Market,Gstore$Segment), FUN=length)
names(z)<-list("Market","Segment","NumOfSales")

SalesTotal<-data.frame(w,x,y,z)
SalesTotal<-SalesTotal[,c(1,2,3,6,9,12)]

# Adding Profit as a percentage of Sales amount to the dataframe
SalesTotal$ProfitPercent<-(SalesTotal$TotalProfit/SalesTotal$TotalSalesAmount)*100

# Analysis of TotalMonthlyProfit, AvgMonthlySales, TotalMonthlySales, NoOfMonthlySales on Market, Segment, Month and Year 

v1<-aggregate(Gstore$Quantity, by=list(Gstore$Market,Gstore$Segment,format(as.Date(Gstore$Order.Date), "%Y%m")), FUN=sum)
names(v1)<-list("Market","Segment","Month","TotalMonthlyQty")
w1<-aggregate(Gstore$Profit, by=list(Gstore$Market,Gstore$Segment,format(as.Date(Gstore$Order.Date), "%Y%m")), FUN=mean)
names(w1)<-list("Market","Segment","Month","TotalMonthlyProfit")
x1<-aggregate(Gstore$Sales, by=list(Gstore$Market,Gstore$Segment,format(as.Date(Gstore$Order.Date), "%Y%m")), FUN=mean)
names(x1)<-list("Market","Segment","Month","AvgMonthlySales")
y1<-aggregate(Gstore$Sales, by=list(Gstore$Market,Gstore$Segment,format(as.Date(Gstore$Order.Date), "%Y%m")), FUN=sum)
names(y1)<-list("Market","Segment","Month","TotalMonthlySales")
z1<-aggregate(Gstore$Sales, by=list(Gstore$Market,Gstore$Segment,format(as.Date(Gstore$Order.Date), "%Y%m")), FUN=length)
names(z1)<-list("Market","Segment","Month","NoOfMonthlySales")

SalesMonthly<-data.frame(v1,w1,x1,y1,z1)
SalesMonthly<-SalesMonthly[,c(1,2,3,4,8,12,16,20)]

# Add Monthly profit as a percentage of monthly sales
SalesMonthly$MonthlyProfitPercent<-(SalesMonthly$TotalMonthlyProfit/SalesMonthly$TotalMonthlySales)*100
SalesMonthly<-SalesMonthly[order(SalesMonthly$TotalMonthlySales,decreasing=TRUE),]

# Finding standard deviation of monthly profit percentage across Market and Segment
y2<-aggregate(SalesMonthly$MonthlyProfitPercent, by=list(SalesMonthly$Market,SalesMonthly$Segment), FUN=mean)
names(y2)<-list("Market","Segment","AvgMonthlyProfitPercent")
z2<-aggregate(SalesMonthly$MonthlyProfitPercent, by=list(SalesMonthly$Market,SalesMonthly$Segment), FUN=sd)
names(z2)<-list("Market","Segment","MonthlyProfitPercentSD")

# Adding SD of monthly profit percentage to the SalesTotal
SalesTotal<-data.frame(SalesTotal,y2,z2)
SalesTotal<-SalesTotal[,-c(8,9,11,12)]
SalesTotal$CVMonthlyProfitPercent<-SalesTotal$MonthlyProfitPercentSD/SalesTotal$AvgMonthlyProfitPercent

# Order on Total profit to find the most profitable and consistent Market Segment 
SalesTotal<-SalesTotal[order(SalesTotal$TotalProfit,decreasing=TRUE),]

# Plotting Market,Segment Vs. Total Profit

# Market and Segment generating most profit
plot1<-ggplot(SalesTotal,aes(x=Market,y=TotalProfit,fill=Segment))
plot1+geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("Profit")+ggtitle("Total Profit")

# Market and Segment having most profit margin
plot2<-ggplot(SalesTotal,aes(x=Market,y=ProfitPercent,fill=Segment))
plot2+geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("Profit %age")+ggtitle("Profit percent")

# Market and Segment having most profit margin
plot3<-ggplot(SalesTotal,aes(x=Market,y=CVMonthlyProfitPercent,fill=Segment))
plot3<-plot3+geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("Coeff. of variance of monthly profit")
plot3+ggtitle("Coeff. of variance in monthly profit Vs. Market Segment")

grid.arrange(plot1, plot2, plot3)

# Based on the maximum profits and consistent profit month on month, we have chosen these Market Segements
# 1. APAC Consumer
# 2. EU Consumer

# US Consumer also has high profits but the coefficient of variance of profit percentage month on month is high
# Therefore we are ignoring it

############################ Model building ###############################################

--------------------------- APAC Consumer -------------------------------------------------

APACConsumer<-subset(SalesMonthly,(SalesMonthly$Market=="APAC")
                              &(SalesMonthly$Segment=="Consumer"))
APACConsumer<-APACConsumer[order(APACConsumer$Month),]
APACConsumer$MonthNum<-c(1:nrow(APACConsumer))  

# Preparing Time series data
APACConsumerProfit.TS<-APACConsumer[,c("MonthNum","TotalMonthlyProfit")]
APACConsumerQty.TS<-APACConsumer[,c("MonthNum","TotalMonthlyQty")]  

# Separating test data
RowCount<-nrow(APACConsumer)
APACConsumerProfit.TS.test<-APACConsumerProfit.TS[(RowCount-5):RowCount,] # Test data
APACConsumerProfit.TS<-APACConsumerProfit.TS[1:(RowCount-6),]
APACConsumerQty.TS.test<-APACConsumerQty.TS[(RowCount-5):RowCount,] # Test data
APACConsumerQty.TS<-APACConsumerQty.TS[1:(RowCount-6),]

xcol<-c(1)
ycol<-c(2)

#################### Plotting timeseries data for Profit ######################################

APAC.Consumer.Profit.timeser<-ts(APACConsumerProfit.TS[,ycol[1]])

# Decompose timeseries to see the components
# Added frequency = 10 because decomposition can't happen with frequency = 1
APAC.Consumer.Profit.timeser.d<-ts(APACConsumerProfit.TS[,ycol[1]],frequency=10)
APAC.Consumer.Profit.timeser.decompose <- decompose(APAC.Consumer.Profit.timeser.d)
plot(APAC.Consumer.Profit.timeser.decompose)

# Decomposotion showed that:
# 1. Trend in a high wavelength sine curve
# 2. Seasonality is a low wavelength sine curve

# Plot the timeseries
plot(APAC.Consumer.Profit.timeser)

# Smoothening the curve 
w <-1
APAC.Consumer.Profit.timeser.smooth <- filter(APAC.Consumer.Profit.timeser, 
                                              filter=rep(1/(2*w+1),(2*w+1)), 
                                              method='convolution', sides=2)
diff <- APAC.Consumer.Profit.timeser.smooth[w+2] - APAC.Consumer.Profit.timeser.smooth[w+1]
for (i in seq(w,1,-1)) {
  APAC.Consumer.Profit.timeser.smooth[i] <- APAC.Consumer.Profit.timeser.smooth[i+1] - diff
}
n <- length(APAC.Consumer.Profit.timeser)

timevals <- APACConsumerProfit.TS[[xcol[1]]]
timevals.test <- APACConsumerProfit.TS.test[[xcol[1]]]
diff <- APAC.Consumer.Profit.timeser.smooth[n-w] - APAC.Consumer.Profit.timeser.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC.Consumer.Profit.timeser.smooth[i] <- APAC.Consumer.Profit.timeser.smooth[i-1] + diff
}
lines(APAC.Consumer.Profit.timeser.smooth, col="blue", lwd=2)

APAC.Consumer.Profit.timeser.smoothdf <- as.data.frame(cbind(timevals, as.vector(APAC.Consumer.Profit.timeser.smooth)))
colnames(APAC.Consumer.Profit.timeser.smoothdf) <- c('MonthNum', 'Profit')
APAC.Consumer.Profit.timeser.smoothdf$MonthNum<-as.numeric(APAC.Consumer.Profit.timeser.smoothdf$MonthNum)
APAC.Consumer.Profit.timeser.smoothdf$Profit<-as.numeric(APAC.Consumer.Profit.timeser.smoothdf$Profit)
str(APAC.Consumer.Profit.timeser.smoothdf)

str(APACConsumerProfit.TS)

lmfit <- lm(APAC.Consumer.Profit.timeser.smoothdf$Profit ~ sin(0.5*APAC.Consumer.Profit.timeser.smoothdf$MonthNum) *
              poly(APAC.Consumer.Profit.timeser.smoothdf$MonthNum,2) 
            + cos(0.5*APAC.Consumer.Profit.timeser.smoothdf$MonthNum) * 
              poly(APAC.Consumer.Profit.timeser.smoothdf$MonthNum,2)
            + sin(0.05*APAC.Consumer.Profit.timeser.smoothdf$MonthNum)*
              APAC.Consumer.Profit.timeser.smoothdf$MonthNum, 
            data=APAC.Consumer.Profit.timeser.smoothdf)
summary(lmfit)
accuracy(lmfit)


trend <- predict(lmfit, data.frame(x=timevals))

lines(timevals, trend, col="red", lwd=2)

############### Manual Arima ###############################################################
#-------------
resi <- APAC.Consumer.Profit.timeser - trend
plot(resi, col='red')

acf(resi)
acf(resi, type="partial")
par("mar") #5.1 4.1 4.1 2.1
par(mar=c(1,1,1,1))
armafit <- auto.arima(resi)
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit #ARIMA(0,0,0) with zero mean  

############################ Auto Arima #######################################################

autoarima <- auto.arima(APAC.Consumer.Profit.timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
accuracy(autoarima)

#--------------------- Forecasting APAC Consumer Profit------------------

APACConProfitForecast <- HoltWinters(APAC.Consumer.Profit.timeser, beta=FALSE, gamma=FALSE)
APACConProfitForecast
plot(APACConProfitForecast)

APACConProfitForecast6months <- forecast.HoltWinters(APACConProfitForecast,h=6)
APACConProfitForecast6months
plot(APACConProfitForecast6months)

# Comparing with Test data
accuracy(APACConProfitForecast6months,APAC.Consumer.Profit.TS.test$TotalMonthlyProfit)

#-----Using Forecast function to calculate next six month profit-----------
forecast(APAC.Consumer.Profit.timeser,h=6)
forecast(autoarima,h=6)

#--------------------- Forecasting EU_CORPORATE_Sales----------------------

# Plotting timeseries data for Quantity
#---------------------------------------
APAC.Consumer.Qty.timeser<-ts(APACConsumerQty.TS[,ycol[1]])

# Decompose timeseries to see the components
# Added frequency = 12 because decomposition can't happen with frequency = 1
APAC.Consumer.Qty.timeser.d<-ts(APACConsumerQty.TS[,ycol[1]],frequency=12)
APAC.Consumer.Qty.timeser.decompose <- decompose(APAC.Consumer.Qty.timeser.d)
plot(APAC.Consumer.Qty.timeser.decompose)
# Decomposotion showed that:
# 1. Trend in a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

# Plot the timeseries
plot(APAC.Consumer.Qty.timeser)

# Smoothening the curve 
w <-1
APAC.Consumer.Qty.timeser.smooth <- filter(APAC.Consumer.Qty.timeser, 
                                           filter=rep(1/(2*w+1),(2*w+1)), 
                                           method='convolution', sides=2)
diff <- APAC.Consumer.Qty.timeser.smooth[w+2] - APAC.Consumer.Qty.timeser.smooth[w+1]
for (i in seq(w,1,-1)) {
  APAC.Consumer.Qty.timeser.smooth[i] <- APAC.Consumer.Qty.timeser.smooth[i+1] - diff
}
n <- length(APAC.Consumer.Qty.timeser)

timevals <- APACConsumerQty.TS[[xcol[1]]]
diff <- APAC.Consumer.Qty.timeser.smooth[n-w] - APAC.Consumer.Qty.timeser.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC.Consumer.Qty.timeser.smooth[i] <- APAC.Consumer.Qty.timeser.smooth[i-1] + diff
}
lines(APAC.Consumer.Qty.timeser.smooth, col="blue", lwd=2)

APAC.Consumer.Qty.timeser.smoothdf <- as.data.frame(cbind(timevals, as.vector(APAC.Consumer.Qty.timeser.smooth)))
colnames(APAC.Consumer.Qty.timeser.smoothdf) <- c('MonthNum', 'Profit')
APAC.Consumer.Qty.timeser.smoothdf$MonthNum<-as.numeric(APAC.Consumer.Qty.timeser.smoothdf$MonthNum)
APAC.Consumer.Qty.timeser.smoothdf$Profit<-as.numeric(APAC.Consumer.Qty.timeser.smoothdf$Profit)
str(APAC.Consumer.Qty.timeser.smoothdf)

lmfit <- lm(APAC.Consumer.Qty.timeser.smoothdf$Profit ~ sin(0.6*APAC.Consumer.Qty.timeser.smoothdf$MonthNum) *
              poly(APAC.Consumer.Qty.timeser.smoothdf$MonthNum,2) 
            + cos(0.6*APAC.Consumer.Qty.timeser.smoothdf$MonthNum) * 
              poly(APAC.Consumer.Qty.timeser.smoothdf$MonthNum,2) +
              + sin(0.05*APAC.Consumer.Qty.timeser.smoothdf$MonthNum)*
              APAC.Consumer.Qty.timeser.smoothdf$MonthNum, 
            data=APAC.Consumer.Qty.timeser.smoothdf)
summary(lmfit)

trend <- predict(lmfit, data.frame(x=timevals))

trend2<-predict(lmfit, data.frame(x=c(47,48)))

lines(timevals, trend, col="red", lwd=2)

############################ Manual Arima ###################################################
#-------------
resi <- APAC.Consumer.Qty.timeser - trend
plot(resi, col='red')

acf(resi)
acf(resi, type="partial")

par("mar") #5.1 4.1 4.1 2.1
armafit <- auto.arima(resi)
par(mar=c(1,1,1,1))
tsdiag(armafit)#--------------------- Forecasting APAC Consumer Quantity------------------

APACConQtyForecast <- HoltWinters(APAC.Consumer.Qty.timeser, beta=FALSE, gamma=FALSE)
APACConQtyForecast
plot(APACConQtyForecast)

APACConQtyForecast6months <- forecast(APACConQtyForecast,h=6)
APACConQtyForecast6months
plot(APACConQtyForecast6months)
# Comparing with Test data
accuracy(APACConQtyForecast6months,APACConsumerQty.TS.test$TotalMonthlyProfit)

#-----Using Forecast function to calculate next six month quantity-----------
forecast(APAC.Consumer.Qty.timeser,h=6)
forecast(autoarima,h=6)
par(mar=c(5.1,4.1,4.1,2.1))
armafit #ARIMA(0,0,0) with zero mean   

# Auto Arima
#-------------
autoarima <- auto.arima(APAC.Consumer.Qty.timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")


