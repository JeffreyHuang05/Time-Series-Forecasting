#Arima forecasting with precipitation
library(RcppCNPy)
thedata <- npyLoad("C:\\Users\\jeffr\\OneDrive\\Documents\\thedata.npy",type="numeric",dotranspose=TRUE)
dim(thedata)

x<-array(thedata,dim=c(2881,66,77))
x1<-array(thedata,dim=c(2881,77,66))
data<-apply(x,1,mean)
data1<-apply(x1,1,mean)
Box.test(data,lag=1,type = "Ljung-Box")
acf(data)
pacf(data)
adf.test(data,alternative = "stationary")
library(TSA)
p_temp=periodogram(data)
dd_temp= data.frame(freq=p_temp$freq, spec=p_temp$spec)
order_temp = dd_temp[order(-dd_temp$spec),]
top2_temp=head(order_temp,1)
top2_temp
time_temp=1/top2_temp$f
time_temp

library(forecast)
precip_data<-ts(data,frequency = 91.12)
number<-as.integer(2881*.796875)
number_1<-number+1
train<-precip_data[1:number]
test<-precip_data[number_1:2881]
train_precip<-ts(train,frequency = 91.12)
train_precip2<-ts(train,frequency = 91)
test_precip<-ts(data[number_1:2881])

plot(ts(data1),xlab="Time Step",ylab="Precipitation Values")
precip_comps<-decompose(precip_data)
plot(precip_comps)

#training data with ARIMA
fit<-auto.arima(train_precip)
summary(fit)
pred=forecast(fit, h=586)
op <- par(cex.axis = 1.5, cex.lab = 1.5) 

mean_pred=ts(pred$mean)
plot(test_precip,ylab="Precipitation Values")
lines(mean_pred,col="blue")


#exponential smoothing on training set
hw_fit<-HoltWinters(train_precip2)
hw_pred=forecast(hw_fit, h=586)

op <- par(cex.axis = 1.5, cex.lab = 1.5) 
mean_hw_pred=ts(hw_pred$mean)
accuracy(mean_hw_pred, test)
plot(test_precip,ylab="Precipitation Values")
lines(mean_hw_pred,col="blue")
summary(train_precip)
xx<-decompose(train_precip)
summary(xx)
plot(xx)


#forecasting with tbats model 
tbats_fit = tbats(train_precip)
tbats_fit
plot(tbats_fit)
tbats_pred = forecast(tbats_fit, h=586)
accuracy(tbats_pred,test)
mean_tbats_pred=ts(tbats_pred$mean)
plot(test_precip,ylab="Precipitation Values")
lines(mean_tbats_pred,col="blue")

#using theta method
library(forecast)
theta_pred=thetaf(train_precip, h=586, level=c(95), fan=FALSE)
summary(theta_pred)
accuracy(theta_pred,test)
mean_theta_pred=ts(theta_pred$mean)
plot(test_precip,ylab="Precipitation Values")
lines(mean_theta_pred,col="blue")
