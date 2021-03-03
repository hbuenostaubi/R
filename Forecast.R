# options(repos="https://CRAN.R-project.org")  ### firewall (ctrk+Shift+C)
# install.packages("astsa", dependencies = T);install.packages("forecast", dependencies = T);install.packages("lubridate",dependencies = T)
library(astsa);library(forecast);library(lubridate)

Pax=read.csv("Total.Pax.Month.csv", header = T);head(Pax)
# Pax$Date=strptime(Pax$Date, format = "%m/%d/%y");
# Pax$Date=as.Date(Pax$Date, format = "%b/%y")

Pax=ts(Pax$Pax)

par(mfrow=c(2,2))
plot(Pax, lwd=.85, col="blue", main="Monthly Sched Total Pax")
plot(log(Pax));plot(diff(log(Pax)));plot(diff(diff(log(Pax)),12))
Trans.Pax=diff(diff(log(Pax)),12)

acf2(Trans.Pax,12)

per=12;d=1;DD=1

for(p in 1:7){
  for(q in 1:5){
    for(i in 1:2){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(Pax), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }}}}}

# a<-sarima.for(log(suv),12,1,1,0,0,1,1,12)
# plot.ts(c(suv,exp(a$pred)), main='Monthly sales + Forecast', ylab='', col='blue', lwd=3)
par(mfrow=c(2,1))
model=arima(log(Pax), order = c(0,1,0), seasonal = list(order=c(0,1,2), period=26))
a=sarima.for(log(Pax),12, 0,1,0,0,1,2,12)
plot.ts(c(Pax,exp(a$pred)), main='Total Sked Pax', ylab='', col='blue', lwd=3)


plot(forecast(model))
qqnorm(model$residuals)
qqline(model$residuals)
forecast(model)


bk=read.csv("Booking.data.csv", header = T);head(bk[1:35,2])
bk.ts=ts(bk[1:35,2],start = c(2016), frequency = 12)
plot(log10(bk.ts))

m=HoltWinters(bk.ts,seasonal="multiplicative")
plot(forecast(m))
