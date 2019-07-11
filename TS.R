#-------------------------------------------------------------------------------
# Description:  Time Series Analysis Forecast for AIoT
# Project Name: 
# Name:         Multi-Season Time Series Analysis
# Author:       Song yongjun
# DateTime:     2019/7/5 14:49
#-------------------------------------------------------------------------------


################################################################
library(data.table)
library(TSA)
library(Metrics)
library(tseries)
library(forecast)
library(fpp2)
library(GGally)
library(gridExtra)
library(seasonal)
library(urca)
library(hts)
library(tidyverse)
library(imputeTS)



#######################数据读入和预处理开始######################
hmAirport=read.csv('hm.csv',header = T,sep=',')
mete=data.frame(as.vector(unique(hmAirport$mete_id)),c('烟雾','网络状态','水浸','视屏信号','温度','智能电表电流','智能电表电压','智能电表功率','湿度'),c('smoke','net','water','video','temp','curr','volt','power','hum'))
colnames(mete)=c('mete_id','mete_name','mete_name_abbr')
mete<-within(mete,{
  mete_name<-as.character(mete_name)
  mete_name_abbr<-as.character((mete_name_abbr))
})
str(mete)
hmAirport=merge(hmAirport,mete,by='mete_id')
hmAirport<-within(hmAirport,{
  data_time<-strptime(data_time,'%Y%m%d%H%M')
})
colnames(hmAirport)[3]<-'data_datetime'
hmAirport$create_time<-strptime(as.character(hmAirport$create_time),'%Y-%m-%d %H:%M')
#新增两列，分别是data_datetime列的date和time
hmAirport<-within(hmAirport,{
  data_date<-format(data_datetime,'%Y-%m-%d')
  data_time<-format(data_datetime,'%H:%M:%S')
})
#把factor类型转化成number类型
hmAirport$min_value<-as.numeric(as.character(hmAirport$min_value))
hmAirport$max_value<-as.numeric(as.character(hmAirport$max_value))
#检查min_value和max_value是否有缺失值
#结果：视频信号监控值有6个缺失值，保存在x中
#结果：数据日期没有缺失值
sum(is.na(hmAirport$data_datetime))
sum(is.na(hmAirport$min_value))
sum(is.na(hmAirport$max_value))
x=hmAirport[which(is.na(hmAirport$min_value)),]
str(hmAirport)
hmAirport<-hmAirport[,c(2,4,1,10,11,3,13,12,6,5,9)]
hmAirport<-hmAirport[order(hmAirport$id),]


##################################################################
#对device进行分析，每个device_id的记录条数
count=NULL
sum=0
for(i in 1:length(unique(hmAirport$device_id))){
  count[i]=nrow(hmAirport[which(hmAirport$device_id==unique(hmAirport$device_id)[i]),])
  sum<-sum+count[i]
}
sum
count
as.data.frame(cbind(Device_id=unique(as.character(hmAirport$device_id)),
                    Count=count))

#hm_0220为device_id为422c05550220的记录
hm_0220<-subset(hmAirport,hmAirport$device_id=='422c05550220')
nrow(hm_0220)
unique(hm_0220$mete_name)#hm_0220有全部9项监测指标

nrow(subset(hm_0220,hm_0220$mete_name=='湿度'))#12940
nrow(subset(hm_0220,hm_0220$mete_name=='温度'))#12942
nrow(subset(hm_0220,hm_0220$mete_name=='智能电表电压'))#12942
nrow(subset(hm_0220,hm_0220$mete_name=='智能电表电流'))#12942
nrow(subset(hm_0220,hm_0220$mete_name=='智能电表功率'))#12942
nrow(subset(hm_0220,hm_0220$mete_name=='网络状态'))#14093
nrow(subset(hm_0220,hm_0220$mete_name=='水浸'))#14093
nrow(subset(hm_0220,hm_0220$mete_name=='烟雾'))#5
nrow(subset(hm_0220,hm_0220$mete_name=='视频信号'))#0
#进一步筛选出4个指标的监测记录
hm_0220<-subset(hm_0220,is.element(hm_0220$mete_name,c('智能电表电流','智能电表电压','智能电表功率','温度')))
#为了按照时间排序对列类型进行转化
hm_0220<-within(hm_0220,{
  data_datetime<-as.POSIXct(data_datetime)
  create_time<-as.POSIXct(create_time)
})

#进一步把有很少监测数据的那些天的数据过滤掉
difftime(strptime('2019-05-09 19:10:00','%Y-%m-%d %H:%M:%S'),strptime('2019-02-27 19:10:00','%Y-%m-%d %H:%M:%S'),units = 'days')#71 days
#'2019-02-27 19:10:00'---'2019-05-09 19:10:00' 共71天数据，4个监测指标（温度、智能电表电流、智能电表电压、智能电表功率）
hm_0220<-subset(hm_0220,hm_0220$data_datetime>=strptime('2019-02-27 19:10:00','%Y-%m-%d %H:%M:%S') & hm_0220$data_datetime<=strptime('2019-05-09 19:10:00','%Y-%m-%d %H:%M:%S'))

#把长表转化成宽表，生成的hm0220为接近时间序列格式的data frame 
df_temp=subset(hm_0220,hm_0220$mete_name=='温度')[,c('data_datetime','min_value','max_value')]
df_curr=subset(hm_0220,hm_0220$mete_name=='智能电表电流')[,c('data_datetime','min_value','max_value')]
df_volt=subset(hm_0220,hm_0220$mete_name=='智能电表电压')[,c('data_datetime','min_value','max_value')]
df_power=subset(hm_0220,hm_0220$mete_name=='智能电表功率')[,c('data_datetime','min_value','max_value')]

colnames(df_temp)[2]<-c('temp_min')
colnames(df_temp)[3]<-c('temp_max')
colnames(df_curr)[2]<-c('curr_min')
colnames(df_curr)[3]<-c('curr_max')
colnames(df_volt)[2]<-c('volt_min')
colnames(df_volt)[3]<-c('volt_max')
colnames(df_power)[2]<-c('power_min')
colnames(df_power)[3]<-c('power_max')

hm0220<-list(df_temp,df_curr,df_volt,df_power)%>%
  reduce(left_join,by='data_datetime')

#把相邻两个监测数据的时间间隔限制在5分钟和10分钟，就可以认为是连续时间的序列
a=which(difftime(hm0220$data_datetime,lag(hm0220$data_datetime,1))>=15)
b=lag(a,1)
c=a-b
x1=a[which(c==max(c,na.rm = T),arr.ind = T)]-1;x1
x2=a[which(c==max(c,na.rm = T),arr.ind = T)-1];x2
#得到连续时间的序列hm0220，满足条件：相邻两个间隔绝大多数为5分钟，极少数为10分钟
hm0220=hm0220[x1:x2,]
nrow(hm0220)#4593条记录
#按照时间排序
hm0220<-hm0220[order(hm0220$data_datetime),]

#对连续时间的序列进一步选取整数周期的数据，并验证时间的周期性
#16天
#一天24小时，每5分钟一条记录，一天是287条记录
days.hm0220=floor(nrow(hm0220)/287);days.hm0220#16
#验证时间的周期性
hm0220[1+287*0:days.hm0220,c('data_datetime')]
end=1+287*days.hm0220

##########最后，得到整数周期的连续时间序列数据ts.hm0220##########
hm0220=hm0220[1:end,]
ts.hm0220=ts(hm0220[,2:9],start = 1,frequency = 287 )

#Detecting of seasonality
p_temp_min=periodogram(hm0220$temp_min,plot = T)
data.table(period=1/p_temp_min$freq, spec=p_temp_min$spec)[order(-spec)][1:2]
p_temp_max=periodogram(hm0220$temp_max,plot = T)
data.table(period=1/p_temp_max$freq, spec=p_temp_max$spec)[order(-spec)][1:2]
p_curr_min=periodogram(hm0220$curr_min,plot = T)
data.table(period=1/p_curr_min$freq, spec=p_curr_min$spec)[order(-spec)][1:2]
p_curr_max=periodogram(hm0220$curr_max,plot = T)
data.table(period=1/p_curr_max$freq, spec=p_curr_max$spec)[order(-spec)][1:2]
p_volt_min=periodogram(hm0220$volt_min,plot = T)
data.table(period=1/p_volt_min$freq, spec=p_volt_min$spec)[order(-spec)][1:2]
p_volt_max=periodogram(hm0220$volt_max,plot = T)
data.table(period=1/p_volt_max$freq, spec=p_volt_max$spec)[order(-spec)][1:2]
p_power_min=periodogram(hm0220$power_min,plot = T)
data.table(period=1/p_power_min$freq, spec=p_power_min$spec)[order(-spec)][1:2]
p_power_max=periodogram(hm0220$power_max,plot = T)
data.table(period=1/p_power_max$freq, spec=p_power_max$spec)[order(-spec)][1:2]

#################
p_temp_min_s=periodogram(hm0220$temp_min[1:864],plot = T)
data.table(period=1/p_temp_min_s$freq, spec=p_temp_min_s$spec)[order(-spec)][1:2]

#Multi-Seasonal Time Series
help(msts)
msts.hm0220=msts(hm0220[,2:9],seasonal.periods = c(144,288),start=1)
str(msts.hm0220)
#选择3天数据做训练集
train_msts.hm0220=window(msts.hm0220,end=4)
test_msts.hm0220=window(msts.hm0220,start=4,end=5)
tsp(test_msts.hm0220)
###############################################
###############################################

#多个时间序列在一个时序图
autoplot(ts.hm0220)+
  scale_x_continuous(breaks=seq(1,17,by=1))
glimpse(ts.hm0220)
summary(ts.hm0220)

#每个时间序列一个时序图
par(mfrow=c(4,2))
plot(ts.hm0220[,'temp_min'],type='l',xlab='time of day(5 minutes interval)',ylab='temp_min')
plot(ts.hm0220[,'temp_max'],type='l',xlab='time of day(5 minutes interval)',ylab='temp_max')
plot(ts.hm0220[,'curr_min'],type='l',xlab='time of day(5 minutes interval)',ylab='curr_min')
plot(ts.hm0220[,'curr_max'],type='l',xlab='time of day(5 minutes interval)',ylab='curr_max')
plot(ts.hm0220[,'volt_min'],type='l',xlab='time of day(5 minutes interval)',ylab='volt_min')
plot(ts.hm0220[,'volt_max'],type='l',xlab='time of day(5 minutes interval)',ylab='volt_max')
plot(ts.hm0220[,'power_min'],type='l',xlab='time of day(5 minutes interval)',ylab='power_min')
plot(ts.hm0220[,'power_max'],type='l',xlab='time of day(5 minutes interval)',ylab='power_max')


#######开始对temp_min进行分析########
#差分建议
ndiffs(ts.hm0220[,'temp_min'])
ndiffs(hm0220$temp_min)
#temp_min的时序图，把16天数据以及4天数据的时序图画在一起，看清楚24小时变化情况
ap_temp_min<-autoplot(ts.hm0220[,'temp_min'])+
  xlab('time of day(by hourly break)')+ylab('temp_min')+
  scale_x_continuous(breaks = seq(1,17,by=1))+
  ggtitle('time plot of temp_min(16 days)')
ap_temp_min_minor<-autoplot(window(ts.hm0220[,'temp_min'],end=4))+
  xlab('time of day(by hourly break)')+ylab('temp_min')+
  scale_x_continuous(minor_breaks = seq(1,4,by=1/24))+
  ggtitle('time plot of temp_min(3 days)')
gridExtra::grid.arrange(ap_temp_min,ap_temp_min_minor)

#对temp_min的season变化用图形表示
#seasonal plot
ggseasonplot(ts.hm0220[,'temp_min'],col=rainbow(12))+
  ylab('seasonal index of temp_min')+
  ggtitle('seasonal plot of temp_min(16 days)')

ggseasonplot(ts.hm0220[,'temp_min'],polar = T,col=rainbow(12))+
  ylab('seasonal index of temp_min')+
  ggtitle('seasonal plot of temp_min(16 days)')

ggsubseriesplot(ts.hm0220[,'temp_min'],year.labels=TRUE, year.labels.left=TRUE)+
  ylab('seasonal index of temp_min')+
  ggtitle('seasonal plot of temp_min(16 days)')

#reveal relationships between time series.
autoplot(ts.hm0220[,c('temp_min','temp_max')],facets=T)+
  xlab('time of day')+ylab('temp_min vs temp_max')+
  ggtitle('temp_min and temp_max')

qplot(hm0220$temp_min,hm0220$temp_max)+
  ylab('temp_max')+
  xlab('temp_min')

#linear relationship
ggpairs(hm0220[,c('temp_min','temp_max')])

#Forecasting with long seasonal periods 
# dynamic harmonic regression model(Arima+Fourier)
#for multi-season time series,Choose the best model by AICc

bestK=c(0,0)
bestfit.temp_min <- list(aicc=Inf)
for(i in seq(5)){
  for(j in seq(5)){
    fit<-auto.arima(train_msts.hm0220[,'temp_min'],
                    xreg = fourier(train_msts.hm0220[,'temp_min'],K=c(i,j)),
                    seasonal = F)
    if(fit[['aicc']]< bestfit.temp_min[['aicc']]){
      bestfit.temp_min<-fit
      bestK<- c(i,j)
    }
  }
} 
bestfit.temp_min;bestK
#预测1天的数据
#Arima+Fourier model forecast
fc.fourier.temp_min <- forecast(bestfit.temp_min,
               xreg=fourier(test_msts.hm0220[,'temp_min'], K=bestK, h=288))
#训练数据、拟合数据、预测数据
autoplot(fc.fourier.temp_min,series='forecast by Arima fourier')+
  autolayer(window(msts.hm0220[,'temp_min'],start=1,end=5),series='original')+
  autolayer(fitted((fc.fourier.temp_min)),series = 'fitted ')+
  scale_x_continuous(minor_breaks =seq(1,5,by=1/24))

autoplot(train_msts.hm0220[,'temp_min'],series='original')+
  autolayer(test_msts.hm0220[,'temp_min'],series='original')+
  autolayer(fc.fourier.temp_min,series='forecast by Arima+Fourier model',PI=F)+
  autolayer(fitted((fc.fourier.temp_min)),series = 'fitted ')+
  xlab('time of day(5 mins interval,4 days in total)')+
  ylab('temp_min')+
  scale_x_continuous(minor_breaks =seq(1,5,by=1/24))+
  ggtitle('forecast of temp_min by Arima+Fourier model ')

summary(fc.fourier.temp_min)

#Arima+Fourier model accuracy evaluated on training and test set
accuracy(fc.fourier.temp_min,test_msts.hm0220[,'temp_min'])

#TBATS model forecast
fc.tbats.temp_min<-train_msts.hm0220[,'temp_min']%>%
  tbats(seasonal.periods = c(144,288))%>%
  forecast(h=288)

autoplot(train_msts.hm0220[,'temp_min'],series='original')+
  autolayer(test_msts.hm0220[,'temp_min'],series='original')+
  autolayer(fc.tbats.temp_min,series='forecast of temp_min by TBATS model',PI=F)+
  autolayer(fitted(fc.tbats.temp_min),series='fitted')+
  xlab('time of day(5 mins interval,4 days in total)')+
  ylab('temp_min')+
  scale_x_continuous(minor_breaks =seq(1,5,by=1/24))+
  ggtitle('forecast of temp_min by TBATS model')

# TBATS model accuracy evaluated on training and test set
accuracy(fc.tbats.temp_min,test_msts.hm0220[,'temp_min'])

# mstl function for time series decomposition  
msts.hm0220[,'temp_min']%>%mstl()%>%
  autoplot()+
  xlab('time of day(16 days)')+ylab('temp_min')+
  scale_x_continuous(breaks = seq(1,17,by=1))+
  ggtitle('decomposition(Trend+Season+Residual) of temp_min by STL model')

#STLF model forecast
fc.stlf.temp_min<-train_msts.hm0220[,'temp_min']%>%stlf(h=288)

autoplot(train_msts.hm0220[,'temp_min'],series='original')+
  autolayer(test_msts.hm0220[,'temp_min'],series='original')+
  autolayer(fc.stlf.temp_min,series='forecast of temp_min by STLF(STL+ETS(A,Ad,N)) model',PI=F)+
  autolayer(fitted(fc.stlf.temp_min),series='fitted')+
  xlab('time of day(5 mins interval,4 days in total)')+
  ylab('temp_min')+
  scale_x_continuous(minor_breaks =seq(1,5,by=1/24))+
  ggtitle('forecast of temp_min by STLF(STL+ETS(A,Ad,N)) model')

#STL+ETS(A,Ad,N) model accuracy evaluated on training and test set
accuracy(fc.stlf.temp_min,test_msts.hm0220[,'temp_min'])

