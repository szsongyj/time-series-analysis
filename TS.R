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



#######################���ݶ����Ԥ������ʼ######################
hmAirport=read.csv('hm.csv',header = T,sep=',')
mete=data.frame(as.vector(unique(hmAirport$mete_id)),c('����','����״̬','ˮ��','�����ź�','�¶�','���ܵ������','���ܵ����ѹ','���ܵ������','ʪ��'),c('smoke','net','water','video','temp','curr','volt','power','hum'))
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
#�������У��ֱ���data_datetime�е�date��time
hmAirport<-within(hmAirport,{
  data_date<-format(data_datetime,'%Y-%m-%d')
  data_time<-format(data_datetime,'%H:%M:%S')
})
#��factor����ת����number����
hmAirport$min_value<-as.numeric(as.character(hmAirport$min_value))
hmAirport$max_value<-as.numeric(as.character(hmAirport$max_value))
#���min_value��max_value�Ƿ���ȱʧֵ
#�������Ƶ�źż��ֵ��6��ȱʧֵ��������x��
#�������������û��ȱʧֵ
sum(is.na(hmAirport$data_datetime))
sum(is.na(hmAirport$min_value))
sum(is.na(hmAirport$max_value))
x=hmAirport[which(is.na(hmAirport$min_value)),]
str(hmAirport)
hmAirport<-hmAirport[,c(2,4,1,10,11,3,13,12,6,5,9)]
hmAirport<-hmAirport[order(hmAirport$id),]


##################################################################
#��device���з�����ÿ��device_id�ļ�¼����
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

#hm_0220Ϊdevice_idΪ422c05550220�ļ�¼
hm_0220<-subset(hmAirport,hmAirport$device_id=='422c05550220')
nrow(hm_0220)
unique(hm_0220$mete_name)#hm_0220��ȫ��9����ָ��

nrow(subset(hm_0220,hm_0220$mete_name=='ʪ��'))#12940
nrow(subset(hm_0220,hm_0220$mete_name=='�¶�'))#12942
nrow(subset(hm_0220,hm_0220$mete_name=='���ܵ����ѹ'))#12942
nrow(subset(hm_0220,hm_0220$mete_name=='���ܵ������'))#12942
nrow(subset(hm_0220,hm_0220$mete_name=='���ܵ������'))#12942
nrow(subset(hm_0220,hm_0220$mete_name=='����״̬'))#14093
nrow(subset(hm_0220,hm_0220$mete_name=='ˮ��'))#14093
nrow(subset(hm_0220,hm_0220$mete_name=='����'))#5
nrow(subset(hm_0220,hm_0220$mete_name=='��Ƶ�ź�'))#0
#��һ��ɸѡ��4��ָ��ļ���¼
hm_0220<-subset(hm_0220,is.element(hm_0220$mete_name,c('���ܵ������','���ܵ����ѹ','���ܵ������','�¶�')))
#Ϊ�˰���ʱ������������ͽ���ת��
hm_0220<-within(hm_0220,{
  data_datetime<-as.POSIXct(data_datetime)
  create_time<-as.POSIXct(create_time)
})

#��һ�����к��ټ�����ݵ���Щ������ݹ��˵�
difftime(strptime('2019-05-09 19:10:00','%Y-%m-%d %H:%M:%S'),strptime('2019-02-27 19:10:00','%Y-%m-%d %H:%M:%S'),units = 'days')#71 days
#'2019-02-27 19:10:00'---'2019-05-09 19:10:00' ��71�����ݣ�4�����ָ�꣨�¶ȡ����ܵ�����������ܵ����ѹ�����ܵ�����ʣ�
hm_0220<-subset(hm_0220,hm_0220$data_datetime>=strptime('2019-02-27 19:10:00','%Y-%m-%d %H:%M:%S') & hm_0220$data_datetime<=strptime('2019-05-09 19:10:00','%Y-%m-%d %H:%M:%S'))

#�ѳ���ת���ɿ��������ɵ�hm0220Ϊ�ӽ�ʱ�����и�ʽ��data frame 
df_temp=subset(hm_0220,hm_0220$mete_name=='�¶�')[,c('data_datetime','min_value','max_value')]
df_curr=subset(hm_0220,hm_0220$mete_name=='���ܵ������')[,c('data_datetime','min_value','max_value')]
df_volt=subset(hm_0220,hm_0220$mete_name=='���ܵ����ѹ')[,c('data_datetime','min_value','max_value')]
df_power=subset(hm_0220,hm_0220$mete_name=='���ܵ������')[,c('data_datetime','min_value','max_value')]

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

#����������������ݵ�ʱ����������5���Ӻ�10���ӣ��Ϳ�����Ϊ������ʱ�������
a=which(difftime(hm0220$data_datetime,lag(hm0220$data_datetime,1))>=15)
b=lag(a,1)
c=a-b
x1=a[which(c==max(c,na.rm = T),arr.ind = T)]-1;x1
x2=a[which(c==max(c,na.rm = T),arr.ind = T)-1];x2
#�õ�����ʱ�������hm0220������������������������������Ϊ5���ӣ�������Ϊ10����
hm0220=hm0220[x1:x2,]
nrow(hm0220)#4593����¼
#����ʱ������
hm0220<-hm0220[order(hm0220$data_datetime),]

#������ʱ������н�һ��ѡȡ�������ڵ����ݣ�����֤ʱ���������
#16��
#һ��24Сʱ��ÿ5����һ����¼��һ����287����¼
days.hm0220=floor(nrow(hm0220)/287);days.hm0220#16
#��֤ʱ���������
hm0220[1+287*0:days.hm0220,c('data_datetime')]
end=1+287*days.hm0220

##########��󣬵õ��������ڵ�����ʱ����������ts.hm0220##########
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
#ѡ��3��������ѵ����
train_msts.hm0220=window(msts.hm0220,end=4)
test_msts.hm0220=window(msts.hm0220,start=4,end=5)
tsp(test_msts.hm0220)
###############################################
###############################################

#���ʱ��������һ��ʱ��ͼ
autoplot(ts.hm0220)+
  scale_x_continuous(breaks=seq(1,17,by=1))
glimpse(ts.hm0220)
summary(ts.hm0220)

#ÿ��ʱ������һ��ʱ��ͼ
par(mfrow=c(4,2))
plot(ts.hm0220[,'temp_min'],type='l',xlab='time of day(5 minutes interval)',ylab='temp_min')
plot(ts.hm0220[,'temp_max'],type='l',xlab='time of day(5 minutes interval)',ylab='temp_max')
plot(ts.hm0220[,'curr_min'],type='l',xlab='time of day(5 minutes interval)',ylab='curr_min')
plot(ts.hm0220[,'curr_max'],type='l',xlab='time of day(5 minutes interval)',ylab='curr_max')
plot(ts.hm0220[,'volt_min'],type='l',xlab='time of day(5 minutes interval)',ylab='volt_min')
plot(ts.hm0220[,'volt_max'],type='l',xlab='time of day(5 minutes interval)',ylab='volt_max')
plot(ts.hm0220[,'power_min'],type='l',xlab='time of day(5 minutes interval)',ylab='power_min')
plot(ts.hm0220[,'power_max'],type='l',xlab='time of day(5 minutes interval)',ylab='power_max')


#######��ʼ��temp_min���з���########
#��ֽ���
ndiffs(ts.hm0220[,'temp_min'])
ndiffs(hm0220$temp_min)
#temp_min��ʱ��ͼ����16�������Լ�4�����ݵ�ʱ��ͼ����һ�𣬿����24Сʱ�仯���
ap_temp_min<-autoplot(ts.hm0220[,'temp_min'])+
  xlab('time of day(by hourly break)')+ylab('temp_min')+
  scale_x_continuous(breaks = seq(1,17,by=1))+
  ggtitle('time plot of temp_min(16 days)')
ap_temp_min_minor<-autoplot(window(ts.hm0220[,'temp_min'],end=4))+
  xlab('time of day(by hourly break)')+ylab('temp_min')+
  scale_x_continuous(minor_breaks = seq(1,4,by=1/24))+
  ggtitle('time plot of temp_min(3 days)')
gridExtra::grid.arrange(ap_temp_min,ap_temp_min_minor)

#��temp_min��season�仯��ͼ�α�ʾ
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
#Ԥ��1�������
#Arima+Fourier model forecast
fc.fourier.temp_min <- forecast(bestfit.temp_min,
               xreg=fourier(test_msts.hm0220[,'temp_min'], K=bestK, h=288))
#ѵ�����ݡ�������ݡ�Ԥ������
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
