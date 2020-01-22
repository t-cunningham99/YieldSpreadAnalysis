# YieldSpreadAnalysis
##An Analysis of the Predictive Capacity of Contemporary Yield Spreads for US Recessions
rm(list = ls())
# Installing and loading the necessary packages.

library('stargazer')

library('Quandl')

library('tseries')

library('zoo')

library('lubridate')

library('forecast')

options(warn=-1)

Quandl.api_key("UMo9bYuaDpyAy55mMfGS")

# Importing FRED Data for 10yr/3m spread with lag and recession

tenyr3m = Quandl(c('FRED/T10Y3MM'), collapse = 'quarterly', type = 'ts')

recession = Quandl(c('FRED/USREC'), collapse = 'quarterly', type = 'ts')

tenyr3m_lag = lag(tenyr3m,-4)

# Overlap of Spread and Recession Status


data = ts.intersect(tenyr3m, recession, tenyr3m_lag)

tenyr3mo = data[,1]

recessiono = data[,2]

tenyr3m_lago = data[,3]



# One-step ahead forecast



# Fixed window



t=4*10

T=length(recessiono)



# Periods need to be forecasted



ns=T-t

fixw=ts(recessiono[1:t])

pro=glm(fixw~tenyr3m_lago[1:t],family=binomial(link="probit"))

fcast_profix = matrix(NA,nrow=ns,ncol=1)

for(i in 1:ns)

{

fcast_profix[i,] = pnorm(pro$coef[1]+pro$coef[2]*tenyr3m_lago[t+i])

}



# Convert forcast series from numeric arrays to ts objects



convert=function(data){

return(ts(data,start=c(1993,1),freq=4))

}

fcast_profix.ts=convert(fcast_profix)

recessiono.real=window(recessiono,start=c(1993,1))

plot(recessiono.real, ylab="Probability of recession",xlab="Date",lty=4)

lines(fcast_profix.ts,col="red")

title("Figure1. 10yr3m Fixed Forecasting Scheme")

legend("topright",legend=c("Probit","Real"),col=c("red","black"),lty=c(1,2))

rmsfe_fcast_profix= sqrt(mean((recessiono[(t+1):T]-fcast_profix)^2))

rmsfe_fcast_profix



# Rolling window



fcast_pror = matrix(NA,nrow=ns,ncol=1)

for(i in 1:ns)

{

probitr=glm(recessiono[i:(i+t-1)]~tenyr3m_lago[i:(i+t-1)],family=binomial(link="probit"))

fcast_pror[i,]=pnorm(probitr$coef[1]+probitr$coef[2]*tenyr3m_lago[i+t])

}

fcast_pror.ts=convert(fcast_pror)

plot(recessiono.real, ylab="Probability of recession",xlab="Date",lty=4)

lines(fcast_pror.ts,col="red")

title("Figure2. 10yr3m Rolling Forecasting Scheme")

legend("topright",legend=c("Probit","Real"),col=c("red","black"),lty=c(1,2))

rmsfe_fcast_prorolling= sqrt(mean((recessiono[(t+1):T]-fcast_pror)^2))

rmsfe_fcast_prorolling



# Recursive one-step forecasting



fcast_prorec = matrix(NA,nrow=ns,ncol=1)

p_rec=matrix(NA,nrow=ns,ncol=1)

for(i in 1:ns)

{

prorec=glm(recessiono[1:(i+t-1)]~tenyr3m_lago[1:(i+t-1)],family=binomial(link="probit"))

fcast_prorec[i,]=pnorm(prorec$coef[1]+prorec$coef[2]*tenyr3m_lago[i+t])

}

fcast_prorec.ts=convert(fcast_prorec)

plot(recessiono.real, ylab="Probability of recession",xlab="Date",lty=4)

lines(fcast_prorec.ts,col="red")

title("Figure3. 10yr3m Recursive Forecasting Scheme")

legend("topright",legend=c("Probit","Real"),col=c("red","black"),lty=c(1,2))

rmsfe_fcast_prorec = sqrt(mean((recessiono[(t+1):T]-fcast_prorec)^2))

rmsfe_fcast_prorec

stargazer(pro,probitr,prorec,title="Table 1. 10yr3m Yield Curve Spread Recession Indicator

Probability",align=TRUE, dep.var.caption= "Forecasting Models", dep.var.labels=c("Fixed

Scheme","Rolling Scheme","Recursive Scheme"),type='text',single.row=TRUE,add.lines =

list(c('RMSE','.2776903','.2836188','.2670552'),c("Number of False Positive","1","2","1"),c("Number of

False Negative","0","1","0")),omit =c("tenyr3m_lago","Constant"),omit.table.layout = "sn")



# Importing FRED Data for 10yr/1yr spread with lag and recession



tenyr1yr=Quandl(c('FRED/DGS10'),collapse='quarterly',type="ts")-

Quandl(c('FRED/DGS1'),collapse='quarterly',type="ts")

recession = Quandl(c('FRED/USREC'), collapse = 'quarterly', type = 'ts')

tenyr1yr_lag = lag(tenyr1yr,-4)



# Overlap of Spread and Recession Status



data1 = ts.intersect(tenyr1yr, recession, tenyr1yr_lag)

data1_=window(data1,start=c(1983),freeny=4)

tenyr1yro = data1_[,1]

recessiono1 = data1_[,2]

tenyr1yr_lago = data1_[,3]



# One-step ahead forecast



# Fixed window



t1=4*10

T1=length(recessiono1)



# periods need to be forecasted



ns1=T1-t1

fixw1=ts(recessiono1[1:t1])

pro1=glm(fixw1~tenyr1yr_lago[1:t1],family=binomial(link="probit"))

fcast_profix1 = matrix(NA,nrow=ns1,ncol=1)

for(i in 1:ns1)

{

fcast_profix1[i,] = pnorm(pro1$coef[1]+pro1$coef[2]*tenyr1yr_lago[t1+i])

}



# Convert forcast series from numeric arrays to ts objects



convert=function(data){

return(ts(data,start=c(1993,1),freq=4))

}

fcast_profix1.ts=convert(fcast_profix1)

recessiono.real1=window(recessiono1,start=c(1993,1))

plot(recessiono.real1, ylab="Probability of recession",xlab="Date",lty=4)

lines(fcast_profix1.ts,col="red")

title("Figure4. 10yr1yr Fixed Forecasting Scheme")

legend("topright",legend=c("Probit","Real"),col=c("red","black"),lty=c(1,2))

rmsfe_fcast_profix1= sqrt(mean((recessiono1[(t1+1):T1]-fcast_profix1)^2))

rmsfe_fcast_profix1



# Rolling window



fcast_pror1 = matrix(NA,nrow=ns1,ncol=1)

for(i in 1:ns1)

{

probitr1=glm(recessiono1[i:(i+t1-1)]~tenyr1yr_lago[i:(i+t1-1)],family=binomial(link="probit"))

fcast_pror1[i,]=pnorm(probitr1$coef[1]+probitr1$coef[2]*tenyr1yr_lago[i+t1])

}

fcast_pror1.ts=convert(fcast_pror1)

plot(recessiono.real1, ylab="Probability of recession",xlab="Date",lty=4)

lines(fcast_pror1.ts,col="red")

title("Figure5. 10yr1yr Rolling Forecasting Scheme")

legend("topright",legend=c("Probit","Real"),col=c("red","black"),lty=c(1,2))

rmsfe_fcast_prorolling1= sqrt(mean((recessiono1[(t1+1):T1]-fcast_pror1)^2))

rmsfe_fcast_prorolling1



# Recursive one-step forecasting



fcast_prorec1 = matrix(NA,nrow=ns1,ncol=1)

p_rec1=matrix(NA,nrow=ns1,ncol=1)

for(i in 1:ns1)

{

prorec1=glm(recessiono1[1:(i+t1-1)]~tenyr1yr_lago[1:(i+t1-1)],family=binomial(link="probit"))

fcast_prorec1[i,]=pnorm(prorec1$coef[1]+prorec1$coef[2]*tenyr1yr_lago[i+t1])

}

fcast_prorec1.ts=convert(fcast_prorec1)

plot(recessiono.real1, ylab="Probability of recession",xlab="Date",lty=4)

lines(fcast_prorec1.ts,col="red")

title("Figure6. 10yr1yr Recursive Forecasting Scheme")

legend("topright",legend=c("Probit","Real"),col=c("red","black"),lty=c(1,2))

rmsfe_fcast_prorec1 = sqrt(mean((recessiono1[(t1+1):T1]-fcast_prorec1)^2))

rmsfe_fcast_prorec1

stargazer(pro1,probitr1,prorec1,title="Table 2. 10yr1yr Yield Curve Spread Recession Indicator

Probability",align=TRUE, dep.var.caption= "Forecasting Models", dep.var.labels=c("Fixed

Scheme","Rolling Scheme","Recursive Scheme"),type='text',single.row=TRUE,add.lines =

list(c('RMSE','.2553772','.2839421','.2591375'),c("Number of False Positive","0","0","0"),c("Number of

False Negative","0","0","1")),omit =c("tenyr1yr_lago","Constant"),omit.table.layout = "sn")



# Importing FRED Data for 10yr/2yr spread with lag and recession



tenyr2yr=Quandl(c('FRED/T10Y2Y'),collapse='quarterly',type="ts")

recession = Quandl(c('FRED/USREC'), collapse = 'quarterly', type = 'ts')

tenyr2yr_lag = lag(tenyr2yr,-4)



# Overlap of Spread and Recession Status



data2= ts.intersect(tenyr2yr, recession, tenyr2yr_lag)

data2_=window(data2,start=c(1983),freq=4)

tenyr2yro = data2_[,1]

recessiono2 = data2_[,2]

tenyr2yr_lago = data2_[,3]



# One-step ahead forecast



# Fixed window



t2=4*10

T2=length(recessiono2)



# Periods need to be forecasted



ns2=T2-t2

fixw2=ts(recessiono2[1:t2])

pro2=glm(fixw2~tenyr2yr_lago[1:t2],family=binomial(link="probit"))

fcast_profix2 = matrix(NA,nrow=ns2,ncol=1)

for(i in 1:ns2)

{

fcast_profix2[i,] = pnorm(pro2$coef[1]+pro2$coef[2]*tenyr2yr_lago[t2+i])

}



# convert our forcast series from numeric arrays to ts objects



convert=function(data){

return(ts(data,start=c(1993,1),freq=4))

}

fcast_profix2.ts=convert(fcast_profix2)

recessiono.real2=window(recessiono2,start=c(1993,1))

plot(recessiono.real2, ylab="Probability of recession",xlab="Date",lty=4)

lines(fcast_profix2.ts,col="red")

title("Figure7. 10yr2yr Fixed Forecasting Scheme")

legend("topright",legend=c("Probit","Real"),col=c("red","black"),lty=c(1,2))

rmsfe_fcast_profix2= sqrt(mean((recessiono2[(t2+1):T2]-fcast_profix2)^2))

rmsfe_fcast_profix2



# Rolling window



fcast_pror2= matrix(NA,nrow=ns2,ncol=1)

for(i in 1:ns2)

{

probitr2=glm(recessiono2[i:(i+t2-1)]~tenyr2yr_lago[i:(i+t2-1)],family=binomial(link="probit"))

fcast_pror2[i,]=pnorm(probitr2$coef[1]+probitr2$coef[2]*tenyr2yr_lago[i+t2])

}

fcast_pror2.ts=convert(fcast_pror2)

plot(recessiono.real2, ylab="Probability of recession",xlab="Date",lty=4)

lines(fcast_pror2.ts,col="red")

title("Figure8. 10yr2yr Rolling Forecasting Scheme")

legend("topright",legend=c("Probit","Real"),col=c("red","black"),lty=c(1,2))

rmsfe_fcast_prorolling2= sqrt(mean((recessiono2[(t2+1):T2]-fcast_pror2)^2))

rmsfe_fcast_prorolling2



# Recursive one-step forecasting



fcast_prorec2 = matrix(NA,nrow=ns2,ncol=1)

p_rec2=matrix(NA,nrow=ns2,ncol=1)

for(i in 1:ns2)

{

prorec2=glm(recessiono2[1:(i+t2-1)]~tenyr2yr_lago[1:(i+t2-1)],family=binomial(link="probit"))

fcast_prorec2[i,]=pnorm(prorec2$coef[1]+prorec2$coef[2]*tenyr2yr_lago[i+t2])

}

fcast_prorec2.ts=convert(fcast_prorec2)

plot(recessiono.real2, ylab="Probability of recession",xlab="Date",lty=4)

lines(fcast_prorec2.ts,col="red")

title("Figure9. 10yr2yr Recursive Forecasting Scheme")

legend("topright",legend=c("Probit","Real"),col=c("red","black"),lty=c(1,2))

rmsfe_fcast_prorec2 = sqrt(mean((recessiono2[(t2+1):T2]-fcast_prorec2)^2))

rmsfe_fcast_prorec2

stargazer(pro2,probitr2,prorec2,title="Table 3. 10yr2yr Yield Curve Spread Recession Indicator

Probability",align=TRUE, dep.var.caption= "Forecasting Models", dep.var.labels=c("Fixed

Scheme","Rolling Scheme","Recursive Scheme"),type='text',single.row=TRUE,add.lines =

list(c('RMSE','.2586696','.3003397','.2626499'),c("Number of False Positive","0","0","0"),c("Number of

False Negative","1","1","1")),omit =c("tenyr2yr_lago","Constant"),omit.table.layout = "sn")



# Importing FRED Data for 5yr/3m spread with lag and recession



fiveyr3m=Quandl(c('FRED/DGS5'),collapse='quarterly',type="ts")-

Quandl(c('FRED/DGS3MO'),collapse='quarterly',type="ts")

recession = Quandl(c('FRED/USREC'), collapse = 'quarterly', type = 'ts')

fiveyr3m_lag = lag(fiveyr3m,-4)



# Overlap of Spread and Recession Status



data3 = ts.intersect(fiveyr3m, recession, fiveyr3m_lag)

fiveyr3mo = data3[,1]

recessiono3 = data3[,2]

fiveyr3m_lago3 = data3[,3]



# One-step ahead forecast



# Fixed window



t3=4*10

T3=length(recessiono3)



# Periods need to be forecasted



ns3=T3-t3

fixw3=ts(recessiono3[1:t3])

pro3=glm(fixw3~fiveyr3m_lago3[1:t3],family=binomial(link="probit"))

fcast_profix3 = matrix(NA,nrow=ns3,ncol=1)

for(i in 1:ns3)

{

fcast_profix3[i,] = pnorm(pro3$coef[1]+pro3$coef[2]*fiveyr3m_lago3[t3+i])

}



# Convert our forcast series from numeric arrays to ts objects



convert=function(data){

return(ts(data,start=c(1993,1),freq=4))

}

fcast_profix3.ts=convert(fcast_profix3)

recessiono.real3=window(recessiono3,start=c(1993,1))

plot(recessiono.real3, ylab="Probability of recession",xlab="Date",lty=4)

lines(fcast_profix3.ts,col="red")

title("Figure10. 5yr3m Fixed Forecasting Scheme")

legend("topright",legend=c("Probit","Real"),col=c("red","black"),lty=c(1,2))

rmsfe_fcast_profix3= sqrt(mean((recessiono3[(t3+1):T3]-fcast_profix3)^2))

rmsfe_fcast_profix3



# Rolling window



fcast_pror3 = matrix(NA,nrow=ns3,ncol=1)

for(i in 1:ns3)

{

probitr3=glm(recessiono3[i:(i+t3-1)]~fiveyr3m_lago3[i:(i+t3-1)],family=binomial(link="probit"))

fcast_pror3[i,]=pnorm(probitr3$coef[1]+probitr3$coef[2]*fiveyr3m_lago3[i+t3])

}

fcast_pror3.ts=convert(fcast_pror3)

plot(recessiono.real3, ylab="Probability of recession",xlab="Date",lty=4)

lines(fcast_pror3.ts,col="red")

title("Figure11. 5yr3m Rolling Forecasting Scheme")

legend("topright",legend=c("Probit","Real"),col=c("red","black"),lty=c(1,2))

rmsfe_fcast_prorolling3= sqrt(mean((recessiono3[(t3+1):T3]-fcast_pror3)^2))

rmsfe_fcast_prorolling3



# Recursive one-step forecasting



fcast_prorec3 = matrix(NA,nrow=ns3,ncol=1)

p_rec3=matrix(NA,nrow=ns3,ncol=1)

for(i in 1:ns3)

{

prorec3=glm(recessiono3[1:(i+t3-1)]~fiveyr3m_lago3[1:(i+t3-1)],family=binomial(link="probit"))

fcast_prorec3[i,]=pnorm(prorec3$coef[1]+prorec3$coef[2]*fiveyr3m_lago3[i+t3])

}

fcast_prorec3.ts=convert(fcast_prorec3)

plot(recessiono.real3, ylab="Probability of recession",xlab="Date",lty=4)

lines(fcast_prorec3.ts,col="red")

title("Figure12. 5yr3m Recursive Forecasting Scheme")

legend("topright",legend=c("Probit","Real"),col=c("red","black"),lty=c(1,2))

rmsfe_fcast_prorec3 = sqrt(mean((recessiono3[(t3+1):T3]-fcast_prorec3)^2))

rmsfe_fcast_prorec3

stargazer(pro3,probitr3,prorec3,title="Table 4. 5yr3m Yield Curve Spread Recession Indicator

Probability",align=TRUE, dep.var.caption= "Forecasting Models", dep.var.labels=c("Fixed

Scheme","Rolling Scheme","Recursive Scheme"),type='text',single.row=TRUE,add.lines =

list(c('RMSE','.2644944','.2815175','.2620607'),c("Number of False Positive","1","1","1"),c("Number of

False Negative","0","1","1")),omit =c("fiveyr3m_lago","Constant"),omit.table.layout = "sn")



# Importing FRED Data for 10yr/FF spread with lag and recession



tenyrFF=Quandl(c('FRED/T10YFF'),collapse='quarterly',type="ts")

recession = Quandl(c('FRED/USREC'), collapse = 'quarterly', type = 'ts')

tenyrFF_lag = lag(tenyrFF,-4)



# Overlap of Spread and Recession Status



dataFF = ts.intersect(tenyrFF, recession, tenyrFF_lag)

dataFF_=window(dataFF,start=c(1983),freeny=4)

tenyrFFo = dataFF_[,1]

recessionoFF = dataFF_[,2]

tenyrFF_lago = dataFF_[,3]



# One-step ahead forecast



# Fixed window



tFF=4*10

TFF=length(recessionoFF)


# periods need to be forecasted



nsFF=TFF-tFF

fixwFF=ts(recessionoFF[1:tFF])

proFF=glm(fixwFF~tenyrFF_lago[1:tFF],family=binomial(link="probit"))

fcast_profixFF = matrix(NA,nrow=nsFF,ncol=1)

for(i in 1:nsFF)

{

fcast_profixFF[i,] = pnorm(proFF$coef[1]+proFF$coef[2]*tenyrFF_lago[tFF+i])

}



# Convert forcast series from numeric arrays to ts objects


convert=function(data){

return(ts(data,start=c(1993,1),freq=4))

}

fcast_profixFF.ts=convert(fcast_profixFF)

recessiono.realFF=window(recessionoFF,start=c(1993,1))

plot(recessiono.realFF, ylab="Probability of recession",xlab="Date",lty=4)

lines(fcast_profixFF.ts,col="red")

title("Figure13. 10yrFF Fixed Forecasting Scheme")

legend("topright",legend=c("Probit","Real"),col=c("red","black"),lty=c(1,2))

rmsfe_fcast_profixFF= sqrt(mean((recessionoFF[(tFF+1):TFF]-fcast_profixFF)^2))

rmsfe_fcast_profixFF


# Rolling window



fcast_prorFF = matrix(NA,nrow=nsFF,ncol=1)

for(i in 1:nsFF)

{

probitrFF=glm(recessionoFF[i:(i+tFF-1)]~tenyrFF_lago[i:(i+tFF-1)],family=binomial(link="probit"))

fcast_prorFF[i,]=pnorm(probitrFF$coef[1]+probitrFF$coef[2]*tenyrFF_lago[i+tFF])

}

fcast_prorFF.ts=convert(fcast_prorFF)

plot(recessiono.realFF, ylab="Probability of recession",xlab="Date",lty=4)

lines(fcast_prorFF.ts,col="red")

title("Figure14. 10yrFF Rolling Forecasting Scheme")

legend("topright",legend=c("Probit","Real"),col=c("red","black"),lty=c(1,2))

rmsfe_fcast_prorollingFF= sqrt(mean((recessionoFF[(tFF+1):TFF]-fcast_prorFF)^2))

rmsfe_fcast_prorollingFF


# Recursive one-step forecasting


fcast_prorecFF = matrix(NA,nrow=nsFF,ncol=1)

p_rec1=matrix(NA,nrow=nsFF,ncol=1)

for(i in 1:nsFF)

{

prorecFF=glm(recessionoFF[1:(i+tFF-1)]~tenyrFF_lago[1:(i+tFF-1)],family=binomial(link="probit"))

fcast_prorecFF[i,]=pnorm(prorecFF$coef[1]+prorecFF$coef[2]*tenyrFF_lago[i+tFF])

}

fcast_prorecFF.ts=convert(fcast_prorecFF)

plot(recessiono.realFF, ylab="Probability of recession",xlab="Date",lty=4)

lines(fcast_prorecFF.ts,col="red")

title("Figure15. 10yrFF Recursive Forecasting Scheme")

legend("topright",legend=c("Probit","Real"),col=c("red","black"),lty=c(1,2))

rmsfe_fcast_prorecFF= sqrt(mean((recessionoFF[(tFF+1):TFF]-fcast_prorecFF)^2))

rmsfe_fcast_prorecFF

stargazer(proFF,probitrFF,prorecFF,title="Table 5. 10yrFF Yield Curve Spread Recession Indicator

Probability",align=TRUE, dep.var.caption= "Forecasting Models", dep.var.labels=c("Fixed

Scheme","Rolling Scheme","Recursive Scheme"),type='text',single.row=TRUE,add.lines =

list(c('RMSE','.2611373','.2833007','.2607636'),c("Number of False Positive","0","2","0"),c("Number of

False Negative","2","2","2")),omit =c("tenyrFF_lago","Constant"),omit.table.layout = "sn")



