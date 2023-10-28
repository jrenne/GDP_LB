#===============================================================================
# This script prepares the dataset.
#===============================================================================


#===============================================================================
# Load yield data
#===============================================================================

US_yields <- read.csv("data/US_yields.csv")
US_yields$date <- as.Date(US_yields$date,"%Y-%m-%d")

plot(US_yields$date,US_yields$DTB3,type="l")



#===============================================================================
# Compute realised volatility of US yields
#===============================================================================

US_yields$month <- as.integer(format(US_yields$date,"%m"))
US_yields$year <- as.integer(format(US_yields$date,"%Y"))
n_col_US<- dim(US_yields)[2]
US_yields <- US_yields[,c(n_col_US-1,n_col_US, 1:(n_col_US-2))]

US_yields_q <- data.frame(US_yields[1,])
n_col_Us<- dim(US_yields_q)[2]
US_yields_q <- cbind(US_yields_q,US_yields_q[,5:n_col_US])
names(US_yields_q)[(n_col_US+1):(2*n_col_US-4)] <- paste(names(US_yields_q)[(n_col_US+1):(2*n_col_US-4)], ".condiVar",sep="")

count <- 0

for(Year in start_Y:end_Y){
  if(Year==start_Y){start_Q=start_q} else{start_Q=1}
  if(Year==end_Y){end_Q=end_q} else{end_Q=4}
  for(Quarter in start_Q:end_Q){
    count <- count+1
    US_yields_reduced <- subset(US_yields,(year==Year) & (month %in% (3*(Quarter-1)+1):(3*Quarter)))
    nb.obs <- dim(US_yields_reduced)[1]
    US_yields_reduced.in.difference <- data.matrix(US_yields_reduced[2:nb.obs,]) - 
      data.matrix(US_yields_reduced[1:(nb.obs-1),])
    US_yields_q[count,5:n_col_US] <- apply(data.matrix(US_yields_reduced[,5:n_col_US]),2,function(x){mean(x,na.rm=TRUE)})
    US_yields_q[count,(n_col_US+1):(2*n_col_US-4)] <- apply(data.matrix(US_yields_reduced.in.difference[,5:n_col_US]),2,
                                                            function(x){ifelse(sum(is.na(x))==length(x),NaN,sum(x^2,na.rm=TRUE))})
    US_yields_q[count,4] <- as.Date(paste(toString(Year),".",toString(1+3*(Quarter-1)),".01",sep=""),"%Y.%m.%d")
  }
}

plot(US_yields_q$date,
     US_yields_q$SVENY05.condiVar,
     type="l")



#===============================================================================
# Macro Data
#===============================================================================

macro.data <- read.csv("data/macro_data.csv")
macro.data$date <- as.Date(macro.data$date,"%d.%m.%Y")

if(indic.same.dates.4.macro.fi == 1){
  first.date <- US_yields_q$date[1]
  last.date <- US_yields_q$date[dim(US_yields_q)[1]]
}else{
  first.date <- first.date.macro
  last.date <- last.date.macro
}

indic.first.date <- which(macro.data$date==first.date)
indic.last.date <- which(macro.data$date==last.date)


dy.US <- log(macro.data$GDP.US[indic.first.date:indic.last.date]/
               macro.data$GDP.US[(indic.first.date-1):(indic.last.date-1)])
dc.US <- log(macro.data$CONSO.NONDUR.US[indic.first.date:indic.last.date]/
               macro.data$CONSO.NONDUR.US[(indic.first.date-1):(indic.last.date-1)])
pi.US<- log(macro.data$GDPDEF.US[indic.first.date:indic.last.date]/
              macro.data$GDPDEF.US[(indic.first.date-1):(indic.last.date-1)])


par(plt=c(.1,.9,.1,.9))
plot(macro.data$date[indic.first.date:indic.last.date],dy.US,type="l")
lines(macro.data$date[indic.first.date:indic.last.date],dc.US,col="red")
lines(macro.data$date[indic.first.date:indic.last.date],pi.US,col="blue")

par(plt=c(.1,.9,.1,.9))
par(mfrow=c(1,2))
lines(macro.data$date[indic.first.date:indic.last.date],dy.US,col="red")
lines(macro.data$date[indic.first.date:indic.last.date],dc.US,col="red")


# Load SPF data:

gdp.forecast.4Q  <- macro.data$SPF_PDFbased_1Y_GDPgrowth[indic.first.date:indic.last.date]/100
gdp.forecast.8Q  <- macro.data$SPF_PDFbased_2Y_GDPgrowth[indic.first.date:indic.last.date]/100
infl.forecast.4Q <- macro.data$SPF_PDFbased_1Y_Infl[indic.first.date:indic.last.date]/100
infl.forecast.8Q <- macro.data$SPF_PDFbased_2Y_Infl[indic.first.date:indic.last.date]/100

plot(macro.data$date[indic.first.date:indic.last.date],pi.US,col="blue",type="l")
lines(macro.data$date,macro.data$SPF_PDFbased_1Y_Infl/400)
points(macro.data$date,macro.data$SPF_PDFbased_2Y_Infl/400)

plot(macro.data$date[indic.first.date:indic.last.date],dy.US,col="blue",type="l")
lines(macro.data$date,macro.data$SPF_PDFbased_1Y_GDPgrowth/400)
points(macro.data$date,macro.data$SPF_PDFbased_2Y_GDPgrowth/400)


#===============================================================================
# Stock prices data (Shiller database / http://www.econ.yale.edu/~shiller/data.htm)
#===============================================================================

Data_Shiller <- read.csv2("data/Shiller_Data_4R.csv")
Data_Shiller$Date <- as.Date(Data_Shiller$Date,"%d.%m.%Y")

indic.first.date.in.Shiller.database <- which(Data_Shiller$Date==first.date)
indic.last.date.in.Shiller.database <- which(Data_Shiller$Date==last.date)

Data_Shiller$P.real <- Data_Shiller$P/Data_Shiller$CPI
Data_Shiller$D.real <- Data_Shiller$D/Data_Shiller$CPI

stock.returns <- log((Data_Shiller$P + Data_Shiller$D)[indic.first.date.in.Shiller.database:indic.last.date.in.Shiller.database]/
                       Data_Shiller$P[(indic.first.date.in.Shiller.database-12):(indic.last.date.in.Shiller.database-12)])
stock.returns.real <- log((Data_Shiller$P.real + Data_Shiller$D.real)[indic.first.date.in.Shiller.database:indic.last.date.in.Shiller.database]/
                            Data_Shiller$P.real[(indic.first.date.in.Shiller.database-12):(indic.last.date.in.Shiller.database-12)])

plot(stock.returns,type="l")
avg.annualized.return <- mean(stock.returns)
std.annualized.return <- sd(stock.returns)

avg.annualized.return.real <- mean(stock.returns.real)
std.annualized.return.real <- sd(stock.returns.real)

# Prepare quarterly stock returns:
T.Shiller <- dim(Data_Shiller)[1]
Data_ShillerQ <- Data_Shiller[seq(1,T.Shiller,by=3),]
T.Shiller.Q <- dim(Data_ShillerQ)[1]
Data_ShillerQ$stock.returns.real <- NaN
Data_ShillerQ$stock.returns.real[5:T.Shiller.Q] <- 
  log((Data_ShillerQ$P.real + Data_ShillerQ$D.real)[5:T.Shiller.Q]/
        Data_ShillerQ$P.real[1:(T.Shiller.Q-4)])


# US ===========================================================================

param.HP <- 100 # for computation of trends in y, c and pi.

# Estimation of eps.y using the HO filter:
HP.dy.US <- hpfilter(dy.US,freq=param.HP,type=c("lambda"))
dy.US.trend <- HP.dy.US$trend
eps.y.US <- HP.dy.US$cycle
plot(macro.data$date[indic.first.date:indic.last.date],dy.US,type="l")
lines(macro.data$date[indic.first.date:indic.last.date],dy.US.trend,col="red")
lines(macro.data$date[indic.first.date:indic.last.date],eps.y.US,col="blue",lty=2)

# Estimation of eps.c using the HO filter:
HP.dc.US <- hpfilter(dc.US,freq=param.HP,type=c("lambda"))
dc.US.trend <- HP.dc.US$trend
eps.c.US <- HP.dc.US$cycle
plot(macro.data$date[indic.first.date:indic.last.date],dc.US,type="l")
lines(macro.data$date[indic.first.date:indic.last.date],dc.US.trend,col="red")
lines(macro.data$date[indic.first.date:indic.last.date],eps.c.US,col="blue",lty=2)

# Estimation of eps.pi using the HO filter:
HP.pi.US <- hpfilter(pi.US,freq=param.HP,type=c("lambda"))
pi.US.trend <- HP.pi.US$trend
eps.pi.US <- HP.pi.US$cycle
plot(macro.data$date[indic.first.date:indic.last.date],pi.US,type="l")
lines(macro.data$date[indic.first.date:indic.last.date],pi.US.trend,col="red")
lines(macro.data$date[indic.first.date:indic.last.date],eps.pi.US,col="blue",lty=2)

matrix.eps <- cbind(eps.y.US,eps.c.US,eps.pi.US)
Omega.US <- var(matrix.eps) # Estimation of Omega based on HP-estimated eps
print(cor(matrix.eps))



#===============================================================================
# US 3-month real rate data
#===============================================================================
US_survey_3m <- read.csv("data/Mean_CPI_Level.csv")

US_survey_3m$date <- as.Date(
  paste("01-",
        US_survey_3m$QUARTER*3-2,
        "-",
        US_survey_3m$YEAR,sep=""),
  "%d-%m-%Y")


endT <- dim(matrix(US_yields_q$DTB3))[1]      # yields end in Jan 2018    
endT2 <- dim(matrix(US_survey_3m$CPI2))[1]-5  # surveys end in 2019

US_real_3m <- US_yields_q$DTB3[41:endT] - US_survey_3m$CPI2[106:endT2]

US_yields_q$US_real_3m_SurveyBased <- NaN
US_yields_q$US_real_3m_SurveyBased[41:endT] <- US_real_3m

