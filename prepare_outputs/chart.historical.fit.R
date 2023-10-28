

# Approximation method:
NB.values.s <- 100

max.matur <- 40 # expressed at the model frequency
vec.maturities <- 1:max.matur

# Build estimated model:
Model.est <- Theta.2.Model(Full.Theta)

vec.dates <- macro.data$date[indic.first.date:indic.last.date]

# ==============================================================================
# Solve the model and get prices' specifications:
res.prices <- compute.prices(Model.est,
                             vec.maturities.4.estimation,
                             nb.values.s = NB.values.s,
                             h.stock = 1,
                             curvature = CURVATURE,
                             grid.4.S = NaN)
# Computation of  bond yields under P (for term premium chart):
res.prices.underP <- compute.prices(Model.est,
                                    vec.maturities.4.estimation,
                                    nb.values.s = NB.values.s,
                                    h.stock = 1,
                                    curvature = CURVATURE,
                                    grid.4.S = NaN,
                                    indic.compute.TP = 1)

# Run filtering approach:
res.estim <-  estimate.latent.factors(res.prices,
                                      dc.US,dy.US,pi.US,
                                      gdp.forecast.4Q,infl.forecast.4Q,
                                      gdp.forecast.8Q,infl.forecast.8Q,
                                      US_yields_q)

# Save esitmated regime and inflation process:
z.hat  <- res.estim$z.hat
pi.hat <- res.estim$pi.hat

# ==============================================================================
# Term premium chart :


# Comparison with Kim and Wright:
USY_Yds_TermPremium <- read.csv("data/USY_Yds_TermPremium.csv")
USY_Yds_TermPremium$DATE <- as.Date(USY_Yds_TermPremium$DATE)
first.year <- as.numeric(format(USY_Yds_TermPremium$DATE[1],"%Y"))
last.year  <- as.numeric(format(last.date,"%Y"))
USY_Yds_TermPremium$year  <- as.numeric(format(USY_Yds_TermPremium$DATE,"%Y"))
USY_Yds_TermPremium$month <- as.numeric(format(USY_Yds_TermPremium$DATE,"%m"))
USY_Yds_TermPremium_Q <- data.frame(USY_Yds_TermPremium[1,])
count <- 0
for(Year in first.year:last.year){
  for(Q in 1:4){
    count <- count + 1
    data.tempo <- subset(USY_Yds_TermPremium,(year==Year)&(month %in% c(3*(Q-1) + 1:3)))
    USY_Yds_TermPremium_Q[count,2:dim(USY_Yds_TermPremium_Q)[2]] <- 
      apply(data.tempo[,2:dim(USY_Yds_TermPremium_Q)[2]],2,function(x){mean(x,na.rm=TRUE)})
    USY_Yds_TermPremium_Q$DATE[count] <- as.Date(paste(Year,"-",3*(Q-1)+1,"-01",sep=""))
  }
}

file   <- "Figure_termpremiums"
pdf(file=paste(output.figures.folder,"/",file,".pdf",sep=""),
    pointsize=8, width=9, height=4)

fitted.2yr.underP <- res.prices.underP$all.nom.bond.F.yields[2,z.hat] +
  res.prices.underP$all.nom.bond.b.yields[2] * pi.hat
fitted.2yr.underQ <- res.prices$all.nom.bond.F.yields[2,z.hat] +
  res.prices$all.nom.bond.b.yields[2] * pi.hat
fitted.10yr.underP <- res.prices.underP$all.nom.bond.F.yields[3,z.hat] +
  res.prices.underP$all.nom.bond.b.yields[3] * pi.hat
fitted.10yr.underQ <- res.prices$all.nom.bond.F.yields[3,z.hat] +
  res.prices$all.nom.bond.b.yields[3] * pi.hat

data.frame.TP <- data.frame(DATE=vec.dates,
                            TP2=100*(fitted.2yr.underQ-fitted.2yr.underP),
                            TP10=100*(fitted.10yr.underQ-fitted.10yr.underP))
data.frame.TP <- merge(data.frame.TP,USY_Yds_TermPremium_Q,by="DATE")
cor(data.frame.TP$TP2,data.frame.TP$THREEFYTP2)
cor(data.frame.TP$TP10,data.frame.TP$THREEFYTP10)


par(mfrow=c(1,2))
par(plt=c(.15,.95,.1,.85))
plot(vec.dates,fitted.2yr.underQ,type="l",lwd=2,ylim=c(0,.09),
     main="(a) 2-year maturity",xlab="",ylab="",las=1)
lines(vec.dates,fitted.2yr.underP,col="dark grey",lwd=2,las=1)
lines(vec.dates,fitted.2yr.underQ-fitted.2yr.underP,col="blue",lwd=3,lty=3)
grid()
legend("topright", # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend 
       c("yield","yield under EH","Term premium"),
       lty=c(1,1,3), # gives the legend appropriate symbols (lines)       
       lwd=c(2,2,3), # line width
       col=c("black","dark grey","blue"),
       bg="white",
       #pch=c(NaN,22),
       #pt.bg=c(NaN,rgb(0,0,0,alpha=0.2)),
       #pt.cex = c(2),
       seg.len = 2
)
plot(vec.dates,fitted.10yr.underQ,type="l",ylim=c(0,.09),
     main="(b) 10-year maturity",xlab="",ylab="",lwd=2,las=1)
lines(vec.dates,fitted.10yr.underP,col="dark grey",lwd=2)
lines(vec.dates,fitted.10yr.underQ-fitted.10yr.underP,col="blue",lwd=3,lty=3)
grid()


# plot(vec.dates,fitted.2yr.underQ-fitted.2yr.underP,type="l",lwd=3,lty=3,
#      ylim=c(-.005,.015),col="blue",
#      main="(c) 2-year term premium",xlab="",ylab="",las=1)
# lines(USY_Yds_TermPremium_Q$DATE,USY_Yds_TermPremium_Q$THREEFYTP2/100,
#       col="dark grey",lwd=3,lty=1)
# grid()
# legend("topright", # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend 
#        c("Model","Kim and Wright"),
#        lty=c(3,3), # gives the legend appropriate symbols (lines)       
#        lwd=c(3,3), # line width
#        col=c("blue","dark grey"),
#        bg="white",
#        #pch=c(NaN,22),
#        #pt.bg=c(NaN,rgb(0,0,0,alpha=0.2)),
#        #pt.cex = c(2),
#        seg.len = 2
# )
# plot(vec.dates,fitted.10yr.underQ-fitted.10yr.underP,type="l",lwd=3,lty=3,
#      ylim=c(-.005,.05),col="blue",
#      main="(d) 10-year term premium",xlab="",ylab="",las=1)
# lines(USY_Yds_TermPremium_Q$DATE,USY_Yds_TermPremium_Q$THREEFYTP10/100,
#       col="dark grey",lwd=3,lty=1)


dev.off()



# ==============================================================================
# Fit of historical data :


file   <- "Figure_historical_fit_yds"
pdf(file=paste(output.figures.folder,"/",file,".pdf",sep=""),
    pointsize=10, width=7, height=5)


par(mfrow=c(2,2))
par(plt=c(.1,.95,.2,.85))

maturities.in.yrs.yields <- c(2,10)
indic.of.yds.in.res.prices <- c(
  which(res.prices$vec.maturities == Model$freq*maturities.in.yrs.yields[1]),
  which(res.prices$vec.maturities == Model$freq*maturities.in.yrs.yields[2])
)

# Check resulting 2-year real rates:
resulting.2Y.TIPS <- res.prices$all.rea.bond.yields[indic.of.yds.in.res.prices[1],z.hat]
plot(vec.dates,resulting.2Y.TIPS,type="l",lwd=2,
     main="(a) 2-year real yield",xlab="",ylab="")
points(vec.dates,US_yields_q$TIPSY02/100,col="dark grey",pch=3,lwd=2)

legend("topright", # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend 
       c("Data","Model"),
       lty=c(NaN,1), # gives the legend appropriate symbols (lines)       
       lwd=c(2,2), # line width
       col=c("dark grey","black"),
       bg="white",
       pch=c(3,NaN),
       #pt.bg=c(NaN,rgb(0,0,0,alpha=0.2)),
       #pt.cex = c(2),
       seg.len = 2
)

# Check resulting 10-year real rates:
resulting.10Y.TIPS <- res.prices$all.rea.bond.yields[indic.of.yds.in.res.prices[2],z.hat]
plot(vec.dates,resulting.10Y.TIPS,type="l",lwd=2,
     main="(b) 10-year real yield",xlab="",ylab="")
points(vec.dates,US_yields_q$TIPSY10/100,col="dark grey",pch=3,lwd=2)

# Check resulting 2-year nominal rates:
# resulting.2Y.nom <- res.prices$all.nom.bond.F.yields[8,z.hat] + 
#   res.prices$all.nom.bond.b.yields[8] * pi.hat
resulting.2Y.nom <- res.estim$fitted[,1]
plot(vec.dates,resulting.2Y.nom,type="l",lwd=2,
     main="(c) 2-year nominal yield",xlab="",ylab="")
points(vec.dates,US_yields_q$SVENY02/100,col="dark grey",pch=3,lwd=2)

# Check resulting 10-year nominal rates:
observed.10Y.nom  <- res.estim$observed[,2]
resulting.10Y.nom <- res.estim$fitted[,2]
plot(vec.dates,resulting.10Y.nom,type="l",lwd=2,
     main="(d) 10-year nominal yield",xlab="",ylab="")
points(vec.dates,US_yields_q$SVENY10/100,col="dark grey",pch=3,lwd=2)

dev.off()




file   <- "Figure_historical_fit_macro"
pdf(file=paste(output.figures.folder,"/",file,".pdf",sep=""),
    pointsize=10, width=8, height=7)


par(mfrow=c(3,2))
par(plt=c(.1,.95,.2,.85))

# Plot fitted inflation:
plot(vec.dates,pi.hat,type="l",lwd=2,
     main="(a) Inflation",xlab="",ylab="")
points(vec.dates,pi.US,col="dark grey",pch=3,lwd=2)
legend("topright", # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend 
       c("Data","Model"),
       lty=c(NaN,1), # gives the legend appropriate symbols (lines)       
       lwd=c(2,2), # line width
       col=c("dark grey","black"),
       bg="white",
       pch=c(3,NaN),
       #pt.bg=c(NaN,rgb(0,0,0,alpha=0.2)),
       #pt.cex = c(2),
       seg.len = 2
)

# Recover delta(c_t):
z.hat_1 <- c(NaN,z.hat[1:(length(z.hat)-1)])
nu.hat <- 1/res.prices$Model.solved$lambda[z.hat_1] * (res.prices$Model.solved$mu[z.hat] -
                                                         (1-Model$phi)*res.prices$s.bar
                                                       - Model$phi * res.prices$Model.solved$mu[z.hat_1]
)
resulting.dc <- res.prices$Model.solved$g[z.hat_1] + nu.hat
plot(vec.dates,resulting.dc,type="l",lwd=2,
     main="(b) Consumption growth",xlab="",ylab="")
points(vec.dates,dc.US,col="dark grey",pch=3,lwd=2)


# # Stock returns
# # Compute quarterly stock return:
# T <- length(z.hat)
# Mt_1t <- apply(matrix(1:T,T,1),1,
#                function(i){res.prices$M[z.hat_1[i],z.hat[i]]})
# Rt_4t <- Mt_1t * c(NaN,Mt_1t[1:(T-1)]) *
#   c(NaN,NaN,Mt_1t[1:(T-2)]) * c(NaN,NaN,NaN,Mt_1t[1:(T-3)]) - 1
# plot(vec.dates,Rt_4t,col="red",type="l")
# # lines(Data_Shiller$Date[indic.first.date.in.Shiller.database:indic.last.date.in.Shiller.database],
# #       stock.returns,type="l")
# points(Data_ShillerQ$Date,Data_ShillerQ$stock.returns.real)


# Infl forecasts 4Q:
resulting.infl4Q <- res.estim$fitted[,6]
plot(vec.dates,resulting.infl4Q,type="l",
     ylim=c(min(resulting.infl4Q,infl.forecast.4Q,na.rm = TRUE),
            max(resulting.infl4Q,infl.forecast.4Q,na.rm = TRUE)),lwd=2,
     main="(c) 1-year inflation forecasts",xlab="",ylab="")
points(vec.dates,infl.forecast.4Q,col="dark grey",pch=3,lwd=2)

# GDP forecasts 4Q:
resulting.GDP4Q <- res.estim$fitted[,5]
plot(vec.dates,resulting.GDP4Q,type="l",
     ylim=c(min(resulting.GDP4Q,gdp.forecast.4Q,na.rm = TRUE),
            max(resulting.GDP4Q,gdp.forecast.4Q,na.rm = TRUE)),lwd=2,
     main="(d) 1-year GDP growth forecasts",xlab="",ylab="")
points(vec.dates,gdp.forecast.4Q,col="dark grey",pch=3,lwd=2)

# Infl forecasts 8Q:
resulting.infl8Q <- res.estim$fitted[,8]
plot(vec.dates,resulting.infl8Q,type="l",
     ylim=c(min(resulting.infl8Q,infl.forecast.8Q,na.rm = TRUE),
            max(resulting.infl8Q,infl.forecast.8Q,na.rm = TRUE)),lwd=2,
     main="(e) 2-year inflation forecasts",xlab="",ylab="")
points(vec.dates,infl.forecast.8Q,col="dark grey",pch=3,lwd=2)

# GDP forecasts 8Q:
resulting.GDP8Q <- res.estim$fitted[,7]
plot(vec.dates,resulting.GDP8Q,type="l",
     ylim=c(min(resulting.GDP8Q,gdp.forecast.8Q,na.rm = TRUE),
            max(resulting.GDP8Q,gdp.forecast.8Q,na.rm = TRUE)),lwd=2,
     main="(f) 2-year GDP growth forecasts",xlab="",ylab="")
points(vec.dates,gdp.forecast.8Q,col="dark grey",pch=3,lwd=2)



dev.off()

