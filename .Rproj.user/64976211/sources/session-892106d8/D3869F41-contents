
y.lim <- c(-0.02,.08)
#y.lim <- c(-0.02,.04)

area <- "US"

#NB.values.s <- 50

max.matur.4.TS.charts <- 50 # in years

# Multiplier controls the width of shaded areas (in terms of std dev)
multiplier <- abs(qnorm(.025))
multiplier <- 1


file <- "Figure_avg_curves"
pdf(file=paste(output.figures.folder,"/",file,".pdf",sep=""),
    pointsize=10, width=6, height=4)

Model.est <- Theta.2.Model(Full.Theta)

vec.maturities.postestim <- 1:(Model.est$freq*max.matur.4.TS.charts)

res.prices <- compute.prices(Model.est,
                             vec.maturities.postestim,
                             nb.values.s = NB.values.s,
                             h.stock = 1,
                             curvature = CURVATURE,
                             grid.4.S = NaN)
res.mom.pi.z <- compute.uncond.mom.pi.z(res.prices$Model.solved)

# Plots of average model-implied rates:
res.all.moments <- compute.moments(res.prices$Model.solved,
                                   vec.maturities.postestim,
                                   nb.values.s = NB.values.s,
                                   h.stock = 1,
                                   curvature = CURVATURE,
                                   grid.4.S = NaN,
                                   indic.slope.nom.curve,
                                   indic.slope.rea.curve,
                                   indic.condVar.nom.rate,
                                   indic.condVar.rea.rate)


par(mfrow=c(1,1))
par(plt=c(.13,.95,.22,.95))

lower.bound.nom <- res.all.moments$mean.nom.yields - multiplier * sqrt(diag(res.all.moments$var.nom.yds))
upper.bound.nom <- res.all.moments$mean.nom.yields + multiplier * sqrt(diag(res.all.moments$var.nom.yds))

lower.bound.rea <- res.all.moments$mean.rea.yields - multiplier * sqrt(diag(res.all.moments$var.rea.yds))
upper.bound.rea <- res.all.moments$mean.rea.yields + multiplier * sqrt(diag(res.all.moments$var.rea.yds))

lower.bound.GDP <- res.all.moments$mean.GDP.yields - multiplier * sqrt(diag(res.all.moments$var.GDP.yds))
upper.bound.GDP <- res.all.moments$mean.GDP.yields + multiplier * sqrt(diag(res.all.moments$var.GDP.yds))

plot(vec.maturities.postestim/Model.est$freq,
     res.all.moments$mean.nom.yields,type="l",ylim=y.lim,lwd=2,
     xlab="Maturity (in years)",ylab="Annualized yield to maturity",
     las=1,xlim=c(1,max(vec.maturities.postestim)/FREQ),
     col="white")

if(indic.plot.nominal == 1){
  lines(vec.maturities.postestim/Model.est$freq,col="dark grey",
        res.all.moments$mean.nom.yields,lwd=2,lty=2)
}

lines(vec.maturities.postestim/Model.est$freq,
      res.all.moments$mean.rea.yields,col="dark grey",lwd=2)

lines(vec.maturities.postestim/Model.est$freq,
      res.all.moments$mean.GDP.yields,col="black",lwd=2)

if((area == "US")|is.na(area)){
  vector.of.avg.nom.yds <- c(
    mean(US_yields_q$DTB3,na.rm=TRUE)/100,    
    mean(US_yields_q$SVENY02,na.rm=TRUE)/100,    
    mean(US_yields_q$SVENY10,na.rm=TRUE)/100,    
    mean(US_yields_q$SVENY30,na.rm=TRUE)/100
  )
  if(indic.plot.nominal == 1){
    points(vec.maturities.4.estimation/Model.est$freq,
           vector.of.avg.nom.yds,pch=1,cex=1.5,lwd=2,col="black")
  }
  
  vector.of.avg.rea.yds <- c(
    NaN,    
    mean(US_yields_q$TIPSY02,na.rm=TRUE)/100,    
    mean(US_yields_q$TIPSY10,na.rm=TRUE)/100,    
    NaN
  )
  points(vec.maturities.4.estimation/Model.est$freq,
         vector.of.avg.rea.yds,pch=19,cex=1.5,lwd=2,col="dark grey")
}

#col.nom.shaded <- "#EECCEEBB"
#col.rea.shaded <- "#CCEEEEBB"

if(indic.plot.nominal){
  legend("bottomright", # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend 
         c("Average nominal rates (model)",
           "Average nominal rates (data)",
           "Average ILB rates (model)",
           "Average ILB rates (data)",
           "Average GDP-LB rates (model)"
         ),
         lty=c(2,NaN,1,NaN,1), # gives the legend appropriate symbols (lines)       
         lwd=c(2,2,2,2,2), # line width
         col=c("dark grey","black","dark grey","dark grey","black"), # gives the legend lines the correct color and width
         pt.bg=c(NaN,"red",NaN,"dark grey",NaN),
         #text.width = 2,
         #cex=1.0,# size of the text
         pch = c(NaN,1,NaN,19,NaN),#symbols,
         pt.cex = c(NaN,1.5,NaN,1.5,NaN),
         bg="white",
         seg.len = 3
  )
}else{
  legend("bottomright", # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend 
         c("Average ILB rates (model)",
           "Average ILB rates (data)",
           "Average GDP-LB rates (model)"
         ),
         lty=c(1,NaN,1), # gives the legend appropriate symbols (lines)       
         lwd=c(4,2,4), # line width
         col=c("dark grey","dark grey","black"), # gives the legend lines the correct color and width
         pt.bg=c(NaN,"blue",NaN),
         #text.width = 2,
         #cex=1.0,# size of the text
         pch = c(NaN,19,NaN),#symbols,
         pt.cex = c(NaN,1.5,NaN),
         bg="white",
         seg.len = 3
  )
}



dev.off()













