
# ==============================================================================
# Debt Dynamics
# Exercise with debt-stabilizing surplus
# ==============================================================================


# For simulations, the maximum maturity will be the max entry of vector.of.H:
max.matur <- max(vector.of.H) # expressed at the model frequency

smoothing.H       <- Model$freq # lag used to measure change in bs

nb.periods.2.be.removed <- 100 # at the beginning of the sample


# ==============================================================================
# Solve the model:
vec.maturities <- 1:max.matur
res.prices <- compute.prices(Model,
                             vec.maturities,
                             nb.values.s = NB.values.s,
                             h.stock = 1,
                             curvature = CURVATURE,
                             grid.4.S = NaN)


# ==============================================================================
# Compute model-implied moments:
res.mom.pi.z <- compute.uncond.mom.pi.z(res.prices$Model.solved)


# ==============================================================================
# Simulate model:
print("")
print("")
print("==========================================================================")
print(" Simulation of a long sample (can take some time)")
print("==========================================================================")

res.simul <- simul.model.solved(res.prices,T = nb.periods)


# ==============================================================================
# Look at simulated yields:
maturity <- min(20,max.matur)
par(mfrow=c(3,2))
plot(res.simul$nom.yields[maturity,],type="l",ylim=c(0,.02))
lines(res.simul$rea.yields[maturity,],col="red")
lines(res.simul$GDP.yields[maturity,],col="blue")
plot(res.simul$sim.nu,type="l",las=1)
par(new=TRUE)
plot(res.simul$sim.z.integer,type="l",col="red",xaxt="n",yaxt="n",
     col.axis="red",xlab="",
     ylab="")
axis(side = 4,
     labels=FALSE, # because we will use mtext to choose the color
     col="red")
mtext("XXXXX", side=4, line=3,col="black")
at = axTicks(2)
mtext(side = 4, text = at, at = at,
      col = "red",
      line = 1,
      las=1 # vertical reading
)

plot(res.simul$sim.pi,type="l")
plot(res.simul$sim.y,type="l")



# ==============================================================================
# Prepare inputs for "simulate.debt":

# prepare
simulated.yields <- list(
  nom.bond.yields = res.simul$nom.yields,
  rea.bond.yields = res.simul$rea.yields,
  GDP.bond.yields = res.simul$GDP.yields
)

simulated.inflation  <- res.simul$sim.pi
simulated.GDP.growth <- res.simul$sim.y

simulated.GDP.expectation <- res.simul$GDP.fcsts + 
  matrix(.5*res.prices$Model.solved$sigma.y^2,
         dim(res.simul$GDP.fcsts)[1],
         dim(res.simul$GDP.fcsts)[2])


# Initial issuance schedule, expressed in fractions of GDP:
aux.ini.issuances <- matrix(0,max.matur,max.matur)
aux.ini.issuances[!upper.tri(aux.ini.issuances)] <- 1
aux.ini.issuances <- aux.ini.issuances[,max.matur:1]

ini.issuances <- list()
ini.issuances$nom.bonds <- aux.ini.issuances
ini.issuances$rea.bonds <- aux.ini.issuances
ini.issuances$GDP.bonds <- aux.ini.issuances

sum.weights <- sum(ini.issuances$nom.bonds +
                     ini.issuances$rea.bonds +
                     ini.issuances$GDP.bonds)
weights <- ini.debt.to.GDP/sum.weights

ini.issuances$nom.bonds <- weights * ini.issuances$nom.bonds
ini.issuances$rea.bonds <- weights * ini.issuances$rea.bonds
ini.issuances$GDP.bonds <- weights * ini.issuances$GDP.bonds


param <- list() # matrix of weights indicating the types of bonds that are issued on each period
param$matrix <- cbind(
  matrix(0,max.matur,1), # nominal bonds
  matrix(0,max.matur,1), # inflation-linked (real) bonds
  matrix(1,max.matur,1) # nominal-GDP-indexed bonds
)
param$matrix <- param$matrix/sum(param$matrix)


# ==============================================================================
# Simulate debt dynamics:

print("")
print("")
print("==========================================================================")
print(" Simulation of debt dynamics (can take some time)")
print("==========================================================================")

res.sim.debt <- simulate.debt(simulated.yields,
                              simulated.inflation,
                              simulated.GDP.growth,
                              simulated.GDP.expectation,
                              ini.issuances,
                              param,
                              freq = Model$freq)

plot(res.sim.debt$all.bs,type="l",main="s_t")
lines(res.sim.debt$Rolling.bs,type="l",col="red",lwd=1)

plot(res.sim.debt$all.iss,type="l",main="Issuances")





all.bs <- NULL
all.rolling.bs <- NULL

all.avg.bs <- NULL
all.sd.bs <- NULL
all.cov.bs.og <- NULL
all.weights <- NULL
all.H <- NULL
all.txt.strategy <- NULL
for(H in vector.of.H){
  for(type.of.bond in c(1,2,3)){
    AUX <- rep(0,3)
    AUX[type.of.bond] <- 1
    weights.nom <- AUX[1]
    weights.rea <- AUX[2]
    weights.GDP <- AUX[3]
    print(paste(" Maturity: ",toString(H)," (out of ",toString(length(vector.of.H)),"), ",
                " Type of bond: ",toString(type.of.bond)," (out of 3).",
                sep=""))
    param <- list() # matrix of weights indicating the types of bonds are issued on each period
    param$matrix <- cbind(
      matrix(weights.nom,max.matur,1), # nominal bonds
      matrix(weights.rea,max.matur,1), # inflation-linked (real) bonds
      matrix(weights.GDP,max.matur,1) # nominal-GDP-indexed bonds
    )
    # remove maturities larger than H:
    if(H<max(vector.of.H)){
      param$matrix[(H+1):max.matur,] <- 0
    }
    # Alternative approach: only bonds of maturity H are issued:
    # param$matrix <- matrix(0,max.matur,3)
    # param$matrix[H,] <- c(weights.nom,weights.rea,weights.GDP)
    
    param$matrix <- param$matrix/sum(param$matrix)
    
    if(sum(AUX)>0){
      all.txt.strategy <- rbind(all.txt.strategy,
                                paste("(",toString(weights.nom),",",toString(weights.rea),",",toString(weights.GDP),")",sep=""))
      all.weights <- rbind(all.weights,
                           AUX)
      all.H <- rbind(all.H,H)
      # Compute budget surplus:
      res.sim.debt <- simulate.debt(simulated.yields,
                                    simulated.inflation,
                                    simulated.GDP.growth,
                                    simulated.GDP.expectation,
                                    ini.issuances,
                                    param,
                                    freq = Model$freq)
      
      all.bs         <- cbind(all.bs,res.sim.debt$all.bs[(nb.periods.2.be.removed+1):nb.periods])
      all.rolling.bs <- cbind(all.rolling.bs,res.sim.debt$Rolling.bs[(nb.periods.2.be.removed+1):nb.periods])
      
      all.avg.bs <- c(all.avg.bs,
                      mean(res.sim.debt$Rolling.bs[(nb.periods.2.be.removed+1):nb.periods],
                           na.rm=TRUE))
      all.sd.bs <- c(all.sd.bs,
                     sd(res.sim.debt$Rolling.bs[(nb.periods.2.be.removed+1):nb.periods],
                        na.rm=TRUE))
    }
  }
}



nb.strategies <- dim(all.weights)[1]


for(jjj in 1:2){# 2 charts are prepared
  if(jjj==1){
    all.sd.bs <- apply(all.rolling.bs,2,sd)
  }else if(jjj==2){
    all.sd.bs <- apply(all.rolling.bs[(smoothing.H+1):(dim(all.rolling.bs)[1]),]-
                         all.rolling.bs[(1):(dim(all.rolling.bs)[1]-smoothing.H),],2,sd)
  }
  
  file <- ifelse(jjj==1,"Figure_CostRisk_SDbs","Figure_CostRisk_FCSTbs")
  
  pdf(file=paste(output.figures.folder,"/",file,".pdf",sep=""),
      pointsize=10, width=6, height=4)
  
  par(mfrow=c(1,1))
  par(plt=c(.15,.95,.23,.95))
  plot(all.avg.bs,all.sd.bs,pch=19,
       xlab="Average of debt-stabilizing budget surplus (% of GDP)",
       ylab=ifelse(jjj==1,
                   "Std. dev. of budget surplus (% of GDP)",
                   "Std. dev. of change in budget surplus (% of GDP)"),
       col="white",las=1,
       xlim=c(min(all.avg.bs) - 1*(max(all.avg.bs)-min(all.avg.bs)),max(all.avg.bs)),
       ylim=c(.9*min(all.sd.bs),1.1*max(all.sd.bs)))
  #text(x=all.avg.bs,y=all.sd.bs-.1*sd(all.sd.bs),labels = all.txt.strategy)
  for(i in 1:nb.strategies){
    if(all.weights[i,1]==1){
      bg.strat <- "white"
    }else if(all.weights[i,2]==1){
      bg.strat <- "dark grey"
    }else{
      bg.strat <- "black"
    }
    points(all.avg.bs[i],all.sd.bs[i],pch=21,
           bg=bg.strat,lwd=1,
           #col=bg.strat,
           cex=3*sqrt(all.H[i])/sqrt(mean(vector.of.H)))
  }
  
  legend("topleft", # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend 
         c("Nominal (N)","Inflation-linked (IL)","GDP-linked (GDP-L)",
           paste("Maturity: ",toString(vector.of.H[1]/4),ifelse(vector.of.H[1]<=4," year"," years"),sep=""),
           paste("Maturity: ",toString(vector.of.H[2]/4)," years",sep=""),
           paste("Maturity: ",toString(vector.of.H[3]/4)," years",sep="")
         ),
         lty=c(NaN), # gives the legend appropriate symbols (lines)       
         lwd=1, # line width
         pt.bg=c("white","dark grey","black",
                 "white","white","white"), # gives the legend lines the correct color and width
         col="black",
         pch = c(22,22,22,21,21,21),#symbols,
         pt.cex = c(rep(1.5,3),2*sqrt(vector.of.H)/sqrt(mean(vector.of.H))),
         bg="white",
         seg.len = 2
  )
  
  
  dev.off()
}




# ==============================================================================
# Chart with different measures on same Figure

file <- "Figure_4CostRisk"
pdf(file=paste(output.figures.folder,"/",file,".pdf",sep=""),
    pointsize=10, width=8, height=8)


par(mfrow=c(2,2))
par(plt=c(.2,.95,.23,.85))

for(jjj in 1:4){# 2 charts are prepared
  if(jjj==1){
    all.sd.bs <- apply(all.rolling.bs,2,sd)
    main.t = "Risk measure: Std dev. of DSBS"
  }else if(jjj==2){
    all.sd.bs <- apply(all.rolling.bs[(smoothing.H+1):(dim(all.rolling.bs)[1]),]-
                         all.rolling.bs[(1):(dim(all.rolling.bs)[1]-smoothing.H),],2,sd)
    main.t = "Risk measure: Std dev. of chges in DSBS"
  }else if(jjj==3){
    all.sd.bs <- apply(all.rolling.bs,2,function(x){quantile(x,.9)})
    main.t = "Risk measure: 90th percentile of DSBS"
  }else if(jjj==4){
    all.sd.bs <- apply(all.rolling.bs,2,function(x){quantile(x,.95)})
    main.t = "Risk measure: 95th percentile of DSBS"
  }
  
  max.risk <- max(all.sd.bs)
  min.risk <- min(all.sd.bs)
  distance <- max.risk - min.risk
  if(jjj>1){
    y.lim <- c(min.risk - .1*distance, max.risk + .1*distance)
  }else{
    y.lim <- c(min.risk - .1*distance, max.risk + .6*distance)
  }
  plot(all.avg.bs,all.sd.bs,pch=19,
       xlab="Average of DSBS (% of GDP)",
       ylab= "% of GDP",
       col="white",las=1,
       xlim=c(min(all.avg.bs) - .1*(max(all.avg.bs)-min(all.avg.bs)),max(all.avg.bs)),
       ylim=y.lim,
       main = main.t
  )
  for(i in 1:nb.strategies){
    if(all.weights[i,1]==1){
      bg.strat <- "white"
    }else if(all.weights[i,2]==1){
      bg.strat <- "dark grey"
    }else{
      bg.strat <- "black"
    }
    points(all.avg.bs[i],all.sd.bs[i],pch=21,
           bg=bg.strat,lwd=1,
           #col=bg.strat,
           cex=3*sqrt(all.H[i])/sqrt(mean(vector.of.H)))
  }
  
  if(jjj==1){
    legend("topleft",
           c("Nominal (N)","Inflation-linked (IL)","GDP-linked (GDP-L)",
             paste("Maturity: ",toString(vector.of.H[1]/4),ifelse(vector.of.H[1]<=4," year"," years"),sep=""),
             paste("Maturity: ",toString(vector.of.H[2]/4)," years",sep=""),
             paste("Maturity: ",toString(vector.of.H[3]/4)," years",sep="")
           ),
           lty=c(NaN), # gives the legend appropriate symbols (lines)       
           lwd=1, # line width
           pt.bg=c("white","dark grey","black",
                   "white","white","white"), # gives the legend lines the correct color and width
           col="black",
           #text.width = 2,
           #cex=1.0,# size of the text
           pch = c(22,22,22,21,21,21),#symbols,
           pt.cex = c(rep(1.5,3),2*sqrt(vector.of.H)/sqrt(mean(vector.of.H))),
           bg="white",
           seg.len = 2
    )
  }
  
}

dev.off()








