
# ==============================================================================
# Sensitivity of yields to consumption surplus
# ==============================================================================


names.of.regimes <- c("Low-growth regime",
                      "Medium-growth regime",
                      "High-growth regime")

Model <- Theta.2.Model(Full.Theta)

ylim <- c(-.05,.13)

# Approximation method:
# NB.values.s <- 300
# index.min.S <- 50
# index.min.S.4.price_sensitivity <- 50
# index.max.S.4.price_sensitivity <- 295

NB.values.s <- 50
index.min.S <- 5
index.min.S.4.price_sensitivity <- 5
index.max.S.4.price_sensitivity <- 45


# For simulations, the maximum maturity will be the max entry of vector.of.H:
max.matur <- 40 # expressed at the model frequency

matur.shown.curves <- c(4,40)

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


file <- "Figure_price_sensitivity"
pdf(file=paste(output.figures.folder,"/",file,".pdf",sep=""),
    pointsize=10, width=9, height=4)


par(mfrow=c(1,3))
par(plt=c(.16,.95,.15,.85))

for(j in 1:3){# Loop on regimes
  plot(res.prices$values.S[index.min.S.4.price_sensitivity:index.max.S.4.price_sensitivity],
       res.prices$all.GDP.bond.yields[matur.shown.curves[1],index.min.S.4.price_sensitivity:index.max.S.4.price_sensitivity],
       type="l",lwd=2,las=1,col="white",
       ylim = ylim,
       main=names.of.regimes[j],
       xlab=expression(paste("Consumption surplus (",S[t],")",sep="")),
       ylab="Annualized yield to maturity")
  
  adj <- (res.prices$values.S[(index.min.S.4.price_sensitivity+1):index.max.S.4.price_sensitivity] - res.prices$values.S[(index.min.S.4.price_sensitivity):(index.max.S.4.price_sensitivity-1)])
  #/(res.prices$values.s[(index.min.S.4.price_sensitivity+1):NB.values.s] - res.prices$values.s[(index.min.S.4.price_sensitivity):(NB.values.s-1)])
  adj <- 1/adj
  Ez <- res.mom.pi.z$E.z[(NB.values.s*(j-1)+index.min.S.4.price_sensitivity+1):
                           (NB.values.s*(j-1)+index.max.S.4.price_sensitivity)]
  polygon(c(res.prices$values.S[(index.min.S.4.price_sensitivity+1):index.max.S.4.price_sensitivity],res.prices$values.S[index.max.S.4.price_sensitivity:(index.min.S.4.price_sensitivity+1)]),
          .002*c(adj*Ez,0*res.mom.pi.z$E.z[(index.min.S.4.price_sensitivity+1):index.max.S.4.price_sensitivity]),
          col=rgb(0,0,0,alpha=0.2),border=NA)
  #abline(v=res.prices$values.S[res.prices$index.s.bar],lwd=1,col="blue")
  
  
  lines(
    res.prices$values.S[index.min.S.4.price_sensitivity:index.max.S.4.price_sensitivity],
    res.prices$all.GDP.bond.yields[matur.shown.curves[1],(NB.values.s*(j-1)+index.min.S.4.price_sensitivity):
                                     (NB.values.s*(j-1)+index.max.S.4.price_sensitivity)],
    lty=3,lwd=2)
  lines(
    res.prices$values.S[index.min.S.4.price_sensitivity:index.max.S.4.price_sensitivity],
    res.prices$all.GDP.bond.yields[matur.shown.curves[2],(NB.values.s*(j-1)+index.min.S.4.price_sensitivity):
                                     (NB.values.s*(j-1)+index.max.S.4.price_sensitivity)],
    lty=1,lwd=2)
  lines(
    res.prices$values.S[index.min.S.4.price_sensitivity:index.max.S.4.price_sensitivity],
    res.prices$all.rea.bond.yields[matur.shown.curves[1],(NB.values.s*(j-1)+index.min.S.4.price_sensitivity):
                                     (NB.values.s*(j-1)+index.max.S.4.price_sensitivity)],
    lty=3,lwd=2,col="dark grey")
  lines(
    res.prices$values.S[index.min.S.4.price_sensitivity:index.max.S.4.price_sensitivity],
    res.prices$all.rea.bond.yields[matur.shown.curves[2],(NB.values.s*(j-1)+index.min.S.4.price_sensitivity):
                                     (NB.values.s*(j-1)+index.max.S.4.price_sensitivity)],
    lty=1,lwd=2,col="dark grey")
  
  if(j==1){
    legend("topright", # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend 
           c("GDP-LB (1 year)","GDP-LB (10 years)",
             "ILB (1 year)","ILB (10 years)",
             expression(paste("p.d.f. of consumption surplus (",S[t],")",sep=""))
           ),
           lty=c(3,1,3,1,NaN), # gives the legend appropriate symbols (lines)       
           lwd=c(2,2,2,2), # line width
           col=c("black","black","dark grey","dark grey","grey"),
           bg="white",
           pch=c(NaN,NaN,NaN,NaN,22),
           pt.bg=c(NaN,NaN,NaN,NaN,"grey"),
           pt.cex = c(NaN,NaN,NaN,NaN,2),
           seg.len = 3
    )
  }
}


dev.off()



