
# Plot interest rate distributions

names.of.regimes <- c("Low-growth regime",
                      "Medium-growth regime",
                      "High-growth regime")

Model <- Theta.2.Model(Full.Theta)

# Approximation method:
NB.values.s <- 300
index.min.S <- 50
index.min.S.4.price_sensitivity <- 50
index.max.S.4.price_sensitivity <- 295

NB.values.s <- 50
index.min.S <- 5
index.min.S.4.price_sensitivity <- 5
index.max.S.4.price_sensitivity <- 45




# For simulations, the maximum maturity will be the max entry of vector.of.H:
max.matur <- 40 # expressed at the model frequency

# ==========================================
# ==========================================
vector.of.maturities.4 <- c(8,40)
# ==========================================
# ==========================================

x.axis <- c(-.05,.10)

# =============================
# Solve the model:
vec.maturities <- 1:max.matur
res.prices <- compute.prices(Model,
                             vec.maturities,
                             nb.values.s = NB.values.s,
                             h.stock = 1,
                             curvature = CURVATURE,
                             grid.4.S = NaN)


# =============================
# Compute model-implied moments:
res.mom.pi.z <- compute.uncond.mom.pi.z(res.prices$Model.solved)


file <- "Figure_IR_distri"
pdf(file=paste(output.figures.folder,"/",file,".pdf",sep=""),
    pointsize=10, width=6, height=7)


par(mfrow=c(3,2))
par(plt=c(.1,.95,.23,.85))


for(j in 1:3){# Loop on regimes
  
  
  for(maturity in vector.of.maturities.4){
    adj <- res.prices$all.GDP.bond.yields[maturity,(index.min.S+1):NB.values.s] -
      res.prices$all.GDP.bond.yields[maturity,(index.min.S):(NB.values.s-1)]
    adj <- -1/adj
    plot(res.prices$all.GDP.bond.yields[maturity,(index.min.S+1):NB.values.s],
         pmax(res.mom.pi.z$E.z[(index.min.S+1):NB.values.s]*adj,0),
         type="l",lwd=1,las=1,
         xlab="Annualized yield to maturity",
         ylab="",
         yaxt="n",
         col="white",
         xlim=x.axis,
         ylim = c(0,20),
         main=paste(names.of.regimes[j],", maturity: ",toString(maturity/FREQ),ifelse(maturity<=4," year"," years"),sep="")
    )
    
    adj <- res.prices$all.rea.bond.yields[maturity,(index.min.S+1):NB.values.s] -
      res.prices$all.rea.bond.yields[maturity,(index.min.S):(NB.values.s-1)]
    adj <- -1/adj
    
    Ez <- res.mom.pi.z$E.z[((j-1)*NB.values.s + index.min.S+1):
                             (j*NB.values.s)]
    
    polygon(c(res.prices$all.rea.bond.yields[maturity,(index.min.S+1):NB.values.s],
              res.prices$all.rea.bond.yields[maturity,NB.values.s:(index.min.S+1)]),
            c(adj*Ez,0*Ez),
            col=rgb(0,0,0,alpha=0.2),border=NA)
    
    adj <- res.prices$all.GDP.bond.yields[maturity,(index.min.S+1):NB.values.s] -
      res.prices$all.GDP.bond.yields[maturity,(index.min.S):(NB.values.s-1)]
    adj <- -1/adj
    lines(res.prices$all.GDP.bond.yields[maturity,(index.min.S+1):NB.values.s],
          pmax(Ez*adj,0),
          col="black",
          lwd=1)
    
    avg.GDP.yield <- res.prices$all.GDP.bond.yields[maturity,] %*% res.mom.pi.z$E.z
    avg.rea.yield <- res.prices$all.rea.bond.yields[maturity,] %*% res.mom.pi.z$E.z
    
    abline(v=avg.GDP.yield,col="black",lwd=2,lty=3)
    abline(v=avg.rea.yield,col="dark grey",lwd=2,lty=3)
    abline(h=0,lwd=1)
    
    if((j==1)&(maturity==vector.of.maturities.4[1])){
      legend("topright", # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend 
             c("GDP-LB yields","ILB yields"),
             lty=c(1,NaN), # gives the legend appropriate symbols (lines)       
             lwd=c(1,1), # line width
             col=c("black",rgb(0,0,0,alpha=0.4)),
             bg="white",
             pch=c(NaN,22),
             pt.bg=c(NaN,rgb(0,0,0,alpha=0.2)),
             pt.cex = c(2),
             seg.len = 2
      )
    }
    
  }
  
}



dev.off()


