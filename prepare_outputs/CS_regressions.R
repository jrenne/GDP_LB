# ==============================================================================
# Campbell - Shiller regressions
# ==============================================================================


# ==============================================================================
# Use formula of population regression coefficient:

compute.slope <- function(a_Y,b_Y,c_Y,h,Var.pi.z,PHI){
  slope <- (t(a_Y) %*%  Var.pi.z %*% t(PHI%^%h) - t(b_Y) %*% Var.pi.z) %*% c_Y /
    (t(c_Y) %*% Var.pi.z %*% c_Y)
  return(slope)
}

# Approximation method:
NB.values.s <- 100

max.matur <- 40 # expressed at the model frequency
vec.maturities <- 1:max.matur

# Build estimated model:
Model.est <- Theta.2.Model(Full.Theta)

# ==============================================================================
# Solve the model and get prices' specifications:
res.prices <- compute.prices(Model.est,
                             1:max.matur,
                             nb.values.s = NB.values.s,
                             h.stock = 1,
                             curvature = CURVATURE,
                             grid.4.S = NaN)

res.mom.pi.z <- compute.uncond.mom.pi.z(res.prices$Model.solved)
Var.pi.z <- res.mom.pi.z$Var.pi.z
PHI <- res.mom.pi.z$PHI

all.h <- c(1,2,4)
all.slopes <- matrix(NaN,max.matur,length(all.h))
count.h <- 0
for(h in all.h){
  count.h <- count.h + 1
  all.n.in.q <- (h+1):max.matur
  for(n.in.q in all.n.in.q){
    a_Y <- matrix(c(res.prices$all.nom.bond.b.yields[n.in.q-h],
                    res.prices$all.nom.bond.F.yields[n.in.q-h,]),
                  ncol=1)
    b_Y <- matrix(c(res.prices$all.nom.bond.b.yields[n.in.q],
                    res.prices$all.nom.bond.F.yields[n.in.q,]),
                  ncol=1)
    c_Y <- h/(n.in.q-h) * 
      matrix(c(res.prices$all.nom.bond.b.yields[n.in.q]-res.prices$all.nom.bond.b.yields[h],
               res.prices$all.nom.bond.F.yields[n.in.q,]-res.prices$all.nom.bond.F.yields[h,]),
             ncol=1)
    all.slopes[n.in.q,count.h] <- compute.slope(a_Y,b_Y,c_Y,h,Var.pi.z,PHI)
  }
}


# Compute coefficients in the data:
first.date <- as.Date("1985-01-01")
last.date  <- as.Date("2018-03-01")
T <- length(US_yields_q$SVENY01)

Index_1Yr <- which(names(US_yields_q)=="SVENY01")

all.coef <- matrix(NaN,10,length(all.h))
all.stdv <- matrix(NaN,10,length(all.h))

count.h <- 0
for(h in all.h){
  count.h <- count.h + 1
  if(h==1){short.term <- 1.00*US_yields_q$DTB3 + 0.00*US_yields_q$SVENY01}
  if(h==2){short.term <- 0.66*US_yields_q$DTB3 + 0.33*US_yields_q$SVENY01}
  if(h==3){short.term <- 0.33*US_yields_q$DTB3 + 0.66*US_yields_q$SVENY01}
  if(h==4){short.term <- 0.00*US_yields_q$DTB3 + 1.00*US_yields_q$SVENY01}
  for(n in 1:10){
    y <- US_yields_q[,Index_1Yr-1+n]
    dep.var <- y[(1+h):T] - y[1:(T-h)]
    indep.var <- h/(4*n - h) * (y - short.term)[1:(T-h)]
    
    if(h!=4*n){
      eq <- lm(dep.var ~ indep.var)
      all.coef[n,count.h] <- eq$coef[2]
      all.stdv[n,count.h] <- sqrt(NeweyWest(eq)[2,2])
    }
  }
}




file <- "Figure_CS_regressions"
pdf(file=paste(output.figures.folder,"/",file,".pdf",sep=""),
    pointsize=10, width=5, height=3.5)
par(mfrow=c(1,1))
par(plt=c(.15,.95,.2,.95))

plot(1:max.matur/4,all.slopes[,1],type="l",lwd=2,las=1,
     xlab="maturity (in years)",ylab="regression coefficient",ylim=c(0,1),col="white")
abline(h=1,col="grey")     
lines(1:max.matur/4,all.slopes[,1],lwd=2,lty=1)
lines(1:max.matur/4,all.slopes[,2],lwd=2,lty=2)
lines(1:max.matur/4,all.slopes[,3],lwd=2,lty=3)

#lines(1:10,all.coef[,1],lwd=2,lty=1,col="grey")
#lines(1:10,all.coef[,2],lwd=2,lty=2,col="grey")
#lines(1:10,all.coef[,3],lwd=2,lty=3,col="grey")

legend("bottomright", # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend 
       c("h = 1","h = 2","h = 4"),
       lty=c(1,2,3), # gives the legend appropriate symbols (lines)       
       lwd=c(2), # line width
       col=c("black"), # gives the legend lines the correct color and width
       #pt.bg=c(NaN,"blue",NaN),
       #text.width = 2,
       #cex=1.0,# size of the text
       #pch = c(NaN,19,NaN),#symbols,
       #pt.cex = c(NaN,1.5,NaN),
       bg="white",
       seg.len = 3
)

dev.off()


file <- "Figure_CS_regressions_comparisonData"
pdf(file=paste(output.figures.folder,"/",file,".pdf",sep=""),
    pointsize=10, width=8, height=4)
par(mfrow=c(1,3))
par(plt=c(.2,.95,.15,.85))

for(i in 1:length(all.h)){
  plot(1:max.matur/4,all.slopes[,1],type="l",lwd=2,las=1,
       xlab="maturity (in years)",ylab="regression coefficient",ylim=c(-5,3),col="white",
       main=paste("Holding period of ",all.h[i],ifelse(all.h[i]==1," quarter"," quarters"),sep=""))
  abline(h=1,col="grey")     
  lines(1:max.matur/4,all.slopes[,i],lwd=2,lty=1)
  lines(1:10,all.coef[,i],lwd=2,lty=1,col="dark grey")
  lines(1:10,all.coef[,i]-2*all.stdv[,i],lwd=2,lty=2,col="dark grey")
  lines(1:10,all.coef[,i]+2*all.stdv[,i],lwd=2,lty=2,col="dark grey")
}

legend("bottomright", # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend 
       c("Data","Model"),
       lty=c(1), # gives the legend appropriate symbols (lines)       
       lwd=c(2), # line width
       col=c("dark grey","black"), # gives the legend lines the correct color and width
       #pt.bg=c(NaN,"blue",NaN),
       #text.width = 2,
       #cex=1.0,# size of the text
       #pch = c(NaN,19,NaN),#symbols,
       #pt.cex = c(NaN,1.5,NaN),
       bg="white",
       seg.len = 3
)

dev.off()



# ==============================================================================
# Stock returns

max.matur.XS.in.yrs <- 4

res.all.moments <- compute.moments(res.prices$Model.solved,
                                   1:(4*max.matur.XS.in.yrs),
                                   nb.values.s = NB.values.s,
                                   h.stock = H.stock,
                                   curvature = CURVATURE,
                                   grid.4.S = NaN,
                                   indic.slope.nom.curve,
                                   indic.slope.rea.curve,
                                   indic.condVar.nom.rate,
                                   indic.condVar.rea.rate,
                                   indic.compute.slopes = TRUE)


Data_Shiller <- read.csv("data/Shiller_Data_4R_new.csv")
Data_Shiller$Date <- as.Date(Data_Shiller$Date,"%d.%m.%Y")
T <- dim(Data_Shiller)[1]

Data_Shiller$P.real <- Data_Shiller$P/Data_Shiller$CPI
Data_Shiller$D.real <- Data_Shiller$D/Data_Shiller$CPI

Data_Shiller$return <- NaN
Data_Shiller$return[2:T] <- log((Data_Shiller$P.real + Data_Shiller$D.real/12)[2:T]/
                                  Data_Shiller$P.real[1:(T-1)])

Data_Shiller$xs_return <- Data_Shiller$return - Data_Shiller$rf/1200


first.date <- as.Date("1979-12-01")
last.date  <- as.Date("2001-03-01")

first.date <- as.Date("2001-04-01")
last.date  <- as.Date("2018-03-01")

first.date <- as.Date("1985-01-01")
last.date  <- as.Date("2018-03-01")

indic.first.date.in.Shiller.database <- which(Data_Shiller$Date==first.date)
indic.last.date.in.Shiller.database  <- which(Data_Shiller$Date==last.date)

all.coef <- NULL
all.stdv <- NULL
all.h.in.months <- 1:(12*max.matur.XS.in.yrs)
for(h in all.h.in.months){
  Data_Shiller$xs_return_h <- Data_Shiller$xs_return
  if(h>1){
    for(i in 2:h){
      Data_Shiller$xs_return_h[i:T] <- Data_Shiller$xs_return_h[i:T] + 
        Data_Shiller$xs_return[1:(T-i+1)]
    }
  }
  Data_Shiller$xs_return_h <- 12/h*Data_Shiller$xs_return_h
  
  y <- Data_Shiller$xs_return_h[indic.first.date.in.Shiller.database:
                                  indic.last.date.in.Shiller.database]
  x <- log(Data_Shiller$P.real/Data_Shiller$D.real)[(indic.first.date.in.Shiller.database-h):
                                                   (indic.last.date.in.Shiller.database-h)]
  eq <- lm(y ~ x)
  all.coef <- rbind(all.coef,
                    c(eq$coef[2],sqrt(NeweyWest(eq)[2,2])))
}


file <- "Figure_XSreturns_regressions"
pdf(file=paste(output.figures.folder,"/",file,".pdf",sep=""),
    pointsize=10, width=5, height=3.5)
par(mfrow=c(1,1))
par(plt=c(.15,.95,.2,.95))

plot(1:(4*max.matur.XS.in.yrs),res.all.moments$all.slope.xs.logPD,
     type="l",ylim=c(-.7,.3),lwd=2,las=1,xlab="holding period (in quarters)",ylab="slope coefficient")
lines(all.h.in.months/3,all.coef[,1],col="dark grey",lwd=2)
lines(all.h.in.months/3,all.coef[,1]+2*all.coef[,2],col="dark grey",lty=2,lwd=2)
lines(all.h.in.months/3,all.coef[,1]-2*all.coef[,2],col="dark grey",lty=2,lwd=2)
abline(h=0,col="grey")

legend("bottomleft", # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend 
       c("Data","Model (population coef.)"),
       lty=c(1,1), # gives the legend appropriate symbols (lines)       
       lwd=c(2,2), # line width
       col=c("dark grey","black"), # gives the legend lines the correct color and width
       bg="white",
       seg.len = 3
)

dev.off()
