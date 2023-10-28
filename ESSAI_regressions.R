
Data_Shiller <- read.csv("data/Shiller_Data_4R_new.csv")
Data_Shiller$Date <- as.Date(Data_Shiller$Date,"%d.%m.%Y")

first.date <- as.Date("1979-12-01")
last.date  <- as.Date("2001-03-01")

first.date <- as.Date("2001-04-01")
last.date  <- as.Date("2018-03-01")

indic.first.date.in.Shiller.database <- which(Data_Shiller$Date==first.date)
indic.last.date.in.Shiller.database  <- which(Data_Shiller$Date==last.date)

Data_Shiller$P.real <- Data_Shiller$P/Data_Shiller$CPI
Data_Shiller$D.real <- Data_Shiller$D/Data_Shiller$CPI

h <- 12
stock.returns.real <- log((Data_Shiller$P.real + Data_Shiller$D.real)[indic.first.date.in.Shiller.database:indic.last.date.in.Shiller.database]/
                            Data_Shiller$P.real[(indic.first.date.in.Shiller.database-h):(indic.last.date.in.Shiller.database-h)])
xs.stock.returns <- 12/h*stock.returns.real - 
  Data_Shiller$rf[(indic.first.date.in.Shiller.database-h):(indic.last.date.in.Shiller.database-h)]/100
lagged.logPD <- log(c(rep(NaN,h),(Data_Shiller$P.real/Data_Shiller$D.real)[1:(length(stock.returns.real)-h)]))
eq <- lm(xs.stock.returns~lagged.logPD)
summary(eq)


# Campbell-Shiller regressions:
T <- length(US_yields_q$SVENY01)
dep.var   <- US_yields_q$SVENY05[2:T] - US_yields_q$SVENY05[1:(T-1)]
indep.var <- 1/(4*5-1) * (US_yields_q$SVENY05[1:(T-1)] - US_yields_q$DTB3[1:(T-1)])
summary(lm(dep.var~indep.var))
dep.var   <- US_yields_q$SVENY10[2:T] - US_yields_q$SVENY10[1:(T-1)]
indep.var <- 1/(4*10-1) * (US_yields_q$SVENY10[1:(T-1)] - US_yields_q$DTB3[1:(T-1)])
summary(lm(dep.var~indep.var))

stop()

res.prices <- compute.prices(Model.est,
                             vec.maturities=1:40,
                             nb.values.s = NB.values.s,
                             h.stock = 1,
                             curvature = CURVATURE,
                             grid.4.S = NaN)

T <- 2000
sim <- simul.model.solved(res.prices,T)
n.in.q <- 20
dep.var   <- sim$nom.yields[n.in.q-1,2:T] - sim$nom.yields[n.in.q,1:(T-1)]
indep.var <- 1/(n.in.q-1) * (sim$nom.yields[n.in.q,1:(T-1)] - sim$nom.yields[1,1:(T-1)])
summary(lm(dep.var~indep.var))

dep.var   <- 5*sim$nom.yields[n.in.q,1:(T-4)] - 4*sim$nom.yields[n.in.q-4,5:T] - 
  sim$nom.yields[4,1:(T-4)]
indep.var <- sim$nom.yields[n.in.q,1:(T-4)] - sim$nom.yields[4,1:(T-4)]
summary(lm(dep.var~indep.var))

dep.var   <- sim$nom.yields[n.in.q-4,5:T] - sim$nom.yields[n.in.q,1:(T-4)]
indep.var <- 1/(n.in.q-4) * (sim$nom.yields[n.in.q,1:(T-4)] - sim$nom.yields[4,1:(T-4)])
summary(lm(dep.var~indep.var))

# dep.var   <- sim$rea.yields[n.in.q-1,2:T] - sim$rea.yields[n.in.q,1:(T-1)]
# indep.var <- 1/(n.in.q-1) * (sim$rea.yields[n.in.q,1:(T-1)] - sim$rea.yields[1,1:(T-1)])
# summary(lm(dep.var~indep.var))





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
                             1:40,
                             nb.values.s = NB.values.s,
                             h.stock = 1,
                             curvature = CURVATURE,
                             grid.4.S = NaN)

res.mom.pi.z <- compute.uncond.mom.pi.z(res.prices$Model.solved)
Var.pi.z <- res.mom.pi.z$Var.pi.z
PHI <- res.mom.pi.z$PHI

# # Fit of nominal rates:
# fitted01yr_nom <- res.prices$all.nom.bond.F.yields[1,z.hat] + 
#   res.prices$all.nom.bond.b.yields[1] * pi.hat
# fitted02yr_nom <- res.prices$all.nom.bond.F.yields[2,z.hat] + 
#   res.prices$all.nom.bond.b.yields[2] * pi.hat
# fitted05yr_nom <- res.prices$all.nom.bond.F.yields[3,z.hat] + 
#   res.prices$all.nom.bond.b.yields[3] * pi.hat
# fitted10yr_nom <- res.prices$all.nom.bond.F.yields[4,z.hat] + 
#   res.prices$all.nom.bond.b.yields[4] * pi.hat

# Regression of 

all.n.in.q <- (h+1):40
all.slopes <- NULL
h <- 4
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
  all.slopes <- c(all.slopes,
                  compute.slope(a_Y,b_Y,c_Y,h,Var.pi.z,PHI))
}
plot(all.n.in.q/4,all.slopes)
