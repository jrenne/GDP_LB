


nb.dec <- 2 # number of decimal numbers
format.nb  <- paste("%.",nb.dec,"f",sep="")
format.nb0 <- paste("%.",0,"f",sep="")
format.nb1 <- paste("%.",1,"f",sep="")
format.nb5 <- paste("%.",5,"f",sep="")


# Approximation method:
NB.values.s <- 100

max.matur <- 40 # expressed at the model frequency
vec.maturities <- 1:max.matur

# Build estimated model:
Model.est <- Theta.2.Model(Full.Theta)

vec.maturities.4.table <- 4*c(1,2,5,10)

# ==============================================================================
# Solve the model and get prices' specifications:
res.prices <- compute.prices(Model.est,
                             vec.maturities.4.table,
                             nb.values.s = NB.values.s,
                             h.stock = 1,
                             curvature = CURVATURE,
                             grid.4.S = NaN)

# Run filtering approach:
res.estim <-  estimate.latent.factors(res.prices,
                                      dc.US,dy.US,pi.US,
                                      gdp.forecast.4Q,infl.forecast.4Q,
                                      gdp.forecast.8Q,infl.forecast.8Q,
                                      US_yields_q)

# Save esitmated regime and inflation process:
z.hat  <- res.estim$z.hat
pi.hat <- res.estim$pi.hat

indic.of.yds.in.res.prices <- 1:length(vec.maturities.4.table)

# Fit of real rates:
fitted02yr_real <- res.prices$all.rea.bond.yields[2,z.hat]
fitted05yr_real <- res.prices$all.rea.bond.yields[3,z.hat]
fitted10yr_real <- res.prices$all.rea.bond.yields[4,z.hat]
# Fit of nominal rates:
fitted01yr_nom <- res.prices$all.nom.bond.F.yields[1,z.hat] + 
  res.prices$all.nom.bond.b.yields[1] * pi.hat
fitted02yr_nom <- res.prices$all.nom.bond.F.yields[2,z.hat] + 
  res.prices$all.nom.bond.b.yields[2] * pi.hat
fitted05yr_nom <- res.prices$all.nom.bond.F.yields[3,z.hat] + 
  res.prices$all.nom.bond.b.yields[3] * pi.hat
fitted10yr_nom <- res.prices$all.nom.bond.F.yields[4,z.hat] + 
  res.prices$all.nom.bond.b.yields[4] * pi.hat

# Observed yields:
observed02yr_real <- US_yields_q$TIPSY02/100
observed05yr_real <- US_yields_q$TIPSY05/100
observed10yr_real <- US_yields_q$TIPSY10/100
observed01yr_nom  <- US_yields_q$SVENY01/100
observed02yr_nom  <- US_yields_q$SVENY02/100
observed05yr_nom  <- US_yields_q$SVENY05/100
observed10yr_nom  <- US_yields_q$SVENY10/100

observed <- cbind(observed02yr_real,
                  observed05yr_real,
                  observed10yr_real,
                  observed01yr_nom,
                  observed02yr_nom,
                  observed05yr_nom,
                  observed10yr_nom)

fitted <- cbind(fitted02yr_real,
                fitted05yr_real,
                fitted10yr_real,
                fitted01yr_nom,
                fitted02yr_nom,
                fitted05yr_nom,
                fitted10yr_nom)

stdev.observed <- apply(observed,2,function(x){sd(x,na.rm = TRUE)})
stdev.fitted   <- apply(fitted,2,function(x){sd(x,na.rm = TRUE)})
stdev.error    <- apply(fitted - observed,2,function(x){sd(x,na.rm = TRUE)})

stdev.error^2/stdev.fitted^2


names.of.rows <- c("2-year real rate",
                   "5-year real rate",
                   "10-year real rate",
                   "1-year nominal rate",
                   "2-year nominal rate",
                   "5-year nominal rate",
                   "10-year nominal rate")

latex.table <- NULL

for(i in 1:length(names.of.rows)){
  this.line <- names.of.rows[i]
  this.line <- paste(this.line,"&",
                     sprintf(format.nb5,mean(observed[,i],na.rm=TRUE)),"&",
                     sprintf(format.nb5,mean(fitted[!is.na(observed[,i]),i],na.rm=TRUE)),"&",
                     sprintf(format.nb5,sd(observed[,i],na.rm=TRUE)),"&",
                     sprintf(format.nb5,sd(fitted[!is.na(observed[,i]),i],na.rm=TRUE)),"&",
                     sprintf(format.nb5,stdev.error[i]),"&",
                     sprintf(format.nb0,100*stdev.error[i]^2/stdev.fitted[i]^2),"\\%",
                     "\\\\",
                     sep="")
  latex.table <- rbind(latex.table,this.line)
}

latex.file <- paste(output.tables.folder,"/tableFITyds.txt",sep="")
write(latex.table, file = latex.file)

