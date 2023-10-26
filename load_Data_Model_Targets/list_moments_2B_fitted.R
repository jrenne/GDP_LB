
# =========================================
# List of moments to be fitted
# =========================================

all.targets.US <- NULL
all.targets.ZE <- NULL
all.target.names <- NULL

# =========================================
# Means of macro growth rates
all.targets.US <- c(all.targets.US,
                    mean(dy.US),
                    mean(dc.US),
                    mean(pi.US))
all.targets.ZE <- c(all.targets.ZE,
                    mean(dy.ZE),
                    mean(dc.ZE),
                    mean(pi.ZE))

all.target.names <- c(all.target.names,
                      "Mean of GDP growth rate $\\Delta y_t$",
                      "Mean of consumption growth rate $\\Delta c_t$",
                      "Mean of inflation $\\pi_t$"
)

# Computation of short-term real rates:
EA_yields_q$ST.REAL.RATE <- EA_yields_q$swap_eonia_3m - 400*mean(pi.ZE,na.rm=TRUE)
US_yields_q$ST.REAL.RATE <- US_yields_q$DTB3 - 400*mean(pi.US,na.rm=TRUE)
# NEW VERSION BASED ON FORECASTS:
US_yields_q$ST.REAL.RATE <- US_yields_q$US_real_3m_SurveyBased



# =========================================
# Std. dev. of macro growth rates
all.targets.US <- c(all.targets.US,
                    sd(dy.US),
                    sd(dc.US),
                    sd(pi.US))
all.targets.ZE <- c(all.targets.ZE,
                    sd(dy.ZE),
                    sd(dc.ZE),
                    sd(pi.ZE))

all.target.names <- c(all.target.names,
                      "Std. dev. of GDP growth rate $\\Delta y_t$",
                      "Std. dev. of consumption growth rate $\\Delta c_t$",
                      "Std. dev. of inflation $\\pi_t$"
)


# =========================================
# Means, Std. dev. and auto-correlation of nom. interest rates

maturities.in.years.4.nominal.rates <- c(.25,5,20)
maturities.in.years.4.nominal.slope <- c(.25,20)

all.targets.US <- c(all.targets.US,
                    mean(US_yields_q$DTB3,na.rm=TRUE)/100,
                    mean(US_yields_q$SVENY10,na.rm=TRUE)/100,
                    mean(US_yields_q$SVENY30,na.rm=TRUE)/100)
all.targets.ZE <- c(all.targets.ZE,
                    mean(EA_yields_q$swap_eonia_3m ,na.rm=TRUE)/100,
                    mean(EA_yields_q$swap_eonia_10y ,na.rm=TRUE)/100,
                    mean(EA_yields_q$swap_eonia_30y,na.rm=TRUE)/100)

all.target.names <- c(all.target.names,
                      "Mean of short-term nom. rate",
                      "Mean of 10-year nom. rate",
                      "Mean of 30-year nom. rate")

all.targets.US <- c(all.targets.US,
                    sd(US_yields_q$DTB3,na.rm=TRUE)/100,
                    sd(US_yields_q$SVENY10,na.rm=TRUE)/100,
                    sd(US_yields_q$SVENY30,na.rm=TRUE)/100)
all.targets.ZE <- c(all.targets.ZE,
                    sd(EA_yields_q$swap_eonia_3m ,na.rm=TRUE)/100,
                    sd(EA_yields_q$swap_eonia_10y ,na.rm=TRUE)/100,
                    sd(EA_yields_q$swap_eonia_30y,na.rm=TRUE)/100)

all.target.names <- c(all.target.names,
                      "Std. dev. of short-term nom. rate",
                      "Std. dev. of 10-year nom. rate",
                      "Std. dev. of 30-year nom. rate")

TT <- length(US_yields_q$DTB3)
all.targets.US <- c(all.targets.US,
                    correl.rm.na(US_yields_q$DTB3[2:TT],US_yields_q$DTB3[1:(TT-1)]),
                    correl.rm.na(US_yields_q$SVENY10[2:TT],US_yields_q$SVENY10[1:(TT-1)]),
                    correl.rm.na(US_yields_q$SVENY30[2:TT],US_yields_q$SVENY30[1:(TT-1)]))
TT <- length(EA_yields_q$swap_eonia_3m)
all.targets.ZE <- c(all.targets.ZE,
                    correl.rm.na(EA_yields_q$swap_eonia_3m[2:TT] ,EA_yields_q$swap_eonia_3m[1:(TT-1)]),
                    correl.rm.na(EA_yields_q$swap_eonia_10y[2:TT] ,EA_yields_q$swap_eonia_10y[1:(TT-1)]),
                    correl.rm.na(EA_yields_q$swap_eonia_30y[2:TT],EA_yields_q$swap_eonia_30y[1:(TT-1)]))

all.target.names <- c(all.target.names,
                      "Auto-correl. of short-term nom. rate",
                      "Auto-correl. of 10-year nom. rate",
                      "Auto-correl. of 30-year nom. rate")

all.targets.US <- c(all.targets.US,
                    mean(US_yields_q$SVENY30 - US_yields_q$DTB3,na.rm=TRUE)/100,
                    sd(US_yields_q$SVENY30 - US_yields_q$DTB3,na.rm=TRUE)/100)
all.targets.ZE <- c(all.targets.ZE,
                    mean(EA_yields_q$swap_eonia_30y - EA_yields_q$swap_eonia_3m,na.rm=TRUE)/100,
                    sd(EA_yields_q$swap_eonia_30y - EA_yields_q$swap_eonia_3m,na.rm=TRUE)/100)

all.target.names <- c(all.target.names,
                      "Mean of slope of the nom. yd curve (3m-30yrs)",
                      "Std. dev. of slope of the nom. yd curve (3m-30yrs)")




# =========================================
# Means, Std. dev. and auto-correlation of real interest rates

# ==================
# ==================
# ==================
# ==================
maturities.in.years.4.real.rates <- c(2,5,20)
maturities.in.years.4.real.slope <- c(2,20)
# ==================
# ==================
# ==================
# ==================
#maturities.in.years.4.real.rates <- c(1,5,10)
#maturities.in.years.4.real.slope <- c(1,10)

all.targets.US <- c(all.targets.US,
                    mean(US_yields_q$ST.REAL.RATE,na.rm=TRUE)/100,
                    mean(US_yields_q$TIPSY02,na.rm=TRUE)/100,
                    mean(US_yields_q$TIPSY10,na.rm=TRUE)/100)
all.targets.ZE <- c(all.targets.ZE,
                    mean(EA_yields_q$ST.REAL.RATE,na.rm=TRUE)/100,
                    mean(EA_yields_q$ILS.2,na.rm=TRUE)/100,
                    mean(EA_yields_q$ILS.10,na.rm=TRUE)/100)

all.target.names <- c(all.target.names,
                      "Mean of short-term real rate",
                      "Mean of 2-year real rate",
                      "Mean of 10-year real rate")

all.targets.US <- c(all.targets.US,
                    sd(US_yields_q$ST.REAL.RATE,na.rm=TRUE)/100,
                    sd(US_yields_q$TIPSY02,na.rm=TRUE)/100,
                    sd(US_yields_q$TIPSY10,na.rm=TRUE)/100)
all.targets.ZE <- c(all.targets.ZE,
                    sd(EA_yields_q$ST.REAL.RATE,na.rm=TRUE)/100,
                    sd(EA_yields_q$ILS.2,na.rm=TRUE)/100,
                    sd(EA_yields_q$ILS.10,na.rm=TRUE)/100)

all.target.names <- c(all.target.names,
                      "Std. dev. of short-term real rate",
                      "Std. dev. of 2-year real rate",
                      "Std. dev. of 10-year real rate")

TT <- length(US_yields_q$DTB3)
all.targets.US <- c(all.targets.US,
                    correl.rm.na(US_yields_q$ST.REAL.RATE[2:TT],US_yields_q$ST.REAL.RATE[1:(TT-1)]),
                    correl.rm.na(US_yields_q$TIPSY02[2:TT],US_yields_q$TIPSY02[1:(TT-1)]),
                    correl.rm.na(US_yields_q$TIPSY10[2:TT],US_yields_q$TIPSY10[1:(TT-1)]))
TT <- length(EA_yields_q$swap_eonia_3m)
all.targets.ZE <- c(all.targets.ZE,
                    correl.rm.na(EA_yields_q$ST.REAL.RATE[2:TT] ,EA_yields_q$ST.REAL.RATE[1:(TT-1)]),
                    correl.rm.na(EA_yields_q$ILS.2[2:TT] ,EA_yields_q$ILS.2[1:(TT-1)]),
                    correl.rm.na(EA_yields_q$ILS.10[2:TT],EA_yields_q$ILS.10[1:(TT-1)]))

all.target.names <- c(all.target.names,
                      "Auto-correl. of short-term real rate",
                      "Auto-correl. of 2-year real rate",
                      "Auto-correl. of 10-year real rate")

all.targets.US <- c(all.targets.US,
                    mean(US_yields_q$TIPSY10 - US_yields_q$ST.REAL.RATE,na.rm=TRUE)/100,
                    sd(US_yields_q$TIPSY10 - US_yields_q$ST.REAL.RATE,na.rm=TRUE)/100)
all.targets.ZE <- c(all.targets.ZE,
                    mean(EA_yields_q$ILS.10 - EA_yields_q$ST.REAL.RATE,na.rm=TRUE)/100,
                    sd(EA_yields_q$ILS.10 - EA_yields_q$ST.REAL.RATE,na.rm=TRUE)/100)

all.target.names <- c(all.target.names,
                      "Mean of slope of the real yd curve (3m-10yrs)",
                      "Std. dev. of slope of the real yd curve (3m-10yrs)")


# =========================================
# Means and variances of condi. var. of interest rates

maturities.in.years.4.condiVar.nominal.rates <- c(.25,30)
maturities.in.years.4.condiVar.real.rates <- c(10)

all.targets.US <- c(all.targets.US,
                    mean(US_yields_q$DTB3.condiVar/100^2,na.rm=TRUE),
                    mean(US_yields_q$SVENY30.condiVar/100^2,na.rm=TRUE),
                    #sd(  US_yields_q$DTB3.condiVar/100^2,na.rm=TRUE),
                    #sd(  US_yields_q$SVENY10.condiVar/100^2,na.rm=TRUE),
                    mean(US_yields_q$TIPSY10.condiVar/100^2,na.rm=TRUE)
                    #sd(  US_yields_q$TIPSY10.condiVar/100^2,na.rm=TRUE)
)
all.targets.ZE <- c(all.targets.ZE,
                    mean(EA_yields_q$swap_eonia_3m.condiVar/100^2,na.rm=TRUE),
                    mean(EA_yields_q$swap_eonia_30y.condiVar/100^2,na.rm=TRUE),
                    #sd(EA_yields_q$swap_eonia_3m.condiVar/100^2,na.rm=TRUE),
                    #sd(EA_yields_q$swap_eonia_10y.condiVar/100^2,na.rm=TRUE),
                    mean(EA_yields_q$ILS.10.condiVar/100^2,na.rm=TRUE)
                    #sd(EA_yields_q$ILS.10.condiVar/100^2,na.rm=TRUE)
)


all.target.names <- c(all.target.names,
                      "Mean of condi. var. of the short-term nom. rate",
                      "Mean of condi. var. of the 30-year nom. rate",
                      #"Std. dev. of condi. var. of the short-term nom. rate",
                      #"Std. dev. of condi. var. of the ten-year nom. rate",
                      "Mean of condi. var. of the 10-year real rate"
                      #"Std. dev. of condi. var. of the ten-year real rate")
)

# =========================================
# Stock returns

all.targets.US <- c(all.targets.US,
                    avg.annualized.return.real -
                      (mean(US_yields_q$DTB3,na.rm=TRUE)/100 - mean(pi.US)*FREQ),
                    std.annualized.return.real)
all.targets.ZE <- c(all.targets.ZE,
                    0.05,
                    0.1)

all.target.names <- c(all.target.names,
                      "Average expected excess return (annualized)",
                      "Average cond. volat. of stock return (annualized)")

all.targets.US <- c(all.targets.US,
                    mean(Data_Shiller$P/Data_Shiller$D),
                    sd(Data_Shiller$P/Data_Shiller$D))
all.targets.ZE <- c(all.targets.ZE,
                    30,10)

all.target.names <- c(all.target.names,
                      "Average P/D",
                      "Std. dev. of P/D")



# =========================================
# Definition of weights used in the computation of the loss function:

all.weights.US <- rep(10^100,length(all.targets.US))

all.weights.US[c(2,3)] <- abs(all.targets.US)[c(2,3)]/40 # means of pi and c
all.weights.US[c(5,6)] <- abs(all.targets.US)[c(5,6)]/5 # st dev of pi and c

all.weights.US[7:9] <- abs(all.targets.US)[7:9]/4 # means of nominal rates
all.weights.US[10:12] <- abs(all.targets.US)[10:12] # std dev of nominal rates
all.weights.US[13:15] <- (1-all.targets.US[13:15]) # auto-cor of nominal rates

all.weights.US[18:20] <- abs(all.targets.US)[18:20]/4 # mean of real rates
all.weights.US[21:23] <- abs(all.targets.US)[21:23] # std dev of real rates
all.weights.US[24:26] <- (1-all.targets.US[24:26]) # auto-cor of real rates

# Alternative (not linked to level, which raises problems when close to 0)
all.weights.US[c(2,3)] <- .001 # means of pi and c
all.weights.US[c(4,5,6)] <- .005 # st dev of pi and c

all.weights.US[7:9] <- c(.003,.002,.001) # means of nominal rates
all.weights.US[10:12] <- .001 # std dev of nominal rates
all.weights.US[13:15] <- .1 # auto-cor of nominal rates

all.weights.US[18:20] <- c(.003,.002,.001) # mean of real rates
all.weights.US[21:23] <- .002 # std dev of real rates
all.weights.US[24:26] <- .1 # auto-cor of real rates

#all.weights.US[c(16,27)] <- .002 # Avg of slope of the nom and real yd curve
all.weights.US[c(17,28)] <- .003 # Std. dev. of slope of the nom and real yd curve
all.weights.US[c(29,30,31)] <- 2e-05 # Mean of condi. var. of rates

all.weights.US[32]  <- abs(all.targets.US)[32] / 5
all.weights.US[33]  <- abs(all.targets.US)[33] / 2
all.weights.US[34]  <- abs(all.targets.US)[34] / 40 # Average P/D
all.weights.US[35]  <- abs(all.targets.US)[35] / 20 # Std. dev. of P/D

if(indic.include.nominal == 0){
  all.weights.US[c(7:15)] <- 10^40
}

all.weights.ZE <- 100000*abs(all.targets.ZE)

# all.weights.ZE[c(2,5)] <- abs(all.targets.ZE)[c(2,5)]
# #all.weights.ZE[1:3] <- abs(all.targets.ZE)[1:3]
# 
# #all.weights.ZE[18:23] <- abs(all.targets.ZE)[18:23]
# all.weights.ZE[c(19,22)] <- abs(all.targets.ZE)[c(19,22)]
# #all.weights.ZE[24:26] <- (1-all.targets.ZE[24:26])*4
# all.weights.ZE[25] <- (1-all.targets.ZE[25]) * 10
# 
# all.weights.ZE[c(27,28)] <- abs(all.targets.ZE)[c(27,28)] # slope of real yield curve
# 
# all.weights.ZE[33:34]  <- abs(all.targets.ZE)[33:34] * 5
# 
# all.weights.ZE[35:36]  <- abs(all.targets.ZE)[35:36] / 2






# all.weights.US <- 1000*abs(all.targets.US)
# 
# all.weights.US[1:3] <- abs(all.targets.US)[1:3]
# 
# all.weights.US[18:23] <- abs(all.targets.US)[18:23]
# all.weights.US[24:26] <- (1-all.targets.US[24:26])*4
# 
# all.weights.US[33:34]  <- abs(all.targets.US)[33:34]/2




# =========================================
# Specifiy multiplicative factors (for fit table)

all.multiplicative.factors <- c(
  2,2,2,2,2,2,
  2,2,2,2,2,2,
  0,0,0,
  2,2,
  2,2,2,2,2,2,
  0,0,0,
  2,2,
  5,5,5,
  2,2,
  0,0
)




