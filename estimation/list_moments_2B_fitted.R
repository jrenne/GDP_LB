
# =========================================
# List of moments to be fitted
# =========================================

all.targets.US <- NULL
all.target.names <- NULL

# =========================================
# Means of macro growth rates

all.targets.US <- c(all.targets.US,
                    mean(dy.US),
                    mean(dc.US),
                    mean(pi.US))

all.target.names <- c(all.target.names,
                      "Mean of GDP growth rate $\\Delta y_t$",
                      "Mean of consumption growth rate $\\Delta c_t$",
                      "Mean of inflation $\\pi_t$"
)


# Computation of short-term real rates, using inflation surveys:
US_yields_q$ST.REAL.RATE <- US_yields_q$US_real_3m_SurveyBased



# =========================================
# Std. dev. of macro growth rates

all.targets.US <- c(all.targets.US,
                    sd(dy.US),
                    sd(dc.US),
                    sd(pi.US))

all.target.names <- c(all.target.names,
                      "Std. dev. of GDP growth rate $\\Delta y_t$",
                      "Std. dev. of consumption growth rate $\\Delta c_t$",
                      "Std. dev. of inflation $\\pi_t$"
)


# =========================================
# Skewness and kurtosis of consumption growth:

skewness.dc.US <- mean((dc.US - mean(dc.US))^3)/sd(dc.US)^3
kurtosis.dc.US <- mean((dc.US - mean(dc.US))^4)/sd(dc.US)^4

all.targets.US <- c(all.targets.US,
                    skewness.dc.US,
                    kurtosis.dc.US)

all.target.names <- c(all.target.names,
                      "Skewness of consumption growth rate $\\Delta c_t$",
                      "Kurtosis of consumption growth rate $\\Delta c_t$"
)


# =========================================
# Means, Std. dev. and auto-correlation of nom. interest rates

all.targets.US <- c(all.targets.US,
                    mean(US_yields_q$DTB3,na.rm=TRUE)/100,
                    mean(US_yields_q$SVENY10,na.rm=TRUE)/100,
                    mean(US_yields_q$SVENY30,na.rm=TRUE)/100)

all.target.names <- c(all.target.names,
                      "Mean of short-term nom. rate",
                      "Mean of 10-year nom. rate",
                      "Mean of 30-year nom. rate")

all.targets.US <- c(all.targets.US,
                    sd(US_yields_q$DTB3,na.rm=TRUE)/100,
                    sd(US_yields_q$SVENY10,na.rm=TRUE)/100,
                    sd(US_yields_q$SVENY30,na.rm=TRUE)/100)

all.target.names <- c(all.target.names,
                      "Std. dev. of short-term nom. rate",
                      "Std. dev. of 10-year nom. rate",
                      "Std. dev. of 30-year nom. rate")

TT <- length(US_yields_q$DTB3)
all.targets.US <- c(all.targets.US,
                    correl.rm.na(US_yields_q$DTB3[2:TT],US_yields_q$DTB3[1:(TT-1)]),
                    correl.rm.na(US_yields_q$SVENY10[2:TT],US_yields_q$SVENY10[1:(TT-1)]),
                    correl.rm.na(US_yields_q$SVENY30[2:TT],US_yields_q$SVENY30[1:(TT-1)]))

all.target.names <- c(all.target.names,
                      "Auto-correl. of short-term nom. rate",
                      "Auto-correl. of 10-year nom. rate",
                      "Auto-correl. of 30-year nom. rate")

all.targets.US <- c(all.targets.US,
                    mean(US_yields_q$SVENY30 - US_yields_q$DTB3,na.rm=TRUE)/100,
                    sd(US_yields_q$SVENY30 - US_yields_q$DTB3,na.rm=TRUE)/100)

all.target.names <- c(all.target.names,
                      "Mean of slope of the nom. yd curve (3m-30yrs)",
                      "Std. dev. of slope of the nom. yd curve (3m-30yrs)")




# =========================================
# Means, Std. dev. and auto-correlation of real interest rates

all.targets.US <- c(all.targets.US,
                    mean(US_yields_q$ST.REAL.RATE,na.rm=TRUE)/100,
                    mean(US_yields_q$TIPSY02,na.rm=TRUE)/100,
                    mean(US_yields_q$TIPSY10,na.rm=TRUE)/100)

all.target.names <- c(all.target.names,
                      "Mean of short-term real rate",
                      "Mean of 2-year real rate",
                      "Mean of 10-year real rate")

all.targets.US <- c(all.targets.US,
                    sd(US_yields_q$ST.REAL.RATE,na.rm=TRUE)/100,
                    sd(US_yields_q$TIPSY02,na.rm=TRUE)/100,
                    sd(US_yields_q$TIPSY10,na.rm=TRUE)/100)

all.target.names <- c(all.target.names,
                      "Std. dev. of short-term real rate",
                      "Std. dev. of 2-year real rate",
                      "Std. dev. of 10-year real rate")

TT <- length(US_yields_q$DTB3)
all.targets.US <- c(all.targets.US,
                    correl.rm.na(US_yields_q$ST.REAL.RATE[2:TT],US_yields_q$ST.REAL.RATE[1:(TT-1)]),
                    correl.rm.na(US_yields_q$TIPSY02[2:TT],US_yields_q$TIPSY02[1:(TT-1)]),
                    correl.rm.na(US_yields_q$TIPSY10[2:TT],US_yields_q$TIPSY10[1:(TT-1)]))

all.target.names <- c(all.target.names,
                      "Auto-correl. of short-term real rate",
                      "Auto-correl. of 2-year real rate",
                      "Auto-correl. of 10-year real rate")

all.targets.US <- c(all.targets.US,
                    mean(US_yields_q$TIPSY10 - US_yields_q$ST.REAL.RATE,na.rm=TRUE)/100,
                    sd(US_yields_q$TIPSY10 - US_yields_q$ST.REAL.RATE,na.rm=TRUE)/100)

all.target.names <- c(all.target.names,
                      "Mean of slope of the real yd curve (3m-10yrs)",
                      "Std. dev. of slope of the real yd curve (3m-10yrs)")


# =========================================
# Means and variances of condi. var. of interest rates

all.targets.US <- c(all.targets.US,
                    mean(US_yields_q$DTB3.condiVar/100^2,na.rm=TRUE),
                    mean(US_yields_q$SVENY30.condiVar/100^2,na.rm=TRUE),
                    mean(US_yields_q$TIPSY10.condiVar/100^2,na.rm=TRUE))


all.target.names <- c(all.target.names,
                      "Mean of condi. var. of the short-term nom. rate",
                      "Mean of condi. var. of the 30-year nom. rate",
                      "Mean of condi. var. of the 10-year real rate")

# =========================================
# Stock returns

all.targets.US <- c(all.targets.US,
                    avg.annualized.return.real -
                      (mean(US_yields_q$DTB3,na.rm=TRUE)/100 - mean(pi.US)*FREQ),
                    std.annualized.return.real)

all.target.names <- c(all.target.names,
                      "Average expected excess return (annualized)",
                      "Average cond. volat. of stock return (annualized)")

all.targets.US <- c(all.targets.US,
                    mean(Data_Shiller$P/Data_Shiller$D),
                    sd(Data_Shiller$P/Data_Shiller$D))

all.target.names <- c(all.target.names,
                      "Average P/D",
                      "Std. dev. of P/D")



# =========================================
# Definition of weights used in the computation of the loss function:

all.weights.US <- rep(10^100,length(all.targets.US))

all.weights.US[c(2,3)] <- .001 # means of pi and c
all.weights.US[c(4,5,6)] <- .005 # st dev of pi and c

#all.weights.US[c(7,8)] <- c(.1,2) # skewness and kurtosis
all.weights.US[c(7,8)] <- c(100,100) # skewness and kurtosis

all.weights.US[9:11] <- c(.003,.002,.001) # means of nominal rates
all.weights.US[12:14] <- .001 # std dev of nominal rates
all.weights.US[15:17] <- .1 # auto-cor of nominal rates

all.weights.US[20:22] <- c(.003,.002,.001) # mean of real rates
all.weights.US[23:25] <- .002 # std dev of real rates
all.weights.US[26:28] <- .1 # auto-cor of real rates

all.weights.US[c(19,30)] <- .003 # Std. dev. of slope of the nom and real yd curve
all.weights.US[c(31,32,33)] <- 2e-05 # Mean of condi. var. of rates

all.weights.US[34]  <- abs(all.targets.US)[34] / 5
all.weights.US[35]  <- abs(all.targets.US)[35] / 2
all.weights.US[36]  <- abs(all.targets.US)[36] / 40 # Average P/D
all.weights.US[37]  <- abs(all.targets.US)[37] / 20 # Std. dev. of P/D


# =========================================
# Specifiy multiplicative factors (for fit table)

all.multiplicative.factors <- c(
  2,2,2,2,2,2,
  0,0, # skewness and kurtosis
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




