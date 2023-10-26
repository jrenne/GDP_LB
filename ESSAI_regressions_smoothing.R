

dy.US.4 <- log(macro.data$GDP.US[indic.first.date:indic.last.date]/
                 macro.data$GDP.US[(indic.first.date-4):(indic.last.date-4)])

pi.US.4 <- log(macro.data$GDPDEF.US[indic.first.date:indic.last.date]/
                 macro.data$GDPDEF.US[(indic.first.date-4):(indic.last.date-4)])

dy.US.12 <- log(macro.data$GDP.US[indic.first.date:indic.last.date]/
               macro.data$GDP.US[(indic.first.date-12):(indic.last.date-12)])

pi.US.12 <- log(macro.data$GDPDEF.US[indic.first.date:indic.last.date]/
              macro.data$GDPDEF.US[(indic.first.date-12):(indic.last.date-12)])

eq <- lm(US_yields_q$SVENY05~dy.US.4 + pi.US.4 + dy.US.12 + pi.US.12)
summary(eq)


dy.US.smooth <- rep(NaN,T)
pi.US.smooth <- rep(NaN,T)

rho <- .9

T <- dim(macro.data)[1]
macro.data$dy.US.1 <- NaN
macro.data$dy.US.1[2:T] <- log(macro.data$GDP.US[2:T]/macro.data$GDP.US[1:(T-1)])
macro.data$pi.US.1 <- NaN
macro.data$pi.US.1[2:T] <- log(macro.data$GDPDEF.US[2:T]/macro.data$GDPDEF.US[1:(T-1)])

n <- 20
weights <- 1/rho^(1:n)

macro.data$dy.US.smooth <- NaN
macro.data$pi.US.smooth <- NaN

for(i in n:dim(macro.data)[1]){
  macro.data$dy.US.smooth[i] <- sum(macro.data$dy.US.1[(i-n+1):i]*weights/sum(weights),na.rm = TRUE)
  macro.data$pi.US.smooth[i] <- sum(macro.data$pi.US.1[(i-n+1):i]*weights/sum(weights),na.rm = TRUE)
}

dy.US.smooth <- 4*macro.data$dy.US.smooth[indic.first.date:indic.last.date]
pi.US.smooth <- 4*macro.data$pi.US.smooth[indic.first.date:indic.last.date]



# Levels:

eq0 <- lm(US_yields_q$SVENY10~dy.US.4 + pi.US.4)
eq1 <- lm(US_yields_q$SVENY10~dy.US.4 + pi.US.4 + dy.US.smooth + pi.US.smooth)
eq2 <- lm(US_yields_q$SVENY10~dy.US.smooth + pi.US.smooth)
summary(eq0)
summary(eq1)
summary(eq2)

plot(US_yields_q$SVENY10,type="l")
lines(eq0$fitted.values,col="red")
lines(eq1$fitted.values,col="blue")
lines(eq2$fitted.values,col="green")


# First differences:

eq0 <- lm(diff(US_yields_q$SVENY10)~diff(dy.US.4) + diff(pi.US.4))
eq1 <- lm(diff(US_yields_q$SVENY10)~diff(dy.US.4) + diff(pi.US.4) +
            diff(dy.US.smooth) + diff(pi.US.smooth))
eq2 <- lm(diff(US_yields_q$SVENY10)~diff(dy.US.smooth) + diff(pi.US.smooth))
summary(eq0)
summary(eq1)
summary(eq2)


# Slope, levels:

slope <- US_yields_q$SVENY10 -  US_yields_q$SVENY01
  
eq0 <- lm(slope ~ dy.US.4 + pi.US.4)
eq1 <- lm(slope ~ dy.US.4 + pi.US.4 + dy.US.smooth + pi.US.smooth)
eq2 <- lm(slope ~ dy.US.smooth + pi.US.smooth)
summary(eq0)
summary(eq1)
summary(eq2)

plot(slope,type="l")
lines(eq0$fitted.values,col="red")
lines(eq1$fitted.values,col="blue")
lines(eq2$fitted.values,col="green")







Y <- US_yields_q[,6:16]
res.PC.Y <- prcomp(Y)
cumsum(res.PC.Y$sdev/sum(res.PC.Y$sdev))

DY <- Y[2:dim(Y)[1],] - Y[1:(dim(Y)[1]-1),]
res.PC.DY <- prcomp(DY)
cumsum(res.PC.DY$sdev/sum(res.PC.DY$sdev))



