


# Multivariate normal distribution:
N.multi.pdf <- function(eta,Omega){
  # eta of dimension T x N
  # Omega (covariance matrix) of dimension N x N
  T <- dim(eta)[1]
  N <- dim(eta)[2]
  Omega_1 <- solve(Omega)
  det.Omega <- det(Omega)
  aux <- (eta %x% matrix(1,1,N)) * (matrix(1,1,N) %x% eta) *
    (matrix(1,T,1) %*% matrix(Omega_1,nrow=1))
  res <- - log(sqrt((2*pi)^N*det(Omega))) -.5*apply(aux,1,function(x){sum(x,na.rm = TRUE)})
  return(res)
}




# Determine regime and pi(t) (seen as latent), taking into account consumption:


estimate.latent.factors <-function(res.prices,
                                   dc.US,dy.US,pi.US,
                                   gdp.forecast.4Q,infl.forecast.4Q,
                                   gdp.forecast.8Q,infl.forecast.8Q,
                                   US_yields_q,
                                   multi.factor.for.pi.obs = .5, # determine inflation quality of fit in OLS step
                                   multi.factor.for.pi.fcst = .5 # determine inflation-fcst quality of fit in OLS step
){
  
  mod.sol <- res.prices$Model.solved
  
  N <- length(mod.sol$mu)
  
  # Computation of nu w.r.t. regime:
  aux_1overLambda <- t(matrix(1/mod.sol$lambda,N,N))
  aux_s           <- matrix(mod.sol$mu,N,N)
  aux_s_1         <- t(matrix(mod.sol$mu,N,N))
  matrix_nu       <- aux_1overLambda * (aux_s  - (1-mod.sol$phi)*res.prices$s.bar
                                        - mod.sol$phi * aux_s_1)
  
  T <- length(dc.US)
  
  # Initialization -------------------------------------------------------------
  # Initialize pi estimate
  pi.hat <- matrix(NaN,T,1) 
  # Initialize z estimate
  uncond.distri.z <- ((t(res.prices$P))%^%100)[,30]
  z.hat    <- matrix(0,T,1)
  z.hat[1] <- which(uncond.distri.z==max(uncond.distri.z))
  
  # Prepare data to fit --------------------------------------------------------
  Y.OLS <- cbind(US_yields_q$SVENY02/100,
                 US_yields_q$SVENY10/100,
                 pi.US,
                 dc.US,
                 gdp.forecast.4Q,
                 infl.forecast.4Q)
  
  Y.likelihood <- cbind(Y.OLS,
                        gdp.forecast.8Q,
                        infl.forecast.8Q)
  # Determine targeted fit (4 likelihood):
  R <- diag(c(.1*var(Y.likelihood[,1],na.rm = TRUE),
              .1*var(Y.likelihood[,2],na.rm = TRUE),
              .1*var(Y.likelihood[,3],na.rm = TRUE),
              .1*var(Y.likelihood[,4],na.rm = TRUE),
              .1*var(Y.likelihood[,5],na.rm = TRUE),
              .1*var(Y.likelihood[,6],na.rm = TRUE),
              .1*var(Y.likelihood[,7],na.rm = TRUE),
              .1*var(Y.likelihood[,8],na.rm = TRUE)))
  
  maturities.in.yrs.nom.yields <- c(2,10)
  indic.of.nom.yds.in.res.prices <- c(
    which(res.prices$vec.maturities == mod.sol$freq*maturities.in.yrs.nom.yields[1]),
    which(res.prices$vec.maturities == mod.sol$freq*maturities.in.yrs.nom.yields[2])
  )
  
  freq <- mod.sol$freq
  
  
  # Prepare specification of model-implied forecasts ---------------------------
  
  # Inflation, horizon 4Q:
  loading.pi.4.Infl4Q <- mod.sol$psi + mod.sol$psi^2 +
    mod.sol$psi^3 + mod.sol$psi^4
  mu.4.Infl4Q <- (1-mod.sol$psi)*
    (1 + mod.sol$psi + mod.sol$psi^2 + mod.sol$psi^3)*mod.sol$freq*mod.sol$pi.bar
  
  # Inflation, horizon 8Q:
  loading.pi.4.Infl8Q <- 
    (mod.sol$psi^1 + mod.sol$psi^2 + mod.sol$psi^3 + mod.sol$psi^4 + 
       mod.sol$psi^5 + mod.sol$psi^6 + mod.sol$psi^7 + mod.sol$psi^8)/2
  mu.4.Infl8Q <- (1-mod.sol$psi)*
    (1 + mod.sol$psi + mod.sol$psi^2 + mod.sol$psi^3 +
       mod.sol$psi^4 + mod.sol$psi^5 + mod.sol$psi^6 + mod.sol$psi^37)*
    mod.sol$freq*mod.sol$pi.bar/2
  
  # GDP growth, horizon 4Q:
  vec.4.GDP4Q  <- mod.sol$g
  for(h in 1:3){
    vec.4.GDP4Q <- vec.4.GDP4Q + ((res.prices$P))%^%h %*% mod.sol$g
  }
  # GDP growth, horizon 8Q:
  vec.4.GDP8Q  <- vec.4.GDP4Q
  for(h in 4:7){
    vec.4.GDP8Q <- vec.4.GDP8Q + ((res.prices$P))%^%h %*% mod.sol$g
  }
  vec.4.GDP8Q <- vec.4.GDP8Q/2 # annualization
  
  
  fitted <- NaN * Y.likelihood
  
  vec.logl <- rep(NaN,T)
  
  for(t in 2:T){
    
    # First step: For each regime, determine optimal pi(t): --------------------
    
    observations <- Y.OLS[t,]
    
    loadings.on.pi <- 
      matrix(c(res.prices$all.nom.bond.b.yields[indic.of.nom.yds.in.res.prices[1]],
               res.prices$all.nom.bond.b.yields[indic.of.nom.yds.in.res.prices[1]],
               1 * multi.factor.for.pi.obs,
               0,
               0,
               loading.pi.4.Infl4Q * multi.factor.for.pi.fcst),ncol=1)
    
    observations.per.regime <- matrix(observations,ncol=1) %*% matrix(1,1,N)
    
    # Remove constant terms:
    # first nom yields:
    observations.per.regime[1,] <- observations.per.regime[1,] - 
      res.prices$all.nom.bond.F.yields[indic.of.nom.yds.in.res.prices[1],]
    # second nom yields:
    observations.per.regime[2,] <- observations.per.regime[2,] - 
      res.prices$all.nom.bond.F.yields[indic.of.nom.yds.in.res.prices[2],]
    # Inflation:
    observations.per.regime[3,] <- observations.per.regime[3,] * multi.factor.for.pi.obs
    # Inflation forecast:
    observations.per.regime[6,] <- (observations.per.regime[6,] - mu.4.Infl4Q) * multi.factor.for.pi.fcst
    
    # Apply OLS approach (find best pi estimate for each regime):
    XX_1 <- 1/sum(loadings.on.pi*loadings.on.pi)
    pi.hats <- XX_1 * t(loadings.on.pi) %*% observations.per.regime
    
    
    # Second step: detect regime that gives the best fit: ----------------------
    
    observations <- Y.likelihood[t,]
    
    # Compute measurement errors per regime:
    measur.errors    <- matrix(0,length(observations),N)
    fitted.4.regimes <- matrix(0,length(observations),N)
    
    all.dc <- matrix_nu[,z.hat[t-1]] + mod.sol$g
    
    fitted.4.regimes[1,] <- res.prices$all.nom.bond.F.yields[indic.of.nom.yds.in.res.prices[1],] + 
      res.prices$all.nom.bond.b.yields[indic.of.nom.yds.in.res.prices[1]] * pi.hats
    fitted.4.regimes[2,] <- res.prices$all.nom.bond.F.yields[indic.of.nom.yds.in.res.prices[2],] + 
      res.prices$all.nom.bond.b.yields[indic.of.nom.yds.in.res.prices[2]] * pi.hats
    fitted.4.regimes[3,] <- pi.hats
    fitted.4.regimes[4,] <- all.dc
    fitted.4.regimes[5,] <- vec.4.GDP4Q
    fitted.4.regimes[6,] <- mu.4.Infl4Q + loading.pi.4.Infl4Q * pi.hats
    fitted.4.regimes[7,] <- vec.4.GDP8Q
    fitted.4.regimes[8,] <- mu.4.Infl8Q + loading.pi.4.Infl8Q * pi.hats
    
    measur.errors <- matrix(observations,length(observations),N) -
      fitted.4.regimes
    
    if((sum(is.na(measur.errors))/N)>sum(is.na(observations))){
      return(list(
        z.hat = z.hat,
        pi.hat = pi.hat,
        observed = Y.likelihood, 
        fitted = fitted,
        vec.logl = vec.logl,
        logl = -100000*sum(is.na(measur.errors))
      ))
    }else{
      logl <- N.multi.pdf(t(measur.errors),R)
      # Store results ----------------------------------------------------------
      z.hat[t]    <- which(logl == max(logl,na.rm = TRUE))
      pi.hat[t]   <- pi.hats[z.hat[t]]
      fitted[t,]  <- fitted.4.regimes[,z.hat[t]]
      vec.logl[t] <- max(logl)
    }
  }
  
  return(list(
    z.hat = z.hat,
    pi.hat = pi.hat,
    observed = Y.likelihood, 
    fitted = fitted,
    vec.logl = vec.logl,
    logl = sum(vec.logl,na.rm = TRUE)
  ))
}


compute.uncond.mom.pi.z <- function(Model){
  
  P <- Model$P
  
  N <- dim(P)[1]
  
  EIG <- eigen(t(P))
  E.z <- matrix(
    abs(EIG$vectors[,1])/sum(abs(EIG$vectors[,1])),
    ncol=1)
  V.z <- diag(c(E.z)) - E.z %*% t(E.z)
  
  B <-
    matrix(1/Model$lambda,ncol=1) %*% matrix(Model$mu,nrow=1) -
    matrix((1 - Model$phi) * Model$s.bar / Model$lambda + Model$phi * Model$mu / Model$lambda,
           N,N)
  
  V.pi <- (Model$sigma.pi^2 +
             Model$rho.pi^2 * matrix(
               diag(P %*% (t(B) * t(B)) - (P %*% t(B))*(P %*% t(B))),1,N) %*% E.z)/
    (1-Model$psi^2)
  
  Cov.pi.z <- Model$rho.pi * solve(diag(N) - Model$psi * t(P)) %*%
    (t(P) * t(B) - t(P) * (matrix(1,N,N) %*% (t(P) * t(B)))) %*% E.z
  
  Var.pi.z <- rbind(
    c(V.pi,Cov.pi.z),
    cbind(Cov.pi.z,V.z)
  )
  
  E.pi.z <- matrix(c(Model$pi.bar,E.z),ncol=1)
  
  PHI <- matrix(0,N+1,N+1)
  PHI[1,1] <- Model$psi
  PHI[2:dim(PHI)[1],2:dim(PHI)[2]] <- t(P)
  
  AutoCov.pi.z <- PHI %*% Var.pi.z
  
  # Computation of average conditional covariance matrix of (pi,z')':
  G.E.z_pi <- (Model$sigma.pi^2 +
                 Model$rho.pi^2 * matrix(
                   diag(P %*% (t(B) * t(B)) - (P %*% t(B))*
                          (P %*% t(B))),1,N) %*% E.z)
  G.E.z_Cov.pi.z <- Model$rho.pi *
    (t(P) * t(B) - t(P) * (matrix(1,N,N)
                           %*% (t(P) * t(B)))) %*% E.z
  G.E.z_z <- diag(c(t(P) %*% E.z)) - t(P) %*% diag(c(E.z)) %*% P
  
  G.E.z <- rbind(
    c(G.E.z_pi,G.E.z_Cov.pi.z),
    cbind(G.E.z_Cov.pi.z,G.E.z_z)
  )
  
  # # Computation of Var(nu):
  # Var.nu <- Model$sigma.nu^2 +
  #   (1 - Model$p) * (Model$eta * Model$p / (1 - Model$p))^2 +
  #   Model$p * Model$eta^2
  # 
  # # Computation of Var(pi,y,c)
  # Phi <- matrix(0,3,3)
  # Phi[1,1] <- Model$psi
  # Sigma <- diag(3)
  # Sigma[1,3] <- Model$rho.pi
  # Sigma[2,3] <- Model$rho.y
  # vec.Var.pi.y.c <- solve(diag(9) - Phi %x% Phi) %*%
  #   c(Sigma %*% diag(c(Model$sigma.pi^2,Model$sigma.y^2,Var.nu)) %*% t(Sigma))
  # Var.pi.y.c <- matrix(vec.Var.pi.y.c,3,3)
  # stdv.pi.y.c <- sqrt(diag(Var.pi.y.c))
  # Cor.pi.y.c <- Var.pi.y.c / (matrix(stdv.pi.y.c,ncol=1)%*%matrix(stdv.pi.y.c,nrow=1))
  
  # Computation of Var(pi,y,c)
  aux <- matrix(0,3,1+N)
  aux[1,1] <- Model$psi
  aux[2,2:(N+1)] <- t(Model$g)
  aux[3,2:(N+1)] <- t(Model$g)
  Sigma <- diag(3)
  Sigma[1,3] <- Model$rho.pi
  Sigma[2,3] <- Model$rho.y
  aux2 <- diag(c(Model$sigma.pi^2,
                 Model$sigma.y^2,
                 Model$sigma.nu^2))
  
  Var.pi.y.c  <- aux %*% Var.pi.z %*% t(aux) + Sigma %*% aux2 %*% t(Sigma)
  stdv.pi.y.c <- sqrt(diag(Var.pi.y.c))
  Cor.pi.y.c  <- Var.pi.y.c / (matrix(stdv.pi.y.c,ncol=1)%*%matrix(stdv.pi.y.c,nrow=1))
  
  return(list(
    E.z = E.z,
    V.z = V.z,
    V.pi = V.pi,
    Cov.pi.z = Cov.pi.z,
    Var.pi.z = Var.pi.z,
    E.pi.z = E.pi.z,
    AutoCov.pi.z = AutoCov.pi.z,
    Var.pi.y.c = Var.pi.y.c,
    Cor.pi.y.c = Cor.pi.y.c,
    G.E.z = G.E.z
  ))
}


compute.moments <- function(Model,
                            vec.maturities, # maturities considered (expressed in number of model periods)
                            nb.values.s,
                            h.stock,
                            curvature,
                            grid.4.S,
                            indic.slope.nom.curve,
                            indic.slope.rea.curve,
                            indic.condVar.nom.rate,
                            indic.condVar.rea.rate,
                            indic.compute.slopes = FALSE
)
{
  # vec.maturities is a vector indicating the maturities considered, these maturities
  #       are expressed in number of model's periods (i.e. 40 for 10-year bonds and quarterly-frequency models).
  # Yields output are annualized.
  
  RES.prices <- compute.prices(Model,
                               vec.maturities, # maturities considered (expressed in number of model periods)
                               nb.values.s,
                               h.stock,
                               curvature, # this controls the dispersion of S values in the grid (the higher this parameter, the larger the number of values XXX)
                               grid.4.S # If it is non NaN, then these values are used as a grid for S)
  )
  Model.solved <- RES.prices$Model.solved
  
  P <- RES.prices$P
  Q <- RES.prices$P
  values.s    <- RES.prices$values.s
  nb.values.s <- RES.prices$nb.values.s
  index.s.bar <- RES.prices$index.s.bar
  
  unc.mom.pi.z <- compute.uncond.mom.pi.z(Model.solved)
  E.z <- unc.mom.pi.z$E.z
  V.z <- unc.mom.pi.z$V.z
  V.pi.z <- unc.mom.pi.z$Var.pi.z
  G.E.z <- unc.mom.pi.z$G.E.z
  AutoCov.pi.z <- unc.mom.pi.z$AutoCov.pi.z
  AutoCov.z <- AutoCov.pi.z[2:dim(AutoCov.pi.z)[1],2:dim(AutoCov.pi.z)[2]]
  
  stdv.pi <- sqrt(unc.mom.pi.z$Var.pi.y.c[1,1])
  stdv.dc <- sqrt(unc.mom.pi.z$Var.pi.y.c[3,3])
  stdv.pi <- sqrt(unc.mom.pi.z$Var.pi.y.c[1,1])
  stdv.dy <- sqrt(unc.mom.pi.z$Var.pi.y.c[2,2])
  
  # Average nominal yields
  RES.prices$all.nom.bond.F[RES.prices$all.nom.bond.F<=0] <- 10^(-50)
  mean.nom.yields <- Model$freq/vec.maturities * (
    RES.prices$all.nom.bond.b * Model$pi.bar - log(RES.prices$all.nom.bond.F) %*% E.z
  )
  
  # Std Dev nominal yields
  loadings.nom.yields <- Model$freq/vec.maturities * cbind(
    c(RES.prices$all.nom.bond.b),
    - log(RES.prices$all.nom.bond.F))
  var.nom.yds <- loadings.nom.yields %*% V.pi.z %*% t(loadings.nom.yields)
  autocov.nom.yds <- loadings.nom.yields %*% AutoCov.pi.z %*% t(loadings.nom.yields)
  stdv.nom.yds = sqrt(diag(var.nom.yds))
  cor.nom.yds <- var.nom.yds /
    (matrix(stdv.nom.yds,ncol=1) %*% matrix(stdv.nom.yds,nrow=1))
  autocor.nom.yds <- autocov.nom.yds /
    (matrix(stdv.nom.yds,ncol=1) %*% matrix(stdv.nom.yds,nrow=1))
  
  # Slope of the nominal yield curve
  mean.slope.nom.curve <- mean.nom.yields[indic.slope.nom.curve[2]]-
    mean.nom.yields[indic.slope.nom.curve[1]]
  
  # Std Dev slope of nominal yield curve
  loadings.nom.yield.slope <- loadings.nom.yields[indic.slope.nom.curve[2],] - 
    loadings.nom.yields[indic.slope.nom.curve[1],]
  loadings.nom.yield.slope <- matrix(loadings.nom.yield.slope,nrow=1)
  var.nom.yield.slope <- loadings.nom.yield.slope %*% V.pi.z %*% t(loadings.nom.yield.slope)
  stdv.nom.yield.slope <- sqrt(diag(var.nom.yield.slope))
  
  # Average real yields
  mean.rea.yields <- Model$freq/vec.maturities * (
    - log(RES.prices$all.rea.bond.prices) %*% E.z
  )
  
  # Std Dev real yields
  var.rea.yds <- RES.prices$all.rea.bond.yields %*% V.z %*% t(RES.prices$all.rea.bond.yields)
  autocov.rea.yds <- RES.prices$all.rea.bond.yields %*% AutoCov.z %*% t(RES.prices$all.rea.bond.yields)
  stdv.rea.yds <- sqrt(diag(var.rea.yds))
  cor.rea.yds <- var.rea.yds /
    (matrix(stdv.rea.yds,ncol=1) %*% matrix(stdv.rea.yds,nrow=1))
  autocor.rea.yds <- autocov.rea.yds /
    (matrix(stdv.rea.yds,ncol=1) %*% matrix(stdv.rea.yds,nrow=1))
  
  # Slope of the real yield curve
  mean.slope.rea.curve <- mean.nom.yields[indic.slope.rea.curve[2]]-
    mean.nom.yields[indic.slope.rea.curve[1]]
  
  # Std Dev slope of real yield curve
  loadings.rea.yield.slope <- RES.prices$all.rea.bond.yields[indic.slope.rea.curve[2],] - 
    RES.prices$all.rea.bond.yields[indic.slope.rea.curve[1],]
  loadings.rea.yield.slope <- matrix(loadings.rea.yield.slope,nrow=1)
  var.rea.yield.slope <- loadings.rea.yield.slope %*% V.z %*% t(loadings.rea.yield.slope)
  stdv.rea.yield.slope <- sqrt(diag(var.rea.yield.slope))
  
  # Average GDP-indexed yields
  mean.GDP.yields <- Model$freq/vec.maturities * (
    - log(RES.prices$all.GDP.bond.prices) %*% E.z
  )
  
  # Std Dev GDP-indexed yields
  var.GDP.yds <- RES.prices$all.GDP.bond.yields %*% V.z %*% t(RES.prices$all.GDP.bond.yields)
  stdv.GDP.yds <- sqrt(diag(var.GDP.yds))
  cor.GDP.yds <- var.GDP.yds /
    (matrix(stdv.GDP.yds,ncol=1) %*% matrix(stdv.GDP.yds,nrow=1))
  
  # Excess stock return
  E.r.m <- Model$freq * sum(RES.prices$Exp.stock.return * E.z)
  E.r.rf <- Model$freq * sum(RES.prices$mu.r * E.z)
  E.xs <- E.r.m - E.r.rf # average expected return (real terms)
  V.r.m <- sum(RES.prices$CondVar.stock.return * E.z) # average conditional variance
  E.P.over.D <- sum(RES.prices$Chi * E.z) / Model$freq
  var.P.over.D <- matrix(RES.prices$Chi/Model$freq,nrow=1) %*%
    V.z %*% matrix(RES.prices$Chi/Model$freq,ncol=1)
  
  # Regression coefficient stock excess return on log(P/D):
  all.slope.xs.logPD <- matrix(NaN,max(vec.maturities),1)
  if(indic.compute.slopes){
    P     <- RES.prices$P
    M     <- RES.prices$M
    Chi   <- RES.prices$Chi
    logChiannual <- log(Chi/Model$freq)
    N     <- dim(P)[1]
    H.h_1 <- diag(N) # initialization
    for(h in 1:max(vec.maturities)){
      H.h <- M %*% diag(diag(H.h_1)) %*% t(P)
      mu.rh <- matrix(res.prices$all.rea.bond.yields[h,],ncol=1)
      all.slope.xs.logPD[h] <- (t(Model$freq/h * diag(H.h)*logChiannual - mu.rh*logChiannual) %*% E.z - 
                                  (t(Model$freq/h * diag(H.h) - mu.rh) %*% E.z) * t(logChiannual) %*% E.z) /
        (t(logChiannual) %*% V.z %*% logChiannual)
      H.h_1 <- H.h
    }
  }
  
  # (Average) Conditional variances of nominal rates
  loadings.nom.4.condVar <- loadings.nom.yields[indic.condVar.nom.rate,]
  mean.of.CondVar.nom.yds <- loadings.nom.4.condVar %*% G.E.z %*%
    t(loadings.nom.4.condVar)
  mean.of.CondVar.nom.yds <- diag(mean.of.CondVar.nom.yds)
  
  # (Average) Conditional variances of real rates
  loadings.rea.4.condVar <- RES.prices$all.rea.bond.yields[indic.condVar.rea.rate,]
  loadings.rea.4.condVar <- matrix(loadings.rea.4.condVar,nrow=1)
  mean.of.CondVar.rea.yds <- loadings.rea.4.condVar %*% G.E.z[2:dim(G.E.z)[1],2:dim(G.E.z)[2]] %*%
    t(loadings.rea.4.condVar)
  mean.of.CondVar.rea.yds <- c(mean.of.CondVar.rea.yds)
  
  # Skewness and kurtosis of consumption growth:
  E.g1z <- sum(Model.solved$g^1 * E.z)
  E.g2z <- sum(Model.solved$g^2 * E.z)
  E.g3z <- sum(Model.solved$g^3 * E.z)
  E.g4z <- sum(Model.solved$g^4 * E.z)
  
  E.dc.3rd.order <- E.g3z - 3*E.g2z*E.g1z + 2*E.g1z^3
  E.dc.4th.order <- E.g4z - 4*E.g3z*E.g1z + 6*(E.g2z)*(E.g1z)^2 - 3*E.g1z^4 +
    6 * Model$sigma.nu^2 * (E.g2z - E.g1z^2) + 3*Model$sigma.nu^4
  sd.dc <- sqrt(E.g2z - E.g1z^2 + Model$sigma.nu^2)
  
  skewness.dc <- E.dc.3rd.order / sd.dc^3
  kurtosis.dc <- E.dc.4th.order / sd.dc^4
  
  return(list(
    mean.nom.yields = mean.nom.yields,
    mean.rea.yields = mean.rea.yields,
    mean.GDP.yields = mean.GDP.yields,
    stdv.nom.yds = stdv.nom.yds,
    stdv.rea.yds = stdv.rea.yds,
    stdv.GDP.yds = stdv.GDP.yds,
    skewness.dc = skewness.dc,
    kurtosis.dc = kurtosis.dc,
    var.nom.yds = var.nom.yds,
    var.rea.yds = var.rea.yds,
    var.GDP.yds = var.GDP.yds,
    cor.nom.yds = cor.nom.yds,
    cor.rea.yds = cor.rea.yds,
    cor.GDP.yds = cor.GDP.yds,
    autocor.nom.yds = autocor.nom.yds,
    autocor.rea.yds = autocor.rea.yds,
    autocov.nom.yds = autocov.nom.yds,
    autocov.rea.yds = autocov.rea.yds,
    stdv.pi = stdv.pi,
    stdv.dy = stdv.dy,
    stdv.dc = stdv.dc,
    E.r.m = E.r.m,
    E.r.rf = E.r.rf,
    E.xs = E.xs,
    V.r.m = V.r.m,
    E.P.over.D = E.P.over.D,
    stdv.P.over.D = sqrt(var.P.over.D),
    mean.slope.nom.curve = mean.slope.nom.curve,
    mean.slope.rea.curve = mean.slope.rea.curve,
    stdv.nom.yield.slope = stdv.nom.yield.slope,
    stdv.rea.yield.slope = stdv.rea.yield.slope,
    mean.of.CondVar.nom.yds = mean.of.CondVar.nom.yds,
    mean.of.CondVar.rea.yds = mean.of.CondVar.rea.yds,
    all.slope.xs.logPD = all.slope.xs.logPD,
    E.z = E.z,
    res.prices = RES.prices
  ))
}







correl.rm.na <- function(x,y){
  AUX <- x+y
  indic.which.is.not.na <- which(!is.na(AUX))
  x.aux <- x[indic.which.is.not.na]
  y.aux <- y[indic.which.is.not.na]
  return(cor(x.aux,y.aux))
}


compare.target.and.model <- function(Model,
                                     vec.maturities, # maturities considered (expressed in number of model periods)
                                     nb.values.s,
                                     h.stock,
                                     curvature,
                                     grid.4.S,
                                     indic.the.ones.used.in.nom.curve,
                                     indic.the.ones.used.in.rea.curve,
                                     indic.slope.nom.curve,
                                     indic.slope.rea.curve,
                                     indic.condVar.nom.rate,
                                     indic.condVar.rea.rate){
  
  res <- compute.moments(Model,
                         vec.maturities, # maturities considered (expressed in number of model periods)
                         nb.values.s,
                         h.stock,
                         curvature,
                         grid.4.S,
                         indic.slope.nom.curve,
                         indic.slope.rea.curve,
                         indic.condVar.nom.rate,
                         indic.condVar.rea.rate)
  
  modelled.E.dc <- sum(Model$E.z*Model$g_c)
  modelled.E.dy <- modelled.E.dc
  modelled.moments <- c(modelled.E.dy,modelled.E.dc,Model$pi.bar,
                        res$stdv.dy,res$stdv.dc,res$stdv.pi,
                        #res$skewness.dc, res$kurtosis.dc,
                        res$mean.nom.yields[indic.the.ones.used.in.nom.curve],
                        res$stdv.nom.yds[indic.the.ones.used.in.nom.curve],
                        res$autocor.nom.yds[indic.the.ones.used.in.nom.curve[1],indic.the.ones.used.in.nom.curve[1]],
                        res$autocor.nom.yds[indic.the.ones.used.in.nom.curve[2],indic.the.ones.used.in.nom.curve[2]],
                        res$autocor.nom.yds[indic.the.ones.used.in.nom.curve[3],indic.the.ones.used.in.nom.curve[3]],
                        res$mean.slope.nom.curve, # slope of nominal yield curve
                        res$stdv.nom.yield.slope, # stdv of nominal yield curve
                        res$mean.rea.yields[indic.the.ones.used.in.rea.curve],
                        res$stdv.nom.yds[indic.the.ones.used.in.rea.curve],
                        res$autocor.rea.yds[indic.the.ones.used.in.rea.curve[1],indic.the.ones.used.in.rea.curve[1]],
                        res$autocor.rea.yds[indic.the.ones.used.in.rea.curve[2],indic.the.ones.used.in.rea.curve[2]],
                        res$autocor.rea.yds[indic.the.ones.used.in.rea.curve[3],indic.the.ones.used.in.rea.curve[3]],
                        res$mean.slope.rea.curve, # slope of real yield curve
                        res$stdv.rea.yield.slope, # stdv of real yield curve
                        res$mean.of.CondVar.nom.yds, # mean of conditional var of nominal yields (2 maturities)
                        #c(1000,1000), # stdv of conditional var of nominal yields (2 maturities)
                        res$mean.of.CondVar.rea.yds, # mean of condiitonal var of real yields (1 maturity)
                        #1000,  # stdv of condiitonal var of real yields (1 maturity)
                        res$E.xs, # annualized expected excess return on stocks
                        sqrt(Model$freq) * sqrt(res$V.r.m), # annualized conditional volatility
                        res$E.P.over.D, # average P over D (annualized)
                        res$stdv.P.over.D # stdv of P over D (annualized)
  )
  return(list(modelled.moments = modelled.moments,
              res.prices = res$res.prices))
}


Theta.2.Model <- function(theta){
  Model <- list(
    sigma.nu = exp(theta[1]),
    phi = exp(theta[2])/(1 + exp(theta[2])),
    delta = exp(theta[3])/(1 + exp(theta[3])),
    b = NaN,
    Gamma = abs(theta[5]),
    pi.bar = theta[6],
    psi = exp(theta[7])/(1 + exp(theta[7])),
    sigma.pi = exp(theta[8]),
    rho.pi = theta[9],
    sigma.y = exp(theta[10]),
    rho.y = theta[11],
    sigma.d = exp(theta[12]),
    rho.d = theta[13],
    g_c = c(theta[14],theta[14]+exp(theta[15]),
            theta[14]+exp(theta[15])+exp(theta[16])),
    p_ll = .01 + .98*exp(theta[17])/(1 + exp(theta[17])),
    p_hh = .01 + .98*exp(theta[18])/(1 + exp(theta[18])),
    p_ii = .01 + .98*exp(theta[19])/(1 + exp(theta[19])),
    freq = FREQ
  )
  # Make sure that b < Gamma * (1 - phi)
  Model$b <- (1 - 2 *exp(theta[4])/(1 + exp(theta[4]))) *
    Model$Gamma * (1 - Model$phi)
  
  # Determine p_il:
  Model$p_il = (1-Model$p_ii)*(.01 + .98*exp(theta[20])/(1 + exp(theta[20])))
  
  # Add g_d:
  P <- matrix(0,3,3)
  P[1,1] <- Model$p_ll
  P[1,2] <- 1-Model$p_ll
  P[3,3] <- Model$p_hh
  P[2,3] <- 1-Model$p_hh
  P[2,2] <- Model$p_ii
  P[2,1] <- Model$p_il
  P[2,3] <- 1 - Model$p_ii - Model$p_il
  EIG <- eigen(t(P))
  E.z <- matrix(
    abs(EIG$vectors[,1])/sum(abs(EIG$vectors[,1])),
    ncol=1)
  mean.g_c  <- sum(E.z*Model$g_c)
  Model$g_d <- mean.g_c + Model$rho.d*(Model$g_c-mean.g_c)
  Model$E.z <- E.z
  return(Model)
}

inv.logit <- function(rho){
  return(log(rho/(1-rho)))
}

Model.2.Theta <- function(Model){
  Theta <- c(
    log(Model$sigma.nu),
    inv.logit(Model$phi),
    inv.logit(Model$delta),
    inv.logit(.5*(1 - Model$b/(Model$Gamma * (1 - Model$phi)))),
    Model$Gamma,
    Model$pi.bar,
    inv.logit(Model$psi),
    log(Model$sigma.pi),
    Model$rho.pi,
    log(Model$sigma.y),
    Model$rho.y,
    log(Model$sigma.d),
    Model$rho.d,
    Model$g_c[1],
    log(Model$g_c[2]-Model$g_c[1]),
    log(Model$g_c[3]-Model$g_c[2]),
    inv.logit((Model$p_ll-.01)/.98),
    inv.logit((Model$p_hh-.01)/.98),
    inv.logit((Model$p_ii-.01)/.98),
    inv.logit((Model$p_il/(1-Model$p_ii)-.01)/.98)
  )
  Theta <- matrix(Theta,ncol=1)
  row.names(Theta) <- c("sigma.nu","phi","delta","b","Gamma",
                        "pi.bar","psi","sigma.pi","rho.pi","sigma.y",
                        "rho.y","sigma.d","rho.d","g_cl","g_ci","g_ch",
                        "p_ll","p_hh","p_ii","p_il")
  return(Theta)
}



compute.criteria <- function(Theta.0,
                             vec.maturities, # maturities considered (expressed in number of model periods)
                             nb.values.s,
                             h.stock,
                             curvature,
                             grid.4.S,
                             indic.the.ones.used.in.nom.curve,
                             indic.the.ones.used.in.rea.curve,
                             indic.slope.nom.curve,
                             indic.slope.rea.curve,
                             indic.condVar.nom.rate,
                             indic.condVar.rea.rate,
                             all.targets,
                             all.weights,
                             weight.historical.fit = 0){
  
  Theta <- Full.Theta
  Theta[Filter==1] <- Theta.0
  
  Model <- Theta.2.Model(Theta)
  
  res <- compare.target.and.model(Model,
                                  vec.maturities, # maturities considered (expressed in number of model periods)
                                  nb.values.s,
                                  h.stock,
                                  curvature,
                                  grid.4.S,
                                  indic.the.ones.used.in.nom.curve,
                                  indic.the.ones.used.in.rea.curve,
                                  indic.slope.nom.curve,
                                  indic.slope.rea.curve,
                                  indic.condVar.nom.rate,
                                  indic.condVar.rea.rate)
  
  modelled.moments <- res$modelled.moments
  res.prices       <- res$res.prices
  
  res.estim <-  estimate.latent.factors(res.prices,
                                        dc.US,dy.US,pi.US,
                                        gdp.forecast.4Q,infl.forecast.4Q,
                                        gdp.forecast.8Q,infl.forecast.8Q,
                                        US_yields_q)
  
  loss <- sum((modelled.moments - all.targets)^2/all.weights^2)
  
  loss <- loss + 10^10 * sum((abs(Theta.0)>max.abs.value.params)*(abs(Theta.0)-max.abs.value.params))
  
  loss <- loss - weight.historical.fit * res.estim$logl
  
  return(c(loss))
}

