# ======================================================
# ======================================================
# Set of procedures for Consumption-habit model
# ======================================================
# ======================================================


simul.model <- function(Model,nb.sim,ini.growth.reg=1){
  
  S.bar <- Model$sigma.nu * sqrt(Model$Gamma / (1 - Model$phi - Model$b/Model$Gamma))
  s.bar <- log(S.bar)
  s.max <- s.bar + .5*(1-S.bar^2)
  
  nu <- rnorm(nb.sim)*Model$sigma.nu
  
  P_c <- matrix(0,3,3)
  P_c[1,1] <- Model$p_ll
  P_c[1,2] <- 1-Model$p_ll
  P_c[3,3] <- Model$p_hh
  P_c[3,2] <- 1-Model$p_hh
  P_c[2,2] <- Model$p_ii
  P_c[2,1] <- Model$p_il
  P_c[2,3] <- 1 - Model$p_ii - Model$p_il
  
  simul.z_c <- simul.z(P_c,ini.state=ini.growth.reg,nb.sim)$all.z.integer
  
  simul.s <- NULL
  simul.delta.c <- NULL
  s <- s.bar
  delta.c <- Model$g_c[ini.growth.reg]
  
  for(t in 1:nb.sim){
    lambda.s <- 1/S.bar * sqrt(1 - 2*(s - s.bar)) - 1
    
    delta.c <- Model$g_c[simul.z_c[t]] + nu[t]
    
    s <- min((1 - Model$phi)*s.bar + Model$phi*s +
               lambda.s * (nu[t] - Model$eta * bernoulli[t] + Model$p * Model$eta),s.max)
    simul.s       <- c(simul.s,s)
    simul.delta.c <- c(simul.delta.c,delta.c)
  }
  return(list(simul.s   = simul.s,
              simul.z_c = simul.z_c,
              lambda.s  = lambda.s,
              bernoulli = bernoulli,
              simul.delta.c = simul.delta.c
  ))
}

simul.z <- function(P,ini.state,T,nb.of.periods.with.ini.state=1){
  # This function simulates a Markov chain whose
  # transition probabilities are in P, specifically,
  # the (i,j) entry of P is the proba to switch from state i to state j.
  # "ini.state" indicates in which state the chain begins (on date t=1). 
  #      ini.state is an integer that is lower than the dimension of P
  # if nb.of.periods.with.ini.state>1 then ini.state replicated nb.of.periods.with.ini.state times
  
  N <- dim(P)[1]
  
  z.integer <- ini.state
  all.z.integer <- rep(z.integer,nb.of.periods.with.ini.state)
  all.z <- matrix(0,N,nb.of.periods.with.ini.state)
  all.z[z.integer,] <- 1
  
  u <- runif(T-1)
  for(i in 1:(T-nb.of.periods.with.ini.state)){
    vector.of.cum.proba <- cumsum(P[z.integer,])
    aux <- u[i] - vector.of.cum.proba
    negative.aux <- which(aux<0)
    z.integer <- negative.aux[1]
    all.z.integer <- c(all.z.integer,z.integer)
    aux.z <- matrix(0,N,1)
    aux.z[z.integer] <- 1
    all.z <- cbind(all.z,aux.z)
  }
  
  return(list(
    all.z.integer = all.z.integer,
    all.z = all.z
  ))
}

simul.model.solved <- function(res.prices,T,
                               nb.of.periods.with.ini.state=1,
                               ini.state=NaN){
  # This function takes the output of "compute.prices" as an input
  # T is the number of periods on which it is simulated
  # If ini.state is NaN, then the initial state is randomly selected (drawn in the stationary distri. of states).
  # if nb.of.periods.with.ini.state>1, then the initial state is replicated nb.of.periods.with.ini.state times.
  # Simulated yields are not annualized but consistent with the model periodicity.
  
  N <- dim(res.prices$P)[1]
  
  res.mom.pi.z <- compute.uncond.mom.pi.z(res.prices$Model.solved)
  
  if(is.na(ini.state)){
    # ini.state <- which(res.mom.pi.z$E.z==max(res.mom.pi.z$E.z))[1] # most likely state
    # Dray inittial state in stationary distribution of states:
    u <- runif(1)
    ini.state <- which(u<cumsum(res.mom.pi.z$E.z))[1]
  }
  sim.z     <- simul.z(res.prices$Model.solved$P,ini.state,T+1,nb.of.periods.with.ini.state+1)
  # WARNING: first date is for date 0
  
  # Compute associated series of nu_t shocks:
  mu     <- matrix(1,length(Model$g_c),1) %x% matrix(res.prices$values.s,ncol=1)
  lambda <- matrix(1,length(Model$g_c),1) %x% matrix(res.prices$Model.solved$lambda.s,ncol=1)
  phi <- res.prices$Model.solved$phi
  s.bar <- res.prices$Model.solved$s.bar
  B <- matrix(1/lambda,ncol=1) %*% matrix(mu,nrow=1) -
    (1 - phi) * s.bar * matrix(1/lambda,N,N) -
    phi * matrix(mu/lambda,ncol=1) %*% matrix(1,1,N)
  
  z_t_1.integer <- sim.z$all.z.integer[1:T]
  z_t.integer   <- sim.z$all.z.integer[2:(T+1)]
  indices.nu.in.B <- z_t_1.integer + N*(z_t.integer - 1)
  nu <- B[indices.nu.in.B] # 0 is added for nu to be of dimension T.
  
  sim.y  <- res.prices$Model.solved$g[z_t_1.integer] + 
    res.prices$Model.solved$rho.y * nu +
    res.prices$Model.solved$sigma.y * rnorm(T)
  sim.d  <- res.prices$Model.solved$gdtilde[z_t_1.integer] + 
    res.prices$Model.solved$rho.d * nu +
    res.prices$Model.solved$sigma.d * rnorm(T)
  
  sim.y[1:nb.of.periods.with.ini.state] <- sim.y[1]
  sim.d[1:nb.of.periods.with.ini.state] <- sim.d[1]
  
  pi.t   <- res.prices$Model.solved$pi.bar
  sim.pi <- rep(pi.t,nb.of.periods.with.ini.state)
  
  # inflation simulation:
  for(t in (nb.of.periods.with.ini.state+1):T){
    pi.t <- (1 - res.prices$Model.solved$psi) * res.prices$Model.solved$pi.bar +
      res.prices$Model.solved$psi * pi.t +
      res.prices$Model.solved$rho.pi * nu[t] +
      res.prices$Model.solved$sigma.pi * rnorm(1)
    sim.pi <- c(sim.pi,pi.t)
  }
  
  vec.maturities <- 1:length(c(res.prices$all.nom.bond.b))
  
  # ===============================================
  # Nominal yields:
  res.prices$all.nom.bond.F[res.prices$all.nom.bond.F<=0] <- 10^(-50)
  nom.yields <- 1/vec.maturities * (
    matrix(res.prices$all.nom.bond.b,ncol=1) %*% matrix(sim.pi,nrow=1) - 
      log(res.prices$all.nom.bond.F %*% sim.z$all.z[,2:(T+1)])
  )
  
  # ===============================================
  # Real yields:
  rea.yields <- 1/vec.maturities * (
    - log(res.prices$all.rea.bond.prices %*% sim.z$all.z[,2:(T+1)])
  )
  
  # ===============================================
  # GDP-indexed bond yields:
  GDP.yields <- 1/vec.maturities * (
    - log(res.prices$all.GDP.bond.prices %*% sim.z$all.z[,2:(T+1)])
  )
  
  # ===============================================
  # GDP forecasts:
  GDP.fcsts <- 1/vec.maturities * (
    log(res.prices$all.GDP.forecasts %*% sim.z$all.z[,2:(T+1)]))
  
  return(list(
    sim.z = sim.z$all.z,
    sim.z.integer = sim.z$all.z.integer,
    sim.nu = nu,
    sim.y = sim.y,
    sim.d = sim.d,
    sim.pi = sim.pi,
    nom.yields = nom.yields,
    rea.yields = rea.yields,
    GDP.yields = GDP.yields,
    GDP.fcsts = GDP.fcsts
  ))
}


cdf.c.innov <- function(q,arguments){
  if(length(arguments)==1){
    # In this case, growth ~ N(0,arguments[1]^2)
    proba <- pnorm(q/arguments[1])
  }else{# Mixture of Gaussian distribution
    # In this case, Delta c_t ~ (1-B)*N(mu * p/(1-p),arguments[1]^2) + B*N(-mu,arguments[1]^2)
    #  with mu=argument[2] and p=argument[3] in [0,1] 
    # and B i.i.d. Bernoulli of parameter p
    sigma <- arguments[1]
    mu <- arguments[2]
    p <- arguments[3]
    proba <- (1-p)*pnorm((q-mu*p/(1-p))/arguments[1]) + p*pnorm((q+mu)/arguments[1])
  }
  return(proba)
}


f.4.make.S.values <- function(x,curvature){
  return(exp(-curvature * x * (x-1)))
}

make.S.values <- function(S.max,curvature=1,nb.values.s){
  min.dev <- 1/nb.values.s
  step <- (1-min.dev)/nb.values.s
  aux.values <- seq(.5*min.dev,1-.5*min.dev,by=step)
  aux.steps <- f.4.make.S.values(aux.values,curvature)
  sum.steps <- sum(aux.steps)
  steps <- S.max * (aux.steps/sum.steps)
  values.S <- cumsum(steps)[1:nb.values.s]
  return(values.S)
}


compute.prices <- function(Model,
                           vec.maturities, # maturities considered (expressed in number of model periods)
                           nb.values.s = NB.values.s,
                           h.stock=1,
                           curvature=1, # this controls the dispersion of S values in the grid (the higher this parameter, the larger the number of values XXX)
                           grid.4.S = NaN, # If it is non NaN, then these values are used as a grid for S
                           indic.compute.TP = 0 # if 1, then computation of yields "under P" to comoute term premiums
){
  # This function computes various prices and yields.
  # Importantly, it also derives Pi, the matrix of transition probabilities.
  # Yields output are annualized.
  
  N <- nb.values.s * length(Model$g_c)
  vec1 <- matrix(1,N,1)
  
  S.bar <- sqrt(Model$sigma.nu^2) * sqrt(Model$Gamma / (1 - Model$phi - Model$b/Model$Gamma))
  s.bar <- log(S.bar)
  s.max <- s.bar + .5*(1-S.bar^2)
  S.max <- exp(s.max)
  
  Model.solved <- Model
  Model.solved$s.max <- s.max
  Model.solved$S.max <- S.max
  Model.solved$s.bar <- s.bar
  Model.solved$S.bar <- S.bar
  
  g <- matrix(Model$g_c,ncol=1) %x% matrix(1,nb.values.s,1)
  Model.solved$g <- g
  
  if(is.na(grid.4.S[1])){
    min.dev <- S.max/nb.values.s
    step <- (S.max-min.dev)/(nb.values.s-1)
    values.S <- make.S.values(S.max,curvature,nb.values.s)
  }else{
    values.S <- grid.4.S
    nb.values.s <- length(values.S)
  }
  
  values.s <- log(values.S)
  Model.solved$values.s <- values.s
  Model.solved$nb.values.s <- length(Model.solved$values.s)
  
  # Look for s.bar:
  deviation <- (s.bar - values.s)^2
  index.s.bar <- which(deviation==min(deviation))
  
  lambda.s <- pmax(1/S.bar * sqrt(1 - 2*(values.s - s.bar)) - 1,0)
  
  Model.solved$lambda.s <- lambda.s
  
  # ======================================================================================
  # Computation of the matrix of transition probabilities
  
  values.s.ext <- c(-40,values.s,40)
  X1 <- (matrix(1/lambda.s,ncol=1) %*% matrix(1,1,nb.values.s))*
    (
      .5 * matrix(1,nb.values.s,1) %*% matrix(values.s.ext[3:(nb.values.s+2)]+
                                                values.s.ext[2:(nb.values.s+1)],nrow=1) -
        (1 - Model$phi) * s.bar - Model$phi * matrix(values.s,ncol=1) %*% matrix(1,1,nb.values.s)
    )
  X2 <- (matrix(1/lambda.s,ncol=1) %*% matrix(1,1,nb.values.s))*
    (
      .5 * matrix(1,nb.values.s,1) %*% matrix(values.s.ext[2:(nb.values.s+1)]+
                                                values.s.ext[1:(nb.values.s)],nrow=1) -
        (1 - Model$phi) * s.bar - Model$phi * matrix(values.s,ncol=1) %*% matrix(1,1,nb.values.s)
    )
  
  arguments <- c(Model$sigma.nu)
  # arguments <- c(Model$sigma.nu,
  #                Model$eta,
  #                Model$p)
  P_s <- cdf.c.innov(X1,arguments) - cdf.c.innov(X2,arguments)
  
  P_c <- matrix(0,3,3)
  P_c[1,1] <- Model$p_ll
  P_c[1,2] <- 1-Model$p_ll
  P_c[3,3] <- Model$p_hh
  P_c[3,2] <- 1-Model$p_hh
  P_c[2,2] <- Model$p_ii
  P_c[2,1] <- Model$p_il
  P_c[2,3] <- 1 - Model$p_ii - Model$p_il
  P <- P_c %x% P_s
  
  mu     <- matrix(1,length(Model$g_c),1) %x% matrix(values.s,ncol=1)
  lambda <- matrix(1,length(Model$g_c),1) %x% matrix(lambda.s,ncol=1)
  Model.solved$mu     <- mu
  Model.solved$lambda <- lambda
  
  Q.num <- exp(-Model$Gamma * (1 + 1/lambda) %*% t(mu))
  Q.num[Q.num==Inf] <- 10^300
  Q.denom <- (Q.num * P) %*% matrix(1,N,N)
  Q <- (Q.num/Q.denom) * P
  
  matrix.M.on.E.M <- Q.num/Q.denom
  
  Model.solved$P <- P
  Model.solved$Q <- Q
  
  
  # ======================================================================================
  # Computation of short-term real rates
  
  M.AUX <- - Model$Gamma * (1 + 1/lambda) %*% t(mu)
  M.AUX[M.AUX>700] <- 700
  
  mu.r <- - log(Model$delta) + Model$Gamma * g - Model$Gamma * mu -
    Model$Gamma * (1 - Model$phi) * s.bar / lambda - Model$Gamma * Model$phi * mu / lambda -
    log(apply(P * exp(M.AUX),1,sum))
  mu.r <- matrix(mu.r,ncol=1)
  # **** Checkings:
  # Closed-form formula when Gaussian shocks (p=0 or eta=0):
  mu.r.1 <- - log(Model$delta) + Model$Gamma * g - .5*(Model$Gamma * (1 - Model$phi) - Model$b) +
    Model$b * (s.bar - values.s)
  # Alternative formula (with function F):
  F1 <- function.Fh(-Model$Gamma*mu,-Model$Gamma,lambda,mu,P,Model$phi,s.bar,max.h=1)
  mu.r.2 <- - log(Model$delta) + Model$Gamma * g  - Model$Gamma * mu - t(log(F1))
  
  Model.solved$mu.r <- mu.r
  
  # Compute Maximum Sharpe ratio
  A <- matrix(exp(-mu.r),N,N) * matrix.M.on.E.M
  cond.var.SDF <- diag(P %*% (A * A) - (P %*% A) * (P %*% A))
  cond.max.Sharpe.ratios <- sqrt(cond.var.SDF) / exp(-mu.r)
  
  # # Alternative ILB:
  # real.bond.prices.FFF <- function.Ftildeh(-matrix(mu.r,ncol=1),0,lambda,mu,Q,Model$phi,s.bar,
  #                                          max.h=max(vec.maturities))
  # # Certain one for k = 2:
  # exact <- c(exp(-mu.r) * (t(vec1) %*% diag(exp(-mu.r)) %*% t(Q)))
  
  
  
  # ======================================================================================
  # Computation of bond prices and rates
  
  all.rea.bond.prices   <- NULL
  all.rea.bond.yields   <- NULL
  all.nom.bond.F        <- NULL
  all.nom.bond.b        <- NULL
  all.GDP.bond.prices   <- NULL
  all.GDP.bond.yields   <- NULL
  all.nom.bond.F.yields <- NULL
  all.nom.bond.b.yields <- NULL
  all.GDP.forecasts     <- NULL
  
  if(indic.compute.TP==0){
    matrix.QorP <- Q
  }else{
    matrix.QorP <- P
  }

  # For real bonds (initialization):
  Theta.tilde._mu_r <- compute.Theta.tilde(u=-mu.r,w=0,
                                           lambda=lambda,mu=mu,
                                           matrix.QorP,
                                           Model$phi,s.bar)
  Theta.tilde._mu_r.h <- diag(N) # initialization
  
  # For GDP-indexed bonds (initialization):
  Theta.tilde.dy.P <- compute.Theta.tilde(u=g,
                                          w=Model$rho.y,
                                          lambda=lambda,mu=mu,
                                          P,Model$phi,s.bar)
  Theta.tilde.dy_mur.Q <- compute.Theta.tilde(u=g-mu.r,
                                              w=Model$rho.y,
                                              lambda=lambda,mu=mu,
                                              matrix.QorP,
                                              Model$phi,s.bar)
  Theta.tilde.dy.P.h     <- diag(N)
  Theta.tilde.dy_mur.Q.h <- diag(N)
  
  # For nominal bonds (initialization):
  b.h_1 <- 0
  F.nom <- matrix(1,N,1)
  
  count <- 0
  for(h in 1:max(vec.maturities)){
    
    # Real bonds: ---------------------------------------------------------
    Theta.tilde._mu_r.h <- Theta.tilde._mu_r.h %*% Theta.tilde._mu_r
    F.rea <- Theta.tilde._mu_r.h %*% vec1
    
    # GDP.indexed bonds: ---------------------------------------------------------
    Theta.tilde.dy.P.h     <- Theta.tilde.dy.P.h %*% Theta.tilde.dy.P
    Theta.tilde.dy_mur.Q.h <- Theta.tilde.dy_mur.Q.h %*% Theta.tilde.dy_mur.Q
    F.GDP <- apply(Theta.tilde.dy_mur.Q.h,1,sum) /
      apply(Theta.tilde.dy.P.h,1,sum)
    
    # Nominal bonds: ---------------------------------------------------------
    F.nom <- exp(-(b.h_1+1)*Model$pi.bar*(1 - Model$psi) + .5 * (b.h_1+1)^2 * Model$sigma.pi^2) *
      (exp(
        matrix(- mu.r + Model$rho.pi * (b.h_1+1) * ((1 - Model$phi) * s.bar / lambda + Model$phi * mu / lambda),
               N,N) - Model$rho.pi * (b.h_1+1) *
          (1/lambda) %*% t(mu)
      ) * matrix.QorP) %*% F.nom
    b.h_1 <- (1 + b.h_1) * Model$psi
    
    if(sum(h==vec.maturities)>0){
      count <- count + 1
      all.rea.bond.prices <- rbind(all.rea.bond.prices,c(F.rea))
      all.GDP.bond.prices <- rbind(all.GDP.bond.prices,c(F.GDP))
      
      all.rea.bond.yields <- rbind(all.rea.bond.yields,
                                   c(-Model$freq/vec.maturities[count] * log(F.rea)))
      all.GDP.bond.yields <- rbind(all.GDP.bond.yields,
                                   c(-Model$freq/vec.maturities[count] * log(F.GDP)))
      
      all.nom.bond.F      <- rbind(all.nom.bond.F,c(F.nom))
      all.nom.bond.b      <- rbind(all.nom.bond.b,b.h_1)
      
      all.nom.bond.F.yields <- rbind(all.nom.bond.F.yields,
                                     c(-Model$freq/vec.maturities[count] * log(F.nom)))
      all.nom.bond.b.yields <- rbind(all.nom.bond.b.yields,
                                     c(Model$freq/vec.maturities[count] * b.h_1))
      all.GDP.forecasts <- rbind(all.GDP.forecasts,
                                   apply(Theta.tilde.dy.P.h,1,sum))
    }
  }

  # ======================================================================================
  # Computation of stock returns
  
  gdtilde <- matrix(Model$g_d %x% matrix(1,nb.values.s,1),ncol=1)
  Model.solved$gdtilde <- gdtilde
  
  J <- exp(.5 * Model$sigma.d^2) * 
    compute.Theta.tilde(u=gdtilde - mu.r,
                        w = Model$rho.d,
                        lambda=lambda,mu=mu,
                        matrix.QorP,Model$phi,s.bar)
  
  Chi <- solve(diag(N) - J) %*% J %*% matrix(1,N,1)
  
  # M_old <- exp(Model$d.bar + Model$sigma.d^2/2) *
  #   exp(
  #     matrix(- (1 - Model$phi) * s.bar / lambda.s - Model$phi * values.s / lambda.s,
  #            nb.values.s,nb.values.s) +
  #       matrix(1/lambda.s,ncol=1) %*% matrix(values.s,nrow=1)) *
  #   (matrix(1/Chi,nb.values.s,1) %*% matrix(1 + Chi,1,nb.values.s))
  
  
  M <- exp(Model$sigma.d^2/2) *
    exp(matrix(Model$rho.d/lambda,ncol=1) %*% matrix(mu,nrow=1) + Model$rho.d *
          matrix(- (1 - Model$phi) * s.bar / lambda - Model$phi * mu / lambda,N,1) %*% t(vec1) +
          gdtilde %*% t(vec1)) *
    (matrix(1/Chi,ncol=1) %*% matrix(1 + Chi,nrow=1))
  
  Exp.stock.return <- (M * P) %*% matrix(1,N,1) - 1
  CondVar.stock.return <- diag(
    P %*% (M * M) - (P %*% M) * (P %*% M)
  )
  
  # Computation of conditional returns for horizon h.stock:
  
  H.h_1 <- diag(N) # initialization
  V.h_1 <- matrix(0,N,1) # initialization
  
  for(i in 1:h.stock){
    A.h_1 <- M * (matrix(1,N,1) %*% matrix(diag(H.h_1),nrow=1))
    V.h_1 <- diag(
      (M * M * (matrix(1,N,1) %*% matrix(V.h_1,nrow=1))) %*% t(P) +
        P %*% (A.h_1 * A.h_1) - (P %*% A.h_1) * (P %*% A.h_1)
    )
    H.h_1 <- M %*% diag(diag(H.h_1)) %*% t(P)
  }
  
  V.h <- V.h_1
  H.h <- H.h_1 - 1
  
  # prepare stock-return outputs:
  Exp.stock.return.h <- diag(H.h)
  Var.stock.return.h <- exp(h * Model$sigma.d^2) * V.h + 
    (exp(h * Model$sigma.d^2) - 1) * (Exp.stock.return.h * Exp.stock.return.h)
  cond.stock.Sharpe.ratios.h <- Exp.stock.return.h/sqrt(Var.stock.return.h)
  
  return(list(
    Model.solved = Model.solved,
    values.s = values.s,
    values.S = values.S,
    nb.values.s = nb.values.s,
    lambda.s = lambda.s,
    s.bar = s.bar,
    index.s.bar = index.s.bar,
    J = J,
    Chi = Chi,
    Exp.stock.return = Exp.stock.return,
    CondVar.stock.return = CondVar.stock.return,
    Exp.stock.return.h = Exp.stock.return.h,
    Var.stock.return.h = Var.stock.return.h,
    cond.var.SDF = cond.var.SDF,
    cond.stock.Sharpe.ratios.h = cond.stock.Sharpe.ratios.h,
    P = P,
    Q = Q,
    M = M,
    matrix.M.on.E.M = matrix.M.on.E.M,
    cond.max.Sharpe.ratios = cond.max.Sharpe.ratios,
    A=A,
    mu.r = mu.r,
    mu.r.1 = mu.r.1,
    mu.r.2 = mu.r.2,
    all.rea.bond.prices = all.rea.bond.prices,
    all.rea.bond.yields = all.rea.bond.yields,
    all.GDP.bond.prices = all.GDP.bond.prices,
    all.GDP.bond.yields = all.GDP.bond.yields,
    all.nom.bond.F = all.nom.bond.F,
    all.nom.bond.b = all.nom.bond.b,
    all.nom.bond.F.yields = all.nom.bond.F.yields,
    all.nom.bond.b.yields = all.nom.bond.b.yields,
    all.GDP.forecasts = all.GDP.forecasts,
    vec.maturities = vec.maturities
  ))
}

compute.Theta <- function(u,w,lambda,mu,P,phi,s.bar){
  # u, mu, Lambda are vectors of dimension N x 1.
  N <- dim(mu)[1]
  vec1 <- matrix(1,N,1)
  Theta <- exp(-((1-phi)*w*s.bar/lambda+w*phi*mu/lambda) %*% t(vec1) +
                 w * (1/lambda) %*% t(mu) + vec1 %*% t(u)) * P
  return(Theta)
}

compute.Theta.tilde <- function(u,w,lambda,mu,P,phi,s.bar){
  # u, mu, Lambda are vectors of dimension N x 1.
  N <- dim(mu)[1]
  vec1 <- matrix(1,N,1)
  Theta.tilde <- diag(c(exp(u-((1-phi)*w*s.bar/lambda + w*phi*mu/lambda)))) %*%
    (exp(w * (1/lambda) %*% t(mu)) * P)
  return(Theta.tilde)
}

function.Fh <- function(u,w,lambda,mu,P,phi,s.bar,max.h){
  # u, mu, Lambda are vectors of dimension N x 1.
  N <- dim(mu)[1]
  vec1 <- matrix(1,N,1)
  Theta <- compute.Theta(u,w,lambda,mu,P,phi,s.bar)
  Theta.h <- diag(N)
  all.F <- matrix(NaN,max.h,N)
  for(h in 1:max.h){
    Theta.h <- Theta.h %*% Theta
    all.F[h,] <- Theta.h %*% vec1
  }
  return(all.F)
}

function.Ftildeh <- function(u,w,lambda,mu,P,phi,s.bar,max.h){
  # u, mu, Lambda are vectors of dimension N x 1.
  N <- dim(mu)[1]
  vec1 <- matrix(1,N,1)
  Theta.tilde <- compute.Theta.tilde(u,w,lambda,mu,P,phi,s.bar)
  Theta.h <- diag(N)
  all.F <- matrix(NaN,max.h,N)
  for(h in 1:max.h){
    Theta.h <- Theta.h %*% Theta.tilde
    all.F[h,] <- Theta.h %*% vec1
  }
  return(all.F)
}

# function.Ftildeh <- function(u,w,lambda,mu,P,phi,s.bar,max.h){
#   # u, mu, Lambda are vectors of dimension N x 1.
#   N <- dim(mu)[1]
#   vec1 <- matrix(1,N,1)
#   Theta.tilde <- compute.Theta.tilde(u,w,lambda,mu,P,phi,s.bar)
#   all.F <- matrix(NaN,max.h,N)
#   F.h <- vec1
#   for(h in 1:max.h){
#     F.h <- Theta.tilde %*% F.h
#     all.F[h,] <- F.h
#   }
#   return(all.F)
# }



simul.debt <- function(r.series,
                       r.gdp.series,past.issuances,
                       y.a.series,y.series){
  # past.issuances is a (h x 1) vector whose components are iss_{-h+1},iss_{-h+2},...iss_{0}.
  # The first h values of r.series, y.series etc correspond to periods t = -h+1,...,-1,0.
  # y.series contains values of log(Y_t/Y_{t-1})
  
  h <- length(past.issuances)
  T <- length(r.series)
  
  all.issuances <- past.issuances
  all.s <- rep(NaN,h)
  all.d <- rep(NaN,h)
  
  all.issuances.star <- past.issuances
  all.s.star <- rep(NaN,h)
  
  past.issuances.t_1 <- past.issuances
  past.issuances.star.t_1 <- past.issuances
  
  for(t in (h+1):T){
    vec.y.t_h.t <- cumsum(y.series[t:(t-h+1)])[h:1]
    vec.y.t_h.t_1 <- vec.y.t_h.t - vec.y.t_h.t[h]
    
    vec.t   <- exp((h:1)    *r.series[(t-h):(t-1)] - vec.y.t_h.t)
    vec.t_1 <- exp(((h-1):0)*r.series[(t-h):(t-1)] - vec.y.t_h.t_1)
    
    s.t <- sum((vec.t - vec.t_1) * past.issuances.t_1)
    iss.t <- past.issuances.t_1[1] * exp(h*r.series[t-h] - vec.y.t_h.t[1]) - s.t
    
    all.s <- c(all.s,s.t)
    all.issuances <- c(all.issuances,iss.t)
    
    
    d.t <- sum(vec.t * past.issuances.t_1) - s.t
    all.d <- c(all.d,d.t)
    
    
    if(h>1){
      past.issuances.t_1 <- c(past.issuances.t_1[2:h],iss.t)
    }else{
      past.issuances.t_1 <- c(iss.t)
    }
    
    
    vec.y.a.t_h.t <- cumsum(y.a.series[t:(t-h+1)])[h:1]
    vec.y.a.t_h.t_1 <- vec.y.a.t_h.t - vec.y.a.t_h.t[h]
    
    vec.star.t   <- exp((h:1)    *r.gdp.series[(t-h):(t-1)] - vec.y.a.t_h.t)
    vec.star.t_1 <- exp(((h-1):0)*r.gdp.series[(t-h):(t-1)] - vec.y.a.t_h.t_1)
    
    s.t.star <- sum((vec.star.t - vec.star.t_1) * past.issuances.star.t_1)
    iss.t.star <- past.issuances.star.t_1[1] * exp(h*r.gdp.series[t-h] - vec.y.a.t_h.t[1]) - s.t.star
    
    all.s.star <- c(all.s.star,s.t.star)
    all.issuances.star <- c(all.issuances.star,iss.t.star)
    
    if(h>1){
      past.issuances.star.t_1 <- c(past.issuances.star.t_1[2:h],iss.t.star)
    }else{
      past.issuances.star.t_1 <- c(iss.t.star)
    }
    
  }
  
  return(list(
    all.s = all.s,
    all.d = all.d,
    all.issuances = all.issuances,
    all.s.star = all.s.star,
    all.issuances.star = all.issuances.star
  ))
}


