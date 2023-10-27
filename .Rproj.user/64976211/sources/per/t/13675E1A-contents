
issuance.strategy <- function(total.issuance,param){
  # This function defines how the amount "total.issuance" is issued.
  # Three types of bonds are considered:
  # - Nominal bonds
  # - Real bonds (inflation-linked bonds)
  # - nominal-GDP-indexed bonds
  # "param" contains parameters characterizing the issuance strategy.
  # The output is a matrix of dimension H x 3, where H is the maximum maturity.
  
  issuance.matrix <- param$matrix * total.issuance
  
  return(issuance.matrix)
}


simulate.debt <- function(simulated.yields,
                          simulated.inflation,
                          simulated.GDP.growth,
                          simulated.GDP.expectation,
                          ini.issuances,
                          param,freq,
                          indic.compute.bs=1, # by default bs computed to have a constant debt-to-GDP ratio
                          predefined.bs=NaN){
  # "simulated.yields" contains trajectories of yields
  #   simulated.yields$XXXX.bond.yields, with XXX in {rea,nom,GDP} is of
  #   dimension max.matur x (max.matur + nb.periods),
  #   the first max.matur periods of simulation correspond to the initialization period.
  #   They are not expressed in annualized terms but as returns consistent with the model periodicity.
  
  # ini.issuances describes the initial redemption schedule. It describes the issuances
  #   that took place during the initialization period, that are the first
  #   max.matur periods.
  #   ini.debt$XXX.bonds, with XXX in {rea,nom,GDP} is of dimension max.matur x max.matur,
  #   the first column corresponds to the issuances that took place max.matur periods before
  #       the "first" simulation period and so on.
  #   It is expressed in fractions of nominal GDP (at the issuance period).
  
  # simulated.GDP.expectation is of
  #   dimension max.matur x (max.matur + nb.periods),
  #   the first max.matur periods of simulation correspond to the initialization period.
  #   These expectations are not expressed in annualized terms but as growth rates consistent with the model periodicity.
  #   It corresponds to the y_{t-k,t}^a of the note.
  #   Each column corresponds to the term structure of conditional expect for a given date.
  
  # simulated.inflation and simulated.inflation are vectors of length (max.matur + nb.periods),
  #   the first max.matur periods correspond to the initialization period.
  #   They are not expressed in annualized terms but in growth rates at the model periodicity.
  
  # param is a vector of arguments that is passed to function "issuance.strategy".
  
  max.matur  <- dim(simulated.yields$rea.bond.yields)[1]
  nb.periods <- dim(simulated.yields$rea.bond.yields)[2] - max.matur
  
  all.issuances <- ini.issuances
  all.bs <- NULL
  all.iss <- NULL
  all.d   <- NULL
  
  for(t in 1:nb.periods){
    past.rea.yields <- simulated.yields$rea.bond.yields[,(max.matur + t - max.matur):(max.matur + t - 1)]
    past.nom.yields <- simulated.yields$nom.bond.yields[,(max.matur + t - max.matur):(max.matur + t - 1)]
    past.GDP.yields <- simulated.yields$GDP.bond.yields[,(max.matur + t - max.matur):(max.matur + t - 1)]
    
    # In all.issuances$XXX.bonds, the i^th column indicate the issuances of XXX-type of bonds issued on date i.
    past.iss.rea <- all.issuances$rea.bonds[,(max.matur + t - max.matur):(max.matur + t - 1)]
    past.iss.nom <- all.issuances$nom.bonds[,(max.matur + t - max.matur):(max.matur + t - 1)]
    past.iss.GDP <- all.issuances$GDP.bonds[,(max.matur + t - max.matur):(max.matur + t - 1)]
    
    past.GDP.fcsts <- simulated.GDP.expectation[(max.matur + t - max.matur):(max.matur + t - 1)]
    
    # The y_{t-k,t}s:
    cumulated.growth   <- cumsum(simulated.GDP.growth[(max.matur + t):(max.matur + t - max.matur + 1)])
    # first entry of cumulated.growth is growth between t-1 and t, second between t-2 and t, etc
    cumulated.growth_1 <- cumsum(simulated.GDP.growth[(max.matur + t - 1):(max.matur + t - max.matur + 1)])
    cumulated.growth_1 <- c(0,cumulated.growth_1)
    
    # The \pi_{t-k,t}s:
    cumulated.inflation   <- cumsum(simulated.inflation[(max.matur + t):(max.matur + t - max.matur + 1)])
    # first entry of cumulated.inflation is inflation between t-1 and t, second between t-2 and t, etc
    cumulated.inflation_1 <- cumsum(simulated.inflation[(max.matur + t - 1):(max.matur + t - max.matur + 1)])
    cumulated.inflation_1 <- c(0,cumulated.inflation_1)
    
    matrix.cumulated.growth      <- matrix(1,max.matur,1) %*% matrix(cumulated.growth[max.matur:1],nrow=1)
    # all lines are the same. First entry is growth between (t - max.matur + 1) and t, second between (t - max.matur + 2) and t, etc.
    matrix.cumulated.growth_1    <- matrix(1,max.matur,1) %*% matrix(cumulated.growth_1[max.matur:1],nrow=1)
    matrix.cumulated.inflation   <- matrix(1,max.matur,1) %*% matrix(cumulated.inflation[max.matur:1],nrow=1)
    matrix.cumulated.inflation_1 <- matrix(1,max.matur,1) %*% matrix(cumulated.inflation_1[max.matur:1],nrow=1)
    
    matrix.k.1   <- t(matrix(max.matur:1,max.matur,max.matur)) # each row is max.matur:1
    matrix.k_1.0 <- t(matrix((max.matur-1):0,max.matur,max.matur)) # each row is (max.matur-1):0
    
    # ====================================================
    # Computation of stabilizing budget surplus:
    
    nom.bonds.accru <-
      exp(matrix.k.1 * past.nom.yields - matrix.cumulated.growth - matrix.cumulated.inflation)
    nom.bonds.accru_1 <-
      exp(matrix.k_1.0 * past.nom.yields - matrix.cumulated.growth_1 - matrix.cumulated.inflation_1)
    
    rea.bonds.accru <-
      exp(matrix.k.1 * past.rea.yields - matrix.cumulated.growth)
    rea.bonds.accru_1 <-
      exp(matrix.k_1.0 * past.rea.yields - matrix.cumulated.growth_1)
    
    GDP.bonds.accru <-
      exp(matrix.k.1 * past.GDP.yields - matrix.k.1 * past.GDP.fcsts)
    GDP.bonds.accru_1 <-
      exp(matrix.k_1.0 * past.GDP.yields - matrix.k_1.0 * past.GDP.fcsts)
    
    mask <- ((!upper.tri(past.iss.nom))*1)[,max.matur:1] # This will be used to remove already-matured bonds.
    
    if(indic.compute.bs==1){
      bs.t <- sum( past.iss.nom * (nom.bonds.accru - nom.bonds.accru_1) * mask +
                     past.iss.rea * (rea.bonds.accru - rea.bonds.accru_1) * mask +
                     past.iss.GDP * (GDP.bonds.accru - GDP.bonds.accru_1) * mask)
      # Note the first column of past.iss.XXX corresponds to bonds issued max.matur periods ago.
      # The first line correspond to one-period bonds, second line to 2-period bonds, etc.
    }else{
      if(is.na(predefined.bs[1])){
        print("Please provide a series for budget surplus (predefined.bs)")
        return(0)
      }else{
        bs.t <- predefined.bs[t]
      }
    }
    
    all.bs <- c(all.bs,bs.t)
    all.d  <- c(all.d,sum(past.iss.nom * nom.bonds.accru * mask +
                            past.iss.rea * rea.bonds.accru * mask +
                            past.iss.GDP * GDP.bonds.accru * mask) - bs.t)
    
    # ====================================================
    # Computation of date-t issuance:
    
    maturing.nom.bonds <- diag((past.iss.nom * nom.bonds.accru)[,max.matur:1])
    maturing.rea.bonds <- diag((past.iss.rea * rea.bonds.accru)[,max.matur:1])
    maturing.GDP.bonds <- diag((past.iss.GDP * GDP.bonds.accru)[,max.matur:1])
    
    iss.t <- - bs.t + sum(maturing.nom.bonds + maturing.rea.bonds + maturing.GDP.bonds)
    
    all.iss <- c(all.iss,iss.t)
    
    new.issuances <- issuance.strategy(iss.t,param)
    # new.issuances[,1] contains the proceeds of issuances of nominal bonds,
    #    the first entry correspond to 1-period bonds, second to 2-period bonds etc.
    # new.issuances[,2] contains the proceeds of issuances of ILBs,
    #    the first entry correspond to 1-period bonds, second to 2-period bonds etc.
    # new.issuances[,3] contains the proceeds of issuances of GDP-LBs,
    #    the first entry correspond to 1-period bonds, second to 2-period bonds etc.
    
    # update matrix describing issuances of all dates:
    all.issuances$nom.bonds <- cbind(all.issuances$nom.bonds,new.issuances[,1])
    all.issuances$rea.bonds <- cbind(all.issuances$rea.bonds,new.issuances[,2])
    all.issuances$GDP.bonds <- cbind(all.issuances$GDP.bonds,new.issuances[,3])
  }
  all.bs  <- c(rep(NaN,max.matur),all.bs)
  all.d   <- c(rep(NaN,max.matur),all.d)
  all.iss <- c(rep(NaN,max.matur),all.iss)
  
  # Compute annual values (rolling windows)
  Real.GDP  <- exp(cumsum(simulated.GDP.growth))
  Price.index <- exp(cumsum(simulated.inflation))
  Nominal.GDP <- Real.GDP * Price.index
  
  all.BS <- all.bs * Nominal.GDP
  # Compute debt service (model frequency):
  all.D <- all.d * Nominal.GDP
  all.DS <- c(NaN,all.D[2:length(all.D)] - all.D[1:(length(all.D)-1)]) + all.BS
  
  Rolling.nominal.GDP <- Nominal.GDP
  Rolling.BS <- all.BS
  Rolling.DS <- all.DS
  for(i in 1:(freq-1)){
    Rolling.nominal.GDP[freq:(max.matur+nb.periods)] <- 
      Rolling.nominal.GDP[freq:(max.matur+nb.periods)] +
      Nominal.GDP[(freq-i):(max.matur+nb.periods-i)]
    
    Rolling.BS[freq:(max.matur+nb.periods)] <- 
      Rolling.BS[freq:(max.matur+nb.periods)] +
      all.BS[(freq-i):(max.matur+nb.periods-i)]
    
    Rolling.DS[freq:(max.matur+nb.periods)] <- 
      Rolling.DS[freq:(max.matur+nb.periods)] +
      all.DS[(freq-i):(max.matur+nb.periods-i)]
  }
  
  Rolling.bs <- Rolling.BS / Rolling.nominal.GDP
  Rolling.ds <- Rolling.DS / Rolling.nominal.GDP
  
  
  return(list(
    all.issuances = all.issuances,
    all.bs = all.bs,
    all.d = all.d,
    all.iss = all.iss,
    Nominal.GDP = Nominal.GDP,
    Real.GDP = Real.GDP,
    Rolling.nominal.GDP = Rolling.nominal.GDP,
    Price.index = Price.index,
    Rolling.BS = Rolling.BS,
    Rolling.bs = Rolling.bs,
    Rolling.DS = Rolling.DS,
    Rolling.ds = Rolling.ds
  ))
}


quantile.density <-
  function (x, probs = seq(0.25, 0.75, 0.25), names = TRUE, normalize = TRUE, 
            ...) 
  {
    my.quantile.density = function(x, probs, names, normalize, 
                                   ...) {
      ycs = (cumsum(x$y) - (x$y - x$y[[1]])/2) * diff(x$x[1:2])
      if (normalize) 
        ycs = ycs/(ycs[[length(ycs)]])
      xin = x$x
      maxi = length(ycs)
      qqs = sapply(as.list(probs), function(qu) {
        iii = sum(ycs <= qu)
        if (iii == maxi) 
          return(Inf)
        else if (iii == 0L) 
          return(-Inf)
        else {
          return(xin[[iii + 1]] + ((ycs[[iii + 1]] - qu)/(ycs[[iii + 
                                                                 1]] - ycs[[iii]])) * (xin[[iii]] - xin[[iii + 
                                                                                                           1]]))
        }
      })
      if (as.logical(names)) 
        names(qqs) = paste(format(100 * probs, trim = TRUE, 
                                  digits = max(2L, getOption("digits"))), "%", 
                           sep = "")
      return(qqs)
    }
    probs = as.vector(probs)
    if (is.element("density", class(x))) 
      return(my.quantile.density(x = x, probs = probs, names = names, 
                                 normalize = normalize))
    if (!all(sapply(x, function(dd) is.element("density", class(dd))))) 
      stop("x needs to be a density or list of densities")
    if (length(x) == 1L) 
      return(my.quantile.density(x = x[[1]], probs = probs, 
                                 names = names, normalize = normalize))
    qout = sapply(x, my.quantile.density, probs = probs, names = FALSE, 
                  normalize = normalize)
    if (!is.matrix(qout)) {
      if (length(probs) > 1) 
        return(qout)
      qout = as.matrix(qout)
    }
    else qout = t(qout)
    if (as.logical(names)) 
      colnames(qout) = paste(format(100 * probs, trim = TRUE, 
                                    digits = max(2L, getOption("digits"))), "%", sep = "")
    return(qout)
  }

