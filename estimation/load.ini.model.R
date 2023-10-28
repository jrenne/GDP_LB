# ==============================================================================
# This script loads pre-specified parameterizations
# ==============================================================================

FREQ <- 4 # 4 for quarterly

if((indic.estim==0)|(indic.start.from.last==1)){# Load a saved model
  FILE <- paste(getwd(),"/estimation/results/",result.file,".Rdat",sep="")
  load(FILE) # Loads estimated Full.Theta
  Model <- Theta.2.Model(Full.Theta)
}else{# Start from scratch
  Model <- list()
  Model$sigma.nu <- .005419973
  Model$phi      <- .9765
  Model$delta    <- .9961
  Model$b        <- .02
  Model$Gamma    <- 2
  Model$pi.bar   <- .00697
  Model$psi      <- .9808
  Model$sigma.pi <- .000066
  Model$rho.pi   <- .0392
  Model$sigma.y  <- 1.53e-5
  Model$rho.y    <- 1
  Model$d.bar    <- .0055
  Model$sigma.d  <- 3.06e-5
  Model$rho.d    <- 2
  Model$freq     <- 4
  Model$g_c      <- c(-.005,.005,.010001)
  Model$p_ll     <- 0.5
  Model$p_hh     <- .95
  Model$p_ii     <- .85
  Model$p_il     <- .1
  Model$g_d      <- c(NaN,NaN,NaN)
}

Full.Theta <- Model.2.Theta(Model)
Model      <- Theta.2.Model(Full.Theta) # to compute g_d
Model.est  <- Model
