
# ===================================================================================
# Estimation of model parameters:

# -------- in "Full.Theta" (parameterization of the model),
#          parameters are organized as follows:

# 1  : sigma.nu  - variance of Gaussian shock in dc
# 2  : phi       - auto-regressive parameter of consumption surplus
# 3  : delta     - preference for present
# 4  : b         - specifies conditional variance of lambda (see Wachter 2006)
# 5  : Gamma     - utility curvature
# 6  : pi.bar    - average inflation rate
# 7  : psi       - auto-regressive parameter of inflation
# 8  : sigma.pi  - std deviation of inflation-specific shock
# 9  : rho.pi    - exposure of inflation to consumption shock
# 10 : sigma.y   - std deviation of output-specific shock
# 11 : rho.y     - exposure of output
# 12 : sigma.d   - std deviation of dividend-specific shock
# 13 : rho.d     - exposure of growth rate of dividends
# 14 : g_cl      - conditional consumption growth in low-growth regime
# 15 : g_ci      - conditional consumption growth in intermediary-growth regime
# 16 : g_ch      - conditional consumption growth in high-growth regime
# 17 : p_ll      - proba to stay in low-growth regime
# 18 : p_hh      - proba to stay in high-growth regime
# 19 : p_ii      - proba to stay in intermed-growth regime
# 20 : p_il      - proba to switch from intermed to low growth regime
# --------------------------------------------------------------------------------

Filter <- 0*Full.Theta

Filter.matrix <- matrix(0,length(Filter),2)
rownames(Filter.matrix) <- rownames(Full.Theta)

# When jj odd number
param.2.estim <- c("phi","delta","b","sigma.y","sigma.d")
Filter.matrix[param.2.estim,1] <- 1
if(indic.estim.distri == 1){
  param.2.estim <- c("g_cl","g_ci","g_ch","p_ll","p_hh","p_ii","p_il")
  Filter.matrix[param.2.estim,1] <- 1
}
if(indic.include.nominal == 1){
  param.2.estim <- c("pi.bar","psi","sigma.pi","rho.pi")
  Filter.matrix[param.2.estim,1] <- 1
}

# When jj even number
param.2.estim <- c("phi","delta","b","sigma.y","sigma.d")
Filter.matrix[param.2.estim,2] <- 1

# Run numerical optimization:
if(indic.estim==1){
  
  for(jj in 0:(nb.loops-1)){
    
    column.Filter.matrix <- (jj %% dim(Filter.matrix)[2]) + 1
    
    Filter <- Filter.matrix[,column.Filter.matrix]
    
    Theta.ini <- Full.Theta[Filter==1]
    
    compute.criteria(Theta.ini,
                     vec.maturities = vec.maturities.4.estimation,
                     nb.values.s = NB.values.s,
                     h.stock = H.stock,
                     curvature = CURVATURE,
                     grid.4.S = NaN,
                     indic.the.ones.used.in.nom.curve = indic.the.ones.used.in.nom.curve,
                     indic.the.ones.used.in.rea.curve = indic.the.ones.used.in.rea.curve,
                     indic.slope.nom.curve = indic.slope.nom.curve,
                     indic.slope.rea.curve = indic.slope.rea.curve,
                     indic.condVar.nom.rate = indic.condVar.nom.rate,
                     indic.condVar.rea.rate = indic.condVar.rea.rate,
                     all.targets = all.targets,
                     all.weights = all.weights,
                     weight.historical.fit = weight.historical.fit)
    
    
    for(i in 1:nb.sub.loops){
      res.estim <- optimx(Theta.ini,compute.criteria,
                          vec.maturities = vec.maturities.4.estimation,
                          nb.values.s = NB.values.s,
                          h.stock = H.stock,
                          curvature = CURVATURE,
                          grid.4.S = NaN,
                          indic.the.ones.used.in.nom.curve = indic.the.ones.used.in.nom.curve,
                          indic.the.ones.used.in.rea.curve = indic.the.ones.used.in.rea.curve,
                          indic.slope.nom.curve = indic.slope.nom.curve,
                          indic.slope.rea.curve = indic.slope.rea.curve,
                          indic.condVar.nom.rate = indic.condVar.nom.rate,
                          indic.condVar.rea.rate = indic.condVar.rea.rate,
                          all.targets = all.targets,
                          all.weights = all.weights,
                          weight.historical.fit = weight.historical.fit,
                          method = "Nelder-Mead",
                          control=list(trace=1, maxit=mult.fact*sum(Filter)*MAXIT.NM, kkt = FALSE))
      if(sum(is.na(as.matrix(res.estim)[1:length(Theta.ini)]))==0){
        Theta.est <- c(as.matrix(res.estim)[1:length(Theta.ini)])
        Theta.ini <- Theta.est
      }
      
      Full.Theta[Filter==1] <- Theta.est
      
      res.estim <- optimx(Theta.ini,compute.criteria,
                          vec.maturities = vec.maturities.4.estimation,
                          nb.values.s = NB.values.s,
                          h.stock = H.stock,
                          curvature = CURVATURE,
                          grid.4.S = NaN,
                          indic.the.ones.used.in.nom.curve = indic.the.ones.used.in.nom.curve,
                          indic.the.ones.used.in.rea.curve = indic.the.ones.used.in.rea.curve,
                          indic.slope.nom.curve = indic.slope.nom.curve,
                          indic.slope.rea.curve = indic.slope.rea.curve,
                          indic.condVar.nom.rate = indic.condVar.nom.rate,
                          indic.condVar.rea.rate = indic.condVar.rea.rate,
                          all.targets = all.targets,
                          all.weights = all.weights,
                          weight.historical.fit = weight.historical.fit,
                          method = "nlminb",
                          control=list(trace=1, maxit=mult.fact*sum(Filter)*MAXIT.nlminb, kkt = FALSE))
      if(sum(is.na(as.matrix(res.estim)[1:length(Theta.ini)]))==0){
        Theta.est <- c(as.matrix(res.estim)[1:length(Theta.ini)])
        Theta.ini <- Theta.est
      }
      
      Full.Theta[Filter==1] <- Theta.est
      
    }
    
    save(Full.Theta,file = "estimation/results/tempo.Rdat")
  }
  
  name.of.result.file <- "last.result" # name of result file (contains Full.Theta = parameterization of the model)
  save(Full.Theta,file = paste("estimation/results/",name.of.result.file,".Rdat",sep=""))
}

if(indic.save.results.if.estim == 1){
  # Overwrite previous result file:
  save(Full.Theta,file = paste("estimation/results/",result.file,".Rdat",sep=""))
}



