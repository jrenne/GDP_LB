# ==============================================================================
# Check some model outputs
# ==============================================================================

Filter <- rep(1,length(Full.Theta))
max.criteria <- compute.criteria(Full.Theta,
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


print("=================================================")
print(paste("Maximized criteria: ",toString(round(max.criteria,2)),sep=""))

# Build estimated model:
Model.est <- Theta.2.Model(Full.Theta)
Model <- Model.est

# Compute modeled moments:
res <- compare.target.and.model(Model.est,
                                vec.maturities.4.estimation,
                                nb.values.s = NB.values.s,
                                h.stock = H.stock,
                                curvature = CURVATURE,
                                grid.4.S = NaN,
                                indic.the.ones.used.in.nom.curve,
                                indic.the.ones.used.in.rea.curve,
                                indic.slope.nom.curve = indic.slope.nom.curve,
                                indic.slope.rea.curve = indic.slope.rea.curve,
                                indic.condVar.nom.rate = indic.condVar.nom.rate,
                                indic.condVar.rea.rate = indic.condVar.rea.rate)

modelled.moments <- res$modelled.moments

# Compare model-implied to observed moments:
print("=================================================")
print(" Model-implied versus data-based moments: (last column is contribution to loss)")
print(cbind(
  all.target.names,
  round(all.targets,6),
  round(modelled.moments,6),
  round((modelled.moments - all.targets)^2/all.weights^2,3)
))
print("=================================================")



# Asset-pricing implications:
vec.maturities.postestim <- 1:200
res.prices <- compute.prices(Model.est,
                             vec.maturities.postestim,
                             nb.values.s = NB.values.s,
                             h.stock = H.stock,
                             curvature = CURVATURE,
                             grid.4.S = NaN)
res.mom.pi.z <- compute.uncond.mom.pi.z(res.prices$Model.solved)

# Plots of average model-implied rates:
res.all.moments <- compute.moments(res.prices$Model.solved,
                                   vec.maturities.postestim,
                                   nb.values.s = NB.values.s,
                                   h.stock = H.stock,
                                   curvature = CURVATURE,
                                   grid.4.S = NaN,
                                   indic.slope.nom.curve,
                                   indic.slope.rea.curve,
                                   indic.condVar.nom.rate,
                                   indic.condVar.rea.rate)

par(mfrow=c(1,2))
plot(vec.maturities.postestim/Model$freq,
     res.all.moments$mean.nom.yields,type="l",ylim=c(0,.08))
lines(vec.maturities.postestim/Model$freq,
      res.all.moments$mean.rea.yields,col="red")
lines(vec.maturities.postestim/Model$freq,
      res.all.moments$mean.GDP.yields,col="blue")

# Plot of P/D ratios:
plot(exp(res.prices$Model.solved$mu),res.prices$Chi/Model$freq)








