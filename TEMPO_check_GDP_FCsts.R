
N <- dim(res.prices$all.GDP.forecasts)[2]
NN <- N/3

plot(res.prices$all.GDP.forecasts[,1],type="l")

for(i in 2:NN){
  lines(res.prices$all.GDP.forecasts[,i],col="black")
}

for(i in (NN+1):(2*NN)){
  lines(res.prices$all.GDP.forecasts[,i],col="blue")
}
for(i in (2*NN+1):(3*NN)){
  lines(res.prices$all.GDP.forecasts[,i],col="red")
}


plot(res.prices$all.nom.bond.F.yields[,1],type="l",ylim=c(0,1))
for(i in 2:N){
  lines(res.prices$all.nom.bond.F.yields[,i],type="l")
}


res.mom.pi.z <- compute.uncond.mom.pi.z(res.prices$Model.solved)
E.z <- res.mom.pi.z$E.z

mean.rates <- res.prices$all.nom.bond.F.yields %*% E.z
plot(mean.rates)

