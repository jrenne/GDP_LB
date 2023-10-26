# Prepare chart illustrating habit consumption model

nb.sim <- 200

# Simulation
res.simul <- simul.model(Model,nb.sim)
C <- exp(cumsum(res.simul$simul.delta.c))
S <- exp(res.simul$simul.s)
X <- C - S * C

for(i in 1:2){
  
  if(i==2){
    FILE = "/figures/Figure_illustr_habits.pdf"
    #pdf(file=paste(getwd(),FILE,sep=""),pointsize=10,width=6, height=5)  
    pdf(file=paste(getwd(),FILE,sep=""),pointsize=10,width=6, height=6)  
  }
  
  par(mfrow=c(2,1))
  par(plt=c(.06,.95,.2,.85))
  plot(res.simul$simul.delta.c,type="l",lwd=2,
       xlab="",ylab="",
       main=expression(paste("Consumption growth (",Delta,c[t],")",sep="")))
  abline(h=0,col="grey")
  plot(C,type="l",ylim=c(min(X),max(C)),lwd=2,
       xlab="",ylab="",
       main=expression(paste("Consumption (",C[t],", in black) and habits (",X[t],", in grey)",,sep="")))
  lines(X,col="dark grey",lwd=2)
  
  if(i==2){
    dev.off()
  }
}





