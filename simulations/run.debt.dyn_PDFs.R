
# ===========================================================
# Debt Dynamics
# Computation of distri of d for different horizons (constant bs)
# ===========================================================

# specification of kernel density estimations:
bw.4.kernel.density <- .001
bw.4.kernel.density.ds <- .005 # for debt service

# For simulations, the maximum maturity will be the max entry of vector.of.H:
max.matur <- max(vector.of.H) # expressed at the model frequency

# the maximum horizon is increased by maximum maturity:
max.horizon <-  max.matur + max(horizons.used.4.plots) # number of simulation periods

ini.debt.to.rolling.GDP <- ini.debt.to.GDP/Model$freq


# =============================
# Solve the model:
vec.maturities <- 1:max.matur
res.prices <- compute.prices(Model,
                             vec.maturities,
                             nb.values.s = NB.values.s,
                             h.stock = 1,
                             curvature = CURVATURE,
                             grid.4.S = NaN)

# =============================
# Compute model-implied moments:
res.mom.pi.z <- compute.uncond.mom.pi.z(res.prices$Model.solved)

# Array where all time series of d_t will be stored:
all.sim.d <- array(NaN,c(max.horizon,N.sim,3,length(vector.of.H)))
all.sim.debtservice <- array(NaN,c(max.horizon,N.sim,3,length(vector.of.H)))


print("==========================================================================")
print(" Simulations in progress (can take some time / progression is displayed)")
print("==========================================================================")

for(i.sim in 1:N.sim){
  
  if(i.sim/50 == round(i.sim/50)){
    print(paste("Processing simulation No ",toString(i.sim)," (out of ",toString(N.sim),")",
                sep=""))
  }
  
  # =============================
  # Simulate model:
  res.simul <- simul.model.solved(res.prices,T = max.horizon,
                                  nb.of.periods.with.ini.state = max.matur)
  
  # =================================================
  # Prepare inputs for "simulate.debt":
  
  # prepare
  simulated.yields <- list(
    nom.bond.yields = res.simul$nom.yields,
    rea.bond.yields = res.simul$rea.yields,
    GDP.bond.yields = res.simul$GDP.yields
    #GDP.bond.yields = res.simul$GDP.yields - 0*0.008/4
  )
  
  simulated.inflation  <- res.simul$sim.pi
  simulated.GDP.growth <- res.simul$sim.y
  
  simulated.GDP.expectation <- res.simul$GDP.fcsts + 
    matrix(.5*res.prices$Model.solved$sigma.y^2,max.matur,max.horizon)
  
  count.maturity <- 0
  for(maturity in vector.of.H){
    count.maturity <-   count.maturity + 1
    
    for(jjj in 1:3){# Loop on the type of bonds
      
      # Initial issuance schedule, expressed in fractions of GDP:
      aux.ini.issuances <- matrix(0,max.matur,max.matur)
      aux.ini.issuances[!upper.tri(aux.ini.issuances)] <- 1
      aux.ini.issuances <- aux.ini.issuances[,max.matur:1]
      
      ini.issuances <- list()
      ini.issuances$nom.bonds <- aux.ini.issuances * (jjj==1)
      ini.issuances$rea.bonds <- aux.ini.issuances * (jjj==2)
      ini.issuances$GDP.bonds <- aux.ini.issuances * (jjj==3)
      
      # Remove debt schedule associated with maturities larger than the one into consideration
      if(maturity < max.matur){
        ini.issuances$nom.bonds[,(maturity+1):max.matur] <- 0
        ini.issuances$rea.bonds[,(maturity+1):max.matur] <- 0
        ini.issuances$GDP.bonds[,(maturity+1):max.matur] <- 0
      }
      
      sum.weights <- sum(ini.issuances$nom.bonds +
                           ini.issuances$rea.bonds +
                           ini.issuances$GDP.bonds)
      
      weights <- ini.debt.to.GDP/sum.weights
      
      ini.issuances$nom.bonds <- weights * ini.issuances$nom.bonds
      ini.issuances$rea.bonds <- weights * ini.issuances$rea.bonds
      ini.issuances$GDP.bonds <- weights * ini.issuances$GDP.bonds
      
      param <- list() # matrix of weights indicating the types of bonds that are issued on each period
      
      # Define issuance strategy:
      param$matrix <- matrix(0,max.matur,3) # nominal / ILB / GDP-LB
      
      # param$matrix[maturity,jjj] <- 1
      param$matrix[1:maturity,jjj] <- 1
      
      param$matrix <- param$matrix/sum(param$matrix)
      
      res.sim.debt <- simulate.debt(simulated.yields,
                                    simulated.inflation,
                                    simulated.GDP.growth,
                                    simulated.GDP.expectation,
                                    ini.issuances,
                                    param,
                                    freq = Model$freq,
                                    indic.compute.bs = 0,
                                    predefined.bs = rep(fixed.bs,max.horizon))
      
      #debt.time.series <- res.sim.debt$all.d * res.sim.debt$Rolling.nominal.GDP /
      #  (Model$freq*res.sim.debt$Nominal.GDP) * (ini.debt.to.GDP / Model$freq) / (res.sim.debt$all.d[max.matur+1])
      
      #debt.time.series <- res.sim.debt$all.d
      
      debt.time.series <- (ini.debt.to.GDP/res.sim.debt$all.d[max.matur+1]) * 
        res.sim.debt$all.d * res.sim.debt$Nominal.GDP / res.sim.debt$Rolling.nominal.GDP
      
      debtservice.time.series <- (ini.debt.to.GDP/res.sim.debt$all.d[max.matur+1]) * 
        res.sim.debt$Rolling.ds
      
      # Consider 4-quarter averages:
      debt.to.rolling.GDP <- res.sim.debt$all.d * res.sim.debt$Nominal.GDP / res.sim.debt$Rolling.nominal.GDP
      debt.time.series <- debt.to.rolling.GDP/debt.to.rolling.GDP[max.matur+1] * ini.debt.to.rolling.GDP
      debtservice.time.series <- res.sim.debt$Rolling.ds /debt.to.rolling.GDP[max.matur+1] * ini.debt.to.rolling.GDP
      
      # Consider Debt-to-Quarterly-GDP:
      debt.to.rolling.GDP <- res.sim.debt$all.d * res.sim.debt$Nominal.GDP / res.sim.debt$Rolling.nominal.GDP
      debt.time.series <- res.sim.debt$all.d/res.sim.debt$all.d[max.matur+1] * ini.debt.to.rolling.GDP
      debtservice.time.series <- res.sim.debt$Rolling.ds /debt.to.rolling.GDP[max.matur+1] * ini.debt.to.rolling.GDP
      
      if(jjj==1){
        #plot(debt.time.series,type="l",col="red")
        all.sim.d[,i.sim,1,count.maturity] <- debt.time.series
        all.sim.debtservice[,i.sim,1,count.maturity] <- debtservice.time.series
      }else if(jjj==2){
        #lines(debt.time.series,col="blue")
        all.sim.d[,i.sim,2,count.maturity] <- debt.time.series
        all.sim.debtservice[,i.sim,2,count.maturity] <- debtservice.time.series
      }else{
        #lines(debt.time.series,col="black")
        all.sim.d[,i.sim,3,count.maturity] <- debt.time.series
        all.sim.debtservice[,i.sim,3,count.maturity] <- debtservice.time.series
      }
      
    }
  }
  
}






# Distributions of debt-to-GDP ratios:

file <- "Figure_debt_pdf"
pdf(file=paste(output.figures.folder,"/",file,".pdf",sep=""),
    pointsize=10, width=8, height=6)

par(mfrow=c(length(vector.of.H),length(horizons.used.4.plots)))
par(plt = c(.05,.95,.25,.85))

for(i in 1:length(vector.of.H)){
  for(j in 1:length(horizons.used.4.plots)){
    
    if(horizons.used.4.plots[j]<21){
      x.lim <- c(.8,1.2)
    }else{
      x.lim <- c(.5,2)
    }
    
    # Nominal bonds:
    pdf.nom <- density(all.sim.d[max.matur+horizons.used.4.plots[j],,1,i])
    #,bw = bw.4.kernel.density*sqrt(horizons.used.4.plots[j]))
    pdf.rea <- density(all.sim.d[max.matur+horizons.used.4.plots[j],,2,i])
    #,bw = bw.4.kernel.density*sqrt(horizons.used.4.plots[j]))
    pdf.GDP <- density(all.sim.d[max.matur+horizons.used.4.plots[j],,3,i])
    #,bw = bw.4.kernel.density*sqrt(horizons.used.4.plots[j]))
    
    plot(pdf.nom$x,pdf.nom$y,type="l",
         main=paste("Maturity of bonds: ",toString(vector.of.H[i]/FREQ),ifelse(vector.of.H[i]<=4," year","  years"),", Horizon: ",toString(horizons.used.4.plots[j]/FREQ)," years",sep=""),
         yaxt="n",ylab="",xlab="Debt-to-GDP ratio",
         col="white",lwd=1,
         ylim=c(0,1.2*max(pdf.nom$y,pdf.rea$y,pdf.GDP$y)),
         xlim=x.lim)
    
    lines(pdf.nom$x,pdf.nom$y,lwd=2,col="dark grey",lty=2)
    lines(pdf.rea$x,pdf.rea$y,lwd=2,col="dark grey",lty=1)
    lines(pdf.GDP$x,pdf.GDP$y,lwd=2,col="black",lty=1)
    
    abline(h=0)
    
    if((i==1)*(j==1))
      legend("topright", 
             c("Nominal bonds",
               "ILBs",
               "GDP-LBs"),
             lty=c(2,1,1), # gives the legend appropriate symbols (lines)       
             lwd=c(2,2,2), # line width
             col=c("dark grey","dark grey","black"), # gives the legend lines the correct color and width
             bg="white",
             seg.len = 2
      )
    
  }
}

dev.off()







# Distributions of debt services:

file <- "Figure_debtservice_pdf"
pdf(file=paste(output.figures.folder,"/",file,".pdf",sep=""),
    pointsize=10, width=8, height=6)

par(mfrow=c(length(vector.of.H),length(horizons.used.4.plots)))
par(plt = c(.05,.95,.25,.85))

for(i in 1:length(vector.of.H)){
  for(j in 1:length(horizons.used.4.plots)){
    
    if(horizons.used.4.plots[j]<21){
      x.lim <- c(-0.04,.14)
    }else{
      x.lim <- c(-0.04,.14)
    }
    
    # Nominal bonds:
    pdf.nom <- density(all.sim.debtservice[max.matur+horizons.used.4.plots[j],,1,i])
    #,bw = bw.4.kernel.density.ds)
    pdf.rea <- density(all.sim.debtservice[max.matur+horizons.used.4.plots[j],,2,i])
    #,bw = bw.4.kernel.density.ds)
    pdf.GDP <- density(all.sim.debtservice[max.matur+horizons.used.4.plots[j],,3,i])
    #,bw = bw.4.kernel.density.ds)
    
    plot(pdf.nom$x,pdf.nom$y,type="l",
         main=paste("Maturity of bonds: ",toString(vector.of.H[i]/FREQ),ifelse(vector.of.H[i]<=4," year","  years"),", Horizon: ",toString(horizons.used.4.plots[j]/FREQ)," years",sep=""),
         yaxt="n",ylab="",xlab="Debt service (as a fraction of GDP)",
         col="white",lwd=1,
         ylim=c(0,1.2*max(pdf.nom$y,pdf.rea$y,pdf.GDP$y)),
         xlim=x.lim)
    
    lines(pdf.nom$x,pdf.nom$y,lwd=2,col="dark grey",lty=2)
    lines(pdf.rea$x,pdf.rea$y,lwd=2,col="dark grey",lty=1)
    lines(pdf.GDP$x,pdf.GDP$y,lwd=2,col="black",lty=1)
    
    abline(h=0)
    
    if((i==1)*(j==1))
      legend("topright",
             c("Nominal bonds",
               "ILBs",
               "GDP-LBs"),
             lty=c(2,1,1), # gives the legend appropriate symbols (lines)       
             lwd=c(2,2,2), # line width
             col=c("dark grey","dark grey","black"), # gives the legend lines the correct color and width
             bg="white",
             seg.len = 2
      )
    
  }
}

dev.off()




file <- "Figure_few_debt_simul"
pdf(file=paste(output.figures.folder,"/",file,".pdf",sep=""),
    pointsize=10, width=7, height=5)


nb.of.simul <- 4

par(mfrow=c(2,nb.of.simul))
par(plt = c(.2,.95,.25,.85))

for(i in 1:length(vector.of.H)){
  for(j in 1:nb.of.simul){
    # Nominal bonds:
    d.nom <- all.sim.d[,j,1,i]
    d.rea <- all.sim.d[,j,2,i]
    d.GDP <- all.sim.d[,j,3,i]
    
    plot((1:max(horizons.used.4.plots))/FREQ,d.nom[(max.matur+1):length(d.nom)],
         type="l",
         main=paste(toString(vector.of.H[i]/FREQ),"-yr bonds, path ",toString(j),sep=""),
         ylab="",xlab="time (in years)",
         col="dark grey",lwd=2,las=1,lty=2,
         #         ylim=c(.75,1.15))
         ylim=c(
           min(d.nom[(max.matur+1):length(d.nom)],d.rea[(max.matur+1):length(d.nom)],d.GDP[(max.matur+1):length(d.nom)]),
           max(d.nom[(max.matur+1):length(d.nom)],d.rea[(max.matur+1):length(d.nom)],d.GDP[(max.matur+1):length(d.nom)])
         ))
    lines((1:max(horizons.used.4.plots))/FREQ,d.rea[(max.matur+1):length(d.nom)],
          col="dark grey",lwd=2)
    lines((1:max(horizons.used.4.plots))/FREQ,d.GDP[(max.matur+1):length(d.nom)],
          col="black",lwd=2)
    
    if((i==1)*(j==1)){
      legend(ifelse(d.rea[length(d.nom)]>1,"bottomright","topright"), 
             c("Nominal bonds",
               "ILBs",
               "GDP-LBs"),
             lty=c(2,1,1), # gives the legend appropriate symbols (lines)       
             lwd=c(2,2,2), # line width
             col=c("dark grey","dark grey","black"), # gives the legend lines the correct color and width
             bg="white",
             seg.len = 2
      )
    }
    
  }
}

dev.off()





# =====================================
# =====================================
# Tables
# =====================================
# =====================================

# Table with quantiles of debt-to-GDP

latex.table <- rbind(
  "\\begin{table}",
  "\\caption{Influence of issuance strategies on debt-to-GDP quantiles}",
  "\\label{table_debt2GDP_quantiles}",
  "\\begin{center}",
  "\\begin{tabular*}{\\textwidth}{l@{\\extracolsep{\\fill}}lccccccccccc}",
  #"\\hline",
  "\\\\")


vec.of.quantiles <- c(.5,.9,.95)


first.line <- paste("Quantile &",
                    "\\multicolumn{3}{c}{Horizon: ",
                    toString(horizons.used.4.plots[1]/FREQ),ifelse(horizons.used.4.plots[1]<=4," year"," years"),"}",
                    "&",
                    "\\multicolumn{3}{c}{Horizon: ",
                    toString(horizons.used.4.plots[2]/FREQ),ifelse(horizons.used.4.plots[2]<=4," year"," years"),"}",
                    "\\\\",
                    sep="")


second.line <- "&Nominal&Infl.-linked&GDP-linked&Nominal&Infl.-linked&GDP-linked \\\\"

for(i in 1:length(vector.of.H)){
  
  latex.table <- rbind(latex.table,
                       paste("&\\multicolumn{6}{c}{{\\bf Panel ",ifelse(i==1,"A","B")," -- Maturity of issued bonds: ",
                             toString(vector.of.H[i]/FREQ),
                             ifelse(vector.of.H[i]<=4," year"," years"),"}}\\\\",sep=""),
                       "\\\\",
                       "\\hline",
                       first.line,
                       "\\hline",
                       second.line,
                       "\\hline")
  
  for(k in 1:length(vec.of.quantiles)){
    this.line <- paste(toString(100*vec.of.quantiles[k]),"\\%",sep="")
    
    for(j in 1:length(horizons.used.4.plots)){
      for(type.of.bond in c(1,2,3)){
        pdf <- density(all.sim.d[max.matur+horizons.used.4.plots[j],,type.of.bond,i])
        #,bw = bw.4.kernel.density*sqrt(horizons.used.4.plots[j]))
        
        this.line <- paste(this.line,"&",
                           round.fixed.length(quantile.density(pdf,vec.of.quantiles[k]),2)
                           ,sep="")
      }
    }
    this.line <- paste(this.line,"\\\\")
    
    latex.table <- rbind(latex.table,
                         this.line)
    
  }
  if(i != length(vector.of.H)){
    latex.table <- rbind(latex.table,
                         "\\hline",
                         "\\\\")
  }else{
    latex.table <- rbind(latex.table,
                         "\\hline")
  }
  
}


latex.table <- rbind(latex.table,
                     "\\end{tabular*}",
                     "\\end{center}",
                     "\\begin{scriptsize}",
                     "\\parbox{\\linewidth}{",
                     "Note: This table reports the quantiles of the distributions of debt-to-GDP ratios resulting 
                     from different issuance strategies. The issuance strategies are basic strategies whereby the government,
                     on each period, issues the same type of bonds to meet its funding needs.",
                     "}",
                     "\\end{scriptsize}",
                     "\\end{table}")


latex.file <- paste(output.tables.folder,"/table_debt_quantiles.txt",sep="")
write(latex.table, file = latex.file)



