
# ==============================================================================
# Prepare table illustrating model fit
# Make sure Model.est is avaulable before running this script
# ==============================================================================

format.nb0 <- paste("%.",0,"f",sep="")
format.nb1 <- paste("%.",1,"f",sep="")
format.nb2 <- paste("%.",2,"f",sep="")
format.nb3 <- paste("%.",3,"f",sep="")
format.nb5 <- paste("%.",5,"f",sep="")


part.table.US <- (cbind(all.target.names,
                        all.targets,
                        modelled.moments))


indic.hline <- c(7,18,29,32) # indicate where hlines will be put in table

latex.table <- NULL
for(i in 1:length(all.targets)){
  if(sum(i==indic.hline)>0){
    latex.table <- rbind(latex.table,"\\hline")
  }
  latex.table <- rbind(latex.table,
                       c(paste(part.table.US[i,1],
                               ifelse(all.multiplicative.factors[i]==0,"&",paste("&$\\times 10^{",toString(all.multiplicative.factors[i]),"}$",sep="")),
                               "&",
                               "&",sprintf(format.nb2,10^all.multiplicative.factors[i]*as.numeric(part.table.US[i,2])),
                               "&",sprintf(format.nb2,10^all.multiplicative.factors[i]*as.numeric(part.table.US[i,3])),
                               "\\\\",
                               sep="")))
  
}
latex.file <- paste(output.tables.folder,"/tableFIT.txt",sep="")
write(latex.table, file = latex.file)


# ==============================================================================
# First part of the table
latex.table <- NULL
for(i in 1:17){
  if(sum(i==indic.hline)>0){
    latex.table <- rbind(latex.table,"\\hline")
  }
  latex.table <- rbind(latex.table,
                       c(paste(part.table.US[i,1],
                               ifelse(all.multiplicative.factors[i]==0,"&",paste("&$\\times 10^{",toString(all.multiplicative.factors[i]),"}$",sep="")),
                               "&",
                               "&",sprintf(format.nb2,10^all.multiplicative.factors[i]*as.numeric(part.table.US[i,2])),
                               "&",sprintf(format.nb2,10^all.multiplicative.factors[i]*as.numeric(part.table.US[i,3])),
                               "\\\\",
                               sep="")))
  
}
latex.file <- paste(output.tables.folder,"/tableFIT_part1.txt",sep="")
write(latex.table, file = latex.file)


# ==============================================================================
# Second part of the table
latex.table <- NULL
for(i in 18:35){
  latex.table <- rbind(latex.table,
                       c(paste(part.table.US[i,1],
                               ifelse(all.multiplicative.factors[i]==0,"&",paste("&$\\times 10^{",toString(all.multiplicative.factors[i]),"}$",sep="")),
                               "&",
                               "&",sprintf(format.nb2,10^all.multiplicative.factors[i]*as.numeric(part.table.US[i,2])),
                               "&",sprintf(format.nb2,10^all.multiplicative.factors[i]*as.numeric(part.table.US[i,3])),
                               "\\\\",
                               sep="")))
  if(sum(i==indic.hline-1)>0){
    latex.table <- rbind(latex.table,"\\hline")
  }
  
}
latex.file <- paste(output.tables.folder,"/tableFIT_part2.txt",sep="")
write(latex.table, file = latex.file)



# ==============================================================================
# ==============================================================================
# Param Table

latex.file <- paste(output.tables.folder,"/tableParam.txt",sep="")

latex.table <- NULL

# ==============================================================================
# Preference parameters

# -------- delta
coef.mult <- 0
nb.decim <- 3
this.line <- "Rate of preference for present&$\\delta$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")

this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$delta),"$",
                   #"&(XXX)&",
                   sep="")

this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)


# -------- gamma
coef.mult <- 0
nb.decim <- 1
this.line <- "Risk aversion parameter&$\\gamma$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")

this.line <- paste(this.line,"&$",
                   sprintf(format.nb1,10^coef.mult * Model.est$Gamma),"$",
                   #"&(XXX)&",
                   sep="")

this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

latex.table <- rbind(latex.table,"\\hline")




# -------- Distribution of consumption shocks
coef.mult <- 2
nb.decim <- 3
this.line <- "\\multirow{7}{*}{Consumption growth (eq.\\,\\ref{eq:deltaC} and Subsection\\,\\ref{sub:calibration})}&$g_{c,l}$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")
this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$g_c[1]),"$",
                   #"&(XXX)&",
                   sep="")
this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

this.line <- "&$g_{c,i}$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")
this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$g_c[2]),"$",
                   #"&(XXX)&",
                   sep="")
this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

this.line <- "&$g_{c,h}$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")
this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$g_c[3]),"$",
                   #"&(XXX)&",
                   sep="")
this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

coef.mult <- 2

this.line <- "&$p_{ll}$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")

this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$p_ll),"$",
                   #"&(XXX)&",
                   sep="")
this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

this.line <- "&$p_{hh}$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")

this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$p_hh),"$",
                   #"&(XXX)&",
                   sep="")
this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

this.line <- "&$p_{ii}$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")

this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$p_ii),"$",
                   #"&(XXX)&",
                   sep="")
this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

this.line <- "&$p_{il}$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")

this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$p_il),"$",
                   #"&(XXX)&",
                   sep="")
this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

latex.table <- rbind(latex.table,"\\hline")




# -------- GDP growth shocks
coef.mult <- 0
nb.decim <- 3
this.line <- "\\multirow{2}{*}{GDP growth shocks  (eq.\\,\\ref{eq:y})}&$\\rho_y$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")

this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$rho.y),"$",
                   #"&(XXX)&",
                   sep="")

this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

coef.mult <- 3
this.line <- "&$\\sigma_y$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")

this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$sigma.y),"$",
                   #"&(XXX)&",
                   sep="")

this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

latex.table <- rbind(latex.table,"\\hline")



# -------- Inflation dynamics
coef.mult <- 2
nb.decim <- 3
this.line <- "\\multirow{4}{*}{Inflation dynamics (eq.\\,\\ref{eq:pi})}&$\\overline{\\pi}$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")

this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$pi.bar),"$",
                   #"&(XXX)&",
                   sep="")

this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)


coef.mult <- 0
this.line <- "&$\\psi$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")

this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$psi),"$",
                   #"&(XXX)&",
                   sep="")

this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)


coef.mult <- 0
this.line <- "&$\\rho_\\pi$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")

this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$rho.pi),"$",
                   #"&(XXX)&",
                   sep="")

this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

coef.mult <- 4
this.line <- "&$\\sigma_\\pi$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")

this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$sigma.pi),"$",
                   #"&(XXX)&",
                   sep="")

this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

latex.table <- rbind(latex.table,"\\hline")



# -------- Dynamics of consumption ratio
coef.mult <- 0
nb.decim <- 3
this.line <- "\\multirow{2}{*}{Dynamics of consumption ratio (eqs.\\,\\ref{eq:s_dyn2}, \\ref{eq:lambda} and \\ref{eq:bar_s})}&$\\phi$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")

this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$phi),"$",
                   #"&(XXX)&",
                   sep="")

this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

coef.mult <- 2
this.line <- "&$b$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")

this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$b),"$",
                   #"&(XXX)&",
                   sep="")

this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

latex.table <- rbind(latex.table,"\\hline")




# -------- Dividend shocks
coef.mult <- 0
nb.decim <- 2
this.line <- "\\multirow{2}{*}{Growth rate of dividends (eq.\\,\\ref{eq:dividends})}&$\\rho_d$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")
this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$rho.d),"$",
                   #"&(XXX)&",
                   sep="")

this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

coef.mult <- 4
nb.decim <- 3
this.line <- "&$\\sigma_d$"
this.line <- paste(this.line,ifelse(coef.mult==0,"&&",
                                    paste("&$\\times 10^",toString(coef.mult),"$&",sep="")),sep="")

this.line <- paste(this.line,"&$",
                   sprintf(format.nb3,10^coef.mult * Model.est$sigma.d),"$",
                   #"&(XXX)&",
                   sep="")

this.line <- paste(this.line,"\\\\",sep="")
latex.table <- rbind(latex.table,this.line)

latex.table <- rbind(latex.table,"\\hline")

write(latex.table, file = latex.file)

