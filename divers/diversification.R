
# This code compute correlations between x-year-ahead GDP forecast errors.
# Annual GDP (Local Currency Unit) come from the World Bank.

library(forecast)
library(stargazer)

GDPs <- read.csv2("data/WorldBank_GDP_LocalCurrency/dataGDPWorld.csv")
nb.ctries <- dim(GDPs)[2] - 1
all.ctries <- names(GDPs)[2:dim(GDPs)[2]]

h <- 2 # forecast horizon
roll.T <- 5 # size of the estimation sample
first.year <- 1980
first.period <- which(GDPs$year==first.year)

T <- dim(GDPs)[1]

lGDP <- log(GDPs[,2:dim(GDPs)[2]])
dlGDP <- lGDP[(h+1):T,] - lGDP[1:(T-h),]

T <- dim(dlGDP)[1]

all.fcst.errors <- NaN * dlGDP

for(ctry in 1:nb.ctries){
  for(T0 in (first.period-1):(T-h)){
    x <- dlGDP[(T0-roll.T+1):T0,ctry]
    #plot(x,type="l")
    if(sum(is.na(x))==0){
      #ar.x <- arima(x,order=c(1,0,0))
      #fcast <- forecast(ar.x,h=h)
      #fcasted.chge <- sum(fcast$mean)
      fcasted.chge <- h*mean(x)
      realized.chge <- sum(dlGDP[(T0+1):(T0+h),ctry])
      fcast.error <- realized.chge - fcasted.chge
      all.fcst.errors[T0,ctry] <- fcast.error
    }
  }
}

ctries.list <- c("United.States","United.Kingdom","Germany","France","Canada","Japan",
                 "Brazil","China","India")
names.4.table <- c("U.S.","U.K.","Germany","France","Canada","Japan",
                   "Brazil","China","India")

all.indic <- NULL

par(mfrow=c(1,1))
count <- 0
for(ctry in ctries.list){
  count <- count + 1
  indic <- which(ctry == all.ctries)
  all.indic <- c(all.indic,indic)
  if(count==1){
    plot(all.fcst.errors[,indic],type="l",col=count)
  }else{
    lines(all.fcst.errors[,indic],col=count)
  }
}
legend("topleft", # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend 
       ctries.list,
       lty=c(1), # gives the legend appropriate symbols (lines)       
       lwd=c(2,2,2), # line width
       col=1:length(ctries.list), # gives the legend lines the correct color and width
       bg="white",
       seg.len = 2
)

all.fcst.errors <- all.fcst.errors[,all.indic]
indic.not.na <- !is.na(apply(all.fcst.errors,1,sum))
all.fcst.errors <- all.fcst.errors[indic.not.na,]

Table <- cor(all.fcst.errors)
Table[upper.tri(Table)] <- NaN
colnames(Table) <- names.4.table
rownames(Table) <- names.4.table
stargazer(Table,digits = 2)



# Prepare lines with coloured cells:
latex.lines <- NULL

correlation.threshold <- .4

Table.correl <- cor(all.fcst.errors)

count <- 1
for(ctry in ctries.list[2:length(ctries.list)]){
  count <- count + 1
  indic <- which(ctry == all.ctries)
  this.line <- names.4.table[count - 1]
  for(j in 1:(count-1)){
    this.line <- paste(this.line,"&",
                       ifelse(Table.correl[count,j]>correlation.threshold,
                              paste("\\cellcolor{gray!25}$",toString(round(Table.correl[count,j],2)),"$",sep=""),
                              paste("$",toString(round(Table.correl[count,j],2)),"$",sep="")),sep="")
  }
  latex.lines <- rbind(latex.lines,
                       paste(this.line,"\\\\",sep=""))
}

latex.file <- paste("prepare_outputs/Tables/table_diversif",toString(first.year),".txt",sep="")
write(latex.lines, file = latex.file)



