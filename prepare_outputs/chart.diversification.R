# ===========================================================================
# ===========================================================================
# This script produces charts illustrating the limitations of diversification
# ===========================================================================
# ===========================================================================


setwd("~/Dropbox/13_Macrofounded_SDF/3_Codes/A_Habits/prepare_outputs/LatexPlots")

pprint <- 1

first.date.diversification <- as.Date("01.01.1995","%d.%m.%Y")
last.date.diversification  <- as.Date("01.01.2018","%d.%m.%Y")

indic.first.date <- which(macro.data$date==first.date.diversification)
indic.last.date <- which(macro.data$date==last.date.diversification)


h <- 4



if(pprint==1){
  options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
                                 "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
                                 "\\usepackage{amssymb}"))
  ## I need the amssymb package because I use \mathcal and \mathbb
  tikz("Figure_diversif.tex", width = 7, height = 4, standAlone = TRUE,
       packages = c("\\usepackage{tikz}",
                    "\\usepackage[active,tightpage,psfixbb]{preview}",
                    "\\PreviewEnvironment{pgfpicture}",
                    "\\setlength\\PreviewBorder{0pt}",
                    "\\usepackage{amssymb}"))
}

par(mfrow=c(1,1))
par(plt=c(.15,.85,.13,.95))

plot(macro.data$date[indic.first.date:indic.last.date],
     log(macro.data$CONSO.US[indic.first.date:indic.last.date]/
           macro.data$CONSO.US[(indic.first.date-h):(indic.last.date-h)]),
     type="l",
     col="black",lwd=3,ylim=c(-.06,.05),las=1,xlab="",ylab="US GDP and consumption")

AUX <- macro.data$GDP.US * macro.data$GDPDEF.US / macro.data$GDPDEF.US
lines(macro.data$date[indic.first.date:indic.last.date],
     log(AUX[indic.first.date:indic.last.date]/AUX[(indic.first.date-h):(indic.last.date-h)]),
     col="dark grey",lwd=3)

AUX <- macro.data$GDP.ZE * macro.data$GDPDEF.ZE / macro.data$GDPDEF.US
par(new=TRUE)
plot(macro.data$date[indic.first.date:indic.last.date],
     log(AUX[indic.first.date:indic.last.date]/AUX[(indic.first.date-h):(indic.last.date-h)]),
     type="l",col="black",lwd=4,lty=3,xaxt="n",yaxt="n",xlab="",ylab="",
     ylim=c(-.08,.04))
axis(side = 4,
     labels=FALSE, # because we will use mtext to choose the color
     col="black",las=3)
mtext("Euro-area nominal GDP, deflated with US price index", side=4, line=3,col="black")
at = axTicks(2)
mtext(side = 4, text = at, at = at,
      col = "black",
      line = 1,
      las=1 # vertical reading
)

legend("bottomleft", # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend 
       c("US consumption growth (left-hand scale)","US GDP growth (left-hand scale)","EA nominal GDP, deflated with US price index (right-hand scale)"),
       lty=c(1,1,3), # gives the legend appropriate symbols (lines)       
       lwd=c(3,3,4), # line width
       col=c("black","dark grey","black"), # gives the legend lines the correct color and width
       #pt.bg=c(NaN,"red",NaN,"blue",NaN),
       #text.width = 2,
       #cex=1.0,# size of the text
       #pch = c(NaN,19,NaN,19,NaN),#symbols,
       #pt.cex = c(NaN,NaN,2,NaN,NaN,2),
       bg="white",
       seg.len = 3
)


if(pprint==1){
  dev.off()
  tools::texi2pdf("Figure_diversif.tex")
  system(paste(getOption("pdfviewer"), "Figure_diversif.pdf"))
}

