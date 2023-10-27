
# ==============================================================================
# Debt-Stabilizing Properties of GDP-Linked Securities:
# A Macro-Finance Perspective
#
# Sarah Mouabbi, Jean-Paul Renne, Jean-Guillaume Sahuc
# 
# This version: November 2023.
# ==============================================================================


# Clear environment and console
rm(list = ls(all = TRUE)) # clear environment 
cat("\014") # clear console
par(plt=c(.1,.9,.1,.9)) # set initial margins for plots


# ==============================================================================
# SETTINGS ---------------------------------------------------------------------

# Numerical optimization (1) of moments or not (0)?
indic.estim <- 1
indic.start.from.last <- 0 # if = 1, then start from the last estimated model.
indic.save.results.if.estim <- 1 # save results if estimation? (1 = Yes)

# Produce Latex outputs:
indic.produce.Latex.outputs <- 1

# Run debt simulations (1) or not (0)?
indic.debt.simulations <- 0


# if estimation: ---------------------------------------------------------------
weight.historical.fit <- .5 # if >0, then the goodness of fit is taken into account
# Numerical optimization settings (see in run.aux.estim.R for details):
nb.loops      <- 6
nb.sub.loops  <- 1
mult.fact     <- 1
MAXIT.NM      <- 30
MAXIT.nlminb  <- 4
# Set maximum value of estimated param:
max.abs.value.params <- 20
# Explanation: changes in variable are such that paramrters larger
# than max.abs.value.params are either very large or very close to specified bounds.


# Folders for figures and tables:
output.figures.folder <- "prepare_outputs/PDF_Figures"
output.tables.folder  <- "prepare_outputs/Tables"

# ==============================================================================


# ==============================================================================
# Load libraries:
library(mFilter) # HP filter
library(optimx)
library(stringr)
library(expm)
library(tseries)
library(tikzDevice)

# Load procedures:
source('procedures/set.of.procedures.macrofinance.model.R')
source('procedures/set.of.procedures.DebtDyn.R')
source('procedures/set.of.procedures.Estimation.R')
# ==============================================================================



# ==============================================================================
# Estimation sample

# Sample used for financial data:
start_Y <- 1985
end_Y<- 2018
start_q <- 1 # Quarter
end_q<- 1

indic.same.dates.4.macro.fi <- 1
# if = 1, then uses the same sample dates for macro and financial data
#      otherwise, macro sample:
first.date.macro <- as.Date("01.01.1985","%d.%m.%Y")
last.date.macro  <- as.Date("01.01.2018","%d.%m.%Y")

H.stock <- 4 # holding period for stocks
# ==============================================================================


# ==============================================================================
# Load initial model:
result.file <- "results_Main"
source('estimation/load.ini.model.R')

# Load dataset:
source('estimation/load.data.R')

# Compute moments to be fit:
source('estimation/list_moments_2B_fitted.R')
# ==============================================================================


# ==============================================================================
# Approximation settings (the model is solved by grid-based approaches):
NB.values.s <- 50 # number of discretized values of s
CURVATURE   <- 20  # the discretized values of s are selected linaerly, this parameter determines the non-linearity of discretized values of s
# ==============================================================================


# ==============================================================================
# Select rates to be fitted:
# Warning: Make sure the targets are consistent with these maturities (in "list_moments_2B_fitted.R")
vec.maturities.4.estimation      <- c(1,4*c(2,10,30)) # in number of quarters
indic.the.ones.used.in.nom.curve <- c(1,3,4) # indicators of the components of vec.maturities.4.estimation
indic.the.ones.used.in.rea.curve <- c(1,2,3)
indic.slope.nom.curve            <- c(1,4)
indic.slope.rea.curve            <- c(1,3)
indic.condVar.nom.rate           <- c(1,4)
indic.condVar.rea.rate           <- 3
# ==============================================================================


# ==============================================================================
# Model estimation:

all.targets <- all.targets.US
all.weights <- all.weights.US
Omega       <- Omega.US

if(indic.estim == 1){
  source('estimation/run.estimation.R')
}
# ==============================================================================


# ==============================================================================
# Model outputs (except debt simultations):

source('estimation/show.outputs.R')


# Generate outputs:
if(indic.produce.Latex.outputs == 1){
  
  # Table with model parameters:
  source('prepare_outputs/make.tables.R')
  
  # Chart showing average yield curves:
  indic.plot.nominal <- 1
  source('prepare_outputs/chart.curves.R')

  # Chart showing sensitivity of yields to consumption surplus:
  source('prepare_outputs/chart.sensitivity.R')

  # Chart showing interest rate distributions:
  source('prepare_outputs/chart.distri.IR.R')
  
  # Chart showing historical fit:
  source('prepare_outputs/chart.historical.fit.R')
}
# ==============================================================================





# ==============================================================================
# Debt simulations:

if(indic.debt.simulations == 1){
  
  # ----------------------------------------------------------------------------
  # Run script computing conditional pdf of debt-to-GDP ratios:
  
  # Initialize the random number generator:
  set.seed(124)
  
  NB.values.s <- 100 # number of discretized values of s
  
  fixed.bs <- -0.01 # fixed budget primary surplus
  
  ini.debt.to.GDP <- 4.00 # expressed in GDP percent (nominal GDP of one model period)
  
  vector.of.H           <- c(4,40) # Maturites of issued bonds -- expressed at the model frequency
  horizons.used.4.plots <- c(8,80) # Horizons considered, expressed in number of model periods
  
  N.sim <- 200 # number of simulations
  #N.sim <- 200 # number of simulations
  
  source('simulations/run.debt.dyn_PDFs.R')
  
  # ----------------------------------------------------------------------------
  # Run script computing Risk measures (Cost/Risk plots):
  
  # Initialize the random number generator:
  set.seed(124)

  vector.of.H       <- c(4,20,40) # maturities of considered bonds -- expressed at the model frequency
  vector.of.weights <- 0:1 # relative importances of Nom/ILB/GDP-LB
  
  nb.periods <- 40000 # number of simulated periods
  #nb.periods <- 1000 # number of simulated periods
  
  ini.debt.to.GDP <- 400 # expressed in GDP percent. Caution: GDP is nominal GDP for one model period

  source('simulations/run.debt.dyn_XYplots.R')

}

