
# Calibration:
mu         <- .015
sigma_eta  <- .018
d          <- -.05
ell_0      <- .01
ell_1      <- .01
nu         <- .966
Theta      <- 1
chi        <- .2
sigma_zeta <- .01

n <- 1000

# Mean of E: 
E.0 <- Theta * mu / ( 1 - nu )

all.E       <- NULL
all.delta.c <- NULL
all.delta.c.no.D <- NULL
all.T       <- NULL
all.N       <- NULL
all.pi       <- NULL
all.D       <- NULL


E <- E.0 # Simulation start from unconditional value of E_t.
for(t in 2:n){
  eta  <- rnorm(1)
  zeta <- rnorm(1)
  E <- nu * E + Theta * (mu + sigma_eta * eta) + sigma_zeta * zeta
  T <- chi * E
  pi <- ell_0 + ell_1 * T
  N <- rpois(1,pi)
  D <- N * d
  delta.c <- mu + sigma_eta * eta + D
  delta.c.no.D <- mu + sigma_eta * eta
  
  all.E       <- c(all.E,E)
  all.delta.c <- c(all.delta.c,delta.c)
  all.delta.c.no.D <- c(all.delta.c.no.D,delta.c.no.D)
  all.T       <- c(all.T,T)
  all.N       <- c(all.N,N)
  all.D       <- c(all.D,D)
  all.pi     <- c(all.pi,pi)
}

plot(all.E,type="l")
plot(all.pi,type="l")
plot(all.delta.c,type="l")
lines(all.D,col="red")

#plot(density(all.delta.c-mu))
#lines(density(all.delta.c.no.D-mu),col="red")



