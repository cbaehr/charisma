#Name: Figure1.R
#Author: James Hollyer, Marko Klasnja, and Rocio Titiunik
##Purpose: To generate a graph of the comparative statics on p(.,\bar{nu})
## for all values of psi
#Date: 4/7/2021
#install.packages('foreign')
library(foreign)

#Set parameter values

NL <- 150
NH <- 50
N <- NL+NH
w <- .3
v <- 5
L <- 12
deltaI <- 0.5
b <- 1.5
B <- 10

psi <- c(1:1000)
psi <- psi/2000

# Calculate the relevant incentive compatibilty constraints and thresholds in delta_p


punderbar <- (N - L*psi)/(b*deltaI*(N*((2*L)/2) + NL*L*psi))

deltahatp1 <- (B*v)/((NL+B*NH)*((L*NL)/N) - NL + B*v)
deltahatp2 <- B*v*(NL*(w+punderbar-w*punderbar)-NH)/((NL+B*NH)*((L*psi*NL)/N)-NL)

deltahatp <- matrix(,ncol=1,nrow=1000)

##For lambda*psi<1, the relevant delta is always 1.  Otherwise we
##have the deltas as usual

Nthresh <- NL*(w+punderbar-w*punderbar)-NH

for (i in 1:NROW(psi)){
    if (L*psi[i]>=1){
        if (Nthresh[i] >= 1){
            deltahatp[i] <- deltahatp2[i]
        }
        else {
            deltahatp[i] <- deltahatp1
        }
    }
    else{
        deltahatp[i] <- 1
    }
}
##Set an acutal value of delta, something intermediate

delta <- .2

## Now define the probability high types are promoted

pofnu <- matrix(,ncol=1,nrow=1000)

for (i in 1:NROW(pofnu)){
    if (delta>=deltahatp[i]){
        pofnu[i] <- min((NH-punderbar[i]*NL)/(w*NL),1)
    }
    else{
        pofnu[i] <- 1
    }
}

##Obs. 462 denotes division between equilibria
    

pdf("PsiCompStat.pdf")
plot(x=c(),y=c(), xlim=c(0,0.5), ylim=c(0,1), axes=F, xlab=expression(psi), ylab=expression(p(.,bar(nu))), main=expression(paste("Comparative Statics in ", psi)))
axis(side=1, lwd=2, pos=0)
axis(side=2, lwd=2, pos=0)
lines(y=pofnu, x=psi)
abline(v=psi[462], ylim=c(0,1), lty=2)
text(x=.1, y=.6, "Uncommitted", pos=4)
text(x=.35, y=.6, "Committed", pos=4)
dev.off()


