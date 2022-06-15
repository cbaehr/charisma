#Name: Figure2.R
#Author: James Hollyer, Marko Klasnja and Rocio Titiunik
#Purpose: To generate a graph of the regions where various equilibria (ideologial, committed, uncommitted) exist #in alpha-delta space, for different realization of volatility.
#Date: 4/7/2021
#install.packages('foreign')
library(foreign)

# Set parameter values

NL <- 150
NH <- 50
N <- NL+NH
w <- .3
v <- 5
L <- 12
deltaI <- 0.5
b <- 1.5
B <- 10
psi <- 0.12

baralpha <- (N-L*psi)/(L*psi*(N-1))

alpha <- c(1:1000)
alpha <- alpha/1000

for (i in 1:1000){
  if (alpha[i]>baralpha){
    alpha[i] <- NA
  }
}

punderbar <- (N - L*psi - alpha*(N-1)*L*psi)/(b*deltaI*((N*(2+L)/2) + NL*L*psi))

deltahatp1 <- (B*v)/((NL+B*NH)*((L*NL)/N) - NL + B*v)
deltahatp2 <- B*v*(NL*(w+punderbar-w*punderbar)-NH)/((NL+B*NH)*((L*psi*NL)/N)-NL)

deltahatp <- matrix(,ncol=1,nrow=1000)

##Compute relevant thresholds in delta

Nthresh <- NL*(w+punderbar-w*punderbar)-NH

for (i in 1:NROW(alpha)){
    if (is.na(alpha[i])==F){
        if (Nthresh[i] >= 1){
            deltahatp[i] <- deltahatp2[i]
        }
        else {
            deltahatp[i] <- deltahatp1
        }
    }
}

for (i in 1:NROW(deltahatp)){
    if (is.na(deltahatp[i])==F){
        if (deltahatp[i]>1){
            deltahatp[i] <- 1
        }
    }
}
        

## Plot leftmost (low psi) graph

pdf("AlphaDeltaSpacePlotLowPsi.pdf")
plot(x=c(),y=c(), xlim=c(0,1), ylim=c(0,1), axes=F, xlab=expression(alpha), ylab=expression(delta[p]), main=expression(paste("Low ", psi)))
axis(side=1, lwd=2, pos=0)
axis(side=2, lwd=2, pos=0)
lines(y=deltahatp, x=alpha)
abline(v=baralpha, ylim=c(0,1))
text(x=.4, y=.8, "Committed", pos=4)
text(x=.05, y=.2, "Uncommitted", pos=4)
text(x=.82, y=.5, "Loyal", pos=4)
text(x=0.05, y=deltahatp[200], expression(bar(delta[P])), pos=4)
text(x=baralpha+.025, y=0, expression(bar(alpha)), pos=3)
dev.off()

## Now, repeat for higher values of psi, for rightmost graph

psi <- .2

baralpha <- (N-L*psi)/(L*psi*(N-1))

alpha <- c(1:1000)
alpha <- alpha/1000

for (i in 1:1000){
  if (alpha[i]>baralpha){
    alpha[i] <- NA
  }
}

punderbar <- (N - L*psi - alpha*(N-1)*L*psi)/(b*deltaI*((N*(2+L)/2) + NL*L*psi))

deltahatp1 <- (B*v)/((NL+B*NH)*((L*NL)/N) - NL + B*v)
deltahatp2 <- B*v*(NL*(w+punderbar-w*punderbar)-NH)/((NL+B*NH)*((L*psi*NL)/N)-NL)

deltahatp <- matrix(,ncol=1,nrow=1000)


Nthresh <- NL*(w+punderbar-w*punderbar)-NH

for (i in 1:NROW(alpha)){
    if (is.na(alpha[i])==F){
        if (Nthresh[i] >= 1){
            deltahatp[i] <- deltahatp2[i]
        }
        else {
            deltahatp[i] <- deltahatp1
        }
    }
}

for (i in 1:NROW(deltahatp)){
    if (is.na(deltahatp[i])==F){
        if (deltahatp[i]>1){
            deltahatp[i] <- 1
        }
    }
}
        


pdf("AlphaDeltaSpacePlotHighPsi.pdf")
plot(x=c(),y=c(), xlim=c(0,1), ylim=c(0,1), axes=F, xlab=expression(alpha), ylab=expression(delta[p]), main=expression(paste("High ", psi)))
axis(side=1, lwd=2, pos=0)
axis(side=2, lwd=2, pos=0)
lines(y=deltahatp, x=alpha)
abline(v=baralpha, ylim=c(0,1))
text(x=.15, y=.8, "Committed", pos=4)
text(x=.01, y=.1, "Uncommitted", pos=4)
text(x=.6, y=.5, "Loyal", pos=4)
text(x=0.05, y=deltahatp[100], expression(bar(delta[P])), pos=4)
text(x=baralpha+.025, y=0, expression(bar(alpha)), pos=3)
dev.off()
