# This is for question 4
# Use a discrete prior distribution on θ
theta.g = seq( 0.5, 1 , length=501 )  # grid of thetas used. 
# Prior
PDiscreteTheta <- function(theta.g){ # get pdf for a discrete theta, assuming all values are equally likely
  return( rep(1/length(theta.g),length(theta.g)) )
} 
# Plot
pdf.g <- PDiscreteTheta(theta.g)
ymax <- 1.1*max(pdf.g)
xlab <- expression(theta)
ylab <- expression(paste(italic("p"),"(",theta,")", sep=""))
main <- "Prior"
coluse <- 1
plot(pdf.g~theta.g, type = "h", lwd = 5, main = main, xlim = c(0, 1), ylim = c(0, ymax), ylab = ylab, col = coluse, xlab = xlab)
abline(h=0)
 
#dev.off()

# Compute the Binomial likelihood at each value of Theta:
n <- 100
y <- 10
PDataGivenTheta <- function(theta, nsuccess, samplesize) dbinom(nsuccess, samplesize, theta) 
# Plot the likelihood function for the grid of thetas
pdf.g <- PDataGivenTheta(theta = theta.g, nsuccess = y, samplesize = n)
ymax <- 1.1*max(pdf.g)
ylab <- expression(paste(italic("p"),"(y|",theta,")", sep=""))
main <- "Likelihood"
coluse <- 1
plot(pdf.g~theta.g, type = "h", lwd = 5, main = main, xlim = c(0, 1), ylim = c(0, ymax), ylab = ylab, col = coluse, xlab = xlab)
abline(h=0)
 
#dev.off()

# Compute pData = p(y) and the posterior via Bayes' rule:
PData <- function(theta, nsuccess, samplesize) 
  sum( PDataGivenTheta(theta, nsuccess, samplesize) * PDiscreteTheta(theta) )
PThetaGivenData <- function(theta, nsuccess, samplesize) 
  PDataGivenTheta(theta, nsuccess, samplesize) * PDiscreteTheta(theta) / PData(theta, nsuccess, samplesize)
PData(theta = theta.g, nsuccess = y, samplesize = n) 
## [1] 1.817497e-19
# Plot the posterior for the grid of thetas
pdf.g <- postPdf.g <- PThetaGivenData(theta = theta.g, nsuccess = y, samplesize = n)
ymax <- 1.1*max(pdf.g)
ylab <- expression(paste(italic("p"),"(",theta,"|y)", sep=""))
main <- "Posterior"
coluse <- 1
plot(pdf.g~theta.g, type = "h", lwd = 5, main = main, xlim = c(0, 1), ylim = c(0, ymax), ylab = ylab, col = coluse, xlab = xlab)
abline(h=0)
 
#dev.off()

# get posterior mean
sum(theta.g * postPdf.g)
## [1] 0.5055842
