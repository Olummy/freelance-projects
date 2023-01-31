## Header #==================

# ---
# title: "Transmuted New Weibull-Pareto Distribution"
# subtitle: "Mathematical Statistics"
# author: "Ayodele Ayodeji Theophilus"
# date: "2/16/2022"
# ---


## PDF #=============================================

x <- seq(0, 3, by = 0.0001)
beta <- 1.5
delta <- 0.5
theta <- 0.5
lambda <- 0.0

tnwp.pdf <- function(x, beta=1.5, delta=0.5, theta=0.5, lambda=0.0){
  (beta*delta/theta) * ((x/theta)^(beta-1))*exp(-(delta*(x/theta)^beta)) *(1-lambda + 2*lambda*exp(-(delta*(x/theta)^ beta)))
}


 par(mfrow = c(2,2), cex.axis = 0.5, cex.lab = 0.5,
     mar = c(3.5, 3.5, 3.5, 3.5) - 2.5)

plot(x, tnwp.pdf(x,1.5,0.5,0.5,0.0), type = "l", xlab = "x", 
     ylab = "density", ylim = c(0, 1.7), 
     main = "", cex.axis = 0.5, cex.lab = 0.5, ann = TRUE)

lines(x, tnwp.pdf(x,1.5,0.5,0.5,0.2), col = 2)
lines(x, tnwp.pdf(x,1.5,0.5,0.5,0.4), col = 3)
lines(x, tnwp.pdf(x,1.5,0.5,0.5,0.6), col = 4)
lines(x, tnwp.pdf(x,1.5,0.5,0.5,0.8), col = 5)
lines(x, tnwp.pdf(x,1.5,0.5,0.5,1.0), col = 6)
legend("topright", cex=0.7, c(expression(paste(beta, "=1.5, ",
                                                delta, "=0.5, ",
                                                theta, "=0.5, ",
                                                lambda, "=0.0"),
                                                paste(beta, "=1.5, ",
                                                      delta, "=0.5, ",
                                                      theta, "=0.5, ",
                                                      lambda, "=0.2"),
                                                paste(beta, "=1.5, ",
                                                      delta, "=0.5, ",
                                                      theta, "=0.5, ",
                                                      lambda, "=0.4"),
                                                paste(beta, "=1.5, ",
                                                      delta, "=0.5, ",
                                                      theta, "=0.5, ",
                                                      lambda, "=0.6"),
                                                paste(beta, "=1.5, ",
                                                      delta, "=0.5, ",
                                                      theta, "=0.5, ",
                                                      lambda, "=0.8"),
                                                paste(beta, "=1.5, ",
                                                      delta, "=0.5, ",
                                                      theta, "=0.5, ",
                                                      lambda, "=1.0"))),
       col = c(1,2,3,4,5,6),
       lty = c(1,1,1,1,1,1), 
       lwd = c(2,2,2,2,2,2), 
       horiz = FALSE,
       bg = "grey96")


####========== 2nd plot


plot(x, tnwp.pdf(x,1.5,0.5,0.5,0.0), type = "l", xlab = "x", 
     ylab = "density", ylim = c(0, 1.5))
lines(x, tnwp.pdf(x,1.5,0.5,0.5,-0.2), col = 2)
lines(x, tnwp.pdf(x,1.5,0.5,0.5,-0.4), col = 3)
lines(x, tnwp.pdf(x,1.5,0.5,0.5,-0.6), col = 4)
lines(x, tnwp.pdf(x,1.5,0.5,0.5,-0.8), col = 5)
lines(x, tnwp.pdf(x,1.5,0.5,0.5,-1.0), col = 6)
legend("topright",cex=0.7, c(expression(paste(beta, "=1.5, ",
                                                           delta, "=0.5, ",
                                                           theta, "=0.5, ",
                                                           lambda, "=0.0"),
                                                     paste(beta, "=1.5, ",
                                                           delta, "=0.5, ",
                                                           theta, "=0.5, ",
                                                           lambda, "=-0.2"),
                                                     paste(beta, "=1.5, ",
                                                           delta, "=0.5, ",
                                                           theta, "=0.5, ",
                                                           lambda, "=-0.4"),
                                                     paste(beta, "=1.5, ",
                                                           delta, "=0.5, ",
                                                           theta, "=0.5, ",
                                                           lambda, "=-0.6"),
                                                     paste(beta, "=1.5, ",
                                                           delta, "=0.5, ",
                                                           theta, "=0.5, ",
                                                           lambda, "=-0.8"),
                                                     paste(beta, "=1.5, ",
                                                           delta, "=0.5, ",
                                                           theta, "=0.5, ",
                                                           lambda, "=-1.0"))),
       col = c(1,2,3,4,5,6),
       lty = c(1,1,1,1,1,1), 
       lwd = c(2,2,2,2,2,2), 
       horiz = F,
       bg = "grey96")


####========== 3rd plot


plot(x, tnwp.pdf(x,3.0,5.0,2.0,0.0), type = "l", xlab = "x", 
     ylab = "density", ylim = c(0, 1.4))
lines(x, tnwp.pdf(x,3.0,5.0,2.0,0.2), col = 2)
lines(x, tnwp.pdf(x,3.0,5.0,2.0,0.4), col = 3)
lines(x, tnwp.pdf(x,3.0,5.0,2.0,0.6), col = 4)
lines(x, tnwp.pdf(x,3.0,5.0,2.0,0.8), col = 5)
lines(x, tnwp.pdf(x,3.0,5.0,2.0,1.0), col = 6)
legend("topright",cex=0.7, c(expression(paste(beta, "=3.0, ",
                                                           delta, "=5.0, ",
                                                           theta, "=2.0, ",
                                                           lambda, "=0.0"),
                                                     paste(beta, "=3.0, ",
                                                           delta, "=5.0, ",
                                                           theta, "=2.0, ",
                                                           lambda, "=0.2"),
                                                     paste(beta, "=3.0, ",
                                                           delta, "=5.0, ",
                                                           theta, "=2.0, ",
                                                           lambda, "=0.4"),
                                                     paste(beta, "=3.0, ",
                                                           delta, "=5.0, ",
                                                           theta, "=2.0, ",
                                                           lambda, "=0.6"),
                                                     paste(beta, "=3.0, ",
                                                           delta, "=5.0, ",
                                                           theta, "=2.0, ",
                                                           lambda, "=0.8"),
                                                     paste(beta, "=3.0, ",
                                                           delta, "=5.0, ",
                                                           theta, "=2.0, ",
                                                           lambda, "=1.0"))),
       col = c(1,2,3,4,5,6),
       lty = c(1,1,1,1,1,1), 
       lwd = c(2,2,2,2,2,2), 
       horiz = F,
       bg = "grey96")




####========== 4th plot


plot(x, tnwp.pdf(x,0.4,0.5,0.0,-1.0), type = "l", xlab = "x", 
     ylab = "density", ylim = c(0, 1.8))
lines(x, tnwp.pdf(x,0.9,1.2,1.1,0.6), col = 2)
lines(x, tnwp.pdf(x,4.6,2.9,2.8,-0.5), col = 3)
lines(x, tnwp.pdf(x,4.0,1.9,1.5,0.2), col = 4)
lines(x, tnwp.pdf(x,5.5,5.0,6.2,1.0), col = 5)
lines(x, tnwp.pdf(x,5.9,6.2,7.5,1.0), col = 6)
legend("topright",cex=0.7, c(expression(paste(beta, "=0.4, ",
                                                           delta, "=0.5, ",
                                                           theta, "=0.0, ",
                                                           lambda, "=-1.0"),
                                                     paste(beta, "=0.9, ",
                                                           delta, "=1.2, ",
                                                           theta, "=1.1, ",
                                                           lambda, "=0.6"),
                                                     paste(beta, "=4.6, ",
                                                           delta, "=2.9, ",
                                                           theta, "=2.8, ",
                                                           lambda, "=-0.5"),
                                                     paste(beta, "=4.0, ",
                                                           delta, "=1.9, ",
                                                           theta, "=1.5, ",
                                                           lambda, "=0.2"),
                                                     paste(beta, "=5.5, ",
                                                           delta, "=5.0, ",
                                                           theta, "=6.2, ",
                                                           lambda, "=1.0"),
                                                     paste(beta, "=5.9, ",
                                                           delta, "=6.2, ",
                                                           theta, "=7.5, ",
                                                           lambda, "=1.0"))),
       col = c(1,2,3,4,5,6),
       lty = c(1,1,1,1,1,1), 
       lwd = c(2,2,2,2,2,2), 
       horiz = F,
       bg = "grey96")
                                                
                                                
                                                
                                        

dev.off()

## ==== R-code for the CDF plot =========================

x <- seq(0,10,0.025)

cd <- function(x,beta,delta,theta,lambda){
  
  (1-exp(-(delta*(x/theta)^beta)))*(1+lambda*(exp(-(delta*(x/theta)^beta))))
  
  }


c1 <- cd(x,beta=1,delta=2,theta=1,lambda=1) 

plot(x,c1, main="CDF of Transmuted Weibull Pareto Distribution",
     type="n", xlab = "x",ylab = "F(x)", cex.main = 0.8, cex.lab = 0.7,
     family  = "mono", cex.axis = 0.7) 

lines(x, cd(x,beta=1,delta=2,theta=1,lambda=1), col="red",lwd=2,lty=1) 

lines(x, cd(x,beta=2,delta=2,theta=1,lambda=0),col="blue",lwd=2,lty=1) 

lines(x, cd(x,beta=3,delta=2,theta=1.5,lambda=0.5),col="green",lwd=2,lty=1) 

lines(x, cd(x,beta=4,delta=2,theta=1.5,lambda=1),col="black",lwd=2,lty=1) 

legend("bottomright",cex=0.8, c("a=1,b=2,c=1,d=1","a=2,b=2,c=1,d=0","a=3,b=2,c=1.5,d=0.5","a=4,b=2,c=1.5,d=1"),horiz=F, lty=c(1,1,1,1),lwd=c(2,2,2,2), col=c("red","blue","green","black"), bg="grey96") 


## ====R-code for the Survival Plot ======

x <- seq(0,10,0.025) 

h <- function(x, beta, delta, theta, lambda){
  
  (1-((1-exp(-(delta*(x/theta)^beta)))*(1+(lambda*(exp(-(delta*(x/theta)^beta)))))))
  
} 

x1 <- h(x,beta=1,delta=2,theta=1,lambda=1) 

x2 <- h(x,beta=2,delta=2,theta=1,lambda=0) 

x3 <- h(x,beta=3,delta=2,theta=1.5,lambda=0.5) 

x4 <- h(x,beta=4,delta=2,theta=1.5,lambda=1)

plot(x, x1, main = "Survival Plot of Transmuted Weibull Pareto Dist.",
     ylim = c(0,1),type = "n", xlab = "x",ylab = "S(x)",
     cex.main = 0.8, cex.lab = 0.7,
     family  = "mono", cex.axis = 0.7)

lines(x,x1,col="red",lwd=2,lty=1) 

lines(x,x2,col="blue",lwd=2,lty=1) 

lines(x,x3,col="black",lwd=2,lty=1) 

lines(x,x4,col="green",lwd=2,lty=1) 

legend("topright",cex=1, c("a=1,b=2,c=1,lambda=1","a=2,b=2,c=1,d=0","a=3,b=2,c=1.5,d=0.5","a=4,b=2,c=1.5,d=1"),horiz=F, lty=c(1,1,1,1),lwd=c(2,2,2,2), col=c("red","blue","black","green"), bg="grey96") 


## ==== R-code for the Hazard Plot 

x <- seq(0,10,0.025) 

ha <- function(x,beta,delta,theta,lambda){
  
  ((beta*delta/theta)*((x/theta)^(beta-1))*(1-lambda+2*lambda*exp(-(delta*(x/theta)^beta))))/(lambda*exp(-(delta*(x/theta)^beta))+1-lambda)
  
  } 

x1 <- ha(x,beta=0.5,delta=3,theta=2,lambda=-1) 

x2 <- ha(x,beta=1,delta=3,theta=2,lambda=0) 

x3 <- ha(x,beta=1.5,delta=3,theta=2,lambda=0.5) 

x4 <- ha(x,beta=2,delta=3,theta=2,lambda=1) 

plot(x,x1, main="Hazard Plot of Transmuted Weibull Pareto Dist.",type="n",xlab = "x",ylab = "H(x)",cex.main = 0.8, cex.lab = 0.7,
     family  = "mono", cex.axis = 0.7) 

lines(x,x1,col="red",lwd=2,lty=1)

lines(x,x2,col="blue",lwd=2,lty=1)

lines(x,x3,col="green",lwd=2,lty=1) 

lines(x,x4,col="black",lwd=2,lty=1) 

legend("topright",cex=0.8, c("a=0.5,b=3,c=2,d=-1","a=1,b=3,c=2,d=0","a=1.5,b=3,c=2,d=0.5","a=2,b=3,c=2,d=1"),horiz=F, lty=c(1,1,1,1),lwd=c(2,2,2,2), col=c("red","blue","green","black"), bg="grey96")


## ===R-code for the Maximum likelihood estimation =====

x <- c(1.7,2.2,14.4,1.1,0.4,20.6, 5.3 ,0.7 ,1.4 ,18.7, 8.5, 25.5, 11.6, 14.1, 22.1, 1.1, 0.6, 2.2, 39.0, 0.3, 15.0, 11.0, 7.3, 22.9, 0.9, 1.7, 7.0, 20.1, 0.4, 2.8, 14.1, 9.9, 5.6, 30.8, 13.3, 4.2, 25.5, 3.4, 11.9, 21.5, 1.5 ,2.5 ,27.4, 1.0, 27.1, 20.2, 16.8, 5.3 ,1.9 ,10.4, 13.0, 10.7, 12.0, 30.0, 9.3, 3.6, 2.5, 27.6, 14.4, 36.4, 1.7, 2.7, 37.6, 64.0, 1.7, 9.7, 0.1, 27.5, 1.1, 2.5, 0.6, 27.0) 


library(maxLik) 
options(scipen = 999, digits = 3)

loglik <- function(a) {
  
  beta <- a[1]
  delta <- a[2]
  theta <- a[3]
  lambda <- a[4]
  
log_lik <-   n*log(a[1])+n*log(a[2])-n*log(a[3])-(a[2])*sum(x/a[3])^a[1]+(a[1]-1)*sum(log(x/a[3]))+sum(log(1 - a[4] + 2 * a[4]*exp(-(a[2]*(x/a[3])^a[1]))))
  
  } 

n <- length(x) 

mle_estimate <- maxLik(loglik, start = c(3.421, 8.6231, 2.25, -0.4608)) 

summary(mle_estimate)
