rm(list = ls())

source('GOLLN_gamlss.R')

# Function to generate regression model with 1 covariate

reg_GOLLN <- function(n=1,beta10,beta11,beta20,beta21,a0,b0){
  x <- c()
  y <- c()
  i <- 1
  while(i <= n){
    x[i] <- rbinom(1,1,0.5)
    mu <- beta10+beta11*x[i]
    sigma <- exp(beta20+beta21*x[i])
    y[i] <- rGOLLN(n=n,mu=mu,sigma=sigma,nu=a0,tau=b0)
    i <- i+1
  }
  z <- data.frame(x,y)
  return(z)
}

## Testing the function
beta10 <- 1.5
beta11 <- 2;
beta20 <- 1.5
beta21 <- 2;
nu <- 0.5; tau <- 0.7
d1 <- reg_GOLLN(150, beta10,beta11,beta20,beta21, nu,tau)
mu <- beta10+beta11*d1$x
sigma <- exp(beta20+beta21*d1$x)
ajuste1 <- gamlss(y~x,sigma.formula = ~x,family = GOLLN,n.cyc=200,data=d1,c.crit=0.01,sigma.start = sigma,
                  mu.start = mu,nu.start = nu,tau.start = tau)
coefAll(ajuste1)

##Initial values
beta10 <- 2.0 ; beta11 <- 1.5 ;beta20<-0.5;beta21 <- 2.5; a0 <- exp(0.6); b0 <- exp(0.4)
inicial <- c(beta10,beta11,beta20,beta21,a0,b0)
inicial2 <- c(beta10,beta11,beta20,beta21,log(a0),log(b0))


# 50
n <- 50 
iter <- 0
r <- 1000 
theta <- matrix(0,r,6)
se <- list()
res.rq = matrix(0,r,n)
i <- 1

set.seed(1311)
while(i<=r){
  dados1 <- reg_GOLLN(n,beta10,beta11,beta20,beta21,a0,b0)
  x <- dados1$x
  y <- dados1$y
  mu1 <- beta10+beta11*dados1$x
  sigma1 <- exp(beta20+beta21*dados1$x)
  ajuste1=try(gamlss(y~x,sigma.formula = ~x,family = GOLLN,n.cyc=200,data=dados1,c.crit=0.1,
                     sigma.start = sigma1, mu.start = mu1,nu.start = a0,tau.start = b0,trace=T))
  if((class(ajuste1)[1] != "try-error")==T){
    teste <- ajuste1$converged
    if(teste == TRUE){
      betas <- c(mu=ajuste1$mu.coefficients ,sigma=ajuste1$sigma.coefficients,nu=ajuste1$nu.coefficients,tau=ajuste1$tau.coefficients)
      theta[i,] <-  betas
      res.rq[i,] = ajuste1$residuals
      se[[i]] <- summary(ajuste1,type='qr')
      i <- i+1
    }
    else{i <- i}
  }
  else{i <- i}
  cat("iteration = ", iter <- iter + 1, i,"\n")
}


theta_50 <- theta
rq_50 <- res.rq
se_50 <- se


# 150
n <- 150 
iter <- 0
r <- 1000 
theta <- matrix(0,r,6)
se <- list()
res.rq = matrix(0,r,n)
i <- 1

set.seed(1311)
while(i<=r){
  dados1 <- reg_GOLLN(n,beta10,beta11,beta20,beta21,a0,b0)
  x <- dados1$x
  y <- dados1$y
  mu1 <- beta10+beta11*dados1$x
  sigma1 <- exp(beta20+beta21*dados1$x)
  ajuste1=try(gamlss(y~x,sigma.formula = ~x,family = GOLLN,n.cyc=200,data=dados1,c.crit=0.1,
                     sigma.start = sigma1, mu.start = mu1,nu.start = a0,tau.start = b0,trace=T))
  if((class(ajuste1)[1] != "try-error")==T){
    teste <- ajuste1$converged
    if(teste == TRUE){
      betas <- c(mu=ajuste1$mu.coefficients ,sigma=ajuste1$sigma.coefficients,nu=ajuste1$nu.coefficients,tau=ajuste1$tau.coefficients)
      theta[i,] <-  betas
      res.rq[i,] = ajuste1$residuals
      se[[i]] <- summary(ajuste1,type='qr')
      i <- i+1
    }
    else{i <- i}
  }
  else{i <- i}
  cat("iteration = ", iter <- iter + 1, i,"\n")
}


theta_150 <- theta
rq_150 <- res.rq
se_150 <- se

# 450
n <- 450 
iter <- 0
r <- 1000 
theta <- matrix(0,r,6)
se <- list()
res.rq = matrix(0,r,n)
i <- 1

set.seed(1311)
while(i<=r){
  dados1 <- reg_GOLLN(n,beta10,beta11,beta20,beta21,a0,b0)
  x <- dados1$x
  y <- dados1$y
  mu1 <- beta10+beta11*dados1$x
  sigma1 <- exp(beta20+beta21*dados1$x)
  ajuste1=try(gamlss(y~x,sigma.formula = ~x,family = GOLLN,n.cyc=200,data=dados1,c.crit=0.1,
                     sigma.start = sigma1, mu.start = mu1,nu.start = a0,tau.start = b0,trace=T))
  if((class(ajuste1)[1] != "try-error")==T){
    teste <- ajuste1$converged
    if(teste == TRUE){
      betas <- c(mu=ajuste1$mu.coefficients ,sigma=ajuste1$sigma.coefficients,nu=ajuste1$nu.coefficients,tau=ajuste1$tau.coefficients)
      theta[i,] <-  betas
      res.rq[i,] = ajuste1$residuals
      se[[i]] <- summary(ajuste1,type='qr')
      i <- i+1
    }
    else{i <- i}
  }
  else{i <- i}
  cat("iteration = ", iter <- iter + 1, i,"\n")
}


theta_450 <- theta
rq_450 <- res.rq
se_450 <- se


# Calculate results


library(ggplot2)


resultado_reg2 <- function(estimativas,n,valores){
  media <- apply(estimativas, 2, mean) 
  sd <- apply(estimativas, 2, sd)
  errop <- sd/sqrt(n) 
  var <- apply(estimativas, 2,var) 
  eqm <- c( var[1]+(media[1]-valores[1])^2,
            var[2]+(media[2]-valores[2])^2,
            var[3]+(media[3]-valores[3])^2,
            var[4]+(media[4]-valores[4])^2,
            var[5]+(media[5]-valores[5])^2,
            var[6]+(media[6]-valores[6])^2
            
            
  )
  vies <- c( (media[1]-valores[1]),
             (media[2]-valores[2]),
             (media[3]-valores[3]),
             (media[4]-valores[4]),
             (media[5]-valores[5]),
             (media[6]-valores[6])
             
             
  )
  parametro <- rep(c('beta10', 'beta11','beta20', 'beta21','a','b'))
  tamanho <- rep(n,6)
  resultado <- cbind(valores,tamanho,parametro, data.frame(media, vies,eqm))
  colnames(resultado) <- c('Valor real','n','Parameters', 'AEs','Biases', 'MSEs')
  return(resultado)
}


t1 <- resultado_reg2(theta_50,50,inicial2)
t2 <- resultado_reg2(theta_150,150,inicial2)
t3 <- resultado_reg2(theta_450,450,inicial2)

# Table 1: Findings from the simulated GOLLN regression model.
tabela_simu <- cbind(t1[,2:6],rep('',6),t2[,4:6],rep('',6),t3[,4:6])
library(xtable)
xtable(tabela_simu,digits = 4)


##'
##'Residuos
##'


n <- (50*1000)
df <- data.frame(theoretical.quant=qnorm(1:n/(n+1)),sample.quant=sort(rq_50))

g <-  ggplot(df , aes(x=theoretical.quant, y=sample.quant)) + geom_point()+xlab('Theoretical quantile')+ylab('Sample quantile')+ xlim(-3,3)+ ylim(-3.5,3.5)

g+geom_abline(aes(slope = 1, intercept = 0), col='gray',size=1.5)+theme_bw()+
  theme(axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))


##150

n <- (150*1000)
df <- data.frame(theoretical.quant=qnorm(1:n/(n+1)),sample.quant=sort(rq_150))

g <-  ggplot(df , aes(x=theoretical.quant, y=sample.quant)) + geom_point()+xlab('Theoretical quantile')+ylab('Sample quantile')+ xlim(-3,3)+ ylim(-3.5,3.5)

g+geom_abline(aes(slope = 1, intercept = 0), col='gray',size=1.5)+theme_bw()+
  theme(axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))


##450

n <- (450*1000)
df <- data.frame(theoretical.quant=qnorm(1:n/(n+1)),sample.quant=sort(rq_450))

g <-  ggplot(df , aes(x=theoretical.quant, y=sample.quant)) + geom_point()+xlab('Theoretical quantile')+ylab('Sample quantile')+ xlim(-3,3)+ ylim(-3.5,3.5)

g+geom_abline(aes(slope = 1, intercept = 0), col='gray',size=1.5)+theme_bw()+
  theme(axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))
