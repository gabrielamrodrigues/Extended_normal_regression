
rm(list=ls(all=TRUE))

library(ggplot2)
library(dplyr)
library(readxl)
library(RColorBrewer)
library(moments)
library(xtable)

source('GOLLN_gamlss.R')

new_data <- read_excel("data3.xlsx")

y <- new_data$height_45
x1 <- as.factor(new_data$fertilizer)
x2 <- as.factor(new_data$varieties)
x3 <- new_data$ages
data3 <- data.frame(y,x1,x2,x3)
plot(y~x1)
plot(y~x2)
plot(y~as.factor(x3))


## Descriptive analysis


figure11c <-  ggplot(data3, aes(x=x1, y=y)) + 
  geom_boxplot(outlier.color = 1,outlier.size = 2,fill='gray') + theme_bw()+xlab("cyl")+ labs(title="",x="Nitrogen fertilizer rates", y = "Plant height (cm) 45 DAT")+scale_fill_brewer(palette="BuPu")
figure11c+ theme(legend.position="none",axis.text=element_text(size=10),strip.text.x = element_text(size = 14), axis.title=element_text(size=12),plot.title = element_text(size=12))+stat_summary(fun.y=mean, geom="point", shape=15, size=2, color="1")

figure11a <-  ggplot(data3, aes(x=x2, y=y)) + 
  geom_boxplot(outlier.color = 1,outlier.size = 2,fill='gray') + theme_bw()+xlab("cyl")+ labs(title="",x="Rice varieties", y = "Plant height (cm) 45 DAT")+scale_fill_brewer(palette="BuPu")
figure11a + theme(legend.position="none",axis.text=element_text(size=10),strip.text.x = element_text(size = 14), axis.title=element_text(size=12),plot.title = element_text(size=12))+stat_summary(fun.y=mean, geom="point", shape=15, size=2, color="1")


figure11b <-  ggplot(data3, aes(x=as.factor(x3), y=y)) + 
  geom_boxplot(outlier.color = 1,outlier.size = 2,fill='gray') + theme_bw()+xlab("cyl")+ labs(title="",x="Seedling transplanting ages", y = "Plant height (cm) 45 DAT")+scale_fill_brewer(palette="BuPu")
figure11b + theme(legend.position="none",axis.text=element_text(size=10),strip.text.x = element_text(size = 14), axis.title=element_text(size=12),plot.title = element_text(size=12))+stat_summary(fun.y=mean, geom="point", shape=15, size=2, color="1")

figure1c <- ggplot(data3,aes(x=y)) +geom_histogram(mapping = aes(x = y, y=..density..), binwidth=5.3, color = '#999999', fill = "#CCCCCC")+geom_density(size=1.1)+ylim(0,0.04)+xlim(21,83)
figure1c+xlab('y')+ylab('Density')+theme_bw()+  theme(legend.position = c(0.9,0.89),legend.background = element_rect(fill = "transparent"),axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))

##'
##'Marginal analysis
##'

fit1 <- gamlss(y~1,family = NO,n.cyc=5000,c.crit=0.001)

fit2en <- gamlss(y~1,family = GOLLN,n.cyc=5000,c.crit=0.001,tau.fix = T, tau.start =1)

fit3olln <- gamlss(y~1,family = GOLLN,n.cyc=5000,c.crit=0.001, nu.fix = T, nu.start =1)

fit4golln <- gamlss(y~1,family = GOLLN,n.cyc=5000,c.crit=0.001)

fit5 <- gamlss(y~1,family = SN1,n.cyc=5000,c.crit=0.001)


fit1 <- fit1
fit2 <- fit2en
fit3 <- fit3olln
fit4 <- fit4golln
fit5skew <- fit5


figure12a <- ggplot(data3,aes(x=y)) +geom_histogram(mapping = aes(x = y, y=..density..), binwidth=5.3, color = '#999999', fill = "#CCCCCC")+ylim(0,0.04)+xlim(22,82) #+geom_density(size=1.1)
figure12a+xlab('y')+ylab('Density')+theme_bw()+stat_function(fun = dGOLLN, args = list(mu=fit4$mu.fv[1], sigma =fit4$sigma.fv[1], nu=fit4$nu.fv[1], tau=fit4$tau.fv[1]),size=1.1,aes(colour='GOLLN'))+
  stat_function(fun = dGOLLN, args = list(mu=fit3$mu.fv[1],sigma=fit3$sigma.fv[1],nu=1,tau=fit3$tau.fv[1]),size=1.1,aes(colour='OLLN'))+
  stat_function(fun = dGOLLN, args = list(mu=fit2$mu.fv[1],sigma=fit2$sigma.fv[1],nu=fit2$nu.fv[1],tau=1),size=1.1,aes(colour='Exp-N'))+
  stat_function(fun = dGOLLN, args = list(mu=fit1$mu.fv[1], sigma = fit1$sigma.fv[1], nu=1, tau=1),size=1.1,aes(colour='Normal'))+
  stat_function(fun = dSN1, args = list(mu=fit5skew$mu.fv[1], sigma = fit5skew$sigma.fv[1], nu=fit5skew$nu.fv[1]),size=1.1,aes(colour='Skew-Normal'))+
  scale_color_manual(breaks = c('GOLLN', 'Exp-N',"OLLN",'Normal','Skew-Normal'),values = c('red', "green", 'blue','orange',6))+labs(colour = "")+theme(legend.position = c(0.85,0.83),legend.background = element_rect(fill = "transparent"),axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))



figure12b <- ggplot(data3,aes(x=y)) +
  stat_ecdf(geom = "step",size=1)+
  stat_ecdf(geom = "point",size=2)

figure12b+xlab('y')+ylab('Cumulative density function')+theme_bw()+stat_function(fun = pGOLLN, args = list(mu=fit4$mu.fv[1], sigma =fit4$sigma.fv[1], nu=fit4$nu.fv[1], tau=fit4$tau.fv[1]),size=1.1,aes(colour='GOLLN'))+
  stat_function(fun = pGOLLN, args = list(mu=fit3$mu.fv[1],sigma=fit3$sigma.fv[1],nu=1,tau=fit3$tau.fv[1]),size=1.1,aes(colour='OLLN'))+
  stat_function(fun = pGOLLN, args = list(mu=fit2$mu.fv[1],sigma=fit2$sigma.fv[1],nu=fit2$nu.fv[1],tau=1),size=1.1,aes(colour='Exp-N'))+
  stat_function(fun = pGOLLN, args = list(mu=fit1$mu.fv[1], sigma = fit1$sigma.fv[1], nu=1, tau=1),size=1.1,aes(colour='Normal'))+
  stat_function(fun = pSN1, args = list(mu=fit5skew$mu.fv[1], sigma = fit5skew$sigma.fv[1], nu=fit5skew$nu.fv[1]),size=1.1,aes(colour='Skew-Normal'))+scale_color_manual(breaks = c('GOLLN', 'Exp-N',"OLLN",'Normal','Skew-Normal'),values = c('red', "green", 'blue','orange',6))+labs(colour = "")+theme(legend.position = c(0.15,0.85),legend.background = element_rect(fill = "transparent"),axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))


##'Selection of covariates 
##'

#Normal model

m0.normal <- gamlss(y~1,family = NO,n.cyc=5000,c.crit=0.001)
normal.select <- stepGAICAll.A(m0.normal, scope=list(lower=~1,upper=~x1+x2+x3))

#exp.normal

m0.exp <- gamlss(y~1,family = GOLLN,n.cyc=5000,c.crit=0.1,tau.fix = T, tau.start =1)
exp.select <- stepGAICAll.A(m0.exp, scope=list(lower=~1,upper=~x1+x2+x3), tau.try = FALSE,nu.try = FALSE)

#odd

m0.odd <- gamlss(y~1,family = GOLLN,n.cyc=5000,c.crit=0.1, nu.fix = T, nu.start =1)
odd.select <- stepGAICAll.A(m0.odd, scope=list(lower=~1,upper=~x1+x2+x3), tau.try = FALSE,nu.try = FALSE)

#golln

m0.golln <- gamlss(y~1,family = GOLLN,n.cyc=5000,c.crit=0.1)
golln.select <- stepGAICAll.A(m0.golln, scope=list(lower=~1,upper=~x1+x2+x3),tau.try = FALSE,nu.try = FALSE)


##skew Normal model
m0.snormal <- gamlss(y~1,family = SN1,n.cyc=5000,c.crit=0.001)
snormal.select <- stepGAICAll.A(m0.snormal, scope=list(lower=~1,upper=~x1+x2+x3),nu.try = FALSE)


##Final models

normal.final <- normal.final <- gamlss(y~x2+x3,sigma.formula=~x2, amily = NO,n.cyc=5000,c.crit=0.001)

exp.final <- gamlss(y~x3,sigma.formula=~x2,family = GOLLN,n.cyc=5000,c.crit=0.001,tau.fix = T, tau.start =1)

odd.final <- gamlss(y~x3 +x2+ x1,family = GOLLN,n.cyc=5000,c.crit=0.001, nu.fix = T, nu.start =1)

golln.final <- gamlss(y~x2+x3,sigma.formula=~x2, family = GOLLN,n.cyc=5000,c.crit=0.001)

skew.final <- gamlss(formula = y ~ x3, sigma.formula = ~x2, family = SN1,  
                     n.cyc = 5000, c.crit = 0.001)


##AIC e GD - Table 8


tab1 <- cbind(c(AIC(fit4golln), AIC(fit3olln), AIC(fit2en), AIC(fit1)),
              c(fit4golln$G.deviance, fit3olln$G.deviance,fit2en$G.deviance,fit1$G.deviance),
              c(AIC(golln.final), AIC(odd.final), AIC(exp.final), AIC(normal.final)),
              c(golln.final$G.deviance, odd.final$G.deviance, exp.final$G.deviance, normal.final$G.deviance))

rownames(tab1) <- c('GOLLN','OLLN','Exp-N','Normal')
colnames(tab1) <- c('AIC','GD','AIC','GD')
library(xtable)
xtable(tab1)

AIC(fit5skew)
fit5skew$G.deviance

AIC(skew.final)
skew.final$G.deviance
# 958.9633 & 952.9633$ 902.3345 & 892.3345


# Table 9: Findings from the fitted GOLLN regression model.
table9 <- summary(golln.final,type='qr')
xtable(table9,digits = 4)




## Residuals 

#Figure 13

wp(golln.final,pch=20,col='darkgrey',ylim.all = 1.5)
wp(odd.final,pch=20,col='darkgrey',ylim.all = 1.5)
wp(exp.final,pch=20,col='darkgrey',ylim.all = 1.5)
wp(normal.final,pch=20,col='darkgrey',ylim.all = 1.5)
wp(skew.final,pch=20,col='darkgrey',ylim.all = 1.5)



# Likelihood distance

dumm <- model.matrix(~x2)
x2 <- dumm[,2]


golln_loglik <- function(par){
  n <- length(y)
  beta10 <- par[1]
  beta11 <- par[2]
  beta12 <- par[3]
  beta20 <- par[4]
  beta21 <- par[5]
  beta30 <- par[6]
  beta40 <- par[7]
  mu0 <- beta10+beta11*x2+beta12*x3
  sigma0 <- exp(beta20+beta21*x2)
  nu0 <- exp(beta30)
  tau0 <- exp(beta40)
  f <- dGOLLN(y, mu=mu0, sigma=sigma0, nu=nu0,tau=tau0,log=F)
  loglik <- sum(log(f))
  return(loglik)
}


model <- golln.final
theta <- t(as.numeric(c(model$mu.coefficients,model$sigma.coefficients,model$nu.coefficients,model$tau.coefficients)))
inf <- -optimHess(theta,golln_loglik)

likd <- c()
cook <- c()

iter <- 0

for(i in 1:length(y)){
  y1=y[-i]
  x2i=x2[-i]
  x3i=x3[-i]
  mod <- gamlss(y1~x2i+x3i,sigma.formula=~x2i,family=GOLLN(),trace=F,mu.start = model$mu.fv[-i], sigma.start = model$sigma.fv[-i],
                c.crit=0.1, n.cyc=2000)
  lik = logLik(mod)
  likd <- c(likd,lik)
  theta.i <- t(as.numeric(c(mod$mu.coefficients,mod$sigma.coefficients,mod$nu.coefficients,mod$tau.coefficients)))
  cook[i] <- (theta.i-theta)%*%inf%*%t(theta.i-theta)
  cat("iteration = ", iter <- iter + 1, i,"\n")
}

n <- golln.final$N
df <- data.frame(Index = 1:n,  Cook = abs(cook))
df$group <- factor(ifelse(df$Cook > 10, 1, 0))

#figure 15c 

cook.plot <- ggplot(df, aes(Index, Cook, color=group, group=group)) +
  geom_point(size=3) +
  geom_segment(aes(Index, xend=Index, 0, yend=Cook, color=group), data=df)  +
  scale_color_manual(values=c("black", "#666666")) +
  ylab('|Cook distance|') +ylim(0,30)+theme_bw()
cook.plot+theme(legend.position="none",axis.title = element_text( size = (14)),axis.text = element_text(size=13))


df <- data.frame(Index = 1:n,  Cook = abs(distance))
df$group <- factor(ifelse(df$Cook > 35, 1, 0))

#figure 15d 

like.plot <- ggplot(df, aes(Index, Cook, color=group, group=group)) +
  geom_point(size=3) +
  geom_segment(aes(Index, xend=Index, 0, yend=Cook, color=group), data=df)  +
  scale_color_manual(values=c("black", "red1")) +
  ylab('|Likelihood distance|') +ylim(0,30)+theme_bw()
like.plot+theme(legend.position="none",axis.title = element_text( size = (14)),axis.text = element_text(size=13))
