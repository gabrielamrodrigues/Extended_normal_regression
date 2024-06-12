rm(list=ls(all=TRUE))

library(readxl)
library(ggplot2)
library(dplyr)
library(xtable)

source('GOLLN_gamlss.R')

data1 <-  read_excel("data1.xlsx")
y <- data1$y


figure1a <- ggplot(data1,aes(x=y)) +geom_histogram(mapping = aes(x = y, y=..density..), binwidth=5.1, color = '#999999', fill = "#CCCCCC")+geom_density(size=1.1)
figure1a+xlab('x')+ylab('Density')+theme_bw()+xlim(55,110)+ylim(0,0.04)+
  theme(legend.position = c(0.1,0.89),legend.background = element_rect(fill = "transparent"),axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))

#fits

fit1 <- gamlss(y~1,family = NO,n.cyc=5000,c.crit=0.01)

fit3 <- gamlss(y~1,family = GOLLN,n.cyc=5000,c.crit=0.01, 
               tau.fix = T, tau.start =1)

fit2 <- gamlss(y~1,family = GOLLN,n.cyc=5000,c.crit=0.01, 
               nu.fix = T, nu.start =1)

fit4 <- gamlss(y~1,family = GOLLN,n.cyc=5000,c.crit=0.01)

fit5 <- gamlss(y~1,family = SN1,n.cyc=5000,c.crit=0.01)


## table 2 - summary, AIC and GD

summary(fit4)

gd <- c(fit5$G.deviance,fit4$G.deviance,fit2$G.deviance,fit3$G.deviance,fit1$G.deviance);gd

aic <- c(AIC(fit5),
         AIC(fit4),
         AIC(fit2),
         AIC(fit3),
         AIC(fit1));aic



figure5a <- ggplot(data1,aes(x=y)) +geom_histogram(mapping = aes(x = y, y=..density..), binwidth=5.1, color = '#999999', fill = "#CCCCCC")+xlim(55,110)+ylim(0,0.04) #+geom_density(size=1.1)
figure5a +xlab('y')+ylab('Density')+theme_bw()+
  stat_function(fun = dGOLLN, args = list(mu=fit4$mu.fv[1], sigma =fit4$sigma.fv[1], nu=fit4$nu.fv[1], tau=fit4$tau.fv[1]),size=1.1,aes(colour='GOLLN'))+
  stat_function(fun = dGOLLN, args = list(mu=fit2$mu.fv[1],sigma=fit2$sigma.fv[1],nu=1,tau=fit2$tau.fv[1]),size=1.1,aes(colour='OLLN'))+
  stat_function(fun = dGOLLN, args = list(mu=fit3$mu.fv[1],sigma=fit3$sigma.fv[1],nu=fit3$nu.fv[1],tau=1),size=1.1,aes(colour='Exp-N'))+
  stat_function(fun = dNO, args = list(mu=fit1$mu.fv[1], sigma = fit1$sigma.fv[1]),size=1.1,aes(colour='Normal'))+
  stat_function(fun = dSN1, args = list(mu=fit5$mu.fv[1], sigma = fit5$sigma.fv[1], nu=fit5$nu.fv[1]),size=1.1,aes(colour='Skew-Normal'))+
  scale_color_manual(breaks = c('GOLLN', 'Exp-N',"OLLN",'Normal','Skew-Normal'),values = c('red', "green", 'blue','orange',6))+
  labs(colour = "")+
  theme(legend.position = c(0.15,0.84),legend.background = element_rect(fill = "transparent"),axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))


figure5b <- ggplot((data1),aes(x=y)) +
  stat_ecdf(geom = "step",size=1)+
  stat_ecdf(geom = "point",size=2)

figure5b+xlab('y')+ylab('Cumulative density function')+theme_bw()+stat_function(fun = pGOLLN, args = list(mu=fit4$mu.fv[1], sigma =fit4$sigma.fv[1], nu=fit4$nu.fv[1], tau=fit4$tau.fv[1]),size=1.1,aes(colour='GOLLN'))+
  stat_function(fun = pGOLLN, args = list(mu=fit2$mu.fv[1],sigma=fit2$sigma.fv[1],nu=1,tau=fit2$tau.fv[1]),size=1.1,aes(colour='OLLN'))+
  stat_function(fun = pGOLLN, args = list(mu=fit3$mu.fv[1],sigma=fit3$sigma.fv[1],nu=fit3$nu.fv[1],tau=1),size=1.1,aes(colour='Exp-N'))+
  stat_function(fun = pGOLLN, args = list(mu=fit1$mu.fv[1], sigma = fit1$sigma.fv[1], nu=1, tau=1),size=1.1,aes(colour='Normal'))+
  stat_function(fun = pSN1, args = list(mu=fit5$mu.fv[1], sigma = fit5$sigma.fv[1], nu=fit5$nu.fv[1]),size=1.1,aes(colour='Skew-Normal'))+
  scale_color_manual(breaks = c('GOLLN', 'Exp-N',"OLLN",'Normal','Skew-Normal'),values = c('red', "green", 'blue','orange',6))+labs(colour = "")+theme(legend.position = c(0.15,0.84),legend.background = element_rect(fill = "transparent"),axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))


#table 3
LR.test(fit2,fit4)
LR.test(fit3,fit4)
LR.test(fit1,fit4)


#figure4
n <- length(y)

df <- data.frame(theoretical.quant=qnorm(1:n/(n+1)),sample.quant=sort(fit4$residuals))
figure4a <-  ggplot(df , aes(x=theoretical.quant, y=sample.quant)) + geom_point()+xlab('Theoretical quantile')+ylab('Sample quantile')+ xlim(-3,3)+ ylim(-3.5,3.5)
figure4a+geom_abline(aes(slope = 1, intercept = 0), col='gray',size=1.5)+theme_bw()+
  theme(axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))


df <- data.frame(theoretical.quant=qnorm(1:n/(n+1)),sample.quant=sort(fit2$residuals))
figure4b <-  ggplot(df , aes(x=theoretical.quant, y=sample.quant)) + geom_point()+xlab('Theoretical quantile')+ylab('Sample quantile')+ xlim(-3,3)+ ylim(-3.5,3.5)
figure4b+geom_abline(aes(slope = 1, intercept = 0), col='gray',size=1.5)+theme_bw()+
  theme(axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))


df <- data.frame(theoretical.quant=qnorm(1:n/(n+1)),sample.quant=sort(fit3$residuals))
figure4c <-  ggplot(df , aes(x=theoretical.quant, y=sample.quant)) + geom_point()+xlab('Theoretical quantile')+ylab('Sample quantile')+ xlim(-3,3)+ ylim(-3.5,3.5)
figure4c+geom_abline(aes(slope = 1, intercept = 0), col='gray',size=1.5)+theme_bw()+
  theme(axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))


df <- data.frame(theoretical.quant=qnorm(1:n/(n+1)),sample.quant=sort(fit1$residuals))
figure4d <-  ggplot(df , aes(x=theoretical.quant, y=sample.quant)) + geom_point()+xlab('Theoretical quantile')+ylab('Sample quantile')+ xlim(-3,3)+ ylim(-3.5,3.5)
figure4d+geom_abline(aes(slope = 1, intercept = 0), col='gray',size=1.5)+theme_bw()+
  theme(axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))


df <- data.frame(theoretical.quant=qnorm(1:n/(n+1)),sample.quant=sort(fit5$residuals))
figure4e <-  ggplot(df , aes(x=theoretical.quant, y=sample.quant)) + geom_point()+xlab('Theoretical quantile')+ylab('Sample quantile')+ xlim(-3,3)+ ylim(-3.5,3.5)
figure4e +geom_abline(aes(slope = 1, intercept = 0), col='gray',size=1.5)+theme_bw()+
  theme(axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))

