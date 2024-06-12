rm(list=ls(all=TRUE))

library(ggplot2)
library(dplyr)
library(readxl)
library(RColorBrewer)
library(moments)
library(xtable)

source('GOLLN_gamlss.R')

data2 <-  read_excel("data2.xlsx")

df <- data.frame(y=data2$`Weight (g)`,x1=data2$Treat)

figure6 <-  ggplot(df, aes(x=x1, y=y)) + 
  geom_boxplot(outlier.color = 1,outlier.size = 2,fill='gray') + theme_bw()+xlab("cyl")+ labs(title="",x="Treatment", y = "Weight (g)")+scale_fill_brewer(palette="BuPu")
figure6 + theme(legend.position="none",axis.text=element_text(size=10),strip.text.x = element_text(size = 14), axis.title=element_text(size=12),plot.title = element_text(size=12))+stat_summary(fun.y=mean, geom="point", shape=15, size=2, color="1")+
  scale_x_discrete(labels=c("Treat 1" = "Control", "Treat 2" = "Control+Ethanol",
                            "Treat 3" = "Melatonin 50 mg",'Treat 4'='Melatonin 100 mg'))

#Marginal weight analysis

y <- data2$`Weight (g)`
x1 <- as.factor(data2$treat)

dados <- data.frame(y,x1)


figure1b <- ggplot(dados,aes(x=y)) +geom_histogram(mapping = aes(x = y, y=..density..), binwidth=9.37, color = '#999999', fill = "#CCCCCC")+geom_density(size=1.1)+xlim(40,140)+ylim(0,0.028)
figure1b+xlab('y')+ylab('Density')+theme_bw()+  theme(legend.position = c(0.9,0.89),legend.background = element_rect(fill = "transparent"),axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))

# Table 4: Descriptive analysis
medias <- aggregate(y~x1, FUN=mean)
desvios <- aggregate(y~x1, FUN=sd)
min <- aggregate(y~x1, FUN=min)
max <- aggregate(y~x1, FUN=max)
assimetria <- aggregate(y~x1, FUN=skewness)
mediana <- aggregate(y~x1, FUN=median)
kurtosi <-  aggregate(y~x1, FUN=kurtosis)
cv <- 100*desvios[,2]/medias[,2]
descritiva <- cbind(Trat=medias[,1],Mean=medias[,2],Median=mediana[,2] ,s.d.=desvios[,2], Min.=min[,2],Max=max[,2],Skewness=assimetria[,2], VC=cv,Kurtosis=kurtosi[,2])

xtable(descritiva)

fit1 <- gamlss(y~1,family = NO,n.cyc=5000,c.crit=0.001)

fit2en <- gamlss(y~1,family = GOLLN,n.cyc=5000,c.crit=0.001,tau.fix = T, tau.start =1)

fit3olln <- gamlss(y~1,family = GOLLN,n.cyc=5000,c.crit=0.001, nu.fix = T, nu.start =1)

fit4golln <- gamlss(y~1,family = GOLLN,n.cyc=5000,c.crit=0.001)

fit.skew <- gamlss(y~1,family = SN1,n.cyc=5000,c.crit=0.001)


fit1 <- fit1
fit3 <- fit3olln
fit2 <- fit2en
fit4 <- fit4golln


figure7a <- ggplot(dados,aes(x=y)) +geom_histogram(mapping = aes(x = y, y=..density..), binwidth=9.37, color = '#999999', fill = "#CCCCCC") +xlim(40,140)+ylim(0,0.028)#+geom_density(size=1.1)
figure7a+xlab('y')+ylab('Density')+theme_bw()+stat_function(fun = dGOLLN, args = list(mu=fit4$mu.fv[1], sigma =fit4$sigma.fv[1], nu=fit4$nu.fv[1], tau=fit4$tau.fv[1]),size=1.1,aes(colour='GOLLN'))+
  stat_function(fun = dGOLLN, args = list(mu=fit3$mu.fv[1],sigma=fit3$sigma.fv[1],nu=1,tau=fit3$tau.fv[1]),size=1.1,aes(colour='OLLN'))+
  stat_function(fun = dGOLLN, args = list(mu=fit2$mu.fv[1],sigma=fit2$sigma.fv[1],nu=fit2$nu.fv[1],tau=1),size=1.1,aes(colour='Exp-N'))+
  stat_function(fun = dGOLLN, args = list(mu=fit1$mu.fv[1], sigma = fit1$sigma.fv[1], nu=1, tau=1),size=1.1,aes(colour='Normal'))+scale_color_manual(breaks = c('GOLLN', 'Exp-N',"OLLN",'Normal'),values = c('red', "green", 'blue','orange'))+labs(colour = "")+theme(legend.position = c(0.84,0.89),legend.background = element_rect(fill = "transparent"),axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))



figure7b <- ggplot(dados,aes(x=y)) +
  stat_ecdf(geom = "step",size=1)+
  stat_ecdf(geom = "point",size=2)

figure7b+xlab('y')+ylab('Cumulative density function')+theme_bw()+stat_function(fun = pGOLLN, args = list(mu=fit4$mu.fv[1], sigma =fit4$sigma.fv[1], nu=fit4$nu.fv[1], tau=fit4$tau.fv[1]),size=1.1,aes(colour='GOLLN'))+
  stat_function(fun = pGOLLN, args = list(mu=fit3$mu.fv[1],sigma=fit3$sigma.fv[1],nu=1,tau=fit3$tau.fv[1]),size=1.1,aes(colour='OLLN'))+
  stat_function(fun = pGOLLN, args = list(mu=fit2$mu.fv[1],sigma=fit2$sigma.fv[1],nu=fit2$nu.fv[1],tau=1),size=1.1,aes(colour='Exp-N'))+
  stat_function(fun = pGOLLN, args = list(mu=fit1$mu.fv[1], sigma = fit1$sigma.fv[1], nu=1, tau=1),size=1.1,aes(colour='Normal'))+scale_color_manual(breaks = c('GOLLN', 'Exp-N',"OLLN",'Normal'),values = c('red', "green", 'blue','orange'))+labs(colour = "")+theme(legend.position = c(0.17,0.89),legend.background = element_rect(fill = "transparent"),axis.text=element_text(size=12),strip.text.x = element_text(size = 14), axis.title=element_text(size=14),plot.title = element_text(size=15))



##Regression with covariates in mu

fitr1 <- gamlss(y~x1,family = NO,n.cyc=5000,c.crit=0.001)

fitr3.olln <- gamlss(y~x1,family = GOLLN,n.cyc=5000,c.crit=0.001, nu.fix = T, nu.start =1)

fitr2.en <- gamlss(y~x1,family = GOLLN,n.cyc=5000,c.crit=0.001,tau.fix = T, tau.start =1)

fitr4.golln <- gamlss(y~x1,family = GOLLN,n.cyc=5000,c.crit=0.001)

fitr.skew <- gamlss(y~x1,family = SN1,n.cyc=5000,c.crit=0.001)

##Regression with covariates in mu and sigma

fitr1.2 <- gamlss(y~x1,sigma.formula = ~x1, family = NO,n.cyc=5000,c.crit=0.001)

fitr3.olln2 <- gamlss(y~x1,sigma.formula = ~x1,family = GOLLN,n.cyc=5000,c.crit=0.001, nu.fix = T, nu.start =1)

fitr2.en2 <- gamlss(y~x1,sigma.formula = ~x1,family = GOLLN,n.cyc=5000,c.crit=0.001, tau.fix = T, tau.start =1)

fitr4.golln2 <- gamlss(y~x1,sigma.formula = ~x1,family = GOLLN,n.cyc=5000,c.crit=0.001)

fitr2.skew <- gamlss(y~x1,sigma.formula = ~x1, family = SN1,n.cyc=5000,c.crit=0.001)


##Table 5: Adequacy measures for some fitted models.

tab1 <- cbind(c(AIC(fit4golln),AIC(fit3olln),AIC(fit2en),AIC(fit1)),
              c(fit4golln$G.deviance,fit3olln$G.deviance,fit2en$G.deviance,fit1$G.deviance),
              
              c(AIC(fitr4.golln),AIC(fitr3.olln),AIC(fitr2.en),AIC(fitr1)),
              c(fitr4.golln$G.deviance,fitr3.olln$G.deviance,fitr2.en$G.deviance,fitr1$G.deviance),
              
              c(AIC(fitr4.golln2),AIC(fitr3.olln2),AIC(fitr2.en2),AIC(fitr1.2)),
              c(fitr4.golln2$G.deviance,fitr3.olln2$G.deviance,fitr2.en2$G.deviance,fitr1.2$G.deviance))
rownames(tab1) <- c('GOLLN','OLLN','Exp-N','Normal')
colnames(tab1) <- c('AIC','GD','AIC','GD','AIC','GD')
library(xtable)
xtable(tab1)

AIC(fit.skew)
fit.skew$G.deviance

AIC(fitr.skew)
fitr.skew$G.deviance

AIC(fitr2.skew)
fitr2.skew$G.deviance


#Table 6: LR statistics
LR.test(fitr2.en2,fitr4.golln2)
LR.test(fitr3.olln2,fitr4.golln2)
LR.test(fitr1.2,fitr4.golln2)

#Table 7: Findings from the fitted GOLLN regression model under M2
table7 <- summary(fitr4.golln2,type='qr')
xtable(table7,digits = 4)




## Residuals 
qqnorm(fitr1.2$residuals,pch=20)
abline(0,1,col='red',lwd=2)
qqnorm(fitr2.en2$residuals,pch=20)
abline(0,1,col='red',lwd=2)
qqnorm(fitr3.olln2$residuals,pch=20)
abline(0,1,col='red',lwd=2)
qqnorm(fitr4.golln2$residuals,pch=20)
abline(0,1,col='red',lwd=2)


#Figure 8
wp(fitr1.2,pch=20,col='darkgrey',ylim.all = 1.5)
wp(fitr2.en2,pch=20,col='darkgrey',ylim.all = 1.5)
wp(fitr3.olln2,pch=20,col='darkgrey',ylim.all = 1.5)
wp(fitr4.golln2,pch=20,col='darkgrey',ylim.all = 1.5)
wp(fitr2.skew,pch=20,col='darkgrey',ylim.all = 1.5)



# Likelihood distance

dumm <- model.matrix(~x1)
x1 <- dumm[,2]
x2 <- dumm[,3]
x3 <- dumm[,4]


golln_loglik <- function(par){
  n <- length(y)
  beta10 <- par[1]
  beta11 <- par[2]
  beta12 <- par[3]
  beta13 <- par[4]
  beta20 <- par[5]
  beta21 <- par[6]
  beta22 <- par[7]
  beta23 <- par[8]
  beta30 <- par[9]
  beta40 <- par[10]
  mu0 <- beta10+beta11*x1+beta12*x2+beta13*x3
  sigma0 <- exp(beta20+beta21*x1+beta22*x2+beta23*x3)
  nu0 <- exp(beta30)
  tau0 <- exp(beta40)
  f <- dGOLLN(y, mu=mu0, sigma=sigma0, nu=nu0,tau=tau0,log=F)
  loglik <- sum(log(f))
  return(loglik)
}

model <- fitr4.golln2
theta <- t(as.numeric(c(model$mu.coefficients,model$sigma.coefficients,model$nu.coefficients,model$tau.coefficients)))
inf <- -optimHess(theta,golln_loglik)

likd <- c()
cook <- c()

iter <- 0

for(i in 1:length(y)){
  y1=y[-i]
  x1i=x1[-i]
  x2i=x2[-i]
  x3i=x3[-i]
  mod <- gamlss(y1~x1i+x2i+x3i,sigma.formula=~x1i+x2i+x3i,family=GOLLN(),trace=F,mu.start = model$mu.fv[-i], sigma.start = model$sigma.fv[-i],
                c.crit=0.01, n.cyc=2000)
  lik = logLik(mod)
  likd <- c(likd,lik)
  theta.i <- t(as.numeric(c(mod$mu.coefficients,mod$sigma.coefficients,mod$nu.coefficients,mod$tau.coefficients)))
  cook[i] <- (theta.i-theta)%*%inf%*%t(theta.i-theta)
  cat("iteration = ", iter <- iter + 1, i,"\n")
}


n <- fitr4.golln2$N

df <- data.frame(Index = 1:n,  Cook = abs(cook))
df$group <- factor(ifelse(df$Cook > 5, 1, 0))

cook.plot <- ggplot(df, aes(Index, Cook, color=group, group=group)) +
  geom_point(size=3) +
  geom_segment(aes(Index, xend=Index, 0, yend=Cook, color=group), data=df)  +
  scale_color_manual(values=c("black", "#666666")) +
  ylab('|Cook distance|') +ylim(0,20)+theme_bw()
cook.plot+theme(legend.position="none",axis.title = element_text( size = (14)),axis.text = element_text(size=13))+annotate("text", x=102, y=11, label= 102,col='#666666')+annotate("text", x=29, y=8.6, label= 29,col='#666666')


lik1 <-  logLik(fitr4.golln2)
likd0 <- c(rep(lik1,length(likd)))
distance <- (2*(likd0-likd))

df <- data.frame(Index = 1:n,  Cook = abs(distance))
df$group <- factor(ifelse(df$Cook > 13, 1, 0))

like.plot <- ggplot(df, aes(Index, Cook, color=group, group=group)) +
  geom_point(size=3) +
  geom_segment(aes(Index, xend=Index, 0, yend=Cook, color=group), data=df)  +
  scale_color_manual(values=c("black", "#666666")) +
  ylab('|Likelihood distance|') +ylim(0,40)+theme_bw()
like.plot+theme(legend.position="none",axis.title = element_text( size = (14)),axis.text = element_text(size=13))+annotate("text", x=102, y=17, label= 102,col='#666666')+annotate("text", x=29, y=16, label= 29,col='#666666')



