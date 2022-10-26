# Power simulations

# Load the required packages
library(lme4)
library(ggplot2)
library(multcomp)

# Set seed for replicability
set.seed(2022)

# Specify the parameters
m=15 # Number of posts
b0=-2.007515 # Average from the previous misinformation and choice architecture experiments
b2=-1.05738 # From previous misinformation experiment (inoculation arm (arm 3))
b3=-0.70237 # From choice architecture experiment (click to reveal arm (arm4))

# Estimates from previous studies that we used for estimates of effect sizes in this study
plogis(2.2)

plogis(-2.007515+-1.05738) - plogis(-2.007515)

plogis(-2.007515+-0.70237) - plogis(-2.007515)

plogis(-2.007515+-1.05738) - plogis(-2.007515+-0.70237)

(1.4039+2.1104)/2
# 1.75715

(0.6842+0.5734)/2
# 0.6288

mu1=0
sigma1= 1.75715 # Average from the previous misinformation and choice architecture experiments
mu2=0
sigma2= 0.6288 # Average from the previous misinformation and choice architecture experiments
mu3=0
sigma3=0.100 # Noise

allpowerestimates=data.frame(matrix(nrow=0,ncol=0))
alpha=0.05
nsimulations = 1000
samplesizes = c(1500,1800,2100,2400)

for(n in samplesizes){
  simresults=data.frame(matrix(nrow=0,ncol=0))
  participant=rep(1:n,each=m)
  participant <- as.factor(participant)
  post=rep(1:m,n)
  post <- as.factor(post)
  a=rep(0,m*n/3)
  b=rep(1,m*n/3)
  arm2=c(a,a,b)
  arm3=c(b,a,a)
  rm(a,b)  
  for (i in 1:nsimulations){
    u1=rep(rnorm(n,mu1,sigma1),each=m)
    u2=rep(rnorm(m,mu2,sigma2),n)
    e=rnorm(n*m,mu3,sigma3)
    simdata=data.frame(participant,post,arm2,arm3,b0,b2,b3,u1,u2,e)
    simdata$y0=b0+b2*arm2+b3*arm3+u1+u2+e
    simdata$y=rbinom(n*m,p=plogis(simdata$y0),size=1)
    
    model=glmer(formula=y~1+arm2+arm3+(1|as.factor(participant))+(1|as.factor(post)),family=binomial(link="logit"),data=simdata)
    
    modelsummary = summary(model)
    
    contrast.matrix <- rbind(
      "Arm 1 vs 2" = c( 1, 1, 0)- c( 1, 0, 0),
      "Arm 1 vs 3" = c( 1, 0, 1)- c( 1, 0, 0),
      "Arm 2 vs 3" = c( 1, 0, 1)- c( 1, 1, 0))
    glht.contrast.matrix <- glht(model,contrast.matrix)
    
    cont <- summary(glht.contrast.matrix, test=adjusted("bonferroni"))
    contsummary=summary(cont)
    tempresults=data.frame(simulation=i,
                           samplesize=n,
                           "coef2"=modelsummary$coefficients[2,1],
                           "coefp2"=modelsummary$coefficients[2,4],
                           "coef3"=modelsummary$coefficients[3,1],
                           "coefp3"=modelsummary$coefficients[3,4],
                           "diff1"=as.numeric(contsummary$test$coefficients[1]),
                           "diffp1"=as.numeric(contsummary$test$pvalues[1]),
                           "diff2"=as.numeric(contsummary$test$coefficients[2]),
                           "diffp2"=as.numeric(contsummary$test$pvalues[2]),
                           "diff3"=as.numeric(contsummary$test$coefficients[3]),
                           "diffp3"=as.numeric(contsummary$test$pvalues[3]))
    
    simresults=rbind(simresults,tempresults)
  }
  
  simresults$significance1=ifelse(simresults$coefp2<alpha,1,0)
  simresults$significance2=ifelse(simresults$coefp3<alpha,1,0)
  simresults$significance3=ifelse(simresults$diffp1<alpha,1,0)
  simresults$significance4=ifelse(simresults$diffp2<alpha,1,0)
  simresults$significance5=ifelse(simresults$diffp3<alpha,1,0)
  
  powerestimates=data.frame(samplesize=n,
                            par1=b2,
                            par2=b3,
                            par3=sigma1,
                            par4=sigma2,
                            par5=sigma3,                        
                            power1=mean(simresults$significance1)*100,
                            power2=mean(simresults$significance2)*100,
                            power3=mean(simresults$significance3)*100,
                            power4=mean(simresults$significance4)*100,
                            power5=mean(simresults$significance5)*100)
  
  allpowerestimates=rbind(allpowerestimates,powerestimates)
}


# powerplot1=ggplot(allpowerestimates)+
#   geom_point(aes(samplesize,power1))+
#   geom_line(aes(samplesize,power1))
# powerplot1 
# 
# 
# powerplot2=ggplot(allpowerestimates)+
#   geom_point(aes(samplesize,power2))+
#   geom_line(aes(samplesize,power2))
# powerplot2

# Arm 1 vs. 3 adjusting for multiple comparisons
powerplot3=ggplot(allpowerestimates)+
  geom_point(aes(samplesize,power3))+
  geom_line(aes(samplesize,power3))
powerplot3

# Arm 2 vs. 3 adjusting for multiple comparisons
powerplot4=ggplot(allpowerestimates)+
  geom_point(aes(samplesize,power4))+
  geom_line(aes(samplesize,power4))
powerplot4

# Arm 2 vs. 3 adjusting for multiple comparisons
powerplot5=ggplot(allpowerestimates)+
  geom_point(aes(samplesize,power5))+
  geom_line(aes(samplesize,power5))
powerplot5

