# Primary analysis and sensitivity checks

# Load the required packages
library(plyr)
library(reshape2)
library(lme4)
library(ggplot2)
library(MuMIn)
library(multcomp)
library(sjPlot)
library(DHARMa)
library(merTools)

# Set seed for replicability
set.seed(2022)

# Set the working directory to where the data is
setwd("")

# Read in the data
p_m_262400278 <- read.csv("p_m_262400278_v1.csv")

# Treat arm as a factor
p_m_262400278$ARM <- as.factor(p_m_262400278$ARM)

# Primary Outcome (Liking/Loving)

# Contingency tables
# Primary (likes and love reactions), misinformation posts only
table(p_m_262400278$Primary)
# 0     1 
# 35261  1189 

# Primary (likes and love reactions) by arm, misinformation posts only
table(p_m_262400278$ARM, p_m_262400278$Primary)
# 0     1
# 1 11737   533
# 2 11914   401
# 3 11610   255

# Liking and loving are rare behaviours


# Models
# Starting from maximal and reducing the random effects structure

# p_m_1 has a singular fit, not using this model
p_m_1 <-  glmer(Primary ~ ARM + (ARM + 1|uuid) + (ARM + 1|Post), family = "binomial", data = p_m_262400278, 
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))
# boundary (singular) fit: see ?isSingular

# p_m_2 has a singular fit, not using this model
p_m_2 <-  glmer(Primary ~ ARM + (1|uuid) + (ARM + 1|Post), family = "binomial", data = p_m_262400278,
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))
# boundary (singular) fit: see ?isSingular

p_m_3 <-  glmer(Primary ~ ARM + (ARM + 1|uuid) + (1|Post), family = "binomial", data = p_m_262400278,
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))

summary(p_m_3)
# Random effects:
#   Groups Name        Variance Std.Dev. Corr       
# uuid   (Intercept) 1.6763   1.2947              
# ARM2        1.1804   1.0865   -0.22      
# ARM3        0.4924   0.7017    0.78 -0.15
# Post   (Intercept) 0.4829   0.6949              
# Number of obs: 36450, groups:  uuid, 2430; Post, 15
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  -4.1271     0.2085 -19.793  < 2e-16 ***
#   ARM2         -0.5736     0.1870  -3.067  0.00216 ** 
#   ARM3         -1.5279     0.3047  -5.014 5.32e-07 ***

p_m_4 <-  glmer(Primary ~ ARM + (1|uuid) + (1|Post), family = "binomial", data = p_m_262400278,
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))

summary(p_m_4)
# Random effects:
#   Groups Name        Variance Std.Dev.
# uuid   (Intercept) 2.1438   1.464   
# Post   (Intercept) 0.4886   0.699   
# Number of obs: 36450, groups:  uuid, 2430; Post, 15
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  -4.2737     0.2047 -20.880  < 2e-16 ***
#   ARM2         -0.3954     0.1155  -3.423  0.00062 ***
#   ARM3         -0.9020     0.1247  -7.232 4.75e-13 ***

# The more complex model does not improve the goodness-of-fit at the pre-specified significance
# threshold of p < 0.2. Thus, the simpler model, p_m_4, is preferred.
anova(p_m_3, p_m_4)
# Models:
#   p_m_4: Primary ~ ARM + (1 | uuid) + (1 | Post)
# p_m_3: Primary ~ ARM + (ARM + 1 | uuid) + (1 | Post)
# npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# p_m_4    5 9043.9 9086.4 -4517.0   9033.9                    
# p_m_3   10 9046.9 9131.9 -4513.4   9026.9 7.051  5     0.2169

# Testing the utility of random intercepts.

p_m_5 <-  glmer(Primary ~ ARM + (1|Post), family = "binomial", data = p_m_262400278,
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))

# Random intercepts of uuid significantly improve the goodness-of-fit. Thus, they are warranted.
anova(p_m_4, p_m_5)
# p_m_5: Primary ~ ARM + (1 | Post)
# p_m_4: Primary ~ ARM + (1 | uuid) + (1 | Post)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# p_m_5    4 9760.5 9794.5 -4876.3   9752.5                         
# p_m_4    5 9043.9 9086.4 -4517.0   9033.9 718.63  1  < 2.2e-16 ***

p_m_6 <-  glmer(Primary ~ ARM + (1|uuid), family = "binomial", data = p_m_262400278,
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))

# Random intercepts of Post significantly improve the goodness-of-fit. Thus, they are warranted.
anova(p_m_4, p_m_6)
# p_m_6: Primary ~ ARM + (1 | uuid)
# p_m_4: Primary ~ ARM + (1 | uuid) + (1 | Post)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# p_m_6    4 9637.3 9671.3 -4814.7   9629.3                         
# p_m_4    5 9043.9 9086.4 -4517.0   9033.9 595.41  1  < 2.2e-16 ***


# Optimal model, p_m_4, diagnostics.

# Creates scaled residuals by simulating from the fitted model (p_m_4).
# (new data will be simulated and scaled residuals will be created by comparing observed 
# data with new data.)
# Given 1000 simulations to stabilize the simulated values.
SimulatedResidualsP <- simulateResiduals(fittedModel = p_m_4, plot = F, n = 1000)

# Using these metrics, the model seems to be performing well.
plot(SimulatedResidualsP)

# Singularity check -- the definition of singularity is that some of the constrained 
# parameters of the random effects theta parameters are on the boundary (equal to zero, 
# or very close to 0, such as -10e6)
tt <- getME(p_m_4, "theta")
ll <- getME(p_m_4, "lower")
min(tt[ll==0])
# [1] 0.6990115 -- no singularity

# Double check gradient calculations.
derivs1 <- p_m_4@optinfo$derivs
sc_grad1 <- with(derivs1, solve(Hessian, gradient))
max(abs(sc_grad1))
# [1] 2.138045e-06

# Large scaled gradients are often associated with small absolute gradients; 
# worth testing the (parallel) minimum of these two quantities:
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
# [1] 2.138045e-06

# Tolerance is typically set at 0.001; the gradient does not exceed this threshold.

# Trying different optimisers to get a different gradient statistic
modelfit.all <- lme4::allFit(p_m_4)
is.OK <- sapply(modelfit.all, is,"merMod")
modelfit.all.OK <- modelfit.all[is.OK]
lapply(modelfit.all.OK, function(x) x@optinfo$conv$lme4$messages)
# $bobyqa
# NULL
# 
# $Nelder_Mead
# NULL
# 
# $nlminbwrap
# NULL
# 
# $nmkbw
# NULL
# 
# $`optimx.L-BFGS-B`
# NULL
# 
# $nloptwrap.NLOPT_LN_NELDERMEAD
# NULL
# 
# $nloptwrap.NLOPT_LN_BOBYQA
# NULL


# All optimisers managed to calculate the gradient without exceeding 
# lme4's convergence tolerance levels.

# Checking whether log likelihoods are similar.
(lliks <- sort(sapply(modelfit.all.OK,logLik)))
# nloptwrap.NLOPT_LN_NELDERMEAD                         nmkbw               optimx.L-BFGS-B 
# -4516.952                     -4516.952                     -4516.952 
# nlminbwrap     nloptwrap.NLOPT_LN_BOBYQA                   Nelder_Mead 
# -4516.952                     -4516.952                     -4516.952 
# bobyqa 
# -4516.952 

# The above output shows that the log likelihood of models ran using different 
# optimsers are the same to three decimal places.

# Plotting the fixed effects estimates by optimiser.
modelfit.all.fixef <- t(sapply(modelfit.all.OK,fixef))
modelfit.all.fixef.m <- melt(modelfit.all.fixef)
models <- levels(modelfit.all.fixef.m$Var1)
ylabs <- substr(models, 1, 3)
modelfit.all.fixef.m <- transform(modelfit.all.fixef.m, Var1=factor(Var1, levels=names(lliks)))
(gplot1 <- ggplot(modelfit.all.fixef.m, aes(x=value,y=Var1,colour=Var1))+geom_point()+
    facet_wrap(~Var2, scale="free")+
    scale_colour_brewer(palette,"Dark2") +
    scale_y_discrete(breaks=models,
                     labels=ylabs)+
    labs(x="",y=""))

# Coefficients of variation of fixed-effect parameter estimates are nearly the same across
# optimisers.
summary(unlist(daply(modelfit.all.fixef.m,"Var2", summarise, sd(value)/abs(mean(value)))))
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 1.296e-05 5.099e-05 8.902e-05 7.744e-05 1.097e-04 1.303e-04 

# Random intercepts estimates do not differ by otpimiser, either.
modelfit.all.stddev <- t(sapply(modelfit.all.OK, function(x) sqrt(unlist(VarCorr(x)))))
print(modelfit.all.stddev, digits = 3)
#                               uuid  Post
# bobyqa                        1.46 0.699
# Nelder_Mead                   1.46 0.699
# nlminbwrap                    1.46 0.699
# nmkbw                         1.46 0.699
# optimx.L-BFGS-B               1.46 0.699
# nloptwrap.NLOPT_LN_NELDERMEAD 1.46 0.699
# nloptwrap.NLOPT_LN_BOBYQA     1.46 0.699



# Calculating pseudo R2 (Nakagawa, Johnson, and Schielzeth, 2017)
# We favour the delta estimates.
r.squaredGLMM(p_m_4)
#                    R2m       R2c
# theoretical 0.022313575 0.45689203
# delta       0.003395717 0.06953058


# Creating a multiple comparisons matrix for multiple comparisons.
contrast.matrix <- rbind(
  "Arm 1 vs 2" = c( 1, 1, 0)- c( 1, 0, 0),
  "Arm 1 vs 3" = c( 1, 0, 1)- c( 1, 0, 0),
  "Arm 2 vs 3" = c( 1, 0, 1)- c( 1, 1, 0))

glht.contrast.matrix_p_m_4 <- glht(p_m_4,contrast.matrix)

# Arm contrasts, primary model
p_m_4con <- summary(glht.contrast.matrix_p_m_4, test=adjusted("bonferroni"))

# The log-odds of liking/loving are significantly lower in arm 2 compared to arm 1, 
# and in arm 3 compared to arms 1 and 2.
p_m_4con
# Linear Hypotheses:
#   Estimate Std. Error z value Pr(>|z|)    
#   Arm 1 vs 2 == 0  -0.3954     0.1155  -3.423 0.001860 ** 
#   Arm 1 vs 3 == 0  -0.9020     0.1247  -7.232 1.43e-12 ***
#   Arm 2 vs 3 == 0  -0.5067     0.1279  -3.961 0.000224 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# (Adjusted p values reported -- bonferroni method)

# Converting model's estimates to odds ratios.
exp(p_m_4con$coef)
# (Intercept)        ARM2        ARM3 
# 0.01393059  0.67344272  0.40575564 

# Converting multiple comparisons estimates to odds ratios.
exp(p_m_4con$test$coefficients)
# Arm 1 vs 2 Arm 1 vs 3 Arm 2 vs 3 
# 0.6734427  0.4057556  0.6025095 

# Calculating confidence intervals for multiple comparisons.
p_m_4conci <- confint(p_m_4con)
p_m_4conci
# Quantile = 2.3424
# 95% family-wise confidence level
# 
# 
# Linear Hypotheses:
#   Estimate lwr     upr    
# Arm 1 vs 2 == 0 -0.3954  -0.6659 -0.1248
# Arm 1 vs 3 == 0 -0.9020  -1.1941 -0.6099
# Arm 2 vs 3 == 0 -0.5067  -0.8063 -0.2070

# Converting the multiple comparisons' confidence intervals to odds ratios.
exp(p_m_4conci$confint)
# Estimate       lwr       upr
# Arm 1 vs 2 0.6734427 0.5138011 0.8826861
# Arm 1 vs 3 0.4057556 0.3029628 0.5434252
# Arm 2 vs 3 0.6025095 0.4465181 0.8129966
# attr(,"conf.level")
# [1] 0.95
# attr(,"calpha")
# [1] 2.342353

# Model summary table with odds ratios estimates.
tab_model(p_m_4)




# Sensitivity checks.
# Secondary outcome 1 (all reactions), misinformation posts only

# Contingency tables
# Secondary outcome 1 (all reactions), misinformation posts only
table(p_m_262400278$Accuracy_Reactions)
# 0     1 
# 31024  5426 

# Secondary outcome 1 (all reactions) by arm, misinformation posts only
table(p_m_262400278$ARM, p_m_262400278$Accuracy_Reactions)
# 0     1
# 1  9968  2302
# 2 10483  1832
# 3 10573  1292

# Reacting is more common than liking/loving (primary outcome)

## Models
# Starting from maximal and reducing the random effects structure
s_m_1 <- glmer(Accuracy_Reactions ~ ARM + (ARM + 1|uuid) + (ARM + 1|Post), family = "binomial", data = p_m_262400278,
               control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))
summary(s_m_1)
# Random effects:
#   Groups Name        Variance Std.Dev. Corr       
# uuid   (Intercept) 3.32243  1.8228              
# ARM2        1.10225  1.0499   -0.16      
# ARM3        0.05789  0.2406    0.69  0.48
# Post   (Intercept) 0.16936  0.4115              
# ARM2        0.02584  0.1607   -0.91      
# ARM3        0.09344  0.3057   -0.48  0.79
# Number of obs: 36450, groups:  uuid, 2430; Post, 15
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  -2.2960     0.1302 -17.635  < 2e-16 ***
#   ARM2         -0.4827     0.1201  -4.019 5.84e-05 ***
#   ARM3         -1.0935     0.1437  -7.609 2.76e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) ARM2  
# ARM2 -0.618       
# ARM3 -0.519  0.478

s_m_2 <- glmer(Accuracy_Reactions ~ ARM + (1|uuid) + (ARM + 1|Post), family = "binomial", data = p_m_262400278,
               control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))
# Warning messages:
#   1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                     Model failed to converge with max|grad| = 0.531035 (tol = 0.002, component 1)
#                   2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                     Model is nearly unidentifiable: very large eigenvalue
#                                   - Rescale variables?

s_m_3 <- glmer(Accuracy_Reactions ~ ARM + (ARM + 1|uuid) + (1|Post), family = "binomial", data = p_m_262400278,
               control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))
summary(s_m_3)
# Random effects:
#   Groups Name        Variance Std.Dev. Corr       
# uuid   (Intercept) 3.2764   1.8101              
# ARM2        1.0423   1.0209   -0.12      
# ARM3        0.9492   0.9743   -0.10  0.53
# Post   (Intercept) 0.1103   0.3321              
# Number of obs: 36450, groups:  uuid, 2430; Post, 15
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -2.2766     0.1136 -20.033  < 2e-16 ***
#   ARM2         -0.5316     0.1120  -4.745 2.08e-06 ***
#   ARM3         -1.0719     0.1182  -9.067  < 2e-16 ***

# Random differences (ARM + 1 | Post) significantly improve the goodness-of-fit,
# s_m_1 is preferred over s_m_3.
anova(s_m_1, s_m_3)
# s_m_3: Accuracy_Reactions ~ ARM + (ARM + 1 | uuid) + (1 | Post)
# s_m_1: Accuracy_Reactions ~ ARM + (ARM + 1 | uuid) + (ARM + 1 | Post)
# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
# s_m_3   10 24122 24207 -12051    24102                         
# s_m_1   15 24098 24226 -12034    24068 33.992  5  2.389e-06 ***

# A model with random intercepts only did not converge.
s_m_4 <- glmer(Accuracy_Reactions ~ ARM + (1|uuid) + (1|Post), family = "binomial", data = p_m_262400278,
               control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))
# Warning messages:
#   1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                     Model failed to converge with max|grad| = 0.509795 (tol = 0.002, component 1)
#                   2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                     Model is nearly unidentifiable: very large eigenvalue
#                                   - Rescale variables?


# Testing the utility of random intercepts.
s_m_5 <- glmer(Accuracy_Reactions ~ ARM + (1|Post), family = "binomial", data = p_m_262400278,
               control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))
summary(s_m_5)
# Random effects:
#   Groups Name        Variance Std.Dev.
#   Post   (Intercept) 0.09038  0.3006  
# Number of obs: 36450, groups:  Post, 15
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -1.49333    0.08103 -18.429  < 2e-16 ***
#   ARM2        -0.28214    0.03447  -8.184 2.74e-16 ***
#   ARM3        -0.64340    0.03763 -17.099  < 2e-16 ***

# Random effects, and random differences, significantly improve the goodness-of-fit.
anova(s_m_1, s_m_5)
# s_m_5: Accuracy_Reactions ~ ARM + (1 | Post)
# s_m_1: Accuracy_Reactions ~ ARM + (ARM + 1 | uuid) + (ARM + 1 | Post)
# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
# s_m_5    4 30035 30069 -15014    30027                         
# s_m_1   15 24098 24226 -12034    24068 5958.9 11  < 2.2e-16 ***

# s_m_1 is preferred.


s_m_6 <- glmer(Accuracy_Reactions ~ ARM + (1|uuid), family = "binomial", data = p_m_262400278,
               control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))

summary(s_m_6)
# Random effects:
#   Groups Name        Variance Std.Dev.
# uuid   (Intercept) 3.553    1.885   
# Number of obs: 36450, groups:  uuid, 2430
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -2.26950    0.07867 -28.849  < 2e-16 ***
#   ARM2        -0.45909    0.10973  -4.184 2.87e-05 ***
#   ARM3        -0.99433    0.11409  -8.715  < 2e-16 ***

# Random effects, and random differences, significantly improve the goodness-of-fit.
anova(s_m_1, s_m_6)
# s_m_6: Accuracy_Reactions ~ ARM + (1 | uuid)
# s_m_1: Accuracy_Reactions ~ ARM + (ARM + 1 | uuid) + (ARM + 1 | Post)
# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
# s_m_6    4 24582 24616 -12287    24574                         
# s_m_1   15 24098 24226 -12034    24068 505.85 11  < 2.2e-16 ***

# Preferred model, s_m_1, diagnostics.

# Creates scaled residuals by simulating from the fitted model (s_m_1).
# (new data will be simulated and scaled residuals will be created by comparing observed 
# data with new data.)
# Given 1000 simulations to stabilize the simulated values.
SimulatedResidualsS1 <- simulateResiduals(fittedModel = s_m_1, plot = F, n = 1000)

# Using these metrics, the model seems to be performing well.
plot(SimulatedResidualsS1)

# Singularity check -- the definition of singularity is that some of the constrained 
# parameters of the random effects theta parameters are on the boundary (equal to zero, 
# or very close to 0, such as -10e6)
tt <- getME(s_m_1, "theta")
ll <- getME(s_m_1, "lower")
min(tt[ll==0])
# [1] 0.06786805 -- no singularity

# Double check gradient calculations.
derivs1 <- s_m_1@optinfo$derivs
sc_grad1 <- with(derivs1, solve(Hessian, gradient))
max(abs(sc_grad1))
# [1] 5.67926e-05

# Large scaled gradients are often associated with small absolute gradients; 
# worth testing the (parallel) minimum of these two quantities:
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
# [1] 5.67926e-05

# Tolerance is typically set at 0.001; the gradient does not exceed this threshold.

# Trying different optimisers to get a different gradient statistic
modelfit.all <- lme4::allFit(s_m_1)
is.OK <- sapply(modelfit.all, is,"merMod")
modelfit.all.OK <- modelfit.all[is.OK]
lapply(modelfit.all.OK, function(x) x@optinfo$conv$lme4$messages)
# $bobyqa
# NULL
# 
# $Nelder_Mead
# [1] "Model failed to converge with max|grad| = 1.52674 (tol = 0.002, component 1)"
# 
# $nlminbwrap
# [1] "unable to evaluate scaled gradient"                                       
# [2] "Model failed to converge: degenerate  Hessian with 3 negative eigenvalues"
# 
# $nmkbw
# [1] "boundary (singular) fit: see ?isSingular"
# 
# $`optimx.L-BFGS-B`
# [1] "unable to evaluate scaled gradient"                                       
# [2] "Model failed to converge: degenerate  Hessian with 3 negative eigenvalues"
# 
# $nloptwrap.NLOPT_LN_NELDERMEAD
# [1] "Model failed to converge with max|grad| = 0.00761606 (tol = 0.002, component 1)"
# 
# $nloptwrap.NLOPT_LN_BOBYQA
# [1] "unable to evaluate scaled gradient"                                       
# [2] "Model failed to converge: degenerate  Hessian with 3 negative eigenvalues"


# Only BOBYQA optimiser managed to calculate the gradient without exceeding lme4's 
# convergence tolerance threshold.

# Checking whether log likelihoods are similar.
(lliks <- sort(sapply(modelfit.all.OK,logLik)))
# nmkbw     nloptwrap.NLOPT_LN_BOBYQA                        bobyqa 
# -12034.89                     -12034.12                     -12034.12 
# nlminbwrap                   Nelder_Mead               optimx.L-BFGS-B 
# -12034.12                     -12033.82                     -12033.53 
# nloptwrap.NLOPT_LN_NELDERMEAD 
# -12033.53 

# The above output shows that the likelihood of the models produced using different 
# optimisers are similar (within 2).

# Plotting the fixed effects estimates by optimiser.
modelfit.all.fixef <- t(sapply(modelfit.all.OK,fixef))
modelfit.all.fixef.m <- melt(modelfit.all.fixef)
models <- levels(modelfit.all.fixef.m$Var1)
ylabs <- substr(models, 1, 3)
modelfit.all.fixef.m <- transform(modelfit.all.fixef.m, Var1=factor(Var1, levels=names(lliks)))
(gplot1 <- ggplot(modelfit.all.fixef.m, aes(x=value,y=Var1,colour=Var1))+geom_point()+
    facet_wrap(~Var2, scale="free")+
    scale_colour_brewer(palette,"Dark2") +
    scale_y_discrete(breaks=models,
                     labels=ylabs)+
    labs(x="",y=""))

# Coefficients of variation of fixed-effect parameter estimates vary by no more than 0.03 between
# different optimisers.
summary(unlist(daply(modelfit.all.fixef.m,"Var2", summarise, sd(value)/abs(mean(value)))))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0009915 0.0144910 0.0279905 0.0196687 0.0290074 0.0300243 

# Random intercepts estimates differ by otpimiser.
modelfit.all.stddev <- t(sapply(modelfit.all.OK, function(x) sqrt(unlist(VarCorr(x)))))
print(modelfit.all.stddev, digits = 3)
# uuid1 uuid2 uuid3 uuid4 uuid5  uuid6 uuid7  uuid8 uuid9 Post1 Post2
# bobyqa                         1.82   NaN 0.549   NaN 1.050 0.3467 0.549 0.3467 0.241 0.412   NaN
# Nelder_Mead                    1.80   NaN   NaN   NaN 1.090 1.1177   NaN 1.1177 1.283 0.386   NaN
# nlminbwrap                     1.82   NaN   NaN   NaN 1.189 0.2179   NaN 0.2179 1.159 0.412   NaN
# nmkbw                          1.82 0.534   NaN 0.534 0.159    NaN   NaN    NaN 2.617 0.395   NaN
# optimx.L-BFGS-B                1.82   NaN   NaN   NaN 1.004 0.0859   NaN 0.0859 1.034 0.409   NaN
# nloptwrap.NLOPT_LN_NELDERMEAD  1.82 0.527   NaN 0.527 0.177 0.0834   NaN 0.0834 1.159 0.409   NaN
# nloptwrap.NLOPT_LN_BOBYQA      1.82   NaN   NaN   NaN 1.369    NaN   NaN    NaN 1.108 0.412   NaN
# Post3 Post4 Post5 Post6 Post7 Post8 Post9
# bobyqa                          NaN   NaN 0.161 0.197   NaN 0.197 0.306
# Nelder_Mead                     NaN   NaN 0.147 0.174   NaN 0.174 0.292
# nlminbwrap                      NaN   NaN 0.161 0.197   NaN 0.197 0.306
# nmkbw                           NaN   NaN 0.117 0.120   NaN 0.120 0.280
# optimx.L-BFGS-B                 NaN   NaN 0.161 0.198   NaN 0.198 0.306
# nloptwrap.NLOPT_LN_NELDERMEAD   NaN   NaN 0.161 0.198   NaN 0.198 0.306
# nloptwrap.NLOPT_LN_BOBYQA       NaN   NaN 0.162 0.197   NaN 0.197 0.306

# NaN's are problematic, but the inference, in terms of the direction of the effects, is not
# sensitive to the specification of random effects, comparing s_m_1, s_m_3, s_m_5 and s_m_6.

# Calculating pseudo R2 (Nakagawa, Johnson, and Schielzeth, 2017)
# We favour the delta estimates.
r.squaredGLMM(s_m_1)
#                    R2m       R2c
# theoretical 0.0271304 0.5505286
# delta       0.0156148 0.3168547

# Creating a multiple comparisons matrix for multiple comparisons.
contrast.matrix <- rbind(
  "Arm 1 vs 2" = c( 1, 1, 0)- c( 1, 0, 0),
  "Arm 1 vs 3" = c( 1, 0, 1)- c( 1, 0, 0),
  "Arm 2 vs 3" = c( 1, 0, 1)- c( 1, 1, 0))

glht.contrast.matrix_s_m_1 <- glht(s_m_1,contrast.matrix)

# Arm contrasts, s_m_1
s_m_1con <- summary(glht.contrast.matrix_s_m_1, test=adjusted("bonferroni"))

# The log-odds of reacting are significantly lower in arm 2 compared to arm 1, 
# and in arm 3 compared to arms 1 and 2.
s_m_1con
# Linear Hypotheses:
#                   Estimate Std. Error z value Pr(>|z|)    
#   Arm 1 vs 2 == 0  -0.4827     0.1201  -4.019 0.000175 ***
#   Arm 1 vs 3 == 0  -1.0935     0.1437  -7.609 8.26e-14 ***
#   Arm 2 vs 3 == 0  -0.6109     0.1363  -4.482 2.22e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# (Adjusted p values reported -- bonferroni method)

# Converting model's estimates to odds ratios.
exp(s_m_1con$coef)
# (Intercept)        ARM2        ARM3 
# 0.1006610   0.6171363   0.3350263 

# Converting multiple comparisons estimates to odds ratios.
exp(s_m_1con$test$coefficients)
# Arm 1 vs 2 Arm 1 vs 3 Arm 2 vs 3 
# 0.6171363  0.3350263  0.5428724 

# Calculating confidence intervals for multiple comparisons.
s_m_1conci <- confint(s_m_1con)
s_m_1conci
# Quantile = 2.3416
# 95% family-wise confidence level
# 
# 
# Linear Hypotheses:
#   Estimate lwr     upr    
# Arm 1 vs 2 == 0 -0.4827  -0.7639 -0.2015
# Arm 1 vs 3 == 0 -1.0935  -1.4301 -0.7570
# Arm 2 vs 3 == 0 -0.6109  -0.9301 -0.2917

# Converting the multiple comparisons' confidence intervals to odds ratios.
exp(s_m_1conci$confint)
#             Estimate       lwr       upr
# Arm 1 vs 2 0.6171363 0.4658635 0.8175296
# Arm 1 vs 3 0.3350263 0.2392899 0.4690653
# Arm 2 vs 3 0.5428724 0.3945301 0.7469910


# Model summary table with odds ratios estimates.
tab_model(s_m_1)


## Checking whether inference varies if we use models with simpler random effects structure
glht.contrast.matrix_s_m_5 <- glht(s_m_5,contrast.matrix)

# Arm contrasts, s_m_5
summary(glht.contrast.matrix_s_m_5, test=adjusted("bonferroni"))
# Linear Hypotheses:
#                   Estimate Std. Error z value Pr(>|z|)    
#   Arm 1 vs 2 == 0 -0.28214    0.03447  -8.184 6.66e-16 ***
#   Arm 1 vs 3 == 0 -0.64340    0.03763 -17.099  < 2e-16 ***
#   Arm 2 vs 3 == 0 -0.36126    0.03902  -9.259  < 2e-16 ***

glht.contrast.matrix_s_m_6 <- glht(s_m_6,contrast.matrix)

# Arm contrasts, s_m_6
summary(glht.contrast.matrix_s_m_6, test=adjusted("bonferroni"))
# Linear Hypotheses:
#                   Estimate Std. Error z value Pr(>|z|)    
#   Arm 1 vs 2 == 0  -0.4591     0.1097  -4.184 8.60e-05 ***
#   Arm 1 vs 3 == 0  -0.9943     0.1141  -8.715  < 2e-16 ***
#   Arm 2 vs 3 == 0  -0.5352     0.1155  -4.635 1.07e-05 ***

# The direction of the effects does not change, but the magnitude does.

#Caterpillar_plot
Caterpillar_plot_s_m_1 <- REsim(s_m_1)

# Function for plotting random effects.
plotREsim2 <- function(data, level = 0.95, stat = "median", sd = TRUE,
                       sigmaScale = NULL, oddsRatio = FALSE, labs = FALSE,
                       facet= TRUE){
  # error checking
  # check for faceting
  facet_logical <- is.logical(facet)
  if (!facet_logical) {
    data <- data[data$groupFctr == facet[[1]] & data$term == facet[[2]], ]
  }
  
  if(!missing(sigmaScale)){
    data[, "sd"] <- data[, "sd"] / sigmaScale
    data[, stat] <- data[, stat] / sigmaScale
  }
  data[, "sd"] <- data[, "sd"] * qnorm(1-((1-level)/2))
  data[, "ymax"] <- data[, stat] + data[, "sd"]
  data[, "ymin"] <- data[, stat] - data[, "sd"]
  data[, "sig"] <- data[, "ymin"] > 0 | data[, "ymax"] < 0
  hlineInt <- 0
  if(oddsRatio == TRUE){
    data[, "ymax"] <- exp(data[, "ymax"])
    data[, stat] <- exp(data[, stat])
    data[, "ymin"] <- exp(data[, "ymin"])
    hlineInt <- 1
  }
  #  data <- data[order(data[,"groupFctr"], data[,"term"], data[,stat]),]
  rownames(data) <- 1:nrow(data)
  data[,"xvar"] <- factor(paste(data$groupFctr, data$groupID, sep=""),
                          levels=unique(paste(data$groupFctr,data$groupID, sep="")),
                          ordered=TRUE)
  if(labs == TRUE){
    xlabs.tmp <- element_text(face = "bold", angle=90, vjust=.5)
  } else {
    data[,"xvar"] <- as.numeric(data[,"xvar"])
    xlabs.tmp <- element_blank()
  }
  
  p <- ggplot(data, aes_string(x = "xvar", y = stat, ymax = "ymax", ymin = "ymin")) +
    geom_hline(yintercept = hlineInt, color = I("#56B4E9"), size = I(1.1)) +
    geom_point(color="black", alpha=1/(nrow(data)^.33), size=I(0.5)) +
    geom_point(data=subset(data, sig==TRUE),color="blue") +
    labs(x = "Group", y = "Effect Range", title = "Effect Ranges") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = xlabs.tmp,
          axis.ticks.x = element_blank())
  if (sd) {
    p <- p +
      geom_pointrange(alpha = 1/(nrow(data)^.33)) +
      geom_pointrange(data=subset(data, sig==FALSE),color="#D55E00")
  }
  # check facet
  if (facet_logical) {
    return(p + facet_grid(term ~ groupFctr, scales = "free_x"))
  } else {
    return(p)
  }
}


# Plot the results of a simulation of the random effects
# object s_arm3
s_arm3 <- plotREsim2(Caterpillar_plot_s_m_1, 
                     facet= list(groupFctr= "Post", term= "ARM3"), oddsRatio = TRUE)


S_arm3_post_caterpillar <- s_arm3 + theme_minimal() + theme(legend.position="none") +
  theme(axis.text.x = element_text(size=16,  color = "black"),
        axis.text.y = element_text(size=16, color = "black")) +
  theme(axis.title = element_text(size=20, face="bold")) + 
  theme(axis.title.x = element_text(vjust=-0.5)) + 
  scale_x_continuous("Post", labels = p_m_262400278$Post, breaks = p_m_262400278$Post) + 
  labs(title="",x="Post", y = "Effect Range (odds ratio)", face="bold")

# Generate plot
S_arm3_post_caterpillar


# library(svglite)
# svglite("S_arm3_post_caterpillar.svg", width=14, height=6)
# 
# S_arm3_post_caterpillar
# 
# dev.off()


# Sensitivity checks.
# Secondary outcome 2 (shares), misinformation posts only

# Contingency tables
# Secondary outcome 1 (all reactions), misinformation posts only
table(p_m_262400278$Share_Accuracy)
# 0     1 
# 35777   673 

# Sharing is a very rare behaviour, rarer than liking/loving (primary outcome)

# Secondary outcome 1 (all reactions) by arm, misinformation posts only
table(p_m_262400278$ARM, p_m_262400278$Share_Accuracy)
# 0     1
# 1 11937   333
# 2 12107   208
# 3 11733   132

## Models
# Starting from maximal and reducing the random effects structure
s2_m_1 <- glmer(Share_Accuracy ~ ARM + (ARM + 1|uuid) + (ARM + 1|Post), family = "binomial", data = p_m_262400278,
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))

# The model should not be trusted since random effect correlation cannot be +1, 
# it is likely that the optimization algorithm hit a boundary. There is not enough data to
# estimate the all parameters reliably.
summary(s2_m_1)
# Random effects:
#   Groups Name        Variance Std.Dev. Corr       
# uuid   (Intercept)  3.91259 1.9780              
# ARM2        11.78859 3.4335   1.00       
# ARM3        23.25974 4.8228   0.47  0.46 

s2_m_2 <- glmer(Share_Accuracy ~ ARM + (1|uuid) + (ARM + 1|Post), family = "binomial", data = p_m_262400278,
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))

summary(s2_m_2)
# Random effects:
#   Groups Name        Variance Std.Dev. Corr       
# uuid   (Intercept) 23.64312 4.8624              
# Post   (Intercept)  0.13717 0.3704              
# ARM2         0.01612 0.1269   -0.32      
# ARM3         0.19252 0.4388   -0.39 -0.47
# Number of obs: 36450, groups:  uuid, 2430; Post, 15
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  -8.4385     0.3932 -21.462   <2e-16 ***
#   ARM2         -0.4547     0.2924  -1.555   0.1200    
#   ARM3         -0.8257     0.3386  -2.439   0.0147 *  

s2_m_3 <- glmer(Share_Accuracy ~ ARM + (ARM + 1|uuid) + (1|Post), family = "binomial", data = p_m_262400278,
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))

# Effect estimates seem implausible
summary(s2_m_3)
# Random effects:
#   Groups Name        Variance Std.Dev. Corr     
# uuid   (Intercept)  3.9495  1.9873            
# ARM2        13.1887  3.6316   0.84     
# ARM3        21.7223  4.6607   0.64 0.13
# Post   (Intercept)  0.1245  0.3528            
# Number of obs: 36450, groups:  uuid, 2430; Post, 15
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -5.4227     0.2914 -18.609  < 2e-16 ***
#   ARM2         -3.8732     0.5406  -7.165 7.78e-13 ***
#   ARM3         -4.7405     0.5719  -8.289  < 2e-16 ***

s2_m_4 <- glmer(Share_Accuracy ~ ARM + (1|uuid) + (1|Post), family = "binomial", data = p_m_262400278,
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))

summary(s2_m_4)
# Random effects:
#   Groups Name        Variance Std.Dev.
# uuid   (Intercept) 23.8839  4.8871  
# Post   (Intercept)  0.1074  0.3277  
# Number of obs: 36450, groups:  uuid, 2430; Post, 15
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -8.3617     0.3793 -22.047  < 2e-16 ***
#   ARM2         -0.5893     0.2577  -2.287 0.022212 *  
#   ARM3         -1.0253     0.2789  -3.676 0.000237 ***

# s_m_2 is preferred, according to the goodness-of-fit comparison (p < 0.2)
# The inference regarding ARM2 varies by model, s2_m_2 has larger error term estimates,
# accounts for more uncertainty in the data. Thus, s2_m_2 seems to be the better 
# choice for inference.
anova(s2_m_2, s2_m_4)
# s2_m_4: Share_Accuracy ~ ARM + (1 | uuid) + (1 | Post)
# s2_m_2: Share_Accuracy ~ ARM + (1 | uuid) + (ARM + 1 | Post)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# s2_m_4    5 5560.0 5602.5 -2775.0   5550.0                     
# s2_m_2   10 5561.2 5646.2 -2770.6   5541.2 8.7919  5     0.1177


s2_m_5 <- glmer(Share_Accuracy ~ ARM + (1|Post), family = "binomial", data = p_m_262400278,
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))

# Random intercepts of uuid significantly improve the goodness-of-fit. Thus, they are warranted.
anova(s2_m_2, s2_m_5)
# s2_m_5: Share_Accuracy ~ ARM + (1 | Post)
# s2_m_4: Share_Accuracy ~ ARM + (1 | uuid) + (1 | Post)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# s2_m_5    4 6375.6 6409.6 -3183.8   6367.6                         
# s2_m_2   10 5561.2 5646.2 -2770.6   5541.2 826.38  6  < 2.2e-16 ***

# s2_m_2 is preferred.

s2_m_6 <- glmer(Share_Accuracy ~ ARM + (1|uuid), family = "binomial", data = p_m_262400278,
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))

# Random intercepts of Post significantly improve the goodness-of-fit. Thus, they are warranted.
anova(s2_m_2, s2_m_6)
# s2_m_6: Share_Accuracy ~ ARM + (1 | uuid)
# s2_m_4: Share_Accuracy ~ ARM + (1 | uuid) + (1 | Post)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# s2_m_6    4 5771.9 5805.9 -2881.9   5763.9                         
# s2_m_2   10 5561.2 5646.2 -2770.6   5541.2 222.67  6  < 2.2e-16 ***

# s2_m_2 is preferred.

# Given 1000 simulations to stabilize the simulated values.
SimulatedResidualsS2 <- simulateResiduals(fittedModel = s2_m_2, plot = F, n = 1000)

# Significant deviations from uniformity of residuals are detected.
# This is likely to be due to to the low count of shares, overall.
# We are ignoring this given that this model is not of primary
# concern.
plot(SimulatedResidualsS2)

# Singularity check -- the definition of singularity is that some of the constrained 
# parameters of the random effects theta parameters are on the boundary (equal to zero, 
# or very close to 0, such as -10e6)
tt <- getME(s2_m_2, "theta")
ll <- getME(s2_m_2, "lower")
min(tt[ll==0])
# [1] 0.1202971 -- no singularity

# Double check gradient calculations.
derivs1 <- s2_m_2@optinfo$derivs
sc_grad1 <- with(derivs1, solve(Hessian, gradient))
max(abs(sc_grad1))
# [1] 8.9096e-05

# Large scaled gradients are often associated with small absolute gradients; 
# worth testing the (parallel) minimum of these two quantities:
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
# [1] 8.9096e-05

# Trying different optimisers to get a different gradient statistic
modelfit.all <- lme4::allFit(s2_m_2)
is.OK <- sapply(modelfit.all, is,"merMod")
modelfit.all.OK <- modelfit.all[is.OK]
lapply(modelfit.all.OK, function(x) x@optinfo$conv$lme4$messages)
# $bobyqa
# NULL
# 
# $Nelder_Mead
# [1] "Model failed to converge with max|grad| = 0.708016 (tol = 0.002, component 1)"
# 
# $nlminbwrap
# [1] "boundary (singular) fit: see ?isSingular"
# 
# $nmkbw
# [1] "unable to evaluate scaled gradient"                                       
# [2] "Model failed to converge: degenerate  Hessian with 1 negative eigenvalues"
# 
# $`optimx.L-BFGS-B`
# [1] "boundary (singular) fit: see ?isSingular"
# 
# $nloptwrap.NLOPT_LN_NELDERMEAD
# [1] "boundary (singular) fit: see ?isSingular"
# 
# $nloptwrap.NLOPT_LN_BOBYQA
# [1] "boundary (singular) fit: see ?isSingular"

# Only BOBYQA managed to calculate the gradient 
# without exceeding lme4's convergence tolerance levels.

# Checking whether log likelihoods are similar.
(lliks <- sort(sapply(modelfit.all.OK,logLik)))
# optimx.L-BFGS-B     nloptwrap.NLOPT_LN_BOBYQA nloptwrap.NLOPT_LN_NELDERMEAD 
# -2770.825                     -2770.825                     -2770.825 
# Nelder_Mead                    nlminbwrap                         nmkbw 
# -2770.739                     -2770.613                     -2770.612 
# bobyqa 
# -2770.603 

# The above output shows that the likelihoods of the models produced using different 
# optimisers are within 1.

# Plotting the fixed effects estimates by optimiser.
modelfit.all.fixef <- t(sapply(modelfit.all.OK,fixef))
modelfit.all.fixef.m <- melt(modelfit.all.fixef)
models <- levels(modelfit.all.fixef.m$Var1)
ylabs <- substr(models, 1, 3)
modelfit.all.fixef.m <- transform(modelfit.all.fixef.m, Var1=factor(Var1, levels=names(lliks)))
(gplot1 <- ggplot(modelfit.all.fixef.m, aes(x=value,y=Var1,colour=Var1))+geom_point()+
    facet_wrap(~Var2, scale="free")+
    scale_colour_brewer(palette,"Dark2") +
    scale_y_discrete(breaks=models,
                     labels=ylabs)+
    labs(x="",y=""))

# Coefficients of variation of fixed-effect parameter estimates do not vary by more
# than 0.01 across optimisers.
summary(unlist(daply(modelfit.all.fixef.m,"Var2", summarise, sd(value)/abs(mean(value)))))
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0008102 0.0048510 0.0088919 0.0074823 0.0108183 0.0127448 

# Random effect estimates are sensitive to the choice of optimiser.
modelfit.all.stddev <- t(sapply(modelfit.all.OK, function(x) sqrt(unlist(VarCorr(x)))))
print(modelfit.all.stddev, digits = 3)
# uuid Post1 Post2 Post3 Post4  Post5  Post6 Post7  Post8 Post9
# bobyqa                        4.86 0.370   NaN   NaN   NaN 0.1269    NaN   NaN    NaN 0.439
# Nelder_Mead                   4.84 0.370   NaN   NaN   NaN 0.2106    NaN   NaN    NaN 0.393
# nlminbwrap                    4.86 0.368   NaN   NaN   NaN 0.0953    NaN   NaN    NaN 0.436
# nmkbw                         4.86 0.368   NaN   NaN   NaN 0.1003    NaN   NaN    NaN 0.435
# optimx.L-BFGS-B               4.87 0.376   NaN   NaN   NaN 0.0471 0.1000   NaN 0.1000 0.467
# nloptwrap.NLOPT_LN_NELDERMEAD 4.86 0.376   NaN   NaN   NaN 0.0470 0.1000   NaN 0.1000 0.467
# nloptwrap.NLOPT_LN_BOBYQA     4.86 0.376   NaN   NaN   NaN 0.0470 0.0999   NaN 0.0999 0.467

# NaNs are problematic, but BOBYQA does not detect lack of convergence. Keeping for now because
# the effect estimates using this model make sense.

# Calculating pseudo R2 (Nakagawa, Johnson, and Schielzeth, 2017)
# We favour the delta estimates.
r.squaredGLMM(s2_m_2)
# R2m       R2c
# theoretical 0.004167947 0.8790522
# delta       0.002996649 0.6320164

# Creating a multiple comparisons matrix for multiple comparisons.
contrast.matrix <- rbind(
  "Arm 1 vs 2" = c( 1, 1, 0)- c( 1, 0, 0),
  "Arm 1 vs 3" = c( 1, 0, 1)- c( 1, 0, 0),
  "Arm 2 vs 3" = c( 1, 0, 1)- c( 1, 1, 0))


glht.contrast.matrix_s2_m_2 <- glht(s2_m_2,contrast.matrix)

# Arm contrasts, s2_m_2
s2_m_2con <- summary(glht.contrast.matrix_s2_m_2, test=adjusted("bonferroni"))

# The log-odds of reacting are significantly lower in arm 3 compared to arm 1.
s2_m_2con
# Linear Hypotheses:
#   Estimate Std. Error z value Pr(>|z|)    
# Arm 1 vs 2 == 0  -0.4547     0.2924  -1.555   0.3599  
# Arm 1 vs 3 == 0  -0.8257     0.3386  -2.439   0.0442 *
# Arm 2 vs 3 == 0  -0.3710     0.3576  -1.037   0.8985  

# Converting model's estimates to odds ratios.
exp(s2_m_2con$coef)
# (Intercept)        ARM2        ARM3 
# 0.0002163818 0.6346450670 0.4379128915 

# Converting multiple comparisons estimates to odds ratios.
exp(s2_m_2con$test$coefficients)
# Arm 1 vs 2 Arm 1 vs 3 Arm 2 vs 3 
# 0.6346451  0.4379129  0.6900123 

# Calculating confidence intervals for multiple comparisons.
s2_m_2conci <- confint(s2_m_2con)
s2_m_2conci
# Quantile = 2.339
# 95% family-wise confidence level
# 
# 
# Linear Hypotheses:
#   Estimate lwr     upr    
# Arm 1 vs 2 == 0 -0.4547  -1.1387  0.2293
# Arm 1 vs 3 == 0 -0.8257  -1.6177 -0.0338
# Arm 2 vs 3 == 0 -0.3710  -1.2076  0.4655


# Converting the multiple comparisons' confidence intervals to odds ratios.
exp(s2_m_2conci$confint)
#            Estimate       lwr       upr
# Arm 1 vs 2 0.6346451 0.3202452 1.2577060
# Arm 1 vs 3 0.4379129 0.1983604 0.9667638
# Arm 2 vs 3 0.6900123 0.2989278 1.5927489

# Model summary table with odds ratios estimates.
tab_model(s2_m_2)