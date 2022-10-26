
# Moderation analysis using the primary outcome

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

# Random intercepts-only model
p_m_m_1 <- glmer(Primary ~ ARM + CRT_Score + ARM:CRT_Score + (1|uuid) + (1|Post), family = "binomial", data = p_m_262400278,
                 control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))

summary(p_m_m_1)
# Random effects:
#   Groups Name        Variance Std.Dev.
# uuid   (Intercept) 2.0805   1.4424  
# Post   (Intercept) 0.4991   0.7065  
# Number of obs: 36450, groups:  uuid, 2430; Post, 15
#
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)    -3.71037    0.25595 -14.496  < 2e-16 ***
#   ARM2           -0.54825    0.25627  -2.139  0.03241 *  
#   ARM3           -1.33361    0.28593  -4.664  3.1e-06 ***
#   CRT_Score      -0.13750    0.03956  -3.475  0.00051 ***
#   ARM2:CRT_Score  0.04067    0.05793   0.702  0.48261    
#   ARM3:CRT_Score  0.11142    0.06353   1.754  0.07947 .  


# p_m_m_1, diagnostics

# Creates scaled residuals by simulating from the fitted model (p_m_m_1)
# (new data will be simulated and scaled residuals will be created by comparing observed 
# data with new data.)
# Given 1000 simulations to stabilize the simulated values.
SimulatedResidualsP <- simulateResiduals(fittedModel = p_m_m_1, plot = F, n = 1000)

# Using these metrics, the model seems to be performing well
plot(SimulatedResidualsP)

# Singularity check -- the definition of singularity is that some of the constrained 
# parameters of the random effects theta parameters are on the boundary (equal to zero, 
# or very close to 0, such as -10e6)
tt <- getME(p_m_m_1, "theta")
ll <- getME(p_m_m_1, "lower")
min(tt[ll==0])
# [1] 0.7064888 -- no singularity

# Double check gradient calculations.
derivs1 <- p_m_m_1@optinfo$derivs
sc_grad1 <- with(derivs1, solve(Hessian, gradient))
max(abs(sc_grad1))
# [1] 1.993982e-05

# Large scaled gradients are often associated with small absolute gradients; 
# worth testing the (parallel) minimum of these two quantities:
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
# [1] 1.993982e-05

# Tolerance is typically set at 0.001; the gradient does not exceed this threshold.

# Trying different optimisers to get a different gradient statistic
modelfit.all <- lme4::allFit(p_m_m_1)
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
# [1] "Model failed to converge with max|grad| = 0.00553106 (tol = 0.002, component 1)"
# 
# $`optimx.L-BFGS-B`
# [1] "Model failed to converge with max|grad| = 0.0187213 (tol = 0.002, component 1)"
# 
# $nloptwrap.NLOPT_LN_NELDERMEAD
# NULL
# 
# $nloptwrap.NLOPT_LN_BOBYQA
# NULL

(lliks <- sort(sapply(modelfit.all.OK,logLik)))
# optimx.L-BFGS-B                         nmkbw nloptwrap.NLOPT_LN_NELDERMEAD 
# -4508.253                     -4508.252                     -4508.252 
# nloptwrap.NLOPT_LN_BOBYQA                   Nelder_Mead                    nlminbwrap 
# -4508.252                     -4508.252                     -4508.252 
# bobyqa 
# -4508.252 

# The above output shows that the likelihoods of the models produced using different 
# optimisers are nearly the same (within 0.001 of each other).

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
# optimisers

summary(unlist(daply(modelfit.all.fixef.m,"Var2", summarise, sd(value)/abs(mean(value)))))
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0004339 0.0009687 0.0017059 0.0020986 0.0021561 0.0057164  

# Standard deviation parameters are nearly the same (within 0.001 of each other)
modelfit.all.stddev <- t(sapply(modelfit.all.OK, function(x) sqrt(unlist(VarCorr(x)))))
print(modelfit.all.stddev, digits = 3)
# uuid  Post
# bobyqa                        1.44 0.706
# Nelder_Mead                   1.44 0.706
# nlminbwrap                    1.44 0.706
# nmkbw                         1.44 0.706
# optimx.L-BFGS-B               1.44 0.707
# nloptwrap.NLOPT_LN_NELDERMEAD 1.44 0.706
# nloptwrap.NLOPT_LN_BOBYQA     1.44 0.706


# Calculating pseudo R2 (Nakagawa, Johnson, and Schielzeth, 2017)
# We favour the delta estimates.
r.squaredGLMM(p_m_m_1)
# R2m        R2c
# theoretical 0.027343862 0.45482245
# delta       0.004147833 0.06899273


# Creating a multiple comparisons matrix for multiple comparisons.
contrast.matrix <- rbind(
  "Arm 1 vs 2 Baseline CRT" = c( 1, 1, 0, 0 ,0, 0)- c( 1, 0, 0, 0, 0, 0),
  "Arm 1 vs 3 Baseline CRT" = c( 1, 0, 1, 0, 0, 0)- c( 1, 0, 0, 0, 0, 0),
  "Arm 2 vs 3 Baseline CRT" = c( 1, 0, 1, 0, 0, 0)- c( 1, 1, 0, 0, 0, 0),
  
  "Arm 1 vs 2 + 1 CRT" = c( 1, 1, 0, 1 ,1, 0)- c( 1, 0, 0, 1, 0, 0),
  "Arm 1 vs 3 + 1 CRT" = c( 1, 0, 1, 1, 0, 1)- c( 1, 0, 0, 1, 0, 0),
  "Arm 2 vs 3 + 1 CRT" = c( 1, 0, 1, 1, 0, 1)- c( 1, 1, 0, 1, 1, 0))

glht.contrast.matrix_p_m_m_1 <- glht(p_m_m_1,contrast.matrix)

# Arm contrasts, p_m_m_1 (differences between arms at different CRT scores)
summary(glht.contrast.matrix_p_m_m_1, test=adjusted("bonferroni"))
# Linear Hypotheses:
#   Estimate Std. Error z value Pr(>|z|)    
#   Arm 1 vs 2 Baseline CRT == 0  -0.5482     0.2563  -2.139   0.1945    
#   Arm 1 vs 3 Baseline CRT == 0  -1.3336     0.2859  -4.664 1.86e-05 ***
#   Arm 2 vs 3 Baseline CRT == 0  -0.7854     0.2949  -2.663   0.0464 *  
#   Arm 1 vs 2 + 1 CRT == 0       -0.5076     0.2061  -2.463   0.0827 .  
#   Arm 1 vs 3 + 1 CRT == 0       -1.2222     0.2303  -5.306 6.72e-07 ***
#   Arm 2 vs 3 + 1 CRT == 0       -0.7146     0.2376  -3.008   0.0158 *  


# Creating a multiple comparisons matrix for multiple comparisons (unit increase in
# CRT within arms).
contrast.matrix_2 <- rbind(
  "Arm 1 Baseline CRT vs + 1 CRT" = c( 1, 0, 0, 1 ,0, 0)- c( 1, 0, 0, 0, 0, 0),
  "Arm 2 Baseline CRT vs + 1 CRT" = c( 1, 1, 0, 1, 1, 0)- c( 1, 1, 0, 0, 0, 0),
  "Arm 3 Baseline CRT vs + 1 CRT" = c( 1, 0, 1, 1, 0, 1)- c( 1, 0, 1, 0, 0, 0))

glht.contrast.matrix_p_m_m_2 <- glht(p_m_m_1,contrast.matrix_2)

# One unit increase in CRT has a significant effect on the log-odds of liking/loving
# in Arm 1, only (reduction in the log-odds of liking/loving with higher CRT score,
# compared to lower).
p_m_m_2con <- summary(glht.contrast.matrix_p_m_m_2, test=adjusted("bonferroni"))
p_m_m_2con
# Linear Hypotheses:
#   Estimate Std. Error z value Pr(>|z|)   
#   Arm 1 Baseline CRT vs + 1 CRT == 0 -0.13750    0.03956  -3.475  0.00153 **
#   Arm 2 Baseline CRT vs + 1 CRT == 0 -0.09683    0.04232  -2.288  0.06642 . 
#   Arm 3 Baseline CRT vs + 1 CRT == 0 -0.02608    0.04981  -0.524  1.00000   

# Converting model's estimates to odds ratios.
exp(p_m_m_2con$coef)
# (Intercept)           ARM2           ARM3      CRT_Score ARM2:CRT_Score ARM3:CRT_Score 
# 0.02446856     0.57796305     0.26352525     0.87153513     1.04150969     1.11785886 

# Converting multiple comparisons estimates to odds ratios.
exp(p_m_m_2con$test$coefficients)
# Arm 1 Baseline CRT vs + 1 CRT Arm 2 Baseline CRT vs + 1 CRT Arm 3 Baseline CRT vs + 1 CRT 
# 0.8715351                     0.9077123                     0.9742533 

# Calculating confidence intervals for multiple comparisons.
p_m_m_2conci <- confint(p_m_m_2con)
p_m_m_2conci
# Quantile = 2.3876
# 95% family-wise confidence level
# 
# 
# Linear Hypotheses:
#   Estimate  lwr       upr      
# Arm 1 Baseline CRT vs + 1 CRT == 0 -0.137499 -0.231961 -0.043037
# Arm 2 Baseline CRT vs + 1 CRT == 0 -0.096828 -0.197872  0.004216
# Arm 3 Baseline CRT vs + 1 CRT == 0 -0.026084 -0.145017  0.092849

# Converting the multiple comparisons' confidence intervals to odds ratios.
exp(p_m_m_2conci$confint)
#                                Estimate       lwr       upr
# Arm 1 Baseline CRT vs + 1 CRT 0.8715351 0.7929772 0.9578755
# Arm 2 Baseline CRT vs + 1 CRT 0.9077123 0.8204750 1.0042251
# Arm 3 Baseline CRT vs + 1 CRT 0.9742533 0.8650074 1.0972963

# p_m_m_1 summary table
tab_model(p_m_m_1)


# Sensitivity check of moderation analysis (given primary outcome).
# Secondary outcome 1 (reacting), misinformation posts only

# Moderation analysis using the first secondary outcome (reacting)

# Model with random differences of arm failed to converge
e_m_m_1 <- glmer(Accuracy_Reactions ~ ARM + CRT_Score + ARM:CRT_Score + (ARM + 1|uuid) + (ARM + 1|Post), family = "binomial", data = p_m_262400278,
                 control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))
# Warning message:
#   In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                  Model failed to converge with max|grad| = 0.00233158 (tol = 0.002, component 1)

# Replicating with a simplified model with no random differences (random intercepts, only)
e_m_m_2 <- glmer(Accuracy_Reactions ~ ARM + CRT_Score + ARM:CRT_Score + (1|uuid) + (1|Post), family = "binomial", data = p_m_262400278,
                 control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))

summary(e_m_m_2)
# Random effects:
#   Groups Name        Variance Std.Dev.
# uuid   (Intercept) 3.5316   1.8793  
# Post   (Intercept) 0.1118   0.3343  
# Number of obs: 36450, groups:  uuid, 2430; Post, 15
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)    -1.48955    0.19121  -7.790 6.69e-15 ***
#   ARM2           -0.61397    0.24467  -2.509   0.0121 *  
#   ARM3           -1.56106    0.25670  -6.081 1.19e-09 ***
#   CRT_Score      -0.20161    0.03796  -5.311 1.09e-07 ***
#   ARM2:CRT_Score  0.03983    0.05437   0.733   0.4638    
#   ARM3:CRT_Score  0.14366    0.05656   2.540   0.0111 *  

# e_m_m_2, diagnostics

# Creates scaled residuals by simulating from the fitted model (e_m_m_2)
# (new data will be simulated and scaled residuals will be created by comparing observed 
# data with new data.)
# Given 1000 simulations to stabilize the simulated values.
SimulatedResidualsP2 <- simulateResiduals(fittedModel = e_m_m_2, plot = F, n = 1000)

# Using these metrics, the model seems to be performing well
plot(SimulatedResidualsP2)

# Singularity check -- the definition of singularity is that some of the constrained 
# parameters of the random effects theta parameters are on the boundary (equal to zero, 
# or very close to 0, such as -10e6)
tt <- getME(e_m_m_2, "theta")
ll <- getME(e_m_m_2, "lower")
min(tt[ll==0])
# [1] 0.3343311 -- no singularity

# Double check gradient calculations.
derivs1 <- e_m_m_2@optinfo$derivs
sc_grad1 <- with(derivs1, solve(Hessian, gradient))
max(abs(sc_grad1))
# 4.352802e-05

# Large scaled gradients are often associated with small absolute gradients; 
# worth testing the (parallel) minimum of these two quantities:
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
# 4.352802e-05 

# Tolerance is typically set at 0.001; the gradient does not exceed this threshold.

# Trying different optimisers to get a different gradient statistic
modelfit.all <- lme4::allFit(e_m_m_2)
is.OK <- sapply(modelfit.all, is,"merMod")
modelfit.all.OK <- modelfit.all[is.OK]
lapply(modelfit.all.OK, function(x) x@optinfo$conv$lme4$messages)
# $bobyqa
# NULL
# 
# $Nelder_Mead
# [1] "Model failed to converge with max|grad| = 0.0361604 (tol = 0.002, component 1)"
# 
# $nlminbwrap
# $nlminbwrap[[1]]
# [1] "Model failed to converge with max|grad| = 0.485143 (tol = 0.002, component 1)"
# 
# $nlminbwrap[[2]]
# [1] "Model is nearly unidentifiable: very large eigenvalue\n - Rescale variables?"
# 
# 
# $nmkbw
# [1] "Model failed to converge with max|grad| = 0.00218934 (tol = 0.002, component 1)"
# 
# $`optimx.L-BFGS-B`
# [1] "unable to evaluate scaled gradient"                                       
# [2] "Model failed to converge: degenerate  Hessian with 2 negative eigenvalues"
# 
# $nloptwrap.NLOPT_LN_NELDERMEAD
# [1] "Model failed to converge with max|grad| = 0.00345628 (tol = 0.002, component 1)"
# 
# $nloptwrap.NLOPT_LN_BOBYQA
# [1] "Model failed to converge with max|grad| = 0.00591235 (tol = 0.002, component 1)"

# Only BOBYQA managed to calculate the gradient 
# without exceeding lme4's convergence tolerance levels.

(lliks <- sort(sapply(modelfit.all.OK,logLik)))
# nloptwrap.NLOPT_LN_BOBYQA                        bobyqa                    nlminbwrap 
# -12030.77                     -12030.77                     -12030.53 
# optimx.L-BFGS-B                   Nelder_Mead nloptwrap.NLOPT_LN_NELDERMEAD 
# -12030.41                     -12030.36                     -12030.35 
# nmkbw 
# -12030.35 

# The above output shows that the log likelihoods of the models produced using different 
# optimisers are within 1 of each other.

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

# Coefficients of variation of fixed-effect parameter estimates are somewhat different
# between optimisers.

summary(unlist(daply(modelfit.all.fixef.m,"Var2", summarise, sd(value)/abs(mean(value)))))
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.01388 0.02141 0.03165 0.04918 0.06629 0.12208 

# Standard deviation parameters are nearly the same (within 0.01 of each other)
modelfit.all.stddev <- t(sapply(modelfit.all.OK, function(x) sqrt(unlist(VarCorr(x)))))
print(modelfit.all.stddev, digits = 3)
# uuid  Post
# bobyqa                        1.88 0.334
# Nelder_Mead                   1.89 0.328
# nlminbwrap                    1.89 0.337
# nmkbw                         1.89 0.328
# optimx.L-BFGS-B               1.89 0.329
# nloptwrap.NLOPT_LN_NELDERMEAD 1.89 0.328
# nloptwrap.NLOPT_LN_BOBYQA     1.88 0.334



# Calculating pseudo R2 (Nakagawa, Johnson, and Schielzeth, 2017)
# We favour the delta estimates.
r.squaredGLMM(e_m_m_2)
# R2m       R2c
# theoretical 0.03436821 0.5418023
# delta       0.02087729 0.3291229

# Creating a multiple comparisons matrix for multiple comparisons.
contrast.matrix <- rbind(
  "Arm 1 vs 2 Baseline CRT" = c( 1, 1, 0, 0 ,0, 0)- c( 1, 0, 0, 0, 0, 0),
  "Arm 1 vs 3 Baseline CRT" = c( 1, 0, 1, 0, 0, 0)- c( 1, 0, 0, 0, 0, 0),
  "Arm 2 vs 3 Baseline CRT" = c( 1, 0, 1, 0, 0, 0)- c( 1, 1, 0, 0, 0, 0),
  
  "Arm 1 vs 2 + 1 CRT" = c( 1, 1, 0, 1 ,1, 0)- c( 1, 0, 0, 1, 0, 0),
  "Arm 1 vs 3 + 1 CRT" = c( 1, 0, 1, 1, 0, 1)- c( 1, 0, 0, 1, 0, 0),
  "Arm 2 vs 3 + 1 CRT" = c( 1, 0, 1, 1, 0, 1)- c( 1, 1, 0, 1, 1, 0))

glht.contrast.matrix_e_m_m_2 <- glht(e_m_m_2,contrast.matrix)

# Arm contrasts, p_m_m_1 (differences between arms at different CRT scores)
summary(glht.contrast.matrix_e_m_m_2, test=adjusted("bonferroni"))
# Linear Hypotheses:
#   Estimate Std. Error z value Pr(>|z|)    
#   Arm 1 vs 2 Baseline CRT == 0  -0.6140     0.2447  -2.509 0.072561 .  
#   Arm 1 vs 3 Baseline CRT == 0  -1.5611     0.2567  -6.081 7.15e-09 ***
#   Arm 2 vs 3 Baseline CRT == 0  -0.9471     0.2601  -3.641 0.001629 ** 
#   Arm 1 vs 2 + 1 CRT == 0       -0.5741     0.1971  -2.914 0.021436 *  
#   Arm 1 vs 3 + 1 CRT == 0       -1.4174     0.2069  -6.851 4.40e-11 ***
#   Arm 2 vs 3 + 1 CRT == 0       -0.8433     0.2096  -4.023 0.000345 ***


# Creating a multiple comparisons matrix for multiple comparisons (unit increase in
# CRT within arms).
contrast.matrix_2 <- rbind(
  "Arm 1 Baseline CRT vs + 1 CRT" = c( 1, 0, 0, 1 ,0, 0)- c( 1, 0, 0, 0, 0, 0),
  "Arm 2 Baseline CRT vs + 1 CRT" = c( 1, 1, 0, 1, 1, 0)- c( 1, 1, 0, 0, 0, 0),
  "Arm 3 Baseline CRT vs + 1 CRT" = c( 1, 0, 1, 1, 0, 1)- c( 1, 0, 1, 0, 0, 0))

glht.contrast.matrix_e_m_m_3 <- glht(e_m_m_2,contrast.matrix_2)

# One unit increase in CRT has a significant effect on the log-odds of reacting
# in Arm 1 and Arm 2 (reduction in the log-odds of reacting with higher CRT score,
# compared to lower).
e_m_m_3con <- summary(glht.contrast.matrix_e_m_m_3, test=adjusted("bonferroni"))
e_m_m_3con
# Linear Hypotheses:
#   Estimate Std. Error z value Pr(>|z|)   
#   Arm 1 Baseline CRT vs + 1 CRT == 0 -0.20161    0.03796  -5.311 3.28e-07 ***
#   Arm 2 Baseline CRT vs + 1 CRT == 0 -0.16178    0.03896  -4.152 9.87e-05 ***
#   Arm 3 Baseline CRT vs + 1 CRT == 0 -0.05794    0.04194  -1.381    0.501   

# Converting model's estimates to odds ratios.
exp(e_m_m_3con$coef)
# (Intercept)           ARM2           ARM3      CRT_Score ARM2:CRT_Score ARM3:CRT_Score 
# 0.2254740      0.5412007      0.2099133      0.8174150      1.0406301      1.1544966 

# Converting multiple comparisons estimates to odds ratios.
exp(e_m_m_3con$test$coefficients)
# Arm 1 Baseline CRT vs + 1 CRT Arm 2 Baseline CRT vs + 1 CRT Arm 3 Baseline CRT vs + 1 CRT 
# 0.8174150                     0.8506267                     0.9437029 

# Calculating confidence intervals for multiple comparisons.
e_m_m_3conci <- confint(e_m_m_3con)
e_m_m_3conci
# Quantile = 2.3876
# 95% family-wise confidence level
# 
# 
# Linear Hypotheses:
#   Estimate  lwr       upr      
# Arm 1 Baseline CRT vs + 1 CRT == 0 -0.20161 -0.29225 -0.11097
# Arm 2 Baseline CRT vs + 1 CRT == 0 -0.16178 -0.25480 -0.06876
# Arm 3 Baseline CRT vs + 1 CRT == 0 -0.05794 -0.15809  0.04220

# Converting the multiple comparisons' confidence intervals to odds ratios.
exp(e_m_m_3conci$confint)
#                                Estimate       lwr       upr
# Arm 1 Baseline CRT vs + 1 CRT 0.8174150 0.7465825 0.8949678
# Arm 2 Baseline CRT vs + 1 CRT 0.8506267 0.7750686 0.9335505
# Arm 3 Baseline CRT vs + 1 CRT 0.9437029 0.8537749 1.0431030

# e_m_m_2 summary table
tab_model(e_m_m_2)

#######################################################################
# Moderation plot

# Load, additional, required libraries
library(cowplot)
library(svglite)

# Set the colour scheme palette
cbbPalette <- c("#D55E00", "#009E73", "#0072B2")


# Create a plot of the predicted probabilities of liking/loving using p_m_m_1's predictions
# of the effects of changes in the CRT score by intervention Arm.
primary_crt <- plot_model(p_m_m_1, type = "pred", terms = c("CRT_Score", "ARM"), 
                          axis.title = c("CRT","Probability of liking/loving"), 
                          title = "", colors = cbbPalette) + theme_minimal() + theme(legend.position="top") +
  theme(axis.text.x = element_text(size=16,  color = "black"),
        axis.text.y = element_text(size=16, color = "black")) +
  theme(axis.title = element_text(size=20, face="bold")) + 
  theme(axis.title.x = element_text(vjust=-0.5)) + 
  scale_x_continuous("CRT Score", labels = as.character(p_m_262400278$CRT_Score), 
                     breaks = p_m_262400278$CRT_Score)

# Rename levels of the ARM variable
levels(primary_crt$data$group_col) <- c("Control", "False tag", "Inoculation")

# Plot primary_crt
primary_crt


# Create a plot of the predicted probabilities of reacting using e_m_m_2's predictions
# of the effects of changes in the CRT score by intervention Arm.
sensitivity_crt <- plot_model(e_m_m_2, type = "pred", terms = c("CRT_Score", "ARM"), 
                              axis.title = c("CRT","Probability of reacting"), 
                              title = "", colors = cbbPalette) + theme_minimal() + theme(legend.position="top") +
  theme(axis.text.x = element_text(size=16,  color = "black"),
        axis.text.y = element_text(size=16, color = "black")) +
  theme(axis.title = element_text(size=20, face="bold")) + 
  theme(axis.title.x = element_text(vjust=-0.5)) + 
  scale_x_continuous("CRT Score", labels = as.character(p_m_262400278$CRT_Score), 
                     breaks = p_m_262400278$CRT_Score)

# Rename levels of the ARM variable
levels(sensitivity_crt$data$group_col) <- c("Control", "False tag", "Inoculation")

# Plot sensitivity_crt
sensitivity_crt


# Combine the two plot objects into one
pred_prob <- plot_grid(primary_crt, sensitivity_crt)

# Plot pred_prob
pred_prob


# Save the object in the working directory as an svg file
# setwd("")

# svglite("pred_prob.svg", width=14, height=6)
# 
# plot_grid(primary_crt, sensitivity_crt)
# 
# dev.off()