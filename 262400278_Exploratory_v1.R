
# Exploratory analyses (legitimate posts)

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
s_m_262400278 <- read.csv("s_m_262400278_v1.csv")

# Subset the data to include only legitimate posts
l_m_262400278 <- subset(s_m_262400278, Misinfo < 1)

# Treat arm as a factor
l_m_262400278$ARM <- as.factor(l_m_262400278$ARM)

# Replication of the primary model used for the analysis of misinformation posts (Likes/loves)
l_m_4 <-  glmer(Primary ~ ARM + (1|uuid) + (1|Post), family = "binomial", data = l_m_262400278,
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))

summary(l_m_4)
# Random effects:
#   Groups Name        Variance Std.Dev.
# uuid   (Intercept) 2.5772   1.6054  
# Post   (Intercept) 0.7928   0.8904  
# Number of obs: 36450, groups:  uuid, 2430; Post, 15
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -3.25927    0.24043 -13.556  < 2e-16 ***
#   ARM2        -0.06655    0.09644  -0.690  0.49014    
#   ARM3        -0.30351    0.09851  -3.081  0.00206 ** 


# l_m_4, diagnostics

# Creates scaled residuals by simulating from the fitted model (l_m_4)
# (new data will be simulated and scaled residuals will be created by comparing observed 
# data with new data.)
# Given 1000 simulations to stabilize the simulated values.
SimulatedResidualsl_m_4 <- simulateResiduals(fittedModel = l_m_4, plot = F, n = 1000)

# Using these metrics, the model seems to be performing well
plot(SimulatedResidualsl_m_4)

# Singularity check -- the definition of singularity is that some of the constrained 
# parameters of the random effects theta parameters are on the boundary (equal to zero, 
# or very close to 0, such as -10e6)
tt <- getME(l_m_4, "theta")
ll <- getME(l_m_4, "lower")
min(tt[ll==0])
# [1] 0.8904184 -- no singularity

# Double check gradient calculations.
derivs1 <- l_m_4@optinfo$derivs
sc_grad1 <- with(derivs1, solve(Hessian, gradient))
max(abs(sc_grad1))
# [1] 1.533921e-05

# Large scaled gradients are often associated with small absolute gradients; 
# worth testing the (parallel) minimum of these two quantities:
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
# [1] 1.533921e-05

# Tolerance is typically set at 0.001; the gradient does not exceed this threshold.

# Trying different optimisers to get a different gradient statistic
modelfit.all <- lme4::allFit(l_m_4)
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
# [1] "Model failed to converge with max|grad| = 0.00214501 (tol = 0.002, component 1)"
# 
# $nloptwrap.NLOPT_LN_BOBYQA
# NULL

# Only one model, with nloptwrap.NLOPT_LN_NELDERMEAD optimiser, failed to converge.

(lliks <- sort(sapply(modelfit.all.OK,logLik)))
# nmkbw                   Nelder_Mead                        bobyqa 
# -9534.809                     -9534.809                     -9534.809 
# optimx.L-BFGS-B                    nlminbwrap     nloptwrap.NLOPT_LN_BOBYQA 
# -9534.809                     -9534.809                     -9534.809 
# nloptwrap.NLOPT_LN_NELDERMEAD 
# -9534.809 

# The above output shows that the likelihoods of the models produced using different 
# optimisers are the same.

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
# 2.818e-05 1.024e-04 1.766e-04 3.390e-04 4.943e-04 8.121e-04 

# Standard deviation parameters are nearly the same (within 0.001 of each other)
modelfit.all.stddev <- t(sapply(modelfit.all.OK, function(x) sqrt(unlist(VarCorr(x)))))
print(modelfit.all.stddev, digits = 3)
# uuid  Post
# bobyqa                        1.61 0.890
# Nelder_Mead                   1.61 0.890
# nlminbwrap                    1.61 0.890
# nmkbw                         1.61 0.890
# optimx.L-BFGS-B               1.61 0.890
# nloptwrap.NLOPT_LN_NELDERMEAD 1.61 0.891
# nloptwrap.NLOPT_LN_BOBYQA     1.61 0.890


# Calculating pseudo R2 (Nakagawa, Johnson, and Schielzeth, 2017)
# We favour the delta estimates.
r.squaredGLMM(l_m_4)
# R2m        R2c
# theoretical 0.002512311 0.5072569
# delta       0.001095173 0.2211246

# Creating a multiple comparisons matrix for multiple comparisons.
contrast.matrix <- rbind(
  "Arm 1 vs 2" = c( 1, 1, 0)- c( 1, 0, 0),
  "Arm 1 vs 3" = c( 1, 0, 1)- c( 1, 0, 0),
  "Arm 2 vs 3" = c( 1, 0, 1)- c( 1, 1, 0))

glht.contrast.matrix_l_m_4 <- glht(l_m_4,contrast.matrix)

# Arm contrasts, replication of the primary model (with legitimate posts)
l_m_4con <- summary(glht.contrast.matrix_l_m_4, test=adjusted("bonferroni"))

# The log-odds of liking/loving legitimate posts are significantly lower in arm 3 
# compared to arm 1, and in arm 3 compared to arm 2. No difference in the log-odds 
# of liking/loving legitimate posts between arm 1 and arm 2.
l_m_4con
# Linear Hypotheses:
#                   Estimate Std. Error z value Pr(>|z|)   
# Arm 1 vs 2 == 0 -0.06655    0.09644  -0.690  1.00000   
# Arm 1 vs 3 == 0 -0.30351    0.09851  -3.081  0.00619 **
# Arm 2 vs 3 == 0 -0.23696    0.09869  -2.401  0.04905 * 

# Converting model's estimates to odds ratios.
exp(l_m_4con$coef)
# (Intercept)        ARM2        ARM3 
# 0.0384163   0.9356138   0.7382198 

# Converting multiple comparisons estimates to odds ratios.
exp(l_m_4con$test$coefficients)
# Arm 1 vs 2 Arm 1 vs 3 Arm 2 vs 3 
# 0.9356138  0.7382198  0.7890219 

# Calculating confidence intervals for multiple comparisons.
l_m_4conci <- confint(l_m_4con)
l_m_4conci
# Quantile = 2.3434
# 95% family-wise confidence level
# 
# 
# Linear Hypotheses:
#   Estimate  lwr       upr      
# Arm 1 vs 2 == 0 -0.066552 -0.292553  0.159448
# Arm 1 vs 3 == 0 -0.303514 -0.534370 -0.072658
# Arm 2 vs 3 == 0 -0.236961 -0.468236 -0.005686

# Converting the multiple comparisons' confidence intervals to odds ratios.
exp(l_m_4conci$confint)
#             Estimate       lwr       upr
# Arm 1 vs 2 0.9356138 0.7463560 1.1728629
# Arm 1 vs 3 0.7382198 0.5860386 0.9299191
# Arm 2 vs 3 0.7890219 0.6261055 0.9943302

# Model summary table
tab_model(l_m_4)



# Replication of the secondary model used for the analysis of misinformation posts (reacting)

sl_m_1 <- glmer(Accuracy_Reactions ~ ARM + (ARM + 1|uuid) + (ARM + 1|Post), family = "binomial", data = l_m_262400278,
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))
summary(sl_m_1)
# Random effects:
#   Groups Name        Variance Std.Dev. Corr       
# uuid   (Intercept) 2.857337 1.69037             
# ARM2        1.014066 1.00701  -0.25      
# ARM3        1.200397 1.09563   0.03 -0.02
# Post   (Intercept) 0.435470 0.65990             
# ARM2        0.002568 0.05068  -0.99      
# ARM3        0.154717 0.39334  -0.39  0.28
# Number of obs: 36450, groups:  uuid, 2430; Post, 15
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -1.71604    0.18287  -9.384  < 2e-16 ***
#   ARM2        -0.13464    0.09843  -1.368    0.171    
#   ARM3        -0.69236    0.14880  -4.653 3.27e-06 ***

# The model with random differences of arm converges, but the correlation of -0.99 is
# implausible.

# Simplifying the model by defaulting to a random intercepts only model.
sl_m_4 <- glmer(Accuracy_Reactions ~ ARM + (1|uuid) + (1|Post), family = "binomial", data = l_m_262400278,
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))
summary(sl_m_4)
# Random effects:
#   Groups Name        Variance Std.Dev.
# uuid   (Intercept) 3.2470   1.8019  
# Post   (Intercept) 0.3666   0.6055  
# Number of obs: 36450, groups:  uuid, 2430; Post, 15
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -1.73546    0.17101 -10.148  < 2e-16 ***
#   ARM2        -0.13518    0.09835  -1.374    0.169    
#   ARM3        -0.55632    0.10054  -5.534 3.14e-08 ***

# The direction of the effects of the interventions is not sensitive to the random effects
# structure of models (random differences versus random intercepts only).

# Due to the implausible correlation of -0.99, sl_m_4 will be used for inference.

# sl_m_4, diagnostics

# Creates scaled residuals by simulating from the fitted model (sl_m_4)
# (new data will be simulated and scaled residuals will be created by comparing observed 
# data with new data.)
# Given 1000 simulations to stabilize the simulated values.
SimulatedResidualssl_m_4 <- simulateResiduals(fittedModel = sl_m_4, plot = F, n = 1000)

# Using these metrics, the model seems to be performing well
plot(SimulatedResidualssl_m_4)

# Singularity check -- the definition of singularity is that some of the constrained 
# parameters of the random effects theta parameters are on the boundary (equal to zero, 
# or very close to 0, such as -10e6)
tt <- getME(sl_m_4, "theta")
ll <- getME(sl_m_4, "lower")
min(tt[ll==0])
# [1] 0.6054817 -- no singularity

# Double check gradient calculations.
derivs1 <- sl_m_4@optinfo$derivs
sc_grad1 <- with(derivs1, solve(Hessian, gradient))
max(abs(sc_grad1))
# [1] 7.51619e-06

# Large scaled gradients are often associated with small absolute gradients; 
# worth testing the (parallel) minimum of these two quantities:
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
# [1] 7.51619e-06

# Tolerance is typically set at 0.001; the gradient does not exceed this threshold.

# Trying different optimisers to get a different gradient statistic
modelfit.all <- lme4::allFit(sl_m_4)
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

# All models converged.

(lliks <- sort(sapply(modelfit.all.OK,logLik)))
# nmkbw     nloptwrap.NLOPT_LN_BOBYQA                   Nelder_Mead 
# -15638.92                     -15638.92                     -15638.92 
# bobyqa                    nlminbwrap               optimx.L-BFGS-B 
# -15638.92                     -15638.92                     -15638.92 
# nloptwrap.NLOPT_LN_NELDERMEAD 
# -15638.92 

# The above output shows that the log likelihoods of the models produced using different 
# optimisers are the same.

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
# 2.666e-05 4.306e-05 5.945e-05 9.010e-05 1.218e-04 1.842e-04 

# Standard deviation parameters are nearly the same (within 0.001 of each other)
modelfit.all.stddev <- t(sapply(modelfit.all.OK, function(x) sqrt(unlist(VarCorr(x)))))
print(modelfit.all.stddev, digits = 3)
# uuid  Post
# bobyqa                         1.8 0.605
# Nelder_Mead                    1.8 0.605
# nlminbwrap                     1.8 0.606
# nmkbw                          1.8 0.605
# optimx.L-BFGS-B                1.8 0.606
# nloptwrap.NLOPT_LN_NELDERMEAD  1.8 0.605
# nloptwrap.NLOPT_LN_BOBYQA      1.8 0.606


# Calculating pseudo R2 (Nakagawa, Johnson, and Schielzeth, 2017)
# We favour the delta estimates.
r.squaredGLMM(sl_m_4)
# R2m        R2c
# theoretical 0.007975172 0.5272460
# delta       0.005941058 0.3927688


# Creating a multiple comparisons matrix for multiple comparisons.
contrast.matrix <- rbind(
  "Arm 1 vs 2" = c( 1, 1, 0)- c( 1, 0, 0),
  "Arm 1 vs 3" = c( 1, 0, 1)- c( 1, 0, 0),
  "Arm 2 vs 3" = c( 1, 0, 1)- c( 1, 1, 0))

glht.contrast.matrix_sl_m_4 <- glht(sl_m_4,contrast.matrix)

# Arm contrasts, replication of the secondary model (reacting, with legitimate posts)
sl_m_4con <- summary(glht.contrast.matrix_sl_m_4, test=adjusted("bonferroni"))

# The log-odds of reacting to legitimate posts are significantly lower in arm 3 
# compared to arm 1, and in arm 3 compared to arm 2. No difference in the log-odds 
# of reacting to legitimate posts between arm 1 and arm 2.
# Same as l_m_4con (liking/loving model of legitimate posts).
sl_m_4con
# Linear Hypotheses:
#                   Estimate Std. Error z value Pr(>|z|)   
# Arm 1 vs 2 == 0 -0.13518    0.09835  -1.374    0.508    
# Arm 1 vs 3 == 0 -0.55632    0.10054  -5.534 9.42e-08 ***
# Arm 2 vs 3 == 0 -0.42114    0.10075  -4.180 8.73e-05 ***

# Converting model's estimates to odds ratios.
exp(sl_m_4con$coef)
# (Intercept)        ARM2        ARM3 
# 0.1763189   0.8735610   0.5733147 

# Converting multiple comparisons estimates to odds ratios.
exp(sl_m_4con$test$coefficients)
# Arm 1 vs 2 Arm 1 vs 3 Arm 2 vs 3 
# 0.8735610  0.5733147  0.6562961

# Calculating confidence intervals for multiple comparisons.
sl_m_4conci <- confint(sl_m_4con)
sl_m_4conci
# Quantile = 2.3445
# 95% family-wise confidence level
# 
# 
# Linear Hypotheses:
#   Estimate lwr     upr    
# Arm 1 vs 2 == 0 -0.1352  -0.3658  0.0954
# Arm 1 vs 3 == 0 -0.5563  -0.7920 -0.3206
# Arm 2 vs 3 == 0 -0.4211  -0.6573 -0.1849

# Converting the multiple comparisons' confidence intervals to odds ratios.
exp(sl_m_4conci$confint)
#             Estimate       lwr       upr
# Arm 1 vs 2 0.8735610 0.6936705 1.1001027
# Arm 1 vs 3 0.5733147 0.4529231 0.7257076
# Arm 2 vs 3 0.6562961 0.5182257 0.8311524

# Model summary table
tab_model(sl_m_4)

# Summary table of l_m_4 and sl_m_4 models
tab_model(l_m_4, sl_m_4)
# Replication using two outcomes (in terms of the direction of the effects)


# Replication of the secondary model used for the analysis of misinformation posts (Shares)

shl_m_2 <- glmer(Share_Accuracy ~ ARM + (1|uuid) + (ARM + 1|Post), family = "binomial", data = l_m_262400278,
                 control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))
summary(shl_m_2)
# Random effects:
#   Groups Name        Variance Std.Dev. Corr       
# uuid   (Intercept) 3.917500 1.97927             
# Post   (Intercept) 0.393661 0.62742             
# ARM2        0.002207 0.04698  -1.00      
# ARM3        0.048994 0.22135  -0.17  0.23
# Number of obs: 36450, groups:  uuid, 2430; Post, 15
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  -4.2808     0.1979 -21.628  < 2e-16 ***
#   ARM2         -0.1033     0.1394  -0.741    0.459    
#   ARM3         -0.6656     0.1591  -4.184 2.86e-05 ***

# The model with random differences of arm converges, but the correlation of -1 is
# implausible.

# Simplifying the model by defaulting to a random intercepts only model.
shl_m_4 <- glmer(Share_Accuracy ~ ARM + (1|uuid) + (1|Post), family = "binomial", data = l_m_262400278,
                 control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)))
summary(shl_m_4)

# Random effects:
#   Groups Name        Variance Std.Dev.
# uuid   (Intercept) 3.902    1.9752  
# Post   (Intercept) 0.365    0.6042  
# Number of obs: 36450, groups:  uuid, 2430; Post, 15
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  -4.2605     0.1916 -22.233  < 2e-16 ***
#   ARM2         -0.1421     0.1333  -1.066    0.286    
#   ARM3         -0.6575     0.1404  -4.682 2.84e-06 ***

# The direction of the effects of the interventions is not sensitive to the random effects
# structure of models (random differences versus random intercepts only).

# Due to the implausible correlation of -1, shl_m_4 will be used for inference.

# shl_m_4, diagnostics

# Creates scaled residuals by simulating from the fitted model (shl_m_4)
# (new data will be simulated and scaled residuals will be created by comparing observed 
# data with new data.)
# Given 1000 simulations to stabilize the simulated values.
SimulatedResidualsshl_m_4 <- simulateResiduals(fittedModel = shl_m_4, plot = F, n = 1000)

# Using these metrics, the model seems to be performing well
plot(SimulatedResidualsshl_m_4)

# Singularity check -- the definition of singularity is that some of the constrained 
# parameters of the random effects theta parameters are on the boundary (equal to zero, 
# or very close to 0, such as -10e6)
tt <- getME(shl_m_4, "theta")
ll <- getME(shl_m_4, "lower")
min(tt[ll==0])
# [1] 0.6041565 -- no singularity

# Double check gradient calculations.
derivs1 <- shl_m_4@optinfo$derivs
sc_grad1 <- with(derivs1, solve(Hessian, gradient))
max(abs(sc_grad1))
# [1] 2.704338e-06

# Large scaled gradients are often associated with small absolute gradients; 
# worth testing the (parallel) minimum of these two quantities:
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
# [1] 2.704338e-06

# Tolerance is typically set at 0.001; the gradient does not exceed this threshold.

# Trying different optimisers to get a different gradient statistic
modelfit.all <- lme4::allFit(shl_m_4)
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
# [1] "Model failed to converge with max|grad| = 0.00265037 (tol = 0.002, component 1)"
# 
# $`optimx.L-BFGS-B`
# NULL
# 
# $nloptwrap.NLOPT_LN_NELDERMEAD
# NULL
# 
# $nloptwrap.NLOPT_LN_BOBYQA
# NULL

# Only one model, with nmkbw optimiser, failed to converge.

(lliks <- sort(sapply(modelfit.all.OK,logLik)))
# nmkbw nloptwrap.NLOPT_LN_NELDERMEAD     nloptwrap.NLOPT_LN_BOBYQA 
# -6420.954                     -6420.954                     -6420.954 
# nlminbwrap                   Nelder_Mead                        bobyqa 
# -6420.954                     -6420.954                     -6420.954 
# optimx.L-BFGS-B 
# -6420.954 

# The above output shows that the likelihoods of the models produced using different 
# optimisers are the same.

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
# 1.412e-05 8.287e-05 1.516e-04 2.150e-04 3.155e-04 4.793e-04 

# Standard deviation parameters are the same
modelfit.all.stddev <- t(sapply(modelfit.all.OK, function(x) sqrt(unlist(VarCorr(x)))))
print(modelfit.all.stddev, digits = 3)
# uuid  Post
# bobyqa                        1.98 0.604
# Nelder_Mead                   1.98 0.604
# nlminbwrap                    1.98 0.604
# nmkbw                         1.98 0.604
# optimx.L-BFGS-B               1.98 0.604
# nloptwrap.NLOPT_LN_NELDERMEAD 1.98 0.604
# nloptwrap.NLOPT_LN_BOBYQA     1.98 0.604


# Calculating pseudo R2 (Nakagawa, Johnson, and Schielzeth, 2017)
# We favour the delta estimates.
r.squaredGLMM(shl_m_4)
# R2m        R2c
# theoretical 0.010330919 0.5691245
# delta       0.003118038 0.1717709


# Creating a multiple comparisons matrix for multiple comparisons.
contrast.matrix <- rbind(
  "Arm 1 vs 2" = c( 1, 1, 0)- c( 1, 0, 0),
  "Arm 1 vs 3" = c( 1, 0, 1)- c( 1, 0, 0),
  "Arm 2 vs 3" = c( 1, 0, 1)- c( 1, 1, 0))

glht.contrast.matrix_shl_m_4 <- glht(shl_m_4,contrast.matrix)

# Arm contrasts, replication of the secondary model (sharing, with legitimate posts)
shl_m_4con <- summary(glht.contrast.matrix_shl_m_4, test=adjusted("bonferroni"))

# The log-odds of sharing legitimate posts are significantly lower in arm 3 
# compared to arm 1, and in arm 3 compared to arm 2. No difference in the log-odds 
# of sharing legitimate posts between arm 1 and arm 2.
# Same as l_m_4con (liking/loving model of legitimate posts) and sl_m_4con (reacting to
# legitimate posts model).
shl_m_4con
# Linear Hypotheses:
#                   Estimate Std. Error z value Pr(>|z|)   
# Arm 1 vs 2 == 0  -0.1421     0.1333  -1.066 0.858651    
# Arm 1 vs 3 == 0  -0.6575     0.1404  -4.682 8.51e-06 ***
# Arm 2 vs 3 == 0  -0.5154     0.1411  -3.652 0.000782 ***

# Converting model's estimates to odds ratios.
exp(shl_m_4con$coef)
# (Intercept)        ARM2        ARM3 
# 0.01411514  0.86751979  0.51815006 

# Converting multiple comparisons estimates to odds ratios.
exp(shl_m_4con$test$coefficients)
# Arm 1 vs 2 Arm 1 vs 3 Arm 2 vs 3 
# 0.8675198  0.5181501  0.5972775 

# Calculating confidence intervals for multiple comparisons.
shl_m_4conci <- confint(shl_m_4con)
shl_m_4conci
# Quantile = 2.3431
# 95% family-wise confidence level
# 
# 
# Linear Hypotheses:
#   Estimate lwr     upr    
# Arm 1 vs 2 == 0 -0.1421  -0.4544  0.1701
# Arm 1 vs 3 == 0 -0.6575  -0.9865 -0.3285
# Arm 2 vs 3 == 0 -0.5154  -0.8461 -0.1847

# Converting the multiple comparisons' confidence intervals to odds ratios.
exp(shl_m_4conci$confint)
#             Estimate       lwr       upr
# Arm 1 vs 2 0.8675198 0.6348511 1.1854599
# Arm 1 vs 3 0.5181501 0.3728771 0.7200214
# Arm 2 vs 3 0.5972775 0.4290922 0.8313840

# Model summary table
tab_model(shl_m_4)

# Summary table of l_m_4, sl_m_4, and shl_m_4 models
tab_model(l_m_4, sl_m_4, shl_m_4)
# Replication using three outcomes (in terms of the direction of the effects)