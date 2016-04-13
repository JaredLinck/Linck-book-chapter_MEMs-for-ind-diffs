##### Cognitive Individual Differences book
#####   Sample code for "Analyzing individual differences in second language research with mixed effects models"
#####       Jared A. Linck
#####       University of Maryland
#####
#####   The models below were fitted using R version 3.1.2 and lme4 version 1.1-7 in RStudio on a Windows machine.

### clear workspace
rm(list = ls())

### Load lme4 library
library(lme4)

### Load data
rtdata <- read.delim("Linck - sample dataset.txt")
rtdata$Subject <- as.factor(rtdata$Subject)

### Manually create factor coding for 'language' and 'condition' variables. See text for details on model interpretation with contrast vs. dummy coding:
  ### Dummy coding for L1, L2, and L3 language variables, to allow proper uncorrelated random effects specification:
  rtdata$L1 <- 0
  rtdata$L1[rtdata$language == "L1"] <- 1
  rtdata$L2 <- 0
  rtdata$L2[rtdata$language == "L2"] <- 1
  rtdata$L3 <- 0
  rtdata$L3[rtdata$language == "L3"] <- 1
  
  ### Contrast (sum) coding for condition variable. 
  rtdata$condition <- -0.5
  rtdata$condition[rtdata$trial_type == "switch"] <- 0.5

##### *** Analysis: switch costs by language (ignoring prior trial's language - for simplicity of exposition):
  ### Estimating individual differences in outcomes only: (3) language x (2) switch condition ("trial_type")
    ### Random intercepts only, to see how things look
    m.3x2_Rints <- lmer(formula = rt ~ (L2 + L3)*condition + (1|Subject) + (1|stimulus), data = rtdata)
  
  ### "Going maximal": allowing slopes to fully correlate (i.e., saturated, fully-specified random effects structure)
    m.3x2.Rfull <- lmer(formula = rt ~ (L2 + L3)*condition + ((L2 + L3)*condition|Subject) + ((L2 + L3)*condition|stimulus), data = rtdata)
    ### ---> note: this model took approximately 10 minutes to fit in 64-bit RStudio running on a laptop running Window 8.1 with 8 GB RAM, 2.4 GHz processor and a solid-state hard drive.
    ###      R returned a warning re: "max fun < 10 * length(par)^2 not recommended"
    ### Print out summary of model
    summary(m.3x2.Rfull)

  ### ** Save models to disk **
  save(m.3x2_Rints, m.3x2.Rfull, file = "Non-IndDiffs models, R-ints and full Ranef structure.RData")
  
  
  
### Now adding cognitive individual differences predictors to account for variability in outcomes

  ### Allowing slopes to fully correlate (i.e., saturated, fully-specified random effects structure), including 
  ###   the full factorial combination of the experimental factors and the subjects' individual difference variable 
  ###   z.ic varying by item only (not by subject, because it is a between-subject variable):

  #   m.3x2.IC_Rmaximal <- lmer(formula = rt ~ (L2 + L3)*condition*z.ic + ((L2 + L3)*condition|Subject) + ((L2 + L3)*condition*z.ic|stimulus), data = rtdata)
  #               ## Note: Failed to converge after 93 minutes. Error messages:
  #               1: In commonArgs(par, fn, control, environment()) :
  #                 maxfun < 10 * length(par)^2 is not recommended.
  #               2: In optwrap(optimizer, devfun, getStart(start, rho$lower, rho$pp),  :
  #                 convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded
  #               3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  #                 Model failed to converge with max|grad| = 0.0314238 (tol = 0.002, component 73)
  #               4: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  #                 Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
  

  ### Including correlated random effect for the main effect of z.ic (but no interactions):
    #   m.3x2.IC_Rmain <- lmer(formula = rt ~ (L2 + L3)*condition*z.ic + ((L2 + L3)*condition|Subject) + ((L2 + L3)*condition + z.ic|stimulus), data = rtdata)
        ### ---> Note: failed to converge. Further simplified model by eliminating z.ic's random effect correlations with the remaining random effects.

  ### Random effect of the main effect of z.ic, uncorrelated with any of the other by-item random effects:
  m.3x2.IC_Rmain.uncorr <- lmer(formula = rt ~ (L2 + L3)*condition*z.ic + ((L2 + L3)*condition|Subject) + ((L2 + L3)*condition|stimulus) + (0 + z.ic|stimulus), data = rtdata)
  # was m.3x2.IC_Runcorr
      ### ---> Note: It fit in 11 minutes, with this warning message:
          # Warning message:
          #   In commonArgs(par, fn, control, environment()) :
          #   maxfun < 10 * length(par)^2 is not recommended.


    ### Refitting L2-baseline model for the above model:
      m.3x2.IC_Rmain.uncorr_L2 <- lmer(formula = rt ~ (L1 + L3)*condition*z.ic + ((L1 + L3)*condition|Subject) + ((L1 + L3)*condition|stimulus) + (0 + z.ic|stimulus), data = rtdata)
      # was m.3x2.IC_Runcorr_L2
    ### Refitting L3-baseline model for the above model:
      m.3x2.IC_Rmain.uncorr_L3 <- lmer(formula = rt ~ (L1 + L2)*condition*z.ic + ((L1 + L2)*condition|Subject) + ((L1 + L2)*condition|stimulus) + (0 + z.ic|stimulus), data = rtdata)
      # was m.3x2.IC_Runcorr_L3


      ### Saving all three models w/ uncorrelated z.ic:
        save(m.3x2.IC_Rmain.uncorr, m.3x2.IC_Rmain.uncorr_L2, m.3x2.IC_Rmain.uncorr_L3, file = "Ind diffs models to report - max ranefs w convergence, uncorred z.ic by item.RData")


### ----- Printout of each model's summary ----- ###
  ### No ind diffs yet - random intercepts only:
    summary(m.3x2_Rints)
  ### No ind diffs yet - full random effects structure:
    summary(m.3x2.Rfull)
  ### Adding ind diffs - best-fitting model with full random effects structure minus z.ic's random effects correlations:
    summary(m.3x2.IC_Rmain.uncorr)
  ### Refitted model w/ L2 baseline
    summary(m.3x2.IC_Rmain.uncorr_L2)
  ### Refitted model w/ L3 baseline
    summary(m.3x2.IC_Rmain.uncorr_L3)


##### -----------------------------------

#### Function to identify which R version you are running, as well as 
####  the specific version of the packages loaded in your current workspace:
  sessionInfo()
    ### For published version of the chapter, models were fit using R 3.1.2 and lme4 v1.1-7