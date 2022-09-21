#environ concern

#earthquake anxiety
#Edu cause God etc.
#Cause disbelief.

# church-use R
# set digits = 3
options(scipen = 999)

#libraries and functions
# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

conflict_prefer("pool", "mice")
conflict_prefer("cbind", "base")
# for saving models
push_mods <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/mods")
push_figs <-
  fs::path_expand("~/Users/joseph/The\ Virtues\ Project\ Dropbox/outcomewide/figs")


# read data
pull_path <-
  fs::path_expand(
    "~/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/ldf.5"
  )



###### READ THIS DATA IN   #########
ml <- readh("l_belief_cause_all")
mf <- readh("f_belief_cause_all")

###############  RENAME YOUR IMPUTED DATASET  'df"  ###############  ###############  ###############
###############   IMPORANT DO THIS   ###############  ###############  ###############  ###############

df <- ml

############### SET YOUR EXPOSURE VARIABLE, ###############  ###############  ###############

max(mf$Env.ClimateChgConcern_lead1_z)
## HERE WE USE THE EXAMPLE OF HOURS WORK / 10
###############   IMPORTANT SET YOUR EXPOSURE VARIABLE
X = "Env.ClimateChgConcern_lead1_z"


############### NEXT SET UP VARIABLES FOR MODELS AND GRAPHS
#min(mf$Your.Future.Security_lead1_z)
# You may set your label for your graphs  HERE WE STICK TO THE EXAMPLE OF WORK

xlab = "Env.ClimateChgConcern_lead1_z"  ## Monthly Church

# SET THE RANGE OF religious service FROM ZERO TO 80
min = -2
max = 1


# set full range of X
x =  min:max


# range for some graphs
minmax <- paste(c(x), sep = ",")


# baseline condition here is 20 hours of work.  We could make it different
r = -2

# focal contrast for X  Someone who goes from 20 to 60 hours of work.
f = 1

# REQUIRED for certain model model functions
c = x

# contrast for graphs -- absolute distance from baseline
p = c(r, f) #


# Needed for E-VALUES -- how much do we move on the X scale to obtain our effect?
#delta = 4 #
delta = abs(r - f)

ylim_contrast = c(.8,1.2)# SET AS YOU LIKE (FOR CONTRASTS )

# mice imputed data
## THIS IS KEY, NAME THE DATA I GAVE YOU "DF"

# n imputations
m = 10

# standard deviation of the outcome (for evalues)
# We have stanadardised the (non-binary) outcomes for comparable effect sizes.
sd = 1



##### BASELINE VARIABLES
cvars = c(
  "AGREEABLENESS_z",
  "CONSCIENTIOUSNESS_z",
  "EXTRAVERSION_z",
  "HONESTY_HUMILITY_z",
  "NEUROTICISM_z",
  "OPENNESS_z",
  "Age_z",
  "Alcohol.Frequency_z",
  "Alcohol.Intensity_log_z", # log?
  "Bodysat_z",
  "BornNZ_z",
  "Believe.God_z",
  "Believe.Spirit_z",
  "BELONG_z",
  "CharityDonate_log_z",
  "ChildrenNum_z",
  "Church_z",
  "community",
  "Earthquake.Anxiety_z",
  "Edu_z",
  "Env.ClimateChgConcern_z",
  "Employed_z",
  #"Emp.JobSecure_z",
  # "EmotionRegulation1_z",
  # "EmotionRegulation2_z",
  # "EmotionRegulation3_z",
  #"Euro_z",
  "EthCat",
  "Gender3",
  "GRATITUDE_z",
  "HomeOwner_z",
  "Hours.Exercise_log_z",
  "Hours.Work_10_z",
  "HLTH.BMI_z",
  "HLTH.Disability_z",
  "HLTH.Fatigue_z",
  "HLTH.SleepHours_z",
  "ImpermeabilityGroup_z",
  #  "income_log_z",
  "KESSLER6sum_z",
  "LIFEMEANING_z",
  "LIFESAT_z",
  "lost_job_z",
  "NZdep_z",
  "NWI_z",
  "NZSEI13_z",
  "Parent_z",
  "Partner_z",
  "PERFECTIONISM_z",
  "PermeabilityIndividual_z",
  "Pol.Orient_z",
  "POWERDEPENDENCE1_z",
  "POWERDEPENDENCE2_z",
  "Relid_z",
  "Respect.Self_z",
  "Retiredp_z",
  "Rumination_z",
  "SELF.CONTROL_z",
  "SELF.ESTEEM_z",
  "SexualOrientation_z",
  "SexualSatisfaction_z",
  "SFHEALTH_z",
  "Smoker_z",
  "Spiritual.Identification_z",
  "Standard.Living_z",
  "SUPPORT_z",
  "Urban_z",
  "VENGEFUL.RUMIN_z",
  "Volunteers_z",
  "Your.Health_z",
  "Your.Future.Security_z",
  "Your.Personal.Relationships_z"
)


#
# ### BASELINE for ML models
#
#*** Demographic
# Race
# Age
# Gender
# Marital Status
# *** Economic, Social and Political
# Income
# Education
# Employment
# Social integration Neighborhood
# Religious service attendance
# Political affiliation
### *** Health
# Self-rated health
# Number of health conditions
# Exercise
# Smoking
# Alcohol consumption
# Depression
# Happiness Loneliness
# Parental warmth Purpose/Meaning Big five personality

## STATEMENT OF "VANDERWEEL-E-VALUE FROM TYLER

# “With an observed risk ratio of RR = XX, an unmeasured confounder that was associated with both the outcome and the exposure by a risk ratio of XX -fold each, above and beyond the measured confounders, could explain away the estimate, but weaker joint confounder associations could not; to move the confidence interval to include the null, an unmeasured confounder that was associated with the outcome and the exposure by a risk ratio of XX -fold each could do so, but weaker joint confounder associations could not.”

# EVALUES FOR CONTINOUS VARS - p.448
# For a continuous outcome, with a standardized effect size “d” (obtained by dividing the mean difference on the outcome variable between exposure groups by the pooled standard deviation of the outcome) and a stan- dard error for this effect size sd , an approximate E-value can be obtained (VanderWeele and Ding, 2017) by ap- plying the approximation RR ≈ exp(0.91 × d) and then using the E-value formula above (E-value = RRobs + √RRobs(RRobs − 1)). An approximate confidence inter- val can be found using the approximation
# 􏰛exp{0.91×d −1.78×sd},exp{0.91×d +1.78×sd}􏰜

# We could include statements like this in all empirical papers


# NOTE THAT I HAVE WRITTEN WRAPPER FUNCTIONS TO AUTOMATE REPORTING OF EVALUES, ALSO TO CREATE TABLES -- YOUR WORK IS LIGHT!
# however the code is:


# round(EValue::evalues.OLS(
#   ,
#   se = ,
#   sd = sd,
#   delta = delta,
#   true = 0
# ), 3)
# round(EValue::evalues.RR(, lo =  , hi = , true = 1), 4)
#

################# BELOW THE MANY OUTCOMES!  ########################################

# KESSLER6sum,
# Your.Future.Security,
# Your.Health,
# Earthquake.Anxiety,
# Env.ClimateChgConcern,
# Believe.Spirit,
# Believe.God,
# Your.Personal.Relationships, # Neg control
# Standard.Living, # neg control
#
#
vanderweelevalue_rr_lo = function(out, f) {
  require("EValue")
  coef <- round(out, 3) %>%
    slice(r + 1) |>
    select(-row)
  evalout <-
    as.data.frame(round(EValue::evalues.RR(
      coef[1, 1] ,
      lo =  coef[1, 4],
      hi = coef[1, 3],
      true = 1
    ), 3))
  evalout2 <- subset(evalout[2,])
  evalout3 <- evalout2 |>
    select_if(~ !any(is.na(.)))
  colnames(evalout3) <- c("E-value", "threshold")
  tab <- cbind.data.frame(coef, evalout3)
  rownames(tab) <- c(main)
  return(tab)
}



# BELIEVE GOD --------------------------------------------------------------
#Hours spent in activities
#Hours spent … voluntary/charitable work
Y = "Believe.God_lead2"
main = "Believe God (RR)"
ylab = "Believe God (Risk Ratio)"
family = "poisson" # binary outcome not rare
sub = "Do you believe in a God?"
# clean oven
rm(out_m)
rm(out_ct)
# fit regression model
out_m <- mice_generalised(
  df = df,
  X = X,
  Y = Y,
  cvars = cvars,
  family = family
)
# g-computation - contrasts
out_ct <-
  pool_stglm_contrast_ratio(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
#table
out_ct
# coef + estimate

# Create risk ratio table
god_c <- vanderweelevalue_rr(out_ct, f)
god_c


god_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
god_t
god_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim_contrast,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  ) +  expand_limits(x = 0, y = 0)
god_p




# spirit ------------------------------------------------------------------
Y = "Believe.Spirit_lead2"
main = "Believe Spirit (RR)"
ylab = "Believe Spirit (Risk Ratio)"
family = "poisson" # binary outcome not rare
sub = "Do you believe in some form of spirit or lifeforce?"
# clean oven
rm(out_m)
rm(out_ct)
# fit regression model
out_m <- mice_generalised(
  df = df,
  X = X,
  Y = Y,
  cvars = cvars,
  family = family
)
# g-computation - contrasts
out_ct <-
  pool_stglm_contrast_ratio(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
#table
out_ct
# coef + estimate

# Create risk ratio table
spirit_c <- vanderweelevalue_rr(out_ct, f)
spirit_c


spirit_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
spirit_t
spirit_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim_contrast,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  ) +  expand_limits(x = 0, y = 0)
spirit_p




# TABLE ECONOMIC WELLBEING and Charity ------------------------------------------------

main = "Distress Effects Beliefs/ Evalues"
belief_tab_security <- rbind(#  income_c,
  god_c,
  spirit_c)

belief_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  # kable_styling() %>%
  row_spec(c(1, 5),
           bold = T,
           color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)

# # # GRAPHS EMBODIED --------------------------------------------
# belief_plots_k6<-
#   god_p + spirit_p + plot_annotation(title = "Causal effects of education on belief", #subtitle = "xyz",
#                                      tag_levels = "A") +
#   plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)
#
#
# ggsave(
#   belief_plots_k6,
#   path = here::here(here::here("figs", "figs_cause_belief")),
#   width = 16,
#   height = 12,
#   units = "in",
#   filename = "belief_plots_k6.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 600
# )
#



