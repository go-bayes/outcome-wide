# pets loss

# pets loss

# perfectionism

# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")


# for saving models
push_mods <-
  fs::path_expand("/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets")
push_figs <-
  fs::path_expand("/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets")



###############  import  'df"  ###############


df <- readRDS(here::here( "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets", "outcomewide-pets-ml_one_base"))
ml <- readRDS(here::here( "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets", "outcomewide-pets-mf_one_base"))



############### SET YOUR EXPOSURE VARIABLE, ###############  ###############  ###############

## HERE WE USE THE EXAMPLE OF HOURS WORK / 10
###############   IMPORTANT SET YOUR EXPOSURE VARIABLE

# say we go from 3 to 6
# meand is 5.7 ... so this is about 2 x sd below mean to just over the mean.

X = "pets_lead1"


############### NEXT SET UP VARIABLES FOR MODELS AND GRAPHS

# You may set your label for your graphs  HERE WE STICK TO THE EXAMPLE OF WORK

xlab = "Do you have any pets?"  ## Weekly hours devided by 10


# SET THE RANGE OF WORK HOURS FROM ZERO TO 80
min = 0
max =  1

# set full range of X
x =  min:max
x


# range for some graphs
minmax <- paste(c(x), sep = ",")

ml$pets_lead1
# baseline condition

r = 1

# focal contrast for X
f = 0

# REQUIRED for certain model model functions
c = x

# contrast for graphs -- absolute distance from baseline
p = c(r, f) #


# Needed for E-VALUES -- how much do we move on the X scale to obtain our effect?

delta = abs(r - f)

ylim = c(-.5, .5)  # SET AS YOU LIKE -- here, how much movement across a standard deviation unit of the outcome
ylim_contrast = c(0, 3)  # SET AS YOU LIKE (FOR CONTRASTS )

# mice imputed data
## THIS IS KEY, NAME THE DATA I GAVE YOU "DF"

# n imputations
m = 10

# standard deviation of the outcome (for evalues)
# We have stanadardised the (non-binary) outcomes for comparable effect sizes.
sd = 1


family = "gaussian"

##### BASELINE VARIABLES


cvars = c(
 # "pets",
  "AGREEABLENESS_z",
  "CONSCIENTIOUSNESS_z",
  "EXTRAVERSION_z",
  "HONESTY_HUMILITY_z",
  "NEUROTICISM_z",
  "OPENNESS_z",
  "kessler_hopeless",
  #   # …  you feel hopeless?
  "kessler_depressed",
  #   #…  you feel so depressed that nothing could cheer you up?
  "kessler_restless",
  #   #…  you feel restless or fidgety?
  "kessler_effort",
  #   #…  you feel that everything was an effort?
  "kessler_worthless",
  #   #…  you feel worthless?
  "kessler_nervous",
  "Age_z",
  "Alcohol.Frequency_z",
  "Alcohol.Intensity_log_z",
  "BornNZ_z",
  "Believe.God_z",
  "Believe.Spirit_z",
  "BELONG_z",
  "CharityDonate_log_z",
  "ChildrenNum_z",
  "Church_z",
  "community_z",
  "Edu_z",
  "Employed_z",
  "EthCat",
  "Gender3",
  "Hours.Exercise_log_z",
  "Hours.Work_z",
  "HLTH.Disability_z",
  "HLTH.Fatigue_z",
  "HLTH.SleepHours_z",
  "income_log_z",
  #  "KESSLER6sum_z",
  "LIFESAT_z",
  "NZDep2013_z",
  "NWI_z",
  "NZSEI13_z",
  "Parent_z",
  "Partner_z",
  "Pol.Orient_z",
  "Relid_z",
  "SexualOrientation_z",
  "SFHEALTH_z",
  "Smoker_z",
  "Standard.Living_z",
  "SUPPORT_z",
  "Rural_GCH2018",
  "Volunteers_z",
  "Your.Health_z",
  "Your.Future.Security_z",
  "Your.Personal.Relationships_z"
)

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


# # exercise ---------------------------------------------------------------
# # Hours spent … exercising/physical activity
# Y = "Hours.Exercise_lead2_log_z"
# main = "Log Hours Exercise"
# ylab = "Log Hours Exercise (SD)"
# sub = "Hours spent … exercising/physical activity"
#
# # regression
# out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
#
# ## g-computation
# out_ct <-
#   pool_stglm_contrast(
#     out_m,
#     df = df,
#     m = 10,
#     X = X,
#     x = x,
#     r = r
#   )
#
# #contrast
# out_ct %>%
#   slice(f + 1 - min) |>
#   kbl(digits = 3, "markdown")
# max
# excercise_t <- out_ct %>%
#   #slice(1:max) |>
#   tibble() |>
#   rename(
#     Contrast = row,
#     Estimate = est,
#     Std_error = se,
#     CI_hi = ui,
#     CI_lo = li
#   ) |>
#   kbl(caption = main,
#       digits = 3,
#       "html") |>
#   kable_styling() %>%
#   row_spec(c(f + 1 - min),
#            bold = T,
#            color = "white",
#            background = "dodgerblue") |>
#   kable_minimal(full_width = F)
#
# # show table
# excercise_t
# # graph
# exercise_p <-
#   ggplot_stglm(
#     out_ct,
#     ylim = ylim,
#     main,
#     xlab,
#     ylab,
#     min = min,
#     p = p,
#     sub = sub
#   )
# exercise_p
#
# # coef + estimate
# exercise_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
# exercise_c




mice_iptw_lin = function(X,Y,df, family = "gaussian") {
  # requires that a MATCH THEM dataset is converted to a mice object
  # weights must be called "weights)
  require("mice")
  out_m <- with(df, glm(
    as.formula(paste(Y, "~ (", X , ")")),    weights = weights,
    family = family
  ))
  return(out_m)
}



# sf-health ---------------------------------------------------------------
# Short-Form Subjective Health Scale (General Health Perception Subscale)
# In general, would you say your health is...
# I seem to get sick a little easier than other people.
# I expect my health to get worse.

sub = "In general, would you say your health is...\nI seem to get sick a little easier than other people.\nI expect my health to get worse."
Y = "SFHEALTH_lead2_z"
main = "SF Health"
ylab = "SF Health (SD)"

# regression
cvars

out_m <- mice_iptw_lin(df = df, X = X, Y = Y, cvars = cvars, family = "gaussian")

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

sfhealth_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
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
# show table
sfhealth_t
# graph
sfhealth_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
sfhealth_p

# coef + estimate
sfhealth_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
sfhealth_c


### EMBODIED WELL BEING ----------------------------------------------------


# kessler 6 ---------------------------------------------------------------

# Kessler-6
# During the last 30 days, how often did.... you feel hopeless?
#   During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
#   During the last 30 days, how often did.... you feel restless or fidgety?
#   During the last 30 days, how often did.... you feel that everything was an effort?
#   During the last 30 days, how often did.... you feel worthless?
#   During the last 30 days, how often did.... you feel nervous?

Y = "KESSLER6_lead2_z"
main = "Kessler 6 Distress"
ylab = "Kessler 6 Distress (SD)"
sub = "During the last 30 days, how often did....\nyou feel hopeless?\nyou feel so depressed that nothing could cheer you up?\nyou feel restless or fidgety?\nyou feel that everything was an effort?\nyou feel worthless?\nyou feel nervous?"

# regression
out_m <- mice_generalised_lin(df = df, X = X, Y = Y, cvars = cvars, family = "gaussian")

mf$KESSLER6sum_lead2_z

summary(pool(out_m))
## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

# coef + estimate
distress_c <- vanderweelevalue_ols(out_ct, f - min, delta, sd)
distress_c

distress_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
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
# show table
distress_t
# graph
distress_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
distress_p


# fatigue -----------------------------------------------------------------
#During the last 30 days, how often did.... you feel exhausted?

Y = "HLTH.Fatigue_lead2_z"
main = "Fatigue"
ylab = "Fatigue (SD)"
sub = "During the last 30 days, how often did....\nyou feel exhausted?"


# regression
out_m <- mice_generalised_lin(df = df, X = X, Y = Y, cvars = cvars, family = "gaussian")

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

# coef + estimate
fatigue_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
fatigue_c


fatigue_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
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
# show table
fatigue_t
# graph
fatigue_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
fatigue_p


# life sat ----------------------------------------------------------------
# Satisfaction with life
# I am satisfied with my life.
# In most ways my life is close to ideal.

Y = "LIFESAT_lead2_z"
main = "Life Satisfaction"
ylab = "Life Satisfaction (SD)"
sub = "I am satisfied with my life.\nIn most ways my life is close to ideal."

# regression
out_m <- mice_generalised_lin(df = df, X = X, Y = Y, cvars = cvars, family = "gaussian")

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

lifesat_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
lifesat_c


lifesat_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
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
# show table
lifesat_t
# graph
lifesat_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
lifesat_p




#PWI ---------------------------------------------------------
#Your health.
#Your standard of living.
#Your future security.
#Your personal relationships.


Y = "PWI_lead2_z"
main = "Person Wellbeing Index"
ylab = "PWI (SD)"
sub = "Satisfied with...\nYour health.\nYour standard of living.\nYour future security.\nYour personal relationships."

# regression
out_m <- mice_generalised_lin(df = df, X = X, Y = Y, cvars = cvars, family = "gaussian")

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

pwi_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
pwi_c

pwi_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
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
# show table
pwi_t
# graph
pwi_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
pwi_p



# SOCIAL CONNECTION AND BELONGING -----------------------------------------

###
# belonging ---------------------------------------------------------------
# Felt belongingness
# Know that people in my life accept and value me.
# Feel like an outsider.
# Know that people around me share my attitudes and beliefs.


Y = "BELONG_lead2_z"
main = "Social Belonging"
ylab = "Social Belonging (SD)"
sub = " Know that people in my life accept and value me.\nFeel like an outsider.\nKnow that people around me share my attitudes and beliefs."


# regression
out_m <- mice_generalised_lin(df = df, X = X, Y = Y, cvars = cvars, family = "gaussian")

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

belong_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
belong_c


belong_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
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
# show table
belong_t
# graph
belong_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
belong_p



# community ----------------------------------------------------------
#I feel a sense of community with others in my local neighbourhood.
Y = "community_lead2_z"
main = "Community"
ylab = "Community (SD)"
sub = "I feel a sense of community with others\nin my local neighbourhood."

# regression
out_m <- mice_generalised_lin(df = df, X = X, Y = Y, cvars = cvars, family = "gaussian")

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

community_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
community_c

community_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
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
# show table
community_t
# graph
community_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
community_p



# soc support -------------------------------------------------------------
# Perceived social support
# There are people I can depend on to help me if I really need it.
# There is no one I can turn to for guidance in times of stress.
# I know there are people I can turn to when I need help.
## fit
Y = "SUPPORT_lead2_z"
main = "Social Support"
ylab = "Social Support (SD)"
sub = 'There are people I can depend on to help me if I really need it.\nThere is no one I can turn to for guidance in times of stress.\nI know there are people I can turn to when I need help.'


# regression
out_m <- mice_generalised_lin(df = df, X = X, Y = Y, cvars = cvars, family = "gaussian")

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")


support_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
support_c


support_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
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
# show table
support_t
# graph
support_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
support_p




#### CHARITABLE BEHAVIOURS  --------------------------------------------------
# honesty humility --------------------------------------------------------
#
# # Mini-IPIP6 Honesty-Humility (item overlap with Psychological Entitlement)
# # Would like to be seen driving around in a very expensive car.
# # Would get a lot of pleasure from owning expensive luxury goods.
# # Feel entitled to more of everything.
# # Deserve more things in life.
# Y = "HONESTY_HUMILITY_lead2_z"
# main = "Honesty Humility"
# ylab = "Honesty Humility (SD)"
# sub = "Would like to be seen driving around in a very expensive car.\nWould get a lot of pleasure from owning expensive luxury goods.\nFeel entitled to more of everything.\nDeserve more things in life."
#
#
# # regression
# out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
#
# ## g-computation
# out_ct <-
#   pool_stglm_contrast(
#     out_m,
#     df = df,
#     m = 10,
#     X = X,
#      x = x,
#     r = r
#   )
# out_ct %>%
#    slice( f + 1 - min) |>
#   kbl(digits = 3, "markdown")
#
#
# humility_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
# humility_c
#
# humility_t <- out_ct %>%
#    #slice(1:max) |>
#   tibble() |>
#   rename(
#     Contrast = row,
#     Estimate = est,
#     Std_error = se,
#     CI_hi = ui,
#     CI_lo = li
#   ) |>
#   kbl(caption = main,
#       digits = 3,
#       "html") |>
#   kable_styling() %>%
#   row_spec(c(f + 1 - min),
#            bold = T,
#            color = "white",
#            background = "dodgerblue") |>
#   kable_minimal(full_width = F)
# # show table
# humility_t
# # graph
# humility_p <-
#   ggplot_stglm(
#     out_ct,
#     ylim = ylim,
#     main,
#     xlab,
#     ylab,
#     min = min,
#     p = p,
#     sub = sub
#   )
# humility_p
#

`# stand living ------------------------------------------------------------
# Part of pwi
# Personal Wellbeing Index
# Your health.
# Your standard of living.
# Your future security.
# Your personal relationships.

Y = "Standard.Living_lead2_z"
main = "Standard Living"
ylab = "Standard Living (SD)"
sub  = "Satisfied with ...Your standard of living."


# regression
out_m <- mice_generalised_lin(df = df, X = X, Y = Y, cvars = cvars, family = "gaussian")

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

standardliving_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
standardliving_c


standardliving_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
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
# show table
standardliving_t
# graph
standardliving_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
standardliving_p







# PWI -- YYour.Future.Security_lead2_z -----------------------------------------------
Y = "Your.Future.Security_lead2_z"
main = "Future Security"
ylab = "FutureSecurity (SD)"
sub  = "Satisfied with ...Your future security."
# regression
out_m <- mice_generalised_lin(df = df, X = X, Y = Y, cvars = cvars, family = "gaussian")

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

futuresecurity_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
futuresecurity_c

# graph
futuresecurity_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
futuresecurity_p



# PWI -- Your personal relationships --------------------------------------

Y = "Your.Personal.Relationships_lead2_z"
main = "Personal Relationships"
ylab = "Personal Relationships (SD)"
sub  = "Satisfied with ...Your personal relationships."
# regression
out_m <- mice_generalised_lin(df = df, X = X, Y = Y, cvars = cvars, family = "gaussian")

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

yourpersonalrelationships_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
yourpersonalrelationships_c

# graph
yourpersonalrelationships_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
yourpersonalrelationships_p



# Your Health -------------------------------------------------------------
Y = "Your.Health_lead2_z"
main = "Your Health"
ylab = "Your Health (SD)"
sub  = "Satisfied with ...your health."
# regression
out_m <- mice_generalised_lin(df = df, X = X, Y = Y, cvars = cvars, family = "gaussian")

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

yourhealth_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
yourhealth_c

# graph
yourhealth_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
yourhealth_p


mf$KESSLER6_lead2

## TABLE HEALTH

# TABLE  HEALTH  -----------------------------------------------
main = "Health outcome estimands / Evalues"
h_tab <- rbind(
  sfhealth_c)

h_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  # kable_styling() %>%
  row_spec(c(0),  # Bold out the lines where EVALUES do not cross zero or for ratios, 1
           bold = T,
           # color = "black",
           background = "bold")|>
  kable_minimal(full_width = F)


# TABLE EMBODIED ----------------------------------------------------------

main = "Embodied wellbeing estimands / Evalues"
embody_tab <- rbind(
  distress_c,
  fatigue_c
)
#
embody_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(0),  # Bold out the lines where EVALUES do not cross zero or for ratios, 1
           bold = T,
           # color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)



# TABLE REFLECTIVE WELLBEING ----------------------------------------------


main = "Reflective wellbeing estimands / Evalues"
reflect_tab <- rbind(
  lifesat_c,
  pwi_c,
)

reflect_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(2,4,8,10,11),
           bold = T,
           color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)



# TABLE SOCIAL WELLBEING --------------------------------------------------

main = "Social wellbeing estimands / Evalues"
social_tab <- rbind(belong_c,
                    community_c,
                    support_c)

social_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(1),
           bold = T,
           color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)


# pwi ---------------------------------------------------------

main = "PWI subscales / Evalues"
pwi_tab <- rbind(
  yourpersonalrelationships_c,
  yourhealth_c,
  standardliving_c,
  futuresecurity_c
  #,
  #  volunteers_c
)

pwi_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(3),
           bold = T,
           color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)



# GRAPHS EMBODIED --------------------------------------------
bodysat_p
distress_p
fatigue_p
rumination_p
selfcontrol_p
sleep_p
sexualsat_p


#
# # GRAPH PWI SUBSCALE ------------------------------------------------------
# yourpersonalrelationships_p
# yourhealth_p
# standardliving_p
# futuresecurity_p
#
# pwi_plots <-
#   yourpersonalrelationships_p+
#   yourhealth_p+
#   standardliving_p+
#   futuresecurity_p+plot_annotation(title = "Causal effects of income on PWI subcales", #subtitle = "xyz",
#                                    tag_levels = "A") +
#   plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)
#
#
#
# # view
# pwi_plots
#
# ggsave(
#   pwi_plots,
#   path = here::here(here::here("figs", "examples")),
#   width = 15,
#   height = 12,
#   units = "in",
#   filename = "pwi_plots.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1200
# )


# EXAMPLE OF A COMPARISON STUDY -------------------------------------------
#During the last 30 days, how often did.... you feel exhausted?



# forestplots -------------------------------------------------------------


list_outcomes_pets_loss <- c(list(
                                sfhealth_p,
                                fatigue_p,
                                distress_p,
                                lifesat_p,
                                belong_p,
                                support_p,
                                yourpersonalrelationships_p,
                                yourhealth_p,
                                standardliving_p,
                                futuresecurity_p
                                ))

gcomp_forestplot


out_pets_loss <- bind_forestplot(list_outcomes_pets_loss)
out_pets_loss

gcomp_forestplot_pets_loss <- gcomp_forestplot(out_pets_loss, title = "Outcomewide Perfectionism", ylim = c(-.3,.3), xlab = "Incidence Perfectionism (SD)")

gcomp_forestplot_pets_loss


ggsave(
  gcomp_forestplot_pets_loss,
  path = here::here(here::here("figs", "pets")),
  width = 12,
  height = 8,
  units = "in",
  filename = "gcomp_forestplot_pets_loss.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)




## Risk ratio plot
out_perfect_rr <- bind_forestplot(list(smoker_p, volunteers_p))

# save for future use
saveh(out_perfect_rr, "out_volunteers_rr_iptw")

# plot
gcomp_perfect_rr <-
  gcomp_forestplot_rr(out_perfect_rr,title = "Perfectionism RR",
                      ylim = c(.5,1.5))
gcomp_perfect_rr
ggsave(
  gcomp_perfect_rr,
  path = here::here(here::here("figs", "figs_perfect")),  width = 12,
  height = 8,
  units = "in",
  filename = "gcomp_forestplot_rr_perfect.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200)


# STATEMENT OF EVALUE -----------------------------------------------------

# “With an observed risk ratio of RR=0.28, an unmeasured confounder that was associated with both the outcome and the exposure by a risk ratio of 6.6-fold each, above and beyond the measured confounders, could explain away the estimate, but weaker joint confounder associations could not; to move the confidence interval to include the null, an unmeasured confounder that was associated with the outcome and the exposure by a risk ratio of 3.1-fold each could do so, but weaker joint confounder associations could not.”






#COVID19.Timeline
# bels:
#   value                                                                                              label
# 0.0                                                                                   The Before Times
# 1.0                                31.12.2019 -- 27.02.2020 [First cluster of cases in Wuhan reported]
# 1.1                                      28.02.2020 -- 25.02.2020 [First case recorded in New Zealand]
# 1.2                                                           26.03.2020 -- 27.04-2020 [Alert Level 4]
# 1.3                                                           28.04.2020 -- 13.05.2020 [Alert Level 3]
# 1.4                                                          14.05.2020 -- 08.06.2020 [Alert Level 2].
# 1.5                                                           09.06.2020 -- 11.08.2020 [Alert Level 1]
# 2.1 12.08.2020 -- 30.08.2020 [Second Outbreak - Auckland Alert Level 3, Rest of Country Alert Level 2]
# 2.2                 30.08.2020 -- 21.09.2020 [Auckland Alert Level 2.5, Rest of Country Alert Level 2]
# 2.3                    22.09.2020 -- 07.10.2020 [Auckland Alert Level 2, Rest of Country Alert Level 1
# 2.4                                                              08.10.2020 -- onwards [Alert Level 1]

# dat$REGC_2018

# labels:
#   value                     label
# 1          Northland Region
# 2           Auckland Region
# 3            Waikato Region
# 4      Bay of Plenty Region
# 5           Gisborne Region
# 6         Hawkes Bay Region
# 7           Taranaki Region
# 8 Manawatu-Whanganui Region
# 9         Wellington Region
# 12         West Coast Region
# 13         Canterbury Region
# 14              Otago Region
# 15          Southland Region
# 16             Tasman Region
# 17             Nelson Region
# 18        Marlborough Region
# 99       Area Outside Region


#How many times did you pray in the last week?



# honesty humility --------------------------------------------------------

# Mini-IPIP6 Honesty-Humility (item overlap with Psychological Entitlement)
# Would like to be seen driving around in a very expensive car.
# Would get a lot of pleasure from owning expensive luxury goods.
# Feel entitled to more of everything.
# Deserve more things in life.
#
# Y = "HONESTY_HUMILITY_lead2_z"
# main = "Honesty Humility"
# ylab = "Honesty Humility (SD)"
#
#
# # regression
# out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
#
# ## g-computation
# out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X,  x = x,,r= r)
# out_ct %>%
#   slice(f+1) |>
#   kbl(digits = 3, "markdown")
#
# humility_t <- out_ct %>%
#    #slice(1:max) |>
#   tibble() |>
#   rename(Contrast = row,
#          Estimate = est,
#          Std_error = se,
#          CI_hi = ui,
#          CI_lo = li) |>
#   kbl(caption = main,
#       digits = 3,
#       "html") |>
#   kable_styling() %>%
#   row_spec(c(f+1), bold = T, color = "white", background = "dodgerblue") |>
#   kable_minimal(full_width = F)
# # show table
# humility_t
# # graph
# humility_p<- ggplot_stglm(out_ct, ylim = ylim, main, xlab, ylab, min = min, p=p, r= 1)
# humility_p
#
# round( EValue::evalues.OLS( , se = , sd = sd, delta = delta, true = 0), 3)
# round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)





##
# Confounding control variables  ---------------------------------------------------------
# These variables can be modified depending on your model and assumptions.
#  Here, we use vanderweele's "disjunctive cause criterion"

# FROM Outcomewide longitudinal designs: https://doi.org/10.1214/19-STS728
#" A modified disjunctive cause criterion that might thus be more useful in practice could articulated as follows (VanderWeele, 2019): control for each covari- ate that is a cause of the exposure, or of the outcome, or of both; exclude from this set any variable known to be an instrumental variable; and include as a covariate any proxy for an unmeasured variable that is a common cause of both the exposure and the outcome." p.443

# TYLERS LIST,  https://doi.org/10.1214/19-STS728 p.442
# *** Demographic
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

# NOTE: WE USE MORE VARIABLES
