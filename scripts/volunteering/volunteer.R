# VOLUTNEERING

options(scipen = 999)

#libraries and functions
# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

#conflict_prefer("pool", "mice")
#conflict_prefer("cbind", "base")
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

###############  RENAME YOUR IMPUTED DATASET  'df"  ###############  ###############  ###############
###############   IMPORANT DO THIS   ###############  ###############  ###############  ###############

df <- readh( "ml-volunteers_all" )
mf <- readh( "mf-volunteers_all" )

hist(mf$HoursCharity_log)
summary(mf$HoursCharity_lead1)


###############   IMPORTANT SET YOUR EXPOSURE VARIABLE

X = "Volunteers_lead1_z"


############### NEXT SET UP VARIABLES FOR MODELS AND GRAPHS

# You may set your label for your graphs  HERE WE STICK TO THE EXAMPLE OF WORK

#xlab = "FOREGIVENESS_lead1"  ## Monthly Church
xlab = "Hours spent … voluntary/charitable work"

# SET THE RANGE OF THE SCALE
min = 0
max = 10


# set full range of X
x =  min:max


# range for some graphs
minmax <- paste(c(x), sep = ",")


# baseline condition. We could make it different
r = 0

# focal contrast for X
f = 4

# REQUIRED for certain model model functions
c = x

# contrast for graphs -- absolute distance from baseline
p = c(r, f) #


# Needed for E-VALUES -- how much do we move on the X scale to obtain our effect?
#delta = 4 #
delta = abs(r - f)

ylim = c(-.5, .5)  # SET AS YOU LIKE -- here, how much movement across a standard deviation unit of the outcome
ylim_contrast = c(.5, 2)# SET AS YOU LIKE (FOR CONTRASTS )

# mice imputed data
## THIS IS KEY, NAME THE DATA I GAVE YOU "DF"

# n imputations
m = 10

# standard deviation of the outcome (for evalues)
# We have stanadardised the (non-binary) outcomes for comparable effect sizes.
sd = 1




# Pull mids data
cvars = c(
  "AGREEABLENESS_z",
  "CONSCIENTIOUSNESS_z",
  "EXTRAVERSION_z",
  "HONESTY_HUMILITY_z",
  "NEUROTICISM_z",
  "OPENNESS_z",
  "Age_z",
  "Alcohol.Frequency_z",
  "Alcohol.Intensity_log_z",
  "Bodysat_z",
  "BornNZ_z",
  "Believe.God_z",
  "Believe.Spirit_z",
  "BELONG_z",
  "CharityDonate_log_z",
  "ChildrenNum_z",
  "Church_z",
  "community",
  "Edu_z",
  "Employed_z",
  # "Emp.JobSecure_z",
  "EmotionRegulation1_z",
  "EmotionRegulation2_z",
  "EmotionRegulation3_z",
  #"Euro_z",
  "EthCat",
  "FOREGIVENESS_z",
  "Gender3",
  "GRATITUDE_z",
  "HomeOwner_z",
  "HoursCharity_log",
  "Hours.Exercise_log_z",
 # "Hours.Work_z",
  "HLTH.BMI_z",
  "HLTH.Disability_z",
  "HLTH.Fatigue_z",
  "HLTH.SleepHours_z",
  "ImpermeabilityGroup_z",
  # "income_log_z",
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
  "POWERDEPENDENCE_z",
  "Relid_z",
  "Retiredp_z",
  "Respect.Self_z",
  "Rumination_z",
  "SELF.CONTROL_z",
  "SELF.ESTEEM_z",
  # "semiretired",
  "SexualOrientation_z",
  "SexualSatisfaction_z",
  "SFHEALTH_z",
  "Smoker_z",
  "Spiritual.Identification_z",
  "Standard.Living_z",
  "SUPPORT_z",
  "Urban_z",
  # "VENGEFUL.RUMIN_z",
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


# HEALTH  INDICATORS ------------------------------------------------------------------
# alcohol freq ------------------------------------------------------------
#How often do you have a drink containing alcohol?
Y = "Alcohol.Frequency_lead2ord_z"
main = "Alcohol Frequency"
ylab = "Alcohol Frequency (SD)"
sub = "How often do you have a drink containing alcohol?"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
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
out_ct

# coef + estimate for the contrast of interest # We  will combine the coeffients
#  into a large table, later.
alcoholfreq_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
alcoholfreq_c


## table for all contrasts (exploratory )
# alcoholfreq_t <- out_ct %>%
#   # #slice(1:max) |>
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
# alcoholfreq_t
# # graph
alcoholfreq_p <-
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

alcoholfreq_p



# Alcohol.Intensity ----------------------------------------------------------
#How many drinks containing alcohol do you have on a typical day when drinking?

Y = "Alcohol.Intensity_log_lead2_z"
main = "Alcohol Intensity"
ylab = "Alcohol Intensity (SD)"
sub = "How many drinks containing alcohol do you have on a typical day when drinking?"

# mice_gaussian = function(df, X, Y, cvars) {
#   require("splines")
#   require("mice")
#   out <- with(df, glm(as.formula(paste(
#     paste(Y, "~ bs(", X , ")+"),
#     paste(cvars,
#           collapse = "+")
#   ))))
#   out
# }
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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

# coef + estimate
alcoholintensity_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
alcoholintensity_c

alcoholintensity_p <-
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
alcoholintensity_p



# bmi ---------------------------------------------------------------------
# What is your height? (metres)
# What is your weight? (kg)
# Kg/(m*m)

Y = "HLTH.BMI_lead2_z"
main = "BMI"
ylab = "BMI (SD)"
sub = " What is your height? (metres)\nWhat is your weight? (kg)\nKg *1/(m*m)"


# run model
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
# summary(pool(out_m))
## contrasts
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
# g-computation - contrasts
#table
bmi_t <- out_ct %>%
  slice(1:8) |>
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
bmi_t
bmi_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  ) #+ expand_limits(x = 0, y = 0)
bmi_p

# coef + estimate
bmi_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
bmi_c


# exercise ---------------------------------------------------------------
# Hours spent … exercising/physical activity
Y = "Hours.Exercise_lead2_log_z"
main = "Log Hours Exercise"
ylab = "Log Hours Exercise (SD)"
sub = "Hours spent … exercising/physical activity"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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

#contrast
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")
max
excercise_t <- out_ct %>%
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
excercise_t
# graph
exercise_p <-
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
exercise_p

# coef + estimate
exercise_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
exercise_c


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
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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


# HLTH.Sleep --------------------------------------------------------------
#During the past month, on average, how many hours of actual sleep did you get per night?

Y = "HLTH.SleepHours_lead2_z"
main = "Hours Sleep"
ylab = "Hours Sleep (SD)"
sub = "During the past month, on average, how many hours\nof actual sleep did you get per night?"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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

sleep_t <- out_ct %>%
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
sleep_t
# graph
sleep_p <-
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
sleep_p

# coef + estimate
sleep_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
sleep_c


# smoker ------------------------------------------------------------------
#Do you currently smoke?

Y = "Smoker_lead2"
family = "binomial" # could be binomial if binary utcome is rare
main = "Smoking (RR)"
ylab = "Smoking (Risk Ratio)"
sub = "Do you currently smoke?"
# clean oven
rm(out_m)
rm(out_ct)
# bake
out_m <- mice_generalised(df = df,
                          X = X,
                          Y = Y,
                          cvars = cvars,
                          family = family)
out_m
## contrasts
out_ct <-
  pool_stglm_contrast_ratio(
    out_m,
    df = df,
    m = m,
    X = X,
    x = x,
    r = r
  )
# g-computation - contrasts
#table
smoker_t <- out_ct %>%
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
smoker_t
smoker_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim_contrast,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  ) + expand_limits(x = 0, y = 0)
smoker_p


# coef + estimate
smoker_c <- vanderweelevalue_rr(out_ct, f)
smoker_c


### EMBODIED WELL BEING ----------------------------------------------------


# body satisfaction -------------------------------------------------------
# Am satisfied with the appearance, size and shape of my body.
Y = "Bodysat_lead2_z"
main = "Body Satisfaction"
ylab = "Body Satisfaction (SD)"
sub = "Am satisfied with the appearance,\nsize and shape of my body."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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
bodysat_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
bodysat_c


bodysat_t <- out_ct %>%
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
bodysat_t
# graph
bodysat_p <-
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
bodysat_p


# kessler 6 ---------------------------------------------------------------

# Kessler-6
# During the last 30 days, how often did.... you feel hopeless?
#   During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
#   During the last 30 days, how often did.... you feel restless or fidgety?
#   During the last 30 days, how often did.... you feel that everything was an effort?
#   During the last 30 days, how often did.... you feel worthless?
#   During the last 30 days, how often did.... you feel nervous?

Y = "KESSLER6sum_lead2_z"
main = "Kessler 6 Distress"
ylab = "Kessler 6 Distress (SD)"
sub = "During the last 30 days, how often did....\nyou feel hopeless?\nyou feel so depressed that nothing could cheer you up?\nyou feel restless or fidgety?\nyou feel that everything was an effort?\nyou feel worthless?\nyou feel nervous?"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
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
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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


# rumination --------------------------------------------------------------
# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?

Y = "Rumination_lead2ord_z"
main = "Rumination"
ylab = "Rumination (SD)"
sub = "During the last 30 days, how often did....\nyou have negative thoughts that repeated over and over?"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
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
rumination_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
rumination_c

rumination_t <- out_ct %>%
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
rumination_t
# graph
rumination_p <-
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
rumination_p


# self control ------------------------------------------------------------
#In general, I have a lot of self-control.
#I wish I had more self-discipline.
Y = "SELF.CONTROL_lead2_z"
main = "Self Control"
ylab = "Self Control (SD)"
sub = "In general, I have a lot of self-control.\nI wish I had more self-discipline."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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

selfcontrol_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
selfcontrol_c

selfcontrol_t <- out_ct %>%
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
selfcontrol_t
# graph
selfcontrol_p <-
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
selfcontrol_p


# sex satisfaction --------------------------------------------------------
# How satisfied are you with your sex life?
Y = "SexualSatisfaction_lead2_z"
main = "Sexual Satisfaction"
ylab = "Sexual Satisfaction (SD)"
sub = "How satisfied are you with your sex life?"
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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
sexualsat_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
sexualsat_c

sexualsat_t <- out_ct %>%
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
sexualsat_t
# graph
sexualsat_p <-
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
sexualsat_p


# REFLECTIVE WELL-BEING ---------------------------------------------------



# gratitude ---------------------------------------------------------------
# Gratitude
# I have much in my life to be thankful for.
# When I look at the world, I don’t see much to be grateful for.
# I am grateful to a wide variety of people.

Y = "GRATITUDE_lead2_z"
main = "Gratitude"
ylab = "Gratitude (SD)"
sub = "I have much in my life to be thankful for.\nWhen I look at the world, I don’t see much to be grateful for.\nI am grateful to a wide variety of people."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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
gratitude_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
gratitude_c

gratitude_t <- out_ct %>%
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
gratitude_t
# graph
gratitude_p <-
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
gratitude_p




# perm group ------------------------------------------------------------
#The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.
Y = "ImpermeabilityGroup_lead2_z"
main = "Impermeability Group"
ylab = "Impermeability Group (SD)"
sub = "The current income gap between New Zealand Europeans and\nother ethnic groups would be very hard to change."


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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

groupimperm_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
groupimperm_c

groupimperm_t <- out_ct %>%
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
groupimperm_t
# graph
groupimperm_p <-
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
groupimperm_p


# permeability self ----------------------------------------------------------------
#I believe I am capable, as an individual\nof improving my status in society.

Y = "PermeabilityIndividual_lead2_z"
main = "Permeability of Individual"
ylab = "Permeability of Individual (SD)"
sub = "I believe I am capable, as an individual,\nof improving my status in society."


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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

selfperm_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
selfperm_c


selfperm_t <- out_ct %>%
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
selfperm_t
# graph
selfperm_p <-
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
selfperm_p

# life sat ----------------------------------------------------------------
# Satisfaction with life
# I am satisfied with my life.
# In most ways my life is close to ideal.

Y = "LIFESAT_lead2_z"
main = "Life Satisfaction"
ylab = "Life Satisfaction (SD)"
sub = "I am satisfied with my life.\nIn most ways my life is close to ideal."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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


# life meaning ------------------------------------------------------------
# Meaning in Life
# My life has a clear sense of purpose.
# I have a good sense of what makes my life meaningful.

Y = "LIFEMEANING_lead2_z"
main = "Life Meaning"
ylab = "Life Meaning (SD)"
sub = "My life has a clear sense of purpose.\nI have a good sense of what makes my life meaningful."


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

summary(pool(out_m))
## g-computation
x
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

meaning_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
meaning_c
#
# meaning_t <- out_ct %>%
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
# # show table
# meaning_t
# graph
meaning_p <-
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
meaning_p

# perfectionism  ----------------------------------------------------------
# Perfectionism Discrepancy Subscale
# Doing my best never seems to be enough.
# My performance rarely measures up to my standards.
# I am hardly ever satisfied with my performance.

Y = "PERFECTIONISM_lead2_z"
main = "Perfectionism"
ylab = "Perfectionism (SD)"
sub = "Doing my best never seems to be enough.\nMy performance rarely measures up to my standards.\nI am hardly ever satisfied with my performance"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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

perfect_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
perfect_c
#
# perfect_t <- out_ct %>%
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
# # show table
# perfect_t
# graph
perfect_p <-
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
perfect_p

# PWI ---------------------------------------------------------
#Your health.
#Your standard of living.
#Your future security.
#Your personal relationships.
#
#
# Y = "PWI_lead2_z"
# main = "Person Wellbeing Index"
# ylab = "PWI (SD)"
# sub = "Satisfied with...\nYour health.\nYour standard of living.\nYour future security.\nYour personal relationships."
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
# out_ct %>%
#   slice(f + 1 - min) |>
#   kbl(digits = 3, "markdown")
#
# pwi_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
# pwi_c
#
# pwi_t <- out_ct %>%
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
# # show table
# pwi_t
# # graph
# pwi_p <-
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
# pwi_p



# power dependence 1 ------------------------------------------------------
# I do not have enough power or control over\nimportant parts of my life.
# Other people have too much power or control over\nimportant parts of my life

Y = "POWERDEPENDENCE_lead2_z"
main = "Power Dependence"
ylab = "Power Dependence(SD)"
sub = "I do not have enough power or control\nover important parts of my life.\nOther people have too much power or control\nover important parts of my life."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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

powerdependence_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
powerdependence_c

powerdependence_t <- out_ct %>%
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
powerdependence_t
# graph
powerdependence_p <-
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
powerdependence_p


# # power dependence 2 ------------------------------------------------------
# #Other people have too much power or control over\nimportant parts of my life.
#
# Y = "POWERDEPENDENCE2_lead2_z"
# main = "Power Dependence 2"
# ylab = "Power Dependence 2(SD)"
# sub = "Other people have too much power or control\nover important parts of my life."
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
# out_ct %>%
#   slice(f + 1 - min) |>
#   kbl(digits = 3, "markdown")
#
# powerdependence2_c <-
#   vanderweelevalue_ols(out_ct, f - min, delta, sd)
# powerdependence2_c
#
# powerdependence2_t <- out_ct %>%
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
# # show table
# powerdependence2_t
# # graph
# powerdependence2_p <-
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
# powerdependence2_p

# self esteem -------------------------------------------------------------
# Self-esteem
# On the whole am satisfied with myself.
# Take a positive attitude toward myself.
# Am inclined to feel that I am a failure.


Y = "SELF.ESTEEM_lead2_z"
main = "Self Esteem"
ylab = "Self Esteem (SD)"
sub = "On the whole am satisfied with myself.\nTake a positive attitude toward myself.\nAm inclined to feel that I am a failure."


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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

selfesteem_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
selfesteem_c

selfesteem_t <- out_ct %>%
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
selfesteem_t
# graph
selfesteem_p <-
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
selfesteem_p


#
# # veng rumination ---------------------------------------------------------
# Forgivingness versus Vengeful Rumination
# Sometimes I can't sleep because of thinking about past wrongs I have suffered.
# I can usually forgive and forget when someone does me wrong.
# I find myself regularly thinking about past times that I have been wronged.
#mf$FOREGIVENESS_lead2_z
Y = "FOREGIVENESS_lead2_z"
main = "Forgiveness"
ylab = "Forgiveness (SD)"
sub = "Sometimes I can't sleep because of thinking about\npast wrongs I have suffered.\nI can usually forgive and forget when someone does me wrong.\nI find myself regularly thinking about past times that I have been wronged."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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

veng_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
veng_c

veng_t <- out_ct %>%
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
veng_t
# graph
veng_p <-
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
veng_p




# Work-life balance -------------------------------------------------------
# note-- we have no measure currently at baseline, so less confoundign control
# I have a good balance between work and other important things in my life.

Y = "Emp.WorkLifeBalance_lead2_z"
main = "Work Life Balance"
ylab = "Work Life Balance (SD)"
sub = "I have a good balance between work and\nother important things in my life."


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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


worklife_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
worklife_c

worklife_t <- out_ct %>%
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
worklife_t
# graph
worklife_p <-
  ggplot_stglm(
    out_ct,
    ylim = c(-1,.3),
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
worklife_p


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
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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


# NWI national wellbeing ------------------------------------------------------

# National Wellbeing Index
# The economic situation in New Zealand.
# The social conditions in New Zealand.
# Business in New Zealand.
#Please rate your level of satisfaction with the following aspects of your life and New Zealand.

Y = "NWI_lead2_z"
main = "National Well Being"
ylab = "National Well Being (SD)"
sub = "Satisfied with ...\nThe economic situation in New Zealand.\nThe social conditions in New Zealand.\nBusiness in New Zealand."


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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

nwi_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
nwi_c

nwi_t <- out_ct %>%
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
nwi_t
# graph
nwi_p <-
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
nwi_p

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
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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

# charity donate ----------------------------------------------------------
#How much money have you donated to charity in the last year?

Y = "CharityDonate_log_lead2_z"
main = "Charity Donations (annual)"
ylab = "Charity Donations (annual)"
sub = "How much money have you donated to charity in the last year?"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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

charity_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
charity_c

charity_t <- out_ct %>%
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
charity_t
# graph
charity_p <-
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
charity_p


# volunteers --------------------------------------------------------------
#Hours spent in activities
#Hours spent … voluntary/charitable work

Y = "Volunteers_lead2"
main = "Volunteer (RR)"
ylab = "Volunteer (Risk Ratio)"
family = "binomial" # poisson for binary outcome not rare
sub = "Hours spent … voluntary/charitable work"

# clean oven
rm(out_m)
rm(out_ct)

# fit regression model
out_m <- mice_generalised(df = df,
                          X = X,
                          Y = Y,
                          cvars = cvars,
                          family = family)
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
out_ct<- as.data.frame(out_ct)
volunteers_c <- vanderweelevalue_rr(out_ct, f)
volunteers_c


volunteers_t <- out_ct %>%
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
volunteers_t
volunteers_p <-
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
volunteers_p



# log household income --------------------------------------------------------------
# #Please estimate your total household income (before tax) for the last year.
# Y = "income_log_lead2_z"
# main = "Log Income"
# ylab = "Log Income (SD)"
# sub = "Please estimate your total household income (before tax) for the last year."
# # regression
# out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
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
# out_ct %>%
#   slice(f + 1 - min) |>
#   kbl(digits = 3, "markdown")
#
# income_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
# income_c
#
#
# income_t <- out_ct %>%
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
# # show table
# income_t
# income_c
# # graph
# income_p <-
#   ggplot_stglm(
#     out_ct,
#     ylim =ylim,
#     main,
#     xlab,
#     ylab,
#     min = min,
#     p = p,
#     sub = sub
#   )
# income_p
#


# HOME OWNER --------------------------------------------------------------
#Do you own your own home? (either partly or fully owned)
#
# Y = "HomeOwner_lead2"
# main = "Home Owner (RR)"
# ylab = "Home Owner (Risk Ratio)"
# family = "poisson" # binary outcome not rare
# sub = "Do you own your own home? (either partly or fully owned)"
# # clean oven
# rm(out_m)
# rm(out_ct)
# # fit regression model
# out_m <- mice_generalised(df = df,
#                           X = X,
#                           Y = Y,
#                           cvars = cvars,
#                           family = family)
# # g-computation - contrasts
# out_ct <-
#   pool_stglm_contrast_ratio(
#     out_m,
#     df = df,
#     m = 10,
#     X = X,
#     x = x,
#     r = r
#   )
# #table
#
# # coef + estimate
# homeowner_c <- vanderweelevalue_rr(out_ct, f)
# homeowner_c
#
#
# homeowner_t <- out_ct %>%
#   #slice(1:max) |>
#   tibble() |>
#   rename(
#     Contrast = row,
#     Estimate = est,
#     std_error = se,
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
# homeowner_t
# homeowner_p <-
#   ggplot_stglm(
#     out_ct,
#     ylim = ylim_contrast,
#     main,
#     xlab,
#     ylab,
#     min = min,
#     p = p,
#     sub = sub
#   )
# homeowner_p


# income ------------------------------------------------------------------


# Promotion NZSEI ---------------------------------------------------------------
#Occupational prestige/status
#NZSEI06 (NZ Socio-economic index) Milne, B. J., Byun, U., & Lee, A. (2013). New Zealand socio-economic index 2006. Wellington: Statistics New Zealand.
#NZSEI13 (NZ Socio-economic index) Fahy, K. M., Lee, A., & Milne, B. J. (2017). New Zealand socio-economic index 2013. Wellington: Statistics New Zealand.
#NZSEI18 (NZ Socio-economic index) Boven, N., Shackleton, N., Bolton, L., Milne, B. (2021). The 2018 New Zealand Socioeconomic Index (NZSEI-19): A brief technical summary. Compass Research Centre.

Y = "NZSEI13_lead2_10_z"
main = "Occupational Status/10"
ylab = "Occupational Status/10"
sub = "NZ Socio-economic index 2013: Occupational Prestige"


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
nzsei_t <- out_ct <-
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

nzsei_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)

nzsei_c

nzsei_t <- out_ct %>%
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
nzsei_t
# graph
nzsei_p <-
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






# stand living ------------------------------------------------------------
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
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

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


