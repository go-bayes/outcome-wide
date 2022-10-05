

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
# ml <- readh("outcomewide-god-all")
mf <- readh("outcomewide-god-all-mf")
cor(mf$Believe.God, mf$PWI)
cor(mf$Believe.Spirit, mf$PWI)
cor(mf$Believe.Spirit, mf$LIFEMEANING)
cor(mf$Believe.Spirit, mf$LIFESAT)




###############  RENAME YOUR IMPUTED DATASET  'df"  ###############  ###############  ###############
###############   IMPORANT DO THIS   ###############  ###############  ###############  ###############

df <- readh("outcomewide-beliefs-raw-data-use.rds")


############### SET YOUR EXPOSURE VARIABLE, ###############  ###############  ###############

## HERE WE USE THE EXAMPLE OF HOURS WORK / 10
###############   IMPORTANT SET YOUR EXPOSURE VARIABLE

X = "Believe.God_lead1"


############### NEXT SET UP VARIABLES FOR MODELS AND GRAPHS

# You may set your label for your graphs  HERE WE STICK TO THE EXAMPLE OF WORK

xlab = "Do you believe in a God?"  ## Monthly Church

# SET THE RANGE OF religious service FROM ZERO TO 80
min = 0
max = 1

# set full range of X
x =  min:max

# range for some graphs
minmax <- paste(c(x), sep = ",")


# baseline condition here is 20 hours of work.  We could make it different
r = 0

# focal contrast for X  Someone who goes from 20 to 60 hours of work.
f = 1

# REQUIRED for certain model model functions
c = x

# contrast for graphs -- absolute distance from baseline
p = c(r, f) #


# Needed for E-VALUES -- how much do we move on the X scale to obtain our effect?
#delta = 4 #
delta = abs(r - f)

ylim = c(-.25, .4)  # SET AS YOU LIKE -- here, how much movement across a standard deviation unit of the outcome
ylim_contrast = c(.5, 2)# SET AS YOU LIKE (FOR CONTRASTS )

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
  # "Male_z",
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
  # "SexualOrientation",
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


family = "gaussian"


# functions ---------------------------------------------------------------

#
#
# ggplot_stglm_nomi <-
#   function(out_ct, ylim, main, xlab, ylab, min, p, sub) {
#     require(ggplot2)
#     out <-  out_ct
#     out$row <- 1:nrow(out)
#     out <- out |> dplyr::rename(est = "Estimate",
#                                 li = "lower.0.95",
#                                 ui = "upper.0.95",
#                                 se = "Std..Error")
#     g1 <- out[match(p, x), ]
#     g1
#     ggplot2::ggplot(out, aes(x = row, y = est)) +
#       geom_point() +
#       geom_pointrange(aes(ymin =  li, ymax = ui), colour = "darkgray")  +
#       scale_y_continuous(limits = ylim) +
#       labs(
#         title = main,
#         subtitle = sub,
#         x = xlab,
#         y = ylab
#       ) +
#       geom_pointrange(data = g1, aes(ymin = li, ymax = ui), colour = "red") +  # highlight contrast
#       theme_classic()
#   }
#
#
#
# glm_nomi_lin = function(X,Y,df, cvars, family = family) {
#   # requires that a MATCH THEM dataset is converted to a mice object
#   # weights must be called "weights)
#   out_m <- glm(
#     as.formula(paste(
#       paste(Y, "~ (", X , ")+"),
#       paste(cvars, collapse = "+")
#     )), family = family, data = df)
#   return(out_m)
# }
#
# glm_nomi = function(X, Y, df, cvars, family = family) {
#   # requires that a MATCH THEM dataset is converted to a mice object
#   # weights must be called "weights)
#   require("splines")
#   out_m <- glm(as.formula(paste(
#     paste(Y, "~ bs(", X , ")+"),
#     paste(cvars, collapse = "+")
#   )), family = family, data = df)
#   return(out_m)
# }
#
# vanderweelevalue_ols_nomi = function(out_ct, f, delta, sd) {
#   coef <- round(out_ct, 3)  |>  slice(f + 1)
#   evalout <-
#     as.data.frame(round(
#       EValue::evalues.OLS(
#         coef[1, 1],
#         se = coef[1, 2],
#         sd = 1,
#         delta = delta,
#         true = 0
#       ),
#       3
#     ))
#   evalout2 <- subset(evalout[2, ])
#   evalout2
#   evalout3 <- evalout2 |>
#     select_if( ~ !any(is.na(.)))
#   evalout3
#   colnames(evalout3) <- c("E-value", "threshold")
#   evalout3
#   tab <- round(cbind.data.frame(coef, evalout3), 3)
#   rownames(tab) <- main
#   return(tab)
# }
#
# vanderweelevalue_rr_nomi = function(out_ct, f) {
#   require("EValue")
#   coef <- round(out_ct, 3)  |>  slice(f + 1)
#   evalout <-
#     as.data.frame(round(EValue::evalues.RR(
#       coef[1, 1] ,
#       lo =  coef[1, 4],
#       hi = coef[1, 3],
#       true = 1
#     ), 3))
#   evalout2 <- subset(evalout[2, ])
#   evalout3 <- evalout2 |>
#     select_if( ~ !any(is.na(.)))
#   colnames(evalout3) <- c("E-value", "threshold")
#   tab <- cbind.data.frame(coef, evalout3)
#   rownames(tab) <- c(main)
#   return(tab)
# }

# plots
  #      se = "Std..Error")
  #   g1 <- out[match(p, x), ]
  #   g1
  #   ggplot2::ggplot(out, aes(x = row, y = est)) +
  #     geom_point() +
  #     geom_pointrange(aes(ymin =  li, ymax = ui), colour = "darkgray")  +
  #     scale_y_continuous(limits = ylim) +
  #     labs(
  #       title = main,
  #       subtitle = sub,
  #       x = xlab,
  #       y = ylab
  #     ) +
  #     geom_pointrange(data = g1, aes(ymin = li, ymax = ui), colour = "red") +  # highlight contrast
  #     theme_classic()
  # }

# HEALTH  INDICATORS ------------------------------------------------------------------
# alcohol freq ------------------------------------------------------------
#How often do you have a drink containing alcohol?


#cvars = "1"

Y = "Alcohol.Frequency_lead2ord_z"
main = "Alcohol Frequency"
ylab = "Alcohol Frequency (SD)"
sub = "How often do you have a drink containing alcohol?"
# regression


out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
)

out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

alcoholfreq_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
alcoholfreq_c
# graph
alcoholfreq_p <-
  ggplot_stglm_nomi(
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

# HEALTH  INDICATORS ------------------------------------------------------------------
# alcohol freq ------------------------------------------------------------
#How often do you have a drink containing alcohol?
Y = "Alcohol.Frequency_lead2ord_z"
main = "Alcohol Frequency"
ylab = "Alcohol Frequency (SD)"
sub = "How often do you have a drink containing alcohol?"
# regression

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

alcoholfreq_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
alcoholfreq_c
# graph
alcoholfreq_p <-
  ggplot_stglm_nomi(
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
#-----------------------------------#How many drinks containing alcohol do you have on a typical day when drinking?

Y = "Alcohol.Intensity_log_lead2_z"
main = "Alcohol Intensity"
ylab = "Alcohol Intensity (SD)"
sub = "How many drinks containing alcohol do you have on a typical day when drinking?"

# coef + estimate

out_m <-
 glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

alcoholintensity_c <-
  vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)


# graph
alcoholintensity_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
alcoholintensity_c
alcoholintensity_p



# bmi ---------------------------------------------------------------------
# What is your height? (metres)
# What is your weight? (kg)
# Kg/(m*m)

Y = "HLTH.BMI_lead2_z"
main = "BMI"
ylab = "BMI (SD)"
sub = "What is your height? (metres)\nWhat is your weight? (kg)\nKg *1/(m*m)"


out_m <-
 glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))


bmi_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
bmi_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

bmi_p

# exercise ---------------------------------------------------------------
# Hours spent … exercising/physical activity
Y = "Hours.Exercise_lead2_log_z"
main = "Log Hours Exercise"
ylab = "Log Hours Exercise (SD)"
sub = "Hours spent … exercising/physical activity"


out_m <-
 glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

exercise_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
exercise_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

exercise_p
exercise_c

# sf-health ---------------------------------------------------------------
# Short-Form Subjective Health Scale (General Health Perception Subscale)
# In general, would you say your health is...
# I seem to get sick a little easier than other people.
# I expect my health to get worse.

Y = "SFHEALTH_lead2_z"
main = "SF Health"
ylab = "SF Health (SD)"
sub = "In general, would you say your health is...\nI seem to get sick a little easier than other people.\nI expect my health to get worse."



out_m <-
 glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))


sfhealth_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
sfhealth_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
sfhealth_c
sfhealth_p

# HLTH.Sleep --------------------------------------------------------------
#During the past month, on average, how many hours of actual sleep did you get per night?

Y = "HLTH.SleepHours_lead2_z"
main = "Hours Sleep"
ylab = "Hours Sleep (SD)"
sub = "During the past month, on average, how many hours\nof actual sleep did you get per night?"

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

sleep_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
sleep_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

sleep_p


# smoker ------------------------------------------------------------------
#Do you currently smoke?

Y = "Smoker_lead2"
family = "binomial" # could be binomial if binary utcome is rare
main = "Smoking (RR)"
ylab = "Smoking (Risk Ratio)"
sub = "Do you currently smoke?"


out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "binomial",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "ratio"
  )))






smoker_c <- vanderweelevalue_rr_nomi_lo(out_ct, f)
smoker_c

# graph
smoker_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim_contrast,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

smoker_p

### EMBODIED WELL BEING ----------------------------------------------------


# body satisfaction -------------------------------------------------------
# Am satisfied with the appearance, size and shape of my body.
Y = "Bodysat_lead2_z"
main = "Body Satisfaction"
ylab = "Body Satisfaction (SD)"
sub = "Am satisfied with the appearance,\nsize and shape of my body."

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))


bodysat_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
bodysat_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
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

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

distress_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
distress_p <- ggplot_stglm_nomi(
  out_ct,
  ylim = ylim,
  main = main
  ,
  xlab,
  ylab,
  min = min,
  p = p,
  sub = sub
)
distress_c
distress_p



# fatigue -----------------------------------------------------------------
#During the last 30 days, how often did.... you feel exhausted?

Y = "HLTH.Fatigue_lead2_z"
main = "Fatigue"
ylab = "Fatigue (SD)"
sub = "During the last 30 days, how often did....\nyou feel exhausted?"

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

fatigue_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
fatigue_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

fatigue_c
fatigue_p



# rumination --------------------------------------------------------------
# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?

Y = "Rumination_lead2ord_z"
main = "Rumination"
ylab = "Rumination (SD)"
sub = "During the last 30 days, how often did....\nyou have negative thoughts that repeated over and over?"

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

rumination_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
rumination_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

rumination_p

rumination_c


# self control ------------------------------------------------------------
#In general, I have a lot of self-control.
#I wish I had more self-discipline.
Y = "SELF.CONTROL_lead2_z"
main = "Self Control"
ylab = "Self Control (SD)"
sub = "In general, I have a lot of self-control.\nI wish I had more self-discipline."

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

selfcontrol_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
selfcontrol_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

selfcontrol_c
selfcontrol_p



# sex satisfaction --------------------------------------------------------
# How satisfied are you with your sex life?
Y = "SexualSatisfaction_lead2_z"
main = "Sexual Satisfaction"
ylab = "Sexual Satisfaction (SD)"
sub = "How satisfied are you with your sex life?"


out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

sexualsat_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
sexualsat_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

sexualsat_c
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

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

gratitude_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
gratitude_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

gratitude_c
gratitude_p





# perm group ------------------------------------------------------------
#The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.
Y = "ImpermeabilityGroup_lead2_z"
main = "Impermeability Group"
ylab = "Impermeability Group (SD)"
sub = "The current income gap between New Zealand Europeans and\nother ethnic groups would be very hard to change."

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

groupimperm_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
groupimperm_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

groupimperm_c
groupimperm_p


# permeability self ----------------------------------------------------------------
#I believe I am capable, as an individual\nof improving my status in society.

Y = "PermeabilityIndividual_lead2_z"
main = "Permeability of Individual"
ylab = "Permeability of Individual (SD)"
sub = "I believe I am capable, as an individual,\nof improving my status in society."


out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

selfperm_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
selfperm_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

selfperm_c
selfperm_p

# life sat ----------------------------------------------------------------
# Satisfaction with life
# I am satisfied with my life.
# In most ways my life is close to ideal.

Y = "LIFESAT_lead2_z"
main = "Life Satisfaction"
ylab = "Life Satisfaction (SD)"
sub = "I am satisfied with my life.\nIn most ways my life is close to ideal."

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

lifesat_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
lifesat_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

lifesat_c
lifesat_p


# life meaning ------------------------------------------------------------
# Meaning in Life
# My life has a clear sense of purpose.
# I have a good sense of what makes my life meaningful.

Y = "LIFEMEANING_lead2_z"
main = "Life Meaning"
ylab = "Life Meaning (SD)"
sub = "My life has a clear sense of purpose.\nI have a good sense of what makes my life meaningful."

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

meaning_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
meaning_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

meaning_c
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

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

perfect_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
perfect_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

perfect_c
perfect_p

# PWI ---------------------------------------------------------
#Your health.
#Your standard of living.
#Your future security.
#Your personal relationships.


Y = "PWI_lead2_z"
main = "Person Wellbeing Index"
ylab = "PWI (SD)"
sub = "Satisfied with...\nYour health.\nYour standard of living.\nYour future security.\nYour personal relationships."

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

pwi_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
pwi_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


pwi_c
pwi_p



# power dependence 1 ------------------------------------------------------
# I do not have enough power or control over\nimportant parts of my life.
Y = "POWERDEPENDENCE1_lead2_z"
main = "Power Dependence 1"
ylab = "Power Dependence 1(SD)"
sub = "I do not have enough power or control\nover important parts of my life."

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

powerdependence1_c <-
  vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
powerdependence1_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

powerdependence1_c
powerdependence1_p



# power dependence 2 ------------------------------------------------------
#Other people have too much power or control over\nimportant parts of my life.

Y = "POWERDEPENDENCE2_lead2_z"
main = "Power Dependence 2"
ylab = "Power Dependence 2(SD)"
sub = "Other people have too much power or control\nover important parts of my life."

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

powerdependence2_c <-
  vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
powerdependence2_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

powerdependence2_c
powerdependence2_p




# self esteem -------------------------------------------------------------
# Self-esteem
# On the whole am satisfied with myself.
# Take a positive attitude toward myself.
# Am inclined to feel that I am a failure.


Y = "SELF.ESTEEM_lead2_z"
main = "Self Esteem"
ylab = "Self Esteem (SD)"
sub = "On the whole am satisfied with myself.\nTake a positive attitude toward myself.\nAm inclined to feel that I am a failure."




out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

selfesteem_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
selfesteem_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

selfesteem_c
selfesteem_p



# veng rumination ---------------------------------------------------------
# Forgivingness versus Vengeful Rumination
# Sometimes I can't sleep because of thinking about past wrongs I have suffered.
# I can usually forgive and forget when someone does me wrong.
# I find myself regularly thinking about past times that I have been wronged.

Y = "VENGEFUL.RUMIN_lead2_z"
main = "Vengefulness (anti-Foregiveness)"
ylab = "Vengefulness (anti-Foregiveness) (SD)"
sub = "Sometimes I can't sleep because of thinking about\npast wrongs I have suffered.\nI can usually forgive and forget when someone does me wrong.\nI find myself regularly thinking about past times that I have been wronged."



out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

veng_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
veng_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

veng_c
veng_p




# Work-life balance -------------------------------------------------------
# note-- we have no measure currently at baseline, so less confoundign control
# I have a good balance between work and other important things in my life.

Y = "Emp.WorkLifeBalance_lead2_z"
main = "Work Life Balance"
ylab = "Work Life Balance (SD)"
sub = "I have a good balance between work and\nother important things in my life."


out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

worklife_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
worklife_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

worklife_c
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

out_m <-
 glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

belong_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
belong_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

belong_c
belong_p



# community ----------------------------------------------------------
#I feel a sense of community with others in my local neighbourhood.
Y = "community_lead2_z"
main = "Community"
ylab = "Community (SD)"
sub = "I feel a sense of community with others\nin my local neighbourhood."

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

community_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
community_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

community_c
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

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

nwi_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
nwi_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

nwi_c
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


out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

support_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
support_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

support_c
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


out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

charity_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
charity_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

charity_c
charity_p


# volunteers --------------------------------------------------------------
#Hours spent in activities
#Hours spent … voluntary/charitable work

Y = "Volunteers_lead2"
main = "Volunteer (RR)"
ylab = "Volunteer (Risk Ratio)"
family = "binomial" # binary outcome  rare
sub = "Hours spent … voluntary/charitable work"


out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "binomial",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "ratio"
  )))

out_ct
volunteers_c <- vanderweelevalue_rr_nomi(out_ct, f)
volunteers_c

# graph
volunteers_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim_contrast,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

volunteers_p

volunteers_c
volunteers_p


#
# # log household income --------------------------------------------------------------
# # Please estimate your total household income (before tax) for the last year.
# Y = "income_lead2_log_z"
# main = "Log Income"
# ylab = "Log Income (SD)"
# sub = "Please estimate your total household income (before tax) for the last year."
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
# # graph
# income_p <-
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
# income_p


#
# # HOME OWNER --------------------------------------------------------------
# #Do you own your own home? (either partly or fully owned)
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
#

# Promotion NZSEI ---------------------------------------------------------------
#Occupational prestige/status
#NZSEI06 (NZ Socio-economic index) Milne, B. J., Byun, U., & Lee, A. (2013). New Zealand socio-economic index 2006. Wellington: Statistics New Zealand.
#NZSEI13 (NZ Socio-economic index) Fahy, K. M., Lee, A., & Milne, B. J. (2017). New Zealand socio-economic index 2013. Wellington: Statistics New Zealand.
#NZSEI18 (NZ Socio-economic index) Boven, N., Shackleton, N., Bolton, L., Milne, B. (2021). The 2018 New Zealand Socioeconomic Index (NZSEI-19): A brief technical summary. Compass Research Centre.

Y = "NZSEI13_lead2_10_z"
main = "Occupational Status/10"
ylab = "Occupational Status/10"
sub = "NZ Socio-economic index 2013: Occupational Prestige"


out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

nzsei_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
nzsei_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

nzsei_c
nzsei_p

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



out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

standardliving_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
standardliving_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


standardliving_c
standardliving_p



# Future Security  ------------------------------------------------------------
# Part of pwi
# Personal Wellbeing Index
# Your health.
# Your standard of living.
# Your future security.
# Your personal relationships.

Y = "Your.Future.Security_lead2_z"
main = "Your Future Security"
ylab = "Your Future Security (SD)"
sub  = "Satisfied with ...Your Future Security."


out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

yourfuturesecurity_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
yourfuturesecurity_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

yourfuturesecurity_c
yourfuturesecurity_p



## TABLE HEALTH

# Your Health -------------------------------------------------------------


Y = "Your.Health_lead2_z"
main = "Your Health"
ylab = "Your Health (SD)"
sub  = "Satisfied with ...Your Health."

out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

yourhealth_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
yourhealth_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

yourhealth_c
yourhealth_p




# Your Personal Relationships ---------------------------------------------

Y = "Your.Personal.Relationships_lead2_z"
main = "YourPersonal Relationships"
ylab = "Your Personal Relationships (SD)"
sub  = "Satisfied with ...Your Personal Relationships"



out_m <-
  glm_nomi_lin(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

yourpersonalrelationships_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
yourpersonalrelationships_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


yourpersonalrelationships_c
yourpersonalrelationships_p







## TABLE HEALTH
alcoholfreq_c
alcoholintensity_c
bmi_c
exercise_c
sfhealth_c
sleep_c
smoker_c
# TABLE  HEALTH  -----------------------------------------------
main = "Health outcome estimands / Evalues"
h_tab <- rbind(alcoholfreq_c,
               alcoholintensity_c,
               bmi_c,
               exercise_c,
               sfhealth_c,
               sleep_c,
               smoker_c)

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
rm(embody_tab)
embody_tab <- rbind(
  bodysat_c,
  distress_c,
  exercise_c,
  fatigue_c,
  rumination_c,
  selfcontrol_c,
  sexualsat_c
)
#
embody_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(2,4,5),  # Bold out the lines where EVALUES do not cross zero or for ratios, 1
           bold = T,
           # color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)



# TABLE REFLECTIVE WELLBEING ----------------------------------------------

meaning_p
main = "Reflective wellbeing estimands / Evalues"
reflect_tab <- rbind(
  gratitude_c,
  groupimperm_c,
  selfperm_c,
  lifesat_c,
  meaning_c,
  perfect_c,
  pwi_c,
  powerdependence1_c,
  powerdependence2_c,
  selfesteem_c,
  veng_c
)

reflect_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(3,4,7,8),
           bold = T,
           color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)



# TABLE SOCIAL WELLBEING --------------------------------------------------

main = "Social wellbeing estimands / Evalues"
social_tab <- rbind(belong_c,
                    community_c,
                    nwi_c,
                    support_c)

social_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(0),
           bold = T,
           color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)


# TABLE ECONOMIC WELLBEING and Charity ------------------------------------------------

main = "Economic wellbeing estimands / Evalues"
econ_tab <- rbind(
  charity_c,
  #  homeowner_c,
  #nzsei_c,
  standardliving_c,
  worklife_c#,
  #  volunteers_c
)

econ_tab |>
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


embody_plots <-
  bodysat_p +
  distress_p +
  fatigue_p +
  rumination_p +
  selfcontrol_p +
  sleep_p +
  sexualsat_p + plot_annotation(title = "Causal effects of income on embodied wellbeing", #subtitle = "xyz",
                                tag_levels = "A") +
  plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)

embody_plots

ggsave(
  embody_plots,
  path = here::here(here::here("figs", "beliefs-no-missing")),
  width = 15,
  height = 12,
  units = "in",
  filename = "embody_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


# GRAPHS HEALTH -----------------------------------------------------------
alcoholfreq_p
alcoholintensity_p
bmi_p
exercise_p
sfhealth_p
smoker_p

health_plots <- alcoholfreq_p +
  alcoholintensity_p +
  bmi_p +
  exercise_p +
  sfhealth_p +
  smoker_p +
  plot_annotation(title = "Causal effects of income on health outcomes",
                  # subtitle = "xyz",
                  tag_levels = "A") + plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)

# view
health_plots

ggsave(
  health_plots,
  path = here::here(here::here("figs", "beliefs-no-missing")),
  width = 15,
  height = 12,
  units = "in",
  filename = "health_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

dev.off()




# GRAPHS REFLECTIVE WELL-BEING ------------------------------------------------
gratitude_p
groupimperm_p
selfperm_p
lifesat_p
meaning_p
perfect_p
pwi_p
powerdependence1_p
powerdependence2_p
selfesteem_p
veng_p

reflective_plots <- gratitude_p +
  groupimperm_p +
  selfperm_p +
  lifesat_p +
  meaning_p +
  perfect_p +
  pwi_p +
  powerdependence1_p +
  powerdependence2_p +
  selfesteem_p +
  veng_p +
  plot_annotation(title = "Causal effects of income on reflective wellbeing") +
  plot_layout(guides = 'collect')

reflective_plots

# save

ggsave(
  reflective_plots,
  path = here::here(here::here("figs", "beliefs-no-missing")),
  width = 15,
  height = 12,
  units = "in",
  filename = "reflective_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

# GRAPHS SOCIAL WELL-BEING ------------------------------------------------
belong_p
community_p
nwi_p
support_p

social_plots <- belong_p +
  community_p +
  nwi_p +
  support_p + plot_annotation(title = "Causal effects of income on social wellbeing") +
  plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)

social_plots

ggsave(
  social_plots,
  path = here::here(here::here("figs", "beliefs-no-missing")),
  width = 15,
  height = 12,
  units = "in",
  filename = "social_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

social_plots
dev.off()


### GRAPHS ECONOMIC_SUCCESS GRAPHS ------------------------------------------------
charity_p
#nzsei_p
#nzsei_p
standardliving_p
worklife_p
volunteers_p

econ_plots <-# income_p +
  charity_p +
  # nzsei_p +
  standardliving_p +
  worklife_p +
  volunteers_p +
  plot_annotation(title = "Causal effects of income on economic wellbeing") +
  plot_layout(guides = 'collect')  # + plot_layout(ncol = 2)

# view
econ_plots

ggsave(
  econ_plots,
  path = here::here(here::here("figs", "beliefs-no-missing")),
  width = 15,
  height = 12,
  units = "in",
  filename = "econ_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

dev.off()





# GRAPH PWI SUBSCALE ------------------------------------------------------
yourpersonalrelationships_p
yourhealth_p
standardliving_p
futuresecurity_p

pwi_plots <-
  yourpersonalrelationships_p+
  yourhealth_p+
  standardliving_p+
  futuresecurity_p+plot_annotation(title = "Causal effects of income on PWI subcales", #subtitle = "xyz",
                                   tag_levels = "A") +
  plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)



# view
pwi_plots

ggsave(
  pwi_plots,
  path = here::here(here::here("figs", "beliefs-no-missing")),
  width = 15,
  height = 12,
  units = "in",
  filename = "pwi_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


