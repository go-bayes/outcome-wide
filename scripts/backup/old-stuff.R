
library(MatchThem)
library(optmatch)
models_hw <- weightthem(
  Hours.Work_lead1_z ~
    Hours.Work_z +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Bodysat_z +
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Church_z +
    community +
    Edu_z +
    Employed_z +
    EmotionRegulation1_z +
    EmotionRegulation2_z +
    EmotionRegulation3_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    # Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    ImpermeabilityGroup_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    Male_z +
    NZdep_z +
    NWI_z +
    NZSEI13_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    PermeabilityIndividual_z +
    Pol.Orient_z +
    POWERDEPENDENCE1_z + #
    POWERDEPENDENCE2_z + #
    # PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z + #
    SFHEALTH_z + #
    Smoker_z + #
    Spiritual.Identification_z +
    Standard.Living_z +
    SUPPORT_z + #
    Urban_z +
    VENGEFUL.RUMIN_z +
    Volunteers_z +
    Your.Health_z +
    Your.Future.Security_z +
    Your.Personal.Relationships_z,
  out2_sl,
  approach = "within",
  estimand = "ATE",
  stabilize = TRUE,
  method = "ebal"
)
saveh(models_hw, "models_hw")
sum <- summary(models_hw)
plot(sum)
sum
bal.tab(models_hw)


ctrim_st <- trim(models_hw, at = .998)
bal.tab(ctrim_st)
summary(ctrim_st)


# iptw models  STANDARD LIVING -------------------------------------------------------------
# no need to trim


out <- with(ctrim_st, glm(HLTH.BMI_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)


out <- with(ctrim_st, glm(SFHEALTH_lead2_z  ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)


out <-
  with(ctrim_st,
       glm(Hours.Exercise_lead2_log_z ~ Hours.Work_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st,
       glm(Smoker_lead2 ~ Hours.Work_lead1_z, family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp(-3.02 - .14)


round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st, glm(HLTH.Fatigue_lead2ord ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  0.01023787,
  se = 0.003899096,
  sd = 1,
  delta = 2,
  true = 0
),
3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)


out <-
  with(ctrim_st,
       glm(Alcohol.Frequency_lead2ord_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  0.01264201,
  se = 0.005129188,
  sd = 1,
  delta = 2,
  true = 0
),
3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st, glm(Alcohol.Intensity_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <- with(ctrim_st, glm(Bodysat_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  -0.007741457,
  se = 0.003929686,
  sd = 1,
  delta = 2,
  true = 0
),
3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

# out <- with(ctrim, glm(PWI_lead2_z ~ Standard.Living_lead1_z ))
# output <- pool(out, dfcom = NULL)
# summary(output, conf.int = TRUE)

out <-
  with(ctrim_st, glm(Rumination_lead2ord_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)


out <-
  with(ctrim_st,
       glm(SexualSatisfaction_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  0.009236795 ,
  se = 0.004273584 ,
  sd = 1,
  delta = 2,
  true = 0
),
3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st,
       glm(EmotionRegulation1_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st,
       glm(EmotionRegulation2_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st,
       glm(EmotionRegulation3_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st,
       glm(KESSLER6sum_lead2_z ~ Hours.Work_lead1_z , family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  -0.006205853 ,
  se = 0.002415414,
  sd = 1,
  delta = 2,
  true = 0
),
3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)


out <-
  with(ctrim_st, glm(POWERDEPENDENCE1_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st, glm(POWERDEPENDENCE1_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st, glm(PERFECTIONISM_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st, glm(SELF.ESTEEM_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <- with(ctrim_st, glm(GRATITUDE_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st, glm(VENGEFUL.RUMIN_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st, glm(LIFEMEANING_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)


out <-
  with(ctrim_st, glm(HONESTY_HUMILITY_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <- with(ctrim_st, glm(BELONG_lead2_z ~ Hours.Work_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <- with(ctrim_st, glm(SUPPORT_lead2_z ~ Hours.Work_lead1_10))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)


out <- with(ctrim_st, glm(Volunteers_lead2 ~ Hours.Work_lead1_10))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st,
       glm(CharityDonate_lead2 ~ Hours.Work_lead1_10 , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp(6.9785313)
exp(6.9785313 - 0.1001891)
exp(6.9785313 + 0.1001891 + 0.01807244)
exp(6.9785313 + 0.1001891 - 0.01807244)



round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(
  0.9046663,
  lo = 0.8884637,
  hi = 0.9211645,
  true = 1
),
3)

exp(-0.1001891  - 6.9785313) / exp(-6.9785313)
exp(-0.1001891 + 0.01807244 - 6.9785313) / exp(-6.9785313)
exp(-0.1001891 - 0.01807244 - 6.9785313) / exp(-6.9785313)


out <- with(ctrim_st, glm(community_lead2_z ~ Hours.Work_lead1_10))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <- with(ctrim_st, glm(NWI_lead2_z ~ Hours.Work_lead1_10))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  0.025172099 ,
  se = 0.007768097,
  sd = 1,
  delta = 2,
  true = 0
),
3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st,
       glm(ImpermeabilityGroup_lead2 ~ Hours.Work_lead1_10))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)


out <-
  with(ctrim_st,
       glm(Standard.Living_lead2ord_z ~ Hours.Work_lead1_10))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)


out <-
  with(ctrim_st,
       glm(Your.Future.Security_lead2_z ~ Hours.Work_lead1_10))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st, glm(Your.Health_lead2_z  ~ Hours.Work_lead1_10))

output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

out <-
  with(ctrim_st,
       glm(Your.Personal.Relationships_lead2ord_z ~ Hours.Work_lead1_10))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round(EValue::evalues.OLS(
  ,
  se = ,
  sd = 1,
  delta = 2,
  true = 0
), 3)
round(EValue::evalues.RR(, lo =  , hi = , true = 1), 3)


# NZSEI13 -- status variable



# Weights but now with the PWI  variables individually

library(MatchThem)
library(optmatch)
library(splines)
models_sl <- weightthem(
  Standard.Living_lead1_z ~
    bs(Standard.Living_z) +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Bodysat_z +
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Church_z +
    community +
    Edu_z +
    Employed_z +
    EmotionRegulation1_z +
    EmotionRegulation2_z +
    EmotionRegulation3_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    ImpermeabilityGroup_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    Male_z +
    NZdep_z +
    NWI_z +
    NZSEI13_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    PermeabilityIndividual_z +
    Pol.Orient_z +
    POWERDEPENDENCE1_z + #
    POWERDEPENDENCE2_z + #
    # PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z + #
    SFHEALTH_z + #
    Smoker_z + #
    Spiritual.Identification_z +
    #  Standard.Living_z +
    SUPPORT_z + #
    Urban_z +
    VENGEFUL.RUMIN_z +
    Volunteers_z +
    Your.Health_z +
    Your.Future.Security_z +
    Your.Personal.Relationships_z,
  out2_sl,
  approach = "within",
  estimand = "ATE",
  stabilize = TRUE,
  method = "ebal"
)


saveh(models_sl, "models_sl.rds")


sum <- summary(models_sl)
plot(sum)
sum
bal.tab(models_sl)


ctrim <- trim(models_sl, at = .999)
bal.tab(ctrim)
summary(ctrim)


# iptw models -------------------------------------------------------------
# no need to trim


out <-
  with(ctrim, glm(HLTH.BMI_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim, glm(SFHEALTH_lead2_z  ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim,
       glm(Hours.Exercise_lead2 ~ Standard.Living_lead1_z, family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim,
       glm(Smoker_lead2 ~ Standard.Living_lead1_z , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <-
  with(models_sl, glm(HLTH.Fatigue_lead2ord_z ~ bs(Standard.Living_lead1_z)))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

library(splines)
out2 <-  with(
  long3,
  #  long_f3,
  glm(
    HLTH.Fatigue_lead2ord_z ~  bs(Standard.Living_lead1_z) +
      Standard.Living_z +
      AGREEABLENESS_z +
      CONSCIENTIOUSNESS_z +
      EXTRAVERSION_z  +
      HONESTY_HUMILITY_z +
      NEUROTICISM_z +
      OPENNESS_z +
      Age_z +
      Alcohol.Frequency_z + #
      Alcohol.Intensity_log_z + #
      Bodysat_z +
      Believe.God_z +
      Believe.Spirit_z +
      BELONG_z + #
      CharityDonate_log_z + #
      ChildrenNum_z +
      Church_z +
      community +
      Edu_z +
      Employed_z +
      EmotionRegulation1_z +
      EmotionRegulation2_z +
      EmotionRegulation3_z +
      Euro_z +
      GRATITUDE_z +
      HomeOwner_z +
      Hours.Exercise_log_z +
      Hours.Work_z +
      HLTH.BMI_z  + #
      HLTH.Fatigue_z + #
      #  income_log_z +
      ImpermeabilityGroup_z +
      KESSLER6sum_z + #
      LIFEMEANING_z + #
      LIFESAT_z + #
      Male_z +
      NZdep_z +
      NWI_z +
      NZSEI13_z +
      Parent_z +
      Partner_z +
      PERFECTIONISM_z +
      PermeabilityIndividual_z +
      Pol.Orient_z +
      POWERDEPENDENCE1_z + #
      POWERDEPENDENCE2_z + #
      # PWI_z +
      Relid_z +
      Respect.Self_z + #
      Rumination_z + #
      SELF.CONTROL_z + #
      SELF.ESTEEM_z + #
      SexualSatisfaction_z + #
      SFHEALTH_z + #
      Smoker_z + #
      Spiritual.Identification_z +
      SUPPORT_z + #
      Urban_z +
      VENGEFUL.RUMIN_z +
      Volunteers_z +
      Your.Health_z +
      Your.Future.Security_z +
      Your.Personal.Relationships_z
  )
)
library(stdReg)
options(scipen = 999)

summary(out2)

output2 <- pool(out2, dfcom = NULL)
output2
#out2$df.null <- 27072

gform_m1 <-
  stdGlm(
    fit = out2,
    data = long3,
    X  = "Standard.Living_lead1_z",
    x = seq(-1, 1)
  )
summary(gform_m1)
plot(gform_m1)

Estimate Std. Error lower 0.95 upper 0.95
- 1  0.02876    0.00347   0.021970    0.03556
0   0.00491    0.00238   0.000244    0.00957
1  - 0.03225    0.00326  - 0.038644   - 0.02586

stimate Std. Error lower 0.95 upper 0.95
- 1   0.0306    0.00337   0.023963    0.03716
0    0.0050    0.00230   0.000485    0.00952
1   - 0.0330    0.00316  - 0.039235   - 0.02684

out <-
  with(ctrim,
       glm(Alcohol.Frequency_lead2ord ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim, glm(Alcohol.Intensity_lead2 ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(Bodysat_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

# out <- with(ctrim, glm(PWI_lead2_z ~ Standard.Living_lead1_z ))
# output <- pool(out, dfcom = NULL)
# summary(output, conf.int = TRUE)

out <-
  with(ctrim, glm(Rumination_lead2ord ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <-
  with(ctrim,
       glm(SexualSatisfaction_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <-
  with(ctrim,
       glm(EmotionRegulation1_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim,
       glm(EmotionRegulation2_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim,
       glm(EmotionRegulation3_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim,
       glm(KESSLER6sum_lead2 ~ Standard.Living_lead1_z , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim,
       glm(LIFEMEANING_lead2ord_z ~ Standard.Living_lead1_z , family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)




Estimate Std. Error lower 0.95 upper 0.95
- 1 - 0.09386    0.00340   - 0.10053  - 0.087199
0  - 0.00529    0.00222   - 0.00965  - 0.000932
1   0.08709    0.00294    0.08134   0.092848

out <-
  with(ctrim,
       glm(POWERDEPENDENCE1_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim,
       glm(POWERDEPENDENCE1_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim, glm(PERFECTIONISM_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim, glm(SELF.ESTEEM_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim, glm(GRATITUDE_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim, glm(VENGEFUL.RUMIN_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim, glm(LIFEMEANING_lead2ord ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim,
       glm(HONESTY_HUMILITY_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(BELONG_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(SUPPORT_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim, glm(Volunteers_lead2 ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim,
       glm(CharityDonate_log_lead2_z ~ Standard.Living_lead1_z , family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp(6.867947 + 0.2492043)


out <-
  with(ctrim, glm(community_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(NWI_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim,
       glm(ImpermeabilityGroup_lead2 ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <-
  with(ctrim,
       glm(PermeabilityIndividual_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)



# out <- with(ctrim2, glm( Standard.Living_lead2ord ~ Standard.Living_lead1_z ))
# output <- pool(out, dfcom = NULL)
# summary(output, conf.int = TRUE)


out <-
  with(ctrim,
       glm(Your.Future.Security_lead2_z ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim, glm(Your.Health_lead2_z  ~ Standard.Living_lead1_z))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <-
  with(ctrim,
       glm(
         Your.Personal.Relationships_lead2ord ~ Standard.Living_lead1_z
       ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)










# BRMS --------------------------------------------------------------------



# create list of data frames

out_c <-
  complete(ctrim,
           action = "long",
           include = FALSE,
           mild = TRUE)

m <- 10
listdat <- list()
for (i in 1:m) {
  listdat[[i]] <- as.data.frame(out_c[[i]])
}

# create list of data frames

out_c2 <-
  complete(ctrim2,
           action = "long",
           include = FALSE,
           mild = TRUE)

m <- 10
listdat2 <- list()
for (i in 1:m) {
  listdat2[[i]] <- as.data.frame(out_c2[[i]])
}


# enable memory
options(future.globals.maxSize = 8000 * 1024 ^ 2)  # needed



# BRMS MODELS forms -------------------------------------------------------------

bf_HLTH.BMI_lead2_z <-
  bf(HLTH.BMI_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_SFHEALTH_lead2_z <-
  bf(SFHEALTH_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_Hours.Exercise_lead2 <-
  bf(Hours.Exercise_lead2 |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Smoker_lead2 <-
  bf(Smoker_lead2 | weights(weights) ~ Standard.Living_lead1_z)
bf_HLTH.Fatigue_lead2ord <-
  bf(HLTH.Fatigue_lead2ord |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Alcohol.Frequency_lead2ord <-
  bf(Alcohol.Frequency_lead2ord |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Alcohol.Intensity_lead2 <-
  bf(as.integer(Alcohol.Intensity_lead2) |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Bodysat_lead2_z <-
  bf(Bodysat_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_PWI_lead2_z <-
  bf(PWI_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_Rumination_lead2ord <-
  bf(Rumination_lead2ord | weights(weights) ~ Standard.Living_lead1_z)
bf_SexualSatisfaction_lead2_z <-
  bf(SexualSatisfaction_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_PWI_lead2_z <-
  bf(PWI_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_EmotionRegulation1_lead2_z <-
  bf(EmotionRegulation1_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_EmotionRegulation2_lead2_z <-
  bf(EmotionRegulation2_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_EmotionRegulation3_lead2_z <-
  bf(EmotionRegulation3_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_KESSLER6sum_lead2 <-
  bf(KESSLER6sum_lead2 | weights(weights) ~ Standard.Living_lead1_z)
bf_LIFESAT_lead2ord <-
  bf(LIFESAT_lead2ord | weights(weights) ~ Standard.Living_lead1_z)
bf_POWERDEPENDENCE_lead2_z <-
  bf(POWERDEPENDENCE_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_PERFECTIONISM_lead2_z <-
  bf(PERFECTIONISM_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_SELF.ESTEEM_lead2_z <-
  bf(SELF.ESTEEM_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_Emp.WorkLifeBalance_lead2_z <-
  bf(Emp.WorkLifeBalance_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_GRATITUDE_lead2_z <-
  bf(GRATITUDE_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_VENGEFUL.RUMIN_lead2ord <-
  bf(VENGEFUL.RUMIN_lead2ord  |
       weights(weights) ~ Standard.Living_lead1_z)
bf_LIFEMEANING_lead2ord <-
  bf(LIFEMEANING_lead2ord  |
       weights(weights) ~ Standard.Living_lead1_z)
bf_HONESTY_HUMILITY_lead2_z <-
  bf(HONESTY_HUMILITY_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_BELONG_lead2_z <-
  bf(BELONG_lead2_z  | weights(weights) ~ Standard.Living_lead1_z)
bf_SUPPORT_lead2ord <-
  bf(SUPPORT_lead2ord | weights(weights) ~ Standard.Living_lead1_z)
bf_Volunteers_lead2 <-
  bf(Volunteers_lead2 | weights(weights) ~ Standard.Living_lead1_z)
bf_CharityDonate_lead2 <-
  bf(CharityDonate_lead2 | weights(weights) ~ Standard.Living_lead1_z)
bf_community_lead2_z <-
  bf(community_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_NWI_lead2_z <-
  bf(NWI_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_ImpermeabilityGroup_z <-
  bf(ImpermeabilityGroup_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_PermeabilityIndividual_z <-
  bf(PermeabilityIndividual_z |
       weights(weights) ~ Standard.Living_lead1_z)


## ADD ONE

bf_Standard.Living_lead2ord <-
  bf(Standard.Living_lead2ord |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Health_lead2_z <-
  bf(Your.Health_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Future.Security_lead2 <-
  bf(Your.Future.Security_lead2 |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Personal.Relationships_lead2ord <-
  bf(Your.Personal.Relationships_lead2ord |
       weights(weights) ~ Standard.Living_lead1_z)




# bmi ---------------------------------------------------------------------

m1_bmi_stome <- brm_multiple(
  bf_HLTH.BMI_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m1_bmi_stome.rds"),
)

m2__SFHEALTH_lead2_z <- brm_multiple(
  bf_SFHEALTH_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m2_SFHEALTH_lead2_z.rds"),
)


m3_Hours.Exercise_lead2 <- brm_multiple(
  bf_Hours.Exercise_lead2 ,
  data = listdat,
  # family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m3_Hours.Exercise_lead2.rds"),
)

m4_Smoker_lead2 <- brm_multiple(
  bf_Smoker_lead2,
  data = listdat,
  # family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m4_Smoker_lead2.rds"),
)

m5_HLTH.Fatigue_lead2ord <- brm_multiple(
  bf_HLTH.Fatigue_lead2ord ,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),
  # Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m5_HLTH.Fatigue_lead2ord.rds"),
)

m6_Alcohol.Frequency_lead2ord <- brm_multiple(
  bf_Alcohol.Frequency_lead2ord ,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here(
    "mods",
    "standardliving",
    "m6_Alcohol.Frequency_lead2ord.rds"
  ),
)

m7_Alcohol.Intensity_lead2 <- brm_multiple(
  bf_Alcohol.Intensity_lead2 ,
  data = listdat,
  # family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m7_Alcohol.Intensity_lead2.rds"),
)

m8_Bodysat_lead2_z <- brm_multiple(
  bf_Bodysat_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m8_Bodysat_lead2_z.rds"),
)

m9_PWI_lead2_z <- brm_multiple(
  bf_PWI_lead2_z ,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m9_PWI_lead2_z.rds"),
)

m10_Rumination_lead2ord <- brm_multiple(
  bf_Rumination_lead2ord,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m10_Rumination_lead2ord.rds"),
)

m11_SexualSatisfaction_lead2_z <- brm_multiple(
  bf_SexualSatisfaction_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here(
    "mods",
    "standardliving",
    "m11_SexualSatisfaction_lead2_z.rds"
  ),
)


m12_PWI_lead2_z  <- brm_multiple(
  bf_PWI_lead2_z,
  data = listdat,
  family = "gaussian",
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  file = here::here("mods", "standardliving", "m12_PWI_lead2_z.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

m13_EmotionRegulation1_lead2_z <- brm_multiple(
  bf_EmotionRegulation1_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here(
    "mods",
    "standardliving",
    "m13_EmotionRegulation1_lead2_z.rds"
  ),
)

m14_EmotionRegulation2_lead2_z <- brm_multiple(
  bf_EmotionRegulation2_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here(
    "mods",
    "standardliving",
    "m14_EmotionRegulation2_lead2_z.rds"
  ),
)


m15_EmotionRegulation3_lead2_z <- brm_multiple(
  bf_EmotionRegulation3_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here(
    "mods",
    "standardliving",
    "m15_EmotionRegulation3_lead2_z.rds"
  ),
)


m16_KESSLER6sum_lead2 <- brm_multiple(
  bf_KESSLER6sum_lead2,
  data = listdat,
  #  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m16_KESSLER6sum_lead2.rds"),
)


m17_LIFESAT_lead2ord <- brm_multiple(
  bf_LIFESAT_lead2ord ,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m17_LIFESAT_lead2_z.rds"),
)


m18_POWERDEPENDENCE_lead2_z <- brm_multiple(
  bf_POWERDEPENDENCE_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m18_POWERDEPENDENCE_lead2_z.rds"),
)


m19_PERFECTIONISM_lead2_z <- brm_multiple(
  bf_PERFECTIONISM_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m19_PERFECTIONISM_lead2_z.rds"),
)


m20_SELF.ESTEEM_lead2_z <- brm_multiple(
  bf_SELF.ESTEEM_lead2_z ,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m20_SELF.ESTEEM_lead2_z.rds"),
)


m21_Emp.WorkLifeBalance_lead2_z <- brm_multiple(
  bf_Emp.WorkLifeBalance_lead2_z ,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here(
    "mods",
    "standardliving",
    "m21_Emp.WorkLifeBalance_lead2_z.rds"
  ),
)


m22_GRATITUDE_lead2_z <- brm_multiple(
  bf_GRATITUDE_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m22_GRATITUDE_lead2_z.rds"),
)


m23_VENGEFUL.RUMIN_lead2ord <- brm_multiple(
  bf_VENGEFUL.RUMIN_lead2ord,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m23_VENGEFUL.RUMIN_lead2_z.rds"),
)


m24_LIFEMEANING_lead2ord <- brm_multiple(
  bf_LIFEMEANING_lead2ord ,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m24_LIFEMEANING_lead2ord"),
)


m25_HONESTY_HUMILITY_lead2_z <- brm_multiple(
  bf_HONESTY_HUMILITY_lead2_z ,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m25_HONESTY_HUMILITY_lead2_z.rds"),
)


m26_BELONG_lead2_z <- brm_multiple(
  bf_BELONG_lead2_z ,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m26_BELONG_lead2_z.rds"),
)


m27_SUPPORT_lead2_z <- brm_multiple(
  bf_SUPPORT_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m27_SUPPORT_lead2_z.rds"),
)


m28_Volunteers_lead2 <- brm_multiple(
  bf_Volunteers_lead2,
  data = listdat,
  #family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m28_Volunteers_lead2.rds"),
)


m28_CharityDonate_lead2 <- brm_multiple(
  bf_CharityDonate_lead2,
  data = listdat,
  # family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m28_CharityDonate_lead2.rds"),
)


m29_community_lead2_z <- brm_multiple(
  bf_community_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m29_community_lead2_z.rds"),
)


m30_NWI_lead2_z <- brm_multiple(
  bf_NWI_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m30_NWI_lead2_z.rds"),
)


m31_ImpermeabilityGroup_z <- brm_multiple(
  bf_ImpermeabilityGroup_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m31_ImpermeabilityGroup_z.rds"),
)


m32_PermeabilityIndividual_z <- brm_multiple(
  bf_PermeabilityIndividual_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m32_PermeabilityIndividual_z.rds"),
)

## try GEE

library(geepack)

out3 <- with(
  ctrim,
  geeglm(
    PWI_lead2_z  ~ Standard.Living_lead1_z,
    data = cmodels,
    id = 1:nrow(cmodels),
    family = gaussian
  )
)

# same result
output <- pool(out3)
summary(output, conf.int = TRUE)
plot(output)


# brms pwi follow up ------------------------------------------------------

bf_Standard.Living_lead2_z <-
  bf(Standard.Living_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Health_lead2_z <-
  bf(Your.Health_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Future.Security_lead2_z <-
  bf(Your.Future.Security_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Personal.Relationships_lead2ord <-
  bf(Your.Personal.Relationships_lead2ord |
       weights(weights) ~ Standard.Living_lead1_z)


long2$Standard.Living_lead2_z


m33_Standard.Living_lead2_z <- brm_multiple(
  bf_Standard.Living_lead2_z,
  data = listdat2,
  family = "gaussian",
  #  family = cumulative("probit"),
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m33_Standard.Living_lead2_z.rds"),
)



m34_Your.Health_lead2_z <- brm_multiple(
  bf_Your.Health_lead2_z,
  data = listdat2,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m34_Your.Health_lead2_z.rds"),
)

m35_Your.Future.Security_lead2_z <- brm_multiple(
  bf_Your.Future.Security_lead2_z,
  data = listdat2,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here(
    "mods",
    "standardliving",
    "m35_Your.Future.Security_lead2_z.rds"
  ),
)


m36_Your.Personal.Relationships_lead2ord <- brm_multiple(
  bf_Your.Personal.Relationships_lead2ord,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),
  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here(
    "mods",
    "standardliving",
    "m36_Your.Personal.Relationships_lead2ord.rds"
  ),
)


# dag ---------------------------------------------------------------------



## GGDAG
# This isn't correct. EVERYONE experiences COVID LOCKDOWNS by the time they are measured the following year.  So COVID cannot differentially affect the outcome.


library(ggdag)
dg2 <- dagify(
  Y2 ~  A2 + C1 + Y1 + A0 + C0,
  A2 ~ A1 + CV + Y1 + C1 + C0 + A0 + CV,
  Y1 ~ CV + A1 + C1 + Y0 + A0 + CV,
  A1 ~ C1 + C0 + CV +Y0 + CV + A0,
  C1 ~ CV + C0 + A0 + Y0,
  C1 ~ CV + A0 + Y0 + C0,
  A0 ~ C0,
  Y0 ~ A0 + C0,
  #Y0 ~ A0 + C0,
  exposure =  "A1",
  outcome =   "Y2") |>
  # labels = c(
  #   "wm" = "Warmth Muslims",
  #   "a" = "age",
  #   "eth" = "ethnicity",
  #   "pp" = "pre/postattacks",
  #   "cs" = "political conservativism",
  #   "edu" = "education",
  #   "ur" = "urban",
  #   "yr" = "years",
  #   "rel" = "religion",
  #   "news" = "media",
#   "ml" = "male",
#   "U" = "Soc Networks"
# )%>%
tidy_dagitty()
#%>%
#  control_for("cs")
ggdag(dg2,
      # use_labels = "label",
      text = T) + ggtitle("COVID") + theme_dag_blank()

ggdag::ggdag_collider(dg2,
                      #  use_labels = "label",
                      text = T) +
  theme_dag_blank() +
  labs(title = "Causal analysis of X")


ggdag::ggdag_adjustment_set(dg2,
                            #  use_labels = "label",
                            text = T) + theme_dag_blank() +
  labs(title = "To obtained an unbiased estimate, condition on *years*")

ggdag_dseparated(
  dg2,
  from = "A1",
  to = "Y2",
  controlling_for = c("C0","CV","Y0","A1"),
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  # text_col = "white",
  #label_col = text_col,
  #node = TRUE,
  #stylized = FALSE,
  text = TRUE,
  #use_labels = F,
  collider_lines = TRUE
) + theme_dag_blank()

# old IPTW ----------------------------------------------------------------

# iptw income  --------------------------------------------------------------------
#income

# Weights but now with the PWI  variables individually
