# Hours work outcomewide

# read data from multiple imputation performed in the "standard_living.R" script
# data were imputed in mice
out2_a <- readh("out2_a")


# iptw --------------------------------------------------------------------


# Weights but now with the PWI  variables individually

library(MatchThem)
library(optmatch)
models_hw <- weightthem( Hours.Work_lead1ord ~ Hours.Work_ord +
                          AGREEABLENESS_z +
                          CONSCIENTIOUSNESS_z +
                          EXTRAVERSION_z  +
                          HONESTY_HUMILITY_z +
                          NEUROTICISM_z +
                          OPENNESS_z +
                          Age_z +
                          Alcohol.Frequency_z +
                          Alcohol.Intensity_log_z +
                          Bodysat_z +
                          Believe.God_z +
                          Believe.Spirit_z +
                          BELONG_z +
                          CharityDonate_log_z +
                          ChildrenNum_z +
                          Church_z +
                          community +
                          Edu_z +
                          Employed_z +
                          EmotionRegulation1_z +
                          EmotionRegulation2_z +
                          EmotionRegulation3_z +
                          as.factor(EthCat) +
                          GRATITUDE_z +
                          HomeOwner_z +
                          Hours.Exercise_log_z +
                         # Hours.Work_z +
                          HLTH.BMI_z  +
                          HLTH.Fatigue_z +
                          income_log_z +
                          ImpermeabilityGroup_z +
                          KESSLER6sum_z +
                          LIFEMEANING_z +
                          LIFESAT_z +
                          Male_z +
                          NZdep_z +
                          NWI_z +
                          NZSEI18_z +
                          Parent_z +
                          Partner_z +
                          PERFECTIONISM_z +
                          PermeabilityIndividual_z +
                          Pol.Orient_z +
                          POWERDEPENDENCE1_z +
                          POWERDEPENDENCE2_z +
                          # PWI_z +
                          Relid_z +
                          Respect.Self_z +
                          Rumination_z +
                          SELF.CONTROL_z +
                          SELF.ESTEEM_z +
                          SexualSatisfaction_z +
                          SFHEALTH_z +
                          Smoker_z +
                          Standard.Living_z +
                          SUPPORT_z +
                          Urban_z +
                          VENGEFUL.RUMIN_z +
                          Volunteers_z +
                          Your.Health_z +
                          Your.Future.Security_z +
                          Your.Personal.Relationships_z,
                         out2_a,
                        approach = "within",
                        estimand = "ATE",
                        stabilize = TRUE,
                        method = "optweight")


saveh(models_hw,"models_hw.rds")


bal.tab(models_hw)
sum<- summary(models_hw)
plot(sum)
sum

ctrim<- models_hw
#ctrim <- trim(models_hw, at = .99)
bal.tab(ctrim)
summary(ctrim)

library(mice)
library(MatchThem)

#"weighted.dataset" is our .wimids object

#Extracting the original dataset with missing value
maindataset <- complete(models_hw, action = 0)

#Some spit-and-polish
maindataset <- data.frame(.imp = 0, .id = seq_len(nrow(maindataset)), maindataset)

#Extracting imputed-weighted datasets in the long format
alldataset  <- complete(models_hw, action = "long")

#Binding them together
alldataset  <- rbind(maindataset, alldataset)

#Converting to .mids
newmids <- as.mids(alldataset)

out <- with(newmids, glm( HLTH.BMI_lead2_z ~ Hours.Work_lead1ord ))

## Iptw
newdata = data.frame(
  n = 1,
  Church_lead1 = c(0, 4),
# iptw models  ------------------------------------------------------------


#y +
out <- with(ctrim, lm( HLTH.BMI_lead2_z ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary <- summary(output, conf.int = TRUE)
summary


#y+
out <- with(ctrim, glm(SFHEALTH_lead2_z  ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y-
out <- with(ctrim, lm(Hours.Exercise_lead2  ~ Hours.Work_lead1ord, family = "poisson" ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

# gcomputation
predm <- lapply(getfit(out), predict, newdata = data.frame(
  Hours.Work_lead1ord = c(0, 1, 2, 3, 4, 5)), se.fit = TRUE)

predd <- lapply(getfit(out), predict, newdata = data.frame(
  Hours.Work_lead1ord = c(4)), se.fit = TRUE)

predm
Q <- sapply(predm, `[[`, "fit")
U <- sapply(predm, `[[`, "se.fit")^2
dfcom <- predm[[1]]$df

# pool predictions
pred <- matrix(NA, nrow = nrow(Q), ncol = 3,
               dimnames = list(NULL, c("fit", "se.fit", "df")))
for(i in 1:nrow(Q)) {
  pi <- pool.scalar(Q[i, ], U[i, ], n = dfcom + 1)
  pred[i, 1] <- pi[["qbar"]]
  pred[i, 2] <- sqrt(pi[["t"]])
  pred[i, 3] <- pi[["df"]]
}


str(pred)
pred<- data.frame(pred)
pred$x<- c(0:5)
pred
ggplot(pred, aes(x, fit)) + geom_point() + geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=.2,
                                                         position=position_dodge(.9)) +
  scale_y_continuous(limits = c(0,8))


#n
out <- with(ctrim, glm( Smoker_lead2 ~ Hours.Work_lead1ord , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp(-3.02-.14)

#n
out <- with(ctrim, glm(HLTH.Fatigue_lead2ord ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y
out <- with(ctrim, glm(Alcohol.Frequency_lead2ord ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y+
out <- with(ctrim, glm(Alcohol.Intensity_lead2~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y-
out <- with(ctrim, glm(Bodysat_lead2_z ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

# out <- with(ctrim, glm(PWI_lead2_z ~ Hours.Work_lead1ord ))
# output <- pool(out, dfcom = NULL)
# summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( Rumination_lead2ord ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm(SexualSatisfaction_lead2_z ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y-
out <- with(ctrim, glm(EmotionRegulation1_lead2_z ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm(EmotionRegulation2_lead2_z~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( EmotionRegulation3_lead2_z ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y-
out <- with(ctrim, glm(KESSLER6sum_lead2 ~ Hours.Work_lead1ord , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm(POWERDEPENDENCE_lead2_z ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm(PERFECTIONISM_lead2_z ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm(SELF.ESTEEM_lead2_z ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( GRATITUDE_lead2_z ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( VENGEFUL.RUMIN_lead2_z ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( LIFEMEANING_lead2ord ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( HONESTY_HUMILITY_lead2_z ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( BELONG_lead2_z ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( SUPPORT_lead2_z ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( Volunteers_lead2 ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( CharityDonate_lead2 ~ Hours.Work_lead1ord , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp( 6.867947 + 0.2492043)

#n
out <- with(ctrim, glm(community_lead2_z ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y
out <- with(ctrim, glm(NWI_lead2_z~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm(ImpermeabilityGroup_lead2 ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


#n
out <- with(ctrim, glm( Standard.Living_lead2ord ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( Your.Future.Security_lead2_z ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( Your.Health_lead2_z  ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


#n
out <- with(ctrim2, glm( Your.Personal.Relationships_lead2ord ~ Hours.Work_lead1ord ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)



###

# create list of data frames

out_c <- complete(ctrim, action ="long", include = FALSE, mild = TRUE)

m <- 10
listdat<- list()
for (i in 1:m) {
  listdat[[i]] <- as.data.frame(out_c[[i]])
}

# enable memory
options(future.globals.maxSize = 8000 * 1024^2)  # needed



# BRMS MODELS forms -------------------------------------------------------------

bf_HLTH.BMI_lead2_z <- bf(HLTH.BMI_lead2_z |weights(weights) ~ mo(Hours.Work_lead1ord))
bf_SFHEALTH_lead2_z <- bf(SFHEALTH_lead2_z |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_Hours.Exercise_lead2 <- bf(Hours.Exercise_lead2 |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_Smoker_lead2 <- bf( Smoker_lead2 |weights(weights) ~ mo(Hours.Work_lead1ord))
bf_HLTH.Fatigue_lead2ord <- bf( HLTH.Fatigue_lead2ord |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_Alcohol.Frequency_lead2ord <- bf( Alcohol.Frequency_lead2ord |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_Alcohol.Intensity_lead2 <- bf( as.integer(Alcohol.Intensity_lead2) |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_Bodysat_lead2_z <- bf( Bodysat_lead2_z |weights(weights) ~ mo(Hours.Work_lead1ord))
bf_PWI_lead2_z <- bf( PWI_lead2_z |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_Rumination_lead2ord <- bf(Rumination_lead2ord |weights(weights) ~ mo(Hours.Work_lead1ord))
bf_SexualSatisfaction_lead2_z <- bf(SexualSatisfaction_lead2_z |weights(weights) ~  mo(Hours.Work_lead1ord))
# bf_PWI_lead2_z <- bf(PWI_lead2_z |weights(weights) ~ mo(Hours.Work_lead1ord))
bf_EmotionRegulation1_lead2_z <- bf(EmotionRegulation1_lead2_z |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_EmotionRegulation2_lead2_z <- bf(EmotionRegulation2_lead2_z |weights(weights) ~ mo(Hours.Work_lead1ord))
bf_EmotionRegulation3_lead2_z <- bf(EmotionRegulation3_lead2_z |weights(weights) ~ mo(Hours.Work_lead1ord))
bf_KESSLER6sum_lead2 <- bf(KESSLER6sum_lead2 |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_LIFESAT_lead2ord <- bf(LIFESAT_lead2ord |weights(weights) ~ mo(Hours.Work_lead1ord))
bf_POWERDEPENDENCE_lead2_z <- bf(POWERDEPENDENCE_lead2_z |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_PERFECTIONISM_lead2_z <- bf(PERFECTIONISM_lead2_z |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_SELF.ESTEEM_lead2_z <- bf(SELF.ESTEEM_lead2_z |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_Emp.WorkLifeBalance_lead2_z <- bf( Emp.WorkLifeBalance_lead2_z |weights(weights) ~ mo(Hours.Work_lead1ord))
bf_GRATITUDE_lead2_z <- bf( GRATITUDE_lead2_z |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_VENGEFUL.RUMIN_lead2ord <- bf(VENGEFUL.RUMIN_lead2ord  |weights(weights) ~ mo(Hours.Work_lead1ord))
bf_LIFEMEANING_lead2ord <- bf(LIFEMEANING_lead2ord  |weights(weights) ~ mo(Hours.Work_lead1ord))
bf_HONESTY_HUMILITY_lead2_z <- bf( HONESTY_HUMILITY_lead2_z |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_BELONG_lead2_z <- bf( BELONG_lead2_z  |weights(weights) ~ mo(Hours.Work_lead1ord))
bf_SUPPORT_lead2ord <- bf( SUPPORT_lead2ord |weights(weights) ~ mo(Hours.Work_lead1ord))
bf_Volunteers_lead2 <- bf( Volunteers_lead2 |weights(weights) ~ mo(Hours.Work_lead1ord))
bf_CharityDonate_lead2 <- bf( CharityDonate_lead2 |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_community_lead2_z <- bf(community_lead2_z |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_NWI_lead2_z <- bf(NWI_lead2_z |weights(weights) ~ mo(Hours.Work_lead1ord))
bf_ImpermeabilityGroup_z <- bf(ImpermeabilityGroup_z |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_PermeabilityIndividual_z<- bf( PermeabilityIndividual_z|weights(weights) ~  mo(Hours.Work_lead1ord))

bf_Standard.Living_lead2ord<- bf( Standard.Living_lead2ord|weights(weights) ~  mo(Hours.Work_lead1ord))
bf_Your.Health_lead2_z<- bf( Your.Health_lead2_z|weights(weights) ~  mo(Hours.Work_lead1ord))
bf_Your.Future.Security_lead2 <- bf(Your.Future.Security_lead2 |weights(weights) ~  mo(Hours.Work_lead1ord))
bf_Your.Personal.Relationships_lead2ord<- bf( Your.Personal.Relationships_lead2ord|weights(weights) ~  mo(Hours.Work_lead1ord))



# bmi ---------------------------------------------------------------------

m1_bmi_income <- brm_multiple(
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
  file = here::here("mods", "hourswork", "m1_bmi_income.rds"),
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
  file = here::here("mods", "hourswork", "m2_SFHEALTH_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m3_Hours.Exercise_lead2.rds"),
)

m4_Smoker_lead2<- brm_multiple(
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
  file = here::here("mods", "hourswork", "m4_Smoker_lead2.rds"),
)

m5_HLTH.Fatigue_lead2ord <- brm_multiple(
  bf_HLTH.Fatigue_lead2ord ,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"), # Chose family
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
  file = here::here("mods", "hourswork", "m5_HLTH.Fatigue_lead2ord.rds"),
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
  file = here::here("mods", "hourswork", "m6_Alcohol.Frequency_lead2ord.rds"),
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
  file = here::here("mods", "hourswork", "m7_Alcohol.Intensity_lead2.rds"),
)

m8_Bodysat_lead2_z<- brm_multiple(
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
  file = here::here("mods", "hourswork", "m8_Bodysat_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m9_PWI_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m10_Rumination_lead2ord.rds"),
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
  file = here::here("mods", "hourswork", "m11_SexualSatisfaction_lead2_z.rds"),
)


# m12_PWI_lead2_z  <- brm_multiple(
#   bf_PWI_lead2_z,
#   data = listdat,
#   family = "gaussian",
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   init = 0,
#   backend = "cmdstanr",
#   file = here::here("mods", "hourswork", "m12_PWI_lead2_z.rds"),
#   set_prior('normal(0, 1)', class = 'b')
# )

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
  file = here::here("mods", "hourswork", "m13_EmotionRegulation1_lead2_z.rds"),
)

m14_EmotionRegulation2_lead2_z<- brm_multiple(
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
  file = here::here("mods", "hourswork", "m14_EmotionRegulation2_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m15_EmotionRegulation3_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m16_KESSLER6sum_lead2.rds"),
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
  file = here::here("mods", "hourswork", "m17_LIFESAT_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m18_POWERDEPENDENCE_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m19_PERFECTIONISM_lead2_z.rds"),
)


m20_SELF.ESTEEM_lead2_z<- brm_multiple(
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
  file = here::here("mods", "hourswork", "m20_SELF.ESTEEM_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m21_Emp.WorkLifeBalance_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m22_GRATITUDE_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m23_VENGEFUL.RUMIN_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m24_LIFEMEANING_lead2ord"),
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
  file = here::here("mods", "hourswork", "m25_HONESTY_HUMILITY_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m26_BELONG_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m27_SUPPORT_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m28_Volunteers_lead2.rds"),
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
  file = here::here("mods", "hourswork", "m28_CharityDonate_lead2.rds"),
)


m29_community_lead2_z<- brm_multiple(
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
  file = here::here("mods", "hourswork", "m29_community_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m30_NWI_lead2_z.rds"),
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
  file = here::here("mods", "hourswork", "m31_ImpermeabilityGroup_z.rds"),
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
  file = here::here("mods", "hourswork", "m32_PermeabilityIndividual_z.rds"),
)


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
  file = here::here("mods", "incomemods", "m33_Standard.Living_lead2_z.rds"),
)



m34_Your.Health_lead2_z <- brm_multiple(
  bf_Your.Health_lead2_z,
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
  file = here::here("mods", "incomemods", "m34_Your.Health_lead2_z.rds"),
)

m35_Your.Future.Security_lead2_z <- brm_multiple(
  bf_Your.Future.Security_lead2_z,
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
  file = here::here("mods", "incomemods", "m35_Your.Future.Security_lead2_z.rds"),
)


m36_Your.Personal.Relationships_lead2ord <- brm_multiple(
  bf_Your.Personal.Relationships_lead2ord,
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
  file = here::here("mods", "incomemods", "m36_Your.Personal.Relationships_lead2ord.rds"),
)





## try GEE

library(geepack)

out3 <- with(ctrim, geeglm(
  NWI_lead2_z  ~ Hours.Work_lead1ord,
  id = 1:31280, # n idea
  family = gaussian))

# same result
output3 <- pool(out3)
summary(output3, conf.int = TRUE)


## GLM


out4 <- with(ctrim, glm(
  NWI_lead2_z  ~ Hours.Work_lead1ord,
  family = gaussian))

# same result
output4 <- pool(out4)
summary(output4, conf.int = TRUE)

