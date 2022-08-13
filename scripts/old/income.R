
# income.R
# install
# https://github.com/markmfredrickson/optmatch


## MICE MODEL
# library("dplyr")
# library("here")
# library("skimr")
# library("tidyr")
# library("Amelia")
# library("ggplot2")
# library("purrr")
# library("patchwork")
# library("kableExtra")
# library("parameters")
# library("mice")
# library("ggokabeito")   # color palette
# library("brms") # bayesian estimation
# library("ggpubr")
# library("cmdstanr")
# rstan_options(auto_write = TRUE) # bayesian estimation
# options(mc.cores = parallel::detectCores ()) # use all course
# theme_set(theme_pubclean()) # nice theme
# # theme_set(theme_classic())


# read data
df <- readRDS(here::here("data_raw", "df.Rds"))

# SWB.SoC01.T10	I feel a sense of community with others in my local neighbourhood.
# read files
source(here::here("scripts", "funs.R"))

# read libraries in
source(here::here("scripts", "libs.R"))
library("dplyr")
library("tidyr")


# order vars
df$GenCohort <-
  ordered(
    df$GenCohort,
    levels = c(
      "Gen_Silent: born< 1946",
      "Gen Boomers: born >= 1946 & b.< 1965",
      " GenX: born >=1961 & b.< 1981",
      "GenZ: born >= 1996 "
    )
  )


# # view
# df %>%   # not there
#   dplyr::filter(Wave == 2020) %>%
#   summarise(Respect.Self) #fully missing


# table for participant N
tabinc_df <- df %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(Id != 9630) %>% # problematic
  group_by(Id) %>%
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  ungroup() %>%
  dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
  droplevels() %>%
  arrange(Id, Wave) %>%
  dplyr::group_by(Id) %>%   # get all people who were in that wave unless they died
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  ungroup() %>%
  data.frame() %>%
  arrange(Id, Wave)

# check n # 34782
table1::table1(~ Household.INC | Wave , data = tabinc_df, overall = FALSE)
# check N of ids
length(unique(tabinc_df$Id)) # 34782


## select vars
i_df <- tabinc_df %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(Id != 9630) %>% # problematic
  select(
    Id,
    YearMeasured,
    Wave,
    EthCat,
    Age,
    GendAll,
    Male,
    NZSEI18,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    Edu,
    NZdep,
    Employed,
    HomeOwner,
    Pol.Orient,
    Urban,
    Household.INC,
    Parent,
    Partner,
    Relid,
    Religion.Church,
    Believe.Spirit,
    Believe.God,
    SWB.SoC01,
    EmotionRegulation1,
    EmotionRegulation2,
    EmotionRegulation3,
    Bodysat,
    VENGEFUL.RUMIN,
    retired,
    semiretired,
    # BornNZ, nor working
    KESSLER6sum,
    HLTH.Fatigue,
    Rumination,
    Smoker,
    #   ChildrenNum,
    NWI,
    BELONG,
    SUPPORT,
    CharityDonate,
    HoursCharity,
    GRATITUDE,
    # Volunteers,
    Hours.Work,
    Hours.Exercise,
    LIFEMEANING,
    LIFESAT,
    PWI,
    NWI,
    SFHEALTH,
    SELF.CONTROL,
    SFHEALTH,
    SELF.ESTEEM,
    Respect.Self,
    #  GenCohort,
    SELF.ESTEEM,
    SELF.CONTROL,
    #  Respect.Self,
    Emp.WorkLifeBalance,
    Alcohol.Frequency,
    Alcohol.Intensity,
    HLTH.BMI,
    Smoker,
    ChildrenNum,
    # GenCohort,
    # Euro,
    # partnerlost_job, rare
    #lost_job,
    #began_relationship,
    Alcohol.Intensity,
    Alcohol.Frequency,
    SexualSatisfaction,
    POWERDEPENDENCE,
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living,
    # Env.SacWilling,
    #Env.SacMade,
    PERFECTIONISM ,
    PermeabilityIndividual,
    ImpermeabilityGroup
    # Emp.JobSecure,
    #  Env.ClimateChgCause,
    # Env.ClimateChgReal #,
  ) %>%
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  group_by(Id) %>%
  dplyr::mutate(org2019 =  ifelse(Wave == 2019 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  # dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  ungroup() %>%
  dplyr::filter(YearMeasured  != -1) %>%
  droplevels() %>%
  arrange(Id, Wave) %>%
  dplyr::group_by(Id) %>%   # get all people who were in that wave unless they died
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  ungroup() %>%
  dplyr::mutate(across(!c(Id, EthCat, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
  arrange(Id, Wave) %>%
  dplyr::mutate(
    Edu = as.numeric(Edu),
    Volunteers = if_else(HoursCharity == 1, 1, 0),
    Depressed = (as.numeric(
      cut(
        KESSLER6sum,
        breaks = c(-Inf, 13, Inf),
        labels = c("0", "1"),
        right = FALSE
      )
    ) - 1),
    EthCat = factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    Euro = as.numeric(if_else(EthCat == "Euro", 1, 0)),
    Male = ifelse(GendAll == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1),
  ) %>%
  arrange(Id, Wave)  %>%
  dplyr::mutate(income_log_lead1 = lead(income_log, n = 1)) %>%
  dplyr::mutate(retired_lead1 = lead(retired, n = 1)) %>%
  dplyr::mutate(semiretired_lead1 = lead(semiretired, n = 1)) %>%
  #dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::mutate(across(
    c(
      income_log,
      Depressed,
      KESSLER6sum,
      HLTH.Fatigue,
      Rumination,
      community,
      SFHEALTH,
      LIFEMEANING,
      LIFESAT,
      PWI,
      #  not_euro,
      SELF.ESTEEM,
      SELF.CONTROL,
      Respect.Self,
      Alcohol.Frequency,
      Hours.Exercise,
      HLTH.BMI,
      Smoker,
      ChildrenNum,
      NWI,
      BELONG,
      SUPPORT,
      Volunteers,
      GRATITUDE,
      SexualSatisfaction,
      POWERDEPENDENCE,
      #Env.SacWilling,
      #Env.SacMade,
      #  Env.ClimateChgCause,
      #  Env.ClimateChgReal,
      CharityDonate,
      Alcohol.Intensity,
      PERFECTIONISM,
      Bodysat,
      VENGEFUL.RUMIN,
      community,
      HONESTY_HUMILITY,
      EmotionRegulation1,
      EmotionRegulation2,
      EmotionRegulation3,
      Emp.WorkLifeBalance,
      PermeabilityIndividual,
      ImpermeabilityGroup,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
 dplyr::filter(retired != 1) %>%
 dplyr::filter(retired_lead1 != 1) %>%  #needed for the intervention
  dplyr::filter(semiretired != 1) %>%
  dplyr::filter(semiretired_lead1 != 1) %>%  #needed for the intervention
  #  dplyr::filter(!is.na(Church)) %>%
  #  dplyr::filter(!is.na(Church_lead1)) %>%  #needed for the intervention
  dplyr::select(
    -c(
      Religion.Church,
      EthCat,
      HoursCharity,
      Respect.Self_lead2,
      Household.INC,
      org2018,
      #  not_euro,
      #  not_euro_lead2,
      hold18,
    #  Euro,
      Emp.WorkLifeBalance,
      YearMeasured,
      org2019,
      hold19,
      retired,
      retired_lead1,
      semiretired,
      semiretired_lead1
    )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)

table1::table1( ~ income_log + income_log_lead1, data = i_df)


length(unique(i_df$Id)) # 31280

# inspect data
skim(i_df)

i_df %>%
  group_by(Wave) %>%
  summarise(across(Id, n_distinct))


# glimse
i_df%>%
  summarise(across(c(PWI_lead2, LIFESAT_lead2, LIFEMEANING_lead2), ~mean(.x, na.rm=TRUE), ~sd(.x, na.rm=TRUE), n_distinct()))

#check
skimr::skim(i_df)

# save function
saveh(i_df, "i_df")

# read if needed
i_df<- readh("i_df")



i_df %>%
  summarise(across(c(income_log, income_log_lead2),
                   list(mean = mean, range=range, sd = sd), na.rm = TRUE, .names = "{col}_{fn}"))


# GFS coding
# Church
#[Never, a few times a year, a few times a month, weekly, more than once per week] [BMMRS 34]

# Religious TXTs
#[Never, occasionally, daily, more than daily] [BMMRS 15, modified]

#  Charity
#  In the past month, have you volunteered your time to an organization?2


table1::table1(
  ~ income_log +
    income_log_lead1 +
    Depressed +
    Euro +
    KESSLER6sum +
    HLTH.Fatigue +
    Rumination +
    SFHEALTH  +
    LIFEMEANING +
    LIFESAT +
    PWI +
    SELF.ESTEEM +
    SELF.CONTROL +
    Respect.Self +
    #Emp.WorkLifeBalance +
    Alcohol.Frequency +
    Alcohol.Intensity+
    Hours.Exercise +
    HLTH.BMI +
    Smoker +
    NWI +
    ChildrenNum +
    BELONG +
    SUPPORT +
    CharityDonate+
    Volunteers +
    SexualSatisfaction+
    POWERDEPENDENCE +
  PERFECTIONISM +
  Bodysat +
  VENGEFUL.RUMIN +
  community +
  HONESTY_HUMILITY +
  EmotionRegulation1 +
  EmotionRegulation2 +
  EmotionRegulation3,# +
 # Emp.WorkLifeBalance,
    # +
   # Your.Future.Security +
   # Your.Personal.Relationships +
   # Your.Health +
   # Standard.Living,
  #Env.SacWilling,
  #Env.SacMade,
  data = i_df
)


# missingness in 2020  1/3 of the data are missing

# I do not have enough power or control over important parts of my life.

skimr::skim(i_df)

# mice model  -------------------------------------------------------------
library(mice)

inc_mice <- i_df %>%
  dplyr::select(-c( Wave, Id))
# Visualise missing
library(naniar)
naniar::gg_miss_var(inc_mice)

vis_miss(inc_mice,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(inc_mice)

# qp <- quickpred(inc_mice)  https://stefvanbuuren.name/fimd/sec-toomany.html
# qp
#for_mice$inc_prop <-
#  for_mice$income_log / (for_mice$income_log_lead1 - 1)


ini <- mice(inc_mice, m = 1, maxit = 0)
ini
meth <- ini$meth
#meth
#meth["inc_prop"] <- "~ I(income_log/(income_log_lead1 - 1))"
pred <- ini$pred
pred


# impute
out_idf <- mice::mice(inc_mice,
                  meth = meth,
                  pred = pred,
                  seed = 0,
                  m = 10)

# save
saveh(out_idf, "out_idf")

# read
out_idf <- readh("out_idf")

# https://www.r-bloggers.com/2020/12/iptw-with-missing-data/
# IPTW   see https://stats.stackexchange.com/questions/563057/multiple-imputation-and-inverse-probability-weighting-for-multiple-treatment##
#https://stats.stackexchange.com/questions/563057/multiple-imputation-and-inverse-probability-weighting-for-multiple-treatment



# models <- weightthem(formula, out_idf,
#                      approach = "within",
#                      method = "ps",
#                      estimand = "ATT", ...)
#

outlist2 <-
  row.names(out_idf)[out_idf$outflux < 0.5]
length(outlist2)

head(out_idf$loggedEvents, 10)

#test <- mice.impute.fastpmm(out_for_mice,  donors = 5, type = 1, ridge = 1e-05,)


# try mice ranger
# library(doParallel)
# #cl <- makeCluster(2)
# #registerDoParallel(cl)
# library(miceRanger)
# miceObjPar <- miceRanger(for_mice,
#                          m=10,
#                         # meth = meth,
#                          seed = 0,
#                          pred = out,
#                         # parallel = TRUE,
#                          valueSelector = "meanMatch")
#
# saveh(miceObjPar, "miceObjPar")
#
is.mids(out_idf)
# data warangling
long <- mice::complete(out_idf, "long", inc = TRUE)
# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(long)
#long$LIFESAT_lead1_z <- with(long, scale(LIFESAT_lead1))
min(long$KESSLER6sum, na.rm = TRUE)

min(long$Alcohol.Frequency_lead2, na.rm = TRUE)


# check these vars
hist(long$ImpermeabilityGroup)
hist(long$PermeabilityIndividual)
hist(long$Standard.Living)
hist(long$Your.Future.Security)
hist(long$Your.Future.Security)
hist(long$Your.Health)

# create variables in z score
long2 <- long %>%
  dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0)))%>%
  dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0))%>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0))%>%
  dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0))%>%
  dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1))%>%
  plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0))%>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0))%>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0))%>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2+1))%>%
  dplyr::mutate(Alcohol.Intensity_log_lead2 = log(Alcohol.Intensity_lead2+1))%>%
  dplyr::mutate(Exercise_log_lead2 = log(Hours.Exercise_lead2+1))%>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate+1))%>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity+1))%>%
  dplyr::mutate(Rumination_lead2ord = as.integer(round(Rumination_lead2, digits = 0) + 1)) %>%  # needs to start at 1
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0) )) %>%
  dplyr::mutate(PERFECTIONISM_lead2ord = as.integer(round(PERFECTIONISM_lead2, digits = 0) )) %>%
  dplyr::mutate(VENGEFUL.RUMIN_lead2ord = as.integer(round(VENGEFUL.RUMIN_lead2, digits = 0) )) %>%
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0) )) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(Your.Personal.Relationships_lead2, digits = 0)+1 )) %>%
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0) )) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0)+1 )) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1))%>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0)+1))%>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0) )) %>%
  dplyr::mutate(alcohol_bin2 = if_else( Alcohol.Frequency > 3, 1, 0))%>%
  dplyr::mutate(alcohol_bin = if_else( Alcohol.Frequency > 2, 1, 0))%>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))# Respect for Self is fully missing

table(long2$Your.Personal.Relationships_lead2ord)
# review
# table(long_out_for_mice2$Rumination_lead2ord)
# table(long_out_for_mice2$LIFEMEANING_lead2ord)
# str(long_out_for_mice2$SUPPORT_lead2ord)
# hist(long_out_for_mice2$HLTH.Fatigue_lead2ord)
# hist(long_out_for_mice2$community_lead2)
# str(long_out_for_mice2$alcohol_bin2)


# get colnames
hist(long2$Hours.Exercise_lead2) # get names

#long$attrition <- with(long, ndf$attrition_lead2)
# neect to get into shape
long3 <- long2 %>% mutate_if(is.matrix, as.vector)
out2_inc <- mice::as.mids(long3)
saveh(out2_inc, "out2_inc")
out2_inc <- readh("out2_inc")

# Min                                               Max
# all   0 |---------------------------| 9151771289268548608
#library(SuperLearner)

# iptw --------------------------------------------------------------------

library(MatchThem)
library(optmatch)
models_income <- weightthem(income_log_lead1_z ~
                       income_log_z  +
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
                       NZSEI18_z +
                       Parent_z +
                       Partner_z +
                       PERFECTIONISM_z +
                       PermeabilityIndividual_z +
                       Pol.Orient_z +
                       POWERDEPENDENCE_z + #
                       PWI_z +
                       Relid_z +
                       Respect.Self_z + #
                       Rumination_z + #
                       SELF.CONTROL_z + #
                       SELF.ESTEEM_z + #
                       SexualSatisfaction_z +#
                       SFHEALTH_z +#
                       Smoker_z +#
                       SUPPORT_z +#
                       Urban_z +
                       VENGEFUL.RUMIN_z +
                       Volunteers_z,
                      out2_inc,
                     approach = "within",
                     estimand = "ATE",
                     stabilize = TRUE,
                     method = "optweight")


saveh(models_income,"models_income.rds")


bal.tab(models_income)
sum<- summary(models)
plot(sum)
sum

ctrim <- trim(models, at = .98)
bal.tab(ctrim)
summary(ctrim)


# Weights but now with the PWI  variables individually

library(MatchThem)
library(optmatch)
models_income2 <- weightthem(income_log_lead1_z ~
                              income_log_z  +
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
                           #   income_log_z +
                              ImpermeabilityGroup_z +
                              KESSLER6sum_z + #
                              LIFEMEANING_z + #
                              LIFESAT_z + #
                              Male_z +
                              NZdep_z +
                              NWI_z +
                              NZSEI18_z +
                              Parent_z +
                              Partner_z +
                              PERFECTIONISM_z +
                              PermeabilityIndividual_z +
                              Pol.Orient_z +
                              POWERDEPENDENCE_z + #
                              # PWI_z +
                              Relid_z +
                              Respect.Self_z + #
                              Rumination_z + #
                              SELF.CONTROL_z + #
                              SELF.ESTEEM_z + #
                              SexualSatisfaction_z +#
                              SFHEALTH_z +#
                              Smoker_z +#
                              Standard.Living_z +
                              SUPPORT_z +#
                              Urban_z +
                              VENGEFUL.RUMIN_z +
                              Volunteers_z +
                              Your.Health_z +
                              Your.Future.Security_z +
                              Your.Personal.Relationships_z,
                            out2_inc,
                            approach = "within",
                            estimand = "ATE",
                            stabilize = TRUE,
                            method = "optweight")


saveh(models_income2,"models_income2.rds")


bal.tab(models_income2)
sum<- summary(models_income2)
plot(sum)
sum

ctrim2 <- trim(models_income2, at = .99)
bal.tab(ctrim2)
summary(ctrim2)




## Iptw
HLTH.BMI_lead2_z

out <- with(ctrim, glm( HLTH.BMI_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


## Iptw
HLTH.BMI_lead2_z

out <- with(ctrim, glm( HLTH.BMI_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(SFHEALTH_lead2_z  ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(Hours.Exercise_lead2  ~ income_log_lead1_z, family = "poisson" ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Smoker_lead2 ~ income_log_lead1_z , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp(-3.02-.14)


out <- with(ctrim, glm(HLTH.Fatigue_lead2ord ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <- with(ctrim, glm(Alcohol.Frequency_lead2ord ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(Alcohol.Intensity_lead2~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(Bodysat_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(PWI_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Rumination_lead2ord ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <- with(ctrim, glm(SexualSatisfaction_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(EmotionRegulation1_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(EmotionRegulation2_lead2_z~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( EmotionRegulation3_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(KESSLER6sum_lead2 ~ income_log_lead1_z , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <- with(ctrim, glm(POWERDEPENDENCE_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(PERFECTIONISM_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(SELF.ESTEEM_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( GRATITUDE_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( VENGEFUL.RUMIN_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( LIFEMEANING_lead2ord ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( HONESTY_HUMILITY_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( BELONG_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( SUPPORT_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Volunteers_lead2 ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( CharityDonate_lead2 ~ income_log_lead1_z , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp( 6.867947 + 0.2492043)

out <- with(ctrim, glm(community_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(NWI_lead2_z~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(ImpermeabilityGroup_lead2 ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(ImpermeabilityGroup_lead2 ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(PermeabilityIndividual_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

PermeabilityIndividual_z
### PWI Breakdown

hist(long$ImpermeabilityGroup)
hist(long$PermeabilityIndividual)
hist(long$Standard.Living)
hist(long$Your.Future.Security)
hist(long$Your.Future.Security)
hist(long$Your.Personal.Relationships)

###
out <- with(ctrim2, glm( Standard.Living_lead2ord ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim2, glm( Your.Future.Security_lead2_z ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Your.Health_lead2_z  ~ income_log_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim2, glm( Your.Personal.Relationships_lead2ord ~ income_log_lead1_z ))
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

# create list of data frames

out_c2 <- complete(ctrim2, action ="long", include = FALSE, mild = TRUE)

m <- 10
listdat2<- list()
for (i in 1:m) {
  listdat2[[i]] <- as.data.frame(out_c2[[i]])
}


# enable memory
options(future.globals.maxSize = 8000 * 1024^2)  # needed



# BRMS MODELS forms -------------------------------------------------------------

bf_HLTH.BMI_lead2_z <- bf(HLTH.BMI_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_SFHEALTH_lead2_z <- bf(SFHEALTH_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_Hours.Exercise_lead2 <- bf(Hours.Exercise_lead2 |weights(weights) ~ income_log_lead1_z)
bf_Smoker_lead2 <- bf( Smoker_lead2 |weights(weights) ~ income_log_lead1_z)
bf_HLTH.Fatigue_lead2ord <- bf( HLTH.Fatigue_lead2ord |weights(weights) ~ income_log_lead1_z)
bf_Alcohol.Frequency_lead2ord <- bf( Alcohol.Frequency_lead2ord |weights(weights) ~ income_log_lead1_z)
bf_Alcohol.Intensity_lead2 <- bf( as.integer(Alcohol.Intensity_lead2) |weights(weights) ~ income_log_lead1_z)
bf_Bodysat_lead2_z <- bf( Bodysat_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_PWI_lead2_z <- bf( PWI_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_Rumination_lead2ord <- bf(Rumination_lead2ord |weights(weights) ~ income_log_lead1_z)
bf_SexualSatisfaction_lead2_z <- bf(SexualSatisfaction_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_PWI_lead2_z <- bf(PWI_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_EmotionRegulation1_lead2_z <- bf(EmotionRegulation1_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_EmotionRegulation2_lead2_z <- bf(EmotionRegulation2_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_EmotionRegulation3_lead2_z <- bf(EmotionRegulation3_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_KESSLER6sum_lead2 <- bf(KESSLER6sum_lead2 |weights(weights) ~ income_log_lead1_z)
bf_LIFESAT_lead2ord <- bf(LIFESAT_lead2ord |weights(weights) ~ income_log_lead1_z)
bf_POWERDEPENDENCE_lead2_z <- bf(POWERDEPENDENCE_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_PERFECTIONISM_lead2_z <- bf(PERFECTIONISM_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_SELF.ESTEEM_lead2_z <- bf(SELF.ESTEEM_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_Emp.WorkLifeBalance_lead2_z <- bf( Emp.WorkLifeBalance_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_GRATITUDE_lead2_z <- bf( GRATITUDE_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_VENGEFUL.RUMIN_lead2ord <- bf(VENGEFUL.RUMIN_lead2ord  |weights(weights) ~ income_log_lead1_z)
bf_LIFEMEANING_lead2ord <- bf(LIFEMEANING_lead2ord  |weights(weights) ~ income_log_lead1_z)
bf_HONESTY_HUMILITY_lead2_z <- bf( HONESTY_HUMILITY_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_BELONG_lead2_z <- bf( BELONG_lead2_z  |weights(weights) ~ income_log_lead1_z)
bf_SUPPORT_lead2ord <- bf( SUPPORT_lead2ord |weights(weights) ~ income_log_lead1_z)
bf_Volunteers_lead2 <- bf( Volunteers_lead2 |weights(weights) ~ income_log_lead1_z)
bf_CharityDonate_lead2 <- bf( CharityDonate_lead2 |weights(weights) ~ income_log_lead1_z)
bf_community_lead2_z <- bf(community_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_NWI_lead2_z <- bf(NWI_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_ImpermeabilityGroup_z <- bf(ImpermeabilityGroup_z |weights(weights) ~ income_log_lead1_z)
bf_PermeabilityIndividual_z<- bf( PermeabilityIndividual_z|weights(weights) ~ income_log_lead1_z)


## ADD ONE

bf_Standard.Living_lead2ord<- bf( Standard.Living_lead2ord|weights(weights) ~ income_log_lead1_z)
bf_Your.Health_lead2_z<- bf( Your.Health_lead2_z|weights(weights) ~ income_log_lead1_z)
bf_Your.Future.Security_lead2 <- bf(Your.Future.Security_lead2 |weights(weights) ~ income_log_lead1_z)
bf_Your.Personal.Relationships_lead2ord<- bf( Your.Personal.Relationships_lead2ord|weights(weights) ~ income_log_lead1_z)




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
  file = here::here("mods", "incomemods", "m1_bmi_income.rds"),
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
  file = here::here("mods", "incomemods", "m2_SFHEALTH_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m3_Hours.Exercise_lead2.rds"),
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
  file = here::here("mods", "incomemods", "m4_Smoker_lead2.rds"),
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
  file = here::here("mods", "incomemods", "m5_HLTH.Fatigue_lead2ord.rds"),
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
  file = here::here("mods", "incomemods", "m6_Alcohol.Frequency_lead2ord.rds"),
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
  file = here::here("mods", "incomemods", "m7_Alcohol.Intensity_lead2.rds"),
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
  file = here::here("mods", "incomemods", "m8_Bodysat_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m9_PWI_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m10_Rumination_lead2ord.rds"),
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
  file = here::here("mods", "incomemods", "m11_SexualSatisfaction_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m12_PWI_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m13_EmotionRegulation1_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m14_EmotionRegulation2_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m15_EmotionRegulation3_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m16_KESSLER6sum_lead2.rds"),
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
  file = here::here("mods", "incomemods", "m17_LIFESAT_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m18_POWERDEPENDENCE_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m19_PERFECTIONISM_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m20_SELF.ESTEEM_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m21_Emp.WorkLifeBalance_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m22_GRATITUDE_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m23_VENGEFUL.RUMIN_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m24_LIFEMEANING_lead2ord"),
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
  file = here::here("mods", "incomemods", "m25_HONESTY_HUMILITY_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m26_BELONG_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m27_SUPPORT_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m28_Volunteers_lead2.rds"),
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
  file = here::here("mods", "incomemods", "m28_CharityDonate_lead2.rds"),
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
  file = here::here("mods", "incomemods", "m29_community_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m30_NWI_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m31_ImpermeabilityGroup_z.rds"),
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
  file = here::here("mods", "incomemods", "m32_PermeabilityIndividual_z.rds"),
)

## try GEE

library(geepack)

out3 <- with(ctrim, geeglm(
  PWI_lead2_z  ~ income_log_lead1_z,
  data = cmodels,
  id = 1:nrow(cmodels),
  family = gaussian))

# same result
output <- pool(out3)
summary(output, conf.int = TRUE)
plot(output)


# brms pwi follow up ------------------------------------------------------

bf_Standard.Living_lead2_z<- bf( Standard.Living_lead2_z|weights(weights) ~ income_log_lead1_z)
bf_Your.Health_lead2_z<- bf( Your.Health_lead2_z|weights(weights) ~ income_log_lead1_z)
bf_Your.Future.Security_lead2_z <- bf(Your.Future.Security_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_Your.Personal.Relationships_lead2ord<- bf( Your.Personal.Relationships_lead2ord|weights(weights) ~ income_log_lead1_z)


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
  file = here::here("mods", "incomemods", "m33_Standard.Living_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m34_Your.Health_lead2_z.rds"),
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
  file = here::here("mods", "incomemods", "m35_Your.Future.Security_lead2_z.rds"),
)


m36_Your.Personal.Relationships_lead2ord <- brm_multiple(
  bf_Your.Personal.Relationships_lead2ord,
  data = listdat2,
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


# brms-evalues ------------------------------------------------------------




# brms-plots --------------------------------------------------------------

m1_out <- model_parameters(m1_bmi_income, dispersion = TRUE, centrality = "mean", test = "pd", digits = 5, diagnostic =  NULL,
                           rope_range = NULL)%>% slice(2)



predict
m2_out <- model_parameters( m2__SFHEALTH_lead2_z
, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m3_out <- model_parameters( m3_Hours.Exercise_lead2
, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m4_out <- model_parameters(m4_Smoker_lead2
 , dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m5_out <- model_parameters( m5_HLTH.Fatigue_lead2ord
, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m6_out <- model_parameters(m6_Alcohol.Frequency_lead2ord
 , dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m7_out <- model_parameters(m7_Alcohol.Intensity_lead2
 , dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m8_out <- model_parameters( m8_Bodysat_lead2_z
, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m9_out <- model_parameters( m9_PWI_lead2_z, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m10_out <- model_parameters( m10_Rumination_lead2ord
, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m11_out <- model_parameters( m11_SexualSatisfaction_lead2_z
, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m12_out <- model_parameters( m12_PWI_lead2_z
, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m13_out <- model_parameters( m13_EmotionRegulation1_lead2_z
 , dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m14_out <- model_parameters( m14_EmotionRegulation2_lead2_z
, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m15_out <- model_parameters( m15_EmotionRegulation3_lead2_z, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m16_out <- model_parameters( m16_KESSLER6sum_lead2
, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m17_out <- model_parameters(m17_LIFESAT_lead2ord  , dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m18_out <- model_parameters(m18_POWERDEPENDENCE_lead2_z  , dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m19_out <- model_parameters(m19_PERFECTIONISM_lead2_z  , dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m20_out <- model_parameters( m20_SELF.ESTEEM_lead2_z
, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m21_out <- model_parameters( m21_Emp.WorkLifeBalance_lead2_z
, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m22_out <- model_parameters(m22_GRATITUDE_lead2_z
 , dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m23_out <- model_parameters( m23_VENGEFUL.RUMIN_lead2ord
, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m24_out <- model_parameters( m24_LIFEMEANING_lead2ord, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m25_out <- model_parameters( m25_HONESTY_HUMILITY_lead2_z, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m26_out <- model_parameters( m26_BELONG_lead2_z, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m27_out <- model_parameters( m27_SUPPORT_lead2_z, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m28_out <- model_parameters( m28_Volunteers_lead2, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m29_out <- model_parameters( m29_community_lead2_z, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m30_out <- model_parameters( m30_NWI_lead2_z, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m31_out <- model_parameters( m31_ImpermeabilityGroup_z
, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m32_out <- model_parameters( m32_PermeabilityIndividual_z , dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)


bf_Standard.Living_lead2_z<- bf( Standard.Living_lead2_z|weights(weights) ~ income_log_lead1_z)
bf_Your.Health_lead2_z<- bf( Your.Health_lead2_z|weights(weights) ~ income_log_lead1_z)
bf_Your.Future.Security_lead2_z <- bf(Your.Future.Security_lead2_z |weights(weights) ~ income_log_lead1_z)
bf_Your.Personal.Relationships_lead2ord<- bf( Your.Personal.Relationships_lead2ord|weights(weights) ~ income_log_lead1_z)


m33_out <- model_parameters( m33_Standard.Living_lead2_z , dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m34_out <- model_parameters(  m34_Your.Health_lead2_z, dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m35_out <- model_parameters( m35_Your.Future.Security_lead2_z , dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)
m36_out <- model_parameters( m36_Your.Personal.Relationships_lead2ord , dispersion = TRUE, centrality = "mean",  test = "pd", digits = 5, diagnostic =  NULL,  rope_range = NULL)%>% slice(2)



# evalue
round( EValue::evalues.OLS(    , se =   , sd = 1, delta = 1, true = 0), 3)
# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

# graph
m11_PWI_income_t_plot <-
  plot(conditional_effects(
    m11_PWI_income_t,
    "income_log_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m11_PWI_income_t_plot_z  <-  m11_PWI_income_t_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Personal Well-Being",
       y = "Personal Well-Being (sd)",
       x = "Income (sd)")

m11_PWI_income_t_plot_z
ggsave(
  m11_PWI_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m11_PWI_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)


## Compare with stratification
form ~ income_log  + AGREEABLENESS_z + CONSCIENTIOUSNESS_z +  EXTRAVERSION_z  +   HONESTY_HUMILITY_z + NEUROTICISM_z +OPENNESS_z + Age_z +Alcohol.Frequency_z + Alcohol.Intensity_log_z + Bodysat_z +  Believe.God_z +Believe.Spirit_z +BELONG_z + CharityDonate_log_z +  ChildrenNum_z + Church_z +community +Edu_z +  Employed_z + EmotionRegulation1_z + EmotionRegulation2_z +  EmotionRegulation3_z + Euro_z + GRATITUDE_z +HomeOwner_z +Hours.Exercise_log_z +Hours.Work_z + HLTH.BMI_z  + HLTH.Fatigue_z +  income_log_z + ImpermeabilityGroup_z +KESSLER6sum_z +  LIFEMEANING_z +  LIFESAT_z + Male_z +NZdep_z + NWI_z + NZSEI18_z + Parent_z +   Partner_z + PERFECTIONISM_z +  PermeabilityIndividual_z + Pol.Orient_z + POWERDEPENDENCE_z +  PWI_z +Relid_z + Respect.Self_z +   Rumination_z +   SELF.CONTROL_z +  SELF.ESTEEM_z + SexualSatisfaction_z +SFHEALTH_z +  Smoker_z +  SUPPORT_z + Urban_z + VENGEFUL.RUMIN_z +  Volunteers_z

mform <- update.formula(form, PWI_lead2_z ~ .)

covariates

out2 <- with(out2_inc, lm( mform  ))



## for logistic models
newdata = data.frame(
  n = 1,
  VENGEFUL.RUMIN_lead1_z = c(-1, 1),
  transform = TRUE,
  re_formula = NA)



# HLTH.BMI_lead2_z ---------------------------------------------------------
options(future.globals.maxSize = 8000 * 1024^2)  # needed

m1_bmi_income <- brm_multiple(
  HLTH.BMI_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init= 0,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m1_bmi_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
# looks good
pp_check(m1_bmi_forgive)


out <- model_parameters(m1_bmi_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)



# evalue
round( EValue::evalues.OLS( -0.00320 , se = 0.00493, sd = 1, delta = 2, true = 0), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m1_bmi_forgive)
m1_bayes_table
plot(m1_bayes_table)

# graph
m1_bmi_forgive_plot <-
  plot(conditional_effects(
    m1_bmi_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m1_bmi_forgive_plot_z  <-  m1_bmi_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "BMI",
       # subtitle = "Loss shows greater distress",
       y = "BMI (sd)",
       x = "Un-Forgiving (sd)")

m1_bmi_forgive_plot_z

ggsave(
  m1_bmi_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m1_bmi_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# SFHEALTH_lead2_z --------------------------------------------------------------


m2_sfhealth_forgive <- brm_multiple(
  SFHEALTH_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  init = 0,
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods",  "m2_sfhealth_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

out <- model_parameters(m2_sfhealth_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out


# Evalue
round( EValue::evalues.OLS(  -0.03758 , se = 0.00634, sd = 1, delta = 2, true = 0), 3)



## posterior predictive checks
pp_check(m2_sfhealth_forgive)

#table
lazerhawk::brms_SummaryTable(m2_sfhealth_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m2_sfhealth_forgive)
m1_bayes_table
plot(m1_bayes_table)

# graph
m2_sfhealth_forgive_plot <-
  plot(conditional_effects(
    m2_sfhealth_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m2_sfhealth_forgive_plot_z  <-  m2_sfhealth_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5))+ scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Short Form Health",
       # subtitle = "Loss shows greater distress",
       y = "Short Form Health (sd)",
       x = "Un-Forgiving (sd)")

m2_sfhealth_forgive_plot_z

ggsave(
  m2_sfhealth_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m2_sfhealth_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# Hours.Exercise_log_lead2z -----------------------------------------------
m3_exercise_forgive <- brm_multiple(
  Hours.Exercise_lead2_log_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  init = 0,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m3_exercise_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

m3a_exercise_forgive <- brm_multiple(
  Hours.Exercise_lead2   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "negbinomial",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m3a_exercise_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)


##  BAD
pp_check(m3a_exercise_forgive)



## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])

# model coeff
out <- model_parameters(m3a_exercise_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

log(7.443313)
(exp( 0.00784+0.00696))

sd(l2$Hours.Exercise_lead2, na.rm = TRUE)


# evalue
round( EValue::evalues.OLS( -0.00466, se = 0.00692, sd = 7.443313, delta = 2, true = 0), 3)


# evalue for RR
round( EValue::evalues.RR(1.007871, lo =  1.00088, hi = 1.01491, true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m3a_exercise_forgive_plot <-
  plot(conditional_effects(
    m3a_exercise_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m3a_exercise_forgive_plot_z  <-  m3a_exercise_forgive_plot[[1]]  +
  # scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Exercise",
       # subtitle = "Loss shows greater distress",
       y = "Exercise (weekly hours)",
       x = "Un-Forgiving (sd)")


m3a_exercise_forgive_plot_z
ggsave(
  m3a_exercise_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m3a_exercise_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)




# Smoker_bz ---------------------------------------------------------------

m4_smoker_forgive <- brm_multiple(
  Smoker_lead2   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = bernoulli(link = "cloglog"),
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init=0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m4_smoker_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)


##
pp_check(m4_smoker_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    m4_smoker_forgive,
    newdata = newdata,
  )
)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  , se = , sd = 1, delta = 2, true = 0), 3)

1.01704-0.01661302
1.01704+0.01661302
# evalue for RR
round( EValue::evalues.RR( 1.01704, lo =  1.000427, hi = 1.033653, true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m4_smoker_forgive)
m1_bayes_table
plot(m1_bayes_table)

# graph
m4_smoker_forgive_plot <-
  plot(conditional_effects(
    m4_smoker_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m4_smoker_forgive_plot_z  <-  m4_smoker_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(0, .3)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Smoker",
       # subtitle = "Loss shows greater distress",
       y = "Smoker (y=1/n=0)",
       x = "Un-Forgiving (sd)")

m4_smoker_forgive_plot_z
ggsave(
  m4_smoker_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m4_smoker_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

m4_smoker_forgive_plot_z

# health-fatigue ----------------------------------------------------------

m5_fatigue_forgive <- brm_multiple(
  HLTH.Fatigue_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  init =0,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m5_fatigue_forgive"),
  set_prior('normal(0, 1)', class = 'b')
)


# NOTE NAME ISSUE
m5a_fatigue_forgive <- brm_multiple(
  HLTH.Fatigue_lead2ord ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = cumulative("probit"),
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m5a_fatigue_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

options(scipen=999)

##
pp_check(m5_fatigue_forgive)

pp_check(m5a_fatigue_forgive)


## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
pp_check(m5_fatigue_forgive)

out <- model_parameters(m5_fatigue_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out
out <- model_parameters(m5a_fatigue_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(5)
out
sd(l2$HLTH.Fatigue_lead2ord, na.rm=TRUE)
# evalue
round( EValue::evalues.OLS(0.05257 , se = 0.00857, sd = 1.049608, delta = 2, true = 0), 3)

# standardised
round( EValue::evalues.OLS( 0.04221 , se = 0.00687, sd = 1, delta = 2, true = 0), 3)


# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m5a_fatigue_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m5a_fatigue_forgive_plot <-
  plot(conditional_effects(
    m5a_fatigue_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m5a_fatigue_forgive_plot_z  <-  m5a_fatigue_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(2, 3)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Fatigue",
       # subtitle = "Loss shows greater distress",
       y = "Fatigue (1-5)",
       x = "Un-Forgiving (sd)")

m5a_fatigue_forgive_plot_z
ggsave(
  m5a_fatigue_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m5a_fatigue_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)


# Alcohol.Frequency_lead2_z , ---------------------------------------------------

m6_AlcoholFrequency_forgive <-
  brm_multiple(
    Alcohol.Frequency_lead2_z   ~
      VENGEFUL.RUMIN_lead1_z +
      VENGEFUL.RUMIN  +
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
      EmotionRegulation_z +
      Euro_z +
      GRATITUDE_z +
      HomeOwner_z +
      Hours.Exercise_log_z +
      Hours.Work_z +
      HLTH.BMI_z  + #
      HLTH.Fatigue_z + #
      income_log_z +
      KESSLER6sum_z + #
      LIFEMEANING_z + #
      LIFESAT_z + #
      lost_job_z +
      Male_z +
      NZdep_z +
      NWI_z +
      Parent_z +
      Partner_z +
      PERFECTIONISM_z +
      Pol.Orient_z +
      POWERDEPENDENCE_z + #
      PWI_z +
      Relid_z +
      Respect.Self_z + #
      Rumination_z + #
      SELF.CONTROL_z + #
      SELF.ESTEEM_z + #
      SexualSatisfaction_z +#
      SFHEALTH_z +#
      Smoker_z +#
      SUPPORT_z +#
      Urban_z +
      Volunteers_z,
    family = "gaussian",
    data = out2_for,
    seed = 1234,
    warmup = 1000,
    init =0,
    iter = 2000,
    chains = 4,
    backend = "cmdstanr",
    file = here::here("mods", "forgivemods", "m6_AlcoholFrequency_forgive.rds"),
    set_prior('normal(0, 1)', class = 'b')
  )

m6a_AlcoholFrequency_forgive <-
  brm_multiple(
    Alcohol.Frequency_lead2ord   ~
      VENGEFUL.RUMIN_lead1_z +
      VENGEFUL.RUMIN  +
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
      EmotionRegulation_z +
      Euro_z +
      GRATITUDE_z +
      HomeOwner_z +
      Hours.Exercise_log_z +
      Hours.Work_z +
      HLTH.BMI_z  + #
      HLTH.Fatigue_z + #
      income_log_z +
      KESSLER6sum_z + #
      LIFEMEANING_z + #
      LIFESAT_z + #
      lost_job_z +
      Male_z +
      NZdep_z +
      NWI_z +
      Parent_z +
      Partner_z +
      PERFECTIONISM_z +
      Pol.Orient_z +
      POWERDEPENDENCE_z + #
      PWI_z +
      Relid_z +
      Respect.Self_z + #
      Rumination_z + #
      SELF.CONTROL_z + #
      SELF.ESTEEM_z + #
      SexualSatisfaction_z +#
      SFHEALTH_z +#
      Smoker_z +#
      SUPPORT_z +#
      Urban_z +
      Volunteers_z,
    family = cumulative("probit"),
    data = out2_for,
    seed = 1234,
    warmup = 1000,
    init =0,
    iter = 2000,
    chains = 4,
    backend = "cmdstanr",
    file = here::here("mods", "forgivemods", "m6a_AlcoholFrequency_forgive.rds"),
    set_prior('normal(0, 1)', class = 'b')
  )


## this works
pp_check(m6a_AlcoholFrequency_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
sd(l2$Alcohol.Frequency_lead2ord, na.rm=TRUE)

outa <- model_parameters(m6_AlcoholFrequency_forgive, dispersion = TRUE, centrality = "mean",
                         test = "pd",
                         digits = 5,
                         diagnostic =  NULL,
                         rope_range = NULL)%>% slice(2)
outa
out <- model_parameters(m6a_AlcoholFrequency_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(6)
out

# evalue
# standard
round( EValue::evalues.OLS(  0.01483, se = 0.00562, sd = 1, delta = 2, true = 0), 3)

round( EValue::evalues.OLS(  0.02323, se = 0.00851, sd = 1.358344, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m6a_AlcoholFrequency_forgive_plot <-
  plot(conditional_effects(
    m6a_AlcoholFrequency_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    categorical = F,
    spaghetti = T
  ))

m6a_AlcoholFrequency_forgive_plot_z  <-  m6a_AlcoholFrequency_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(3, 3.5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Alcohol Frequency",
       # subtitle = "Loss shows greater distress",
       y = " Alcohol Frequency (ordinal 1-6)",
       x = "Un-Forgiving (sd)")

m6a_AlcoholFrequency_forgive_plot_z
ggsave(
  m6a_AlcoholFrequency_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m6a_AlcoholFrequency_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)




#  Alcohol.Intensity_logz -------------------------------------------------

m7_AlcoholIntensity_forgive <-
  brm_multiple(
    bf(Alcohol.Intensity_lead2_z   ~
         VENGEFUL.RUMIN_lead1_z +
         VENGEFUL.RUMIN  +
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
         EmotionRegulation_z +
         Euro_z +
         GRATITUDE_z +
         HomeOwner_z +
         Hours.Exercise_log_z +
         Hours.Work_z +
         HLTH.BMI_z  + #
         HLTH.Fatigue_z + #
         income_log_z +
         KESSLER6sum_z + #
         LIFEMEANING_z + #
         LIFESAT_z + #
         lost_job_z +
         Male_z +
         NZdep_z +
         NWI_z +
         Parent_z +
         Partner_z +
         PERFECTIONISM_z +
         Pol.Orient_z +
         POWERDEPENDENCE_z + #
         PWI_z +
         Relid_z +
         Respect.Self_z + #
         Rumination_z + #
         SELF.CONTROL_z + #
         SELF.ESTEEM_z + #
         SexualSatisfaction_z +#
         SFHEALTH_z +#
         Smoker_z +#
         SUPPORT_z +#
         Urban_z +
         Volunteers_z),
    family = "gaussian",
    data = out2_for,
    seed = 1234,
    warmup = 1000,
    iter = 2000,
    init = 0,
    chains = 4,
    backend = "cmdstanr",
    file = here::here("mods", "forgivemods", "m7_AlcoholIntensity_forgive.rds"),
    set_prior('normal(0, 1)', class = 'b')
  )


m7a_AlcoholIntensity_forgive <-
  brm_multiple(
    as.integer(Alcohol.Intensity_lead2)   ~
      VENGEFUL.RUMIN_lead1_z +
      VENGEFUL.RUMIN  +
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
      EmotionRegulation_z +
      Euro_z +
      GRATITUDE_z +
      HomeOwner_z +
      Hours.Exercise_log_z +
      Hours.Work_z +
      HLTH.BMI_z  + #
      HLTH.Fatigue_z + #
      income_log_z +
      KESSLER6sum_z + #
      LIFEMEANING_z + #
      LIFESAT_z + #
      lost_job_z +
      Male_z +
      NZdep_z +
      NWI_z +
      Parent_z +
      Partner_z +
      PERFECTIONISM_z +
      Pol.Orient_z +
      POWERDEPENDENCE_z + #
      PWI_z +
      Relid_z +
      Respect.Self_z + #
      Rumination_z + #
      SELF.CONTROL_z + #
      SELF.ESTEEM_z + #
      SexualSatisfaction_z +#
      SFHEALTH_z +#
      Smoker_z +#
      SUPPORT_z +#
      Urban_z +
      Volunteers_z,
    family = "negbinomial",
    data = out2_for,
    seed = 1234,
    warmup = 1000,
    init = 0,
    iter = 2000,
    chains = 4,
    backend = "cmdstanr",
    file = here::here("mods", "forgivemods", "m7a_AlcoholIntensity_forgive"),
    set_prior('normal(0, 1)', class = 'b')
  )

sd(l2$Alcohol.Intensity_lead2, na.rm=TRUE)

##
pp_check(m7a_AlcoholIntensity_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m7a_AlcoholIntensity_forgive,
                        dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out
sd(l2$Alcohol.Intensity_lead2, na.rm=TRUE)

# evalue
#round( EValue::evalues.OLS(  0.01685, se = 0.00576, sd = 1.990131, delta = 2, true = 0), 3)


# Use
# evalue for RR
round( EValue::evalues.RR(1.016993, lo =  1.011152, hi = 1.022868, true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m7a_AlcoholIntensity_forgive_plot <-
  plot(conditional_effects(
    m7a_AlcoholIntensity_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m7a_AlcoholIntensity_forgive_plot_z  <-  m7a_AlcoholIntensity_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(1.5, 2)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Alcohol Intensity",
       # subtitle = "Loss shows greater distress",
       y = " (Alcohol Intensity (drinks per session)",
       x = "Un-Forgiving (sd)")

m7a_AlcoholIntensity_forgive_plot_z
ggsave(
  m7a_AlcoholIntensity_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m7a_AlcoholIntensity_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# bodysat  ----------------------------------------------------------------


m8_bodysat_forgive <- brm_multiple(
  Bodysat_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  init = 0,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m8_bodysat_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)


m8a_bodysat_forgive <- brm_multiple(
  Bodysat_lead2   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  init = 0,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m8a_bodysat_forgive"),
  set_prior('normal(0, 1)', class = 'b')
)

##
pp_check(m8_bodysat_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m8_bodysat_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.02824 , se = 0.00628, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m8_bodysat_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m8a_bodysat_forgive_plot <-
  plot(conditional_effects(
    m8a_bodysat_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m8_bodysat_forgive_plot_z  <-  m8_bodysat_forgive[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Body Satisfaction",
       # subtitle = "Loss shows greater distress",
       y = "Body Satisfaction (sd)",
       x = "Un-Forgiving (sd)")

m8_bodysat_forgive_plot_z

ggsave(
  m8_bodysat_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m8_bodysat_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# rumination --------------------------------------------------------------

m9_rumination_forgive <- brms::brm_multiple(
  Rumination_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  init = 0,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m9_rumination_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)


m9a_rumination_forgive <- brms::brm_multiple(
  Rumination_lead2ord ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = cumulative("probit"),
  data = out2_for,
  init = 0,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m9a_rumination_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

##
pp_check(m9_rumination_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m9a_rumination_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(5)
out
sd(l2$Rumination_lead2ord , na.rm = T)

# evalue
round( EValue::evalues.OLS(0.14326  , se = 0.00857, sd = 0.9525767, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m9a_rumination_forgive_plot <-
  plot(conditional_effects(
    m9a_rumination_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m9a_rumination_forgive_plot_z  <-  m9a_rumination_forgive_plot[[1]]  +
  # scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Rumination",
       # subtitle = "Loss shows greater distress",
       y = "Forgiveness(sd)",
       x = "Un-Forgiving (sd)")

m9a_rumination_forgive_plot_z
ggsave(
  m9a_rumination_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m9a_rumination_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# SexualSatisfaction_z ----------------------------------------------------

m10_SexualSatisfaction_forgive  <- brm_multiple(
  SexualSatisfaction_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m10_SexualSatisfaction_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

##
pp_check()

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m10_SexualSatisfaction_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS( -0.04229 , se = 0.00703, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m10_SexualSatisfaction_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m10_SexualSatisfaction_forgive_plot <-
  plot(conditional_effects(
    m10_SexualSatisfaction_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m10_SexualSatisfaction_forgive_plot_z  <-  m10_SexualSatisfaction_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Sexual Satisfaction",
       # subtitle = "Loss shows greater distress",
       y = "Sexual Satisfaction (sd)",
       x = "Un-Forgiving (sd)")

m10_SexualSatisfaction_forgive_plot_z

ggsave(
  m10_SexualSatisfaction_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m10_SexualSatisfaction_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# PWI_z -------------------------------------------------------------------


# model coeff
out <- model_parameters(m11_PWI_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.03350 , se =  0.00628, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m11_PWI_forgive_plot <-
  plot(conditional_effects(
    m11_PWI_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m11_PWI_forgive_plot_z  <-  m11_PWI_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Personal Well-Being",
       # subtitle = "Loss shows greater distress",
       y = "Personal Well-Being (sd)",
       x = "Un-Forgiving (sd)")

m11_PWI_forgive_plot_z
ggsave(
  m11_PWI_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m11_PWI_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# emotion regulation ------------------------------------------------------

m12_EmotionRegulation_forgive <- brm_multiple(
  EmotionRegulation1_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init =0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m12_EmotionRegulation_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check()

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m12_EmotionRegulation_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS( 0.00609  , se = 0.00720, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m12_EmotionRegulation_forgive_plot <-
  plot(conditional_effects(
    m12_EmotionRegulation_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m12_EmotionRegulation_forgive_plot_z  <-  m12_EmotionRegulation_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Emotional Regulation",
       # subtitle = "Loss shows greater distress",
       y = "Emotional Regulation (sd)",
       x = "Un-Forgiving (sd)")

m12_EmotionRegulation_forgive_plot_z

ggsave(
  m12_EmotionRegulation_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m12_EmotionRegulation_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# k6 ----------------------------------------------------------------------

# need to increase memory
options(future.globals.maxSize = 8000 * 1024^2)

m13_KESSLER6sum_forgive <- brm_multiple(
  KESSLER6sum_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m13_KESSLER6sum_forgive.rds")#,
  # set_prior('normal(0, 1)', class = 'b')
)

m13a_KESSLER6sum_forgive <- brm_multiple(
  as.integer(KESSLER6sum_lead2) ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "negbinomial",
  data = out2_for,
  seed = 1234,
  init = 0,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods",  "m13a_KESSLER6sum_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check()

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m13_KESSLER6sum_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS( 0.08044 , se = 0.00608, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m13_KESSLER6sum_forgive_plot <-
  plot(conditional_effects(
    m13_KESSLER6sum_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m13_KESSLER6sum_forgive_plot_z  <-  m13_KESSLER6sum_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Kessler-6 Distress",
       # subtitle = "Loss shows greater distress",
       y = "Kessler-6 Distress (0-24)",
       x = "Un-Forgiving (sd)")
m13_KESSLER6sum_forgive_plot_z


ggsave(
  m13_KESSLER6sum_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m13_KESSLER6sum_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# test with

test_evalue <- with(out_ch,
                    lm(KESSLER6sum_lead2_z ~
                         VENGEFUL.RUMIN_lead1_z +
                         VENGEFUL.RUMIN  +
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
                         EmotionRegulation_z +
                         Euro_z +
                         GRATITUDE_z +
                         HomeOwner_z +
                         Hours.Exercise_log_z +
                         Hours.Work_z +
                         HLTH.BMI_z  + #
                         HLTH.Fatigue_z + #
                         income_log_z +
                         KESSLER6sum_z + #
                         LIFEMEANING_z + #
                         LIFESAT_z + #
                         lost_job_z +
                         Male_z +
                         NZdep_z +
                         NWI_z +
                         Parent_z +
                         Partner_z +
                         PERFECTIONISM_z +
                         Pol.Orient_z +
                         POWERDEPENDENCE_z + #
                         PWI_z +
                         Relid_z +
                         Respect.Self_z + #
                         Rumination_z + #
                         SELF.CONTROL_z + #
                         SELF.ESTEEM_z + #
                         SexualSatisfaction_z +#
                         SFHEALTH_z +#
                         Smoker_z +#
                         SUPPORT_z +#
                         Urban_z +
                         Volunteers_z))

summary(test_evalue)


##
pp_check()

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  , se = , sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m13a_KESSLER6sum_forgive_plot <-
  plot(conditional_effects(
    m13a_KESSLER6sum_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m13a_KESSLER6sum_forgive_plot_z  <-  m13a_KESSLER6sum_forgive_plot[[1]]  +
  # scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "",
       # subtitle = "Loss shows greater distress",
       y = " (sd)",
       x = "Un-Forgiving (0-24)")

ggsave(
  m13a_KESSLER6sum_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m13a_KESSLER6sum_forgive_plot_z",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)


# LIFESAT_z ---------------------------------------------------------------


m14_LIFESAT_forgive  <- brms::brm_multiple(
  LIFESAT_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  init =0,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m14_LIFESAT_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

##
pp_check(m14_LIFESAT_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m14_LIFESAT_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.05360, se = 0.00632 , sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m14_LIFESAT_forgive_plot <-
  plot(conditional_effects(
    m14_LIFESAT_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m14_LIFESAT_forgive_plot_z  <-  m14_LIFESAT_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Life Satisfaction",
       # subtitle = "Loss shows greater distress",
       y = "Life Satisfaction (sd)",
       x = "Un-Forgiving (sd)")
m14_LIFESAT_forgive_plot_z
ggsave(
  m14_LIFESAT_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m14_LIFESAT_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)


# POWERDEPENDENCE_z -------------------------------------------------------

m15_POWERDEPENDENCE_forgive <- brms::brm_multiple(
  POWERDEPENDENCE_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m15_POWERDEPENDENCE_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m15_POWERDEPENDENCE_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m15_POWERDEPENDENCE_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(0.01457  , se = 0.00664, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m15_POWERDEPENDENCE_forgive_plot <-
  plot(conditional_effects(
    m15_POWERDEPENDENCE_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m15_POWERDEPENDENCE_forgive_plot_z  <-  m15_POWERDEPENDENCE_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Power Dependence",
       # subtitle = "Loss shows greater distress",
       y = "Power Dependence(sd)",
       x = "Un-Forgiving (sd)")

m15_POWERDEPENDENCE_forgive_plot_z
ggsave(
  m15_POWERDEPENDENCE_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m15_POWERDEPENDENCE_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# perfectionism -----------------------------------------------------------

m16_PERFECTIONISM_forgive <- brm_multiple(
  PERFECTIONISM_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init =0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m16_PERFECTIONISM_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m16_PERFECTIONISM_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m16_PERFECTIONISM_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  0.05948 , se =  0.00629, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m16_PERFECTIONISM_forgive_plot <-
  plot(conditional_effects(
    m16_PERFECTIONISM_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m16_PERFECTIONISM_forgive_plot_z  <-  m16_PERFECTIONISM_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Perfectionism",
       # subtitle = "Loss shows greater distress",
       y = "Perfectionism (sd)",
       x = "Un-Forgiving (sd)")

m16_PERFECTIONISM_forgive_plot_z

ggsave(
  m16_PERFECTIONISM_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m16_PERFECTIONISM_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# SELF.ESTEEM_lead2_z -----------------------------------------------------------

m17_SELFESTEEM_forgive <- brm_multiple(
  SELF.ESTEEM_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m17_SELFESTEEM_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m17_SELFESTEEM_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m17_SELFESTEEM_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.06856 , se =  0.00582 , sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m17_SELFESTEEM_forgive_plot <-
  plot(conditional_effects(
    m17_SELFESTEEM_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m17_SELFESTEEM_forgive_plot_z  <-  m17_SELFESTEEM_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Self Esteem",
       # subtitle = "Loss shows greater distress",
       y = "Self Esteem (sd)",
       x = "Un-Forgiving (sd)")

m17_SELFESTEEM_forgive_plot_z

ggsave(
  m17_SELFESTEEM_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m17_SELFESTEEM_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# Emp.WorkLifeBalance_lead2_z ---------------------------------------------------------------

m18_EmpWorkLifeBalance_forgive <-
  brm_multiple(
    Emp.WorkLifeBalance_lead2_z ~
      VENGEFUL.RUMIN_lead1_z +
      VENGEFUL.RUMIN  +
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
      EmotionRegulation_z +
      Euro_z +
      GRATITUDE_z +
      HomeOwner_z +
      Hours.Exercise_log_z +
      Hours.Work_z +
      HLTH.BMI_z  + #
      HLTH.Fatigue_z + #
      income_log_z +
      KESSLER6sum_z + #
      LIFEMEANING_z + #
      LIFESAT_z + #
      lost_job_z +
      Male_z +
      NZdep_z +
      NWI_z +
      Parent_z +
      Partner_z +
      PERFECTIONISM_z +
      Pol.Orient_z +
      POWERDEPENDENCE_z + #
      PWI_z +
      Relid_z +
      Respect.Self_z + #
      Rumination_z + #
      SELF.CONTROL_z + #
      SELF.ESTEEM_z + #
      SexualSatisfaction_z +#
      SFHEALTH_z +#
      Smoker_z +#
      SUPPORT_z +#
      Urban_z +
      Volunteers_z,
    family = "gaussian",
    data = out2_for,
    seed = 1234,
    init = 0,
    warmup = 1000,
    iter = 2000,
    chains = 4,
    backend = "cmdstanr",
    file = here::here("mods", "forgivemods", "m18_EmpWorkLifeBalance_forgive.rds"),
    set_prior('normal(0, 1)', class = 'b')
  )
##
pp_check(m18_EmpWorkLifeBalance_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m18_EmpWorkLifeBalance_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.03079 , se =  0.00793 , sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m18_EmpWorkLifeBalance_forgive_plot <-
  plot(conditional_effects(
    m18_EmpWorkLifeBalance_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m18_EmpWorkLifeBalance_forgive_plot_z  <-  m18_EmpWorkLifeBalance_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Work/Life Balance",
       # subtitle = "Loss shows greater distress",
       y = "Work/Life Balance (sd)",
       x = "Un-Forgiving (sd)")

m18_EmpWorkLifeBalance_forgive_plot_z

ggsave(
  m18_EmpWorkLifeBalance_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m18_EmpWorkLifeBalance_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# gratitude ---------------------------------------------------------------

m19_GRATITUDE_forgive <- brm_multiple(
  GRATITUDE_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init =0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m17_GRATITUDE_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m19_GRATITUDE_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m19_GRATITUDE_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.07172, se =  0.00671, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m19_GRATITUDE_forgive_plot <-
  plot(conditional_effects(
    m19_GRATITUDE_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m19_GRATITUDE_forgive_plot_z  <-  m19_GRATITUDE_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Gratitude",
       # subtitle = "Loss shows greater distress",
       y = "Gratitude (sd)",
       x = "Un-Forgiving (sd)")

m19_GRATITUDE_forgive_plot_z

ggsave(
  m19_GRATITUDE_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m19_GRATITUDE_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

#
# # vengful rumination -------------------------------------------------------
#
# m20_VENGEFULRUMIN_forgive <- brm_multiple(
#   VENGEFUL.RUMIN_lead2_z   ~
#     VENGEFUL.RUMIN_lead1_z +
#     VENGEFUL.RUMIN  +
#     AGREEABLENESS_z +
#     CONSCIENTIOUSNESS_z +
#     EXTRAVERSION_z  +
#     HONESTY_HUMILITY_z +
#     NEUROTICISM_z +
#     OPENNESS_z +
#     Age_z +
#     Alcohol.Frequency_z + #
#     Alcohol.Intensity_log_z + #
#     Bodysat_z +
#     Believe.God_z +
#     Believe.Spirit_z +
#     BELONG_z + #
#     CharityDonate_log_z + #
#     ChildrenNum_z +
#     Church_z +
#     community +
#     Edu_z +
#     Employed_z +
#     EmotionRegulation_z +
#     Euro_z +
#     GRATITUDE_z +
#     HomeOwner_z +
#     Hours.Exercise_log_z +
#     Hours.Work_z +
#     HLTH.BMI_z  + #
#     HLTH.Fatigue_z + #
#     income_log_z +
#     KESSLER6sum_z + #
#     LIFEMEANING_z + #
#     LIFESAT_z + #
#     lost_job_z +
#     Male_z +
#     NZdep_z +
#     NWI_z +
#     Parent_z +
#     Partner_z +
#     PERFECTIONISM_z +
#     Pol.Orient_z +
#     POWERDEPENDENCE_z + #
#     PWI_z +
#     Relid_z +
#     Respect.Self_z + #
#     Rumination_z + #
#     SELF.CONTROL_z + #
#     SELF.ESTEEM_z + #
#     SexualSatisfaction_z +#
#     SFHEALTH_z +#
#     Smoker_z +#
#     SUPPORT_z +#
#     Urban_z +
#     Volunteers_z,
#   family = "gaussian",
#   data = out2_for,
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   init =0,
#   chains = 4,
#   backend = "cmdstanr",
#   file = here::here("mods", "forgivemods", "m20_VENGEFULRUMIN_forgive.rds"),
#   set_prior('normal(0, 1)', class = 'b')
# )
# ##
# pp_check()
#
# ## for binary outcomes
# lp <-
#   posterior_linpred(
#     X,
#     newdata = newdata,
#   )
# )
# round( posterior_summary(lp[, 2] / lp[, 1]), 3)
#
# posterior_summary(lp[, 1] / lp[, 2])
#
#
# # model coeff
# out <- model_parameters(m20_VENGEFULRUMIN_forgive, dispersion = TRUE, centrality = "mean",
#                         test = "pd",
#                         digits = 5,
#                         diagnostic =  NULL,
#                         rope_range = NULL)%>% slice(2)
# out
#
# # evalue
# round( EValue::evalues.OLS(  , se = , sd = 1, delta = 2, true = 0), 3)
#
# # evalue for RR
# round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)
#
# #table
# lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)
#
# # another table
# m1_bayes_table <-
#   parameters::model_parameters()
# m1_bayes_table
# plot(m1_bayes_table)
#
# # graph
# m20_VENGEFULRUMIN_forgive_plot <-
#   plot(conditional_effects(
#     m20_VENGEFULRUMIN_forgive,
#     "VENGEFUL.RUMIN_lead1_z",
#     ndraws = 500,
#     spaghetti = T
#   ))
#
# m20_VENGEFULRUMIN_forgive_plot_z  <-  m20_VENGEFULRUMIN_forgive_plot[[1]]  +
#   scale_y_continuous(limits = c(-.5, .5)) +
#   scale_x_continuous(limits = c(-2, 2)) +
#   labs(title = "Vengeful/Forgiveness",
#        # subtitle = "Loss shows greater distress",
#        y = "Vengeful/Forgiveness (sd)",
#        x = "Un-Forgiving (sd)")
#
# ggsave(
#   X_plot_z,
#   path = here::here(here::here("figs", "figs_forgive")),
#   width = 16,
#   height = 9,
#   units = "in",
#   filename = "X_plot_z.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 400
# )

# life meaning ------------------------------------------------------------

m21_LIFEMEANING_forgive <- brm_multiple(
  LIFEMEANING_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  init = 0,
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m21_LIFEMEANING_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)



m21a_LIFEMEANING_forgive <- brm_multiple(
  LIFEMEANING_lead2ord ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = cumulative("probit"),
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m21a_LIFEMEANING_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

##
pp_check(m21_LIFEMEANING_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m21_LIFEMEANING_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.05211  , se =  0.00624, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m21_LIFEMEANING_forgive_plot <-
  plot(conditional_effects(
    m21_LIFEMEANING_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m21_LIFEMEANING_forgive_plot_z  <-  m21_LIFEMEANING_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Life Meaning",
       # subtitle = "Loss shows greater distress",
       y = "Life Meaning (sd)",
       x = "Un-Forgiving (sd)")

m21_LIFEMEANING_forgive_plot_z
ggsave(
  m21_LIFEMEANING_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m21_LIFEMEANING_forgive_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)


# honesty -----------------------------------------------------------------

m22_HONESTYHUMILITY_forgive <- brm_multiple(
  HONESTY_HUMILITY_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init =0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m22_HONESTYHUMILITY_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m22_HONESTYHUMILITY_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m22_HONESTYHUMILITY_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(   -0.04750 , se =  0.00607, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m22_HONESTYHUMILITY_forgive_plot <-
  plot(conditional_effects(
    m22_HONESTYHUMILITY_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m22_HONESTYHUMILITY_forgive_plot_z  <-  m22_HONESTYHUMILITY_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Honesty Humility",
       # subtitle = "Loss shows greater distress",
       y = "Honesty Humility (sd)",
       x = "Un-Forgiving (sd)")
m22_HONESTYHUMILITY_forgive_plot_z
ggsave(
  m22_HONESTYHUMILITY_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m22_HONESTYHUMILITY_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# BELONG_z ----------------------------------------------------------------

m23_BELONG_forgive <- brm_multiple(
  BELONG_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m23_BELONG_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m23_BELONG_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m23_BELONG_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(   -0.06278, se =  0.00625 , sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m23_BELONG_forgive_plot <-
  plot(conditional_effects(
    m23_BELONG_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m23_BELONG_forgive_plot_z  <-  m23_BELONG_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Social Belonging",
       # subtitle = "Loss shows greater distress",
       y = "Social Belonging (sd)",
       x = "Un-Forgiving (sd)")

m23_BELONG_forgive_plot_z

ggsave(
  m23_BELONG_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m23_BELONG_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)


# SUPPORT_z ---------------------------------------------------------------

m24_SUPPORT_forgive <- brm_multiple(
  SUPPORT_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  init = 0,
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m24_SUPPORT_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m24_SUPPORT_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m24_SUPPORT_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.05206 , se = , 0.00649 , sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m24_SUPPORT_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m24_SUPPORT_forgive_plot <-
  plot(conditional_effects(
    m24_SUPPORT_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m24_SUPPORT_forgive_plot_z  <-  m24_SUPPORT_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Social Support",
       # subtitle = "Loss shows greater distress",
       y = "Social Support (sd)",
       x = "Un-Forgiving (sd)")

m24_SUPPORT_forgive_plot_z

ggsave(
  m24_SUPPORT_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m24_SUPPORT_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# Volunteers_lead2 --------------------------------------------------------------

m25_Volunteers_forgive <- brm_multiple(
  Volunteers_lead2 ~ #| trials(1) ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "poisson",
  #family = "poisson",#binomial("identity"),
  #bernoulli(link = "cloglog")
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m25_Volunteers_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
#
#
# ###  version
# m14a_church_volunteers <- brm_multiple(
#   Volunteers_lead2 ~
#     VENGEFUL.RUMIN_lead1_z +
#     VENGEFUL.RUMIN  +
#     AGREEABLENESS_z +
#     CONSCIENTIOUSNESS_z +
#     EXTRAVERSION_z  +
#     HONESTY_HUMILITY_z +
#     NEUROTICISM_z +
#     OPENNESS_z +
#     Age_z +
#     Alcohol.Frequency_z + #
#     Alcohol.Intensity_log_z + #
#     Bodysat_z +
#     Believe.God_z +
#     Believe.Spirit_z +
#     BELONG_z + #
#     CharityDonate_log_z + #
#     ChildrenNum_z +
#     Church_z +
#     community +
#     Edu_z +
#     Employed_z +
#     EmotionRegulation_z +
#     Euro_z +
#     GRATITUDE_z +
#     HomeOwner_z +
#     Hours.Exercise_log_z +
#     Hours.Work_z +
#     HLTH.BMI_z  + #
#     HLTH.Fatigue_z + #
#     income_log_z +
#     KESSLER6sum_z + #
#     LIFEMEANING_z + #
#     LIFESAT_z + #
#     lost_job_z +
#     Male_z +
#     NZdep_z +
#     NWI_z +
#     Parent_z +
#     Partner_z +
#     PERFECTIONISM_z +
#     Pol.Orient_z +
#     POWERDEPENDENCE_z + #
#     PWI_z +
#     Relid_z +
#     Respect.Self_z + #
#     Rumination_z + #
#     SELF.CONTROL_z + #
#     SELF.ESTEEM_z + #
#     SexualSatisfaction_z +#
#     SFHEALTH_z +#
#     Smoker_z +#
#     SUPPORT_z +#
#     Urban_z +
#     Volunteers_z,
#   family = bernoulli(link = "cloglog"),
#   # family = bernoulli(link = "probit"),
#   data = out2_for,
#   seed = 1234,
#   init = 0,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   file = here::here("mods", "forgivemods", "m23a_Volunteers_forgive.rds"),
#   set_prior('normal(0, 1)', class = 'b')
# )
# ##
# pp_check(m14_church_volunteers)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m25_Volunteers_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  , se = , sd = 1, delta = 2, true = 0), 3)

# evalue for RR
exp( 0.02969)
exp( 0.02969 +  0.03690)
exp( 0.02969 -  0.03690)
round( EValue::evalues.RR( 1.030135, lo = 0.9928159 , hi = 1.068857, true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m25_Volunteers_forgive_plot <-
  plot(conditional_effects(
    m25_Volunteers_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m25_Volunteers_forgive_plot_z  <-  m25_Volunteers_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(0, .3)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Volunteers",
       # subtitle = "Loss shows greater distress",
       y = "Volunteers (sd)",
       x = "Un-Forgiving (sd)")

m25_Volunteers_forgive_plot_z
ggsave(
  m25_Volunteers_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m25_Volunteers_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# Charity.Donate_logz -----------------------------------------------

m26_CharityDonate_forgive <- brm_multiple(
  CharityDonate_log_lead2_z
  VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  init = 0,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m26_CharityDonate_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)


m26a_CharityDonate_forgive <- brm_multiple(
  CharityDonate_lead2 ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "negbinomial",
  data = out2_for,
  init = 0,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m26a_CharityDonate_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m26a_CharityDonate_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m26_CharityDonate_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.01605 , se =  0.00829, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m26_CharityDonate_forgive_plot <-
  plot(conditional_effects(
    m26_CharityDonate_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 200,
    spaghetti = T
  ))

m26_CharityDonate_forgive_plot_z  <-  m26_CharityDonate_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5 ,.5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Charity Donate",
       # subtitle = "Loss shows greater distress",
       y = "Charity Donate (sd)",
       x = "Un-Forgiving (sd)")


m26_CharityDonate_forgive_plot_z

ggsave(
  m26_CharityDonate_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m26_CharityDonate_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)


# community ---------------------------------------------------------------

m27_community_forgive <- brm_multiple(
  community_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init =0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m27_community.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m27_community_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m27_community_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(   -0.03028, se =  0.00688, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m27_community_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m27_community_forgive_plot <-
  plot(conditional_effects(
    m27_community_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m27_community_forgive_plot_z  <-  m27_community_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Neighbourhood Community",
       # subtitle = "Loss shows greater distress",
       y = "Neighbourhood Community (sd)",
       x = "Un-Forgiving (sd)")

m27_community_forgive_plot_z

ggsave(
  m27_community_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m27_community_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# NWI ---------------------------------------------------------------------

m28_NWI_forgive <- brm_multiple(
  NWI_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init =0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m28_NWI_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m28_NWI_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m28_NWI_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS( 0.00309 , se = .00721, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m28_NWI_forgive_plot <-
  plot(conditional_effects(
    m28_NWI_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m28_NWI_forgive_plot_z  <-  m28_NWI_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "National Wellbeing",
       # subtitle = "Loss shows greater distress",
       y = "National Wellbeing (sd)",
       x = "Un-Forgiving (sd)")

ggsave(
  m28_NWI_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m28_NWI_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)




# multiplot ---------------------------------------------------------------

library(patchwork)
comp_plot <- m5a_fatigue_forgive_plot_z +
  m10_SexualSatisfaction_forgive_plot_z +
  m16_PERFECTIONISM_forgive_plot_z +
  m11_PWI_forgive_plot_z +
  m14_LIFESAT_forgive_plot_z +
  m17_SELFESTEEM_forgive_plot_z +
  m19_GRATITUDE_forgive_plot_z +
  m13_KESSLER6sum_forgive_plot_z +
  m27_community_forgive_plot_z +
  plot_annotation(title = "Comparative Outcomes Plot",
                  subtitle = "Selected effects of Un-Forgiveness", tag_levels = "A")

comp_plot
ggsave(
  comp_plot,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "comp_plot.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)
comp_plot



# mice example ------------------------------------------------------------

library(mice)
library(dplyr)
library(MatchThem)
library(brms)

data("nhanes2")
head(nhanes)
imp <- mice(nhanes2, printFlag = FALSE, seed = 0, m = 10)

# brms model with mids object
fit_mc1 <- brm_multiple(bmi ~ age + hyp + chl, data = imp,
                        backend = "cmdstanr",
                        family = "gaussian",
                        set_prior('normal(0, 1)',
                                  class = 'b'))

## brms will not accept a complete dataset

mc_imp1 <- mice::complete(imp, "long", inc = TRUE)

##   brms attempt with completed dataset
fit_mc2 <- brm_multiple(bmi ~ age + hyp + chl,
                        data = mc_imp1,
                        backend = "cmdstanr",
                        family = "gaussian",
                        set_prior('normal(0, 1)',
                                  class = 'b'))

# Error: 'data' must be a list of data.frames.

# However, we can convert the completed mc_imp1 object back to a mids object

mc_revertmids1 <- mc_imp1  %>% mutate_if(is.matrix, as.vector)
mc_imp1_r  <- mice::as.mids(mc_revertmids1)

# Brms will now run.

fit_mc2_r <- brm_multiple(bmi ~ age + hyp + chl,
                        data = mc_imp1_r,
                        backend = "cmdstanr",
                        family = "gaussian",
                        set_prior('normal(0, 1)',
                                  class = 'b'))

#Note that we could not return a mids object from a completed object within the "long" and "include = TRUE arguments.

# Consider working with weights using MathThem
w_imp <- weightthem(hyp ~  chl + age, data = imp,
                     approach = "within",
                     estimand = "ATE",
                     method = "ps")

out <- complete(w_imp, action ="long", include = FALSE, mild = TRUE)

m <- 10
listdat<- list()
for (i in 1:m) {
  listdat[[i]] <- as.data.frame(out[[i]])
}


fit_2<- brm_multiple(bmi|weights(weights) ~ age + hyp + chl,
                          data = listdat,
                          backend = "cmdstanr",
                          family = "gaussian",
                          set_prior('normal(0, 1)',
                                    class = 'b'))


c_imp <- MatchThem::complete(w_imp, "all")

m0 <- brm_multiple(Y|weights(weights) ~ A, data = c_imp)


# Error in mylist[[i]] :
#   attempt to select less than one element in integerOneIndex

