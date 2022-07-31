# standard of living

# income.R
# install
# https://github.com/markmfredrickson/optmatch



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
table1::table1(~ Standard.Living | Wave , data = tabinc_df, overall = FALSE)
# check N of ids
length(unique(tabinc_df$Id)) # 34782


## select vars
sl_df <- tabinc_df %>%
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
   # PWI,  ##  we use the individual
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
   # EthCat = factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    EthCat = as.integer(EthCat),
    Euro = as.numeric(if_else(EthCat == "Euro", 1, 0)),
    Male = ifelse(GendAll == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1),
  ) %>%
  arrange(Id, Wave)  %>% #
  dplyr::mutate(income_log_lead1 = lead(income_log, n = 1)) %>%
  dplyr::mutate(Hours.Work_lead1 = lead(Hours.Work, n = 1)) %>%
  dplyr::mutate(Standard.Living_lead1 = lead(Standard.Living, n = 1)) %>%
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
    #  PWI,
      Hours.Work,
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
     # EthCat,
      HoursCharity,
      Respect.Self_lead2,
      Household.INC,
      org2018,
      #  not_euro,
      #  not_euro_lead2,
      hold18,
    #   Euro,
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

length(unique(sl_df$Id)) # 31280

sl_df$EthCat

# inspect data
skim(sl_df)

sl_df %>%
  group_by(Wave) %>%
  summarise(across(Id, n_distinct))

# glimse
sl_df%>%
  summarise(across(c(PWI_lead2, LIFESAT_lead2, LIFEMEANING_lead2), ~mean(.x, na.rm=TRUE), ~sd(.x, na.rm=TRUE), n_distinct()))

#check
skimr::skim(sl_df)

# save function
saveh(sl_df, "sl_df")

# read if needed
sl_df<- readh("sl_df")

sl_df %>%
  summarise(across(c(income_log, income_log_lead2),
                   list(mean = mean, range=range, sd = sd), na.rm = TRUE, .names = "{col}_{fn}"))


# GFS coding
# Church
#[Never, a few times a year, a few times a month, weekly, more than once per week] [BMMRS 34]

# Religious TXTs
#[Never, occasionally, daily, more than daily] [BMMRS 15, modified]

#  Charity
#  In the past month, have you volunteered your time to an organization?2


table1::table1( ~
 factor( EthCat) +
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
   # PWI +
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
  data = sl_df
)


# missingness in 2020  1/3 of the data are missing

# I do not have enough power or control over important parts of my life.

# mice model  -------------------------------------------------------------
library(mice)

sl_mice <- sl_df %>%
  dplyr::select(-c( Wave, Id, Euro))
# Visualise missing
library(naniar)
naniar::gg_miss_var(sl_mice)

vis_miss(sl_mice,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(sl_mice)

# qp <- quickpred(inc_mice)  https://stefvanbuuren.name/fimd/sec-toomany.html
# qp
#for_mice$inc_prop <-
#  for_mice$income_log / (for_mice$income_log_lead1 - 1)


ini <- mice(sl_mice, m = 1, maxit = 0)
ini
meth <- ini$meth
#meth
#meth["inc_prop"] <- "~ I(income_log/(income_log_lead1 - 1))"
pred <- ini$pred
pred


# impute
out_sl <- mice::mice(sl_mice,
                      meth = meth,
                      pred = pred,
                      seed = 0,
                      m = 10)

# save
saveh(out_sl, "out_sl")

# read
out_sl <- readh("out_sl")

# https://www.r-bloggers.com/2020/12/iptw-with-missing-data/
# IPTW   see https://stats.stackexchange.com/questions/563057/multiple-imputation-and-inverse-probability-weighting-for-multiple-treatment##
#https://stats.stackexchange.com/questions/563057/multiple-imputation-and-inverse-probability-weighting-for-multiple-treatment


outlist2 <-
  row.names(out_sl)[out_sl$outflux < 0.5]
length(outlist2)

head(out_sl$loggedEvents, 10)

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
is.mids(out_sl)
# data warangling
long <- mice::complete(out_sl, "long", inc = TRUE)
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
long2$Hours.Exercise_lead2
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
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(Your.Personal.Relationships_lead2, digits = 0) +1)) %>%
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0) )) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0)+1 )) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1))%>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0)+1))%>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0) )) %>%
  dplyr::mutate(alcohol_bin2 = if_else( Alcohol.Frequency > 3, 1, 0))%>%
  dplyr::mutate(alcohol_bin = if_else( Alcohol.Frequency > 2, 1, 0))%>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work/10)%>%
  dplyr::mutate(Hours.Work_lead1_10 = Hours.Work_lead1/10)%>%
  dplyr::mutate(Hours.Work_ord = (as.numeric(
    cut(
      Hours.Work,
      breaks = c(-Inf, 0, 10, 20, 30, 40, 50, Inf),
      labels = c("0", "1", "2","3", "4", "5", "6"),
      right = TRUE
    )
  ) - 1)) %>%
  dplyr::mutate(Hours.Work_lead1ord = (as.numeric(
    cut(
      Hours.Work_lead1,
      breaks = c(-Inf, 0, 10, 20, 30, 40, 50, Inf),
      labels = c("0", "1", "2", "3", "4", "5", "6"),
      right = TRUE
    )
  ) - 1)) %>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))# Respect for Self is fully missing

table(long2$Hours.Work_ord)
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
out2_sl <- mice::as.mids(long3)
saveh(out2_sl, "out2_sl")
out2_sl <- readh("out2_sl")

# iptw --------------------------------------------------------------------


# Weights but now with the PWI  variables individually

library(MatchThem)
library(optmatch)
models_sl <- weightthem(Standard.Living_lead1_z ~ Standard.Living_z +
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
                               as.factor(EthCat) +
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
                             #  Standard.Living_z +
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


saveh(models_sl,"models_sl.rds")


sum<- summary(models_sl)
plot(sum)
sum
bal.tab(models_sl)


ctrim <- trim(models_sl, at = .99)
bal.tab(ctrim)
summary(ctrim)




## Iptw


# iptw models -------------------------------------------------------------
# no need to trim
ctrim <- models_sl


out <- with(ctrim, glm( HLTH.BMI_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(SFHEALTH_lead2_z  ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(Hours.Exercise_lead2 ~ Standard.Living_lead1_z, family = "poisson" ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Smoker_lead2 ~ Standard.Living_lead1_z , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp(-3.02-.14)


out <- with(ctrim, glm(HLTH.Fatigue_lead2ord ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <- with(ctrim, glm(Alcohol.Frequency_lead2ord ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(Alcohol.Intensity_lead2~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(Bodysat_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

# out <- with(ctrim, glm(PWI_lead2_z ~ Standard.Living_lead1_z ))
# output <- pool(out, dfcom = NULL)
# summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Rumination_lead2ord ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <- with(ctrim, glm(SexualSatisfaction_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(EmotionRegulation1_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(EmotionRegulation2_lead2_z~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( EmotionRegulation3_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(KESSLER6sum_lead2 ~ Standard.Living_lead1_z , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <- with(ctrim, glm(POWERDEPENDENCE_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(PERFECTIONISM_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(SELF.ESTEEM_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( GRATITUDE_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( VENGEFUL.RUMIN_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( LIFEMEANING_lead2ord ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( HONESTY_HUMILITY_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( BELONG_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( SUPPORT_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Volunteers_lead2 ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( CharityDonate_lead2 ~ Standard.Living_lead1_z , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp( 6.867947 + 0.2492043)

out <- with(ctrim, glm(community_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(NWI_lead2_z~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(ImpermeabilityGroup_lead2 ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


# out <- with(ctrim2, glm( Standard.Living_lead2ord ~ Standard.Living_lead1_z ))
# output <- pool(out, dfcom = NULL)
# summary(output, conf.int = TRUE)


out <- with(ctrim, glm( Your.Future.Security_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Your.Health_lead2_z  ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Your.Personal.Relationships_lead2ord ~ Standard.Living_lead1_z ))
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

bf_HLTH.BMI_lead2_z <- bf(HLTH.BMI_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_SFHEALTH_lead2_z <- bf(SFHEALTH_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_Hours.Exercise_lead2 <- bf(Hours.Exercise_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_Smoker_lead2 <- bf( Smoker_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_HLTH.Fatigue_lead2ord <- bf( HLTH.Fatigue_lead2ord |weights(weights) ~ Standard.Living_lead1_z)
bf_Alcohol.Frequency_lead2ord <- bf( Alcohol.Frequency_lead2ord |weights(weights) ~ Standard.Living_lead1_z)
bf_Alcohol.Intensity_lead2 <- bf( as.integer(Alcohol.Intensity_lead2) |weights(weights) ~ Standard.Living_lead1_z)
bf_Bodysat_lead2_z <- bf( Bodysat_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_PWI_lead2_z <- bf( PWI_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_Rumination_lead2ord <- bf(Rumination_lead2ord |weights(weights) ~ Standard.Living_lead1_z)
bf_SexualSatisfaction_lead2_z <- bf(SexualSatisfaction_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_PWI_lead2_z <- bf(PWI_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_EmotionRegulation1_lead2_z <- bf(EmotionRegulation1_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_EmotionRegulation2_lead2_z <- bf(EmotionRegulation2_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_EmotionRegulation3_lead2_z <- bf(EmotionRegulation3_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_KESSLER6sum_lead2 <- bf(KESSLER6sum_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_LIFESAT_lead2ord <- bf(LIFESAT_lead2ord |weights(weights) ~ Standard.Living_lead1_z)
bf_POWERDEPENDENCE_lead2_z <- bf(POWERDEPENDENCE_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_PERFECTIONISM_lead2_z <- bf(PERFECTIONISM_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_SELF.ESTEEM_lead2_z <- bf(SELF.ESTEEM_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_Emp.WorkLifeBalance_lead2_z <- bf( Emp.WorkLifeBalance_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_GRATITUDE_lead2_z <- bf( GRATITUDE_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_VENGEFUL.RUMIN_lead2ord <- bf(VENGEFUL.RUMIN_lead2ord  |weights(weights) ~ Standard.Living_lead1_z)
bf_LIFEMEANING_lead2ord <- bf(LIFEMEANING_lead2ord  |weights(weights) ~ Standard.Living_lead1_z)
bf_HONESTY_HUMILITY_lead2_z <- bf( HONESTY_HUMILITY_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_BELONG_lead2_z <- bf( BELONG_lead2_z  |weights(weights) ~ Standard.Living_lead1_z)
bf_SUPPORT_lead2ord <- bf( SUPPORT_lead2ord |weights(weights) ~ Standard.Living_lead1_z)
bf_Volunteers_lead2 <- bf( Volunteers_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_CharityDonate_lead2 <- bf( CharityDonate_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_community_lead2_z <- bf(community_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_NWI_lead2_z <- bf(NWI_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_ImpermeabilityGroup_z <- bf(ImpermeabilityGroup_z |weights(weights) ~ Standard.Living_lead1_z)
bf_PermeabilityIndividual_z<- bf( PermeabilityIndividual_z|weights(weights) ~ Standard.Living_lead1_z)


## ADD ONE

bf_Standard.Living_lead2ord<- bf( Standard.Living_lead2ord|weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Health_lead2_z<- bf( Your.Health_lead2_z|weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Future.Security_lead2 <- bf(Your.Future.Security_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Personal.Relationships_lead2ord<- bf( Your.Personal.Relationships_lead2ord|weights(weights) ~ Standard.Living_lead1_z)




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
  file = here::here("mods", "standardliving", "m1_bmi_income.rds"),
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
  file = here::here("mods", "standardliving", "m4_Smoker_lead2.rds"),
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
  file = here::here("mods", "standardliving", "m6_Alcohol.Frequency_lead2ord.rds"),
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
  file = here::here("mods", "standardliving", "m11_SexualSatisfaction_lead2_z.rds"),
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
  file = here::here("mods", "standardliving", "m13_EmotionRegulation1_lead2_z.rds"),
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
  file = here::here("mods", "standardliving", "m14_EmotionRegulation2_lead2_z.rds"),
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
  file = here::here("mods", "standardliving", "m15_EmotionRegulation3_lead2_z.rds"),
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
  file = here::here("mods", "standardliving", "m21_Emp.WorkLifeBalance_lead2_z.rds"),
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

out3 <- with(ctrim, geeglm(
  PWI_lead2_z  ~ Standard.Living_lead1_z,
  data = cmodels,
  id = 1:nrow(cmodels),
  family = gaussian))

# same result
output <- pool(out3)
summary(output, conf.int = TRUE)
plot(output)


# brms pwi follow up ------------------------------------------------------

bf_Standard.Living_lead2_z<- bf( Standard.Living_lead2_z|weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Health_lead2_z<- bf( Your.Health_lead2_z|weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Future.Security_lead2_z <- bf(Your.Future.Security_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Personal.Relationships_lead2ord<- bf( Your.Personal.Relationships_lead2ord|weights(weights) ~ Standard.Living_lead1_z)


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
  file = here::here("mods", "standardliving", "m35_Your.Future.Security_lead2_z.rds"),
)


m36_Your.Personal.Relationships_lead2ord <- brm_multiple(
  bf_Your.Personal.Relationships_lead2ord,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),  Chose family
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
  file = here::here("mods", "standardliving", "m36_Your.Personal.Relationships_lead2ord.rds"),
)
