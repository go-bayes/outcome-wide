impute_employed.R


# impute-the-lot
# Sexual orientation
#Spiritual identification
# Prayer frequency
# Voted in last election
# read data
# Physical limitation	hlthdisability
rm(df)
df <- readRDS(here::here("data_raw", "df.Rds"))


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
# df |>    # not there
#   dplyr::filter(Wave == 2020) |>
#   summarise(Respect.Self) #fully missing

# table for participant N
tabinc_df <- df |>
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  |>  # Eligibility criteria
  dplyr::filter(YearMeasured  != -1) |>  # remove people who passed away
  dplyr::filter(Id != 9630) |>  # problematic, made inc not reported
  group_by(Id) |>
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) |>   # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) |>   # Hack0
  dplyr::filter(hold19 > 0) |>  # hack to enable repeat of baseline in 201
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) |>   # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) |>   # Hack
  dplyr::filter(hold18 > 0) |>  # hack to enable repeat of baseline
  ungroup() |>
  droplevels() |>
  arrange(Id, Wave)

# check n # 34782
table1::table1(~ Standard.Living | Wave , data = tabinc_df, overall = FALSE)
# check N of ids
length(unique(tabinc_df$Id)) # 34783

tabinc_df |>
  filter(Wave == 2018) |>
  select(NZSEI06) |>
  count(is.na(.))

## USE 13
tabinc_df |>
  filter(Wave == 2018) |>
  select(NZSEI13) |>
  count(is.na(.))


## select vars
a_df <- tabinc_df |>
  select(
    Id,
    YearMeasured,
    Wave,
    EthCat,
    Age,
    GendAll,
    Male,
    SexualOrientationL1,
    NZSEI13,
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
    PWI,  ##  we use the individual
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
    POWERDEPENDENCE1,
    POWERDEPENDENCE2,
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living,
    # Env.SacWilling,
    #Env.SacMade,
    PERFECTIONISM ,
    PermeabilityIndividual,
    ImpermeabilityGroup,
    HLTH.SleepHours,
    LifeMeaning01,
    LifeMeaning02,
    Pol.VotedElection
    # Emp.JobSecure,
    # Env.ClimateChgCause,
    # Env.ClimateChgReal #,
  ) |>
  dplyr::rename(community = SWB.SoC01) |>
  dplyr::mutate(Edu = as.numeric(Edu)) |>
  dplyr::mutate(across(!c(Id, EthCat, Wave), ~ as.numeric(.x))) |>  # make factors numeric for easy of processing
  arrange(Id, Wave) |>
  dplyr::mutate(
    Edu = as.numeric(Edu),
    Volunteers = if_else(HoursCharity == 1, 1, 0),
    # Depressed = (as.numeric(
    #   cut(
    #     KESSLER6sum,
    #     breaks = c(-Inf, 13, Inf),
    #     labels = c("0", "1"),
    #     right = FALSE
    #   )
    # ) - 1),
    # EthCat = factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    EthCat = as.integer(EthCat),
    Euro = as.numeric(if_else(EthCat == "Euro", 1, 0)),
    Male = ifelse(GendAll == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1),
  ) |>
  arrange(Id, Wave)  |>  #
  dplyr::mutate(retired_lead1 = lead(retired, n = 1)) |>
  dplyr::mutate(semiretired_lead1 = lead(semiretired, n = 1)) |>
  #dplyr::mutate(Church_lead1 = lead(Church, n = 1)) |>
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::mutate(across(
    c(
      income_log,
      KESSLER6sum,
      HLTH.Fatigue,
      Rumination,
      community,
      SFHEALTH,
      LIFEMEANING,
      LIFESAT,
      PWI,
      Church,
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
      POWERDEPENDENCE1,
      POWERDEPENDENCE2,
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
      PermeabilityIndividual,
      ImpermeabilityGroup,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living,
      HLTH.SleepHours,
      LifeMeaning01,
      LifeMeaning02,
    ),
    ~ lead(.x, n = 1),
    .names = "{col}_lead1"
  )) |>  # make leads
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
      POWERDEPENDENCE1,
      POWERDEPENDENCE2,
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
      Standard.Living,
      HLTH.SleepHours,
      LifeMeaning01,
      LifeMeaning02
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) |>  # make leads
  dplyr::filter(Wave == 2018) |>
  dplyr::filter(retired != 1) |>
  dplyr::filter(retired_lead1 != 1) |>   #needed for the intervention
  dplyr::filter(semiretired != 1) |>
  dplyr::filter(semiretired_lead1 != 1) |>   #needed for the intervention
  #  dplyr::filter(!is.na(Church)) |>
  #  dplyr::filter(!is.na(Church_lead1)) |>   #needed for the intervention
  dplyr::select(
    -c(
      Religion.Church,
      # EthCat,
      HoursCharity,
      Respect.Self_lead2,
      Household.INC,
      # org2018,
      #  not_euro,
      #  not_euro_lead2,
      #   hold18,
      #   Euro,
      Emp.WorkLifeBalance,
      YearMeasured,
      #  org2019,
      #  hold19,
      retired,
      retired_lead1,
      semiretired,
      semiretired_lead1
    )
  ) |>
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))|>   # standarise vars for easy computing
  arrange(Id, Wave) |>
  data.frame() |>
  mutate(across(where(is.double), as.numeric)) |>
  arrange(Id)

length(unique(a_df$Id)) # 34783

# inspect data
skim(a_df)

a_df |>
  group_by(Wave) |>
  summarise(across(Id, n_distinct))

# glimse
a_df|>
  summarise(across(c( LIFESAT_lead2, LIFEMEANING_lead2), ~mean(.x, na.rm=TRUE), ~sd(.x, na.rm=TRUE), n_distinct()))

#check
skimr::skim(a_df)

# save function
saveh(a_df, "a_df")

# read if needed
a_df<- readh("a_df")

a_df |>
  summarise(across(c(income_log, income_log_lead1, income_log_lead2),
                   list(mean = mean, range=range, sd = sd), na.rm = TRUE, .names = "{col}_{fn}"))


# GFS coding
# Church
#[Never, a few times a year, a few times a month, weekly, more than once per week] [BMMRS 34]

# Religious TXTs
#[Never, occasionally, daily, more than daily] [BMMRS 15, modified]

#  Charity
#  In the past month, have you volunteered your time to an organization?2

# I do not have enough power or control over important parts of my life.

# mice model  -------------------------------------------------------------
library(mice)
dev.off()
a_mice <- a_df |>
  dplyr::select(-c( Wave, Id, Euro))
# Visualise missing
library(naniar)
naniar::gg_miss_var(a_mice)

vis_miss(a_mice,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(a_mice)

# qp <- quickpred(inc_mice)  https://stefvanbuuren.name/fimd/sec-toomany.html
# qp
#for_mice$inc_prop <-
#  for_mice$income_log / (for_mice$income_log_lead1 - 1)


ini <- mice(a_mice, m = 1, maxit = 0)
ini
meth <- ini$meth
#meth
#meth["inc_prop"] <- "~ I(income_log/(income_log_lead1 - 1))"
pred <- ini$pred
pred


# impute
out_a <- mice::mice(a_mice,
                    meth = meth,
                    pred = pred,
                    seed = 0,
                    m = 10)

# save
saveh(out_a, "out_a")

# read
out_a <- readh("out_a")

# https://www.r-bloggers.com/2020/12/iptw-with-missing-data/
# IPTW   see https://stats.stackexchange.com/questions/563057/multiple-imputation-and-inverse-probability-weighting-for-multiple-treatment##
#https://stats.stackexchange.com/questions/563057/multiple-imputation-and-inverse-probability-weighting-for-multiple-treatment


outlist2 <-
  row.names(out_a)[out_a$outflux < 0.5]
length(outlist2)

head(out_a$loggedEvents, 10)

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
is.mids(out_a)
# data warangling
long <- mice::complete(out_a, "long", inc = TRUE)
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
hist(long$HLTH.SleepHours)
long2$Hours.Exercise_lead2
# create variables in z score

HLTH.SleepHours_z

long2 <- long |>
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) |>
  dplyr::mutate(Church_lead1bin = if_else(Church_lead1>=4,1,0))|>
  dplyr::mutate(Church_bin = if_else(Church>=4,1,0))|>
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0)))|>
  dplyr::mutate(HLTH.SleepHours_lead2 = round(as.integer(HLTH.SleepHours_lead2, 0)))|>
  dplyr::mutate(HLTH.SleepHours = round(as.integer(HLTH.SleepHours, 0)))|>
  dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0))|>
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0))|>
  dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0))|>
  dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1))|>
  plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0))|>
  dplyr::mutate(CharityDonate = round(CharityDonate, 0))|>
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0))|>
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2+1))|>
  dplyr::mutate(Alcohol.Intensity_log_lead2 = log(Alcohol.Intensity_lead2+1))|>
  dplyr::mutate(Exercise_log_lead2 = log(Hours.Exercise_lead2+1))|>
  dplyr::mutate(CharityDonate_log = log(CharityDonate+1))|>
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity+1))|>
  dplyr::mutate(Rumination_lead2ord = as.integer(round(Rumination_lead2, digits = 0) + 1)) |>   # needs to start at 1
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0) )) |>
  dplyr::mutate(PERFECTIONISM_lead2ord = as.integer(round(PERFECTIONISM_lead2, digits = 0) )) |>
  dplyr::mutate(VENGEFUL.RUMIN_lead2ord = as.integer(round(VENGEFUL.RUMIN_lead2, digits = 0) )) |>
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0) )) |>
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(Your.Personal.Relationships_lead2, digits = 0) +1)) |>
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0) )) |>
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0)+1 )) |>
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1))|>
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0)+1))|>
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0) )) |>
  dplyr::mutate(alcohol_bin2 = if_else( Alcohol.Frequency > 3, 1, 0))|>
  dplyr::mutate(alcohol_bin = if_else( Alcohol.Frequency > 2, 1, 0))|>
  dplyr::mutate(Hours.Work_10 =  Hours.Work/10)|>
  dplyr::mutate(Hours.Work_lead1_10 = Hours.Work_lead1/10)|>
  dplyr::mutate(Hours.Work_ord = (as.numeric(
    cut(
      Hours.Work,
      breaks = c(-Inf, 0, 10, 20, 30, 40, 50, Inf),
      labels = c("0", "1", "2","3", "4", "5", "6"),
      right = TRUE
    )
  ) - 1)) |>
  dplyr::mutate(Church_lead1 = (as.numeric(
    cut(
      Hours.Work_lead1,
      breaks = c(-Inf, 0, 10, 20, 30, 40, 50, Inf),
      labels = c("0", "1", "2","3", "4", "5", "6"),
      right = TRUE
    )
  ) - 1)) |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) |>
  select(-c(.imp_z, .id_z))# Respect for Self is fully missing

table(long2$Church_lead1bin)
table(long2$Church_bin)
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
long3 <- long2 |>  mutate_if(is.matrix, as.vector)
out2_a <- mice::as.mids(long3)
saveh(out2_a, "out2_a")
out2_a <- readh("out2_a")




# church match -----------------------------------------------------------


# Weights but now with the PWI  variables individually

library(MatchThem)
library(optmatch)
long2$Church_lead1
models_ch <- weightthem( Church_lead1 ~
                           Church +
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
                           # Church_z +
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
                           HLTH.BMI_z  +
                           HLTH.Fatigue_z +
                           HLTH.SleepHours_z
                         income_log_z +
                           ImpermeabilityGroup_z +
                           KESSLER6sum_z +
                           LIFEMEANING_z +
                           LIFESAT_z +
                           Male_z +
                           NZdep_z +
                           NWI_z +
                           NZSEI13_z +
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


saveh(models_ch,"models_ch.rds")
models_ch <- readh("models_ch.rds")

# models_ch1 <- weightthem( Church_lead1~
#                            Church_bin_z +
#                            AGREEABLENESS_z +
#                            CONSCIENTIOUSNESS_z +
#                            EXTRAVERSION_z  +
#                            HONESTY_HUMILITY_z +
#                            NEUROTICISM_z +
#                            OPENNESS_z +
#                            Age_z +
#                            Alcohol.Frequency_z +
#                            Alcohol.Intensity_log_z +
#                            Bodysat_z +
#                            Believe.God_z +
#                            Believe.Spirit_z +
#                            BELONG_z +
#                            CharityDonate_log_z +
#                            ChildrenNum_z +
#                            # Church_z +
#                            community +
#                            Edu_z +
#                            Employed_z +
#                            EmotionRegulation1_z +
#                            EmotionRegulation2_z +
#                            EmotionRegulation3_z +
#                            as.factor(EthCat) +
#                            GRATITUDE_z +
#                            HomeOwner_z +
#                            Hours.Exercise_log_z +
#                            Hours.Work_z +
#                            HLTH.BMI_z  +
#                            HLTH.Fatigue_z +
#                            income_log_z +
#                            ImpermeabilityGroup_z +
#                            KESSLER6sum_z +
#                            LIFEMEANING_z +
#                            LIFESAT_z +
#                            Male_z +
#                            NZdep_z +
#                            NWI_z +
#                            NZSEI18_z +
#                            Parent_z +
#                            Partner_z +
#                            PERFECTIONISM_z +
#                            PermeabilityIndividual_z +
#                            Pol.Orient_z +
#                            POWERDEPENDENCE1_z +
#                            POWERDEPENDENCE2_z +
#                            # PWI_z +
#                            Relid_z +
#                            Respect.Self_z +
#                            Rumination_z +
#                            SELF.CONTROL_z +
#                            SELF.ESTEEM_z +
#                            SexualSatisfaction_z +
#                            SFHEALTH_z +
#                            Smoker_z +
#                            Standard.Living_z +
#                            SUPPORT_z +
#                            Urban_z +
#                            VENGEFUL.RUMIN_z +
#                            Volunteers_z +
#                            Your.Health_z +
#                            Your.Future.Security_z +
#                            Your.Personal.Relationships_z,
#                          out2_a,
#                          approach = 'within',
#                          estimand = "ATE",
#                          stabilize = TRUE,
#                          method = "ebal")
#
# saveh(models_ch1,"models_ch1.rds")

warnings()

bal.tab(models_ch)
sum<- summary(models_ch)
sum


ctrim <- trim(models_ch, at = .9)
bal.tab(ctrim) # good
sum <- summary(ctrim)
plot(sum)


out <- with(ctrim, glm( CharityDonate_lead2 ~ Church_lead1 , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp(6.6072905 + 0.3383447 )



### TEST G-COMP

#Loading libraries
library(MatchThem)
library(marginaleffects)

# #Matching the multiply imputed datasets
# matched.datasets <- matchthem(OSP ~ AGE + SEX,
#                               imputed.datasets,
#                               approach = 'within',
#                               method = 'full')
#
# #Analyzing the weighted datasets
# models <- with(matched.datasets,
#                lm(BMI ~ OSP * splines::ns(qlogis(distance), 5)))





# iptw models  ------------------------------------------------------------

ctrim <- ctrim1
#y +
out <- with(ctrim, lm( HLTH.BMI_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary <- summary(output, conf.int = TRUE)
summary


#y+
out <- with(ctrim, glm(SFHEALTH_lead2_z  ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y-
out <- with(ctrim, glm(Hours.Exercise_lead2  ~ Church_lead1, family = "poisson" ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


#n
out <- with(ctrim, glm( Smoker_lead2 ~ Church_lead1 , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
#exp(0.1032199 )

#n
out <- with(ctrim, glm(HLTH.Fatigue_lead2ord ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y
out <- with(ctrim, glm(Alcohol.Frequency_lead2ord ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y+
out <- with(ctrim, glm(Alcohol.Intensity_lead2~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm(Bodysat_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

# out <- with(ctrim, glm(PWI_lead2_z ~ Church_lead1 ))
# output <- pool(out, dfcom = NULL)
# summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( Rumination_lead2ord ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm(SexualSatisfaction_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm(EmotionRegulation1_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm(EmotionRegulation2_lead2_z~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( EmotionRegulation3_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y
out <- with(ctrim, glm(KESSLER6sum_lead2 ~ Church_lead1 , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp(-.03)
#n
out <- with(ctrim, glm(POWERDEPENDENCE1_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm(PERFECTIONISM2_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y
out <- with(ctrim, glm(SELF.ESTEEM_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y
out <- with(ctrim, glm( GRATITUDE_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y
out <- with(ctrim, glm( VENGEFUL.RUMIN_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( -0.03039559 , se =0.01225761, sd = 1, delta = 4, true = 0), 3)


## posterior predictive checks
pp_check(m2_church_lifemeaning)


#y
out <- with(ctrim, glm( LIFEMEANING_lead2ord ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( HONESTY_HUMILITY_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

# n
out <- with(ctrim, glm( BELONG_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( SUPPORT_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( Volunteers_lead2 ~ Church_lead1, family = "poisson" ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

exp(0.1006672)
#n
out <- with(ctrim, glm( CharityDonate_lead2 ~ Church_lead1 , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp(6.6072905 + 0.3383447 )

#n
out <- with(ctrim, glm(community_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y
out <- with(ctrim, glm(NWI_lead2_z~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm(ImpermeabilityGroup_lead2 ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


#n
out <- with(ctrim, glm( Standard.Living_lead2ord ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( Your.Future.Security_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm( Your.Health_lead2_z  ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


#n
out <- with(ctrim, glm( Your.Personal.Relationships_lead2ord ~ Church_lead1 ))
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





# gcomputation
out <- with(ctrim, glm( VENGEFUL.RUMIN_lead2_z ~ Church_lead1 ))

marg1 <- with(out, margins)


output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
out$analyses
round( EValue::evalues.OLS( -0.03039559 , se =0.01225761, sd = 1, delta = 4, true = 0), 3)

out2 <- with(ctrim, glm( VENGEFUL.RUMIN_lead2_z ~ Church_lead1 ))

for (i in 1:length(out$analyses)) {
  pred0 <- lapply(getfit(out), predict,  newdata = subset(complete(ctrim, i), variables = Church_lead1), se.fit = TRUE)


  # ### GCOMPUTATION
  # library(marginaleffects)
  # #Marginal effects
  # g.comp <- NULL
  # for (i in 1:length(out$analyses)) {
  #   g.comp <- marginaleffects::comparisons(out$analyses[[i]],
  #                        newdata = subset(complete(ctrim, i), variables = Church_lead1),
  #                        vcov = ~subclass)
  #   out$analyses[[i]] <- g.comp
  # }
  #
  # #Pooling results obtained from analyzing the datasets
  # results <- pool(out)
  # summary(results, conf.int = TRUE, exp = FALSE)


  predm <- lapply(getfit(out), predict, newdata = data.frame(
    Church_lead1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8)), se.fit = TRUE)


  #Extracting the original dataset with missing value
  maindataset <- complete(ctrim, action = 0)

  #Some spit-and-polish
  maindataset <- data.frame(.imp = 0, .id = seq_len(nrow(maindataset)), maindataset)
  #Extracting imputed-weighted datasets in the long format
  alldataset  <- complete(ctrim, action = "long")

  #Binding them together
  alldataset  <- rbind(maindataset, alldataset)

  #Converting to .mids
  newmids <- as.mids(alldataset)
  is.mids(newmids)
  test <- with(newmids, glm( VENGEFUL.RUMIN_lead2_z ~ Church_lead1, weights = weights))

  longmids <- complete(newmids, include = TRUE)

  nrow(longmids)

  #Pooling results obtained from analyzing the datasets
  results <- pool(out)
  summary(results, conf.int = TRUE, exp = FALSE)

  str(longmids)
  nrow(longmids)
  nrow(maindataset)
  # marginaleffects
  str(maindataset1)
  str()

  ## GRAPH
  predm <- lapply(getfit(out), predict, newdata = data.frame(
    Church_lead1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8)), se.fit = TRUE)

  pred0 <- lapply(getfit(out), predict, newdata = data.frame(
    Church_lead1 = 0), se.fit = TRUE)

  pred0
  predm <- pred0 # 0.005722974


  Q <- sapply(predm, `[[`, "fit")
  U <- sapply(predm, `[[`, "se.fit")^2
  # dfcom <- predm[[1]]$df  #not for glm
  dfcom<- 31278

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
  pred$x<- c(0:8)
  pred
  ggplot(pred, aes(x, fit)) + geom_point() + geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=.2,
                                                           position=position_dodge(.9)) +
    scale_y_continuous(limits = c(-.5,.5))

  ## SEE https://jmweinstein.github.io/memice/
  ## Marginal effects

  out_c <- complete(ctrim, action ="long", include = FALSE, mild = TRUE)

  m <- 10
  dlist<- NULL
  for (i in 1:m) {
    dlist[[i]] <- as.data.frame(out_c[[i]])
  }

  dlist[[2]]

  maindataset <- as.data.frame(maindataset)

  fit_reg <- function(maindataset) {
    mod <- glm( VENGEFUL.RUMIN_lead2_z ~ Church_lead1, weights = weights, data =  maindataset)
    out <- marginaleffects(mod, newdata = maindataset)
    return(out)
  }

  library(marginaleffects)
  mod_imputation <- lapply( out_c , fit_reg)

  mod_imputation <- mice::pool(mod_imputation)
  summary(mod_imputation)


