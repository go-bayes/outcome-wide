# impute-the-lot
# Sexual orientation
#Spiritual identification
# Prayer frequency
# Voted in last election
# read data
# Physical limitation	hlthdisability
rm(df)
df <- readRDS(here::here("data_raw", "df.Rds"))

df$SWB.Kessler01

source(here::here("scripts", "funs.R"))

# read libraries in
source(here::here("scripts", "libs.R"))
library("dplyr")
library("tidyr")

# SWB.Kessler01.T04	you feel hopeless?
#   SWB.Kessler02.T04	you feel so depressed that nothing could cheer you up?
#   SWB.Kessler03.T04	you feel restless or fidgety?
#   SWB.Kessler04.T04	you feel that everything was an effort?
#   SWB.Kessler05.T04	you feel worthless?
#   SWB.Kessler01.T03	you feel nervous?

# test <- df %>%
#   filter(YearMeasured==1) %>%
#   dplyr::select(Religion.Prayer, Religion.Scripture, Religion.Church, Wave)
# table1::table1(~ Religion.Prayer2 + Religion.Scripture2 + Religion.Church2| Wave, dat = test)
# Perc.Discrim.T11
# Perc.Gend.Discrim.T11
# Perc.Religious.Discrim.T11
# Ethnic.Discrim.T11
# Spiritual.Identification


df <- df |>
  dplyr::rename(
    hopeless_k6 = SWB.Kessler01,
    depressed_k6 = SWB.Kessler02,
    restless_k6 = SWB.Kessler03,
    effort_k6 = SWB.Kessler04,
    worthless_k6 = SWB.Kessler05,
    nervous_k6 = SWB.Kessler06
  )

#
# # order vars
# df$GenCohort <-
#   ordered(
#     df$GenCohort,
#     levels = c(
#       "Gen_Silent: born< 1946",
#       "Gen Boomers: born >= 1946 & b.< 1965",
#       " GenX: born >=1961 & b.< 1981",
#       "GenZ: born >= 1996 "
#     )
#   )
# # # view
# # df %>%   # not there
# #   dplyr::filter(Wave == 2020) %>%
# #   summarise(Respect.Self) #fully missing

# table for participant N
tabinc_df <- df %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
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
  droplevels() %>%
  arrange(Id, Wave)

# check n # 34782
table1::table1(~ Standard.Living | Wave , data = tabinc_df, overall = FALSE)
# check N of ids
length(unique(tabinc_df$Id)) # 34783
table(tabinc_df$Euro)
tabinc_df %>%
  filter(Wave == 2018) %>%
  select(NZSEI06) %>%
  count(is.na(.))

## USE 13
tabinc_df %>%
  filter(Wave == 2018) %>%
  select(NZSEI13) %>%
  count(is.na(.))


## select vars
a_df <- tabinc_df %>%
  select(
    Id,
    YearMeasured,
    Wave,
    EthCat,
    Euro,
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
    Religion.Church2,
    Believe.Spirit,
    Believe.God,
    Religion.Prayer2,
    Religion.Scripture2,
    SWB.SoC01,
    EmotionRegulation1,
    EmotionRegulation2,
    EmotionRegulation3,
    Bodysat,
    VENGEFUL.RUMIN,
    retired,
    semiretired,
    BornNZ, #nor working
    KESSLER6sum,
    hopeless_k6,
    depressed_k6,
    restless_k6,
    effort_k6,
    worthless_k6,
    nervous_k6,
    HLTH.Fatigue,
    Rumination,
    Smoker,
    # ChildrenNum,
    NWI,
    BELONG,
    SUPPORT,
    CharityDonate,
    HoursCharity,
    GRATITUDE,
    # Volunteers,
    Hours.Work,
    Hours.Exercise,
    HLTH.Disability,
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
  ) %>%
  dplyr::rename(community = SWB.SoC01,
                Religion.Church  =  Religion.Church2,
                Religion.Prayer = Religion.Prayer2,
                Religion.Scripture = Religion.Scripture2)%>%  # avoid missingness
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, EthCat, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
  arrange(Id, Wave) %>%
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
    Male = ifelse(GendAll == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1),
  ) %>%
  arrange(Id, Wave)  %>% #
  dplyr::mutate(retired_lead1 = lead(retired, n = 1)) %>%
  dplyr::mutate(semiretired_lead1 = lead(semiretired, n = 1)) %>%
  #dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::mutate(across(
    c(
      income_log,
      KESSLER6sum,
      HLTH.Disability,
      hopeless_k6,
      depressed_k6,
      restless_k6,
      effort_k6,
      worthless_k6,
      nervous_k6,
      HLTH.Fatigue,
      Rumination,
      community,
      SFHEALTH,
      LIFEMEANING,
      LIFESAT,
      PWI,
      Church,
   #   Religion.Prayer, Missing  Prob only useful for religious people
   #   Religion.Scripture, Missing  Prob only useful for religious people
      Hours.Work,
      SELF.ESTEEM,
      SELF.CONTROL,
      Respect.Self,
      Alcohol.Frequency,
      Hours.Exercise,
      HLTH.Disability,
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
  )) %>% # make leads
  dplyr::mutate(across(
    c(
      income_log,
      KESSLER6sum,
      # hopeless_k6,
      # depressed_k6,
      # restless_k6,
      # effort_k6,
      # worthless_k6,
      # nervous_k6,
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
      HLTH.SleepHours#,
     # LifeMeaning01,
     # LifeMeaning02,
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
 # dplyr::filter(retired != 1) %>%
 # dplyr::filter(retired_lead1 != 1) %>%  #needed for the intervention
 # dplyr::filter(semiretired != 1) %>%
 # dplyr::filter(semiretired_lead1 != 1) %>%  #needed for the intervention
  #  dplyr::filter(!is.na(Church)) %>%
  #  dplyr::filter(!is.na(Church_lead1)) %>%  #needed for the intervention
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
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)

length(unique(a_df$Id)) # 34783

#check
skimr::skim(a_df)

# save function
saveh(a_df, "a_df")

# read if needed
a_df<- readh("a_df")



# inspect data
skim(a_df)
a_df %>%
  group_by(Wave) %>%
  summarise(across(Id, n_distinct))

# glimse
a_df%>%
  summarise(across(c( LIFESAT_lead2, LIFEMEANING_lead2), ~mean(.x, na.rm=TRUE), ~sd(.x, na.rm=TRUE), n_distinct()))

a_df %>%
  summarise(across(c(income_log, income_log_lead1, income_log_lead2),
                   list(mean = mean, range=range, sd = sd), na.rm = TRUE, .names = "{col}_{fn}"))

table1::table1( ~ Church_lead1, dat =a_df)


a_df$HLTH.Disability


# include spirit id -------------------------------------------------------

## select vars
b_df <- tabinc_df %>%
  select(
    Id,
    YearMeasured,
    Wave,
    EthCat,
    Euro,
    Age,
    GendAll,
    Male,
   # SexualOrientationL1,
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
    Religion.Church2,
    Believe.Spirit,
    Believe.God,
   # Religion.Prayer2,
   # Religion.Scripture2,
    Spiritual.Identification,
    SWB.SoC01,
    EmotionRegulation1,
    EmotionRegulation2,
    EmotionRegulation3,
    Bodysat,
    VENGEFUL.RUMIN,
    retired,
    semiretired,
    BornNZ, #nor working
    KESSLER6sum,
   # hopeless_k6,
   # depressed_k6,
  #  restless_k6,
  #  effort_k6,
  #  worthless_k6,
  #  nervous_k6,
    HLTH.Fatigue,
    Rumination,
    Smoker,
    # ChildrenNum,
    NWI,
    BELONG,
    SUPPORT,
    CharityDonate,
    HoursCharity,
    GRATITUDE,
    # Volunteers,
    Hours.Work,
    Hours.Exercise,
    HLTH.Disability,
    LIFEMEANING,
    LIFESAT,
    PWI,  ##  we use the individual
    NWI,
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
    HLTH.SleepHours#,
  #  LifeMeaning01,
  #  LifeMeaning02,
  #  Pol.VotedElection,
    # Emp.JobSecure,
    # Env.ClimateChgCause,
    # Env.ClimateChgReal #,
  ) %>%
  dplyr::rename(community = SWB.SoC01,
                Religion.Church  =  Religion.Church2) |> #,
            #    Religion.Prayer = Religion.Prayer2,
            #    Religion.Scripture = Religion.Scripture2)%>%  # avoid missingness
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, EthCat, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
  arrange(Id, Wave) %>%
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
    Male = ifelse(GendAll == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1),
  ) %>%
  arrange(Id, Wave)  %>% #
  dplyr::mutate(retired_lead1 = lead(retired, n = 1)) %>%
  dplyr::mutate(semiretired_lead1 = lead(semiretired, n = 1)) %>%
  #dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::mutate(across(
    c(
    #  income_log,
    #  KESSLER6sum,
    #  HLTH.Disability,
      # hopeless_k6,
      # depressed_k6,
      # restless_k6,
      # effort_k6,
      # worthless_k6,
      # nervous_k6,
    #  HLTH.Fatigue,
    #  Rumination,
    #  community,
    #  SFHEALTH,
    #  LIFEMEANING,
    #  LIFESAT,
    #  PWI,
      Church,
    #  Spiritual.Identification,
      #   Religion.Prayer, Missing  Prob only useful for religious people
      #   Religion.Scripture, Missing  Prob only useful for religious people
    #  Hours.Work,
    #  SELF.ESTEEM,
    #  SELF.CONTROL,
    #  Respect.Self,
    #  Alcohol.Frequency,
    #  Hours.Exercise,
    #  HLTH.Disability,
    #  HLTH.BMI,
    #  Smoker,
     # ChildrenNum,
    #  NWI,
   #   BELONG,
   #   SUPPORT,
    #  Volunteers,
    #  GRATITUDE,
    #  SexualSatisfaction,
    #  POWERDEPENDENCE1,
    #  POWERDEPENDENCE2,
      #Env.SacWilling,
      #Env.SacMade,
      #  Env.ClimateChgCause,
      #  Env.ClimateChgReal,
    #  CharityDonate,
    #  Alcohol.Intensity,
    #  PERFECTIONISM,
    #  Bodysat,
    #  VENGEFUL.RUMIN,
    #  community,
   #   HONESTY_HUMILITY,
   #   EmotionRegulation1,
   #   EmotionRegulation2,
   #   EmotionRegulation3,
   #   PermeabilityIndividual,
   #   ImpermeabilityGroup,
   #   Your.Future.Security,
   #   Your.Personal.Relationships,
   #   Your.Health,
   #   Standard.Living,
   #   HLTH.SleepHours#,
     # LifeMeaning01,
     # LifeMeaning02,
    ),
    ~ lead(.x, n = 1),
    .names = "{col}_lead1"
  )) %>% # make leads
  dplyr::mutate(across(
    c(
      income_log,
      KESSLER6sum,
      # hopeless_k6,
      # depressed_k6,
      # restless_k6,
      # effort_k6,
      # worthless_k6,
    #  nervous_k6,
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
     # ChildrenNum,
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
      HLTH.SleepHours#,
     # LifeMeaning01,
     # LifeMeaning02,
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
  # dplyr::filter(retired != 1) %>%
  # dplyr::filter(retired_lead1 != 1) %>%  #needed for the intervention
  # dplyr::filter(semiretired != 1) %>%
  # dplyr::filter(semiretired_lead1 != 1) %>%  #needed for the intervention
  #  dplyr::filter(!is.na(Church)) %>%
  #  dplyr::filter(!is.na(Church_lead1)) %>%  #needed for the intervention
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
      YearMeasured#,
      #  org2019,
      #  hold19,
     # retired,
      #retired_lead1,
     # semiretired,
     # semiretired_lead1
    )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)

length(unique(b_df$Id)) # 34782


#check
skimr::skim(b_df)

# save function
saveh(b_df, "b_df")

# read if needed
b_df<- readh("b_df")



# inspect data
skim(b_df)
b_df %>%
  group_by(Wave) %>%
  summarise(across(Id, n_distinct))

# glimse
b_df%>%
  summarise(across(c( LIFESAT_lead2, LIFEMEANING_lead2), ~mean(.x, na.rm=TRUE), ~sd(.x, na.rm=TRUE), n_distinct()))

b_df %>%
  summarise(across(c(income_log, income_log_lead1, income_log_lead2),
                   list(mean = mean, range=range, sd = sd), na.rm = TRUE, .names = "{col}_{fn}"))

table1::table1( ~ Church_lead1, dat =b_df)







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
# a_mice <- a_df %>%
#   arrange(Id) %>%
#   dplyr::select(-c( Wave, Id, EthCat))

b_mice <- b_df %>%
  arrange(Id) %>%
  dplyr::select(-c( Wave, Id, EthCat))

glimpse(b_mice)
# Visualise missing
library(naniar)
naniar::gg_miss_var(b_mice)

vis_miss(b_mice,
         warn_large_data = FALSE)
# any colinear vars?
mice:::find.collinear(b_mice)



qp <- quickpred(b_mice) # https://stefvanbuuren.name/fimd/sec-toomany.html
#Just one forgotten missing data mark may introduce large errors into the imputations.

table(rowSums(qp))
# qp
#for_mice$inc_prop <-
#  for_mice$income_log / (for_mice$income_log_lead1 - 1)



ini <- mice(b_mice, m = 1,  maxit = 0)
ini
meth <- ini$meth
#meth
#meth["inc_prop"] <- "~ I(income_log/(income_log_lead1 - 1))"
pred <- ini$pred
pred


# impute
out_b <- mice::mice(b_mice,
                     meth = meth,
                     pred = pred,
                     seed = 0,
                     m = 10)

# save
saveh(out_b, "out_b")

# read
out_b <- readh("out_b")

# https://www.r-bloggers.com/2020/12/iptw-with-missing-data/
# IPTW   see https://stats.stackexchange.com/questions/563057/multiple-imputation-and-inverse-probability-weighting-for-multiple-treatment##
#https://stats.stackexchange.com/questions/563057/multiple-imputation-and-inverse-probability-weighting-for-multiple-treatment


outlist2 <-
  row.names(out_a)[out_b$outflux < 0.5]
length(outlist2)

head(out_b$loggedEvents, 10)

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
is.mids(out_b)
# data warangling
long <- mice::complete(out_b, "long", inc = TRUE)
# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(long)
#long$LIFESAT_lead1_z <- with(long, scale(LIFESAT_lead1))
max(long$KESSLER6sum, na.rm = TRUE)
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

# HLTH.SleepHours_z
# Religion.Prayer2,
# Religion.Scripture2,
# Spiritual.Identification,

long2 <- long %>%
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(Church_lead1bin = if_else(Church_lead1>=1,1,0))%>%
  dplyr::mutate(Church_bin = if_else(Church>=1,1,0))%>%
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0)))%>%
 # dplyr::mutate(HLTH.SleepHours_lead2 = round(as.integer(HLTH.SleepHours_lead2, 0)))%>%
 # dplyr::mutate(HLTH.SleepHours = round(as.integer(HLTH.SleepHours, 0)))%>%
  dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0))%>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0))%>%
  dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0))%>%
  dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1))%>%
  plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0))%>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0))%>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0))%>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2+1))%>%
  dplyr::mutate(CharityDonate_log_lead2ord= round(CharityDonate_log_lead2, 0))%>%
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
 # dplyr::mutate(LifeMeaning01_lead2ord = as.integer(round(LifeMeaning01_lead2, digits = 0) )) %>%
 # dplyr::mutate(LifeMeaning02_lead2ord = as.integer(round(LifeMeaning02_lead2, digits = 0) )) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0)+1 )) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1))%>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0)+1))%>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0) )) %>%
  dplyr::mutate(alcohol_bin2 = if_else( Alcohol.Frequency > 3, 1, 0))%>%
  dplyr::mutate(alcohol_bin = if_else( Alcohol.Frequency > 2, 1, 0))%>%
  #dplyr::mutate(Hours.Work_10 =  Hours.Work/10)%>%
 # dplyr::mutate(Hours.Work_lead1_10 = Hours.Work_lead1/10)%>%
  dplyr::mutate(Hours.Work_ord = (as.numeric(
    cut(
      Hours.Work,
      breaks = c(-Inf, 0, 10, 20, 30, 40, 50, Inf),
      labels = c("0", "1", "2","3", "4", "5", "6"),
      right = TRUE
    )
  ) - 1)) %>%
  # dplyr::mutate(Hours.Work_lead1 = (as.numeric(
  #   cut(
  #     Hours.Work_lead1,
  #     breaks = c(-Inf, 0, 10, 20, 30, 40, 50, Inf),
  #     labels = c("0", "1", "2","3", "4", "5", "6"),
  #     right = TRUE
  #   )
  # ) - 1)) %>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) |>
  # dplyr::mutate(id = 1:length(.)) |>
  # group_by(id) |>
  # dplyr::mutate(LIFEMEANING2 = rowMeans(c(LifeMeaning01, LifeMeaning02, na.rm=TRUE))) %>%
  # dplyr::mutate(KESSLER6sum2 = rowSums(c(hopeless_k6,
  #                                        depressed_k6,
  #                                        restless_k6,
  #                                        effort_k6,
  #                                        worthless_k6,
  #                                        nervous_k6, na.rm=TRUE))) %>%
  select(-c(.imp_z, .id_z))# Respect for Self is fully missing

table(long2$Church_lead1bin)
table(long2$Spiritual.Identification)
table(long2$CharityDonate_log_lead2)
# review
# table(long_out_for_mice2$Rumination_lead2ord)
# table(long_out_for_mice2$LIFEMEANING_lead2ord)
# str(long_out_for_mice2$SUPPORT_lead2ord)
# hist(long_out_for_mice2$HLTH.Fatigue_lead2ord)
# hist(long_out_for_mice2$community_lead2)
# str(long_out_for_mice2$alcohol_bin2)

hist(long2$CharityDonate_lead2_log)
# get colnames
(long2$HLTH.Disability) # get names


#long$attrition <- with(long, ndf$attrition_lead2)
# neect to get into shape
long3 <- long2 %>% mutate_if(is.matrix, as.vector)
out2_b <- mice::as.mids(long3)
saveh(out2_b, "out2_b")
out2_b <- readh("out2_b")



# amelia imputation -------------------------------------------------------




# church match -----------------------------------------------------------


# Weights but now with the PWI  variables individually

# retired_lead1,
# semiretired,
# semiretired_lead1
# HLTH.SleepHours_z
# Religion.Prayer2,
# Religion.Scripture2,
# Spiritual.Identification,
# HLTH.Disability

library(MatchThem)
library(optmatch)
long2$Church_lead1

models_ch1 <- weightthem(
  Church_lead1 ~
    Church_z +
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
  #  Believe.God_z +
  #  Believe.Spirit_z +
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
    Euro_z +
  #  as.factor(EthCat) +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  +
    HLTH.Disability_z +
    HLTH.Fatigue_z +
    HLTH.SleepHours_z +
    income_log_z +
    ImpermeabilityGroup_z +
    KESSLER6sum_z +
    LIFEMEANING_z +
   # LifeMeaning01_lead2_z +
   # LifeMeaning02_lead2_z +
    LIFESAT_z+
    Male_z +
    NZdep_z +
    NWI_z +
    NZSEI13_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    PermeabilityIndividual_z +
    Pol.Orient_z +
   # Pol.VotedElection_z +
    POWERDEPENDENCE1_z +
    POWERDEPENDENCE2_z +
    # PWI_z +
  #  Relid_z +
  #  Religion.Prayer_z +
  #  Religion.Scripture_z +
    Respect.Self_z +
   # retired_z +
    Rumination_z +
    SELF.CONTROL_z +
    SELF.ESTEEM_z +
    #semiretired_z +
   # SexualOrientationL1_z +
    SexualSatisfaction_z +
    SFHEALTH_z +
    Smoker_z +
    Spiritual.Identification_z +
    Standard.Living_z +
    SUPPORT_z +
    Urban_z +
    VENGEFUL.RUMIN_z +
    Volunteers_z +
    Your.Health_z +
    Your.Future.Security_z +
    Your.Personal.Relationships_z,
  out2_b,
  approach = "within",
  estimand = "ATE",
  stabilize = TRUE,
  method = "optweight",
)



models_ch2 <- matchthem(
  Church_lead1bin ~
    Church_bin +
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
    #  Believe.God_z +
    #  Believe.Spirit_z +
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
    Euro_z +
    #  as.factor(EthCat) +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  +
    HLTH.Disability_z +
    HLTH.Fatigue_z +
    HLTH.SleepHours_z +
    income_log_z +
    ImpermeabilityGroup_z +
    KESSLER6sum_z +
    LIFEMEANING_z +
    # LifeMeaning01_lead2_z +
    # LifeMeaning02_lead2_z +
    LIFESAT_z+
    Male_z +
    NZdep_z +
    NWI_z +
    NZSEI13_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    PermeabilityIndividual_z +
    Pol.Orient_z +
    # Pol.VotedElection_z +
    POWERDEPENDENCE1_z +
    POWERDEPENDENCE2_z +
    # PWI_z +
    #  Relid_z +
    #  Religion.Prayer_z +
    #  Religion.Scripture_z +
    Respect.Self_z +
    # retired_z +
    Rumination_z +
    SELF.CONTROL_z +
    SELF.ESTEEM_z +
    #semiretired_z +
    # SexualOrientationL1_z +
    SexualSatisfaction_z +
    SFHEALTH_z +
    Smoker_z +
    Spiritual.Identification_z +
    Standard.Living_z +
    SUPPORT_z +
    Urban_z +
    VENGEFUL.RUMIN_z +
    Volunteers_z +
    Your.Health_z +
    Your.Future.Security_z +
    Your.Personal.Relationships_z,
  out2_b,
  approach = "within",
  estimand = "ATE",
  stabilize = TRUE,
  method = "full"
)

saveh(models_ch2,"models_ch2")
models_ch1 <- readh("models_ch2.rds")



warnings()

bal.tab(models_ch1)
sum<- summary(models_ch1)
sum


ctrim1 <- trim(models_ch1, at = .95)
bal.tab(ctrim1) # good
sum <- summary(ctrim1)
sum
plot(sum1)


out <- with(ctrim1, glm( CharityDonate_log_lead2_z ~ Church_lead1, family = "gaussian"))

output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( 0.08183320 , se =0.009829907, sd = 1, delta = 4, true = 0), 3)





# Disable printing results in scientific notation
options(scipen=999)

output <- pool(out2, dfcom = NULL)
summary(output)


round( EValue::evalues.OLS( 0.06925731 , se =0.02029099, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.OLS( 0.07012287529 , se =.010230794  , sd = 1, delta = 4, true = 0), 3)



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

#n
out <- with(ctrim, glm( HLTH.SleepHours_z ~ Church_lead1 ,
                        family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, lm( HLTH.BMI_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary <- summary(output, conf.int = TRUE)
summary


#n+
out <- with(ctrim, glm(SFHEALTH_lead2_z  ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm(Hours.Exercise_lead2  ~ Church_lead1, family = "poisson" ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


#n
out <- with(ctrim, glm( Smoker_lead2 ~ Church_lead1 , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
#exp(0.1032199 )

#t
out <- with(ctrim, glm(HLTH.Fatigue_lead2ord_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( -0.022048642 , se = 0.009446168, sd = 1, delta = 4, true = 0), 3)


#n
out <- with(ctrim, glm(Alcohol.Frequency_lead2ord_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( -0.04368970 , se = 0.008959088, sd = 1, delta = 4, true = 0), 3)


#n
out <- with(ctrim, glm(Alcohol.Intensity_lead2~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y
out <- with(ctrim, glm(Bodysat_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS( 0.02158401 , se = 0.009766047, sd = 1, delta = 4, true = 0), 3)



# out <- with(ctrim, glm(PWI_lead2_z ~ Church_lead1 ))
# output <- pool(out, dfcom = NULL)
# summary(output, conf.int = TRUE)

#y
out <- with(ctrim, glm( Rumination_lead2ord_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( -0.029630654 , se = 0.007311596, sd = 1, delta = 4, true = 0), 3)


#n
out <- with(ctrim, glm(SexualSatisfaction_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


# y less  weak eval
out <- with(ctrim, glm(EmotionRegulation1_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( -0.01559419 , se =  0.007719069, sd = 1, delta = 4, true = 0), 3)


# y less  stronger eval
out <- with(ctrim, glm(EmotionRegulation2_lead2_z~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS( -0.029832838 , se =  0.009187494 , sd = 1, delta = 4, true = 0), 3)


#n
out <- with(ctrim, glm( EmotionRegulation3_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#y
out <- with(ctrim, glm(KESSLER6sum_lead2_z ~ Church_lead1 , family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp(-.03)
round( EValue::evalues.OLS( -0.024959329 , se =   0.007719375 , sd = 1, delta = 4, true = 0), 3)



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

round( EValue::evalues.OLS(  0.02390447 , se =    0.008433290 , sd = 1, delta = 4, true = 0), 3)


#y
out <- with(ctrim, glm( GRATITUDE_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS(  0.03526302 , se =    0.008078408 , sd = 1, delta = 4, true = 0), 3)

#y
out <- with(ctrim, glm( VENGEFUL.RUMIN_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS(  -0.03727470 , se =    0.007113102 , sd = 1, delta = 4, true = 0), 3)


#y
out <- with(ctrim, glm( LIFEMEANING_lead2ord_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS(  0.05254794 , se =    0.007709672 , sd = 1, delta = 4, true = 0), 3)


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

round( EValue::evalues.OLS(  0.015643269 , se =    0.007794383 , sd = 1, delta = 4, true = 0), 3)

table(long2$Volunteers_lead2)
#n
out <- with(ctrim, glm( as.integer(Volunteers_lead2) ~ Church_lead1, family = poisson))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( -0.022048642 , se = 0.009446168, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR(1.1294, lo = 1.090544 , hi =1.169642, true = 1), 3)

exp(-3.2555996 + 0.1216869)/ exp(-3.2555996 )

exp(-3.2555996 + 0.1216869 - 0.03501059)/ exp(-3.2555996 )


#n

out <- with(ctrim, glm( CharityDonate_lead2 ~ Church_lead1 , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.RR(1.167099, lo = 1.147963 , hi =1.186553, true = 1), 3)

exp(6.6751690 + 0.1545209 - 0.01653194)/exp(6.6751690)

out <- with(ctrim, glm( CharityDonate_log_lead2_z ~ Church_lead1 , family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS(  0.08183320 , se =    0.009829907 , sd = 1, delta = 4, true = 0), 3)



#y
out <- with(ctrim, glm(community_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS(  0.01841627 , se =    0.008719121 , sd = 1, delta = 4, true = 0), 3)


#n
out <- with(ctrim, glm(NWI_lead2_z~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n
out <- with(ctrim, glm(ImpermeabilityGroup_lead2 ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


#n
out <- with(ctrim, glm( Standard.Living_lead2ord_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

#n  smal
out <- with(ctrim, glm( Your.Future.Security_lead2_z ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS(  0.016382694 , se =    0.007823290 , sd = 1, delta = 4, true = 0), 3)


#n
out <- with(ctrim, glm( Your.Health_lead2_z  ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


#n
out <- with(ctrim, glm( Your.Personal.Relationships_lead2ord ~ Church_lead1 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


#n
out <- with(ctrim, glm( PermeabilityIndividual_z ~ Church_lead1 ))
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




long2$CharityDonate_log_lead2_z
# gcomputation
out <- with(ctrim, lm( CharityDonate_log_lead2_z ~ Church_lead1 ))
out <- with(ctrim, glm( CharityDonate_lead2 ~ Church_lead1 , family = "poisson"))


output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out2 <- with(
  out2_b,
  glm(
    CharityDonate_lead2 ~ Church_lead1 +
      Church_z +
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
      #  Believe.God_z +
      #  Believe.Spirit_z +
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
      Euro_z +
      #  as.factor(EthCat) +
      GRATITUDE_z +
      HomeOwner_z +
      Hours.Exercise_log_z +
      Hours.Work_z +
      HLTH.BMI_z  +
      HLTH.Disability_z +
      HLTH.Fatigue_z +
      HLTH.SleepHours_z +
      income_log_z +
      ImpermeabilityGroup_z +
      KESSLER6sum_z +
      LIFEMEANING_z +
      # LifeMeaning01_lead2_z +
      # LifeMeaning02_lead2_z +
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
      # Pol.VotedElection_z +
      POWERDEPENDENCE1_z +
      POWERDEPENDENCE2_z +
      # PWI_z +
      #  Relid_z +
      #  Religion.Prayer_z +
      #  Religion.Scripture_z +
      Respect.Self_z +
      # retired_z +
      Rumination_z +
      SELF.CONTROL_z +
      SELF.ESTEEM_z +
      #semiretired_z +
      # SexualOrientationL1_z +
      SexualSatisfaction_z +
      SFHEALTH_z +
      Smoker_z +
      Spiritual.Identification_z +
      Standard.Living_z +
      SUPPORT_z +
      Urban_z +
      VENGEFUL.RUMIN_z +
      Volunteers_z +
      Your.Health_z +
      Your.Future.Security_z +
      Your.Personal.Relationships_z,
    family = "poisson"
  )
)

output2 <- pool(out2, dfcom = NULL)
summary(output2, conf.int = TRUE)


#round( EValue::evalues.OLS( -0.08183320 , se =0.01225761, sd = 1, delta = 4, true = 0), 3)

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
  Church_lead1 = c(0, 1, 2, 3, 4)), se.fit = TRUE)

## GRAPH

output <- pool(out2, dfcom = NULL)
summary(output, conf.int = TRUE)



predm <- lapply((out2), predict, newdata = data.frame(
  Church_lead1 = c(0, 1, 2, 3, 4)), se.fit = F)


dfcom
Q <- sapply(predm, `[[`, "fit")
U <- sapply(predm, `[[`, "se.fit")^2
#dfcom <- predm[[1]]$df  #not for glm
dfcom<- 34780

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
pred$reponse  <- exp(pred$fit)

pred2 <- pred |>
  mutate(
    response = exp(fit),
    upper = exp(fit + se.fit),
    lower = exp(fit - se.fit),
    church = 0:4
  )

pred2

ggplot(pred2, aes(church, response)) + geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                                                         position=position_dodge(.9)) +
scale_y_continuous(limits = c(0,2000))

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



# method for mids ---------------------------------------------------------

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

# fahrad method -----------------------------------------------------------

#Multiply imputing the missing values

#Matching the multiply imputed datasets
matched.datasets <- matchthem(OSP ~ AGE + SEX,
                              imputed.datasets,
                              approach = 'within',
                              method = 'full')



out <- with(ctrim1, glm( CharityDonate_log_lead2_z ~ Church_lead1 * splines::ns(qlogis(ps), 5), family = "gaussian"))


#Analyzing the weighted datasets
models <- with(ctrim,
               lm(BMI ~ OSP * splines::ns(qlogis(distance), 5)))

ctrim$object
models <- with(ctrim, glm( CharityDonate_log_lead2_z ~ Church_lead1 * splines::ns(qlogis(ps), 5), family = "gaussian"))


#Marginal effects
for (i in 1:length(models$analyses)) {
  g.comp <- comparisons(models$analyses[[i]],
                        newdata = subset(complete(matched.datasets, i), OSP == 1),
                        vcov = ~subclass)
  models$analyses[[i]] <- g.comp
}

#Pooling results obtained from analyzing the datasets
results <- pool(models)
summary(results, conf.int = TRUE, exp = FALSE)


output3 <- pool(out3, dfcom = NULL)
summary(output3, conf.int = TRUE)

# TRY WITH

out4 <- with( newmids, glm(
  CharityDonate_log_lead2_z ~ Church_lead1, weights = weights,
  family = "gaussian"
)
)
output4 <- pool(out4, dfcom = NULL)
summary(output4, conf.int = TRUE)

## get outputs





# not what we want
# newmids2$gcompZero <- lapply(getfit(out4), predict, newdata = data.frame(
#   Church_lead1 = c(0)), se.fit = F)




results <- pool(out4)
summary(results, conf.int = TRUE, exp = FALSE)


###









predm <- lapply((out2), predict, newdata = data.frame(
  Church_lead1 = c(0, 1, 2, 3, 4)), se.fit = F)

ctrim2 <- ctrim
ctrim2$predm <- lapply(getfit(out2), function(x) predict.glm(x, type = "response", se.fit=F))

ctrim2





# amelia imputation -------------------------------------------------------

am_all <-df %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
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
  droplevels() %>%
  arrange(Id, Wave)
  group_by(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(LIFESAT_bz = if_else(Wave == "2018", (as.numeric(LIFESAT)), NA_real_)) %>%
  fill(LIFESAT_bz) %>%
  dplyr::mutate(PWI_bz = if_else(Wave == "2018", (as.numeric(PWI)), NA_real_)) %>%
  fill(PWI_bz) %>%
  dplyr::mutate(SELF.ESTEEM_bz = if_else(Wave == "2018", (as.numeric(SELF.ESTEEM)), NA_real_)) %>%
  fill(SELF.ESTEEM_bz) %>%
  dplyr::mutate(Respect.Self_bz = if_else(Wave == "2018", (as.numeric(Respect.Self)), NA_real_)) %>%
  fill(Respect.Self_bz) %>%
  dplyr::mutate(Rumination_bz = if_else(Wave == "2018", (as.numeric(Rumination)), NA_real_)) %>%
  fill(Rumination_bz) %>%
  dplyr::mutate(ChildrenNum_bz = if_else(Wave == "2018", (as.numeric(ChildrenNum)), NA_real_)) %>%
  fill(ChildrenNum_bz) %>%
  dplyr::mutate(SELF.CONTROL_bz = if_else(Wave == "2018", (as.numeric(SELF.CONTROL)), NA_real_)) %>%
  fill(SELF.CONTROL_bz) %>%
  dplyr::mutate(inc_bz = if_else(Wave == "2018", (as.numeric(Household.INC)), NA_real_)) %>%
  fill(inc_bz) %>%
  dplyr::mutate(k6_bz = if_else(Wave == "2018", (as.numeric(KESSLER6sum)), NA_real_)) %>%
  fill(k6_bz) %>%
  dplyr::mutate(HomeOwner_bz = if_else(Wave == "2018", (as.numeric(HomeOwner)), NA_real_)) %>%
  fill(HomeOwner_bz) %>%
  dplyr::mutate(LIFEMEANING_bz = if_else(Wave == "2018", (as.numeric(LIFEMEANING)), NA_real_)) %>%
  fill(LIFEMEANING_bz) %>%
  dplyr::mutate(agr_bz = if_else(Wave == "2018", (as.numeric(AGREEABLENESS)), NA_real_)) %>%
  fill(agr_bz) %>%
  dplyr::mutate(cs_bz = if_else(Wave == "2018", (as.numeric(CONSCIENTIOUSNESS)), NA_real_)) %>%
  fill(cs_bz) %>%
  dplyr::mutate(op_bz = if_else(Wave == "2018", (as.numeric(OPENNESS)), NA_real_)) %>%
  fill(op_bz) %>%
  dplyr::mutate(h_bz = if_else(Wave == "2018", (as.numeric(HONESTY_HUMILITY)), NA_real_)) %>%
  fill(h_bz) %>%
  dplyr::mutate(ex_bz = if_else(Wave == "2018", (as.numeric(EXTRAVERSION)), NA_real_)) %>%
  fill(ex_bz) %>%
  dplyr::mutate(nr_bz = if_else(Wave == "2018", (as.numeric(NEUROTICISM)), NA_real_)) %>%
  fill(nr_bz) %>%
  dplyr::mutate(lch_bz = if_else(Wave == "2018", (as.numeric(
    log(Religion.Church + 1)
  )), NA_real_)) %>%
  fill(lch_bz) %>%
  dplyr::mutate(pol_bz = if_else(Wave == "2018", (Pol.Orient), NA_real_)) %>%
  fill(pol_bz) %>%
  # dplyr::mutate(rel_bz = if_else(Wave == "2010", (as.numeric(Religious1)), NA_real_)) %>%
  # fill(rel_bz) %>%
  dplyr::mutate(partner_bz = if_else(Wave == "2018", (as.numeric(Partner)), NA_real_)) %>%
  fill(partner_bz) %>%
  dplyr::mutate(parent_bz = if_else(Wave == "2018", (as.numeric(Parent)), NA_real_)) %>%
  fill(parent_bz) %>%
  dplyr::mutate(nzdep_bz = if_else(Wave == "2018", (NZdep), NA_real_)) %>%
  fill(nzdep_bz) %>%
  dplyr::mutate(male_2z = if_else(Wave == "2018", scale(as.numeric(Male)) /
                                    2, NA_real_)) %>%
  fill(male_2z)  %>%
  dplyr::mutate(employed_bz = if_else(Wave == "2018", (as.numeric(Employed)), NA_real_)) %>%
  fill(employed_bz) %>%
  dplyr::mutate(edu_bz = if_else(Wave == "2018", (Edu), NA_real_)) %>%
  fill(edu_bz) %>%
  dplyr::mutate(ubran_bz = if_else(Wave == "2018", (as.numeric(Urban)), NA_real_)) %>%
  fill(ubran_bz) %>%
  dplyr::mutate(EthnicCats_b = if_else(Wave == "2018", as.numeric(EthnicCats), NA_real_)) %>%
  fill(EthnicCats_b) %>%
  #  dplyr::mutate(Rel.Conflict_bz = if_else(Wave == "2018", as.numeric(Rel.Conflict ), NA_real_)) %>%
  #  fill(Rel.Conflict_bz ) %>%
  # dplyr::mutate( Rel.Satisfaction_bz = if_else(Wave == "2018", as.numeric( Rel.Satisfaction  ), NA_real_)) %>%
  # fill( Rel.Satisfaction_bz ) %>%
  dplyr::mutate(SFHEALTH_bz = if_else(Wave == "2018", as.numeric(SFHEALTH), NA_real_)) %>%
  fill(SFHEALTH_bz) %>%
  dplyr::mutate(NWI_bz = if_else(Wave == "2018", as.numeric(NWI), NA_real_)) %>%
  fill(NWI_bz) %>%
  # dplyr::mutate( LOC.HEALTH_bz = if_else(Wave == "2018", as.numeric( LOC.HEALTH   ), NA_real_)) %>%
  # fill( LOC.HEALTH_bz ) %>% # not in T10
  # dplyr::mutate( Emp.WorkLifeBalance_bz = if_else(Wave == "2018", as.numeric( Emp.WorkLifeBalance), NA_real_)) %>%
  # fill( Emp.WorkLifeBalance_bz ) %>%
  # dplyr::mutate( HLTH.Diagnose.Asthma.L2_bz = if_else(Wave == "2018", as.numeric( HLTH.Diagnose.Asthma.L2), NA_real_)) %>%
  # fill( HLTH.Diagnose.Asthma.L2_bz ) %>%
  # dplyr::mutate( HLTH.HlthCareAccess_bz = if_else(Wave == "2018", as.numeric( HLTH.HlthCareAccess), NA_real_)) %>%
  # fill( HLTH.HlthCareAccess_bz ) %>%
  # dplyr::mutate( HLTH.Diagnose.Other.L2_bz = if_else(Wave == "2018", as.numeric( HLTH.Diagnose.Other.L2), NA_real_)) %>%
  # fill( HLTH.Diagnose.Other.L2_bz ) %>%
  # dplyr::mutate( HLTH.Diagnose.BloodPressure.L2_bz = if_else(Wave == "2018", as.numeric( HLTH.Diagnose.BloodPressure.L2), NA_real_)) %>%
# fill( HLTH.Diagnose.BloodPressure.L2_bz ) %>%
# dplyr::mutate( HLTH.Diagnose.Anxiety.L2_bz = if_else(Wave == "2018", as.numeric( HLTH.Diagnose.Anxiety.L2), NA_real_)) %>%
# fill( HLTH.Diagnose.Anxiety.L2_bz ) %>%
# dplyr::mutate(  HLTH.Diagnose.Depression.L2_bz = if_else(Wave == "2018", as.numeric(  HLTH.Diagnose.Depression.L2), NA_real_)) %>%
# fill(  HLTH.Diagnose.Depression.L2_bz ) %>%
dplyr::mutate(partnerlost_job_bz = if_else(Wave == "2018", as.numeric(partnerlost_job), NA_real_)) %>%
  fill(partnerlost_job_bz) %>%
  dplyr::mutate(lost_job_bz = if_else(Wave == "2018", as.numeric(lost_job), NA_real_)) %>%
  fill(lost_job_bz) %>%
  # dplyr::mutate(  brk_relationship_bz = if_else(Wave == "2018", as.numeric( brk_relationship ), NA_real_)) %>%
  # fill(  brk_relationship_bz ) %>%
  # dplyr::mutate(  sep_relationship_bz = if_else(Wave == "2018", as.numeric( sep_relationship  ), NA_real_)) %>%
  # fill(  sep_relationship_bz ) %>%
  dplyr::mutate(began_relationship_bz = if_else(Wave == "2018", as.numeric(began_relationship), NA_real_)) %>%
  fill(began_relationship_bz) %>%
  dplyr::mutate(Hours.Work_bz = if_else(Wave == "2018", as.numeric(Hours.Work), NA_real_)) %>%
  fill(Hours.Work_bz) %>%
  # dplyr::mutate(  death_spouse_bz = if_else(Wave == "2018", as.numeric( death_spouse ), NA_real_)) %>%
  # fill(  death_spouse ) %>%
  dplyr::mutate(yearW = as.numeric(Wave) - 1) %>%
  arrange(Id, yearW) %>%
  dplyr::mutate(bg1 = as.numeric(Believe.God)) %>%
  dplyr::mutate(bs1 = as.numeric(Believe.Spirit)) %>%
  # dplyr::mutate(inc_log = log(Household.INC + 1))  %>%
  dplyr::filter(partner_bz  == 1) %>%  # need for measurement error
  dplyr::select(
    Id,
    SFHEALTH,
    HomeOwner_bz,
    SFHEALTH_bz,
    NWI_bz,
    NWI,
    Hours.Work_bz,
    # death_spouse_bz,
    # death_spouse,
    # began_relationship,
    began_relationship_bz,
    # sep_relationship_bz,
    # sep_relationship,
    # brk_relationship,
    # brk_relationship_bz,
    # Rel.Conflict_bz,
    # Rel.Satisfaction_bz,
    # Rel.Conflict,
    # Rel.Satisfaction,
    # partnerlost_job,
    partnerlost_job_bz,
    lost_job_bz,
    #lost_job,
    # HLTH.Diagnose.Anxiety.L2,
    # HLTH.Diagnose.Anxiety.L2_bz,
    # HLTH.Diagnose.BloodPressure.L2,
    # HLTH.Diagnose.BloodPressure.L2_bz,
    # HLTH.Diagnose.Other.L2_bz,
    # HLTH.Diagnose.Other.L2,
    # HLTH.Diagnose.Asthma.L2_bz,
    # HLTH.Diagnose.Asthma.L2,
    # HLTH.HlthCareAccess,
    # HLTH.HlthCareAccess_bz,
    SFHEALTH_bz,
    SFHEALTH,
    # LOC.HEALTH_bz,
    # LOC.HEALTH,
    Emp.WorkLifeBalance,
    # outcome only
    yearW,
    bg1,
    bs1,
    EthnicCats_b,
    ubran_bz,
    Religious,
    male_2z,
    nzdep_bz,
    parent_bz,
    pol_bz,
    lch_bz,
    nr_bz,
    ex_bz,
    h_bz,
    op_bz,
    cs_bz,
    agr_bz,
    k6_bz,
    inc_bz,
    Age,
    GenCohort,
    KESSLER6sum,
    Household.INC,
    LIFEMEANING_bz,
    LIFEMEANING,
    LIFESAT,
    LIFESAT_bz,
    SELF.ESTEEM,
    SELF.ESTEEM_bz,
    Respect.Self,
    Respect.Self_bz,
    Rumination,
    Rumination_bz,
    ChildrenNum,
    ChildrenNum_bz,
    PWI,
    PWI_bz,
    SELF.CONTROL,
    SELF.CONTROL_bz,
    Religion.Church,
    Wave
  ) %>%
  dplyr::arrange(Id, Wave)#

nrow(amydf)
length(unique(amydf$Id))


# multiple imputation -----------------------------------------------------

# match col
match("KESSLER6sum", names(amydf))# at 33

max(amydf$KESSLER6sum, na.rm = TRUE)
min(amydf$KESSLER6sum, na.rm = TRUE)


# first number is the column of the data, the second is the lower bound, the third is the upper bound
bdsR <- matrix(c(33, 0, 24), nrow = 1, ncol = 3)
bdsR

# you have to make the data a dataframe (can't be a tibble)
amydf = as.data.frame(amydf)


# impute data
library(Amelia)
amy_amelia  <- amelia(
  set.seed = 1234,
  amydf,
  cs = c("Id"),
  # id variable
  ts = c("yearW"),
  # time series variable -- must be numeric
  m = 10,
  # number of imputations
  #ordinal = "religious",
  idvars = c("Wave", "GenCohort"),
  noms = c(
    "EthnicCats_b",
    "Religious",
    "began_relationship_bz",
    "partnerlost_job_bz",
    "lost_job_bz",
    "HomeOwner_bz"
  ),
  # idvars=c("Id"), # yrs not working
  lags = c(
    "KESSLER6sum",
    "Household.INC",
    "LIFEMEANING",
    "LIFESAT",
    "PWI",
    "SELF.ESTEEM",
    "Respect.Self",
    "Rumination",
    "ChildrenNum",
    "SELF.ESTEEM",
    "SFHEALTH",
    "SELF.CONTROL"
  ),
  leads = c(
    "KESSLER6sum",
    "Household.INC",
    "LIFEMEANING",
    "LIFESAT",
    "PWI",
    "SELF.ESTEEM",
    "Respect.Self",
    "Rumination",
    "ChildrenNum",
    "SELF.ESTEEM",
    "SFHEALTH",
    "NWI",
    "SELF.CONTROL"
  ),
  logs = c(
    "Household.INC",
    "KESSLER6sum",
    "k6_bz",
    "inc_bz",
    "ChildrenNum",
    "Hours.Work_bz",
    "ChildrenNum_bz",
    "Religion.Church",
    "lch_bz"
  ),
  polytime = 2,
  # Allow polynomial
  # intercs = T, # often fails to converge
  bounds = bdsR,
  empri = .01 * nrow(amydf)
) # ridge prior if needed see: Amelia pdf documentation p.23

# save data
saveRDS(amy_amelia, here::here("mods", "amy_amelia.rds"))


tabinc_df <- df %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
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
  droplevels() %>%
  arrange(Id, Wave)

# check n # 34782
table1::table1(~ Standard.Living | Wave , data = tabinc_df, overall = FALSE)
# check N of ids
length(unique(tabinc_df$Id)) # 34783

## select vars
b_df <- tabinc_df %>%
  select(
    Id,
    YearMeasured,
    Wave,
    EthCat,
    Euro,
    Age,
    GendAll,
    Male,
    # SexualOrientationL1,
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
    Religion.Church2,
    Believe.Spirit,
    Believe.God,
    # Religion.Prayer2,
    # Religion.Scripture2,
    Spiritual.Identification,
    SWB.SoC01,
    EmotionRegulation1,
    EmotionRegulation2,
    EmotionRegulation3,
    Bodysat,
    VENGEFUL.RUMIN,
    retired,
    semiretired,
    BornNZ, #nor working
    KESSLER6sum,
    # hopeless_k6,
    # depressed_k6,
    #  restless_k6,
    #  effort_k6,
    #  worthless_k6,
    #  nervous_k6,
    HLTH.Fatigue,
    Rumination,
    Smoker,
    # ChildrenNum,
    NWI,
    BELONG,
    SUPPORT,
    CharityDonate,
    HoursCharity,
    GRATITUDE,
    # Volunteers,
    Hours.Work,
    Hours.Exercise,
    HLTH.Disability,
    LIFEMEANING,
    LIFESAT,
    PWI,  ##  we use the individual
    NWI,
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
    HLTH.SleepHours#,
    #  LifeMeaning01,
    #  LifeMeaning02,
    #  Pol.VotedElection,
    # Emp.JobSecure,
    # Env.ClimateChgCause,
    # Env.ClimateChgReal #,
  ) %>%
  dplyr::rename(community = SWB.SoC01,
                Religion.Church  =  Religion.Church2) |> #,
  #    Religion.Prayer = Religion.Prayer2,
  #    Religion.Scripture = Religion.Scripture2)%>%  # avoid missingness
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, EthCat, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
  arrange(Id, Wave) %>%
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
    Male = ifelse(GendAll == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1),
  ) %>%
  arrange(Id, Wave)  %>% #
  dplyr::mutate(retired_lead1 = lead(retired, n = 1)) %>%
  dplyr::mutate(semiretired_lead1 = lead(semiretired, n = 1)) %>%
  #dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::mutate(across(
    c(
      income_log,
      KESSLER6sum,
      HLTH.Disability,
      # hopeless_k6,
      # depressed_k6,
      # restless_k6,
      # effort_k6,
      # worthless_k6,
      # nervous_k6,
      HLTH.Fatigue,
      Rumination,
      community,
      SFHEALTH,
      LIFEMEANING,
      LIFESAT,
      PWI,
      Church,
      # Spiritual.Identification,
      #   Religion.Prayer, Missing  Prob only useful for religious people
      #   Religion.Scripture, Missing  Prob only useful for religious people
      Hours.Work,
      SELF.ESTEEM,
      SELF.CONTROL,
      Respect.Self,
      Alcohol.Frequency,
      Hours.Exercise,
      HLTH.Disability,
      HLTH.BMI,
      Smoker,
      # ChildrenNum,
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
      HLTH.SleepHours#,
      # LifeMeaning01,
      # LifeMeaning02,
    ),
    ~ lead(.x, n = 1),
    .names = "{col}_lead1"
  )) %>% # make leads
  dplyr::mutate(across(
    c(
      income_log,
      KESSLER6sum,
      # hopeless_k6,
      # depressed_k6,
      # restless_k6,
      # effort_k6,
      # worthless_k6,
      #  nervous_k6,
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
      # ChildrenNum,
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
      HLTH.SleepHours#,
      # LifeMeaning01,
      # LifeMeaning02,
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
  # dplyr::filter(retired != 1) %>%
  # dplyr::filter(retired_lead1 != 1) %>%  #needed for the intervention
  # dplyr::filter(semiretired != 1) %>%
  # dplyr::filter(semiretired_lead1 != 1) %>%  #needed for the intervention
  #  dplyr::filter(!is.na(Church)) %>%
  #  dplyr::filter(!is.na(Church_lead1)) %>%  #needed for the intervention
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
      YearMeasured#,
      #  org2019,
      #  hold19,
      # retired,
      #retired_lead1,
      # semiretired,
      # semiretired_lead1
    )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)
