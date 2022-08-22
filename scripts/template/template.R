# options(future.globals.maxSize = 8000 * 1024 ^ 2)  # needed
# Amy Income NZSEI STAND LIVIING
# set science digits
options(scipen = 999)
# JB STUFF
# library(fs)
#
# # import libraries (jb)
# pull_path <-
#   fs::path_expand(
#     "~/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/ldf.5"
#   )
# # import functions
# pull_path_funs  <-
#   fs::path_expand("~/The\ Virtues\ Project\ Dropbox/scripts/funs.R")
# pull_path_libs  <-
#   fs::path_expand("~/The\ Virtues\ Project\ Dropbox/scripts/libs.R")
#
# #libraries
# source(pull_path_libs)
#
# #  functions
# source(pull_path_funs)
#
# # # read data
# # dff<- readRDS(pull_path)
#
# dff <- readRDS(here::here("data_raw", "df.Rds"))


# df %>%
#   filter(Wave == 2020 &  YearMeasured == 1) %>%
#   n_distinct("Id")

# read libraries in
source(here::here("scripts", "libs.R"))
source(here::here("scripts", "funs.R"))

dat$COVID19.Timeline
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

#  This isn't sensible
# dat1 <- dat %>%
#   dplyr::mutate(NZSEI06_lead1 = lead(NZSEI06, n = 1),
#                 KESSLER6_lead1 = lead(KESSLER6, n = 1),
#                 KESSLER6_lag1 = dplyr::lag(KESSLER6),
#                 NZSEI06_lag1 =  dplyr::lag(NZSEI06),
#                 Employed_lead1 = lead(Employed, n = 1),
#                 Employed_lag1 = dplyr::lag(Employed, n = 1))|>
#   dplyr::filter(Wave == 2019 & YearMeasured==1) |>
#   dplyr::mutate(cum_lockdowns_baseline = if_else(COVID19.Timeline < 1.2, 0,
#                                  if_else(COVID19.Timeline >  1:2 & COVID19.Timeline  < 2, 2,
#                                          ifelse(COVID19.Timeline > 2 & REGC_2018 == 2  | COVID19.Timeline > 2 & REGC_2018 == 1, 4, 3))))
#
# summary(test<- lm(KESSLER6~ cum_lockdowns_baseline + KESSLER6_lag1, data = dat1))
# summary(test<- lm(NZSEI06_lead1 ~ cum_lockdowns_baseline + NZSEI06_lag1, data = dat1))
# summary(test<- glm(Employed_lead1 ~ cum_lockdowns_baseline + Employed_lag1, family = "binomial" ,  data = dat1))
# summary(test<- glm(Employed ~ cum_lockdowns_baseline + Employed_lag1, family = "binomial" ,  data = dat1))

# Code for timeline if needed
#   dplyr::mutate(cum_lockdowns_baseline = if_else(COVID19.Timeline < 1.2, 0,
# if_else(COVID19.Timeline >  1:2 & COVID19.Timeline  < 2, 2,
#         ifelse(COVID19.Timeline > 2 & REGC_2018 == 2  | COVID19.Timeline > 2 &
#                  REGC_2018 == 1, 4, 3)))) |>


# Template
# set digits = 3
options(scipen=999)

#libraries and functions
source(here::here("scripts", "libs.R"))
source(here::here("scripts", "funs.R"))

### ELIGIBILITY CRITERIA
# read data
dat <- readRDS(here::here("data_raw", "df.Rds"))

# table for participant N
tab_in <- dat %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
  # dplyr::filter(Id != 9630) %>% # problematic for income
  group_by(Id) %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  ungroup() %>%
  droplevels() %>%
  arrange(Id, Wave)
# check n # 34782

length(unique(tab_in$Id)) # 34783




# increasing rate
dat%>%
  group_by(Wave) %>%
  summarise(mean(HLTH.Disability, na.rm = TRUE))

# Do you have a health condition or disability that limits you, and that has lasted for 6+ months?
dat$Religion.Church2

## select vars
df_cr <- tab_in %>%
  # dplyr::filter(Id != 9630) %>% # problematic
  select(
    Id,
    YearMeasured,
    Wave,
    Age,
    AGREEABLENESS,
    CONSCIENTIOUSNESS,
    EXTRAVERSION,
    HONESTY_HUMILITY,
    NEUROTICISM,
    OPENNESS,
    Alcohol.Frequency,
    Alcohol.Intensity,
    began_relationship,
    BELONG,
    Believe.Spirit,
    Believe.God,
    Bodysat,
    BornNZ,
    CharityDonate,
    ChildrenNum,
    Edu,
    Emp.JobSecure,
    Euro,
    EthCat,
    # EmotionRegulation1,
    # EmotionRegulation2,
    # EmotionRegulation3,
    Employed,
    Emp.WorkLifeBalance,
    #  GenCohort,
    GRATITUDE,
    HLTH.BMI,
    HLTH.Fatigue,
    HLTH.Disability,
    HLTH.SleepHours,
    HomeOwner,
    Household.INC,
    HoursCharity,
    Hours.Exercise,
    Hours.Work,
    ImpermeabilityGroup,
    KESSLER6sum,
    LIFEMEANING,
    LIFESAT,
    lost_job,
    Male,
    NWI,
    NZdep,
    NZSEI13
    Parent,
    Partner,
    partnerlost_job,
    PERFECTIONISM,
    PermeabilityIndividual,
    Pol.Orient,
    POWERDEPENDENCE1,
    POWERDEPENDENCE2,
    Relid,
    Religion.Church2,
    Religion.Prayer2,
    Religion.Scripture2,
    Religious,
    Respect.Self,
    retired,
    RWA,
    Rumination,
    SDO,
    semiretired,
    SELF.CONTROL,
    SELF.ESTEEM,
    SexualSatisfaction,
    SFHEALTH,
    Smoker,
    Spiritual.Identification,
    Standard.Living,
    SUPPORT,
    SWB.SoC01,
    Urban,
    VENGEFUL.RUMIN,
    Your.Health,
    Your.Future.Security,
    Your.Personal.Relationships
  ) %>%
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
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
    Church = ifelse(Religion.Church2 > 8, 8, Religion.Church2),
    income_log = log(Household.INC + 1),
  ) %>%
  arrange(Id, Wave)  %>% #
  dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
  #dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%  Your.Future.Security
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::mutate(across(
    c(
      KESSLER6sum,
      HLTH.Fatigue,
      Rumination,
      community,
      SFHEALTH,
      LIFEMEANING,
      LIFESAT,
      Hours.Work,
      SELF.ESTEEM,
      SELF.CONTROL,
      Respect.Self,
      Alcohol.Frequency,
      HLTH.SleepHours,
      Hours.Exercise,
      HLTH.BMI,
      HLTH.Disability,
      Smoker,
      NWI,
      BELONG,
      SUPPORT,
      Volunteers,
      GRATITUDE,
      SexualSatisfaction,
      POWERDEPENDENCE1,
      POWERDEPENDENCE2,
      CharityDonate,
      Alcohol.Intensity,
      PERFECTIONISM,
      Bodysat,
      VENGEFUL.RUMIN,
      community,
      HONESTY_HUMILITY,
      # EmotionRegulation1,
      # EmotionRegulation2,
      # EmotionRegulation3, Every high missingness
      Emp.WorkLifeBalance,
      PermeabilityIndividual,
      ImpermeabilityGroup,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living,
      PermeabilityIndividual,
      ImpermeabilityGroup,
      NZSEI13
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
  # dplyr::filter(retired != 1) %>%
  # dplyr::filter(retired_lead1 != 1) %>%  #needed for the intervention
  # dplyr::filter(semiretired != 1) %>%
  #dplyr::filter(semiretired_lead1 != 1) %>%
  #dplyr::filter(!is.na(income_log_lead1) )%>%  #   ABOUT
  #dplyr::filter(!is.na(income_log) )%>% #  THINK ABOUT
  # dplyr::filter(Household.INC >= 30975) %>% # min income
  # dplyr::filter(income_log_lead1 > income_log) %>%
  dplyr::filter(!is.na(Church)) %>%
  dplyr::filter(!is.na(Church_lead1)) %>%
  dplyr::mutate(Religious = as.numeric(Religious)-1) |>
  dplyr::filter(Religious == 1) %>%
  #dplyr::filter(!is.na(Standard.Living) )%>%
  # dplyr::filter(!is.na(Standard.Living_lead1) )%>%
  #  dplyr::filter(semiretired_lead1 != 1) %>%  #needed for the intervention
  dplyr::select(-c(
    Religion.Church2,
    # EthCat,
    Religious,
    HoursCharity,
    Respect.Self_lead2,
    Household.INC,
    #  org2018,
    #  not_euro,
    #  not_euro_lead2,
    # hold18,
    #   Euro,
    Emp.WorkLifeBalance,
    YearMeasured,
    #HLTH.Disability_lead1,
    # org2019,
    # hold19,
    # retired,
    # semiretired,
  )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Wave) %>%
  droplevels() %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)
