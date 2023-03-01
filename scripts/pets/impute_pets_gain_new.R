

# labels <- c("no_pets",
#             "dogs_only",
#             "cats_only",
#             "cats_dogs",
#             "other_pets_only")
#
#


# import libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")



options(scipen = 999)

# You can find more information about the NZAVS:[HERE](https://www.psych.auckland.ac.nz/en/about/new-zealand-attitudes-and-values-study.html)

# step one: import libraries:
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")


# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# read data

# set your folder path : unique to each user
pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )

# set path to where you will push data: unique for each user

# for saving models
push_mods <-
  fs::path_expand("/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets/mods")

# for saving figures
push_figs <-
  fs::path_expand(" /Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets/figs")

pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )

# read data: note that you need use the arrow package
dat <- arrow::read_parquet(pull_path)

# check data
str(dat)

# count unique individuals
skimr::n_unique(dat$Id)

# more robust option for counting:
length(unique(dat$Id))

# participants by wave,
dat |>
  filter(YearMeasured == 1) |>
  droplevels() |>
  arrange(Wave) |>
  group_by(Wave) |>
  count()




# for ref
# dt0 <- dat %>%
#   filter(YearMeasured == 1 & Wave == 2019) %>%
#   select(REGC_2022, COVID19.Timeline) %>%
#   mutate(cum_lockdowns_baseline = case_when(
#     COVID19.Timeline < 1.2 ~ 0,
#     COVID19.Timeline > 1.2 & COVID19.Timeline < 2 ~ 2,
#     COVID19.Timeline > 2 & REGC_2022 %in% c(1, 2) ~ 3,
#     COVID19.Timeline > 2 & REGC_2022 == 4 ~ 4,
#     TRUE ~ NA_real_
#   ))


# # example
# dat |>
#   filter(Wave == 2018 & YearMeasured==1) |>
#   select(PERFECTIONISM, Gender, Id, Wave) |>
#   drop_na() |>
#   summarise(count_distinct = n_distinct(Id))
#
#
# # another example
# dat |>
#   filter(Wave == 2018 & YearMeasured==1) |>
#   select(PERFECTIONISM, Gender, Id, Wave) |>
#   mutate(Male = as.factor(Gender)) |>
#   drop_na() |>
#   ggplot(aes(x=as.factor(Gender), y=PERFECTIONISM, colour = factor(Gender))) +
#   geom_boxplot(notch = TRUE) + geom_jitter(shape=16, position=position_jitter(0.2), alpha = .1) + labs(
#     title = "Perfectionism by Gender: NZAVS years 2018-2019, N = 47823",
#     y = "Doing my best never seems to be enough.\nMy performance rarely measures up to my standards.\nI am hardly ever satisfied with my performance.",
#     x = "Male coded as 1, other identities coded as 0") + scale_color_viridis_d(option = "D")


# From jake
# From some initial testing, the following:
#   dplyr::mutate(org2018 = ifelse(Wave == 2018 & YearMeasured == 1, 1, 0)) |>
#   dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) |>
#   dplyr::filter(hold18 > 0)  # hack to enable repeat of baseline
#
# Can be simplified to:
#   dplyr::filter(any(Wave == 2018 & YearMeasured == 1))


# initial wrangling to get cohort: all those who participated in both t10 and t11
#
# dat |>
#   filter(Wave == 2015 & YearMeasured==1 | Wave == 2019 & YearMeasured==1) |>
#   select(KESSLER6, Pets,Pets.coded.catdog_factor, Id, Wave) |>
#   drop_na() |>
#   ggplot(aes(x= Pets.coded.catdog_factor, y=KESSLER6, colour = Pets.coded.catdog_factor)) +
#   facet_grid(.~Wave) +
#   geom_boxplot(notch = TRUE) +
#   geom_jitter(shape=16, position=position_jitter(0.2), alpha = .1) +
#   labs(
#     title = "Do you have any pets?",
#     y = "KESSLER6",
#     x = "'Do you have any pets? (coded)'") + scale_color_viridis_d(option = "D")
#
# test <- dat |>
#   filter(any( Wave == 2015 & YearMeasured==1)) |>
#   filter(any( Wave == 2019 & YearMeasured==1) )|>
#   select(KESSLER6, Pets,Pets.coded.catdog_factor, Id, Wave) |>
#   drop_na()
#
# n_unique(test$Id)
#
#
# test <- dat |>
#   filter(any( Wave == 2015 & YearMeasured==1) |  ( Wave == 2019 & YearMeasured==1) ) |>
#   select(KESSLER6, Pets,Pets.coded.catdog_factor, Id, Wave) |>
#   drop_na()
#
# n_unique(test$Id)
#
# #|>
#   ggplot(aes(x= Pets.coded.catdog_factor, y=KESSLER6, colour = Pets.coded.catdog_factor)) +
#   facet_grid(.~Wave) +
#   geom_boxplot(notch = TRUE) +
#   geom_jitter(shape=16, position=position_jitter(0.2), alpha = .1) +
#   labs(
#     title = "Do you have any pets?",
#     y = "KESSLER6",
#     x = "'Do you have any pets? (coded)'") + scale_color_viridis_d(option = "D")
#
#
# # create a table summary
# library(gtsummary)
#
#
# # not working
# df <- dat |>
#   filter(Wave == 2015 & YearMeasured==1 | Wave == 2019 & YearMeasured==1) |>
#   select(KESSLER6, Pets.coded.catdog_factor, Wave)
#
# # Summarize means of Kessler6 by Pets_coded and wave
# tbl_summary(df,
#             by = Pets.coded.catdog_factor,
#             missing = "no",
#             statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
#   add_p(by = Wave, p = "none")
#



dat_new <- dat |>
  arrange(Id, Wave) |>
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0),
                SexualOrientation = as.factor(if_else(
                  SexualOrientationL1 == 1,
                  "Heterosexual",
                  if_else(SexualOrientationL1 ==
                            2, "Homosexual", "OtherSexuality")
                ))) %>%
  dplyr::mutate(Gender3 = as.factor(ifelse(
    GendAll == 0,
    "Female",
    if_else(GendAll == 1, "Male", "GenderDiverse")
  ))) |>
  dplyr::mutate(Male = as.factor(ifelse(GendAll == 1,
                                        "Male", "Not_male"))) |>
  dplyr::rename(# use this for some studies
    # My life has a clear sense of purpose.
    meaning_purpose = LifeMeaning01,
    # I have a good sense of what makes my life meaningful.
    meaning_sense = LifeMeaning02) |>
  group_by(Id) %>%
  dplyr::mutate(org2015 =  ifelse(Wave == 2015 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold15 = mean(org2015, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold15 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  ungroup() %>%
  mutate(
    Pets = factor(Pets),
    pets = as.numeric(Pets),
    pets_coded = as.numeric(Pets.coded.catdog_factor) - 1
  ) |>
  dplyr::filter((Wave == 2015  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  group_by(Id) |>
  dplyr::mutate(exposed = if_else(!is.na(Pets) & !is.na(Pets.coded.catdog_factor) |
                                    Wave == 2020, 1, 0)) |> # full information on exposure
  dplyr::filter(exposed == 1) |>
  arrange(Id, Wave) |>
  dplyr::mutate(org2015 =  ifelse(Wave == 2015 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold15 = mean(org2015, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold15 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  ungroup() %>%
  arrange(Id, Wave) |>
  droplevels() |>
  mutate(time = as.numeric(Wave)-1) |>
  arrange(Id, time) |>
  droplevels()


# # example


# check no missing in the exposure -- looks good.
# missing in outcomes means censoring case by case if we use censoring approaches.
table1::table1(~ pets + Pets.coded.catdog_factor |
                 time, data = dat_new)



# check change
## Look at change
ss_pets <- dat_new |>
  select(
    Id,
    Pets,
    Pets.coded.catdog_factor,
  ) |>
  mutate(pets = as.numeric(Pets),
         pets_coded = as.numeric(Pets.coded.catdog_factor)) |>
  droplevels()



msm::statetable.msm(round(pets, 0), Id, data = ss_pets) |>
  kbl() |>
  kable_paper(full_width = F)

msm::statetable.msm(round(pets_coded, 0), Id, data = ss_pets) |>
  kbl() |>
  kable_paper(full_width = F)


# decision to use cat_2 based on evolutionary theory that 21 hours = normal for people.

# variables for baseline
table(is.na(dat_new$NZSEI13))
table(is.na(dat_new$NZSEI18))
table(is.na(dat_new$Rural_GCH2018))
table(is.na(dat_new$NZDep2018)) # USE THIS NOT A LOT OF NA


# inclusion criteria
# under 59 at baseline - help to min heterogeneity of treatment
#



# Do you have a health condition or disability that limits you, and that has lasted for 6+ months?

## select vars
dt_prep <- dat_new %>%
  dplyr::filter(Id != 9630) %>% # problematic
  select(
    Id,
    # nCensoring,
    Time,
    Wave,
    Partner,
    EthCat,
    Age,
    Gender3,
    SexualOrientation,
    SampleFrame,
    NZSEI13,
    # fewest NA
    NZDep2018,
    Rural_GCH2018,
    # fewest NA
    REGC_2022,
    # region and fewest NA
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    Edu,
    Employed,
    #   BornNZ,
    #   HomeOwner,
    Pol.Orient,
    Household.INC,
    Parent,
    Relid,
    ChildrenNum,
    #    Religion.Church,
    #   Believe.Spirit,
    #    Believe.God,
    #    Spiritual.Identification,
    #  SWB.SoC01,
    # EmotionRegulation1,
    #  EmotionRegulation2,
    #  EmotionRegulation3,
    #  Bodysat,
    #    VENGEFUL.RUMIN,
    kessler_hopeless,
    #   # …  you feel hopeless?
    kessler_depressed,
    #   #…  you feel so depressed that nothing could cheer you up?
    kessler_restless,
    #   #…  you feel restless or fidgety?
    kessler_effort,
    #   #…  you feel that everything was an effort?
    kessler_worthless,
    #   #…  you feel worthless?
    kessler_nervous,
    #…  you feel nervous?
    # KESSLER6sum,
    #   HLTH.Fatigue,
    # Rumination,
    #   retired,  selection error prone
    #   semiretired, selection error prone
    #  KESSLER6sum,
    #    Rumination,
    #    Smoker,
    ChildrenNum,
    #   NWI,
    #   BELONG,
    #   SUPPORT,
    #  CharityDonate,  # reduce missing
    #  HoursCharity, #educe missing
    #    GRATITUDE,
    # Volunteers,
    Hours.Work,
    #    HLTH.SleepHours,
    #    HLTH.Disability,
    #    Hours.Exercise,
    #  LIFEMEANING,
  #  meaning_purpose,
   # meaning_sense,
    #    LIFESAT,
    # PWI,  ##  we use the individual
    #    NWI,
    #    SFHEALTH,
    #    SELF.CONTROL,
    #    SFHEALTH,
    #   SELF.ESTEEM,
    # Respect.Self, heavy NA
    #  GenCohort,
    #   SELF.ESTEEM,
    #   SELF.CONTROL,
    #  Respect.Self,
    # Emp.WorkLifeBalance, not at baseline
    #    Alcohol.Frequency,
    #    Alcohol.Intensity,
    #    HLTH.BMI,
    #    ChildrenNum,
    # GenCohort,
    # Euro,
    # partnerlost_job, rare
    #lost_job,
    #began_relationship,
    #   SexualSatisfaction,
    #   POWERDEPENDENCE1,
    #   POWERDEPENDENCE2,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living,
    #Env.SacWilling,
    #Env.SacMade,
    #  PERFECTIONISM,
    #  PermeabilityIndividual,
    #  ImpermeabilityGroup,
    # Emp.JobSecure,
    # Env.ClimateChgCause,
    # Env.ClimateChgReal #,
  ) %>%
  #  dplyr::rename(Community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of
  arrange(Id, Wave) %>%
  # dplyr::mutate(#  Volunteers = if_else(HoursCharity == 1, 1, 0),
  #   Church = ifelse(Religion.Church > 8, 8, Religion.Church),) %>%
  arrange(Id, Wave) %>%
  dplyr::mutate(
    Edu = as.numeric(Edu),
    #   Volunteers = if_else(HoursCharity == 1, 1, 0),
    # Depressed = (as.numeric(
    #   cut(
    #     KESSLER6sum,
    #     breaks = c(-Inf, 13, Inf),
    #     labels = c("0", "1"),
    #     right = FALSE
    #   )
    # ) - 1),
    # EthCat = factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    # Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1),
  ) %>%
  select(-Household.INC) |>
  arrange(Id, Time) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  rename(time = Time) |>
  mutate(time = time - 1) |>
  select(-c(Wave)) |>
  arrange(Id)

table(dt_prep$time)

table1::table1(~ work_cat2 + kessler_worthless |
                 time, data = dt_prep)
