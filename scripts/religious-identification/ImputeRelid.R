# impute religious identification

# perfect impute
options(scipen = 999)


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

dat <- readRDS(pull_path)


# create basic outcomewide dataframe from which we will select the a small dataframe.
dat_new <- dat %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0),
                SexualOrientation = as.factor(if_else(SexualOrientationL1 == 1,
                                                      "Heterosexual",
                                                      if_else(SexualOrientationL1==2, "Homosexual", "OtherSexuality" )))) %>%
  dplyr::mutate(Gender3 = as.factor(ifelse(GendAll == 0, "Female", if_else(GendAll == 1, "Male", "GenderDiverse")))) %>%
  dplyr::rename(
    kessler_hopeless = SWB.Kessler01,
    # …  you feel hopeless?
    kessler_depressed = SWB.Kessler02,
    #…  you feel so depressed that nothing could cheer you up?
    kessler_restless  = SWB.Kessler03,
    #…  you feel restless or fidgety?
    kessler_effort = SWB.Kessler04,
    #…  you feel that everything was an effort?
    kessler_worthless = SWB.Kessler05,
    #…  you feel worthless?
    kessler_nervous = SWB.Kessler06 #…  you feel nervous?
  ) |>
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



# Check ids
length(unique(dat_new$Id)) # 34783



### ELIGIBILITY CRITERIA
# 2018/ 2019 - reported religious identification Y/n
# if yes, reported level of religious id.



## select vars
dat_prep  <- dat_new %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(Id != 9630) %>% # problematic
  select(
    Id,
    YearMeasured,
    Wave,
    Gender3,
    Partner,
    EthCat,
    Age,
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
    SDO,
    RWA,
    Urban,
    Household.INC,
    Parent,
    Relid,
    Religion.Church,
    Believe.Spirit,
    Believe.God,
    Spiritual.Identification,
    SWB.SoC01,
    EmotionRegulation1,
    EmotionRegulation2,
    EmotionRegulation3,
    Bodysat,
    VENGEFUL.RUMIN,
    retired,
    semiretired,
    BornNZ,
    KESSLER6sum,
    HLTH.Fatigue,
    Rumination,
    Smoker,
    ChildrenNum,
    NWI,
    BELONG,
    SUPPORT,
    CharityDonate,
    HoursCharity,
    GRATITUDE,
    Hours.Work,
    HLTH.SleepHours,
    HLTH.Disability,
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
    Emp.WorkLifeBalance,
    Alcohol.Frequency,
    Alcohol.Intensity,
    HLTH.BMI,
    Smoker,
    ChildrenNum,
    # GenCohort,
    partnerlost_job,
    lost_job,
    began_relationship,
    Alcohol.Intensity,
    Alcohol.Frequency,
    SexualOrientation,
    SexualSatisfaction,
    POWERDEPENDENCE1,
    POWERDEPENDENCE2,
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living,
    PERFECTIONISM,
    PermeabilityIndividual,
    ImpermeabilityGroup,
    Emp.JobSecure
  ) %>%
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of
  arrange(Id, Wave) %>%
  dplyr::mutate(
    #  Volunteers = if_else(HoursCharity == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
  ) %>%
  arrange(Id, Wave)  %>% # dplyr::mutate(Hours.Work_lead1 = lead(Hours.Work, n = 1)) %>%
  dplyr::mutate(across(
    c(
      Relid
    ),
    ~ lead(.x, n = 1),
    .names = "{col}_lead1"
  )) %>% # make leads
  dplyr::mutate(across(
    c(Relid,
      NZSEI13,
      Household.INC,
      Standard.Living,
      NZdep,
      Employed,
      Household.INC,
      community,
      Hours.Work,
      HLTH.Disability,
      # EmotionRegulation1,
      # EmotionRegulation2,
      # EmotionRegulation3,
      Bodysat,
      VENGEFUL.RUMIN,
      KESSLER6sum,
      HLTH.Fatigue,
      Rumination,
      Smoker,
      HLTH.BMI,
      BELONG,
      SUPPORT,
      CharityDonate,
      HoursCharity,
      GRATITUDE,
      Hours.Work,
      HLTH.SleepHours,
      HLTH.Disability,
      Hours.Exercise,
      LIFEMEANING,
      LIFESAT,
      # PWI, can reconstruct later
      NWI,
      SFHEALTH,
      SELF.CONTROL,
      SFHEALTH,
      SELF.ESTEEM,
      Respect.Self,
      SELF.ESTEEM,
      SELF.CONTROL,
      Emp.WorkLifeBalance,
      Alcohol.Frequency,
      Alcohol.Intensity,
      SexualSatisfaction,
      POWERDEPENDENCE1,
      POWERDEPENDENCE2,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      #  Standard.Living,
      PERFECTIONISM,
      PermeabilityIndividual,
      ImpermeabilityGroup
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
  dplyr::filter(!is.na(Relid)) %>% # min income
  dplyr::filter(!is.na(Relid_lead1)) %>% # min income
  dplyr::select(
    -c(
      Religion.Church,
      Respect.Self_lead2,  # not there
      Emp.WorkLifeBalance, # not at baseline
      YearMeasured    )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)

# number of ids
N <- length(unique(dat_prep$Id))
N  # 33001




# inspect data, with eye to large missingness
skim(dat_prep) |>
  arrange(n_missing)
# save data
saveh(dat_prep, "outcomewide-relid-dat_prep")

# read if needed
dat_prep <- readh("outcomewide-relid-dat_prep")

# mice model  -------------------------------------------------------------
library(mice)
mice_a <- dat_prep %>%
  dplyr::select(-c(Wave, Id))  # won't otherwise run

library(naniar)
naniar::gg_miss_var(mice_a)

# any col.inear vars?
mice:::find.collinear(mice_a)

# impute
ppm_mice <- mice::mice(mice_a,  seed = 0, m = 10)

# save
saveh(ppm_mice, "outcomewide-relid-ppm_mice")

# read
ppm_mice <- readh( "outcomewide-relid-ppm_mice" )

# checks
outlist2 <-
  row.names(ppm_mice)[ppm_mice$outflux < 0.5]
length(outlist2)

# checks
head(ppm_mice$loggedEvents, 10)

# data wrangling


ml <- mice::complete(ppm_mice, "long", inc = TRUE)

# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(ml)


# n ids
dat_prep <- readh("outcomewide-relid-dat_prep")
N <- length(unique(dat_prep$Id))
N
# create variables in z score

ml <- ml %>%
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(Volunteers = if_else(HoursCharity> 0, 1, 0)) |>
  dplyr::mutate(Volunteers_lead2 = if_else(HoursCharity_lead2 > 0, 1, 0)) |>
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0))) %>%
  dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0)) %>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0)) %>%
  dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0)) %>%
  dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2 + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log_lead2 = log(Alcohol.Intensity_lead2 + 1)) %>%
  dplyr::mutate(Exercise_log_lead2 = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity + 1)) %>%
  dplyr::mutate(Rumination_lead2ord = as.integer(round(Rumination_lead2, digits = 0) + 1)) %>%  # needs to start at 1
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0))) %>%
  dplyr::mutate(PERFECTIONISM_lead2ord = as.integer(round(PERFECTIONISM_lead2, digits = 0))) %>%
  dplyr::mutate(VENGEFUL.RUMIN_lead2ord = as.integer(round(VENGEFUL.RUMIN_lead2, digits = 0))) %>%
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0))) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round( Your.Personal.Relationships_lead2, digits = 0
  ) + 1)) %>%
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0))) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0) + 1)) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0) + 1)) %>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0))) %>%
  dplyr::mutate(alcohol_bin2 = if_else(Alcohol.Frequency > 3, 1, 0)) %>%
  dplyr::mutate(alcohol_bin = if_else(Alcohol.Frequency > 2, 1, 0)) %>%
  # dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) %>%
  # dplyr::mutate(income_log_lead1_z =  as.integer(Hours.Work_lead1 / 10)) %>%
  #dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1))) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2 / 10)) %>%
  dplyr::mutate(id = as.factor(rep(1:N, 11))) |> # needed for g-comp
  dplyr::group_by(id) |> mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  dplyr::group_by(id) |> mutate(PWI_lead2 = mean(
    c(
      Your.Future.Security_lead2,
      Your.Personal.Relationships_lead2,
      Your.Health_lead2,
      Standard.Living_lead2
    ),
    na.rm = TRUE
  )) |> mutate(POWERDEPENDENCE = mean(
    c(
      POWERDEPENDENCE1,
      POWERDEPENDENCE2
    ),
    na.rm = TRUE
  )) |>
  mutate(POWERDEPENDENCE_lead2 = mean(
    c(
      POWERDEPENDENCE1_lead2,
      POWERDEPENDENCE2_lead2
    ),
    na.rm = TRUE
  )) |>
  dplyr::ungroup() |>
  droplevels() |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z)) |>
  dplyr::mutate(EthCat = as.factor(EthCat),
                Gender3  = as.factor(Gender3),
                SexualOrientation  = as.factor(SexualOrientation))

# Get data into shape
ml <- ml %>% mutate_if(is.matrix, as.vector)
ml <- mice::as.mids(ml)
mf <- mice::complete(ml, "long", inc = TRUE)

#save
saveh(ml,"outcomewide-relid-ml")
saveh(mf,"outcomewide-relid-mf")

# imputed data
data_imputed <-ml

# imputed data in long format
data_long <-mf

# raw data (pre-imputation) for sensitivity analysis
data_raw <- mf |>
  slice(1:N)



# no missing use data -----------------------------------------------------
no_miss_model <- dat_prep %>%
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(Volunteers = if_else(HoursCharity> 0, 1, 0)) |>
  dplyr::mutate(Volunteers_lead2 = if_else(HoursCharity_lead2 > 0, 1, 0)) |>
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0))) %>%
  dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0)) %>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0)) %>%
  dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0)) %>%
  dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2 + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log_lead2 = log(Alcohol.Intensity_lead2 + 1)) %>%
  dplyr::mutate(Exercise_log_lead2 = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity + 1)) %>%
  dplyr::mutate(Rumination_lead2ord = as.integer(round(Rumination_lead2, digits = 0) + 1)) %>%  # needs to start at 1
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0))) %>%
  dplyr::mutate(PERFECTIONISM_lead2ord = as.integer(round(PERFECTIONISM_lead2, digits = 0))) %>%
  dplyr::mutate(VENGEFUL.RUMIN_lead2ord = as.integer(round(VENGEFUL.RUMIN_lead2, digits = 0))) %>%
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0))) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round( Your.Personal.Relationships_lead2, digits = 0
  ) + 1)) %>%
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0))) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0) + 1)) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0) + 1)) %>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0))) %>%
  dplyr::mutate(alcohol_bin2 = if_else(Alcohol.Frequency > 3, 1, 0)) %>%
  dplyr::mutate(alcohol_bin = if_else(Alcohol.Frequency > 2, 1, 0)) %>%
  # dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) %>%
  # dplyr::mutate(income_log_lead1_z =  as.integer(Hours.Work_lead1 / 10)) %>%
  #dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1))) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2 / 10)) %>%
  dplyr::group_by(Id) |> mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  dplyr::group_by(Id) |> mutate(PWI_lead2 = mean(
    c(
      Your.Future.Security_lead2,
      Your.Personal.Relationships_lead2,
      Your.Health_lead2,
      Standard.Living_lead2
    ),
    na.rm = TRUE
  )) |> mutate(POWERDEPENDENCE = mean(
    c(
      POWERDEPENDENCE1,
      POWERDEPENDENCE2
    ),
    na.rm = TRUE
  )) |>
  mutate(POWERDEPENDENCE_lead2 = mean(
    c(
      POWERDEPENDENCE1_lead2,
      POWERDEPENDENCE2_lead2
    ),
    na.rm = TRUE
  )) |>
  dplyr::ungroup() |>
  droplevels() |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  # select(-c(.imp_z, .id_z)) |>
  dplyr::mutate(EthCat = as.factor(EthCat),
                Gender3  = as.factor(Gender3),
                SexualOrientation  = as.factor(SexualOrientation))




saveh(no_miss_model,"outcomewide-relid-no-miss-model")



# EXAMPLE DEMOGRAPHIC TABLE -----------------------------------------------
data_raw1 <-
  data_raw |> dplyr::group_by(Id) |> mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  ))

data_raw1$Male <-
  factor(df_cr$Male, labels = c("No", "Yes"))
data_raw1$EthCat <-
  factor(df_cr$EthCat, labels = c("Euro", "Maori", "Pacific", "Asian"))
data_raw1$Believe.Spirit <-
  factor(df_cr$Believe.Spirit, labels = c("No", "Yes"))
data_raw1$Believe.God <-
  factor(df_cr$Believe.God, labels = c("No", "Yes"))
data_raw1$Employed <-
  factor(df_cr$Employed, labels = c("No", "Yes"))
data_raw1$Volunteers <-
  factor(df_crt$Volunteers, labels = c("No", "Yes"))
data_raw1$Parent <-
  factor(df_cr$Parent, labels = c("No", "Yes"))
data_raw1$Partner <-
  factor(df_cr$Partner, labels = c("No", "Yes"))
data_raw1$Retired <-
  factor(df_cr$retired, labels = c("No", "Yes"))
data_raw1$SemiRetired <-
  factor(df_cr$semiretired, labels = c("No", "Yes"))
data_raw1$Urban <-
  factor(df_cr$Urban, labels = c("No", "Yes"))
data_raw1$BigDoms <-
  factor(df_cr$BigDoms,
         labels = c("Buddhist", "Christian", "Muslim", "TheOthers"))
data_raw1$NeighbourhoodCommunity <- df_cr$community
data_raw1$MajorDenominations <- df_cr$BigDoms





#and continue this way to obtain factor labels ...etc.

table1::table1(
  ~ Age +
    BornNZ +
    Edu +
    Employed +
    EthCat +
    NZdep +
    NZSEI13 +
    Parent +
    Partner +
    Pol.Orient +
    Male +
    Urban,
  data = data_raw1,
  transpose = F
)


# Personality


table1::table1(
  ~ AGREEABLENESS +
    CONSCIENTIOUSNESS +
    EXTRAVERSION +
    HONESTY_HUMILITY +
    NEUROTICISM +
    OPENNESS +
    KESSLER6sum,
  data = data_raw1,
  transpose = F
)

table1::table1(
  ~ LIFESAT +
    PWI +
    Respect.Self +
    RWA +
    SDO +
    SELF.CONTROL +
    SELF.ESTEEM +
    SFHEALTH,
  data = data_raw1,
  transpose = F
)

# religious
table1::table1(
  ~ Religion.CongregationSize +
    Relid +
    Believe.Spirit +
    Believe.God +
    Church +
    Religion.Prayer +
    Religion.Scripture +
    MajorDenominations,
  data = data_raw1,
  transpose = F
)


# Social variables

table1::table1(
  ~ BELONG +
    NeighbourhoodCommunity +
    SUPPORT +
    National.Identity +
    PATRIOT,
  data = data_raw1,
  transpose = F
)


# raw data no missingness model


ml <- ml %>%
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(income_log_lead2 = log(Household.INC_lead2 + 1)) |>
  dplyr::mutate(income_log_lead1 = log(Household.INC_lead1 + 1)) |>
  dplyr::mutate(Volunteers = if_else(HoursCharity> 0, 1, 0)) |>
  dplyr::mutate(Volunteers_lead2 = if_else(HoursCharity_lead2 > 0, 1, 0)) |>
  dplyr::mutate(NZSEI13_lead1_10 = NZSEI13_lead1/10) |>
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0))) %>%
  dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0)) %>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0)) %>%
  dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0)) %>%
  dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2 + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log_lead2 = log(Alcohol.Intensity_lead2 + 1)) %>%
  dplyr::mutate(Exercise_log_lead2 = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity + 1)) %>%
  dplyr::mutate(Rumination_lead2ord = as.integer(round(Rumination_lead2, digits = 0) + 1)) %>%  # needs to start at 1
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0))) %>%
  dplyr::mutate(PERFECTIONISM_lead2ord = as.integer(round(PERFECTIONISM_lead2, digits = 0))) %>%
  dplyr::mutate(VENGEFUL.RUMIN_lead2ord = as.integer(round(VENGEFUL.RUMIN_lead2, digits = 0))) %>%
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0))) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round( Your.Personal.Relationships_lead2, digits = 0
  ) + 1)) %>%
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0))) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0) + 1)) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0) + 1)) %>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0))) %>%
  dplyr::mutate(alcohol_bin2 = if_else(Alcohol.Frequency > 3, 1, 0)) %>%
  dplyr::mutate(alcohol_bin = if_else(Alcohol.Frequency > 2, 1, 0)) %>%
  # dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) %>%
  # dplyr::mutate(income_log_lead1_z =  as.integer(Hours.Work_lead1 / 10)) %>%
  #dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1))) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2 / 10)) %>%
  dplyr::mutate(id = as.factor(rep(1:N, 11))) |> # needed for g-comp
  dplyr::group_by(id) |> mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  dplyr::group_by(id) |> mutate(PWI_lead2 = mean(
    c(
      Your.Future.Security_lead2,
      Your.Personal.Relationships_lead2,
      Your.Health_lead2,
      Standard.Living_lead2
    ),
    na.rm = TRUE
  )) |>
  dplyr::ungroup() |>
  droplevels() |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z)) |>
  dplyr::mutate(EthCat = as.factor(EthCat),
                Gender3  = as.factor(Gender3),
                SexualOrientation  = as.factor(SexualOrientation))


# MULTILEVEL DATA ---------------------------------------------------------


### Create multi-level data for comparisions
data_ml <- tab_in |>
  select(
    Id,
    YearMeasured,
    Wave,
    Partner,
    EthCat,
    Age,
    Male,
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
    SDO,
    RWA,
    Urban,
    Household.INC,
    Parent,
    Relid,
    Religion.Church,
    Believe.Spirit,
    Believe.God,
    Spiritual.Identification,
    SWB.SoC01,
    EmotionRegulation1,
    EmotionRegulation2,
    EmotionRegulation3,
    Bodysat,
    VENGEFUL.RUMIN,
    retired,
    semiretired,
    BornNZ,
    KESSLER6sum,
    HLTH.Fatigue,
    Rumination,
    Smoker,
    ChildrenNum,
    NWI,
    BELONG,
    SUPPORT,
    CharityDonate,
    HoursCharity,
    GRATITUDE,
    Hours.Work,
    HLTH.SleepHours,
    HLTH.Disability,
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
    Emp.WorkLifeBalance,
    Alcohol.Frequency,
    Alcohol.Intensity,
    HLTH.BMI,
    Smoker,
    ChildrenNum,
    # GenCohort,
    partnerlost_job,
    lost_job,
    began_relationship,
    Alcohol.Intensity,
    Alcohol.Frequency,
    SexualSatisfaction,
    POWERDEPENDENCE1,
    POWERDEPENDENCE2,
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living,
    PERFECTIONISM,
    PermeabilityIndividual,
    ImpermeabilityGroup,
    Emp.JobSecure
  ) |>
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) |>
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(Church = ifelse(Religion.Church > 8, 8, Religion.Church)) |>
  # dplyr::mutate( inc_prop = (income_log / (income_log_lead1) - 1)) |>
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Volunteers = if_else(HoursCharity > 1, 1, 0)) |>
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity + 1)) %>%
  dplyr::mutate(Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(Rumination_ord = as.integer(round(Rumination, digits = 0) + 1)) %>%  # needs to start at 1
  dplyr::mutate(SUPPORT_ord = as.integer(round(SUPPORT, digits = 0))) %>%
  dplyr::mutate(PERFECTIONISM_ord = as.integer(round(PERFECTIONISM, digits = 0))) %>%
  dplyr::mutate(VENGEFUL.RUMIN_ord = as.integer(round(VENGEFUL.RUMIN, digits = 0))) %>%
  dplyr::mutate(Standard.Living_ord = as.integer(round(Standard.Living, digits = 0))) %>%
  dplyr::mutate(Your.Personal.Relationships_ord = as.integer(round(Your.Personal.Relationships, digits = 0) + 1)) %>%
  dplyr::mutate(LIFEMEANING_ord = as.integer(round(LIFEMEANING, digits = 0))) %>%
  dplyr::mutate(HLTH.Fatigue_ord = as.integer(round(HLTH.Fatigue, digits = 0) + 1)) %>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) %>%
  # dplyr::mutate(Hours.Work_lead1_10 =  as.integer(Hours.Work_lead1 / 10)) %>%
  # dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1))) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) %>%
  dplyr::group_by(Id, Wave) |>
  dplyr::mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  # dplyr::mutate(KESSLER6sum = rowSums(across(
  #   c(
  #     kessler_hopeless,
  #     # …  you feel hopeless?
  #     kessler_depressed,
  #     #…  you feel so depressed that nothing could cheer you up?
  #     kessler_restless,
  #     #…  you feel restless or fidgety?
  #     kessler_effort,
  #     #…  you feel that everything was an effort?
  #     kessler_worthless,
#     #…  you feel worthless?
#     kessler_nervous #…  you feel nervous?
#   )
# ))) |>
ungroup() |>
  droplevels() |>
  dplyr::mutate(KESSLER6sum = round(as.integer(KESSLER6sum, 0))) %>%
  dplyr::mutate(across(!c(Id, Wave, EthCat), ~ as.numeric(.x))) %>% # make factors numeric for easy of
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  dplyr::mutate(EthCat = as.factor(EthCat))  # labels = c("Euro", "Maori", "Pacific", "Asian")



table1::table1( ~ KESSLER6sum_z |
                  Wave, data = data_ml)


saveRDS(data_ml, here::here("data","outcomewide" ,"amy", "data_ml"))
