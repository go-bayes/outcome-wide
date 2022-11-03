# Gratitude
# anlaysis template for pedro 18 October 20222

# set digits = 3
options(scipen = 999)

#libraries and functions
# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# for saving models  # ONLY FOR JOSEPH BULUBLIA -- SET TO YOUR PREFERRED FOLDERS
push_mods <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/mods")
push_figs <-
  fs::path_expand("~/Users/joseph/The\ Virtues\ Project\ Dropbox/outcomewide/figs")

# read data # ONLY FOR JOSEPH BULUBLIA
pull_path <-
  fs::path_expand(
    "~/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/ldf.5"
  )

# read data
dat <- readRDS(pull_path)


# initial data wrangling of time 10-12 dataset
tab_in <- dat %>%
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
  ))) %>%
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
                  (Wave == 2020 &
                     YearMeasured  != -1)
  )  %>% # Eligibility criteria
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
  arrange(Id, Wave) |>
  mutate(FOREGIVENESS = 8 - VENGEFUL.RUMIN) # reverse coded to get forgiveness


# to assess positivity
msm::statetable.msm(round(GRATITUDE, 0), Id, data = tab_in) |>
  kbl() %>%
  kable_paper(full_width = F)



# increasing rate
dat %>%
  group_by(Wave) %>%
  summarise(mean(GRATITUDE, na.rm = TRUE))

# Do you have a health condition or disability that limits you, and that has lasted for 6+ months?

## select vars
df_cr <- tab_in  |>
  #dplyr::filter(Id != 9630) %>% # problematic income
  dplyr::select(
    Id,
    YearMeasured,
    Wave,
    Partner,
    EthCat,
    Age,
    Gender3,
    SexualOrientation,
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
    Religious,
    Religion.Church,
    Believe.Spirit,
    Believe.God,
    Spiritual.Identification,
    SWB.SoC01,
    EmotionRegulation1,
    EmotionRegulation2,
    EmotionRegulation3,
    Bodysat,
    #  VENGEFUL.RUMIN,
    FOREGIVENESS,
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
    SWB.SoC01
  ) %>%
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  #  dplyr::mutate(Volunteers = if_else(HoursCharity == 1, 1, 0),
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
  arrange(Id, Wave) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) |>
  arrange(Id, Wave)  %>%
  dplyr::mutate(Church = ifelse(Religion.Church > 8, 8, Religion.Church)) %>%
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::mutate(across(c(GRATITUDE),
                       ~ lead(.x, n = 1),
                       .names = "{col}_lead1")) %>% # make leads
  dplyr::mutate(across(
    c(
      NZSEI13,
      NZdep,
      Employed,
      Household.INC,
      community, # neighbourhood connection
      Hours.Work,
      HLTH.Disability,
      EmotionRegulation1,
      EmotionRegulation2,
      EmotionRegulation3,
      Bodysat,
      FOREGIVENESS,
      #  VENGEFUL.RUMIN,
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
      SELF.ESTEEM,
      Respect.Self,
      Emp.WorkLifeBalance,
      Alcohol.Frequency,
      Alcohol.Intensity,
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
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
  dplyr::mutate(Retiredp = if_else((retired == 1 |
                                      semiretired == 1), 1, 0)) %>%
  dplyr::filter(!is.na(GRATITUDE)) %>%
  dplyr::filter(!is.na(GRATITUDE_lead1)) %>%
  dplyr::select(
    -c(
      Religion.Church,
      # EthCat,
      Religious,
      #  HoursCharity,
      Respect.Self_lead2,
      #  org2018,
      #  not_euro,
      #  not_euro_lead2,
      # hold18,
      #   Euro,
      retired,
      semiretired,
      Emp.WorkLifeBalance,
      YearMeasured
    )
  ) %>%
  # dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Wave) %>%
  droplevels() %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)

str(df_cr)

table1::table1(
  ~ GRATITUDE + GRATITUDE_lead1 +  factor(Retiredp) |
    Wave ,
  data = df_cr,
  overall = FALSE
)#11953







# Filtering retirement -- consistency and positivity assumptions
# number of ids
N <- length(unique(df_cr$Id))
N  #34764

# inspect data
skim(df_cr) |>
  arrange(n_missing)


## tables
df_cr$EthCat

df_crr <-  df_cr |>
  dplyr::mutate(Volunteers = if_else(HoursCharity > 0, 1, 0))

df_crr <- df_cr |> dplyr::group_by(Id) |> mutate(PWI = mean(
  c(
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living
  ),
  na.rm = TRUE
))

df_crr$Male <- factor(df_cr$Male, labels = c("No", "Yes"))
df_crr$EthnicIdentification <-
  factor(df_cr$EthCat, labels = c("Euro", "Maori", "Pacific", "Asian"))
df_crr$Believe.Spirit <-
  factor(df_cr$Believe.Spirit, labels = c("No", "Yes"))
df_crr$Believe.God <-
  factor(df_cr$Believe.God, labels = c("No", "Yes"))
df_crr$Employed <- factor(df_cr$Employed, labels = c("No", "Yes"))
df_crr$Volunteers <-
  factor(df_crr$Volunteers, labels = c("No", "Yes"))
df_crr$Parent <- factor(df_cr$Parent, labels = c("No", "Yes"))
df_crr$Partner <- factor(df_cr$Partner, labels = c("No", "Yes"))
df_crr$Urban <- factor(df_cr$Urban, labels = c("No", "Yes"))
df_crr$Retired_partialorfull <-
  factor(df_cr$Retiredp, labels = c("No", "Yes"))
df_crr$LostJob <-  factor(df_cr$lost_job, labels = c("No", "Yes"))
df_crr$PartnerLostJob <-
  factor(df_cr$partnerlost_job, labels = c("No", "Yes"))
df_crr$ReligiousIdentification <-  df_cr$Relid
df_crr$PoliticalOrientationRight <-  df_cr$Pol.Orient
df_crr$RightWingAuthoritarian <-  df_cr$RWA
df_crr$SocialDominanceOrientation <- df_cr$SDO
df_crr$RespectSelf_baseline <-  df_cr$Respect.Self
df_crr$Disability <-
  factor(df_cr$HLTH.Disability, labels = c("No", "Yes"))
df_crr$HomeOwner <- factor(df_cr$HomeOwner, labels = c("No", "Yes"))
df_crr$SpiritualIdentification <-  df_cr$Spiritual.Identification
df_crr$OccupationalPrestige <-  df_cr$NZSEI13



# df_crt$BigDoms <-
#   factor(df_cr$BigDoms,
#          labels = c("Buddhist", "Christian", "Muslim", "TheOthers"))
df_crr$NeighbourhoodCommunity <- df_cr$community
#df_crt$MajorDenominations <- df_cr$BigDoms





#and continue this way to obtain factor labels ...etc.

table1::table1(
  ~ Age +
    BornNZ +
    Church +
    ChildrenNum +
    Disability +
    Edu +
    Employed +
    EthnicIdentification +
    HomeOwner +
    # Household.INC +
    Hours.Work +
    LostJob +
    Male +
    NZdep +
    OccupationalPrestige +
    Parent +
    Partner +
    PoliticalOrientationRight +
    # PartnerLostJob +
    Spiritual.Identification +
    RespectSelf_baseline +
    Retiredp +
    RightWingAuthoritarian +
    SocialDominanceOrientation +
    SpiritualIdentification +
    Urban +
    AGREEABLENESS +
    CONSCIENTIOUSNESS +
    EXTRAVERSION +
    HONESTY_HUMILITY +
    NEUROTICISM +
    OPENNESS,
  data = df_crr,
  transpose = F
)


table1::table1(
  ~ AGREEABLENESS +
    CONSCIENTIOUSNESS +
    EXTRAVERSION +
    HONESTY_HUMILITY +
    NEUROTICISM +
    OPENNESS +
    KESSLER6sum,
  data = df_crr,
  transpose = F
)

table1::table1(
  ~ NeighbourhoodCommunity +
    LIFESAT +
    PWI +
    RespectSelf_baseline +
    SELF.CONTROL +
    SELF.ESTEEM +
    SFHEALTH,
  data = df_crr,
  transpose = F
)

table1::table1(
  Relid +
    Believe.Spirit +
    Believe.God +
    Church +
    Religion.Prayer +
    Religion.Scripture +
    MajorDenominations,
  data = df_crr,
  transpose = F
)


# Social variables

table1::table1(~ BELONG +
                 NeighbourhoodCommunity,
               # SUPPORT +
               # National.Identity +
               # PATRIOT,
               data = df_crr,
               transpose = F)





# mice model  -------------------------------------------------------------
library(mice)

mice_cr <- df_cr %>%
  dplyr::select(-c(Wave, Id))  # won't otherwise run

library(naniar)
naniar::gg_miss_var(mice_cr)
vis_miss(mice_cr,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(mice_cr)
str(mice_cr)
# impute
cr_mice <- mice::mice(mice_cr,  seed = 0, m = 10)

# save
saveh(cr_mice, "GRATITUDE_all_mice")

# read
cr_mice <- readh("GRATITUDE_all_mice")
# checks
outlist2 <-
  row.names(cr_mice)[cr_mice$outflux < 0.5]
length(outlist2)

# checks
head(cr_mice$loggedEvents, 10)

# data warangling
# we create two completed data sets -- the one without the missing data will be useful for
# determing causal contrasts -- which we'll describe below.

ml <- mice::complete(cr_mice, "long", inc = TRUE)

# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(ml)
# create variables in z score
N <- length(unique(df_cr$Id))
N
ml <- ml %>%
  dplyr::mutate(id = as.factor(rep(1:N, 11))) |> # needed for g-comp
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Volunteers = if_else(HoursCharity > 0, 1, 0)) |>
  plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
  dplyr::mutate(Volunteers_lead2 = if_else(HoursCharity_lead2 > 0, 1, 0)) |>
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0))) %>%
  dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0)) %>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0)) %>%
  dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0)) %>%
  dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2 + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log_lead2 = log(Alcohol.Intensity_lead2 + 1)) %>%
  dplyr::mutate(Exercise_log_lead2 = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity + 1)) %>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0) + 1)) %>%
  dplyr::mutate(Rumination_lead2ord = as.integer(round(Rumination_lead2, digits = 0) + 1)) %>%  # needs to start at 1
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0))) %>%
  dplyr::mutate(PERFECTIONISM_lead2ord = as.integer(round(PERFECTIONISM_lead2, digits = 0))) %>%
  # dplyr::mutate(VENGEFUL.RUMIN_lead2ord = as.integer(round(VENGEFUL.RUMIN_lead2, digits = 0))) %>%
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0))) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(
    Your.Personal.Relationships_lead2, digits = 0
  ) + 1)) %>%
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0))) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0) +
                                                     1)) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0) +
                                                          1)) %>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0))) %>%
  dplyr::mutate(alcohol_bin2 = if_else(Alcohol.Frequency > 3, 1, 0)) %>%
  dplyr::mutate(alcohol_bin = if_else(Alcohol.Frequency > 2, 1, 0)) %>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) %>%
  # dplyr::mutate(Hours.Work_lead1_10 =  as.integer(Hours.Work_lead1/10))%>%
  # dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1)))%>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2 / 10)) %>%
  dplyr::group_by(id) |>
  dplyr::mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  dplyr::mutate(PWI_lead2 = mean(
    c(
      Your.Future.Security_lead2,
      Your.Personal.Relationships_lead2,
      Your.Health_lead2,
      Standard.Living_lead2
    ),
    na.rm = TRUE
  )) |>
  dplyr::mutate(POWERDEPENDENCE = mean(c(POWERDEPENDENCE1,
                                         POWERDEPENDENCE2),
                                       na.rm = TRUE)) |>
  dplyr::mutate(POWERDEPENDENCE_lead2 = mean(c(
    POWERDEPENDENCE1_lead2,
    POWERDEPENDENCE2_lead2
  ),
  na.rm = TRUE)) |>
  ungroup() |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z)) |>
  dplyr::mutate(id = as.factor(rep(1:N, 11))) |> # needed for g-comp#
  dplyr::mutate(
    EthCat = as.factor(EthCat),
    Gender3  = as.factor(Gender3),
    SexualOrientation  = as.factor(SexualOrientation)
  )


# Get data into shape
ml <- ml %>% mutate_if(is.matrix, as.vector)

ml <- mice::as.mids(ml)

mf <- mice::complete(ml, "long", inc = F)

saveh(ml, "ml-gratitude_all")
saveh(mf, "mf-gratitude_all")



# no missing data set up --------------------------------------------------------

no_miss_model <- df_cr %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Volunteers = if_else(HoursCharity > 0, 1, 0)) |>
  plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
  dplyr::mutate(Volunteers_lead2 = if_else(HoursCharity_lead2 > 0, 1, 0)) |>
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0))) %>%
  dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0)) %>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0)) %>%
  dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0)) %>%
  dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2 + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log_lead2 = log(Alcohol.Intensity_lead2 + 1)) %>%
  dplyr::mutate(Exercise_log_lead2 = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity + 1)) %>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0) + 1)) %>%
  dplyr::mutate(Rumination_lead2ord = as.integer(round(Rumination_lead2, digits = 0) + 1)) %>%  # needs to start at 1
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0))) %>%
  dplyr::mutate(PERFECTIONISM_lead2ord = as.integer(round(PERFECTIONISM_lead2, digits = 0))) %>%
  # dplyr::mutate(VENGEFUL.RUMIN_lead2ord = as.integer(round(VENGEFUL.RUMIN_lead2, digits = 0))) %>%
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0))) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(
    Your.Personal.Relationships_lead2, digits = 0
  ) + 1)) %>%
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0))) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0) +
                                                     1)) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0) +
                                                          1)) %>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0))) %>%
  dplyr::mutate(alcohol_bin2 = if_else(Alcohol.Frequency > 3, 1, 0)) %>%
  dplyr::mutate(alcohol_bin = if_else(Alcohol.Frequency > 2, 1, 0)) %>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) %>%
  # dplyr::mutate(Hours.Work_lead1_10 =  as.integer(Hours.Work_lead1/10))%>%
  # dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1)))%>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2 / 10)) %>%
  dplyr::group_by(Id) |>
  dplyr::mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  dplyr::mutate(PWI_lead2 = mean(
    c(
      Your.Future.Security_lead2,
      Your.Personal.Relationships_lead2,
      Your.Health_lead2,
      Standard.Living_lead2
    ),
    na.rm = TRUE
  )) |>
  dplyr::mutate(POWERDEPENDENCE = mean(c(POWERDEPENDENCE1,
                                         POWERDEPENDENCE2),
                                       na.rm = TRUE)) |>
  dplyr::mutate(POWERDEPENDENCE_lead2 = mean(c(
    POWERDEPENDENCE1_lead2,
    POWERDEPENDENCE2_lead2
  ),
  na.rm = TRUE)) |>
  ungroup() |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  # select(-c(.imp_z, .id_z)) |>
  # dplyr::mutate(id = as.factor(rep(1:N, 11))) |> # needed for g-comp#
  dplyr::mutate(
    EthCat = as.factor(EthCat),
    Gender3  = as.factor(Gender3),
    SexualOrientation  = as.factor(SexualOrientation)
  )


saveh(no_miss_model,"outcomewide-gratitude-no-miss-model")




###### READ THIS DATA IN   #########



###############  RENAME YOUR IMPUTED DATASET  'df"  ###############  ###############  ###############
###############   IMPORANT DO THIS   ###############  ###############  ###############  ###############

df <- readh("ml-gratitude_all")
# for reviewing variables
mf <- readh("mf-gratitude_all")

############### SET YOUR EXPOSURE VARIABLE, ###############  ###############  ###############


###############   IMPORTANT SET YOUR EXPOSURE VARIABLE

X = "GRATITUDE_lead1_z"


summary(mf$GRATITUDE_lead1_z)
############### NEXT SET UP VARIABLES FOR MODELS AND GRAPHS

# You may set your label for your graphs  HERE WE STICK TO THE EXAMPLE OF WORK

#xlab = "FOREGIVENESS_lead1"  ## Monthly Church
xlab = "I have much in my life to be thankful for.\nWhen I look at the world, I don’t see much to be grateful for.\nI am grateful to a wide variety of people."

# SET THE RANGE OF THE SCALE
min = -3
max = 1


# set full range of X
x =  min:max


# range for some graphs
minmax <- paste(c(x), sep = ",")


# baseline condition. We could make it different
r = -1

# focal contrast for X
f = 1

# REQUIRED for certain model model functions
c = x

# contrast for graphs -- absolute distance from baseline
p = c(r, f) #


# Needed for E-VALUES -- how much do we move on the X scale to obtain our effect?
#delta = 4 #
delta = abs(r - f)

ylim = c(-.5, .5)  # SET AS YOU LIKE -- here, how much movement across a standard deviation unit of the outcome
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
  # "Emp.JobSecure_z",
  "EmotionRegulation1_z",
  "EmotionRegulation2_z",
  "EmotionRegulation3_z",
  #"Euro_z",
  "EthCat",
  "FOREGIVENESS_z",
  "Gender3",
  "GRATITUDE_z",
  "HomeOwner_z",
  "Hours.Exercise_log_z",
  "Hours.Work_z",
  "HLTH.BMI_z",
  "HLTH.Disability_z",
  "HLTH.Fatigue_z",
  "HLTH.SleepHours_z",
  "ImpermeabilityGroup_z",
  # "income_log_z",
  "KESSLER6sum_z",
  "LIFEMEANING_z",
  "LIFESAT_z",
  "lost_job_z",
  "NZdep_z",
  "NWI_z",
  "NZSEI13_z",
  "Parent_z",
  "Partner_z",
  "PERFECTIONISM_z",
  "PermeabilityIndividual_z",
  "Pol.Orient_z",
  "POWERDEPENDENCE_z",
  "Relid_z",
  "Retiredp_z",
  "Respect.Self_z",
  "Rumination_z",
  "SELF.CONTROL_z",
  "SELF.ESTEEM_z",
  # "semiretired",
  "SexualOrientation_z",
  "SexualSatisfaction_z",
  "SFHEALTH_z",
  "Smoker_z",
  "Spiritual.Identification_z",
  "Standard.Living_z",
  "SUPPORT_z",
  "Urban_z",
 # "VENGEFUL.RUMIN_z",
  "Volunteers_z",
  "Your.Health_z",
  "Your.Future.Security_z",
  "Your.Personal.Relationships_z"
)


#
# ### BASELINE for ML models
#
#*** Demographic
# Race
# Age
# Gender
# Marital Status
# *** Economic, Social and Political
# Income
# Education
# Employment
# Social integration Neighborhood
# Religious service attendance
# Political affiliation
### *** Health
# Self-rated health
# Number of health conditions
# Exercise
# Smoking
# Alcohol consumption
# Depression
# Happiness Loneliness
# Parental warmth Purpose/Meaning Big five personality

## STATEMENT OF "VANDERWEEL-E-VALUE FROM TYLER

# “With an observed risk ratio of RR = XX, an unmeasured confounder that was associated with both the outcome and the exposure by a risk ratio of XX -fold each, above and beyond the measured confounders, could explain away the estimate, but weaker joint confounder associations could not; to move the confidence interval to include the null, an unmeasured confounder that was associated with the outcome and the exposure by a risk ratio of XX -fold each could do so, but weaker joint confounder associations could not.”

# EVALUES FOR CONTINOUS VARS - p.448
# For a continuous outcome, with a standardized effect size “d” (obtained by dividing the mean difference on the outcome variable between exposure groups by the pooled standard deviation of the outcome) and a stan- dard error for this effect size sd , an approximate E-value can be obtained (VanderWeele and Ding, 2017) by ap- plying the approximation RR ≈ exp(0.91 × d) and then using the E-value formula above (E-value = RRobs + √RRobs(RRobs − 1)). An approximate confidence inter- val can be found using the approximation
# 􏰛exp{0.91×d −1.78×sd},exp{0.91×d +1.78×sd}􏰜

# We could include statements like this in all empirical papers


# NOTE THAT I HAVE WRITTEN WRAPPER FUNCTIONS TO AUTOMATE REPORTING OF EVALUES, ALSO TO CREATE TABLES -- YOUR WORK IS LIGHT!
# however the code is:


# round(EValue::evalues.OLS(
#   ,
#   se = ,
#   sd = sd,
#   delta = delta,
#   true = 0
# ), 3)
# round(EValue::evalues.RR(, lo =  , hi = , true = 1), 4)
#

################# BELOW THE MANY OUTCOMES!  ########################################


# HEALTH  INDICATORS ------------------------------------------------------------------
# alcohol freq ------------------------------------------------------------
#How often do you have a drink containing alcohol?
Y = "Alcohol.Frequency_lead2ord_z"
main = "Alcohol Frequency"
ylab = "Alcohol Frequency (SD)"
sub = "How often do you have a drink containing alcohol?"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
summary(pool(out_m))

## g-computation

out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct

# coef + estimate for the contrast of interest # We  will combine the coeffients
#  into a large table, later.
alcoholfreq_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
alcoholfreq_c


## table for all contrasts (exploratory )
# alcoholfreq_t <- out_ct %>%
#   # #slice(1:max) |>
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
#
# # show table
# alcoholfreq_t
# # graph
alcoholfreq_p <-
  ggplot_stglm(
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



# Alcohol.Intensity ----------------------------------------------------------
#How many drinks containing alcohol do you have on a typical day when drinking?

Y = "Alcohol.Intensity_log_lead2_z"
main = "Alcohol Intensity"
ylab = "Alcohol Intensity (SD)"
sub = "How many drinks containing alcohol do you have on a typical day when drinking?"

# mice_gaussian = function(df, X, Y, cvars) {
#   require("splines")
#   require("mice")
#   out <- with(df, glm(as.formula(paste(
#     paste(Y, "~ bs(", X , ")+"),
#     paste(cvars,
#           collapse = "+")
#   ))))
#   out
# }
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
alcoholintensity_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
alcoholintensity_c

alcoholintensity_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
alcoholintensity_p



# bmi ---------------------------------------------------------------------
# What is your height? (metres)
# What is your weight? (kg)
# Kg/(m*m)

Y = "HLTH.BMI_lead2_z"
main = "BMI"
ylab = "BMI (SD)"
sub = " What is your height? (metres)\nWhat is your weight? (kg)\nKg *1/(m*m)"


# run model
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
# summary(pool(out_m))
## contrasts
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
# g-computation - contrasts
#table
bmi_t <- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
bmi_t
bmi_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  ) #+ expand_limits(x = 0, y = 0)
bmi_p

# coef + estimate
bmi_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
bmi_c


# exercise ---------------------------------------------------------------
# Hours spent … exercising/physical activity
Y = "Hours.Exercise_lead2_log_z"
main = "Log Hours Exercise"
ylab = "Log Hours Exercise (SD)"
sub = "Hours spent … exercising/physical activity"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

#contrast
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")
max
excercise_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)

# show table
excercise_t
# graph
exercise_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
exercise_p

# coef + estimate
exercise_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
exercise_c


# sf-health ---------------------------------------------------------------
# Short-Form Subjective Health Scale (General Health Perception Subscale)
# In general, would you say your health is...
# I seem to get sick a little easier than other people.
# I expect my health to get worse.

sub = "In general, would you say your health is...\nI seem to get sick a little easier than other people.\nI expect my health to get worse."

Y = "SFHEALTH_lead2_z"
main = "SF Health"
ylab = "SF Health (SD)"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

sfhealth_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
sfhealth_t
# graph
sfhealth_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
sfhealth_p

# coef + estimate
sfhealth_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
sfhealth_c


# HLTH.Sleep --------------------------------------------------------------
#During the past month, on average, how many hours of actual sleep did you get per night?

Y = "HLTH.SleepHours_lead2_z"
main = "Hours Sleep"
ylab = "Hours Sleep (SD)"
sub = "During the past month, on average, how many hours\nof actual sleep did you get per night?"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

sleep_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
sleep_t
# graph
sleep_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
sleep_p

# coef + estimate
sleep_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
sleep_c


# smoker ------------------------------------------------------------------
#Do you currently smoke?

Y = "Smoker_lead2"
family = "binomial" # could be binomial if binary utcome is rare
main = "Smoking (RR)"
ylab = "Smoking (Risk Ratio)"
sub = "Do you currently smoke?"
# clean oven
rm(out_m)
rm(out_ct)
# bake
out_m <- mice_generalised(df = df,
                          X = X,
                          Y = Y,
                          cvars = cvars,
                          family = family)
out_m
## contrasts
out_ct <-
  pool_stglm_contrast_ratio(
    out_m,
    df = df,
    m = m,
    X = X,
    x = x,
    r = r
  )
# g-computation - contrasts
#table
smoker_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
smoker_t
smoker_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim_contrast,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  ) + expand_limits(x = 0, y = 0)
smoker_p


# coef + estimate
smoker_c <- vanderweelevalue_rr(out_ct, f)
smoker_c


### EMBODIED WELL BEING ----------------------------------------------------


# body satisfaction -------------------------------------------------------
# Am satisfied with the appearance, size and shape of my body.
Y = "Bodysat_lead2_z"
main = "Body Satisfaction"
ylab = "Body Satisfaction (SD)"
sub = "Am satisfied with the appearance,\nsize and shape of my body."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

# coef + estimate
bodysat_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
bodysat_c


bodysat_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
bodysat_t
# graph
bodysat_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
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

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
summary(pool(out_m))
## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

# coef + estimate
distress_c <- vanderweelevalue_ols(out_ct, f - min, delta, sd)
distress_c

distress_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
distress_t
# graph
distress_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
distress_p


# fatigue -----------------------------------------------------------------
#During the last 30 days, how often did.... you feel exhausted?

Y = "HLTH.Fatigue_lead2_z"
main = "Fatigue"
ylab = "Fatigue (SD)"
sub = "During the last 30 days, how often did....\nyou feel exhausted?"


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

# coef + estimate
fatigue_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
fatigue_c


fatigue_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
fatigue_t
# graph
fatigue_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
fatigue_p


# rumination --------------------------------------------------------------
# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?

Y = "Rumination_lead2ord_z"
main = "Rumination"
ylab = "Rumination (SD)"
sub = "During the last 30 days, how often did....\nyou have negative thoughts that repeated over and over?"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")


# coef + estimate
rumination_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
rumination_c

rumination_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
rumination_t
# graph
rumination_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
rumination_p


# self control ------------------------------------------------------------
#In general, I have a lot of self-control.
#I wish I had more self-discipline.
Y = "SELF.CONTROL_lead2_z"
main = "Self Control"
ylab = "Self Control (SD)"
sub = "In general, I have a lot of self-control.\nI wish I had more self-discipline."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

# coef + estimate

selfcontrol_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
selfcontrol_c

selfcontrol_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
selfcontrol_t
# graph
selfcontrol_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
selfcontrol_p


# sex satisfaction --------------------------------------------------------
# How satisfied are you with your sex life?
Y = "SexualSatisfaction_lead2_z"
main = "Sexual Satisfaction"
ylab = "Sexual Satisfaction (SD)"
sub = "How satisfied are you with your sex life?"
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

# coef + estimate
sexualsat_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
sexualsat_c

sexualsat_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
sexualsat_t
# graph
sexualsat_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
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

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

# coef + estimate
gratitude_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
gratitude_c

gratitude_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
gratitude_t
# graph
gratitude_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
gratitude_p




# perm group ------------------------------------------------------------
#The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.
Y = "ImpermeabilityGroup_lead2_z"
main = "Impermeability Group"
ylab = "Impermeability Group (SD)"
sub = "The current income gap between New Zealand Europeans and\nother ethnic groups would be very hard to change."


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

groupimperm_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
groupimperm_c

groupimperm_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
groupimperm_t
# graph
groupimperm_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
groupimperm_p


# permeability self ----------------------------------------------------------------
#I believe I am capable, as an individual\nof improving my status in society.

Y = "PermeabilityIndividual_lead2_z"
main = "Permeability of Individual"
ylab = "Permeability of Individual (SD)"
sub = "I believe I am capable, as an individual,\nof improving my status in society."


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

selfperm_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
selfperm_c


selfperm_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
selfperm_t
# graph
selfperm_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
selfperm_p

# life sat ----------------------------------------------------------------
# Satisfaction with life
# I am satisfied with my life.
# In most ways my life is close to ideal.

Y = "LIFESAT_lead2_z"
main = "Life Satisfaction"
ylab = "Life Satisfaction (SD)"
sub = "I am satisfied with my life.\nIn most ways my life is close to ideal."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

lifesat_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
lifesat_c


lifesat_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
lifesat_t
# graph
lifesat_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
lifesat_p


# life meaning ------------------------------------------------------------
# Meaning in Life
# My life has a clear sense of purpose.
# I have a good sense of what makes my life meaningful.

Y = "LIFEMEANING_lead2_z"
main = "Life Meaning"
ylab = "Life Meaning (SD)"
sub = "My life has a clear sense of purpose.\nI have a good sense of what makes my life meaningful."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

meaning_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
meaning_c
#
# meaning_t <- out_ct %>%
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
# meaning_t
# graph
meaning_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
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

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

perfect_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
perfect_c
#
# perfect_t <- out_ct %>%
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
# perfect_t
# graph
perfect_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
perfect_p

# PWI ---------------------------------------------------------
#Your health.
#Your standard of living.
#Your future security.
#Your personal relationships.
#
#
# Y = "PWI_lead2_z"
# main = "Person Wellbeing Index"
# ylab = "PWI (SD)"
# sub = "Satisfied with...\nYour health.\nYour standard of living.\nYour future security.\nYour personal relationships."
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
#     x = x,
#     r = r
#   )
# out_ct %>%
#   slice(f + 1 - min) |>
#   kbl(digits = 3, "markdown")
#
# pwi_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
# pwi_c
#
# pwi_t <- out_ct %>%
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
# pwi_t
# # graph
# pwi_p <-
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
# pwi_p



# power dependence 1 ------------------------------------------------------
# I do not have enough power or control over\nimportant parts of my life.
# Other people have too much power or control over\nimportant parts of my life

Y = "POWERDEPENDENCE_lead2_z"
main = "Power Dependence"
ylab = "Power Dependence(SD)"
sub = "I do not have enough power or control\nover important parts of my life.\nOther people have too much power or control\nover important parts of my life."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

powerdependence_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
powerdependence_c

powerdependence_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
powerdependence_t
# graph
powerdependence_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
powerdependence_p


# # power dependence 2 ------------------------------------------------------
# #Other people have too much power or control over\nimportant parts of my life.
#
# Y = "POWERDEPENDENCE2_lead2_z"
# main = "Power Dependence 2"
# ylab = "Power Dependence 2(SD)"
# sub = "Other people have too much power or control\nover important parts of my life."
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
#     x = x,
#     r = r
#   )
# out_ct %>%
#   slice(f + 1 - min) |>
#   kbl(digits = 3, "markdown")
#
# powerdependence2_c <-
#   vanderweelevalue_ols(out_ct, f - min, delta, sd)
# powerdependence2_c
#
# powerdependence2_t <- out_ct %>%
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
# powerdependence2_t
# # graph
# powerdependence2_p <-
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
# powerdependence2_p

# self esteem -------------------------------------------------------------
# Self-esteem
# On the whole am satisfied with myself.
# Take a positive attitude toward myself.
# Am inclined to feel that I am a failure.


Y = "SELF.ESTEEM_lead2_z"
main = "Self Esteem"
ylab = "Self Esteem (SD)"
sub = "On the whole am satisfied with myself.\nTake a positive attitude toward myself.\nAm inclined to feel that I am a failure."


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

selfesteem_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
selfesteem_c

selfesteem_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
selfesteem_t
# graph
selfesteem_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
selfesteem_p


#
# # veng rumination ---------------------------------------------------------
# Forgivingness versus Vengeful Rumination
# Sometimes I can't sleep because of thinking about past wrongs I have suffered.
# I can usually forgive and forget when someone does me wrong.
# I find myself regularly thinking about past times that I have been wronged.
#mf$FOREGIVENESS_lead2_z
Y = "FOREGIVENESS_lead2_z"
main = "Forgiveness"
ylab = "Forgiveness (SD)"
sub = "Sometimes I can't sleep because of thinking about\npast wrongs I have suffered.\nI can usually forgive and forget when someone does me wrong.\nI find myself regularly thinking about past times that I have been wronged."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

veng_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
veng_c

veng_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
veng_t
# graph
veng_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
veng_p




# Work-life balance -------------------------------------------------------
# note-- we have no measure currently at baseline, so less confoundign control
# I have a good balance between work and other important things in my life.

Y = "Emp.WorkLifeBalance_lead2_z"
main = "Work Life Balance"
ylab = "Work Life Balance (SD)"
sub = "I have a good balance between work and\nother important things in my life."


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")


worklife_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
worklife_c

worklife_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
worklife_t
# graph
worklife_p <-
  ggplot_stglm(
    out_ct,
    ylim = c(-1,.3),
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
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


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

belong_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
belong_c


belong_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
belong_t
# graph
belong_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
belong_p



# community ----------------------------------------------------------
#I feel a sense of community with others in my local neighbourhood.
Y = "community_lead2_z"
main = "Community"
ylab = "Community (SD)"
sub = "I feel a sense of community with others\nin my local neighbourhood."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

community_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
community_c

community_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
community_t
# graph
community_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
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


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

nwi_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
nwi_c

nwi_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
nwi_t
# graph
nwi_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
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


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")


support_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
support_c


support_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
support_t
# graph
support_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
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

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

charity_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
charity_c

charity_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
charity_t
# graph
charity_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
charity_p


# volunteers --------------------------------------------------------------
#Hours spent in activities
#Hours spent … voluntary/charitable work

Y = "Volunteers_lead2"
main = "Volunteer (RR)"
ylab = "Volunteer (Risk Ratio)"
family = "binomial" # poisson for binary outcome not rare
sub = "Hours spent … voluntary/charitable work"

# clean oven
rm(out_m)
rm(out_ct)

# fit regression model
out_m <- mice_generalised(df = df,
                          X = X,
                          Y = Y,
                          cvars = cvars,
                          family = family)
# g-computation - contrasts
out_ct <-
  pool_stglm_contrast_ratio(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
#table
out_ct

# coef + estimate
out_ct<- as.data.frame(out_ct)
volunteers_c <- vanderweelevalue_rr(out_ct, f)
volunteers_c


volunteers_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
volunteers_t
volunteers_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim_contrast,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  ) +  expand_limits(x = 0, y = 0)
volunteers_p



# log household income --------------------------------------------------------------
# #Please estimate your total household income (before tax) for the last year.
# Y = "income_log_lead2_z"
# main = "Log Income"
# ylab = "Log Income (SD)"
# sub = "Please estimate your total household income (before tax) for the last year."
# # regression
# out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
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
# income_c
# # graph
# income_p <-
#   ggplot_stglm(
#     out_ct,
#     ylim =ylim,
#     main,
#     xlab,
#     ylab,
#     min = min,
#     p = p,
#     sub = sub
#   )
# income_p
#


# HOME OWNER --------------------------------------------------------------
#Do you own your own home? (either partly or fully owned)
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


# income ------------------------------------------------------------------


# Promotion NZSEI ---------------------------------------------------------------
#Occupational prestige/status
#NZSEI06 (NZ Socio-economic index) Milne, B. J., Byun, U., & Lee, A. (2013). New Zealand socio-economic index 2006. Wellington: Statistics New Zealand.
#NZSEI13 (NZ Socio-economic index) Fahy, K. M., Lee, A., & Milne, B. J. (2017). New Zealand socio-economic index 2013. Wellington: Statistics New Zealand.
#NZSEI18 (NZ Socio-economic index) Boven, N., Shackleton, N., Bolton, L., Milne, B. (2021). The 2018 New Zealand Socioeconomic Index (NZSEI-19): A brief technical summary. Compass Research Centre.

Y = "NZSEI13_lead2_10_z"
main = "Occupational Status/10"
ylab = "Occupational Status/10"
sub = "NZ Socio-economic index 2013: Occupational Prestige"


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
nzsei_t <- out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

nzsei_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)

nzsei_c

nzsei_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
nzsei_t
# graph
nzsei_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )






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


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

standardliving_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
standardliving_c


standardliving_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
standardliving_t
# graph
standardliving_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
standardliving_p







# PWI -- YYour.Future.Security_lead2_z -----------------------------------------------


Y = "Your.Future.Security_lead2_z"
main = "Future Security"
ylab = "FutureSecurity (SD)"
sub  = "Satisfied with ...Your future security."
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

futuresecurity_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
futuresecurity_c

# graph
futuresecurity_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
futuresecurity_p



# PWI -- Your personal relationships --------------------------------------

Y = "Your.Personal.Relationships_lead2_z"
main = "Personal Relationships"
ylab = "Personal Relationships (SD)"
sub  = "Satisfied with ...Your personal relationships."
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

yourpersonalrelationships_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
yourpersonalrelationships_c

# graph
yourpersonalrelationships_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
yourpersonalrelationships_p



# Your Health -------------------------------------------------------------
Y = "Your.Health_lead2_z"
main = "Your Health"
ylab = "Your Health (SD)"
sub  = "Satisfied with ...your health."
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

yourhealth_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
yourhealth_c

# graph
yourhealth_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
yourhealth_p




## TABLE HEALTH

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
  row_spec(c(1,5),  # Bold out the lines where EVALUES do not cross zero or for ratios, 1
           bold = T,
           # color = "black",
           background = "bold")|>
  kable_minimal(full_width = F)


# TABLE EMBODIED ----------------------------------------------------------

main = "Embodied wellbeing estimands / Evalues"
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
  row_spec(c(0),  # Bold out the lines where EVALUES do not cross zero or for ratios, 1
           bold = T,
           # color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)



# TABLE REFLECTIVE WELLBEING ----------------------------------------------


main = "Reflective wellbeing estimands / Evalues"
reflect_tab <- rbind(
  gratitude_c,
  groupimperm_c,
  selfperm_c,
  lifesat_c,
  meaning_c,
  perfect_c,
  # pwi_c,
  powerdependence_c,
  selfesteem_c#,
  # veng_c
)

reflect_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(0),
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
  row_spec(c(1),
           bold = T,
           color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)


# TABLE ECONOMIC WELLBEING and Charity ------------------------------------------------

main = "Economic wellbeing estimands / Evalues"
econ_tab <- rbind(
  charity_c,
  #  homeowner_c,
  nzsei_c,
  standardliving_c,
  worklife_c,
  volunteers_c
)

econ_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(1,3,4),
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
  row_spec(c(1,2,3,4),
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
  path = here::here(here::here("figs", "figs_Gratitude")),
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
  path = here::here(here::here("figs", "figs_Gratitude")),
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
#perfect_p
pwi_p
powerdependence_p
selfesteem_p
veng_p

reflective_plots <- gratitude_p +
  groupimperm_p +
  selfperm_p +
  lifesat_p +
  meaning_p +
  perfect_p +
  # pwi_p +
  powerdependence_p +
  selfesteem_p +
  veng_p +
  plot_annotation(title = "Causal effects of income on reflective wellbeing") +
  plot_layout(guides = 'collect')

reflective_plots

# save
perfect_p

ggsave(
  reflective_plots,
  path = here::here(here::here("figs", "figs_Gratitude")),
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
  path = here::here(here::here("figs", "figs_Gratitude")),
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
nzsei_p
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
  path = here::here(here::here("figs", "figs_Gratitude")),
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
  path = here::here(here::here("figs", "figs_Gratitude")),
  width = 15,
  height = 12,
  units = "in",
  filename = "pwi_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



# tab all ---------------------------------------------------------------
main = "Gratitude on wellbeing estimands / Evalues"
Gratitude <- rbind(
  alcoholfreq_c,
  alcoholintensity_c,
  bmi_c,
  smoker_c,
  exercise_c,
  sfhealth_c,
  fatigue_c,
  sleep_c,
  rumination_c,
  distress_c,
  bodysat_c,
  sexualsat_c,
  selfcontrol_c,
 # gratitude_c,
  veng_c,
  groupimperm_c,
  selfperm_c,
  lifesat_c,
  meaning_c,
  perfect_c,
  powerdependence_c,
  selfesteem_c,
  belong_c,
  nwi_c,
  support_c,
  yourpersonalrelationships_c,
  yourhealth_c,
  standardliving_c,
  futuresecurity_c,
  charity_c,
  volunteers_c,
  nzsei_c,
  worklife_c)

Gratitude_tab <- Gratitude |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  # kable_styling() %>%
  row_spec(c(6:32),  # Bold out the lines where EVALUES do not cross zero or for ratios, 1
           bold = T,
           # color = "black",
           background = "bold")|>
  kable_minimal(full_width = F)

Gratitude_tab

#save
saveh(Gratitude_tab, "outcomewide-Gratitude-tab")

# read
Gratitude_tab <- readh("outcomewide-Gratitude-tab")

# graph all ---------------------------------------------------------------

list_outcomes_Gratitude<- c(list(alcoholfreq_p,
                                 alcoholintensity_p,
                                 bmi_p,
                                 exercise_p,
                                 sfhealth_p,
                                 fatigue_p,
                                 sleep_p,
                                 rumination_p,
                                 distress_p,
                                 bodysat_p,
                                 sexualsat_p,
                                 selfcontrol_p,
                               #  gratitude_p,
                                 veng_p,
                                 groupimperm_p,
                                 selfperm_p,
                                 lifesat_p,
                                 meaning_p,
                                 perfect_p,
                                 powerdependence_p,
                                 selfesteem_p,
                                 belong_p,
                                 nwi_p,
                                 support_p,
                                 yourpersonalrelationships_p,
                                 yourhealth_p,
                                 standardliving_p,
                                 futuresecurity_p,
                                 charity_p,
                                 #  volunteers_p,
                                 nzsei_p,
                                 worklife_p))


out_Gratitude <- bind_forestplot(list_outcomes_Gratitude)
out_Gratitude
# save
saveh(out_Gratitude, "out_Gratitude")

dev.off()
gcomp_forestplot_Gratitude <- gcomp_forestplot(out_Gratitude, title = "Outcomewide Gratitude", ylim = ylim, xlab = "Incidence Effect Gratitude (SD)")
gcomp_forestplot_Gratitude



ggsave(
  gcomp_forestplot_Gratitude,
  path = here::here(here::here("figs", "figs_Gratitude")),
  width = 12,
  height = 8,
  units = "in",
  filename = "gcomp_forestplot_Gratitude_short.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


## Risk ratio plot
out_rr_gratitude <- bind_forestplot(list(smoker_p, volunteers_p))

saveh(out_rr_gratitude, "out_rr_gratitude")

gcomp_forestplot_Gratitude_rr <-
  gcomp_forestplot_rr(out_rr_gratitude,title = "Outcomewide Gratitude RR",
                      xlab = "Incidence Effect Foregiveness (SD)",
                      ylim = c(.5,1.5) )

gcomp_forestplot_Gratitude_rr
ggsave(
  gcomp_forestplot_Gratitude_rr,
  path = here::here(here::here("figs", "figs_Gratitude")),
  width = 12,
  height = 8,
  units = "in",
  filename = "gcomp_forestplot_Gratitude_rr_short.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")
