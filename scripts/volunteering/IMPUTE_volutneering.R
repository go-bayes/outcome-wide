# Voluntering IMPUTE

set_digits = 3
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
  mutate(FOREGIVENESS = 8 - VENGEFUL.RUMIN) |>
  mutate(Volunteers = if_else(HoursCharity>0,1, 0))


# to assess positivity
msm::statetable.msm(round(Volunteers, 0), Id, data = tab_in) |>
  kbl() %>%
  kable_paper(full_width = F)



# increasing rate
dat %>%
  group_by(Wave) %>%
  summarise(mean(HoursCharity, na.rm = TRUE))

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
  dplyr::mutate(across(c(HoursCharity),
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
  dplyr::filter(!is.na(HoursCharity)) %>%
  dplyr::filter(!is.na(HoursCharity_lead1)) %>%
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
  ~ HoursCharity + HoursCharity_lead1 +  factor(Retiredp) |
    Wave ,
  data = df_cr,
  overall = FALSE
)#33148







# Filtering retirement -- consistency and positivity assumptions
# number of ids
N <- length(unique(df_cr$Id))
N  #33148

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
saveh(cr_mice, "VOLUNTEER_all_mice")

# read
cr_mice <- readh("VOLUNTEER_all_mice")
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
  dplyr::mutate(Volunteers_lead1 = if_else(HoursCharity_lead1 > 0, 1, 0)) |>
  dplyr::mutate(HoursCharity_log = log(HoursCharity + 1)) |>
  dplyr::mutate(HoursCharity_lead1= if_else(HoursCharity_lead1 > 20, 20, HoursCharity_lead1)) |>
  dplyr::mutate(HoursCharity_lead1_log = log(HoursCharity_lead1+1)) |>
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

saveh(ml, "ml-volunteers_all")
saveh(mf, "mf-volunteers_all")
