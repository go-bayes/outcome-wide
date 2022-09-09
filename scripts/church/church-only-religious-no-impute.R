religious service with no imputation

# church-use R
# set digits = 3
options(scipen = 999)

#libraries and functions
# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

conflict_prefer("pool", "mice")

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
dat %>%
  group_by(Wave) %>%
  summarise(mean(Religion.Church2, na.rm = TRUE))

# Do you have a health condition or disability that limits you, and that has lasted for 6+ months?

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
    # Emp.JobSecure, Not relevant for retired
    Euro,
    EthCat,
    EmotionRegulation1,
    EmotionRegulation2,
    EmotionRegulation3,
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
    HoursCharity,
    ImpermeabilityGroup,
    KESSLER6sum,
    LIFEMEANING,
    LIFESAT,
    lost_job,
    Male,
    NWI,
    NZdep,
    NZSEI13,
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
    Your.Personal.Relationships,
  ) %>%
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
  arrange(Id, Wave) %>%
  dplyr::mutate(
    Edu = as.numeric(Edu),
    # Volunteers = if_else(HoursCharity == 1, 1, 0),
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
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::mutate(across(
    c(
      HLTH.Fatigue,
      Rumination,
      KESSLER6sum,
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
      HoursCharity,
      HLTH.BMI,
      HLTH.Disability,
      Smoker,
      NWI,
      BELONG,
      SUPPORT,
      #   Volunteers,
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
  dplyr::mutate(Retiredp = if_else((retired == 1 |
                                      semiretired == 1), 1, 0)) %>%
  dplyr::filter(!is.na(Church)) %>%
  dplyr::filter(!is.na(Church_lead1)) %>%
  dplyr::mutate(Religious = as.numeric(Religious) - 1) |>
  dplyr::filter(Religious == 1) %>%
  dplyr::select(
    -c(
      Religion.Church2,
      # EthCat,
      Religious,
      #   HoursCharity,
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



table1::table1( ~ Church + SDO +  factor(Retiredp) |
                  Wave ,
                data = df_cr,
                overall = FALSE)#11953


# Filtering retirement -- consistency and positivity assumptions
# number of ids
N <- length(unique(df_cr$Id))
N  #11953

# inspect data
skim(df_cr) |>
  arrange(n_missing)


## tables


df_crr <-  df_cr |>
  dplyr::mutate(Volunteers = if_else(HoursCharity > 0, 1, 0)) |>
  dplyr::mutate(Volunteers_lead2 = if_else(HoursCharity_lead2 > 0, 1, 0))


df_crr <- df_crr |> dplyr::group_by(Id) |>
  mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  mutate(PWI_lead2 = mean(
    c(
      Your.Future.Security_lead2,
      Your.Personal.Relationships_lead2,
      Your.Health_lead2,
      Standard.Living_lead2
    ),
    na.rm = TRUE
  )) |>
  ungroup() |>
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  #  dplyr::mutate(Volunteers = if_else(HoursCharity > 1, 1, 0)) |>
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
  dplyr::mutate(Euro = ifelse(EthCat == 0, 1, 0)) |>
  dplyr::mutate(Retiredp = if_else((retired == 1 |
                                      semiretired == 1), 1, 0)) %>%
  dplyr::mutate(Your.Personal.Relationships_ord = as.integer(round(Your.Personal.Relationships, digits = 0) + 1)) %>%
  dplyr::mutate(LIFEMEANING_ord = as.integer(round(LIFEMEANING, digits = 0))) %>%
  dplyr::mutate(HLTH.Fatigue_ord = as.integer(round(HLTH.Fatigue, digits = 0) + 1)) %>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) %>%
  # dplyr::mutate(Hours.Work_lead1_10 =  as.integer(Hours.Work_lead1 / 10)) %>%
  # dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1))) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) |>
  droplevels() |>
  dplyr::mutate(KESSLER6sum = round(as.integer(KESSLER6sum, 0))) %>%
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0))) %>%
  dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0)) %>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0)) %>%
  dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0)) %>%
  dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1)) %>%
  plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
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
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) #%>%





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
df_crr$Retired <- factor(df_cr$retired, labels = c("No", "Yes"))
df_crr$SemiRetired <-
  factor(df_cr$semiretired, labels = c("No", "Yes"))
df_crr$Urban <- factor(df_cr$Urban, labels = c("No", "Yes"))
df_crr$Retired_partialorfull <-
  factor(df_cr$Retiredp labels = c("No", "Yes"))
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
    PartnerLostJob +
    Spiritual.Identification +
    RespectSelf_baseline +
    Retired +
    RightWingAuthoritarian +
    SocialDominanceOrientation +
    SemiRetired +
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

table1::table1(
  ~ BELONG +
    NeighbourhoodCommunity +
    SUPPORT +
    National.Identity +
    PATRIOT,
  data = df_crr,
  transpose = F
)



# cvars model  -------------------------------------------------------------

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
  "Euro_z",
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
  "Male_z",
  "NZdep_z",
  "NWI_z",
  "NZSEI13_z",
  "Parent_z",
  "Partner_z",
  "PERFECTIONISM_z",
  "PermeabilityIndividual_z",
  "Pol.Orient_z",
  "POWERDEPENDENCE1_z",
  "POWERDEPENDENCE2_z",
  "Relid_z",
  "Respect.Self_z",
  "Retiredp_z",
  "Rumination_z",
  "RWA_z",
  "SDO_z",
  "SELF.CONTROL_z",
  "SELF.ESTEEM_z",
  "SexualSatisfaction_z",
  "SFHEALTH_z",
  "Smoker_z",
  "Spiritual.Identification_z",
  "Standard.Living_z",
  "SUPPORT_z",
  "Urban_z",
  "VENGEFUL.RUMIN_z",
  "Volunteers_z",
  "Your.Health_z",
  "Your.Future.Security_z",
  "Your.Personal.Relationships_z"
)




###############  RENAME YOUR IMPUTED DATASET  'df"  ###############  ###############  ###############
###############   IMPORANT DO THIS   ###############  ###############  ###############  ###############

df <- df_crr

############### SET YOUR EXPOSURE VARIABLE, ###############  ###############  ###############

## HERE WE USE THE EXAMPLE OF HOURS WORK / 10
###############   IMPORTANT SET YOUR EXPOSURE VARIABLE

X = "Church_lead1"


############### NEXT SET UP VARIABLES FOR MODELS AND GRAPHS

# You may set your label for your graphs  HERE WE STICK TO THE EXAMPLE OF WORK

xlab = "Church_lead1"  ## Monthly Church


# SET THE RANGE OF religious service FROM ZERO TO 80
min = 0
max = 6


# set full range of X
x =  min:max


# range for some graphs
minmax <- paste(c(x), sep = ",")


# baseline condition here is 20 hours of work.  We could make it different
r = 0

# focal contrast for X  Someone who goes from 20 to 60 hours of work.
f = 4

# REQUIRED for certain model model functions
c = x

# contrast for graphs -- absolute distance from baseline
p = c(r, f) #


# Needed for E-VALUES -- how much do we move on the X scale to obtain our effect?
#delta = 4 #
delta = abs(r - f)

ylim = c(-.25, .4)  # SET AS YOU LIKE -- here, how much movement across a standard deviation unit of the outcome
ylim_contrast = c(0, 3)  # SET AS YOU LIKE (FOR CONTRASTS )

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
  #"Emp.JobSecure_z",
  # "EmotionRegulation1_z",
  # "EmotionRegulation2_z",
  # "EmotionRegulation3_z",
  #"Euro_z",
  "EthCat",
  "GRATITUDE_z",
  "HomeOwner_z",
  "Hours.Exercise_log_z",
  "Hours.Work_10_z",
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
  "Male_z",
  "NZdep_z",
  "NWI_z",
  "NZSEI13_z",
  "Parent_z",
  "Partner_z",
  "PERFECTIONISM_z",
  "PermeabilityIndividual_z",
  "Pol.Orient_z",
  "POWERDEPENDENCE1_z",
  "POWERDEPENDENCE2_z",
  "Relid_z",
  "Respect.Self_z",
  "retired",
  "Rumination_z",
  "SELF.CONTROL_z",
  "SELF.ESTEEM_z",
  "semiretired",
  "SexualSatisfaction_z",
  "SFHEALTH_z",
  "Smoker_z",
  "Spiritual.Identification_z",
  "Standard.Living_z",
  "SUPPORT_z",
  "Urban_z",
  "VENGEFUL.RUMIN_z",
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

# functions ---------------------------------------------------------------



glm_nomi = function(X, Y, df, cvars, family = family) {
  # requires that a MATCH THEM dataset is converted to a mice object
  # weights must be called "weights)
  require("splines")
  out_m <- glm(as.formula(paste(
    paste(Y, "~ bs(", X , ")+"),
    paste(cvars, collapse = "+")
  )), family = family, data = df)
  return(out_m)
}

vanderweelevalue_ols_nomi = function(out_ct, f, delta, sd) {
  coef <- round(out_ct, 3)  |>  slice(f + 1)
  evalout <-
    as.data.frame(round(
      EValue::evalues.OLS(
        coef[1, 1],
        se = coef[1, 2],
        sd = 1,
        delta = delta,
        true = 0
      ),
      3
    ))
  evalout2 <- subset(evalout[2, ])
  evalout2
  evalout3 <- evalout2 |>
    select_if( ~ !any(is.na(.)))
  evalout3
  colnames(evalout3) <- c("E-value", "threshold")
  evalout3
  tab <- round(cbind.data.frame(coef, evalout3), 3)
  rownames(tab) <- main
  return(tab)
}

vanderweelevalue_rr_nomi = function(out_ct, f) {
  require("EValue")
  coef <- round(out_ct, 3)  |>  slice(f + 1)
  evalout <-
    as.data.frame(round(EValue::evalues.RR(
      coef[1, 1] ,
      lo =  coef[1, 4],
      hi = coef[1, 3],
      true = 1
    ), 3))
  evalout2 <- subset(evalout[2, ])
  evalout3 <- evalout2 |>
    select_if( ~ !any(is.na(.)))
  colnames(evalout3) <- c("E-value", "threshold")
  tab <- cbind.data.frame(coef, evalout3)
  rownames(tab) <- c(main)
  return(tab)
}

# plots
ggplot_stglm_nomi <-
  function(out_ct, ylim, main, xlab, ylab, min, p, sub) {
    require(ggplot2)
    out <-  out_ct
    out$row <- 1:nrow(out)
    out <- out |> dplyr::rename(est = "Estimate",
                                li = "lower.0.95",
                                ui = "upper.0.95",
                                se = "Std..Error")
    g1 <- out[match(p, x), ]
    g1
    ggplot2::ggplot(out, aes(x = row, y = est)) +
      geom_point() +
      geom_pointrange(aes(ymin =  li, ymax = ui), colour = "darkgray")  +
      scale_y_continuous(limits = ylim) +
      labs(
        title = main,
        subtitle = sub,
        x = xlab,
        y = ylab
      ) +
      geom_pointrange(data = g1, aes(ymin = li, ymax = ui), colour = "red") +  # highlight contrast
      theme_classic()
  }




vanderweelevalue_rr_nomi = function(out_ct, f) {
  require("EValue")
  coef <- round(out_ct, 3) |>  slice(f + 1)
  evalout <-
    as.data.frame(round(EValue::evalues.RR(
      coef[1, 1] ,
      lo =  coef[1, 3],
      hi = coef[1, 4],
      true = 1
    ), 3))
  evalout2 <- subset(evalout[2, ])
  evalout3 <- evalout2 |>
    select_if( ~ !any(is.na(.)))
  colnames(evalout3) <- c("E-value", "threshold")
  tab <- cbind.data.frame(coef, evalout3)
  rownames(tab) <- c(main)
  return(tab)
}

# HEALTH  INDICATORS ------------------------------------------------------------------
# alcohol freq ------------------------------------------------------------
#How often do you have a drink containing alcohol?
Y = "Alcohol.Frequency_lead2ord_z"
main = "Alcohol Frequency"
ylab = "Alcohol Frequency (SD)"
sub = "How often do you have a drink containing alcohol?"
# regression

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

alcoholfreq_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
alcoholfreq_c
# graph
alcoholfreq_p <-
  ggplot_stglm_nomi(
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
#-----------------------------------#How many drinks containing alcohol do you have on a typical day when drinking?

Y = "Alcohol.Intensity_log_lead2_z"
main = "Alcohol Intensity"
ylab = "Alcohol Intensity (SD)"
sub = "How many drinks containing alcohol do you have on a typical day when drinking?"

# coef + estimate

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

alcoholintensity_c <-
  vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)


# graph
alcoholintensity_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
alcoholintensity_c
alcoholintensity_p



# bmi ---------------------------------------------------------------------
# What is your height? (metres)
# What is your weight? (kg)
# Kg/(m*m)

Y = "HLTH.BMI_lead2_z"
main = "BMI"
ylab = "BMI (SD)"
sub = "What is your height? (metres)\nWhat is your weight? (kg)\nKg *1/(m*m)"


out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))


bmi_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
bmi_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

bmi_p

# exercise ---------------------------------------------------------------
# Hours spent … exercising/physical activity
Y = "Hours.Exercise_lead2_log_z"
main = "Log Hours Exercise"
ylab = "Log Hours Exercise (SD)"
sub = "Hours spent … exercising/physical activity"


out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

excercise_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
excercise_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

excercise_p
excercise_c

# sf-health ---------------------------------------------------------------
# Short-Form Subjective Health Scale (General Health Perception Subscale)
# In general, would you say your health is...
# I seem to get sick a little easier than other people.
# I expect my health to get worse.

Y = "SFHEALTH_lead2_z"
main = "SF Health"
ylab = "SF Health (SD)"
sub = "In general, would you say your health is...\nI seem to get sick a little easier than other people.\nI expect my health to get worse."



out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))


sfhealth_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
sfhealth_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
sfhealth_c
sfhealth_p

# HLTH.Sleep --------------------------------------------------------------
#During the past month, on average, how many hours of actual sleep did you get per night?

Y = "HLTH.SleepHours_lead2_z"
main = "Hours Sleep"
ylab = "Hours Sleep (SD)"
sub = "During the past month, on average, how many hours\nof actual sleep did you get per night?"

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

sleep_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
sleep_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

sleep_p


# smoker ------------------------------------------------------------------
#Do you currently smoke?

Y = "Smoker_lead2"
family = "binomial" # could be binomial if binary utcome is rare
main = "Smoking (RR)"
ylab = "Smoking (Risk Ratio)"
sub = "Do you currently smoke?"


out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "binomial",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "ratio"
  )))


smoker_c <- vanderweelevalue_rr_nomi(out_ct, f)
smoker_c


# graph
smoker_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim_contrast,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

smoker_p

### EMBODIED WELL BEING ----------------------------------------------------


# body satisfaction -------------------------------------------------------
# Am satisfied with the appearance, size and shape of my body.
Y = "Bodysat_lead2_z"
main = "Body Satisfaction"
ylab = "Body Satisfaction (SD)"
sub = "Am satisfied with the appearance,\nsize and shape of my body."

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))


bodysat_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
bodysat_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
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

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

distress_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
distress_p <- ggplot_stglm_nomi(
  out_ct,
  ylim = ylim,
  main = main
  ,
  xlab,
  ylab,
  min = min,
  p = p,
  sub = sub
)
distress_c
distress_p



# fatigue -----------------------------------------------------------------
#During the last 30 days, how often did.... you feel exhausted?

Y = "HLTH.Fatigue_lead2_z"
main = "Fatigue"
ylab = "Fatigue (SD)"
sub = "During the last 30 days, how often did....\nyou feel exhausted?"

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

fatigue_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
fatigue_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

fatigue_c
fatigue_p



# rumination --------------------------------------------------------------
# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?

Y = "Rumination_lead2ord_z"
main = "Rumination"
ylab = "Rumination (SD)"
sub = "During the last 30 days, how often did....\nyou have negative thoughts that repeated over and over?"

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

rumination_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
rumination_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

rumination_p

rumination_c


# self control ------------------------------------------------------------
#In general, I have a lot of self-control.
#I wish I had more self-discipline.
Y = "SELF.CONTROL_lead2_z"
main = "Self Control"
ylab = "Self Control (SD)"
sub = "In general, I have a lot of self-control.\nI wish I had more self-discipline."

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

selfcontrol_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
selfcontrol_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

selfcontrol_c
selfcontrol_p



# sex satisfaction --------------------------------------------------------
# How satisfied are you with your sex life?
Y = "SexualSatisfaction_lead2_z"
main = "Sexual Satisfaction"
ylab = "Sexual Satisfaction (SD)"
sub = "How satisfied are you with your sex life?"


out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

sexualsat_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
sexualsat_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

sexualsat_c
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

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

gratitude_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
gratitude_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

gratitude_c
gratitude_p





# perm group ------------------------------------------------------------
#The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.
Y = "ImpermeabilityGroup_lead2_z"
main = "Impermeability Group"
ylab = "Impermeability Group (SD)"
sub = "The current income gap between New Zealand Europeans and\nother ethnic groups would be very hard to change."

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

groupimperm_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
groupimperm_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

groupimperm_c
groupimperm_p


# permeability self ----------------------------------------------------------------
#I believe I am capable, as an individual\nof improving my status in society.

Y = "PermeabilityIndividual_lead2_z"
main = "Permeability of Individual"
ylab = "Permeability of Individual (SD)"
sub = "I believe I am capable, as an individual,\nof improving my status in society."


out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

selfperm_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
selfperm_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

selfperm_c
selfperm_p

# life sat ----------------------------------------------------------------
# Satisfaction with life
# I am satisfied with my life.
# In most ways my life is close to ideal.

Y = "LIFESAT_lead2_z"
main = "Life Satisfaction"
ylab = "Life Satisfaction (SD)"
sub = "I am satisfied with my life.\nIn most ways my life is close to ideal."

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

lifesat_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
lifesat_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

lifesat_c
lifesat_p


# life meaning ------------------------------------------------------------
# Meaning in Life
# My life has a clear sense of purpose.
# I have a good sense of what makes my life meaningful.

Y = "LIFEMEANING_lead2_z"
main = "Life Meaning"
ylab = "Life Meaning (SD)"
sub = "My life has a clear sense of purpose.\nI have a good sense of what makes my life meaningful."

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

meaning_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
meaning_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

meaning_c
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

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

perfect_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
perfect_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

perfect_c
perfect_p

# PWI ---------------------------------------------------------
#Your health.
#Your standard of living.
#Your future security.
#Your personal relationships.


Y = "PWI_lead2_z"
main = "Person Wellbeing Index"
ylab = "PWI (SD)"
sub = "Satisfied with...\nYour health.\nYour standard of living.\nYour future security.\nYour personal relationships."

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

pwi_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
pwi_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


pwi_c
pwi_p



# power dependence 1 ------------------------------------------------------
# I do not have enough power or control over\nimportant parts of my life.
Y = "POWERDEPENDENCE1_lead2_z"
main = "Power Dependence 1"
ylab = "Power Dependence 1(SD)"
sub = "I do not have enough power or control\nover important parts of my life."

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

powerdependence1_c <-
  vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
powerdependence1_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

powerdependence1_c
powerdependence1_p



# power dependence 2 ------------------------------------------------------
#Other people have too much power or control over\nimportant parts of my life.

Y = "POWERDEPENDENCE2_lead2_z"
main = "Power Dependence 2"
ylab = "Power Dependence 2(SD)"
sub = "Other people have too much power or control\nover important parts of my life."

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

powerdependence2_c <-
  vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
powerdependence2_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

powerdependence2_c
powerdependence2_p




# self esteem -------------------------------------------------------------
# Self-esteem
# On the whole am satisfied with myself.
# Take a positive attitude toward myself.
# Am inclined to feel that I am a failure.


Y = "SELF.ESTEEM_lead2_z"
main = "Self Esteem"
ylab = "Self Esteem (SD)"
sub = "On the whole am satisfied with myself.\nTake a positive attitude toward myself.\nAm inclined to feel that I am a failure."




out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

selfesteem_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
selfesteem_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

selfesteem_c
selfesteem_p



# veng rumination ---------------------------------------------------------
# Forgivingness versus Vengeful Rumination
# Sometimes I can't sleep because of thinking about past wrongs I have suffered.
# I can usually forgive and forget when someone does me wrong.
# I find myself regularly thinking about past times that I have been wronged.

Y = "VENGEFUL.RUMIN_lead2_z"
main = "Vengefulness (anti-Foregiveness)"
ylab = "Vengefulness (anti-Foregiveness) (SD)"
sub = "Sometimes I can't sleep because of thinking about\npast wrongs I have suffered.\nI can usually forgive and forget when someone does me wrong.\nI find myself regularly thinking about past times that I have been wronged."



out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

veng_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
veng_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

veng_c
veng_p




# Work-life balance -------------------------------------------------------
# note-- we have no measure currently at baseline, so less confoundign control
# I have a good balance between work and other important things in my life.

Y = "Emp.WorkLifeBalance_lead2_z"
main = "Work Life Balance"
ylab = "Work Life Balance (SD)"
sub = "I have a good balance between work and\nother important things in my life."


out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

worklife_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
worklife_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

worklife_c
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

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

belong_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
belong_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

belong_c
belong_p



# community ----------------------------------------------------------
#I feel a sense of community with others in my local neighbourhood.
Y = "community_lead2_z"
main = "Community"
ylab = "Community (SD)"
sub = "I feel a sense of community with others\nin my local neighbourhood."

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

community_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
community_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

community_c
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

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

nwi_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
nwi_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

nwi_c
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


out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

support_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
support_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

support_c
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


out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

charity_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
charity_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

charity_c
charity_p


# volunteers --------------------------------------------------------------
#Hours spent in activities
#Hours spent … voluntary/charitable work

Y = "Volunteers_lead2"
main = "Volunteer (RR)"
ylab = "Volunteer (Risk Ratio)"
family = "binomial" # binary outcome  rare
sub = "Hours spent … voluntary/charitable work"


out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "binomial",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "ratio"
  )))

out_ct
volunteers_c <- vanderweelevalue_rr_nomi(out_ct, f)
volunteers_c

# graph
volunteers_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim_contrast,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

volunteers_p

volunteers_c
volunteers_p


#
# # log household income --------------------------------------------------------------
# # Please estimate your total household income (before tax) for the last year.
# Y = "income_lead2_log_z"
# main = "Log Income"
# ylab = "Log Income (SD)"
# sub = "Please estimate your total household income (before tax) for the last year."
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
# # graph
# income_p <-
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
# income_p


#
# # HOME OWNER --------------------------------------------------------------
# #Do you own your own home? (either partly or fully owned)
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
#

# Promotion NZSEI ---------------------------------------------------------------
#Occupational prestige/status
#NZSEI06 (NZ Socio-economic index) Milne, B. J., Byun, U., & Lee, A. (2013). New Zealand socio-economic index 2006. Wellington: Statistics New Zealand.
#NZSEI13 (NZ Socio-economic index) Fahy, K. M., Lee, A., & Milne, B. J. (2017). New Zealand socio-economic index 2013. Wellington: Statistics New Zealand.
#NZSEI18 (NZ Socio-economic index) Boven, N., Shackleton, N., Bolton, L., Milne, B. (2021). The 2018 New Zealand Socioeconomic Index (NZSEI-19): A brief technical summary. Compass Research Centre.

Y = "NZSEI13_lead2_10_z"
main = "Occupational Status/10"
ylab = "Occupational Status/10"
sub = "NZ Socio-economic index 2013: Occupational Prestige"


out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

nzsei_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
nzsei_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

nzsei_c
nzsei_p

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



out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

standardliving_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
standardliving_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


standardliving_c
standardliving_p



# Future Security  ------------------------------------------------------------
# Part of pwi
# Personal Wellbeing Index
# Your health.
# Your standard of living.
# Your future security.
# Your personal relationships.

Y = "Your.Future.Security_lead2_z"
main = "Your Future Security"
ylab = "Your Future Security (SD)"
sub  = "Satisfied with ...Your Future Security."


out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

yourfuturesecurity_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
yourfuturesecurity_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

yourfuturesecurity_c
yourfuturesecurity_p



## TABLE HEALTH

# Your Health -------------------------------------------------------------


Y = "Your.Health_lead2_z"
main = "Your Health"
ylab = "Your Health (SD)"
sub  = "Satisfied with ...Your Health."

out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

yourhealth_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
yourhealth_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

yourhealth_c
yourhealth_p




# Your Personal Relationships ---------------------------------------------

Y = "Your.Personal.Relationships_lead2_z"
main = "YourPersonal Relationships"
ylab = "Your Personal Relationships (SD)"
sub  = "Satisfied with ...Your Personal Relationships"



out_m <-
  glm_nomi(
    df = df,
    X = X,
    Y = Y,
    family = "gaussian",
    cvars = cvars
  )
out_c <- stdGlm(out_m, df, X, x)
out_ct <-
  data.frame(print(summary(
    out_c, reference = r, contrast = "difference"
  )))

yourpersonalrelationships_c <- vanderweelevalue_ols_nomi(out_ct, f - min, delta, sd)
yourpersonalrelationships_p <-
  ggplot_stglm_nomi(
    out_ct,
    ylim = ylim,
    main = main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


yourpersonalrelationships_c
yourpersonalrelationships_p





# TABLE  HEALTH  -----------------------------------------------
main = "Health behaviours / Evalues"

h_tab <- rbind(alcoholfreq_c,
               alcoholintensity_c,
               bmi_c,
               excercise_c,
               sfhealth_c,
               sleep_c,
               smoker_c)

h_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  #kable_styling() %>%
  row_spec(c(1),  # Bold out the lines where EVALUES do not cross zero or for ratios, 1
           bold = T,
           # color = "black",
           background = "bold") |>
  row_spec(c(2),  # Bold out the lines where EVALUES do not cross zero or for ratios, 1
           bold = T,
            color = "blue",
           background = "bold") |>
  kable_minimal(full_width = F)




# TABLE EMBODIED ----------------------------------------------------------

main = "Embodied wellbeing / Evalues"
embody_tab <- rbind(
  bodysat_c,
  distress_c,
  fatigue_c,
  rumination_c,
  selfcontrol_c,
  sexualsat_c
)


embody_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  #kable_styling() %>%
  row_spec(c(4),  # Bold out the lines where EVALUES do not cross zero or for ratios, 1
           bold = T,
           # color = "black",
           background = "bold")
  kable_minimal(full_width = F)



# TABLE REFLECTIVE WELLBEING ----------------------------------------------


main = "Reflective wellbeing / Evalues"
reflect_tab <- rbind(
  gratitude_c,
  groupimperm_c,
  selfperm_c,
  lifesat_c,
  meaning_c,
  perfect_c,
  pwi_c,
  powerdependence1_c,
  powerdependence2_c,
  selfesteem_c,
  veng_c
)

reflect_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  # kable_styling() %>%
  row_spec(c(5, 7,  11),
           bold = T,
           color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)



# TABLE SOCIAL WELLBEING --------------------------------------------------

main = "Social wellbeing / Evalues"
social_tab <- rbind(belong_c,
                    community_c,
                    nwi_c,
                    support_c)

social_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  # kable_styling() %>%
  row_spec(c(1, 4),
           bold = T,
           color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)


# TABLE ECONOMIC WELLBEING and Charity ------------------------------------------------

main = "Economic wellbeing and behaviours / Evalues"
econ_tab <- rbind(#  income_c,
  charity_c,
  #  homeowner_c,
  nzsei_c,
  standardliving_c,
  worklife_c,
  volunteers_c)

econ_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  # kable_styling() %>%
  row_spec(c(1, 5),
           bold = T,
           color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)

# GRAPHS EMBODIED --------------------------------------------
embody_plots <-
  bodysat_p +
  distress_p +
  fatigue_p +
  rumination_p +
  selfcontrol_p +
  sleep_p +
  sexualsat_p + plot_annotation(title = "Causal effects of religious service on embodied wellbeing", #subtitle = "xyz",
                                tag_levels = "A") +
  plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)

embody_plots

ggsave(
  embody_plots,
  path = here::here(here::here("figs", "figs_church", "standardised", "noimpute")),
  width = 16,
  height = 12,
  units = "in",
  filename = "embody_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


# GRAPHS HEALTH -----------------------------------------------------------

health_plots <- alcoholfreq_p +
  alcoholintensity_p +
  bmi_p +
  exercise_p +
  smoker_p +
  sfhealth_p +
  plot_annotation(title = "Causal effects of religious service on health outcomes",
                  # subtitle = "xyz",
                  tag_levels = "A") + plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)

# view
health_plots

ggsave(
  health_plots,
  path = here::here(here::here("figs", "figs_church", "standardised", "noimpute")),
  width = 16,
  height = 12,
  units = "in",
  filename = "health_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

dev.off()




# GRAPHS REFLECTIVE WELL-BEING ------------------------------------------------

reflective_plots <- gratitude_p +
  groupimperm_p +
  selfperm_p +
  lifesat_p +
  meaning_p +
  perfect_p +
  pwi_p +
  powerdependence1_p +
  powerdependence2_p +
  selfesteem_p +
  veng_p +
  plot_annotation(title = "Causal effects of religious service on reflective wellbeing") +
  plot_layout(guides = 'collect')

reflective_plots

# save

ggsave(
  reflective_plots,
  path = here::here(here::here("figs", "figs_church", "standardised", "noimpute")),
  width = 16,
  height = 12,
  units = "in",
  filename = "reflective_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

# GRAPHS SOCIAL WELL-BEING ------------------------------------------------

social_plots <- belong_p +
  community_p +
  nwi_p +
  support_p + plot_annotation(title = "Causal effects of religious service on social wellbeing") +
  plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)

social_plots

ggsave(
  social_plots,
  path = here::here(here::here("figs", "figs_church", "standardised", "noimpute")),
  width = 16,
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

econ_plots <- charity_p +
  nzsei_p +
  standardliving_p +
  volunteers_p +  worklife_p +
  plot_annotation(title = "Causal effects of religious service on economic wellbeing") +
  plot_layout(guides = 'collect') + plot_layout(ncol = 2)

# view
econ_plots

ggsave(
  econ_plots,
  path = here::here(here::here("figs", "figs_church", "standardised", "noimpute")),
  width = 16,
  height = 12,
  units = "in",
  filename = "econ_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

dev.off()



# PWI COMPARE -------------------------------------------------------------


pwi_plots <-
  pwi_p +
  yourhealth_p +
  standardliving_p  +
  yourfuturesecurity_p  +
  yourpersonalrelationships_p +
  plot_annotation(title = "Causal effects of religious service on PWI dimensions") +
  plot_layout(guides = 'collect') +  plot_layout(nrow = 1)

pwi_plots

# save

ggsave(
  pwi_plots,
  path = here::here(here::here("figs", "figs_church", "standardised")),
  width = 16,
  height = 12,
  units = "in",
  filename = "pwi_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


# MULTILEVEL COMPARE ------------------------------------------------------


# MULTILEVEL DATA ---------------------------------------------------------



### Create multi-level data for comparisions
data_ml <- tab_in |>
  select(
    Id,
    YearMeasured,
    Wave,
    Partner,
    Euro,
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
    #   Household.INC,
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
    EthCat,
    Emp.JobSecure
  ) |>
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) |>
  #  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(Church = as.integer(ifelse(Religion.Church > 8, 8, Religion.Church))) |>
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
  dplyr::mutate(Euro = ifelse(EthCat == 0, 1, 0)) |>
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
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) #%>%
# dplyr::mutate(EthCat = as.factor(EthCat))  # labels = c("Euro", "Maori", "Pacific", "Asian")


# cvars multilevel --------------------------------------------------------


cvars_ml = c(
  "Church_z",
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
  "community",
  "Edu_z",
  "Employed_z",
  "Euro_z",
  "GRATITUDE_z",
  "HomeOwner_z",
  "Hours.Exercise_log_z",
  "Hours.Work_z",
  "HLTH.BMI_z",
  "HLTH.Disability_z",
  "HLTH.Fatigue_z",
  "HLTH.SleepHours_z",
  "ImpermeabilityGroup_z",
  "KESSLER6sum_z",
  "LIFEMEANING_z",
  "LIFESAT_z",
  "Male_z",
  "NZdep_z",
  "NWI_z",
  "NZSEI13_z",
  "Parent_z",
  "Partner_z",
  "PERFECTIONISM_z",
  "PermeabilityIndividual_z",
  "Pol.Orient_z",
  "POWERDEPENDENCE1_z",
  "POWERDEPENDENCE2_z",
  "Relid_z",
  "Respect.Self_z",
  "retired",
  "Rumination_z",
  "RWA_z",
  "SDO_z",
  "SELF.CONTROL_z",
  "SELF.ESTEEM_z",
  "semiretired",
  "SexualSatisfaction_z",
  "SFHEALTH_z",
  "Smoker_z",
  "Spiritual.Identification_z",
  "Standard.Living_z",
  "SUPPORT_z",
  "Urban_z",
  "VENGEFUL.RUMIN_z",
  "Volunteers_z",
  "Your.Health_z",
  "Your.Future.Security_z",
  "Your.Personal.Relationships_z"
) # "income_log_z",



# EXAMPLE OF A COMPARISON STUDY -------------------------------------------
#During the last 30 days, how often did.... you feel exhausted?

Y = "KESSLER6sum_lead2_z"
main = "Kessler 6 Distress"
ylab = "Kessler 6 Distress (SD)"
sub = "During the last 30 days, how often did....\nyou feel ...?"


# regression
out_m <- mice_gaussian(df = df,
                       X = X,
                       Y = Y,
                       cvars = cvars)

## g-computation
out_ct <-
  pool_stglm(out_m,
             df = df,
             m = 10,
             X = X,
             x = x)
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")



d_t <- out_ct %>%
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
  # row_spec(c(f + 1 - min),
  #          bold = T,
  #          color = "white") |>
  kable_minimal(full_width = F)
# show table
d_t
# graph
d_p <-
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
d_p


### COMPARE
out_ct2 <-
  pool_stglm(out_m,
             df = df,
             m = 10,
             X = X,
             x = x)



d_p_raw <-
  ggplot_stglm(
    out_ct2,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
d_p_raw
d_p #+ d_p_raw



## GLM NO CONTROL
# data_raw1 <- data_raw |>
#   mutate(KESSLER6sum_lead2_z = scale(KESSLER6sum_lead2, scale = TRUE, center = TRUE),
#          Hours.Work_lead1_10 = Hours.Work_lead1/10
#   )
#
# predict_k <- glm(KESSLER6sum_lead2_z ~  bs(Hours.Work_lead1_10), data = data_raw1 )
#
#
# k_BAD <- plot(
#   ggeffects::ggpredict(predict_k, terms = "Hours.Work_lead1_10[0:7]")
# ) + scale_y_continuous(limits = ylim) + theme_classic()
#
# k_BAD
# k_BAD +d_p

# Try lmer

cov <- paste(cvars_ml, collapse = "+")



table1::table1( ~ KESSLER6sum_z | Wave, data = data_ml)

k_lmer <- lme4::lmer(
  KESSLER6sum_z ~ wave * bs(Church) +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    BornNZ_z +
    ChildrenNum_z +
    Edu_z +
    Employed_z +
    Emp.JobSecure_z +
    EmotionRegulation1_z +
    Male_z +
    NZdep_z +
    NZSEI13_z +
    Parent_z +
    Partner_z +
    Pol.Orient_z +
    Relid_z +
    Urban_z + (1 | Id),
  data = data_ml
)



k_ml <- plot(ml_tab <-
               ggeffects::ggpredict(k_lmer, terms = c("Church[0:6]"), facet = F)) + scale_y_continuous(limits = ylim) +
  theme_classic()

d_p + k_ml


# table for ML model
k_ml_tab <- ml_tab %>%
  select(-group) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  # #row_spec(c(f + 1 - min),
  #          bold = T,
  #          color = "white")|>
  kable_minimal(full_width = F)
k_ml_tab

# Compare tables   note that for multi-level model, stress only kicks in after 60 hours of work
# These are very different models
# gcomp
d_t
# Ml
k_ml_tab


# STATEMENT OF EVALUE -----------------------------------------------------

# “With an observed risk ratio of RR=0.28, an unmeasured confounder that was associated with both the outcome and the exposure by a risk ratio of 6.6-fold each, above and beyond the measured confounders, could explain away the estimate, but weaker joint confounder associations could not; to move the confidence interval to include the null, an unmeasured confounder that was associated with the outcome and the exposure by a risk ratio of 3.1-fold each could do so, but weaker joint confounder associations could not.”






#COVID19.Timeline
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


#How many times did you pray in the last week?



# honesty humility --------------------------------------------------------

# Mini-IPIP6 Honesty-Humility (item overlap with Psychological Entitlement)
# Would like to be seen driving around in a very expensive car.
# Would get a lot of pleasure from owning expensive luxury goods.
# Feel entitled to more of everything.
# Deserve more things in life.
#
# Y = "HONESTY_HUMILITY_lead2_z"
# main = "Honesty Humility"
# ylab = "Honesty Humility (SD)"
#
#
# # regression
# out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
#
# ## g-computation
# out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X,  x = x,,r= r)
# out_ct %>%
#   slice(f+1) |>
#   kbl(digits = 3, "markdown")
#
# humility_t <- out_ct %>%
#    #slice(1:max) |>
#   tibble() |>
#   rename(Contrast = row,
#          Estimate = est,
#          Std_error = se,
#          CI_hi = ui,
#          CI_lo = li) |>
#   kbl(caption = main,
#       digits = 3,
#       "html") |>
#   kable_styling() %>%
#   row_spec(c(f+1), bold = T, color = "white", background = "dodgerblue") |>
#   kable_minimal(full_width = F)
# # show table
# humility_t
# # graph
# humility_p<- ggplot_stglm(out_ct, ylim = ylim, main, xlab, ylab, min = min, p=p, r= 1)
# humility_p
#
# round( EValue::evalues.OLS( , se = , sd = sd, delta = delta, true = 0), 3)
# round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)





##
# Confounding control variables  ---------------------------------------------------------
# These variables can be modified depending on your model and assumptions.
#  Here, we use vanderweele's "disjunctive cause criterion"

# FROM Outcomewide longitudinal designs: https://doi.org/10.1214/19-STS728
#" A modified disjunctive cause criterion that might thus be more useful in practice could articulated as follows (VanderWeele, 2019): control for each covari- ate that is a cause of the exposure, or of the outcome, or of both; exclude from this set any variable known to be an instrumental variable; and include as a covariate any proxy for an unmeasured variable that is a common cause of both the exposure and the outcome." p.443

# TYLERS LIST,  https://doi.org/10.1214/19-STS728 p.442
# *** Demographic
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

# NOTE: WE USE MORE VARIABLES



# BRMS --------------------------------------------------------------------



# create list of data frames

out_c <-
  complete(ctrim,
           action = "long",
           include = FALSE,
           mild = TRUE)

m <- 10
listdat <- list()
for (i in 1:m) {
  listdat[[i]] <- as.data.frame(out_c[[i]])
}

# create list of data frames

out_c2 <-
  complete(ctrim2,
           action = "long",
           include = FALSE,
           mild = TRUE)

m <- 10
listdat2 <- list()
for (i in 1:m) {
  listdat2[[i]] <- as.data.frame(out_c2[[i]])
}


# enable memory
options(future.globals.maxSize = 8000 * 1024 ^ 2)  # needed



# BRMS MODELS forms -------------------------------------------------------------

bf_HLTH.BMI_lead2_z <-
  bf(HLTH.BMI_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_SFHEALTH_lead2_z <-
  bf(SFHEALTH_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_Hours.Exercise_lead2 <-
  bf(Hours.Exercise_lead2 |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Smoker_lead2 <-
  bf(Smoker_lead2 | weights(weights) ~ Standard.Living_lead1_z)
bf_HLTH.Fatigue_lead2ord <-
  bf(HLTH.Fatigue_lead2ord |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Alcohol.Frequency_lead2ord <-
  bf(Alcohol.Frequency_lead2ord |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Alcohol.Intensity_lead2 <-
  bf(as.integer(Alcohol.Intensity_lead2) |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Bodysat_lead2_z <-
  bf(Bodysat_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_PWI_lead2_z <-
  bf(PWI_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_Rumination_lead2ord <-
  bf(Rumination_lead2ord |
       weights(weights) ~ Standard.Living_lead1_z)
bf_SexualSatisfaction_lead2_z <-
  bf(SexualSatisfaction_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_PWI_lead2_z <-
  bf(PWI_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_EmotionRegulation1_lead2_z <-
  bf(EmotionRegulation1_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_EmotionRegulation2_lead2_z <-
  bf(EmotionRegulation2_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_EmotionRegulation3_lead2_z <-
  bf(EmotionRegulation3_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_KESSLER6sum_lead2 <-
  bf(KESSLER6sum_lead2 | weights(weights) ~ Standard.Living_lead1_z)
bf_LIFESAT_lead2ord <-
  bf(LIFESAT_lead2ord | weights(weights) ~ Standard.Living_lead1_z)
bf_POWERDEPENDENCE_lead2_z <-
  bf(POWERDEPENDENCE_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_PERFECTIONISM_lead2_z <-
  bf(PERFECTIONISM_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_SELF.ESTEEM_lead2_z <-
  bf(SELF.ESTEEM_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Emp.WorkLifeBalance_lead2_z <-
  bf(Emp.WorkLifeBalance_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_GRATITUDE_lead2_z <-
  bf(GRATITUDE_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_VENGEFUL.RUMIN_lead2ord <-
  bf(VENGEFUL.RUMIN_lead2ord  |
       weights(weights) ~ Standard.Living_lead1_z)
bf_LIFEMEANING_lead2ord <-
  bf(LIFEMEANING_lead2ord  |
       weights(weights) ~ Standard.Living_lead1_z)
bf_HONESTY_HUMILITY_lead2_z <-
  bf(HONESTY_HUMILITY_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_BELONG_lead2_z <-
  bf(BELONG_lead2_z  | weights(weights) ~ Standard.Living_lead1_z)
bf_SUPPORT_lead2ord <-
  bf(SUPPORT_lead2ord | weights(weights) ~ Standard.Living_lead1_z)
bf_Volunteers_lead2 <-
  bf(Volunteers_lead2 | weights(weights) ~ Standard.Living_lead1_z)
bf_CharityDonate_lead2 <-
  bf(CharityDonate_lead2 |
       weights(weights) ~ Standard.Living_lead1_z)
bf_community_lead2_z <-
  bf(community_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_NWI_lead2_z <-
  bf(NWI_lead2_z | weights(weights) ~ Standard.Living_lead1_z)
bf_ImpermeabilityGroup_z <-
  bf(ImpermeabilityGroup_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_PermeabilityIndividual_z <-
  bf(PermeabilityIndividual_z |
       weights(weights) ~ Standard.Living_lead1_z)


## ADD ONE

bf_Standard.Living_lead2ord <-
  bf(Standard.Living_lead2ord |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Health_lead2_z <-
  bf(Your.Health_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Future.Security_lead2 <-
  bf(Your.Future.Security_lead2 |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Personal.Relationships_lead2ord <-
  bf(Your.Personal.Relationships_lead2ord |
       weights(weights) ~ Standard.Living_lead1_z)




# bmi ---------------------------------------------------------------------

m1_bmi_stome <- brm_multiple(
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
  file = here::here("mods", "standardliving", "m1_bmi_stome.rds"),
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

m4_Smoker_lead2 <- brm_multiple(
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
  family = cumulative("probit"),
  # Chose family
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
  file = here::here(
    "mods",
    "standardliving",
    "m6_Alcohol.Frequency_lead2ord.rds"
  ),
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

m8_Bodysat_lead2_z <- brm_multiple(
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
  file = here::here(
    "mods",
    "standardliving",
    "m11_SexualSatisfaction_lead2_z.rds"
  ),
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
  file = here::here(
    "mods",
    "standardliving",
    "m13_EmotionRegulation1_lead2_z.rds"
  ),
)

m14_EmotionRegulation2_lead2_z <- brm_multiple(
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
  file = here::here(
    "mods",
    "standardliving",
    "m14_EmotionRegulation2_lead2_z.rds"
  ),
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
  file = here::here(
    "mods",
    "standardliving",
    "m15_EmotionRegulation3_lead2_z.rds"
  ),
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


m20_SELF.ESTEEM_lead2_z <- brm_multiple(
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
  file = here::here(
    "mods",
    "standardliving",
    "m21_Emp.WorkLifeBalance_lead2_z.rds"
  ),
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


m29_community_lead2_z <- brm_multiple(
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

out3 <- with(
  ctrim,
  geeglm(
    PWI_lead2_z  ~ Standard.Living_lead1_z,
    data = cmodels,
    id = 1:nrow(cmodels),
    family = gaussian
  )
)

# same result
output <- pool(out3)
summary(output, conf.int = TRUE)
plot(output)


# brms pwi follow up ------------------------------------------------------

bf_Standard.Living_lead2_z <-
  bf(Standard.Living_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Health_lead2_z <-
  bf(Your.Health_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Future.Security_lead2_z <-
  bf(Your.Future.Security_lead2_z |
       weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Personal.Relationships_lead2ord <-
  bf(Your.Personal.Relationships_lead2ord |
       weights(weights) ~ Standard.Living_lead1_z)


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
  file = here::here(
    "mods",
    "standardliving",
    "m35_Your.Future.Security_lead2_z.rds"
  ),
)


m36_Your.Personal.Relationships_lead2ord <- brm_multiple(
  bf_Your.Personal.Relationships_lead2ord,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),
  Chose family
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
  file = here::here(
    "mods",
    "standardliving",
    "m36_Your.Personal.Relationships_lead2ord.rds"
  ),
)



# MULTILEVEL COMPARE ------------------------------------------------------


# MULTILEVEL DATA ---------------------------------------------------------



### Create multi-level data for comparisions
data_ml <- tab_in |>
  select(
    Id,
    YearMeasured,
    Wave,
    Partner,
    Euro,
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
    #   Household.INC,
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
    EthCat,
    Emp.JobSecure
  ) |>
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) |>
  #  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(Church = as.integer(ifelse(Religion.Church > 8, 8, Religion.Church))) |>
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
  dplyr::mutate(Euro = ifelse(EthCat == 0, 1, 0)) |>
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
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) #%>%
# dplyr::mutate(EthCat = as.factor(EthCat))  # labels = c("Euro", "Maori", "Pacific", "Asian")
