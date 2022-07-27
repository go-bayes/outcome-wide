#Internet.R
#https://pubmed.ncbi.nlm.nih.gov/18190618/

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


# view
df %>%   # not there
  dplyr::filter(Wave == 2020) %>%
  summarise(Respect.Self) #fully missing

# df$Hours.CompGames
# df$Hours.SocialMedia
# df$Hours.Internet
# df$Hours.News
# df$Hours.Children
#
# Hours.TV,
# Hours.News,
# Hours.Internet,
# Hours.SocialMedia,
# Hours.CompGames
# Hours.Work
# Hours.Housework,
# Hours.Children,
# HLTH.SleepHours
# df$Hours.Commute
df <- haven::zap_formats(df)
df <- haven::zap_label(df)
df <- haven::zap_widths(df)
df <- haven::zap_labels(df)

# select variables
hi_df <- df %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
  dplyr::filter((Wave == 2016  & YearMeasured  == 1) |
                  (Wave == 2017  &
                     YearMeasured  == 1) |
                  (Wave == 2019))  %>% # Eligibility criteria
#  dplyr::filter(Id != 9630) %>% # problematic
  select(
    Id,
    YearMeasured,
    Wave,
    EthCat,
    Age,
    # Euro,
    Male,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    Edu,
    NZdep,
    Employed,
    retired,
    semiretired,
    NZSEI18,
    PermeabilityIndividual,
    ImpermeabilityGroup,
    HomeOwner,
    Pol.Orient,
    Urban,
    Household.INC,
    Parent,
    Partner,
    Relid,
    Religion.Church,
    #   Believe.Spirit,
    #   Believe.God,
    SWB.SoC01,
    EmotionRegulation,
    Bodysat,
    VENGEFUL.RUMIN,
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
   # Hours.Work,
   # Hours.Exercise,
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
    Hours.TV,
    Hours.News,
    Hours.Internet,
    Hours.SocialMedia,
    Hours.CompGames,
    Hours.Work,
    Hours.Housework,
    Hours.Children,
    HLTH.SleepHours,
    Hours.Exercise,
    Hours.Commute,
    # GenCohort,
    # Euro,
    # partnerlost_job, rare
    #lost_job,
    #began_relationship,
    Alcohol.Intensity,
    Alcohol.Frequency,
    SexualSatisfaction,
    POWERDEPENDENCE,
    # Your.Future.Security,
    #  Your.Personal.Relationships,
    # Your.Health,
    # Standard.Living,
    #Env.SacWilling,
    #Env.SacMade,
    PERFECTIONISM ,
    HLTH.SleepHours
    # Emp.JobSecure,
    #  Env.ClimateChgCause,
    # Env.ClimateChgReal #,
  )%>%
  dplyr::rename(community =SWB.SoC01) %>%
  dplyr::mutate(Edu =as.numeric(Edu)) %>%
  dplyr::mutate(org2017 =  ifelse(Wave == 2017 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold17 = mean(org2017, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold17 > 0) %>% # hack to enable repeat of baseline in 201
  dplyr::mutate(org2016 =  ifelse(Wave == 2016 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold16 = mean(org2016, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold16 > 0) %>% # hack to enable repeat of baseline
  # dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  ungroup() %>%
  dplyr::filter(YearMeasured  != -1) %>%
  droplevels() %>%
  arrange(Id, Wave) %>%
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
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1),
  ) %>%
  arrange(Id, Wave)  %>%
  group_by(Id, Wave) %>%
  rowwise() %>%
  mutate(hoursinternet = sum(c(Hours.TV,
                                 Hours.News,
                                 Hours.Internet,
                                 Hours.SocialMedia,
                                 Hours.CompGames#,
                               #  Hours.Work,
                               #  Hours.Housework,
                               #  Hours.Children,
                                # HLTH.SleepHours,
                                # Hours.Exercise,
                                # Hours.Commute
                               ))) %>%
  ungroup() %>%
  dplyr::mutate(hoursinternet_lead1 = lead(hoursinternet, n = 1)) %>%
  dplyr::mutate(Hours.Work_lead1 = lead(Hours.Work, n = 1)) %>%
  dplyr::mutate(Employed_lead1 = lead(Employed, n = 1)) %>%
  dplyr::mutate(retired_lead1 = lead(retired, n = 1)) %>%
  dplyr::mutate(semiretired_lead1 = lead(semiretired, n = 1)) %>%
  # dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
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
      Hours.TV,
      Hours.News,
      Hours.Internet,
      Hours.SocialMedia,
      Hours.CompGames,
      Hours.Work,
      Hours.Housework,
      Hours.Children,
      HLTH.SleepHours,
      Hours.Exercise,
      Hours.Commute,
      hoursinternet,
      HLTH.BMI,
      Smoker,
      #  ChildrenNum,
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
      EmotionRegulation,
      Emp.WorkLifeBalance,
      retired,
      semiretired,
      NZSEI18,
      PermeabilityIndividual,
      ImpermeabilityGroup
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2016) %>%
  dplyr::filter(!is.na(hoursinternet))%>%
 # dplyr::filter(!is.na(Employed)) %>%
#  dplyr::filter(!is.na(Hours.Work)) %>%
#  dplyr::filter(!is.na(retired)) %>%
#  dplyr::filter(!is.na(semiretired)) %>%
  dplyr::filter(!is.na(hoursinternet_lead1)) %>%
#  dplyr::filter(!is.na(Employed_lead1)) %>%
#  dplyr::filter(!is.na(Hours.Work_lead1)) %>%
 # dplyr::filter(!is.na(retired_lead1)) %>%
#  dplyr::filter(!is.na(semiretired_lead1)) %>%
  #needed for the intervention
  #  dplyr::filter(!is.na(Church)) %>%
  #  dplyr::filter(!is.na(Church_lead1)) %>%  #needed for the intervention
  dplyr::select(
    -c(
      Religion.Church,
      EthCat,
      HoursCharity,
      Respect.Self_lead2,
      Household.INC,
      org2016,
      #  not_euro,
      #  not_euro_lead2,
      hold16,
   #   Euro,
      Emp.WorkLifeBalance,
      YearMeasured,
      org2017,
      hold17
    )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)

mean(hi_df$hoursinternet)


# check N of ids
length(unique(hi_df$Id)) # 32343


table1::table1(
  ~ Age + Euro + Male +
    Employed +
    Employed_lead1 +
    semiretired +
    semiretired_lead1 +
    retired +
    retired_lead1 +
    hoursinternet +
    hoursinternet_lead1,
  data = hi_df
)
hi_df$Age

for_df
for_df %>%
  select(Church, VENGEFUL.RUMIN_lead1) %>%
  summarise(across(everything(), c(mean = mean, sd = sd)))


# inspect data
skim(for_df)

for_df %>%
  group_by(Wave) %>%
  summarise(across(Id, n_distinct))

hist(for_df$Volunteers)



# consider removing
for_df %>%
  dplyr::filter(Id == 9630)

# glimse
for_df %>%
  summarise(across(
    c(PWI_lead2, Church),
    ~ mean(.x, na.rm = TRUE),
    ~ sd(.x, na.rm = TRUE),
    n_distinct()
  ))

# cases
length(unique(for_df$Id))

for_df


# save function
saveh(for_df, "for_df")

# read if needed
for_df <- readh("for_df")


str(for_df)
# facets ------------------------------------------------------------------
library(lcsm)
Relidwide <-  df %>%
  dplyr::filter(YearMeasured == 1) %>%
  dplyr::filter(!is.na(Religion.Church)) %>%
  dplyr::select(Id, Religion.Church, Wave) %>%
  group_by(Id) %>% filter(n() > 9) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Religion.Church)

Relidwide <- Relidwide[complete.cases(Relidwide),]
dim(Relidwide)
Relidwide
x_var_list <- names(Relidwide[, 2:11])

Relidwide

library(lcsm)
plot_trajectories(
  data = Relidwide,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Religious",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.086,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)

dev.off()

df$Believe.God


## TRY THIS

cc_df <- df %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
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
    Edu,
    Euro,
    NZdep,
    Employed,
    HomeOwner,
    Pol.Orient,
    Male,
    Urban,
    Household.INC,
    Parent,
    Partner,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    Relid,
    Religion.Church,
    Believe.Spirit,
    Believe.God,
    SWB.SoC01,
    EmotionRegulation,
    Bodysat,
    VENGEFUL.RUMIN,
    # BornNZ, nor working
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
    # GenCohort,
    # Euro,
    # partnerlost_job, rare
    lost_job,
    began_relationship,
    Alcohol.Intensity,
    Alcohol.Frequency,
    SexualSatisfaction,
    POWERDEPENDENCE,
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living,
    #Env.SacWilling,
    #Env.SacMade,
    PERFECTIONISM,
    # Emp.JobSecure,
    Env.ClimateChgCause,
    Env.ClimateChgReal #,
  ) %>%
  dplyr::rename(community =SWB.SoC01) %>%
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
    # Church = cut(
    #   Religion.Church,
    #   breaks = c(-Inf,0,1,2,3,4,Inf),
    #   labels = c("Never","1xMonth","2xMonth","3xMonth","weekly",">weekly"),
    #   right = FALSE),
    EthCat = factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    not_euro = as.numeric(if_else(EthCat == "Euro", 0, 1)),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    Exercise_log = log(Hours.Exercise + 1),
    income_log = log(Household.INC + 1),
    CharityDonate_log = log(CharityDonate + 1),
    Alcohol.Intensity_log = log(Alcohol.Intensity + 1)
  ) %>%
  arrange(Id, Wave)  %>%
  # dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
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
      not_euro,
      SELF.ESTEEM,
      SELF.CONTROL,
      Respect.Self,
      Emp.WorkLifeBalance,
      Alcohol.Frequency,
      Alcohol.Intensity_log,
      Hours.Exercise,
      Exercise_log,
      HLTH.BMI,
      Smoker,
      ChildrenNum,
      NWI,
      BELONG,
      SUPPORT,
      CharityDonate_log,
      Volunteers,
      GRATITUDE,
      SexualSatisfaction,
      POWERDEPENDENCE,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living,
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause,
      Env.ClimateChgReal,
      CharityDonate,
      Alcohol.Intensity,
      PERFECTIONISM,
      Bodysat,
      VENGEFUL.RUMIN,
      community,
      HONESTY_HUMILITY,
      EmotionRegulation
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::mutate(VENGEFUL.RUMIN_lead1 = lead(VENGEFUL.RUMIN, n = 1)) %>%
  dplyr::filter(Wave == 2018) %>%
  dplyr::filter(!is.na(VENGEFUL.RUMIN)) %>%
  dplyr::filter(!is.na(VENGEFUL.RUMIN_lead1)) %>%  #needed for the intervention
  dplyr::select(
    -c(
      Religion.Church,
      EthCat,
      Respect.Self_lead2,
      #   CharityDonate,
      Household.INC,
      #    Alcohol.Intensity,
      org2018,
      not_euro,
      not_euro_lead2,
      hold18,
      Emp.WorkLifeBalance,
      YearMeasured,
      #   Hours.Exercise,
      #  Employed,
      #   Hours.Exercise_lead2,
      org2019,
      hold19
    )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric))

# check N of ids
length(unique(cc_df$Id)) # 33189

