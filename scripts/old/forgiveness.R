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


# table for participant N
tab_df <- df %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(Id != 9630) %>% # problematic
  select(
    Id,
    YearMeasured,
    Wave,
    VENGEFUL.RUMIN
  ) %>%
group_by(Id) %>%
  dplyr::mutate(org2019 =  ifelse(Wave == 2019 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  ungroup() %>%
  dplyr::filter(YearMeasured  != -1) %>%# remove people who passed away
  droplevels() %>%
  arrange(Id, Wave) %>%
  dplyr::group_by(Id) %>%   # get all people who were in that wave unless they died
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  ungroup() %>%
  dplyr::mutate(VENGEFUL.RUMIN_lead1 = lead(VENGEFUL.RUMIN, n = 1)) %>%
  # dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::filter(!is.na(VENGEFUL.RUMIN | Wave == 2018)) %>%
  dplyr::filter(!is.na(VENGEFUL.RUMIN_lead1 | Wave == 2018)) %>%  #needed for the intervention
  data.frame() %>%
  arrange(Id, Wave)



# check N of ids
length(unique(tab_df$Id)) # 34782

table1::table1( ~ VENGEFUL.RUMIN | Wave, data = tab_df)




# select variables
for_df <- df %>%
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
    PERFECTIONISM #,
    # Emp.JobSecure,
  #  Env.ClimateChgCause,
   # Env.ClimateChgReal #,
  ) %>%
  dplyr::rename(community =SWB.SoC01) %>%
  dplyr::mutate(Edu =as.numeric(Edu)) %>%
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
    EthCat = factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    Euro = as.numeric(if_else(EthCat == "Euro", 1, 0)),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1),
  ) %>%
  arrange(Id, Wave)  %>%
  dplyr::mutate(VENGEFUL.RUMIN_lead1 = lead(VENGEFUL.RUMIN, n = 1)) %>%
  dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
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
      Hours.Exercise,
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
    Emp.WorkLifeBalance
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
  dplyr::filter(!is.na(VENGEFUL.RUMIN)) %>%
  dplyr::filter(!is.na(VENGEFUL.RUMIN_lead1)) %>%  #needed for the intervention
#  dplyr::filter(!is.na(Church)) %>%
#  dplyr::filter(!is.na(Church_lead1)) %>%  #needed for the intervention
  dplyr::select(
    -c(
      Religion.Church,
      EthCat,
      HoursCharity,
      Respect.Self_lead2,
      Household.INC,
      org2018,
    #  not_euro,
    #  not_euro_lead2,
      hold18,
      Euro,
      Emp.WorkLifeBalance,
      YearMeasured,
      org2019,
      hold19
    )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)

str(for_df$Employed)
# check N of ids
length(unique(for_df$Id)) # 38962

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




for_df
for_df %>%
  select(Church, VENGEFUL.RUMIN_lead1) %>%
  summarise(across(everything(), c(mean = mean, sd = sd)))


# i

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


# mice model  -------------------------------------------------------------
options(future.globals.maxSize = 8000 * 1024^2)

library(mice)
for_mice <- for_df %>%
  select(-Wave, Id)


gg_mice <- cc_df %>%
  dplyr::select(-c(Wave, Id,
                   Alcohol.Intensity_log,
                   CharityDonate_log,
                   Exercise_log,
                   Alcohol.Intensity_log_lead2,
                   CharityDonate_log_lead2,
                   Exercise_log_lead2))


str(for_mice)
#for_mice<- as.data.frame(for_mice)
# Visualise missing
library(naniar)
naniar::gg_miss_var(for_mice)
vis_miss(for_mice,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(for_mice)

nrow(for_mice)
str(for_mice)
#for_mice$inc_prop <-
#  for_mice$income_log / (for_mice$income_log_lead1 - 1)
ini <- mice(gg_mice, m = 1, maxit = 0)
ini
meth <- ini$meth
meth



# impute
out <- quickpred(gg_mice)
out
out_formice <- mice::mice(gg_mice, seed = 0,  m = 10)


# save
saveh(out_formice, "out_formice")

# read
out_formice <- readh("out_formice")

outlist2 <-
  row.names(out_formice)[out_formice$outflux < 0.5]
length(outlist2)

head(out_formice$loggedEvents, 10)

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

# data warangling
long_out_for_mice <- mice::complete(out_formice, "long", inc = TRUE)
# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(long_out_for_mice)
#long$LIFESAT_lead1_z <- with(long, scale(LIFESAT_lead1))
min(long_out_for_mice$KESSLER6sum, na.rm = TRUE)

min(long_out_for_mice$Alcohol.Frequency_lead2, na.rm = TRUE)

# create variables in z score
long_out_for_mice2 <- long_out_for_mice %>%
  dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
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
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0) )) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0)+1 )) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1))%>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0)+1))%>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0) )) %>%
  dplyr::mutate(alcohol_bin2 = if_else( Alcohol.Frequency > 3, 1, 0))%>%
  dplyr::mutate(alcohol_bin = if_else( Alcohol.Frequency > 2, 1, 0))%>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))# Respect for Self is fully missing

l2 <- long_out_for_mice2
table(l2$Alcohol.Frequency_lead2ord)
# review
table(long_out_for_mice2$Rumination_lead2ord)
table(long_out_for_mice2$LIFEMEANING_lead2ord)
str(long_out_for_mice2$SUPPORT_lead2ord)
hist(long_out_for_mice2$HLTH.Fatigue_lead2ord)
hist(long_out_for_mice2$community_lead2)
str(long_out_for_mice2$alcohol_bin2)


# get colnames
hist(long_out_for_mice2$Hours.Exercise_lead2) # get names

#long$attrition <- with(long, ndf$attrition_lead2)
# neect to get into shape
long3 <- long_out_for_mice2 %>% mutate_if(is.matrix, as.vector)
out2_for <- mice::as.mids(long3)
saveh(out2_for, "out2_for")
out2_for <- readh("out2_for")


## for logistic models
newdata = data.frame(
  n = 1,
  VENGEFUL.RUMIN_lead1_z = c(-1, 1),
  VENGEFUL.RUMIN = 0,
  AGREEABLENESS_z  = 0,
  CONSCIENTIOUSNESS_z  = 0,
  EXTRAVERSION_z  = 0,
  HONESTY_HUMILITY_z = 0,
  NEUROTICISM_z = 0,
  OPENNESS_z = 0,
  Age_z = 0,
  Alcohol.Frequency_z = 0,
  Alcohol.Intensity_log_z = 0,
  Bodysat_z = 0,
  Believe.God_z = 0,
  Believe.Spirit_z = 0,
  BELONG_z = 0,
  CharityDonate_log_z = 0,
  ChildrenNum_z = 0,
  Church_z = 0,
  community = 0,
  Edu_z = 0,
  Employed_z = 0,
  EmotionRegulation_z = 0,
  Euro_z = 0,
  GRATITUDE_z = 0,
  HomeOwner_z = 0,
  Hours.Exercise_log_z = 0,
  Hours.Work_z = 0,
  HLTH.BMI_z  = 0,
  HLTH.Fatigue_z = 0,
  income_log_z = 0,
  KESSLER6sum_z = 0,
  LIFEMEANING_z = 0,
  LIFESAT_z = 0,
  lost_job_z= 0,
  Male_z = 0,
  NZdep_z= 0,
  NWI_z = 0,
  Parent_z = 0,
  Partner_z = 0,
  PERFECTIONISM_z = 0,
  Pol.Orient_z = 0,
  POWERDEPENDENCE_z = 0, #
  PWI_z = 0,
  Relid_z = 0,
  Respect.Self_z = 0, #
  Rumination_z = 0, #
  SELF.CONTROL_z = 0, #
  SELF.ESTEEM_z = 0,#
  SexualSatisfaction_z = 0,#
  SFHEALTH_z = 0,
  Smoker_z = 0,
  SUPPORT_z = 0,
  Urban_z = 0,
  Volunteers_z= 0,
  transform = TRUE,
  re_formula = NA)



# HLTH.BMI_lead2_z ---------------------------------------------------------
options(future.globals.maxSize = 8000 * 1024^2)  # needed

m1_bmi_forgive <- brm_multiple(
  HLTH.BMI_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init= 0,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m1_bmi_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
# looks good
pp_check(m1_bmi_forgive)


out <- model_parameters(m1_bmi_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)



# evalue
round( EValue::evalues.OLS( -0.00320 , se = 0.00493, sd = 1, delta = 2, true = 0), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m1_bmi_forgive)
m1_bayes_table
plot(m1_bayes_table)

# graph
m1_bmi_forgive_plot <-
  plot(conditional_effects(
    m1_bmi_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m1_bmi_forgive_plot_z  <-  m1_bmi_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "BMI",
       # subtitle = "Loss shows greater distress",
       y = "BMI (sd)",
       x = "Un-Forgiving (sd)")

m1_bmi_forgive_plot_z

ggsave(
  m1_bmi_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m1_bmi_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# SFHEALTH_lead2_z --------------------------------------------------------------


m2_sfhealth_forgive <- brm_multiple(
  SFHEALTH_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  init = 0,
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods",  "m2_sfhealth_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

out <- model_parameters(m2_sfhealth_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out


# Evalue
round( EValue::evalues.OLS(  -0.03758 , se = 0.00634, sd = 1, delta = 2, true = 0), 3)



## posterior predictive checks
pp_check(m2_sfhealth_forgive)

#table
lazerhawk::brms_SummaryTable(m2_sfhealth_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m2_sfhealth_forgive)
m1_bayes_table
plot(m1_bayes_table)

# graph
m2_sfhealth_forgive_plot <-
  plot(conditional_effects(
    m2_sfhealth_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m2_sfhealth_forgive_plot_z  <-  m2_sfhealth_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5))+ scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Short Form Health",
       # subtitle = "Loss shows greater distress",
       y = "Short Form Health (sd)",
       x = "Un-Forgiving (sd)")

m2_sfhealth_forgive_plot_z

ggsave(
  m2_sfhealth_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m2_sfhealth_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# Hours.Exercise_log_lead2z -----------------------------------------------
m3_exercise_forgive <- brm_multiple(
  Hours.Exercise_lead2_log_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  init = 0,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m3_exercise_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

m3a_exercise_forgive <- brm_multiple(
  Hours.Exercise_lead2   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "negbinomial",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m3a_exercise_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)


##  BAD
pp_check(m3a_exercise_forgive)



## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])

# model coeff
out <- model_parameters(m3a_exercise_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

log(7.443313)
(exp( 0.00784+0.00696))

sd(l2$Hours.Exercise_lead2, na.rm = TRUE)


# evalue
round( EValue::evalues.OLS( -0.00466, se = 0.00692, sd = 7.443313, delta = 2, true = 0), 3)


# evalue for RR
round( EValue::evalues.RR(1.007871, lo =  1.00088, hi = 1.01491, true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m3a_exercise_forgive_plot <-
  plot(conditional_effects(
    m3a_exercise_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m3a_exercise_forgive_plot_z  <-  m3a_exercise_forgive_plot[[1]]  +
  # scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Exercise",
       # subtitle = "Loss shows greater distress",
       y = "Exercise (weekly hours)",
       x = "Un-Forgiving (sd)")


m3a_exercise_forgive_plot_z
ggsave(
  m3a_exercise_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m3a_exercise_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)




# Smoker_bz ---------------------------------------------------------------

m4_smoker_forgive <- brm_multiple(
  Smoker_lead2   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = bernoulli(link = "cloglog"),
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init=0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m4_smoker_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)


##
pp_check(m4_smoker_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    m4_smoker_forgive,
    newdata = newdata,
  )
)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  , se = , sd = 1, delta = 2, true = 0), 3)

1.01704-0.01661302
1.01704+0.01661302
# evalue for RR
round( EValue::evalues.RR( 1.01704, lo =  1.000427, hi = 1.033653, true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m4_smoker_forgive)
m1_bayes_table
plot(m1_bayes_table)

# graph
m4_smoker_forgive_plot <-
  plot(conditional_effects(
    m4_smoker_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m4_smoker_forgive_plot_z  <-  m4_smoker_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(0, .3)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Smoker",
       # subtitle = "Loss shows greater distress",
       y = "Smoker (y=1/n=0)",
       x = "Un-Forgiving (sd)")

m4_smoker_forgive_plot_z
ggsave(
  m4_smoker_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m4_smoker_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

m4_smoker_forgive_plot_z

# health-fatigue ----------------------------------------------------------

m5_fatigue_forgive <- brm_multiple(
  HLTH.Fatigue_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  init =0,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m5_fatigue_forgive"),
  set_prior('normal(0, 1)', class = 'b')
)


# NOTE NAME ISSUE
m5a_fatigue_forgive <- brm_multiple(
  HLTH.Fatigue_lead2ord ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = cumulative("probit"),
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m5a_fatigue_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

options(scipen=999)

##
pp_check(m5_fatigue_forgive)

pp_check(m5a_fatigue_forgive)


## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
pp_check(m5_fatigue_forgive)

out <- model_parameters(m5_fatigue_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out
out <- model_parameters(m5a_fatigue_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(5)
out
sd(l2$HLTH.Fatigue_lead2ord, na.rm=TRUE)
# evalue
round( EValue::evalues.OLS(0.05257 , se = 0.00857, sd = 1.049608, delta = 2, true = 0), 3)

# standardised
round( EValue::evalues.OLS( 0.04221 , se = 0.00687, sd = 1, delta = 2, true = 0), 3)


# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m5a_fatigue_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m5a_fatigue_forgive_plot <-
  plot(conditional_effects(
    m5a_fatigue_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m5a_fatigue_forgive_plot_z  <-  m5a_fatigue_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(2, 3)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Fatigue",
       # subtitle = "Loss shows greater distress",
       y = "Fatigue (1-5)",
       x = "Un-Forgiving (sd)")

m5a_fatigue_forgive_plot_z
ggsave(
  m5a_fatigue_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m5a_fatigue_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)


# Alcohol.Frequency_lead2_z , ---------------------------------------------------

m6_AlcoholFrequency_forgive <-
  brm_multiple(
    Alcohol.Frequency_lead2_z   ~
      VENGEFUL.RUMIN_lead1_z +
      VENGEFUL.RUMIN  +
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
      EmotionRegulation_z +
      Euro_z +
      GRATITUDE_z +
      HomeOwner_z +
      Hours.Exercise_log_z +
      Hours.Work_z +
      HLTH.BMI_z  + #
      HLTH.Fatigue_z + #
      income_log_z +
      KESSLER6sum_z + #
      LIFEMEANING_z + #
      LIFESAT_z + #
      lost_job_z +
      Male_z +
      NZdep_z +
      NWI_z +
      Parent_z +
      Partner_z +
      PERFECTIONISM_z +
      Pol.Orient_z +
      POWERDEPENDENCE_z + #
      PWI_z +
      Relid_z +
      Respect.Self_z + #
      Rumination_z + #
      SELF.CONTROL_z + #
      SELF.ESTEEM_z + #
      SexualSatisfaction_z +#
      SFHEALTH_z +#
      Smoker_z +#
      SUPPORT_z +#
      Urban_z +
      Volunteers_z,
    family = "gaussian",
    data = out2_for,
    seed = 1234,
    warmup = 1000,
    init =0,
    iter = 2000,
    chains = 4,
    backend = "cmdstanr",
    file = here::here("mods", "forgivemods", "m6_AlcoholFrequency_forgive.rds"),
    set_prior('normal(0, 1)', class = 'b')
  )

m6a_AlcoholFrequency_forgive <-
  brm_multiple(
    Alcohol.Frequency_lead2ord   ~
      VENGEFUL.RUMIN_lead1_z +
      VENGEFUL.RUMIN  +
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
      EmotionRegulation_z +
      Euro_z +
      GRATITUDE_z +
      HomeOwner_z +
      Hours.Exercise_log_z +
      Hours.Work_z +
      HLTH.BMI_z  + #
      HLTH.Fatigue_z + #
      income_log_z +
      KESSLER6sum_z + #
      LIFEMEANING_z + #
      LIFESAT_z + #
      lost_job_z +
      Male_z +
      NZdep_z +
      NWI_z +
      Parent_z +
      Partner_z +
      PERFECTIONISM_z +
      Pol.Orient_z +
      POWERDEPENDENCE_z + #
      PWI_z +
      Relid_z +
      Respect.Self_z + #
      Rumination_z + #
      SELF.CONTROL_z + #
      SELF.ESTEEM_z + #
      SexualSatisfaction_z +#
      SFHEALTH_z +#
      Smoker_z +#
      SUPPORT_z +#
      Urban_z +
      Volunteers_z,
    family = cumulative("probit"),
    data = out2_for,
    seed = 1234,
    warmup = 1000,
    init =0,
    iter = 2000,
    chains = 4,
    backend = "cmdstanr",
    file = here::here("mods", "forgivemods", "m6a_AlcoholFrequency_forgive.rds"),
    set_prior('normal(0, 1)', class = 'b')
  )


## this works
pp_check(m6a_AlcoholFrequency_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
sd(l2$Alcohol.Frequency_lead2ord, na.rm=TRUE)

outa <- model_parameters(m6_AlcoholFrequency_forgive, dispersion = TRUE, centrality = "mean",
                         test = "pd",
                         digits = 5,
                         diagnostic =  NULL,
                         rope_range = NULL)%>% slice(2)
outa
out <- model_parameters(m6a_AlcoholFrequency_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(6)
out

# evalue
# standard
round( EValue::evalues.OLS(  0.01483, se = 0.00562, sd = 1, delta = 2, true = 0), 3)

round( EValue::evalues.OLS(  0.02323, se = 0.00851, sd = 1.358344, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m6a_AlcoholFrequency_forgive_plot <-
  plot(conditional_effects(
    m6a_AlcoholFrequency_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    categorical = F,
    spaghetti = T
  ))

m6a_AlcoholFrequency_forgive_plot_z  <-  m6a_AlcoholFrequency_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(3, 3.5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Alcohol Frequency",
       # subtitle = "Loss shows greater distress",
       y = " Alcohol Frequency (ordinal 1-6)",
       x = "Un-Forgiving (sd)")

m6a_AlcoholFrequency_forgive_plot_z
ggsave(
  m6a_AlcoholFrequency_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m6a_AlcoholFrequency_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)




#  Alcohol.Intensity_logz -------------------------------------------------

m7_AlcoholIntensity_forgive <-
  brm_multiple(
    bf(Alcohol.Intensity_lead2_z   ~
         VENGEFUL.RUMIN_lead1_z +
         VENGEFUL.RUMIN  +
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
         EmotionRegulation_z +
         Euro_z +
         GRATITUDE_z +
         HomeOwner_z +
         Hours.Exercise_log_z +
         Hours.Work_z +
         HLTH.BMI_z  + #
         HLTH.Fatigue_z + #
         income_log_z +
         KESSLER6sum_z + #
         LIFEMEANING_z + #
         LIFESAT_z + #
         lost_job_z +
         Male_z +
         NZdep_z +
         NWI_z +
         Parent_z +
         Partner_z +
         PERFECTIONISM_z +
         Pol.Orient_z +
         POWERDEPENDENCE_z + #
         PWI_z +
         Relid_z +
         Respect.Self_z + #
         Rumination_z + #
         SELF.CONTROL_z + #
         SELF.ESTEEM_z + #
         SexualSatisfaction_z +#
         SFHEALTH_z +#
         Smoker_z +#
         SUPPORT_z +#
         Urban_z +
         Volunteers_z),
    family = "gaussian",
    data = out2_for,
    seed = 1234,
    warmup = 1000,
    iter = 2000,
    init = 0,
    chains = 4,
    backend = "cmdstanr",
    file = here::here("mods", "forgivemods", "m7_AlcoholIntensity_forgive.rds"),
    set_prior('normal(0, 1)', class = 'b')
  )


m7a_AlcoholIntensity_forgive <-
  brm_multiple(
    as.integer(Alcohol.Intensity_lead2)   ~
      VENGEFUL.RUMIN_lead1_z +
      VENGEFUL.RUMIN  +
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
      EmotionRegulation_z +
      Euro_z +
      GRATITUDE_z +
      HomeOwner_z +
      Hours.Exercise_log_z +
      Hours.Work_z +
      HLTH.BMI_z  + #
      HLTH.Fatigue_z + #
      income_log_z +
      KESSLER6sum_z + #
      LIFEMEANING_z + #
      LIFESAT_z + #
      lost_job_z +
      Male_z +
      NZdep_z +
      NWI_z +
      Parent_z +
      Partner_z +
      PERFECTIONISM_z +
      Pol.Orient_z +
      POWERDEPENDENCE_z + #
      PWI_z +
      Relid_z +
      Respect.Self_z + #
      Rumination_z + #
      SELF.CONTROL_z + #
      SELF.ESTEEM_z + #
      SexualSatisfaction_z +#
      SFHEALTH_z +#
      Smoker_z +#
      SUPPORT_z +#
      Urban_z +
      Volunteers_z,
    family = "negbinomial",
    data = out2_for,
    seed = 1234,
    warmup = 1000,
    init = 0,
    iter = 2000,
    chains = 4,
    backend = "cmdstanr",
    file = here::here("mods", "forgivemods", "m7a_AlcoholIntensity_forgive"),
    set_prior('normal(0, 1)', class = 'b')
  )

sd(l2$Alcohol.Intensity_lead2, na.rm=TRUE)

##
pp_check(m7a_AlcoholIntensity_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m7a_AlcoholIntensity_forgive,
                        dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out
sd(l2$Alcohol.Intensity_lead2, na.rm=TRUE)

# evalue
#round( EValue::evalues.OLS(  0.01685, se = 0.00576, sd = 1.990131, delta = 2, true = 0), 3)


# Use
# evalue for RR
round( EValue::evalues.RR(1.016993, lo =  1.011152, hi = 1.022868, true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m7a_AlcoholIntensity_forgive_plot <-
  plot(conditional_effects(
    m7a_AlcoholIntensity_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m7a_AlcoholIntensity_forgive_plot_z  <-  m7a_AlcoholIntensity_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(1.5, 2)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Alcohol Intensity",
       # subtitle = "Loss shows greater distress",
       y = " (Alcohol Intensity (drinks per session)",
       x = "Un-Forgiving (sd)")

m7a_AlcoholIntensity_forgive_plot_z
ggsave(
  m7a_AlcoholIntensity_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m7a_AlcoholIntensity_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# bodysat  ----------------------------------------------------------------


m8_bodysat_forgive <- brm_multiple(
  Bodysat_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  init = 0,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m8_bodysat_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)


m8a_bodysat_forgive <- brm_multiple(
  Bodysat_lead2   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  init = 0,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m8a_bodysat_forgive"),
  set_prior('normal(0, 1)', class = 'b')
)

##
pp_check(m8_bodysat_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m8_bodysat_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.02824 , se = 0.00628, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m8_bodysat_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m8a_bodysat_forgive_plot <-
  plot(conditional_effects(
    m8a_bodysat_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m8_bodysat_forgive_plot_z  <-  m8_bodysat_forgive[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Body Satisfaction",
       # subtitle = "Loss shows greater distress",
       y = "Body Satisfaction (sd)",
       x = "Un-Forgiving (sd)")

m8_bodysat_forgive_plot_z

ggsave(
  m8_bodysat_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m8_bodysat_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# rumination --------------------------------------------------------------

m9_rumination_forgive <- brms::brm_multiple(
  Rumination_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  init = 0,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m9_rumination_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)


m9a_rumination_forgive <- brms::brm_multiple(
  Rumination_lead2ord ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = cumulative("probit"),
  data = out2_for,
  init = 0,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m9a_rumination_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

##
pp_check(m9_rumination_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m9a_rumination_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(5)
out
sd(l2$Rumination_lead2ord , na.rm = T)

# evalue
round( EValue::evalues.OLS(0.14326  , se = 0.00857, sd = 0.9525767, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m9a_rumination_forgive_plot <-
  plot(conditional_effects(
    m9a_rumination_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m9a_rumination_forgive_plot_z  <-  m9a_rumination_forgive_plot[[1]]  +
 # scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Rumination",
       # subtitle = "Loss shows greater distress",
       y = "Forgiveness(sd)",
       x = "Un-Forgiving (sd)")

m9a_rumination_forgive_plot_z
ggsave(
  m9a_rumination_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m9a_rumination_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# SexualSatisfaction_z ----------------------------------------------------

m10_SexualSatisfaction_forgive  <- brm_multiple(
  SexualSatisfaction_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m10_SexualSatisfaction_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

##
pp_check()

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m10_SexualSatisfaction_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS( -0.04229 , se = 0.00703, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m10_SexualSatisfaction_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m10_SexualSatisfaction_forgive_plot <-
  plot(conditional_effects(
    m10_SexualSatisfaction_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m10_SexualSatisfaction_forgive_plot_z  <-  m10_SexualSatisfaction_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Sexual Satisfaction",
       # subtitle = "Loss shows greater distress",
       y = "Sexual Satisfaction (sd)",
       x = "Un-Forgiving (sd)")

m10_SexualSatisfaction_forgive_plot_z

ggsave(
  m10_SexualSatisfaction_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m10_SexualSatisfaction_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# PWI_z -------------------------------------------------------------------


m11_PWI_forgive <- brm_multiple(
  PWI_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  init = 0,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m11_PWI_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m11_PWI_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m11_PWI_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.03350 , se =  0.00628, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m11_PWI_forgive_plot <-
  plot(conditional_effects(
    m11_PWI_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m11_PWI_forgive_plot_z  <-  m11_PWI_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Personal Well-Being",
       # subtitle = "Loss shows greater distress",
       y = "Personal Well-Being (sd)",
       x = "Un-Forgiving (sd)")

m11_PWI_forgive_plot_z
ggsave(
  m11_PWI_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m11_PWI_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# emotion regulation ------------------------------------------------------

m12_EmotionRegulation_forgive <- brm_multiple(
  EmotionRegulation_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init =0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m12_EmotionRegulation_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check()

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m12_EmotionRegulation_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS( 0.00609  , se = 0.00720, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m12_EmotionRegulation_forgive_plot <-
  plot(conditional_effects(
    m12_EmotionRegulation_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m12_EmotionRegulation_forgive_plot_z  <-  m12_EmotionRegulation_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Emotional Regulation",
       # subtitle = "Loss shows greater distress",
       y = "Emotional Regulation (sd)",
       x = "Un-Forgiving (sd)")

m12_EmotionRegulation_forgive_plot_z

ggsave(
  m12_EmotionRegulation_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m12_EmotionRegulation_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# k6 ----------------------------------------------------------------------

# need to increase memory
options(future.globals.maxSize = 8000 * 1024^2)

m13_KESSLER6sum_forgive <- brm_multiple(
  KESSLER6sum_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m13_KESSLER6sum_forgive.rds")#,
  # set_prior('normal(0, 1)', class = 'b')
)

m13a_KESSLER6sum_forgive <- brm_multiple(
  as.integer(KESSLER6sum_lead2) ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "negbinomial",
  data = out2_for,
  seed = 1234,
  init = 0,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods",  "m13a_KESSLER6sum_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check()

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m13_KESSLER6sum_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS( 0.08044 , se = 0.00608, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m13_KESSLER6sum_forgive_plot <-
  plot(conditional_effects(
    m13_KESSLER6sum_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m13_KESSLER6sum_forgive_plot_z  <-  m13_KESSLER6sum_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Kessler-6 Distress",
       # subtitle = "Loss shows greater distress",
       y = "Kessler-6 Distress (0-24)",
       x = "Un-Forgiving (sd)")
m13_KESSLER6sum_forgive_plot_z


ggsave(
  m13_KESSLER6sum_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m13_KESSLER6sum_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# test with

test_evalue <- with(out_ch,
                    lm(KESSLER6sum_lead2_z ~
                         VENGEFUL.RUMIN_lead1_z +
                         VENGEFUL.RUMIN  +
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
                         EmotionRegulation_z +
                         Euro_z +
                         GRATITUDE_z +
                         HomeOwner_z +
                         Hours.Exercise_log_z +
                         Hours.Work_z +
                         HLTH.BMI_z  + #
                         HLTH.Fatigue_z + #
                         income_log_z +
                         KESSLER6sum_z + #
                         LIFEMEANING_z + #
                         LIFESAT_z + #
                         lost_job_z +
                         Male_z +
                         NZdep_z +
                         NWI_z +
                         Parent_z +
                         Partner_z +
                         PERFECTIONISM_z +
                         Pol.Orient_z +
                         POWERDEPENDENCE_z + #
                         PWI_z +
                         Relid_z +
                         Respect.Self_z + #
                         Rumination_z + #
                         SELF.CONTROL_z + #
                         SELF.ESTEEM_z + #
                         SexualSatisfaction_z +#
                         SFHEALTH_z +#
                         Smoker_z +#
                         SUPPORT_z +#
                         Urban_z +
                         Volunteers_z))

summary(test_evalue)


##
pp_check()

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  , se = , sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m13a_KESSLER6sum_forgive_plot <-
  plot(conditional_effects(
    m13a_KESSLER6sum_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m13a_KESSLER6sum_forgive_plot_z  <-  m13a_KESSLER6sum_forgive_plot[[1]]  +
 # scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "",
       # subtitle = "Loss shows greater distress",
       y = " (sd)",
       x = "Un-Forgiving (0-24)")

ggsave(
  m13a_KESSLER6sum_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m13a_KESSLER6sum_forgive_plot_z",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)


# LIFESAT_z ---------------------------------------------------------------


m14_LIFESAT_forgive  <- brms::brm_multiple(
  LIFESAT_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  init =0,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m14_LIFESAT_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

##
pp_check(m14_LIFESAT_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m14_LIFESAT_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.05360, se = 0.00632 , sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m14_LIFESAT_forgive_plot <-
  plot(conditional_effects(
    m14_LIFESAT_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m14_LIFESAT_forgive_plot_z  <-  m14_LIFESAT_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Life Satisfaction",
       # subtitle = "Loss shows greater distress",
       y = "Life Satisfaction (sd)",
       x = "Un-Forgiving (sd)")
m14_LIFESAT_forgive_plot_z
ggsave(
  m14_LIFESAT_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m14_LIFESAT_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)


# POWERDEPENDENCE_z -------------------------------------------------------

m15_POWERDEPENDENCE_forgive <- brms::brm_multiple(
  POWERDEPENDENCE_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m15_POWERDEPENDENCE_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m15_POWERDEPENDENCE_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m15_POWERDEPENDENCE_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(0.01457  , se = 0.00664, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m15_POWERDEPENDENCE_forgive_plot <-
  plot(conditional_effects(
    m15_POWERDEPENDENCE_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m15_POWERDEPENDENCE_forgive_plot_z  <-  m15_POWERDEPENDENCE_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Power Dependence",
       # subtitle = "Loss shows greater distress",
       y = "Power Dependence(sd)",
       x = "Un-Forgiving (sd)")

m15_POWERDEPENDENCE_forgive_plot_z
ggsave(
  m15_POWERDEPENDENCE_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m15_POWERDEPENDENCE_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# perfectionism -----------------------------------------------------------

m16_PERFECTIONISM_forgive <- brm_multiple(
  PERFECTIONISM_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init =0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m16_PERFECTIONISM_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m16_PERFECTIONISM_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m16_PERFECTIONISM_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  0.05948 , se =  0.00629, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m16_PERFECTIONISM_forgive_plot <-
  plot(conditional_effects(
    m16_PERFECTIONISM_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m16_PERFECTIONISM_forgive_plot_z  <-  m16_PERFECTIONISM_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Perfectionism",
       # subtitle = "Loss shows greater distress",
       y = "Perfectionism (sd)",
       x = "Un-Forgiving (sd)")

m16_PERFECTIONISM_forgive_plot_z

ggsave(
  m16_PERFECTIONISM_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m16_PERFECTIONISM_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# SELF.ESTEEM_lead2_z -----------------------------------------------------------

m17_SELFESTEEM_forgive <- brm_multiple(
  SELF.ESTEEM_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m17_SELFESTEEM_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m17_SELFESTEEM_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m17_SELFESTEEM_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.06856 , se =  0.00582 , sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m17_SELFESTEEM_forgive_plot <-
  plot(conditional_effects(
    m17_SELFESTEEM_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m17_SELFESTEEM_forgive_plot_z  <-  m17_SELFESTEEM_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Self Esteem",
       # subtitle = "Loss shows greater distress",
       y = "Self Esteem (sd)",
       x = "Un-Forgiving (sd)")

m17_SELFESTEEM_forgive_plot_z

ggsave(
  m17_SELFESTEEM_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m17_SELFESTEEM_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# Emp.WorkLifeBalance_lead2_z ---------------------------------------------------------------

m18_EmpWorkLifeBalance_forgive <-
  brm_multiple(
    Emp.WorkLifeBalance_lead2_z ~
      VENGEFUL.RUMIN_lead1_z +
      VENGEFUL.RUMIN  +
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
      EmotionRegulation_z +
      Euro_z +
      GRATITUDE_z +
      HomeOwner_z +
      Hours.Exercise_log_z +
      Hours.Work_z +
      HLTH.BMI_z  + #
      HLTH.Fatigue_z + #
      income_log_z +
      KESSLER6sum_z + #
      LIFEMEANING_z + #
      LIFESAT_z + #
      lost_job_z +
      Male_z +
      NZdep_z +
      NWI_z +
      Parent_z +
      Partner_z +
      PERFECTIONISM_z +
      Pol.Orient_z +
      POWERDEPENDENCE_z + #
      PWI_z +
      Relid_z +
      Respect.Self_z + #
      Rumination_z + #
      SELF.CONTROL_z + #
      SELF.ESTEEM_z + #
      SexualSatisfaction_z +#
      SFHEALTH_z +#
      Smoker_z +#
      SUPPORT_z +#
      Urban_z +
      Volunteers_z,
    family = "gaussian",
    data = out2_for,
    seed = 1234,
    init = 0,
    warmup = 1000,
    iter = 2000,
    chains = 4,
    backend = "cmdstanr",
    file = here::here("mods", "forgivemods", "m18_EmpWorkLifeBalance_forgive.rds"),
    set_prior('normal(0, 1)', class = 'b')
  )
##
pp_check(m18_EmpWorkLifeBalance_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m18_EmpWorkLifeBalance_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.03079 , se =  0.00793 , sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m18_EmpWorkLifeBalance_forgive_plot <-
  plot(conditional_effects(
    m18_EmpWorkLifeBalance_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m18_EmpWorkLifeBalance_forgive_plot_z  <-  m18_EmpWorkLifeBalance_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Work/Life Balance",
       # subtitle = "Loss shows greater distress",
       y = "Work/Life Balance (sd)",
       x = "Un-Forgiving (sd)")

m18_EmpWorkLifeBalance_forgive_plot_z

ggsave(
  m18_EmpWorkLifeBalance_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m18_EmpWorkLifeBalance_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# gratitude ---------------------------------------------------------------

m19_GRATITUDE_forgive <- brm_multiple(
  GRATITUDE_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init =0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m17_GRATITUDE_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m19_GRATITUDE_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m19_GRATITUDE_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.07172, se =  0.00671, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m19_GRATITUDE_forgive_plot <-
  plot(conditional_effects(
    m19_GRATITUDE_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m19_GRATITUDE_forgive_plot_z  <-  m19_GRATITUDE_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Gratitude",
       # subtitle = "Loss shows greater distress",
       y = "Gratitude (sd)",
       x = "Un-Forgiving (sd)")

m19_GRATITUDE_forgive_plot_z

ggsave(
  m19_GRATITUDE_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m19_GRATITUDE_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

#
# # vengful rumination -------------------------------------------------------
#
# m20_VENGEFULRUMIN_forgive <- brm_multiple(
#   VENGEFUL.RUMIN_lead2_z   ~
#     VENGEFUL.RUMIN_lead1_z +
#     VENGEFUL.RUMIN  +
#     AGREEABLENESS_z +
#     CONSCIENTIOUSNESS_z +
#     EXTRAVERSION_z  +
#     HONESTY_HUMILITY_z +
#     NEUROTICISM_z +
#     OPENNESS_z +
#     Age_z +
#     Alcohol.Frequency_z + #
#     Alcohol.Intensity_log_z + #
#     Bodysat_z +
#     Believe.God_z +
#     Believe.Spirit_z +
#     BELONG_z + #
#     CharityDonate_log_z + #
#     ChildrenNum_z +
#     Church_z +
#     community +
#     Edu_z +
#     Employed_z +
#     EmotionRegulation_z +
#     Euro_z +
#     GRATITUDE_z +
#     HomeOwner_z +
#     Hours.Exercise_log_z +
#     Hours.Work_z +
#     HLTH.BMI_z  + #
#     HLTH.Fatigue_z + #
#     income_log_z +
#     KESSLER6sum_z + #
#     LIFEMEANING_z + #
#     LIFESAT_z + #
#     lost_job_z +
#     Male_z +
#     NZdep_z +
#     NWI_z +
#     Parent_z +
#     Partner_z +
#     PERFECTIONISM_z +
#     Pol.Orient_z +
#     POWERDEPENDENCE_z + #
#     PWI_z +
#     Relid_z +
#     Respect.Self_z + #
#     Rumination_z + #
#     SELF.CONTROL_z + #
#     SELF.ESTEEM_z + #
#     SexualSatisfaction_z +#
#     SFHEALTH_z +#
#     Smoker_z +#
#     SUPPORT_z +#
#     Urban_z +
#     Volunteers_z,
#   family = "gaussian",
#   data = out2_for,
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   init =0,
#   chains = 4,
#   backend = "cmdstanr",
#   file = here::here("mods", "forgivemods", "m20_VENGEFULRUMIN_forgive.rds"),
#   set_prior('normal(0, 1)', class = 'b')
# )
# ##
# pp_check()
#
# ## for binary outcomes
# lp <-
#   posterior_linpred(
#     X,
#     newdata = newdata,
#   )
# )
# round( posterior_summary(lp[, 2] / lp[, 1]), 3)
#
# posterior_summary(lp[, 1] / lp[, 2])
#
#
# # model coeff
# out <- model_parameters(m20_VENGEFULRUMIN_forgive, dispersion = TRUE, centrality = "mean",
#                         test = "pd",
#                         digits = 5,
#                         diagnostic =  NULL,
#                         rope_range = NULL)%>% slice(2)
# out
#
# # evalue
# round( EValue::evalues.OLS(  , se = , sd = 1, delta = 2, true = 0), 3)
#
# # evalue for RR
# round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)
#
# #table
# lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)
#
# # another table
# m1_bayes_table <-
#   parameters::model_parameters()
# m1_bayes_table
# plot(m1_bayes_table)
#
# # graph
# m20_VENGEFULRUMIN_forgive_plot <-
#   plot(conditional_effects(
#     m20_VENGEFULRUMIN_forgive,
#     "VENGEFUL.RUMIN_lead1_z",
#     ndraws = 500,
#     spaghetti = T
#   ))
#
# m20_VENGEFULRUMIN_forgive_plot_z  <-  m20_VENGEFULRUMIN_forgive_plot[[1]]  +
#   scale_y_continuous(limits = c(-.5, .5)) +
#   scale_x_continuous(limits = c(-2, 2)) +
#   labs(title = "Vengeful/Forgiveness",
#        # subtitle = "Loss shows greater distress",
#        y = "Vengeful/Forgiveness (sd)",
#        x = "Un-Forgiving (sd)")
#
# ggsave(
#   X_plot_z,
#   path = here::here(here::here("figs", "figs_forgive")),
#   width = 16,
#   height = 9,
#   units = "in",
#   filename = "X_plot_z.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 400
# )

# life meaning ------------------------------------------------------------

m21_LIFEMEANING_forgive <- brm_multiple(
  LIFEMEANING_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  init = 0,
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m21_LIFEMEANING_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)



m21a_LIFEMEANING_forgive <- brm_multiple(
  LIFEMEANING_lead2ord ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = cumulative("probit"),
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m21a_LIFEMEANING_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

##
pp_check(m21_LIFEMEANING_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m21_LIFEMEANING_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.05211  , se =  0.00624, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m21_LIFEMEANING_forgive_plot <-
  plot(conditional_effects(
    m21_LIFEMEANING_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m21_LIFEMEANING_forgive_plot_z  <-  m21_LIFEMEANING_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Life Meaning",
       # subtitle = "Loss shows greater distress",
       y = "Life Meaning (sd)",
       x = "Un-Forgiving (sd)")

m21_LIFEMEANING_forgive_plot_z
ggsave(
  m21_LIFEMEANING_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m21_LIFEMEANING_forgive_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)


# honesty -----------------------------------------------------------------

m22_HONESTYHUMILITY_forgive <- brm_multiple(
  HONESTY_HUMILITY_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init =0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m22_HONESTYHUMILITY_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m22_HONESTYHUMILITY_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m22_HONESTYHUMILITY_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(   -0.04750 , se =  0.00607, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m22_HONESTYHUMILITY_forgive_plot <-
  plot(conditional_effects(
    m22_HONESTYHUMILITY_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m22_HONESTYHUMILITY_forgive_plot_z  <-  m22_HONESTYHUMILITY_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Honesty Humility",
       # subtitle = "Loss shows greater distress",
       y = "Honesty Humility (sd)",
       x = "Un-Forgiving (sd)")
m22_HONESTYHUMILITY_forgive_plot_z
ggsave(
  m22_HONESTYHUMILITY_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m22_HONESTYHUMILITY_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)



# BELONG_z ----------------------------------------------------------------

m23_BELONG_forgive <- brm_multiple(
  BELONG_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m23_BELONG_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m23_BELONG_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m23_BELONG_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(   -0.06278, se =  0.00625 , sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m23_BELONG_forgive_plot <-
  plot(conditional_effects(
    m23_BELONG_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m23_BELONG_forgive_plot_z  <-  m23_BELONG_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Social Belonging",
       # subtitle = "Loss shows greater distress",
       y = "Social Belonging (sd)",
       x = "Un-Forgiving (sd)")

m23_BELONG_forgive_plot_z

ggsave(
  m23_BELONG_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m23_BELONG_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)


# SUPPORT_z ---------------------------------------------------------------

m24_SUPPORT_forgive <- brm_multiple(
  SUPPORT_lead2_z ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  init = 0,
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m24_SUPPORT_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m24_SUPPORT_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m24_SUPPORT_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.05206 , se = , 0.00649 , sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m24_SUPPORT_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m24_SUPPORT_forgive_plot <-
  plot(conditional_effects(
    m24_SUPPORT_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m24_SUPPORT_forgive_plot_z  <-  m24_SUPPORT_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Social Support",
       # subtitle = "Loss shows greater distress",
       y = "Social Support (sd)",
       x = "Un-Forgiving (sd)")

m24_SUPPORT_forgive_plot_z

ggsave(
  m24_SUPPORT_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m24_SUPPORT_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# Volunteers_lead2 --------------------------------------------------------------

m25_Volunteers_forgive <- brm_multiple(
  Volunteers_lead2 ~ #| trials(1) ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "poisson",
  #family = "poisson",#binomial("identity"),
  #bernoulli(link = "cloglog")
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m25_Volunteers_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
#
#
# ###  version
# m14a_church_volunteers <- brm_multiple(
#   Volunteers_lead2 ~
#     VENGEFUL.RUMIN_lead1_z +
#     VENGEFUL.RUMIN  +
#     AGREEABLENESS_z +
#     CONSCIENTIOUSNESS_z +
#     EXTRAVERSION_z  +
#     HONESTY_HUMILITY_z +
#     NEUROTICISM_z +
#     OPENNESS_z +
#     Age_z +
#     Alcohol.Frequency_z + #
#     Alcohol.Intensity_log_z + #
#     Bodysat_z +
#     Believe.God_z +
#     Believe.Spirit_z +
#     BELONG_z + #
#     CharityDonate_log_z + #
#     ChildrenNum_z +
#     Church_z +
#     community +
#     Edu_z +
#     Employed_z +
#     EmotionRegulation_z +
#     Euro_z +
#     GRATITUDE_z +
#     HomeOwner_z +
#     Hours.Exercise_log_z +
#     Hours.Work_z +
#     HLTH.BMI_z  + #
#     HLTH.Fatigue_z + #
#     income_log_z +
#     KESSLER6sum_z + #
#     LIFEMEANING_z + #
#     LIFESAT_z + #
#     lost_job_z +
#     Male_z +
#     NZdep_z +
#     NWI_z +
#     Parent_z +
#     Partner_z +
#     PERFECTIONISM_z +
#     Pol.Orient_z +
#     POWERDEPENDENCE_z + #
#     PWI_z +
#     Relid_z +
#     Respect.Self_z + #
#     Rumination_z + #
#     SELF.CONTROL_z + #
#     SELF.ESTEEM_z + #
#     SexualSatisfaction_z +#
#     SFHEALTH_z +#
#     Smoker_z +#
#     SUPPORT_z +#
#     Urban_z +
#     Volunteers_z,
#   family = bernoulli(link = "cloglog"),
#   # family = bernoulli(link = "probit"),
#   data = out2_for,
#   seed = 1234,
#   init = 0,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   file = here::here("mods", "forgivemods", "m23a_Volunteers_forgive.rds"),
#   set_prior('normal(0, 1)', class = 'b')
# )
# ##
# pp_check(m14_church_volunteers)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m25_Volunteers_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  , se = , sd = 1, delta = 2, true = 0), 3)

# evalue for RR
exp( 0.02969)
exp( 0.02969 +  0.03690)
exp( 0.02969 -  0.03690)
round( EValue::evalues.RR( 1.030135, lo = 0.9928159 , hi = 1.068857, true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m25_Volunteers_forgive_plot <-
  plot(conditional_effects(
    m25_Volunteers_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m25_Volunteers_forgive_plot_z  <-  m25_Volunteers_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(0, .3)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Volunteers",
       # subtitle = "Loss shows greater distress",
       y = "Volunteers (sd)",
       x = "Un-Forgiving (sd)")

m25_Volunteers_forgive_plot_z
ggsave(
  m25_Volunteers_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m25_Volunteers_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# Charity.Donate_logz -----------------------------------------------

m26_CharityDonate_forgive <- brm_multiple(
  CharityDonate_log_lead2_z
  VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  init = 0,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m26_CharityDonate_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)


m26a_CharityDonate_forgive <- brm_multiple(
  CharityDonate_lead2 ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "negbinomial",
  data = out2_for,
  init = 0,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m26a_CharityDonate_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m26a_CharityDonate_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m26_CharityDonate_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(  -0.01605 , se =  0.00829, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m26_CharityDonate_forgive_plot <-
  plot(conditional_effects(
    m26_CharityDonate_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 200,
    spaghetti = T
  ))

m26_CharityDonate_forgive_plot_z  <-  m26_CharityDonate_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5 ,.5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Charity Donate",
       # subtitle = "Loss shows greater distress",
       y = "Charity Donate (sd)",
       x = "Un-Forgiving (sd)")


m26_CharityDonate_forgive_plot_z

ggsave(
  m26_CharityDonate_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m26_CharityDonate_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)


# community ---------------------------------------------------------------

m27_community_forgive <- brm_multiple(
  community_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init =0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m27_community.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m27_community_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m27_community_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS(   -0.03028, se =  0.00688, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m27_community_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m27_community_forgive_plot <-
  plot(conditional_effects(
    m27_community_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m27_community_forgive_plot_z  <-  m27_community_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "Neighbourhood Community",
       # subtitle = "Loss shows greater distress",
       y = "Neighbourhood Community (sd)",
       x = "Un-Forgiving (sd)")

m27_community_forgive_plot_z

ggsave(
  m27_community_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m27_community_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

# NWI ---------------------------------------------------------------------

m28_NWI_forgive <- brm_multiple(
  NWI_lead2_z   ~
    VENGEFUL.RUMIN_lead1_z +
    VENGEFUL.RUMIN  +
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
    EmotionRegulation_z +
    Euro_z +
    GRATITUDE_z +
    HomeOwner_z +
    Hours.Exercise_log_z +
    Hours.Work_z +
    HLTH.BMI_z  + #
    HLTH.Fatigue_z + #
    income_log_z +
    KESSLER6sum_z + #
    LIFEMEANING_z + #
    LIFESAT_z + #
    lost_job_z +
    Male_z +
    NZdep_z +
    NWI_z +
    Parent_z +
    Partner_z +
    PERFECTIONISM_z +
    Pol.Orient_z +
    POWERDEPENDENCE_z + #
    PWI_z +
    Relid_z +
    Respect.Self_z + #
    Rumination_z + #
    SELF.CONTROL_z + #
    SELF.ESTEEM_z + #
    SexualSatisfaction_z +#
    SFHEALTH_z +#
    Smoker_z +#
    SUPPORT_z +#
    Urban_z +
    Volunteers_z,
  family = "gaussian",
  data = out2_for,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init =0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "forgivemods", "m28_NWI_forgive.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
##
pp_check(m28_NWI_forgive)

## for binary outcomes
lp <-
  posterior_linpred(
    X,
    newdata = newdata,
  )
)
round( posterior_summary(lp[, 2] / lp[, 1]), 3)

posterior_summary(lp[, 1] / lp[, 2])


# model coeff
out <- model_parameters(m28_NWI_forgive, dispersion = TRUE, centrality = "mean",
                        test = "pd",
                        digits = 5,
                        diagnostic =  NULL,
                        rope_range = NULL)%>% slice(2)
out

# evalue
round( EValue::evalues.OLS( 0.00309 , se = .00721, sd = 1, delta = 2, true = 0), 3)

# evalue for RR
round( EValue::evalues.RR(, lo =  , hi = , true = 1), 3)

#table
lazerhawk::brms_SummaryTable(m1_bmi_forgive, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters()
m1_bayes_table
plot(m1_bayes_table)

# graph
m28_NWI_forgive_plot <-
  plot(conditional_effects(
    m28_NWI_forgive,
    "VENGEFUL.RUMIN_lead1_z",
    ndraws = 500,
    spaghetti = T
  ))

m28_NWI_forgive_plot_z  <-  m28_NWI_forgive_plot[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  scale_x_continuous(limits = c(-2, 2)) +
  labs(title = "National Wellbeing",
       # subtitle = "Loss shows greater distress",
       y = "National Wellbeing (sd)",
       x = "Un-Forgiving (sd)")

ggsave(
  m28_NWI_forgive_plot_z,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "m28_NWI_forgive_plot_z.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)




# multiplot ---------------------------------------------------------------

library(patchwork)
comp_plot <- m5a_fatigue_forgive_plot_z +
m10_SexualSatisfaction_forgive_plot_z +
  m16_PERFECTIONISM_forgive_plot_z +
m11_PWI_forgive_plot_z +
m14_LIFESAT_forgive_plot_z +
m17_SELFESTEEM_forgive_plot_z +
m19_GRATITUDE_forgive_plot_z +
m13_KESSLER6sum_forgive_plot_z +
  m27_community_forgive_plot_z +
  plot_annotation(title = "Comparative Outcomes Plot",
                  subtitle = "Selected effects of Un-Forgiveness", tag_levels = "A")

comp_plot
ggsave(
  comp_plot,
  path = here::here(here::here("figs", "figs_forgive")),
  width = 16,
  height = 9,
  units = "in",
  filename = "comp_plot.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)
comp_plot

