# pedro's gratitude study
# read files
source(here::here("scripts", "funs.R"))

# read libraries in
source(here::here("scripts", "libs.R"))
library("dplyr")
library("tidyr")

# note env

# read data: NZAVS waves 2018, 2019, 2020
df <- readRDS(here::here("data_raw", "df.rds"))

#check data
skimr::skim(df)

# order vars
df$GenCohort <- ordered(df$GenCohort, levels = c("Gen_Silent: born< 1946", "Gen Boomers: born >= 1946 & b.< 1965",
                                                 " GenX: born >=1961 & b.< 1981","GenZ: born >= 1996 "))

# view
df %>%   # not there
  filter(Wave == 2020) %>%
  summarise(Respect.Self) #fully missing


# select variables
gdf <-
  df %>%
  select(
    Id,
    YearMeasured,
    Wave,
    GendAll,
    EthCat,
    Age,
    Edu,
    NZdep,
    Employed,
    Urban,
    Household.INC,
    Parent,
    Partner,
    HomeOwner,
    Pol.Orient,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    Religious,
    Religion.Church,
    Believe.Spirit,
    Believe.God,
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
    Env.ClimateChgCause,
    Env.ClimateChgReal #,
  ) %>%
  dplyr::mutate(org2019 =  ifelse(Wave == 2019 & YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
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
  dplyr::mutate(Edu = as.numeric(Edu),
                Volunteers = if_else(HoursCharity == 1, 1, 0),
                Depressed = (as.numeric(cut(
                  KESSLER6sum,
                  breaks = c(-Inf, 13, Inf),
                  labels = c("0", "1"),
                  right = FALSE))-1),
                # Church = cut(
                #   Religion.Church,
                #   breaks = c(-Inf,0,1,2,3,4,Inf),
                #   labels = c("Never","1xMonth","2xMonth","3xMonth","weekly",">weekly"),
                #   right = FALSE),
                EthCat = factor(EthCat, labels = c("Euro","Maori","Pacific","Asian")),
                not_euro = as.numeric(if_else(EthCat =="Euro",0,1)),
                Church = ifelse(Religion.Church >8, 8, Religion.Church),
                Exercise_log = log(Hours.Exercise + 1 ),
                income_log = log(Household.INC + 1),
                CharityDonate_log = log(CharityDonate + 1),
                Alcohol.Intensity_log = log(Alcohol.Intensity +1))%>%
  arrange(Id, Wave)  %>%
  dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
  dplyr::mutate(income_log_lead1 = lead(income_log, n=1),
                GRATITUDE_lead1 = lead(GRATITUDE, n=1)) %>%
  # inc_prop = (income_log / (income_log_lead1) - 1),
  # KESSLER6sum_log = log(KESSLER6sum + 1),
  # HLTH.BMI_z = as.numeric(scale(HLTH.BMI)),
  # Age_z = as.numeric(scale(Age)),
  # Hours.Work_z = as.numeric(scale(Hours.Work)))%>%  # Proportional change
  dplyr::mutate(across(
    c(
      income_log,
      Depressed,
      KESSLER6sum,
      HLTH.Fatigue,
      Rumination,
      SFHEALTH,
      LIFEMEANING,
      LIFESAT,
      PWI,
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
      Env.ClimateChgReal
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
  dplyr::filter(!is.na(GRATITUDE),    # needed for the intervention
                !is.na(GRATITUDE_lead1)) %>%  #needed for the intervention
  dplyr::select(-c(Religion.Church,
                   EthCat,
                   Respect.Self_lead2,
                   CharityDonate,
                   Household.INC,
                   Alcohol.Intensity,
                   org2018,
                   hold18,
                   Emp.WorkLifeBalance,
                   YearMeasured,
                   Hours.Exercise,
                   Employed,
                   Hours.Exercise_lead2,org2019,hold19))%>%
#  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing
  arrange(Id,Wave) %>%
  data.frame() %>%
  mutate(across( where(is.double), as.numeric))

# check N of ids
length(unique(gdf$Id)) # 34917

# Exercise = cut(
#   Hours.Exercise,
#   breaks = c(-Inf,0,2.5,7,Inf),
#   labels = c("Never","0-upto-2.5hrs","2.5-upto-7hrs",">7hrs"),
#   right = FALSE),


# inspect data
skim(gdf)

#
gdf %>%
  group_by(Wave) %>%
  summarise(across(Id, n_distinct))



# consider removing
df%>%
  dplyr::filter(Id != 9630)

# glimse
gdf%>%
  summarise(across(c(PWI_lead2, LIFESAT_lead2, LIFEMEANING_lead2), ~mean(.x, na.rm=TRUE), ~sd(.x, na.rm=TRUE), n_distinct()))


# cases
length(unique(gdf$Id))


#check
skimr::skim(gdf)

# save function
saveh(gdf, "gdf")

# read if needed
gdf<- readh("gdf")



gdf %>%
  summarise(across(c(income_log, income_log_lead2),
                   list(mean = mean, range=range, sd = sd), na.rm = TRUE, .names = "{col}_{fn}"))


# GFS coding
# Church
#[Never, a few times a year, a few times a month, weekly, more than once per week] [BMMRS 34]

# Religious TXTs
#[Never, occasionally, daily, more than daily] [BMMRS 15, modified]

#  Charity
#  In the past month, have you volunteered your time to an organization?2


table1::table1(
  ~ income_log +
  Depressed +
  KESSLER6sum +
  HLTH.Fatigue +
  Rumination +
  SFHEALTH  +
  LIFEMEANING +
  LIFESAT +
  PWI +
  SELF.ESTEEM +
  SELF.CONTROL +
  Respect.Self +
  #Emp.WorkLifeBalance +
  Alcohol.Frequency +
  Alcohol.Intensity_log +
  Exercise_log +
  HLTH.BMI +
  Smoker +
  ChildrenNum +
  NWI +
  BELONG +
  SUPPORT +
  CharityDonate_log +
  Volunteers,
  data = gdf
)


# missingness in 2020  1/3 of the data are missing

table1::table1(
  ~ income_log_lead2 +
    Depressed_lead2 +
    KESSLER6sum_lead2 +
    HLTH.Fatigue_lead2 +
    Rumination_lead2 +
    SFHEALTH_lead2  +
    LIFEMEANING_lead2 +
    LIFESAT_lead2 +
    PWI_lead2 +
    SELF.ESTEEM_lead2 +
    SELF.CONTROL_lead2 +
    #Respect.Self_lead2 +
    Emp.WorkLifeBalance_lead2 +
    Alcohol.Frequency_lead2 +
    Alcohol.Intensity_log_lead2 +
    Exercise_log_lead2 +
    HLTH.BMI_lead2 +
    Smoker_lead2 +
    ChildrenNum_lead2 +
    NWI_lead2 +
    BELONG_lead2 +
    SUPPORT_lead2 +
    CharityDonate_log_lead2 +
    Volunteers_lead2,
  data = gdf
)



# mice model  -------------------------------------------------------------
library(mice)
g_mice <- gdf %>%
  dplyr::select(-c( Wave, Id))

# Visualise missing
library(naniar)
naniar::gg_miss_var(g_mice)

vis_miss(g_mice,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(g_mice)

#for_mice$inc_prop <-
#  for_mice$income_log / (for_mice$income_log_lead1 - 1)
ini <- mice(g_mice, m = 1, maxit = 0)
ini
meth <- ini$meth
meth
# #meth["inc_prop"] <- "~ I(income_log/(income_log_lead1 - 1))"
# pred <- ini$pred


# impute
out <- mice::mice(g_mice,
                  #meth = "rfcont",
                  #pred = pred,
                  seed = 0, m = 10)

# save
saveh(out, "out")

# read
out <- readh("out")

# plots  (takes time)
#dev.off()
#stripplot(out)
#densityplot(out) # too much time
#plot(out)



# data warangling
long <- mice::complete(out, "long", inc = TRUE)
# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(long)

#long$LIFESAT_lead1_z <- with(long, scale(LIFESAT_lead1))


# create variables in z score
long2 <- long %>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z"))%>%
  select(-c(.imp_z, .id_z))# Respect for Self is fully missing

# review
skim(long2)

# get colnames
colnames(long2) # get names

#long$attrition <- with(long, ndf$attrition_lead2)
# neect to get into shape
long3 <- long2 %>% mutate_if(is.matrix, as.vector)
out2 <- mice::as.mids(long3)


saveh(out2, "out2")
out2 <- readh("out2")


# urban vs rural
#xyplot(out2, attrition ~ LIFESAT_lead1_z,pch=18,cex=1)

# Try full model
#devtools::install_github('IQSS/Zelig')

# k6 ----------------------------------------------------------------------

test1 <-
  with(out2, glm(
    as.numeric(KESSLER6sum_lead2_z) ~
      GRATITUDE_lead1_z +
      GRATITUDE_z +
      income_log_z +
      Church_lead1_z +
      Church_z +
      income_log_z +  #inc_prop
      AGREEABLENESS_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      Age_z +
      began_relationship_z +
      Believe.God_z +
      Believe.Spirit_z +
      CharityDonate_log_z +
      ChildrenNum_z +
      Church_z +
      Edu_z +
      Exercise_log_z +
      GendAll_z +
      HomeOwner_z +
      HoursCharity_z +
      Hours.Work_z +
      lost_job_z +
      not_euro_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      # partnerlost_job_z +
      Pol.Orient_z +
      Religious_z +
      Smoker_z +
      SUPPORT_z +
      Volunteers_z +
      Urban_z +
      Alcohol.Frequency_z +
      Alcohol.Intensity_log_z +
      BELONG_z +
      ChildrenNum_z +
      # Depressed +
      KESSLER6sum_z +
      HLTH.BMI_z +
      HLTH.Fatigue_z +
      Rumination_z +
      LIFEMEANING_z +
      SFHEALTH_z +
      LIFESAT_z +
      NWI_z +
      PWI_z +
      Respect.Self_z +
      Rumination_z +
      SELF.CONTROL_z +
      SELF.ESTEEM_z +
      SUPPORT_z
  ))
tp <- parameters::model_parameters(test1)
plot(tp)


parameters::model_parameters(test1) %>% print_html()
