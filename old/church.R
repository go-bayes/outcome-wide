
# read data
df <- readRDS(here::here("data_raw", "df.Rds"))


# pedro's gratitude study
# read files
source(here::here("scripts", "funs.R"))

# read libraries in
source(here::here("scripts", "libs.R"))
library("dplyr")
library("tidyr")


#check data
skimr::skim(df)

# order vars
df$GenCohort <- ordered(df$GenCohort, levels = c("Gen_Silent: born< 1946", "Gen Boomers: born >= 1946 & b.< 1965",
                                                 " GenX: born >=1961 & b.< 1981","GenZ: born >= 1996 "))

# view
df %>%   # not there
  dplyr::filter(Wave == 2020) %>%
  summarise(Respect.Self) #fully missing

table(df$GendAll)

# select variables
c_df <- df %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0) )%>%
  dplyr::filter(
    (Wave == 2018  & YearMeasured  == 1 ) |
      (Wave == 2019  &  YearMeasured  == 1 ) |  (Wave == 2020 )
  )  %>% # Eligibility criteria
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
  )%>%
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
  droplevels()%>%
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
  # dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
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
      Env.ClimateChgReal
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
  dplyr::filter(Wave == 2018) %>%
  dplyr::filter(!is.na(Church),    # needed for the intervention
                !is.na(Church_lead1)) %>%  #needed for the intervention
  # dplyr::filter(!is.na(GRATITUDE),    # needed for the intervention
  #               !is.na(GRATITUDE_lead1)) %>%  #needed for the intervention
  dplyr::select(-c(Religion.Church,
                   EthCat,
                   Respect.Self_lead2,
                   CharityDonate,
                   Household.INC,
                   Alcohol.Intensity,
                   org2018,
                   not_euro,
                   not_euro_lead2,
                   hold18,
                   Emp.WorkLifeBalance,
                   YearMeasured,
                   Hours.Exercise,
                   Employed,
                   Hours.Exercise_lead2,
                   org2019,
                   hold19))%>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing
  arrange(Id,Wave) %>%
  data.frame() %>%
  mutate(across( where(is.double), as.numeric))

# check N of ids
length(unique(c_df$Id)) # 33189

# Exercise = cut(
#   Hours.Exercise,
#   breaks = c(-Inf,0,2.5,7,Inf),
#   labels = c("Never","0-upto-2.5hrs","2.5-upto-7hrs",">7hrs"),
#   right = FALSE),

c_df %>%
  select(Church, Church_lead1) %>%
  summarise(across(everything(), c(mean=mean,sd=sd)))

# inspect data
skim(c_df)

c_df %>%
  group_by(Wave) %>%
  summarise(across(Id, n_distinct))



# consider removing
c_df%>%
  dplyr::filter(Id == 9630)

# glimse
c_df%>%
  summarise(across(c(PWI_lead2, Church, Church_lead1), ~mean(.x, na.rm=TRUE), ~sd(.x, na.rm=TRUE), n_distinct()))


# cases
length(unique(c_df$Id))



# save function
saveh(c_df, "c_df")

# read if needed
c_df<- readh("c_df")



c_df %>%
  summarise(across(c(Church, Church_lead1),
                   list(mean = mean, range=range, sd = sd), na.rm = TRUE, .names = "{col}_{fn}"))


# GFS coding
# Church
#[Never, a few times a year, a few times a month, weekly, more than once per week] [BMMRS 34]

# Religious TXTs
#[Never, occasionally, daily, more than daily] [BMMRS 15, modified]

#  Charity
#  In the past month, have you volunteered your time to an organization?2


table1::table1(
  ~ Church +
    Church_lead1 +
    income_log +
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
    Volunteers +
    SexualSatisfaction+
    POWERDEPENDENCE +
    Your.Future.Security +
    Your.Personal.Relationships +
    Your.Health +
    Standard.Living +
    #Env.SacWilling,
    #Env.SacMade,
    Env.ClimateChgCause +
    Env.ClimateChgReal,
  data = c_df
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
    Volunteers_lead2 +
    SexualSatisfaction_lead2 +
    POWERDEPENDENCE_lead2 +
    Your.Future.Security_lead2 +
    Your.Personal.Relationships_lead2 +
    Your.Health_lead2 +
    Standard.Living_lead2 +
    #Env.SacWilling,
    #Env.SacMade,
    Env.ClimateChgCause_lead2 +
    Env.ClimateChgReal_lead2,
  data = c_df
)

# I do not have enough power or control over important parts of my life.
str(ch_df)


# facets ------------------------------------------------------------------
library(lcsm)
df$Religion.Church
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




# mice model  -------------------------------------------------------------
library(mice)
g_mice <- c_df %>%
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
out_cdf <- mice::mice(g_mice,
                      #meth = "rfcont",
                      #pred = pred,
                      seed = 0, m = 10)

# save
saveh(out_cdf, "out_cdf")

# read
out_cdf <- readh("out_cdf")
warnings()

outlist2 <- row.names(out_cdf)[out_cdf$outflux < 0.5]
length(outlist2)

head(out_cdf$loggedEvents, 10)

# plots  (takes time)
#dev.off()
#stripplot(out)
#densityplot(out) # too much time
#plot(out)



# data warangling
long <- mice::complete(out_cdf, "long", inc = TRUE)
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
out2_ch <- mice::as.mids(long3)


saveh(out2_ch, "out2_ch")
out2_ch <- readh("out2_ch")


# urban vs rural
#xyplot(out2, attrition ~ LIFESAT_lead1_z,pch=18,cex=1)
c_df$Alcohol.Intensity_log
# Try full model
#devtools::install_github('IQSS/Zelig')
c_df$Religious
# k6 ----------------------------------------------------------------------
m1 <-
  with(out2_ch, lm(
    as.numeric(KESSLER6sum_lead2_z) ~
      Church_lead1 +
      Church +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
     # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z  +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      Standard.Living_z +
      NWI_z +
      BELONG_z +
      SUPPORT_z +
    #  CharityDonate_z +
    #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.CONTROL_z +
      #  Respect.Self,
     # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log
  ))
p1 <- parameters::model_parameters(m1)
plot(p1)
parameters::model_parameters(m1) %>% print_html()




# life meaning ------------------------------------------------------------

m2 <-
  with(out2_ch, lm(
    as.numeric(LIFEMEANING_lead2_z) ~
      Church_lead1 +
      Church +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
     # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.CONTROL_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log
  ))
p2 <- parameters::model_parameters(m2)
plot(p2)
parameters::model_parameters(m2) %>% print_html()
p2 <- parameters::model_parameters(m2)
plot(p2)
parameters::model_parameters(test1) %>% print_html()

# health-fatigue ----------------------------------------------------------

m3 <-
  with(out2_ch, lm(
    as.numeric(HLTH.Fatigue_lead2_z) ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
     # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.CONTROL_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log
  ))
p3 <- parameters::model_parameters(m3)
plot(p3)
parameters::model_parameters(cp3) %>% print_html()



# depressed ---------------------------------------------------------------



m3c <-
  with(out2_ch, glm(
    as.numeric(Depressed) ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.CONTROL_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log,
    family = "poisson"))
p3c <- parameters::model_parameters(m3c)
plot(p3c)
summary(m3c)
parameters::model_parameters(cp3) %>% print_html()

# LIFESAT_z ---------------------------------------------------------------
m3b <-
  with(out2_ch, glm(
    as.numeric(LIFESAT_lead2_z) ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.CONTROL_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log
  ))
m3b <- parameters::model_parameters(m3b)
plot(m3b)
parameters::model_parameters(cp3) %>% print_html()


# rumination --------------------------------------------------------------

m4 <-
  with(out2_ch, glm(
    as.numeric(Rumination_lead2_z) ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log
  ))
cp4 <- parameters::model_parameters(m4)
plot(cp4)


# SexualSatisfaction_z ----------------------------------------------------


m5 <-
  with(out2_ch, glm(
    as.numeric(SexualSatisfaction_lead2_z) ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log
  ))
cp5 <- parameters::model_parameters(m5)
plot(cp5)


# POWERDEPENDENCE_z -------------------------------------------------------


m6 <-
  with(out2_ch, glm(
    as.numeric(POWERDEPENDENCE_lead2_z) ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log
  ))
cp6 <- parameters::model_parameters(m6)
plot(cp6)



# BELONG_z ----------------------------------------------------------------

m7 <-
  with(out2_ch, lm(
    as.numeric(BELONG_lead2_z) ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log
  ))
p7 <- parameters::model_parameters(m7)
plot(p7)


# SUPPORT_z ---------------------------------------------------------------

m8 <-
  with(out2_ch, lm(
    as.numeric(SUPPORT_lead2_z) ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log
  ))
p8 <- parameters::model_parameters(m8)
plot(p8)



# PWI_z -------------------------------------------------------------------


m9 <-
  with(out2_ch, lm(
    as.numeric(PWI_lead2_z) ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log
  ))
p9<- parameters::model_parameters(m9)
plot(p9)



# NWI_lead2_z -------------------------------------------------------------------


m9 <-
  with(out2_ch, lm(
    as.numeric(PWI_lead2_z) ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log
  ))
p9<- parameters::model_parameters(m9)
plot(p9)



# SFHEALTH_lead2_z --------------------------------------------------------------


m10 <-
  with(out2_ch, lm(
    as.numeric(SFHEALTH_lead2_z) ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log
  ))
p10<- parameters::model_parameters(m10)
plot(p10)


# SELF.ESTEEM_lead2_z -----------------------------------------------------------
ch_df$SELF.ESTEEM

m11 <-
  with(out2_ch, lm(
    as.numeric(SELF.ESTEEM_lead2_z) ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log
  ))
p11<- parameters::model_parameters(m11)
plot(p11)


# Emp.WorkLifeBalance_lead2_z ---------------------------------------------------------------


m12 <-
  with(out2_ch, lm(
    as.numeric(Emp.WorkLifeBalance_lead2_z) ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log
  ))
p12<- parameters::model_parameters(m12)
plot(p12)


# Volunteers_lead2 --------------------------------------------------------------

m13 <-
  with(out2_ch, glm(
    Volunteers_lead2 ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log,
    family = "poisson"
  ))
p13 <- parameters::model_parameters(m13)
p13
plot(p13)


# Charity.Donate_logz -----------------------------------------------
c_df$CharityDonate_log_lead2
m14 <-
  with(out2_ch, lm(
    CharityDonate_log_lead2 ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log
  ))
p14 <- parameters::model_parameters(m14)
p14
plot(p14)


#  Alcohol.Intensity_logz -------------------------------------------------
str(c_df)

m15 <-
  with(out2_ch, lm(
    Alcohol.Intensity_log_lead2_z   ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log_z
  ))
p15 <- parameters::model_parameters(m15)
p15
plot(p15)


# Alcohol.Frequency_lead2_z , ---------------------------------------------------

m16 <-
  with(out2_ch, lm(
    Alcohol.Frequency_lead2_z   ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log_z
  ))
p16 <- parameters::model_parameters(m16)
p16
plot(p16)

# Smoker_bz ---------------------------------------------------------------

m17 <-
  with(out2_ch, glm(
    Smoker_lead2   ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log_z,
    family = "poisson"
  ))
p17 <- parameters::model_parameters(m17)
p17
plot(p17)

# HLTH.BMI_lead2_z ---------------------------------------------------------

m18 <-
  with(out2_ch, lm(
    HLTH.BMI_lead2_z   ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log_z
  ))
p18 <- parameters::model_parameters(m18)
p18
plot(p18)


# Hours.Exercise_log_lead2z -----------------------------------------------
m19 <-
  with(out2_ch, lm(
    Exercise_log_lead2_z   ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum_z +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log_z
  ))
p19 <- parameters::model_parameters(m19)
p19
plot(p19)






# ChildrenNum_lead2 -------------------------------------------------------

m20 <-
  with(out2_ch, glm(
    ChildrenNum_lead2   ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log_z,
    family = "poisson"
  ))
p20 <- parameters::model_parameters(m20)
p20
plot(p20)




# Env.ClimateChgReal_z  ---------------------------------------------------

m21 <-
  with(out2_ch, lm(
    Env.ClimateChgReal_lead2_z   ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log_z
  ))
p21 <- parameters::model_parameters(m21)
p21
plot(p21)





#  Env.ClimateChgCause_z --------------------------------------------------

m22 <-
  with(out2_ch, lm(
    Env.ClimateChgCause_lead2_z   ~
      Church_lead1 +
      Church +
      Exercise_log_z +
      Age_z +
      Hours.Work_z +
      Male_z +
      ChildrenNum +
      Edu_z +
      # Employed_z +
      Exercise_log_z +
      Euro_z +
      HomeOwner_z +
      income_log_z +
      Pol.Orient_z +
      Urban_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      Smoker_z +
      HLTH.BMI_z +
      Smoker_z +
      lost_job_z +
      began_relationship_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      AGREEABLENESS_z +
      Religious +
      Believe.Spirit_z +
      Believe.God_z +
      # BornNZ, nor working
      KESSLER6sum_z +
      HLTH.Fatigue_z +
      Rumination_z +
      SexualSatisfaction_z +
      POWERDEPENDENCE_z +
      # Your.Future.Security_z +
      # Your.Personal.Relationships_z +
      # Your.Health_z +
      # Standard.Living_z +
      BELONG_z +
      SUPPORT_z +
      #  CharityDonate_z +
      #  HoursCharity_z +
      GRATITUDE_z +
      # Volunteers,
      LIFEMEANING_z +
      LIFESAT_z +
      # partnerlost_job
      PWI_z +
      NWI_z +
      #Env.SacWilling,
      #Env.SacMade,
      Env.ClimateChgCause_z +
      Env.ClimateChgReal_z +
      SFHEALTH_z +
      SELF.CONTROL_z +
      SFHEALTH_z +
      SELF.ESTEEM_z +
      Respect.Self_z +
      #  GenCohort,
      SELF.ESTEEM_z +
      #  Respect.Self,
      # Emp.WorkLifeBalance_z + not measured at t1
      Alcohol.Frequency_z +
      Alcohol.Intensity_log_z
  ))
p22 <- parameters::model_parameters(m22)
p22
plot(p22)  + theme_minimal()

library(ggeffects)
library(parameters)
summary(pool, (m22))
ggeffects::ggemmeans(m22, terms = "Church_lead1")
out <- summary(pool(m22))
shout <- data.frame(out)

ggeffects::ggpredict( shout, 'Env.ClimateChgCause_lead2_z')




