# read data
df <- readRDS(here::here("data_raw", "df.Rds"))

# read files
source(here::here("scripts", "funs.R"))

# read libraries in
source(here::here("scripts", "libs.R"))
library("dplyr")
library("tidyr")


#check data
skimr::skim(df)

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

table(df$Alcohol.Intensity)

# select variables
c_df <- df %>%
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
    Env.ClimateChgCause,
    Env.ClimateChgReal #,
  ) %>%
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
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
  dplyr::filter(Wave == 2018) %>%
  dplyr::filter(!is.na(Church)) %>%
  dplyr::filter(!is.na(Church_lead1)) %>%  #needed for the intervention
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
length(unique(c_df$Id)) # 33189

# Exercise = cut(
#   Hours.Exercise,
#   breaks = c(-Inf,0,2.5,7,Inf),
#   labels = c("Never","0-upto-2.5hrs","2.5-upto-7hrs",">7hrs"),
#   right = FALSE),

c_df %>%
  select(Church, Church_lead1) %>%
  summarise(across(everything(), c(mean = mean, sd = sd)))

# inspect data
skim(c_df)

c_df %>%
  group_by(Wave) %>%
  summarise(across(Id, n_distinct))

# consider removing
c_df %>%
  dplyr::filter(Id == 9630)

# glimse
c_df %>%
  summarise(across(
    c(PWI_lead2, Church, Church_lead1),
    ~ mean(.x, na.rm = TRUE),
    ~ sd(.x, na.rm = TRUE),
    n_distinct()
  ))

# cases
length(unique(c_df$Id))


# save function
saveh(c_df, "c_df")

# read if needed
c_df <- readh("c_df")


# GFS coding
# Church
#[Never, a few times a year, a few times a month, weekly, more than once per week] [BMMRS 34]

# Religious TXTs
#[Never, occasionally, daily, more than daily] [BMMRS 15, modified]

#  Charity
#  In the past month, have you volunteered your time to an organization?2

# view vars
table1::table1(
  ~ Church +
    Church_lead1 +
    Relid +
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
    SexualSatisfaction +
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

hist(exp(c_df$Exercise_log -1))

sum((exp(c_df$CharityDonate_log_lead2 -1)/exp(c_df$income_log -1)) > 1, na.rm = TRUE)


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
Exercise_log_lead2
# I do not have enough power or control over important parts of my life.
str(c_df)


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

godwide <-  df %>%
  dplyr::filter(YearMeasured == 1 & Wave %in% c("2010",
                                                "2011",
                                                "2012",
                                                "2013",
                                                "2014",
                                                "2015",
                                                "2016",
                                                "2017",
                                                "2018",
                                                "2019",
                                                "2020")) %>%
  dplyr::filter(!is.na(Believe.God)) %>%
  dplyr::select(Id, Believe.God, Wave) %>%
  group_by(Id) %>% filter(n() > 4) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Believe.God)

godwide <- godwide[complete.cases(godwide),]
dim(godwide)
godwide
x_var_list <- names(godwide[, 2:11])


library(lcsm)
plot_trajectories(
  data = godwide,
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



spiritwide <-  df %>%
  dplyr::filter(YearMeasured == 1 & Wave %in% c("2010",
                                                "2011",
                                                "2012",
                                                "2013",
                                                "2014",
                                                "2015",
                                                "2016",
                                                "2017",
                                                "2018",
                                                "2019",
                                                "2020")) %>%
  dplyr::filter(!is.na(Believe.Spirit)) %>%
  dplyr::select(Id, Believe.Spirit, Wave) %>%
  group_by(Id) %>% filter(n() > 4) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Believe.Spirit)

spiritwide <- godwide[complete.cases(spiritwide),]
dim(spiritwide)
spiritwide
x_var_list <- names(spiritwide[, 2:11])


library(lcsm)
plot_trajectories(
  data = spiritwide,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Believe in Spirit",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.086,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)


# Prayer
spiritwide <-  df %>%
  dplyr::filter(YearMeasured == 1 & Wave %in% c("2010",
                                                "2011",
                                                "2012",
                                                "2013",
                                                "2014",
                                                "2015",
                                                "2016",
                                                "2017",
                                                "2018",
                                                "2019",
                                                "2020")) %>%
  dplyr::filter(!is.na(Believe.Spirit)) %>%
  dplyr::select(Id, Believe.Spirit, Wave) %>%
  group_by(Id) %>% filter(n() > 4) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Believe.Spirit)

spiritwide <- godwide[complete.cases(spiritwide),]
dim(spiritwide)
spiritwide
x_var_list <- names(spiritwide[, 2:11])


library(lcsm)
plot_trajectories(
  data = spiritwide,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Believe in Spirit",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.086,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)


# mice model  -------------------------------------------------------------
library(mice)
g_mice <- c_df %>%
  dplyr::select(-c(Wave, Id,
                   Alcohol.Intensity_log,
                   CharityDonate_log,
                   Exercise_log,
                   Alcohol.Intensity_log_lead2,
                   CharityDonate_log_lead2,
                   Exercise_log_lead2))

length(unique(c_df$Id))
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

outlist2 <-
  row.names(out_cdf)[out_cdf$outflux < 0.5]
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
min(long$KESSLER6sum, na.rm = TRUE)
# create variables in z score
long2 <- long %>%
  dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(church_any = ifelse(Church_lead1 > 0, 1, 0)) %>%
  dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0))%>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0))%>%
  dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0))%>%
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
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0) )) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0)+1 )) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1))%>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))# Respect for Self is fully missing

HLTH.Fatigue_lead2_z
# long$Rumination_lead2ord <- with(long, round(Rumination_lead2, digits = 0))
# long$Rumination_lead2ord <- with(long, Rumination_lead2ord + 1)
# long$SUPPORT_lead2ord <- with(long, round(SUPPORT_lead2, digits = 0))
# long$LIFEMEANING_lead2ord <- with(long, round(LIFEMEANING_lead2, digits = 0))
HLTH.Fatigue_lead2_z

# review
table(long2$Rumination_lead2ord)
table(long2$LIFEMEANING_lead2ord)
str(long2$SUPPORT_lead2ord)
hist(long2$HLTH.Fatigue_lead2ord)


hist(long2$Hours.Exercise_lead2)
hist(long2$CharityDonate_lead2)
table(long2$Alcohol.Intensity_lead2)

table(long2$Alcohol.Intensity_lead2)
hist(long2$Alcohol.Intensity_lead2, breaks = 100)



# get colnames
colnames(long2) # get names

#long$attrition <- with(long, ndf$attrition_lead2)
# neect to get into shape
long3 <- long2 %>% mutate_if(is.matrix, as.vector)
out2_ch <- mice::as.mids(long3)
saveh(out2_ch, "out2_ch")
out2_ch <- readh("out2_ch")


# Children data

long2a <- long3 %>%
  dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  # dplyr::filter(newkids >= 0  & newkids <= 4) %>%
  dplyr::mutate(newkids = as.integer(round(newkids, digits = 0))) %>%
  dplyr::mutate(newkids = ifelse(newkids > 4, 4,
                                 ifelse(newkids < 0 , 0,
                                        newkids))) %>%
  # dplyr::filter(!is.na(newkids)) %>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  dplyr::select(-c(.imp_z, .id_z))  # Respect for Self is fully missing

table((long2a$newkids))
hist((long2a$newkids))



long3a <- long2a %>% mutate_if(is.matrix, as.vector)
out2_cha <- mice::as.mids(long3a)

saveh(out2_cha, "out2_cha")
out2_cha <- readh("out2_cha")

# urban vs rural
#xyplot(out2, attrition ~ LIFESAT_lead1_z,pch=18,cex=1)
c_df$Alcohol.Intensity_log
# Try full model
#devtools::install_github('IQSS/Zelig')

# k6 ----------------------------------------------------------------------

# need to increase memory
options(future.globals.maxSize = 8000 * 1024^2)

m1_church_k6_nb <- brm_multiple(
  as.integer(KESSLER6sum_lead2) ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m1_church_k6_nb"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
pp_check(m1_church_k6_nb)

#table
lazerhawk::brms_SummaryTable(m1_church_k6_nb, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m1_church_k6_nb)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m1 <- plot(
    conditional_effects(
      m1_church_k6_nb,
      "Church_lead1",
      ndraws = 500,
      spaghetti = T ),
    points = T,
    alpha = .01,
    point_args = list(alpha = .005, width = .5)
)

plot_smooth_m1
# tidy graph
m1_plot <-
  plot_smooth_m1[[1]] + # scale_x_continuous(limits = c(-5,5)) +
#   scale_y_continuous(limits = c(0,5)) +
  # geom_hline(
  #   yintercept = 5,
  #   linetype = "dashed",
  #   color = "red",
  #   size = .5
  # ) +
  labs(title = "Kessler 6 Distress", # (negative binomial regression)
       # subtitle = "Loss shows greater distress",
       y = "Kessler6 distress (0-24)",
       x = "Monthly Church Frequency") #+ scale_colour_okabe_ito(alpha=.5)

m1_plot



# Try with standardize

m1_church_k6 <- brm_multiple(
  KESSLER6sum_lead2_z ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m1_church_k6")#,
  # set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
pp_check(m1_church_k6)

#table
lazerhawk::brms_SummaryTable(m1_church_k6, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m1_church_nb)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m1 <-
  plot(
    conditional_effects(
      m1_church_nb,
      "Church_lead1",
      ndraws = 500,
      spaghetti = T
    ),
    points = F,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )

# tidy graph
m1_plot_nb <-
  plot_smooth_m1_nb[[1]] + # scale_x_continuous(limits = c(-5,5)) +
  # scale_y_continuous(limits = c(0,24)) +
  # geom_hline(
  #   yintercept = 5,
  #   linetype = "dashed",
  #   color = "red",
  #   size = .5
  # ) +
  labs(title = "Kessler 6 Distress (negative binomial regression)",
       # subtitle = "Loss shows greater distress",
       y = "Kessler6 distress (0-24)",
       x = "Monthly Church Frequency") #+ scale_colour_okabe_ito(alpha=.5)

m1_plot_nb


# depressed ---------------------------------------------------------------
m1_ab_church_depressed  <- brm_multiple(
  Depressed_lead2 ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m1_ab_church_depressed"),
  set_prior('normal(0, 1)', class = 'b')
)


lp <-
  posterior_linpred(
    m1_ab_church_depressed,
    newdata = data.frame(
      n = 1,
      Church_lead1 = c(0, 4),
      Church  = 0,
      AGREEABLENESS_z = 0,
      CONSCIENTIOUSNESS_z = 0,
      EXTRAVERSION_z  = 0,
      HONESTY_HUMILITY_z = 0,
      NEUROTICISM_z = 0,
      OPENNESS_z = 0,
      Age_z = 0,
      Alcohol.Frequency_z = 0, #
      Alcohol.Intensity_log_z = 0, #
      Believe.God_z = 0,
      Believe.Spirit_z = 0,
      BELONG_z = 0, #
      CharityDonate_log_z = 0, #
      ChildrenNum_z = 0,
      Edu_z = 0,
      Employed_z = 0,
      Euro_z = 0,
      GRATITUDE_z = 0,
      HomeOwner_z = 0,
      Hours.Exercise_log_z = 0,
      Hours.Work_z = 0,
      HLTH.BMI_z  = 0, #
      HLTH.Fatigue_z = 0, #
      income_log_z = 0,
      KESSLER6sum_z = 0, #
      LIFEMEANING_z = 0, #
      LIFESAT_z = 0, #
      lost_job_z = 0,
      Male_z = 0,
      NZdep_z = 0,
      NWI_z = 0,
      Parent_z = 0,
      Partner_z = 0,
      PERFECTIONISM_z = 0,
      Pol.Orient_z = 0,
      POWERDEPENDENCE_z = 0,
      PWI_z = 0,
      Relid_z = 0,
      Respect.Self_z = 0,
      Rumination_z = 0,
      SELF.CONTROL_z = 0,
      SELF.ESTEEM_z = 0,
      SexualSatisfaction_z = 0,
      SFHEALTH_z = 0,
      Smoker_z = 0,
      SUPPORT_z = 0,
      Urban_z = 0,
      Volunteers_z = 0,
      transform = TRUE,
      re_formula = NA
    )
  )
posterior_summary(lp[, 1] / lp[, 2])
m_test


library(brm)
m_test <- with(
  out2_ch,
  brm(
    y = Depressed,
    x = church_any,
    va =  Church +
      Church_lead1 +
      Church +
      AGREEABLENESS_z +
      CONSCIENTIOUSNESS_z +
      EXTRAVERSION_z  +
      HONESTY_HUMILITY_z +
      NEUROTICISM_z +
      OPENNESS_z +
      Age_z +
      Alcohol.Frequency_z + #
      Alcohol.Intensity_log_z + #
      Believe.God_z +
      Believe.Spirit_z +
      BELONG_z + #
      CharityDonate_log_z + #
      ChildrenNum_z +
      Edu_z +
      Employed_z +
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
    # va,
    # vb = NULL,
    param = "RR",
    est.method = "DR",
    # vc = NULL,
    optimal = TRUE,
    # weights = NULL,
    # subset = NULL,
    # max.step = NULL,
    thres = 1e-08,
    # alpha.start = NULL,
    #  beta.start = NULL,
    message = TRUE
  )
)






# life meaning ------------------------------------------------------------

m2_church_lifemeaning <- brm_multiple(
  LIFEMEANING_lead2_z ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m2_church_lifemeaning"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
pp_check(m2_church_lifemeaning)



m2a_church_lifemeaning <- brm_multiple(
  LIFEMEANING_lead2ord ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m2a_church_lifemeaning"),
  set_prior('normal(0, 1)', class = 'b')
)


# wow nice
pp_check(m2a_church_lifemeaning)


#table
lazerhawk::brms_SummaryTable(m2_church_lifemeaning, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m2_church_lifemeaning)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m2 <-
  plot(
    conditional_effects(
      m2a_church_lifemeaning,
      "Church_lead1",
      ndraws = 500,
      spaghetti = T
    ),
    points = T,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )

plot(
  conditional_effects(
    m2a_church_lifemeaning,
    "Church_lead1",
    ndraws = 500,
    spaghetti = T
  ),
  points = F,
  alpha = .01,
  point_args = list(alpha = .005, width = .1)
)
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph
m2_plot <-  plot_smooth_m2[[1]]  +
#  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "Life Meaning",
       # subtitle = "Loss shows greater distress",
       y = "Life Meaning (1-7)",
       x = "Monthly Church Frequency")

m2_plot


# health-fatigue ----------------------------------------------------------
m3_church_fatigue <- brm_multiple(
  HLTH.Fatigue_lead2_z ~
    Church_lead1 +
    Church +
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m3_church_fatigue"),
  set_prior('normal(0, 1)', class = 'b')
)

m3a_church_fatigue <- brm_multiple(
  HLTH.Fatigue_lead2ord ~
    Church_lead1 +
    Church +
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m3a_church_fatigue"),
  set_prior('normal(0, 1)', class = 'b')
)


## posterior predictive checks
pp_check(m3a_church_fatigue)

#table
lazerhawk::brms_SummaryTable(m3_church_fatigue, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m3_church_fatigue)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m3 <-
  plot(conditional_effects(
    m3a_church_fatigue,
    "Church_lead1",
    ndraws = 500,
    spaghetti = T ),
    points = T,
    point_args = list(alpha = .01,  width = .45, height = .45)
  )

# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))

# tidy graph
m3_plot <-
  plot_smooth_m3[[1]] + # scale_x_continuous(limits = c(-5,5)) +
#  scale_y_continuous(limits = c(-.5, .5)) +
  # geom_hline(
  #   yintercept = 5,
  #   linetype = "dashed",
  #   color = "red",
  #   size = .5
  # ) +
  labs(title = "Fatigue",
       # subtitle = "Loss shows greater distress",
       y = "Fatigue (sd)",
       x = "Monthly Church Frequency") #+ scale_colour_okabe_ito(alpha=.5)

m3_plot



# depressed ---------------------------------------------------------------
m1_ab_church_depressed  <- brm_multiple(
  Depressed_lead2 ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m1_ab_church_depressed"),
  set_prior('normal(0, 1)', class = 'b')
)


lp <-
  posterior_linpred(
    m1_ab_church_depressed,
    newdata = data.frame(
      n = 1,
      Church_lead1 = c(0, 4),
      Church  = 0,
      KESSLER6sum_z = 0,
      Age_z = 0,
      Hours.Work_z = 0,
      Male_z = 0,
      ChildrenNum_z = 0,
      Edu_z = 0,
      Employed_z = 0,
      Exercise_log_z = 0,
      Euro_z = 0,
      HomeOwner_z = 0,
      income_log_z = 0,
      Pol.Orient_z = 0,
      Urban_z = 0,
      NZdep_z = 0,
      Parent_z = 0,
      Partner_z = 0,
      Smoker_z = 0,
      HLTH.BMI_z  = 0,
      lost_job_z = 0,
      began_relationship_z = 0,
      CONSCIENTIOUSNESS_z = 0,
      OPENNESS_z = 0,
      HONESTY_HUMILITY_z = 0,
      EXTRAVERSION_z = 0,
      NEUROTICISM_z = 0,
      AGREEABLENESS_z = 0,
      Relid_z = 0,
      Believe.Spirit_z = 0,
      Believe.God_z = 0,
      HLTH.Fatigue_z = 0,
      Rumination_z = 0,
      SexualSatisfaction_z = 0,
      POWERDEPENDENCE_z = 0,
      Standard.Living_z = 0,
      NWI_z = 0,
      BELONG_z = 0,
      SUPPORT_z = 0,
      GRATITUDE_z = 0,
      LIFEMEANING_z = 0,
      LIFESAT_z = 0,
      PWI_z = 0,
      NWI_z = 0,
      SFHEALTH_z = 0,
      SELF.CONTROL_z = 0,
      SFHEALTH_z = 0,
      SELF.ESTEEM_z = 0,
      Respect.Self_z = 0,
      SELF.CONTROL_z = 0,
      Alcohol.Frequency_z = 0,
      Alcohol.Intensity_log = 0,
      transform = TRUE,
      re_formula = NA
    )
  )
posterior_summary(lp[, 1] / lp[, 2])
m_test

library(brm)
m_test <- with(
  out2_ch,
  brm(
    y = Depressed,
    x = church_any,
    va =  Church +
      Church_lead1 +
      Church +
      AGREEABLENESS_z +
      CONSCIENTIOUSNESS_z +
      EXTRAVERSION_z  +
      HONESTY_HUMILITY_z +
      NEUROTICISM_z +
      OPENNESS_z +
      Age_z +
      Alcohol.Frequency_z + #
      Alcohol.Intensity_log_z + #
      Believe.God_z +
      Believe.Spirit_z +
      BELONG_z + #
      CharityDonate_log_z + #
      ChildrenNum_z +
      Edu_z +
      Employed_z +
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
    # va,
    # vb = NULL,
    param = "RR",
    est.method = "DR",
    # vc = NULL,
    optimal = TRUE,
    # weights = NULL,
    # subset = NULL,
    # max.step = NULL,
    thres = 1e-08,
    # alpha.start = NULL,
    #  beta.start = NULL,
    message = TRUE
  )
)



# LIFESAT_z ---------------------------------------------------------------

m4_church_lifsat  <- brms::brm_multiple(
  LIFESAT_lead2_z ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m4_church_lifsat.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
m4_church_lifsat <-
  readRDS(here::here("mods", "m4_church_lifsat.rds"))
m4_church_lifsat
## posterior predictive checks
pp_check(m4_church_lifsat)

#table
lazerhawk::brms_SummaryTable(m4_church_lifsat, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m4_church_lifsat)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m4 <-
  plot(conditional_effects(
    m4_church_lifsat,
    "Church_lead1",
    ndraws = 500,
    spaghetti = T),
    points = F,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )

m4_plot <-  plot_smooth_m4[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "Life Satisfaction",
       # subtitle = "Loss shows greater distress",
       y = "Life Satisfaction (sd)",
       x = "Monthly Church Frequency")

m4_plot

# rumination --------------------------------------------------------------

m5_church_rumination <- brms::brm_multiple(
  Rumination_lead2_z ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m5_church_rumination.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
pp_check(m5_church_rumination)



# not good
Rumination_lead2ord

m5a_church_rumination <- brms::brm_multiple(
  Rumination_lead2ord ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  init = 0,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m5a_church_rumination.rds"),
  set_prior('normal(0, 1)', class = 'b')
)


#table
lazerhawk::brms_SummaryTable(m5a_church_rumination, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m5_church_rumination)
m1_bayes_table
plot(m5_church_rumination)

# graph
plot_smooth_m5 <-
  plot(
    conditional_effects(
      m5a_church_rumination,
      "Church_lead1",
      ndraws = 500,
      spaghetti = T),
      points = T,
      alpha = .01,
      point_args = list(alpha = .005, width = .2)
  )

# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph
m5_plot <-  plot_smooth_m5[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "Rumination",
       # subtitle = "Loss shows greater distress",
       y = "Rumination (sd)",
       x = "Monthly Church Frequency")

m5_plot

# SexualSatisfaction_z ----------------------------------------------------

m6_church_sexual_satisfaction  <- brm_multiple(
  SexualSatisfaction_lead2_z ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m6_church_sexual_satisfaction.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
pp_check(m6_church_sexual_satisfaction)

#table
lazerhawk::brms_SummaryTable(m6_church_sexual_satisfaction, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m6_sexual_satisfaction)
m1_bayes_table
plot()

# graph
plot_smooth_m6 <-
  plot(
    conditional_effects(
      m6_church_sexual_satisfaction,
      "Church_lead1",
      ndraws = 500,
      spaghetti = T,
      points = F,
      alpha = .01,
      point_args = list(alpha = .005, width = .1)
    ))
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph
m6_plot <-  plot_smooth_m6[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "Sexual Satisfaction",
       # subtitle = "Loss shows greater distress",
       y = "Sexual Satisfaction (sd)",
       x = "Monthly Church Frequency")

m6_plot


# POWERDEPENDENCE_z -------------------------------------------------------

m7_church_powerindependence <- brms::brm_multiple(
  POWERDEPENDENCE_lead2_z ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m7_church_powerindependence.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
pp_check(m7_church_powerindependence)

#table
lazerhawk::brms_SummaryTable(m7_church_powerindependence, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m7_church_powerindependence)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m7 <-
  plot(
    conditional_effects(
      m7_church_powerindependence,
      "Church_lead1",
      ndraws = 500,
      spaghetti = T,
      points = F,
      alpha = .01,
      point_args = list(alpha = .005, width = .1)
    ))
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph
m7_plot <-  plot_smooth_m7[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "Power Dependence",
       # subtitle = "Loss shows greater distress",
       y = "Power Dependence (sd)",
       x = "Monthly Church Frequency")

m7_plot


# I do not have enough power or control over important parts of my life.
# Other people have too much power or control over important parts of my life.
# BELONG_z ----------------------------------------------------------------

m8_church_belong <- brm_multiple(
  BELONG_lead2_z ~
    Church_lead1 +
    Church +
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m8_church_belong.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
pp_check(m8_church_belong)

#table
lazerhawk::brms_SummaryTable(m8_church_belong, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m8_church_belong)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m8 <-
  plot(conditional_effects(
    m8_church_belong,
    "Church_lead1",
    ndraws = 500,
    spaghetti = T,
    points = F,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  ))
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph
m8_plot <-  plot_smooth_m8[[1]]  +
  scale_y_continuous(limits = c(-.25, .25)) +
  labs(title = "Social Belonging",
       # subtitle = "Loss shows greater distress",
       y = "Social Belonging (sd)",
       x = "Monthly Church Frequency")

m8_plot


# SUPPORT_z ---------------------------------------------------------------

m9_church_support <- brm_multiple(
  SUPPORT_lead2_z ~
    Church_lead1 +
    Church +
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m9_church_support.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
pp_check(m9_church_support)

#Bad
m9a_church_support <- brm_multiple(
  SUPPORT_lead2ord ~
    Church_lead1 +
    Church +
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init = 0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m9a_church_support.rds"),
  set_prior('normal(0, 1)', class = 'b')
)




#table
lazerhawk::brms_SummaryTable(m9a_church_support, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m9_church_support)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m9 <-
  plot(conditional_effects(
    m9_church_support,
    "Church_lead1",
    ndraws = 500,
    spaghetti = T),
    points = T,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )

plot_smooth_m9
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph
m9_plot <-  plot_smooth_m9[[1]]  +
  scale_y_continuous(limits = c(-.25, .25)) +
  labs(title = "Social Support",
       # subtitle = "Loss shows greater distress",
       y = "Social Support (sd)",
       x = "Monthly Church Frequency")

m9_plot




# PWI_z -------------------------------------------------------------------


m10_church_pwi <- brm_multiple(
  PWI_lead2_z ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m10_church_pwi.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
pp_check(m10_church_pwi)

#table
lazerhawk::brms_SummaryTable(m10_church_pwi, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m10_church_pwi)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m10 <-
  plot(conditional_effects(
    m10_church_pwi,
    "Church_lead1",
    ndraws = 500,
    spaghetti = T),
    points = F,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph
m10_plot <-  plot_smooth_m10[[1]]  +
  scale_y_continuous(limits = c(-.25, .25)) +
  labs(title = "Personal Wellbeing Index",
       # subtitle = "Loss shows greater distress",
       y = "Personal Wellbeing Index (sd)",
       x = "Monthly Church Frequency")

m10_plot

# SFHEALTH_lead2_z --------------------------------------------------------------


m11_church_sfhealth <- brm_multiple(
  SFHEALTH_lead2_z ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m11_church_sfhealth.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
pp_check(m11_church_sfhealth)

#table
lazerhawk::brms_SummaryTable(m11_church_sfhealth, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m11_church_sfhealth)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m11 <-
  plot(conditional_effects(
    m11_church_sfhealth,
    "Church_lead1",
    ndraws = 500,
    spaghetti = T
  ))
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph
m11_plot <-  plot_smooth_m11[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "Short Form Health",
       # subtitle = "Loss shows greater distress",
       y = "Short Form Health (sd)",
       x = "Monthly Church Frequency")

m11_plot

# SELF.ESTEEM_lead2_z -----------------------------------------------------------

m12_church_selfesteem <- brm_multiple(
  SELF.ESTEEM_lead2_z ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m12_church_selfesteem.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
pp_check(m12_church_selfesteem)

#table
lazerhawk::brms_SummaryTable(m12_church_selfesteem, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m12_church_selfesteem)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m12 <-
  plot(
    conditional_effects(
      m12_church_selfesteem,
      "Church_lead1",
      ndraws = 500,
      spaghetti = T
    )
  )
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph

m12_plot <-  plot_smooth_m12[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "Self Esteem",
       # subtitle = "Loss shows greater distress",
       y = "Self Esteem (sd)",
       x = "Monthly Church Frequency")

m12_plot

# Emp.WorkLifeBalance_lead2_z ---------------------------------------------------------------

m13_church_worklifebalance <-
  brm_multiple(
    Emp.WorkLifeBalance_lead2_z ~
      Church_lead1 +
      Church +
      Church_lead1 +
      Church +
      AGREEABLENESS_z +
      CONSCIENTIOUSNESS_z +
      EXTRAVERSION_z  +
      HONESTY_HUMILITY_z +
      NEUROTICISM_z +
      OPENNESS_z +
      Age_z +
      Alcohol.Frequency_z + #
      Alcohol.Intensity_log_z + #
      Believe.God_z +
      Believe.Spirit_z +
      BELONG_z + #
      CharityDonate_log_z + #
      ChildrenNum_z +
      Edu_z +
      Employed_z +
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
    data = out2_ch,
    seed = 1234,
    warmup = 1000,
    iter = 2000,
    chains = 4,
    backend = "cmdstanr",
    file = here::here("mods", "m13_church_worklifebalance.rds"),
    set_prior('normal(0, 1)', class = 'b')
  )

## posterior predictive checks
pp_check(m13_church_worklifebalance)

#table
lazerhawk::brms_SummaryTable(m13_church_worklifebalance, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m13_church_worklifebalance)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m13 <-
  plot(
    conditional_effects(
      m13_church_worklifebalance,
      "Church_lead1",
      ndraws = 500,
      spaghetti = T
    )
  )
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph

m13_plot <-  plot_smooth_m13[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "Work/life Balance",
       # subtitle = "Loss shows greater distress",
       y = "Work/Life Balance(sd)",
       x = "Monthly Church Frequency")

m13_plot
# Volunteers_lead2 --------------------------------------------------------------

m14_church_volunteers <- brm_multiple(
  Volunteers_lead2 | trials(1) ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  family = binomial(),
  #bernoulli(link = "cloglog")
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m14_church_volunteers.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

lp <-
  posterior_linpred(
    m14_church_volunteers,
    newdata = data.frame(
      n = 1,
      Church_lead1 = c(4, 0),
      transform = TRUE,
      re_formula = NA
    )
  )
posterior_summary(lp[, 1] / lp[, 2])
## posterior predictive checks
pp_check(m14_church_volunteers)

#table
lazerhawk::brms_SummaryTable(m14_church_volunteers, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m14_church_volunteers)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m14 <-
  plot(
    conditional_effects(
      m14_church_volunteers,
      "Church_lead1",
      ndraws = 500,
      spaghetti = T
    )
  )
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph

m14_plot <-  plot_smooth_m14[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "Volunteerse",
       # subtitle = "Loss shows greater distress",
       y = "Volunteers",
       x = "Volunteerse")

m14_plot

###  version
m14a_church_volunteers <- brm_multiple(
  Volunteers_lead2 ~
    Church_lead1 +
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  family = bernoulli(link = "probit"),
  data = out2_ch,
  seed = 1234,
  init = 0,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m14a_church_volunteers.rds"),
  set_prior('normal(0, 1)', class = 'b')
)



m14c_church_volunteers <- brm_multiple(
  Volunteers_lead2 ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  init = 0,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m14c_church_volunteers.rds"),
  set_prior('normal(0, 1)', class = 'b')
)


lp <-
  posterior_linpred(
    m14_church_volunteers,
    newdata = data.frame(
      n = 1,
      Church_lead1 = c(0, 4),
      Church  = 0,
      KESSLER6sum_z = 0,
      Age_z = 0,
      Hours.Work_z = 0,
      Male_z = 0,
      ChildrenNum_z = 0,
      Edu_z = 0,
      Employed_z = 0,
      Exercise_log_z = 0,
      Euro_z = 0,
      HomeOwner_z = 0,
      income_log_z = 0,
      Pol.Orient_z = 0,
      Urban_z = 0,
      NZdep_z = 0,
      Parent_z = 0,
      Partner_z = 0,
      Smoker_z = 0,
      HLTH.BMI_z  = 0,
      lost_job_z = 0,
      Alcohol.Intensity_log_z = 0,
      CharityDonate_log_z = 0,
      PERFECTIONISM_z = 0,
      Volunteers_z = 0,
      Hours.Exercise_log_z = 0,
      began_relationship_z = 0,
      CONSCIENTIOUSNESS_z = 0,
      OPENNESS_z = 0,
      HONESTY_HUMILITY_z = 0,
      EXTRAVERSION_z = 0,
      NEUROTICISM_z = 0,
      AGREEABLENESS_z = 0,
      Relid_z = 0,
      Believe.Spirit_z = 0,
      Believe.God_z = 0,
      HLTH.Fatigue_z = 0,
      Rumination_z = 0,
      SexualSatisfaction_z = 0,
      POWERDEPENDENCE_z = 0,
      Standard.Living_z = 0,
      NWI_z = 0,
      BELONG_z = 0,
      SUPPORT_z = 0,
      GRATITUDE_z = 0,
      LIFEMEANING_z = 0,
      LIFESAT_z = 0,
      PWI_z = 0,
      NWI_z = 0,
      SFHEALTH_z = 0,
      SELF.CONTROL_z = 0,
      SFHEALTH_z = 0,
      SELF.ESTEEM_z = 0,
      Respect.Self_z = 0,
      SELF.CONTROL_z = 0,
      Alcohol.Frequency_z = 0,
      Alcohol.Intensity_log = 0,
      transform = TRUE,
      re_formula = NA
    )
  )
posterior_summary(lp[, 1] / lp[, 2])
lp <-
  posterior_linpred(
    m14c_church_volunteers,
    newdata = data.frame(
      n = 1,
      Church_lead1 = c(0, 4),
      Church  = 0,
      KESSLER6sum_z = 0,
      Age_z = 0,
      Hours.Work_z = 0,
      Male_z = 0,
      ChildrenNum_z = 0,
      Edu_z = 0,
      Employed_z = 0,
      Exercise_log_z = 0,
      Euro_z = 0,
      HomeOwner_z = 0,
      income_log_z = 0,
      Pol.Orient_z = 0,
      Urban_z = 0,
      NZdep_z = 0,
      Parent_z = 0,
      Partner_z = 0,
      Smoker_z = 0,
      HLTH.BMI_z  = 0,
      lost_job_z = 0,
      Alcohol.Intensity_log_z = 0,
      CharityDonate_log_z = 0,
      PERFECTIONISM_z = 0,
      Volunteers_z = 0,
      Hours.Exercise_log_z = 0,
      began_relationship_z = 0,
      CONSCIENTIOUSNESS_z = 0,
      OPENNESS_z = 0,
      HONESTY_HUMILITY_z = 0,
      EXTRAVERSION_z = 0,
      NEUROTICISM_z = 0,
      AGREEABLENESS_z = 0,
      Relid_z = 0,
      Believe.Spirit_z = 0,
      Believe.God_z = 0,
      HLTH.Fatigue_z = 0,
      Rumination_z = 0,
      SexualSatisfaction_z = 0,
      POWERDEPENDENCE_z = 0,
      Standard.Living_z = 0,
      NWI_z = 0,
      BELONG_z = 0,
      SUPPORT_z = 0,
      GRATITUDE_z = 0,
      LIFEMEANING_z = 0,
      LIFESAT_z = 0,
      PWI_z = 0,
      NWI_z = 0,
      SFHEALTH_z = 0,
      SELF.CONTROL_z = 0,
      SFHEALTH_z = 0,
      SELF.ESTEEM_z = 0,
      Respect.Self_z = 0,
      SELF.CONTROL_z = 0,
      Alcohol.Frequency_z = 0,
      Alcohol.Intensity_log = 0,
      transform = TRUE,
      re_formula = NA
    )
  )
posterior_summary(lp[, 1] / lp[, 2])

## posterior predictive checks
pp_check(m14_church_volunteers)
pp_check(m14c_church_volunteers)

#table
lazerhawk::brms_SummaryTable(m14c_church_volunteers, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m14c_church_volunteers)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m14 <-
  plot(
    conditional_effects(
      m14c_church_volunteers,
      "Church_lead1",
      ndraws = 500,
      spaghetti = T
    )
  )
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph

m14_plot <-  plot_smooth_m14[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "Volunteerse",
       # subtitle = "Loss shows greater distress",
       y = "Volunteers",
       x = "Volunteerse")

m14_plot

# Charity.Donate_logz -----------------------------------------------

m15_church_charity <- brm_multiple(
  CharityDonate_log_lead2_z ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m15_church_charity.rds"),
  set_prior('normal(0, 1)', class = 'b')
)



m15a_church_charity <- brm_multiple(
 bf(CharityDonate_lead2 ~
   Church_lead1 +
   Church +
   AGREEABLENESS_z +
   CONSCIENTIOUSNESS_z +
   EXTRAVERSION_z  +
   HONESTY_HUMILITY_z +
   NEUROTICISM_z +
   OPENNESS_z +
   Age_z +
   Alcohol.Frequency_z + #
   Alcohol.Intensity_log_z + #
   Believe.God_z +
   Believe.Spirit_z +
   BELONG_z + #
   CharityDonate_log_z + #
   ChildrenNum_z +
   Edu_z +
   Employed_z +
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
   Volunteers_z, zi ~  Church_lead1 + Church + CharityDonate_log_z),
  family = "zero_inflated_negbinomial",
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m15a_church_charity.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
pp_check(m15a_church_charity) # not a good fit

#table
m15a_church_charity
lazerhawk::brms_SummaryTable(m15a_church_charity, panderize = F)

plogis(.18)
# another table
m1_bayes_table <-
  parameters::model_parameters(m15a_church_charity)
m1_bayes_table
plot(m1_bayes_table)

plogis(-1.67 + 0.01 -0.92 -(0.05*4))

0.06723245/0.05841456
# graph
plot_smooth_m15 <-
  plot(conditional_effects(
    m15a_church_charity,
    "Church_lead1",
    ndraws = 500,
    spaghetti = T
  ))
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph

m15_plot <-  plot_smooth_m15[[1]]  +
 # scale_y_continuous(limits = c(0, 1000)) +
  labs(title = "Charitable Donations",
       # subtitle = "Loss shows greater distress",
       y = "Charitable Donations (dollars)",
       x = "Monthly Church Frequency")

m15_plot

#  Alcohol.Intensity_logz -------------------------------------------------

m16_church_alcohol_intensity <-
  brm_multiple(
   bf(Alcohol.Intensity_lead2   ~
      Church_lead1 +
      Church +
      AGREEABLENESS_z +
      CONSCIENTIOUSNESS_z +
      EXTRAVERSION_z  +
      HONESTY_HUMILITY_z +
      NEUROTICISM_z +
      OPENNESS_z +
      Age_z +
      Alcohol.Frequency_z + #
      Alcohol.Intensity_log_z + #
      Believe.God_z +
      Believe.Spirit_z +
      BELONG_z + #
      CharityDonate_log_z + #
      ChildrenNum_z +
      Edu_z +
      Employed_z +
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
      Volunteers_z, zi ~ Church_lead1 + Church + Alcohol.Intensity_log_z),
    family = "zero_inflated_negbinomial",
    data = out2_ch,
    seed = 1234,
    warmup = 1000,
    iter = 2000,
    chains = 4,
    backend = "cmdstanr",
    file = here::here("mods", "m16_church_alcohol_intensity.rds"),
    set_prior('normal(0, 1)', class = 'b')
  )


m16a_church_alcohol_intensity <-
  brm_multiple(
    as.integer(Alcohol.Intensity_lead2)   ~
      Church_lead1 +
      Church +
      AGREEABLENESS_z +
      CONSCIENTIOUSNESS_z +
      EXTRAVERSION_z  +
      HONESTY_HUMILITY_z +
      NEUROTICISM_z +
      OPENNESS_z +
      Age_z +
      Alcohol.Frequency_z + #
      Alcohol.Intensity_log_z + #
      Believe.God_z +
      Believe.Spirit_z +
      BELONG_z + #
      CharityDonate_log_z + #
      ChildrenNum_z +
      Edu_z +
      Employed_z +
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
    data = out2_ch,
    seed = 1234,
    warmup = 1000,
    iter = 2000,
    chains = 4,
    backend = "cmdstanr",
    file = here::here("mods", "m16a_church_alcohol_intensity.rds"),
    set_prior('normal(0, 1)', class = 'b')
  )


## posterior predictive checks
pp_check(m16_church_alcohol_intensity)

youla
#table
lazerhawk::brms_SummaryTable(m16_church_alcohol_intensity, panderize = F)

plogis(10.75 + 0.08 )
# another table
m1_bayes_table <-
  parameters::model_parameters(m16_church_alcohol_intensity)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m16 <-
  plot(
    conditional_effects(
      m16_church_alcohol_intensity,
      "Church_lead1",
      ndraws = 500,
      spaghetti = T
    )
  )
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph

m16_plot <-  plot_smooth_m16[[1]]  +
 # scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "Alcohol Intensity",
       # subtitle = "Loss shows greater distress",
       y = "Alcohol Intensity (drinks)",
       x = "Monthly Church Frequency")


plogis(.51)

# Alcohol.Frequency_lead2_z , ---------------------------------------------------

m17_church_alcohol_frequency <-
  brm_multiple(
    Alcohol.Frequency_lead2_z   ~
      Church_lead1 +
      Church +
      AGREEABLENESS_z +
      CONSCIENTIOUSNESS_z +
      EXTRAVERSION_z  +
      HONESTY_HUMILITY_z +
      NEUROTICISM_z +
      OPENNESS_z +
      Age_z +
      Alcohol.Frequency_z + #
      Alcohol.Intensity_log_z + #
      Believe.God_z +
      Believe.Spirit_z +
      BELONG_z + #
      CharityDonate_log_z + #
      ChildrenNum_z +
      Edu_z +
      Employed_z +
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
    data = out2_ch,
    seed = 1234,
    warmup = 1000,
    iter = 2000,
    chains = 4,
    backend = "cmdstanr",
    file = here::here("mods", "m17_church_alcohol_frequency.rds"),
    set_prior('normal(0, 1)', class = 'b')
  )

## posterior predictive checks
pp_check(m17_church_alcohol_frequency)

#table
lazerhawk::brms_SummaryTable(m17_church_alcohol_frequency, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m17_church_alcohol_frequency)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m17 <-
  plot(
    conditional_effects(
      m17_church_alcohol_frequency,
      "Church_lead1",
      ndraws = 500,
      spaghetti = T
    )
  )
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph

m17_plot <-  plot_smooth_m17[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "Alcohol Frequency",
       # subtitle = "Loss shows greater distress",
       y = "Alcohol Frequency (sd)",
       x = "Monthly Church Frequency")

m17_plot
# Smoker_bz ---------------------------------------------------------------

m18_church_smoker <- brm_multiple(
  Smoker_lead2   ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init=0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m18_church_smoker.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

lp <-
  posterior_linpred(
    m18_church_smoker,
    newdata = data.frame(
      n = 1,
      Church_lead1 = c(0, 4),
      Church  = 0,
      KESSLER6sum_z = 0,
      Age_z = 0,
      Hours.Work_z = 0,
      Male_z = 0,
      ChildrenNum_z = 0,
      Edu_z = 0,
      Employed_z = 0,
      Exercise_log_z = 0,
      Euro_z = 0,
      HomeOwner_z = 0,
      income_log_z = 0,
      Pol.Orient_z = 0,
      Urban_z = 0,
      NZdep_z = 0,
      Parent_z = 0,
      Partner_z = 0,
      Smoker_z = 0,
      HLTH.BMI_z  = 0,
      lost_job_z = 0,
      Alcohol.Intensity_log_z = 0,
      CharityDonate_log_z = 0,
      PERFECTIONISM_z = 0,
      Volunteers_z = 0,
      Hours.Exercise_log_z = 0,
      began_relationship_z = 0,
      CONSCIENTIOUSNESS_z = 0,
      OPENNESS_z = 0,
      HONESTY_HUMILITY_z = 0,
      EXTRAVERSION_z = 0,
      NEUROTICISM_z = 0,
      AGREEABLENESS_z = 0,
      Relid_z = 0,
      Believe.Spirit_z = 0,
      Believe.God_z = 0,
      HLTH.Fatigue_z = 0,
      Rumination_z = 0,
      SexualSatisfaction_z = 0,
      POWERDEPENDENCE_z = 0,
      Standard.Living_z = 0,
      NWI_z = 0,
      BELONG_z = 0,
      SUPPORT_z = 0,
      GRATITUDE_z = 0,
      LIFEMEANING_z = 0,
      LIFESAT_z = 0,
      PWI_z = 0,
      NWI_z = 0,
      SFHEALTH_z = 0,
      SELF.CONTROL_z = 0,
      SFHEALTH_z = 0,
      SELF.ESTEEM_z = 0,
      Respect.Self_z = 0,
      SELF.CONTROL_z = 0,
      Alcohol.Frequency_z = 0,
      Alcohol.Intensity_log = 0,
      transform = TRUE,
      re_formula = NA
    )
  )
posterior_summary(lp[, 1] / lp[, 2])

posterior_summary(lp[, 1] / lp[, 2])

## posterior predictive checks
pp_check(m18_church_smoker)

#table
lazerhawk::brms_SummaryTable(m18_church_smoker, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m18_church_smoker)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m18 <-
  plot(conditional_effects(
    m18_church_smoker,
    "Church_lead1",
    ndraws = 500,
    spaghetti = T
  ))
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph

m18_plot <-  plot_smooth_m18[[1]]  +
  scale_y_continuous(limits = c(0, .1)) +
  labs(title = "Smoker",
       # subtitle = "Loss shows greater distress",
       y = "Smoker (sd)",
       x = "Monthly Church Frequency")

m18_plot
# HLTH.BMI_lead2_z ---------------------------------------------------------

m19_church_bmi <- brm_multiple(
  HLTH.BMI_lead2_z   ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init= 0,
  backend = "cmdstanr",
  file = here::here("mods", "m19_church_bmi.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
pp_check(m19_church_bmi)

#table
lazerhawk::brms_SummaryTable(m19_church_bmi, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m19_church_bmi)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m19 <-
  plot(conditional_effects(
    m19_church_bmi,
    "Church_lead1",
    ndraws = 500,
    spaghetti = T
  ))
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph

m19_plot <-  plot_smooth_m19[[1]]  +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "BMI",
       # subtitle = "Loss shows greater distress",
       y = "BMI (sd)",
       x = "Monthly Church Frequency")

m19_plot



# Hours.Exercise_log_lead2z -----------------------------------------------
m20_church_exercise <- brm_multiple(
  Hours.Exercise_log_lead2_z   ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m20_church_exercise.rds"),
  set_prior('normal(0, 1)', class = 'b')
)
mean(long2$Hours.Exercise_lead2, na.rm=T)

#Hours spent … exercising/physical activity
m20a_church_exercise <- brm_multiple(
  as.integer(Hours.Exercise_lead2)   ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m20a_church_exercise.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
pp_check(m20a_church_exercise)
m20a_church_exercise

#table
lazerhawk::brms_SummaryTable(m20a_church_exercise, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m20a_church_exercise)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m20 <-
  plot(conditional_effects(
    m20a_church_exercise,
    "Church_lead1",
    ndraws = 500,
    spaghetti = T
  ))
# points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph

m20_plot <-  plot_smooth_m20[[1]]  +
 # scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "Exercise",
       # subtitle = "Loss shows greater distress",
       y = "Exercise (weekly hours)",
       x = "Monthly Church Frequency")

m20_plot


#


# NWI ---------------------------------------------------------------------

m21_church_nwi <- brm_multiple(
  NWI_lead2_z   ~
    Church_lead1 +
    Church +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Alcohol.Frequency_z + #
    Alcohol.Intensity_log_z + #
    Believe.God_z +
    Believe.Spirit_z +
    BELONG_z + #
    CharityDonate_log_z + #
    ChildrenNum_z +
    Edu_z +
    Employed_z +
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
  data = out2_ch,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  init =0,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m21_church_nwi.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

## posterior predictive checks
pp_check(m21_church_nwi)
m21_church_nwi

#table
lazerhawk::brms_SummaryTable(m21_church_nwi, panderize = F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m21_church_nwi)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m21 <-
  plot(conditional_effects(
    m21_church_nwi,
    "Church_lead1",
    ndraws = 500,
    spaghetti = T
  ))
  # points = T,
# alpha = .01,
# point_args = list(alpha = .005, width = .1))
library(ggokabeito)
# tidy graph

m21_plot <-  plot_smooth_m21[[1]]  +
  # scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "National Well-Being Index",
       # subtitle = "Loss shows greater distress",
       y = "NWI (sd)",
       x = "Monthly Church Frequency")

m21_plot




#
#
# # ChildrenNum_lead2 -------------------------------------------------------
# m21_church_children_nb <- brm_multiple(
#   (newkids) ~
#     Church_lead1 +
#     Church +
#     KESSLER6sum_z +
#     Age_z +
#     Hours.Work_z +
#     Male_z +
#     ChildrenNum_z + # offset
#     Edu_z +
#     Employed_z +
#     Exercise_log_z +
#     Euro_z +
#     HomeOwner_z +
#     income_log_z +
#     Pol.Orient_z +
#     Urban_z +
#     NZdep_z +
#     Parent_z +
#     Partner_z +
#     Smoker_z +
#     HLTH.BMI_z  +
#     lost_job_z +
#     began_relationship_z +
#     CONSCIENTIOUSNESS_z +
#     OPENNESS_z +
#     HONESTY_HUMILITY_z +
#     EXTRAVERSION_z +
#     NEUROTICISM_z +
#     AGREEABLENESS_z +
#     Relid_z +
#     Believe.Spirit_z +
#     Believe.God_z +
#     # BornNZ, nor working
#     HLTH.Fatigue_z +
#     Rumination_z +
#     SexualSatisfaction_z +
#     POWERDEPENDENCE_z +
#     # Your.Future.Security_z +
#     # Your.Personal.Relationships_z +
#     # Your.Health_z +
#     Standard.Living_z +
#     NWI_z +
#     BELONG_z +
#     SUPPORT_z +
#     #  CharityDonate_z +
#     #  HoursCharity_z +
#     GRATITUDE_z +
#     # Volunteers,
#     LIFEMEANING_z +
#     LIFESAT_z +
#     # partnerlost_job
#     PWI_z +
#     NWI_z +
#     #Env.SacWilling,
#     #Env.SacMade,
#     # Env.ClimateChgCause_z +
#     # Env.ClimateChgReal_z +
#     SFHEALTH_z +
#     SELF.CONTROL_z +
#     SFHEALTH_z +
#     SELF.ESTEEM_z +
#     Respect.Self_z +
#     #  GenCohort,
#     SELF.CONTROL_z +
#     #  Respect.Self,
#     # Emp.WorkLifeBalance_z + not measured at t1
#     Alcohol.Frequency_z +
#     Alcohol.Intensity_log,
#   family = "negbinomial",
#   data = out2_cha,
#   # different data
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   file = here::here("mods", "m20_church_children_nb")#,
#   # set_prior('normal(0, 1)', class = 'b')
# )
#
#
#
# ## posterior predictive checks
# pp_check(m20_church_children_nb)
# summary(m20_church_children_nb)
# #table
# lazerhawk::brms_SummaryTable(m20_church_children_nb, panderize = F)
#
# # another table
# m1_bayes_table <-
#   parameters::model_parameters(m20_church_children_nb)
# m1_bayes_table
# plot(m1_bayes_table)
#
# # graph
# plot_smooth_m20_church_children_nb <-
#   plot(
#     conditional_effects(
#       m20_church_children_nb,
#       "Church_lead1",
#       ndraws = 500,
#       spaghetti = T
#     )#,
#     # points = T,
#     #  alpha = .01,
#     #  point_args = list(alpha = .005, width = .1)
#   )
#
# # tidy graph
# m20_plot_nb <-
#   plot_smooth_m20_church_children_nb[[1]] + # scale_x_continuous(limits = c(-5,5)) +
#   # scale_y_continuous(limits = c(0,24)) +
#   # geom_hline(
#   #   yintercept = 5,
#   #   linetype = "dashed",
#   #   color = "red",
#   #   size = .5
#   # ) +
#   labs(title = "Children (negative binomial regression)",
#        # subtitle = "Loss shows greater distress",
#        y = "Children",
#        x = "Monthly Church Frequency") #+ scale_colour_okabe_ito(alpha=.5)
#
# m20_plot_nb
#
#
# p20 <- parameters::model_parameters(m20_church - children - nb)
# p20
# plot(p20)
#
#
#
# # NOT RUN
#
# # Env.ClimateChgReal_z  ---------------------------------------------------
#
# m21 <-
#   with(
#     out2_ch,
#     lm(
#       Env.ClimateChgReal_lead2_z   ~
#         Church_lead1 +
#         Church +
#         Exercise_log_z +
#         Age_z +
#         Hours.Work_z +
#         Male_z +
#         ChildrenNum +
#         Edu_z +
#         # Employed_z +
#         Exercise_log_z +
#         Euro_z +
#         HomeOwner_z +
#         income_log_z +
#         Pol.Orient_z +
#         Urban_z +
#         NZdep_z +
#         Parent_z +
#         Partner_z +
#         Smoker_z +
#         HLTH.BMI_z +
#         Smoker_z +
#         lost_job_z +
#         began_relationship_z +
#         CONSCIENTIOUSNESS_z +
#         OPENNESS_z +
#         HONESTY_HUMILITY_z +
#         EXTRAVERSION_z +
#         NEUROTICISM_z +
#         AGREEABLENESS_z +
#         Religious +
#         Believe.Spirit_z +
#         Believe.God_z +
#         # BornNZ, nor working
#         KESSLER6sum_z +
#         HLTH.Fatigue_z +
#         Rumination_z +
#         SexualSatisfaction_z +
#         POWERDEPENDENCE_z +
#         # Your.Future.Security_z +
#         # Your.Personal.Relationships_z +
#         # Your.Health_z +
#         # Standard.Living_z +
#         BELONG_z +
#         SUPPORT_z +
#         #  CharityDonate_z +
#         #  HoursCharity_z +
#         GRATITUDE_z +
#         # Volunteers,
#         LIFEMEANING_z +
#         LIFESAT_z +
#         # partnerlost_job
#         PWI_z +
#         NWI_z +
#         #Env.SacWilling,
#         #Env.SacMade,
#         Env.ClimateChgCause_z +
#         Env.ClimateChgReal_z +
#         SFHEALTH_z +
#         SELF.CONTROL_z +
#         SFHEALTH_z +
#         SELF.ESTEEM_z +
#         Respect.Self_z +
#         #  GenCohort,
#         SELF.ESTEEM_z +
#         #  Respect.Self,
#         # Emp.WorkLifeBalance_z + not measured at t1
#         Alcohol.Frequency_z +
#         Alcohol.Intensity_log_z
#     )
#   )
# p21 <- parameters::model_parameters(m21)
# p21
# plot(p21)
#
#
# #  Env.ClimateChgCause_z --------------------------------------------------
#
# m22 <-
#   with(
#     out2_ch,
#     lm(
#       Env.ClimateChgCause_lead2_z   ~
#         Church_lead1 +
#         Church +
#         Exercise_log_z +
#         Age_z +
#         Hours.Work_z +
#         Male_z +
#         ChildrenNum +
#         Edu_z +
#         # Employed_z +
#         Exercise_log_z +
#         Euro_z +
#         HomeOwner_z +
#         income_log_z +
#         Pol.Orient_z +
#         Urban_z +
#         NZdep_z +
#         Parent_z +
#         Partner_z +
#         Smoker_z +
#         HLTH.BMI_z +
#         Smoker_z +
#         lost_job_z +
#         began_relationship_z +
#         CONSCIENTIOUSNESS_z +
#         OPENNESS_z +
#         HONESTY_HUMILITY_z +
#         EXTRAVERSION_z +
#         NEUROTICISM_z +
#         AGREEABLENESS_z +
#         Religious +
#         Believe.Spirit_z +
#         Believe.God_z +
#         # BornNZ, nor working
#         KESSLER6sum_z +
#         HLTH.Fatigue_z +
#         Rumination_z +
#         SexualSatisfaction_z +
#         POWERDEPENDENCE_z +
#         # Your.Future.Security_z +
#         # Your.Personal.Relationships_z +
#         # Your.Health_z +
#         # Standard.Living_z +
#         BELONG_z +
#         SUPPORT_z +
#         #  CharityDonate_z +
#         #  HoursCharity_z +
#         GRATITUDE_z +
#         # Volunteers,
#         LIFEMEANING_z +
#         LIFESAT_z +
#         # partnerlost_job
#         PWI_z +
#         NWI_z +
#         #Env.SacWilling,
#         #Env.SacMade,
#         Env.ClimateChgCause_z +
#         Env.ClimateChgReal_z +
#         SFHEALTH_z +
#         SELF.CONTROL_z +
#         SFHEALTH_z +
#         SELF.ESTEEM_z +
#         Respect.Self_z +
#         #  GenCohort,
#         SELF.ESTEEM_z +
#         #  Respect.Self,
#         # Emp.WorkLifeBalance_z + not measured at t1
#         Alcohol.Frequency_z +
#         Alcohol.Intensity_log_z
#     )
#   )
# p22 <- parameters::model_parameters(m22)
# p22
# plot(p22)  + theme_minimal()
#
# library(ggeffects)
# library(parameters)
# summary(pool, (m22))
# ggeffects::ggemmeans(m22, terms = "Church_lead1")
# out <- summary(pool(m22))
# shout <- data.frame(out)
#
# ggeffects::ggpredict(shout, 'Env.ClimateChgCause_lead2_z')
#
