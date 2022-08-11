# standard of living

# income.R
# install
# https://github.com/markmfredrickson/optmatch

### ELIGIBILITY CRITERIA  INCOME STUDY
# 2018/ 2019

# inclusion -- 2019 if you have an income of xx
# baseline -- washout perior ... income above xx at baseline (your income was above  xxx 20,000 )

# retired / semiretired  baseline


### Your Standard of living
##  enrolled


### inclusion for the hours of work.
## 1. you are > 35 at baseline line


## 2. population of people who are non workers --  stratified , **and not retired or semi-retired.**
# intervene to give this population work hours,



# read data
df <- readRDS(here::here("data_raw", "df.Rds"))

df %>%
  filter(Wave == 2020 &  YearMeasured == 1) %>%
  n_distinct("Id")

# read libraries in
source(here::here("scripts", "libs.R"))
library("dplyr")
library("tidyr")


# table for participant N
tab_in <- df %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
 # dplyr::filter(Id != 9630) %>% # problematic for income
  group_by(Id) %>%
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  ungroup() %>%
  droplevels() %>%
  arrange(Id, Wave)
# check n # 34782
table1::table1(~ Household.INC | Wave , data = tab_in, overall = FALSE)

length(unique(tab_in$Id)) # 34783


## select vars
df_st <- tab_in %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(Id != 9630) %>% # problematic
  select(
    Id,
    YearMeasured,
    Wave,
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
    Urban,
    Household.INC,
    Parent,
    Partner,
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
    # Volunteers,
    Hours.Work,
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
    #  Respect.Self,
    Emp.WorkLifeBalance,
    Alcohol.Frequency,
    Alcohol.Intensity,
    HLTH.BMI,
    Smoker,
    ChildrenNum,
    # GenCohort,
    # Euro,
    # partnerlost_job, rare
    #lost_job,
    #began_relationship,
    Alcohol.Intensity,
    Alcohol.Frequency,
    SexualSatisfaction,
    POWERDEPENDENCE1,
    POWERDEPENDENCE2,
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living,
    # Env.SacWilling,
    #Env.SacMade,
    PERFECTIONISM,
    PermeabilityIndividual,
    ImpermeabilityGroup
    # Emp.JobSecure,
    #  Env.ClimateChgCause,
    # Env.ClimateChgReal #,
  ) %>%
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
  arrange(Id, Wave) %>%
  dplyr::mutate(
    Edu = as.numeric(Edu),
    Volunteers = if_else(HoursCharity == 1, 1, 0),
    # Depressed = (as.numeric(
    #   cut(
    #     KESSLER6sum,
    #     breaks = c(-Inf, 13, Inf),
    #     labels = c("0", "1"),
    #     right = FALSE
    #   )
    # ) - 1),
    # EthCat = factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1),
  ) %>%
  arrange(Id, Wave)  %>% #
 # dplyr::mutate(income_log_lead1 = lead(income_log, n = 1)) %>%
#  dplyr::mutate(Hours.Work_lead1 = lead(Hours.Work, n = 1)) %>%
  dplyr::mutate(Standard.Living_lead1 = lead(Standard.Living, n = 1)) %>%
 # dplyr::mutate(retired_lead1 = lead(retired, n = 1)) %>%
#  dplyr::mutate(semiretired_lead1 = lead(semiretired, n = 1)) %>%
  #dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%  Your.Future.Security
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::mutate(across(
    c(
      KESSLER6sum,
      HLTH.Fatigue,
      Rumination,
      community,
      SFHEALTH,
      LIFEMEANING,
      LIFESAT,
      #  PWI,
      Hours.Work,
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
      POWERDEPENDENCE1,
      POWERDEPENDENCE2,
      CharityDonate,
      Alcohol.Intensity,
      PERFECTIONISM,
      Bodysat,
      VENGEFUL.RUMIN,
      community,
      HONESTY_HUMILITY,
      EmotionRegulation1,
      EmotionRegulation2,
      EmotionRegulation3,
      Emp.WorkLifeBalance,
      PermeabilityIndividual,
      ImpermeabilityGroup,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living,
      PermeabilityIndividual,
      ImpermeabilityGroup
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
  #  dplyr::filter(retired != 1) %>%
  #  dplyr::filter(retired_lead1 != 1) %>%  #needed for the intervention
  #  dplyr::filter(semiretired != 1) %>%
  # dplyr::filter(semiretired_lead1 != 1) %>%
  #dplyr::filter(!is.na(income_log_lead1) )%>%  #   ABOUT
  #dplyr::filter(!is.na(income_log) )%>% #  THINK ABOUT
 # dplyr::filter(Household.INC >= 30975) %>% # min income
 # dplyr::filter(income_log_lead1 > income_log) %>%
  # dplyr::filter(!is.na(Hours.Work)) %>%
  dplyr::filter(!is.na(Standard.Living) )%>%
  dplyr::filter(!is.na(Standard.Living_lead1) )%>%
  #  dplyr::filter(semiretired_lead1 != 1) %>%  #needed for the intervention
  dplyr::select(
    -c(
      Religion.Church,
      # EthCat,
      HoursCharity,
      Respect.Self_lead2,
      Household.INC,
      #  org2018,
      #  not_euro,
      #  not_euro_lead2,
      # hold18,
      #   Euro,
      Emp.WorkLifeBalance,
      YearMeasured,
      # org2019,
      # hold19,
      retired,
     # retired_lead1,
      semiretired#,
     # semiretired_lead1
    )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)

length(unique(df_st$Id)) # 34412

# inspect data
skim(df_st)
df_st %>%
  group_by(Wave) %>%
  summarise(across(Id, n_distinct))


# save function
saveh(df_st, "df_st")

# read if needed
df_st<- readh("df_st")



# mice model  -------------------------------------------------------------
library(mice)

mice_st <- df_st %>%
  dplyr::select(-c( Wave, Id))
# Visualise missing
library(naniar)
naniar::gg_miss_var(mice_st)
vis_miss(mice_st,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(mice_st)

# qp <- quickpred(inc_mice)  https://stefvanbuuren.name/fimd/sec-toomany.html
# qp
#for_mice$inc_prop <-
#  for_mice$income_log / (for_mice$income_log_lead1 - 1)

#
# ini <- mice(mice_upinc, m = 1, maxit = 0)
# predmat<- ini$predictorMatrix
# meth<- ini$method
# meth
# predmat["Id",]= 0
# predmat
# #meth
# #meth["inc_prop"] <- "~ I(income_log/(income_log_lead1 - 1))"
# #meth["Id"] <- 0
#

# impute
st_mice <- mice::mice(mice_st,
                     seed = 0,
                     m = 10)

# save
saveh(st_mice, "st_mice")

# read
st_mice <- readh("st_mice")

# https://www.r-bloggers.com/2020/12/iptw-with-missing-data/
# IPTW   see https://stats.stackexchange.com/questions/563057/multiple-imputation-and-inverse-probability-weighting-for-multiple-treatment##
#https://stats.stackexchange.com/questions/563057/multiple-imputation-and-inverse-probability-weighting-for-multiple-treatment


outlist2 <-
  row.names(st_mice)[st_mice$outflux < 0.5]
length(outlist2)

head(st_mice$loggedEvents, 10)

#test <- mice.impute.fastpmm(out_for_mice,  donors = 5, type = 1, ridge = 1e-05,)
#
is.mids(st_mice)

# data warangling
i_f <- mice::complete(st_mice, "long", inc = F)
i_l <- mice::complete(st_mice, "long", inc = TRUE)
nrow(i_f)/10
# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(i_l)
#long$LIFESAT_lead1_z <- with(long, scale(LIFESAT_lead1))

# create variables in z score
i_l2 <- i_l %>%
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0)))%>%
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
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0) )) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(Your.Personal.Relationships_lead2, digits = 0) +1)) %>%
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0) )) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0)+1 )) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1))%>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0)+1))%>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0) )) %>%
  dplyr::mutate(alcohol_bin2 = if_else( Alcohol.Frequency > 3, 1, 0))%>%
  dplyr::mutate(alcohol_bin = if_else( Alcohol.Frequency > 2, 1, 0))%>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work/10)%>%
  # dplyr::mutate(Hours.Work_lead1_10 = Hours.Work_lead1/10)%>%
  # dplyr::mutate(Hours.Work_lead1ord = (as.numeric(
  #   cut(
  #     Hours.Work_lead1,
  #     breaks = c(-Inf,  20, 40, Inf),
  #     labels = c("0", "20", "40",  "over40"),
  #     right = TRUE
  #   )
  # ) - 1)) %>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))%>%
  dplyr::mutate(id = as.factor(rep(1:34412, 11)))# needed for g-comp# Respect for Self is fully missing

table(i_l2$Euro)
nrow(i_f)/10
i_f2 <- i_f %>%
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0)))%>%
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
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0) )) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(Your.Personal.Relationships_lead2, digits = 0) +1)) %>%
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0) )) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0)+1 )) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1))%>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0)+1))%>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0) )) %>%
  dplyr::mutate(alcohol_bin2 = if_else( Alcohol.Frequency > 3, 1, 0))%>%
  dplyr::mutate(alcohol_bin = if_else( Alcohol.Frequency > 2, 1, 0))%>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work/10)%>%
 # dplyr::mutate(Hours.Work_lead1_10 = Hours.Work_lead1/10)%>%
  # dplyr::mutate(Hours.Work_lead1ord = (as.numeric(
  #   cut(
  #     Hours.Work_lead1,
  #     breaks = c(-Inf,  20, 40, Inf),
  #     labels = c("0", "20", "40",  "over40"),
  #     right = TRUE
  #   )
  # ) - 1)) %>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z)) %>%
  dplyr::mutate(id = as.factor(rep(1:34412, 10)))# needed for g-comp



# Get data into shape
stf3 <- i_f2 %>% mutate_if(is.matrix, as.vector)
stl3 <- i_l2 %>% mutate_if(is.matrix, as.vector)

st_m <- mice::as.mids(stl3)

saveh(st_m, "st_m")

st_m <- readh("st_m")




# model equations ---------------------------------------------------------
baselinevars = c("AGREEABLENESS_z","CONSCIENTIOUSNESS_z","EXTRAVERSION_z","HONESTY_HUMILITY_z","NEUROTICISM_z","OPENNESS_z","Age_z","Alcohol.Frequency_z","Alcohol.Intensity_log_z","Bodysat_z","Believe.God_z","Believe.Spirit_z","BELONG_z","CharityDonate_log_z","ChildrenNum_z","Church_z", "community","Edu_z","Employed_z","EmotionRegulation1_z", "EmotionRegulation2_z","EmotionRegulation3_z","Euro_z", "GRATITUDE_z","HomeOwner_z","Hours.Exercise_log_z","Hours.Work_z","HLTH.BMI_z", "HLTH.Fatigue_z", "ImpermeabilityGroup_z","income_log_z", "KESSLER6sum_z", "LIFEMEANING_z", "LIFESAT_z", "Male_z","NZdep_z", "NWI_z","NZSEI13_z","Parent_z","Partner_z","PERFECTIONISM_z", "PermeabilityIndividual_z", "Pol.Orient_z", "POWERDEPENDENCE1_z","POWERDEPENDENCE2_z","Relid_z", "Respect.Self_z","Rumination_z","SELF.CONTROL_z", "SELF.ESTEEM_z","SexualSatisfaction_z","SFHEALTH_z","Smoker_z", "Spiritual.Identification_z","Standard.Living_z", "SUPPORT_z","Urban_z", "VENGEFUL.RUMIN_z", "Volunteers_z", "Your.Health_z", "Your.Future.Security_z", "Your.Personal.Relationships_z")


# models ------------------------------------------------------------------

gform_m1$est[2]
diff_loq <- function(est){
  lo <- est[1]
  z <- est[2]
  lodiff <- z+lo
  return(lodiff)
}


diff_hi <- function(est){
  hi <- est[3]
  z <- est[2]
  hidiff <- z+hi
  return(hidiff)
}


confint(object= gform_m1, fun=diff_loq)


# bmi ---------------------------------------------------------------------

#conditional
library(splines)
library(parameters)
bmi_st <- as.formula(paste("HLTH.BMI_lead2_z ~bs(Standard.Living_lead1_z) +",
                            paste(baselinevars,
                                  collapse = "+")))
bmi_st


m1_bmi  <- lapply(1:10, function(i) {
  m <- glm(bmi_st, data = complete(inc_m, action = i))
})

parameters::pool_parameters(m1_bmi, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m1_bmi, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

# for
m1_long <- glm(bmi_st, data = stf3)
m1_long
gform_m1<- stdGlm(fit = m1_long, data = stf3, X = "Standard.Living_lead1_z", x =-1:1, clusterid="id")
summary(gform_m1, contrast = "difference", reference = 0)

round( EValue::evalues.OLS( -0.0117 , se = 0.00518  , sd = 1, delta = 1, true = 0), 3)
round( EValue::evalues.OLS( 0.0102 , se = 0.00549  , sd = 1, delta = 1, true = 0), 3)

round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

# Graph
dev.off()
plot(gform_m1, ylim = c(-.1,.1),
     contrast = "difference", reference = 0,
     main="BMI", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="BMIl",
     col.lab="black", cex.lab=0.75)


# sf-health ---------------------------------------------------------------
sfhealth_st <- as.formula(paste("SFHEALTH_lead2_z ~bs(Standard.Living_lead1_z) +",
                                 paste(baselinevars,
                                       collapse = "+")))

m2_sfhealth  <- lapply(1:10, function(i) {
  m <- glm(sfhealth_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m2_sfhealth, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m2_sfhealth, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

# for
m2_long <- glm(sfhealth_st, data = stf3)
gform_m2<- stdGlm(fit = m2_long, data = stf3, X = "Standard.Living_lead1_z", x =-1:1)
summary(gform_m2, contrast = "difference", reference = 0)

round( EValue::evalues.OLS( -0.0539  , se =0.00273   , sd = 1, delta = 1, true = 0), 3)
round( EValue::evalues.OLS( 0.0513 , se = 0.00293  , sd = 1, delta = 1, true = 0), 3)

# Graph
dev.off()
plot(gform_m2, ylim = c(-.1,.1),
     contrast = "difference", reference = 0,
     main="SF Health", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="SF Health",
     col.lab="black", cex.lab=0.75)


# excercise ---------------------------------------------------------------

Hours.Exercise_st <- as.formula(paste("Hours.Exercise_lead2_log_z ~bs(Standard.Living_lead1_z) +",
                                       paste(baselinevars,
                                             collapse = "+")))

m3_Hours.Exercise  <- lapply(1:10, function(i) {
  m <- glm(Hours.Exercise_st, data = complete(inc_m, action = i))
})

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m3_Hours.Exercise, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m3_Hours.Exercise, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

# for
m3_long <- glm(Hours.Exercise_st, data = stf3)
m3_long
gform_m3<- stdGlm(fit = m3_long, data = stf3, X = "Standard.Living_lead1_z", x =-1:1)
summary(gform_m3, contrast = "difference", reference = 0)
dev.off()
plot(gform_m3)

round( EValue::evalues.OLS( -0.0195, se = 0.00338 , sd = 1, delta = 1, true = 0), 3)
round( EValue::evalues.OLS( 0.0175, se = 0.00365, sd = 1, delta = 1, true = 0), 3)


# Graph
dev.off()
plot(gform_m3, ylim = c(-.1,.1),
     contrast = "difference", reference = 0,
     main="Hours Exercise (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Hours Exercise (SD)",
     col.lab="black", cex.lab=0.75)

# smoker ------------------------------------------------------------------

Smoker_st <- as.formula(paste("Smoker_lead2 ~bs(Standard.Living_lead1_z) +",
                               paste(baselinevars,
                                     collapse = "+")))

m4_Smoker  <- lapply(1:10, function(i) {
  m <- glm(Smoker_st, family = "binomial", data = complete(inc_m, action = i))
})


round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m4_Smoker, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m4_Smoker, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m4_long <- glm(Smoker_st, data = stf3, family = "poisson")
m4_long
gform_m4<- stdGlm(fit = m4_long, data = stf3, X = "Standard.Living_lead1_z", x =-1:1)
summary(gform_m4, contrast = "ratio", type="log" ,reference = 0)
plot(gform_m4)

round( EValue::evalues.OLS( 0.894, se = 0.000437, sd = 1, delta = 1, true = 0), 3)


exp(-.846)
# Graph
dev.off()
plot(gform_m4, contrast = "ratio", reference = 0, type="log" )

plot(gform_m4, ylim = c(0,.1),
     contrast = "ratio", reference = 0, type = "log",
     main="Smoking rate", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Smoking rate",
     col.lab="black", cex.lab=0.75)

dev.off()
# fatigue -----------------------------------------------------------------


HLTH.Fatigue_st <- as.formula(paste("HLTH.Fatigue_lead2_z ~bs(Standard.Living_lead1_z) +",
                                     paste(baselinevars,
                                           collapse = "+")))

m5_HLTH.Fatigue  <- lapply(1:10, function(i) {
  m <- glm(HLTH.Fatigue_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m5_HLTH.Fatigue, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m5_HLTH.Fatigue, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m5_long <- glm(HLTH.Fatigue_st, data = stf3)
m5_long
gform_m5<- stdGlm(fit = m5_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m5)
plot(gform_m5)

# Graph
dev.off()
plot(gform_m5, ylim = c(-.1,.1),
     main="Fatigue (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Fatigue (SD)",
     col.lab="black", cex.lab=0.75)


# alcohol freq ------------------------------------------------------------


Alcohol.Frequency_st <- as.formula(paste("Alcohol.Frequency_lead2ord_z ~bs(Standard.Living_lead1_z) +",
                                          paste(baselinevars,
                                                collapse = "+")))

m6_Alcohol.Frequency  <- lapply(1:10, function(i) {
  m <- glm(Alcohol.Frequency_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m6_Alcohol.Frequency, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m6_Alcohol.Frequency, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m6_long <- glm(Alcohol.Frequency_st, data = stf3)
m6_long
gform_m6<- stdGlm(fit = m6_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m6)
plot(gform_m6)

# Graph
dev.off()
plot(gform_m6, ylim = c(-.1,.1),
     main="Alcohol.Frequency (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Alcohol.Frequency (SD)",
     col.lab="black", cex.lab=0.75)



# alch intensity ----------------------------------------------------------

Alcohol.Intensity_st <- as.formula(paste("Alcohol.Intensity_log_lead2_z ~bs(Standard.Living_lead1_z) +",
                                          paste(baselinevars,
                                                collapse = "+")))

m7_Alcohol.Intensity  <- lapply(1:10, function(i) {
  m <- glm(Alcohol.Intensity_st, data = complete(inc_m, action = i), family = "gaussian")
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m7_Alcohol.Intensity, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m7_Alcohol.Intensity, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m7_long <- glm(Alcohol.Intensity_st, data = stf3)
m7_long
gform_m7<- stdGlm(fit = m7_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m7)
plot(gform_m7)

# Graph
dev.off()
plot(gform_m7, ylim = c(-.1,.1),
     main="Log Alcohol Intensity (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Log Alcohol Intensity (SD)(SD)",
     col.lab="black", cex.lab=0.75)

# body satisfaction -------------------------------------------------------

Bodysat_st <- as.formula(paste("Bodysat_lead2_z ~bs(Standard.Living_lead1_z) +",
                                paste(baselinevars,
                                      collapse = "+")))

m8_Bodysat  <- lapply(1:10, function(i) {
  m <- glm(Bodysat_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m8_Bodysat, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m8_Bodysat, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m8_long <- glm(Bodysat_st, data = stf3)
m8_long
gform_m8<- stdGlm(fit = m8_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m8)
plot(gform_m8)

# Graph
dev.off()
plot(gform_m8, ylim = c(-.1,.1),
     main="Body Satisfaction (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Body Satisfaction (SD)",
     col.lab="black", cex.lab=0.75)

# summary(output, conf.int = TRUE)


# rumination --------------------------------------------------------------
Rumination_st <- as.formula(paste("Rumination_lead2ord_z ~bs(Standard.Living_lead1_z) +",
                                   paste(baselinevars,
                                         collapse = "+")))

m9_Rumination  <- lapply(1:10, function(i) {
  m <- glm(Rumination_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m9_Rumination, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m9_Rumination, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m9_long <- glm(Rumination_st, data = stf3)
m9_long
gform_m9<- stdGlm(fit = m9_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m9)
plot(gform_m9)

# Graph
dev.off()
plot(gform_m9, ylim = c(-.2,.2),
     main="Rumination (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Rumination (SD)",
     col.lab="black", cex.lab=0.75)



# sex satisfaction --------------------------------------------------------


SexualSatisfaction_st <- as.formula(paste("SexualSatisfaction_lead2_z ~bs(Standard.Living_lead1_z) +",
                                           paste(baselinevars,
                                                 collapse = "+")))

m10_SexualSatisfaction  <- lapply(1:10, function(i) {
  m <- glm(SexualSatisfaction_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m10_SexualSatisfaction, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m10_SexualSatisfaction, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m10_long <- glm(SexualSatisfaction_st, data = stf3)
m10_long
gform_m10<- stdGlm(fit = m10_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m10)
plot(gform_m10)

# Graph
dev.off()
plot(gform_m10, ylim = c(-.1,.1),
     main="Sexual Satisfaction (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Sexual Satisfaction (SD)",
     col.lab="black", cex.lab=0.75)

# emotional regulation 1 ----------------------------------------------------

EmotionRegulation1_st <- as.formula(paste("EmotionRegulation1_lead2_z ~bs(Standard.Living_lead1_z) +",
                                           paste(baselinevars,
                                                 collapse = "+")))

m11_EmotionRegulation1  <- lapply(1:10, function(i) {
  m <- glm(EmotionRegulation1_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m11_EmotionRegulation1, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m11_EmotionRegulation1, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m11_long <- glm(EmotionRegulation1_st, data = stf3)
m11_long
gform_m11<- stdGlm(fit = m11_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m11)
plot(gform_m11)

# Graph
plot(gform_m11, ylim = c(-.1,.1),
     main="EmotionRegulation 1 (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="EmotionRegulation 1 (SD)",
     col.lab="black", cex.lab=0.75)


# emotional reg 2 ---------------------------------------------------------
EmotionRegulation2_st <- as.formula(paste("EmotionRegulation2_lead2_z ~bs(Standard.Living_lead1_z) +",
                                           paste(baselinevars,
                                                 collapse = "+")))

m12_EmotionRegulation2  <- lapply(1:10, function(i) {
  m <- glm(EmotionRegulation2_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m12_EmotionRegulation2, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m12_EmotionRegulation2, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m12_long <- glm(EmotionRegulation2_st, data = stf3)
m12_long
gform_m12<- stdGlm(fit = m12_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m12)
plot(gform_m12)

# Graph
dev.off()
plot(gform_m12, ylim = c(-.1,.1),
     main="EmotionRegulation 2 (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="EmotionRegulation 2 (SD)",
     col.lab="black", cex.lab=0.75)



# emotional reg 3 ---------------------------------------------------------


EmotionRegulation3_st <- as.formula(paste("EmotionRegulation3_lead2_z ~bs(Standard.Living_lead1_z) +",
                                           paste(baselinevars,
                                                 collapse = "+")))

m13_EmotionRegulation3  <- lapply(1:10, function(i) {
  m <- glm(EmotionRegulation3_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m13_EmotionRegulation3, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m13_EmotionRegulation3, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m13_long <- glm(EmotionRegulation3_st, data = stf3)
m13_long
gform_m13<- stdGlm(fit = m13_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m13)
plot(gform_m13)

# Graph
dev.off()
plot(gform_m13, ylim = c(-.1,.1),
     main="EmotionRegulation 3 (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="EmotionRegulation 3 (SD)",
     col.lab="black", cex.lab=0.75)


# kessler 6 ---------------------------------------------------------------

KESSLER6sum_st <- as.formula(paste("KESSLER6sum_lead2_z ~bs(Standard.Living_lead1_z) +",
                                    paste(baselinevars,
                                          collapse = "+")))

m14_KESSLER6sum_st  <- lapply(1:10, function(i) {
  m <- glm(KESSLER6sum_st, data = complete(inc_m, action = i), family = "gaussian")
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m14_KESSLER6sum_st, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m14_KESSLER6sum_st, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m14_long <- glm(KESSLER6sum_st, data = stf3)
m14_long
gform_m14<- stdGlm(fit = m14_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m14)
plot(gform_m14)

# Graph
dev.off()
plot(gform_m14, ylim = c(-.2,.2),
     main="Kessler6 Distress (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Kessler6 Distress (SD)",
     col.lab="black", cex.lab=0.75)


# power dependence 1 ------------------------------------------------------

POWERDEPENDENCE1_st <- as.formula(paste("POWERDEPENDENCE1_lead2_z ~bs(Standard.Living_lead1_z) +",
                                         paste(baselinevars,
                                               collapse = "+")))

m15_POWERDEPENDENCE  <- lapply(1:10, function(i) {
  m <- glm(POWERDEPENDENCE1_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m15_POWERDEPENDENCE, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m15_POWERDEPENDENCE, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m15_long <- glm(POWERDEPENDENCE1_st, data = stf3)
m15_long
gform_m15<- stdGlm(fit = m15_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m15)
plot(gform_m15)

# Graph
dev.off()
plot(gform_m15, ylim = c(-.1,.1),
     main="Power Dependence 1 (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Power Dependence 1 (SD)",
     col.lab="black", cex.lab=0.75)


# power dependence 2 ------------------------------------------------------


POWERDEPENDENCE2_st <- as.formula(paste("POWERDEPENDENCE2_lead2_z ~bs(Standard.Living_lead1_z) +",
                                         paste(baselinevars,
                                               collapse = "+")))

m16_POWERDEPENDENCE2  <- lapply(1:10, function(i) {
  m <- glm(POWERDEPENDENCE2_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m16_POWERDEPENDENCE2, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m16_POWERDEPENDENCE2, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m16_long <- glm(POWERDEPENDENCE2_st, data = stf3)
m16_long
gform_m16<- stdGlm(fit = m16_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m16)
plot(gform_m16)

# Graph
dev.off()
plot(gform_m16, ylim = c(-.1,.1),
     main="Power Dependence 2 (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Power Dependence 2 (SD)",
     col.lab="black", cex.lab=0.75)



# perfectionism  ----------------------------------------------------------

PERFECTIONISM_st <- as.formula(paste("PERFECTIONISM_lead2_z ~bs(Standard.Living_lead1_z) +",
                                      paste(baselinevars,
                                            collapse = "+")))

m17_PERFECTIONISM  <- lapply(1:10, function(i) {
  m <- glm(PERFECTIONISM_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m17_PERFECTIONISM, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m17_PERFECTIONISM, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m17_long <- glm(PERFECTIONISM_st, data = stf3)
m17_long
gform_17<- stdGlm(fit = m17_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_17)
plot(gform_17)

# Graph
dev.off()
plot(gform_17, ylim = c(-.1,.1),
     main="Perfectionism (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Perfectionism (SD)",
     col.lab="black", cex.lab=0.75)


# self esteem -------------------------------------------------------------

SELF.ESTEEM_st <- as.formula(paste("SELF.ESTEEM_lead2_z ~bs(Standard.Living_lead1_z) +",
                                    paste(baselinevars,
                                          collapse = "+")))

m18_SELF.ESTEEM <- lapply(1:10, function(i) {
  m <- glm(SELF.ESTEEM_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m18_SELF.ESTEEM, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m18_SELF.ESTEEM, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m18_long <- glm(SELF.ESTEEM_st, data = stf3)
m18_long
gform_m18<- stdGlm(fit = m18_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m18)
plot(gform_m18)

# Graph
dev.off()
plot(gform_m18, ylim = c(-.1,.1),
     main="Self Esteem (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Self Esteem (SD)",
     col.lab="black", cex.lab=0.75)



# gratitude ---------------------------------------------------------------
GRATITUDE_st <- as.formula(paste("GRATITUDE_lead2_z ~bs(Standard.Living_lead1_z) +",
                                  paste(baselinevars,
                                        collapse = "+")))

m19_GRATITUDE  <- lapply(1:10, function(i) {
  m <- glm(GRATITUDE_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m19_GRATITUDE, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m19_GRATITUDE, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m19_long <- glm(GRATITUDE_st, data = stf3)
m19_long
gform_m19<- stdGlm(fit = m19_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m19)
plot(gform_m19)

# Graph
dev.off()
plot(gform_m19, ylim = c(-.1,.1),
     main="Gratitude (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Gratitude (SD)",
     col.lab="black", cex.lab=0.75)



# veng rumination ---------------------------------------------------------

VENGEFUL.RUMIN_st <- as.formula(paste("VENGEFUL.RUMIN_lead2_z ~bs(Standard.Living_lead1_z) +",
                                       paste(baselinevars,
                                             collapse = "+")))

m20_VENGEFUL.RUMIN  <- lapply(1:10, function(i) {
  m <- glm(VENGEFUL.RUMIN_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m20_VENGEFUL.RUMIN, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m20_VENGEFUL.RUMIN, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m20_long <- glm(VENGEFUL.RUMIN_st, data = stf3)
m20_long
gform_m20<- stdGlm(fit = m20_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m20)
plot(gform_m20)

# Graph
dev.off()
plot(gform_m20, ylim = c(-.1,.1),
     main="Vengeful Rumination (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Vengeful Rumination (SD)",
     col.lab="black", cex.lab=0.75)



# life meaning ------------------------------------------------------------
LIFEMEANING_st <- as.formula(paste("LIFEMEANING_lead2ord_z ~bs(Standard.Living_lead1_z) +",
                                    paste(baselinevars,
                                          collapse = "+")))

m21_LIFEMEANING  <- lapply(1:10, function(i) {
  m <- glm(LIFEMEANING_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m21_LIFEMEANING, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m21_LIFEMEANING, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m21_long <- glm(LIFEMEANING_st, data = stf3)
m21_long
gform_m21<- stdGlm(fit = m21_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m21)
plot(gform_m21)

# Graph
plot(gform_m21, ylim = c(-.1,.1),
     main="Hours Exercise (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Hours Exercise (SD)",
     col.lab="black", cex.lab=0.75)

# honesty humility --------------------------------------------------------

out <- with(ctrim_st, glm( HONESTY_HUMILITY_lead2_z ~ income_log_lead1_z ))

HONESTY_HUMILITY_st <- as.formula(paste("HONESTY_HUMILITY_lead2_z ~bs(Standard.Living_lead1_z) +",
                                         paste(baselinevars,
                                               collapse = "+")))

m22_HONESTY_HUMILITY  <- lapply(1:10, function(i) {
  m <- glm(HONESTY_HUMILITY_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m22_HONESTY_HUMILITY, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m22_HONESTY_HUMILITY, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m22_long <- glm(HONESTY_HUMILITY_st, data = stf3)
m22_long
gform_m22<- stdGlm(fit = m22_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-1,0,1))
summary(gform_m22, contrast="difference", reference=0)
plot(gform_m22,contrast="difference", reference=0)

confint(gform_m22)
# Graph
dev.off()
plot(gform_m22, contrast="difference", reference=0, ylim = c(-.1,.1),
     main="Honesty Humility (SD)", col.main="black",
     # sub="Difference in standardized means (G-computation)", col.sub="black",
     xlab="Standard Living (SD)", ylab="Honesty Humility (SD)",
     col.lab="black", cex.lab=0.75)



# belonging ---------------------------------------------------------------
BELONG_st <- as.formula(paste("BELONG_lead2_z ~bs(Standard.Living_lead1_z) +",
                               paste(baselinevars,
                                     collapse = "+")))

m23_BELONG  <- lapply(1:10, function(i) {
  m <- glm(BELONG_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m23_BELONG, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m23_BELONG, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m23_long <- glm(BELONG_st, data = stf3)
m23_long
gform_m23<- stdGlm(fit = m23_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m23)
plot(gform_m23)

# Graph
dev.off()
plot(gform_m23, ylim = c(-.1,.1),
     main="Social Belonging (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Social Belonging (SD)",
     col.lab="black", cex.lab=0.75)


# soc support -------------------------------------------------------------


SUPPORT_st <- as.formula(paste("SUPPORT_lead2_z ~bs(Standard.Living_lead1_z) +",
                                paste(baselinevars,
                                      collapse = "+")))

m24_SUPPORT <- lapply(1:10, function(i) {
  m <- glm(SUPPORT_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m24_SUPPORT, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m24_SUPPORT, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m24_long <- glm(SUPPORT_st, data = stf3)
m24_long
gform_m24<- stdGlm(fit = m24_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m24)
plot(gform_m24)

# Graph
dev.off()
plot(gform_m24, ylim = c(-.1,.1),
     main="Social Support (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Social Support (SD)",
     col.lab="black", cex.lab=0.75)



# volunteers --------------------------------------------------------------

Volunteers_st <- as.formula(paste("Volunteers_lead2 ~bs(Standard.Living_lead1_z) +",
                                   paste(baselinevars,
                                         collapse = "+")))

m25_Volunteers  <- lapply(1:10, function(i) {
  m <- glm(Volunteers_st, data = complete(inc_m, action = i), family = "poisson")
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m25_Volunteers, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m25_Volunteers, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m25_long <- glm(Volunteers_st, data = stf3,family = "poisson")
m25_long
gform_m25<- stdGlm(fit = m25_long, data = stf3, X = "Standard.Living_lead1_z", x = -2:2)
summary(gform_m25)
plot(gform_m25)

# Graph
dev.off()
plot(gform_m25, ylim = c(0,.05),
     main="Volunteering (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Volunteering (yes/no)",
     col.lab="black", cex.lab=0.75)


# charity donate ----------------------------------------------------------

CharityDonate_st <- as.formula(paste("CharityDonate_log_lead2_z ~bs(Standard.Living_lead1_z) +",
                                      paste(baselinevars,
                                            collapse = "+")))

m26_CharityDonate  <- lapply(1:10, function(i) {
  m <- glm(CharityDonate_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m26_CharityDonate, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m26_CharityDonate, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m26_long <- glm(CharityDonate_st, data = stf3)
m26_long
gform_m26<- stdGlm(fit = m26_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m26)
plot(gform_m26)

# Graph
plot(gform_m26, ylim = c(-.1,.1),
     main="Log Charity (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Log Charity (SD)",
     col.lab="black", cex.lab=0.75)



# community lead ----------------------------------------------------------

community_st <- as.formula(paste("community_lead2_z ~bs(Standard.Living_lead1_z) +",
                                  paste(baselinevars,
                                        collapse = "+")))

m27_community  <- lapply(1:10, function(i) {
  m <- glm(community_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m27_community, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m27_community, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m27_long <- glm(community_st, data = stf3)

gform_m27 <- stdGlm(fit = m27_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m27)
plot(gform_m27)

# Graph
dev.off()
plot(gform_m27, ylim = c(-.1,.1),
     main="Community Connection (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Community Connection (SD)",
     col.lab="black", cex.lab=0.75)



# national wellbeing ------------------------------------------------------

NWI_st <- as.formula(paste("NWI_lead2_z ~bs(Standard.Living_lead1_z) +",
                            paste(baselinevars,
                                  collapse = "+")))

m28_NWI  <- lapply(1:10, function(i) {
  m <- glm(NWI_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m28_NWI, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m28_NWI, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m28_long <- glm(NWI_st, data = stf3)
m28_long
gform_m28<- stdGlm(fit = m28_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m28)
plot(gform_m28)

# Graph
plot(gform_m28, ylim = c(-.1,.1),
     main="NWI (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Hours Exercise (SD)",
     col.lab="black", cex.lab=0.75)



# imperm group ------------------------------------------------------------
ImpermeabilityGroup_st <- as.formula(paste("ImpermeabilityGroup_lead2_z ~bs(Standard.Living_lead1_z) +",
                                            paste(baselinevars,
                                                  collapse = "+")))

m29_ImpermeabilityGroup <- lapply(1:10, function(i) {
  m <- glm(ImpermeabilityGroup_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m29_ImpermeabilityGroup, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m29_ImpermeabilityGroup, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m29_long <- glm(ImpermeabilityGroup_st, data = stf3)
m29_long
gform_m29<- stdGlm(fit = m29_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m29)
plot(gform_m29)

# Graph
dev.off()
plot(gform_m29, ylim = c(-.2,.2),
     main="Impermeability Group (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Impermeability Group (SD)",
     col.lab="black", cex.lab=0.75)



# stand living ------------------------------------------------------------

Standard.Living_st <- as.formula(paste("Standard.Living_lead2ord_z ~bs(Standard.Living_lead1_z) +",
                                        paste(baselinevars,
                                              collapse = "+")))

m30_Standard.Living  <- lapply(1:10, function(i) {
  m <- glm(Standard.Living_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m30_Standard.Living, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m30_Standard.Living, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m30_long <- glm(Standard.Living_st, data = stf3)
m30_long
gform_m30<- stdGlm(fit = m30_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m30)
plot(gform_m30)

# Graph
#dev.off()
plot(gform_m30, ylim = c(-.1,.1),
     main="Standard Living (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Standard Living (SD)",
     col.lab="black", cex.lab=0.75)


# future security ---------------------------------------------------------


out <- with(ctrim_st, glm( Your.Future.Security_lead2_z ~ income_log_lead1_z ))
#output <- pool(out, dfcom = NULL)
#summary(output, conf.int = TRUE)

Your.Future.Security_st <- as.formula(paste("Your.Future.Security_lead2_z ~bs(Standard.Living_lead1_z) +",
                                             paste(baselinevars,
                                                   collapse = "+")))

m31_Your.Future.Security  <- lapply(1:10, function(i) {
  m <- glm(Your.Future.Security_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m31_Your.Future.Security, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m31_Your.Future.Security, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m31_long <- glm(Your.Future.Security_st, data = stf3)
m31_long
gform_m31<- stdGlm(fit = m31_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m31)
plot(gform_m31)

# Graph
dev.off()
plot(gform_m31, ylim = c(-.1,.1),
     main="Your.Future.Security (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Your.Future.Security (SD)",
     col.lab="black", cex.lab=0.75)



# your health -------------------------------------------------------------


Your.Health_st <- as.formula(paste("Your.Health_lead2_z ~bs(Standard.Living_lead1_z) +",
                                    paste(baselinevars,
                                          collapse = "+")))

m32_Your.Health  <- lapply(1:10, function(i) {
  m <- glm(Your.Health_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m32_Your.Health, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m32_Your.Health, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m32_long <- glm(Your.Health_st, data = stf3)
m32_long
gform_m32<- stdGlm(fit = m32_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m32)
plot(gform_m32)

# Graph
dev.off()
plot(gform_m32, ylim = c(-.1,.1),
     main="Your Health (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Your Health (SD)",
     col.lab="black", cex.lab=0.75)



# personal relationships --------------------------------------------------

out <- with(ctrim_st, glm( Your.Personal.Relationships_lead2ord_z ~ income_log_lead1_z ))

Your.Personal.Relationships_st <- as.formula(paste("Your.Personal.Relationships_lead2ord_z ~bs(Standard.Living_lead1_z) +",  paste(baselinevars,collapse = "+")))

m33_Your.Personal.Relationships  <- lapply(1:10, function(i) {
  m <- glm(Your.Personal.Relationships_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m33_Your.Personal.Relationships, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m33_Your.Personal.Relationships, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

m33_long <- glm(Your.Personal.Relationships_st, data = stf3)
m33_long
gform_m33<- stdGlm(fit = m33_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m33)
plot(gform_m33)

# Graph
dev.off()

plot(gform_m33, ylim = c(-.1,.1),
     main="Your Personal Relationships (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Your Personal Relationships (SD)",
     col.lab="black", cex.lab=0.75)


# Work-life balance -------------------------------------------------------

Emp.WorkLifeBalance_st <- as.formula(paste("Emp.WorkLifeBalance_lead2_z ~bs(Standard.Living_lead1_z) +",
                                            paste(baselinevars,
                                                  collapse = "+")))

m34_Emp.WorkLifeBalance  <- lapply(1:10, function(i) {
  m <- glm(Emp.WorkLifeBalance_st, data = complete(inc_m, action = i))
})


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 3)

parameters::pool_parameters(m34_Emp.WorkLifeBalance, ci_method="wald") %>%
  slice(2:4) %>%
  plot()# conditional effect

parameters::pool_parameters(m34_Emp.WorkLifeBalance, ci_method="wald") %>%
  slice(2:4) %>%
  print_md(digits = 3)

# for
m34_long <- glm(Emp.WorkLifeBalance_st, data = stf3)
m34_long
gform_m34<- stdGlm(fit = m34_long, data = stf3, X = "Standard.Living_lead1_z", x =c(-2,0,2))
summary(gform_m34)
plot(gform_m34)

# Graph
dev.off()
plot(gform_m34, ylim = c(-.1,.1),
     main="Work/life Balance (SD)", col.main="black",
     #sub="My Sub-title", col.sub="black",
     xlab="Standard Living (SD)", ylab="Work/life Balance (SD)",
     col.lab="black", cex.lab=0.75)




# hours-work --------------------------------------------------------------

library(MatchThem)
library(optmatch)
models_hw <- weightthem(Hours.Work_lead1_z ~
                          Hours.Work_z +
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
                          EmotionRegulation1_z +
                          EmotionRegulation2_z +
                          EmotionRegulation3_z +
                          Euro_z +
                          GRATITUDE_z +
                          HomeOwner_z +
                          Hours.Exercise_log_z +
                          # Hours.Work_z +
                          HLTH.BMI_z  + #
                          HLTH.Fatigue_z + #
                          income_log_z +
                          ImpermeabilityGroup_z +
                          KESSLER6sum_z + #
                          LIFEMEANING_z + #
                          LIFESAT_z + #
                          Male_z +
                          NZdep_z +
                          NWI_z +
                          NZSEI13_z +
                          Parent_z +
                          Partner_z +
                          PERFECTIONISM_z +
                          PermeabilityIndividual_z +
                          Pol.Orient_z +
                          POWERDEPENDENCE1_z + #
                          POWERDEPENDENCE2_z + #
                          # PWI_z +
                          Relid_z +
                          Respect.Self_z + #
                          Rumination_z + #
                          SELF.CONTROL_z + #
                          SELF.ESTEEM_z + #
                          SexualSatisfaction_z +#
                          SFHEALTH_z +#
                          Smoker_z +#
                          Spiritual.Identification_z +
                          Standard.Living_z +
                          SUPPORT_z +#
                          Urban_z +
                          VENGEFUL.RUMIN_z +
                          Volunteers_z +
                          Your.Health_z +
                          Your.Future.Security_z +
                          Your.Personal.Relationships_z,
                        out2_sl,
                        approach = "within",
                        estimand = "ATE",
                        stabilize = TRUE,
                        method = "ebal")


saveh(models_hw,"models_hw")


sum<- summary(models_hw)
plot(sum)
sum
bal.tab(models_hw)


ctrim_st <- trim(models_hw, at = .998)
bal.tab(ctrim_st)
summary(ctrim_st)


# iptw models  STANDARD LIVING -------------------------------------------------------------
# no need to trim


out <- with(ctrim_st, glm( HLTH.BMI_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm(SFHEALTH_lead2_z  ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm(Hours.Exercise_lead2_log_z ~ Hours.Work_lead1_z, family = "gaussian" ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( Smoker_lead2 ~ Hours.Work_lead1_z, family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp(-3.02-.14)


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(HLTH.Fatigue_lead2ord ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( 0.01023787, se = 0.003899096, sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm(Alcohol.Frequency_lead2ord_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( 0.01264201, se = 0.005129188, sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(Alcohol.Intensity_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(Bodysat_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( -0.007741457, se = 0.003929686, sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

# out <- with(ctrim, glm(PWI_lead2_z ~ Standard.Living_lead1_z ))
# output <- pool(out, dfcom = NULL)
# summary(output, conf.int = TRUE)

out <- with(ctrim_st, glm( Rumination_lead2ord_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm(SexualSatisfaction_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( 0.009236795 , se = 0.004273584 , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(EmotionRegulation1_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(EmotionRegulation2_lead2_z~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( EmotionRegulation3_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(KESSLER6sum_lead2_z ~ Hours.Work_lead1_z , family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS(-0.006205853 , se = 0.002415414, sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm(POWERDEPENDENCE1_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(POWERDEPENDENCE1_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(PERFECTIONISM_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(SELF.ESTEEM_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( GRATITUDE_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( VENGEFUL.RUMIN_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( LIFEMEANING_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm( HONESTY_HUMILITY_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( BELONG_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( SUPPORT_lead2_z ~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm( Volunteers_lead2 ~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( CharityDonate_lead2 ~ Hours.Work_lead1_10 , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp( 6.9785313)
exp( 6.9785313 - 0.1001891 )
exp( 6.9785313 + 0.1001891 + 0.01807244)
exp( 6.9785313 + 0.1001891 - 0.01807244)



round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(0.9046663, lo = 0.8884637, hi = 0.9211645, true = 1), 3)

exp(-0.1001891  - 6.9785313)/ exp(-6.9785313 )
exp(-0.1001891 + 0.01807244 - 6.9785313)/ exp(-6.9785313 )
exp(-0.1001891 - 0.01807244 - 6.9785313)/ exp(-6.9785313 )


out <- with(ctrim_st, glm(community_lead2_z ~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(NWI_lead2_z~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS(0.025172099 , se = 0.007768097, sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(ImpermeabilityGroup_lead2 ~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm( Standard.Living_lead2ord_z ~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm( Your.Future.Security_lead2_z ~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( Your.Health_lead2_z  ~ Hours.Work_lead1_10 ))

output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( Your.Personal.Relationships_lead2ord_z ~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


# NZSEI13 -- status variable

# standard living ---------------------------------------------------------


# Weights but now with the PWI  variables individually

library(MatchThem)
library(optmatch)
library(splines)
models_sl <- weightthem(Standard.Living_lead1_z ~
                          bs(Standard.Living_z) +
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
                          EmotionRegulation1_z +
                          EmotionRegulation2_z +
                          EmotionRegulation3_z +
                          Euro_z +
                          GRATITUDE_z +
                          HomeOwner_z +
                          Hours.Exercise_log_z +
                          Hours.Work_z +
                          HLTH.BMI_z  + #
                          HLTH.Fatigue_z + #
                          income_log_z +
                          ImpermeabilityGroup_z +
                          KESSLER6sum_z + #
                          LIFEMEANING_z + #
                          LIFESAT_z + #
                          Male_z +
                          NZdep_z +
                          NWI_z +
                          NZSEI13_z +
                          Parent_z +
                          Partner_z +
                          PERFECTIONISM_z +
                          PermeabilityIndividual_z +
                          Pol.Orient_z +
                          POWERDEPENDENCE1_z + #
                          POWERDEPENDENCE2_z + #
                          # PWI_z +
                          Relid_z +
                          Respect.Self_z + #
                          Rumination_z + #
                          SELF.CONTROL_z + #
                          SELF.ESTEEM_z + #
                          SexualSatisfaction_z +#
                          SFHEALTH_z +#
                          Smoker_z +#
                          Spiritual.Identification_z +
                          #  Standard.Living_z +
                          SUPPORT_z +#
                          Urban_z +
                          VENGEFUL.RUMIN_z +
                          Volunteers_z +
                          Your.Health_z +
                          Your.Future.Security_z +
                          Your.Personal.Relationships_z,
                        out2_sl,
                        approach = "within",
                        estimand = "ATE",
                        stabilize = TRUE,
                        method = "ebal")


saveh(models_sl,"models_sl.rds")


sum<- summary(models_sl)
plot(sum)
sum
bal.tab(models_sl)


ctrim <- trim(models_sl, at = .999)
bal.tab(ctrim)
summary(ctrim)


# iptw models -------------------------------------------------------------
# no need to trim


out <- with(ctrim, glm( HLTH.BMI_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(SFHEALTH_lead2_z  ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(Hours.Exercise_lead2 ~ Standard.Living_lead1_z, family = "poisson" ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Smoker_lead2 ~ Standard.Living_lead1_z , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <- with(models_sl, glm(HLTH.Fatigue_lead2ord_z ~ bs(Standard.Living_lead1_z) ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

library(splines)
out2 <-  with(
  long3,
  #  long_f3,
  glm(
    HLTH.Fatigue_lead2ord_z ~  bs(Standard.Living_lead1_z) +
      Standard.Living_z +
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
      EmotionRegulation1_z +
      EmotionRegulation2_z +
      EmotionRegulation3_z +
      Euro_z +
      GRATITUDE_z +
      HomeOwner_z +
      Hours.Exercise_log_z +
      Hours.Work_z +
      HLTH.BMI_z  + #
      HLTH.Fatigue_z + #
      #  income_log_z +
      ImpermeabilityGroup_z +
      KESSLER6sum_z + #
      LIFEMEANING_z + #
      LIFESAT_z + #
      Male_z +
      NZdep_z +
      NWI_z +
      NZSEI13_z +
      Parent_z +
      Partner_z +
      PERFECTIONISM_z +
      PermeabilityIndividual_z +
      Pol.Orient_z +
      POWERDEPENDENCE1_z + #
      POWERDEPENDENCE2_z + #
      # PWI_z +
      Relid_z +
      Respect.Self_z + #
      Rumination_z + #
      SELF.CONTROL_z + #
      SELF.ESTEEM_z + #
      SexualSatisfaction_z + #
      SFHEALTH_z + #
      Smoker_z + #
      Spiritual.Identification_z +
      SUPPORT_z + #
      Urban_z +
      VENGEFUL.RUMIN_z +
      Volunteers_z +
      Your.Health_z +
      Your.Future.Security_z +
      Your.Personal.Relationships_z))
library(stdReg)
options(scipen=999)

summary(out2)

output2 <- pool(out2, dfcom = NULL)
output2
#out2$df.null <- 27072

gform_m1<- stdGlm(fit = out2, data = long3, X  = "Standard.Living_lead1_z", x =seq(-1,1))
summary(gform_m1)
plot(gform_m1)

Estimate Std. Error lower 0.95 upper 0.95
-1  0.02876    0.00347   0.021970    0.03556
0   0.00491    0.00238   0.000244    0.00957
1  -0.03225    0.00326  -0.038644   -0.02586

stimate Std. Error lower 0.95 upper 0.95
-1   0.0306    0.00337   0.023963    0.03716
0    0.0050    0.00230   0.000485    0.00952
1   -0.0330    0.00316  -0.039235   -0.02684

out <- with(ctrim, glm(Alcohol.Frequency_lead2ord ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(Alcohol.Intensity_lead2~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(Bodysat_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

# out <- with(ctrim, glm(PWI_lead2_z ~ Standard.Living_lead1_z ))
# output <- pool(out, dfcom = NULL)
# summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Rumination_lead2ord ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <- with(ctrim, glm(SexualSatisfaction_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <- with(ctrim, glm(EmotionRegulation1_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(EmotionRegulation2_lead2_z~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( EmotionRegulation3_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(KESSLER6sum_lead2 ~ Standard.Living_lead1_z , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(LIFEMEANING_lead2ord_z ~ Standard.Living_lead1_z , family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)




Estimate Std. Error lower 0.95 upper 0.95
-1 -0.09386    0.00340   -0.10053  -0.087199
0  -0.00529    0.00222   -0.00965  -0.000932
1   0.08709    0.00294    0.08134   0.092848

out <- with(ctrim, glm(POWERDEPENDENCE1_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(POWERDEPENDENCE1_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(PERFECTIONISM_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(SELF.ESTEEM_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( GRATITUDE_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( VENGEFUL.RUMIN_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( LIFEMEANING_lead2ord ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( HONESTY_HUMILITY_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( BELONG_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( SUPPORT_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Volunteers_lead2 ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( CharityDonate_log_lead2_z ~ Standard.Living_lead1_z , family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp( 6.867947 + 0.2492043)


out <- with(ctrim, glm(community_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(NWI_lead2_z~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(ImpermeabilityGroup_lead2 ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <- with(ctrim, glm(PermeabilityIndividual_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)



# out <- with(ctrim2, glm( Standard.Living_lead2ord ~ Standard.Living_lead1_z ))
# output <- pool(out, dfcom = NULL)
# summary(output, conf.int = TRUE)


out <- with(ctrim, glm( Your.Future.Security_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Your.Health_lead2_z  ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Your.Personal.Relationships_lead2ord ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)










# BRMS --------------------------------------------------------------------



# create list of data frames

out_c <- complete(ctrim, action ="long", include = FALSE, mild = TRUE)

m <- 10
listdat<- list()
for (i in 1:m) {
  listdat[[i]] <- as.data.frame(out_c[[i]])
}

# create list of data frames

out_c2 <- complete(ctrim2, action ="long", include = FALSE, mild = TRUE)

m <- 10
listdat2<- list()
for (i in 1:m) {
  listdat2[[i]] <- as.data.frame(out_c2[[i]])
}


# enable memory
options(future.globals.maxSize = 8000 * 1024^2)  # needed



# BRMS MODELS forms -------------------------------------------------------------

bf_HLTH.BMI_lead2_z <- bf(HLTH.BMI_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_SFHEALTH_lead2_z <- bf(SFHEALTH_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_Hours.Exercise_lead2 <- bf(Hours.Exercise_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_Smoker_lead2 <- bf( Smoker_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_HLTH.Fatigue_lead2ord <- bf( HLTH.Fatigue_lead2ord |weights(weights) ~ Standard.Living_lead1_z)
bf_Alcohol.Frequency_lead2ord <- bf( Alcohol.Frequency_lead2ord |weights(weights) ~ Standard.Living_lead1_z)
bf_Alcohol.Intensity_lead2 <- bf( as.integer(Alcohol.Intensity_lead2) |weights(weights) ~ Standard.Living_lead1_z)
bf_Bodysat_lead2_z <- bf( Bodysat_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_PWI_lead2_z <- bf( PWI_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_Rumination_lead2ord <- bf(Rumination_lead2ord |weights(weights) ~ Standard.Living_lead1_z)
bf_SexualSatisfaction_lead2_z <- bf(SexualSatisfaction_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_PWI_lead2_z <- bf(PWI_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_EmotionRegulation1_lead2_z <- bf(EmotionRegulation1_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_EmotionRegulation2_lead2_z <- bf(EmotionRegulation2_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_EmotionRegulation3_lead2_z <- bf(EmotionRegulation3_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_KESSLER6sum_lead2 <- bf(KESSLER6sum_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_LIFESAT_lead2ord <- bf(LIFESAT_lead2ord |weights(weights) ~ Standard.Living_lead1_z)
bf_POWERDEPENDENCE_lead2_z <- bf(POWERDEPENDENCE_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_PERFECTIONISM_lead2_z <- bf(PERFECTIONISM_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_SELF.ESTEEM_lead2_z <- bf(SELF.ESTEEM_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_Emp.WorkLifeBalance_lead2_z <- bf( Emp.WorkLifeBalance_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_GRATITUDE_lead2_z <- bf( GRATITUDE_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_VENGEFUL.RUMIN_lead2ord <- bf(VENGEFUL.RUMIN_lead2ord  |weights(weights) ~ Standard.Living_lead1_z)
bf_LIFEMEANING_lead2ord <- bf(LIFEMEANING_lead2ord  |weights(weights) ~ Standard.Living_lead1_z)
bf_HONESTY_HUMILITY_lead2_z <- bf( HONESTY_HUMILITY_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_BELONG_lead2_z <- bf( BELONG_lead2_z  |weights(weights) ~ Standard.Living_lead1_z)
bf_SUPPORT_lead2ord <- bf( SUPPORT_lead2ord |weights(weights) ~ Standard.Living_lead1_z)
bf_Volunteers_lead2 <- bf( Volunteers_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_CharityDonate_lead2 <- bf( CharityDonate_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_community_lead2_z <- bf(community_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_NWI_lead2_z <- bf(NWI_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_ImpermeabilityGroup_z <- bf(ImpermeabilityGroup_z |weights(weights) ~ Standard.Living_lead1_z)
bf_PermeabilityIndividual_z<- bf( PermeabilityIndividual_z|weights(weights) ~ Standard.Living_lead1_z)


## ADD ONE

bf_Standard.Living_lead2ord<- bf( Standard.Living_lead2ord|weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Health_lead2_z<- bf( Your.Health_lead2_z|weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Future.Security_lead2 <- bf(Your.Future.Security_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Personal.Relationships_lead2ord<- bf( Your.Personal.Relationships_lead2ord|weights(weights) ~ Standard.Living_lead1_z)




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

m4_Smoker_lead2<- brm_multiple(
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
  family = cumulative("probit"), # Chose family
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
  file = here::here("mods", "standardliving", "m6_Alcohol.Frequency_lead2ord.rds"),
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

m8_Bodysat_lead2_z<- brm_multiple(
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
  file = here::here("mods", "standardliving", "m11_SexualSatisfaction_lead2_z.rds"),
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
  file = here::here("mods", "standardliving", "m13_EmotionRegulation1_lead2_z.rds"),
)

m14_EmotionRegulation2_lead2_z<- brm_multiple(
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
  file = here::here("mods", "standardliving", "m14_EmotionRegulation2_lead2_z.rds"),
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
  file = here::here("mods", "standardliving", "m15_EmotionRegulation3_lead2_z.rds"),
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


m20_SELF.ESTEEM_lead2_z<- brm_multiple(
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
  file = here::here("mods", "standardliving", "m21_Emp.WorkLifeBalance_lead2_z.rds"),
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


m29_community_lead2_z<- brm_multiple(
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

out3 <- with(ctrim, geeglm(
  PWI_lead2_z  ~ Standard.Living_lead1_z,
  data = cmodels,
  id = 1:nrow(cmodels),
  family = gaussian))

# same result
output <- pool(out3)
summary(output, conf.int = TRUE)
plot(output)


# brms pwi follow up ------------------------------------------------------

bf_Standard.Living_lead2_z<- bf( Standard.Living_lead2_z|weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Health_lead2_z<- bf( Your.Health_lead2_z|weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Future.Security_lead2_z <- bf(Your.Future.Security_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Personal.Relationships_lead2ord<- bf( Your.Personal.Relationships_lead2ord|weights(weights) ~ Standard.Living_lead1_z)


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
  file = here::here("mods", "standardliving", "m35_Your.Future.Security_lead2_z.rds"),
)


m36_Your.Personal.Relationships_lead2ord <- brm_multiple(
  bf_Your.Personal.Relationships_lead2ord,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),  Chose family
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
  file = here::here("mods", "standardliving", "m36_Your.Personal.Relationships_lead2ord.rds"),
)



# old IPTW ----------------------------------------------------------------

# iptw income  --------------------------------------------------------------------
#income

# Weights but now with the PWI  variables individually

library(MatchThem)
library(optmatch)
models_st <- weightthem(income_log_lead1_z ~
                           income_log_z +
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
                           EmotionRegulation1_z +
                           EmotionRegulation2_z +
                           EmotionRegulation3_z +
                           Euro_z +
                           GRATITUDE_z +
                           HomeOwner_z +
                           Hours.Exercise_log_z +
                           Hours.Work_z +
                           HLTH.BMI_z  + #
                           HLTH.Fatigue_z + #
                           #  income_log_z +
                           ImpermeabilityGroup_z +
                           KESSLER6sum_z + #
                           LIFEMEANING_z + #
                           LIFESAT_z + #
                           Male_z +
                           NZdep_z +
                           NWI_z +
                           NZSEI13_z +
                           Parent_z +
                           Partner_z +
                           PERFECTIONISM_z +
                           PermeabilityIndividual_z +
                           Pol.Orient_z +
                           POWERDEPENDENCE1_z + #
                           POWERDEPENDENCE2_z + #
                           # PWI_z +
                           Relid_z +
                           Respect.Self_z + #
                           Rumination_z + #
                           SELF.CONTROL_z + #
                           SELF.ESTEEM_z + #
                           SexualSatisfaction_z +#
                           SFHEALTH_z +#
                           Smoker_z +#
                           Spiritual.Identification_z +
                           Standard.Living_z +
                           SUPPORT_z +#
                           Urban_z +
                           VENGEFUL.RUMIN_z +
                           Volunteers_z +
                           Your.Health_z +
                           Your.Future.Security_z +
                           Your.Personal.Relationships_z,
                         out2_sl,
                         approach = "within",
                         estimand = "ATE",
                         stabilize = TRUE,
                         method = "ebal")


saveh(models_st,"models_st.rds")


sum<- summary(models_st)
plot(sum)
sum
bal.tab(models_st)


ctrim_st <- trim(models_st, at = .999)
bal.tab(ctrim_st)
summary(ctrim_st)


# iptw models  income -------------------------------------------------------------
# no need to trim

Hours.Work_lead1_10

out <- with(ctrim_st, glm( HLTH.BMI_lead2_z ~ income_log_lead1_z ))
out

output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(0.02376129 , se = 0.009437003, sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

