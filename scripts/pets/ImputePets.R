# perfect impute
options(scipen = 999)


# import libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# set preferences
conflict_prefer("pool", "mice")
conflict_prefer("cbind", "base")

# SATLI1TK	SWB.LifeSat01.T11	I am satisfied with my life.
# SATLI2TK	SWB.LifeSat02.T11	In most ways my life is close to ideal.

# for saving models -- use paths that work for your computer
push_mods <-
  fs::path_expand("/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets")
push_figs <-
  fs::path_expand("/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets")


# read data
pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )

# note that you need use the arrow package
dat <- arrow::read_parquet(pull_path)

# check unique ids
length(unique(dat$Id))

dat |>
  filter(Wave == 2015 & YearMeasured == 1 | Wave == 2019 & YearMeasured == 1) |>
  select(Pets, Id, Wave) |>
  drop_na() |>
  summarise(count_distinct = n_distinct(Id))


dat |>
  filter(Wave == 2015 & YearMeasured == 1 | Wave == 2015 & YearMeasured == 1) |>
  select(Pets, Id, Wave) |>
  drop_na() |>
  summarise(pets_mean = mean(Pets))


# plot suggests stress going up?
bad_box_plot_coded <- dat |>
  filter(Wave == 2015 &
           YearMeasured == 1 | Wave == 2019 & YearMeasured == 1) |>
  select(KESSLER6sum, Pets, Pets.coded.catdog_factor, Id, Wave) |>
  drop_na() |>
  ggplot(aes(x = Pets.coded.catdog_factor, y = KESSLER6sum, colour = Pets.coded.catdog_factor)) +
  facet_grid(. ~ Wave) +
  geom_boxplot(notch = TRUE) +
  geom_jitter(shape = 16,
              position = position_jitter(0.2),
              alpha = .05) +
  labs(title = "Do you have any pets?,  N = 46,799",
       y = "KESSLER6",
       x = "'Do you have any pets? (coded)'") + scale_colour_okabe_ito()

bad_box_plot_coded

bad_box_pets<- dat |>
  filter(Wave == 2015 &
           YearMeasured == 1 | Wave == 2019 & YearMeasured == 1) |>
  select(KESSLER6sum, Pets, Pets.coded.catdog_factor, Id, Wave) |>
  drop_na() |>
  mutate(Pets = as.factor(Pets)) |>
  ggplot(aes(x = Pets, y = KESSLER6sum, colour = Pets)) +
  facet_grid(. ~ Wave) +
  geom_boxplot(notch = TRUE) +
  geom_jitter(shape = 16,
              position = position_jitter(0.2),
              alpha = .1) +
  labs(title = "Do you have any pets?",
       y = "KESSLER6",
       x = "'Do you have any pets? (coded)', N = 46,799") + scale_colour_okabe_ito()
bad_box_pets

ggsave(
  bad_box_plot_coded,
  path = here::here(here::here("figs", "pets")),
  width = 12,
  height = 8,
  units = "in",
  filename = "bad_box_plot_coded.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


ggsave(
  bad_box_pets,
  path = here::here(here::here("figs", "pets")),
  width = 12,
  height = 8,
  units = "in",
  filename = "bad_box_pets_coded.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

bad_box_plot


means <- dat %>%
  filter(Wave == 2015 &
           YearMeasured == 1 | Wave == 2019 & YearMeasured == 1) |>
  group_by(Pets.coded.catdog_factor, Wave) %>%
  summarize(
    mean_Kessler6 = mean(KESSLER6sum, na.rm = TRUE),
    sd_Kessler6 = sd(KESSLER6sum, na.rm = TRUE),
    sem_Kessler6 = sd(KESSLER6sum, na.rm = TRUE) / sqrt(n())
  ) |>
  drop_na()

# id counts

# MNLI1TK	LifeMeaning01.T11	My life has a clear sense of purpose.
# MNLI2TK	LifeMeaning02.T11	I have a good sense of what makes my life meaningful.


# Format the means table using kableExtra

kable(means, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

library(viridis)
bad_means <- ggplot(means,
       aes(x = Pets.coded.catdog_factor, y = mean_Kessler6, fill = Wave)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(
    aes(ymin = mean_Kessler6 - sem_Kessler6, ymax = mean_Kessler6 + sem_Kessler6),
    width = 0.2,
    position = position_dodge(.9)
  ) +
  labs(x = "Wave", y = "Mean Kessler6") +
  scale_color_viridis_d()


bad_means

ggsave(
  gcomp_forestplot_pets,
  path = here::here(here::here("figs", "pets")),
  width = 12,
  height = 8,
  units = "in",
  filename = "gcomp_forestplot_pets.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


ggsave(
  gcomp_forestplot_pets,
  path = here::here(here::here("figs", "pets")),
  width = 12,
  height = 8,
  units = "in",
  filename = "gcomp_forestplot_pets.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)







means_pets <- dat %>%
  filter(Wave == 2015 &
           YearMeasured == 1 | Wave == 2019 & YearMeasured == 1) |>
  mutate(Pets = as.factor(Pets)) |>
  group_by(Pets, Wave) %>%
  summarize(
    mean_Kessler6 = mean(KESSLER6sum, na.rm = TRUE),
    sd_Kessler6 = sd(KESSLER6sum, na.rm = TRUE),
    sem_Kessler6 = sd(KESSLER6sum, na.rm = TRUE) / sqrt(n())
  ) |>
  drop_na()


# looks higher
ggplot(means_pets, aes(x = Pets, y = mean_Kessler6, fill = Wave)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(
    aes(ymin = mean_Kessler6 - sem_Kessler6, ymax = mean_Kessler6 + sem_Kessler6),
    width = 0.2,
    position = position_dodge(.9)
  ) +
  labs(x = "Wave", y = "Mean Kessler6")  +
  scale_colour_okabe_ito()

## table summary



# MNLI1TK	LifeMeaning01.T11	My life has a clear sense of purpose.
# MNLI2TK	LifeMeaning02.T11	I have a good sense of what makes my life meaningful.
# table for participant N
dat_new <- dat %>%
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
  ))) |>
  #%>%
  # dplyr::rename(. # did this in the man dataframe
  #   kessler_hopeless = SWB.Kessler01,
  #   # …  you feel hopeless?
  #   kessler_depressed = SWB.Kessler02,
  #   #…  you feel so depressed that nothing could cheer you up?
  #   kessler_restless  = SWB.Kessler03,
  #   #…  you feel restless or fidgety?
  #   kessler_effort = SWB.Kessler04,
  #   #…  you feel that everything was an effort?
  #   kessler_worthless = SWB.Kessler05,
#   #…  you feel worthless?
#   kessler_nervous = SWB.Kessler06 #…  you feel nervous?
# ) |>
# dplyr::rename(
#  # My life has a clear sense of purpose.
#   meaning_purpose= LifeMeaning01,
#  # I have a good sense of what makes my life meaningful.
#   meaning_sense = LifeMeaning02,
#   #…  you feel so depressed that nothing could cheer you up?
#
# ) |>
dplyr::filter((Wave == 2015  & YearMeasured  == 1) |
                (Wave == 2019  &
                   YearMeasured  == 1) |
                (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
  group_by(Id) %>%
  dplyr::mutate(org2015 =  ifelse(Wave == 2015 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold15 = mean(org2015, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold15 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  ungroup() %>%
  mutate(
    Pets = factor(Pets),
    pets = as.numeric(Pets),
    pets_coded = as.numeric(Pets.coded.catdog_factor) - 1
  ) |>
  droplevels() %>%
  arrange(Id, Wave)

## NOTE differenCeS
dat_new |>
  filter(Wave == 2015 &
           YearMeasured == 1 | Wave == 2019 & YearMeasured == 1) |>
  select(KESSLER6sum, Pets, Pets.coded.catdog_factor, Id, Wave) |>
  drop_na() |>
  ggplot(aes(x = Pets.coded.catdog_factor, y = KESSLER6sum, colour = Pets.coded.catdog_factor)) +
  facet_grid(. ~ Wave) +
  geom_boxplot(notch = TRUE) +
  geom_jitter(shape = 16,
              position = position_jitter(0.2),
              alpha = .05) +
  labs(title = "Do you have any pets?",
       y = "KESSLER6",
       x = "'Do you have any pets? (coded)'") + scale_colour_okabe_ito()


dat_new |>
  filter(Wave == 2015 &
           YearMeasured == 1 | Wave == 2019 & YearMeasured == 1) |>
  select(KESSLER6sum, Pets, Id, Wave) |>
  drop_na() |>
  ggplot(aes(x = Pets, y = KESSLER6sum, fill = Pets)) +
  facet_grid(. ~ Wave) +
  geom_boxplot(notch = TRUE) +
  geom_jitter(shape = 16,
              position = position_jitter(0.2),
              alpha = .05) +
  labs(title = "Do you have any pets?",
       y = "KESSLER6",
       x = "'Do you have any pets? (coded)'") + scale_colour_okabe_ito()


# check change

length(unique(dat_new$Id)) # 9074





## Look at change
ss_pets <- dat_new |>
  select(
    Id,
    Pets,
    Pets.coded.catdog_factor,
    KESSLER6,
    KESSLER6sum,
    kessler_hopeless,
    #   # …  you feel hopeless?
    kessler_depressed,
    #   #…  you feel so depressed that nothing could cheer you up?
    kessler_restless,
    #   #…  you feel restless or fidgety?
    kessler_effort,
    #   #…  you feel that everything was an effort?
    kessler_worthless,
    #   #…  you feel worthless?
    kessler_nervous#…  you feel nervous?
  ) |>
  mutate(pets = as.numeric(Pets),
         pets_coded = as.numeric(Pets.coded.catdog_factor)) |>
  droplevels()

msm::statetable.msm(round(KESSLER6sum, 0), Id, data = ss_pets) |>
  kbl() |>
  kable_paper(full_width = F)


msm::statetable.msm(round(KESSLER6, 0), Id, data = ss_pets) |>
  kbl() |>
  kable_paper(full_width = F)

msm::statetable.msm(round(kessler_depressed, 0), Id, data = ss_pets) |>
  kbl() |>
  kable_paper(full_width = F)

msm::statetable.msm(round(kessler_restless, 0), Id, data = ss_pets) |>
  kbl() |>
  kable_paper(full_width = F)


msm::statetable.msm(round(kessler_worthless, 0), Id, data = ss_pets) |>
  kbl() |>
  kable_paper(full_width = F)

msm::statetable.msm(round(kessler_hopeless, 0), Id, data = ss_pets) |>
  kbl() |>
  kable_paper(full_width = F)


msm::statetable.msm(round(kessler_restless, 0), Id, data = ss_pets) |>
  kbl() |>
  kable_paper(full_width = F)

msm::statetable.msm(round(kessler_nervous, 0), Id, data = ss_pets) |>
  kbl() |>
  kable_paper(full_width = F)


msm::statetable.msm(round(kessler_effort, 0), Id, data = ss_pets) |>
  kbl() |>
  kable_paper(full_width = F)


ss_pets$pets

msm::statetable.msm(round(pets, 0), Id, data = ss_pets) |>
  kbl() |>
  kable_paper(full_width = F)

msm::statetable.msm(round(pets_coded, 0), Id, data = ss_pets) |>
  kbl() |>
  kable_paper(full_width = F)


Pets.coded.catdog_factor


# Check ids
length(unique(dat_new$Id)) # 34783



### ELIGIBILITY CRITERIA
# 2017/ 2019 - no pets
# GAINED PETS



dat_new |>
  filter(Wave == 2015) |>
  summarise(mean_fatigue = mean(HLTH.Fatigue, na.rm = TRUE))


#na
dat_new |>
  filter(Wave == 2015) |>
  summarise(mean_rumination = mean(Rumination, na.rm = TRUE))

# na
dat_new |>
  filter(Wave == 2017) |>
  summarise(mean_gr = mean(GRATITUDE, na.rm = TRUE))

dat_new |>
  filter(Wave == 2017) |>
  summarise(mean_sa = mean(SexualSatisfaction, na.rm = TRUE))

dat_new |>
  filter(Wave == 2017) |>
  summarise(mean_sa = mean(Respect.Self, na.rm = TRUE))

## select vars
dat_prep  <- dat_new %>%
  arrange(Wave, Id) |>
  select(
    Id,
    YearMeasured,
    Wave,
    Gender3,
    Partner,
    EthCat,
    Age,
    NZSEI13,
    pets,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    Edu,
    NZDep2013,
    Employed,
    # HomeOwner,
    Pol.Orient,
    SDO,
    RWA,
    Rural_GCH2018,
    Household.INC,
    Parent,
    Relid,
    Religion.Church,
    Believe.Spirit,
    Believe.God,
    #  Spiritual.Identification,
    SWB.SoC01,
    #  EmotionRegulation1,
    #  EmotionRegulation2,
    #  EmotionRegulation3,
    #  Bodysat,
    #  VENGEFUL.RUMIN,
   # retired,
   # semiretired,
    BornNZ,
    kessler_hopeless,
    #   # …  you feel hopeless?
    kessler_depressed,
    #   #…  you feel so depressed that nothing could cheer you up?
    kessler_restless,
    #   #…  you feel restless or fidgety?
    kessler_effort,
    #   #…  you feel that everything was an effort?
    kessler_worthless,
    #   #…  you feel worthless?
    kessler_nervous,
    #…  you feel nervous?
    # KESSLER6sum,
    HLTH.Fatigue,
    # Rumination,
    Smoker,
    ChildrenNum,
    NWI,
    BELONG,
    SUPPORT,
    CharityDonate,
    HoursCharity,
    #  GRATITUDE,
    Hours.Work,
    HLTH.SleepHours,
    HLTH.Disability,
    Hours.Exercise,
    #   LIFEMEANING,
    LIFESAT,
    # PWI,  ##  we use the individual
    #  NWI,
    SFHEALTH,
    #  SELF.CONTROL,
    # SFHEALTH,
    #  SELF.ESTEEM,
    #    Respect.Self,
    #  GenCohort,
    #  SELF.ESTEEM,
    #    SELF.CONTROL,
    #  Emp.WorkLifeBalance,
    #   Alcohol.Frequency,
    #  Alcohol.Intensity,
    #   HLTH.BMI,
    #   Smoker,
    ChildrenNum,
    # GenCohort,
    #  partnerlost_job,
    #  lost_job,
    #  began_relationship,
    Alcohol.Intensity,
    Alcohol.Frequency,
    SexualOrientation,
   Hours.Pets,
    #   SexualSatisfaction,
    #  POWERDEPENDENCE1,
    #  POWERDEPENDENCE2,
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living#,
    #    PERFECTIONISM,
    #   PermeabilityIndividual,
    #   ImpermeabilityGroup,
  #  Emp.JobSecure. # lots missing
  ) %>%
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(#  Volunteers = if_else(HoursCharity == 1, 1, 0),
    Hours.Pets_log = log(Hours.Pets + 1)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of
  arrange(Id, Wave) %>%
  dplyr::mutate(#  Volunteers = if_else(HoursCharity == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),) %>%
  arrange(Id, Wave)  %>% # dplyr::mutate(Hours.Work_lead1 = lead(Hours.Work, n = 1)) %>%
  dplyr::mutate(across(c(pets),
                       ~ lead(.x, n = 1),
                       .names = "{col}_lead1")) %>% # make leads
  dplyr::mutate(across(
    c(
      #NZSEI13,
      #Household.INC,
      #    Standard.Living,
      #  NZDep.2018,
      #  Employed,
      #  Household.INC,

      #    Hours.Work,
      #    HLTH.Disability,
      #    EmotionRegulation1,
      #    EmotionRegulation2,
      #    EmotionRegulation3,
      #    Bodysat,
      #    VENGEFUL.RUMIN,
      kessler_hopeless,
      #   # …  you feel hopeless?
      kessler_depressed,
      #   #…  you feel so depressed that nothing could cheer you up?
      kessler_restless,
      #   #…  you feel restless or fidgety?
      kessler_effort,
      #   #…  you feel that everything was an effort?
      kessler_worthless,
      #   #…  you feel worthless?
      kessler_nervous,
      #…  you feel nervous?
      # KESSLER6sum,
      HLTH.Fatigue,
    #  Rumination,
    #  Smoker,
    #  HLTH.BMI,
      BELONG,
      SUPPORT,
      community,
      # CharityDonate,
      # HoursCharity,
      # GRATITUDE,
      # Hours.Work,
      # HLTH.SleepHours,
      # HLTH.Disability,
      # Hours.Exercise,
      #  LIFEMEANING,
      LIFESAT,
      # PWI, can reconstruct later
      # NWI,
      SFHEALTH,
      #  SELF.CONTROL,
      #   SFHEALTH,
      #  SELF.ESTEEM,
      #  Respect.Self,
      #  SELF.ESTEEM,
      #  SELF.CONTROL,
      #  Emp.WorkLifeBalance,
      #  Alcohol.Frequency,
      #  Alcohol.Intensity,
      #  SexualSatisfaction,
      #  POWERDEPENDENCE1,
      #  POWERDEPENDENCE2,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2015) %>%
  dplyr::filter(!is.na(pets)) %>% # no missingness in intervention
  dplyr::filter(!is.na(pets_lead1)) %>% #  no missingness in intervention
  dplyr::select(-c(
    Religion.Church,
   # Respect.Self_lead2,
    # not there
   # Emp.WorkLifeBalance,
    # not at baseline,
   Hours.Pets,
    YearMeasured
  )) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)

corr_pwi <- cbind.data.frame(
  dat_prep$Your.Future.Security,
  dat_prep$Your.Personal.Relationships,
  dat_prep$Your.Health,
  dat_prep$Standard.Living
)

corr_pwi <- corr_pwi |>
  drop_na()

head(corr_pwi)

# does not work
corr_pwi |>
  correlation::correlation()


corr_powerdep <- cbind.data.frame(dat_prep$kessler_hopeless,
                                  dat_prep$kessler_depressed,
                                  dat_prep$kessler_restless,
                                  dat_prep$kessler_effort,
                                  dat_prep$kessler_worthless,
                                  dat_prep$kessler_nervous
                                  )

corr_powerdep <- corr_powerdep |>
  drop_na()

head(corr_powerdep)

# OK
corr_powerdep |>
  correlation::correlation()

# Filtering retirement -- consistency and positivity assumptions

# number of ids
N <- length(unique(dat_prep$Id))
N  # 8675
dat_prep

# inspect data, with eye to large missingness
skim(dat_prep) |>
  arrange(n_missing)
# save data
# saveh(dat_prep, "outcomewide-pets-dat_prep")
#
# # read if needed
# dat_prep <- readh("outcomewide-pets-dat_prep")


# mice model  -------------------------------------------------------------
library(mice)
str(mice_a)
mice_a <- dat_prep %>%
  dplyr::select(-c(Wave, Id))  # won't otherwise run

library(naniar)
naniar::gg_miss_var(mice_a)

# any col.inear vars?
mice:::find.collinear(mice_a)

# impute
ppm_mice <- mice::mice(mice_a,  seed = 0, m = 10)

# save
saveRDS(ppm_mice, here::here("/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets", "outcomewide-pets-ppm_mice"))

# read
ppm_mice <- readRDS(here::here( "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets", "outcomewide-pets-ppm_mice"))

# impute using random forest  (Same results )

# rf_mice <- mice(mice_a, method = "rf", ntrees = 10, seed = 0)
# saveh(rf_mice, "outcomewide-perfect-rf_mice")
#
# plot(rf_mice)
# library("lattice")
# plot(rf_mice)
# a# save
# saveh(rf_mice, "rf_mice")
#
# # read
# rf_mice <- readh("rf_mice")

# checks
outlist2 <-
  row.names(ppm_mice)[ppm_mice$outflux < 0.5]
length(outlist2)

# checks
head(ppm_mice$loggedEvents, 10)

# data warangling
# we create two completed data sets -- the one without the missing data will be useful for inspecting variable


ml <- mice::complete(ppm_mice, "long", inc = TRUE)

moz <- mice::complete(ppm_mice, "long", inc = TRUE)



# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(ml)


# n ids
dat_prep <- readh("outcomewide-pets-dat_prep")
N <- length(unique(dat_prep$Id))
N


# create variables in z score

ml <- ml %>%
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(pets = pets -1) |>
  dplyr::mutate(pets_lead1 = pets_lead1 -1) |>
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(Volunteers = if_else(HoursCharity > 0, 1, 0)) |>
  dplyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity + 1)) %>%
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0))) %>%
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0))) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(
    Your.Personal.Relationships_lead2, digits = 0
  ) + 1)) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0) + 1)) %>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0))) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
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
dplyr::group_by(id) |> mutate(KESSLER6sum = sum(
  c(
    kessler_hopeless,
    #   # …  you feel hopeless?
    kessler_depressed,
    #   #…  you feel so depressed that nothing could cheer you up?
    kessler_restless,
    #   #…  you feel restless or fidgety?
    kessler_effort,
    #   #…  you feel that everything was an effort?
    kessler_worthless,
    #   #…  you feel worthless?
    kessler_nervous
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
 dplyr::group_by(id) |>
    mutate(KESSLER6_lead2 = mean(
      c(
        kessler_hopeless_lead2,
        kessler_depressed_lead2,
        kessler_restless_lead2,
        kessler_effort_lead2,
        kessler_worthless_lead2,
        kessler_nervous_lead2
      ),
      na.rm = TRUE
    )) |>
  dplyr::ungroup() |>
  droplevels() |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z)) |>
  dplyr::mutate(
    EthCat = as.factor(EthCat),
    Gender3  = as.factor(Gender3),
    SexualOrientation  = as.factor(SexualOrientation),
    SexualOrientation
  )

# Get data into shape
ml <- ml %>% mutate_if(is.matrix, as.vector)
ml <- mice::as.mids(ml)
mf <- mice::complete(ml, "long", inc = TRUE)

mf$KESSLER6_lead2
#save
saveRDS(ml, here::here( "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets", "outcomewide-pets-ml"))
saveRDS(mf,  here::here( "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets", "outcomewide-pets-mf"))


length(unique(mf$id))
# data for loss and gain only -----------------------------------------------------



n1 <- dat_prep |>
  dplyr::mutate(pets = pets -1) |>
  filter(pets == 1)

N1 <- length(unique(n1$Id))
N1




ml_one_base <- moz |>
  dplyr::mutate(pets = pets -1) |>
  filter(pets == 1) |>
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(Volunteers = if_else(HoursCharity > 0, 1, 0)) |>
  dplyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity + 1)) %>%
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0))) %>%
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0))) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(
    Your.Personal.Relationships_lead2, digits = 0
  ) + 1)) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0) + 1)) %>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0))) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::mutate(id = as.factor(rep(1:N1, 11))) |> # needed for g-comp
  dplyr::group_by(id) |> mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  dplyr::group_by(id) |> mutate(KESSLER6sum = sum(
    c(
      kessler_hopeless,
      #   # …  you feel hopeless?
      kessler_depressed,
      #   #…  you feel so depressed that nothing could cheer you up?
      kessler_restless,
      #   #…  you feel restless or fidgety?
      kessler_effort,
      #   #…  you feel that everything was an effort?
      kessler_worthless,
      #   #…  you feel worthless?
      kessler_nervous
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
  dplyr::group_by(id) |>
  mutate(KESSLER6_lead2 = mean(
    c(
      kessler_hopeless_lead2,
      kessler_depressed_lead2,
      kessler_restless_lead2,
      kessler_effort_lead2,
      kessler_worthless_lead2,
      kessler_nervous_lead2
    ),
    na.rm = TRUE
  )) |>
  dplyr::ungroup() |>
  droplevels() |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z)) |>
  dplyr::mutate(
    EthCat = as.factor(EthCat),
    Gender3  = as.factor(Gender3),
    SexualOrientation  = as.factor(SexualOrientation),
    SexualOrientation
  )


# now the zeros
n0 <- dat_prep |>
  dplyr::mutate(pets = pets -1) |>
  filter(pets == 0)

n0 <- length(unique(n0$Id))
n0





ml_zero_base <- moz |>
  dplyr::mutate(pets = pets -1) |>
  filter(pets == 0) |>
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(Volunteers = if_else(HoursCharity > 0, 1, 0)) |>
  dplyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity + 1)) %>%
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0))) %>%
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0))) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(
    Your.Personal.Relationships_lead2, digits = 0
  ) + 1)) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0) + 1)) %>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0))) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::mutate(id = as.factor(rep(1:n0, 11))) |> # needed for g-comp
  dplyr::group_by(id) |> mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  dplyr::group_by(id) |> mutate(KESSLER6sum = sum(
    c(
      kessler_hopeless,
      #   # …  you feel hopeless?
      kessler_depressed,
      #   #…  you feel so depressed that nothing could cheer you up?
      kessler_restless,
      #   #…  you feel restless or fidgety?
      kessler_effort,
      #   #…  you feel that everything was an effort?
      kessler_worthless,
      #   #…  you feel worthless?
      kessler_nervous
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
  dplyr::group_by(id) |>
  mutate(KESSLER6_lead2 = mean(
    c(
      kessler_hopeless_lead2,
      kessler_depressed_lead2,
      kessler_restless_lead2,
      kessler_effort_lead2,
      kessler_worthless_lead2,
      kessler_nervous_lead2
    ),
    na.rm = TRUE
  )) |>
  dplyr::ungroup() |>
  droplevels() |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z)) |>
  dplyr::mutate(
    EthCat = as.factor(EthCat),
    Gender3  = as.factor(Gender3),
    SexualOrientation  = as.factor(SexualOrientation),
    SexualOrientation
  )

# save data ---------------------------------------------------------------




# save
ml_zero_base <- ml_zero_base %>% mutate_if(is.matrix, as.vector)
ml_zero_base <- mice::as.mids(ml_zero_base)
mf_zero_base <- mice::complete(ml_zero_base, "long", inc = TRUE)


saveRDS(ml_zero_base, here::here( "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets", "outcomewide-pets-ml_zero_base"))
saveRDS(mf_zero_base,  here::here( "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets", "outcomewide-pets-mf_zero_base"))


# save
ml_one_base <- ml_one_base %>% mutate_if(is.matrix, as.vector)
ml_one_base <- mice::as.mids(ml_one_base)
mf_one_base <- mice::complete(ml_one_base, "long", inc = TRUE)

saveRDS(ml_one_base, here::here( "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets", "outcomewide-pets-ml_one_base"))
saveRDS(mf_one_base,  here::here( "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets", "outcomewide-pets-mf_one_base"))


# # imputed data
# data_imputed <- ml
#
# # imputed data in long format
# data_long <- mf
#
# # raw data (pre-imputation) for sensitivity analysis
# data_raw <- mf |>
#   slice(1:N) |>
#   droplevels()
#
#
# nrow(data_raw)
# no missing use data -----------------------------------------------------
no_miss_model <- data_raw %>%
  dplyr::mutate(id = factor(length(1:N))) |>
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0))) %>%
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0))) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(
    Your.Personal.Relationships_lead2, digits = 0
  ) + 1)) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0) + 1)) %>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0))) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::group_by(id) |> mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  dplyr::group_by(id) |> mutate(KESSLER6sum = sum(
    c(
      kessler_hopeless,
      #   # …  you feel hopeless?
      kessler_depressed,
      #   #…  you feel so depressed that nothing could cheer you up?
      kessler_restless,
      #   #…  you feel restless or fidgety?
      kessler_effort,
      #   #…  you feel that everything was an effort?
      kessler_worthless,
      #   #…  you feel worthless?
      kessler_nervous
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
  # dplyr::group_by(id) |>
  mutate(KESSLER6sum_lead2 = sum(
    c(
      kessler_hopeless_lead2,
      kessler_depressed_lead2,
      kessler_restless_lead2,
      kessler_effort_lead2,
      kessler_worthless_lead2,
      kessler_nervous_lead2
    ),
    na.rm = TRUE
  )) |>
  dplyr::ungroup() |>
  droplevels() |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
 # select(-c(
 #           .id_z)) |>
  dplyr::mutate(
    EthCat = as.factor(EthCat),
    Gender3  = as.factor(Gender3),
    SexualOrientation  = as.factor(SexualOrientation),
    SexualOrientation
  )




saveRDS(no_miss_model, here::here( "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/pets","outcomewide-pets-no-miss-model"))







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
    NZDep.2018 +
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
  dplyr::mutate(Volunteers = if_else(HoursCharity > 0, 1, 0)) |>
  dplyr::mutate(Volunteers_lead2 = if_else(HoursCharity_lead2 > 0, 1, 0)) |>
  dplyr::mutate(NZSEI13_lead1_10 = NZSEI13_lead1 / 10) |>
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
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(
    Your.Personal.Relationships_lead2, digits = 0
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
  dplyr::mutate(
    EthCat = as.factor(EthCat),
    Gender3  = as.factor(Gender3),
    SexualOrientation  = as.factor(SexualOrientation)
  )


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
    NZDep.2018,
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



table1::table1(~ KESSLER6sum_z |
                 Wave, data = data_ml)


saveRDS(data_ml, here::here("data", "outcomewide" , "amy", "data_ml"))
