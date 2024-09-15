#time trend

#libraries and functions
# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")


# read data

pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )

# for saving models
push_mods <-
  fs::path_expand("/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/jm/mods")


dat <- arrow::read_parquet(pull_path)


# save data
dt_00_t <- read_parquet(here::here(push_mods, "dt_wide_t"))


dt_00_t$t0_w_GendAgeEuro

table1( ~ t1_Warm.MentalIllness + t0_Warm.MentalIllness |
          t1_exposure,
        data = dt_wide_t)

dt_00_t$t1_w_GendAgeEuro

dt_wide2_t <- dt_00_t |>
  filter(t1_exposure == 0) |>
  #  select(starts_with("t0"), "t1_TSCORE", "t1_exposure" , starts_with("t1_Warm.")) |>
  select(-c(t0_exposure, t0_TSCORE)) |>
  mutate(t1_TSCORE = t1_TSCORE - min(t1_TSCORE)) |>  # get zero for lowest value
  rename(t0_weights = t0_w_GendAgeEuro,
         t1_weights = t1_w_GendAgeEuro) |>
  dplyr::mutate(across(starts_with("t0_Warm."), ~ scale(.x), .names = "{col}_z")) |>
  dplyr::mutate(across(starts_with("t1_Warm."), ~ scale(.x), .names = "{col}_z"))

table1( ~ t1_Warm.MentalIllness + t0_Warm.MentalIllness |
          t1_exposure,
        data = dt_wide2_t)


# bias
hist(dt_wide2_t$t1_TSCORE)

# only 7
sum(dt_wide2_t$t1_TSCORE < 50)

# only 26 but big jumpt to follow
sum(dt_wide2_t$t1_TSCORE < 75)



dt_f <- dt_wide2_t |>
  dplyr::filter(t1_TSCORE > 75) |>
  mutate(t1_TSCORE = (t1_TSCORE - min(t1_TSCORE)) / 365 * 2)  # recalibrate

nrow(dt_f)# 28293
dt_f$t0_weights
# looks better
hist(dt_f$t1_TSCORE)
dt_f$t1_Warm.Immigrants_z

# not much missingness
vis_miss(dt_f, warn_large_data = FALSE)

hist(dt_f$t1_TSCORE)
colnames(dt_wide2_t)
#get control vars


cvars = dt_f |>
  #  dplyr::select(!starts_with("t0_Warm.")) |>
  dplyr::select(!starts_with("t1_")) |>
  dplyr::select(-c(Id,
                   t0_weights,
                   t0_post_attacks)) |>
  colnames()


#cvars
#|>
#  relocate(starts_with("t0_"), .before = starts_with("t1_"))

min (dt_f$t1_TSCORE)

### function for models
Y = "Y"
X = "t1_TSCORE"

# test
# as.formula(paste(paste(Y, "~ bs(", X , ")+"),
#                  paste(cvars, collapse = "+")))


#test function
lm_jm_covid = function(df, Y, X, cvars, base) {
  require("splines")
  fit <-  lm(as.formula(paste(
    paste(Y, "~ bs(", X , ")+"),
    paste(cvars, collapse = "+")
  )),
  weights = t0_weights,
  # repeated measures
  data = df)
}

df = dt_f
X = "t1_TSCORE"




# cvars = mf |>
#   dplyr::select(-c(
#     .imp,
#     .id,
#     Id,
#     time,
#     weights
#   )) |> # include?
#   dplyr::select(!starts_with("Warm.")) |>
#   colnames()
#
# cvars

# hack
# my_vec <- c("Partner", "Euro", "GenCohort", "Male", "NZSEI13",
#             "NZDep2018", "Rural_GCH2018", "REGC_2022", "CONSCIENTIOUSNESS",
#             "OPENNESS", "HONESTY_HUMILITY", "EXTRAVERSION", "NEUROTICISM",
#             "AGREEABLENESS", "edu_n", "Employed", "BornNZ", "Pol.Orient",
#             "Pol.Wing", "Parent", "Relid")

#my_vec <- setdiff(my_vec, c("Rural_GCH2018", "REGC_2022"))
#my_vec

# needed to plot effects

dt_f[] <- lapply(dt_f, as.numeric)


# increasing

m_mus <-
  lm_jm_covid(df, "t1_Warm.Muslims_z", X, cvars = cvars)

model_parameters(m_mus <- lm(data = dt_f,
                             t1_Warm.Muslims_z ~ bs(t1_TSCORE) + t0_Warm.Muslims_z))

model_parameters(m_mus)
plot_mus  <-
  plot(ggeffects::ggpredict(m_mus, terms = "t1_TSCORE")) + scale_y_continuous(limits = c(-.2, .2))

plot_mus

# increasing
model_parameters(m_chi <- lm(data = dt_f,
                             t1_Warm.Chinese_z ~ bs(t1_TSCORE) + t0_Warm.Chinese_z)) |>
  print_md(digits = 2)


plot_chi <-
  plot(ggeffects::ggpredict(m_chi, terms = "t1_TSCORE")) + scale_y_continuous(limits = c(-.2, .2))

# increasing
plot_chi

model_parameters(m_imm <- lm(
  data = dt_f,
  t1_Warm.Immigrants_z ~  bs(t1_TSCORE) + t0_Warm.Immigrants_z
)) |>
  print_md(digits = 2)


plot_imm <-
  plot(ggeffects::ggpredict(m_imm, terms = "t1_TSCORE")) + scale_y_continuous(limits = c(-.2, .2))

plot_imm

model_parameters(m_nze <- lm(data = dt_f,
                             t1_Warm.NZEuro_z ~  bs(t1_TSCORE) + t0_Warm.NZEuro_z)) |>
  print_md(digits = 2)

plot_nze <-
  plot(ggeffects::ggpredict(m_nze, terms = "t1_TSCORE")) + scale_y_continuous(limits = c(-.2, .2))

plot_nze

model_parameters(m_ind <- lm(data = dt_f,
                             t1_Warm.Indians_z ~ bs(t1_TSCORE) + t0_Warm.Indians_z)) |>
  print_md(digits = 2)


plot_ind <-
  plot(ggeffects::ggpredict(m_ind, terms = "t1_TSCORE")) + scale_y_continuous(limits = c(-.2, .2))

plot_ind


model_parameters(m_asi <- lm(data = dt_f,
                             t1_Warm.Asians_z ~ bs(t1_TSCORE) + t0_Warm.Asians_z)) |>
  print_md(digits = 2)

plot_asi <-
  plot(ggeffects::ggpredict(m_asi, terms = "t1_TSCORE")) + scale_y_continuous(limits = c(-.2, .2))

plot_asi



model_parameters(m_ref <- lm(data = dt_f,
                             t1_Warm.Refugees_z ~  bs(t1_TSCORE) + t0_Warm.Refugees_z)) |>
  print_md(digits = 2)


plot_ref <-
  plot(ggeffects::ggpredict(m_ref, terms = "t1_TSCORE")) + scale_y_continuous(limits = c(-.2, .2))

plot_ref



model_parameters(m_mao <- lm(data = dt_f,
                             t1_Warm.Maori_z ~ bs(t1_TSCORE) + t0_Warm.Maori_z)) |>
  print_md(digits = 2)



plot_mao <-
  plot(ggeffects::ggpredict(m_mao, terms = "t1_TSCORE")) + scale_y_continuous(limits = c(-.2, .2))

plot_mao



model_parameters(m_pac <- lm(data = dt_f,
                             t1_Warm.Pacific_z ~  bs(t1_TSCORE)  + t0_Warm.Pacific_z)) |>
  print_md(digits = 2)

plot_pac <-
  plot(ggeffects::ggpredict(m_pac, terms = "t1_TSCORE")) + scale_y_continuous(limits = c(-.2, .2))

plot_pac



model_parameters(m_men <- lm(
  data = dt_f,
  t1_Warm.MentalIllness_z ~ +bs(t1_TSCORE) + t0_Warm.MentalIllness_z
)) |>
  print_md(digits = 2)


plot_men <-
  plot(ggeffects::ggpredict(m_men, terms = "t1_TSCORE")) + scale_y_continuous(limits = c(-.2, .2))

plot_men



model_parameters(m_men <- lm(
  data = dt_f,
  t1_Warm.MentalIllness_z ~  +bs(t1_TSCORE) + t0_Warm.MentalIllness_z
)) |>
  print_md(digits = 2)


plot_men <-
  plot(ggeffects::ggpredict(m_men, terms = "t1_TSCORE")) + scale_y_continuous(limits = c(-.2, .2))

plot_men


model_parameters(m_ove <- lm(
  data = dt_f,
  t1_Warm.Overweight_z ~  +bs(t1_TSCORE) + t0_Warm.Overweight_z
)) |>
  print_md(digits = 2)


plot_ove <-
  plot(ggeffects::ggpredict(m_ove, terms = "t1_TSCORE")) + scale_y_continuous(limits = c(-.2, .2))

plot_ove


model_parameters(m_eld <- lm(data = dt_f,
                             t1_Warm.Elderly_z ~  +bs(t1_TSCORE) + t0_Warm.Elderly_z)) |>
  print_md(digits = 2)


plot_eld <-
  plot(ggeffects::ggpredict(m_eld, terms = "t1_TSCORE")) + scale_y_continuous(limits = c(-.2, .2))

plot_eld



graphs <- plot_mus/plot_ind/plot_chi/plot_imm/plot_asi / plot_mao/plot_pac/plot_nze/plot_eld/plot_ove/plot_men + plot_annotation(tag_levels = "a")



ggsave(
  graphs,
  path = here::here(here::here("figs")),
  width = 5,
  height = 30,
  units = "in",
  filename = "graphs.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)
