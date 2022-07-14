# description religion 

# read data
df <- readRDS(here::here("data_raw", "df.Rds"))
# read files
source(here::here("scripts", "funs.R"))

# read libraries in
source(here::here("scripts", "libs.R"))

# facets ------------------------------------------------------------------
library(lcsm)

churchwide <-  df %>%
  dplyr::filter(YearMeasured == 1) %>%
  dplyr::filter(!is.na(Religion.Church)) %>%
  dplyr::select(Id, Religion.Church, Wave) %>%
  group_by(Id) %>% filter(n() > 9) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Religion.Church)

churchwide <- churchwide[complete.cases(churchwide),]
dim(churchwide)
Relidwide
x_var_list <- names(churchwide[, 2:11])


plot_trajectories(
  data = churchwide,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Church Attendance",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.086,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)

dev.off()


# facet religious ---------------------------------------------------------

Relid <-  df %>%
  dplyr::filter(YearMeasured == 1) %>%
  dplyr::filter(!is.na(Relid)) %>%
  dplyr::select(Id, Relid, Wave) %>%
  group_by(Id) %>% filter(n() > 9) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Relid)

Relid <- Relid[complete.cases(Relid),]
dim(Relid)
Relid
x_var_list <- names(Relid[, 2:11])


plot_trajectories(
  data = Relid,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Religious Identification",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.086,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)

dev.off()







# facet religious ---------------------------------------------------------

Relid <-  df %>%
  dplyr::filter(YearMeasured == 1) %>%
  dplyr::filter(!is.na(Relid)) %>%
  dplyr::select(Id, Relid, Wave) %>%
  group_by(Id) %>% filter(n() > 9) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Relid)

Relid <- Relid[complete.cases(Relid),]
dim(Relid)
Relid
x_var_list <- names(Relid[, 2:11])


plot_trajectories(
  data = Relid,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Religious Identification",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.086,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)

dev.off()



# facet God ---------------------------------------------------------------
library(dplyr)
Believe.God <-  df %>%
  dplyr::filter(
    YearMeasured == 1 & Wave %in%
      c("2010",
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
  droplevels() %>%
  dplyr::select(Id, Believe.God, Wave) %>%
  group_by(Id) %>% filter(n() > 5) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Believe.God)

Believe.God <- Believe.God[complete.cases(Believe.God),]
dim(Believe.God)
Believe.God
x_var_list <- names(Believe.God[, 2:11])


plot_trajectories(
  data = Relid,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Religious Identification",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.086,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)

dev.off()

