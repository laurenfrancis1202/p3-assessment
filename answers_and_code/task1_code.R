library(tidyverse)
library(openxlsx)
library(writexl)
library(readxl)
library(janitor)
library(ggplot2)

# UNICEF P3 assessment :: task 1

USERNAME    <- Sys.getenv("USERNAME")
USERPROFILE <- Sys.getenv("USERPROFILE")
wd          <- str_glue("{USERPROFILE}/Documents/Github/p3-assessment")

## input data ----
mnch <- read.csv(str_glue("{wd}/fusion_GLOBAL_DATAFLOW_UNICEF_1.0_.MNCH_ANC4+MNCH_SAB.csv"))
wpp_est <- read_excel(str_glue("{wd}/01_rawdata/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx"), sheet = "Estimates", skip = 16, col_types = c("text"))
wpp_proj <- read_excel(str_glue("{wd}/01_rawdata/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx"), sheet = "Projections", skip = 16, col_types = c("text"))
u5mr_class <- read_excel(str_glue("{wd}/01_rawdata/On-track and off-track countries.xlsx"))
## input data ----

## step 1: data preparation ----
# clean mnch data
mnch_clean <- mnch %>%
  clean_names() %>%
  filter(time_period_time_period %in% c(2018:2022),
         age_current_age == "Y15T49: 15 to 49 years old") %>%
  # extract country iso3c and indicator name (short)
  mutate(iso3c = str_extract(ref_area_geographic_area, "^[^:]+"),
         indicator = str_extract(indicator_indicator, "^[^:]+"),
         indicator = str_extract(indicator, "(?<=_).+"),
         time_period = str_glue("{min(time_period_time_period)}-{max(time_period_time_period)}")) %>%
  group_by(iso3c, indicator) %>%
  # filter for countries based on 3-letter iso3c and latest datapoint per country-indicator
  filter(str_length(iso3c) == 3,
         time_period_time_period == max(time_period_time_period)) %>%
  ungroup() %>%
  select(iso3c, 
         indicator,
         time_period,
         obs_val = obs_value_observation_value)

# clean wpp data
wpp_clean <- rbind(wpp_est, wpp_proj) %>%
  clean_names() %>%
  # filter for countries and latest year
  filter(type == "Country/Area",
         year %in% c(2022)) %>%
  select(iso3c = iso3_alpha_code,
         births_thousands) %>%
  # convert numeric character values to numeric
  mutate(births_thousands = as.numeric(births_thousands)) %>%
  filter(!is.na(births_thousands))

# clean u5mr classification data
u5mr_class_clean <- u5mr_class %>%
  select(iso3c = ISO3Code,
         u5mr_status = `Status.U5MR`) %>%
  # clean kosovo iso3c
  mutate(iso3c = ifelse(iso3c == "RKS", "XKX", iso3c))

# merge data
df <- mnch_clean %>%
  left_join(., wpp_clean) %>%
  left_join(., u5mr_class_clean) %>%
  filter(!is.na(u5mr_status))
## step 1: data preparation ----

## step 2: calculate weighted averages for on-track and off-track countries ----
df_av <- df %>%
  group_by(u5mr_status, indicator) %>%
  mutate(num = births_thousands * (obs_val/100),
         num = sum(num),
         denom = sum(births_thousands),
         n_ctry = n_distinct(iso3c)) %>%
  ungroup() %>%
  select(u5mr_status, indicator, time_period, n_ctry, num, denom) %>%
  distinct() %>%
  mutate(weighted_av = (num / denom) * 100)
## step 2: calculate weighted averages for on-track and off-track countries ----

## step 3: create a visualisation ----
df_av$u5mr_status <- factor(df_av$u5mr_status, levels = c("Acceleration Needed", "On Track", "Achieved"))

plot <-
  ggplot(df_av, aes(x = indicator, y = weighted_av, fill = u5mr_status)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5, colour = "black") +
    geom_text(
    aes(label = round(weighted_av,1), y = weighted_av + 2),
    position = position_dodge(0.5),
    vjust = 0,
    size = 3.5) +
  labs(title = str_glue("MNCH indicator coverage, by under-5 mortality rate progress classification, {unique(df_av$time_period)}"), 
       x = "", 
       y = "Coverage (%)", 
       fill = "U5MR classification",
       caption = str_glue("Source: Analysis of data from UNICEF Global databases and World Population Prospects (WPP) 2022 revision.\n
       Note: ANC4 = Antenatal care 4+ visits - percentage of women (aged 15-49 years) attended at least four times during pregnancy by any provider;
                          SAB = Skilled birth attendant - percentage of deliveries attended by skilled health personnel.
                          ")) +
  theme_minimal() +
  scale_fill_manual(name = 'U5MR classification', values = c('#E2231A','#FFC20E','#00833D'))

# see word document for summary
## step 3: create a visualisation ----

# export
write_csv(df_av, str_glue("{wd}/answers_and_code/task1_weighted_av.csv"))
ggsave(filename = str_glue("{wd}/answers_and_code/task1_plot.png"), plot = plot, width = 10, height = 7, units = "in", dpi = 300, bg = 'white')
