library(readxl)
library(dplyr)
library(table1)
library(tidyr)

source("helper.R")

## 1. Preprocessing dataset  -----
data <- read_excel("data2.xlsx", sheet = "data")

data_clean <- data %>%
  mutate(
    Age = USIA,
    Gender = factor(
      ifelse(GENDER_CAT == 1, "Male", "Female"),
      levels = c("Female", "Male")
    ),
    Weight = BB,
    Height = TB,
    Activity = dplyr::case_when(
      ACTIVITY_CAT == 1 ~ "High",
      ACTIVITY_CAT == 2 ~ "Medium",
      ACTIVITY_CAT == 3 ~ "Low",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("High", "Medium", "Low")),
    `Food_Intake` = dplyr::case_when(
      ENERGY_CAT == 0 ~ "Normal",
      ENERGY_CAT == 1 ~ "Excess",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Normal", "Excess")),
    Obesity =  dplyr::case_when(
      BMI_CAT == 0 ~ "Non-Obese",
      BMI_CAT == 1 ~ "Obese",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Non-Obese", "Obese")),
    Central_Obesity =  dplyr::case_when(
      WC_CAT == 0 ~ "Normal",
      WC_CAT == 1 ~ "Central Obesity",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Normal", "Central Obesity")),
    Codominant = GENOTYPE %>%
      factor(levels = c("TT", "AT", "AA")),
    Dominant = case_when(
      GENOTYPE == "TT" ~ "TT",
      TRUE ~ "AT + AA"
    ) %>% factor(levels = c ("TT", "AT + AA")),
    Recessive = case_when(
      GENOTYPE == "AA" ~ "AA",
      TRUE ~ "AT + TT"
    ) %>% factor(levels = c ("AT + TT", "AA"))
  ) %>%
  select(Age, Gender, Weight, Height, Activity, `Food_Intake`, Codominant, Dominant, Recessive, Obesity, Central_Obesity)

model.mult_codominant1 <- glm(Central_Obesity ~ Age + Gender + Activity + Food_Intake + Codominant, data = data_clean, family = binomial)
model.mult_dominant1 <- glm(Central_Obesity ~ Age + Gender + Activity + Food_Intake + Dominant, data = data_clean, family = binomial)
model.mult_recessive1 <- glm(Central_Obesity ~ Age + Gender + Activity + Food_Intake + Recessive, data = data_clean, family = binomial)

model.mult_codominant2 <- glm(Obesity ~ Age + Gender + Activity + Food_Intake + Codominant, data = data_clean, family = binomial)
model.mult_dominant2 <- glm(Obesity ~ Age + Gender + Activity + Food_Intake + Dominant, data = data_clean, family = binomial)
model.mult_recessive2 <- glm(Obesity ~ Age + Gender + Activity + Food_Intake + Recessive, data = data_clean, family = binomial)

model_list <- list()

model_list[["Central_Obesity"]][["Codominant"]] <- model.mult_codominant1
model_list[["Central_Obesity"]][["Dominant"]] <- model.mult_dominant1
model_list[["Central_Obesity"]][["Recessive"]] <- model.mult_recessive1

model_list[["Obesity"]][["Codominant"]] <- model.mult_codominant2
model_list[["Obesity"]][["Dominant"]] <- model.mult_dominant2
model_list[["Obesity"]][["Recessive"]] <- model.mult_recessive2

## 2. Generating Stats Analysis Results -----
codominant.central_obesity <- stat_analysis(data = data_clean, output = "Central_Obesity", model = "Codominant")
dominant.central_obesity <- stat_analysis(data = data_clean, output = "Central_Obesity", model = "Dominant")
recessive.central_obesity <- stat_analysis(data = data_clean, output = "Central_Obesity", model = "Recessive")

codominant.obesity <- stat_analysis(data = data_clean, output = "Obesity", model = "Codominant")
dominant.obesity <- stat_analysis(data = data_clean, output = "Obesity", model = "Dominant")
recessive.cobesity <- stat_analysis(data = data_clean, output = "Obesity", model = "Recessive")

## 3. Generating Plots -----
## Obesity
# Generate the plot and assign it to an object

# Save the plot
p1 <- generate_plot(data = data_clean, stats = "adjusted", model = "Codominant", output = "Obesity")
p2 <- generate_plot(data = data_clean, stats = "adjusted", model = "Dominant", output = "Obesity")
p3 <- generate_plot(data = data_clean, stats = "adjusted", model = "Recessive", output = "Obesity")
p4 <- generate_plot(data = data_clean, stats = "unadjusted", model = "Codominant", output = "Obesity")
p5 <- generate_plot(data = data_clean, stats = "unadjusted", model = "Dominant", output = "Obesity")
p6 <- generate_plot(data = data_clean, stats = "unadjusted", model = "Recessive", output = "Obesity")

ggsave(filename = "plot_obesity_adjusted_codominant.png", plot = p1, width = 12, height = 4, dpi = 300)
ggsave(filename = "plot_obesity_adjusted_dominant.png", plot = p2, width = 12, height = 4, dpi = 300)
ggsave(filename = "plot_obesity_adjusted_recessive.png", plot = p3, width = 12, height = 4, dpi = 300)
ggsave(filename = "plot_obesity_unadjusted_codominant.png", plot = p4, width = 12, height = 4, dpi = 300)
ggsave(filename = "plot_obesity_unadjusted_dominant.png", plot = p5, width = 12, height = 4, dpi = 300)
ggsave(filename = "plot_obesity_unadjusted_recessive.png", plot = p6, width = 12, height = 4, dpi = 300)

## Central Obesity
p7 <- generate_plot(data = data_clean, stats = "adjusted", model = "Codominant", output = "Central_Obesity")
p8 <- generate_plot(data = data_clean, stats = "adjusted", model = "Dominant", output = "Central_Obesity")
p9 <- generate_plot(data = data_clean, stats = "adjusted", model = "Recessive", output = "Central_Obesity")
p10 <- generate_plot(data = data_clean, stats = "unadjusted", model = "Codominant", output = "Central_Obesity")
p11 <- generate_plot(data = data_clean, stats = "unadjusted", model = "Dominant", output = "Central_Obesity")
p12 <- generate_plot(data = data_clean, stats = "unadjusted", model = "Recessive", output = "Central_Obesity")

ggsave(filename = "plot_central_obesity_adjusted_codominant.png", plot = p7, width = 12, height = 4, dpi = 300)
ggsave(filename = "plot_central_obesity_adjusted_dominant.png", plot = p8, width = 12, height = 4, dpi = 300)
ggsave(filename = "plot_central_obesity_adjusted_recessive.png", plot = p9, width = 12, height = 4, dpi = 300)
ggsave(filename = "plot_central_obesity_unadjusted_codominant.png", plot = p10, width = 12, height = 4, dpi = 300)
ggsave(filename = "plot_central_obesity_unadjusted_dominant.png", plot = p11, width = 12, height = 4, dpi = 300)
ggsave(filename = "plot_central_obesity_unadjusted_recessive.png", plot = p12, width = 12, height = 4, dpi = 300)
