# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(fuzzyjoin)
library(tigris)
library(sf)
library(stringdist)
library(purrr)
library(ggplot2)
library(readr)
library(geosphere)



# File paths
path_2024 <- "RawData/2023-2024_School_Vaccination_Coverage_Levels_Kindergarten.xlsx"
path_2023 <- "RawData/22-23-School-Vaccination-Coverage-by-District-and-County-K.xlsx"
path_2022 <- "RawData/2021-2022-School-Vaccination-Coverage-by-District-and-County-Kindergarten.xlsx"
path_2021 <- "RawData/2020-2021-School-Vaccination-Coverage-Levels-by-District-Private-School-and-County---Kindergarten.xlsx"
path_2020 <- "RawData/2019-2020-School-Vaccination-Coverage-Levels---Kindergarten.xlsx"

# Read sheets
sheet_2024 <- excel_sheets(path_2024)[1]
sheet_2023 <- excel_sheets(path_2023)[1]
sheet_2022 <- excel_sheets(path_2022)[1]
sheet_2021 <- excel_sheets(path_2021)[1]
sheet_2020 <- excel_sheets(path_2020)[1]


# Read and label each year
vaccine_2024 <- read_excel(path_2024, sheet = sheet_2024, skip = 2) %>%
  rename(
    DTP = "DTP/DTaP/DT/Td",
    school_type = "School Type",
    District = "Facility Name",
    Hepatitis_A = "Hepatitis A",
    Hepatitis_B = "Hepatitis B"
  ) %>%
  mutate(
    Year = 2024,
    DTP = as.numeric(DTP),
    Hepatitis_A = as.numeric(Hepatitis_A),
    Hepatitis_B = as.numeric(Hepatitis_B),
    MMR = as.numeric(MMR),
    Polio = as.numeric(Polio),
    Varicella = as.numeric(Varicella)
  ) %>%
  dplyr::filter(
    school_type == "Public ISD"
  ) %>%
  select(
    District,
    County,
    Year,
    DTP,
    Hepatitis_A,
    Hepatitis_B,
    MMR,
    Polio,
    Varicella
  )


vaccine_2023 <- read_excel(path_2023, sheet = sheet_2023, skip = 2)  %>%
  rename(
    DTP = "DTP/DTaP/DT/Td",
    school_type = "School Type",
    District = "Facility Name",
    Hepatitis_A = "Hepatitis A",
    Hepatitis_B = "Hepatitis B"
  ) %>%
  mutate(
    Year = 2023,
    DTP = as.numeric(DTP),
    Hepatitis_A = as.numeric(Hepatitis_A),
    Hepatitis_B = as.numeric(Hepatitis_B),
    MMR = as.numeric(MMR),
    Polio = as.numeric(Polio),
    Varicella = as.numeric(Varicella)
  ) %>%
  dplyr::filter(
    school_type == "Public School"
  ) %>%
  select(
    District,
    County,
    Year,
    DTP,
    Hepatitis_A,
    Hepatitis_B,
    MMR,
    Polio,
    Varicella
  )



vaccine_2022 <- read_excel(path_2022, sheet = sheet_2022, skip = 2)  %>%
  rename(
    DTP = "DTP/DTaP/DT/Td",
    school_type = "School Type",
    District = "Facility Name",
    Hepatitis_A = "Hepatitis A",
    Hepatitis_B = "Hepatitis B"
  ) %>%
  mutate(
    Year = 2022,
    DTP = as.numeric(DTP),
    Hepatitis_A = as.numeric(Hepatitis_A),
    Hepatitis_B = as.numeric(Hepatitis_B),
    MMR = as.numeric(MMR),
    Polio = as.numeric(Polio),
    Varicella = as.numeric(Varicella)
  ) %>%
  dplyr::filter(
    school_type == "Public School"
  ) %>%
  select(
    District,
    County,
    Year,
    DTP,
    Hepatitis_A,
    Hepatitis_B,
    MMR,
    Polio,
    Varicella
  )



vaccine_2021 <- read_excel(path_2021, sheet = sheet_2021, skip = 2)  %>%
  rename(
    DTP = "DTP/DTaP/DT/Td",
    school_type = "School Type",
    District = "Facility Name",
    Hepatitis_A = "Hepatitis A",
    Hepatitis_B = "Hepatitis B"
    ) %>%
  mutate(
    Year = 2021,
    DTP = as.numeric(DTP),
    Hepatitis_A = as.numeric(Hepatitis_A),
    Hepatitis_B = as.numeric(Hepatitis_B),
    MMR = as.numeric(MMR),
    Polio = as.numeric(Polio),
    Varicella = as.numeric(Varicella)
  ) %>%
  dplyr::filter(
    school_type == "Public School"
  ) %>%
  select(
    District,
    County,
    Year,
    DTP,
    Hepatitis_A,
    Hepatitis_B,
    MMR,
    Polio,
    Varicella
  )



vaccine_2020 <- read_excel(path_2020, sheet = sheet_2020, skip = 2)  %>%
  rename(
    DTP = "DTP/DTaP/DT/Td",
    school_type = "School Type",
    District = "Facility Name",
    Hepatitis_A = "Hepatitis A",
    Hepatitis_B = "Hepatitis B"
  ) %>%
  mutate(
    Year = 2020,
    DTP = as.numeric(DTP),
    Hepatitis_A = as.numeric(Hepatitis_A),
    Hepatitis_B = as.numeric(Hepatitis_B),
    MMR = as.numeric(MMR),
    Polio = as.numeric(Polio),
    Varicella = as.numeric(Varicella)
  ) %>%
  dplyr::filter(
    school_type == "Public ISD"
  ) %>%
  select(
    District,
    County,
    Year,
    DTP,
    Hepatitis_A,
    Hepatitis_B,
    MMR,
    Polio,
    Varicella
  )


# Bind all into one dataframe
vaccine_all <- bind_rows(vaccine_2024, vaccine_2023, vaccine_2022, vaccine_2021, vaccine_2020)

# Define suboptimal thresholds
thresholds <- c(
  Polio = .85,
  MMR = .95,
  Varicella = .90,
  DTP = .95,
  Hepatitis_A = .90,
  Hepatitis_B = .95
)

# Pivot to long format
vaccine_long <- vaccine_all %>%
  pivot_longer(
    cols = names(thresholds),
    names_to = "Vaccine", 
    values_to = "Coverage"
    ) %>%
  filter(
    !is.na(Coverage)
    ) %>%
  mutate(
    Suboptimal = Coverage < thresholds[Vaccine]
  )



# Count number of districts below suboptimal for each vaccine/year
below_counts <- vaccine_long %>%
  filter(
    Suboptimal == 1
    ) %>%
  group_by(
    Year, 
    Vaccine
    ) %>%
  summarise(
    Num_Districts = n(),
    .groups = "drop"
    )

# Plot
ggplot(
  below_counts,
  aes(x = factor(Year), y = Num_Districts, fill = Vaccine)
  ) +
  geom_bar(
    stat = "identity", 
    position = "dodge"
    ) +
  labs(
    title = "Number of Districts Below Suboptimal Vaccine Coverage by Year",
    x = "Year",
    y = "Number of Districts"
  ) +
  theme_minimal() +
  scale_fill_brewer(
    palette = "Set2"
    )












