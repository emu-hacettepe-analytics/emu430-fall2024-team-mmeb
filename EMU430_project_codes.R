if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("broom", quietly = TRUE)) install.packages("broom")
if (!requireNamespace("plm", quietly = TRUE)) install.packages("plm")
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")
if (!requireNamespace("lmtest", quietly = TRUE)) install.packages("lmtest")
if (!requireNamespace("oaxaca", quietly = TRUE)) install.packages("oaxaca")
if (!requireNamespace("quarto", quietly = TRUE)) install.packages("quarto")
if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
if (!requireNamespace("rmarkdown", quietly = TRUE)) install.packages("rmarkdown")

library(dplyr)
library(ggplot2)
library(readxl)
library(broom)
library(plm)
library(MASS)
library(lmtest)
library(oaxaca)
library(quarto)
library(knitr)
library(rmarkdown)

setwd("C:/Users/Melik/Desktop")

file_path_csv <- "C:/Users/Melik/Desktop/Table 1.csv"

if (file.exists(file_path_csv)) {
  table1 <- read.csv(file_path_csv, header = TRUE)
  colnames(table1) <- c("year", "score", "wage_equality", "income_ppp", "managers", "technical_workers")
  print(head(table1))
  
  output_file1 <- "C:/Users/Melik/Desktop/economic_participation_plot.png"
  png(output_file1, width = 1200, height = 900, res = 300)
  g1 <- ggplot(table1, aes(x = year, y = score)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_point(color = "red", size = 2) +
    labs(
      title = "Gender Equality Index Over the Years",
      x = "Year",
      y = "Gender Equality Index"
    )
  print(g1)
  dev.off()
}

library(dplyr)
library(tidyr)
library(ggplot2)

table1 <- read.csv("C:/Users/Melik/Desktop/Table1.csv", header = TRUE)
colnames(table1) <- paste0(table1[1, ], " ", table1[2, ])
table1 <- table1[-c(1, 2), ]

table1_long <- table1 %>%
  pivot_longer(
    cols = -`Gender gap index year`,
    names_to = "category",
    values_to = "value"
  ) %>%
  rename(year = `Gender gap index year`) %>%
  mutate(
    year = as.numeric(year),
    value = as.numeric(value)
  )

ggplot(table1_long, aes(x = year, y = value, color = category, group = category)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Economic Participation and Opportunity Sub-indexes for Turkey",
    x = "Year",
    y = "Value",
    color = "Category"
  ) +
  scale_x_continuous(breaks = seq(2006, 2021, by = 3)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "gray80", size = 0.5),
    panel.grid.minor = element_line(color = "gray90", size = 0.25)
  )


library(dplyr)
library(ggplot2)
library(tidyr)

file_path <- "C:/Users/Melik/Desktop/Table 2.csv"
table2 <- read.csv(file_path, header = TRUE)

colnames(table2) <- table2[1, ]
table2 <- table2[-1, ]

colnames(table2) <- c(
  "education_level",
  "2002_male", "2002_female",
  "2008_male", "2008_female",
  "2014_male", "2014_female",
  "2019_male", "2019_female"
)

table2_long <- table2 %>%
  pivot_longer(
    cols = c("2002_male", "2002_female", "2008_male", "2008_female",
             "2014_male", "2014_female", "2019_male", "2019_female"),
    names_to = c("year", "gender"),
    names_sep = "_",
    values_to = "percentage"
  ) %>%
  mutate(
    year = as.numeric(year),
    percentage = as.numeric(percentage),
    education_level = factor(education_level)
  ) %>%
  filter(!is.na(percentage))

ggplot(table2_long, aes(x = year, y = percentage, color = gender, group = gender)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(
    ~education_level,
    scales = "free_y",
    labeller = label_wrap_gen(width = 30)
  ) +
  labs(
    title = "Educational Attainment by Gender in Turkey (2002-2019)",
    x = "Year",
    y = "Percentage (%)",
    color = "Gender"
  ) +
  scale_x_continuous(breaks = c(2002, 2005, 2008, 2011, 2014, 2019)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(10, 10, 10, 10)
  )

install.packages("readxl")
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

# Dosya Yolu
file_path <- "C:/Users/Melik/Desktop/Assessing-the-gender-wage-gap-Turkey-in-the-years-20022019 (1).xlsx"

# Veri Yükleme
data <- read_excel(file_path, sheet = "Table 3")

# Veri Tipi Dönüşümü
data <- data %>%
  mutate(across(where(is.character), as.numeric))

# Uzun Formata Dönüştürme
long_table3 <- data %>%
  pivot_longer(
    cols = -Year,
    names_to = "Category",
    values_to = "Value"
  )

# İlk Satırları Görüntüleme
head(long_table3)

# Grafik Oluşturma
ggplot(long_table3, aes(x = Year, y = Value, color = Category)) +
  geom_line(size = 1) +
  labs(
    title = "Marital Status by Gender and Age (%)",
    x = "Year",
    y = "Percentage"
  ) +
  theme_minimal()

install.packages("tidyr")
library(readxl)
file_path <- "C:/Users/Melik/Desktop/Assessing-the-gender-wage-gap-Turkey-in-the-years-20022019 (1).xlsx"
data_clean <- read_excel(file_path, sheet = "Table44")
colnames(data)
head(data)
library(ggplot2)
library(dplyr)
library(tidyr)
packageVersion("tidyr")
str(data_clean)
if (!all(c("Male_Mean", "Female_Mean") %in% colnames(data_clean))) {
  stop("Required columns 'Male_Mean' and 'Female_Mean' are missing in data_clean.")
}
data_clean <- data_clean %>%
  mutate(
    Log_Male_Mean = log(Male_Mean),
    Log_Female_Mean = log(Female_Mean)
  )
head(data_clean)
data_long <- data_clean %>%
  pivot_longer(
    cols = c(Log_Male_Mean, Log_Female_Mean),
    names_to = "Gender",
    values_to = "Log_Wage"
  ) %>%
  mutate(Gender = ifelse(Gender == "Log_Male_Mean", "Male", "Female"))
head(data_long)
ggplot(data_long, aes(x = Gender, y = Log_Wage, fill = Gender)) + 
  geom_boxplot() +
  labs(
    title = "Summary Statistics for Hourly Wages by Gender in Logarithmic Terms",
    x = "Gender",
    y = "Logarithmic Hourly Wage"
  ) +
  theme_minimal()

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the Excel file
file_path <- "C:/Users/Melik/Desktop/Assessing-the-gender-wage-gap-Turkey-in-the-years-20022019.xlsx"
table_5 <- read_excel(file_path, sheet = "Table 5", skip = 3)

# Ensure column names are correctly assigned
colnames(table_5) <- c(
  "Year", "Male_Age_Mean", "Male_Age_StdDev", "Female_Age_Mean", "Female_Age_StdDev",
  "Male_Education_Mean", "Male_Education_StdDev", "Female_Education_Mean", "Female_Education_StdDev",
  "Male_Tenure_Mean", "Male_Tenure_StdDev", "Female_Tenure_Mean", "Female_Tenure_StdDev",
  "Male_Marital_Mean", "Male_Marital_StdDev", "Female_Marital_Mean", "Female_Marital_StdDev"
)

# Transform data to long format
table_5_long <- table_5 %>%
  pivot_longer(cols = -Year, names_to = c("Gender", "Category", "Stat"), names_sep = "_") %>%
  pivot_wider(names_from = Stat, values_from = value)

# Split data by category
age_data <- table_5_long %>% filter(Category == "Age")
education_data <- table_5_long %>% filter(Category == "Education")
tenure_data <- table_5_long %>% filter(Category == "Tenure")
marital_data <- table_5_long %>% filter(Category == "Marital")

# Visualization
p1 <- ggplot(age_data, aes(x = Year, y = Mean, color = Gender)) +
  geom_line(size = 1) + geom_point(size = 2) +
  labs(title = "Mean Age by Gender Over Time", x = "Year", y = "Mean Age") +
  scale_color_manual(name = "Gender", values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal()

p2 <- ggplot(education_data, aes(x = Year, y = Mean, color = Gender)) +
  geom_line(size = 1) + geom_point(size = 2) +
  labs(title = "Mean Education by Gender Over Time", x = "Year", y = "Mean Education (Years)") +
  scale_color_manual(name = "Gender", values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal()

p3 <- ggplot(tenure_data, aes(x = Year, y = Mean, color = Gender)) +
  geom_line(size = 1) + geom_point(size = 2) +
  labs(title = "Mean Tenure by Gender Over Time", x = "Year", y = "Mean Tenure (Years)") +
  scale_color_manual(name = "Gender", values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal()

p4 <- ggplot(marital_data, aes(x = Year, y = Mean, color = Gender)) +
  geom_line(size = 1) + geom_point(size = 2) +
  labs(title = "Marital Status by Gender Over Time", x = "Year", y = "Mean Marital Status (0 = Single, 1 = Married)") +
  scale_color_manual(name = "Gender", values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal()

# Combine plots into one
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2)


library(tidyverse)
library(readxl)
library(ggthemes)

file_path <- "C:/Users/Melik/Desktop/Assessing-the-gender-wage-gap-Turkey-in-the-years-20022019.xlsx"
sheet_name <- "Table 6"

data <- read_excel(file_path, sheet = sheet_name, col_names = TRUE)


colnames(data) <- c("Year", "Difference", "Endowments_Effect", "Discrimination_Effect", "Selection_Effect")


data <- data |> 
  mutate(across(everything(), ~ str_replace_all(as.character(.), ",", "."))) |> 
  mutate(across(-Year, as.numeric), Year = as.numeric(Year))


data_long <- data |> 
  pivot_longer(
    cols = -Year,
    names_to = "Component",
    values_to = "Value"
  )


data_long |> 
  ggplot(aes(x = Year, y = Value, color = Component)) +  
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Decomposition of Gender Wage Gap Over Years (2002–2019)",
    x = "Year",
    y = "Log Point Value",
    color = "Component"
  ) +
  theme_fivethirtyeight() +  
  theme(
    text = element_text(size = 14, family = "Comic Sans MS"), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom"
  )

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

file_path <- "C:/Users/Melik/Desktop/Assessing-the-gender-wage-gap-Turkey-in-the-years-20022019 (1).xlsx"
data <- read_excel(file_path, sheet = "Table 7")

colnames(data) <- c("Variables", paste(rep(2002:2019, each = 2), c("Male", "Female"), sep = "_"))

data_clean <- data %>%
  filter(Variables != "Number of obs.") %>%
  pivot_longer(
    cols = starts_with("200"),
    names_to = c("Year", "Gender"),
    names_sep = "_"
  ) %>%
  mutate(
    Year = as.numeric(Year),
    Value = as.numeric(str_extract(value, "-?[0-9]+\\.?[0-9]*")),
    Meta = str_extract(value, "\\(.*\\)"),
    Stars = str_extract(value, "\\*+")
  ) %>%
  complete(Year = seq(2002, 2019, 1), Variables, Gender, fill = list(Value = NA)) %>%
  filter(!is.na(Value) & !is.na(Variables))

data_clean$Variables <- as.factor(data_clean$Variables)

ggplot(data_clean, aes(x = Year, y = Value, color = Gender, group = Gender)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ Variables, scales = "free_y") +
  scale_x_continuous(breaks = seq(2002, 2019, 5)) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Results of Heckman’s Two-Step Procedure Predictions (in Log Points)",
    x = "Year",
    y = "Predicted Values (Log Points)"
  )