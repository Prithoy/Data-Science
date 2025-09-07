install.packages("readxl")
install.packages("zoo")
install.packages("modeest")
install.packages("ROSE")

install.packages("caTools")

install.packages("dplyr")
install.packages("ggplot2")

library(readxl)
library(zoo)


library(modeest)
library(ROSE)
library(caTools)
library(dplyr)
library(ggplot2)

dataset_df <- read_excel("D:/Study Metarials/Projects/Midterm_Dataset_Section(E).xlsx")
dataset_df <- data.frame(dataset_df)
View(dataset_df)
summary(dataset_df)

dataset_df$Have.you.ever.had.suicidal.thoughts.. <- gsub("Yess", "Yes", dataset_df$Have.you.ever.had.suicidal.thoughts.., ignore.case = TRUE)
head(dataset_df, 20)

missing_counts <- colSums(is.na(dataset_df))
missing_counts
barplot(missing_counts, main = "Missing Values Count per Column", xlab = "Columns", ylab = "Missing Values", col = "skyblue", las = 2)

mean_age <- mean(dataset_df$Age, na.rm = TRUE)
dataset_df$Age[is.na(dataset_df$Age)] <- ceiling(mean_age)
dataset_df$Age
summary(dataset_df$Age)

mode_gender <- as.character(mlv(dataset_df$Gender, method = "mfv"))
dataset_df$Gender[is.na(dataset_df$Gender)] <- mode_gender
dataset_df$Gender
dataset_df$Gender <- gsub("Feemale", "Female", dataset_df$Gender, ignore.case = TRUE)
dataset_df$Gender <- gsub("Maleee", "Male", dataset_df$Gender, ignore.case = TRUE)
table(dataset_df$Gender)

median_study_hours <- median(dataset_df$Study.Hours, na.rm = TRUE)
dataset_df$Study.Hours[is.na(dataset_df$Study.Hours)] <- median_study_hours
dataset_df$Study.Hours
summary(dataset_df$Study.Hours)

dataset_df$Sleep.Duration <- as.character(dataset_df$Sleep.Duration)
dataset_df$Sleep.Duration <- trimws(dataset_df$Sleep.Duration)
dataset_df$Sleep.Duration[dataset_df$Sleep.Duration == ""] <- NA
mode_sleep <- names(sort(table(dataset_df$Sleep.Duration), decreasing = TRUE))[1]

dataset_df$Sleep.Duration[is.na(dataset_df$Sleep.Duration)] <- mode_sleep
dataset_df$Sleep.Duration <- as.factor(dataset_df$Sleep.Duration)
table(dataset_df$Sleep.Duration, useNA = "ifany")

mode_depression <- as.character(mlv(dataset_df$Depression, method = "mfv"))
dataset_df$Depression[is.na(dataset_df$Depression)] <- mode_depression
dataset_df$Depression
table(dataset_df$Depression)

mode_study_satisfaction <- as.numeric(mlv(dataset_df$Study.Satisfaction, method = "mfv"))
dataset_df$Study.Satisfaction[is.na(dataset_df$Study.Satisfaction)] <- mode_study_satisfaction
summary(dataset_df$Study.Satisfaction)

missing_counts <- colSums(is.na(dataset_df))
missing_counts
barplot(missing_counts, main = "Missing Values Count per Column", xlab = "Columns", ylab = "Missing Values", col = "skyblue", las = 2)

dataset_df$Gender <- factor(dataset_df$Gender, levels = c("Male", "Female"))
dataset_df$Dietary.Habits <- factor(dataset_df$Dietary.Habits, levels = c("Healthy", "Moderate", "Unhealthy"))
dataset_df$Have.you.ever.had.suicidal.thoughts.. <- factor(dataset_df$Have.you.ever.had.suicidal.thoughts.., levels = c("No", "Yes"))
dataset_df$Family.History.of.Mental.Illness <- factor(dataset_df$Family.History.of.Mental.Illness, levels = c("No", "Yes"))
dataset_df$Depression <- factor(dataset_df$Depression, levels = c("No", "Yes"))
dataset_df$Sleep.Duration <- factor(dataset_df$Sleep.Duration, levels = c("Less than 5 hours", "5-6 hours", "7-8 hours", "More than 8 hours"))

sleep_map <- c("Less than 5 hours" = 4, "5-6 hours" = 5.5, "7-8 hours" = 7.5, "More than 8 hours" = 9)

dataset_df$Sleep_Duration_num <- sleep_map[as.character(dataset_df$Sleep.Duration)]
head(dataset_df[, c("Sleep.Duration", "Sleep_Duration_num")])

handle_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  median_val <- median(x, na.rm = TRUE)
  x[x < lower_bound | x > upper_bound] <- median_val
  return(x)
}

dataset_df$Age <- handle_outliers(dataset_df$Age)
dataset_df$Study.Hours <- handle_outliers(dataset_df$Study.Hours)

ggplot(dataset_df, aes(y = Age)) + geom_boxplot(fill = "skyblue") + ggtitle("Boxplot of Age Before Outlier Handling")
ggplot(dataset_df, aes(y = Study.Hours)) + geom_boxplot(fill = "lightgreen") + ggtitle("Boxplot of Study Hours Before Outlier Handling")

normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

dataset_df$Age_norm <- normalize(dataset_df$Age)
dataset_df$Study_Hours_norm <- normalize(dataset_df$Study.Hours)
head(dataset_df[, c("Age", "Age_norm", "Study.Hours", "Study_Hours_norm")])

duplicate_count <- sum(duplicated(dataset_df))
duplicate_count
dataset_df <- dataset_df[!duplicated(dataset_df), ]

majority <- filter(dataset_df, Depression == "No")
minority <- filter(dataset_df, Depression == "Yes")
set.seed(123)
oversampled_minority <- minority %>% sample_n(nrow(majority), replace = TRUE)
balanced_data <- bind_rows(majority, oversampled_minority)
table(balanced_data$Depression)

high_study_students <- balanced_data %>% filter(Study.Hours > 6)
female_depressed_students <- balanced_data %>% filter(Gender == "Female", Depression == "Yes")

set.seed(123)
split <- sample.split(balanced_data$Depression, SplitRatio = 0.8)
train_data <- subset(balanced_data, split == TRUE)
test_data <- subset(balanced_data, split == FALSE)

study_hours_stats <- balanced_data %>% group_by(Depression) %>%
  summarise(mean_study_hours = mean(Study.Hours, na.rm = TRUE),
            median_study_hours = median(Study.Hours, na.rm = TRUE),
            sd_study_hours = sd(Study.Hours, na.rm = TRUE))
study_hours_stats

age_stats <- balanced_data %>% group_by(Depression) %>%
  summarise(mean_age = mean(Age, na.rm = TRUE),
            median_age = median(Age, na.rm = TRUE),
            sd_age = sd(Age, na.rm = TRUE))
age_stats

sleep_comparison <- balanced_data %>% group_by(Depression) %>%
  summarise(avg_sleep_duration = mean(Sleep_Duration_num, na.rm = TRUE))
sleep_comparison

study_spread <- balanced_data %>% group_by(Study.Satisfaction) %>%
  summarise(sd_study_hours = sd(Study.Hours, na.rm = TRUE))
study_spread

ggplot(balanced_data, aes(x = Depression, y = Study.Hours)) + geom_boxplot() + ggtitle("Study Hours by Depression")
ggplot(balanced_data, aes(x = Depression, y = Age)) + geom_boxplot() + ggtitle("Age by Depression")
ggplot(balanced_data, aes(x = factor(Study.Satisfaction), y = Study.Hours)) + geom_boxplot() + ggtitle("Study Hours by Study Satisfaction")
View(dataset_df)

