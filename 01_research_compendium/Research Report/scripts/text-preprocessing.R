# Load packages
library(readxl)
library(dplyr)
library(stringr)
library(textclean)
library(stringdist)
library(stopwords)
library(textstem)
library(tidyr)
library(purrr)
library(ggplot2)

# Load data
exam_data <- read_xlsx("../data/data.xlsx")
set.seed(123)

## ============
## Integrate char length to the data
## ============
exam_data <- exam_data %>%
  mutate(char_len_raw = nchar(answer))

# Compute character length (no preprocessing yet)
exam_item_length_raw <- exam_data %>%
  group_by(question) %>%
  summarise(
    median_char_length = median(char_len_raw, na.rm = TRUE),
    n_responses = n(),
    .groups = "drop"
  )

# Normalize character length, within subject
exam_item_length_raw <- exam_item_length_raw %>%
  mutate(
    length_norm = (median_char_length - min(median_char_length)) /
      (max(median_char_length) - min(median_char_length) + 1e-9)
  )

# Create item groups
# ntile() splits into bins
exam_item_length_raw <- exam_item_length_raw %>%
  mutate(item_length_group = ntile(length_norm, 3))

# Merge back into dataset
exam_data <- exam_data %>%
  left_join(exam_item_length_raw,
            by = "question")

## ============
## Outlier Check
## ============
raw_length_stats <- exam_data %>%
  group_by(item_length_group) %>%
  summarise(
    upper = quantile(median_char_length, 0.95, na.rm = TRUE),
    lower = quantile(median_char_length, 0.05, na.rm = TRUE),
    .groups = "drop"  )

# Add back into data to flag outliers
exam_data <- exam_data %>%
  left_join(raw_length_stats, by = "item_length_group") %>%
  mutate(
    outlier_raw = char_len_raw < lower | char_len_raw > upper
  )

outlier_summary_raw <- exam_data %>%
  group_by(item_length_group) %>%
  summarise(
    n = n(),
    n_outliers = sum(outlier_raw, na.rm = TRUE),
    prop_outliers = n_outliers / n
  )

## ============
## Pre-process
## ============
# Apply sequential preprocessing steps
exam_processed <- exam_data %>%
  mutate(
    # Stage 1: decapitalization
    response_1 = str_to_lower(answer),
    ed_1 = stringdist(answer, response_1, method = "lv"),
    
    # Stage 2: remove punctuation
    response_2 = str_remove_all(response_1, "[[:punct:]]"),
    ed_2 = stringdist(response_1, response_2, method = "lv"),
    
    # Stage 3: Lemmatization (Dutch)
    response_3 = textstem::lemmatize_strings(response_2, language = "nl"),
    ed_3 = map2_int(
      str_split(response_2, "\\s+"),
      str_split(response_3, "\\s+"),
      ~ stringdist::stringdist(
        paste(.x, collapse = " "),
        paste(.y, collapse = " "),
        method = "lv",
        q = 1  # ensures token-level distance based on words
      )),
    
    # Stage 4: remove Dutch stopwords
    response_4 = str_squish(
      str_remove_all(response_3,
                     paste0("\\b(", paste(stopwords("nl"), collapse = "|"), ")\\b"))
    ),
    wc_4 = str_count(response_3, "\\S+") - str_count(response_4, "\\S+")
  )

## ============
## Char length and Outlier Check again
## ============
# Compute character length
exam_processed <- exam_processed %>%
  mutate(char_len_2 = nchar(response_4))

# Compute character length (no preprocessing yet)
exam_item_length_2 <- exam_processed %>%
  group_by(question) %>%
  summarise(
    median_char_length2 = median(char_len_raw, na.rm = TRUE),
    n_responses = n(),
    .groups = "drop"
  )

# Normalize character length, within subject
exam_item_length_2 <- exam_item_length_2 %>%
  group_by(question) %>%                  # normalization per subject
  mutate(
    length_norm2 = (median_char_length2 - min(median_char_length2)) /
      (max(median_char_length2) - min(median_char_length2) + 1e-9)
  ) %>%
  ungroup()

# Create item groups
# ntile() splits into bins
exam_item_length_2 <- exam_item_length_2 %>%
  mutate(item_length_group2 = ntile(length_norm2, 3))

# Merge back into dataset
exam_processed <- exam_processed %>%
  left_join(exam_item_length_2,
            by = "question")

processed_length_stats <- exam_processed %>%
  group_by(item_length_group2) %>%
  summarise(
    lower2 = quantile(char_len_2, 0.05, na.rm = TRUE),
    upper2 = quantile(char_len_2, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

exam_processed <- exam_processed %>%
  left_join(processed_length_stats, by = "item_length_group2") %>%
  mutate(
    outlier_processed = char_len_2 < lower2 |
      char_len_2 > upper2
  )

outlier_summary_processed <- exam_processed %>%
  group_by(item_length_group2) %>%
  summarise(
    n = n(),
    n_outliers = sum(outlier_processed, na.rm = TRUE),
    prop_outliers = n_outliers / n,
    .groups = "drop"
  ) %>%
  mutate(
    prop_outliers = round(prop_outliers, 4)
  )

## ============
## Distributions
## ============
exam_compare <- exam_processed %>%
  select(question,
         item_length_group2,      # use the same grouping for both
         char_len_raw, 
         char_len_2) %>%
  pivot_longer(cols = c(char_len_raw, char_len_2),
               names_to = "stage",
               values_to = "char_len")

ggplot(exam_compare, aes(x = char_len, fill = stage)) +
  geom_density(alpha = 0.4) +
  labs(title = "Character Length Distribution by Item Length Group")

## ============
## Score Variability
## ============
exam_cluster <- exam_processed %>%
  # reshape data for clustering
  pivot_longer(
    cols = starts_with("response_"),
    names_to = "stage",
    values_to = "response"
  ) %>%
  mutate(
    stage = gsub("_", " ", stage), # replace _ with space
    stage = tools::toTitleCase(stage) # capitalize
  )%>%
  group_by(question, stage, item_length_group2, response) %>%
  summarize(
    mean_score = mean(score, na.rm = TRUE),
    sd_score = sd(score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n > 1)

plot1 <- ggplot(exam_cluster, aes(x = stage, y = sd_score)) +
  geom_violin(fill = "gray80", color = "gray50") +
  geom_boxplot(width = 0.1, outlier.size = 0.5, color = "black") +
  facet_wrap(~ item_length_group2, scales = "free_y") +
  stat_summary(fun = mean, geom = "point", shape = 20, color = "red", size = 2) +
  labs(
    title = "Within-Response Score Variability per Preprocessing Stage",
    x = "Stage",
    y = "Score SD"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

high_var_responses <- exam_cluster %>%
  filter(sd_score > 0.5)