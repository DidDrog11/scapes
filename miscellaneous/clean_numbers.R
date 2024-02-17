library(dplyr)
library(readxl)
library(here)
library(stringr)
library(english)

df <- read_xlsx(here("data", "additional", "20240116_IAI.xlsx"), sheet = 1) %>%
  select(`_index`, income = "On average, how much money do you make from bushmeat in a month?")

# Remove the word Naira or # sign and trim all beginning or end whitespaces
df <- df %>%
  mutate(income = str_trim(str_remove(df$income, "^#|naira|Naira")))

# Extract numbers
numbers <- str_extract_all(df$income, "-?\\d{1,3}(,\\d{3})*(\\.\\d+)?k?", simplify = TRUE)
  
extracted_numbers <- tibble(numbers = ifelse(numbers[, 2] == "", numbers[, 1], paste(numbers[, 1], numbers[, 2], sep = ""))) %>%
  mutate(confidence = case_when(str_detect(numbers, ",") ~ "Removed comma",
                                str_detect(numbers, "k") ~ "Replaced k - 000",
                                TRUE ~ NA)) %>%
  mutate(temp_1 = str_remove_all(numbers, ","),
         temp_2 = str_replace(numbers, "k", "000"),
         numbers = coalesce(as.numeric(temp_1), as.numeric(temp_2))) %>%
  select(numbers, confidence) %>%
  bind_cols(df, .)

# Extract words
# Sort number_words by length in descending order
# Range can be changed
number_words = as.character(english(1:250, UK = TRUE))
sorted_number_words <- number_words[order(nchar(number_words), decreasing = TRUE)]

# Generate the regex pattern with longer words prioritized
regex_pattern <- paste0("\\b(?:", paste(sorted_number_words, collapse = "|"), ")\\b")

# Use the regex pattern to extract words
extracted_words <- df %>%
  mutate(
    income = tolower(income),
    words_thousand = if_else(str_detect(income, "(?<=\\d)k|thousand"), "000", NA_character_), # Turn k after a number and thousand into 000
    words_number = str_extract(income, regex_pattern),  # Extract the matched word from number_words, prioritizing longer words
    words_numeric = case_when(is.na(words_thousand) ~ match(words_number, number_words),
                              !is.na(words_thousand) & !is.na(words_number) ~  match(words_number, number_words) * 1000), #add 000s as needed
    confidence = case_when(!is.na(words_numeric) ~ "Imputed number from word",
                           !is.na(words_thousand) ~ "Replaced thousand - 000")
    )

# Join numbers and words
clean_income <- df %>%
  left_join(extracted_numbers) %>%
  left_join(extracted_words %>%
              select(-income), by = c("_index")) %>%
  mutate(confidence = coalesce(confidence.x, confidence.y)) %>%
  select(-confidence.x, -confidence.y) %>%
  mutate(income_clean = coalesce(numbers, words_numeric),
         confidence = case_when(!is.na(confidence) ~ confidence,
                                income_clean < 1000 ~ "Multipled by 1000",
                                is.na(confidence) & is.na(income_clean) ~ as.character(NA),
                                is.na(confidence) ~ "No change",
                                TRUE ~ confidence),
         income_clean = case_when(income_clean < 1000 & !is.na(words_thousand) ~ income_clean * 1000, # Combine number extracted and thousand
                                  income_clean < 1000 ~ income_clean * 1000, # Assume all values are in thousands seems reasonable
                                  TRUE ~ income_clean)) %>%
  select(`_index`, income, income_clean, confidence)

# You can use _index to join it back to your main dataframe.
