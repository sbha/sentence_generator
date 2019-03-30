### Sample Text ###
# Download sample text and convert to a data frame




# download from Project Gutenberg
txt_url <- 'https://www.gutenberg.org/files/2814/2814-0.txt'
txt_raw <- readr::read_file(txt_url)

# remove Project Gutenberg formatting
txt_clean <- str_remove(txt_raw, 'End of the Project Gutenberg EBook of Dubliners.+$')
txt_clean <- str_remove(txt_raw, '^.+Produced by David Reed, Karol Pietrzak, and David Widger')
txt_clean <- str_replace_all(txt_clean, '\r\n', ' ')
txt_clean <- str_replace_all(txt_clean, '\\s+', ' ')
txt_clean <- str_replace_all(txt_clean, '“|”', '"')
txt_clean <- str_replace_all(txt_clean, '’', "'")

# parse the text into sentences with rstrings::split_into_sentences
sentences <- split_into_sentences(txt_clean)
names(sentences) <- 'sentence'

# format sentences as a dataframe
df_txt <- purrr::map_df(sentences, ~.x) %>% 
  rename(sen_raw = sentence)

