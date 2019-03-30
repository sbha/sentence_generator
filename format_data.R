# Format the text data frame 
# use sample text to import df_txt
df_ngs <- 
  df_txt %>% 
  mutate(sen_no = row_number()) %>% 
  filter(sen_no != 1) %>%
  mutate(sen_clean = trimws(tolower(sen_raw))) %>% 
  mutate(sen_clean = str_remove_all(sen_clean, '[[:punct:]]')) %>% 
  mutate(tokens = str_split(sen_clean, "\\s+")) %>%
  unnest() %>% 
  arrange(sen_no) %>% 
  group_by(sen_no) %>% 
  mutate(sen_index = row_number()) %>% 
  mutate(sen_order = if_else(sen_index == 1, 'first', 
                             if_else(sen_index == max(sen_index), 'last', 'middle'))) %>%
  mutate(bigram = lead(tokens)) %>% 
  mutate(bigram = paste(tokens, bigram)) %>% 
  mutate(bigram = str_remove_all(bigram, '\\s+NA$')) %>% 
  mutate(bigram = ifelse(str_count(bigram, '\\w+') < 2, NA, bigram)) %>% 
  mutate(bg_start = str_extract(bigram, '^\\w+')) %>%
  mutate(bg_end = str_extract(bigram, '\\w+$')) %>% 
  mutate(bg_ender = !bg_end %in% bg_start) %>% 
  mutate(trigram = lead(tokens, 2)) %>% 
  mutate(trigram = paste(bigram, trigram)) %>% 
  mutate(trigram = str_remove_all(trigram, '\\s+NA$')) %>%
  mutate(trigram = ifelse(str_count(trigram, '\\w+') < 3, NA, trigram)) %>% 
  mutate(tg_start = str_extract(trigram, '^\\w+')) %>%
  mutate(tg_end = str_extract(trigram, '\\w+$')) %>% 
  mutate(tg_ender = !tg_end %in% tg_start) %>% 
  ungroup() 



df_ngs <- 
  df_ngs %>% 
  left_join(
    df_ngs %>% 
      ungroup() %>% 
      filter(!is.na(bigram)) %>% 
      count(bigram, sort = T) %>% 
      mutate(bg_freq = n / sum(n)),
    by = 'bigram'
  ) %>% 
  rename(bg_cnt = n) %>% 
  left_join(
    df_ngs %>% 
      ungroup() %>% 
      filter(!is.na(trigram)) %>% 
      count(trigram, sort = T) %>% 
      mutate(tg_freq = n / sum(n)),
    by = 'trigram'
  ) %>% 
  rename(tg_cnt = n) %>% 
  mutate(word_cnt = str_count(sen_raw, '\\w+'))


df_ngs %>% 
  select(-sen_clean, -sen_raw) %>% print(n = 50)
