# not properly taggin or handling sentence enders?
# all ngs for enders are NA
# some ending words can't be sentence enders
# sample still fails occasionally


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
  mutate(bigram = gsub('\\s+NA$', '', bigram)) %>% 
  mutate(bigram = ifelse(str_count(bigram, '\\w+') < 2, NA, bigram)) %>% 
  mutate(bg_start = str_extract(bigram, '^\\w+')) %>%
  mutate(bg_end = str_extract(bigram, '\\w+$')) %>% 
  mutate(bg_ender = !bg_end %in% bg_start) %>% 
  mutate(trigram = lead(tokens, 2)) %>% 
  mutate(trigram = paste(bigram, trigram)) %>% 
  mutate(trigram = gsub('\\s+NA$', '', trigram)) %>% 
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





#top_ng <- 25

#This is the start of something awesome! 
sen_gen <- function(sen_len, top_ng = 25){
  mid_len <-  sen_len - 2
  # sentence beginning
  # generate a randon bigram from the beginning of a sentence
  sen_lead <- sample(df_ngs$bigram[df_ngs$sen_index==1], 1, replace = TRUE)
  lead_start = gsub(' \\w+$', '', sen_lead)
  lead_end = gsub('^\\w+ ', '', sen_lead)
  # generate the middle 
  # take last word from beginning bigram and begin function
  next_word <- function(end_word){
    df_out <-
      df_ngs %>%
      filter(bg_start == end_word & sen_order == 'middle' & bg_ender == FALSE) %>%
      distinct(bg_end, .keep_all = TRUE) %>%
      arrange(desc(bg_cnt)) %>%
      top_n(top_ng, bg_cnt) %>%
      sample_n(1, replace = TRUE) %>%
      distinct(bg_end)
    new_word <- unlist(df_out$bg_end, use.names = FALSE)
    new_word
  }
  
  # both ways eventually get an error?
  # recur <- function(base_string, mid_len) {
  #   if (str_count(base_string, '\\w+') == mid_len) base_string #break
  #   #else recur(paste(base_string, next_word(base_string)), mid_len)
  #   else recur(paste(base_string, next_word(str_extract(base_string, '\\w+$'))), mid_len)
  #   #else print('stu')
  # }
  recur <- function(base_string, mid_len) {
    if (str_count(base_string, '\\w+') < mid_len)  #break
      #recur(paste(base_string, next_word(base_string)), mid_len)
      recur(paste(base_string, next_word(str_extract(base_string, '\\w+$'))), mid_len)
    else return(base_string)
  }
  
  sen_mid <- recur(next_word(lead_end), mid_len)
  
  # end word 
  mid_end <- str_extract(sen_mid, '\\w+$')
  
  ending_word <- function(end_word){
    df_out <-
      df_ngs %>%
      #filter(bg_start == end_word & sen_order == 'last') %>%
      filter(bg_start == end_word & bg_end == TRUE) %>%
      distinct(bg_end, .keep_all = TRUE) %>%
      arrange(desc(bg_cnt)) %>%
      top_n(top_ng, bg_cnt)
    
    if (nrow(df_out) == 0){
      df_out <-
        df_ngs %>%
        filter(bg_start == end_word) %>%
        distinct(bg_end, .keep_all = TRUE) %>%
        arrange(desc(bg_cnt)) %>%
        top_n(top_ng, bg_cnt)
    }
    
    df_out <- df_out %>% 
      sample_n(1, replace = TRUE) %>%
      distinct(bg_end)
    new_word <- unlist(df_out$bg_end, use.names = FALSE)
    new_word
  }
  sen_end <- ending_word(mid_end)
  
  sen_out <- paste(sen_lead, sen_mid, sen_end)
  sen_out
}

#sen_gen(sen_length)
sen_gen(10)


df_ngs %>% filter(bg_ender == TRUE & bg_end == 'down')
df_ngs %>% filter(tokens == 'soninlaw')
#df_ngs %>% filter(bg_ender == TRUE & bg_end == 'mr')
df_ngs %>% filter(bg_end == 'mr')


# https://stackoverflow.com/questions/13052522/how-to-leave-the-r-browser-mode-in-the-console-window
