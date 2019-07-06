# using trigrams

#This is the start of something awesome! 
sen_gen <- function(sen_len, top_ng = 25){
  mid_len <-  sen_len - 2
  # sentence beginning
  # generate a randon bigram from the beginning of a sentence 
  # keep as a bigram - not trigram
  sen_lead <- sample(df_ngs$bigram[df_ngs$sen_index==1], 1, replace = TRUE)
  lead_start = str_remove(sen_lead, ' \\w+$')
  lead_end = str_remove(sen_lead, '^\\w+ ')
  
  # generate the middle 
  # take last word from beginning bigram and begin function
  next_word <- function(end_word){
    df_out <-
      df_ngs %>%
      filter(tg_start == end_word & sen_order == 'middle' & tg_ender == FALSE) %>%
      distinct(tg_end, .keep_all = TRUE) %>%
      arrange(desc(tg_cnt)) %>%
      top_n(top_ng, tg_cnt) %>%
      sample_n(1, replace = TRUE) %>%
      distinct(tg_end)
    new_word <- unlist(df_out$tg_end, use.names = FALSE)
    new_word
  }
  
  # keep generating a new middle word based on the number of middle words
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
  
  # last word in the middle section
  mid_end <- str_extract(sen_mid, '\\w+$')
  
  ending_word <- function(end_word){
    df_out <-
      df_ngs %>%
      #filter(tg_start == end_word & sen_order == 'last') %>%
      #filter(tg_start == end_word & tg_end == TRUE) %>%
      filter(tg_start == end_word & tg_ender == TRUE) %>%
      distinct(tg_end, .keep_all = TRUE) %>%
      arrange(desc(tg_cnt)) %>%
      top_n(top_ng, tg_cnt)
    
    if (nrow(df_out) == 0){
      df_out <-
        df_ngs %>%
        filter(tg_start == end_word) %>%
        distinct(tg_end, .keep_all = TRUE) %>%
        arrange(desc(tg_cnt)) %>%
        top_n(top_ng, tg_cnt)
    }
    
    df_out <- df_out %>% 
      sample_n(1, replace = TRUE) %>%
      distinct(tg_end)
    new_word <- unlist(df_out$tg_end, use.names = FALSE)
    new_word
  }
  sen_end <- ending_word(mid_end)
  
  sen_out <- paste(sen_lead, sen_mid, sen_end)
  sen_out
}


