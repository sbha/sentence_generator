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
      #filter(bg_start == end_word & bg_end == TRUE) %>%
      filter(bg_start == end_word & bg_ender == TRUE) %>%
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