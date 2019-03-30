# not properly taggin or handling sentence enders?
# all ngs for enders are NA
# some ending words can't be sentence enders
# sample still fails occasionally

library(tidyverse)
# install.packages("devtools")
# devtools::install_github("sbha/rstrings")
library(rstrings)

source('sample_text.R')
source('format_data.R')
source('sen_gen_function.R')
#top_ng <- 25


wd_cnt <- df_ngs %>% distinct(sen_no, word_cnt) %>% summarise(mean = round(mean(word_cnt)))

#sen_gen(sen_length)
sen_gen(wd_cnt$mean)
sen_gen(12)

# living for better babes
 
