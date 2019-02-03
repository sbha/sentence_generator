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



#sen_gen(sen_length)
sen_gen(10)


df_ngs %>% filter(bg_ender == TRUE & bg_end == 'down')
df_ngs %>% filter(tokens == 'soninlaw')
#df_ngs %>% filter(bg_ender == TRUE & bg_end == 'mr')
df_ngs %>% filter(bg_end == 'mr')


# https://stackoverflow.com/questions/13052522/how-to-leave-the-r-browser-mode-in-the-console-window

# our shouts echoed the way you might calm enmity while mr
