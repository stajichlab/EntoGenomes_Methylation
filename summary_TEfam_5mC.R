library(tidyverse)
library(dplyr)
library(vroom) # speed up reading large files

infile="data/TE_rs_5mc_c.tsv.gz" # compressed files can be read directly
conTE <- read_tsv(infile,col_names = c("CHROM","SOURCE","TYPE","START","END","SCORE","STRAND","PHASE","GROUP","COUNT_5mC"))
conTE.sp <- conTE %>% mutate(TE_type = str_replace(GROUP,"type=([^;]+);family=.+$","\\1")) %>% 
  mutate(TE_family = str_replace(GROUP,"^.+family=([^;]+)$","\\1")) %>% select(-c(GROUP,SOURCE,TYPE,PHASE,SCORE))
conTE.clean <- conTE.sp %>% filter(TE_type != "Simple_repeat" & TE_type != "Low_complexity")
