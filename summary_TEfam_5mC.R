library(tidyverse)
library(dplyr)
library(vroom) # speed up reading large files
library(cowplot)
infile="data/TE_rs_5mc_c.tsv.gz" # compressed files can be read directly
conTE <- read_tsv(infile,col_names = c("CHROM","SOURCE","TYPE","START","END","SCORE","STRAND","PHASE","GROUP","COUNT_5mC"))
conTE.sp <- conTE %>% mutate(TE_type = str_replace(GROUP,"type=([^;]+);family=.+$","\\1")) %>% 
  mutate(TE_family = str_replace(GROUP,"^.+family=([^;]+)$","\\1")) %>% select(-c(GROUP,SOURCE,TYPE,PHASE,SCORE))

conTE.clean <- conTE.sp %>% filter(TE_type != "Simple_repeat" & TE_type != "Low_complexity")

#conTE.clean %>% group_by(TE_family) %>% summarize(famcount = n())
famNoM <- conTE.clean %>% group_by(TE_family) %>% filter(COUNT_5mC == 0) %>% summarize( No5mC = n())
famM <- conTE.clean %>% group_by(TE_family) %>% filter(COUNT_5mC > 0) %>% summarize( With5mC = n())
famTE5mCNum <- famM %>% full_join(famNoM) %>%  # this joins so we have 2 columns of data summarized
  replace_na(list(With5mC=0,No5mC=0)) %>% # the join may have some families missing in either set, replace those NAs with 0
  mutate(TotalFamCt = With5mC + No5mC) %>% # we may want to filter plot by families above a size
  mutate(Freq5mC = With5mC / TotalFamCt) # also compute a frequency of those w methylation

hp <- ggplot(famTE5mCNum,aes(x=Freq5mC)) + geom_histogram(color="black", fill="lightblue") + theme_cowplot(12)
ggsave("plots/5mC_family_Freq.pdf",hp)
