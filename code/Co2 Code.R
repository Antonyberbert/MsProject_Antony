library(tidyverse)
library(dplyr)

#Read csv data of vote from 13.06.2021
vote_df <- read.csv("https://swissvotes.ch/attachments/7293958a71051fb963b701a676158da7d82cd36355c433591a22407a1c2bcfaf",sep=";",header=T, dec=".")

#Select relevant columns and create subset of no-voters that gave a main reason
svote_df <- vote_df%>%
  select(BIRTHYEARR, CONTROL1, S11, POLINT, PART, NONVOTE_01:NONVOTE_98, PARTY15,
         VOTE4:REASON2DEN4, TRUST_1:VALUE16, DIFFICUL4:ARGU4_6, MEDIAUSE_1:DISCUS,
         PARTYA:HABITAT) %>%
  filter(VOTE4==2 & REASON1DEN4_01 != 99)

#Get a random sample of 100 entries with the reasons why they voted no
set.seed(1)
rvote_df <- slice_sample(svote_df, n=100)
rreasons_df <- rvote_df%>%
  select(REASON1DEN4_01:REASON2DEN4)

#Export to excel
library("writexl")
write_xlsx(rreasons_df, "F:/Unibas/Master/Masterprojekt/MsProject_Antony/data/reasons.xlsx")

