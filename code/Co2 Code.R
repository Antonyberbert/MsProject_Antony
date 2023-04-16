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

#----------------------------------------------------------------------------------------------------------------------------------------------------
#What are the most frequent reasons?
#1) Get all reasons into one column
allreasons_df <- data.frame(reason = c(svote_df[,"REASON1DEN4_01"],svote_df[,"REASON1DEN4_02"],
                                       svote_df[,"REASON1DEN4_03"], svote_df[,"REASON1DEN4_04"]))%>%
  filter(!is.na(reason) | reason != 99)
#2) Order Reasons by frequency
allreasons_df <- table(allreasons_df$reason)%>%
  as.data.frame()%>%
  arrange(desc(Freq))%>%
  rename(Reason = Var1)
allreasons_df

#3) Add Description as a new column
allreasons_df$descr <- c("Kosten-Nutzen-Verhältnis stimmt nicht",
           "gegen CO2-Steuer",
           "zu extrem/übertriebene Massnahmen/unrealistisch",
           "Schweiz ist zu klein um etwas zu bewirken/Schweizer Alleingang bringt nichts",
           "höherer Benzinpreis",
           "schwächt den Mittelstand/die Mittelschicht, nicht auf Kosten der Mittelschicht",
           "nützt dem Klima zu wenig",
           "Gesetz war überladen",
           "ländliche Bevölkerung klar im Nachteil",
           "derzeitige Gesetzgebung genügt bereits",
           "höherer Heizöl-Preis",
           "gegen Umverteilung",
           "Schweiz zu klein als Vorbild",
           "Einnahmequelle für den Bund",
           "fördert Zweiklassen-Gesellschaft",
           "direkt betroffen (z.B. leidenschaftlicher Autofahrer/Vielflieger/alte Heizung etc)",
           "Ungerechtes/asoziales Gesetz",
           "allgemeine negative Äusserungen (z.B. schlecht/bringt nichts/sinnlos, ungerechtes/asoziales Gesetz)",
           "zu viel Bürokratie/zu starker Markteingriff",
           "gegen «grüne» Anliegen",
           "schadet der Schweizer Wirtschaft",
           "Stadt-Land-Graben nicht berücksichtigt",
           "Reisen/Fliegen wird teurer",
           "wirtschaftliche Gründe",
           "Klimadebatte basiert auf falschen Fakten",
           "Problem muss mit Innovation/Technik gelöst werden",
           "Corona",
           "Anderes zu politische Gründe",
           "schadet Randregionen",
           "Empfehlungen von Bundesrat/Parlament/Kantonen(-Regierungen)",
           "Anderes zu wirtschaftliche Gründe",
           "Anderes",
           "Allgemeines",
           "allgemeine Antworten (z.B. schon mal gehört)",
           "Bauchgefühl",
           "Abstimmungskampf",
           "politische Gründe",
           "Hysterie um Klimawandel übertrieben",
           "falsche Nennungen/Antworten",
           "Empfehlungen von Parteien",
           "Verwechslung mit anderen Vorlagen",
           "weiss nicht/Nein",
           "Anderes zu Umwelt",
           "gegen Klimajugend",
           "Empfehlungen von Verbänden")

           

allreasons_df%>%
  ggplot(aes(x=Reason,y=Freq))+
  geom_bar(stat="identity")

#Export to excel
library("writexl")
write_xlsx(allreasons_df, "F:/Unibas/Master/Masterprojekt/MsProject_Antony/data/freq_reasons.xlsx")
#----------------------------------------------------------------------------------------------------------------------------------------------------
#Get a random sample of 100 entries with the reasons why they voted no
set.seed(1)
rvote_df <- slice_sample(svote_df, n=100)
rreasons_df <- rvote_df%>%
  select(REASON1DEN4_01:REASON2DEN4)

#Export to excel
library("writexl")
write_xlsx(rreasons_df, "F:/Unibas/Master/Masterprojekt/MsProject_Antony/data/reasons.xlsx")

