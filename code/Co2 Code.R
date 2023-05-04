library(tidyverse)
library(dplyr)

#Read csv data of vote from 13.06.2021
vote_df <- read.csv("https://swissvotes.ch/attachments/7293958a71051fb963b701a676158da7d82cd36355c433591a22407a1c2bcfaf",sep=";",header=T, dec=".")

#Select relevant columns and create subset of no-voters that gave a main reason
novote_df <- vote_df%>%
  select(BIRTHYEARR, CONTROL1, S11, POLINT, PART, NONVOTE_01:NONVOTE_98, PARTY15,
         VOTE4:REASON2DEN4, TRUST_1:VALUE16, DIFFICUL4:ARGU4_6, MEDIAUSE_1:DISCUS,
         PARTYA:HABITAT) %>%
  filter(VOTE4==2 & REASON1DEN4_01 != 99)

#Select relevant columns and create subset of yes-voters that gave a main reason
yesvote_df <- vote_df%>%
  select(BIRTHYEARR, CONTROL1, S11, POLINT, PART, NONVOTE_01:NONVOTE_98, PARTY15,
         VOTE4:REASON2DEN4, TRUST_1:VALUE16, DIFFICUL4:ARGU4_6, MEDIAUSE_1:DISCUS,
         PARTYA:HABITAT) %>%
  filter(VOTE4==1 & REASON1ACC4_01 != 99)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#What are the most frequent reasons for being against the CO2 vote?
#1) Get all reasons into one column
no_allreasons_df <- data.frame(reason = c(novote_df[,"REASON1DEN4_01"],novote_df[,"REASON1DEN4_02"],
                                       novote_df[,"REASON1DEN4_03"], novote_df[,"REASON1DEN4_04"]))%>%
  filter(!is.na(reason) | reason != 99)
#2) Order Reasons by frequency
no_allreasons_df <- table(allreasons_df$reason)%>%
  as.data.frame()%>%
  arrange(desc(Freq))%>%
  rename(Reason = Var1)
no_allreasons_df

#3) Add Description as a new column
no_allreasons_df$descr <- c("Kosten-Nutzen-Verhältnis stimmt nicht",
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

           

no_allreasons_df%>%
  ggplot(aes(x=Reason,y=Freq))+
  geom_bar(stat="identity")

#Export to excel
library("writexl")
write_xlsx(no_allreasons_df, "F:/Unibas/Master/Masterprojekt/MsProject_Antony/data/freq_reasons.xlsx")
#----------------------------------------------------------------------------------------------------------------------------------------------------
#What are the most frequent reasons for being for the CO2 vote?
#1) Get all reasons into one column
yes_allreasons_df <- data.frame(reason = c(yesvote_df[,"REASON1ACC4_01"],yesvote_df[,"REASON1ACC4_02"],
                                          yesvote_df[,"REASON1ACC4_03"], yesvote_df[,"REASON1ACC4_04"]))%>%
  filter(!is.na(reason) | reason != 99)
#2) Order Reasons by frequency
yes_allreasons_df <- table(yes_allreasons_df$reason)%>%
  as.data.frame()%>%
  arrange(desc(Freq))%>%
  rename(Reason = Var1)
yes_allreasons_df
#3) Add Description as a new column
yes_allreasons_df$descr <- c("Klimawandel",
                             "dringend handeln",
                             "Umweltschutz",
                             "CO2 Bilanz verbessern",
                             "Verursacherprinzip fördern/Mehrkosten (nur für Klimasünder",
                             "(Pariser) Klimaabkommen einhalten",
                             "nächste Generation schützen",
                             "Umweltschutz",
                             "weiss nicht/Nein",
                             "Handeln statt Reden (z.B. jetzt oder nie)",
                             "nur ein Planet",
                             "lieber kleine Schritte als gar keine Schritte",
                             "Atemluft",
                             "klimafreundliche Technologien/Investitionen fördern",
                             "Verantwortung für nächste Generation",
                             "zukunftsweisendes Gesetz",
                             "politisches Zeichen setzen",
                             "Änderung des Reiseverhaltens",
                             "Änderung des Konsumverhaltens",
                             "Schäden von Klimawandel wären teuer",
                             "guter Kompromiss",
                             "Schweiz als Vorbild",
                             "allgemeine positive Äusserungen",
                             "Unabhängigkeit von Öl-Firmen",
                             "Steuerung via Marktmechanismen",
                             "direkt betroffen",
                             "wirtschaftliche Gründe",
                             "Empfehlung von Bundesrat/Parlament/Kantonen",
                             "Bauchgefühl",
                             "Energiewende herbeiführen",
                             "Verwechslung mit anderen Vorlagen",
                             "Sympathie-Votum/Diskussion unterstützen",
                             "Verteuerung von schädlichen Produkten/Produktionen",
                             "falsche Nennungen/Antworten",
                             "Empfehlung von Parteien",
                             "Anderes zu wirtschaftliche Gründe",
                             "Anderes",
                             "Empfehlung von Familienmitgliedern/Verwandten/Angehörigen",
                             "wegen Fernsehbeitrag/Zeitungsartikel/Medienberichterstattung",
                             "Allgemeines",
                             "allgemeine negative Äusserungen (z.B. nicht gut so wie es jetzt ist",
                             "Abstimmungskampf",
                             "politische Gründe",
                             "nächste Generation")

#Export to excel
library("writexl")
write_xlsx(yes_allreasons_df, "F:/Unibas/Master/Masterprojekt/MsProject_Antony/data/freq_reasons_yes.xlsx")


#----------------------------------------------------------------------------------------------------------------------------------------------------
#Get a random sample of 100 entries with the reasons why they voted no
set.seed(1)
rvote_df <- slice_sample(svote_df, n=100)
rreasons_df <- rvote_df%>%
  select(REASON1DEN4_01:REASON2DEN4)

#Export to excel
library("writexl")
write_xlsx(rreasons_df, "F:/Unibas/Master/Masterprojekt/MsProject_Antony/data/reasons.xlsx")

