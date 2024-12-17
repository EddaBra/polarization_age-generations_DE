# Forschungsprojekt Polarisierung & Generationen --------------------------- 
# Edda Brandes
# last edit: 04.01.2022
# Step 1: Read data and filter variables 


## Load libraries ====
library(readstata13)
library(tidyverse)
library(hIRT) 
library(splines)
library(emdist)
library(Rcpp)
library(dplyr)

## Read Data ====
allbus <- read.dta13("ALLBUS1980-2018.dta", generate.factors=F)

## Prepare data for the time span 1990 -2018 ====
  #respid: Identifikationsnr. des Befragten, ALLBUS sortiert nach Fällen
  #za_nr: Studiennr. des Befragten
  #year: Erhebungsjahr nur die auswählen zwischen 1991 und 2018

allbus_zeit <- allbus%>% filter(year > 1990) 

## save intermediated data ====
save(allbus_zeit, file = "Data_Allbus,Zeit.Rdata")


## Prepare data for the necessary variables ====

  #Generation
    #Alter CB 1398: age
    #Geburtsjahr CB 1396: yborn

  #Geschlecht
    #sex CB 1395: sex

  #primäre politische Sozialisation
    #youth in the east CB 2376 : dg03
      #problem: data begins 2006
    #born in the east or west CB 2380: dg04
      #problem: only Data to 2004 ( 2004 included)
    #since when in the current federal state? CB: 2382: dg07
      #problem: only data to 2004 (2004 included)
    # federal state currently living in: land
      # continually asked but difficult to extrapolate primary socialisation from it
    # origin country CB 2392: dm01
      # problem: lot of missing values
    # country where the respondent grew up CB 2406: dm06
      # problem: lot of missing values
    #politische Wahlabsicht/ Einstellung der Eltern nicht erhoben!

  #vote intention and political interests
    #Zufriedenheit mit Bundesregierung CB 17: ps01
    #Zufriedenheit mit Landesregierung CB 18: ps02
    #Zufriedenheit mit Demokratie?CB 19: ps03
    #Funktioniert das politische System? CB 21: ps04
    #politisches Interesse ordinal CB 37: pa02a
    #politisches Interesse, 10er Skala CB 39: pa02b
    #Parteipräferenz vorhanden? CB 41: pa03 
    #Parteipräferenz der Befragten, CB 42: pa04
    #Wahlabsicht Bundestagswahl CB 44: pv01

  #politische Einstellung nach Munzert & Bauer (2013)
    #hohe Zahl rechts- konservativ - marktliberalistisch
    # niedrige Zahl links-liberitär - sozialstaatlich

  #traditionelles Familienbild 
    #berufstätige Frau ebenso herzliches & vertrauensvolles Verhältnis zum Kind CB 659: fr01  
    #Frau nicht arbeiten, Kleinkind? CB 663: fr03a
    #Frau berufstätig bessere Mutter? CB 667: fr05a
    #Frau nach Arbeitsplatz freimachen? CB 669: fr06
    #lieber Mann bei der Karriere helfen? CB 661: fr02
    #Frau zu Hause Kinder versorgen? CB 665: fr04a

  #Moral
    #Abtreibung gesetzlich erlaubt, verheiratete Frau, keine Kinder mehr CB 1167: vm02
    #Abtr. finanzielle Notlage der Familie CB 1171: vm04
    #Abtr. ledige Mutter ohne Kinderwunsch CB 1175: vm06
    #Abtr. wenn die Frau es will CB 1177: vm07
    #Verhaltensberurteilung: ärztliche Sterbehilfe CB 1333: ca02
    #Verhaltensbeurteilung: Haschischkonsum CB 1349: ca12
    #Verhaltensbeurteilung: Homosexualität CB 1351: ca13

  #Umverteilung
    #Staat, soziale Sicherung reduziert Arbeitswillen CB 430: iw05
    #Unternehmensgewinne fördern die Wirtschaft CB 426: iw02
    #Gewinne werden in der BRD gerecht verteilt CB 431: iw06
    #Ungleichheit in der BRD nicht weiter reduzierbar CB 432: iw07
    #Einkommendifferenz erhöht Motivation CB 466: im19
    #Rangunterschiede sind  akzeptabel CB 468: im20
    #soziale Unterschiede sind gerecht CB 470: im21

  #Migration
    #Zuzug von Asylsuchenden CB 475: mi02
    #Zuzug von Nicht EU Arbeitnehmern CB 479: mi04
    #Ausländer, Gastarbeiter: mehr Anpassung CB 524: ma01
    #Aus., Gast, unter sich heiraten CB 536: ma04
    #Aus., Gast, Heim bei knapper Arbeit CB 528: ma02
    #Aus., Gast, keine politischen Aktionen CB 532: ma03

load(file="Data_Allbus,Zeit.Rdata")

allbus_sample <- allbus_zeit %>%  dplyr::select( respid, za_nr, year,
  age, yborn, 
                sex, fr01,fr03a,fr05a,fr06, fr02, fr04a, fn02,
                vm02, vm04, vm06, vm07, ca02, ca12, ca13,
                iw05, iw02, iw06, iw07, im19, im20, im21,
                mi02, mi04, ma01, ma04, ma02, ma03) %>% 
  rename(item_fr01 ="fr01",item_fr03a ="fr03a",item_fr05a="fr05a",item_fr06="fr06",item_fr02="fr02", item_fr04a ="fr04a", item_fn02 ="fn02",
         item_abtreib1="vm02", item_abtreib2="vm04", item_abtreib3="vm06", item_abtreib4="vm07", item_sterbe="ca02", item_hasch="ca12", item_homo="ca13",
         item_iw05="iw05", item_iw02="iw02", item_iw06="iw06", item_iw07="iw07",item_im19="im19", item_im20="im20", item_im21="im21",
         item_mi02="mi02", item_mi04="mi04", item_ma01="ma01", item_ma04="ma04", item_ma02="ma02", item_ma03="ma03")

## save intermediated data ====

save(allbus_sample, file = "Data_Allbus,Sample.Rdata")
