# Forschungsprojekt Polarisierung & Generationen --------------------------- 
# Edda Brandes
# last edit: 09.01.2022
# Step : Prepare data frame


## Load libraries====
library(readstata13)
library(tidyverse)
library(hIRT) 
library(splines)
library(emdist)
library(Rcpp)
library(dplyr)
library(haven)
library(purrr)
library(psych)


## Load data set ====
load(file="Data_Allbus,Sample.Rdata")
dim(allbus_sample)
  #50013 cases and 34 Variables


## Redefine value labels and Vectors ====
  ### Covariate missings definine & redefine ####
table(is.na.data.frame(allbus_sample)) # keine fehlenden Werte bisher definiert
table(allbus_sample$sex) #no missings
table(allbus_sample$year) #no missings
table(allbus_sample$age) #-32 74 missings
table(allbus_sample$yborn) #-9 74 missings

allbus_sample <- allbus_sample %>% mutate(sex=recode_factor(sex, MANN="Mann", FRAU="Frau"))
allbus_sample <- allbus_sample %>% mutate(age = na_if(age, "-32")) %>% mutate(yborn = na_if(yborn, "-9"))

  #Test NA for covariate and exclude NA´s
table(is.na(allbus_sample$age))
    #>FALSE  TRUE 
    #> 49939    74 
table(is.na(allbus_sample$yborn))
#>FALSE  TRUE 
#> 49939    74 

  #exclude 74 cases, where yborn is missing -> rowise
allbus_sample <- allbus_sample %>% drop_na(yborn)


  ##Distribution of cases, units for survey years ====
table_year <- table(allbus_sample$year)
print(table_year)
    #> 1991 1992 1994 1996 1998 2000 2002 2004 2006 2008 2010 2012 2014 2016 2018 
    #> 3053 3547 3446 3512 3234 3804 2813 2943 3413 3457 2818 3473 3468 3486 3472 
write.table(table(allbus_sample$year),file="2.Output/table cases per survey year.txt", dec= ",", sep= "\t")


  #Items auf Value Labels testen, Value Labels in Zahlen
names(allbus_sample)
table(allbus_sample$item_fr01) 
    #Value Labels
      #TNZ: SPLIT         KEINE ANGABE          WEISS NICHT        NICHT ERHOBEN       STIMME VOLL ZU       STIMME EHER ZU STIMME EHER NICHT ZU  STIMME GAR NICHT ZU 
           # 3489                   46                  282                22664                14133                 5543                 2853                  929 
table(allbus_sample$item_fn02)
    #Value Labels
      #KEINE ANGABE   WEISS NICHT NICHT ERHOBEN            JA          NEIN UNENTSCHIEDEN 
        #       65           330         20039         15791          9826          3888 
table(allbus_sample$item_abtreib1) 
    # Value Labels
      #TNZ: SPLIT        KEINE ANGABE         WEISS NICHT       NICHT ERHOBEN    JA,MOEGLICH SEIN NEIN,NICHT MOEGLICH 
      #      3310                  70                1161               32190                7287                5921 
table(allbus_sample$item_sterbe) 
    # Value Labels 
      #TNZ: SPLIT      KEINE ANGABE       WEISS NICHT     NICHT ERHOBEN      SEHR SCHLIMM  ZIEMLICH SCHLIMM   WENIGER SCHLIMM GAR NICHT SCHLIMM 
      #     1533               104               274             39849              1269              1214              3307              2389 
table(allbus_sample$item_iw05) 
    # Value Labels 
        #KEINE ANGABE          WEISS NICHT        NICHT ERHOBEN       STIMME VOLL ZU       STIMME EHER ZU STIMME EHER NICHT ZU  STIMME GAR NICHT ZU 
        #         29                  317                40082                 1252                 2691                 3453  
table(allbus_sample$item_iw05) 
    # Value Labels 
      #KEINE ANGABE          WEISS NICHT        NICHT ERHOBEN       STIMME VOLL ZU       STIMME EHER ZU STIMME EHER NICHT ZU  STIMME GAR NICHT ZU 
      #         29                  317                40082                 1252                 2691                 3453                 2115 
table(allbus_sample$item_mi02)
    # Value Labels 
      #TNZ: SPLIT     KEINE ANGABE      WEISS NICHT       VERWEIGERT    NICHT ERHOBEN UNEINGESCHRAENKT  ZUZUG BEGRENZEN GANZ UNTERBINDEN 
      #     3394               69              405                0            29124             2339            11593             3015 
table(allbus_sample$item_ma01)
    # Value Labels 
      #TNZ: SPLIT         TNZ: FILTER        KEINE ANGABE         WEISS NICHT          VERWEIGERT       NICHT ERHOBEN STIMME GAR NICHT ZU              .._(2)              .._(3) 
      #       1605                1395                 115                   0                   0               23174                 995                1085                1720 
      #     .._(4)              .._(5)              .._(6)      STIMME VOLL ZU 
      #       3437                4151                3548                871

allbus_sample <- allbus_sample %>% mutate_at(.vars = vars(starts_with("item")),
            funs(recode(., `STIMME GAR NICHT ZU`=4, `STIMME EHER NICHT ZU`=3, `STIMME EHER ZU`=2, `STIMME VOLL ZU`=1, 
                        `NEIN`=3, `UNENTSCHIEDEN`=2, `JA`=1, 
                        `NEIN,NICHT MOEGLICH`=2, `JA,MOEGLICH SEIN`=1, 
                        `SEHR SCHLIMM`=4, `ZIEMLICH SCHLIMM`=3, `WENIGER SCHLIMM`=2, `GAR NICHT SCHLIMM`=1,
                        `STIMME GAR NICHT ZU`=7, `.._(2)`=6, `.._(3)`=5, `.._(4)`=4, `.._(5)`=3, `.._(6)`=2, `STIMME VOLL ZU`=1,
                        `GANZ UNTERBINDEN`=3, `ZUZUG BEGRENZEN`=2, `UNEINGESCHRAENKT`=1,
                        .default = NA_real_)))  %>% 
  mutate_at(.vars = vars(starts_with("item")), funs(as.integer))

## Items auf NAs testen ====

x <- allbus_sample %>% select(starts_with("item")) %>% rowwise()
na_item <- function(x){
  na_item <-  sum(is.na(x))
  }

table_na <- sapply(x, na_item)
print(table_na)
write.table(table_na, file="2.Output/table_NAs per Item.txt", dec= ",", sep= "\t")

    # bei allen Items genügend Werte, kein Item hat nur NA´s oder fast nur NA´s

## Create Vector for generation ====
  # 5 Generations 
allbus_sample <- allbus_sample %>% mutate(gen_f = case_when(yborn >1939 & yborn <=1949 ~ "1",
                                                          yborn >1949 & yborn <=1964 ~ "2",
                                                          yborn >1964 & yborn <=1980 ~ "3",
                                                          yborn >1980 & yborn <=1995 ~ "4",
                                                          yborn >1995 & yborn <=2004 ~ "5")) %>%
  mutate(gen_f=factor(gen_f,labels=c("nach","wohl","genx","geny", "genz")))


   #distribution of gen per year
table_gen_f_year <- table(allbus_sample$year, allbus_sample$gen_f)
print(table_gen_f_year)
    # erst bei 2000 Daten für Generation Y
    # erst ab 2016 Daten für Generation Z 
    #zu wenig Daten für Gen Z, Genz raus

  #2 Gruppen von Generationen definieren -> Entscheidung für vereinfachtes Modell


allbus_sample <- allbus_sample %>% mutate(gen = case_when(yborn <= 1964 ~ "1",
                                                          yborn > 1964  ~ "2")) %>%
  mutate(gen=factor(gen,labels=c("genalt", "genjung")))
print(table(allbus_sample$year, allbus_sample$gen))

write.table(table(allbus_sample$year), file="2.Output/gen per year per survey year.txt", dec= ",", sep= "\t")


table(is.na(allbus_sample$gen))
    #keine NAs mehr, alle Fälle zugeordnet

  #save intermediated data
save(allbus_sample, file = "Data_Allbus,Sample_prep.Rdata")



