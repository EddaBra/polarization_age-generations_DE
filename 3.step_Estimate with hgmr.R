# Forschungsprojekt Polarisierung & Generationen --------------------------- 
# Edda Brandes
# last edit: 22.02.2022
# Step 3: Estimation of the Hierarchical Item Response Modell


## Load libraries ====
library(tidyverse)
library(hIRT) 
library(splines)
library(emdist)
library(Rcpp)
library(dplyr)
library(haven)
library(purrr)
library(psych)
library(ltm)
library(splines)
library(msm)
library(polycor)
library(MASS)


## Load data set ====
load(file="Data_Allbus,Sample_prep.Rdata")

# Rename data set & invert variables
#all <- allbus_sample
#all$item_fr01 <- recode(all$item_fr01, `4`=1, `3`=2, `2`=3, `1`=4)
#fr05a Frau berufstätig bessere Mutter? 
#all$item_fr05a <- recode(all$item_fr05a, `4`=1, `3`=2, `2`=3, `1`=4)
#fn02 unentschieden und nein tauschen
#all$item_fn02 <- recode(all$item_fn02, `1`=1, `3`=2, `2`=3)

#Moral Abtreibung 
#all$item_abtreib1 <- recode(all$item_abtreib1, `2`=1, `1`=2)
#all$item_abtreib2 <- recode(all$item_abtreib2, `2`=1, `1`=2)
#all$item_abtreib3 <- recode(all$item_abtreib3, `2`=1, `1`=2)
#all$item_abtreib4 <- recode(all$item_abtreib4, `2`=1, `1`=2)
#all$item_sterbe <- recode(all$item_sterbe, `4`=1, `3`=2, `2`=3, `1`=4)
#all$item_hasch <- recode(all$item_hasch, `4`=1, `3`=2, `2`=3, `1`=4)
#all$item_homo <- recode(all$item_homo, `4`=1, `3`=2, `2`=3, `1`=4)

#Migration
#all$item_mi02 <- recode(all$item_mi02, `3`=1, `2`=2, `1`=3)
#all$item_mi04 <- recode(all$item_mi04, `3`=1, `2`=2, `1`=3)

#save data set with short name
#save(all, file="Data_all.Rdata")
load(file="Data_all.Rdata")


## hIRT Model One Dimensional Model, Year, Generation - overall political attitude ====

### store all items in an extra data frame ### 
items <- all %>% dplyr::select(starts_with("item"))  
is.data.frame(items)
table(all$item_abtreib1)

### Prepare the covaritate year ###
all$yearseq <- all$year - 1991
table(all$yearseq)
length(all$yearseq)
length(all$gen)
table(is.na(all$gen))
 
## Hierarchical Item Response Modell ====
  #Intercept gleich 1. Intercept wird drin gelassen, weil so die Koeffizienten leichter interpretiert werden können. 
  # Generationengruppen dichotom in jung, alt -> der Intercept liegt bei 1. Alle die der jungen Generationengruppe angehören, kriegen eine 1 zugewiesen, sonst 0. 
  #Diese dichotome Variable wird multipliziert mit der Jahreszahl. So entsteht eine dichotome Variable, die Generation und Year in Kombination angibt "gengenjung:yearseq"
  #die so entstandene Matrix entspricht den Fällen aus der Allbuserhebung.


mdl_pid <- model.matrix(~ gen*yearseq,data = all)
dim(mdl_pid)
view(mdl_pid)

### Estimation HGRM ###
hgrm_all <- hgrm(y = items, x=mdl_pid, z=mdl_pid, init="irt")

# Save
print(hgrm_all)
hgrm_all$coefficients
save(hgrm_all, file="hgrm_all.RDS")
save(all, file="Data_all.Rdata")


