# Forschungsprojekt Polarisierung & Generationen --------------------------- 
# Edda Brandes
# last edit: 22.02.2022
# Step 5: Robustness Test, Korrelationsmatrix

## Load data and data subsetten ----
library(tidyverse)
library(dplyr)
#install.packages("Hmisc")
library(Hmisc)
#install.packages("corrplot")
library(corrplot)

#Links for corrplot: 
  #https://rdrr.io/cran/corrplot/man/corrplot.html 
  #https://stefaneng.github.io/corrplot-title-cut-off/
  #http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software#compute-correlation-matrix



load(file="Data_Allbus,Sample_prep.Rdata")
Sys.setenv(LANG = "en")


items <- allbus_sample %>% dplyr::select(starts_with("item")) 
options(digits=4)


##Items umkodieren in Subset items----
  #hohe Zahl =linksliberal
  #niedrige Zahl = rechtskonservativ

  #trad. Familienbild:
  #fr01 berufstätige Frau ebenso herzliches & vertrauensvolles Verhältnis zum Kind
items$item_fr01 <- recode(items$item_fr01, `4`=1, `3`=2, `2`=3, `1`=4)
  #fr05a Frau berufstätig bessere Mutter? 
items$item_fr05a <- recode(items$item_fr05a, `4`=1, `3`=2, `2`=3, `1`=4)
  
  #Moral Abtreibung 
all$item_fn02 <- recode(all$item_fn02, `1`=1, `3`=2, `2`=3)
items$item_abtreib1 <- recode(items$item_abtreib1, `2`=1, `1`=2)
items$item_abtreib2 <- recode(items$item_abtreib2, `2`=1, `1`=2)
items$item_abtreib3 <- recode(items$item_abtreib3, `2`=1, `1`=2)
items$item_abtreib4 <- recode(items$item_abtreib4, `2`=1, `1`=2)
items$item_sterbe <- recode(items$item_sterbe, `4`=1, `3`=2, `2`=3, `1`=4)
items$item_hasch <- recode(items$item_hasch, `4`=1, `3`=2, `2`=3, `1`=4)
items$item_homo <- recode(items$item_homo, `4`=1, `3`=2, `2`=3, `1`=4)

  #Migration
items$item_mi02 <- recode(items$item_mi02, `3`=1, `2`=2, `1`=3)
items$item_mi04 <- recode(items$item_mi04, `3`=1, `2`=2, `1`=3)


## Korrelation berechnen ----
cor_items <- cor(items, method = "spearman", use="pairwise.complete.obs")
which.min(cor_items)
cor_items[348]
is.numeric(cor_items)
cor_items.r <- round(cor_items, digits=2)
cor_items.r
write.table(cor_items.r, file = "2.Output/Korrelationsmatrix.txt", sep= ";")
 
## Korrelation mit Signifikanzniveau berechnen ----
cor_data <- as.matrix(items[sapply(items, is.numeric)])
is.numeric(cor_data)
dim(cor_data)
cor_sig <- rcorr(as.matrix(items), type= "spearman")
view(cor_sig)


## extract the coefficients------
cor_sig$r

write.table(format(cor_sig$r, digits=2),file="2.Output/Korrelationskoeffizienten.txt", sep=";", col.names = TRUE, dec=",")

## extract the p-values---- 
cor_sig$P
write.table(format(cor_sig$P, digits=2), file="2.Output/Korrelationsmatrix p-value.txt", sep=";", col.names=TRUE, dec=",")

## format as a table----- 
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

cor_sig_table <-flattenCorrMatrix(cor_sig$r, cor_sig$P)
write.table(format(cor_sig_table, digits=2), file="2.Output/Korrelationsmatrix as table.txt", sep=";", col.names=TRUE, dec=",")

## Tabelle vereinfachen und anstatt der Werte Zahlengrenzen eingeben----
which.min(cor_sig$r)
cor_sig$r[60]
cor_vtable <- symnum(cor_sig$r, cutpoints = c( 0, 0.1, 0.2, 0.4, 0.6, 0.8, 1),
       symbols = c("+" , "++", "+++", "++++", "+++++", "++++++", "."),
       na="NA",
       abbr.colnames = TRUE)
  
  #gibt mir keine Tabelle aus

## Korrelationsmatrix visualisieren------
cor <- cor_sig$r
is.matrix(cor_data)
str(cor_data)

cor_p <- cor_sig$P
dim(cor_p)
dim(cor)
table(is.na(cor_p))
table(is.na(cor))
is.matrix(cor_p)

  #cor Data runden, damit bessere Skalen
cor2 <- round(cor, 1)

COL2(diverging = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu"), n = 200)


corrplot(cor2, method= "ellipse", type = "upper", 
         title="Korrelation der Items",
         order="original",
         col= COL2("RdBu",10),
         tl.cex = 0.8,
         mar=c(0,0,2,0),
         col.lim = c(-0.2,1),
         is.corr=TRUE,
         tl.col = "black", tl.srt = 90)

png(file="2.Output/Korrelationsmatrix,visualisiert.png", height=1800, width = 1800, res = 300)

corrplot(cor2, method= "ellipse", type = "upper", 
         title="Korrelation der Items",
         order="original",
         col= COL2("RdBu",10),
         tl.cex = 0.8,
         mar=c(0,0,2,0),
         col.lim = c(-0.2,1),
         is.corr=TRUE,
         tl.col = "black", tl.srt = 90)
dev.off()



corrplot(cor, method="circle", type="upper", 
         title="signifikante Korrelation der Items", 
         order="original",
         tl.cex=1,
         col.lim=c(-0.1,1),
         tl.col="black",
         p.mat = cor_p, sig.level = 0.01, insig = "blank")

    #Signifikanzniveau einfügen funktioniert nicht, weil unterschiedliche NA´s

## Korrelationen für einzelne Dimensionen----

  ###traditionelles Familienbild Korrelation----
items_trad <- items %>% dplyr::select(starts_with("item_fr")) 
items_trad <- as.matrix(items_trad[sapply(items_trad, is.numeric)])
is.numeric(items_trad)
dim(items_trad)
cor_trad <- rcorr(as.matrix(items_trad), type= "spearman")
cor_trad$r

write.table(format(cor_trad$r, digits=2),file="2.Output/Korrelationskoeffizienten_itemstrad.txt", sep=";", col.names = TRUE, dec=",")

    #extract the p-values 
cor_trad$P
write.table(format(cor_trad$P, digits=2), file="2.Output/Korrelationsmatrix_itemstrad p-value.txt", sep=";", col.names=TRUE, dec=",")

    #format as a table
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

cor_trad_table <-flattenCorrMatrix(cor_trad$r, cor_trad$P)
write.table(format(cor_trad_table, digits=2), file="2.Output/Korrelationsmatrix as table trad .txt", sep=";", col.names=TRUE, dec=",")
    
    #Korrelationsmatrix visualisieren
cor_trad.r <- cor_trad$r
cor_trad.p <- cor_trad$P
is.matrix(cor_trad.r)
str(cor_trad)

png(file="2.Output/Korrelationsmatrix trad,visualisiert.png", height=1800, width = 1800, res = 300)
cor_trad.r_plot1 <- corrplot(cor_trad.r, method= "circle", type = "upper", 
                           tl.col = "black", tl.srt = 90)
dev.off()


### Korrelationsmatrix Moral----
items_mo <- items %>% dplyr::select(starts_with("item_a"), item_fn02,item_sterbe, item_hasch, item_homo)
items_mo <- as.matrix(items_mo[sapply(items_mo, is.numeric)])
is.numeric(items_mo)
dim(items_mo)
cor_mo <- rcorr(as.matrix(items_mo), type= "spearman")
cor_mo$r

write.table(format(cor_mo$r, digits=2),file="2.Output/Korrelationskoeffizienten_itemsmo.txt", sep=";", col.names = TRUE, dec=",")

#extract the p-values 
cor_mo$P
write.table(format(cor_mo$P, digits=2), file="2.Output/Korrelationsmatrix_itemsmo p-value.txt", sep=";", col.names=TRUE, dec=",")

#format as a table
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

cor_mo_table <-flattenCorrMatrix(cor_mo$r, cor_mo$P)
write.table(format(cor_mo_table, digits=2), file="2.Output/Korrelationsmatrix moral, as table.txt", sep=";", col.names=TRUE, dec=",")

#Korrelationsmatrix visualisieren
cor_mo.r <- cor_mo$r
cor_mo.p <- cor_mo$P
dim(cor_mo$P)
dim(cor_mo$r)
is.matrix(cor_mo.r)
str(cor_mo)

png(file="2.Output/Korrelationsmatrix mo,visualisiert.png", height=1800, width = 1800, res = 300)
cor_mo.r_plot1 <- corrplot(cor_mo.r, method= "circle", type = "upper", 
                             tl.col = "black", tl.srt = 90)
dev.off()



items_mo <- items %>% dplyr::select(starts_with("item_a"), item_fn02,item_sterbe, item_hasch, item_homo)
items_mo <- as.matrix(items_mo[sapply(items_mo, is.numeric)])
is.numeric(items_mo)
dim(items_mo)
cor_mo <- rcorr(as.matrix(items_mo), type= "spearman")
cor_mo$r

write.table(format(cor_mo$r, digits=2),file="2.Output/Korrelationskoeffizienten_itemsmo.txt", sep=";", col.names = TRUE, dec=",")

#extract the p-values 
cor_mo$P
write.table(format(cor_mo$P, digits=2), file="2.Output/Korrelationsmatrix_itemsmo p-value.txt", sep=";", col.names=TRUE, dec=",")

#format as a table
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

cor_mo_table <-flattenCorrMatrix(cor_mo$r, cor_mo$P)
write.table(format(cor_mo_table, digits=2), file="2.Output/Korrelationsmatrix moral, as table.txt", sep=";", col.names=TRUE, dec=",")

#Korrelationsmatrix visualisieren
cor_mo.r <- cor_mo$r
cor_mo.p <- cor_mo$P
dim(cor_mo$P)
dim(cor_mo$r)
is.matrix(cor_mo.r)
str(cor_mo)

png(file="2.Output/Korrelationsmatrix mo,visualisiert.png", height=1800, width = 1800, res = 300)
cor_mo.r_plot1 <- corrplot(cor_mo.r, method= "circle", type = "upper", 
                           tl.col = "black", tl.srt = 90)
dev.off()

###Korrelationsmatrix Umverteilung----
items_i <- items %>% dplyr::select(starts_with("item_i"))
items_i <- as.matrix(items_i[sapply(items_i, is.numeric)])
is.numeric(items_i)
dim(items_i)
cor_i <- rcorr(as.matrix(items_i), type= "spearman")
cor_i$r

write.table(format(cor_i$r, digits=2),file="2.Output/Korrelationskoeffizienten_items Umverteil.txt", sep=";", col.names = TRUE, dec=",")

#extract the p-values 
cor_i$P
write.table(format(cor_i$P, digits=2), file="2.Output/Korrelationsmatrix_itemsmo p-value.txt", sep=";", col.names=TRUE, dec=",")

#format as a table
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

cor_i_table <-flattenCorrMatrix(cor_i$r, cor_i$P)
write.table(format(cor_i_table, digits=2), file="2.Output/Korrelationsmatrix umvert, as table.txt", sep=";", col.names=TRUE, dec=",")

#Korrelationsmatrix visualisieren
cor_i.r <- cor_i$r
cor_i.p <- cor_i$P
dim(cor_i$P)
dim(cor_i$r)
is.matrix(cor_i.r)
str(cor_i)

png(file="2.Output/Korrelationsmatrix umvert,visualisiert.png", height=1800, width = 1800, res = 300)
cor_i.r_plot1 <- corrplot(cor_i.r, method= "circle", type = "upper", 
                           tl.col = "black", tl.srt = 90)
dev.off()

###Korrelationsmatrix Migration----
items_mig <- items %>% dplyr::select(starts_with("item_m"))
items_mig <- as.matrix(items_mig[sapply(items_mig, is.numeric)])
is.numeric(items_mig)
dim(items_mig)
cor_mig <- rcorr(as.matrix(items_mig), type= "spearman")
cor_mig$r

write.table(format(cor_mig$r, digits=2),file="2.Output/Korrelationskoeffizienten_items Migration.txt", sep=";", col.names = TRUE, dec=",")

#extract the p-values 
cor_mig$P
write.table(format(cor_mig$P, digits=2), file="2.Output/Korrelationsmatrix_items mig p-value.txt", sep=";", col.names=TRUE, dec=",")

#format as a table
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

cor_mig_table <-flattenCorrMatrix(cor_mig$r, cor_mig$P)
write.table(format(cor_mig_table, digits=2), file="2.Output/Korrelationsmatrix mig, as table.txt", sep=";", col.names=TRUE, dec=",")

#Korrelationsmatrix visualisieren
cor_mig.r <- cor_mig$r
cor_mig.p <- cor_mig$P
dim(cor_mig$P)
dim(cor_mig$r)
is.matrix(cor_mig.r)
str(cor_mig)

png(file="2.Output/Korrelationsmatrix mig,visualisiert.png", height=1800, width = 1800, res = 300)
cor_mig.r_plot1 <- corrplot(cor_mig.r, method= "circle", type = "upper", 
                          tl.col = "black", tl.srt = 90)
dev.off()


