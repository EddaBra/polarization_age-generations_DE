# Forschungsprojekt Polarisierung & Generationen --------------------------- 
# Edda Brandes
# last edit: 28.02.2022
# Step 6:Interprete Data Themenkomplexs in Comparison

## Load libraries ====
library(dplyr)
library(tidyverse)
library(purrr)
library(kableExtra)
library(ggplot2)
library(stargazer)
library(hIRT)

## Read in Data ====
setwd("D:\\HU Berlin Module/BTW 21_Forschungsprojekt/4.Datenanalyse mit R")
load(file="Data_all.Rdata")
load(file="hgrm_all.RDS")
load(file="5.only redistribution/hgrm_all_re.RDS")
load(file="6.only traditional family/hgrm_all_f.RDS")
load(file="7.only moral/hgrm_all_m.RDS")
load(file="7.only moral/Data_all_m.Rdata")
load(file="8.only migration/hgrm_all_mig.RDS")


## Plotting Theme ====
theme_own <- function(){
  theme_bw() +
    theme(axis.text.x = element_text(size = 22, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 22),
          axis.title = element_text(size = 22),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=28, face="bold", color = "slategrey"),
          panel.grid.major = element_line(colour = "grey70", size = 0.4),
          legend.position="bottom",
          legend.title = element_text(size=22),
          legend.text = element_text(size=22),
          strip.text = element_text(size = 22),
          strip.placement = "outside",
          panel.spacing = unit(1.5, "lines"),
          strip.background = element_blank())}

theme(plot.title=element_text(size=28), axis.text.x = element_text(size = 22, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 20), axis.title = element_text(size = 20), legend.title = element_text(size=20),
      legend.text = element_text(size=20))

# cut years in a certain number of intervalls. In sum 15 surveys, all 5 years 

all <- all %>%   mutate(year_dummy = cut(year,breaks = 5))
levels(all$year_dummy)

# data set group by yearseq & gen -> Erhebungsjahre auf yearseq gesetzt
all$yearseq <- all$year - 1991
all %>% 
  group_by(yearseq, gen) %>%
  summarise()


# neuer Data Frame mit Time Schätzern ===
newdf <- expand.grid(year = unique(all$yearseq),
                     gen = c("alt","jung"))

# die model matrix neu berechnen, alle Variablen werden in einer dichotomen Tabelle abgebildet
x_newdf <- model.matrix(~  gen*year,data = newdf)
z_newdf <- model.matrix(~  gen*year,data = newdf)

dim(x_newdf)
view(x_newdf)

## Umverteilung Daten aufbereiten =======

# Schätzer berechnen
#Parameter Schätzung für die Mittelswertgleichung und Varianz ausgeben lassen
gamma_re <- coef_mean(hgrm_all_re)$Est
lambda_re <- coef_var(hgrm_all_re)$Est

#Parameter als Index umformen
gamma_re_index <- with(hgrm_all_re, if(p>1) seq(sum(H)+1, sum(H)+p-1) else NULL)
lambda_re_index <- with(hgrm_all_re, if(q>1) seq(sum(H)+p, sum(H)+p+q-2) else NULL)
cov_gamma_re <- hgrm_all_re$vcov[gamma_re_index, gamma_re_index]
cov_lambda_re <- hgrm_all_re$vcov[lambda_re_index, lambda_re_index]


#neuen Dataframe mit den Estimators/ Schätzern erstellen 
# für jedes Jahr + für jede Generation wird der mean estimator, der mean standarderror, der obere und untere Mittelwert, die geschätzte Varianz und die obere und untere Varianz gebildet
estdf_re <- newdf %>%
  mutate(year = rep(c(unique(all$year)), times=2),
         mean_est =  x_newdf %*% gamma_re,
         mean_se = apply(x_newdf[, -1], 1, function(x) sqrt(t(x) %*% cov_gamma_re %*% x)),
         mean_upper = mean_est + 1.96 * mean_se,
         mean_lower = mean_est - 1.96 * mean_se,
         var_est = exp(z_newdf %*% lambda_re),
         var_upper =  exp(z_newdf %*% lambda_re + 1.96*apply(z_newdf[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda_re %*% z))),
         var_lower =  exp(z_newdf %*% lambda_re - 1.96*apply(z_newdf[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda_re %*% z))))

# Modell benennen & subsetten
model.labs <- c("alt", "jung")
names(model.labs) <- c("alt", "jung")

### Modell Varianz und Mittelwerte über Zeit =====

ggplot(estdf_re) +
  ggtitle("geschätzter Mittelwert und Varianz Themenkomplex Umverteilung") +
  geom_line(aes(x=year, y=mean_est, color=gen),size=1) +
  geom_linerange(aes(x=year,ymin=qnorm(.45,mean_est,sqrt(var_est)),
                     ymax=qnorm(.55,mean_est,sqrt(var_est)), 
                     col=gen), size=1.7) +
  geom_linerange(aes(x=year,ymin=qnorm(.65,mean_est,sqrt(var_est)),
                     ymax=qnorm(.35,mean_est,sqrt(var_est)), 
                     col=gen), size=1.7, alpha=0.4) +
  geom_linerange(aes(x=year,ymin=qnorm(.25,mean_est,sqrt(var_est)),
                     ymax=qnorm(.75,mean_est,sqrt(var_est)), 
                     col=gen), size=1.7, alpha=0.4) + 
  facet_wrap(~gen, labeller = labeller(gen = model.labs)) +
  labs(x = "Jahre", 
       y = "politische Einstellung") +
  guides(fill=guide_legend(title="Generation"))+
  ylim (-1.2, 1.4) +
  scale_color_manual(values=c("steelblue4", "tan2")) +
  scale_x_continuous(breaks=c(1991,1996, 2001, 2006, 2011, 2016)) +
  theme_own()+
  theme(plot.title=element_text(size=20))

ggsave("2.Output/geschätzter Mittelwert und Varianz Themenkomplex Umverteilung.png", height=18, width=32, dpi = 600)

### Modell Verteilung über Zeit 2 Messzeitpunkte====

x <- seq(-5, 5, by = 0.01)
distdf_re <- filter(estdf_re, year %in% c(1991,2018)) %>%
  dplyr::select(mean_est, var_est, year, gen)
distdf_re_plot <- pmap_df(distdf_re, ~ data_frame(x = x,gen=..4, year = ..3, density = dnorm(x, ..1, sqrt(..2))))


ggplot(distdf_re_plot) +
  ggtitle("  Umverteilung") +
  geom_line(aes(x = x, y = density, color=gen),size=2.5) + 
  scale_color_manual(values=c("steelblue4","tan2"))+
  facet_wrap(year ~., scales = "free_y") +
  ylab("Verteilung") +
  labs("color"="Generation" ) +
  xlab("politische Einstellung") +
  guides(fill=guide_legend(title="Generation"))+
  ylim(0.0, 0.6) +
  theme_own()


ggsave("2.Output/Verteilung über Zeit, Umverteilung, 2.png", width=14, height = 10, dpi=600)



### Modell Distanzmessung zwischen Generationen =======

# Calculate Bhat-Dist function

pol_gen_re <- function(y, data=estdf_re, what="dist"){
  
  df_g1 <- filter(data, gen == "alt", year==y)
  df_g2 <- filter(data, gen =="jung", year ==y)
  
  Bhat_dist <- function(mean,sd){
    0.25 * log(0.25 * ((sd[1]^2)/(sd[2]^2) + (sd[2]^2)/(sd[1]^2) + 2 )) +
      0.25 * (((mean[1] - mean[2])^2)/(sd[1]^2 + sd[2]^2))
  }
  
  if(what=="dist"){
    return(Bhat_dist(c(df_g1$mean_est,df_g2$mean_est),
                     c(df_g1$var_est,df_g2$var_est)))}
  if (what == "mean") {
    return(as.numeric((df_g1$mean_est - df_g2$mean_est)^2))
  }
  
}

# Calulacte emb for all cases
df_cases_re <- expand_grid(year=c(1991, 1994, 1996, 1998, 2000, 2004,
                               2008, 2010, 2014, 2018)) %>%
  rowwise() %>%
  mutate(pol_dist = pol_gen_re(y=year,data=estdf_re),
         pol_mean = pol_gen_re(y=year, what="mean",data=estdf_re)) %>%
  gather(var,val,-year) %>%
  mutate(var = case_when(var == "pol_dist" ~ "Distanz zwischen Verteilungen",
                         var == "pol_mean" ~ "Durchschnitt zwischen Verteilungen"))

ggplot((df_cases_re)) +
  geom_point(aes(x=(year),y=(val)),
             size=3) +
  geom_line(aes(x=(year),y=(val)),
            size=1,alpha=0.5) +
  ylab("Polarisierung") +
  xlab("Jahre") +
  ggtitle("Umverteilung")+
facet_grid(~ var) +
  ylim(0.00, 0.2) +
  scale_x_continuous(breaks=c(1991,1996, 2001, 2006, 2011, 2016)) +
  theme_own() 


# Save
ggsave("2.Output/  Umverteilung.png", width=12, height = 6, dpi=600)


## trad. Frauenbild ======
# Schätzer berechnen
#Parameter Schätzung für die Mittelswertgleichung und Varianz ausgeben lassen
gamma_f <- coef_mean(hgrm_all_f)$Est
lambda_f <- coef_var(hgrm_all_f)$Est

#Parameter als Index umformen
gamma_f_index <- with(hgrm_all_f, if(p>1) seq(sum(H)+1, sum(H)+p-1) else NULL)
lambda_f_index <- with(hgrm_all_f, if(q>1) seq(sum(H)+p, sum(H)+p+q-2) else NULL)
cov_gamma_f <- hgrm_all_f$vcov[gamma_f_index, gamma_f_index]
cov_lambda_f <- hgrm_all_f$vcov[lambda_f_index, lambda_f_index]


#neuen Dataframe mit den Estimators/ Schätzern erstellen 
# für jedes Jahr + für jede Generation wird der mean estimator, der mean standarderror, der obere und untere Mittelwert, die geschätzte Varianz und die obere und untere Varianz gebildet
estdf_f <- newdf %>%
  mutate(year = rep(c(unique(all$year)), times=2),
         mean_est =  x_newdf %*% gamma_f,
         mean_se = apply(x_newdf[, -1], 1, function(x) sqrt(t(x) %*% cov_gamma_f %*% x)),
         mean_upper = mean_est + 1.96 * mean_se,
         mean_lower = mean_est - 1.96 * mean_se,
         var_est = exp(z_newdf %*% lambda_f),
         var_upper =  exp(z_newdf %*% lambda_f + 1.96*apply(z_newdf[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda_f %*% z))),
         var_lower =  exp(z_newdf %*% lambda_f - 1.96*apply(z_newdf[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda_f %*% z))))

# Modell benennen & subsetten
model.labs <- c("alt", "jung")
names(model.labs) <- c("alt", "jung")

### Modell Varianz und Mittelwerte über Zeit =====

ggplot(estdf_f) +
  ggtitle("geschätzter Mittelwert und Varianz Themenkomplex Familienbild") +
  geom_line(aes(x=year, y=mean_est, color=gen),size=1) +
  geom_linerange(aes(x=year,ymin=qnorm(.45,mean_est,sqrt(var_est)),
                     ymax=qnorm(.55,mean_est,sqrt(var_est)), 
                     col=gen), size=1.7) +
  geom_linerange(aes(x=year,ymin=qnorm(.65,mean_est,sqrt(var_est)),
                     ymax=qnorm(.35,mean_est,sqrt(var_est)), 
                     col=gen), size=1.7, alpha=0.4) +
  geom_linerange(aes(x=year,ymin=qnorm(.25,mean_est,sqrt(var_est)),
                     ymax=qnorm(.75,mean_est,sqrt(var_est)), 
                     col=gen), size=1.7, alpha=0.4) + 
  facet_wrap(~gen, labeller = labeller(gen = model.labs)) +
  labs(x = "Jahre", 
       y = "politische Einstellung") +
  ylim (-1.2, 1.4) +
  scale_color_manual(values=c("steelblue4", "tan2")) +
  scale_x_continuous(breaks=c(1991,1996, 2001, 2006, 2011, 2016)) +
  theme_own()+
  theme(plot.title=element_text(size=15))

ggsave("2.Output/geschätzter Mittelwert und Varianz Themenkomplex Familienbild.png", height=9, width=17, dpi = 600)

### Modell Verteilung über Zeit 2 Messzeitpunkte====

distdf_f <- filter(estdf_f, year %in% c(1991,2018)) %>%
  dplyr::select(mean_est, var_est, year, gen)
distdf_f_plot <- pmap_df(distdf_f, ~ data_frame(x = x,gen=..4, year = ..3, density = dnorm(x, ..1, sqrt(..2))))

ggplot(distdf_f_plot) +
  ggtitle("Familienbild") +
  geom_line(aes(x = x, y = density, color=gen),size=2.5) + 
  scale_color_manual(values=c("steelblue4","tan2"))+
  facet_wrap(year ~., scales = "free_y") +
  ylab("Verteilung") +
  labs("color"="Generation" ) +
  xlab("politische Einstellung") +   guides(fill=guide_legend(title="Generation"))+
  ylim(0.0, 0.6) +
  theme_own()


ggsave("2.Output/Verteilung über Zeit, Familienbild, 2.png",  width=14, height = 10, dpi=600)



### Modell Distanzmessung zwischen Generationen =======

# Calculate Bhat-Dist function

pol_gen_f <- function(y, data=estdf_f, what="dist"){
  
  df_g1 <- filter(data, gen == "alt", year==y)
  df_g2 <- filter(data, gen =="jung", year ==y)
  
  Bhat_dist <- function(mean,sd){
    0.25 * log(0.25 * ((sd[1]^2)/(sd[2]^2) + (sd[2]^2)/(sd[1]^2) + 2 )) +
      0.25 * (((mean[1] - mean[2])^2)/(sd[1]^2 + sd[2]^2))
  }
  
  if(what=="dist"){
    return(Bhat_dist(c(df_g1$mean_est,df_g2$mean_est),
                     c(df_g1$var_est,df_g2$var_est)))}
  if (what == "mean") {
    return(as.numeric((df_g1$mean_est - df_g2$mean_est)^2))
  }
  
}

# Calulacte emb for all cases
df_cases_f <- expand_grid(year=c(1991, 1992, 1996, 2000, 2002, 2004, 2006,
                                  2008, 2010, 2012, 2014, 2016,2018)) %>%
  rowwise() %>%
  mutate(pol_dist = pol_gen_f(y=year,data=estdf_f),
         pol_mean = pol_gen_f(y=year, what="mean",data=estdf_f)) %>%
  gather(var,val,-year) %>%
  mutate(var = case_when(var == "pol_dist" ~ "Distanz zwischen Verteilungen",
                         var == "pol_mean" ~ "Durchschnitt zwischen Verteilungen"))

ggplot((df_cases_f)) +
  geom_point(aes(x=(year),y=(val)),
             size=3) +
  geom_line(aes(x=(year),y=(val)),
            size=1,alpha=0.5) +
  ylab("Polarisierung") +
  xlab("Jahre") +
  ylim(0,0.2)+
  ggtitle("  Familienbild")+
  facet_grid(~ var) +
    scale_x_continuous(breaks=c(1991,1996, 2001, 2006, 2011, 2016)) +
  theme_own() 


# Save
ggsave("2.Output/  Familienbild.png", width=12, height = 6, dpi=600)

## Migration ======

# Schätzer berechnen
#Parameter Schätzung für die Mittelswertgleichung und Varianz ausgeben lassen
gamma_mig <- coef_mean(hgrm_all_mig)$Est
lambda_mig <- coef_var(hgrm_all_mig)$Est

#Parameter als Index umformen
gamma_mig_index <- with(hgrm_all_mig, if(p>1) seq(sum(H)+1, sum(H)+p-1) else NULL)
lambda_mig_index <- with(hgrm_all_mig, if(q>1) seq(sum(H)+p, sum(H)+p+q-2) else NULL)
cov_gamma_mig <- hgrm_all_mig$vcov[gamma_mig_index, gamma_mig_index]
cov_lambda_mig <- hgrm_all_mig$vcov[lambda_mig_index, lambda_mig_index]


#neuen Dataframe mit den Estimators/ Schätzern erstellen 
# für jedes Jahr + für jede Generation wird der mean estimator, der mean standarderror, der obere und untere Mittelwert, die geschätzte Varianz und die obere und untere Varianz gebildet
estdf_mig <- newdf %>%
  mutate(year = rep(c(unique(all$year)), times=2),
         mean_est =  x_newdf %*% gamma_mig,
         mean_se = apply(x_newdf[, -1], 1, function(x) sqrt(t(x) %*% cov_gamma_mig %*% x)),
         mean_upper = mean_est + 1.96 * mean_se,
         mean_lower = mean_est - 1.96 * mean_se,
         var_est = exp(z_newdf %*% lambda_mig),
         var_upper =  exp(z_newdf %*% lambda_mig + 1.96*apply(z_newdf[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda_mig %*% z))),
         var_lower =  exp(z_newdf %*% lambda_mig - 1.96*apply(z_newdf[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda_mig %*% z))))

# Modell benennen & subsetten
model.labs <- c("alt", "jung")
names(model.labs) <- c("alt", "jung")

### Modell Varianz und Mittelwerte über Zeit =====

ggplot(estdf_mig) +
  ggtitle("geschätzter Mittelwert und Varianz Themenkomplex Migration") +
  geom_line(aes(x=year, y=mean_est, color=gen),size=1) +
  geom_linerange(aes(x=year,ymin=qnorm(.45,mean_est,sqrt(var_est)),
                     ymax=qnorm(.55,mean_est,sqrt(var_est)), 
                     col=gen), size=1.7) +
  geom_linerange(aes(x=year,ymin=qnorm(.65,mean_est,sqrt(var_est)),
                     ymax=qnorm(.35,mean_est,sqrt(var_est)), 
                     col=gen), size=1.7, alpha=0.4) +
  geom_linerange(aes(x=year,ymin=qnorm(.25,mean_est,sqrt(var_est)),
                     ymax=qnorm(.75,mean_est,sqrt(var_est)), 
                     col=gen), size=1.7, alpha=0.4) + 
  facet_wrap(~gen, labeller = labeller(gen = model.labs)) +
  labs(x = "Jahre", 
       y = "politische Einstellung") +
  ylim (-1.2, 1.4) +
  scale_color_manual(values=c("steelblue4", "tan2")) +
  scale_x_continuous(breaks=c(1991,1996, 2001, 2006, 2011, 2016)) +
  theme_own()+
  theme(plot.title=element_text(size=16))

ggsave("2.Output/geschätzter Mittelwert und Varianz Themenkomplex Migration.png", height=9, width=17, dpi = 600)

### Modell Verteilung über Zeit 2 Messzeitpunkte====

distdf_mig <- filter(estdf_mig, year %in% c(1991,2018)) %>%
  dplyr::select(mean_est, var_est, year, gen)
distdf_mig_plot <- pmap_df(distdf_mig, ~ data_frame(x = x,gen=..4, year = ..3, density = dnorm(x, ..1, sqrt(..2))))

ggplot(distdf_mig_plot) +
  ggtitle("Migration") +
  geom_line(aes(x = x, y = density, color=gen),size=2.5) + 
  scale_color_manual(values=c("steelblue4","tan2"))+
  facet_wrap(year ~., scales = "free_y") +
  ylab("Verteilung") +
  labs("color"="Generation" ) +
  xlab("politische Einstellung") + guides(fill=guide_legend(title="Generation"))+
  ylim(0.0, 0.6) +
  theme_own()

ggsave("2.Output/Verteilung über Zeit, Migration, 2.png",  width=14, height = 10, dpi=600)



### Modell Distanzmessung zwischen Generationen =======

# Calculate Bhat-Dist function

pol_gen_mig <- function(y, data=estdf_mig, what="dist"){
  
  df_g1 <- filter(data, gen == "alt", year==y)
  df_g2 <- filter(data, gen =="jung", year ==y)
  
  Bhat_dist <- function(mean,sd){
    0.25 * log(0.25 * ((sd[1]^2)/(sd[2]^2) + (sd[2]^2)/(sd[1]^2) + 2 )) +
      0.25 * (((mean[1] - mean[2])^2)/(sd[1]^2 + sd[2]^2))
  }
  
  if(what=="dist"){
    return(Bhat_dist(c(df_g1$mean_est,df_g2$mean_est),
                     c(df_g1$var_est,df_g2$var_est)))}
  if (what == "mean") {
    return(as.numeric((df_g1$mean_est - df_g2$mean_est)^2))
  }
  
}

# Calulacte emb for all cases
df_cases_mig <- expand_grid(year=c(1991, 1992, 1994, 1996, 2000, 2002, 2006,
                                 2010, 2012, 2016)) %>%
  rowwise() %>%
  mutate(pol_dist = pol_gen_mig(y=year,data=estdf_mig),
         pol_mean = pol_gen_mig(y=year, what="mean",data=estdf_mig)) %>%
  gather(var,val,-year) %>%
  mutate(var = case_when(var == "pol_dist" ~ "Distanz zwischen Verteilungen",
                         var == "pol_mean" ~ "Durchschnitt zwischen Verteilungen"))

ggplot((df_cases_mig)) +
  geom_point(aes(x=(year),y=(val)),
             size=3) +
  geom_line(aes(x=(year),y=(val)),
            size=1,alpha=0.5) +
  ylab("Polarisierung") +
  xlab("Jahre") +
  ylim(0,0.2)+
  ggtitle("  Migration")+
  facet_grid(~ var) +
  scale_x_continuous(breaks=c(1991,1996, 2001, 2006, 2011, 2016)) +
  theme_own() 


# Save
ggsave("2.Output/  Migration.png", width=12, height = 6, dpi=600)

## Moral ====
# Schätzer berechnen
#Parameter Schätzung für die Mittelswertgleichung und Varianz ausgeben lassen
gamma_m <- coef_mean(hgrm_all_m)$Est
lambda_m <- coef_var(hgrm_all_m)$Est

#Parameter als Index umformen
gamma_m_index <- with(hgrm_all_m, if(p>1) seq(sum(H)+1, sum(H)+p-1) else NULL)
lambda_m_index <- with(hgrm_all_m, if(q>1) seq(sum(H)+p, sum(H)+p+q-2) else NULL)
cov_gamma_m <- hgrm_all_m$vcov[gamma_m_index, gamma_m_index]
cov_lambda_m <- hgrm_all_m$vcov[lambda_m_index, lambda_m_index]


#neuen Dataframe mit den Estimators/ Schätzern erstellen 
# für jedes Jahr + für jede Generation wird der mean estimator, der mean standarderror, der obere und untere Mittelwert, die geschätzte Varianz und die obere und untere Varianz gebildet
estdf_m <- newdf %>%
  mutate(year = rep(c(unique(all$year)), times=2),
         mean_est =  x_newdf %*% gamma_m,
         mean_se = apply(x_newdf[, -1], 1, function(x) sqrt(t(x) %*% cov_gamma_m %*% x)),
         mean_upper = mean_est + 1.96 * mean_se,
         mean_lower = mean_est - 1.96 * mean_se,
         var_est = exp(z_newdf %*% lambda_m),
         var_upper =  exp(z_newdf %*% lambda_m + 1.96*apply(z_newdf[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda_m %*% z))),
         var_lower =  exp(z_newdf %*% lambda_m - 1.96*apply(z_newdf[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda_m %*% z))))

# Modell benennen & subsetten
model.labs <- c("alt", "jung")
names(model.labs) <- c("alt", "jung")

### Modell Varianz und Mittelwerte über Zeit =====

ggplot(estdf_m) +
  ggtitle("geschätzter Mittelwert und Varianz Themenkomplex Moral") +
  geom_line(aes(x=year, y=mean_est, color=gen),size=1) +
  geom_linerange(aes(x=year,ymin=qnorm(.45,mean_est,sqrt(var_est)),
                     ymax=qnorm(.55,mean_est,sqrt(var_est)), 
                     col=gen), size=1.7) +
  geom_linerange(aes(x=year,ymin=qnorm(.65,mean_est,sqrt(var_est)),
                     ymax=qnorm(.35,mean_est,sqrt(var_est)), 
                     col=gen), size=1.7, alpha=0.4) +
  geom_linerange(aes(x=year,ymin=qnorm(.25,mean_est,sqrt(var_est)),
                     ymax=qnorm(.75,mean_est,sqrt(var_est)), 
                     col=gen), size=1.7, alpha=0.4) + 
  facet_wrap(~gen, labeller = labeller(gen = model.labs)) +
  labs(x = "Jahre", 
       y = "politische Einstellung") +
  ylim (-1.2, 1.4) +
  scale_color_manual(values=c("steelblue4", "tan2")) +
  scale_x_continuous(breaks=c(1991,1996, 2001, 2006, 2011, 2016)) +
  theme_own()+
  theme(plot.title=element_text(size=18))

ggsave("2.Output/geschätzter Mittelwert und Varianz Themenkomplex Moral.png", height=9, width=17, dpi = 600)

### Modell Verteilung über Zeit 2 Messzeitpunkte====

distdf_m <- filter(estdf_m, year %in% c(1991,2018)) %>%
  dplyr::select(mean_est, var_est, year, gen)
distdf_m_plot <- pmap_df(distdf_m, ~ data_frame(x = x,gen=..4, year = ..3, density = dnorm(x, ..1, sqrt(..2))))

ggplot(distdf_m_plot) +
  ggtitle("Moral") +
  geom_line(aes(x = x, y = density, color=gen),size=2.5) + 
  scale_color_manual(values=c("steelblue4","tan2"))+
  facet_wrap(year ~., scales = "free_y") +
  ylab("Verteilung") +
  labs("color"="Generation" ) +
  xlab("politische Einstellung") +   guides(fill=guide_legend(title="Generation"))+
  ylim(0.0, 0.6) +
  theme_own()

ggsave("2.Output/Verteilung über Zeit, Moral, 2.png",  width=14, height = 10, dpi=600)



### Modell Distanzmessung zwischen Generationen =======

# Calculate Bhat-Dist function

pol_gen_m <- function(y, data=estdf_m, what="dist"){
  
  df_g1 <- filter(data, gen == "alt", year==y)
  df_g2 <- filter(data, gen =="jung", year ==y)
  
  Bhat_dist <- function(mean,sd){
    0.25 * log(0.25 * ((sd[1]^2)/(sd[2]^2) + (sd[2]^2)/(sd[1]^2) + 2 )) +
      0.25 * (((mean[1] - mean[2])^2)/(sd[1]^2 + sd[2]^2))
  }
  
  if(what=="dist"){
    return(Bhat_dist(c(df_g1$mean_est,df_g2$mean_est),
                     c(df_g1$var_est,df_g2$var_est)))}
  if (what == "mean") {
    return(as.numeric((df_g1$mean_est - df_g2$mean_est)^2))
  }
  
}

# Calulacte emb for all cases
df_cases_m <- expand_grid(year=c(1992, 1996, 2000, 2002, 2006,
                                   2012, 2014)) %>%
  rowwise() %>%
  mutate(pol_dist = pol_gen_m(y=year,data=estdf_m),
         pol_mean = pol_gen_m(y=year, what="mean",data=estdf_m)) %>%
  gather(var,val,-year) %>%
  mutate(var = case_when(var == "pol_dist" ~ "Distanz zwischen Verteilungen",
                         var == "pol_mean" ~ "Durchschnitt zwischen Verteilungen"))

ggplot((df_cases_m)) +
  geom_point(aes(x=(year),y=(val)),
             size=3) +
  geom_line(aes(x=(year),y=(val)),
            size=1,alpha=0.5) +
  ylab("Polarisierung") +
  xlab("Jahre") +
  ggtitle("Moral")+
  facet_grid(~ var) +
  ylim(0,0.2)+
  scale_x_continuous(breaks=c(1991,1996, 2001, 2006, 2011, 2016)) +
  theme_own() 


# Save
ggsave("2.Output/  Moral.png", width=12, height = 6, dpi=600)


       