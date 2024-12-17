# Forschungsprojekt Polarisierung & Generationen --------------------------- 
# Edda Brandes
# last edit: 28.02.2022
# Step 4: Bring Data in a Format for Interpretation

# Prepation ------

## Load libraries ====
library(dplyr)
library(tidyverse)
library(hIRT)
library(emdist)
library(purrr)
library(kableExtra)
library(ggplot2)
library(stargazer)


## Read in Data ====
load(file="Data_all.Rdata")
load(file="hgrm_all.RDS")


# first look into parameters -----

#Grenzwert jüngste Person 1991 genalt, um beim Alterseffekt unteren Grenzwert festzulegen
age_genalt <- all %>% filter(year==1991) 
age_genalt  %>% group_by(gen) %>% summarize(min(age))

age_genalt2 <- all %>% filter(year==2018)
age_genalt2  %>% group_by(gen) %>% summarize(min(age))

mean <- all%>% group_by(gen) %>% summarise(across(starts_with("item"),~ mean(.x,na.rm= TRUE)))
write.table(mean, file="2.Output/means.txt", dec= ",", sep= "\t")

sd <- all%>% group_by(gen) %>% summarise(across(starts_with("item"),~ sd(.x,na.rm= TRUE)))
write.table(sd, file="2.Output/sd.txt", dec= ",", sep= "\t")

sumhgrm <- summary(hgrm_all, by_item=FALSE, digits=3)
summary(hgrm_all, by_item=FALSE, digits=3)
hgrm_all$coefficients
write.table(sumhgrm$mean_coefs, file="2.Output/summary hgrm.txt", dec= ",", sep= "\t")
write.table(sumhgrm$item_coefs, file="2.Output/summary item hgrm.txt", dec= ",", sep= "\t")



# Beschreibende Daten -------

## Plotting Theme ====
theme_own <- function(){
  theme_bw() +
    theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 17),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=20, face="bold", color = "slategrey"),
          panel.grid.major = element_line(colour = "grey70", size = 0.2),
          legend.position="bottom",
          legend.title = element_text(size=15),
          legend.text = element_text(size=15),
          strip.text = element_text(size = 15),
          strip.placement = "outside",
          panel.spacing = unit(1.5, "lines"),
          strip.background = element_blank()) }


# cut years in a certain number of intervalls. In sum 15 surveys, all 5 years 

all <- all %>%   mutate(year_dummy = cut(year,breaks = 5))
levels(all$year_dummy)

# data set group by yearseq & gen -> Erhebungsjahre auf yearseq gesetzt
all$yearseq <- all$year - 1991
all %>% 
  group_by(yearseq, gen) %>%
  summarise()

## Bar charts frequencies ====

### frequencies gen in percent over the years ####
table(all$gen)
levels(all$gen)<- c("alt", "jung")

ggplot(all, aes(x = gen, group= year)) +
  ggtitle("Generationenverteilung über die Zeit")+
  xlab("Zeit")+
  ylab("Anzahl")+
  geom_bar(aes(y = ..count.., fill = factor(..x..)), stat="count", show.legend=FALSE)+
  scale_fill_manual(values=c("steelblue4", "tan2"))+
  facet_grid(.~year) +
  theme_own()

ggsave("2.Output/gen_over_years_onerow.png", height=5, width=15, dpi = 600) 


ggplot(data=all, aes(x=year,fill=gen)) +
  geom_bar(stat="count", position="dodge") +
  geom_text( stat='count',aes(label=..count..),position=position_dodge(width=0.9), hjust=-0.25, angle=90, size=4.5) + 
  labs(y = "Anzahl", x= "Zeit") +
  scale_y_continuous(limits = c(0, 4000))+
  ggtitle("Generationenverteilung über die Zeit") +
  guides(fill=guide_legend(title="Generation"))+
  theme_own()+
  scale_fill_manual(values = c("steelblue4", "tan2"), labels = c("alt","jung"))+
  theme(legend.title = element_text(size=17), legend.text = element_text(size=17))
ggsave("2.Output/gen_over_years_onegraph.png", height=5, width=15, dpi = 600) 

### Bar chart frequencies gen in percent over the years in intervalls ####
ggplot(all, aes(x = gen, group= year_dummy)) +
  ggtitle("Generationenverteilung über die Zeit")+
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count",
           col = "darkseagreen2", fill = "darkseagreen2") +
  facet_wrap(~year_dummy) +
  theme_own()

ggsave("2.Output/gen_over_years_intervalls.png", height=9, width=9, dpi = 600) 

ggplot(all, aes(x = gen, group= year_dummy)) +
  ggtitle("Generationenverteilung über die Zeit")+
  geom_bar(aes(y = ..count.., fill = factor(..x..)), stat="count",
           col = "darkseagreen2", fill = "darkseagreen2") +
  facet_grid(.~year_dummy) +
  theme_own()

ggsave("2.Output/gen_over_years_intervalls_count.png", height=7, width=16, dpi = 600) 

# Verteilungen über Zeit -----

# neuer Data Frame mit Time Schätzern
newdf <- expand.grid(year = unique(all$yearseq),
                             gen = c("alt","jung"))

# die model matrix neu berechnen, alle Variablen werden in einer dichotomen Tabelle abgebildet
x_newdf <- model.matrix(~  gen*year,data = newdf)
z_newdf <- model.matrix(~  gen*year,data = newdf)

dim(x_newdf)
view(x_newdf)

# Schätzer berechnen
  #Parameter Schätzung für die Mittelswertgleichung
gamma <- coef_mean(hgrm_all)$Est
print(gamma)
coef_mean(hgrm_all)

  #Varianz in den Koeffizienten ausgeben lassen
lambda <- coef_var(hgrm_all)$Est
coef_var(hgrm_all)

  #Parameter als Index umformen
gamma_index <- with(hgrm_all, if(p>1) seq(sum(H)+1, sum(H)+p-1) else NULL)
lambda_index <- with(hgrm_all, if(q>1) seq(sum(H)+p, sum(H)+p+q-2) else NULL)
cov_gamma <- hgrm_all$vcov[gamma_index, gamma_index]
cov_lambda <- hgrm_all$vcov[lambda_index, lambda_index]


#neuen Dataframe mit den Estimators/ Schätzern erstellen 
  # für jedes Jahr + für jede Generation wird der mean estimator, der mean standarderror, der obere und untere Mittelwert, die geschätzte Varianz und die obere und untere Varianz gebildet
estdf <- newdf %>%
  mutate(year = rep(c(unique(all$year)), times=2),
         mean_est =  x_newdf %*% gamma,
         mean_se = apply(x_newdf[, -1], 1, function(x) sqrt(t(x) %*% cov_gamma %*% x)),
         mean_upper = mean_est + 1.96 * mean_se,
         mean_lower = mean_est - 1.96 * mean_se,
         var_est = exp(z_newdf %*% lambda),
         var_upper =  exp(z_newdf %*% lambda + 1.96*apply(z_newdf[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda %*% z))),
         var_lower =  exp(z_newdf %*% lambda - 1.96*apply(z_newdf[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda %*% z))))

# Modell benennen & subsetten
model.labs <- c("alt", "jung")
names(model.labs) <- c("alt", "jung")


## Modell 1 Generation Estimates over time =====

ggplot(estdf) +
  ggtitle("geschätzter Mittelwert und Verteilung um den Mittelwert") +
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
  ylim (-1, 1.2) +
  scale_color_manual(values=c("steelblue4", "tan2")) +
  scale_x_continuous(breaks=c(1991,1996, 2001, 2006, 2011, 2016)) +
  theme_own()+
  theme(plot.title=element_text(size=32), axis.text.x = element_text(size = 26, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 26), axis.title = element_text(size = 26), strip.text.x = element_text(size=26), legend.position="none")


ggsave("2.Output/geschätzter Mittelwert und Verteilung um den Mittelwert.png", height=11, width=19, dpi = 600) 

ggplot(estdf) +
  ggtitle("geschätzter Mittelwert") +
  geom_line(aes(x=year, y=mean_est, color=gen),size=1) +
  labs(x = "Jahre", 
       y = "politische Einstellung") +
  ylim (-0.5, 0.5) +
  scale_color_manual(values=c("steelblue4", "tan2")) +
  scale_x_continuous(breaks=c(1991,1996, 2001, 2006, 2011, 2016)) +
  theme_own()+
  theme(plot.title=element_text(size=18))

ggsave("2.Output/geschätzter Mittelwert.png", height=9, width=10, dpi = 600) 


## Modell 2 Generations over estimates certain years in comparison ====

### 4 Messzeitpunkte #### 

x <- seq(-5, 5, by = 0.01)
distdf <- filter(estdf, year %in% c(1991,1998,2008,2018)) %>%
  dplyr::select(mean_est, var_est, year, gen)
distdf_plot <- pmap_df(distdf, ~ data_frame(x = x,gen=..4, year = ..3, density = dnorm(x, ..1, sqrt(..2))))

ggplot(distdf_plot) +
  ggtitle("geschätzte Verteilung über Zeit") +
  geom_line(aes(x = x, y = density, color=gen),size=1.5) + 
  scale_color_manual(values=c("steelblue4","tan2"))+
  facet_wrap(year ~., scales = "free_y") +
  ylab("Verteilung") +
  labs("color"="Generation" ) +
  xlab("politische Einstellung") +
  ylim(0.0, 0.6) +
  theme_own()+
  theme(plot.title=element_text(size=24), axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18), axis.title = element_text(size = 18), legend.title = element_text(size=18),
        legend.text = element_text(size=18))

ggsave("2.Output/Distribution over time.pdf", width=20, height = 9)
ggsave("2.Output/Verteilung über Zeit, 4.png", width=9, height = 9, dpi=600)

### 2 Messzeitpunkte ####
q <- seq(-3, 3, by = 0.01)
distdf2 <- filter(estdf, year %in% c(1991,2018)) %>%
  dplyr::select(mean_est, var_est, year, gen)
distdf_plot2 <- pmap_df(distdf2, ~ data_frame(x = q,gen=..4, year = ..3, density = dnorm(x, ..1, sqrt(..2))))

ggplot(distdf_plot2) +
  ggtitle("geschätzte Verteilung über Zeit") +
  geom_line(aes(x = x, y = density, color=gen),size=1.5) + 
  scale_color_manual(values=c("steelblue4","tan2"))+
  facet_grid(year ~., scales = "free_y") +
  ylab("Verteilung") +
  labs("color"="Generation" ) +
  xlab("politische Einstellung") +
  ylim(0.0, 0.6) +
  theme_own()

ggsave("2.Output/Distribution over time, timespan.pdf", width=20, height = 9)
ggsave("2.Output/Verteilung über Zeit, 2.png", width=9, height = 9, dpi=300)

ggplot(distdf_plot2) +
  ggtitle("geschätzte Verteilung über Zeit") +
  geom_line(aes(x = x, y = density, color=gen),size=1.5) + 
  scale_color_manual(values=c("steelblue4","tan2"))+
  facet_wrap(year ~., scales = "free_y") +
  ylab("Verteilung") +
  labs("color"="Generation" ) +
  xlab("politische Einstellung") +
  ylim(0.0, 0.6) +
  theme_own()

ggsave("2.Output/Verteilung über Zeit, 2, nebeneinander.png", width=13, height = 7, dpi=300)

# Ideological Cohesion over time by gen ========

## 4 Messzeitpunkte ====
ggplot(data=filter(estdf, year %in%c(1991,1998,2008, 2018))) +
  geom_pointrange(aes(y=var_est,ymin=var_lower, ymax=var_upper,
                      x=gen, colour=gen),
                  position = position_dodge(.5),
                  size=1, alpha=0.7) +
  facet_grid(~ year) + 
  labs("color"="Generation") +
  scale_color_manual(values=c("steelblue4", "tan2")) +
  ylab("politische Einstellung") + xlab("") +
  ggtitle("ideologische Kohäsion über die Zeit")+
  theme_own()+
  theme(plot.title=element_text(size=26), axis.text.x = element_text(size = 22, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 22), axis.title = element_text(size = 22), legend.title = element_text(size=22),
        legend.text = element_text(size=22))

# Save
ggsave("2.Output/Cohesion_over_time.png", width=10, height = 5, dpi=600)


## 2 Messzeitpunkte ====
ggplot(data=filter(estdf, year %in%c(1991, 2018))) +
  geom_pointrange(aes(y=var_est,ymin=var_lower, ymax=var_upper,
                      x=gen, color=gen),
                  position = position_dodge(.5),
                  size=1, alpha=0.7) +
  facet_wrap(~ year) + 
  labs("color"="Generation") +
  scale_color_manual(values=c("steelblue4", "tan2")) +
  ylab("politische Einstellung") + xlab("") +
  ggtitle("ideologische Kohäsion über die Zeit")+
  theme_own()

ggsave("2.Output/Cohesion_over_time2.pdf", width=12, height = 12)
ggsave("2.Output/Cohesion_over_time2.png", width=8, height = 5, dpi=300)


# Polarization over time by party -----

# Calculate Bhat-Dist function


pol_gen <- function(y, data=estdf, what="dist"){
  
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
df_cases <- expand_grid(year=c(1991, 1992, 1994, 1996, 1998, 2000, 2002, 2004,
                               2006, 2008, 2010, 2012, 2014, 2016, 2018)) %>%
  rowwise() %>%
  mutate(pol_dist = pol_gen(y=year,data=estdf),
         pol_mean = pol_gen(y=year, what="mean",data=estdf)) %>%
  gather(var,val,-year) %>%
  mutate(var = case_when(var == "pol_dist" ~ "Distanz zwischen Verteilungen",
                         var == "pol_mean" ~ "Durchschnitt Unterschied zwischen Verteilungen"))



## Modell Polarisierung zwischen Generationengruppen ====
ggplot((df_cases)) +
  geom_point(aes(x=(year),y=(val)),
             size=3) +
  geom_line(aes(x=(year),y=(val)),
            size=1,alpha=0.5) +
  ylab("Polarisierung zwischen alt und jung") +
  xlab("Jahre") +
  ggtitle("Polarisierung zwischen den Generationengruppen")+
  facet_grid(~ var) +
  ylim(0.00, 0.2) +
  scale_x_continuous(breaks=c(1991,1996, 2001, 2006, 2011, 2016)) +
  theme_own()+
  theme(plot.title=element_text(size=24), axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18), axis.title = element_text(size = 18), legend.title = element_text(size=18),
        legend.text = element_text(size=18))


# Save
ggsave("2.Output/Polarisierung.png", width=12, height = 6, dpi=600)


