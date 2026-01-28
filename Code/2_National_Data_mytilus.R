# Script to aggregate datasets, adjust sigmoid models and calculate general metrics on mussel mortality in France
# 2026-01-14, Ifremer
# Depends on original datasets Dataset_1.csv, Dataset_2.csv, Dataset_3.csv, Dataset_4.csv, Dataset_5.csv,
# Dataset_6.csv
# Produce Raw_dataset.csv, Clean_dataset.csv and death_predicted.csv
# Produce Figures C, D, E and F
# Produce Supplementary material Figure 2 and Supplementary material Figure 3 : 

# Install necessary libraries and load it ----
list.of.packages <- c("tidyverse", "readxl","cowplot","nls.multstart","broom","ggpubr","viridis","formattable","zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(readxl) 
library(cowplot)
library(nls.multstart)
library(broom)
library(ggpubr)
library(viridis)
library(formattable)
library(zoo)


# Data collection ----
## Dataset 1 ----
# Dataset 1 loading
Dataset_1 <- read.csv(file.path("Data","original_datasets","Dataset_1.csv"), header=TRUE, sep=";")
Dataset_1 <- Dataset_1 %>%filter(!(is.na(alive_nb) & is.na(dead_nb) & is.na(cum_morta)))

# Addition of the column "init_nb": 
Dataset_1<-Dataset_1%>%mutate(init_nb=dead_nb+alive_nb)
Dataset_1<- Dataset_1 %>%
  group_by(year, location) %>%
  mutate(init_nb = max(init_nb)) %>%
  ungroup()

# Creation of several columns :  
Dataset_1<-Dataset_1%>% mutate(spat_year=`year`-1)
Dataset_1<-Dataset_1%>%rename(Site=location,Date=day,Campaign=year)
Dataset_1$Date <- dmy(Dataset_1$Date)
Dataset_1$Batch<-"NA"
Dataset_1$site_spat<-"Pertuis_Breton"
Dataset_1$Specie<-"Edulis"
Dataset_1$Comments<-"NA"
Dataset_1$Support<-"bag"
Dataset_1$Operator<-"Operator_1"
Dataset_1$MR<-"NA"
Dataset_1<-Dataset_1 %>% dplyr::select(Site,Date,spat_year, alive_nb,dead_nb,init_nb,Batch,site_spat,Specie,Support,Operator,Comments,Campaign,MR)

## Dataset 2 ----
# Dataset 2 loading
Dataset_2 <- read.csv(file.path("Data","original_datasets","Dataset_2.csv"), header=TRUE, sep=";", skip=1, fileEncoding = "Windows-1252")
Dataset_2$Support <- ifelse (is.na(Dataset_2$Panier)| Dataset_2$Panier =="","Plate","bag")
Dataset_2$Date <- dmy(Dataset_2$Date)

# We Rename and complete the Batch columns 
Dataset_2$Batch <- as.character(Dataset_2$Lot)
# For all sites other than '3- Lannion', the value of the Batch column is set to 'Rep'
Dataset_2$Batch[Dataset_2$Site!="3- Lannion"] <- "Rep"
Dataset_2$Batch <- ifelse(Dataset_2$Site == "3- Lannion" & Dataset_2$AnneeNais >= "2020", "Rep", Dataset_2$Batch)
# Adding years to the Batch name of'3- Lannion' for the years in the "Annee_Nais" columns before 2017 
Dataset_2$Batch[Dataset_2$Site=="3- Lannion" & Dataset_2$AnneeNais < 2017] <- paste(Dataset_2$Batch[Dataset_2$Site=="3- Lannion" & Dataset_2$AnneeNais < 2017],Dataset_2$AnneeNais[Dataset_2$Site=="3- Lannion" & Dataset_2$AnneeNais < 2017]) 

#Standardization of the content in the "Origine" column
Dataset_2 <- Dataset_2 %>% mutate(Origine = case_when(
  Origine == "Penestin" ~ "Pénestin",
  Origine == "penestin" ~ "Pénestin",
  TRUE ~ Origine))

# We recalculate the number of dead and living individuals counted in the two bags and adjust the result to 100
Dataset_2 <- Dataset_2 %>% group_by(Site,Date,Batch,AnneeNais,Origine,Espece,Support,Type)%>%summarise(NbVivantes=sum(NbVivantes),NbMortes=sum(NbMortes),init_nb=sum(NbInit))%>% ungroup()
Dataset_2$NbVivantes <- Dataset_2$NbVivantes/Dataset_2$init_nb*100 
Dataset_2$NbMortes <-Dataset_2$NbMortes/Dataset_2$init_nb*100
Dataset_2$init_nb <- 100 
Dataset_2 <- subset(Dataset_2, !(is.na(NbVivantes) & is.na(NbMortes)))

# Additional calculations : filling in missing data (NbMortes)
Dataset_2$alive_Calc <- Dataset_2$NbVivantes
Dataset_2$dead_Calc <- Dataset_2$NbMortes
Dataset_2<-Dataset_2%>% arrange(Site, AnneeNais,Batch,Origine,Espece,Date)
Dataset_2$dead_Calc<-ifelse(is.na(Dataset_2$dead_Calc), (Dataset_2$init_nb - Dataset_2$NbVivantes),Dataset_2$NbMortes)

#Rename and creation of several columns : 
Dataset_2<-Dataset_2%>%rename(dead_nb=dead_Calc,alive_nb=alive_Calc,spat_year=AnneeNais, Specie=Espece,site_spat=Origine,Comments=Type)
Dataset_2$Site[Dataset_2$Site %in% c("2- Aber Benoit","2- Aber Wrac’h")] <- "2- Les abers"
Dataset_2$Site[Dataset_2$Site == "1b- Riv. De Daoulas"] <- "1- Rade de Brest"
Dataset_2$Site[Dataset_2$Site == "7- Le Vivier"] <- "7- BMSM"
Dataset_2$Operator<-"Operator_2"
Dataset_2<-Dataset_2%>%mutate(Campaign=spat_year+1)

#Dataset_2$Comments<-"NA"
Dataset_2$MR<-"NA"
Dataset_2<-Dataset_2 %>% dplyr::select(Site,Date,spat_year, alive_nb,dead_nb,init_nb,Batch,site_spat,Specie,Support,Operator,Comments,Campaign,MR)

## Dataset 3 ----
#Dataset loading
Dataset_3 <- read.csv(file.path("Data","original_datasets","Dataset_3.csv"), header=TRUE, sep=";", fileEncoding = "Windows-1252")
Dataset_3$Date <- dmy(Dataset_3$Date)

#We remove rows without counting data when bags have been lost
Dataset_3 <- Dataset_3 %>%filter(!(Commentaires == "Plus de poches" & is.na(Nb_vivantes) & is.na(Nb_mortes_naturelles)))

#Addition of the column "init_nb":
Dataset_3<-Dataset_3%>%mutate(init_nb=Nb_vivantes+Nb_mortes_naturelles+Nb_mortes_prédation)
Dataset_3 <- Dataset_3 %>%
  group_by(Campagne, Site, Poche) %>%
  mutate(init_nb = max(init_nb, na.rm = TRUE)) %>%
  ungroup()

#Rename and creation of several columns : 
Dataset_3<-Dataset_3%>% mutate(spat_year=`Campagne`-1)
Dataset_3 <- Dataset_3 %>%filter(!(is.na(Nb_vivantes) & is.na(Nb_mortes_naturelles)))
Dataset_3<-Dataset_3%>%rename(Batch=Poche,dead_nb=Nb_mortes_naturelles,alive_nb=Nb_vivantes,Comments=Commentaires)
Dataset_3$Operator<-"Operator_3"
Dataset_3<-Dataset_3%>%mutate(Campaign=spat_year+1)
Dataset_3$Specie<-"Edulis"
Dataset_3$Support<-"bag"
Dataset_3$MR<-"NA"
Dataset_3<-Dataset_3 %>% mutate(site_spat = ifelse(Campagne< 2022, "Pertuis_Breton", "local"))
Dataset_3<-Dataset_3 %>% dplyr::select(Site,Date,spat_year, alive_nb,dead_nb,init_nb,Batch,site_spat,Specie,Support,Operator,Comments,Campaign,MR)

## Dataset 4 ----
#Dataset loading
Dataset_4 <- read.csv(file.path("Data","original_datasets","Dataset_4.csv"), header=TRUE, sep=";")

Dataset_4$Dates <- dmy(Dataset_4$Dates)
#Rename of several columns
Dataset_4<-Dataset_4%>%rename(spat_year=`Annee.naissain`,Comments=Visite,Date=Dates,dead_nb='Nb.de.mortes',alive_nb='nb.de.vivantes')
Dataset_4$dead_nb<-as.numeric(Dataset_4$dead_nb)
Dataset_4$alive_nb<-as.numeric(Dataset_4$alive_nb)

#Addition of the column "init_nb"
Dataset_4<-Dataset_4%>%mutate(init_nb=dead_nb+alive_nb)
Dataset_4 <- Dataset_4 %>%filter(!(is.na(dead_nb) & is.na(alive_nb)))

#Addition of several columns 
Dataset_4 <- Dataset_4 %>%mutate(site_spat=ifelse(spat_year=="2017","Pénestin",ifelse(spat_year=="2019","Oléron", "NA")))
Dataset_4$Operator<-"Operator_4"
Dataset_4$Specie<-"Edulis"
Dataset_4<-Dataset_4%>%mutate(Campaign=spat_year+1)
Dataset_4$Support<-"bag"
Dataset_4$Batch<-"NA"
Dataset_4$MR<-"NA"
Dataset_4<-Dataset_4 %>% dplyr::select(Site,Date,spat_year, alive_nb,dead_nb,init_nb,Batch,site_spat,Specie,Support,Operator,Comments,Campaign,MR)

## Dataset 5 ----
#Dataset loading
Dataset_5 <- read.csv(file.path("Data","original_datasets","Dataset_5.csv"), header=TRUE, sep=";")

#The estimated number of dead and alive individuals based on the mortality percentage is rounded 
Dataset_5$`Nbre.vivantes`<-round(Dataset_5$`Nbre.vivantes`)
Dataset_5$`Nbres.mortes`<-round(Dataset_5$`Nbres.mortes`)

#Rename and addition of several columns
Dataset_5<-Dataset_5%>%rename(dead_nb='Nbres.mortes',alive_nb='Nbre.vivantes',init_nb='Total')
Dataset_5$Date<-dmy(Dataset_5$Date)
Dataset_5$year<-year(Dataset_5$Date)
Dataset_52<-Dataset_5%>%group_by(Site,Campagne)%>%summarize(spat_year=min(year))
Dataset_5<-left_join(Dataset_5,Dataset_52,by=c("Site","Campagne"))
rm(Dataset_52)

Dataset_5$Operator<-"Operator_5"
Dataset_5$site_spat<-"baie_de_Vilaine" 
Dataset_5<-Dataset_5%>%mutate(Campaign=spat_year+1)
Dataset_5$Specie<-"Edulis"
Dataset_5$Support<-"bag"
Dataset_5$Comments<-"NA"
Dataset_5$Batch<-"NA"
Dataset_5$MR<-"NA"
Dataset_5<-Dataset_5 %>% dplyr::select(Site,Date,spat_year, alive_nb,dead_nb,init_nb,Batch,site_spat,Specie,Support,Operator,Comments,Campaign,MR)

## Dataset 6 ----
#Dataset loading
Dataset_6 <- read.csv(file.path("Data","original_datasets","Dataset_6.csv"), header=TRUE, sep=";")
Dataset_6$Date<-dmy(Dataset_6$Date)

#We remove rows without data 
Dataset_6 <- Dataset_6 %>%filter(!(`morta.instantané` == "NR" ))

#We remove rows where over-mortality attributed to clogging has been observed
Dataset_6 <- Dataset_6 %>% filter(is.na(Remarque) | Remarque != "Colmatage")
Dataset_6$Campaign <- ifelse(is.na(Dataset_6$Remarque) | Dataset_6$Remarque != "Mise en poche", NA, year(Dataset_6$Date) + 1)

#Rename and addition of several columns
Dataset_6$Campaign <- ifelse(is.na(Dataset_6$Remarque) | Dataset_6$Remarque != "Mise en poche", NA, year(Dataset_6$Date) + 1)
Dataset_6$Campaign <- na.locf(Dataset_6$Campaign, na.rm = FALSE)
Dataset_6<-Dataset_6%>%rename(MR='morta.instantané')
Dataset_6$MR<-as.numeric(Dataset_6$MR)/100

Dataset_6$spat_year<-(Dataset_6$Campaign)-1
Dataset_6$Operator<-"Operator_6"
Dataset_6$site_spat<-"local" 
Dataset_6$Specie<-"Edulis"
Dataset_6$Support<-"bag"
Dataset_6$Comments<-"NA"
Dataset_6$Batch<-"NA"
Dataset_6$alive_nb<-"NA"
Dataset_6$dead_nb<-"NA"
Dataset_6$init_nb<-120

Dataset_6<-Dataset_6 %>% dplyr::select(Site,Date,spat_year, alive_nb,dead_nb,init_nb,Batch,site_spat,Specie,Support,Operator,Comments,Campaign,MR)

# Merging datasets: 
Raw_database<-rbind(Dataset_1, Dataset_2,Dataset_3, Dataset_4, Dataset_5, Dataset_6)
Raw_database<-Raw_database %>% mutate_at(vars(dead_nb,alive_nb,MR),as.numeric)

# Export of the raw_dataset file in .csv
write.csv(Raw_database, file=file.path("Data","new_datasets","Raw_dataset.csv"), row.names = FALSE, quote = TRUE, na = "NA")

# Creation of the Clean_database ----
## Load the raw database :
clean_database <- read.csv(file.path("Data","new_datasets","Raw_dataset.csv"), header=TRUE, sep=",") 
clean_database$Date <- ymd(clean_database$Date)

# Only campaigns before 2023 are retained: 
clean_database <- clean_database %>% filter(Campaign<2023)

# For the "3- Lannion" Site, we only keep monitoring carried out on the Edulis specie originating from Pénestin. 
clean_database_nette <- clean_database %>% filter(site_spat == "Pénestin" & Specie == "Edulis"& Site=="3- Lannion")

# Among the monitored batches, we exclude batches with late start (annotated as "Edulis tard" or "Edulis très tard" in the comments).
clean_database_nette <- clean_database_nette %>% 
  filter(!(Comments == "Edulis tard" | Comments == "Edulis très tard") | is.na(Comments))

# The batches retained for the Lannion site are then added to the clean_database : 
clean_database<-clean_database%>%filter(!Site=="3- Lannion")

clean_database<-rbind(clean_database_nette,clean_database)
rm(clean_database_nette)

# Exclusion of the site "1a- Rivière du Faou", which has only one campaign :
clean_database<-subset(clean_database,!(Site=="1a- Rivière du Faou"))

# Exclusion of batch "2" from Operator_3 data due to incomplete monitoring
clean_database<- clean_database[!(clean_database$Operator == "Operator_3" & clean_database$Batch == "2"), ]

# Removal of combinations Site x Campaign x Operator for which there are fewer than 4 observations : 
clean_database<-clean_database%>%group_by(Site,Campaign,Operator,Batch)%>%mutate(n_count=n())
clean_database <- clean_database%>%filter(n_count >= 4)

# Removing Site x Campaign x Operator combinations that end before the month of May of the survey year : 
End_Date <- clean_database %>%group_by(Site, Campaign,Operator) %>%summarise(End_Date = max(Date))
End_Date<-End_Date %>% mutate(End_month=month(End_Date))%>%mutate(End_year=year(End_Date))
End_Date <- End_Date %>%filter(month(End_Date) < 6)
End_Date <- End_Date %>%filter(End_year <= Campaign)

# Removing Site x Campaign x Operator combinations that start after the month of April of the survey year :
start_Date <- clean_database %>%group_by(Site, Campaign,Operator) %>%summarise(start_Date = min(Date))
start_Date<-start_Date %>% mutate(s_month=month(start_Date))%>%mutate(s_year=year(start_Date))
start_Date<-start_Date %>% mutate(annee_debut = ifelse(Campaign<year(start_Date), "N+1", ifelse(Campaign>year(start_Date),"N-1", 'N')))
start_Date<-start_Date %>% mutate(annees_debut = paste(s_month,annee_debut, sep="-"))
start_Date <- start_Date %>%filter(month(s_month) > 4)
start_Date <- start_Date %>%filter(s_year == Campaign)
clean_database <- anti_join(clean_database, End_Date, by = c("Site", "Campaign", "Operator"))
rm(start_Date,End_Date)

# The number of individuals newly deceased at date t is recalculated :
clean_database<-clean_database%>% arrange(Site,Campaign,Operator,Batch,Date)

clean_database <- clean_database %>%group_by(Site,Campaign,Operator,Batch)%>%mutate(dead_new = ifelse(row_number() == 1, dead_nb, dead_nb - lag(dead_nb, default = first(dead_nb))))

# Instantaneous mortality ic calculated per Site x Campaign x Batch x Date by dividing the number of dead mussels by the sum of alive and dead mussel at time t (dead_nb is replaced by dead new except for Operator_1 observation campaigns after 2019 and for the Operator_3 data) : 
clean_database$MR <- ifelse(clean_database$Operator == "Operator_1" & clean_database$spat_year >= 2020, 
                            clean_database$dead_nb / (clean_database$dead_nb + clean_database$alive_nb),clean_database$MR)

clean_database$MR <- ifelse(clean_database$Operator == "Operator_3", 
                            clean_database$dead_nb / (clean_database$dead_nb + clean_database$alive_nb), clean_database$MR)

clean_database$MR <- ifelse(is.na(clean_database$MR), 
                            clean_database$dead_new / (clean_database$dead_new + clean_database$alive_nb), 
                            clean_database$MR)
clean_database <- clean_database %>%filter(!(is.na(alive_nb) & is.na(dead_nb) & is.na(MR)))

# When the MR<0 we consider that there have been no new mortality cases since the previous date, so the value is replaced by 0 
clean_database$MR <- ifelse(clean_database$MR < 0, 0, clean_database$MR)

# The cumulative proportion of mortality since the beginning of the annual campaign is then calculated : 
# we add a line with MR MC = 0 on first min date for each campaign :
result<-clean_database%>%filter(!(Operator=="Operator_6"))%>%arrange(Site,Campaign,Operator,Batch,Date)%>%
  group_by(Site,Campaign,Operator,Batch) %>%
  summarize(min_date = min(Date))
result <- result %>%
  mutate(Date = as.Date(paste(year(min_date), month(min_date), "01", sep = "-")))
result <- result %>%dplyr::select(-min_date)
result<-result%>%mutate(alive_nb=NA,dead_nb=NA,dead_new=NA,MR=0,CM=0)
clean_database<-rbind(clean_database, result)
rm(result)
#Cumulative mortality : 
clean_database<-clean_database%>% arrange(Site,Campaign,Operator,Batch,Date)
clean_database<- clean_database %>%
  group_by(Site,Campaign,Operator,Batch) %>%
  mutate(CM = 1 - cumprod(1 - MR))%>%ungroup()

#For the site Filiere, we have no data for MR and CM on 2014-12-10. Since all individuals of mussels were dead during the previous sampling, we replaced the values with MR=0 and CM=1:
clean_database$CM <- ifelse(clean_database$Site == "Filiere" & clean_database$Campaign == 2014 & clean_database$Date == "2014-12-10", 1, clean_database$CM)
clean_database$MR <- ifelse(clean_database$Site == "Filiere" & clean_database$Campaign == 2014 & clean_database$Date == "2014-12-10", 0, clean_database$MR)

#Addition of the columns "id_site", "latitude" and "longitude" and modification of the site names :
id_site <- read.csv(file.path("Data","original_datasets","id_site.csv"), header=TRUE, sep=";", fileEncoding = "Windows-1252")
clean_database <- left_join(clean_database,id_site %>% dplyr::select(NOM, operateur, id_site,Y,X,Site), by = c("Site" = "NOM", "Operator" = "operateur"))
clean_database <- clean_database %>% mutate(Site = Site.y) %>% dplyr::select(-Site.y)
clean_database <- clean_database %>% rename(latitude=Y,longitude=X)

# Date in julian day (DOY) :
clean_database<-clean_database %>% mutate (DOY=julian(Date)-julian(as.Date(paste(Campaign,"/01/01",sep=""),format="%Y/%m/%d")))

#Select columns
clean_database<-clean_database %>% dplyr::select(Site,Date,Batch,CM,Campaign,DOY,Operator,id_site,latitude,longitude)

# Export of the clean_database file in .csv
write.csv(clean_database, file=file.path("Data","new_datasets","Clean_dataset.csv"), row.names = FALSE, quote = TRUE, na = "NA")

# Figures ----

## Figure A  ----
## (this figure is not included in the article):
FigA <- ggplot(data = clean_database, aes(x = DOY, y = CM, color = Operator)) +
  geom_point(size=1) +
  ylab("Cumulative mortality") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.4)) +
  scale_x_continuous(limits = c(-122, 364), breaks = c(-120,250)) +
  facet_grid(Campaign~id_site) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),  
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 7),
    strip.text = element_text(color = "black",size=9),legend.position = "top") +
  guides(color = guide_legend(title = "Operator"))
ggsave(FigA, file = file.path("Figures","FigureA.jpeg"),
       width = 14.4, height = 8.825, dpi = 300,units = "in",
       type = "cairo")
graphics.off()
X11()
FigA

## Supplementary material Figure 2 ----
##Number of samples taken for mortality monitoring by site, year and operator, 
counts_morta <- clean_database %>% group_by(Campaign, Operator, Site, latitude, longitude) %>% 
  count() %>% mutate(Site = factor(Site, levels= rev(na.omit(unique(id_site$Site)))))

Supp_mat_Fig2 <- ggplot(data=counts_morta, aes(x=Campaign, y=Site, color=Operator)) + 
  geom_point(size=6) + theme_bw() + 
  scale_x_continuous(breaks = min(counts_morta$Campaign):max(counts_morta$Campaign)) + 
  xlab("Year") + ylab("Site") + 
  scale_color_brewer(name="Dataset", palette="Spectral") + geom_text(aes(label=n), color="black") +
  ggtitle("Number of samples taken for mortality monitoring by site, year and operator")
ggsave(Supp_mat_Fig2, file = file.path("Figures","Supp_mat_Fig2.jpeg"),
       width = 30, height = 20, units="cm", dpi = 300, type = "cairo")
graphics.off()
X11()
Supp_mat_Fig2


# Data standardization ----
#Adjustement of the sinusoidal models
rm(list = ls())
#Load the clean database :
compil<- read.csv(file.path("Data","new_datasets","Clean_dataset.csv"), header=TRUE, sep=",") 

#--------Logistic equation:
Logistic <- function(a, b, c, x) {
  return(a / (1 + exp(-b * (x - c))))
}

#---------Gompertz equation :
Gompertz <- function(a, b, c, x) {
  return(a * (exp(-exp(-b * (x - c)))))
}

#To model the cumulative mortality in function of time, we needed to first set the starting values.
# The `start_lower()` and `start_upper()` values for the parameters were set as follows :
#-   a (the upper asymptote) is between 0 and 1 
#-   b (slope at inflection) is between 0 and 1 
#-   c (time of inflection) is visually estimated (in that case between -90 and 120)

#Second, we can set `lower()` and `upper()` which represent the upper and lower boundaries for parameter estimates. These will be fixed as follows :
#-   a is between 0 and 1 
#-   b is between 0 and 1
#-   c is between -122 and 365 

# Model adjustment: 
set.seed(1)
death_fits <- compil %>%
  group_by(id_site, Campaign) %>%
  tidyr::nest() %>%
  mutate(fits_Logistic = purrr::map(data, ~ nls_multstart(CM ~ Logistic(a, b, c, x = DOY),
                                                          data = .x,
                                                          iter = 5000,
                                                          start_lower = c(a = 0, b = 0, c = -90),
                                                          start_upper = c(a = 1, b = 1, c = 330),
                                                          supp_errors = "Y",
                                                          na.action = na.omit,
                                                          convergence_count = 200,
                                                          lower = c(a = 0, b = 0, c = -122),
                                                          upper = c(a = 1, b = 1, c = 365)
  )),
  fits_Gompertz = purrr::map(data, ~ nls_multstart(CM ~ Gompertz(a, b, c, x = DOY),
                                                   data = .x,
                                                   iter = 5000,
                                                   start_lower = c(a = 0, b = 0, c = -90),
                                                   start_upper = c(a = 1, b = 1, c = 330),
                                                   supp_errors = "Y",
                                                   na.action = na.omit,
                                                   convergence_count = 200,
                                                   lower = c(a = 0, b = 0, c = -122),
                                                   upper = c(a = 1, b = 1, c = 365)
  )))

#Median day : 
#compil%>%group_by(Campaign, id_site) %>%filter(DOY == min(DOY))%>%ungroup() %>%summarize(median(DOY)) %>%as.numeric()
#compil %>%group_by(Campaign, id_site) %>%filter(DOY == max(DOY)) %>%ungroup() %>%summarize(median(DOY)) %>%as.numeric()

#Let's see the if there is a good fit between observed (black circle) and predicted values (red and blue lines). 
#We only make prediction between DOY -122 (beginning of the monitored in September of the year N-1) and DOY 270 (day of the end of the monitoring in September of the year N)
death_model_stack <- gather(death_fits, "model", "best_model", starts_with("fits")) %>%
  filter(!is.null(best_model)) %>%
  mutate(aic = map_dbl(best_model, possibly(AIC, otherwise = NA))) %>%
  mutate(model = case_when(model == "fits_Logistic" ~ "Logistic", model == "fits_Gompertz" ~ "Gompertz"))

to_predict <- compil %>%
  do(data.frame(DOY = seq(-122, 270, by = 1), stringsAsFactors = FALSE))

death_preds <- death_model_stack %>%
  mutate(preds = map(best_model, augment, newdata = to_predict)) %>%
  unnest(preds) %>%
  # to later see the models for which AIC was not calculated
  mutate(aic_compiled = ifelse((aic == "-Inf" | aic == "Inf" | is.na(aic) == TRUE), "no", "yes")) %>%
  dplyr::select(-data, -best_model) %>%
  mutate(model = as.factor(model))

#Figure C (this figure is not included in the article):
FigC <- death_preds %>%
  ggplot() +
  geom_line(aes(DOY, .fitted, col = model), linewidth = 1) +
  geom_point(data = compil, aes(DOY, CM), alpha = 0.5) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.4)) +
  scale_x_continuous(limits = c(-122, 270), breaks = c(-90, 250))+
  facet_grid(Campaign~id_site, scale = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6)) +
  theme(legend.position = "bottom")
ggsave(FigC, file = file.path("Figures","FigureC.jpeg"),
       width = 14.4, height = 8.825, dpi = 300, units = "in",
       type = "cairo")
graphics.off()
x11()
FigC

# We filter the model fits that have not converged and provide a summary of these fits :
death_fits %>%
  mutate(summary = map(fits_Logistic, glance)) %>%
  unnest(summary) %>%
  filter(isConv == "FALSE")

death_fits%>%mutate(summary = map(fits_Gompertz, glance))%>%
  unnest(summary)%>%
  filter(isConv == "FALSE")

# For the Logistic model :
death_info_Logistic <- death_fits %>%
  mutate(summary = map(fits_Logistic, glance)) %>%
  unnest(summary) %>%
  filter(isConv == "FALSE") %>%
  dplyr::select(-data, -fits_Logistic, -fits_Gompertz, -finTol, -BIC)
death_info_Logistic

# For the Gompertz model :
death_info_Gompertz <- death_fits %>%
  mutate(summary = map(fits_Gompertz, glance)) %>%
  unnest(summary) %>%
  filter(isConv == "FALSE") %>%
  dplyr::select(-data, -fits_Logistic, -fits_Gompertz, -finTol, -BIC)
death_info_Gompertz

# Let's observe all these models that do not converge: 
failed_conv_gomp <- death_info_Gompertz %>%
  dplyr::select(id_site, Campaign) %>%
  mutate(model = rep("Gompertz"))

failed_conv_log <- death_info_Logistic %>%
  dplyr::select(id_site, Campaign) %>%
  mutate(model = rep("Logistic"))

failed_conv_death <- rbind(failed_conv_log, failed_conv_gomp)

rm(failed_conv_log, failed_conv_gomp, death_info_Logistic, death_info_Gompertz)

## Figure D ----
## (this figure is not included in the article):
death_model_failled_conv <- inner_join(death_preds, failed_conv_death, by = c("id_site", "Campaign", "model"))

FigD <- ggplot() +
  geom_line(data = death_model_failled_conv, aes(DOY, .fitted, col = model), linewidth = 1) +
  geom_point(data = inner_join(compil, failed_conv_death, by = c("id_site", "Campaign")), aes(DOY, CM), alpha = 0.5) +
  facet_grid(id_site ~ Campaign, scale = "free_x") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.4))+
  scale_x_continuous(limits = c(-122, 270), breaks = seq(-120, 270, by = 150))+
  theme(axis.text.x = element_text(size = 8))+
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(FigD, file = file.path("Figures","FigureD.jpeg"),
      width = 14.4, height = 8.825, dpi = 300, units = "in",
       type = "cairo")
graphics.off()
X11()
FigD

#Now let's see if the AIC was computed for all models:
## Figure E ---- 
## (this figure is not included in the article):
theme_graphic <- function(base_family = "sans", ...) {
  theme_bw(base_family = base_family, ...) +
    theme(
      panel.grid = element_blank(),
      legend.background = element_rect(color = NA, fill = NA),
      strip.text = element_text(size = 11),
      axis.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12),
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm"))}

### Model selection
death_aic_median <- death_model_stack %>%
  group_by(model) %>%
  summarise(median_aic = median(aic), count = n()) %>%
  ungroup()

## Figure E ----
# AIC score for Gompertz and Logistic. This figure is not
# included in the article
mussel_mortality <- ggplot() +
  geom_histogram(data = death_model_stack, aes(aic), alpha = 0.5, col = "black", bins = 40, fill = "#EE3B3B") +
  geom_vline(data = death_aic_median, aes(xintercept = median_aic), linetype = "dashed") +
  facet_wrap(. ~ model, ncol = 1) +
  scale_x_continuous(limits = c(-200, 20))+
  theme_graphic() +
  ylab("Count") +
  xlab("AIC score") +
  geom_text(data = death_aic_median, aes(
    label = paste("Median AIC =", round(median_aic, 1)),
    x = min(death_model_stack$aic), y = 8
  ), col = "#EE3B3B", vjust = "inward", hjust = "inward")
Table3 <- group_by(death_model_stack, id_site, Campaign) %>%
  filter(aic == min(aic, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(model) %>%
  tally() %>%
  mutate(perc_best = round(n / sum(n) * 100)) %>%
  dplyr::select(-n) %>%
  rename(Model = model) %>%
  rename("Percentage of lowest \nAIC score" = perc_best)
death_Table <- ggtexttable(Table3, rows = NULL, theme = ttheme("light"))
FigE <- ggarrange(mussel_mortality, death_Table, ncol = 1, heights = c(1, 0.35))
ggsave(FigE, file = file.path("Figures","FigureE.jpeg"),
       width = 14.4, height = 8.825, dpi = 300, units = "in",
       type = "cairo")
graphics.off()
X11()
FigE

## Figure F ---- 
##(this figure is not included in the article):
#We can also plot the residuals of the logistic models
FigF <- death_model_stack%>%
  filter(model == "Logistic") %>%
  mutate(preds = map(best_model, augment)) %>%
  unnest(preds) %>%
  dplyr::select(-data, -model, -best_model) %>%
  ggplot(aes(DOY, .resid)) +
  geom_point(alpha = 0.5) +
  scale_y_continuous(limits = c(-0.15, 0.2), breaks = seq(0, 1, by = 0.2))+
  scale_x_continuous(limits = c(-122, 270), breaks = c(-90, 250))+
  theme(axis.text.x = element_text(size = 6))+
  geom_hline(yintercept = 0, linetype = 2) +
  facet_grid(Campaign~id_site)
ggsave(FigF, file = file.path("Figures","FigureF.jpeg"),
       width = 14.4, height = 8.825, dpi = 300, units = "in",
       type = "cairo")
graphics.off()
X11()
FigF

# Figure G (this figure is not included in the article):
# We can represent the median of the parameter (for the model that did not failed to converge)
death_params <- death_model_stack %>%
  filter(model == "Logistic") %>%
  mutate(params = map(best_model, tidy)) %>%
  unnest(params) %>%
  dplyr::select(-data, -best_model)

death_param_median <- death_params %>%
  group_by(id_site, term) %>%
  summarise(median = median(estimate), count = n()) %>%
  ungroup()

FigG <- death_params %>%
  ggplot() +
  geom_density(aes(x = estimate, fill = term, after_stat(scaled)), alpha = 0.5) +
  scale_fill_viridis(discrete = TRUE) +
  facet_grid(id_site ~ term, scales = "free") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5))+
  geom_vline(data = death_param_median, aes(xintercept = median), color = "black", size = 1) +
  scale_colour_viridis(discrete = TRUE) +
  xlab("Estimate") +
  ylab("Scale density") +
  theme(legend.position = "none")+
  theme_graphic()
ggsave(FigG, file = file.path("Figures","FigureG.jpeg"),
       width = 14.4, height = 8.825, dpi = 300, units = "in",
       type = "cairo")
graphics.off()
X11()
FigG
## So, we have selected the Logistic model !

## Supplementary material Figure 3 ----
# Calculated and modeled cumulative mortality of oysters of the mussels across 289 campaign x sites combinations
# This figure mimics the graphics of cumulative mortality (obs. vs. estim.) per site and year in the Shiny App

death_preds<-death_preds%>%filter(model == "Logistic")
death_preds$Camp.Site <- paste0(death_preds$Campaign, death_preds$id_site)
death_preds$num <- as.numeric(as.factor(death_preds$Camp.Site))

death_preds<-death_preds %>% 
  mutate(cell.y = as.numeric(substr(num, nchar(num), nchar(num))), 
         cell.x = as.numeric(substr(num, 1, nchar(num)-1))) %>% 
  mutate(cell.y=ifelse(cell.y == 0, 10, cell.y))

compil2 <- compil %>% dplyr::select(Campaign, id_site, DOY, CM)
compil2 <- left_join(death_preds, compil2)
compil2$lab1 <- paste0("Site ", compil2$id_site)
pipo <- 3:9
compil2<-compil2 %>% mutate(cell.x= ifelse(is.na(cell.x) & cell.y %in% pipo, 29, cell.x))
Supp_mat_Fig3 <- ggplot(data= compil2, aes(x= DOY, y=.fitted)) +
  geom_line(col= "darkred", linewidth = 1) +
  geom_point(aes(x= DOY, y= CM), alpha = 0.5) +
  facet_grid(cell.y~cell.x, scale = "free") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.4)) +
  scale_x_continuous(limits = c(-122, 270), breaks = c(-90, 250))+
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  geom_text(aes(x= 80, y= 0.85, label= lab1), size = 3, colour = "darkred")+
  geom_text(aes(x= 80, y= 0.65, label= Campaign), size = 3, colour = "darkred")+
  labs(x= "DOY", y= "Cumulative mortality")
ggsave(Supp_mat_Fig3, file = file.path("Figures","Supp_mat_Fig3.jpeg"),
       width = 19.2, height = 11.77, dpi = 300, units = "in",
       type = "cairo")
graphics.off()
x11()
Supp_mat_Fig3

# We now save the prediction 
death_best_model <- death_preds %>%
  dplyr::select(Campaign, id_site, DOY, .fitted) %>%
  rename("CM_pred" = ".fitted")
death_best_model$CM_pred <- formattable(death_best_model$CM_pred, digits = 3, format = "f")

#Export of the death_predicted file in .csv
write.csv(death_best_model,file=file.path("Data","new_datasets","death_predicted.csv"), row.names = FALSE, quote = TRUE, na = "NA")

rm(mussel_mortality,death_aic_median,death_Table)