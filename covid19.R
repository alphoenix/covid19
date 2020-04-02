library(tidyverse)
library(haven)

#### CAPACITE EN REANIMATION EN 2018 ####

rea<-bind_rows(
  read_sas("files/rea_2018.sas7bdat"),
  read_sas("files/rea_2017.sas7bdat"),
  read_sas("files/rea_2016.sas7bdat"),
  read_sas("files/rea_2015.sas7bdat")
)

url_spf<-"https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7"

tabl_bord %>%
  filter(jour == max(jour)) %>%
  summarise(total_hosp = sum(hosp,na.rm=TRUE),
            total_rea = sum(rea,na.rm = TRUE),
            total_dc = sum(dc,na.rm=TRUE))

tabl_bord<-rea %>%
  mutate(DPT = substr(FI,start=1,stop=2)) %>% 
  filter(UNI == "REAADU" & AN == "2018" & !(DPT %in% c("97","98"))) %>%
  group_by(DPT) %>%
  summarise(LITS = sum(LIT,na.rm = TRUE)) %>%
  left_join(read_delim(url_spf,delim=";") %>% filter(sexe == "0") %>% select(-sexe),by=c("DPT"="dep")) %>%
  left_join(readxl::read_excel("~/INSEE/ensemble-2020.xlsx",sheet="Départements",skip=7) %>% select(nom_reg=`Nom de la région`,code_dpt=`Code département`,nom_dpt=`Nom du département`,pop=`Population municipale`),
            by=c("DPT"="code_dpt")) %>%
  filter(!((nom_reg == "Corse" & jour == "2020-03-19") | (nom_reg == "Corse" & jour == "2020-03-18")))

map_df(lapply(c(1:19,"2A","2B",21:95,971:974,"976"),function(x) stringr::str_pad(x, 2, pad = "0")),function(x) {
  readxl::read_excel("2020-03-27_deces_quotidiens_departement.xlsx",sheet=x,skip=4,col_names = c("Date","2020_demat","total20","2020_tx","2019_demat","total19","2018_demat","total18","",""),col_types = c("date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","skip","skip")) %>%
    filter(!is.na(Date) & !is.na(total20)) %>%
    select(Date,starts_with("total")) %>%
    filter(Date == max(Date)) %>%
    mutate(croissance = (total20-total19)/total19*100) %>%
    mutate(dpt = x)}) %>%
  left_join(readxl::read_excel("~/INSEE/ensemble-2020.xlsx",sheet="Départements",skip=7) %>% select(code_dpt=`Code département`,nom_dpt=`Nom du département`),by=c("dpt"="code_dpt")) %>%
  write_csv("~/Github/covid19/dc_insee.csv")

## HoSP ET REA + CAPACITE

tabl_bord %>%
  filter(jour == max(jour)) %>%
  group_by(nom_reg) %>%
  summarise(hosp_hab = sum(hosp,na.rm = TRUE) / sum(pop,na.rm = TRUE) * 10000,
            hosp = sum(hosp,na.rm = TRUE),
            rea = sum(rea,na.rm = TRUE),
            lits = sum(LITS,na.rm = TRUE)) %>%
  arrange(desc(hosp_hab)) %>%
  write_csv("~/Github/covid19/top_reg.csv")
  

## HOSPITALISATIONS : UNE COURBE PAR REGION METROP.

left_join(
  tabl_bord %>%
    select(hosp,nom_reg,jour,pop) %>%
    group_by(nom_reg,jour) %>%
    summarise(total = sum(hosp,na.rm=TRUE)/sum(pop,na.rm = TRUE)*10000) %>%
    pivot_wider(names_from = nom_reg,values_from = total),
  tabl_bord %>%
    select(hosp,nom_reg,jour,pop) %>%
    group_by(jour) %>%
    summarise(total = sum(hosp,na.rm=TRUE)/sum(pop,na.rm = TRUE)*10000) %>%
    mutate(nom_reg = "France entière") %>%
    pivot_wider(names_from = nom_reg,values_from = total)
) %>% write_csv("~/Github/covid19/hosp_reg.csv")

## DECES : UNE COURBE PAR REGION METROP.

left_join(
  tabl_bord %>%
    select(dc,nom_dpt,jour) %>%
    pivot_wider(names_from = nom_dpt,values_from = dc),
  tabl_bord %>%
    group_by(jour) %>%
    summarise(total = sum(dc,na.rm=TRUE)) %>%
    mutate(nom_dpt = "France entière") %>%
    pivot_wider(names_from = nom_dpt,values_from = total)
) %>% write_csv("~/Github/covid19/dc_dpt.csv")

## CROISSANCE 

bind_rows(tabl_bord %>%
  mutate(date = case_when(
    jour == max(jour) ~ "j",
    jour == as.Date(max(jour))-1 ~ "j_1",
    jour == as.Date(max(jour))-2 ~ "j_2",
    jour == as.Date(max(jour))-3 ~ "j_3",
    jour == as.Date(max(jour))-4 ~ "j_4",
    jour == as.Date(max(jour))-5 ~ "j_5",
    jour == as.Date(max(jour))-6 ~ "j_6"
  )) %>%
  filter(!is.na(date)) %>%
  group_by(nom_reg,date) %>%
  summarise(hosp = sum(hosp,na.rm=TRUE)) %>%
  pivot_wider(names_from = date,values_from = hosp) %>%
  mutate(
    croiss_j5 = (j_5-j_6)/j_6*100,
    croiss_j4 = (j_4-j_5)/j_5*100,
    croiss_j3 = (j_3-j_4)/j_4*100,
    croiss_j2 = (j_2-j_3)/j_3*100,
    croiss_j1 = (j_1-j_2)/j_2*100,
    croiss_j = (j-j_1)/j_1*100,
  ) %>%
  select(nom_reg,starts_with("croiss")),
  tabl_bord %>%
    mutate(date = case_when(
      jour == max(jour) ~ "j",
      jour == as.Date(max(jour))-1 ~ "j_1",
      jour == as.Date(max(jour))-2 ~ "j_2",
      jour == as.Date(max(jour))-3 ~ "j_3",
      jour == as.Date(max(jour))-4 ~ "j_4",
      jour == as.Date(max(jour))-5 ~ "j_5",
      jour == as.Date(max(jour))-6 ~ "j_6"
    )) %>%
    filter(!is.na(date)) %>%
    group_by(date) %>%
    summarise(hosp = sum(hosp,na.rm=TRUE)) %>%
    pivot_wider(names_from = date,values_from = hosp) %>%
    mutate(
      croiss_j5 = (j_5-j_6)/j_6*100,
      croiss_j4 = (j_4-j_5)/j_5*100,
      croiss_j3 = (j_3-j_4)/j_4*100,
      croiss_j2 = (j_2-j_3)/j_3*100,
      croiss_j1 = (j_1-j_2)/j_2*100,
      croiss_j = (j-j_1)/j_1*100,
    ) %>%
    mutate(nom_reg = "France entière") %>%
    select(nom_reg,starts_with("croiss"))
  ) %>% write_csv("~/Github/covid19/croissance_hosp.csv")


## CARTE POUR 10 000 HAB ?

tabl_bord %>%
  filter(jour == max(jour)) %>%
  summarise(total_hosp = sum(hosp,na.rm=TRUE),
            total_rea = sum(rea,na.rm = TRUE),
            total_dc = sum(dc,na.rm=TRUE))

tabl_bord %>%
  filter(jour == max(jour)) %>%
  select(hosp,dc,rea,LITS,nom_dpt,pop,jour) %>%
  mutate(hosp_hab = hosp / pop * 10000) %>%
  mutate(rea_hab = rea / pop * 10000) %>%
  mutate(dc_hab = dc / pop * 10000) %>%
  write_csv("~/Github/covid19/carte_dpt.csv")
  
## TABLEAU DE BORD : Nom du département, hospitalisé jour j, en réa jour j, décès cumulés jour j, lits en 2018, évolution hosp ?

bind_rows(
  tabl_bord %>%
    filter(jour == max(jour)) %>%
    select(DPT,nom_dpt,hosp,rea,dc,LITS,pop) %>%
    left_join(tabl_bord %>%
                select(DPT,jour,hosp) %>%
                mutate(jour = paste0("hosp-",substr(jour,6,10))) %>%
                pivot_wider(names_from = jour,values_from = hosp)) %>%
    mutate(tendu = rea >= LITS) %>%
    mutate(hosp_hab = hosp / pop * 10000) %>%
    mutate(lits_hab = LITS / pop * 10000) %>%
    select(nom_dpt,starts_with('hosp-'),rea,dc,LITS,tendu,hosp_hab),
  tabl_bord %>%
    filter(jour == max(jour)) %>%
    summarise(
      hosp_hab = sum(hosp,na.rm = TRUE)/sum(pop,na.rm = TRUE)*10000,
      lits_hab = sum(LITS,na.rm = TRUE)/sum(pop,na.rm = TRUE)*10000,
      rea = sum(rea,na.rm = TRUE),
      dc = sum(dc,na.rm = TRUE),
      LITS = sum(LITS,na.rm=TRUE)
    ) %>%
    mutate(tendu = rea >= LITS) %>%
    mutate(nom_dpt = "France entière") %>%
    left_join(tabl_bord %>%
                select(jour,hosp) %>%
                group_by(jour) %>%
                summarise(hosp = sum(hosp,na.rm=TRUE)) %>%
                mutate(jour = paste0("hosp-",substr(jour,6,10))) %>%
                pivot_wider(names_from = jour,values_from = hosp) %>%
                mutate(nom_dpt = "France entière")
    ) %>%
    select(nom_dpt,starts_with('hosp-'),rea,dc,LITS,tendu,hosp_hab)
) %>% write_csv("~/Github/covid19/table_bord.csv")
