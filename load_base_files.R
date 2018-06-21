#Preparing merge 
library(tidyr)
library(dplyr)
library(Gmisc)
library(tidyverse)
library(boot)
rm(list = ls())

#Summon files
base_location <- file.path("/Volumes/NO NAME/VARA_DATA_orginal/")
codes_all = c("T81[034]", "L899", 
              "T84[05]", "S730", "T933")
codes_main <- c("I", "J819",
                "J1[358]", "R33")
encoding_type = "latin1"

fn <- file.path(base_location, "aestudiepopvkedj.txt")
ac <- read.delim(file=fn, 
                 header=TRUE, 
                 encoding=encoding_type, 
                 stringsAsFactors=FALSE)

fn <- file.path(base_location, "aestudiepop_patientreg.txt")
sp <- read.delim(file=fn, 
                 header=TRUE, 
                 encoding=encoding_type, 
                 stringsAsFactors=FALSE)

fn <- file.path(base_location, "aestudiepop_patientreg_2012.txt")
sp_12 <- read.delim(file=fn, 
                    header=TRUE, 
                    encoding=encoding_type, 
                    stringsAsFactors=FALSE)

fn_key <- file.path(base_location, "key_file.csv")
keys <- read.csv2(file=fn_key, 
                  header=TRUE, 
                  encoding=encoding_type, 
                  stringsAsFactors=FALSE)  
keys <- select(keys, serial_no, pnr = Personnummer) %>%
        filter(serial_no != "12463" & serial_no != "10652") #excluding 2

#merging datasets
#completing and selecting sp_patreg
sp <- sp %>% 
        select(pnr = Personnummer
               , adm_date = Inskrivningsdatum..num
               , disc_date = Utskrivningsdatum..num
               , adm_days = Vårdtid
               , icd_code = Diagnoser
               , icd_main = Huvuddiagnoskod
               , ward = Medicinskt.verksamhetsområde..klartext)

sp_12 <- sp_12 %>% 
        select(pnr = PNR, adm_date = INDATUM
               , disc_date = UTDATUM
               , adm_days = VTID
               , icd_code = DIAGNOS
               , icd_main = HDIA
               , ward = MVOtext)

ac <- ac %>% select(pnr = Personnummer
                    , op_date = SHPR.Primäroperationsdatum
                    , adm_date = Inskrivningsdatum
                    , disc_date = Utskrivningsdatum
                    , prim_clin = SHPR.Klinik
                    , los = Vårdtid
                    , fx = Fraktur
                    , readm_group = Återinläggningsgrupp
                    , los_group = Vårdtidsindelningsgrupp
                    , sex = Kön) 
keys <- keys %>%
        select(serial_no, pnr) %>%
        filter(serial_no != "12463", serial_no != "10652") #excluding 2 cases

ac <- left_join(keys, ac, by = "pnr")

sp_total <- sp %>%
        bind_rows(sp_12) %>%
        arrange ( pnr) %>% 
        left_join(ac, sp_total, by = "pnr") %>%
        select(pnr 
               , adm_date = adm_date.x
               , op_date
               , disc_date = disc_date.x
               ,icd_main
               ,icd_code
               , ward
        ) 
sp_total <- left_join(keys, sp_total, by = "pnr")       

fn <- file.path(base_location, "aedodsorsaker.txt")
dead <- read.delim(file=fn, 
                   header=TRUE, 
                   encoding=encoding_type, 
                   stringsAsFactors=FALSE)

dead <- dead %>%
        select(pnr = Personnummer
               , death_date = Dödsdatum..SAS.datum
               , death_cause = Underliggande.dödsorsak)

sp_total$adm_date <- as.Date(sp_total$adm_date)
sp_total$disc_date <- as.Date(sp_total$disc_date)
sp_total$op_date <- as.Date(sp_total$op_date)
ac$adm_date <- as.Date(ac$adm_date)
ac$op_date <- as.Date(ac$op_date)
dead$death_date <- as.Date(dead$death_date)

mort <- left_join(dead, ac, by = "pnr") %>%
        mutate(days = death_date - op_date) %>%
        filter(days <= 90) %>%
        select(serial_no, death_date, death_cause)

rm(sp, sp_12, fn_key)