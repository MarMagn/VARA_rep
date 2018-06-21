#rm(list = ls())
library(tidyverse)
base_location <- file.path("/Volumes/NO NAME/VARA_DATA_orginal/")

encoding_type = "latin1"

fn <- file.path(base_location, "aestudiepopvkedj.txt")
ac <- read.delim(file=fn, 
                 header=TRUE, 
                 encoding=encoding_type, 
                 stringsAsFactors=FALSE)
ac <- ac %>% select(pnr = Personnummer
                    , op_date = SHPR.Primäroperationsdatum
                    , adm_date = Inskrivningsdatum
                    , disc_date = Utskrivningsdatum
                    , prim_clin = SHPR.Klinik
                    , los = Vårdtid
                    , fx = Fraktur
                    , readm_group = Återinläggningsgrupp
                    , los_group = Vårdtidsindelningsgrupp) 
fn_key <- file.path(base_location, "key_file.csv")
keys <- read.csv2(file=fn_key, 
                  header=TRUE, 
                  encoding=encoding_type, 
                  stringsAsFactors=FALSE)  
keys <- select(keys, serial_no, pnr = Personnummer) %>%
        filter(serial_no != "12463" & serial_no != "10652") #excluding 2

ac <- left_join(keys, ac, by = "pnr")

fn <- file.path(base_location, "aeindexvtfstudiepop.txt")
index <- read.delim(file=fn, 
                    header=TRUE, 
                    encoding=encoding_type, 
                    stringsAsFactors=FALSE) %>%
        select(pnr = Personnummer, sex = Kön, age = Ålder.vid.utskrivning
               , hospital = Sjukhus..klartext, residence = Hemförsamling, city = Hemkommun)


index <- left_join(keys, index, by = "pnr") %>%
        distinct(serial_no, .keep_all = T) %>%
        left_join(ac, by = "serial_no") 
index <- index %>% mutate(fx = factor(fx, levels=c("Ja", "Nej")
                           , labels=c("Yes", "No"))
               , sex = factor(sex, labels=c("Male", "Female"))) %>%
        select(serial_no, sex, age, los, fx, prim_clin, residence, city)
rm(keys, fn_key, encoding_type, base_location, ac, fn)



#ae_index <- left_join(found_AEs_90, index, by = "serial_no")
