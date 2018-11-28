
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
                    , los_group = Vårdtidsindelningsgrupp
                    , op_code = Operationer
                    , cause = SHPR.Diagnos) 
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
               , hospital = Sjukhus..klartext, residence = Hemförsamling, city = Hemkommun, county = Hemlän)


index <- left_join(keys, index, by = "pnr") %>%
        distinct(serial_no, .keep_all = T) %>%
        left_join(ac, by = "serial_no") 
index <- index %>% mutate(fx = factor(fx, levels=c("Ja", "Nej")
                           , labels=c("Yes", "No"))
               , sex = factor(sex, labels=c("Male", "Female"))) %>%
        select(serial_no, sex, age, adm_date, op_date, disc_date, los, fx, prim_clin, residence, city, op_code, county, los_group, readm_group)

index$adm_date <- as.Date(index$adm_date)
index$op_date <- as.Date(index$op_date)
index$disc_date <- as.Date(index$disc_date)
index <- mutate(index, hospital = prim_clin)
groupHosp <- function(df) {
  mutate(df, hospital_type = if_else(df$hospital == "Karolinska/Huddinge" 
                                     |df$hospital == "Karolinska/Solna"
                                     |df$hospital == "Linköping"
                                     |df$hospital == "SU/Mölndal"
                                     |df$hospital == "SUS/Lund"
                                     |df$hospital == "SUS/Malmö"
                                     |df$hospital == "Umeå"
                                     |df$hospital == "Uppsala"
                                     |df$hospital == "Örebro"
                                     , "University"
                                     , if_else(df$hospital == "Borås-Skene"
                                               |df$hospital == "Danderyd"
                                               |df$hospital == "Eksjö"
                                               |df$hospital == "Eskilstuna"
                                               |df$hospital == "Falun"
                                               |df$hospital == "Gävle"
                                               |df$hospital == "Halmstad"
                                               |df$hospital == "Helsingborg"
                                               |df$hospital == "Hässleholm-Kristianstad"
                                               |df$hospital == "Jönköping"
                                               |df$hospital == "Kalmar"
                                               |df$hospital == "Karlskrona-Karlshamn"
                                               |df$hospital == "Karlstad"
                                               |df$hospital == "Lidköping-Skövde"
                                               |df$hospital == "Norrköping"
                                               |df$hospital == "Sunderbyn"
                                               |df$hospital == "Sundsvall"
                                               |df$hospital == "Södersjukhuset"
                                               |df$hospital == "Uddevalla"
                                               |df$hospital == "Varberg"
                                               |df$hospital == "Västerås"
                                               |df$hospital == "Växjö"
                                               |df$hospital == "Östersund"
                                               #|df$hospital =="Capio S:t Göran" & df$fx == "Yes" #S:t Göran considered county if fracture patient
                                               , "County",
                                               if_else(df$hospital == "Aleris Specialistvård Bollnäs"
                                                       |df$hospital == "Aleris Specialistvård Motala"
                                                       |df$hospital == "Aleris Specialistvård Nacka"
                                                       |df$hospital == "Aleris Specialistvård Sabbatsberg"
                                                       |df$hospital == "Art Clinic Göteborg"
                                                       |df$hospital == "Art clinic Jönköping"
                                                       |df$hospital == "Capio Movement Halmstad"               
                                                       |df$hospital == "Capio Ortopediska Huset"
                                                       |df$hospital == "Capio S:t Göran" #& df$fx == "No"
                                                       |df$hospital == "Carlanderska"
                                                       |df$hospital == "Hermelinen Spec.vård"
                                                       |df$hospital == "Ortho Center IFK-kliniken"
                                                       |df$hospital == "Ortho Center Stockholm"
                                                       |df$hospital == "Sophiahemmet"
                                                       , "Private"
                                                       , "Rural"))))
}
index <- groupHosp(index)
