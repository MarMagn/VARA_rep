library(tidyverse)
source("load_base_files.R")

data_3 <- read.csv("/Volumes/NO NAME/VARA_DATA_orginal/tab_3.csv", encoding="latin1", dec = ",", na.strings = ".") #aggregated data
data_3$VTID_Median <- NULL

base_location <- file.path("/Volumes/NO NAME/VARA_DATA_orginal/")
encoding_type = "latin1"

keys2 <- read.csv("/Volumes/NO NAME/VARA_DATA_orginal/selectedpatients.csv") %>%
  select(-X) #Selecting trainding data
#rm(keys)
ac <- select(ac, - serial_no)
ac <- left_join(keys, ac, by = "pnr")

fn <- file.path(base_location, "aeindexvtfstudiepop.txt")
index <- read.delim(file=fn, 
                    header=TRUE, 
                    encoding=encoding_type, 
                    stringsAsFactors=FALSE) %>%
  select(pnr=Personnummer, hospital = Sjukhus..klartext, residence = Hemförsamling
         , city = Hemkommun, county = Hemlän)

index <- left_join(keys, index, by = "pnr") %>%
  distinct(serial_no, .keep_all = T) %>%
  left_join(ac, by = "serial_no") 
index <- index %>% mutate(fx = factor(fx, levels=c("Ja", "Nej")
                                      , labels=c("Yes", "No"))
                          , sex = factor(sex, labels=c("Male", "Female"))) %>%
  select(serial_no, pnr = pnr.x, sex, age, los, fx, prim_clin, op_date, los_group, readm_group, residence, city, county, cause)

#readmissions
sp_total$adm_date <- as.Date(sp_total$adm_date)
sp_total$disc_date <- as.Date(sp_total$disc_date)
sp_total$op_date <- as.Date(sp_total$op_date)

sp_prim <- sp_total %>%
  mutate(days = op_date - adm_date, days2 = disc_date - op_date) %>% 
  filter(days >= 0, days2 > 0) #Selecting primary admissions

readm <- anti_join(sp_total, sp_prim) %>% 
  mutate(days = adm_date - op_date) %>%     #selecting readmissions within 90 days
  filter(days <= 90 & days > 0)

readm <- group_by(readm, serial_no) %>%
  mutate(readmissions = n()) %>%
  distinct(serial_no, .keep_all = TRUE) %>%
  select(serial_no
         , readmissions)
dead_index <- left_join(sp_prim, mort, by = "serial_no")
dead_index <- dead_index %>%
  mutate(index_dead = if_else(death_date == disc_date, 1, 0)) %>%
  mutate(dead_30 = if_else(death_date - op_date <= 30 & index_dead == 0, 1, 0)) %>%
  mutate(dead_90 = if_else(death_date - op_date <= 90 & index_dead == 0, 1, 0)) %>%
  select(serial_no, dead_30, dead_90) %>%
  distinct(serial_no, .keep_all = T)

ae_data <- sp_total %>% #selecting admissions with pos codes
  mutate(ae = if_else(grepl(paste(codes_main, collapse= "|"), icd_main) 
                      | grepl(paste(codes_all, collapse= "|"), icd_main)
                      | grepl(paste(codes_all, collapse= "|"), icd_code),1,0)) %>%
  filter(adm_date - op_date < 90 & disc_date >= op_date) %>%
  mutate(index_ae = if_else(ae == 1 & op_date >= adm_date & disc_date > op_date, 1, 0)) %>%
  mutate(re_30_ae = if_else(ae == 1 & index_ae == 0 & adm_date - op_date <= 30 & adm_date > op_date, 1, 0)) %>%
  mutate(re_90_ae = if_else(ae == 1 & index_ae == 0 & adm_date - op_date <= 90 & adm_date > op_date, 1, 0)) %>%
  filter(ae == 1) %>%
  select(serial_no, index_ae, re_30_ae, re_90_ae) 

re_30 <- filter(ae_data, re_30_ae == 1) %>%
  distinct(serial_no, .keep_all = T) %>%
  mutate(ae_30 = 1) %>%
  select(serial_no, ae_30)

re_90 <- filter(ae_data, re_90_ae == 1) %>%
  distinct(serial_no, .keep_all = T) %>%
  mutate(ae_90 = 1) %>%
  select(serial_no, ae_90)
index_ae <- filter(ae_data, index_ae == 1) %>%
  distinct(serial_no, .keep_all = T) %>%
  mutate(index_ae = 1) %>%
  select(serial_no, index_ae)
ae_data <- left_join(ac, re_30, by = "serial_no") %>%
  left_join(re_90, by = "serial_no") %>%
  left_join(dead_index, by = "serial_no") %>%
  left_join(index_ae, by = "serial_no")
ae_data$ae_30[is.na(ae_data$ae_30)] <- 0
ae_data$ae_90[is.na(ae_data$ae_90)] <- 0
ae_data$dead_30[is.na(ae_data$dead_30)] <- 0
ae_data$dead_90[is.na(ae_data$dead_90)] <- 0
ae_data$index_ae[is.na(ae_data$index_ae)] <- 0

ae_data <- ae_data %>%       
  mutate(pos_30 = if_else(ae_30 == 1 | dead_30 == 1, 1, 0)) %>%
  mutate(pos_90 = if_else(ae_90 == 1 | dead_90 == 1, 1, 0)) %>%
  select(serial_no, index_ae, pos_30, pos_90)
rm(fn, codes_all, codes_main, sp_total, dead, mort, sp_prim, dead_index, re_30, re_90)

#Adding AEs from the RRR-set
fn <- file.path(base_location, "ae_data.csv")
rrr_data <- read.delim2(file=fn, encoding="latin", sep = ";") %>%
  select(serial_no, event_date, inj_type, causality, avoidability, NCCMERP)
rrr_data <- na.omit(rrr_data)

rrr_data$event_date <- as.Date(rrr_data$event_date)


aes <- rrr_data %>%
  filter( causality > 2 ) %>%
  left_join(keys, by = "serial_no") 
x <- na.omit(aes)
x <- left_join(x, ac, by = "serial_no")
aes <- select(x, serial_no, event_date, inj_type, causality, avoidability, NCCMERP, op_date) %>%
  mutate(days = event_date - op_date) %>%
  mutate(rrr_30 = if_else(event_date - op_date <= 30 , 1, 0)) %>%
  mutate(rrr_90 = if_else(event_date - op_date <= 90 , 1, 0)) 

rrr_30 <- aes %>%
  filter(rrr_30 == 1) %>%
  distinct(serial_no, .keep_all = T) %>%
  select(serial_no, rrr_30)

rrr_90 <- aes %>%
  filter(rrr_90 == 1) %>%
  distinct(serial_no, .keep_all = T) %>%
  select(serial_no, rrr_90)
maj_30 <- filter(aes, avoidability > 2 & NCCMERP > 5 & event_date - op_date <= 30) %>%
  distinct(serial_no) %>%
  mutate(maj_30 = 1)
maj_90 <- filter(aes, avoidability > 2 & NCCMERP > 5 & event_date - op_date <= 90) %>%
  distinct(serial_no) %>%
  mutate(maj_90 = 1)

rrr_all <- left_join(rrr_90, rrr_30, by = "serial_no")
rrr_all <- left_join(rrr_all, maj_30, by = "serial_no")
rrr_all <- left_join(rrr_all, maj_90, by = "serial_no")

AE_frame <- left_join(ae_data, rrr_all, by = "serial_no")
AE_frame[is.na(AE_frame)] <- 0



# fx_frame <- left_join(index, AE_frame, by = "serial_no")
# el_frame <- filter(fx_frame, fx == "No") %>%
#   select(serial_no, cat_30, cat_90)
# fx_frame <- select(fx_frame, serial_no, cat_30, cat_90)
# rm(ac, aes, keys2)

master <- left_join(index, readm, by = "serial_no") %>%
  select(serial_no
         , pnr
         , sex
         , age
         , fx
         , hospital = prim_clin
         , los
         , readmissions
         , op_date, los_group, readm_group
         , residence, city, county, cause)

master$readmissions[is.na(master$readmissions)] <- 0


master <- left_join(master, AE_frame, by = "serial_no") %>%
  select(-pnr, -cause,-residence, -readm_group, -los_group, -city)

groupHosp <- function(df) {
  df <- mutate(df, type = if_else(df$hospital == "Karolinska/Huddinge" 
                                  |df$hospital == "Karolinska/Solna"
                                  |df$hospital == "Linköping"
                                  |df$hospital == "SU/Mölndal"
                                  |df$hospital == "SUS/Lund"
                                  |df$hospital == "SUS/Malmö"
                                  |df$hospital == "Umeå"
                                  |df$hospital == "Uppsala"
                                  |df$hospital == "Örebro"
                                  , "university"
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
                                            |df$hospital =="Capio S:t Göran" & df$fx == "Yes" #S:t Göran considered county if fracture patient
                                            , "county",
                                            if_else(df$hospital == "Aleris Specialistvård Bollnäs"
                                                    |df$hospital == "Aleris Specialistvård Motala"
                                                    |df$hospital == "Aleris Specialistvård Nacka"
                                                    |df$hospital == "Aleris Specialistvård Sabbatsberg"
                                                    |df$hospital == "Art Clinic Göteborg"
                                                    |df$hospital == "Art clinic Jönköping"
                                                    |df$hospital == "Capio Movement Halmstad"               
                                                    |df$hospital == "Capio Ortopediska Huset"
                                                    |df$hospital == "Capio S:t Göran" & df$fx == "No"
                                                    |df$hospital == "Carlanderska"
                                                    |df$hospital == "Hermelinen Spec.vård"
                                                    |df$hospital == "Ortho Center IFK-kliniken"
                                                    |df$hospital == "Ortho Center Stockholm"
                                                    |df$hospital == "Sophiahemmet"
                                                    , "private"
                                                    , "countypart"))))
}
master <- groupHosp(master) %>%
  distinct(serial_no, .keep_all = T)

master$op_date <- lubridate::year(master$op_date)

ag_data <- data_3 %>%
  rename(type = sjktyp, op_date = AR, sex = KON, age = ALDER, fx = fraktur) %>%
  mutate(type = factor(type, levels=c("Länsdelssjukhus", "Länssjukhus", "Privatsjukhus", "Universitetssjukhus")
                       , labels=c("countypart", "county", "private", "university"))
         , sex = factor(sex, labels=c("Male", "Female", "Both"))
         , fx = factor(fx, labels=c("Yes", "No")))

master <- left_join(master, ag_data, by = c("type", "op_date", "age", "sex", "fx"))

master <- select(master, -op_date, -hospital, -county)
master <- mutate(master, type = factor(type, labels=c("countypart", "central_county", "private", "university"))
                 , sex = factor(sex, labels=c("Female", "Male"))
                 , fx = factor(fx, labels=c("Fracture", "Elective")))

training_cases <- mutate(keys2, what_set = "TR")
holdout_cases <- anti_join(keys, keys2) %>%
  mutate(what_set = "HO")
all_cases <- rbind(training_cases, holdout_cases) %>%
  select(-pnr)
final_data <- left_join(master, all_cases, by = "serial_no")
