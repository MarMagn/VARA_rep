#Preparing merge 
library(tidyr)
library(dplyr)
library(Gmisc)
library(tidyverse)
library(boot)
library(randomForest)
library(ranger)
library(neuralnet)
library(nnet)
rm(list = ls())
data_1 <- read.csv("/Users/martinmagneli/Desktop/Testanalys VARA/tab_1_alder.csv", encoding="latin1")
data_2 <- read.csv("/Users/martinmagneli/Desktop/Testanalys VARA/tab_1_kon.csv", encoding="latin1")
data_3 <- read.csv("/Users/martinmagneli/Desktop/Testanalys VARA/tab_3.csv", encoding="latin1", dec = ",", na.strings = ".")

data_3$VTID_Median <- NULL
base_location <- file.path("/Volumes/NO NAME/VARA_DATA_orginal/")
encoding_type = "latin1"

#fn_key <- file.path(base_location, "key_file.csv")
#keys <- read.csv2(file=fn_key, 
                 # header=TRUE, 
                  #encoding=encoding_type, 
                  #stringsAsFactors=FALSE)  
#train_keys <- select(keys, serial_no, pnr = Personnummer) %>%
     #   filter(serial_no != "12463" & serial_no != "10652") #%>% #excluding 2
       # sample_n(1332) # subset of patients for trainingset

#write.csv(keys, file = "selectedpatients.csv") #only use this sample, # away above

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

keys2 <- read.csv("selectedpatients.csv") %>%
        select(serial_no, pnr) 

ac <- left_join(keys2, ac, by = "pnr")

fn <- file.path(base_location, "aeindexvtfstudiepop.txt")
index <- read.delim(file=fn, 
                    header=TRUE, 
                    encoding=encoding_type, 
                    stringsAsFactors=FALSE) %>%
        select(pnr = Personnummer, sex = Kön, age = Ålder.vid.utskrivning
               , hospital = Sjukhus..klartext, residence = Hemförsamling, city = Hemkommun)


index <- left_join(keys2, index, by = "pnr") %>%
        distinct(serial_no, .keep_all = T) %>%
        left_join(ac, by = "serial_no") 
index <- index %>% mutate(fx = factor(fx, levels=c("Ja", "Nej")
                                      , labels=c("Yes", "No"))
                          , sex = factor(sex, labels=c("Male", "Female"))) %>%
        select(serial_no, pnr = pnr.x, sex, age, los, fx, prim_clin, op_date, los_group, readm_group)

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
sp <- sp %>% 
        dplyr::select(pnr = Personnummer
                      , adm_date = Inskrivningsdatum..num
                      , disc_date = Utskrivningsdatum..num
                      , adm_days = Vårdtid
                      , icd_code = Diagnoser
                      , icd_main = Huvuddiagnoskod
                      , operation = Operationer
                      , re_hospital = Sjukhus..klartext
                      , ward = Medicinskt.verksamhetsområde..klartext)

sp_12 <- sp_12 %>% 
        dplyr::select(pnr = PNR 
                      , adm_date = INDATUM
                      , disc_date = UTDATUM
                      , adm_days = VTID
                      , icd_code = DIAGNOS
                      , icd_main = HDIA
                      , operation = OP
                      , re_hospital = Sjukhusnamn
                      , ward = MVOtext)
sp_total <- sp %>%
        bind_rows(sp_12) %>%
        left_join(index, sp_total, by = "pnr") %>%
        select(serial_no
               , pnr
               , adm_date
               , op_date
               , disc_date
               , re_hospital)

#readmission
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
# creating master dataset
master <- left_join(index, readm, by = "serial_no") %>%
        select(serial_no
               , pnr
               , sex
               , age
               , fx
               , hospital = prim_clin
               , los
               , readmissions
               , op_date, los_group, readm_group)

master$readmissions[is.na(master$readmissions)] <- 0
#adding aes
fn <- file.path(base_location, "ae_data.csv")
ae_data <- read.csv(file=fn, encoding=encoding_type)
found_AEs <- ae_data %>%
        filter(causality > 2 ) %>%  #all AEs
        select(serial_no)

AEs <- found_AEs %>%
        group_by(serial_no) %>%
        mutate(AEs = n()) %>%
        distinct(.keep_all = TRUE) 

master <- left_join(master, AEs, by = "serial_no") %>%
        select(serial_no, sex, age, fx, op_date, hospital, los, readmissions, AEs, los_group, readm_group)
master[is.na(master)]<- 0

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
master <- groupHosp(master)
rm(fn, found_AEs, sp, sp_12, sp_prim, sp_total, readm, AEs, ae_data)

master <- mutate(master, age_cat = if_else(age >= 40 & age < 50, 1
                                           , if_else(age >= 50 & age < 60, 2
                                           , if_else(age >= 60 & age < 70, 3
                                           , if_else(age >= 70 & age < 80, 4
                                           , if_else(age >= 80 & age < 85, 5
                                           , if_else(age >= 85 & age < 90, 6
                                           , if_else(age >= 90 & age < 95, 7
                                           , if_else(age >= 95, 8, 9)))))))))

master$op_date <- lubridate::year(master$op_date)

master <- master %>%  #Grouping by percentiles
        mutate(LOS_grp = if_else(los_group == " 0- 55  med AE", "0 - 55"
                        , if_else(los_group == " 0- 55 utan AE", "0 - 55"
                        , if_else(los_group == "56- 80  med AE", "56 - 80"
                        , if_else(los_group == "56- 80 utan AE", "56 - 80"
                        , if_else(los_group == "81-100  med AE", "81 - 100"
                        , if_else(los_group == "81-100 utan AE", "81 - 100"
                        , "readmission")))))))

ag_data <- data_3 %>%
        rename(type = sjktyp, op_date = AR, sex = KON, age = ALDER, fx = fraktur) %>%
        mutate(type = factor(type, levels=c("Länsdelssjukhus", "Länssjukhus", "Privatsjukhus", "Universitetssjukhus")
                , labels=c("countypart", "county", "private", "university"))
               , sex = factor(sex, labels=c("Male", "Female", "Both"))
               , fx = factor(fx, labels=c("Yes", "No"))) 

master <- left_join(master, ag_data, by = c("type", "op_date", "age", "sex", "fx"))
# quantile(master$los, c(0.50, 0.75, 0.90, 0.95))
# master <- master %>%
#         mutate(los_perc = if_else(los < 7, "0 - 49"
#                         , if_else(los >= 7 & los < 13, "50 - 74"
#                         , if_else(los >= 13 & los < 20, "75 - 89"
#                         , if_else(los >= 20 & los < 25, "90 - 94"
#                         , if_else(los >= 25 , "95 - 100", "wrong"))))))


NAs <- filter(master, is.na(VTID_StdDev)) # NAs, S:t Göran fractures considered county hospital
NAS2 <- filter(master, is.na(VTID_P50))
ICD_low <- c("C", #tumors, 
             "D", #hematologi except D629 stor blödning and D649 anemia,
             "F", #psyc,
             "G", #Neuro
             "H", #ÖNH
             "K", #gastro ev K251 perforation ulcus
             "L", #hud ev L024 inf underhud
             "N", #uro
             "O", #gyn
             "Q", #missbildn ev Q65 höftmissbildn
             "R") #symtom

#ICD_high = c("T81[034]", "L899", "T84[05]", "S730", "T933", "I", "J819", "J1[358]", "R33")

#I, J819, J13, J15, J18, R33 eller något av följande koder som en bi-diagnos: I803,
#I269, L899, M243, M244, S730, T810, T813, T814, T840, T845, T933
#imputating data on NAs


# master[master$serial_no == 808, 20] <- 5.5076
# master[master$serial_no == 19998, 15:20] <-c(9.5, 13.0, 13.0, 13.0, 9.5000, 4.9497)
# master[master$serial_no == 18043, 20] <- 4.2426
# master[master$serial_no == 19943, 20] <- 2.1213
# master[master$serial_no == 18357, 20] <- 1.4142
# master[master$serial_no == 4554, 20] <- 1.4142
# master[master$serial_no == 19314, 15:20] <-c(3.0, 4.0, 4.0, 4.0, 3.4000, 0.5477)
# master[master$serial_no == 19763, 15:20] <-c(5.0, 5.0, 5.0, 5.0, 5.0000, "NA")
# master[master$serial_no == 44, 15:20] <-c(7.0, 12.0, 12.0, 12.0, 7.0000, 5.0000)
# master[master$serial_no == 1371, 15:20] <-c(3.0, 3.0, 3.0, 3.0, 3.0000, 0.0000)
# master[master$serial_no == 19156, 15:20] <-c(7.0, 7.0, 7.0, 7.0, 7.0000, "NA")


###### start of analysis

train_data <- select(master, AEs, sex, age, fx, op_date, los, readmissions
                     , type, VTID_P50, VTID_P75, VTID_P90, VTID_P95
                     , VTID_Mean, VTID_StdDev, pos_90) 
train_data <- mutate(train_data, type = factor(type, labels=c("countypart", "county", "private", "university"))
                    , sex = factor(sex, labels=c("Male", "Female"))
                    , fx = factor(fx, labels=c("Yes", "No"))) 

no_na_data <- na.omit(train_data)
no_na_data$has_AE <- factor(no_na_data$AEs > 0, levels = c(FALSE, TRUE), labels = c("No", "Yes"))
no_na_data$AEs <- NULL
rf <- randomForest(has_AE~., data = no_na_data)
summary(rf)
rf
table(no_na_data$has_AE, model=
              predict(rf))

no_splits <- 10
shuffled <- sample(nrow(no_na_data), replace = FALSE)
batch_size <- floor(length(shuffled)/no_splits)
for (i in 0:(no_splits-1)) {
        if (i == 0) results <- list()

        if (i == no_splits - 1) {
                test_rows <- shuffled[(i*batch_size + 1):length(shuffled)]
        } else {
                test_rows <- shuffled[1:batch_size + i*batch_size]
        }
        train_rows <- shuffled[!(shuffled %in% test_rows)]
        #print(test)
        train <- no_na_data[train_rows, ]
        #test_rows <- shuffled[!(shuffled %in% train)]
        test <- no_na_data[test_rows, ]
        cross_model <- randomForest(has_AE ~ ., data = train)
        p <- predict(cross_model, newdata = test)
        #table(p, test$has_AE))
        results <- append(results, list(table(predicted = p, true=test$has_AE)))
}
calcSens <- function(x) {
        y <- x['Yes', 'Yes']/sum(x[ ,'Yes'])
}
sens_rf <- sapply(results, calcSens) %>%
        mean()
calcSpec <- function(x) {
        y <- x['No', 'No']/sum(x[ ,'No'])
}

spec_rf <- sapply(results, calcSpec) %>%
        mean()

for (i in 0:(no_splits-1)) {
        if (i == 0) results <- list()
        
        if (i == no_splits - 1) {
                test_rows <- shuffled[(i*batch_size + 1):length(shuffled)]
        } else {
                test_rows <- shuffled[1:batch_size + i*batch_size]
        }
        train_rows <- shuffled[!(shuffled %in% test_rows)]
        #print(test)
        train <- no_na_data[train_rows, ]
        #test_rows <- shuffled[!(shuffled %in% train)]
        test <- no_na_data[test_rows, ]
        cross_model <- ranger(has_AE~., data = train, case.weights = (1 +(train$has_AE == "No")*3.5))
        p <- predict(cross_model, data = test)
        #table(p, test$has_AE))
        results <- append(results, list(table(predicted = p$predictions, true=test$has_AE)))
}


sens_rf_2 <- sapply(results, calcSens) %>%
        mean()

spec_rf_2 <- sapply(results, calcSpec) %>%
        mean()
#######Logistic regression

log <- glm(has_AE~.+los*age + los*VTID_StdDev + los*readmissions + age*readmissions, family = binomial(link = logit), data = no_na_data)
# anova(log, test = "Chisq")
 result <- predict(log, type = 'response')
 result <- ifelse(result > 0.5, "Yes", "No")

a <- table(test=result, true = no_na_data$has_AE)
 #misClasificError <- mean(result != test$has_AE)
 #print(paste('Accuracy', 1-misClasificError))
 x <- calcSens(a)
 y <- calcSpec(a)
# cross_model <- glm(has_AE ~ los, family = binomial(link = logit), data = train)
# p <- predict(cross_model, newdata = test, type = "response")
# p <- ifelse(p > 0.5, "Yes", "No")
# results <- append(results, list(table(predicted = p, true=test$has_AE)))
# table(result, true = no_na_data$has_AE)
for (i in 0:(no_splits-1)) {
        if (i == 0) results <- list()
        
        if (i == no_splits - 1) {
                test_rows <- shuffled[(i*batch_size + 1):length(shuffled)]
        } else {
                test_rows <- shuffled[1:batch_size + i*batch_size]
        }
        train_rows <- shuffled[!(shuffled %in% test_rows)]
        #print(test)
        train <- no_na_data[train_rows, ]
        #test_rows <- shuffled[!(shuffled %in% train)]
        test <- no_na_data[test_rows, ]
        cross_model <- glm(has_AE ~., family = binomial(link = logit), data = train) #1 will give similar results, otherwise sens 73 and spec 54
        p <- predict(cross_model, newdata = test, type = "response")
        p <- ifelse(p > 0.5, "Yes", "No")
        results <- append(results, list(table(predicted = p, true=test$has_AE)))
}

sens_log1 <- sapply(results, calcSens) %>%
        mean()

spec_log1 <- sapply(results, calcSpec) %>%
        mean()
#######bagging , weights = (1 +(train$has_AE == "No")*1)

no_splits <- 10
shuffled <- sample(nrow(no_na_data), replace = FALSE)
batch_size <- floor(length(shuffled)/no_splits)
for (i in 0:(no_splits-1)) {
        if (i == 0) results <- list()
        
        if (i == no_splits - 1) {
                test_rows <- shuffled[(i*batch_size + 1):length(shuffled)]
        } else {
                test_rows <- shuffled[1:batch_size + i*batch_size]
        }
        train_rows <- shuffled[!(shuffled %in% test_rows)]
        #print(test)
        train <- no_na_data[train_rows, ]
        #test_rows <- shuffled[!(shuffled %in% train)]
        test <- no_na_data[test_rows, ]
        cross_model <- randomForest(has_AE ~ ., data = train, mtry = 12)
        p <- predict(cross_model, newdata = test)
        #table(p, test$has_AE))
        results <- append(results, list(table(predicted = p, true=test$has_AE)))
}

sens_bag <- sapply(results, calcSens) %>%
        mean()

spec_bag <- sapply(results, calcSpec) %>%
        mean()

######## neural network
nn_data <- no_na_data %>%
        select(has_AE, sex, fx, age, los, readmissions, VTID_Mean, VTID_P50, VTID_P75, VTID_P90, VTID_P95, VTID_StdDev, pos_90)
nn_data$has_AE <- ifelse(nn_data$has_AE == "Yes", 1, 0)
nn_data$fx <- ifelse(nn_data$fx == "Yes", 1, 0)
nn_data$sex <- ifelse(nn_data$sex == "Female", 0, 1)
scl <- function(x) {(x - min(x))/(max(x) - min(x))}
nn_data[, 4:ncol(nn_data)] <- data.frame(lapply(nn_data[,4:ncol(nn_data)], scl))
n <- names(nn_data)
f <- as.formula(paste("has_AE ~", paste(n[!n %in% "has_AE"], collapse = " + ")))
no_splits <- 10
shuffled <- sample(nrow(nn_data), replace = FALSE)
batch_size <- floor(length(shuffled)/no_splits)
for (i in 0:(no_splits-1)) {
        if (i == 0) results <- list()
        
        if (i == no_splits - 1) {
                test_rows <- shuffled[(i*batch_size + 1):length(shuffled)]
        } else {
                test_rows <- shuffled[1:batch_size + i*batch_size]
        }
        train_rows <- shuffled[!(shuffled %in% test_rows)]
        train <- nn_data[train_rows, ]
        test <- nn_data[test_rows, ]
        cross_model <- neuralnet(f, data = train, hidden = 3, act.fct = "logistic", linear.output = FALSE, lifesign = "minimal") #3 layers seems best
        p <- compute(cross_model, test[, -1])
        p1 <- p$net.result
        pred1 <- ifelse(p1>0.5, 1, 0)
        results <- append(results, list(table(predicted = pred1, true=test$has_AE)))
}
calcSens <- function(x) {
        y <- x['1', '1']/sum(x[ ,'1'])
}
sens_nn <- sapply(results, calcSens) %>%
        mean()
calcSpec <- calcSens <- function(x) {
        y <- x['0' , '0']/sum(x[ ,'0'])
}
spec_nn <- sapply(results, calcSpec) %>%
        mean()

############  adding codes

codes_all = c("T81[034]", "L899", 
              "T84[05]", "S730", "T933")
codes_main <- c("I", "J819",
                "J1[358]", "R33")

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
sp_total <- left_join(keys2, sp_total, by = "pnr")

ae_data <- sp_total %>% #selecting admissions with pos codes
        mutate(ae = if_else(grepl(paste(codes_main, collapse= "|"), icd_main) 
                            | grepl(paste(codes_all, collapse= "|"), icd_main)
                            | grepl(paste(codes_all, collapse= "|"), icd_code),1,0)) 
ae_pos <- filter(ae_data, ae == 1) %>%
        distinct(serial_no)
ae_pos$pos_code <- 1
ae_neg <- filter(ae_data, ae == 0) %>%
        distinct(serial_no)
ae_neg$pos_code <- 0

ae_neg <- anti_join(ae_neg, ae_pos, by = "serial_no")
pos_code <- bind_rows(ae_neg, ae_pos)
 master <- arrange(master, serial_no)
 pos_code <- arrange(pos_code, serial_no)
master <- bind_cols(master, pos_code)



##### 30 and 90 days

aes <- ae_data
aes <- select(aes, serial_no, pos_30, pos_90)
test <- semi_join(aes, master, by = "serial_no")
master <- left_join(master, aes, by = "serial_no")
