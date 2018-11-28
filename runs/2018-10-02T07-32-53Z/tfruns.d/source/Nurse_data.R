library(dplyr)
rm(list = ls())
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
                    , sex = Kön) 
fn_key <- file.path(base_location, "key_file.csv")
keys <- read.csv2(file=fn_key, 
                  header=TRUE, 
                  encoding=encoding_type, 
                  stringsAsFactors=FALSE)  
keys <- select(keys, serial_no, pnr = Personnummer) %>%
        filter(serial_no != "12463" & serial_no != "10652") #excluding 2
ac <- left_join(keys, ac, by = "pnr")
createGroups <- function(df) {
        df <- mutate(df, group = (if_else(readm_group == "Åter inom  2-30 dar  med AE", 1
                                          , if_else(readm_group == "Åter inom  2-30 dar utan AE", 2
                                                 , if_else(readm_group == "Åter inom 31-90 dar  med AE", 3
                                                 , if_else(readm_group == "Åter inom 31-90 dar utan AE", 4
                                                 , if_else(los_group == " 0- 55  med AE", 5
                                                 , if_else(los_group == " 0- 55 utan AE", 6
                                                 , if_else(los_group == "56- 80  med AE", 7
                                                 , if_else(los_group == "56- 80 utan AE", 8
                                                 , if_else(los_group == "81-100  med AE", 9
                                                 , 10))))))))))) 
}
ac <- createGroups(ac)
nurse_data <- read.csv("nurse_data.csv", encoding = "latin1")
nurse_data <- nurse_data %>%
        filter( causality > 2 ) %>%
        filter(grepl(paste(11, collapse = "|"), Process))
nurse_data <- left_join(nurse_data, ac, by = "serial_no")

fx_all <- filter(nurse_data, fx == "Ja")
nofx_all <- filter(nurse_data, fx == "Nej")

mult_aes_prev <- nurse_data_prev %>%
        group_by(serial_no) %>%
        mutate(AEs = n()) %>%
        distinct(serial_no, .keep_all = T) %>%
        filter(AEs > 1)
table(mult_aes_prev$fx)


nurse_data_prev <- filter(nurse_data, avoidability > 2)
prev_ind <- nurse_data_prev %>%
        distinct(serial_no, .keep_all = T)
fx_prev <- filter(nurse_data_prev, fx == "Ja")
nofx_prev <- filter(nurse_data_prev, fx == "Nej")
fx_prev_ind <- filter(prev_ind, fx == "Ja")
nofx_prev_ind <- filter(prev_ind, fx == "Nej")


nurse_data_ind <- distinct(nurse_data_prev, serial_no, .keep_all = TRUE)
ind_fx <- filter(nurse_data_ind, fx == "Ja")
ind_nofx <- filter(nurse_data_ind, fx == "Nej")


nurse_data_ind <- distinct(nurse_data, serial_no, .keep_all = T)

incidence_all <- nurse_data_ind %>%
        group_by(group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(incidence_all$incidence)/21774

incidence_fx <- ind_fx %>%
        group_by(group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(incidence_fx$incidence)/6544

incidence_nofx <- ind_nofx %>%
        group_by(group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(incidence_nofx$incidence)/15230
########prventable
incidence_prev <- prev_ind %>%
        group_by(group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(incidence_prev$incidence)

incidence_prev_fx <- fx_prev_ind %>%
        group_by(group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(incidence_prev_fx$incidence)/6544

incidence_prev_nofx <- nofx_prev_ind %>%
        group_by(group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(incidence_prev_nofx$incidence)/15230

all_numb <- group_by(nurse_data, serial_no) %>%
        mutate(AEs = n()) %>%
        filter(AEs > 1) %>%
        distinct(serial_no, .keep_all = T) 

incdidence_mult_aes <- all_numb%>%
        group_by(group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(incdidence_mult_aes$incidence)/21774

inc_fx_mult <- filter(all_numb, fx == "Ja")
inc_nofx_mult <- filter(all_numb, fx == "Nej")

incdidence_fx_mult <- inc_fx_mult %>%
        group_by(group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(incdidence_fx_mult$incidence)/6544

incdidence_nofx_mult <- inc_nofx_mult %>%
        group_by(group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666,  206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(incdidence_nofx_mult$incidence)/15230
###
all_numb_prev <- group_by(nurse_data_prev, serial_no) %>%
        mutate(AEs = n()) %>%
        filter(AEs > 1) %>%
        distinct(serial_no, .keep_all = T) 
incdidence_mult_prev_aes <- all_numb_prev %>%
        group_by(group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(incdidence_mult_prev_aes$incidence)/21774

inc_fx_prev_mult <- filter(all_numb_prev, fx == "Ja")
inc_nofx_prev_mult <- filter(all_numb_prev, fx == "Nej")

incdidence_fx_prev_mult <- inc_fx_prev_mult %>%
        group_by(group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(incdidence_fx_prev_mult$incidence)/6544

incdidence_nofx_prev_mult <- inc_nofx_prev_mult %>%
        group_by(group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666,  206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(incdidence_nofx_prev_mult$incidence)/15230

####
table(nurse_data$X8c)
table(nurse_data$X8c, nurse_data$fx)




fx <- group_by(ind_fx, group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(fx$incidence)
no_fx <- group_by(ind_nofx, group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(no_fx$incidence)

fx_all <- filter(nurse_data, fx == "Ja")
nofx_all <- filter(nurse_data, fx == "Nej")
fx_all <- group_by(fx_all, serial_no) %>%
        summarise(n())
median(fx_all$`n()`)
range(fx_all$`n()`)
nofx_all <- group_by(nofx_all, serial_no) %>%
        summarise(n())
median(nofx_all$`n()`)
range(nofx_all$`n()`)
table(nurse_data_avoid$fx.y)
nurse_data_avoid_ind <- distinct(nurse_data_avoid, serial_no, .keep_all = T)
table(nurse_data_avoid_ind$fx.y)

nurse_data_ind_avoid <- distinct(nurse_data_avoid, serial_no, .keep_all = T)
av_fx_in <- filter(nurse_data_avoid_ind, fx == "Ja")
av_nofx_in <- filter(nurse_data_avoid_ind, fx == "Nej")

av_fx_g <- group_by(av_fx_in, group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(av_fx$incidence)
av_nofx_g <- group_by(av_nofx_in, group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(av_nofx$incidence)

av_fx_no <- group_by(av_fx, serial_no) %>%
        summarise(n())
median(av_fx_no$`n()`)
range(av_fx_no$`n()`)

av_nofx_no <- group_by(av_nofx, serial_no) %>%
        summarise(n())
median(av_nofx_no$`n()`)
range(av_nofx_no$`n()`)

fx <- filter(av_fx_no, `n()` > 1) %>%
        left_join(ac, by = "serial_no")

no_fx <- filter(av_nofx_no, `n()` > 1) %>%
        left_join(ac, by = "serial_no")

fx <- group_by(fx, group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(fx$incidence)
no_fx <- group_by(no_fx, group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
sum(no_fx$incidence)

fx_data <- group_by(fx_data, serial_no) %>%
        summarise(aes = n()) %>%
        filter(aes > 1) %>%
        left_join(ac, by = "serial_no")

fx_aes <- group_by(fx_data, group) %>%
        summarise(aes = n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = aes / sample) %>%
        mutate(incidence = rate * pop)
sum(fx_aes$incidence)

nofx_data <- group_by(nofx_data, serial_no) %>%
        summarise(aes = n()) %>%
        filter(aes > 1) %>%
        left_join(ac, by = "serial_no")

nofx_aes <- group_by(nofx_data, group) %>%
        summarise(aes = n()) %>%
        mutate(pop = c(630, 631, 403, 666, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 49, 195, 74, 294)) %>%
        mutate(rate = aes / sample) %>%
        mutate(incidence = rate * pop)
sum(nofx_aes$incidence)


nofx_data <- group_by(nofx_data, serial_no) %>%
        summarise(aes = n()) %>%
        filter(aes > 1)


 yy <- left_join(nurse_data_ind, ac, by = "serial_no")
 yy <- left_join(nurse_data_ind_avoid, ac, by = "serial_no")
xx <- group_by(yy, group) %>%
        summarise(n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = `n()` / sample) %>%
        mutate(incidence = rate * pop)
   
sum(xx$incidence)          
nurse_data_avoid <- left_join(nurse_data_avoid, ac, by = "serial_no")



fx_prev_aes <- group_by(fx_prev, serial_no) %>%
        summarise(aes = n()) %>%
        filter(aes > 1) %>%
        left_join(ac, by = "serial_no")

fx_prev_aes <- group_by(fx_prev_aes, group) %>%
        summarise(aes = n()) %>%
        mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)) %>%
        mutate(rate = aes / sample) %>%
        mutate(incidence = rate * pop)
sum(fx_prev_aes$incidence)

nofx_prev_aes <- group_by(nofx_prev, serial_no) %>%
        summarise(aes = n()) %>%
        filter(aes > 1) %>%
        left_join(ac, by = "serial_no")

nofx_prev_aes <- group_by(nofx_prev_aes, group) %>%
        summarise(aes = n()) %>%
        mutate(pop = c(630, 631, 403, 666, 206, 3237, 537, 2547)
               , sample = c(294, 442, 293, 194, 49, 195, 74, 294)) %>%
        mutate(rate = aes / sample) %>%
        mutate(incidence = rate * pop)
sum(nofx_prev_aes$incidence)

table(nurse_data$correct_ICD_code)
table(fx_data$correct_ICD_code)
table(nofx_data$correct_ICD_code)
sum(nurse_data$added_hosp_days, na.rm = T)
sum(fx_data$added_hosp_days, na.rm = T)
sum(nofx_data$added_hosp_days, na.rm = T)
###### flik2
table(nurse_data$X6b)
table(nurse_data_prev$X6b)
table(nurse_data$NCCMERP)
table(nurse_data_prev$NCCMERP)

table(fx_all$X6b)
table(nofx_all$X6b)
table(fx_prev$X6b)
table(nofx_prev$X6b)

table(fx_all$NCCMERP)
table(fx_prev$NCCMERP)
table(nofx_all$NCCMERP)
table(nofx_prev$NCCMERP)


#finding discrepancy between the scales
x <- filter(nurse_data, X6b == 4 | X6b == 5)
x <- filter(nurse_data, X6b == 4)
filter(x, NCCMERP == 6)
filter(x, NCCMERP == 9)

#####flik3
table(fx_data$inj_type)
table(fx_prev$inj_type)
table(nofx_data$inj_type)
table(nofx_prev$inj_type)

#####flik4
x <- filter(nurse_data_prev, inj_type == "Övrigt")
xx <- filter(nurse_data, inj_type == "Övrigt")
xxx <-filter(nurse_data, fx == "Ja" & inj_type == "Övrigt")
xxxx <-filter(nurse_data, fx == "Nej" & inj_type == "Övrigt")
y <- filter(nurse_data_prev, fx == "Ja" & inj_type == "Övrigt")
yy <- filter(nurse_data_prev, fx == "Nej" & inj_type == "Övrigt")
y <- as.table.data.frame(nurse_data$X10b)
filter(nofx_data, X6b == 0)
summary(y)
########## flik 5
table(nurse_data$Process)

§getAdjustedIncidence <- function(df) {
        aeDf <- table(df$group)
        as.data.frame.matrix(aeDf) %>%
                mutate(AEs = FN + TP
                       , c
                       , rate = AEs / sample
                       , incidence = rate * pop)
}

#### flik 6
x <- filter(nofx_prev, grepl(paste(21), Process))

x <- filter(nurse_data_prev, grepl("1", Process))
y <- filter(nurse_data_prev, grepl("1", fixed = T , Process))

test <- nofx_all %>% 
        mutate(P2 = str_split(Process, ";")) %>% 
        select(-Process) %>% 
        unnest(P2) %>% 
        rename(Process = P2)
test <- select(test, serial_no, aeID, Process)
x <- filter(test, Process == 2)
table(test$Process)

#### flik 7
test <- fx_prev %>%
        rename(reason = X9a) %>%
        mutate(r2 = str_split(reason, ";")) %>% 
        select(-reason) %>% 
        unnest(r2) %>% 
        rename(reason = r2) %>%
        select(serial_no, aeID, reason, prim_clin)


######
x <- filter(nurse_data, inj_type == "Svikt i vitala parametrar - Övrig")


y <- test %>%
        group_by(reason) %>%
        summarise(n()) 

library(stringr)

###### Kaplan Meier
nurse_data$event_date <- as.Date(nurse_data$event_date)
nurse_data$op_date <- as.Date(nurse_data$op_date)
km_data <- mutate(nurse_data, days = event_date - op_date)

km.as.one <- survfit(days ~ )








nurse_data <- left_join(nurse_data, ac, by = "serial_no")
nurse_acute <- filter(nurse_data, fx == "Ja")
nurse_elective <- filter(nurse_data, fx == "Nej")
median(nurse_data$age)
median(index$age)
range(nurse_data$age)
range(index$age)
table(nurse_data$sex)
table(index$sex)
table(index$fx)
table(nurse_data$fx)
median(index$los)
median(nurse_data$los)
range(index$los)
range(nurse_data$los)

table(nurse_data$NCCMERP)
table(nurse_data$X6b)

table(nurse_data$inj_type)

no_aes <- nurse_data %>%
        group_by(serial_no) %>%
        mutate(AEs = n()) %>%
        distinct(serial_no, .keep_all = TRUE) 

plus_ae <- filter(no_aes, AEs > 1)        
median(no_aes$AEs)
range(no_aes$AEs)

table(nurse_data$X9a)

x <- left_join(nurse_data, index, by = "serial_no")

fx <- filter(x, fx.x == "Ja")
fxno <- filter(x, fx.x == "Nej")
table(fxno$sex.y)
median(fxno$age)
range(fxno$age)
median(fxno$los.y)
range(fxno$los.y)


