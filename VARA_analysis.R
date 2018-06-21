source("load_base_files.R")
sp_prim <- sp_total %>%
        mutate(days = op_date - adm_date, days2 = disc_date - op_date) %>% 
        filter(days >= 0, days2 > 0) #Selecting primary admissions
dead_index <- left_join(sp_prim, mort, by = "serial_no")
dead_index <- dead_index %>%
        mutate(index_dead = if_else(death_date == disc_date, 1, 0)) %>%
        mutate(dead_30 = if_else(death_date - op_date <= 30 & index_dead == 0, 1, 0)) %>%
        mutate(dead_90 = if_else(death_date - op_date <= 90 & index_dead == 0, 1, 0)) %>%
        select(serial_no, index_dead, dead_30, dead_90) %>%
        distinct(serial_no, .keep_all = T)

ae_data <- sp_total %>% #selecting admissions with pos codes
        mutate(ae = if_else(grepl(paste(codes_main, collapse= "|"), icd_main) 
                            | grepl(paste(codes_all, collapse= "|"), icd_main)
                            | grepl(paste(codes_all, collapse= "|"), icd_code),1,0)) %>%
        #mutate(days = adm_date - op_date) %>%
        filter(adm_date - op_date < 90 & disc_date >= op_date) %>%
        mutate(index_ae = if_else(ae == 1 & op_date >= adm_date & disc_date > op_date, 1, 0)) %>%
        mutate(re_30_ae = if_else(ae == 1 & index_ae == 0 & adm_date - op_date <= 30 & adm_date > op_date, 1, 0)) %>%
        mutate(re_90_ae = if_else(ae == 1 & index_ae == 0 & adm_date - op_date <= 90 & adm_date > op_date, 1, 0)) %>%
        filter(ae == 1) 
index <- filter(ae_data, index_ae == 1) %>%
        distinct(serial_no, .keep_all = T) %>%
        mutate(index_ae = 1) %>%
        select(serial_no, index_ae)

re_30 <- filter(ae_data, re_30_ae == 1) %>%
        distinct(serial_no, .keep_all = T) %>%
        mutate(ae_30 = 1) %>%
        select(serial_no, ae_30)

re_90 <- filter(ae_data, re_90_ae == 1) %>%
        distinct(serial_no, .keep_all = T) %>%
        mutate(ae_90 = 1) %>%
        select(serial_no, ae_90)
ae_data <- left_join(ac, index, by = "serial_no") %>%
        left_join(re_30, by = "serial_no") %>%
        left_join(re_90, by = "serial_no") %>%
        left_join(dead_index, by = "serial_no")
ae_data$index_ae[is.na(ae_data$index_ae)] <- 0
ae_data$ae_30[is.na(ae_data$ae_30)] <- 0
ae_data$ae_90[is.na(ae_data$ae_90)] <- 0
ae_data$index_dead[is.na(ae_data$index_dead)] <- 0
ae_data$dead_30[is.na(ae_data$dead_30)] <- 0
ae_data$dead_90[is.na(ae_data$dead_90)] <- 0

ae_data <- ae_data %>%       
        mutate(pos_30 = if_else(ae_30 == 1 | dead_30 == 1, 1, 0)) %>%
        mutate(pos_90 = if_else(ae_90 == 1 | dead_90 == 1, 1, 0)) %>%
        select(serial_no, pos_30, pos_90, los_group, readm_group)
rm(fn, codes_all, codes_main, sp_total, dead, mort, sp_prim, dead_index, index, re_30, re_90)
#VARA AEs from RRR
fn <- file.path(base_location, "ae_data.csv")
rrr_data <- read.csv(file=fn, encoding=encoding_type)

rrr_data$event_date <- as.Date(rrr_data$event_date)

aes <- rrr_data %>%
        filter( causality > 2 ) %>%
        left_join(ac, by = "serial_no") 

aes <- aes %>%
        mutate(days = event_date - op_date) %>%
        mutate(rrr_i = if_else(op_date >= adm_date & disc_date > op_date & adm_date <= event_date & event_date <= disc_date , 1, 0)) %>%
        mutate(rrr_30 = if_else(rrr_i == 0 & event_date - op_date <= 30 , 1, 0)) %>%
        mutate(rrr_90 = if_else(rrr_i == 0 & event_date - op_date <= 90 , 1, 0)) 
aes_prev <- filter(aes, avoidability > 2)
aes_serious <- filter(aes, NCCMERP > 5)
aes_all <- aes
aes_s <- aes_serious
aes_p <- aes_prev

transformDF <- function(df) {
        df1 <- df %>%
                filter(rrr_i == 1) %>%
                distinct(serial_no, .keep_all = T) %>%
                select(serial_no, rrr_i)
        
        df2 <- df %>%
                filter(rrr_30 == 1) %>%
                distinct(serial_no, .keep_all = T) %>%
                select(serial_no, rrr_30)
        
        df3 <- df %>%
                filter(rrr_90 == 1) %>%
                distinct(serial_no, .keep_all = T) %>%
                select(serial_no, rrr_90)
        
        df <- left_join(ac, df1, by = "serial_no") %>%
                left_join(df2, by = "serial_no") %>%
                left_join(df3, by = "serial_no") %>%
                mutate(rrr_both_30 = if_else(rrr_i == 1 | rrr_30 == 1, 1, 0)) %>%
                mutate(rrr_both_90 = if_else(rrr_i == 1 | rrr_90 == 1, 1, 0)) %>%
                select(serial_no, rrr_i, rrr_30, rrr_90, rrr_both_30, rrr_both_90)
}

aes <- transformDF(aes)
aes_prev <- transformDF(aes_prev)
aes_ser <- transformDF(aes_serious)

aes[is.na(aes)] <- 0
aes_prev[is.na(aes_prev)] <- 0
aes_ser[is.na(aes_ser)] <- 0

AE_frame <- left_join(ae_data, aes, by = "serial_no")
AE_frame_prev <- left_join(ae_data, aes_prev, by = "serial_no")
AE_frame_ser <- left_join(ae_data, aes_ser, by = "serial_no")
#Fixing groups
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

AE_frame <- createGroups(AE_frame) 
AE_frame_prev <- createGroups(AE_frame_prev) 
AE_frame_ser <- createGroups(AE_frame_ser)

catFun <- function(df) {
        df <- mutate(df, cat_30_std = (if_else(rrr_both_30 == 1 & pos_30 == 1, "TP"
                                     , if_else(rrr_both_30 == 0 & pos_30 == 0, "TN"
                                     , if_else(rrr_both_30 == 1 & pos_30 == 0, "FN","FP")))))

        df<- mutate(df, cat_90_std = (if_else(rrr_both_90 == 1 & pos_90 == 1, "TP"
                                    , if_else(rrr_both_90 == 0 & pos_90 == 0, "TN"
                                    , if_else(rrr_both_90 == 1 & pos_90 == 0, "FN","FP")))))

        df <- mutate(df, cat_30_re = (if_else(rrr_30 == 1 & pos_30 == 1, "TP"
                                    , if_else(rrr_30 == 0 & pos_30 == 0, "TN"
                                    , if_else(rrr_90 == 1 & pos_90 == 0, "FN","FP")))))
        df <- mutate(df, cat_90_re = (if_else(rrr_90 == 1 & pos_90 == 1, "TP"
                                    , if_else(rrr_90 == 0 & pos_90 == 0, "TN"
                                    , if_else(rrr_90 == 1 & pos_90 == 0, "FN","FP")))))

        df <- select(df, serial_no, group, pos_30, pos_90, rrr_i, rrr_30
                   , rrr_90, rrr_both_30, rrr_both_90, cat_30_std, cat_90_std, cat_30_re, cat_90_re) 
}
AE_frame <- catFun(AE_frame)        
AE_frame_prev <- catFun(AE_frame_prev) 
AE_frame_ser <- catFun(AE_frame_ser)

#sensitivity and specificity 
calcAdjusted <- function(df, type) {
        aeDf <- table(df$group, df[[type]])
        as.data.frame.matrix(aeDf) %>%
                mutate(pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)#/21774
                       , adj_sens = (TP /(TP + FN) * pop)
                       , adj_spec = (TN / (TN + FP) * pop))
}

result_30_std <- calcAdjusted(AE_frame, type="cat_30_std")
result_90_std <- calcAdjusted(AE_frame, type="cat_90_std")
result_30_re <- calcAdjusted(AE_frame_prev, type="cat_30_re")
result_90_re <- calcAdjusted(AE_frame, type="cat_90_re")

getAdjustedSensitivity <- function(df, d = 1:nrow(df), type) {
        if (missing(type)) stop("Type required")
        adjDf <- calcAdjusted(df[d, ], type=type)
        sum(adjDf$adj_sens, na.rm = T)
}
getAdjustedSensitivity(AE_frame, type="cat_30_std")
getAdjustedSensitivity(AE_frame, type="cat_90_std")
getAdjustedSensitivity(AE_frame, type="cat_30_re")
getAdjustedSensitivity(AE_frame, type="cat_90_re")

getAdjustedSensitivity(AE_frame_prev, type="cat_30_std")
getAdjustedSensitivity(AE_frame_prev, type="cat_90_std")
getAdjustedSensitivity(AE_frame_prev, type="cat_30_re")
getAdjustedSensitivity(AE_frame_prev, type="cat_90_re")

getAdjustedSensitivity(AE_frame_ser, type="cat_30_std")
getAdjustedSensitivity(AE_frame_ser, type="cat_90_std")
getAdjustedSensitivity(AE_frame_ser, type="cat_30_re")
getAdjustedSensitivity(AE_frame_ser, type="cat_90_re")

getAdjustedSpecificity <- function(df, d = 1:nrow(df), type) {
        if (missing(type)) stop("Type required")
        adjDf <- calcAdjusted(df[d, ], type=type)
        sum(adjDf$adj_spec, na.rm = T)
}

getAdjustedSpecificity(AE_frame, type="cat_30_std")
getAdjustedSpecificity(AE_frame, type="cat_90_std")
getAdjustedSpecificity(AE_frame, type="cat_30_re")
getAdjustedSpecificity(AE_frame, type="cat_90_re")

getAdjustedSpecificity(AE_frame_prev, type="cat_30_std")
getAdjustedSpecificity(AE_frame_prev, type="cat_90_std")
getAdjustedSpecificity(AE_frame_prev, type="cat_30_re")
getAdjustedSpecificity(AE_frame_prev, type="cat_90_re")

getAdjustedSpecificity(AE_frame_ser, type="cat_30_std")
getAdjustedSpecificity(AE_frame_ser, type="cat_90_std")
getAdjustedSpecificity(AE_frame_ser, type="cat_30_re")
getAdjustedSpecificity(AE_frame_ser, type="cat_90_re")

sens_30_all_std <- boot(AE_frame, statistic = getAdjustedSensitivity, R = 2000, type = "cat_30_std")  
sens_90_all_std <- boot(AE_frame, statistic = getAdjustedSensitivity, R = 2000, type = "cat_90_std")
spec_30_all_std <- boot(AE_frame, statistic = getAdjustedSpecificity, R = 2000, type = "cat_30_std")  
spec_90_all_std <- boot(AE_frame, statistic = getAdjustedSpecificity, R = 2000, type = "cat_90_std")

sens_30_all_re <- boot(AE_frame, statistic = getAdjustedSensitivity, R = 2000, type = "cat_30_re")
sens_90_all_re <- boot(AE_frame, statistic = getAdjustedSensitivity, R = 2000, type = "cat_90_re")
spec_30_all_re <- boot(AE_frame, statistic = getAdjustedSpecificity, R = 2000, type = "cat_30_re")
spec_90_all_re <- boot(AE_frame, statistic = getAdjustedSpecificity, R = 2000, type = "cat_90_re")

boot.ci(sens_30_all_std, type = "perc")
boot.ci(sens_90_all_std, type = "perc")
boot.ci(spec_30_all_std, type = "perc")
boot.ci(spec_90_all_std, type = "perc")

boot.ci(sens_30_all_re, type = "perc")
boot.ci(sens_90_all_re, type = "perc")
boot.ci(spec_30_all_re, type = "perc")
boot.ci(spec_90_all_re, type = "perc")

sens_30_prev_std <- boot(AE_frame_prev, statistic = getAdjustedSensitivity, R = 2000, type = "cat_30_std")  
sens_90_prev_std <- boot(AE_frame_prev, statistic = getAdjustedSensitivity, R = 2000, type = "cat_90_std")
spec_30_prev_std <- boot(AE_frame_prev, statistic = getAdjustedSpecificity, R = 2000, type = "cat_30_std")  
spec_90_prev_std <- boot(AE_frame_prev, statistic = getAdjustedSpecificity, R = 2000, type = "cat_90_std")

sens_30_prev_re <- boot(AE_frame_prev, statistic = getAdjustedSensitivity, R = 2000, type = "cat_30_re")
sens_90_prev_re <- boot(AE_frame_prev, statistic = getAdjustedSensitivity, R = 2000, type = "cat_90_re")
spec_30_prev_re <- boot(AE_frame_prev, statistic = getAdjustedSpecificity, R = 2000, type = "cat_30_re")
spec_90_prev_re <- boot(AE_frame_prev, statistic = getAdjustedSpecificity, R = 2000, type = "cat_90_re")

boot.ci(sens_30_prev_std, type = "perc")
boot.ci(sens_90_prev_std, type = "perc")
boot.ci(spec_30_prev_std, type = "perc")
boot.ci(spec_90_prev_std, type = "perc")
boot.ci(sens_30_prev_re, type = "perc")
boot.ci(sens_90_prev_re, type = "perc")
boot.ci(spec_30_prev_re, type = "perc")
boot.ci(spec_90_prev_re, type = "perc")

sens_30_ser_std <- boot(AE_frame_ser, statistic = getAdjustedSensitivity, R = 2000, type = "cat_30_std")  
sens_90_ser_std <- boot(AE_frame_ser, statistic = getAdjustedSensitivity, R = 2000, type = "cat_90_std")
spec_30_ser_std <- boot(AE_frame_ser, statistic = getAdjustedSpecificity, R = 2000, type = "cat_30_std")  
spec_90_ser_std <- boot(AE_frame_ser, statistic = getAdjustedSpecificity, R = 2000, type = "cat_90_std")

sens_30_ser_re <- boot(AE_frame_ser, statistic = getAdjustedSensitivity, R = 2000, type = "cat_30_re")
sens_90_ser_re <- boot(AE_frame_ser, statistic = getAdjustedSensitivity, R = 2000, type = "cat_90_re")
spec_30_ser_re <- boot(AE_frame_ser, statistic = getAdjustedSpecificity, R = 2000, type = "cat_30_re")
spec_90_ser_re <- boot(AE_frame_ser, statistic = getAdjustedSpecificity, R = 2000, type = "cat_90_re")

boot.ci(sens_30_ser_std, type = "perc")
boot.ci(sens_90_ser_std, type = "perc")
boot.ci(spec_30_ser_std, type = "perc")
boot.ci(spec_90_ser_std, type = "perc")

boot.ci(sens_30_ser_re, type = "perc")
boot.ci(sens_90_ser_re, type = "perc")
boot.ci(spec_30_ser_re, type = "perc")
boot.ci(spec_90_ser_re, type = "perc")

rm(result_30_re, result_30_std, result_90_re, result_90_std)


#Incidence total
getAdjustedIncidence <- function(df, type) {
        aeDf <- table(df$group, df[[type]])
        as.data.frame.matrix(aeDf) %>%
                mutate(AEs = `1`
                       , pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)/21774
                       , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)
                       , rate = AEs / sample
                       , incidence = rate * pop)
}

calcAdjustedIncidence <- function(df, d = 1:nrow(df), type) {
        if (missing(type)) stop("Type required")
        adjDf <- getAdjustedIncidence(df[d, ], type=type)
        sum(adjDf$incidence)
}
calcAdjustedIncidence(AE_frame, type = "rrr_both_30")
calcAdjustedIncidence(AE_frame, type = "rrr_both_90")

calcAdjustedIncidence(AE_frame_prev, type = "rrr_both_30")
calcAdjustedIncidence(AE_frame_prev, type = "rrr_both_90")

calcAdjustedIncidence(AE_frame_ser, type = "rrr_both_30")
calcAdjustedIncidence(AE_frame_ser, type = "rrr_both_90")

boot_30 <- boot(AE_frame, statistic = calcAdjustedIncidence, R = 3000, type = "rrr_both_30")
boot_90 <- boot(AE_frame, statistic = calcAdjustedIncidence, R = 3000, type = "rrr_both_90")
boot.ci(boot_30, type = "perc")
boot.ci(boot_90, type = "perc")

boot_30_prev <- boot(AE_frame_prev, statistic = calcAdjustedIncidence, R = 3000, type = "rrr_both_30")
boot_90_prev <- boot(AE_frame_prev, statistic = calcAdjustedIncidence, R = 3000, type = "rrr_both_90")
boot.ci(boot_30_prev, type = "perc")
boot.ci(boot_90_prev, type = "perc")

boot_30_ser <- boot(AE_frame_ser, statistic = calcAdjustedIncidence, R = 1500, type = "rrr_both_30")
boot_90_ser <- boot(AE_frame_ser, statistic = calcAdjustedIncidence, R = 1500, type = "rrr_both_90")
boot.ci(boot_30_ser, type = "perc")
boot.ci(boot_90_ser, type = "perc")
#Incidence Multiple AEs incidence rate
aes_all <- createGroups(aes_all) %>%
        mutate(AE = if_else(rrr_i == 1 | rrr_30 == 1, 1, 0))
aes_p <- createGroups(aes_p) %>%
        mutate(AE = if_else(rrr_i == 1 | rrr_30 == 1, 1, 0))
aes_s <- createGroups(aes_s) %>%
        mutate(AE = if_else(rrr_i == 1 | rrr_30 == 1, 1, 0))

getAdjustedIncidence <- function(df, type) {
        aeDf <- table(df$group, df[[type]])
        as.data.frame.matrix(aeDf) %>%
                mutate(AEs = `1`
                       , pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)/21774
                       , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)
                       , rate = AEs / sample
                       , incidence = rate * pop)
}


getAdjustedIncidence <- function(df, type) {
        aeDf <- table(df$group, df[[type]])
        as.data.frame.matrix(aeDf) %>%
                mutate(AEs = `1`
                       , pop = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547)/21774
                       , sample = c(294, 442, 293, 194, 33, 130, 49, 195, 74, 294)
                       , rate = AEs / sample
                       , incidence = rate * pop)
}

calcAdjustedIncidence <- function(df, d = 1:nrow(df), type) {
        if (missing(type)) stop("Type required")
        adjDf <- getAdjustedIncidence(df[d, ], type=type)
        sum(adjDf$incidence)
}
calcAdjustedIncidence(aes_all, type = "AE")
boot_obj <- boot(aes_all, statistic = calcAdjustedIncidence, R = 3000, type = "AE")
boot.ci(boot_obj, type = "perc")

calcAdjustedIncidence(aes_p, type = "AE")
boot_obj <- boot(aes_p, statistic = calcAdjustedIncidence, R = 3000, type = "AE")
boot.ci(boot_obj, type = "perc")

calcAdjustedIncidence(aes_s, type = "AE")
boot_obj <- boot(aes_s, statistic = calcAdjustedIncidence, R = 2000, type = "AE")
boot.ci(boot_obj, type = "perc")




####
# test <- mutate(aes_all, inj_grp = if_else(grepl("Allergisk", inj_type), "Allergic"
#                         , if_else(grepl("Anestesirelaterad", inj_type), "Anaesthesia"
#                         , if_else(grepl("Blås", inj_type), "Urine retention"
#                         , if_else(grepl("Blödning", inj_type), "Bleeding"
#                         , if_else(grepl("Fall", inj_type), "Fall"
#                         , if_else(grepl("Hudskada", inj_type), "Skin"
#                         , if_else(grepl("Infektion", inj_type), "Infection"
#                         , if_else(grepl("Kirurgisk", inj_type), "Surgical"
#                         , if_else(grepl("Neurologisk", inj_type), "Neurological"
#                         , if_else(grepl("Smärta", inj_type), "Pain"
#                         , if_else(grepl("Stroke", inj_type), "Stroke"
#                         , if_else(grepl("Svikt", inj_type), "Failure"
#                         , if_else(grepl("Trombos", inj_type), "Thrombos"
#                         , if_else(grepl("Trycksår", inj_type), "Decubitus"
#                         , if_else(grepl("Övrigt", inj_type), "Misc", "other" ))))))))))))))))
# 
# table(test$inj_grp, test$correct_ICD_code)
