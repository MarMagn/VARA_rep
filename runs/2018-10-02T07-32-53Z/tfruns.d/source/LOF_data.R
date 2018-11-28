# Analysis LÖF

rm(list = ls())
library(tidyverse)
source("Index frame.R")
base_location <- file.path("/Volumes/NO NAME/VARA_DATA_orginal/")
LOF <- read.delim("/Volumes/NO NAME/LOF_AE_description_x.csv", 
                              header=TRUE, 
                              encoding="latin1", 
                              stringsAsFactors=FALSE, sep = ";")
encoding_type = "latin1"
fn <- file.path(base_location, "ae_data.csv")
rrr_data <- read.csv(file=fn, encoding=encoding_type)

rrr_data$event_date <- as.Date(rrr_data$event_date)

aes <- rrr_data %>%
  filter( causality > 2 ) %>%
  left_join(ac, by = "serial_no")%>%
  distinct(serial_no) %>%
  mutate(class = "AE")

ac <- select(ac, serial_no, cause) 
index <- left_join(index, ac, by = "serial_no")
LOF_index_all <- left_join(LOF, index, by = "serial_no")
LOF_index <- distinct(LOF_index_all, serial_no, .keep_all = T)

l <- LOF_index %>%
  distinct(serial_no) %>%
  mutate(class = "LOF")

aes <- anti_join(aes, l, by = "serial_no")

no_ae <- anti_join(keys, aes, by = "serial_no") %>%
  mutate(class = "no_AE") %>%
  select(serial_no, class) %>%
  anti_join(l, by = "serial_no")

lof_frame <- rbind(l, aes, no_ae)

lof_frame <- left_join(index, lof_frame, by = "serial_no") 

x <- lof_frame %>%
  group_by(cause) %>%
  summarise(n())

#Table 1 data
mean(LOF_index$age)
mean(index$age)
median(LOF_index$age)
median(index$age)
range(LOF_index$age)
range(index$age)
range(LOF_index$age)
table(LOF_index$sex)
table(index$sex)

table(LOF_index$fx)
table(index$fx)
table(index$sex)
table(LOF_index$sex)
table(index$fx)
LOF_index <- mutate(LOF_index, age_grp = if_else(age < "65", 1, 2 ))
index <- mutate(index, age_grp = if_else(age < "65", 1, 2 ))
table(LOF_index$age_grp)
table(index$age_grp)

20/59
39/59

377/1998
1621/1998

LOF_index_all$AE1_reimbursment <- as.numeric(LOF_index_all$AE1_reimbursment)
sum(LOF_index_all$AE1_reimbursment, na.rm = T)



#demographics and sens/spec
a1 <- LOF_index %>%
  distinct(serial_no) %>%
  mutate(class= "LOF", lof = 1)
lof_sens <- left_join(keys, a1, by = "serial_no")
lof_sens$lof[is.na(lof_sens$lof)] <- 0

a2 <- aes_p %>% # for all aes change to aes_all, this is preventable with option for major prev
  #filter(NCCMERP > 5) %>%
  distinct(serial_no) %>%
  mutate(class= "AE", rrr = 1) 
lof_sens <- left_join(lof_sens, a2, by = "serial_no")
lof_sens$rrr[is.na(lof_sens$rrr)] <- 0 
lof_sens <-  mutate(lof_sens, tag = if_else(lof == 1 & rrr == 1, "TP", if_else(lof == 0 & rrr == 0, "TN", if_else(lof == 1 & rrr == 0, "FP", "FN"))))
results <- table(lof_sens$tag)
results
results['TP']/(results['TP'] + results['FN'])
results['TN']/(results['TN'] + results['FP'])

a3 <- anti_join(a2, a1, by = "serial_no")

a3 <- anti_join(keys, a2, by = "serial_no") %>%
  anti_join(a1, by = "serial_no") %>%
  select(serial_no) %>%
  mutate(class = "NONE")

a4 <- rbind(a1, a2, a3) 

a5 <-  left_join(index, a4, by = "serial_no") %>%
  group_by(class) %>%
  summarise(median(age))


b1 <- aes_all %>%
  group_by(inj_type) %>%
  summarise(n())
        
table(a4$class)
