# Analysis LÖF

rm(list = ls())
library(tidyverse)

LOF <- read.delim("/Users/martinmagneli/Desktop/Testanalys VARA/LOF_AE_description_test.csv", 
                              header=TRUE, 
                              encoding="latin1", 
                              stringsAsFactors=FALSE, sep = ";")

table(LOF$AE_type_1)
source("Index frame.R")

LOF_index_all <- left_join(LOF, index, by = "serial_no")
LOF_index <- distinct(LOF_index_all, serial_no, .keep_all = T)
x <- select(LOF_index, serial_no) %>%
        mutate(claim = 1)

index <- left_join(index, x, by = "serial_no")
index$claim[is.na(index$claim)] <- 0

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



mean(LOF_index_all$AE_1_disable_., na.rm = T)
range(LOF_index_all$AE_1_disable_., na.rm = T)

x <- data.frame(sex_l = c, sex_i = index$sex)
chisq.test(index$claim, index$sex)
chisq.test(index$fx, index$claim)
#############
test <- read.delim("/Users/martinmagneli/Desktop/Testanalys VARA/LOF_data1.csv", 
                   header=TRUE, 
                   encoding="latin1", 
                   stringsAsFactors=FALSE, sep = ";")

inf <- filter (LOF, AE_type_1 == "inf_d")
inf <- filter(inf, serial_no != "17413")

t.test(LOF_index$age, index$age)
t.test(LOF_index$los, index$los)
        
