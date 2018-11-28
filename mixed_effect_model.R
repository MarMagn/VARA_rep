library(lme4)
library(tidyverse)
library(htmlTable)
source("load_base_files.R")
rm(dead, keys, mort, sp_total)
fn <- file.path(base_location, "ae_data.csv")
rrr_data <- read.delim2(file=fn, encoding="latin", sep = ";") %>%
  filter(causality > 2) %>%
  select(serial_no, event_date, inj_type, causality, avoidability, NCCMERP)
rrr_data <- na.omit(rrr_data)

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

base_location <- file.path("/Volumes/NO NAME/VARA_DATA_orginal/")
LOF <- read.delim("/Volumes/NO NAME/LOF_AE_description_x.csv", 
                  header=TRUE, 
                  encoding="latin1", 
                  stringsAsFactors=FALSE, sep = ";")
claims <- select(LOF, serial_no) %>%
  group_by(serial_no) %>%
  summarise(claim = n())
a <- data.frame(serial_no = as.integer(14308), age = as.integer(69), los = as.integer(3), sex = as.integer(1), fx = "Nej", cause = "Primär artros (Arthrosis NUD)
", group = as.numeric(6), claim = as.integer(1))

df <- left_join(rrr_data, ac, by = "serial_no") %>%
  distinct(serial_no, .keep_all = T) %>% 
  createGroups %>% 
  left_join(claims, by = "serial_no")%>%
  select(serial_no, age, los, sex, fx, cause, group, claim) 
df <-  bind_rows(df, a) %>%
  mutate(claim = if_else(is.na(claim), 0L, claim),
         serial_no = factor(serial_no),
         sex = factor(sex),
         fx = factor(fx),
         group = factor(group))

library(broom)
res <- by(data = df, INDICES = df$group, FUN = function(subdata) {
  if (all(subdata$claim == 0)) {
    return(list(
      est = NA,
      upper_ci = 1 - 0.05^(1/nrow(subdata)),
      n = nrow(subdata)
    ))
  }
  res <- glm(claim ~ 1, data = subdata, family = poisson)
  list(
    est = exp(coef(res)),
    upper_ci = exp(confint(res)[2]),
    n = nrow(subdata)
  )
})
merged_res <- res[!sapply(as.list(res), function(x) all(is.na(x)))] %>% bind_rows
b <- data.frame(Population = c(630, 631, 403, 666, 289, 12628, 206, 3237, 537, 2547))
res_w_pop <- bind_cols(merged_res, b) %>%
  mutate(est = if_else(is.na(est), 0, est), prop = Population/sum(Population),
         adj_est = prop*est, adj_ci = prop*upper_ci)
tbldf <- adorn_totals(res_w_pop, "row")

sum(res_w_pop$adj_est)
sum(res_w_pop$adj_ci)
c <- c("Estimate", "Upper CI", "AES, n=", "Population, n=", "Population fraction", "Adjusted estimate", "Adjusted upper CI")
htmlTable(tbldf %>% txtRound(digits=3), header = c, total = TRUE)



final_variance <- 
  merged_res %>% 
  mutate(biased_n = n - 1,
         sum_var = var * biased_n) %>% 
  summarise(var = sum(sum_var),
            denominator = sum(biased_n)) %>% 
  mutate(final = var / denominator)

getMEMdata <- function(ds) {
  mem_datasets <- list()
  for (groups in c(1:10)) {
    ds_name <- sprintf("%s", groups)
    data <- filter(ds, group == groups)
    mem_datasets[[paste("group", paste(groups))]]  <- data
  }
  return(mem_datasets)
}
mem_data <- getMEMdata(df)

apply_me_model <- function(ds){
  me_model <- glmer(claim~ age + fx + sex + NCCMERP  + (1|serial_no) 
                    , data = ds, family = poisson
                    , control = glmerControl(optimizer = "bobyqa"))
  se <- sqrt(diag(vcov(me_model)))
  (tab <- cbind(Est = fixef(me_model), LL = fixef(me_model) - 1.96 * se, UL = fixef(me_model) + 1.96 * se))
  exp(tab)
}

mem_results <- lapply(mem_data, apply_me_model)





m1<- glmer(claim~ age + fx + sex + NCCMERP  + (1|serial_no) , data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"))
se <- sqrt(diag(vcov(m1)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(m1), LL = fixef(m1) - 1.96 * se, UL = fixef(m1) + 1.96 *
                se))
exp(tab)

df2 <- distinct(df, serial_no, .keep_all = T)
table(df2$group, df2$claim)
df3 <- filter(df2, claim == 1)
x <- anti_join(LOF, df3, by = "serial_no")


DF <- data.frame(a = 1:3, b = letters[10:12],
                 c = seq(as.Date("2004-01-01"), by = "week", len = 3),
                 stringsAsFactors = TRUE)
data.matrix(DF[1:2])
data.matrix(DF)
