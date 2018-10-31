getModelDatasets <- function(ds) {
  model_datasets <- list()
  for (days in c(30, 90)) {
    ds_name <- sprintf("%s", days)
    drop_vars <- c("serial_no")
    if (days == 30) {
      drop_vars <- c(drop_vars, paste(c("rrr", "pos", "maj"), 90, sep="_"))
      rename_vars <- c(has_AE = "rrr_30")
      rename_maj <- c(has_AE = "maj_30")
      raw_var <- "pos_30"
      ind_var <- "index_ae"
      maj_var <- "maj_30"
      rrr_var <- "rrr_30"
    } else if (days == 90) {
      drop_vars <- c(drop_vars, paste(c("rrr", "pos", "maj"), 30, sep="_"))
      rename_vars <- c(has_AE = "rrr_90")
      rename_maj <- c(has_AE = "maj_90")
      raw_var <- "pos_90"
      ind_var <- "index_ae"
      maj_var <- "maj_90"
      rrr_var <- "rrr_90"
    } else {
      stop("Days not implemented")
    }
    
    model_datasets[[sprintf("%s_index_c", ds_name)]] <- select(ds, -!!drop_vars, -!!maj_var, !!rename_vars)
    model_datasets[[sprintf("%s_wc", ds_name)]] <- model_datasets[[sprintf("%s_index_c", ds_name)]] %>%
    select(-!!ind_var)
    model_datasets[[ds_name]] <- model_datasets[[sprintf("%s_wc", ds_name)]] %>%
    select(-!!raw_var)
    model_datasets[[sprintf("%s_maj_index_c", ds_name)]] <- select(ds, -!!drop_vars, -!!rrr_var, rename_maj)
    model_datasets[[sprintf("%s_maj_wc", ds_name)]] <- model_datasets[[sprintf("%s_maj_index_c", ds_name)]] %>%
      select(-!!ind_var)
    model_datasets[[sprintf("%s_maj", ds_name)]] <- model_datasets[[sprintf("%s_maj_wc", ds_name)]] %>%
      select(-!!raw_var)
    model_datasets[[sprintf(ds_name)]] <- select(ds, -!!drop_vars, -!!maj_var, -!!raw_var, -!!ind_var, rename_vars)
  }
  return(model_datasets)
}
model_datasets <- getModelDatasets(no_na_data)
ds <- no_na_data
