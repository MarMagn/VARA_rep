final_results_30 = list()
class(final_results_30) <- c("results_class", class(final_results_30))
print.results_class <- function(l) {
  if (length(l) == 0) {
    cat('\n None')
    return()
  }
  
  sapply(l, USE.NAMES = TRUE, simplify = FALSE, function(vals) {
    sapply(vals, function(v) {
      sprintf("%.3f", v)
    })
  }) %>% 
    do.call(rbind, .) %>% 
    knitr::kable() %>% 
    print
}
final_results_90 = list()
class(final_results_90) <- c("results_class", class(final_results_90))
print.results_class <- function(l) {
  if (length(l) == 0) {
    cat('\n None')
    return()
  }
  
  sapply(l, USE.NAMES = TRUE, simplify = FALSE, function(vals) {
    sapply(vals, function(v) {
      sprintf("%.3f", v)
    })
  }) %>% 
    do.call(rbind, .) %>% 
    knitr::kable() %>% 
    print
}
final_results_codes_30 = list()
class(final_results_codes_30) <- c("results_class", class(final_results_codes_30))
print.results_class <- function(l) {
  if (length(l) == 0) {
    cat('\n None')
    return()
  }
  
  sapply(l, USE.NAMES = TRUE, simplify = FALSE, function(vals) {
    sapply(vals, function(v) {
      sprintf("%.3f", v)
    })
  }) %>% 
    do.call(rbind, .) %>% 
    knitr::kable() %>% 
    print
}
final_results_codes_90 = list()
class(final_results_codes_90) <- c("results_class", class(final_results_codes_90))
print.results_class <- function(l) {
  if (length(l) == 0) {
    cat('\n None')
    return()
  }
  
  sapply(l, USE.NAMES = TRUE, simplify = FALSE, function(vals) {
    sapply(vals, function(v) {
      sprintf("%.3f", v)
    })
  }) %>% 
    do.call(rbind, .) %>% 
    knitr::kable() %>% 
    print
}
