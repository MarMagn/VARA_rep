all_results_30 = list()
class(all_results_30) <- c("results_class", class(all_results_30))
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
all_results_90 = list()
class(all_results_90) <- c("results_class", class(all_results_90))
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
all_results_fx_30 = list()
class(all_results_fx_30) <- c("results_class", class(all_results_fx_30))
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
all_results_fx_90 = list()
class(all_results_fx_90) <- c("results_class", class(all_results_fx_90))
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
all_results_el_30 = list()
class(all_results_el_30) <- c("results_class", class(all_results_el_30))
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
all_results_el_90 = list()
class(all_results_el_90) <- c("results_class", class(all_results_el_90))
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

code_results_30 = list()
class(code_results_30) <- c("results_class", class(code_results_30))
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

code_results_90 = list()
class(code_results_90) <- c("results_class", class(code_results_90))
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
