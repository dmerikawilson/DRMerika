DRM_data_setup <- function() {
  stopifnot(require(dplyr))
  recode_class <- function(studied, list) {
    if (studied ==1) {
      'target'
    } else if (list == 'critical') {
      'critical'
    } else if (list == '') {
      'lure'
    } else {
      'related'
    }
  }
  
  fList <- list.files(path = file.path("data"), pattern = "*TestData.csv", full.names = TRUE)
  data <- do.call(rbind, lapply(fList, read.csv))
  names(data) <- tolower(names(data))
  
  data <- data %>%  
    mutate(word = tolower(word),
           list = tolower(list),
           rating = as.numeric(substring(new, 1,1)),
           class = mapply(recode_class, studied, list),
           list = replace(list, list == '', NA),
           old_new = ifelse(new <= 3, "new", "old"),
           acc = ifelse((studied == 1 & old_new == 'old') | (studied == 0 & old_new == 'new'), 1, 0)) %>%
    select(subject = subnum, trial = trialnum, word, list, list_type = type, class, studied, rating, rt, old_new, acc)
  
  return(data)
}
