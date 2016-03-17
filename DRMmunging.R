DRM_data_setup <- function() {
  stopifnot(require(dplyr)) #Makes sure package is installed and loaded
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
  options(stringsAsFactors = FALSE)
  fList <- list.files(path = file.path("data"), pattern = "*TestData.csv", full.names = TRUE) #Reading in files in "Data" folder with that extension
  data <- do.call(rbind, lapply(fList, read.csv))
  names(data) <- tolower(names(data))
  
  fList <- list.files(path = file.path("data"), pattern = "*StudyData.csv", full.names = TRUE) #Reading in files in "Data" folder with that extension
  studyData <- do.call(rbind, lapply(fList, read.csv))
  names(studyData) <- tolower(names(studyData))
  list_length <- studyData %>%
    group_by(subnum, list) %>%
    summarise(list_length = n()) %>%
    mutate(list = ifelse(list == "", NA, tolower(list)),
           list_length = replace(list_length, is.na(list), 0))
  
  data <- data %>%  
    mutate(word = tolower(word),
           list = tolower(list),
           rating = as.numeric(substring(new, 1,1)),
           class = mapply(recode_class, studied, list),
           list = replace(list, list == '', NA),
           old_new = ifelse(rating <= 3, "new", "old"),
           acc = ifelse((studied == 1 & old_new == 'old') | (studied == 0 & old_new == 'new'), 1, 0)) %>%
    left_join(y=list_length, by=c("subnum","list")) %>%
    select(subject = subnum, trial = trialnum, word, list, list_type = type,
           list_length, class, studied, rating, rt, old_new, acc)
  
  return(data)
}
