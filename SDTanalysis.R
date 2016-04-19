zTransform <- function(x, correct = TRUE, na.rm = FALSE, ...) {
  rate <- mean(x, na.rm = na.rm)
  if (correct && (rate == 0 || rate == 1)) {
    rate <- max(1/(2 * length(x)), rate - 1/(2 * length(x)))
  }
  return(qnorm(rate, ...))
}

SDTsummary <- function(data, grouping_variables = c("subject", "list_type", "class", "list_length", "group")) {
  
  stopifnot(require(dplyr))
  if ( !all(grouping_variables %in% names(data)) ) {
    stop("Not all grouping variables are found in the dataset")
  }
  
  combinations <- data.frame(target_dist = c("target","target","target","critical","critical","related"), 
                             lure_dist = c("lure","critical","related","lure","related","lure"))
  
  unrel_lures <- filter(data, class == "lure") %>%
    group_by_(.dots = grouping_variables) %>%
    summarise(FA = mean(!acc),
              nObs = n(),
              zFA =  zTransform(!acc)) %>% 
    rename(lure_dist = class)
  
  rel_lures <-  filter(data, class %in% c("related", "critical")) %>%
    group_by_(.dots = grouping_variables) %>% 
    summarise(FA = mean(!acc),
              nObs = n(),
              zFA =  zTransform(!acc)) %>% 
    rename(lure_dist = class)
  
  targets <- filter(data, class == "target") %>%
    group_by_(.dots = grouping_variables) %>% 
    summarise(HR = mean(acc),
              nObs = n(),
              zHR =  zTransform(acc)) %>%
    rename(target_dist = class) %>%
    ungroup()
  
  fullSummary <- targets %>%
    rbind(rename(ungroup(rel_lures), HR = FA, zHR = zFA, target_dist = lure_dist)) %>%
    left_join(combinations, by = "target_dist") %>%
    left_join(rel_lures, by= c(grouping_variables[grouping_variables != 'class'], "lure_dist")) %>%
    rename(nObs.HR = nObs.x, nObs.FA = nObs.y) %>%
    left_join(unrel_lures, by = c(grouping_variables[!(grouping_variables %in% c('list_length','class'))], "lure_dist")) %>%
    mutate(FA.x = pmin(FA.x,FA.y, na.rm = TRUE),
           nObs.FA = pmin(nObs.FA, nObs, na.rm = TRUE),
           zFA.x = pmin(zFA.x,zFA.y, na.rm = TRUE),
           lure_list_length = ifelse(is.na(list_length.y), list_length.x, list_length.y)) %>%
    select(-nObs, -FA.y, -zFA.y, -list_length.y) %>%
    rename(FA = FA.x, zFA = zFA.x, target_list_length = list_length.x) %>%
    mutate(dprime = zHR - zFA)

  return(fullSummary)
  
}
