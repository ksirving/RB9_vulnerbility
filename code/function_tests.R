
thrsh <- 0.79; tails <- 0.1; modls = 'core'

function(datin, thrsh = 0.79, tails = 0.05, modls = c('core', 'full'), lbs = list('likely unconstrained' = 0, 'possibly unconstrained' = 1, 'possibly constrained' = 2, 'likely constrained' = 3)){
  
  # sanity check
  if(tails >= 0.5)
    stop('tails must be less than 0.5')
  
  # models argument
  modls <- match.arg(modls)
  modls
  match.arg
  dat <- spat
  st_geometry(dat) <- NULL
  dat <- dat %>% 
    dplyr::select(matches(paste0('^COMID$|^', modls, '0'))) %>% 
    gather('var', 'val', -COMID) %>% 
    arrange(COMID, var) %>% 
    group_by(COMID) %>% 
    nest %>% 
    mutate(
      strcls_int = purrr::map(data, function(x){
        
        # return NA if any zero values in predictions
        if(any(is.na(x$val))){
          
          cls <- NA
          return(cls)
          
        } 
        
        # get quantile labels to filter
        lovl <- 100 * tails
        hivl <- 100 * (1 -  tails) 
        vls <- c(lovl, 50, hivl) %>% 
          str_pad(2, pad = '0') %>% 
          paste0(modls, '0.', .)
        
        # filter by quantile labels and median
        ints <- x %>% 
          filter(var %in% vls) %>% 
          .$val 
        
        cls <- findInterval(thrsh, ints)
        
        return(cls)
        
      })
      
    ) %>% 
    dplyr::select(-data) %>% 
    unnest 
  
  # subset lbs by those in interval
  lbs <- unique(dat$strcls_int) %>% 
    na.omit %>% 
    as.numeric %>% 
    match(unlist(lbs)) %>% 
    lbs[.] %>% 
    .[names(sort(unlist(.)))]
  
  # strcls as correct factor levels
  dat <- dat %>%
    mutate(strcls = factor(strcls_int, levels = unlist(lbs), labels = names(lbs)))
  
  return(dat)
  
}