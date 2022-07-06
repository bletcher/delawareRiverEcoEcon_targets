# R/functions.R
# Put code that want to source into "_targets.R" here.


  #####################################
  ## general functions 
  #####################################
  `%notin%` <- Negate(`%in%`)
  #####################################
  ## getData functions 
  #####################################
  
  addRiverN <- function(d){
    rivers <- unique(d$Water) %>% sort()

    d %>% 
      mutate(riverN = as.numeric(factor(d$Water, levels = rivers)))
  }



  #####################################
  ## encounter histories 
  #####################################
  getNeverCaptured <- function(d){
    d %>%
      #filter(ageInSamples > 0 & ageInSamples <= maxOccasionValue) %>%
      #filter(ageInSamples %in% 1:maxOccasionValue) %>%
      group_by(tag) %>%
      summarize(sumEnc = sum(enc, na.rm = TRUE)) %>%
      filter(sumEnc == 0) %>%
      dplyr::select(tag)
  }
  
  getGT1ObsPerOcc <- function(d) {
    d %>%
      dplyr::group_by(tag, dateYM) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n > 1L)
  }
  
  keepOnlyFirstObsPerOcc <- function(d) {
    # for fish that were observed gt 1 time per occasion (dateYM)
    rowsToRemove <- d %>%
      filter(tag %in% getGT1ObsPerOcc(d)$tag) %>%
      group_by(tag, dateYM) %>%
      summarise(dateTime = max(dateTime), n = n()) %>%
      filter(n > 1) %>%
      select(tag, dateTime)
    
    dOut <- d %>%
      anti_join(rowsToRemove)
  }
  
  op_call <- function(op, lhs, rhs) {
    call(op, sym(lhs), rhs)
  }
  
  ehFilter <- function(data, cols, ops, vals) {
    exprs <- purrr::pmap(list(ops, cols, vals), op_call)
    data %>% dplyr::filter(!!!exprs)
  }
  
  getEHDataWide <- function(d, cols, ops, vals, var, valuesFill = 0){
    #print(c("in getEHWide",d))
    
    dFiltered0 <- d %>%
      ehFilter(cols, ops, vals) %>% 
      filter(tag != "", tag != "ad") %>%
      filter(species == "brown trout") %>%
      mutate(dateC = as.character(Date)) 
    
    dFiltered <- keepOnlyFirstObsPerOcc(dFiltered0)
    
    dFiltered %>%
      pivot_wider(
        id_cols = tag,
        names_from = dateYM,
        names_prefix = "date_",
        values_from = eval(substitute(var)),
        #values_fill = as.character(valuesFill)
        values_fill = valuesFill
      )
  }
  
  getEH <- function(d, cols, ops, vals ){
    
    # Fish with no observed occasions
    neverCaptured <- getNeverCaptured(d)
    d <- d %>%
      filter(tag %notin% neverCaptured$tag)
    
    encWide <- getEHDataWide(d, cols, ops, vals, "enc", valuesFill = 0)
    eh <- as.matrix(encWide %>% dplyr::select(-tag), nrow = nrow(encWide), ncol = ncol(encWide) - 1)
    
    riverWide <- getEHDataWide(d, cols, ops, vals, "Water", valuesFill = NA)
    riverMatrix <- as.matrix(riverWide %>% dplyr::select(-tag), nrow = nrow(riverWide), ncol = ncol(riverWide) - 1)
    
    riverNWide <- getEHDataWide(d, cols, ops, vals, "riverN", valuesFill = 0)
    riverNMatrix <- as.matrix(riverNWide %>% dplyr::select(-tag), nrow = nrow(riverNWide), ncol = ncol(riverNWide) - 1)
    
    tags <- encWide %>% dplyr::select(tag)
    
    data <- d %>%
      ehFilter(cols, ops, vals) %>% 
      #filter(ageInSamples > 0, ageInSamples <= maxOccasionValue) %>%
      #filter(ageInSamples %in% 1:maxOccasionValue) %>%
      arrange(tag, date)
    
    first <- apply(eh, 1, function(x) min(which(x != 0)))
    last <- apply(riverMatrix, 1, function(x) max(which(!is.na(x))))
    last <- ifelse(last == ncol(riverMatrix), last, last - 1)
    
    return(list(eh = eh,
                riverMatrix = riverMatrix,
                riverNMatrix = riverNMatrix,
                tags = tags, 
                first = first, 
                last = last, 
                data = data))
  }
  
  
