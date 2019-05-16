## Utililies functions
## --------------------------------------------------------------------------------
#' @import magrittr
dur.from <- function(start.time) {
  Sys.time() %>% 
    subtract(start.time) %>%
    as.numeric %>%
    humanFormat::formatSeconds()
}


## Iterative function application with message.n
pipe.fun.list <- function(x, fun.list) {
  for(fun in fun.list)
    x <- if (!is.list(fun)) fun(x)
         else do.call(fun[[1]], c(list(x), fun[-1]))
  return(x)
}

## Tests:
## c(0.3) %>% pipe.fun.list(list(sum, sqrt, list(log, 10), abs))
## c(0.3) %>% pipe.fun.list(list(sum, sqrt, log, abs))


pipe.fun.list.n <- function(fun.list, x
                          , message = ".\t"
                          , appendLF = FALSE) {
  for(i in 1:length(fun.list)) {
    message(i, ".\t", appendLF = FALSE)    
    fun <- fun.list[[i]]
    x <- if (!is.list(fun)) fun(x)
         else do.call(fun[[1]], c(list(x), fun[-1]))
  }
  return(x)
} 

## c(0.3) %>% pipe.fun.list.n(list(sum, sqrt, log, abs))


## Parallelisation of functions
## getOption("cores") - does not work on windows... (blade - 48)

## ## stringr::str_trim
## str.trim <- function(x, ...) 
##   parallel::pvec(x, stringr::str_trim, ..., mc.cores = parallel::detectCores())

## ## stringr::str_extract
## str.extract <- function(x, ...) 
##   parallel::pvec(x, stringr::str_extract, ..., mc.cores = parallel::detectCores())

## ## stringr::str_detect
## str.detect <- function(x, ...) 
##   parallel::pvec(x, stringr::str_detect, ..., mc.cores = parallel::detectCores())

## ## stringr::str_replace
## str.replace <- function(x, ...) 
##   parallel::pvec(x, stringr::str_replace, ..., mc.cores = parallel::detectCores())

## ## stringr::str_replace_all
## str.replace.all <- function(x, ...) 
##   parallel::pvec(x, stringr::str_replace_all, ..., mc.cores = parallel::detectCores())




## no forking... (whatever that means)
## stringr::str_trim
str.trim <- function(x, ...) {
  if(exists("cl")) {
    x <- clusterSplit(cl, x)
    x <- parallel::clusterApply(cl, x, stringr::str_trim, ...)
    x <- unlist(x, recursive = FALSE)
    return(x) 
  } else {
    cl <- parallel::makeCluster(parallel::detectCores(), type='PSOCK')
    x <- clusterSplit(cl, x)
    x <- parallel::clusterApply(cl, x, stringr::str_trim, ...)
    x <- unlist(x, recursive = FALSE)
    parallel::stopCluster(cl)
    return(x) 
  } 
}

## stringr::str_extract
str.extract <- function(x, ...) {
  if(exists("cl")) {
    x <- clusterSplit(cl, x)
    x <- parallel::clusterApply(cl, x, stringr::str_extract, ...)
    x <- unlist(x, recursive = FALSE)
    return(x) 
  } else {
    cl <- parallel::makeCluster(parallel::detectCores(), type='PSOCK')
    x <- clusterSplit(cl, x)
    x <- parallel::clusterApply(cl, x, stringr::str_extract, ...)
    x <- unlist(x, recursive = FALSE)
    parallel::stopCluster(cl)
    return(x) 
  } 
}

## stringr::str_detect
str.detect <- function(x, ...) {
  if(exists("cl")) {
    x <- clusterSplit(cl, x)
    x <- parallel::clusterApply(cl, x, stringr::str_detect, ...)
    x <- unlist(x, recursive = FALSE)
    return(x) 
  } else {
    cl <- parallel::makeCluster(parallel::detectCores(), type='PSOCK')
    x <- clusterSplit(cl, x)
    x <- parallel::clusterApply(cl, x, stringr::str_detect, ...)
    x <- unlist(x, recursive = FALSE)
    parallel::stopCluster(cl)
    return(x) 
  } 
}

## stringr::str_replace
str.replace <- function(x, ...) {
  if(exists("cl")) {
    x <- clusterSplit(cl, x)
    x <- parallel::clusterApply(cl, x, stringr::str_replace, ...)
    x <- unlist(x, recursive = FALSE)
    return(x) 
  } else {
    cl <- parallel::makeCluster(parallel::detectCores(), type='PSOCK')
    x <- clusterSplit(cl, x)
    x <- parallel::clusterApply(cl, x, stringr::str_replace, ...)
    x <- unlist(x, recursive = FALSE)
    parallel::stopCluster(cl)
    return(x) 
  } 
}

## stringr::str_replace_all
str.replace.all <- function(x, ...) {
  if(exists("cl")) {
    x <- clusterSplit(cl, x)
    x <- parallel::clusterApply(cl, x, stringr::str_replace_all, ...)
    x <- unlist(x, recursive = FALSE)
    return(x) 
  } else {
    cl <- parallel::makeCluster(parallel::detectCores(), type='PSOCK')
    x <- clusterSplit(cl, x)
    x <- parallel::clusterApply(cl, x, stringr::str_replace_all, ...)
    x <- unlist(x, recursive = FALSE)
    parallel::stopCluster(cl)
    return(x) 
  }
}

## lapply
plapply <- function(x, fun, ...) {
  if(exists("cl")) {
    x <- parallel::parLapply(cl, x, fun, ...)
    return(x)
  } else {
    cl <- parallel::makeCluster(parallel::detectCores(), type='PSOCK')
    x <- parallel::parLapply(cl, x, fun, ...)
    parallel::stopCluster(cl)
    return(x)
  }
}


## tests
## --------------------------------------------------------------------------------

## system.time("bananas, mangos, oranges & kiwis" %>%
##             paste(1:1000000) %>% 
##             str.replace.all("[ab]+(.)", "\\1\\1"))


## system.time("bananas, mangos, oranges & kiwis" %>%
##             paste(1:1000000) %>% 
##             str_replace_all("[ab]+(.)", "\\1\\1"))


## system.time(lapply(1:4,function(x) Sys.sleep(1)))
## system.time(clusterApply(cl, 1:4,function(x) Sys.sleep(1)))

## Load text function
## --------------------------------------------------------------------------------
#' @import magrittr
sdc.load <- function(sdc.file.path, test = FALSE, test.lines = 10000) {
    message("Loading SDC plain text file...", appendLF = FALSE)
    sdc.load.start  <- Sys.time()
    sdc.file.size <- file.info(sdc.file.path)$size
    if (!test) {
        sdc.txt <- readChar(sdc.file.path
                          , nchars = sdc.file.size)
    } else {
        sdc.txt <- readLines(sdc.file.path
                           , n = test.lines) %>%
            paste(collapse = "\n")
    }
    message("\tDONE ", humanFormat::formatBytes(sdc.file.size)
          , " in ", dur.from(sdc.load.start))
    return(sdc.txt)
}

## Clean functions
## depends on stringi
#' @import magrittr
sdc.clean <- function(patterns, text, replacement = "", fixed = FALSE) {
  message("Cleaning plain text...", appendLF = FALSE)
  sdc.clean.start <- Sys.time()
  if (fixed) {
    text %<>% stringi::stri_replace_all_fixed(patterns
                                            , replacement
                                            , vectorize_all = FALSE
                                            , dotall = FALSE)
  } else {
    text %<>% stringi::stri_replace_all_regex(patterns
                                            , replacement
                                            , vectorize_all = FALSE
                                            , dotall = FALSE)
  }
  message("\t\tDONE in ", dur.from(sdc.clean.start))
  return(text)
}


#' @import magrittr
sdc.clean.jv.lp <- function(text) {
  c("\\f\\s*[Pp]age\\s\\d+\\s*Participants\\s+Deal Type\\s+Deal Date\\s+[-]+\\s+[-]+\\s+[-]+"
  , "\\s*Source: Thomson Reuters\\s+Date:\\s+\\d+/\\d+/\\d+\\s*List of Participants"
  , "(?s)\\f\\s*Session Details.*"
    ) %>% sdc.clean(text)
}


#' @import magrittr
sdc.clean.jv.csr <- function(text) {
  c("\\f\\s*[Pp]age\\s\\d+\\s*"
  , "\\s*Source: Thomson Reuters\\s+Date:\\s+\\d+/\\d+/\\d+\\s*Comprehensive Summary Report[ ]*"
  , "(?s)\\f\\s*Session Details.*"
    ) %>% sdc.clean(text)
}

## Split functions
## --------------------------------------------------------------------------------
#' @import magrittr
sdc.split <- function(text, splitter, fixed = FALSE) {
  message("Splitting records...", appendLF = FALSE)
  sdc.split.start <- Sys.time()
  if (fixed) {
    text %<>% stringi::stri_split_fixed(splitter, omit_empty = TRUE) %>%
      unlist
  } else {
    text %<>% stringi::stri_split_regex(splitter, omit_empty = TRUE) %>%
      unlist
  }
  message("\t\tDONE ", length(text), " records in ", dur.from(sdc.split.start))
  return(text)
}


sdc.split.jv.lp <- function(text) {
  sdc.split(text, "[ \\r]+\\n[ \\r]+\\n")
}


sdc.split.jv.csr <- function(text) {
  sdc.split(text, "(?=\\n Date Announced            : )")
}

## Parse records (parse variables)
## --------------------------------------------------------------------------------
#' @import magrittr
sdc.parse.records.jv.lp <- function(records) {
    message("Parsing records...", appendLF = FALSE)
    sdc.parse.start <- Sys.time()
    pos <- readr::fwf_positions(c(1, 35, 60), c(31, 53, NA), c("names", "type", "date"))
    read.read.rec <- function(txt) {
        suppressWarnings(
            readr::read_fwf(txt %>% paste0("\n")
                   , pos
                   , trim_ws = TRUE
                   , progress = FALSE
                   , skip_empty_rows = TRUE)) %>%
            {data.table::data.table(.$names, .$type[1], .$date[1])}
    }
    records %<>% lapply(read.read.rec) %>% data.table::rbindlist
    message("\t\tDONE in ", dur.from(sdc.parse.start))
    return(records)
}


## faster without split...
#' @import magrittr
sdc.parse.jv.lp <- function(txt) {
    message("Parsing records...", appendLF = FALSE)
    sdc.parse.start <- Sys.time()
    pos <- readr::fwf_empty(txt, col_names = c("names", "type", "date"), n = 100000)
    tab <- suppressWarnings(
        readr::read_fwf(txt
               , pos
               , trim_ws = TRUE
               , progress = FALSE
               , skip_empty_rows = TRUE)) %>%
        dplyr::filter(!is.na(names)) %>% 
        inset(!is.na(.$type), "id", 1:sum(!is.na(.$type))) %>%
        inset(,"id", zoo::na.locf(.$id))
    names.list <- tab %>%
        dplyr::select(names, id) %>% 
        data.table::as.data.table %>% 
        {split(.$names, .$id)}
    tab %<>%
        dplyr::select(type, date) %>% 
        na.omit %>%
        dplyr::mutate(names = names.list)
    message("\t\tDONE in ", dur.from(sdc.parse.start))
    return(tab)
}

## --------------------------------------------------------------------------------
#' @import magrittr
sdc.parse.jv.csr.get.field <- function(records, field, drop.first.record = TRUE) {
  regex <- c(date.announced = 
               "(?<=Date Announced            :).*?(?=Deal Type :)"
           , date.agreement = 
               "(?<=Date Agreement Signed     :).*?(?=Involving :)"
           , deal.type = 
               "(?<=Deal Type :).*?(?=\\n)"
           , involving =
               "(?s)(?<=Involving :).*?(?=Current Status :)"
           , status = 
               "(?<=Current Status :).*?(?=\\n)"
           , synopsis = 
               "(?s)(?<=Synopsis:).*?(?=Location:)"  # (?s) is modifier: Make the dot match all characters including line break characters
           , location = 
               "(?<=Location:).*?(?=\\n)"
           , participants.details = 
               "(?s)(?<=Details on participants:).*?={131}.*?(?=={131}|Main Venture Activity)"
           , financial = 
               "(?s)(?<=Financial Details)\\s+={131}.*?(?=={131})"
           , activity = 
               "(?s)(?<=Main Venture Activity)\\s+-{21}.*?(?=={131})"
             )
  ## drop first record as it just a aliance name
  {if (drop.first.record) records[-1] else records} %>% 
    str.extract(regex[field]) %>%
    str.trim %>%
    return()
}



is.na(NULL)

## Parse separate fields
#' @import magrittr
sdc.parse.jv.csr.field.location  <- function(loc) {
  n <- nchar(loc) %>%
    ifelse(length(.) == 0, 0, .) %>%
    ifelse(is.na(.), 0, .)
  if (n > 27) c(city = substr(loc, 0, 27) %>%
                  stringr::str_trim()
              , country.state = substr(loc, 28, n))
  else c(country.state = loc)
}


#' @import magrittr
sdc.parse.jv.csr.field.participants.details  <- function(participants.txt) {
  if (is.na(participants.txt)) return(NA)
  ## set fixed widht postisions
  participants.positions <- readr::fwf_positions(
                                     start = c(1, 34, 72, 106)
                                   , end = c(33, 71, 105, NA)
                                   , col_names = c("name"
                                                 , "business"
                                                 , "address.ticker"
                                                 , "stake"))
  ## read tables
  suppressWarnings(
    readr::read_fwf(participants.txt
                  , participants.positions
                  , skip = 3)) %>%
    inset(!is.na(.$name), "id", 1:sum(!is.na(.$name))) %>%
    inset( ,"id", na.locf(.$id)) %>%
    data.table::as.data.table() %>% 
    split(by = "id", keep.by = FALSE) %>% 
    lapply(function(p) {
      address.ticker <-
        p$address.ticker %>%
        na.omit
      address.ticker.last.line <-
        address.ticker %>%
        extract(length(.))
      address.ticker.city.state.post.p <-
        address.ticker %>%
        stringr::str_detect("^.{16}\\s[A-Z]{2}\\s{2}.*$")
      address.ticker.city.state.post.line <-
        address.ticker %>%
        extract(address.ticker.city.state.post.p) %>%
        {if (length(.) == 0) NA else .}
      address.lines <-
        address.ticker.city.state.post.p %>% 
        which %>%
        {if (length(.) == 0) TRUE else 1:(.-1)} %>%
        extract(address.ticker, .)
      ## put into table
      data.table::data.table(
                    name = p$name[1]
                  , business = p$business %>%
                      na.omit %>%
                      paste(collapse = " ")
                  , address = address.lines %>%
                      na.omit %>%
                      paste(collapse = "\n")
                  , city = address.ticker.city.state.post.line %>%
                      substr(0,16) %>%
                      stringr::str_trim()
                  , country.state = address.ticker.city.state.post.line %>%
                      substr(17,19)
                  , post = address.ticker.city.state.post.line %>%
                      substr(20,nchar(.))
                  , ticker = address.ticker.last.line %>%
                      stringr::str_extract("(?<=Ticker: ).*$") %>%
                      stringr::str_trim() %>%
                      {if (length(.) == 0) NA else .}
                  , phone = address.ticker.last.line %>%
                      stringr::str_replace("Ticker: .*$", "") %>%
                      stringr::str_extract("^\\s*[\\(\\)\\-\\d]+\\s*$") %>%
                      stringr::str_trim() %>%
                      {if (length(.) == 0) NA else .}
                  , stake = p$stake[1]) %>%
        return()
    }) %>%
    data.table::rbindlist() %>%
    return()
}


#' @import magrittr
sdc.parse.jv.csr.field.financial <- function(financial.txt) {
  if (!is.na(financial.txt)) {
    financial.positions <-
      readr::fwf_positions(
               start = c(1, 88, 107, 114)
             , end = c(87, 105, 110, NA)
             , col_names = c("synopsis"
                           , "indicator"
                           , "units"
                           , "value"))
    financial.tab <-
      suppressWarnings(
        readr::read_fwf(financial.txt
                      , financial.positions
                      , skip = 3)
      )
    list(
      financial.synopsis =
        financial.tab$synopsis %>%
        paste(collapse = " ")
    , financial.indicators =
        financial.tab %>%
        dplyr::select(-synopsis) %>%
        dplyr::filter_all(dplyr::any_vars(!is.na(.)))
    ) %>% return()
  } else NA
}


#' @import magrittr data.table
sdc.parse.jv.csr <- function(records
                           , fields = c(
                               "name"
                             , "participants"
                             , "participants.details"
                             , "financial"
                             , "deal.type"
                             , "date.announced"
                             , "date.agreement"
                             , "involving"
                             , "location"
                             , "synopsis"
                             , "activity"
                             )) {
  message("Parsing records...", appendLF = TRUE)
  sdc.parse.start <- Sys.time()
  sdc <- data.table::data.table()
  cl <- parallel::makeCluster(parallel::detectCores())
  ## Participants
  if (any(c("name", "participants") %in% fields) | is.na(fields)) {
    message("\t\t\t- participants names..", appendLF = FALSE)
    name.line <-
      records %>%
      extract(-length(.)) %>% # drop last record
      str.extract(".+\\s+$")
    if ("name" %in% fields | is.na(fields))
      sdc$name <-
        name.line %>%
        str.extract("(?<=-)[^-/]*$") %>%
        str.trim()
    if ("participants" %in% fields | is.na(fields))
      sdc$participants <- 
        name.line %>%
        str.replace("-+[^-/]*$", "") %>%
        stringi::stri_split_fixed("/") %>%
        parallel::parLapply(stringr::str_trim)
    message("\tdone")
  }
  ## Financial
  if("financial" %in% fields | is.na(fields)) {
    message("\t\t\t- financial details..", appendLF = FALSE)
    sdc$financial <-
      records %>% 
      sdc.parse.jv.csr.get.field("financial") %>% 
      parallel::parLapply(sdc.parse.jv.csr.field.financial)
    message("\tdone")  
  }
  ## Date Announced
  if("date.announced" %in% fields| is.na(fields)) {
    message("\t\t\t- date announced..", appendLF = FALSE)
    sdc$date.announced <-
      records %>% 
      sdc.parse.jv.csr.get.field("date.announced") %>%
      lubridate::mdy()
    message("\tdone")  
  }
  ## Date Agreement
  if("date.agreement" %in% fields| is.na(fields)) {
    message("\t\t\t- date agreement..", appendLF = FALSE)
    sdc$date.agreement <-
      records %>% 
      sdc.parse.jv.csr.get.field("date.agreement") %>%
      lubridate::mdy()
    message("\tdone")  
  }
  ## Deal Type
  if("deal.type" %in% fields| is.na(fields)) {
    message("\t\t\t- deal type..", appendLF = FALSE)
    sdc$deal.type <-
      records %>% 
      sdc.parse.jv.csr.get.field("deal.type")
    message("\tdone")  
  }
  ## Status
  if("status" %in% fields| is.na(fields)) {
    message("\t\t\t- status..", appendLF = FALSE)
    sdc$status <-
      records %>% 
      sdc.parse.jv.csr.get.field("status")
    message("\tdone")  
  }
  ## Involving
  if("involving" %in% fields| is.na(fields)) {
    message("\t\t\t- involving..", appendLF = FALSE)
    sdc$involving <-
      records %>% 
      sdc.parse.jv.csr.get.field("involving") %>%
      stringi::stri_split_fixed("\n") %>%
      parallel::parLapply(stringr::str_trim)
    message("\tdone")  
  }
  ## Location
  if("location" %in% fields| is.na(fields)) {
    message("\t\t\t- location..", appendLF = FALSE)
    sdc$location <-
      records %>% 
      sdc.parse.jv.csr.get.field("location") %>%
      parallel::parLapply(sdc.parse.jv.csr.field.location)
    message("\tdone")  
  }
  ## Synopsis
  if("synopsis" %in% fields| is.na(fields)) {
    message("\t\t\t- synopsis..", appendLF = FALSE)
    sdc$synopsis <-
      records %>% 
      sdc.parse.jv.csr.get.field("synopsis") %>%
      stringr::str_replace_all("\\s+", " ")
    message("\tdone")  
  }
  ## Activity
  if("activity" %in% fields| is.na(fields)) {
    message("\t\t\t- activity..", appendLF = FALSE)
    sdc$activity <-
      records %>% 
      sdc.parse.jv.csr.get.field("activity") %>%
      str.replace.all("[\\s-]+", " ") %>%
      str.trim
    message("\tdone")
  }
  ## Participants Details
  if("participants.details" %in% fields| is.na(fields)) {
    message("\t\t\t- participants details..", appendLF = FALSE)
    sdc$participants.details <- 
      records %>% 
      sdc.parse.jv.csr.get.field("participants.details") %>% 
      parallel::parLapply(sdc.parse.jv.csr.field.participants.details)
    message("\tdone")
  }
  ## Table
  parallel::stopCluster(cl)
  message("\t\t\t\t\tDONE in ", dur.from(sdc.parse.start))
  ## data.table(name, name.line) %>% return()
  sdc %>% return()
}

## Read plain text reports from SDC Platinum database
## Glue all functions of the package together
## ================================================================================
#' @title  Read plain text reports from SDC Platinum database
#'
#' @description
#' It is a convenience function that makes it easier to parse and clean the data from SDC Platinum plain text reports. Several types of reports are supported. However, not everything is yet covered so suggestions, feature requests and issues reports are welcome.
#' @param sdc.file.name Filename of SDC Platinum plain text report
#' @param sdc.dir Location of SDC Platinum plain text report. Current working directory is assumed by default.
#' @param data.type Type of SDC Platinum database. Only Joint Ventures - "jv" is currently supported. M&A, Venture Experts and so forth could be added in future. Default is "jv"
#' @param report.type Type of plain text report. Two types are currently supported. "csr" - Comprehensive Summary Report and "lp" - List of Participants. Default is "csr".
#' @param fields Which fields to parse from the report. Depends on report type. For the "csr" (Comprehensive Summary Report) following fields are available - "name", "participants", "participants.details", "financial", "deal.type", "date.announced", "date.agreement", "involving", "location", "synopsis", "activity". Default value is NA which means include all fields available. Specifying just a few fields increases speed of parsing.
#' @return A data.table where some of the columns are lists of vectors, lists and data.tables. Each row represents separate SDC record.
#' @import magrittr
#' @export
#' @md
sdc.read <- function(sdc.file.name
                   , sdc.dir = getwd()
                   , data.type = "jv"
                   , report.type = c("csr", "lp")
                   , fields = NA) {
  ## File path is initial value for pipe.fun.list.n
  sdc.file.path <- file.path(sdc.dir
                           , sdc.file.name
                           , fsep = "/")
  message("--------------------------------------------------")
  message("= = = Reading SDC Platinum plain text export = = =")
  message("--------------------------------------------------")
  ## Apply procedures and return data
  if(data.type[1] == "jv") {
    message("database:\t Joint Ventures")
    if (report.type[1] == "lp") {
      message("report:\t\t List of Participants")
      list(
        sdc.load
      , sdc.clean.jv.lp
      , sdc.parse.jv.lp
      )
    } else if (report.type[1] == "csr") {
      message("report:\t Comprehensive Summary Report")
      list(
        list(sdc.load, test = FALSE)
      , sdc.clean.jv.csr
      , sdc.split.jv.csr
      , list(sdc.parse.jv.csr, fields)
      )
    } else {
      message("report:\t (error) Invalid type of report!")}
  } %>%
    pipe.fun.list.n(sdc.file.path)
}




#' @title  Read Joint Ventures - List of Participants plain text reports from SDC Platinum database
#'
#' @description
#' A wrapper for sdc.read(..., data.type = "jv", report.type = "lp"). It is a convinience function that makes it easear to parse and clean the data from SDC Platinum plain text reports. Several types of reports are supported. However, not everything is yet covered so suggestions, feature requests and issues reports are wellcome.
#' @param sdc.file.name Filename of SDC Platinum plain text report
#' @param sdc.dir Location of SDC Platinum plain text report, sdc.dir = "../sdc-vjs" is assumed by default.
#' @return A data.table where some of the columns are lists of vectors, lists and data.tables. Each row represents separate SDC record.
#' @export
#' @md
sdc.read.jv.lp <- function(...) {
  sdc.read(...
         , sdc.dir = "../sdc-vjs"
         , data.type = "jv"
         , report.type = "lp")
}


#' @title  Joint Ventures - Comprehensive Summary Report plain text reports from SDC Platinum database
#'
#' @description
#' A wrapper for sdc.read(..., data.type = "jv", report.type = "csr"). It is a convinience function that makes it easear to parse and clean the data from SDC Platinum plain text reports. Several types of reports are supported. However, not everything is yet covered so suggestions, feature requests and issues reports are wellcome.
#' @param sdc.file.name Filename of SDC Platinum plain text report
#' @param sdc.dir Location of SDC Platinum plain text report. "../sdc-vjs" is assumed by default.
#' @return A data.table where some of the columns are lists of vectors, lists and data.tables. Each row represents separate SDC record.
#' @export
#' @md
sdc.read.jv.csr <- function(...) {
  sdc.read(...
            , sdc.dir = "../sdc-vjs"
            , data.type = "jv"
            , report.type = "csr")
}
