
#=== info =====================================================
# author: willem.stolte@deltares.nl

require(httr)
require(sf)
require(tidyverse)
require(readr)


#' Retrieves abiotic data from scheldemonitor wfs
#'
#' @param startyear first year of requested data
#' @param endyear last year of requested data
#' @param parID parameter code in scheldemonitor
#' @param datasetID dataset id in scheldemonitor, default is datasets that are supplied by RWS
#' @return dataframe with observation data
#' @examples
#' parID = c(828, 833)
#' startyear = 1998
#' endyear = 2021
#' nitraat <- getSMdata(startyear, endyear, parID)
#' nitraat %>% ggplot(aes(datetime, value)) +
#'   geom_point(aes(color = valuesign)) +
#'   facet_wrap(~ stationname)
#' @export
getSMdata = function(startyear, endyear, parID, datasetID = c(588,500,479,135,1527,476)) {

  require(httr)
  require(sf)
  require(tidyverse)
  require(readr)

  if(any(!is.na(datasetID))){
    print("you are currently searching for data in datasetID")
    print(datasetID)
    print("if you want to search in all datasets, choose datasetID = NA")
    print("if you want to search in other datasets, provide a vector of ID's")
  }

# TODO check for:
# endyear >= startyear
# startyear and endyear <= year(today)
# parID exists how?
# datasetID exists - how?

  urllist <- structure(list(scheme = "http", hostname = "geo.vliz.be", port = NULL,
                            path = "geoserver/wfs/ows",
                            query = list(service = "WFS",
                                         version = "1.1.0",
                                         request = "GetFeature",
                                         typeName = "Dataportal:abiotic_observations",
                                         resultType = "results",
                                         viewParams = "placeholder",
                                         propertyName = "stationname,longitude,latitude,datetime,depth,parametername,valuesign,value,dataprovider,datasettitle",
                                         # outputFormat = "application/json" to read with sf::st_read()
                                         outputFormat = "csv"),
                            params = NULL,
                            fragment = NULL,
                            username = NULL, password = NULL), class = "url")

  viewParams <- paste("where:obs.context+&&+ARRAY[1]+AND+standardparameterid+IN+(",
                      stringr::str_replace_all(paste(parID, collapse = ","), ",", "\\\\,"),
                      ")+",
                      ifelse(any(!is.na(datasetID)),
                             paste0("AND+imisdatasetid+IN+(", paste(datasetID, collapse = "\\,"), ")+"), # all RWS datasets
                             ""),
                      "AND+((datetime_search+BETWEEN+'", startyear, "-01-01'+AND+'", endyear, "-12-31'+))",
                      ";context:0001", ";loggedin:1", sep = "")

  urllist$query$viewParams <- viewParams
  # replace "+" signs with whitespace to be placed in url
  # text with "+" is copied from webservice url
  urllist$query$viewParams  <- stringr::str_replace_all(urllist$query$viewParams, '\\+', ' ')
  downloadURL = httr::build_url(urllist)
  result <- RETRY("GET", url = downloadURL, times = 3) %>%   # max retry attempts
    content(., "text") %>%
    read_csv(guess_max = 100000) %>%
  # result <- sf::st_read(url) %>%
    mutate(value = na_if(value, "999999999999")) %>%
    mutate(value = as.numeric(value))
  return(result)
}


delete.NULLs  <-  function(x.list){   # delete empty entries in a list
  x.list[unlist(lapply(x.list, nrow) != 0)]
}


get_y_SMdata <- function(startyear, endyear, parID, datasetID = c(588,500,479,135,1527,476)){

  yearlist <- c(startyear:endyear)
  l <- lapply(
    yearlist, function(x) {
      print(paste("reading year", x))
      getSMdata(x, x, parID)
    }
  )

  if(length(unlist(lapply(l, nrow) == 0)) > 0) {
    print(paste("there is no data for years:", yearlist[unlist(lapply(l, nrow) == 0)]))
  }

  lresult <- delete.NULLs(l) %>% bind_rows()

  return(lresult)
}




# get biological occurence per aphiaid. Right now without time limit, and restricted to
# Westerschelde and Scheldemonding (gID == 9 or 10, not parameterized yet)
# log in nog verwerken, zie in function getSMdata
getSMoccurenceData = function(startyear, endyear, parID) {

  urllist <- structure(list(scheme = "http", hostname = "geo.vliz.be", port = NULL,
                            path = "geoserver/wfs/ows",
                            query = list(service = "WFS",
                                         version = "1.1.0",
                                         request = "GetFeature",
                                         typeName = "Dataportal:biotic_observations",
                                         resultType = "results",
                                         viewParams = "placeholder",
                                         propertyName = "stationname,aphiaid,scientificname,observationdate,longitude,latitude,value,parametername,dataprovider,imisdatasetid,datasettitle,datafichetitle",
                                         # outputFormat = "application/json"),
                                         outputFormat = "csv"),
                            params = NULL,
                            fragment = NULL,
                            username = NULL, password = NULL), class = "url")

  viewParams <- paste("where:obs.context+&&+ARRAY[1]+AND+(gID+IN+(10\\,9));context:0001;aphiaid:",

                      stringr::str_replace_all(paste(parID, collapse = ","), ",", "\\\\,"),
                      # ")+",
                      # "AND+imisdatasetid+IN+(588\\,500\\,479\\,135\\,1527\\,476)+", # all RWS datasets
                      # "AND+((datetime_search+BETWEEN+'", startyear, "-01-01'+AND+'", endyear, "-01-31'+))",
                      # ";context:0001",
                      ";loggedin:1", sep = ""
  )

  urllist$query$viewParams <- viewParams
  # replace "+" signs with whitespace to be placed in url
  # text with "+" is copied from webservice url
  urllist$query$viewParams  <- stringr::str_replace_all(urllist$query$viewParams, '\\+', ' ')
  downloadURL = httr::build_url(urllist)
  result <- RETRY("GET", url = downloadURL, times = 3) %>%   # max retry attempts
    content(., "text") %>%
    read_csv(guess_max = 100000) %>%
    # result <- sf::st_read(url) %>%
    mutate(value = na_if(value, "999999999999")) %>%
    mutate(value = as.numeric(value))
  return(result)
}


getSMDataset <- function(startyear, endyear, datasetID){

  urllist <- structure(list(scheme = "http", hostname = "geo.vliz.be", port = NULL,
                            path = "geoserver/wfs/ows",
                            query = list(service = "WFS",
                                         version = "1.1.0",
                                         request = "GetFeature",
                                         typeName = "Dataportal:biotic_observations",
                                         resultType = "results",
                                         viewParams = "placeholder",
                                         propertyName = NULL, #"stationname,aphiaid,scientificname,observationdate,longitude,latitude,value,parametername,parameterunit,dataprovider,imisdatasetid,datasettitle,datafichetitle",
                                         # outputFormat = "application/json"),
                                         outputFormat = "csv"),
                            params = NULL,
                            fragment = NULL,
                            username = NULL, password = NULL), class = "url")

  viewParams <- paste("where:obs.context+&&+ARRAY[1]+AND+",

                      # stringr::str_replace_all(paste(parID, collapse = ","), ",", "\\\\,"),
                      # ")+",
                      "imisdatasetid+IN+(",
                      stringr::str_replace_all(paste(datasetID, collapse = ","), ",", "\\\\,"),
                      ")+",
                      "AND+((datetime_search+BETWEEN+'", startyear, "-01-01'+AND+'", endyear, "-01-31'+))",
                      # ";context:0001",
                      ";loggedin:1", sep = ""
  )

  urllist$query$viewParams <- viewParams
  # replace "+" signs with whitespace to be placed in url
  # text with "+" is copied from webservice url
  urllist$query$viewParams  <- stringr::str_replace_all(urllist$query$viewParams, '\\+', ' ')
  url = httr::build_url(urllist)
  result <- RETRY("GET", url = url, times = 3) %>%   # max retry attempts
    content(., "text") %>%
    read_csv(guess_max = 100000) %>%
    # result <- sf::st_read(url) %>%
    mutate(value = na_if(value, "999999999999")) %>%
    mutate(value = as.numeric(value))
  return(result)
}
