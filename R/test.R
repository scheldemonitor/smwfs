

dontrun({
  # load tabel met parameters
  overz_long_morf_full <- read_delim("data/overz_long_morf_full.csv",
                                     ";", escape_double = FALSE, trim_ws = TRUE)
  #==== test generieke functie ========================

  overz_long_morf_full %>%
    select(`Parameter Name in Scheldemonitor`, `Parameter Ids in Scheldemonitor`) %>%
    filter(grepl("nitraat", `Parameter Name in Scheldemonitor`, ignore.case = T)) %>% distinct
  # there are two IDs containing nitrate. sometimes, nitrate+nitrite is reported. For many purposes, that is equal to nitrate
  parID = c(828, 833)
  startyear = 2015
  endyear = 2021
  nitraat <- getSMdata(startyear, endyear, parID, datasetID = NA)
  nitraat %>% ggplot(aes(datetime, value)) +
    geom_point(aes(color = valuesign)) +
    facet_wrap(~ stationname)

  table(nitraat$valuesign) # need to do something with theses

  # try water level
  overz_long_morf_full %>%
    select(`Parameter Name in Scheldemonitor`, `Parameter Ids in Scheldemonitor`) %>%
    filter(grepl("waterhoogte", `Parameter Name in Scheldemonitor`, ignore.case = T)) %>% distinct
  startyear = 2015
  endyear = 2015
  waterhoogte <- getSMdata(startyear, endyear, 5385)
  # no data!! and not for other years either.. why?

  # golfhoogte
  overz_long_morf_full %>%
    select(`Parameter Name in Scheldemonitor`, `Parameter Ids in Scheldemonitor`) %>%
    filter(grepl("golfhoogte", `Parameter Name in Scheldemonitor`, ignore.case = T)) %>% distinct
  # take ID 1816
  startyear = 2018
  endyear = 2018
  parID = 1816

  # golfdata over alle jaren per jaar opvragen en opbergen als element in list
  # DUURT WEL EVEN... misschien als wrapper rond getSMdata maken?

  golven <- get_y_SMdata(2017, 2020, 1816)
  golven <- golven %>% arrange(datetime)
  wh <- xts::xts(golven[golven$stationname == "Hansweert (HAWI)",]$value, golven[golven$stationname == "Hansweert (HAWI)",]$datetime)
  dygraphs::dygraph(wh)

  golven %>%
    filter(stationname == "Hansweert (HAWI)") %>%
    ggplot(aes(datetime, value)) +
    geom_point()






#==== function by Wout =====================

parID = c(828, 833)
nitraat2 <- f.importAbioticData(param = parID) # does not work
nitraat2 %>% ggplot(aes(datetime, value)) +
  geom_point(aes(color = valuesign)) +
  facet_wrap(~ stationname)



#===== test url in schelde in beeld parametertabel ============================================




}) # end notrun
