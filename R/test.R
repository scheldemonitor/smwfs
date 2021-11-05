
testing = F
if(testing)({
  # load tabel met parameters
  overz_long_morf_full <- read_delim("inst/extdata/overz_long_morf_full.csv",
                                     ";", escape_double = FALSE, trim_ws = TRUE)
  #==== test generieke functie ========================

  overz_long_morf_full %>%
    select(`ParameterName_in_Scheldemonitor`, `Parameter_Ids_in_Scheldemonitor`) %>%
    filter(grepl("fosfaat", `ParameterName_in_Scheldemonitor`, ignore.case = T)) %>% distinct
  # there are two IDs containing nitrate. sometimes, nitrate+nitrite is reported. For many purposes, that is equal to nitrate
  parID = c(529)
  startyear = 1998
  endyear = 2021
  fosfaat <- getSMdata(startyear, endyear, parID)
  fosfaat2 <- cleanSMdata(nitraat, outliergroups = "stationname")
  ggplot() +
    geom_point(data = fosfaat %>% filter(stationname == "Walcheren 2 km uit de kust"),
               aes(datetime, value, color = valuesign), size = 2) +
    geom_point(data = fosfaat2 %>% filter(stationname == "Walcheren 2 km uit de kust"),
               aes(datetime, value), shape = 21, size = 4, fill = "transparent") +
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
    mutate(jaar = lubridate::year(datetime)) %>%
    # filter(stationname == "Hansweert (HAWI)") %>%
    group_by(jaar, stationname) %>%
    summarize(perc90 = quantile(value, 0.9, na.rm = T)) %>%
    mutate(datum = as.Date(paste(jaar, "01", "01", sep = "-"))) %>%
    ggplot(aes(datum, perc90)) +
    geom_step() +
    facet_wrap(~stationname)






#==== function by Wout =====================

parID = c(828, 833)
nitraat2 <- f.importAbioticData(param = parID)
nitraat2 %>% ggplot(aes(datetime, value)) +
  geom_point(aes(color = valuesign)) +
  facet_wrap(~ stationname)

parID = 1816
golven = f.importAbioticData(param = parID, start = 2019, end = 2019)
golven %>% ggplot(aes(datetime, value)) +
  geom_point(aes(color = valuesign)) +
  facet_wrap(~ stationname)

#===== test biotic data via aphiaID (per species) ============================================

startyear = 2000
endyear = 2002
musselID = 140480

df <- getSMoccurenceData(startyear, endyear, musselID)
df %>% distinct(parametername)

# not very useful for eerstelijnsrapportage


#=== test biotic data via datasetID (per dataset) ============================================

result <- getSMDataset(2010, 2015, 479)

result %>% distinct(datasettitle)
result %>% distinct(parametername)
result %>% distinct(scientificname)


}) # end notrun
