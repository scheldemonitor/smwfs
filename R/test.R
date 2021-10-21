
# load tabel met parameters
overz_long_morf_full <- read_delim("data/overz_long_morf_full.csv",
                                   ";", escape_double = FALSE, trim_ws = TRUE)
#==== test generieke functie ========================

overz_long_morf_full %>%
  select(`Parameter Name in Scheldemonitor`, `Parameter Ids in Scheldemonitor`) %>%
  filter(grepl("nitraat", `Parameter Name in Scheldemonitor`, ignore.case = T)) %>% distinct
# there are two IDs containing nitrate. sometimes, nitrate+nitrite is reported. For many purposes, that is equal to nitrate
parID = c(828, 833)
startyear = 1998
endyear = 2021
nitraat <- getSMdata(startyear, endyear, parID)
plot(nitraat$datetime, nitraat$value)
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
golfhoogte <- lapply(
  c(1998:2020), function(x) {
    print(x)
    getSMdata(x, x, parID)
  }
)

delete.NULLs  <-  function(x.list){   # delele null/empty entries in a list
  x.list[unlist(lapply(x.list, length) != 0)]
}

lapply(golfhoogte, dim)
# last element is empty--> no data
golfhoogte.df <- data.table::rbindlist(delete.NULLs(waterhoogte[1:22]))
duplicated(golfhoogte.df$datetime) %>% which() %>% length()
wh <- zoo::zoo(golfhoogte.df[golfhoogte.df$stationname == "Wielingen (WIEL)",]$value, golfhoogte.df[golfhoogte.df$stationname == "Wielingen (WIEL)",]$datetime)
dygraphs::dygraph(wh)


#===== test url in schelde in beeld parametertabel ============================================

