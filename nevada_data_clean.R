# bryan wilcox archuleta 
# nevada data clean
# jan. 9, 2016

# header 
# this script cleans and complies 
# updated jan 17 for the 2012 and 2016 senate races. 
# 

library(tidyverse)

# vote returns 
# clark 
returns_clark <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/clark/clarkFinal.csv")

returns_clark <- returns_clark %>% mutate(county = "003", 
                                          precinct = sprintf("%04d", precinct), 
                                          county_prec = paste(county, precinct, sep = "_")) %>% dplyr::select(
  county, precinct, county_prec, obama, romney, pct_obama = pobama, pct_romney = promney, votes_2012 = ballots12, 
  clinton, trump, pct_clinton = pclinton, pct_trump = ptrump, votes_2016 = ballots16, berkley, heller,pct_berkley = pberkley, pct_heller = pheller,
  cortezmasto, heck, pct_cortezmasto = pcortezmasto, pct_heck = pheck)

head(returns_clark)

# washoe
returns_washoe <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/washoe/washoeFinal.csv")
head(returns_washoe)

returns_washoe <- returns_washoe %>% mutate(county = "031", 
                                            precinct = sprintf("%04d", precinct), 
                                            county_prec = paste(county, precinct, sep = "_")) %>% dplyr::select(
                                              county, precinct, county_prec, obama, romney, pct_obama = pobama, pct_romney = promney, votes_2012 = ballots12, 
                                              clinton, trump, pct_clinton = pclinton, pct_trump = ptrump, votes_2016 = ballots16, berkley, heller,
                                              pct_berkley = pberkley, pct_heller = pheller,
                                              cortezmasto, heck, pct_cortezmasto = pcortezmasto, pct_heck = pheck)

# elko = 007
returns_elko <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/elko/elkoFinal.csv")

returns_elko <- returns_elko %>% mutate(county = "007", 
                                        precinct = sprintf("%04d", precinct), 
                                        county_prec = paste(county, precinct, sep = "_")) %>% dplyr::select(
                                          county, precinct, county_prec, obama, romney, pct_obama = pobama, pct_romney = promney, votes_2012 = ballots12, 
                                          clinton, trump, pct_clinton = pclinton, pct_trump = ptrump, votes_2016 = ballots16, berkley, heller, 
                                          pct_berkley = pberkley, pct_heller = pheller,
                                          cortezmasto, heck, pct_cortezmasto = pcortezmasto, pct_heck = pheck)

head(returns_elko)

# carson city = 510
returns_carson <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/carson/carsonFinal.csv")

returns_carson <- returns_carson %>% mutate(county = "510", 
                                            precinct = sprintf("%04d", precinct), 
                                            county_prec = paste(county, precinct, sep = "_")) %>% dplyr::select(
                                              county, precinct, county_prec, obama, romney, pct_obama = pobama, pct_romney = promney, votes_2012 = ballots12, 
                                              clinton, trump, pct_clinton = pclinton, pct_trump = ptrump, votes_2016 = ballots16, berkley, heller,
                                              pct_berkley = pberkley, pct_heller = pheller,
                                              cortezmasto, heck, pct_cortezmasto = pcortezmasto, pct_heck = pheck)

head(returns_carson)

# lyon = 019
returns_lyon <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/lyon/lyonFinal.csv")
head(returns_lyon)


returns_lyon <- returns_lyon %>% mutate(county = "019", 
                                        precinct = sprintf("%04s", precinct), 
                                        county_prec = paste(county, precinct, sep = "_")) %>% dplyr::select(
                                          county, precinct, county_prec, obama, romney, pct_obama = pobama, pct_romney = promney, votes_2012 = ballots12, 
                                          clinton, trump, pct_clinton = pclinton, pct_trump = ptrump, votes_2016 = ballots16, berkley, heller,
                                          pct_berkley = pberkley, pct_heller = pheller,
                                          cortezmasto, heck, pct_cortezmasto = pcortezmasto, pct_heck = pheck)

head(returns_lyon)

# nye = 023

returns_nye <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nye/nyeFinal.csv")

returns_nye <- returns_nye %>% mutate(county = "023", 
                                      precinct = sprintf("%04d", precinct), 
                                      county_prec = paste(county, precinct, sep = "_")) %>% dplyr::select(
                                        county, precinct, county_prec, obama, romney, pct_obama = pobama, pct_romney = promney, votes_2012 = ballots12, 
                                        clinton, trump, pct_clinton = pclinton, pct_trump = ptrump, votes_2016 = ballots16, berkley, heller,
                                        pct_berkley = pberkley, pct_heller = pheller,
                                        cortezmasto, heck, pct_cortezmasto = pcortezmasto, pct_heck = pheck)

head(returns_nye)


# douglas = 005 

returns_douglas <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/douglas/douglasFinal.csv")

returns_douglas <- returns_douglas %>% mutate(county = "005", 
                                              precinct = sprintf("%04d", precinct), 
                                              county_prec = paste(county, precinct, sep = "_")) %>% dplyr::select(
                                                county, precinct, county_prec, obama, romney, pct_obama = pobama, pct_romney = promney, votes_2012 = ballots12, 
                                                clinton, trump, pct_clinton = pclinton, pct_trump = ptrump, votes_2016 = ballots16, berkley, heller,
                                                pct_berkley = pberkley, pct_heller = pheller,
                                                cortezmasto, heck, pct_cortezmasto = pcortezmasto, pct_heck = pheck)

head(returns_douglas)

# churchill = 001

returns_churchill <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/churchill/churchillFinal.csv")

returns_churchill <- returns_churchill %>% mutate(county = "001", 
                                              precinct = sprintf("%04d", precinct), 
                                              county_prec = paste(county, precinct, sep = "_")) %>% dplyr::select(
                                                county, precinct, county_prec, obama, romney, pct_obama = pobama, pct_romney = promney, votes_2012 = ballots12, 
                                                clinton, trump, pct_clinton = pclinton, pct_trump = ptrump, votes_2016 = ballots16, berkley, heller,
                                                pct_berkley = pberkley, pct_heller = pheller,
                                                cortezmasto, heck, pct_cortezmasto = pcortezmasto, pct_heck = pheck)

head(returns_churchill)


returns <- rbind(returns_clark, returns_washoe, returns_nye, returns_lyon, returns_carson, returns_elko, returns_douglas,returns_churchill)



# precinct demographics 
# clark
dem_clark <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/demographics/2016_nv_clark.csv")
dem_clark[is.na(dem_clark)] <- 0
dem_clark$county <- sprintf("%03s",dem_clark$county)
dem_clark$precinct <- sprintf("%04s", dem_clark$precinct)
head(dem_clark)

# washoe
dem_washoe <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/demographics/2016_nv_washoe.csv")
dem_washoe[is.na(dem_washoe)] <- 0
dem_washoe$county <- sprintf("%03s",dem_washoe$county)
dem_washoe$precinct <- sprintf("%04s", dem_washoe$precinct)
head(dem_washoe)

# elko 
dem_elko <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/demographics/2016_nv_elko.csv")
dem_elko[is.na(dem_elko)] <- 0
dem_elko$county <- sprintf("%03s",dem_elko$county)
dem_elko$precinct <- sprintf("%04s", dem_elko$precinct)
head(dem_elko)

# carson
dem_carson <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/demographics/2016_nv_carson.csv")
dem_carson[is.na(dem_carson)] <- 0
dem_carson$county <- sprintf("%03s",dem_carson$county)
dem_carson$precinct <- sprintf("%04s", dem_carson$precinct)
head(dem_carson)

# lyon 
dem_lyon <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/demographics/2016_nv_lyon.csv")
dem_lyon[is.na(dem_lyon)] <- 0
dem_lyon$county <- sprintf("%03s",dem_lyon$county)
dem_lyon$precinct <- sprintf("%04s", dem_lyon$precinct)
head(dem_lyon)

# nye 
dem_nye <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/demographics/2016_nv_nye.csv")
dem_nye[is.na(dem_nye)] <- 0
dem_nye$county <- sprintf("%03s",dem_nye$county)
dem_nye$precinct <- sprintf("%04s", dem_nye$precinct)
head(dem_nye)

# douglas
dem_douglas <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/demographics/2016_nv_douglas.csv")
dem_douglas[is.na(dem_douglas)] <- 0
dem_douglas$county <- sprintf("%03s",dem_douglas$county)
dem_douglas$precinct <- sprintf("%04s", dem_douglas$precinct)
head(dem_douglas)

# churchill
dem_churchill <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/demographics/2016_nv_churchill.csv")
dem_churchill[is.na(dem_churchill)] <- 0
dem_churchill$county <- sprintf("%03s",dem_churchill$county)
dem_churchill$precinct <- sprintf("%04s", dem_churchill$precinct)
head(dem_churchill)

# fix washoe
dem_washoe$precinct <- substring(dem_washoe$precinct, 1, 4)

dems <- rbind(dem_clark, dem_washoe, dem_elko, dem_carson, dem_lyon, dem_nye, dem_douglas, dem_churchill)

dems$county_prec <- with(dems, paste(county, precinct, sep = "_"))

# need to workshop some of the matches to see what is matching up
# clark 
head(returns_clark)
head(dem_clark)

# washoe 
head(returns_washoe)
head(dem_washoe)

dem_washoe$precinct <- substring(dem_washoe$precinct, 1, 4)

head(returns_washoe)
head(dem_washoe)

# elko 
head(returns_elko)
head(dem_elko)

# carson
head(returns_carson)
head(dem_carson)

# lyon
head(returns_lyon)
head(dem_lyon)

# nye
head(returns_washoe)
head(dem_washoe)

# douglas
head(returns_douglas)
head(dem_douglas)

# merge final results 
df <- left_join(dems,returns, by = "county_prec")

head(df)

df <- df %>%  mutate(total = asian + black + white + latino + as.numeric(native) + other, pct_latino = latino / total) %>% dplyr::select(
  county = county.x, precinct = precinct.x, county_prec, obama, romney, clinton, trump, pct_obama, pct_romney, pct_clinton, pct_trump, 
  berkley, heller, cortezmasto, heck, pct_berkley, pct_heller, pct_cortezmasto, pct_heck, pct_latino, votes_2012, votes_2016)

write_csv(df, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nv_precinct/returns.csv")


