# Install lipdR and geoChronR (if necessary; one-time process): ---------------------------------------
# install.packages("remotes")
# library(remotes)
# remotes::install_github("nickmckay/lipdR")
# remotes::install_github("nickmckay/geoChronR")

# Load the required packages: ----------------------------------------------------------------
library(lipdR) # To handle LiPD format files
library(geoChronR) # For plotting
library(magrittr) # For pipe (%>%)
library(dplyr) # For data.frame manipulation
library(ggplot2) # For plotting

# Load the data in: -------------------------------------------------------
# From the lipdverse directly (v1_0_0 is the current version as of Jan. 2023; check https://lipdverse.org/CoralHydro2k/current_version/ for updates)
ch2k <- readLipd('https://lipdverse.org/CoralHydro2k/current_version/CoralHydro2k1_0_0.zip')
# You can also load in a local copy of the .zip file interactively, or by providing a path:
#ch2k <- readLipd()
#ch2k <- readLipd(path = "")

# Exploring the broader database: -------------------------------------------------
# Let's look at the global distribution of all records in the database:
mapLipd(ch2k,
        projection = "mollweide",
        global = TRUE)

# We can also plot a simple dashboard summarizing any individual record in the database:
plotSummary(ch2k[[123]], 
            paleo.age.var = "year") # Note that paleo.data.var is not supplied here since there are multiple proxy types in the database. 
                                    # You will be prompted to choose the appropriate variable in the console.

# Convert the multi-LiPD object (ch2k) into a LiPD time series object (TS) for easy manipulation and filtering:
TS <- extractTs(ch2k)

# Summarize the spatiotemporal availability of CH2K data in a nice figure using the TS object:
summ <- plotSummaryTs(TS,
                      age.var = "year")

# Filtering the database: -------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Suggested fields for filtering:
# - Group (see Table 1 in database descriptor): paleoData_coralHydro2kGroup
# - Proxy Type (SrCa, d18O, d18Osw): paleoData_variableName
# - Temporal Coverage:
#   - minYear (record start year)
#   - maxYear (record end year)
# - Record Resolution:
#   - hasResolution_nominal (nominal resolution; see Table 3)
#   - hasResolution_minimum (minimum resolution in units of 'Years CE')
#   - hasResolution_mean    (mean resolution)
#   - hasResolution_median  (median resolution)
#   - hasResolution_maximum (maximum resolution)
# - Location:
#   - geo_latitude  (record latitude; units: degrees N)
#   - geo_longitude (record longitude; units: degrees E)
#   - geo_siteName  (name of the site/location)
#   - geo_ocean     (ocean basin of the coral record)
# - Species: paleoData_archiveSpecies
# 
# ** Other fields (e.g. geo_secondarySiteName, geo_ocean2) can also be used
# for filtering, but the list above is a suggested starting point. See
#     metadata tables in the database descriptor paper for more information.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# We can convert the lipd-TS to a tibble to filter the database even more efficiently:
ts.tib <- ts2tibble(TS)

# Now filter out values like "d18O_annual", "SrCaUncertainty", and "year" that aren't especially useful to visualize:
ts.tib2 <- ts.tib %>%
  filter(!grepl("_annual|Uncertainty|year",paleoData_variableName)) 

# Let's look at all of the entries in the CH2K database from the Indian Ocean on a regional map:
IndOce.tib <- ts.tib2 %>% # Note that this only takes primary d18O and Sr/Ca time series into account
  filter(geo_ocean == "Indian Ocean")
IndOceTS <- as.lipdTs(IndOce.tib) 
IndOceSumm <- plotSummaryTs(IndOceTS,
                            age.var = "year",
                            projection = "mollweide",
                            global = FALSE)

# We can also filter the data to see where (and when) different types of primary proxy measurements are available globally:
proxy.tib <- ts.tib2 %>% # Note that this only takes primary d18O and Sr/Ca time series into account
  filter(paleoData_variableName == "d18O" | paleoData_variableName == "SrCa")
proxySumm <- plotSummaryTs(proxyTS,
                           sort.var = "paleoData_variableName",
                           age.var = "year")

# Filter calls can be combined to narrow down to very specific parameters:
filter.tib <- ts.tib2 %>% # Note that this only takes primary d18O and Sr/Ca time series into account
  filter(between(geo_latitude,-10,10) & between(geo_longitude,-180,180)) %>% # a specific range of geographic coordinates
  filter(paleoData_archiveSpecies == "Porites lutea") %>% # a specific coral species
  filter(paleoData_coralHydro2kGroup <= 3) %>% # filter for all paired records in the database (Groups 1-3)
filterTS <- as.lipdTs(filter.tib)
filterSumm <- plotSummaryTs(filterTS,
                            age.var = "year",
                            sort.var = "paleoData_variableName",
                            projection = "mollweide",
                            global = TRUE)

# Or slightly less specific ones:
filter2.tib <- ts.tib2 %>% # Note that this only takes primary d18O and Sr/Ca time series into account
  filter(grepl("Porites", paleoData_archiveSpecies)) %>% # filter for any coral species containing the string "Porites"
  filter(paleoData_archiveSpecies != "NA") %>%
  filter(paleoData_coralHydro2kGroup <= 3) %>% # filter for all paired records in the database (Groups 1-3)
  filter(!grepl("_annual|Uncertainty|year",paleoData_variableName))
filter2TS <- as.lipdTs(filter2.tib)
filter2Summ <- plotSummaryTs(filter2TS,
                             age.var = "year",
                             sort.var = "paleoData_variableName",
                             projection = "mollweide",
                             global = TRUE)

# It's also easy to make stackplots from filtered time series. Here, we use the tibble filtering for only Porites lutea:
longTib <- as.lipdTsTibbleLong(filter.tib) %>% 
  filter(between(year,1950,2000)) # for data between 1950 and 2000 only
plotTimeseriesStack(longTib,
                    color.var = "paleoData_variableName",
                    lab.size = 2.5,
                    lab.space = 3,
                    line.size = 0.4,
                    scale.factor = 1/4)
