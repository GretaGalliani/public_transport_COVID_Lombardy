# ## GOAL: This script performs spatial analysis computing global and local Moran indexes of an epidemic feature
# ##       (the mortality rate in the age class 70+ or 50-69, aggregated into 1 to 4 weeks) comparing spatial weights derived by 
# ##        various mobility data (derived from the Regione Lombardia OD matrix of 2020 or from Trenord estimated OD matrices)
rm(list = ls())
source("utils.R")
set.seed(24091998)

#### 0. PREPROCESSING OF MORTALITY AND POPULATION DATASETS ----
# Load original ISTAT death data (daily deaths in all Italian municipalities)
ISTAT_deaths <- read.csv("Data/ISTAT/Mortality/comuni_giornaliero_31marzo23.csv")
# Load general information to fix the municipalities names
comuni_ISTAT <- read_excel("Data/ISTAT/General_data/Codici-statistici-e-denominazioni-al-01_01_2022.xls")

# Preprocess data and aggregate them into age classes
deaths <- preprocess_mortality(ISTAT_deaths, comuni_ISTAT)
rm(ISTAT_deaths)

# Save
write.csv(deaths, file = "Data/ISTAT/Mortality/Processed/deaths_Lombardia.csv", row.names = F)
rm(deaths)

# Load original ISTAT population data (population in all municipalities at 2020/01/01)
ISTAT_pop <- read.csv("Data/ISTAT/Population/comuni_popolazione_2020.csv")
# Load general information needed to select the municipalities in Lombardia
comuni_ISTAT <- read_excel("Data/ISTAT/General_data/Codici-statistici-e-denominazioni-al-01_01_2022.xls")

# Preprocess data and aggregate them into age classes
pop <- preprocess_population_data(ISTAT_pop, comuni_ISTAT)
rm(ISTAT_pop, comuni_ISTAT)

# Save
write.csv(pop, file = "Data/ISTAT/Population/Processed/pop_Lombardia.csv", row.names = F)
rm(pop)

#### 1. LOMBARDIA AREA ----
# Load OD dataset of Regione Lombardia
OD <- read.csv("Data/RegioneLombardia/matriceOD_lombardia_2020.csv")

# Apply cleaning function to OD
OD <- OD_Lombardia_cleaned(OD)
write.csv(OD, "Data/RegioneLombardia/Processed/OD_cleaned.csv", row.names = F)
OD <- read.csv("Data/RegioneLombardia/Processed/OD_cleaned.csv")

# Generate correspondances between OD areas and ISTAT municipalities
ISTAT <- read_excel("Data/ISTAT/General_data/Codici-statistici-e-denominazioni-al-01_01_2022.xls")
matches <- generate_matches_OD_municipalities(OD, ISTAT)
rm(ISTAT)
write.csv(matches, "Data/RegioneLombardia/Processed/matches_municipalities_ODarea.csv", row.names = F)

#### 1.1. CLEANING AND EXPLORATORY ANALYSIS OF ISTAT DEATH DATA ----
# Generate dataset of cleaned deaths data to perform the spatial analysis considering an aggregation step
# First I load the deaths and pop datasets
deaths <- read.csv("Data/ISTAT/Mortality/Processed/deaths_Lombardia.csv")
pop <- read.csv("Data/ISTAT/Population/Processed/pop_Lombardia.csv")

# Load matches between OD areas and ISTAT municipalities
matches <- read.csv("Data/RegioneLombardia/Processed/matches_municipalities_ODarea.csv")

# Load OD
OD <- read.csv("Data/RegioneLombardia/Processed/OD_cleaned.csv")

# Applying an aggregation into 1,2,3,4 weeks windows for computing the weekly mortality densities
deaths <- deaths_data_cleaned_Lombardia(deaths, pop, matches, mortalityWindows = 1:4)

# Save
write.csv(deaths, "Data/ISTAT/Mortality/Processed/RL_deaths_aggregated_mortalityWindows.csv", row.names = F)
rm(deaths, pop)

# Generate geographical dataset aggregated into OD areas
Lombardia_geo_OD <- Lombardia_geodf_OD(OD, matches)

# Produce exploratory plots
deaths <- read.csv("Data/ISTAT/Mortality/Processed/RL_deaths_aggregated_mortalityWindows.csv")
# Produce exploratory analysis of deaths data, aggregated into 1,2,3,4 weeks for the two age classes (50-69 and 70+)
for (cl_eta in c("11_14", "15_21")){
  for (mortalityWindow in 1:4){
    Exploratory_mortality_Lombardia(deaths, Lombardia_geo_OD, cl_eta, mortalityWindow, paste0("Spatial_analysis_mobility_pandemic/Plots/Analysis_Lombardia_area/Exploratory_deaths/Ageclass_", cl_eta, "/Window", mortalityWindow))
  }
}

#### 1.2. SPATIAL ANALYSIS ----
# Load deaths dataset
deaths <- read.csv("Data/ISTAT/Mortality/Processed/RL_deaths_aggregated_mortalityWindows.csv")

# I compute global and local Moran indexes
# two age classes: 50-69 and 70+
# mortalityWindow (e in the paper) in {1,2,3,4}
for (cl_eta in c("11_14", "15_21")){
  for (mortalityWindow in 1:4){
    # First I use OD counts as spatial weights
    Moran_df_ODW <- MORAN_ANALYSIS_RL(OD, "TOT", deaths, Lombardia_geo_OD, mob_mode = "OD_counts", ep_mode = "mortality_pop",
                                      path = paste0("Spatial_analysis_mobility_pandemic/Plots/Analysis_Lombardia_area/Spatial_analysis_ODW/Ageclass_", cl_eta, "/Window", mortalityWindow), cl_eta = cl_eta,
                                      mortalityWindow = mortalityWindow, test_mode = "perm", signif = 0.05, p_adjust = T)

    # Then I use OD classical contiguity spatial weights
    Moran_df_CW <- MORAN_ANALYSIS_RL(OD, "TOT", deaths, Lombardia_geo_OD, mob_mode = "Cont_weights", ep_mode = "mortality_pop",
                                     path = paste0("Spatial_analysis_mobility_pandemic/Plots/Analysis_Lombardia_area/Spatial_analysis_CW/Ageclass_", cl_eta, "/Window", mortalityWindow), cl_eta = cl_eta,
                                     mortalityWindow = mortalityWindow, test_mode = "perm", signif = 0.05, p_adjust = T)

    # Saving the result
    save(list = c("Moran_df_ODW","Moran_df_CW"), file=paste0("Spatial_analysis_mobility_pandemic/Results/Analysis_Lombardia_area/Ageclass_", cl_eta, "_Window", mortalityWindow, ".RData"))
  }
}
rm(deaths, Moran_df_ODW, Moran_df_CW, Lombardia_geo_OD, matches, OD)

#### 2. BREBEMI AREA ----
# First I have to build a correspondance between municipalities and stations, defining stations basins
# Load municipalities dataset
comuni_ISTAT <- read_excel("Data/ISTAT/General_data/Codici-statistici-e-denominazioni-al-01_01_2022.xls")
colnames(comuni_ISTAT) <- make.names(colnames(comuni_ISTAT))
comuni_ISTAT <- comuni_ISTAT |> filter(Denominazione.Regione %in% c("Lombardia","Veneto"))

# Load distance dataset
net1 <- read.csv2("Data/ISTAT/Distances_comuni/Lombardia_distances.txt")
net2 <-  read.csv2("Data/ISTAT/Distances_comuni/Veneto_distances.txt")
net <- rbind(net1, net2)
rm(net1, net2)
comuni_distances <- distances_cleaned(net)
write.csv(net, "Data/ISTAT/Distances_comuni/distances_PROCESSED.csv", row.names = FALSE)
rm(net)

# load stations
stations <- read.csv("Data/Trenord/stations.csv")
load("Data/Trenord/station_codes.Rdata")

# Creating dataset for stations correspondances
Stations_correspondances <- build_stations_correspondances(comuni_ISTAT, stations, comuni_distances, IS_area)
write.csv(Stations_correspondances, "Data/Trenord/Stations_correspondances.csv", row.names = FALSE)

stations <- stations |> dplyr::filter(Codice %in% station_codes)
# Add IS_area to station
temp <- NULL
temp$Codice <- "S99999"
temp$Nome <- "IS area"
temp$Comune <- "IS area"
temp$Provincia <- "MI"
stations <- rbind(stations, temp)
rm(temp)

# Load correspondances between OD Regione and comuni
C_Lombardia_comuni <- read.csv("Data/RegioneLombardia/Processed/matches_municipalities_ODarea.csv")

# Load correspondances between comuni and stazioni
C_comuni_stations <- read.csv("Data/Trenord/Stations_correspondances.csv")

# Join
matches <- C_Lombardia_comuni |> left_join(C_comuni_stations, by = c("ISTAT_code" = "Comune_code")) |> 
  dplyr::select(-Comune_name.y)
colnames(matches)[2] <- "Comune_name"
matches <- matches |> dplyr::filter(Station_ref_name %in% stations$Comune & Station_ref_minutes <= 30)

# Prepare geographical dataset for the area
stat_agg <- BreBeMi_geodf(matches)

# Aggregated plot
p <- ggplot(stat_agg) +
  geom_sf(aes(fill = Station_ref_name)) +
  theme(legend.position = "None") +
  geom_text(aes(label = Station_ref_name, geometry = geometry), size = 1.7, stat = "sf_coordinates") +
  ylab(NULL) + xlab(NULL)
#ggsave("Spatial_analysis_mobility_pandemic/Plots/Analysis_BreBeMi_area/BreBeMi_area_basins.png", p, height = 20, width = 20, units = "cm")
rm(p)

# #### 2.1. AGGREGATION AND EXPLORATORY ANALYSIS OF ISTAT DEATH DATA INTO STATIONS BASINS ----
# First I load the deaths dataset
# Generate dataset of cleaned deaths data to perform the spatial analysis considering an aggregation step
# First I load the deaths and pop datasets
deaths <- read.csv("Data/ISTAT/Mortality/Processed/deaths_Lombardia.csv")
pop <- read.csv("Data/ISTAT/Population/Processed/pop_Lombardia.csv")

# Compute the dataset with the mortality density aggregated into weeks
deaths <- deaths_data_cleaned_BreBeMi(deaths, pop, matches, mortalityWindows = 1:4)

# Save dataset
write.csv(deaths, "Data/ISTAT/Mortality/Processed/BreBeMi_deaths_aggregated_mortalityWindows.csv", row.names = F)

# Produce exploratory plots
deaths <- read.csv("Data/ISTAT/Mortality/Processed/BreBeMi_deaths_aggregated_mortalityWindows.csv")

# Produce exploratory analysis of deaths data, aggregated into 1,2,3,4 weeks for the two age classes (50-69 and 70+)
for (cl_eta in c("11_14", "15_21")){
  for (mortalityWindow in 1:4){
    Exploratory_mortality_BreBeMi(deaths, stat_agg, cl_eta, mortalityWindow, paste0("Spatial_analysis_mobility_pandemic/Plots/Analysis_BreBeMi_area/Exploratory_deaths/Ageclass_", cl_eta, "/Window", mortalityWindow))
  }
}

#### 2.2. SPATIAL ANALYSIS OF REGIONE LOMBARDIA MOBILITY DATA ----
## Load OD (Lombardia)
OD_RL <- read.csv("Data/RegioneLombardia/Processed/OD_cleaned.csv")

# Select and aggregate data at BreBeMi level 
OD_RL <- aggregate_OD_RL_BreBeMi(OD_RL, matches, stations, IS_area)

# Load deaths dataset
deaths <- read.csv("Data/ISTAT/Mortality/Processed/BreBeMi_deaths_aggregated_mortalityWindows.csv")

# I compute global and local Moran indexes
# age classes: 50-69, 70+
# mortalityWindow (e in the paper): {1,2,3,4}

for (cl_eta in c("11_14", "15_21")){
  for (mortalityWindow in 1:4){
    df_RL_TOT <- MORAN_ANALYSIS_BREBEMI_RL(OD_RL, "TOT", deaths, stat_agg, mob_mode = "OD_counts", ep_mode = "mortality_pop", 
                                           path = paste0("Spatial_analysis_mobility_pandemic/PROVA/Plots/Analysis_BreBeMi_area/OD_RL_TOT/Ageclass_", cl_eta, "/Window", mortalityWindow), cl_eta = cl_eta,
                                           mortalityWindow = mortalityWindow, test_mode = "perm", signif = 0.05, p_adjust = T)
    
    # Analysis with railway mobility
    df_RL_FERRO <- MORAN_ANALYSIS_BREBEMI_RL(OD_RL, "FERRO", deaths, stat_agg, mob_mode = "OD_counts", ep_mode = "mortality_pop", 
                                             path = paste0("Spatial_analysis_mobility_pandemic/PROVA/Plots/Analysis_BreBeMi_area/OD_RL_FERRO/Ageclass_", cl_eta, "/Window", mortalityWindow), cl_eta = cl_eta,
                                             mortalityWindow = mortalityWindow, test_mode = "perm", signif = 0.05, p_adjust = T)
    
    # Saving the result
    save(list = c("df_RL_TOT","df_RL_FERRO"), file=paste0("Spatial_analysis_mobility_pandemic/Results/Analysis_BreBeMi_area/OD_RL/PROVAALBBG_Ageclass_", cl_eta, "_Window", mortalityWindow, ".RData"))
  }
}

#### 2.3. SPATIAL ANALYSIS OF TRENORD MOBILITY DATA ----
# Load OD Trenord
## WARNING: These are SYNTETHIC data for demostrating the code. These data does not produce the results shown in the paper
OD_Trenord <- read.csv("Data/Trenord/OD_Trenord_municipalities_aggregated.csv")

# Load deaths
deaths <- read.csv("Data/ISTAT/Mortality/Processed/BreBeMi_deaths_aggregated_mortalityWindows.csv")

# Now I want to explore 3 parameters:
# 1. mortalityWindow in [1,2,3,4] -> aggregation of the number of weeks to be considered when 
#    computing the mortality density
# 2. lag in [1,2,3,4,5,6,7,8,9,10] -> lag between the mortality and mobility data. The lag 
#    encodes how many weeks before the last week of the mortality data considered in mortalityWindow we need to select the first mobility data to be accounted for in the analysis 
# 3. mobilityWindow in [1,2,3,4,5,6] -> aggregation of the number of weeks to be considered when 
#    computing the mobility weights
# + I want to perform the analyses for the two age classes 50-69 and 70+

# I prepare a dataset to read more easily the results. It counts the number of significant GM and LM indexes
df <- expand_grid(mortalityWindow = 1:4, lag = 1:10, mobilityWindow = 1:6, cl_eta = c("11_14", "15_21"))
df <- df |> add_column(n_GM = 0, n_LM = 0)

# Create progress bar
pb <- progress_bar$new(
  format = "  computing [:bar] :percent eta: :eta",
  total = dim(df)[1], clear = FALSE, width= 60)

for (cl_eta in c("11_14", "15_21")){
  for (mortalityWindow in 1:4){
    for (lag in 1:10){
      for (mobilityWindow in 1:6){
        df_OD_Trenord <- MORAN_ANALYSIS_BREBEMI_TRENORD(OD_Trenord, deaths, stat_agg, mob_mode = "OD_counts", ep_mode = "mortality_pop", 
                                                        path = paste0("Spatial_analysis_mobility_pandemic/Plots/Analysis_BreBeMi_area/OD_Trenord/Ageclass_", cl_eta, "/mortalityWindow_", mortalityWindow, 
                                                                      "/lag_", lag, "/mobilityWindow", mobilityWindow), 
                                                        cl_eta = cl_eta, mortalityWindow = mortalityWindow, lag = lag, mobilityWindow = mobilityWindow,
                                                        test_mode = "perm", signif = 0.05, p_adjust = T)
        
        # Update the results dataset
        df[df$mortalityWindow == mortalityWindow & df$lag == lag & df$mobilityWindow == mobilityWindow & df$cl_eta == cl_eta, "n_GM"] <- length(which(df_OD_Trenord$Global_Moran$p.value <= 0.05))
        df[df$mortalityWindow == mortalityWindow & df$lag == lag & df$mobilityWindow == mobilityWindow & df$cl_eta == cl_eta, "n_LM"] <- length(which(df_OD_Trenord$Local_Moran$p.value <= 0.05))
          
        # Saving the result
        save(df_OD_Trenord, file=paste0("Spatial_analysis_mobility_pandemic/Results/Analysis_BreBeMi_area/OD_Trenord/Ageclass_", cl_eta, "_mortalityWindow", mortalityWindow, "_lag", lag, "_mobilityWindow",mobilityWindow, ".RData"))
        
        pb$tick()
        
      }
    }
  }
}

write.csv(df, file = "Spatial_analysis_mobility_pandemic/Results/Analysis_BreBeMi_area/OD_Trenord/Results_df.csv", row.names = F)

