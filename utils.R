## Auxiliary functions for my analysis 
library(dplyr)
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(tibble)
library(ggplot2)
library(spdep)
library(gridExtra)
library(ggpubr)
library(stringr)
library(lubridate)
library(progress)
library(readxl)
library(mipfp)

#### Preprocessing of mortality dataset (code taken from https://github.com/RiccardoScimone/Mortality-densities-italy-analysis) ----
# Function to aggregate mortality data into age classes
aggregate_classes = function(data,keyname,firstvec,lastvec) #### BE VERY CAREFUL THAT THE VECTORS ARE COHERENT!
{
  n = length(firstvec)
  newdata = data
  for (i in 1:n){
    first = firstvec[i]
    last = lastvec[i]
    classes = as.character(first:last)
    newdata$CL_ETA[newdata$CL_ETA %in% classes] = paste0(first,"_",last)
    newdata = newdata |> group_by(get(keyname), partial_date_death, CL_ETA) |>
      summarise_at(c(paste0("M_",c(11:22)),paste0("F_",c(11:22)),paste0("T_",c(11:22))), sum) |> ungroup()
    names(newdata)[1] = keyname
  }
  return(newdata)
}

# Function to preprocess the raw ISTAT mortality data
preprocess_mortality <- function(deaths, comuni_ISTAT){
  # Removing columms of 2203 with no data
  deaths = deaths |> dplyr::select(-c(M_23, F_23, T_23))
  
  # Filtering only Lombardia data
  deaths = deaths |> filter(NOME_REGIONE == "Lombardia")
  
  # Transform some columns
  deaths$GE = as.character(deaths$GE)
  months = str_sub(deaths$GE,1,-3)
  days = str_sub(deaths$GE,-2,-1)
  deaths$partial_date_death = as.Date(paste0("2020-",months,"-",days))
  deaths$CL_ETA = as.character(deaths$CL_ETA)
  deaths$COD_PROVCOM = as.character(deaths$COD_PROVCOM)
  deaths$T_20 = as.numeric(deaths$T_20)
  deaths$F_20 = as.numeric(deaths$F_20)
  deaths$M_20 = as.numeric(deaths$M_20)
  deaths = deaths |> mutate_at(vars(-partial_date_death), ~replace(., is.na(.), 0))
  deaths = deaths |> dplyr::select(-GE)
  #### Note: All dates are specified at 2020 for simplicity, but the actual year is specified in each column.
  
  #### Aggragation by age (See notation in the pdf from ISTAT) ######
  firstvec = c(0,11,15)
  lastvec = c(10,14,21)
  #### So we are aggregating 0-49 years, 50-69 years, 70+ years #####
  
  #### Aggregate on age basis the Municipality data and carry useful information. This will require some time ############
  municipalities_codes = sort(unique(deaths$COD_PROVCOM))
  municipalities_data = deaths |> group_by(COD_PROVCOM,CL_ETA)
  
  municipalities_georef = comuni_ISTAT |> arrange(`Codice Comune formato numerico`) |> dplyr::select(16, 12, 11, 7) |> 
    distinct(`Codice Comune formato numerico`,.keep_all = TRUE)
  colnames(municipalities_georef) <- c("COD_PROVCOM", "NOME_PROVINCIA", "NOME_REGIONE", "NOME_COMUNE")
  
  deaths = aggregate_classes(municipalities_data,keyname = "COD_PROVCOM",firstvec,lastvec)
  deaths = deaths |> mutate(COD_PROVCOM = as.numeric(COD_PROVCOM)) |>
    left_join(municipalities_georef, by = c("COD_PROVCOM" = "COD_PROVCOM"))
  
  # Remove one municipality created in 2023
  deaths <- deaths |> filter(COD_PROVCOM != 12144)
  
  return(deaths)
}

# Function to convert the population age data into age classes
factorization = function (age)
{
  if(age < 50)
    return("0_10")
  if(age < 70)
    return("11_14")
  return("15_21")
}

age_to_fact = Vectorize(factorization)

# Function to preprocess the population data
preprocess_population_data <- function(pop, comuni_ISTAT){
  ### retrieve resident by age in each municipality. Some data field is in italian with accents and needs their names changed
  pop <- pop |> dplyr::select(1,3,11,19)
  colnames(pop)[2] <- "Eta"
  
  pop <- pop |> dplyr::filter(Eta != 999) |> dplyr::mutate(istat = Codice.comune, num_residenti = Totale.Maschi + Totale.Femmine) |>
    dplyr::select(istat,num_residenti, Eta,Totale.Maschi, Totale.Femmine)
  
  
  ### Now we perform aggregation, see factorization function in Utility_functions, which is written according to our aggregation
  pop <- pop |> dplyr::mutate(CL_ETA = age_to_fact(Eta)) |> dplyr::select(-Eta)
  pop <- pop |> dplyr::group_by(istat, CL_ETA) |> dplyr::summarise_at(c("Totale.Femmine", "Totale.Maschi", "num_residenti"),sum)
  
  pop$istat = as.character(pop$istat)
  
  # I now want to filter the municipalities in Lombardia
  # Select the codes of all the Lombardia municipalities from ISTAT general municipalities info
  Lombardia_codes <- comuni_ISTAT |> dplyr::filter(`Denominazione Regione` == "Lombardia") |> pull(`Codice Comune formato numerico`)
  pop <- pop |> dplyr::filter(istat %in% Lombardia_codes)
  
  return(pop)
}

#### Analysis of Lombardia area ----
# Function to clean OD data of Regione Lombardia
OD_Lombardia_cleaned <- function(OD){
  # Aggregating zones divided into subzones
  OD <- OD %>%
    mutate(
      ZONA_ORIG = str_replace(ZONA_ORIG, " [0-9]+", ""),
      ZONA_DEST = str_replace(ZONA_DEST, " [0-9]+", "")
    )
  
  # Drop fascia oraria and sum by groups
  OD <- OD |> dplyr::select(-(FASCIA_ORARIA)) |> group_by(PROV_ORIG, ZONA_ORIG, PROV_DEST, ZONA_DEST) |>
    summarise(across(where(is.numeric), sum)) |> ungroup()
  
  # Selecting only provinces in Lombardia
  Lombardia_provinces <- c("BG", "BS", "CO", "CR", "LC", "LO", "MB", "MI", "MN", "SO", "VA", "PV")
  
  OD <- OD |> dplyr::filter(PROV_ORIG %in% Lombardia_provinces & PROV_DEST %in% Lombardia_provinces)
  
  ## FIXING SPELLING
  # REM: Municipalities separated by "-" represent a unique comune, while if they are separated by " - " they represent
  #     a group of distant ISTAT comunis
  
  A <- c("ALME'", "BREMBILLA", "CAPRIATE SAN GERVASO", "COSTA DI SERINA", "GARDONE VALTROMPIA", 
         "LONATO", "PUEGNAGO SUL GARDA", "ROE' VOLCIANO", "SALO'", "TEMU'", "TOSCOLANO MADERNO", 
         "TREMOSINE", "VALLIO", "CAGNO", "CANTU'", "CAVALLASCA", "FENEGRO'",
         "DREZZO", "GIRONICO", "LENNO", "MEZZEGRA", "OLTRONA CON SAN MAMETTE", "OSSUCCIO", "PARH",
         "PELLIO INTELVI", "SOLBIATE COMASCO", "CORTENOVA", "GRASSOBIO", "SANT'OMOBONO IMAGNA - VALSECCA",
         "VILLA D'ALME'", "COMEZZANO - CIZZAGO", "RODENGO - SAIANO", "CASASCO D'INTELVI - CASTIGLIONE D'INTELVI - CERANO INTELVI - SAN FEDELE INTELVI",
         "LANZO D'INTELVI - RAMPONIO VERNA", "TREMEZZO", "UGGIATE - TREVANO", "CA' D'ANDREA",
         "DRIZZONA", "GABBIONETA BINANUOVA", "GADESCO PIEVE DELMONA", "GERRE DE'CAPRIOLI", "SAN GIOVANNI INCROCE",
         "BARZANO'", "PEREGO", "ROVAGNATE", "SANTA MARIA HOE'", "VERDERIO INFERIORE", "VERDERIO SUPERIORE", "VIGANO'",
         "BOVISIO MASCIAGO", "MUGGIO'", "CASSINA DE PECCHI", "VERMEZZO", "ZELO SURRIGONE", "BIGARELLO",
         "BORGOFORTE", "BORGOFRANCO SUL PO", "CARBONARA DI PO", "FELONICA", "PIEVE DI CORIANO", "REVERE",
         "SAN GIORGIO DI MANTOVA", "SERMIDE", "VILLA POMA", "VIRGILIO", "BASCAPE'", "CORNALE", 
         "CORTEOLONA", "GAMBOLO'", "MORNICO LOSANNA", "RUINO", "TRAVACO' SICCOMARIO", "ZERBOLO'",
         "BRISSAGO - VALTRAVAGLIA", "CADEGLIANO - VICONAGO", "CADREZZATE - OSMATE", "COCQUIO - TREVISAGO",
         "CUGLIATE - FABIASCO", "GAZZADA - SCHIANNO", "LAVENO MOMBELLO", "TRAVEDONA - MONATE", "VIGGIU'",
         "CAMAIRAGO", "CAVACURTA", "SAN MARTINO IN SCORTE BRUGNATELLA", "TERRANUOVA DEI PASSERINI", "BASTIDA DE' DOSSI",
         "MACCAGNO - PINO SULLA SPONDA DEL LAGO MAGGIORE - TRONZANO LAGO MAGGIORE - VEDDASCA", "MONTESCANO - MONTU' BECCARIA - ZENEVREDO",
         "BASTIDA DE' DOSSI - CASEI GEROLA", "CASALE CREMASCO - VIDOLASCO - CASTEL GABBIANO", "INTROZZO - PAGNONA - TREMENICO",
         "GEROSA - SAN GIOVANNI BIANCO", "BIENNO - PRESTINE", "CIVENNA - MAGREGLIO", "BELLAGIO", "PIADENA - VOLTIDO",
         "CASARGO - MARGNO - VENDROGNO", "BELLANO", "SUEGLIO - VESTRENO", "CANEVINO - MONTECALVO VERSIGGIA - ROCCA DE' GIORGI - VOLPARA",
         "COPIANO - GENZONE", "VALVERDE - VARZI", "GORDONA - MENAROLA"
  )
  B <- c("ALMÈ", "VAL BREMBILLA - SAN GIOVANNI BIANCO", "CAPRIATE SAN GERVASIO", "COSTA SERINA", "GARDONE VAL TROMPIA",
         "LONATO DEL GARDA", "PUEGNAGO DEL GARDA", "ROÈ VOLCIANO", "SALÒ", "TEMÙ", "TOSCOLANO-MADERNO",
         "TREMOSINE SUL GARDA", "VALLIO TERME", "SOLBIATE CON CAGNO", "CANTÙ", "SAN FERMO DELLA BATTAGLIA", "FENEGRÒ",
         "COLVERDE", "COLVERDE", "TREMEZZINA", "TREMEZZINA", "OLTRONA DI SAN MAMETTE", "TREMEZZINA", "COLVERDE",
         "ALTA VALLE INTELVI", "SOLBIATE CON CAGNO", "CORTENUOVA - PARLASCO - TACENO", "GRASSOBBIO", "SANT'OMOBONO TERME",
         "VILLA D'ALMÈ", "COMEZZANO-CIZZAGO", "RODENGO SAIANO", "CERANO D'INTELVI - CENTRO VALLE INTELVI",
         "CERANO D'INTELVI - CENTRO VALLE INTELVI", "TREMEZZINA", "UGGIATE-TREVANO", "TORRE DE' PICENARDI",
         "PIADENA DRIZZONA - VOLTIDO", "GABBIONETA-BINANUOVA", "GADESCO-PIEVE DELMONA", "GERRE DE' CAPRIOLI", "SAN GIOVANNI IN CROCE",
         "BARZANÒ", "LA VALLETTA BRIANZA", "LA VALLETTA BRIANZA", "SANTA MARIA HOÈ", "VERDERIO", "VERDERIO", "VIGANÒ",
         "BOVISIO-MASCIAGO", "MUGGIÒ", "CASSINA DE' PECCHI", "VERMEZZO CON ZELO", "VERMEZZO CON ZELO", "SAN GIORGIO BIGARELLO",
         "BORGO VIRGILIO", "BORGOCARBONARA", "BORGOCARBONARA", "SERMIDE E FELONICA", "BORGO MANTOVANO", "BORGO MANTOVANO",
         "SAN GIORGIO BIGARELLO", "SERMIDE E FELONICA", "BORGO MANTOVANO", "BORGO VIRGILIO", "BASCAPÈ", "CORNALE E BASTIDA - CASEI GEROLA",
         "COPIANO - CORTEOLONA E GENZONE", "GAMBOLÒ", "MORNICO LOSANA", "COLLI VERDI - MONTECALVO VERSIGGIA - ROCCA DE' GIORGI - VOLPARA - VARZI", "TRAVACÒ SICCOMARIO", "ZERBOLÒ",
         "BRISSAGO-VALTRAVAGLIA", "CADEGLIANO-VICONAGO", "CADREZZATE CON OSMATE", "COCQUIO-TREVISAGO",
         "CUGLIATE-FABIASCO", "GAZZADA SCHIANNO", "LAVENO-MOMBELLO", "TRAVEDONA-MONATE", "VIGGIÙ",
         "CASTELGERUNDO", "CASTELGERUNDO", "SAN MARTINO IN STRADA", "TERRANOVA DEI PASSERINI", "CORNALE E BASTIDA - CASEI GEROLA",
         "MACCAGNO CON PINO E VEDDASCA - TRONZANO LAGO MAGGIORE", "MONTESCANO - MONTÙ BECCARIA - ZENEVREDO",
         "CORNALE E BASTIDA - CASEI GEROLA", "CASALE CREMASCO-VIDOLASCO - CASTEL GABBIANO", "SUEGLIO - VALVARRONE - PAGNONA",
         "VAL BREMBILLA - SAN GIOVANNI BIANCO", "BIENNO", "BELLAGIO - MAGREGLIO", "BELLAGIO - MAGREGLIO", "PIADENA DRIZZONA - VOLTIDO",
         "CASARGO - MARGNO - BELLANO", "CASARGO - MARGNO - BELLANO", "SUEGLIO - VALVARRONE - PAGNONA", "COLLI VERDI - MONTECALVO VERSIGGIA - ROCCA DE' GIORGI - VOLPARA - VARZI",
         "COPIANO - CORTEOLONA E GENZONE", "COLLI VERDI - MONTECALVO VERSIGGIA - ROCCA DE' GIORGI - VOLPARA - VARZI", "GORDONA"
  )
  
  # Substitute denominations in vector A in OD with denominations in vector B
  OD <- OD %>%
    mutate(
      ZONA_ORIG = case_when(
        ZONA_ORIG %in% A ~ B[match(ZONA_ORIG, A)],
        TRUE ~ ZONA_ORIG
      ),
      ZONA_DEST = case_when(
        ZONA_DEST %in% A ~ B[match(ZONA_DEST, A)],
        TRUE ~ ZONA_DEST
      )
    )
  
  # Aggregate
  OD <- OD |> group_by(PROV_ORIG, ZONA_ORIG, PROV_DEST, ZONA_DEST) |>
    summarise(across(where(is.numeric), sum)) |> ungroup()
  # Remove self-loops
  OD <- OD |> dplyr::filter(!(ZONA_ORIG == ZONA_DEST))
  
  # Compute the colums summarizing the overall number of movements for each OD couple
  OD <- OD |> mutate(TOT = rowSums(across(where(is.numeric))))
  # Compute the colums summarizing the railway number of movements for each OD couple
  OD <- OD |> mutate(FERRO = rowSums(select(cur_data(), ends_with("FERRO"))))
  
  return(OD)
}

# Function to generate the dataset of correspondances between OD areas and ISTAT municipalities in Lombardia
generate_matches_OD_municipalities <- function(OD, ISTAT){
  # Select only ISTAT municipalities in Lombardia
  ISTAT <- ISTAT |> filter(`Codice Regione` == "03")
  
  # Prepare dataset of correspondances
  # NOTE: I need to select the ISTAT dataset by column indexes because of their naming which causes problems
  Correspondances <- ISTAT[,c(5,7)] |>
    rename(ISTAT_code = 1, Comune_name = 2) |>
    add_column(OD_name = NA)
  
  # FIRST CASE: if the OD matrix contains an OD_area which matches exactly one municipality name, then I add the match
  Correspondances <- Correspondances %>%
    mutate(
      OD_name = ifelse(tolower(Comune_name) %in% tolower(OD$ZONA_ORIG), toupper(Comune_name), OD_name)
    )
  
  # SECOND CASE: when the OD areas are aggregation of municipalities, I separe them and look for the match with an ISTAT municipality
  # This was needed to check if there were unmached municipalities
  unmatched_comunis <- NULL
  # The zones resulting from agregated municipalities can be identified looking for "-" in the name of the zone
  aggr_zones <- unique(OD$ZONA_ORIG)[grep("-", unique(OD$ZONA_ORIG))]
  
  for (act in aggr_zones){
    # I separate the municipalities which constitute the aggregated zone
    municipalities <- unlist(str_split(act, " - "))
    
    for (mun in municipalities){
      # If I have a match with a ISTAT municipality, I add it in the dataset
      if (tolower(mun) %in% tolower(Correspondances$Comune_name))
        Correspondances[tolower(Correspondances$Comune_name) == tolower(mun),"OD_name"] <- act
      # If not, I add it to the unmatched comunis
      else
        unmatched_comunis <- c(unmatched_comunis, tolower(mun))
    }
  }
  
  # Throw an error if I have an unmatched OD name
  if (!is.null(unmatched_comunis))
    stop("There are unmatched OD zones")
  
  # Throw an error if I have an unmatched ISTAT municiplaity
  if (any(is.na(Correspondances$OD_name)))
    stop("There are unmatched municipalities")
  
  return(Correspondances)
}

#### Cleaning and exploratory analysis of ISTAT death data ----
# Function to get mortality data aggregated at Lombardia level and to generate matches between OD areas and 
# municipalities, applying an aggregation for computation of mortality divided by population
deaths_data_cleaned_Lombardia <- function(deaths, pop, matches, mortalityWindows){
  # Select only tot of deaths in the years of 2019, 2020, 2021
  deaths <- deaths |>
    select(all_of(c("COD_PROVCOM", "partial_date_death", "CL_ETA", "T_19", "T_20", "T_21",
                    "NOME_PROVINCIA", "NOME_REGIONE", "NOME_COMUNE"))) |>
    # Select only Lombardia 
    filter(NOME_REGIONE == "Lombardia") |>
    # Now I transform the dataset to have dates dependent on year and only one column counting the deaths for that day
    pivot_longer(cols = T_19:T_21, names_to = "Year", values_to = "T_deaths") |>
    # Then I apply my numbering in weeks of the column partial_date_death
    # weeks of 2020 are 00-52
    # weeks of 2019 are -52-00
    # weeks of 2021 are 52-104
    mutate(partial_date_death = case_when(
      Year == "T_19" ~ -(52-as.numeric(format(as.Date(partial_date_death), "%W"))), 
      Year == "T_20" ~ as.numeric(format(as.Date(partial_date_death), "%W")), 
      Year == "T_21" ~ 52 + as.numeric(format(as.Date(partial_date_death), "%W")), 
    )) |> select(-Year) |>
    # Aggregation into weeks
    group_by(COD_PROVCOM, partial_date_death, CL_ETA, NOME_PROVINCIA, NOME_REGIONE, NOME_COMUNE) |>
    summarise(T_deaths = sum(T_deaths)) |> ungroup() |> 
    # Join population info
    left_join(pop |> select(istat, CL_ETA, num_residenti),
              by = c("COD_PROVCOM" = "istat", "CL_ETA")) |>
    # Aggregation in the OD zones: substitute in deaths NOME_COMUNE with OD_name
    mutate(NOME_COMUNE = matches$OD_name[match(NOME_COMUNE, matches$Comune_name)]) |>
    rename(OD_AREA = NOME_COMUNE) |> 
    # Aggregate
    select(-COD_PROVCOM) |> 
    group_by(CL_ETA, partial_date_death, NOME_PROVINCIA, OD_AREA, NOME_REGIONE) |>
    summarise(across(where(is.numeric), sum)) |> ungroup()
  
  ## COMPUTATION OF THE MORTALITY DENSITIES
  # Add one empty column for every element of the vector mortality Windows
  cols <- paste0("md_window", mortalityWindows)
  deaths[,cols] <- NA
  
  # Create progress bar
  pb <- progress_bar$new(
    format = "  computing [:bar] :percent eta: :eta",
    total = dim(deaths)[1], clear = FALSE, width= 60)
  
  # Aggregation in the sliding window
  for (i in 1:dim(deaths)[1]){
    r <- deaths[i,]
    cl_eta <- r$CL_ETA
    area <- r$OD_AREA
    w <- as.numeric(r$partial_date_death)
    
    for (mw in mortalityWindows){
      # Compute the number of deaths in the sliding window
      act <- deaths |> dplyr::filter(CL_ETA == cl_eta & OD_AREA == area & as.numeric(partial_date_death) %in% (w-mw+1):w) |> 
        summarise(across(where(is.numeric), sum)) |> dplyr::select(T_deaths)
      
      # Compute mortality density as deaths / population
      deaths[i, paste0("md_window", mw)] <- act / r$num_residenti
    }
    
    pb$tick()
  }
  
  # Convert from list to numeric
  deaths <- deaths |>
    mutate(across(starts_with("md_window"), as.numeric))
  
  return(deaths)
}

# Function to generate geographical dataset aggregated into OD areas
Lombardia_geodf_OD <- function(OD, matches){
  italy_geo <- st_read("Data/ISTAT/General_data/Limiti01012022_g/Com01012022_g")
  Lombardia_geo <- italy_geo |> dplyr::filter(COD_REG == 3)
  
  # Join the informations about OD correspondance
  matches$ISTAT_code <- as.numeric(matches$ISTAT_code)
  Lombardia_geo <- Lombardia_geo |> left_join(matches, by = c("PRO_COM" = "ISTAT_code"))
  
  # Aggregation 
  Lombardia_geo_agg <- Lombardia_geo %>%
    group_by(OD_name) %>% 
    summarize(geometry = st_union(geometry))
  
  return(Lombardia_geo_agg)
}

# Function to produce exploratory plots
Exploratory_mortality_Lombardia <- function(deaths, geo_df, cl_eta, mortalityWindow, path, mode = "png"){
  if (mode == 'pdf')
    pdf(paste0(path,".pdf"), onefile = TRUE)
  
  # Get all OD areas
  names = sort(unique(deaths$OD_AREA))
  
  # Selecting only the relevant age class
  deaths <- deaths |> filter(CL_ETA == cl_eta)
  
  # Name of the column which contain the mortality density of the correct mortalityWindow
  col <-paste0("md_window", mortalityWindow)
  
  # Set subtitle
  if(cl_eta == "15_21")
    subtitle <- paste0("Age class 70+, aggregation in ", mortalityWindow, " weeks")
  if(cl_eta == "11_14")
    subtitle <- paste0("Age class 50-69, aggregation in ", mortalityWindow, " weeks")
  
  weeks <- 1:52
  for (w in weeks){
    deaths_cur <- deaths |> dplyr::filter(partial_date_death == as.numeric(w)) |> dplyr::select(all_of(c("OD_AREA", col)))
    
    # Create result
    ep <- data.frame("OD_AREA" = names)
    
    ep <- ep |> dplyr::left_join(deaths_cur, by = c("OD_AREA" = "OD_AREA"))
    ep[is.na(ep[,col]), col] <- 0
    
    geo_df_cur <- geo_df |> left_join(ep, by = c("OD_name" = "OD_AREA"))
    
    # Generate plot with correct dates of start and end of the week
    if (w == 1){
      title <- paste0("Mortality rate from ", as.Date(paste(2019, 12, 30, sep="-"), "%Y-%m-%d"), 
                      " to ", as.Date(paste(2020, as.numeric(w)+1, 7, sep="-"), "%Y-%W-%u"))
      
      g = ggplot(geo_df_cur) + geom_sf(aes(fill = .data[[col]])) +
        theme_bw() + theme(text = element_text(size=10),legend.key.size = unit(1,"line") )+ rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") +
        ggtitle(title, subtitle = subtitle) + 
        scale_fill_gradient(low='white', high='red4', name = "Mortality rate") 
    }
    
    if (w > 1 & w < 52){
      title <- paste0("Mortality rate from ", as.Date(paste(2020, w-1, 1, sep="-"), "%Y-%U-%u"), 
                      " to ", as.Date(paste(2020, as.numeric(w)+1, 7, sep="-"), "%Y-%W-%u"))
      
      g = ggplot(geo_df_cur) + geom_sf(aes(fill = .data[[col]])) +
        theme_bw() + theme(text = element_text(size=10),legend.key.size = unit(1,"line") )+ rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") +
        ggtitle(title, subtitle = subtitle) + 
        scale_fill_gradient(low='white', high='red4', name = "Mortality rate")
    }
    if (w == 52){
      title <- paste0("Mortality rate from ", as.Date(paste(2020, w-1, 1, sep="-"), "%Y-%U-%u"), 
                      " to ", as.Date(paste(2021, 1, 2, sep="-"), "%Y-%m-%d"))
      
      g = ggplot(geo_df_cur) + geom_sf(aes(fill = .data[[col]])) +
        theme_bw() + theme(text = element_text(size=10),legend.key.size = unit(1,"line") )+ rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") +
        ggtitle(title, subtitle = subtitle) + 
        scale_fill_gradient(low='white', high='red4', name = "Mortality rate")
    }
    
    # Save
    if (mode == 'pdf')
      grid.arrange(g)  
    if (mode == 'png')
      ggsave(paste0(path,"/week",w,".png"),g)
  }
  
  if (mode == 'pdf')
    dev.off()
}

#### Spatial analysis of the Lombardia area ----
# REM: The function with suffix _RL are designed for the OD dataset of Regione Lombardia

## WEIGHTS DEFINITION
## Defining functions and auxiliary function to perform weights definition
get_OD_RL <- function(Total_net, nodes, col){
  OD_week <- Total_net |>
    filter(ZONA_ORIG %in% nodes, ZONA_DEST %in% nodes) |>
    arrange(ZONA_DEST) |>
    select(ZONA_ORIG, ZONA_DEST, all_of(col)) |>
    pivot_wider(names_from = ZONA_DEST, values_from = all_of(col), values_fill = 0) |>
    arrange(ZONA_ORIG) |>
    column_to_rownames(var = "ZONA_ORIG")
  
  return(OD_week)
}

# Function to symmetrize the OD matrix, which should be in MATRIX form
make_OD_symmetrical <- function(OD){
  # I copy the OD structure 
  OD2 <- OD 
  
  # Setting diagonal to 0
  diag(OD2) <- 0 
  
  # Every element is given by (OD[i,j] + OD[j,i])/2
  OD2 <- (OD + t(OD))/2
  
  return(OD2)
}

# WEIGHT FUNCTION (Case of OD counts)
get_weights_OD_RL <- function(OD, col, symmetrical = F){
  
  nodes = sort(unique(OD$ZONA_ORIG))
  
  M <- get_OD_RL(OD, nodes, col)
  
  # I set the diagonal equal to 0 
  diag(M) <- 0
  
  if (symmetrical == T)
    M <- make_OD_symmetrical(M)
  
  # Row-standardization
  M <- M / apply(M,1,sum)
  
  M <- as.matrix(M)
  
  # Creating the weight structure
  W <- mat2listw(M, row.names = row.names(M), style="M")
  
  return(W)
}

## EPIDEMIC QUANTITITIES DEFINITION
# function to get the epidemic feature.
get_epidemic_exmort_RL <- function (deaths, w, col, names, scale = FALSE){
  deaths <- deaths |> filter(partial_date_death == w) |> dplyr::select(OD_AREA, all_of(col))
  
  # Order dataset to match weights
  deaths <- deaths[order(deaths$OD_AREA),] 
  
  # Create result
  ep <- data.frame("OD_AREA" = names)
  
  ep <- ep |> dplyr::left_join(deaths, by = c("OD_AREA" = "OD_AREA"))
  ep[is.na(ep[,col]), col] <- 0
  
  # Scale if needed
  if (scale == TRUE)
    X <- ep |> pull(col) |> as.numeric() |> scale()
  else
    X <- ep |> pull(col) |> as.numeric()
  
  # Return the result (a vector of the epidemic variable in alphabetic order)
  return(X)
}

## PLOTTING FUNCTION 
my_plot_Moran_RL <- function(geo_df, X, W, w, cl_eta, mob_mode, local.p.value, signif){
  geo_df <- geo_df[order(geo_df$OD_name),] 
  
  comunis <- geo_df
  
  comunis$scaled = scale(X)
  comunis$lagged = lag.listw(W,X, zero.policy = T)
  
  moran.map = cbind(geo_df, local.p.value)
  quadrant = vector(mode = "numeric", length = length(local.p.value))
  quadrant[comunis$scaled >0 & comunis$lagged>0] <- 4  
  quadrant[comunis$scaled <0 & comunis$lagged<0] <- 1      
  quadrant[comunis$scaled <0 & comunis$lagged>0] <- 2
  quadrant[comunis$scaled >0 & comunis$lagged<0] <- 3
  quadrant[local.p.value>signif] <- 0 
  brks <- c(0,1,2,3,4)
  Moran = c("insignificant","low-low","low-high","high-low", "high-high")
  colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
  names(colors) = Moran
  comunis$Moran = Moran[findInterval(quadrant,brks,all.inside=FALSE)]
  
  # Set subtitle
  if(cl_eta == "15_21" & mob_mode == "OD_counts")
    subtitle <- paste0("Mobility-based spatial weights, age class 70+, aggregation in ", mortalityWindow, " weeks")
  if(cl_eta == "11_14" & mob_mode == "OD_counts")
    subtitle <- paste0("Mobility-based spatial weights, age class 50-69, aggregation in ", mortalityWindow, " weeks")
  if(cl_eta == "15_21" & mob_mode == "Cont_weights")
    subtitle <- paste0("Contiguity-based spatial weights, age class 70+, aggregation in ", mortalityWindow, " weeks")
  if(cl_eta == "11_14" & mob_mode == "Cont_weights")
    subtitle <- paste0("Contiguity-based spatial weights, age class 50-69, aggregation in ", mortalityWindow, " weeks")
  
  if(w != 52){
    g = ggplot(comunis) + geom_sf(aes(fill = Moran))  + # ggrepel::geom_text_repel(aes(label = OD_name, geometry = geometry), size = 1.7, stat = "sf_coordinates",max.overlaps = 20) + 
      scale_fill_manual(name = "",values = colors)+ theme_bw() + rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") +
      labs(title =paste("Local Moran Index of week from", as.Date(paste(2020, w, 1, sep="-"), "%Y-%U-%u"), "to", as.Date(paste(2020, as.numeric(w)+1, 7, sep="-"), "%Y-%W-%u")), subtitle = subtitle) +
      theme(text = element_text(size=15),legend.key.size = unit(1,"line"), plot.subtitle = element_text(size = 12),
            plot.margin = unit(c(0,0,0,0), "cm"))
    
  }
  else
  {
    g = ggplot(comunis) + geom_sf(aes(fill = Moran))  + # ggrepel::geom_text_repel(aes(label = OD_name, geometry = geometry), size = 1.7, stat = "sf_coordinates",max.overlaps = 20) + 
      scale_fill_manual(name = "",values = colors)+ theme_bw() + rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") +
      labs(title = paste("Local Moran Index of week from", as.Date(paste(2020, w, 1, sep="-"), "%Y-%U-%u"), "to", as.Date(paste(2020, 12, 31, sep="-"), "%Y-%m-%d")), subtitle = subtitle) +
      theme(text = element_text(size=15),legend.key.size = unit(1,"line"), plot.subtitle = element_text(size = 12),
            plot.margin = unit(c(0,0,0,0), "cm"))
    
  }
  return(g)
}

# I implement a function to perform Global and Local Moran analysis weekly
MORAN_ANALYSIS_RL <- function(Mob, col, deaths, geo_df, mob_mode = "OD_counts", ep_mode = "mortality_pop", path = "Plots/Local_Moran", cl_eta, mortalityWindow, 
                              test_mode = "base", p_adjust = T, signif = 0.05){
  ####
  # mob_mode = "OD_counts" -> uses OD counts
  #          = "Cont_weights" -> Contiguity weights
  # ep_mode = "mortality_pop" -> uses total mortality divided by population (of the station's basin), age 70+
  # test_mode = "base" -> use standard Moran tests
  #           = "perm" -> use Moran test based on permutations
  ####
  
  # Prepare dataset to store the results
  # Global Moran
  Global_Moran_df <- NULL
  # Local Moran
  Local_Moran_df <- NULL
  
  # Select only the relevant age class
  deaths <- deaths |> filter(CL_ETA == cl_eta)
  
  # Areas are
  names = sort(unique(Mob$ZONA_ORIG))
  
  # Weeks are
  weeks <- 1:52
  
  ## GET WEIGHTS
  # REM: In the case of the OD of Regione Lombardia, weights are not time-varying
  if (mob_mode == "Cont_weights"){
    # Contiguity weights
    geo_df <- geo_df[order(geo_df$OD_name),]
    nb = poly2nb(geo_df,row.names = geo_df$OD_name)
    W = nb2listw(nb, zero.policy = T)
  }
  
  if (mob_mode == "OD_counts"){
    # REM: this function performs ROW SCALING
    #      Weights are given by the OD counts divided by the total of outgoing trips
    W <- get_weights_OD_RL(Mob, col, symmetrical = F) 
  }
  
  # Create list to save epidemic quantities
  list_X <- NULL
  
  # Iterating over weeks            
  for (i in 1:length(weeks)){
    w <- weeks[i]
    
    if(ep_mode == "mortality_pop")
      # This function gets mortality divided by population of the area in 2020
      X <- get_epidemic_exmort_RL(deaths, w = w, col = paste0("md_window", mortalityWindow), names, scale = F)
    
    # Append to list
    list_X[[i]] <- X
    
    # MORAN ANALYSIS
    if (test_mode == "base"){
      # Global Moran
      c <- as.data.frame(t(unlist(moran.test(X, W, zero.policy = T))))
      Global_Moran_df <- rbind(Global_Moran_df, cbind(rep(w, dim(c)[1]), c))
      
      # Local Moran
      c <- as.data.frame(cbind(sort(geo_df$OD_name),localmoran(X, W, zero.policy = T)))
      Local_Moran_df <- rbind(Local_Moran_df, cbind(rep(w, dim(c)[1]), c))
    }
    
    if (test_mode == "perm"){
      # Global Moran
      c <- moran.mc(X, W, nsim = 999, zero.policy = T)
      temp <- data.frame("Week" = w,
                         "Moran.I" = c$statistic, 
                         "p-value" = c$p.value)
      Global_Moran_df <- rbind(Global_Moran_df, temp)
      
      # Local Moran
      c <- as.data.frame(localmoran_perm(X, W, nsim=999, zero.policy=T))
      Local_Moran_df <- rbind(Local_Moran_df, cbind(rep(w, dim(c)[1]), names, c))
    }
    
  }
  
  # Fixing dataframes format
  if (test_mode == "base"){
    Global_Moran_df = as.data.frame(Global_Moran_df)
    colnames(Global_Moran_df)[1:4] <- c("Week", "Statistic", "p.value", "Moran.I")
    
    colnames(Local_Moran_df)[1:7] <- c("Week", "OD_name", "Local.Moran.I", "E.Ii", "Var.Ii", "z.I1", "p.value")
  }
  
  if (test_mode == "perm"){
    Global_Moran_df = as.data.frame(Global_Moran_df)
    colnames(Global_Moran_df) <- c("Week", "Moran.I", "p.value")
    
    Local_Moran_df = as.data.frame(Local_Moran_df)
    Local_Moran_df <- Local_Moran_df |> select(1,2,3,4,5,7)
    colnames(Local_Moran_df) <- c("Week", "OD_name", "Local.Moran.I", "E.Ii", "Var.Ii", "p.value")
  }
  
  
  ## P VALUE ADJUSTMENT
  if(p_adjust == T){
    Global_Moran_df$p.value <- p.adjust(Global_Moran_df$p.value, "BH")
    Local_Moran_df$p.value <- p.adjust(Local_Moran_df$p.value, "BH")
  }
  
  ## PLOTS
  # Prepare plotting
  # pdf(path, onefile = TRUE)
  
  C <- Global_Moran_df
  C[,1:dim(C)[2]] <- sapply(C[,1:dim(C)[2]], as.numeric)
  
  C <- C |>
    mutate(
      sig = case_when(
        p.value < signif ~ "red",
        p.value >= signif ~ "grey"
      ))
  C$Week <- as.Date(as.Date(paste(2020, C$Week, 1, sep="-"), "%Y-%U-%u"))
  # Set subtitle
  if(cl_eta == "15_21" & mob_mode == "OD_counts")
    subtitle <- paste0("Mobility-based spatial weights, age class 70+, aggregation in ", mortalityWindow, " weeks")
  if(cl_eta == "11_14" & mob_mode == "OD_counts")
    subtitle <- paste0("Mobility-based spatial weights, age class 50-69, aggregation in ", mortalityWindow, " weeks")
  if(cl_eta == "15_21" & mob_mode == "Cont_weights")
    subtitle <- paste0("Contiguity-based spatial weights, age class 70+, aggregation in ", mortalityWindow, " weeks")
  if(cl_eta == "11_14" & mob_mode == "Cont_weights")
    subtitle <- paste0("Contiguity-based spatial weights, age class 50-69, aggregation in ", mortalityWindow, " weeks")
  
  p <- ggplot(C, aes(Week, Moran.I))+
    geom_point(color = C$sig)+
    geom_line()+
    # ggtitle(paste("Global Moran Index, aggregation in",step_aggr,"weeks")) +
    labs(title = paste("Global Moran Index"),  subtitle = subtitle) +
    theme_bw() + ylab("Moran I") + xlab("Date") + # ylim(-0.151, 0.51) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%B") + theme(axis.text.x = element_text(angle = 50, hjust=0.95), text = element_text(size=15), axis.title.x = element_blank(),
                                             plot.subtitle = element_text(size = 12)) 
  ggsave(paste0(path,"/GM.png"),p)
  
  ## LOCAL PLOTS 
  # Iterate weekly
  for (i in 1:length(weeks)){
    w <- weeks[i]
    X <- list_X[[i]]
    
    # Plot
    local.p.value <- Local_Moran_df |> dplyr::filter(Week == w) |> pull(p.value)
    g <- my_plot_Moran_RL(geo_df, X, W, w, cl_eta, mob_mode, local.p.value, signif)
    
    # grid.arrange(g) 
    ggsave(paste0(path,"/LM_",w,".png"),g, width = 20, height = 17, units = "cm")
  }
  
  # dev.off()
  
  return(list("Global_Moran" = Global_Moran_df, "Local_Moran" = Local_Moran_df))
}

#### Spatial analysis of the Brescia-Bergamo-Milano area ----
# Function to produce the geographical dataset of BreBeMi area aggregated into basins
BreBeMi_geodf <- function(matches){
  # Load geographical dataset of Italy
  italy_geo <- st_read("Data/ISTAT/General_data/Limiti01012022_g/Com01012022_g")
  
  italy_geo <- italy_geo |> dplyr::filter(COMUNE %in% matches$Comune_name) |> 
    left_join(matches, by = c("COMUNE" = "Comune_name"))
  
  # Aggregation 
  stat_agg <- italy_geo %>%
    group_by(Station_ref_name) %>% 
    summarize(geometry = st_union(geometry))
  rm(italy_geo)
  
  # Remove Verona and Peschiera
  stat_agg <- stat_agg |> dplyr::filter(!(Station_ref_name %in% c("Verona", "Peschiera del Garda")))
  
  return(stat_agg)
}

# Function to clean distances data
distances_cleaned <- function(net){
  # load comuni informations from ISTAT
  # The correspondance between the two files is given by "Codice Comune formato alfanumerico"
  info <- read_excel("Data/ISTAT/General_data/Codici-statistici-e-denominazioni-al-01_01_2022.xls")
  # Remove white spaces since they are causing problems 
  colnames(info) <- make.names(names(info))
  
  # To start, keep only net rows having origin AND destination in the provinces of interest
  provinces_of_interest = c("Monza e della Brianza", "Milano", "Bergamo", "Lecco", "Brescia", "Verona")
  
  # List of the codes respectig this condition
  list_codes <- info |> 
    filter(Denominazione.Regione %in% c("Lombardia", "Veneto")) |>
    dplyr::select(Codice.Comune.formato.numerico) |> pull(Codice.Comune.formato.numerico)
  
  # Selecting only edeges having origin AND destination in this list
  net2 <- net |> filter(Origine %in% list_codes & Destinazione %in% list_codes)
  
  ## Defining nodes and edges
  # I want the IDs with names and provinces 
  nodes <- info |> 
    filter(Denominazione.Regione %in% c("Lombardia", "Veneto"))
  nodes <- nodes[,c(16, 7, 12)]
  colnames(nodes) <- c("ID", "Comune", "Province")
  
  edges <- net2 |> dplyr::select(-Name) |> rename(Minuti = Total_Minu, Metri = Total_Mete)
  # Removing edges having same origin and destination
  edges <- edges |> filter(Origine != Destinazione)
  
  ## 25 municipalities are new; copy their data from old municipalities 
  new_codes <- nodes |> filter(!(ID %in% edges$Origine) | !(list_codes %in% edges$Destinazione)) |> pull(ID)
  
  # I import the dataset of suppressed comunis
  suppressed_comunis <- read_excel("Data/ISTAT/General_data/Elenco_comuni_soppressi.xls")
  colnames(suppressed_comunis) <- make.names(names(suppressed_comunis))
  
  correspondances <- suppressed_comunis |> 
    filter(as.numeric(Codice.del.Comune.associato.alla.variazione) %in% new_codes) |>
    group_by(Codice.del.Comune.associato.alla.variazione) |>
    filter(row_number()==1) |>
    dplyr::select(Codice.del.Comune.associato.alla.variazione, Codice.Comune)
  colnames(correspondances) = c("New_code", "Old_code")
  correspondances$Old_code <- as.numeric(correspondances$Old_code)
  correspondances$New_code <- as.numeric(correspondances$New_code)
  
  idx <- net$Origine %in% correspondances$Old_code
  net$Origine[idx] <- correspondances$New_code[match(net$Origine[idx], correspondances$Old_code)]
  
  idx <- net$Destinazione %in% correspondances$Old_code
  net$Destinazione[idx] <- correspondances$New_code[match(net$Destinazione[idx], correspondances$Old_code)]
  
  # I handle Torre de' Busi since it has not be suppressed; it changed province
  net[net$Origine == 97080,"Origine"] = 16215
  net[net$Destinazione == 97080,"Destinazione"] = 16215
  
  # Check if there are other problems in codes correspondance
  net2 <- net |> filter(Origine %in% list_codes & Destinazione %in% list_codes)
  edges <- net2 |> dplyr::select(-Name) |> rename(Minuti = Total_Minu, Metri = Total_Mete)
  edges <- edges |> filter(Origine != Destinazione)
  nodes |> filter(!(ID %in% edges$Origine) | !(list_codes %in% edges$Destinazione)) 
  
  ## TWO OTHER CASES 
  # Campione d'Italia -> REMOVE; I CANNOT ESTIMATE TIMES
  nodes <- nodes |> filter(!(ID == 13040))
  list_codes <- setdiff(list_codes, 13040)
  
  # Monte Isola -> ADD TIMES TO GO BY BOAT TO SULZANO - 22 minutes, searched by google maps; 5200 metri 
  # Select net of Sulzano
  Sulzano_code <- info |> filter(Denominazione.in.italiano == "Sulzano") |> pull(Codice.Comune.formato.numerico)
  net_MoIs <- net |> filter(Origine ==  Sulzano_code| Destinazione == Sulzano_code)
  
  # Substitute code with Monte Isola code 
  MoIs_code <- info |> filter(Denominazione.in.italiano == "Monte Isola") |> pull(Codice.Comune.formato.numerico)
  net_MoIs[net_MoIs$Origine == Sulzano_code,"Origine"] = MoIs_code
  net_MoIs[net_MoIs$Destinazione == Sulzano_code,"Destinazione"] = MoIs_code
  
  # remove duplicates
  net_MoIs <- net_MoIs |> filter(!(Origine==Destinazione))
  
  # To every arc, add 22 minutes and 5200 metres
  net_MoIs$Total_Minu = net_MoIs$Total_Minu + 22
  net_MoIs$Total_Mete = net_MoIs$Total_Mete + 5200
  
  # add arc for MoIs-Sulzano $ Sulzano-MoIs
  add_arcs <- data.frame("Name" = c(paste(MoIs_code, Sulzano_code, sep="-"), paste(Sulzano_code, MoIs_code, sep="-")),
                         "Origine" = c(MoIs_code, Sulzano_code), 
                         "Destinazione" = c(Sulzano_code, MoIs_code),
                         "Total_Minu" = c(22,22),
                         "Total_Mete" = c(5200,5200))
  net_MoIs <- rbind(net_MoIs, add_arcs)
  
  # add to net
  net <- rbind(net, net_MoIs)
  
  net$Total_Minu <- as.numeric(net$Total_Minu)
  net$Total_Mete <- as.numeric(net$Total_Mete)
  
  return(net)
}

# Function that assigns each municipality to the closest Trenord station
closest_station <- function(Stations_correspondances, stats_codes, comuni_distances){
  
  # Skip Campione d'Italia since distances estimates are not available
  skip_code <- 13040
  
  # Check if the act_code is in stats_codes
  # is_in_stats <- Stations_correspondances$Code %in% stats_codes
  
  # # Find the comune with a station
  # station_indexes <- which(is_in_stats)
  # Stations_correspondances[station_indexes, "Station_ref_code"] <- Stations_correspondances[station_indexes, "Code"]
  # Stations_correspondances[station_indexes, c("Station_distance_meters", "Station_distance_minutes")] <- 0
  # 
  # Find candidates and select the one with minimum distance
  candidates <- comuni_distances %>%
    filter(Origine %in% Stations_correspondances$Code, Destinazione %in% stats_codes) %>%
    group_by(Origine) %>%
    filter(Total_Mete == min(Total_Mete)) %>% slice(1) |>
    ungroup() %>%
    select(Destinazione, Total_Mete, Total_Minu)
  
  # Update Stations_correspondances with the selected candidates
  # update_indexes <- !is_in_stats & Stations_correspondances$Code != skip_code
  
  Stations_correspondances[Stations_correspondances$Code != skip_code, c("Station_ref_code", "Station_distance_meters", "Station_distance_minutes")] <- 
    candidates
  
  
  return(Stations_correspondances)
  
}

# Function that builds the stations correspondances dataset
build_stations_correspondances <- function(comuni_ISTAT, stations, comuni_distances, IS_area) {
  # Filter Stations_correspondances based on Denominazione.Regione
  Stations_correspondances <- comuni_ISTAT %>%
    filter(Denominazione.Regione %in% c("Lombardia", "Veneto")) %>%
    select(Code = 16, Name = 7, Province = 12)
  
  # Filter stats_comuni using inner_join
  stats_comuni <- stations %>%
    filter(Comune %in% Stations_correspondances$Name) %>%
    pull(Comune)
  
  # Filter stats_codes using semi_join
  stats_codes <- comuni_ISTAT %>%
    filter(Denominazione.in.italiano %in% stats_comuni) %>%
    pull(Codice.Comune.formato.numerico)
  
  # Initialize the dataset with NA values
  Stations_correspondances <- Stations_correspondances %>%
    mutate(Station_ref_code = NA,
           Station_distance_meters = NA,
           Station_distance_minutes = NA)
  
  # Apply the function that assigns each municipality to the closest stations
  Stations_correspondances <- closest_station(Stations_correspondances, stats_codes, comuni_distances)
  
  # Perform a left_join with comuni_ISTAT to add comune and province of the station
  Stations_correspondances <- Stations_correspondances %>%
    left_join(comuni_ISTAT, by = c("Station_ref_code" = "Codice.Comune.formato.numerico")) %>%
    select(Comune_code = Code, Comune_name = Name, Comune_province = Province,
           Station_ref_code, Station_ref_meters = Station_distance_meters, Station_ref_minutes = Station_distance_minutes,
           Station_ref_name = Denominazione.in.italiano, Station_ref_province = Denominazione.Regione)
  
  # Replace stations of Milan area with "IS area"
  IS_area_comunis <- stations %>%
    filter(Codice %in% IS_area) %>%
    pull(Comune)
  
  Stations_correspondances$Station_ref_name[Stations_correspondances$Station_ref_name %in% IS_area_comunis] <- "IS area"
  Stations_correspondances$Station_ref_province[Stations_correspondances$Station_ref_name == "IS area"] <- "Milano"
  
  return(Stations_correspondances)
}

#### Aggregation and exploratory analysis of ISTAT death data into stations basins ----
# Function to get mortality data aggregated at BreBeMi basins level, applying an aggregation 
# for computation of mortality divided by population
deaths_data_cleaned_BreBeMi <- function(deaths, pop, matches, mortalityWindows, IS_area){
  deaths <- deaths |>
    select(all_of(c("COD_PROVCOM", "partial_date_death", "CL_ETA", "T_19", "T_20", "T_21",
                    "NOME_PROVINCIA", "NOME_REGIONE", "NOME_COMUNE"))) |>
    # Select only Lombardia 
    filter(NOME_REGIONE == "Lombardia") |>
    # Now I transform the dataset to have dates dependent on year and only one column counting the deaths for that day
    pivot_longer(cols = T_19:T_21, names_to = "Year", values_to = "T_deaths") |>
    # Then I apply my numbering in weeks of the column partial_date_death
    # weeks of 2020 are 00-52
    # weeks of 2019 are -52-00
    # weeks of 2021 are 52-104
    mutate(partial_date_death = case_when(
      Year == "T_19" ~ -(52-as.numeric(format(as.Date(partial_date_death), "%W"))), 
      Year == "T_20" ~ as.numeric(format(as.Date(partial_date_death), "%W")), 
      Year == "T_21" ~ 52 + as.numeric(format(as.Date(partial_date_death), "%W")), 
    )) |> select(-Year) |>
    # Aggregation into weeks
    group_by(COD_PROVCOM, partial_date_death, CL_ETA, NOME_PROVINCIA, NOME_REGIONE, NOME_COMUNE) |>
    summarise(T_deaths = sum(T_deaths)) |> ungroup() |> 
    # Join population info
    left_join(pop |> select(istat, CL_ETA, num_residenti),
              by = c("COD_PROVCOM" = "istat", "CL_ETA")) |>
    mutate(COD_PROVCOM = as.numeric(COD_PROVCOM)) |>
    # Join with the matches dataset to recover the corresponding station
    right_join(matches, by = c("COD_PROVCOM" = "ISTAT_code")) |>
    # Removing Verona and Peschiera 
    filter(!(Station_ref_name %in% c("Verona", "Peschiera del Garda"))) |>
    # Aggregate
    select(-c(Station_ref_code, COD_PROVCOM, Station_ref_meters, Station_ref_minutes)) |>
    group_by(partial_date_death, CL_ETA, Station_ref_name, Station_ref_province) |>
    summarise(across(where(is.numeric), sum))  |>
    ungroup()
  
  ## COMPUTATION OF  MORTALITY DENSITY
  # Add one empty column for every element of the vector mortality Windows
  cols <- paste0("md_window", mortalityWindows)
  deaths[,cols] <- NA
  
  # Create progress bar
  pb <- progress_bar$new(
    format = "  computing [:bar] :percent eta: :eta",
    total = dim(deaths)[1], clear = FALSE, width= 60)
  
  for (i in 1:dim(deaths)[1]){
    r <- deaths[i,]
    cl_eta <- r$CL_ETA
    area <- r$Station_ref_name
    w <- as.numeric(r$partial_date_death)
    
    for (mw in mortalityWindows){
      # Compute the number of deaths in the sliding window
      act <- deaths |> dplyr::filter(CL_ETA == cl_eta & Station_ref_name == area & as.numeric(partial_date_death) %in% (w-mw+1):w) |> 
        summarise(across(where(is.numeric), sum)) |> dplyr::select(T_deaths)
      
      # Compute mortality density as deaths / population
      deaths[i, paste0("md_window", mw)] <- act / r$num_residenti
    }
    
    pb$tick()
  }
  
  # Convert from list to numeric
  deaths <- deaths |>
    mutate(across(starts_with("md_window"), as.numeric))
  
  return(deaths)
}

# Aggregate OD of Regione Lombardia and clean to use at BreBeMi level
aggregate_OD_RL_BreBeMi <- function(OD, matches, stations, IS_area){
  # Compute total movements and railway movements
  OD <- OD |> mutate(TOT = rowSums(across(where(is.numeric))),
                     FERRO = rowSums(across(ends_with('_FERRO')))) |>
    # Filter only the area
    filter(ZONA_ORIG %in% unique(matches$OD_name) & ZONA_DEST %in% unique(matches$OD_name))
  
  # I also have to aggregate the IS area 
  # Comuni in Milan area are 
  comuni_IS_area <- stations |> filter(Codice %in% IS_area) |> pull(Comune) |> unique()
  matches <- matches |> mutate(Station_ref_name = replace(Station_ref_name, Station_ref_name %in% comuni_IS_area, "IS area"))
  
  # Convert zone names to stations names 
  OD$ZONA_ORIG <-  with(matches, Station_ref_name[match(OD$ZONA_ORIG, OD_name)])
  OD$ZONA_DEST <-  with(matches, Station_ref_name[match(OD$ZONA_DEST, OD_name)])
  
  OD <- OD |> dplyr::select(!c(PROV_ORIG, PROV_DEST)) |> group_by(ZONA_ORIG, ZONA_DEST) |>
    summarise(across(where(is.numeric), sum))  |> na.omit()
  
  return(OD)
}

# Function to produce exploratory plots
Exploratory_mortality_BreBeMi <- function(deaths, geo_df, cl_eta, mortalityWindow, path, mode = "png"){
  if (mode == 'pdf')
    pdf(paste0(path,".pdf"), onefile = TRUE)
  
  # Get all OD areas
  names = sort(unique(deaths$Station_ref_name))
  
  # Selecting only the relevant age class
  deaths <- deaths |> filter(CL_ETA == cl_eta)
  
  # Name of the column which contain the mortality density of the correct mortalityWindow
  col <-paste0("md_window", mortalityWindow)
  
  # Set subtitle
  if(cl_eta == "15_21")
    subtitle <- paste0("Age class 70+, aggregation in ", mortalityWindow, " weeks")
  if(cl_eta == "11_14")
    subtitle <- paste0("Age class 50-69, aggregation in ", mortalityWindow, " weeks")
  
  weeks <- 1:52
  for (w in weeks){
    deaths_cur <- deaths |> dplyr::filter(partial_date_death == as.numeric(w)) |> dplyr::select(all_of(c("Station_ref_name", col)))
    
    # Create result
    ep <- data.frame("Station_ref_name" = names)
    
    ep <- ep |> dplyr::left_join(deaths_cur, by = c("Station_ref_name" = "Station_ref_name"))
    ep[is.na(ep[,col]), col] <- 0
    
    geo_df_cur <- geo_df |> left_join(ep, by = c("Station_ref_name" = "Station_ref_name"))
    
    # Generate plot with correct dates of start and end of the week
    if (w == 1){
      title <- paste0("Mortality rate from ", as.Date(paste(2019, 12, 30, sep="-"), "%Y-%m-%d"), 
                      " to ", as.Date(paste(2020, as.numeric(w)+1, 7, sep="-"), "%Y-%W-%u"))
      
      g = ggplot(geo_df_cur) + geom_sf(aes(fill = .data[[col]])) +
        theme_bw() + theme(text = element_text(size=10),legend.key.size = unit(1,"line") )+ rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") +
        ggtitle(title, subtitle = subtitle) + 
        scale_fill_gradient(low='white', high='red4', name = "Mortality rate") 
    }
    
    if (w > 1 & w < 52){
      title <- paste0("Mortality rate from ", as.Date(paste(2020, w-1, 1, sep="-"), "%Y-%U-%u"), 
                      " to ", as.Date(paste(2020, as.numeric(w)+1, 7, sep="-"), "%Y-%W-%u"))
      
      g = ggplot(geo_df_cur) + geom_sf(aes(fill = .data[[col]])) +
        theme_bw() + theme(text = element_text(size=10),legend.key.size = unit(1,"line") )+ rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") +
        ggtitle(title, subtitle = subtitle) + 
        scale_fill_gradient(low='white', high='red4', name = "Mortality rate")
    }
    if (w == 52){
      title <- paste0("Mortality rate from ", as.Date(paste(2020, w-1, 1, sep="-"), "%Y-%U-%u"), 
                      " to ", as.Date(paste(2021, 1, 2, sep="-"), "%Y-%m-%d"))
      
      g = ggplot(geo_df_cur) + geom_sf(aes(fill = .data[[col]])) +
        theme_bw() + theme(text = element_text(size=10),legend.key.size = unit(1,"line") )+ rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") +
        ggtitle(title, subtitle = subtitle) + 
        scale_fill_gradient(low='white', high='red4', name = "Mortality rate")
    }
    
    # Save
    if (mode == 'pdf')
      grid.arrange(g)  
    if (mode == 'png')
      ggsave(paste0(path,"/week",w,".png"),g)
  }
  
  if (mode == 'pdf')
    dev.off()
}

#### Spatial analysis of Regione Lombardia mobility data ----
# Function to get the epidemic exmort quantities in this case
get_epidemic_exmort_BreBeMi <- function (deaths, w, col, names, scale = FALSE){
  deaths <- deaths |> filter(partial_date_death == w) |> dplyr::select(Station_ref_name, all_of(col))
  
  # Order dataset to match weights
  deaths <- deaths[order(deaths$Station_ref_name),] 
  
  # Create result
  ep <- data.frame("Station_ref_name" = names)
  
  ep <- ep |> dplyr::left_join(deaths, by = c("Station_ref_name" = "Station_ref_name"))
  ep[is.na(ep[,col]), col] <- 0
  
  # Scale if needed
  if (scale == TRUE)
    X <- ep |> pull(col) |> as.numeric() |> scale()
  else
    X <- ep |> pull(col) |> as.numeric()
  
  # Return the result (a vector of the epidemic variable in alphabetic order)
  return(X)
}

## PLOTTING FUNCTION 
my_plot_Moran_BreBeMi <- function(geo_df, X, W, w, local.p.value, signif = 0.05, subtitle){
  geo_df <- geo_df[order(geo_df$Station_ref_name),] 
  
  comunis <- geo_df
  comunis <- comunis[order(comunis$Station_ref_name),] 
  
  comunis$scaled = scale(X)
  comunis$lagged = lag.listw(W,X, zero.policy = T)
  
  moran.map = cbind(geo_df, local.p.value)
  quadrant = vector(mode = "numeric", length = length(local.p.value))
  quadrant[comunis$scaled >0 & comunis$lagged>0] <- 4  
  quadrant[comunis$scaled <0 & comunis$lagged<0] <- 1      
  quadrant[comunis$scaled <0 & comunis$lagged>0] <- 2
  quadrant[comunis$scaled >0 & comunis$lagged<0] <- 3
  quadrant[local.p.value>signif] <- 0 
  brks <- c(0,1,2,3,4)
  Moran = c("insignificant","low-low","low-high","high-low", "high-high")
  colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
  names(colors) = Moran
  comunis$Moran = Moran[findInterval(quadrant,brks,all.inside=FALSE)]
  if(w != 52){
    g = ggplot(comunis) + geom_sf(aes(fill = Moran))  + # ggrepel::geom_text_repel(aes(label = OD_name, geometry = geometry), size = 1.7, stat = "sf_coordinates",max.overlaps = 20) + 
      scale_fill_manual(name = "",values = colors)+ theme_bw() + rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") +
      labs(title = paste("Local Moran Index of week from", as.Date(paste(2020, w, 1, sep="-"), "%Y-%U-%u"), "to", as.Date(paste(2020, as.numeric(w)+1, 7, sep="-"), "%Y-%W-%u")), subtitle = subtitle) +
      theme(text = element_text(size=15),legend.key.size = unit(1,"line"), plot.subtitle = element_text(size = 12))
    
  }
  else
  {
    g = ggplot(comunis) + geom_sf(aes(fill = Moran))  + # ggrepel::geom_text_repel(aes(label = OD_name, geometry = geometry), size = 1.7, stat = "sf_coordinates",max.overlaps = 20) + 
      scale_fill_manual(name = "",values = colors)+ theme_bw() + theme(text = element_text(size=10),legend.key.size = unit(1,"line") )+ rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") +
      labs(title = paste("Local Moran Index of week from", as.Date(paste(2020, w, 1, sep="-"), "%Y-%U-%u"), "to", as.Date(paste(2020, 12, 31, sep="-"), "%Y-%m-%d")), subtitle = subtitle) +
      theme(text = element_text(size=15),legend.key.size = unit(1,"line"), plot.subtitle = element_text(size = 12))
    
  }
  return(g)
}

# I implement a function to perform the pipeline of Moran analysis at BreBeMi level on Regione Lombardia data
MORAN_ANALYSIS_BREBEMI_RL <- function(Mob, col, deaths, geo_df, mob_mode = "OD_counts", ep_mode = "mortality_pop", path = "Plots/Local_Moran", cl_eta, mortalityWindow, 
                                      test_mode = "base", signif = 0.05, p_adjust = T){
  ####
  # mob_mode = "OD_counts" -> uses OD counts
  # ep_mode = "mortality_pop" -> uses total mortality divided by population (of the station's basin), age 70+
  # test_mode = "base" -> use standard Moran tests
  #           = "perm" -> use Moran test based on permutations
  ####
  
  # Prepare dataset to store the results
  # Global Moran
  Global_Moran_df <- NULL
  # Local Moran
  Local_Moran_df <- NULL
  
  # Select only the relevant age class
  deaths <- deaths |> filter(CL_ETA == cl_eta)
  
  # Removing Verona and Peschiera from Mob dataset
  Mob <- Mob |> filter(!(ZONA_ORIG %in% c("Verona", "Peschiera del Garda")) & !(ZONA_DEST %in% c("Verona", "Peschiera del Garda")))
  
  # Areas are
  names = sort(unique(Mob$ZONA_ORIG))
  
  # Weeks are
  weeks <- 1:52
  
  ## GET WEIGHTS
  if (mob_mode == "OD_counts"){
    # REM: this function performs ROW SCALING
    #      Weights are given by the OD counts divided by the total of outgoing trips
    W <- get_weights_OD_RL(Mob, col, symmetrical = F) 
  }
  
  # Create list to save epidemic quantities
  list_X <- NULL
  
  # Iterating over weeks            
  for (i in 1:length(weeks)){
    w <- weeks[i]
    
    if(ep_mode == "mortality_pop")
      # This function gets mortality divided by population of the area in 2020
      X <- get_epidemic_exmort_BreBeMi(deaths, w = w, col = paste0("md_window", mortalityWindow), names, scale = F)
    
    # Append to list
    list_X[[i]] <- X
    
    # MORAN ANALYSIS
    if (test_mode == "base"){
      # Global Moran
      c <- as.data.frame(t(unlist(moran.test(X, W, zero.policy = T))))
      Global_Moran_df <- rbind(Global_Moran_df, cbind(rep(w, dim(c)[1]), c))
      
      # Local Moran
      c <- as.data.frame(cbind(names, localmoran(X, W, zero.policy = T)))
      Local_Moran_df <- rbind(Local_Moran_df, cbind(rep(w, dim(c)[1]), c))
    }
    if (test_mode == "perm"){
      # Global Moran
      c <- moran.mc(X, W, nsim = 99999, zero.policy = T)
      temp <- data.frame("Week" = w,
                         "Moran.I" = c$statistic, 
                         "p-value" = c$p.value)
      Global_Moran_df <- rbind(Global_Moran_df, temp)
      
      # Local Moran
      c <- as.data.frame(localmoran_perm(X, W, nsim=99999, zero.policy=T))
      Local_Moran_df <- rbind(Local_Moran_df, cbind(rep(w, dim(c)[1]), names, c))
    }
    
  }
  
  # Fixing dataframes format
  if (test_mode == "base"){
    Global_Moran_df = as.data.frame(Global_Moran_df)
    colnames(Global_Moran_df)[1:4] <- c("Week","Statistic", "p.value", "Moran.I")
    
    colnames(Local_Moran_df)[1:7] <- c("Week", "OD_name", "Local.Moran.I", "E.Ii", "Var.Ii", "z.I1", "p.value")
  }
  if (test_mode == "perm"){
    Global_Moran_df = as.data.frame(Global_Moran_df)
    colnames(Global_Moran_df) <- c("Week", "Moran.I", "p.value")
    
    Local_Moran_df = as.data.frame(Local_Moran_df)
    Local_Moran_df <- Local_Moran_df |> select(1,2,3,4,5,7)
    colnames(Local_Moran_df) <- c("Week", "OD_name", "Local.Moran.I", "E.Ii", "Var.Ii", "p.value")
  }
  
  ## P VALUE ADJUSTMENT
  if(p_adjust == T){
    Global_Moran_df$p.value <- p.adjust(Global_Moran_df$p.value, "BH")
    Local_Moran_df$p.value <- p.adjust(Local_Moran_df$p.value, "BH")
  }
  
  ## PLOTS
  # Prepare plotting
  # pdf(path, onefile = TRUE)
  C <- Global_Moran_df
  C[,1:dim(C)[2]] <- sapply(C[,1:dim(C)[2]], as.numeric)
  
  C <- C |>
    mutate(
      sig = case_when(
        p.value < signif ~ "red",
        p.value >= signif ~ "grey"
      ))
  if (col == "TOT")
    subtitle <- paste("Overall mobility, aggregation of death counts into", mortalityWindow, "weeks")
  if (col == "FERRO")
    subtitle <- paste("Railway mobility, aggregation of death counts into", mortalityWindow, "weeks")
  
  C$Week <- as.Date(as.Date(paste(2020, C$Week, 1, sep="-"), "%Y-%U-%u"))
  
  p <- ggplot(C, aes(Week, Moran.I))+
    geom_point(color = C$sig)+
    geom_line()+
    labs(title = "Global Moran Index", subtitle = subtitle) +
    theme_bw() + theme_bw() + ylab("Moran I") + xlab("Date") + ylim(-0.151, 0.503) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%B") + theme(axis.text.x = element_text(angle = 50, hjust=0.95), text = element_text(size=15), axis.title.x = element_blank(),
                                             plot.subtitle = element_text(size = 12)) 
  ggsave(paste0(path,"/GM.png"),p)
  
  ## LOCAL PLOTS 
  # Iterate weekly
  for (i in 1:length(weeks)){
    w <- weeks[i]
    X <- list_X[[i]]
    
    # Plot
    local.p.value <- Local_Moran_df |> dplyr::filter(Week == w) |> pull(p.value)
    g <- my_plot_Moran_BreBeMi(geo_df, X, W, w, local.p.value, signif, subtitle)
    
    # grid.arrange(g) 
    ggsave(paste0(path,"/LM_",w,".png"), g, height = 7.5, width = 16, units = "cm")
  }
  
  # dev.off()
  
  return(list("Global_Moran" = Global_Moran_df, "Local_Moran" = Local_Moran_df))
}

#### Spatial analysis of Trenord mobility data ----
# Function to select the OD matrix in the vector ws, sum them and convert into matrix form 
OD_week_aggregated <- function(OD, ws, station_codes){
  OD_week <- OD |>
    filter(Start %in% station_codes, End %in% station_codes) |>
    select(Start, End, all_of(paste("Week",ws,sep="_"))) 
  
  # Now I need to find the inactive stations, i.e., stations with no departures AND no arrivals in AT LEAST one of the weeks
  # I compute the number of departures and arrivals by week
  tot_dep <- OD_week |> group_by(Start) |> summarise_at(vars(one_of(paste("Week",ws,sep="_"))), sum)
  tot_arr <- OD_week |> group_by(End) |> summarise_at(vars(one_of(paste("Week",ws,sep="_"))), sum)
  
  removed_stats <- NULL
  for (col in 2:dim(tot_dep)[2]){
    # Find the station which in the week corresponding to col have no arrivals and no departures
    ids <- intersect(pull(tot_dep[which(tot_dep[,col] == 0),1]), pull(tot_arr[which(tot_arr[,col] == 0),1]))
    
    # Add to the list of stations to remove
    removed_stats <- union(removed_stats, ids)
  }
  
  # I remove the stations if needed and then sum the columns
  OD_week <- OD_week |> filter(!(Start %in% removed_stats) & !(End %in% removed_stats)) |>
    arrange(End) |>
    mutate(Value = rowSums(across(starts_with("Week"))), .keep = "unused") |>
    pivot_wider(names_from = End, values_from = Value, values_fill = 0) |>
    arrange(Start) |>
    column_to_rownames(var = "Start") 
  
  return(list("OD_week" = OD_week, "removed_stats" = removed_stats))
}

# Function to produce the Trenord-based OD weights
# WARNING: If the station has been inactive for AT LEAST ONE WEEK in the period (w-mobilityWindow+1):w, it is removed. 
get_weights_OD_Trenord <- function(OD, w, mobilityWindow, symmetrical = FALSE){
  # I define a vector of all the weeks to select and sum
  ws <- (w-mobilityWindow+1):w
  
  # I need ws in the correct format
  ws <- ifelse(ws<10,  paste0("0",as.character(ws)), as.character(ws))
  ws <- paste("2020", ws, sep = "_")
  
  stats = sort(unique(OD$Start))
  
  r <- OD_week_aggregated(OD, ws, stats)
  M <- r$OD_week
  removed_stats <- r$removed_stats
  
  # I set the diagonal equal to 0 
  diag(M) <- 0
  
  if (symmetrical == T)
    M <- make_OD_symmetrical(M)
  
  # Row-standardization
  M <- M / apply(M,1,sum)
  
  M <- as.matrix(M)
  
  # If there are NaN (division by 0), REMOVE THE STATIONS (but save them to remove also the mortalities)
  M[is.nan(M)] <- 0
  id <- apply(M,1,sum) != 0
  names <- row.names(M)
  removed_stats <- union(removed_stats, names[apply(M,1,sum) == 0])
  M <- M[id,id]
  
  # Creating the weight structure
  W <- mat2listw(M, row.names = row.names(M), style="M")
  
  return(list("W" = W, "removed_stats" = removed_stats))
}

# Function to plot Local Moran indexes on the Trenord dataset
my_plot_Moran_BreBeMi_Trenord <- function(geo_df, X, W, w, lag, mortalityWindow, cl_eta, removed_stats, local.p.value, signif = signif){
  stats <- geo_df |> filter(!(Station_ref_name %in% removed_stats))
  stats <- stats[order(stats$Station_ref_name),] 
  
  stats$scaled = scale(X)
  stats$lagged = lag.listw(W,X, zero.policy = T)
  
  moran.map = cbind(stats, local.p.value)
  quadrant = vector(mode = "numeric", length = length(local.p.value))
  quadrant[stats$scaled >0 & stats$lagged>0] <- 4  
  quadrant[stats$scaled <0 & stats$lagged<0] <- 1      
  quadrant[stats$scaled <0 & stats$lagged>0] <- 2
  quadrant[stats$scaled >0 & stats$lagged<0] <- 3
  quadrant[local.p.value>signif] <- 0 
  brks <- c(0,1,2,3,4)
  Moran = c("insignificant","low-low","low-high","high-low", "high-high", "inactive station")
  colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red", "darkgrey")
  names(colors) = Moran
  stats$Moran = Moran[findInterval(quadrant,brks,all.inside=FALSE)]
  
  X <- stats |> select(Station_ref_name, Moran) |> st_drop_geometry()
  
  stats <- geo_df |> left_join(X, by = "Station_ref_name")
  stats$Moran[is.na(stats$Moran)] <- "inactive station"
  
  if (cl_eta == "15_21")
    subtitle <- paste("Railway mobility, age class 70+, aggregation of death counts into", mortalityWindow, 
                      "weeks\nlag of", lag, "weeks, aggregation of mobility data into", mobilityWindow, "weeks")
  
  if (cl_eta == "11_14")
    subtitle <- paste("Railway mobility, age class 50-69, aggregation of death counts into", mortalityWindow, 
                      "weeks\nlag of", lag, "weeks, aggregation of mobility data into", mobilityWindow, "weeks")
  
  if(w+lag+mortalityWindow-1 < 52){
    g = ggplot(stats) + geom_sf(aes(fill = Moran))+ # ggrepel::geom_text_repel(aes(label = Station_ref_name, geometry = geometry), size = 1.7, stat = "sf_coordinates",max.overlaps = 20) + 
      scale_fill_manual(name = "",values = colors)+ theme_bw() + theme(text = element_text(size=10),legend.key.size = unit(1,"line") )+ rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") +
      labs(title = paste("Local Moran Index of week from", as.Date(paste(2020, w+lag+mortalityWindow-1, 1, sep="-"), "%Y-%U-%u"), "to", as.Date(paste(2020, w+lag+mortalityWindow-1, 1, sep="-"), "%Y-%U-%u")+6), 
           subtitle = subtitle) +
      theme(text = element_text(size=15),legend.key.size = unit(1,"line"), plot.subtitle = element_text(size = 12))
    
  }
  if(w+lag+mortalityWindow-1==52)
  {
    g = ggplot(stats) + geom_sf(aes(fill = Moran))+ # ggrepel::geom_text_repel(aes(label = Station_ref_name, geometry = geometry), size = 1.7, stat = "sf_coordinates",max.overlaps = 20) + 
      scale_fill_manual(name = "",values = colors)+ theme_bw() + theme(text = element_text(size=10),legend.key.size = unit(1,"line") )+ rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") +
      labs(title = paste("Local Moran Index of week from", as.Date(paste(2020, w+lag+mortalityWindow-1, 1, sep="-"), "%Y-%U-%u"), "to", as.Date(paste(2021, 1, 3, sep="-"), "%Y-%m-%d")), 
           subtitle = subtitle) +
      theme(text = element_text(size=15),legend.key.size = unit(1,"line"), plot.subtitle = element_text(size = 12))
    
  }
  if(w+lag+mortalityWindow-1>52){
    g = ggplot(stats) + geom_sf(aes(fill = Moran))+ # ggrepel::geom_text_repel(aes(label = Station_ref_name, geometry = geometry), size = 1.7, stat = "sf_coordinates",max.overlaps = 20) + 
      scale_fill_manual(name = "",values = colors)+ theme_bw() + theme(text = element_text(size=10),legend.key.size = unit(1,"line") )+ rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") +
      labs(title = paste("Local Moran Index of week from", as.Date(paste(2021, w+lag+mortalityWindow-1-52, 1, sep="-"), "%Y-%U-%u"), "to", as.Date(paste(2021, as.numeric(w+lag+mortalityWindow-1-52)+1, 7, sep="-"), "%Y-%W-%u")), 
           subtitle = subtitle) +
      theme(text = element_text(size=15),legend.key.size = unit(1,"line"), plot.subtitle = element_text(size = 12))
  }
  return(g)
}

# I implement a function to perform the pipeline of Moran analysis at BreBeMi level on Regione Lombardia data
MORAN_ANALYSIS_BREBEMI_TRENORD <- function(Mob, deaths, geo_df, mob_mode = "OD_counts", ep_mode = "mortality_pop", path, 
                                           cl_eta, mortalityWindow, lag, mobilityWindow, test_mode = "perm", signif = 0.05, p_adjust = T){
  ####
  # mob_mode = "OD_counts" -> uses OD counts
  # ep_mode = "mortality_pop" -> uses total mortality divided by population (of the station's basin), age 70+
  # test_mode = "base" -> use standard Moran tests
  #           = "perm" -> use Moran test based on permutations
  ####
  
  # Prepare dataset to store the results
  # Global Moran
  Global_Moran_df <- NULL
  # Local Moran
  Local_Moran_df <- NULL
  
  # Remove Verona and Peschiera from all quantities and selecting only the relevant age class
  deaths <- deaths |> filter(!(Station_ref_name %in% c("Verona","Peschiera del Garda")) & CL_ETA == cl_eta)
  if (mob_mode == "OD_counts")
    Mob <- Mob |> filter(!(Start %in% c("Verona","Peschiera del Garda")) & !(End %in% c("Verona","Peschiera del Garda")))
  
  # I prepare the vector of weeks, excluding the boundary ones where I have not the complete mobility representation
  # weeks <- c(1:25, 36:51)
  # But I have to select the set of weeks where I can compute the aggregation required by mobilityWindow. These are
  weeks1 <- c(mobilityWindow:25)
  weeks2 <- ((36+mobilityWindow-1):51)
  weeks <- c(weeks1 ,weeks2)
  # BE CAREFUL: I am not handling the case mobilityWindow>15 but it does not make sense to have such a large value
  
  # Iterating       
  for (w in weeks){
    if (mob_mode == "OD_counts"){
      # REM: this function performs ROW SCALING
      #      Weights are given by the OD counts, aggregated into the number of weeks required by mobilityWindow.
      r <- get_weights_OD_Trenord(Mob, w, mobilityWindow, symmetrical = F) 
    }
    
    W <- r$W
    removed_stats <- r$removed_stats
    
    if(ep_mode == "mortality_pop"){
      names <- unique(Mob$Start)[which(!(unique(Mob$Start) %in% removed_stats))]
      # I have to select the correct week: I need to select
      # w
      # + lag -> to consider the lag between the mobility and mortality data
      # + mortalityWindow -> To consider the aggregation into mortalityWindow weeks
      X <- get_epidemic_exmort_BreBeMi(deaths, w+lag+mortalityWindow-1, col = paste0("md_window", mortalityWindow), names)
    }
    
    
    # MORAN ANALYSIS
    if (test_mode == "base"){
      ## Global Moran
      c <- as.data.frame(t(unlist(moran.test(X, W, zero.policy = T))))
      Global_Moran_df <- rbind(Global_Moran_df, cbind(rep(w+lag+mortalityWindow-1, dim(c)[1]), c))
      
      # Local Moran
      c <- as.data.frame(cbind(names,localmoran(X, W, zero.policy = T)))
      Local_Moran_df <- rbind(Local_Moran_df, cbind(rep(w+lag+mortalityWindow-1, dim(c)[1]), c))
    }
    
    if (test_mode == "perm"){
      # Global Moran
      c <- moran.mc(X, W, nsim = 99999, zero.policy = T)
      temp <- data.frame("Week" = w+lag+mortalityWindow-1,
                         "Moran.I" = c$statistic, 
                         "p-value" = c$p.value)
      Global_Moran_df <- rbind(Global_Moran_df, temp)
      
      # Local Moran
      c <- as.data.frame(localmoran_perm(X, W, nsim=99999, zero.policy=T))
      Local_Moran_df <- rbind(Local_Moran_df, cbind(rep(w+lag+mortalityWindow-1, dim(c)[1]), names, c))
    }
    
  }
  
  # Fixing dataframes format
  if (test_mode == "base"){
    Global_Moran_df = as.data.frame(Global_Moran_df)
    colnames(Global_Moran_df)[1:4] <- c("Week", "Statistic", "p.value", "Moran.I")
    colnames(Local_Moran_df)[1:7] <- c("Week", "OD_name", "Local.Moran.I", "E.Ii", "Var.Ii", "z.I1", "p.value")
  }
  if (test_mode == "perm"){
    Global_Moran_df = as.data.frame(Global_Moran_df)
    colnames(Global_Moran_df) <- c("Week", "Moran.I", "p.value")
    
    Local_Moran_df = as.data.frame(Local_Moran_df)
    Local_Moran_df <- Local_Moran_df |> select(1,2,3,4,5,7)
    colnames(Local_Moran_df) <- c("Week", "OD_name", "Local.Moran.I", "E.Ii", "Var.Ii", "p.value")
  }
  
  ## P VALUE ADJUSTMENT
  if(p_adjust == T){
    Global_Moran_df$p.value <- p.adjust(Global_Moran_df$p.value, "BH")
    Local_Moran_df$p.value <- p.adjust(Local_Moran_df$p.value, "BH")
  }
  
  ## PLOTS
  # Prepare plotting
  # pdf(path, onefile = TRUE)
  
  # GLOBAL PLOT
  C <- Global_Moran_df
  C[,1:dim(C)[2]] <- sapply(C[,1:dim(C)[2]], as.numeric)
  
  # Fix the date format
  C$Date <- ifelse(C$Week <= 52,
                   as.Date(paste(2020, C$Week, 1, sep="-"), "%Y-%U-%u"),
                   as.Date(paste(2021, C$Week - 52, 1, sep="-"), "%Y-%U-%u"))
  C$Date[C$Week == 0] <- as.Date("2020-01-01")
  C$Date <- as.Date(C$Date, origin = "1970-01-01")
  
  C <- C |>
    mutate(
      sig = case_when(
        p.value < signif ~ "red",
        p.value >= signif ~ "grey"
      ))
  
  if (cl_eta == "15_21")
    subtitle <- paste("Railway mobility, age class 70+, aggregation of death counts into", mortalityWindow, 
                      "weeks\nlag of", lag, "weeks, aggregation of mobility data into", mobilityWindow, "weeks")
  
  if (cl_eta == "11_14")
    subtitle <- paste("Railway mobility, age class 50-69, aggregation of death counts into", mortalityWindow, 
                      "weeks\nlag of", lag, "weeks, aggregation of mobility data into", mobilityWindow, "weeks")
  
  p <- ggplot(C, aes(Date, Moran.I))+
    geom_point(color = C$sig)+
    geom_line()+
    labs(title = "Global Moran Index", subtitle = subtitle) +
    theme_bw() + theme_bw() + ylab("Moran I") + xlab("Date") +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%B") + theme(axis.text.x = element_text(angle = 50, hjust=0.95), text = element_text(size=15), axis.title.x = element_blank(),
                                             plot.subtitle = element_text(size = 12)) 
  ggsave(paste0(path,"/GM.png"),p)
  # grid.arrange(p)
  
  ## LOCAL PLOTS 
  for (w in weeks){
    if (mob_mode == "OD_counts"){
      # REM: this function performs ROW SCALING
      #      Weights are given by the OD counts
      r <- get_weights_OD_Trenord(Mob, w, mobilityWindow, symmetrical = F) 
    }
    
    W <- r$W
    removed_stats <- r$removed_stats
    
    if(ep_mode == "mortality_pop"){
      names <- unique(Mob$Start)[which(!(unique(Mob$Start) %in% removed_stats))]
      X <- get_epidemic_exmort_BreBeMi(deaths, w+lag+mortalityWindow-1, col = paste0("md_window", mortalityWindow), names)
    }
    
    
    local.p.value <- Local_Moran_df |> dplyr::filter(Week == w+lag+mortalityWindow-1) |> pull(p.value)
    g <- my_plot_Moran_BreBeMi_Trenord(geo_df, X, W, w, lag, mortalityWindow, cl_eta, removed_stats, local.p.value, signif)
    
    # grid.arrange(g) 
    ggsave(paste0(path,"/LM_",w,".png"), g, height = 8, width = 20, units = "cm")
    
  }
  
  # dev.off()
  
  return(list("Global_Moran" = Global_Moran_df, "Local_Moran" = Local_Moran_df))
}