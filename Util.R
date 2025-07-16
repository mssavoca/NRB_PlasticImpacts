# Utilies file

library(tidyverse)
library(readxl)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggExtra)
library(gtable)
library(scales)
library(patchwork)
library(cowplot)
library(ggpubr)
library(grid)  # For text formatting

# Ingestion data----

GLOVE_original <- read_xls("Glove-2025-06-01.xls") 

GLOVE_edited <-  GLOVE_original %>% 
  mutate(
    Reference = str_replace_all(Reference, " and ", " & "), 
    Reference = case_when(
      # in GLOVE ~ in ELM
      #  Reference == "Furness_b, 1985" ~ "Furness, 1985",  # Weird one, need to check,
      Reference == "Wedemeyer Strombel et al., 2015" ~ "Wedemeyer et al., 2015",
      Reference == "Bjorndal et al.,1994" ~ "Bjorndal et al., 1994",
      Reference == "Auman et al., 1997" ~ "Auman et al., 1998",
      Reference == "Barreiros & Barcelos, 2001" ~ "Barreiros  & Barcelos, 2001",
      Reference == "Tom·s et al., 2002" ~ "Tomás et al., 2002",
      Reference == "Brand„o et al., 2011" ~ "Brandão et al, 2011",
      Reference == "Lazar & Gra an, 2011" ~ "Lazar & Gracan, 2011",
      Reference == "K¸hn & van Franeker, 2012" ~ "Kühn & Van Franeker, 2012",
      Reference == "RodrÌguez et al., 2012" ~ "Rodríguez et al., 2012",
      Reference == "Witherington et al., 2012" ~ "Witherington, 2012",
      Reference == "Camedda et al., 2014" ~ "Camedda et al., 2013",
      Reference == "Codina-GarcÌa et al., 2013" ~ "Codina-García et al., 2013",
      Reference == "Stephanis et al., 2013" ~ "de Stephanis et al., 2013",
      Reference == "Di Beneditto & Awabdi, 2014" ~ "Di Beneditto et al., 2013",
      Reference == "Guimar„es et al., 2013" ~ "Guimarães et al. 2013, 2013",
      Reference == "VÈlez-Rubio et al., 2013" ~ "Velez-Rubio, 2013",
      Reference == "Di Beneditto et al., 2014" ~ "Di Beneditto & Ramos, 2014",
      Reference == "Ormedilla et al., 2014" ~ "Ormedilla et al. 2014, 2014",
      Reference == "Poli et al., 2015" ~ "Poli et al., 2014",
      Reference == "De Carvalho, et al., 2015" ~ "de Carvalho et al., 2015",
      Reference == "Gilbert et al., 2016" ~ "GIlbert et al., 2015",
      Reference == "Acampora et al., 2017" ~ "Acampora et al., 2016",
      Reference == "Miranda & Carvalho-Souza, 2016" ~ "de Carvalho-Souza, 2016",
      Reference == "Clukey et al., 2016" ~ "Clukey et al., 2017",
      Reference == "Colferai et al., 2017" ~ "Colferai et al, 2017",
      Reference == "Lenzi et al., 2016" ~ "Lenzi et al., 2017",
      Reference == "Poon et al., 2016" ~ "Poon et al., 2017",
      Reference == "Unger et al., 2016" ~ "Unger et al., 2017",
      Reference == "¡lvarez et al., 2018" ~ "Alvarez et al., 2018",
      Reference == "Godoy & Stockin, 2018" ~ "Godoy and Stockin, 2018",
      Reference == "RodrÌguez et al., 2018" ~ "Rodríguez et al., 2018",
      Reference == "VÈlez-Rubio et al., 2018" ~ "Vélez-Rubio et al., 2018",
      Reference == "Compa et al., 2018" ~ "Compa et al., 2019",
      Reference == "Domenech et al., 2019" ~ "Domènech et al., 2019",
      Reference == "Golubev, 2020" ~ "Golubev et al., 2020",
      Reference == "Hidalgo Ruz et al., 2020" ~ "Hidalgo-Ruz et al., 2020",
      Reference == "IbaÒez et al., 2020" ~ "Ibanez et al., 2020",
      Reference == "Phillips &  Waluda, 2020" ~ "Phillips & Waluda, 2020",
      Reference == "Santill·n et al., 2020" ~ "Santillán et al., 2020",
      Reference == "Vanstreels et al., 2019" ~ "Vanstreels et al., 2020",
      Reference == "Kuhn et al., 2021" ~ "Kühn et al., 2021",
      Reference == "Hidalgo Ruz et al., 2020" ~ "Hidalgo-Ruz et al., 2020",
      Reference == "Hidalgo Ruz et al., 2020" ~ "Hidalgo-Ruz et al., 2020",
      Reference == "Hidalgo Ruz et al., 2020" ~ "Hidalgo-Ruz et al., 2020",
      
      
      TRUE ~ Reference )
  )
  


ELM_ingest_data <- read_xlsx("literature review for Matt_clean.xlsx", sheet = 1) %>% 
  filter(if_any(everything(), ~ !is.na(.))) %>%
  # Remove columns where all values are NA
  select(where(~ !all(is.na(.)))) %>% 
  # Create Decade column
  mutate(
      Year = as.numeric(Year),
      Reference = paste(Authors, Year, sep = ", "),
      Reference = str_replace_all(Reference, " and ", " & "),
      Taxa = case_when(
        str_detect(str_to_lower(Taxa), "reptile") ~ "Reptile and Amphibian",
        TRUE ~ Taxa),
    Taxa = case_when(
      str_detect(str_to_lower(Taxa), "reptile") ~ "Reptile and Amphibian",
      TRUE ~ Taxa
    ),
    Decade = case_when(
      Year >= 1980 & Year < 1990 ~ "1980s",
      Year >= 1990 & Year < 2000 ~ "1990s",
      Year >= 2000 & Year < 2010 ~ "2000s",
      Year >= 2010 & Year < 2020 ~ "2010s",
      Year >= 2020 & Year < 2030 ~ "2020s",
      TRUE ~ NA_character_
    ),
    `Effect measured` = case_when(
      str_to_lower(`Effect measured`) %in% c("exposure", "expsoure") ~ "Exposure",
      str_to_lower(`Effect measured`) %in% c("mortality", "morality", "mortaliy") ~ "Mortality",
      str_to_lower(`Effect measured`) %in% c("chemical contamination") ~ "Chemical contamination",
      str_to_lower(`Effect measured`) %in% c("body condition") ~ "Body condition",
      TRUE ~ `Effect measured`
    ),
    `Effect demonstrated` = case_when(
      str_to_lower(`Effect demonstrated`) %in% c("yes", "Yes") ~ "Yes",
      str_to_lower(`Effect demonstrated`) %in% c("possible") ~ NA_character_,
      TRUE ~ `Effect demonstrated`
    ),
    ) %>% 
  # Remove rows where all values are NA
  filter(if_any(everything(), ~ !is.na(.))) %>%
  
  # Remove columns where all values are NA
  select(where(~ !all(is.na(.)))) %>%
  
  # Remove rows with 'microplastic' or 'microplastics' in the Title (case-insensitive)
  filter(!str_detect(str_to_lower(Title), "microplastics?"))



ELM_summary_df <- ELM_ingest_data %>%
  # First, remove unwanted rows BEFORE creating the Reference column
  filter(
    !str_detect(str_to_lower(Title), "microplastics?"),  # remove if title contains "microplastic" or "microplastics"
    !(Authors == "Marn et al." & Year == 2020), # Modeling study
    !(Authors == "Pfaller et al." & Year == 2020), # Lab study
    !(Authors == "Good et al." & Year == 2020)  # Modeling study
  ) %>%
  mutate(
    Year = as.numeric(Year),
    Reference = paste(Authors, Year, sep = ", "),
    Reference = str_replace_all(Reference, " and ", " & "),
    Taxa = case_when(
      str_detect(str_to_lower(Taxa), "reptile") ~ "Reptile and Amphibian",
      TRUE ~ Taxa
    )
  ) %>%
  filter(
    !is.na(Decade),
    !is.na(Taxa),
    !is.na(`Effect measured`),
    !is.na(Reference)
  ) %>%
  distinct(Reference, `Effect measured`, Taxa, Decade, .keep_all = TRUE)


#write_csv(ELM_summary_df, "ELM_summary_df.csv")

ELM_summary_df_updated_LatLon <- read_xlsx("ELM_summary_df_LatLons_needed.xlsx") %>% 
  mutate(
    # Convert "NA" strings to actual NA, then force to numeric
    Lat = case_when(
      Lat == "NA" | is.na(Lat) ~ NA_real_,
      TRUE ~ as.numeric(as.character(Lat))
    ),
    Lon = case_when(
      Lon == "NA" | is.na(Lon) ~ NA_real_,
      TRUE ~ as.numeric(as.character(Lon))
    )
  )


ELM_ingest_summary_df <- ELM_summary_df %>% 
  bind_rows(ELM_summary_df_updated_LatLon) 


GLOVE_summary <- GLOVE_edited %>%
  # mutate(case_when(
  #   Reference == "van Franeker & Bell, 1988" ~ "Van Franeker & Bell, 1988",
  #   TRUE ~ Reference)
  # ) %>% 
  select(Reference, Lat, Lon) %>%
  distinct(Reference, .keep_all = TRUE)




# Final joined df for ingestion----
ELM_summary_GLOVE_joined <- ELM_ingest_summary_df %>%
  left_join(GLOVE_summary, by = "Reference") %>%
  mutate(
    Lat = coalesce(as.numeric(Lat.x), as.numeric(Lat.y)),
    Lon = coalesce(as.numeric(Lon.x), as.numeric(Lon.y)),
    Effect_general = case_when(
      `Effect measured` == "Exposure" ~ "Exposure only",
      `Effect measured` %in% c("Body condition", "Chemical contamination", "Food consumption", "Illness", "Injury") ~ "Sublethal",
      `Effect measured` %in% c("Mortality", "Population decline") ~ "Lethal",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-Lat.x, -Lat.y, -Lon.x, -Lon.y)





#show which References in ELM_summary_df did not have a match in GLOVE
# Step 1: Join GLOVE_summary first
ELM_with_coords <- ELM_ingest_summary_df %>%
  left_join(GLOVE_summary, by = "Reference")

# Step 2: Filter unmatched references
unmatched_refs_in_ELM <- ELM_with_coords %>%
  filter(is.na(Lat) | is.na(Lon)) %>%
  distinct(Reference, Year) %>% 
  arrange(Year)

# View or print
#View(unmatched_refs_in_ELM)




# Entanglement data----

ELM_entangle_data <- read_xlsx("literature review for Matt_clean.xlsx", sheet = 2) %>% 
  filter(if_any(everything(), ~ !is.na(.))) %>%
  # Remove columns where all values are NA
  select(where(~ !all(is.na(.)))) %>% 
  # Create Decade column
  mutate(
    Year = as.numeric(Year),
    Reference = paste(Authors, Year, sep = ", "),
    Reference = str_replace_all(Reference, " and ", " & "),
    Taxa = case_when(
      str_detect(str_to_lower(Taxa), "reptile") ~ "Reptile and Amphibian",
      TRUE ~ Taxa),
    Taxa = case_when(
      str_detect(str_to_lower(Taxa), "reptile") ~ "Reptile and Amphibian",
      TRUE ~ Taxa
    ),
    Decade = case_when(
      Year >= 1980 & Year < 1990 ~ "1980s",
      Year >= 1990 & Year < 2000 ~ "1990s",
      Year >= 2000 & Year < 2010 ~ "2000s",
      Year >= 2010 & Year < 2020 ~ "2010s",
      Year >= 2020 & Year < 2030 ~ "2020s",
      TRUE ~ NA_character_
    ),
    Lat = as.numeric(Lat),
    Lon = as.numeric(Lon),
    Effect_general = case_when(
      `Effect measured` == "Exposure" ~ "Exposure only",
      `Effect measured` %in% c("Body condition", "Crawl time", "Injury", 
                               "Nesting deterrent",  "Disease", "Body condition", "Mobility",
                               "Reaching the sea", "Community shift", "Speed", "Crawl obstruction", "Nest distribution") ~ "Sublethal",
      `Effect measured` %in% c("Mortality", "Population decline") ~ "Lethal",
      TRUE ~ NA_character_
    )
  )



ELM_entangle_summary_df <- ELM_entangle_data %>%
  filter(!is.na(Reference), !is.na(Taxa), !is.na(Effect_general)) %>%
  distinct(Reference, Year, Decade, Taxa, Effect_general, Lat, Lon) %>%
  separate_rows(Taxa, sep = ";\\s*") %>%             # Split multiple taxa
  separate_rows(Effect_general, sep = ";\\s*") %>%   # Split multiple effect types
  group_by(Reference, Year, Decade, Taxa, Effect_general) %>%
  summarise(
    Lat = mean(Lat, na.rm = TRUE),
    Lon = mean(Lon, na.rm = TRUE),
    .groups = "drop"
  )

