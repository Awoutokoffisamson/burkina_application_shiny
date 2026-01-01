################################################################################
# GLOBAL.R - Préparation des données pour l'application Shiny
# Burkina Faso - Nouvelles Subdivisions 2025
################################################################################

library(sf)
library(dplyr)
library(readr)
library(stringi)

# Chemins des fichiers
base_path <- "."
shp_path <- file.path(base_path, "data/processed/BFA_subdivision_2025")
pop_path <- file.path(base_path, "data/raw/population_citypop.csv")
corr_path <- file.path(base_path, "data/raw/table_correspondance_communes.csv")

# Superficie officielle du Burkina Faso
SUPERFICIE_OFFICIELLE_KM2 <- 274220

# Fonction de nettoyage des noms
clean_names <- function(x) {
    x <- stri_trans_general(x, "Latin-ASCII")
    x <- tolower(x)
    x <- gsub("-", " ", x)
    x <- gsub("'", "", x)
    x <- trimws(x)
    return(x)
}

# 1. Charger les shapefiles
cat("Chargement des shapefiles...\n")

# Communes avec géométrie
communes_shp <- st_read(file.path(shp_path, "BFA_niveau3_communes_2025.shp"), quiet = TRUE)

# Transformer en projection métrique pour calcul de superficie
communes_shp_proj <- st_transform(communes_shp, 32630) # UTM zone 30N
superficie_calculee <- sum(as.numeric(st_area(communes_shp_proj))) / 10^6

# Facteur de correction pour la superficie
facteur_correction <- SUPERFICIE_OFFICIELLE_KM2 / superficie_calculee
communes_shp$Superficie_km2 <- as.numeric(st_area(communes_shp_proj)) / 10^6 * facteur_correction

# Transformer en WGS84 pour Leaflet
communes_shp <- st_transform(communes_shp, 4326)

# 2. Charger la population
cat("Chargement des données de population...\n")
pop_data <- read_delim(pop_path, delim = ";", show_col_types = FALSE)

# Corrections orthographiques
corrections <- c(
    "karangasso sambla" = "karankasso sambla",
    "karangasso vigue" = "karankasso vigue",
    "imasgo" = "imasgho",
    "kokologo" = "kokologho",
    "coalla" = "koalla",
    "manni" = "mani",
    "arbinda" = "aribinda",
    "gueguere" = "gueguere"
)

pop_data <- pop_data %>%
    mutate(
        commune_clean = clean_names(Commune),
        province_clean = clean_names(Province)
    )

pop_data$commune_clean <- ifelse(
    pop_data$commune_clean %in% names(corrections),
    corrections[pop_data$commune_clean],
    pop_data$commune_clean
)

pop_data$commune_clean[pop_data$commune_clean == "samoghohiri"] <- "samogohiri"

# Correction de province: Guéguéré a changé de Ioba à Bougouriba
pop_data$province_clean[pop_data$commune_clean == "gueguere" & pop_data$province_clean == "ioba"] <- "bougouriba"

# 3. Charger la correspondance
cat("Chargement de la table de correspondance...\n")
correspondance <- read_csv(corr_path, show_col_types = FALSE)
correspondance <- correspondance %>%
    mutate(
        commune_clean = clean_names(NAME_3),
        province_old_clean = clean_names(NAME_2),
        province_new = nouvelle_province,
        province_new_clean = clean_names(nouvelle_province),
        region_new = nouvelle_region
    )

# 4. Fusion
cat("Fusion des données...\n")

# Joindre pop avec correspondance (par commune ET ancienne province)
pop_with_new <- pop_data %>%
    left_join(
        correspondance %>% select(commune_clean, province_old_clean, province_new, province_new_clean, region_new),
        by = c("commune_clean", "province_clean" = "province_old_clean")
    )

# Préparer les données du shapefile
communes_shp <- communes_shp %>%
    mutate(
        commune_clean = clean_names(NAME_3),
        province_new = nvll_pr,
        province_new_clean = clean_names(nvll_pr),
        region_new = nvll_rg
    )

# Joindre avec la population en utilisant commune ET nouvelle province
communes_data <- communes_shp %>%
    left_join(
        pop_with_new %>% select(commune_clean, province_new_clean, Population_2019),
        by = c("commune_clean", "province_new_clean")
    )

# Correction manuelle pour les communes Komondjari (province non trouvée dans la correspondance)
# Ces données viennent du RGPH 2019
komondjari_pop <- data.frame(
    commune_clean = c("bartiebougou", "foutouri", "gayeri"),
    Population_2019_fix = c(16934, 12452, 76218)
)

communes_data <- communes_data %>%
    left_join(komondjari_pop, by = "commune_clean") %>%
    mutate(Population_2019 = ifelse(is.na(Population_2019), Population_2019_fix, Population_2019)) %>%
    select(-Population_2019_fix)

# Calculer la densité
communes_data <- communes_data %>%
    mutate(
        Densite = round(Population_2019 / Superficie_km2, 1),
        Superficie_km2 = round(Superficie_km2, 2)
    )

# 5. Agréger par province et région
cat("Agrégation par province et région...\n")

# Provinces
provinces_data <- communes_data %>%
    group_by(province_new, region_new) %>%
    summarise(
        nb_communes = n(),
        Population_2019 = sum(Population_2019, na.rm = TRUE),
        Superficie_km2 = sum(Superficie_km2, na.rm = TRUE),
        geometry = st_union(geometry),
        .groups = "drop"
    ) %>%
    mutate(Densite = round(Population_2019 / Superficie_km2, 1))

# Régions
regions_data <- provinces_data %>%
    group_by(region_new) %>%
    summarise(
        nb_provinces = n(),
        nb_communes = sum(nb_communes),
        Population_2019 = sum(Population_2019, na.rm = TRUE),
        Superficie_km2 = sum(Superficie_km2, na.rm = TRUE),
        geometry = st_union(geometry),
        .groups = "drop"
    ) %>%
    mutate(Densite = round(Population_2019 / Superficie_km2, 1))

# 6. Résumé national
burkina_summary <- data.frame(
    Indicateur = c("Régions", "Provinces", "Communes", "Population (2019)", "Superficie (km²)"),
    Valeur = c(
        nrow(regions_data),
        nrow(provinces_data),
        nrow(communes_data),
        format(sum(communes_data$Population_2019, na.rm = TRUE), big.mark = " "),
        format(round(sum(communes_data$Superficie_km2, na.rm = TRUE)), big.mark = " ")
    )
)

cat("✓ Données préparées avec succès!\n")
cat("  - Régions:", nrow(regions_data), "\n")
cat("  - Provinces:", nrow(provinces_data), "\n")
cat("  - Communes:", nrow(communes_data), "\n")
cat("  - Superficie totale:", round(sum(communes_data$Superficie_km2, na.rm = TRUE)), "km²\n")
