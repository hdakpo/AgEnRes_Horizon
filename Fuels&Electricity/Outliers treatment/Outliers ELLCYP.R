# ============================
# 🚀 TRAITEMENT DES OUTLIERS – ELLCYP SEUL
# ============================
.libPaths("D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Fuels/input/library")

library(dplyr)
library(dbscan)
library(wbacon)
library(tidyr)
library(ggplot2)
library(readxl)   
library(zoo) 

options(bitmapType = "cairo")

# Dossiers de sortie graphes
plot_dir <- "D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Fuels/output/Outliers"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
pdf_dir <- file.path(plot_dir, "pdf")
if (!dir.exists(pdf_dir)) dir.create(pdf_dir, recursive = TRUE)

# ----------------------------
# Paramètres spécifiques ELLCYP
# ----------------------------
code <- "ELLCYP"

input_file  <- "D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Fuels/input/Data pour outliers treat/prepared_objects_FUELS_GC_ELLCYP.RDS"
out_dir <- "D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Fuels/input/Data_for_allocation"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

output_file <- file.path(out_dir, "prepared_list_FUELS_GC_ELLCYP.RDS")

# Sécurité : s’assurer qu’aucun dossier ne porte ce nom
if (dir.exists(output_file)) {
  stop("Un Dossier existe au chemin du fichier attendu : ", output_file,
       "\nSupprime ce dossier (unlink(...)) ou choisis un autre nom de fichier.")
}


cat("\n------ Traitement du fichier :", code, "------\n")

if (!file.exists(input_file)) {
  stop("⚠️ Fichier non trouvé : ", input_file)
}
# ----------------------------
# 🔢 Indice de prix (base 2015 = 100) à lire une fois
# ----------------------------
idx_path <- "D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Fuels/input/Indice des prix des moyens de production agricole, input (2015 = 100) - données annuelles.xlsx"
defl_idx <- read_xlsx(idx_path) |>
  transmute(
    COUNTRY = as.character(COUNTRY),
    YEAR    = as.integer(YEAR),
    PRICE   = as.numeric(Price),  # colonne 'Price' exactement
    DEF     = PRICE/100           # déflateur multiplicatif
  ) |>
  filter(!is.na(YEAR), !is.na(DEF), DEF > 0)


# ----------------------------
# Lecture et préparation
# ----------------------------
my_list   <- readRDS(input_file)
MYDATA_GC <- my_list$MYDATA_GC

# Sécu types + présence COUNTRY
if (!"COUNTRY" %in% names(MYDATA_GC)) {
  stop("La colonne COUNTRY est absente de MYDATA_GC (ELLCYP regroupe probablement ELL et CYP).")
}
MYDATA_GC <- MYDATA_GC %>%
  mutate(
    COUNTRY = as.character(COUNTRY),
    YEAR    = as.integer(YEAR),
    SAUC    = as.numeric(SAUC),
    IFULS_V   = as.numeric(IFULS_V)   # # coût carburants
  )

years_all <- 2017:2022

# table propre "une ligne par (COUNTRY, YEAR)"
defl_idx_yearly <- defl_idx %>%
  dplyr::distinct(COUNTRY, YEAR, DEF, PRICE) %>%
  dplyr::filter(YEAR %in% years_all)

# contrôle : est-ce qu'il manque des (pays, année) ?
needed <- tidyr::expand_grid(COUNTRY = unique(MYDATA_GC$COUNTRY), YEAR = years_all)
missing_pairs <- dplyr::anti_join(needed, defl_idx_yearly, by = c("COUNTRY","YEAR"))
if (nrow(missing_pairs) > 0) {
  stop("Il manque des indices pour :\n", capture.output(print(missing_pairs)) %>% paste(collapse="\n"))
}

# jointure simple, sans interpolation
MYDATA_GC <- MYDATA_GC %>%
  dplyr::left_join(defl_idx_yearly, by = c("COUNTRY","YEAR")) %>%
  dplyr::mutate(
    IFULS_V_const2015 = dplyr::if_else(!is.na(DEF) & DEF > 0, IFULS_V/DEF, IFULS_V),
    FUELS_M_HA        = dplyr::if_else(SAUC > 0, IFULS_V_const2015/SAUC, NA_real_)
  )


# ---- Déflation en €2015 constants ----
# IFULS_V_const2015 = IFULS_V / DEF (si DEF manquant, on garde IFULS_V)
# FUELS_M_HA = IFULS_V_const2015 / SAUC (si SAUC<=0 -> NA)
MYDATA_GC <- MYDATA_GC %>%
  mutate(
    IFULS_V_const2015 = if_else(!is.na(DEF) & DEF > 0, IFULS_V/DEF, IFULS_V),
    FUELS_M_HA       = if_else(SAUC > 0, IFULS_V_const2015/SAUC, NA_real_)
  )

# ----------------------------
# Données pour outliers
# ----------------------------
out_df <- MYDATA_GC %>%
  select(ID, YEAR, FUELS_M_HA, SAUC) %>%
  filter(SAUC > 0) %>%
  drop_na() %>%
  filter(is.finite(FUELS_M_HA), is.finite(SAUC))

# Besoin d'au moins quelques points
if (nrow(out_df) < 10) {
  stop("⚠️ Trop peu d'observations non manquantes pour détecter les outliers (n < 10).")
}

# ----------------------------
# 1) wBACON
# ----------------------------
# X <- as.matrix(out_df[, c("FUELS_M_HA", "SAUC")])
X_std <- scale(as.matrix(out_df[, c("FUELS_M_HA","SAUC")]))
res_bacon <- tryCatch(wbacon::wBACON(x = X, alpha = 0.001), error = function(e) NULL)
if (!is.null(res_bacon) && !is.null(res_bacon$subset)) {
  ss <- res_bacon$subset
  inliers <- if (is.logical(ss)) ss else seq_len(nrow(X)) %in% ss
  out_df$Outlier_wBACON <- !inliers
} else {
  out_df$Outlier_wBACON <- FALSE
}

#sécurité pour wbacon
prop_bacon <- mean(out_df$Outlier_wBACON)
if (!is.na(prop_bacon) && prop_bacon > 0.9) {
  warning("wBACON a marqué >90% des points comme outliers ; on l’ignore pour ce run.")
  out_df$Outlier_wBACON <- FALSE
}

# ----------------------------
# 2) DBSCAN (sur données standardisées)
# ----------------------------
X_scaled <- scale(X)
res_db <- tryCatch(
  dbscan(X_scaled, eps = 0.5, minPts = 5),
  error = function(e) NULL
)
out_df$Outlier_DBSCAN <- if (!is.null(res_db) && !is.null(res_db$cluster)) {
  res_db$cluster == 0
} else {
  FALSE
}

# ----------------------------
# 3) IQR univarié (sur FUELS_M_HA et SAUC)
# ----------------------------
Q1 <- apply(X, 2, quantile, 0.25, na.rm = TRUE)
Q3 <- apply(X, 2, quantile, 0.75, na.rm = TRUE)
IQR_vals <- Q3 - Q1

out_df$Outlier_IQR <- with(out_df,
                           (FUELS_M_HA < (Q1["FUELS_M_HA"] - 1.5 * IQR_vals["FUELS_M_HA"])) |
                             (FUELS_M_HA > (Q3["FUELS_M_HA"] + 1.5 * IQR_vals["FUELS_M_HA"])) |
                             (SAUC      < (Q1["SAUC"]       - 1.5 * IQR_vals["SAUC"])) |
                             (SAUC      > (Q3["SAUC"]       + 1.5 * IQR_vals["SAUC"]))
)

# ----------------------------
# Fusion des méthodes
# ----------------------------
out_df$Outlier_Combo <- dplyr::case_when(
  out_df$Outlier_wBACON & out_df$Outlier_DBSCAN & out_df$Outlier_IQR ~ "3 méthodes",
  out_df$Outlier_wBACON & out_df$Outlier_DBSCAN ~ "wBACON + DBSCAN",
  out_df$Outlier_wBACON & out_df$Outlier_IQR ~ "wBACON + IQR",
  out_df$Outlier_DBSCAN & out_df$Outlier_IQR ~ "DBSCAN + IQR",
  out_df$Outlier_wBACON ~ "wBACON seul",
  out_df$Outlier_DBSCAN ~ "DBSCAN seul",
  out_df$Outlier_IQR ~ "IQR seul",
  TRUE ~ "Aucun"
)

# Seuils utiles
seuil_FUELS_sup <- quantile(out_df$FUELS_M_HA, 0.99, na.rm = TRUE)
seuil_sauc_min <- quantile(out_df$SAUC, 0.01, na.rm = TRUE)
cat("📉 Seuil 99e FUELS_M_HA:", round(seuil_FUELS_sup, 1),
    "| Seuil 1e SAUC:", round(seuil_sauc_min, 3), "\n")

# ----------------------------
# PDF : visualisations
# ----------------------------
pdf(file.path(pdf_dir, paste0("outliers_viz_", code, ".pdf")), width = 7, height = 5)

print(ggplot(out_df, aes(x = FUELS_M_HA, y = SAUC, color = Outlier_wBACON)) +
        geom_point(alpha = 0.6) +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "darkgreen")) +
        labs(title = paste0("Détection par wBACON – ", code)) +
        theme_minimal())

print(ggplot(out_df, aes(x = FUELS_M_HA, y = SAUC, color = Outlier_DBSCAN)) +
        geom_point(alpha = 0.6) +
        scale_color_manual(values = c("TRUE" = "orange", "FALSE" = "blue")) +
        labs(title = paste0("Détection par DBSCAN – ", code)) +
        theme_minimal())

print(ggplot(out_df, aes(x = FUELS_M_HA, y = SAUC, color = Outlier_IQR)) +
        geom_point(alpha = 0.6) +
        scale_color_manual(values = c("TRUE" = "purple", "FALSE" = "grey")) +
        labs(title = paste0("Détection par IQR – ", code)) +
        theme_minimal())

print(ggplot(out_df, aes(x = FUELS_M_HA, y = SAUC, color = Outlier_Combo)) +
        geom_point(alpha = 0.7, size = 1.6) +
        labs(title = paste0("Fusion des détections d'outliers (3 méthodes) – ", code)) +
        theme_minimal())

print(out_df %>%
        dplyr::count(Outlier_Combo, name = "n") %>%
        ggplot(aes(x = reorder(Outlier_Combo, -n), y = n, fill = Outlier_Combo)) +
        geom_bar(stat = "identity") +
        labs(title = "Nombre d'outliers par type", x = "Méthode", y = "N") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 25, hjust = 1), legend.position = "none"))

print(ggplot(out_df, aes(x = Outlier_Combo, y = FUELS_M_HA, fill = Outlier_Combo)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = "Distribution de FUELS_M_HA par type d'outlier") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 20)))

print(ggplot(out_df, aes(x = Outlier_Combo, y = SAUC, fill = Outlier_Combo)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = "Distribution de SAUC par type d'outlier") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 20)))

# ----------------------------
# Règles d'action & nettoyage
# ----------------------------
out_df <- out_df %>%
  group_by(ID) %>% mutate(nb_years = n()) %>% ungroup() %>%
  mutate(Action = case_when(
    Outlier_Combo == "3 méthodes" ~ "Supprimer",
    Outlier_Combo %in% c("wBACON + DBSCAN", "DBSCAN + IQR") ~ "Supprimer",
    Outlier_Combo %in% c("wBACON + IQR", "wBACON seul") & FUELS_M_HA > seuil_FUELS_sup ~ "Corriger",
    Outlier_Combo == "IQR seul" & SAUC < seuil_sauc_min & nb_years < 3 ~ "Supprimer",
    Outlier_Combo == "IQR seul" & SAUC < seuil_sauc_min ~ "Corriger",
    TRUE ~ "Ignorer"
  ))

df_cleaned <- out_df %>%
  mutate(FUELS_M_HA_cleaned = case_when(
    Action == "Corriger" & FUELS_M_HA < 5 ~ 5,
    Action == "Corriger" & FUELS_M_HA > seuil_FUELS_sup ~ seuil_FUELS_sup,
    TRUE ~ FUELS_M_HA
  )) %>%
  filter(Action != "Supprimer")

print(df_cleaned %>%
        ggplot(aes(x = FUELS_M_HA, y = SAUC, color = Action)) +
        geom_point(alpha = 0.7) +
        labs(title = "Nuage de points APRÈS nettoyage", x = "FUELS_M_HA", y = "SAUC") +
        theme_minimal())

dev.off()

# ----------------------------
# Reconstruction de l'objet pour l'allocation
# ----------------------------
MYDATA_GC_cleaned <- MYDATA_GC %>%
  semi_join(df_cleaned, by = c("ID", "YEAR")) %>%
  left_join(df_cleaned %>% dplyr::select(ID, YEAR, FUELS_M_HA_cleaned), by = c("ID", "YEAR")) %>%
  mutate(FUELS_M_HA = FUELS_M_HA_cleaned) %>%
  dplyr::select(-FUELS_M_HA_cleaned)

mylist_for_model <- list(
  MYDATA_GC     = MYDATA_GC_cleaned,
  cname_alloc_A  = my_list$cname_alloc_A,
  cname_alloc_SH = my_list$cname_alloc_SH,
  cname_alloc_gc = my_list$cname_alloc_gc
)

saveRDS(mylist_for_model, file = output_file)
cat("✅ Sauvegarde OK pour", code, "→", output_file, "\n")
