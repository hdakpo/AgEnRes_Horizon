# ============================
# 🔁 TRAITEMENT DES OUTLIERS EN BOUCLE POUR TOUS LES PAYS
# ============================

library(dplyr)
library(dbscan)
library(wbacon)
library(tidyr)
library(ggplot2)

# For compatibility with headless environments
options(bitmapType = "cairo")

# Créer dossier pour sauvegarde des graphes
plot_dir <- "../output/Outliers"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

# Nouveau : dossier PDF
pdf_dir <- file.path(plot_dir, "pdf")
if (!dir.exists(pdf_dir)) dir.create(pdf_dir, recursive = TRUE)

eu_countries <- c("BEL", "BGR", "CYP", "CZE", "DAN", "DEU", "ELL", "ESP", "EST", 
                  "FRA", "HRV", "HUN", "IRE", "ITA", "LTU", "LUX", "LVA", "MLT", 
                  "NED", "OST", "POL", "POR", "ROU", "SUO", "SVE", "SVK", "SVN", "UKI")

library(readxl)
library(zoo)   # pour na.approx si jamais une année manque

# ⚠️ Chemin vers ton fichier Excel
idx_path <- "D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Fuels/input/Indice des prix des moyens de production agricole, input (2015 = 100) - données annuelles.xlsx"

# Lecture + nettoyage minimal
defl_idx <- read_xlsx(idx_path) |>
  dplyr::transmute(
    COUNTRY = as.character(COUNTRY),
    YEAR    = as.integer(YEAR),
    PRICE   = as.numeric(Price),     # indice (base 2015=100)
    DEF     = PRICE/100              # déflateur multiplicatif
  ) |>
  dplyr::filter(!is.na(YEAR), !is.na(DEF), DEF > 0)


for (code in eu_countries) {
  cat("\n------ Traitement du pays :", code, "------\n")
  
  input_file <- paste0("D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Fuels/input/Data pour outliers treat/prepared_objects_FUELS_GC_", code, ".RDS")
  output_file <- paste0("D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Fuels/input/Data_for_allocation/prepared_list_FUELS_M_GC_", code, ".RDS")
  
  if (!file.exists(input_file)) {
    cat("\u26a0\ufe0f Fichier non trouvé pour :", code, "\n")
    next
  }
  
  my_list <- readRDS(input_file)
  MYDATA_GC <- my_list$MYDATA_GC
  
  # S'assurer que COUNTRY et YEAR existent et sont typés correctement
  if (!"COUNTRY" %in% names(MYDATA_GC)) {
    # Si tes fichiers .RDS sont “un pays par fichier”, on peut injecter le code pays de la boucle
    MYDATA_GC$COUNTRY <- code
  }
  MYDATA_GC <- MYDATA_GC |>
    dplyr::mutate(
      COUNTRY = as.character(COUNTRY),
      YEAR    = as.integer(YEAR)
    )
  
  # Join avec l’indice (base 2015 = 100)
  MYDATA_GC <- MYDATA_GC |>
    dplyr::left_join(defl_idx |> dplyr::select(COUNTRY, YEAR, DEF, PRICE),
                     by = c("COUNTRY","YEAR"))
  
  # Option : si certaines années manquent, interpolation par pays (2017–2022)
  MYDATA_GC <- MYDATA_GC |>
    dplyr::group_by(COUNTRY) |>
    dplyr::arrange(YEAR, .by_group = TRUE) |>
    dplyr::mutate(DEF = zoo::na.approx(DEF, x = YEAR, na.rm = FALSE, rule = 2)) |>
    dplyr::ungroup()
  
  # ⚠️ Sécurité : si DEF encore manquant → on ne déflate pas, mais on te le signale
  n_na_def <- sum(is.na(MYDATA_GC$DEF))
  if (n_na_def > 0) {
    warning(sprintf("[%s] %d observations sans déflateur (DEF). Elles resteront en prix courants.",
                    code, n_na_def))
  }
  
  # ---- Déflation en €2015 constants ----
  # 1) Déflater le coût carburants moteur (euros courants → constants 2015)
  MYDATA_GC <- MYDATA_GC |>
    dplyr::mutate(
      IFULS_V_const2015 = dplyr::if_else(!is.na(DEF) & DEF > 0, IFULS_V/DEF, IFULS_V),
      FUELS_M_HA        = dplyr::if_else(SAUC > 0, IFULS_V_const2015/SAUC, NA_real_)  # <- évite Inf
    )
  
  out_df <- MYDATA_GC %>%
    dplyr::filter(SAUC > 0) %>%                      # <- enlève SAUC == 0
    dplyr::select(ID, YEAR, FUELS_M_HA, SAUC) %>%
    tidyr::drop_na() %>%
    dplyr::filter(is.finite(FUELS_M_HA), is.finite(SAUC))
  
  X <- as.matrix(out_df[, c("FUELS_M_HA", "SAUC")])
  res_bacon <- wBACON(x = X, alpha = 0.05)
  out_df$Outlier_wBACON <- is_outlier(res_bacon)
  
  X_scaled <- scale(X)
  res_db <- dbscan(X_scaled, eps = 0.5, minPts = 5)
  out_df$Outlier_DBSCAN <- res_db$cluster == 0
  
  Q1 <- apply(X, 2, quantile, 0.25)
  Q3 <- apply(X, 2, quantile, 0.75)
  IQR_vals <- Q3 - Q1
  out_df$Outlier_IQR <- with(out_df,
                             (FUELS_M_HA < (Q1["FUELS_M_HA"] - 1.5 * IQR_vals["FUELS_M_HA"])) |
                               (FUELS_M_HA > (Q3["FUELS_M_HA"] + 1.5 * IQR_vals["FUELS_M_HA"])) |
                               (SAUC < (Q1["SAUC"] - 1.5 * IQR_vals["SAUC"])) |
                               (SAUC > (Q3["SAUC"] + 1.5 * IQR_vals["SAUC"]))
  )
  
  out_df$Outlier_Combo <- case_when(
    out_df$Outlier_wBACON & out_df$Outlier_DBSCAN & out_df$Outlier_IQR ~ "3 méthodes",
    out_df$Outlier_wBACON & out_df$Outlier_DBSCAN ~ "wBACON + DBSCAN",
    out_df$Outlier_wBACON & out_df$Outlier_IQR ~ "wBACON + IQR",
    out_df$Outlier_DBSCAN & out_df$Outlier_IQR ~ "DBSCAN + IQR",
    out_df$Outlier_wBACON ~ "wBACON seul",
    out_df$Outlier_DBSCAN ~ "DBSCAN seul",
    out_df$Outlier_IQR ~ "IQR seul",
    TRUE ~ "Aucun"
  )
  
  seuil_FUELS_M_sup <- quantile(out_df$FUELS_M_HA, 0.99, na.rm = TRUE)
  seuil_sauc_min <- quantile(out_df$SAUC, 0.01, na.rm = TRUE)
  cat("\ud83d\udcc9 Seuil 99e FUELS_M_HA:", round(seuil_FUELS_M_sup, 1), " | Seuil 1e SAUC:", round(seuil_sauc_min, 3), "\n")
  
  pdf(file.path(pdf_dir, paste0("outliers_viz_", code, ".pdf")), width = 7, height = 5)
  
  print(ggplot(out_df, aes(x = FUELS_M_HA, y = SAUC, color = Outlier_wBACON)) +
          geom_point(alpha = 0.6) +
          scale_color_manual(values = c("TRUE" = "red", "FALSE" = "darkgreen")) +
          labs(title = "Détection par wBACON") +
          theme_minimal())
  
  print(ggplot(out_df, aes(x = FUELS_M_HA, y = SAUC, color = Outlier_DBSCAN)) +
          geom_point(alpha = 0.6) +
          scale_color_manual(values = c("TRUE" = "orange", "FALSE" = "blue")) +
          labs(title = "Détection par DBSCAN") +
          theme_minimal())
  
  print(ggplot(out_df, aes(x = FUELS_M_HA, y = SAUC, color = Outlier_IQR)) +
          geom_point(alpha = 0.6) +
          scale_color_manual(values = c("TRUE" = "purple", "FALSE" = "grey")) +
          labs(title = "Détection par IQR") +
          theme_minimal())
  
  print(ggplot(out_df, aes(x = FUELS_M_HA, y = SAUC, color = Outlier_Combo)) +
          geom_point(alpha = 0.7, size = 1.6) +
          labs(title = "Fusion des détections d'outliers (3 méthodes)") +
          theme_minimal())
  
  print(out_df %>%
          dplyr::ungroup() %>%
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
  
  out_df <- out_df %>%
    group_by(ID) %>%
    mutate(nb_years = n()) %>%
    ungroup() %>%
    mutate(Action = case_when(
      Outlier_Combo == "3 méthodes" ~ "Supprimer",
      Outlier_Combo %in% c("wBACON + DBSCAN", "DBSCAN + IQR") ~ "Supprimer",
      Outlier_Combo %in% c("wBACON + IQR", "wBACON seul") & FUELS_M_HA > seuil_FUELS_M_sup ~ "Corriger",
      Outlier_Combo == "IQR seul" & SAUC < seuil_sauc_min & nb_years < 3 ~ "Supprimer",
      Outlier_Combo == "IQR seul" & SAUC < seuil_sauc_min ~ "Corriger",
      TRUE ~ "Ignorer"
    ))
  
  df_cleaned <- out_df %>%
    mutate(FUELS_M_HA_cleaned = case_when(
      Action == "Corriger" & FUELS_M_HA < 5 ~ 5,
      Action == "Corriger" & FUELS_M_HA > seuil_FUELS_M_sup ~ seuil_FUELS_M_sup,
      TRUE ~ FUELS_M_HA
    )) %>%
    filter(Action != "Supprimer")
  
  print(df_cleaned %>%
          ggplot(aes(x = FUELS_M_HA, y = SAUC, color = Action)) +
          geom_point(alpha = 0.7) +
          labs(title = "Nuage de points APRÈS nettoyage", x = "FUELS_M_HA", y = "SAUC") +
          theme_minimal())
  
  dev.off()
  
  MYDATA_GC_cleaned <- MYDATA_GC %>%
    semi_join(df_cleaned, by = c("ID", "YEAR")) %>%
    left_join(df_cleaned %>% dplyr::select(ID, YEAR, FUELS_M_HA_cleaned), by = c("ID", "YEAR")) %>%
    mutate(FUELS_M_HA = FUELS_M_HA_cleaned) %>%
    dplyr::select(-FUELS_M_HA_cleaned)
  
  mylist_for_model <- list(
    MYDATA_GC = MYDATA_GC_cleaned,
    cname_alloc_A = my_list$cname_alloc_A,
    cname_alloc_SH = my_list$cname_alloc_SH,
    cname_alloc_gc = my_list$cname_alloc_gc
  )
  
  saveRDS(mylist_for_model, file = output_file)
  cat("\u2705 Sauvegarde OK pour", code, "→", output_file, "\n")
}
