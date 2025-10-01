# ============================
# 🔁 TRAITEMENT DES OUTLIERS EN BOUCLE POUR TOUS LES PAYS
# ============================
.libPaths("D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Test cluster/input/library")

library(dplyr)
library(dbscan)
library(wbacon)
library(tidyr)
library(ggplot2)

# For compatibility with headless environments
options(bitmapType = "cairo")

# Créer dossier pour sauvegarde des graphes
plot_dir <- "D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Test cluster/output/Outliers"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

# Nouveau : dossier PDF
pdf_dir <- file.path(plot_dir, "pdf")
if (!dir.exists(pdf_dir)) dir.create(pdf_dir, recursive = TRUE)

eu_countries <- c("BEL", "BGR", "CYP", "CZE", "DAN", "DEU", "ELL", "ESP", "EST", 
                  "FRA", "HRV", "HUN", "IRE", "ITA", "LTU", "LUX", "LVA", "MLT", 
                  "NED", "OST", "POL", "POR", "ROU", "SUO", "SVE", "SVK", "SVN", "UKI")

for (code in eu_countries) {
  cat("\n------ Traitement du pays :", code, "------\n")
  
  input_file <- paste0("D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Test cluster/input/Data pour outliers treat/prepared_objects_FERT_GC_", code, ".RDS")
  output_file <- paste0("D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Test cluster/input/Data_for_allocation/prepared_list_FERT_GC_", code, ".RDS")
  
  if (!file.exists(input_file)) {
    cat("\u26a0\ufe0f Fichier non trouvé pour :", code, "\n")
    next
  }
  
  my_list <- readRDS(input_file)
  MYDATA_GC <- my_list$MYDATA_GC
  MYDATA_GC$FERT_N_HA <- MYDATA_GC$SE296 / MYDATA_GC$SAUC
  
  out_df <- MYDATA_GC %>%
    dplyr::select(ID, YEAR, FERT_N_HA, SAUC) %>%
    tidyr::drop_na()
  
  X <- as.matrix(out_df[, c("FERT_N_HA", "SAUC")])
  res_bacon <- wBACON(x = X, alpha = 0.05)
  out_df$Outlier_wBACON <- is_outlier(res_bacon)
  
  X_scaled <- scale(X)
  res_db <- dbscan(X_scaled, eps = 0.5, minPts = 5)
  out_df$Outlier_DBSCAN <- res_db$cluster == 0
  
  Q1 <- apply(X, 2, quantile, 0.25)
  Q3 <- apply(X, 2, quantile, 0.75)
  IQR_vals <- Q3 - Q1
  out_df$Outlier_IQR <- with(out_df,
                             (FERT_N_HA < (Q1["FERT_N_HA"] - 1.5 * IQR_vals["FERT_N_HA"])) |
                               (FERT_N_HA > (Q3["FERT_N_HA"] + 1.5 * IQR_vals["FERT_N_HA"])) |
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
  
  seuil_fert_sup <- quantile(out_df$FERT_N_HA, 0.99, na.rm = TRUE)
  seuil_sauc_min <- quantile(out_df$SAUC, 0.01, na.rm = TRUE)
  cat("\ud83d\udcc9 Seuil 99e FERT_N_HA:", round(seuil_fert_sup, 1), " | Seuil 1e SAUC:", round(seuil_sauc_min, 3), "\n")
  
  pdf(file.path(pdf_dir, paste0("outliers_viz_", code, ".pdf")), width = 7, height = 5)
  
  print(ggplot(out_df, aes(x = FERT_N_HA, y = SAUC, color = Outlier_wBACON)) +
          geom_point(alpha = 0.6) +
          scale_color_manual(values = c("TRUE" = "red", "FALSE" = "darkgreen")) +
          labs(title = "Détection par wBACON") +
          theme_minimal())
  
  print(ggplot(out_df, aes(x = FERT_N_HA, y = SAUC, color = Outlier_DBSCAN)) +
          geom_point(alpha = 0.6) +
          scale_color_manual(values = c("TRUE" = "orange", "FALSE" = "blue")) +
          labs(title = "Détection par DBSCAN") +
          theme_minimal())
  
  print(ggplot(out_df, aes(x = FERT_N_HA, y = SAUC, color = Outlier_IQR)) +
          geom_point(alpha = 0.6) +
          scale_color_manual(values = c("TRUE" = "purple", "FALSE" = "grey")) +
          labs(title = "Détection par IQR") +
          theme_minimal())
  
  print(ggplot(out_df, aes(x = FERT_N_HA, y = SAUC, color = Outlier_Combo)) +
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
  
  print(ggplot(out_df, aes(x = Outlier_Combo, y = FERT_N_HA, fill = Outlier_Combo)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = "Distribution de FERT_N_HA par type d'outlier") +
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
      Outlier_Combo %in% c("wBACON + IQR", "wBACON seul") & FERT_N_HA > seuil_fert_sup ~ "Corriger",
      Outlier_Combo == "IQR seul" & SAUC < seuil_sauc_min & nb_years < 3 ~ "Supprimer",
      Outlier_Combo == "IQR seul" & SAUC < seuil_sauc_min ~ "Corriger",
      TRUE ~ "Ignorer"
    ))
  
  df_cleaned <- out_df %>%
    mutate(FERT_N_HA_cleaned = case_when(
      Action == "Corriger" & FERT_N_HA < 5 ~ 5,
      Action == "Corriger" & FERT_N_HA > seuil_fert_sup ~ seuil_fert_sup,
      TRUE ~ FERT_N_HA
    )) %>%
    filter(Action != "Supprimer")
  
  print(df_cleaned %>%
          ggplot(aes(x = FERT_N_HA, y = SAUC, color = Action)) +
          geom_point(alpha = 0.7) +
          labs(title = "Nuage de points APRÈS nettoyage", x = "FERT_N_HA", y = "SAUC") +
          theme_minimal())
  
  dev.off()
  
  MYDATA_GC_cleaned <- MYDATA_GC %>%
    semi_join(df_cleaned, by = c("ID", "YEAR")) %>%
    left_join(df_cleaned %>% dplyr::select(ID, YEAR, FERT_N_HA_cleaned), by = c("ID", "YEAR")) %>%
    mutate(FERT_N_HA = FERT_N_HA_cleaned) %>%
    dplyr::select(-FERT_N_HA_cleaned)
  
  mylist_for_model <- list(
    MYDATA_GC = MYDATA_GC_cleaned,
    cname_alloc_A = my_list$cname_alloc_A,
    cname_alloc_SH = my_list$cname_alloc_SH,
    cname_alloc_gc = my_list$cname_alloc_gc
  )
  
  saveRDS(mylist_for_model, file = output_file)
  cat("\u2705 Sauvegarde OK pour", code, "→", output_file, "\n")
}
