
# COmbinaison Grece LTU et EST Chypre -------------------------------------


# Charger les deux fichiers
LTU <- readRDS("D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Test cluster/input/Data_2017_2022/LTU.RDS")
EST <- readRDS("D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Test cluster/input/Data_2017_2022/ESP.RDS")

# Vérifier que les colonnes sont identiques
if(!identical(names(LTU), names(EST))) {
  stop("⚠️ Les colonnes ne sont pas identiques entre LTU et ESP, vérifie avant de combiner.")
}

# Combiner les deux
LTUEST <- rbind(LTU, EST)

# Sauvegarder le nouveau fichier
saveRDS(LTUEST, "D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Test cluster/input/Data_2017_2022/LTUEST.RDS")
# Petit message de confirmation
cat("✅ Le fichier LTUEST.RDS a été créé avec", nrow(LTUEST), "lignes et", ncol(LTUEST), "colonnes.\n")

