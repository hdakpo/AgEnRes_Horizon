
# COmbinaison Grece ELL et CYP Chypre -------------------------------------


# Charger les deux fichiers
ELL <- readRDS("D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Test cluster/input/Data_2017_2022/ELL.RDS")
CYP <- readRDS("D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Test cluster/input/Data_2017_2022/CYP.RDS")

# Vérifier que les colonnes sont identiques
if(!identical(names(ELL), names(CYP))) {
  stop("⚠️ Les colonnes ne sont pas identiques entre ELL et CYP, vérifie avant de combiner.")
}

# Combiner les deux
ELLCYP <- rbind(ELL, CYP)

# Sauvegarder le nouveau fichier
saveRDS(ELLCYP, "D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Test cluster/input/Data_2017_2022/ELLCYP.RDS")
# Petit message de confirmation
cat("✅ Le fichier ELLCYP.RDS a été créé avec", nrow(ELLCYP), "lignes et", ncol(ELLCYP), "colonnes.\n")

