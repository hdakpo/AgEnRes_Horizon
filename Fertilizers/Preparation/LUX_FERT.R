
# Preparation share pour FRANCE -------------------------------------------

# Chargement des données principales
MYDATA <- readRDS("D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Test cluster/input/Data_2017_2022/LUX.RDS")

# ========== IDENTIFICATION DES VARIABLES SELON LE TYPE DE CULTURE ==========
cname_gc <- c("CWHTC", "CWHTD", "CMZ", "CBRL", "CRYE", "COAT", "CRICE",
              "CPOT","CPOTST","CPOTOTH", "CSUGBT",
              "CRAPE", "CSNFL", "CSOYA")

cname_veg_flow <- "SE046"
cname_per_woutvine <- "SE054"
cname_vineyards <- "SE050"

MYDATA <- MYDATA %>% 
  mutate(SE046_A = SE046,
         SE054_A = SE054,
         SE050_A = SE050)

cname <- c(cname_gc, cname_veg_flow, cname_per_woutvine, cname_vineyards)
cname_A <- paste0(cname, sep="_", "A")
cname_SH <- paste0(cname, sep="_", "SH")

# Calcul des autres céréales
cname_cer <- c("CWHTC", "CWHTD", "CMZ", "CBRL", "CRYE", "COAT", "CRICE")
cname_cer_A <- paste0(cname_cer, sep="_", "A")
MYDATA$CCEROTHN_A <- MYDATA$SE035 - rowSums(MYDATA[,cname_cer_A])
MYDATA <- MYDATA %>% 
  mutate(CCEROTHN_A = ifelse(CCEROTHN_A<=0.01,0,CCEROTHN_A))

cname <- c(cname,"CCEROTHN")
cname_A <- paste0(cname, sep="_", "A")
cname_SH <- paste0(cname, sep="_", "SH")

# Calcul SAU
MYDATA$SAUC <- MYDATA$SE025 - MYDATA$SE072 - MYDATA$SE073 - MYDATA$SE071 - MYDATA$CGRSNOUSESUB
MYDATA <- MYDATA[MYDATA$SAUC>0,]
cat("Observations avec SAU > 0:", nrow(MYDATA), "\n")

# Calcul autres terres arables
MYDATA$CARAOTHN_A <- MYDATA$SAUC - rowSums(MYDATA[,cname_A])
MYDATA <- MYDATA %>% 
  mutate(CARAOTHN_A = ifelse(CARAOTHN_A<=0.01,0,CARAOTHN_A))

cname <- c(cname,"CARAOTHN")
cname_A <- paste0(cname, sep="_", "A")
cname_SH <- paste0(cname, sep="_", "SH")

# Calcul des parts de surface
MYDATA[,cname_SH] <- MYDATA[,cname_A]/MYDATA$SAUC
round(colMeans(MYDATA[,cname_SH]),4)*100
MYDATA <- MYDATA %>%
  dplyr::select(-ends_with("_SH"))


# ========== GRANDE CULTURE ==========
MYDATA_GC <- MYDATA[which(MYDATA$TF14 == 15 | MYDATA$TF14 == 16),]
cat("Observations Grande Culture:", nrow(MYDATA_GC), "\n")

MYDATA_GC[,cname_SH] <- (MYDATA_GC[,cname_A]/MYDATA_GC$SAUC)
res <- round(colMeans(MYDATA_GC[,cname_SH]),4)*100

# Sauvegarde des parts GC par pays
share_output_file <- paste0(output_path, "share_GC_FERT_LUX.csv")
write.csv2(res, share_output_file)

# Regroupement pour allocation
cname_alloc_gc <- c("CWHTALL", "CBRL", "CRYE", "COAT", "CCEROTHALL",
                    "CPOTALL", "CSUGBT",
                    "COILSEEDALL",
                    "CARAOTHALL")

CCROPOBS_name <- c("CBRL", "COAT", "CRYE","CSUGBT")

# Création des agrégées
CWHTALL_name <- c("CWHTC", "CWHTD")
CPOTALL_name <- c("CPOT", "CPOTST" , "CPOTOTH")
CCEROTHALL_name <- c("CMZ", "CRICE","CCEROTHN")
COILSEEDALL_name <- c("CRAPE" , "CSNFL", "CSOYA")
CARAOTHALL_name <- c("SE046", "SE054", "SE050", "CARAOTHN")

cname_alloc_A <- paste0(cname_alloc_gc, sep="_", "A")
CWHTALL_name_A <- paste0(CWHTALL_name, sep="_", "A")
CPOTALL_name_A <- paste0(CPOTALL_name, sep="_", "A")
CCEROTHALL_name_A <- paste0(CCEROTHALL_name, sep="_", "A")
COILSEEDALL_name_A <- paste0(COILSEEDALL_name, sep="_", "A")
CARAOTHALL_name_A <- paste0(CARAOTHALL_name, sep="_", "A")
cname_alloc_SH <- paste0(cname_alloc_gc, sep="_", "SH")

# Calculer les superficies des variables créées
MYDATA_GC <- MYDATA_GC %>%
  mutate(CWHTALL_A = rowSums(across(all_of(CWHTALL_name_A))),
         CCEROTHALL_A = rowSums(across(all_of(CCEROTHALL_name_A))),
         COILSEEDALL_A = rowSums(across(all_of(COILSEEDALL_name_A))),
         CPOTALL_A = rowSums(across(all_of(CPOTALL_name_A))),
         CARAOTHALL_A = rowSums(across(all_of(CARAOTHALL_name_A))))

MYDATA_GC <- MYDATA_GC %>% 
  mutate(across(all_of(cname_alloc_A), ~ ifelse(is.infinite(.) | is.na(.), 0, .)))

# Création des parts de superficie
MYDATA_GC[,cname_alloc_SH] <- MYDATA_GC[,cname_alloc_A]/MYDATA_GC$SAUC

#sauvegarde
mylist_for_model <- list(
  MYDATA_GC = MYDATA_GC,
  cname_alloc_A = cname_alloc_A,
  cname_alloc_SH = cname_alloc_SH,
  cname_alloc_gc = cname_alloc_gc
)

saveRDS(mylist_for_model, file = paste0(output_path, "prepared_objects_FERT_GC_LUX.RDS"))





####################LIVESTOCK  AND MIXED ##################################
#Because we add fodder as part of the UAA

# Chargement des données principales
MYDATA <- readRDS("D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Test cluster/input/Data_2017_2022/LUX.RDS")

# ========== IDENTIFICATION DES VARIABLES SELON LE TYPE DE CULTURE ==========
cname_gc <- c("CWHTC", "CWHTD", "CMZ", "CBRL", "CRYE", "COAT", "CRICE",
              "CPOT","CPOTST","CPOTOTH", "CSUGBT",
              "CRAPE", "CSNFL", "CSOYA")

cname_veg_flow <- "SE046"
cname_per_woutvine <- "SE054"
cname_vineyards <- "SE050"
cname_fodder <- "SE071"

MYDATA <- MYDATA %>% 
  mutate(SE046_A = SE046,
         SE054_A = SE054,
         SE050_A = SE050,
         SE071_A = SE071)

cname <- c(cname_gc, cname_veg_flow, cname_per_woutvine, cname_vineyards, cname_fodder)
cname_A <- paste0(cname, sep="_", "A")
cname_SH <- paste0(cname, sep="_", "SH")

# Calcul des autres céréales
cname_cer <- c("CWHTC", "CWHTD", "CMZ", "CBRL", "CRYE", "COAT", "CRICE")
cname_cer_A <- paste0(cname_cer, sep="_", "A")
MYDATA$CCEROTHN_A <- MYDATA$SE035 - rowSums(MYDATA[,cname_cer_A])
MYDATA <- MYDATA %>% 
  mutate(CCEROTHN_A = ifelse(CCEROTHN_A<=0.01,0,CCEROTHN_A))

cname <- c(cname,"CCEROTHN")
cname_A <- paste0(cname, sep="_", "A")
cname_SH <- paste0(cname, sep="_", "SH")

# Calcul SAU
MYDATA$SAUC <- MYDATA$SE025 - MYDATA$SE072 - MYDATA$SE073 
MYDATA <- MYDATA[MYDATA$SAUC>0,]
cat("Observations avec SAU > 0:", nrow(MYDATA), "\n")

# Calcul autres terres arables
MYDATA$CARAOTHN_A <- MYDATA$SAUC - rowSums(MYDATA[,cname_A])
MYDATA <- MYDATA %>% 
  mutate(CARAOTHN_A = ifelse(CARAOTHN_A<=0.01,0,CARAOTHN_A))

cname <- c(cname,"CARAOTHN")
cname_A <- paste0(cname, sep="_", "A")
cname_SH <- paste0(cname, sep="_", "SH")

# Calcul des parts de surface
MYDATA[,cname_SH] <- MYDATA[,cname_A]/MYDATA$SAUC
round(colMeans(MYDATA[,cname_SH]),4)*100
MYDATA <- MYDATA %>%
  dplyr::select(-ends_with("_SH"))

#-------------------- LIVESTOCK ---------------------------------------------------------------

MYDATA_LS <- MYDATA[which(MYDATA$TF14 == 45  | MYDATA$TF14 == 48 | MYDATA$TF14 == 49 | MYDATA$TF14 == 50 | MYDATA$TF14 == 70),]
cat("Observations livestock:", nrow(MYDATA_LS), "\n")

MYDATA_LS[,cname_SH] <- (MYDATA_LS[,cname_A]/MYDATA_LS$SAUC)
res <- round(colMeans(MYDATA_LS[,cname_SH]),4)*100

# Sauvegarde des parts GC par pays
share_output_file <- paste0(output_path, "share_LS_FERT_LUX.csv")
write.csv2(res, share_output_file)

# Regroupement pour allocation
cname_alloc_ls <- c("CWHTALL", "CBRL", "CCEROTHALL",
                    "CFODDER",
                    "CARAOTHALL")

CCROPOBS_name <- c( "CBRL")

# Création des agrégées
CWHTALL_name <- c("CWHTC", "CWHTD")
CCEROTHALL_name <- c("CMZ", "CRYE", "COAT", "CRICE", "CCEROTHN")
CARAOTHALL_name <- c("CPOT", "CPOTST", "CPOTOTH", "CSUGBT", "CRAPE", "CSNFL", "CSOYA", "SE046", "SE054", "SE050", "CARAOTHN")
CFODDER_name <- "SE071"

cname_alloc_A <- paste0(cname_alloc_ls, sep="_", "A")
CCEROTHALL_name_A <- paste0(CCEROTHALL_name, sep="_", "A")
CWHTALL_name_A <- paste0(CWHTALL_name, sep="_", "A")
CARAOTHALL_name_A <- paste0(CARAOTHALL_name, sep="_", "A")
CFODDER_name_A <- paste0(CFODDER_name, sep="_", "A")
cname_alloc_SH <- paste0(cname_alloc_ls, sep="_", "SH")

# Calculer les superficies des variables créées
MYDATA_LS <- MYDATA_LS %>%
  mutate(CCEROTHALL_A = rowSums(across(all_of(CCEROTHALL_name_A))),
         CWHTALL_A = rowSums(across(all_of(CWHTALL_name_A))),
         CFODDER_A = rowSums(across(all_of(CFODDER_name_A))),
         CARAOTHALL_A = rowSums(across(all_of(CARAOTHALL_name_A))))

MYDATA_LS <- MYDATA_LS %>% 
  mutate(across(all_of(cname_alloc_A), ~ ifelse(is.infinite(.) | is.na(.), 0, .)))

# Création des parts de superficie
MYDATA_LS[,cname_alloc_SH] <- MYDATA_LS[,cname_alloc_A]/MYDATA_LS$SAUC

#sauvegarde
mylist_for_model <- list(
  MYDATA_LS = MYDATA_LS,
  cname_alloc_A = cname_alloc_A,
  cname_alloc_SH = cname_alloc_SH,
  cname_alloc_ls = cname_alloc_ls
)

saveRDS(mylist_for_model, file = paste0(output_path, "prepared_objects_FERT_LS_LUX.RDS"))



# --------------------- MIXED SYSTEMS -------------------------------------

MYDATA_MX <- MYDATA[which(MYDATA$TF14 == 80),]
cat("Observations mixed:", nrow(MYDATA_MX), "\n")

MYDATA_MX[,cname_SH] <- (MYDATA_MX[,cname_A]/MYDATA_MX$SAUC)
res <- round(colMeans(MYDATA_MX[,cname_SH]),4)*100

# Sauvegarde des parts GC par pays
share_output_file <- paste0(output_path, "share_MX_FERT_LUX.csv")
write.csv2(res, share_output_file)

# Regroupement pour allocation
cname_alloc_mx <- c("CWHTALL", "CBRL", "CRYE", "COAT", "CCEROTHALL",
                    "CPOTALL",
                    "COILSEEDALL",
                    "CFODDER",
                    "CARAOTHALL")

CCROPOBS_name <- c("CBRL", "CRYE", "COAT")

# Création des agrégées
CWHTALL_name <- c("CWHTC", "CWHTD")
CPOTALL_name <- c("CPOT", "CPOTST", "CPOTOTH")
COILSEEDALL_name <- c("CRAPE" , "CSNFL", "CSOYA")
CCEROTHALL_name <- c("CMZ", "CRICE", "CCEROTHN")
CARAOTHALL_name <- c("CSUGBT", "SE046", "SE054", "SE050", "CARAOTHN")
CFODDER_name <- "SE071"

cname_alloc_A <- paste0(cname_alloc_mx, sep="_", "A")
CWHTALL_name_A <- paste0(CWHTALL_name, sep="_", "A")
CPOTALL_name_A <- paste0(CPOTALL_name, sep="_", "A")
CCEROTHALL_name_A <- paste0(CCEROTHALL_name, sep="_", "A")
COILSEEDALL_name_A <- paste0(COILSEEDALL_name, sep="_", "A")
CARAOTHALL_name_A <- paste0(CARAOTHALL_name, sep="_", "A")
CFODDER_name_A <- paste0(CFODDER_name, sep="_", "A")
cname_alloc_SH <- paste0(cname_alloc_mx, sep="_", "SH")

# Calculer les superficies des variables créées
MYDATA_MX <- MYDATA_MX %>%
  mutate(CPOTALL_A = rowSums(across(all_of(CPOTALL_name_A))),
         COILSEEDALL_A = rowSums(across(all_of(COILSEEDALL_name_A))),
         CCEROTHALL_A = rowSums(across(all_of(CCEROTHALL_name_A))),
         CWHTALL_A = rowSums(across(all_of(CWHTALL_name_A))),
         CFODDER_A = rowSums(across(all_of(CFODDER_name_A))),
         CARAOTHALL_A = rowSums(across(all_of(CARAOTHALL_name_A))))

MYDATA_MX <- MYDATA_MX %>% 
  mutate(across(all_of(cname_alloc_A), ~ ifelse(is.infinite(.) | is.na(.), 0, .)))

# Création des parts de superficie
MYDATA_MX[,cname_alloc_SH] <- MYDATA_MX[,cname_alloc_A]/MYDATA_MX$SAUC

#sauvegarde
mylist_for_model <- list(
  MYDATA_MX = MYDATA_MX,
  cname_alloc_A = cname_alloc_A,
  cname_alloc_SH = cname_alloc_SH,
  cname_alloc_mx = cname_alloc_mx
)

saveRDS(mylist_for_model, file = paste0(output_path, "prepared_objects_FERT_MX_LUX.RDS"))


