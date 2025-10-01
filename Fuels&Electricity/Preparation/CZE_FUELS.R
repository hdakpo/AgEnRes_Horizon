
# Preparation share pour FRANCE -------------------------------------------

# Chargement des données principales
MYDATA <- readRDS("D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Fuels/input/Data_2017_2022/CZE.RDS")

# ========== IDENTIFICATION DES VARIABLES SELON LE TYPE DE CULTURE ==========
cname_gc <- c("CWHTC", "CWHTD", "CMZ", "CBRL", "CRYE", "COAT", "CRICE",
              "CPOT","CPOTST","CPOTOTH", "CSUGBT",
              "CRAPE", "CSNFL", "CSOYA")

cname_veg_flow <- "SE046"
cname_per_woutvine <- "SE054"
cname_vineyards <- "SE050"
cname_fodder <- c("CFODRTBR", "CGRSTMP", "CGRSXRG", "CRG") #on n'a pas la variable CFODOTHPLT pour other fodder plants et CGRSNOUSESUB_A pour Permanent grassland no longer used for production purposes and eligible for the payment of subsidies  Area
MYDATA <- MYDATA %>% 
  mutate(SE046_A = SE046,
         SE054_A = SE054,
         SE050_A = SE050)

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

# Calcul des autres fodder
cname_fodder_A <- paste0(cname_fodder, sep="_", "A")
MYDATA$CFODDOTHN_A <- MYDATA$SE071 - rowSums(MYDATA[,cname_fodder_A])
MYDATA <- MYDATA %>% 
  mutate(CFODDOTHN_A = ifelse(CFODDOTHN_A<=0.01,0,CFODDOTHN_A))

cname <- c(cname,"CFODDOTHN")
cname_A <- paste0(cname, sep="_", "A")
cname_SH <- paste0(cname, sep="_", "SH")

# Calcul SAU
MYDATA$SAUC <- MYDATA$SE025 - MYDATA$SE072 - MYDATA$SE073 #- MYDATA$SE071 - MYDATA$CGRSNOUSESUB
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
share_output_file <- paste0(output_path, "share_GC_FUELS_CZE.csv")
write.csv2(res, share_output_file)

# Regroupement pour allocation
cname_alloc_gc <- c("CWHTALL", "CMZ", "CBRL", "CCEROTHALL",
                    "CPOT", "CPOTOTH", 
                    "CSUGBT", "CRAPE", "COILSEEDOTH", 
                    "CVEGFLOW", 
                    "CFODDOTHALL", "CGRSXRG", "CARAOTHALL")

CCROPOBS_name <- c("CMZ", "CBRL",
                   "CPOT", 
                   "CSUGBT", "CRAPE", "CGRSXRG")

# Création des agrégées
CWHTALL <- c("CWHTC", "CWHTD")
CCEROTHALL_name <- c("CRYE", "COAT", "CRICE", "CCEROTHN")
CPOTOTH_name <- c("CPOTST", "CPOTOTH")
CVEGFLOW_name <- "SE046"
COILSEEDOTH_name <- c("CSNFL", "CSOYA")
CARAOTHALL_name <- c("SE054", "SE050", "CARAOTHN")
CFODDOTHALL_name <- c("CFODRTBR", "CGRSTMP", "CRG", "CFODDOTHN")

cname_alloc_A <- paste0(cname_alloc_gc, sep="_", "A")
CWHTALL_name_A <- paste0(CWHTALL_name, sep="_", "A")
CCEROTHALL_name_A <- paste0(CCEROTHALL_name, sep="_", "A")
CPOTOTH_name_A <- paste0(CPOTOTH_name, sep="_", "A")
CVEGFLOW_name_A <- paste0(CVEGFLOW_name, sep="_", "A")
COILSEEDOTH_name_A <- paste0(COILSEEDOTH_name, sep="_", "A")
CARAOTHALL_name_A <- paste0(CARAOTHALL_name, sep="_", "A")
CFODDOTHALL_name_A <- paste0(CFODDOTHALL_name, sep="_", "A")
cname_alloc_SH <- paste0(cname_alloc_gc, sep="_", "SH")

# Calculer les superficies des variables créées
MYDATA_GC <- MYDATA_GC %>%
  mutate(CWHTALL_A = rowSums(across(all_of(CWHTALL_name_A))),
         CCEROTHALL_A = rowSums(across(all_of(CCEROTHALL_name_A))),
         CPOTOTH_A = rowSums(across(all_of(CPOTOTH_name_A))),
         CFODDOTHALL_A = rowSums(across(all_of(CFODDOTHALL_name_A))),
         COILSEEDOTH_A = rowSums(across(all_of(COILSEEDOTH_name_A))),
         CVEGFLOW_A = rowSums(across(all_of(CVEGFLOW_name_A))),
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

saveRDS(mylist_for_model, file = paste0(share_path, "prepared_objects_FUELS_GC_CZE.RDS"))




####################LIVESTOCK  AND MIXED ##################################

#-------------------- LIVESTOCK ---------------------------------------------------------------

MYDATA_LS <- MYDATA[which(MYDATA$TF14 == 45  | MYDATA$TF14 == 48 | MYDATA$TF14 == 49 | MYDATA$TF14 == 50 | MYDATA$TF14 == 70),]
cat("Observations livestock:", nrow(MYDATA_LS), "\n")

MYDATA_LS[,cname_SH] <- (MYDATA_LS[,cname_A]/MYDATA_LS$SAUC)
res <- round(colMeans(MYDATA_LS[,cname_SH]),4)*100

# Sauvegarde des parts GC par pays
share_output_file <- paste0(output_path, "share_LS_FUELS_CZE.csv")
write.csv2(res, share_output_file)

# Regroupement pour allocation
cname_alloc_ls <- c("CWHTALL", "CBRL", "COAT", "CCEROTHALL",
                    "COILSEEDALL",
                    "CGRSTMP", "CGRSXRG", "CFODDOTHALL",
                    "CARAOTHALL")

CCROPOBS_name <- c("CGRSTMP", "CGRSXRG", "CBRL", "COAT")

# Création des agrégées
CWHTALL_name <- c("CWHTD", "CWHTC")
CCEROTHALL_name <- c("CMZ" , "CRYE", "CRICE", "CCEROTHN")
COILSEEDALL_name <- c("CRAPE","CRYE", "CSNFL")
CARAOTHALL_name <- c("CPOT", "CPOTST", "CPOTOTH", "CSUGBT", "SE046", "SE050" , "SE054", "CARAOTHN")
CFODDOTHALL_name <- c("CFODRTBR","CRG", "CFODDOTHN")

cname_alloc_A <- paste0(cname_alloc_ls, sep="_", "A")
CWHTALL_name_A <- paste0(CWHTALL_name, sep="_", "A")
CCEROTHALL_name_A <- paste0(CCEROTHALL_name, sep="_", "A")
COILSEEDALL_name_A <- paste0(COILSEEDALL_name, sep="_", "A")
CARAOTHALL_name_A <- paste0(CARAOTHALL_name, sep="_", "A")
CFODDOTHALL_name_A <- paste0(CFODDOTHALL_name, sep="_", "A")
cname_alloc_SH <- paste0(cname_alloc_ls, sep="_", "SH")

# Calculer les superficies des variables créées
MYDATA_LS <- MYDATA_LS %>%
  mutate(CWHTALL_A = rowSums(across(all_of(CWHTALL_name_A))),
         CCEROTHALL_A = rowSums(across(all_of(CCEROTHALL_name_A))),
         COILSEEDALL_A = rowSums(across(all_of(COILSEEDALL_name_A))),
         CFODDOTHALL_A = rowSums(across(all_of(CFODDOTHALL_name_A))),
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

saveRDS(mylist_for_model, file = paste0(share_path, "prepared_objects_FUELS_LS_CZE.RDS"))


# --------------------- MIXED SYSTEMS -------------------------------------

MYDATA_MX <- MYDATA[which(MYDATA$TF14 == 80),]
cat("Observations mixed:", nrow(MYDATA_MX), "\n")

MYDATA_MX[,cname_SH] <- (MYDATA_MX[,cname_A]/MYDATA_MX$SAUC)
res <- round(colMeans(MYDATA_MX[,cname_SH]),4)*100

# Sauvegarde des parts GC par pays
share_output_file <- paste0(output_path, "share_MX_FUELS_CZE.csv")
write.csv2(res, share_output_file)

# Regroupement pour allocation
cname_alloc_mx <- c("CWHTALL","CMZ", "CBRL", "COAT", "CCEROTHALL",
                    "CPOTALL",
                    "CSUGBT", "CRAPE", 
                    "CGRSTMP", "CGRSXRG", "CFODDOTHALL",
                    "CARAOTHALL")

CCROPOBS_name <- c("CMZ", "CBRL", "COAT", "CSUGBT", "CRAPE", "CGRSTMP", "CGRSXRG")

# Création des agrégées
CWHTALL_name <- c("CWHTC" , "CWHTD")
CCEROTHALL_name <- c( "CRYE", "CRICE", "CCEROTHN")
CPOTALL_name <- c("CPOT", "CPOTST", "CPOTOTH")
CARAOTHALL_name <- c("CSNFL", "CSOYA", "SE046","SE054", "SE050", "CARAOTHN")
CFODDOTHALL_name <- c("CFODRTBR", "CRG", "CFODDOTHN")

cname_alloc_A <- paste0(cname_alloc_mx, sep="_", "A")
CWHTALL_name_A <- paste0(CCEROTHALL_name, sep="_", "A")
CCEROTHALL_name_A <- paste0(CCEROTHALL_name, sep="_", "A")
CPOTALL_name_A <- paste0(CPOTALL_name, sep="_", "A")
CARAOTHALL_name_A <- paste0(CARAOTHALL_name, sep="_", "A")
CFODDOTHALL_name_A <- paste0(CFODDOTHALL_name, sep="_", "A")
cname_alloc_SH <- paste0(cname_alloc_mx, sep="_", "SH")

# Calculer les superficies des variables créées
MYDATA_MX <- MYDATA_MX %>%
  mutate(CWHTALL_A = rowSums(across(all_of(CWHTALL_name_A))),
         CCEROTHALL_A = rowSums(across(all_of(CCEROTHALL_name_A))),
         CPOTALL_A = rowSums(across(all_of(CPOTALL_name_A))),
         CFODDOTHALL_A = rowSums(across(all_of(CFODDOTHALL_name_A))),
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

saveRDS(mylist_for_model, file = paste0(share_path, "prepared_objects_FUELS_MX_CZE.RDS"))

