# ============================
# ESTIMATION FUELS_M GC — ESSAIS DÉCROISSANTS (K = nb_cultures .. 3)
# TOP-K + OTHER + REDISTRIBUTION (avec patch dimnames)
# ============================

# ---- Libs ----
.libPaths(c("./input/library", .libPaths()))
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(Matrix)
  library(FactoMineR); library(future); library(winputall)
})

# ---- Paramètres généraux ----
eps <- 1e-8  # évite zéros stricts dans alpha

# ---- Country code robuste ----
if (!exists("CURRENT_COUNTRY_CODE") || is.null(CURRENT_COUNTRY_CODE) ||
    !nzchar(as.character(CURRENT_COUNTRY_CODE))) {
  args <- commandArgs(trailingOnly = TRUE)
  env_code <- Sys.getenv("CURRENT_COUNTRY_CODE", unset = "")
  if (length(args) >= 1 && nzchar(args[1])) {
    CURRENT_COUNTRY_CODE <- toupper(args[1])
  } else if (nzchar(env_code)) {
    CURRENT_COUNTRY_CODE <- toupper(env_code)
  } else if (interactive() || .Platform$GUI == "RStudio") {
    CURRENT_COUNTRY_CODE <- "OST"
  } else {
    stop("CURRENT_COUNTRY_CODE introuvable (arg/env).")
  }
}
cat("\n--- TRAITEMENT (K décroissant) :", CURRENT_COUNTRY_CODE, " ---\n")

# ---- Lecture des données ----
input_file <- file.path("D:/Partage_Utilisateurs/RICA/TEST CLUSTER/Fuels/input/Data_for_allocation",
                        paste0("prepared_list_FUELS_M_GC_", CURRENT_COUNTRY_CODE, ".RDS"))
if (!file.exists(input_file)) stop("Fichier non trouvé :", normalizePath(input_file, mustWork = FALSE))
pl <- readRDS(input_file)

MYDATA_GC      <- pl$MYDATA_GC
cname_alloc_A  <- pl$cname_alloc_A
cname_alloc_SH <- pl$cname_alloc_SH
cname_alloc_gc <- pl$cname_alloc_gc

# ---- Prétrait robuste commun ----
stopifnot(all(c("ID","YEAR","FUELS_M_HA") %in% names(MYDATA_GC)))
MYDATA_GC <- MYDATA_GC %>%
  mutate(ID = as.character(ID), YEAR = as.integer(YEAR)) %>%
  filter(!is.na(ID), trimws(ID)!="", !is.na(YEAR))

# LOGNORMAL => garder FUELS_M_HA > 0 & non-NA
if (anyNA(MYDATA_GC$FUELS_M_HA)) {
  cat("⚠️ Retrait FUELS_M_HA NA:", sum(is.na(MYDATA_GC$FUELS_M_HA)),"\n")
  MYDATA_GC <- filter(MYDATA_GC, !is.na(FUELS_M_HA))
}
if (any(MYDATA_GC$FUELS_M_HA <= 0, na.rm=TRUE)) {
  cat("⚠️ Retrait FUELS_M_HA <= 0:", sum(MYDATA_GC$FUELS_M_HA <= 0, na.rm=TRUE), "\n")
  MYDATA_GC <- filter(MYDATA_GC, FUELS_M_HA > 0)
}

# Surfaces d’allocation : coercition numérique + NA→0
for (v in cname_alloc_SH) {
  if (!v %in% names(MYDATA_GC)) stop("Colonne surface manquante: ", v)
  if (!is.numeric(MYDATA_GC[[v]])) MYDATA_GC[[v]] <- suppressWarnings(as.numeric(MYDATA_GC[[v]]))
  MYDATA_GC[[v]][!is.finite(MYDATA_GC[[v]]) | is.na(MYDATA_GC[[v]])] <- 0
}

# Retirer lignes sans aucune surface > 0 (global)
rs_all <- rowSums(as.matrix(MYDATA_GC[, cname_alloc_SH, drop=FALSE]), na.rm=TRUE)
if (any(rs_all <= 0)) {
  cat("⚠️ Retrait lignes sans surface (>0):", sum(rs_all<=0), "\n")
  MYDATA_GC <- MYDATA_GC[rs_all > 0, , drop=FALSE]
}

# IDs avec ≥ 3 années (panel sain)
ids_ok <- names(which(table(MYDATA_GC$ID) >= 3))
MYDATA_GC <- filter(MYDATA_GC, ID %in% ids_ok)
stopifnot(nrow(MYDATA_GC) > 0)

# ---- Dummies YEAR (après filtres) ----
if (dplyr::n_distinct(MYDATA_GC$YEAR) > 1) {
  t_fact  <- factor(MYDATA_GC$YEAR)
  t_dummy <- model.matrix(~ 0 + t_fact)
  colnames(t_dummy) <- paste0("t_fact", levels(t_fact))  # ex: t_fact2018
  MYDATA_GC <- bind_cols(MYDATA_GC, as.data.frame(t_dummy))
  t_dummy_names <- colnames(t_dummy)
} else {
  MYDATA_GC <- MYDATA_GC %>%
    mutate(time_T = min(YEAR, na.rm = TRUE),
           TREND  = YEAR - time_T + 1,
           TREND_z = as.numeric(scale(TREND)))
  t_dummy_names <- "TREND_z"
}

# ---- PCA (sur surfaces agrégées A) + plots ----
vars_pca <- NULL
pca_ok <- TRUE
if (!is.null(cname_alloc_A) && length(cname_alloc_A) > 0) {
  tc <- try({
    MYDATA_GC %>%
      group_by(ID) %>%
      summarise(across(all_of(cname_alloc_A), ~ mean(.x, na.rm=TRUE), .names="{.col}"),
                .groups="drop")
  }, silent = TRUE)
  if (inherits(tc, "try-error") || ncol(tc) <= 1) {
    pca_ok <- FALSE
  } else {
    X <- tc[, -1, drop=FALSE]
    keepv <- vapply(X, function(v) var(v, na.rm=TRUE) > 0, logical(1))
    X <- X[, keepv, drop=FALSE]
    if (nrow(X) < 3 || ncol(X) < 1) pca_ok <- FALSE
    if (pca_ok) {
      pca <- FactoMineR::PCA(X, scale.unit=TRUE,
                             ncp=min(5, ncol(X), nrow(X)-1), graph=FALSE)
      sc  <- as.data.frame(pca$ind$coord)
      Kpc <- min(3, ncol(sc))
      if (Kpc >= 1) {
        sc <- sc[, seq_len(Kpc), drop=FALSE]
        colnames(sc) <- paste0("sh_comp", seq_len(Kpc))
        MYDATA_GC <- left_join(MYDATA_GC, cbind(ID=tc$ID, sc), by="ID")
        vars_pca <- colnames(sc)
        # Plots PCA
        out_dir <- file.path(getwd(), "fig_pca"); dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)
        png(file.path(out_dir, sprintf("pca_var_circle_%s.png", CURRENT_COUNTRY_CODE)), 1200,1200, res=150)
        plot(pca, choix="var", title=sprintf("Cercle des corrélations — %s", CURRENT_COUNTRY_CODE)); dev.off()
        png(file.path(out_dir, sprintf("pca_ind_map_%s.png", CURRENT_COUNTRY_CODE)), 1200,1200, res=150)
        plot(pca, choix="ind", title=sprintf("Individus (scores) — %s", CURRENT_COUNTRY_CODE)); dev.off()
        png(file.path(out_dir, sprintf("pca_scree_%s.png", CURRENT_COUNTRY_CODE)), 1200,900, res=150)
        barplot(pca$eig[,1],
                ylim=c(0, max(5, max(pca$eig[,1], na.rm=TRUE))),
                main=sprintf("Valeurs propres — %s", CURRENT_COUNTRY_CODE),
                ylab="Valeur propre", xlab="Dimensions")
        abline(h=mean(pca$eig[,1], na.rm=TRUE), col="red", lty=2); dev.off()
      } else {
        pca_ok <- FALSE
      }
    }
  }
}
if (!pca_ok || is.null(vars_pca) || !length(vars_pca)) {
  MYDATA_GC <- MYDATA_GC %>%
    mutate(time_T = min(YEAR, na.rm = TRUE),
           TREND  = YEAR - time_T + 1,
           TREND_z = as.numeric(scale(TREND)))
  vars_pca <- "TREND_z"
  cat("⚠️ PCA indisponible → fallback TREND_z\n")
}

# ---- Ordre des cultures par importance (totaux) ----
totals <- colSums(MYDATA_GC[, cname_alloc_SH, drop=FALSE], na.rm=TRUE)
if (sum(totals) <= 0) stop("Somme des surfaces totales nulle.")
ord <- order(totals, decreasing = TRUE)
ordered_crops <- names(totals)[ord]
maxK <- length(ordered_crops)

# ---- Fonction d’essai pour un K donné (sans effet de bord) ----
try_fit_for_K <- function(K, base_df) {
  top_crops   <- ordered_crops[seq_len(K)]
  minor_crops <- setdiff(cname_alloc_SH, top_crops)
  cat(sprintf("➡️  Essai K=%d | TOP: %s | MINOR: %s\n",
              K,
              paste(top_crops, collapse=", "),
              ifelse(length(minor_crops), paste(minor_crops, collapse=", "), "(aucune)")))
  
  DF <- base_df  # copie locale, pas d'effet global
  
  # Construire alpha réduit: TOP-K + OTHER_SH (si mineures)
  if (length(minor_crops)) {
    DF$OTHER_SH <- rowSums(DF[, minor_crops, drop=FALSE], na.rm=TRUE)
    cname_alloc_SH_red <- c(top_crops, "OTHER_SH")
    top_idx <- match(top_crops, pl$cname_alloc_SH)
    cname_alloc_gc_red <- c(pl$cname_alloc_gc[top_idx], "OTHER")
  } else {
    cname_alloc_SH_red <- top_crops
    cname_alloc_gc_red <- pl$cname_alloc_gc[match(top_crops, pl$cname_alloc_SH)]
  }
  
  # Normalisation stricte (alpha >0 & somme=1)
  S <- as.matrix(DF[, cname_alloc_SH_red, drop=FALSE])
  S[!is.finite(S)] <- 0
  S <- pmax(S, 0) + eps
  S <- S / rowSums(S)
  if (any(!is.finite(S))) stop("NA/Inf dans alpha (K=", K, ")")
  DF[, cname_alloc_SH_red] <- S
  
  # Listes de régressives (NOMMÉES)
  crop_rp_indvar <- replicate(length(cname_alloc_SH_red), vars_pca, simplify = FALSE)
  crop_indvar    <- replicate(length(cname_alloc_SH_red), t_dummy_names, simplify = FALSE)
  names(crop_rp_indvar) <- cname_alloc_SH_red
  names(crop_indvar)    <- cname_alloc_SH_red
  
  # Checks rapides
  used_cols <- unique(c("ID","YEAR","FUELS_M_HA", cname_alloc_SH_red,
                        unlist(crop_rp_indvar), unlist(crop_indvar)))
  missing <- setdiff(used_cols, names(DF))
  if (length(missing)) stop("Colonnes manquantes (K=", K, "): ", paste(missing, collapse=", "))
  
  for (v in setdiff(used_cols, c("ID","YEAR"))) {
    x <- DF[[v]]
    if (!is.numeric(x)) x <- suppressWarnings(as.numeric(x))
    x[!is.finite(x) | is.na(x)] <- 0
    DF[[v]] <- x
  }
  alpha_mat <- as.matrix(DF[, cname_alloc_SH_red, drop=FALSE])
  if (min(alpha_mat) <= 0) stop("alpha <= 0 (K=", K, ")")
  if (any(abs(rowSums(alpha_mat) - 1) > 1e-8)) stop("Somme(alpha)!=1 (K=", K, ")")
  
  # Estimation
  future::plan(future::multisession, workers=25)
  saem_control <- list(nb_SA=200, nb_smooth=100, nb_burn_sim=50, estim_rdraw=100)
  
  fit <- rpinpallEst(
    data = DF,
    id_time = c("ID","YEAR"),
    total_input = "FUELS_M_HA",
    crop_acreage = cname_alloc_SH_red,   # TOP-K (+ OTHER)
    crop_rp_indvar = crop_rp_indvar,     # LISTE NOMMÉE
    crop_indvar    = crop_indvar,        # LISTE NOMMÉE
    distrib_method = "lognormal",
    sim_method = "map_imh",
    calib_method = "cmode",
    saem_control = saem_control,
    par_init = list()
  )
  
  list(fit=fit,
       DF=DF,
       cname_alloc_SH_red=cname_alloc_SH_red,
       cname_alloc_gc_red=cname_alloc_gc_red,
       top_crops=top_crops,
       minor_crops=minor_crops)
}

# ---- Boucle d’essais K = max .. 3 ----
selected <- NULL
err_msgs <- c()
for (K in seq(from=maxK, to=3, by=-1)) {
  attempt <- try(try_fit_for_K(K, MYDATA_GC), silent = TRUE)
  if (inherits(attempt, "try-error")) {
    msg <- paste0("K=", K, " ❌ ", as.character(attempt))
    cat(msg, "\n")
    err_msgs <- c(err_msgs, msg)
    next
  } else {
    selected <- attempt
    cat("✅ SUCCÈS avec K=", K, "\n", sep="")
    break
  }
}

if (is.null(selected)) {
  cat("Historique des erreurs:\n", paste(err_msgs, collapse="\n"), "\n")
  stop("Arrêt : estimation non obtenue pour ", CURRENT_COUNTRY_CODE, " après essais K=max..3")
}

future::plan(future::sequential)

fit                   <- selected$fit
DF_est                <- selected$DF                 # DF utilisé à l'estimation
cname_alloc_SH_red    <- selected$cname_alloc_SH_red
cname_alloc_gc_red    <- selected$cname_alloc_gc_red
top_crops_selected    <- selected$top_crops
minor_crops_selected  <- selected$minor_crops
K_selected            <- length(top_crops_selected)
cat("🏁 Modèle final: K=", K_selected,
    " | TOP: ", paste(top_crops_selected, collapse=", "),
    ifelse(length(minor_crops_selected), paste0(" | MINOR: ", paste(minor_crops_selected, collapse=", ")), ""),
    "\n", sep="")
plot(fit)

# ==========
# Prédictions + Redistribution OTHER (PATCH QUI FIXE sh2gc / vapply / OTHER restant)
# ==========

idtime   <- c("ID","YEAR")
xit_pred <- fit$xit_pred

# -- 1) Renommage robuste en suivant l’ordre réel du modèle --
# Ordre SH utilisé par le modèle (peut contenir "OTHER_SH")
used_sh <- tryCatch(fit$crop_acreage, error = function(e) NULL)
if (is.null(used_sh) || !length(used_sh)) used_sh <- cname_alloc_SH_red

# Mapping SH -> GC sûr (pas de setNames() de longueurs différentes)
make_sh2gc <- function(used_sh, pl, cname_alloc_SH, cname_alloc_gc) {
  # Source préférée: pl si cohérente
  if (!is.null(pl$cname_alloc_SH) &&
      !is.null(pl$cname_alloc_gc) &&
      length(pl$cname_alloc_SH) == length(pl$cname_alloc_gc) &&
      length(pl$cname_alloc_SH) > 0) {
    sh <- as.character(pl$cname_alloc_SH)
    gc <- as.character(pl$cname_alloc_gc)
    sh2gc <- setNames(gc, sh)
  } else if (!is.null(cname_alloc_SH) && !is.null(cname_alloc_gc) &&
             length(cname_alloc_SH) == length(cname_alloc_gc) &&
             length(cname_alloc_SH) > 0) {
    sh2gc <- setNames(as.character(cname_alloc_gc), as.character(cname_alloc_SH))
  } else {
    # Fallback: identité sur ce qu’on voit (pour éviter length mismatch)
    sh2gc <- setNames(as.character(used_sh), as.character(used_sh))
  }
  # Ajouter le mapping OTHER_SH -> OTHER
  sh2gc["OTHER_SH"] <- "OTHER"
  sh2gc
}
sh2gc <- make_sh2gc(used_sh, pl, cname_alloc_SH, cname_alloc_gc)

# Construire les noms GC (avec fallback sur SH si absent)
gc_vec <- unname(sh2gc[used_sh])
miss_gc <- is.na(gc_vec) | !nzchar(gc_vec)
if (any(miss_gc)) gc_vec[miss_gc] <- used_sh[miss_gc]

# Renommer xit_pred en s’alignant sur le nombre réel de colonnes prédictives
pred_cols <- ncol(xit_pred) - length(idtime)
pred_name_vec <- paste0(gc_vec, "_FUELS_M")
# Ajustement de longueur si besoin
if (length(pred_name_vec) < pred_cols) {
  pred_name_vec <- c(pred_name_vec, paste0("extra", seq_len(pred_cols - length(pred_name_vec)), "_FUELS_M"))
} else if (length(pred_name_vec) > pred_cols) {
  pred_name_vec <- pred_name_vec[seq_len(pred_cols)]
}
colnames(xit_pred) <- c(idtime, pred_name_vec)

# -- 2) Redistribution de OTHER vers les cultures mineures (si elles existent) --
if (length(minor_crops_selected)) {
  # Aligner par (ID,YEAR)
  pred_df <- merge(fit$data[, idtime, drop=FALSE], xit_pred, by = idtime, sort = FALSE)
  
  # Poids = proportions des surfaces mineures (sur SH)
  df_minor <- DF_est[, c(idtime, minor_crops_selected), drop = FALSE]
  df_minor_aligned <- dplyr::left_join(pred_df[, idtime, drop=FALSE], df_minor, by = idtime)
  
  minor_mat <- as.matrix(df_minor_aligned[, minor_crops_selected, drop=FALSE])
  minor_mat[!is.finite(minor_mat)] <- 0
  w_denom <- rowSums(minor_mat, na.rm = TRUE)
  W <- sweep(minor_mat, 1, ifelse(w_denom > 0, w_denom, NA_real_), "/")
  
  # Nom de la colonne OTHER après renommage
  OTHER_col <- "OTHER_FUELS_M"
  
  if (OTHER_col %in% colnames(pred_df)) {
    OTHER_pred <- pred_df[[OTHER_col]]; OTHER_pred[is.na(OTHER_pred)] <- 0
    
    # Répartition ligne à ligne
    pred_minor <- W
    for (j in seq_len(ncol(pred_minor))) {
      pred_minor[, j] <- ifelse(is.finite(W[, j]) & !is.na(W[, j]),
                                OTHER_pred * W[, j], 0)
    }
    
    # Noms GC pour les mineures (via le même mapping robuste)
    minor_gc <- unname(sh2gc[minor_crops_selected])
    miss_mgc <- is.na(minor_gc) | !nzchar(minor_gc)
    if (any(miss_mgc)) minor_gc[miss_mgc] <- minor_crops_selected[miss_mgc]
    if (length(minor_gc) == ncol(pred_minor)) {
      colnames(pred_minor) <- paste0(minor_gc, "_FUELS_M")
    } else {
      warning("⚠️ pred_minor: ", ncol(pred_minor), " colonnes vs noms ", length(minor_gc), ". Noms génériques.")
      colnames(pred_minor) <- paste0("minor", seq_len(ncol(pred_minor)), "_FUELS_M")
    }
    
    # Conserver toutes les colonnes sauf OTHER, puis ajouter les mineures redistribuées
    keep_cols <- setdiff(colnames(pred_df), OTHER_col)
    out_pred  <- dplyr::bind_cols(pred_df[, keep_cols, drop=FALSE],
                                  as.data.frame(pred_minor))
    
    # Nettoyage final: retirer toute colonne résiduelle "OTHER*"
    other_mask <- grepl("^OTHER(_|$)", colnames(out_pred), ignore.case = TRUE)
    if (any(other_mask)) out_pred <- out_pred[, !other_mask, drop = FALSE]
    
  } else {
    # Pas d'OTHER dans le modèle (K == toutes les cultures)
    out_pred <- xit_pred
  }
} else {
  # Pas de mineures → pas de redistribution
  out_pred <- xit_pred
}

# -- 3) Sortie finale --
MYDATA_GC_estimation <- merge(fit$data, out_pred, by = idtime, sort = FALSE)

dir.create("output", showWarnings = FALSE, recursive = TRUE)
save(list = c("MYDATA_GC_estimation","fit", "out_pred","MYDATA_GC","CURRENT_COUNTRY_CODE",
              "K_selected","top_crops_selected","minor_crops_selected"),
     file = file.path("output", paste0("output_FUELS_M_GC_Kdec_", CURRENT_COUNTRY_CODE, ".RData")))

cat("✅ Terminé pour ", CURRENT_COUNTRY_CODE, " — ./output\n", sep = "")
cat("ℹ️ K sélectionné: ", K_selected, "\n", sep = "")
