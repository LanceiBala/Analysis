
# IMPORTATION DES PACKAGES


install.packages("readxl")

install.packages('tidyverse', dependencies=TRUE)
install.packages("labelled")
install.packages("questionr")
install.packages("skimr", dependencies=TRUE)
install.packages("gtsummary")


###########


library(readxl)
library(tidyverse)
library(dplyr)
library(labelled)
library(questionr)
library(skimr)
library(gtsummary)
db <- read_xlsx("Base_M2_SPSD_Dénis_revue.xlsx")

#######
View(db)
dim(db)
names(db)
str(db)
#######
######
glimpse(db$poss_terr_autr_bien)
look_for(db)
skim(db)
dput(names(db))
###############


var_corrige <- c("N°", "age", "st_mat", "exi_coe", "duree_union",
                 
  "niv_sco", "profess", "res_vil_titao", 
  "Statut", "Provenance", "lieu_acc", "poss_terr_autr_bien", 
  "autr_bien_san_poss_terr", "aucun_bien_mat", "atcd_medicaux", 
  "atcd_psychiatrique", "atcd_chirurgicaux", "gestite", "parite", 
  "vomi_au_cour_gross", "douleur_pelvien_au_cour_gross", 
  "saign_au_cour_gross", "avortement", "mortNe", "type_avortement", 
  "accouch_sprmaturs", "gross_terme", "suivi_cpn", "mod_accouch", 
  "motiv_gross", "profess_conj", "age_conj", 
  "niv_instruc_conj", "impres_par_rapport_gross", 
  "support_ordonn", "accompagnant_accouch", 
  "trbl_psychq_dpist", "diffi_adptation_lie_hist_cont_vie", 
  "diag_trad_envi_parent", "circuit_therapeutik", 
  "conse_sur_rela_enf", "evolution", "atcd_caren_affective", 
  "atcd_maltraitance", "atcd_abu_sex_enf", 
  "atcd_pers_troubl_psych", "atcd _fam_trbl_psych", 
  "gross_non_des", "age_jeun_adolesce", "mere_celibat", 
  "conflit_conjug", "deces_enf", "deuil_prinatal_pers_proch", 
  "stress_pdt_gross", "isol_famil_social", 
  "hmoglobine", "glycmie", "albumine", "GSRh", "primiparite", "multiparit_4-5accouchements", 
  "Pauciparit_2-3 accouchements", "grandemultiparit_>6accouchements", 
  "interup_gross", "suspicion_malform_prenat", 
  "accouch_premat", "accouch_dystocique", "cesar_urgence", 
  "cesar_program", "anesthsie_generale", "exam_general_normal", 
  "exam_gynco_bsttrique_normal", "GE", "VIH", "Widal", "BWtestsyphilis", 
  "Toxoplasmose", "EEG", "ECG", "HepatiteB", "Suivi", "chimio_thrapie_psychiatrique", 
  "psychotherapie", "evolution_clinique_CPON6_jr", "EvolutioncliniqueCPONJ42", 
  "trouble_psychiques_dpist_J6_postpartum", "trouble_psychique_dpist_J42_postpartum", "period_depist_postpartum",
  "dépression_post_natale", 
  "stress_post_traumatique", "diffi_adaptation_liées_histo_ou_cont_vie", 
  "anxiete", "troubles_bipolaires", "Blue_du_post-partum", "troubles_psychique_post-partum", 
  "duree_evolut_troubles_psychique", 
  "impact_evolut_troubles_psychique_J6postpartum", "impact_evolut_troubles_psychique_J42postpartum"
)

names(db) <- var_corrige

View(db)

# CREATION DE LA VARIABLE TRANCHE AGE

db <- db %>% mutate(tranch_age =  cut(age,
                                      breaks = c(0, 18, 20,40,  Inf),
                                      labels = c(" < 18 ans", "18-20 ans", "21-40 ans", "40 et plus"),
                                      right = FALSE), .after = age)
table(db$tranch_age)


db <- db %>% mutate(tranch_age_con =  cut(age_conj,
                                      breaks = c(0, 18, 20,40,  Inf),
                                      labels = c(" < 18 ans", "18-20 ans", "21-40 ans", "40 et plus"),
                                      right = FALSE), .after = age_conj)
table(db$tranch_age_con)




# CREATION DE LA VARIABLE TRANCHE DUREE UNION

db <- db %>% mutate(tranch_duree_union =  cut(duree_union,
                                              breaks = c(0, 1, 5,10,  Inf),
                                              labels = c(" < 1 ans", "1-5ans", "5-10 ans", "10 et plus"),
                                              right = FALSE), .after = duree_union)
table(db$tranch_duree_union)
 View(db)
# Transformer les variables charactere en factor

db <- db %>% mutate_if(is.character, as.factor)
str(db)



# Tableau des fréquences

# SELECTION DES VARIABLES CONCERNEES

var_stat_des <- c("age","tranch_age", "st_mat", "exi_coe", "tranch_duree_union", 
                  "niv_sco", "profess", "poss_terr_autr_bien", 
                  "autr_bien_san_poss_terr", "aucun_bien_mat", "Provenance",
                  "lieu_acc",
                  "atcd_medicaux", 
                  
                  "atcd_psychiatrique", "atcd_chirurgicaux", "gestite", "parite",
                  "multiparit_4-5accouchements", 
                  "Pauciparit_2-3 accouchements", "grandemultiparit_>6accouchements",
                  "accouch_premat","suivi_cpn", "cesar_program", "accouch_dystocique",
                  "motiv_gross", "profess_conj", "niv_instruc_conj","impres_par_rapport_gross",
                  "support_ordonn", "accompagnant_accouch", "troubles_bipolaires", "dépression_post_natale",
                  "Blue_du_post-partum", "anxiete","diffi_adaptation_liées_histo_ou_cont_vie",
                  "diffi_adaptation_liées_histo_ou_cont_vie" )


db_freq_1 <- db %>% select(var_stat_des)

dim(db_freq_1)
lapply(db_freq_1, function(x) freq(x, total = TRUE, valid = FALSE))


#sum <- db %>% tbl_summary(include = var_stat_des_autres )

# TEST D INDEPENDANCES DES VARIABLES

v_ex <-  c( "tranch_age", "st_mat", "exi_coe", "tranch_duree_union", 
                
                "niv_sco", "res_vil_titao", 
                "Statut", "Provenance", "lieu_acc", "poss_terr_autr_bien", 
                "autr_bien_san_poss_terr", "aucun_bien_mat", "atcd_medicaux", 
                "atcd_psychiatrique", "atcd_chirurgicaux","atcd_caren_affective", 
                "atcd_maltraitance", "atcd_abu_sex_enf", 
                "atcd_pers_troubl_psych", "atcd _fam_trbl_psych", "conflit_conjug",  "gross_non_des",
                 "profess_conj", "tranch_age_con", "age_jeun_adolesce", "mere_celibat",
                 "deces_enf", "deuil_prinatal_pers_proch", 
                "stress_pdt_gross", "isol_famil_social", 
                "niv_instruc_conj", 
                "hmoglobine", "glycmie", "albumine", "GSRh", "primiparite", "multiparit_4-5accouchements", 
                "Pauciparit_2-3 accouchements", "grandemultiparit_>6accouchements", 
               "interup_gross", "suspicion_malform_prenat", 
                "accouch_premat", "accouch_dystocique", "cesar_urgence", 
                "cesar_program", "anesthsie_generale", "exam_general_normal", 
                "exam_gynco_bsttrique_normal", "GE", "VIH", "Widal", "BWtestsyphilis", 
                "Toxoplasmose", "EEG", "ECG", "HepatiteB", "Suivi", "chimio_thrapie_psychiatrique", 
                "psychotherapie", "evolution_clinique_CPON6_jr", "EvolutioncliniqueCPONJ42"
              )
                             

### VARIABLE DEPRESSION POST NATAL #########3
###############################################

# Fonction pour effectuer le test du chi-carré ou le test exact de Fisher pour chaque variable
test_independance <- function(outcome, factor) {
  tbl <- table(outcome, factor)
  
  # Vérifier si les effectifs théoriques sont suffisants pour utiliser le chi-carré
  chi2_test <- chisq.test(tbl, simulate.p.value = TRUE)
  if (any(chi2_test$expected < 5)) {
    cat("Utilisation du test exact de Fisher en raison des effectifs faibles.\n")
    test <- fisher.test(tbl)
  } else {
    test <- chi2_test
  }
  return(test)
}

# Liste pour stocker les résultats
resultats_tests <- list()

# Effectuer le test pour chaque facteur
for (facteur in v_ex) {
  cat("\nTest pour", facteur, ":\n")
  test_result <- test_independance(db$dépression_post_natale, db[[facteur]])
  resultats_tests[[facteur]] <- test_result
  print(test_result)
}

# Résumé des résultats
cat("\nRésumé des résultats des tests:\n")
for (facteur in names(resultats_tests)) {
  cat("\nVariable:", facteur, "\n")
  print(resultats_tests[[facteur]]$p.value)
}

### VARIABLE stress_post_traumatique #########3
###############################################

test_independance <- function(outcome, factor) {
  tbl <- table(outcome, factor)
  
  # Vérifier si les effectifs théoriques sont suffisants pour utiliser le chi-carré
  chi2_test <- chisq.test(tbl, simulate.p.value = TRUE)
  if (any(chi2_test$expected < 5)) {
    cat("Utilisation du test exact de Fisher en raison des effectifs faibles.\n")
    test <- fisher.test(tbl)
  } else {
    test <- chi2_test
  }
  return(test)
}

# Liste pour stocker les résultats
resultats_tests <- list()

# Effectuer le test pour chaque facteur
for (facteur in v_ex) {
  cat("\nTest pour", facteur, ":\n")
  test_result <- test_independance(db$stress_post_traumatique, db[[facteur]])
  resultats_tests[[facteur]] <- test_result
  print(test_result)
}

# Résumé des résultats
cat("\nRésumé des résultats des tests:\n")
for (facteur in names(resultats_tests)) {
  cat("\nVariable:", facteur, "\n")
  print(resultats_tests[[facteur]]$p.value)
}

## VARIABLE ANXIETE #########3
###############################################

test_independance <- function(outcome, factor) {
  tbl <- table(outcome, factor)
  
  # Vérifier si les effectifs théoriques sont suffisants pour utiliser le chi-carré
  chi2_test <- chisq.test(tbl, simulate.p.value = TRUE)
  if (any(chi2_test$expected < 5)) {
    cat("Utilisation du test exact de Fisher en raison des effectifs faibles.\n")
    test <- fisher.test(tbl)
  } else {
    test <- chi2_test
  }
  return(test)
}

# Liste pour stocker les résultats
resultats_tests <- list()

# Effectuer le test pour chaque facteur
for (facteur in v_ex) {
  cat("\nTest pour", facteur, ":\n")
  test_result <- test_independance(db$anxiete, db[[facteur]])
  resultats_tests[[facteur]] <- test_result
  print(test_result)
}

# Résumé des résultats
cat("\nRésumé des résultats des tests:\n")
for (facteur in names(resultats_tests)) {
  cat("\nVariable:", facteur, "\n")
  print(resultats_tests[[facteur]]$p.value)
}

## VARIABLE blue du post patrum #########3
###############################################

test_independance <- function(outcome, factor) {
  tbl <- table(outcome, factor)
  
  # Vérifier si les effectifs théoriques sont suffisants pour utiliser le chi-carré
  chi2_test <- chisq.test(tbl, simulate.p.value = TRUE)
  if (any(chi2_test$expected < 5)) {
    cat("Utilisation du test exact de Fisher en raison des effectifs faibles.\n")
    test <- fisher.test(tbl)
  } else {
    test <- chi2_test
  }
  return(test)
}

# Liste pour stocker les résultats
resultats_tests <- list()

# Effectuer le test pour chaque facteur
for (facteur in v_ex) {
  cat("\nTest pour", facteur, ":\n")
  test_result <- test_independance(db$`Blue_du_post-partum`, db[[facteur]])
  resultats_tests[[facteur]] <- test_result
  print(test_result)
}

# Résumé des résultats
cat("\nRésumé des résultats des tests:\n")
for (facteur in names(resultats_tests)) {
  cat("\nVariable:", facteur, "\n")
  print(resultats_tests[[facteur]]$p.value)
}

## VARIABLE diffi_adaptation_liées_histo_ou_cont_vie #########3
###############################################

test_independance <- function(outcome, factor) {
  tbl <- table(outcome, factor)
  
  # Vérifier si les effectifs théoriques sont suffisants pour utiliser le chi-carré
  chi2_test <- chisq.test(tbl, simulate.p.value = TRUE)
  if (any(chi2_test$expected < 5)) {
    cat("Utilisation du test exact de Fisher en raison des effectifs faibles.\n")
    test <- fisher.test(tbl)
  } else {
    test <- chi2_test
  }
  return(test)
}

# Liste pour stocker les résultats
resultats_tests <- list()

# Effectuer le test pour chaque facteur
for (facteur in v_ex) {
  cat("\nTest pour", facteur, ":\n")
  test_result <- test_independance(db$diffi_adaptation_liées_histo_ou_cont_vie, db[[facteur]])
  resultats_tests[[facteur]] <- test_result
  print(test_result)
}

# Résumé des résultats
cat("\nRésumé des résultats des tests:\n")
for (facteur in names(resultats_tests)) {
  cat("\nVariable:", facteur, "\n")
  print(resultats_tests[[facteur]]$p.value)
}

############################## ANALYSE MULTI ######################
###################################################################