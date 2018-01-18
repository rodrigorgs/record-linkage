rm(list=ls())

MIN_CONFIDENCE <- 75
INPUT <- 'tent2/jaro_winkler-4_0.1.csv'

library(readr)
library(dplyr)
library(pander)

ufba <- read_delim('ufbadb-sample.csv', ';')
ldap <- read_delim('ldap-sample.csv', ';')
any(duplicated(ldap$cpf))
any(duplicated(ufba$pef_nu_cpf))
any(is.na(ufba$pes_nm_pessoa))
any(is.na(ldap$displayName))

# pre_tent <- read_csv('tent2/jaro_winkler-4_0.1.csv')
# pre_tent <- read_csv('tent2/EditDistance_a0_p1_acc75.csv')
# pre_tent <- read_csv('tent2/EditDistance_a0_p1_acc80.csv')
pre_tent <- read_csv(INPUT)

colnames(pre_tent) <- tolower(colnames(pre_tent))

pre_tent <- pre_tent %>% filter(confidence >= MIN_CONFIDENCE)

any(duplicated(pre_tent$`cpf@ldap`))
any(duplicated(pre_tent$`pef_nu_cpf@ufbadb`))

stopifnot(0 == sum(!(pre_tent$`cpf@ldap` %in% ldap$cpf)))
stopifnot(0 == sum(!(pre_tent$`pef_nu_cpf@ufbadb` %in% ufba$pef_nu_cpf)))

tent <- pre_tent

# Filtra links duplicados
# tent <- pre_tent %>%
#   arrange(desc(Confidence), desc(`cpf@ldap`)) %>%
#   filter(!duplicated(`pef_nu_cpf@ufbadb`))
# any(duplicated(tent$`cpf@ldap`))
# any(duplicated(tent$`pef_nu_cpf@ufbadb`))

# Monta tabelão da predição

pred <- ldap %>%
  left_join(tent, by=c("cpf" = "cpf@ldap")) %>%
  right_join(ufba, by=c("pef_nu_cpf@ufbadb" = "pef_nu_cpf")) %>%
  select(cpf_ldap = cpf,
         nome_ldap = displayName,
         cpf_ufba = `pef_nu_cpf@ufbadb`,
         nome_ufba = `pes_nm_pessoa@ufbadb`,
         confidence = confidence) %>%
  mutate(ldap_linkavel = cpf_ldap %in% cpf_ufba,
         ufba_linkavel = cpf_ufba %in% cpf_ldap,
         is_match = cpf_ldap == cpf_ufba,
         is_match = ifelse(is.na(is_match), FALSE, is_match),
         is_predicted = !is.na(confidence))

# Monta tabela de links corretos
cpfs_correctly_predicted <- pred %>%
  filter(is_predicted & is_match) %>%
  .$cpf_ldap
matches <- ldap %>%
  inner_join(ufba, by=c("cpf" = "pef_nu_cpf")) %>%
  mutate(is_pred = cpf %in% cpfs_correctly_predicted)

# Computa métricas

n_ufba <- sum(!is.na(pred$cpf_ldap))
n_ldap <- sum(!is.na(pred$cpf_ufba))
n_total_links <- n_ufba * n_ldap
n_links_true <- sum(pred$ldap_linkavel)
n_links_false <- n_total_links - n_links_true
n_links_pred <- nrow(tent)
n_true_positives <- sum(pred$is_predicted & pred$is_match)
n_false_positives <- sum(pred$is_predicted & !pred$is_match)
n_false_negatives <- sum(!matches$is_pred)
n_true_negatives <- n_total_links - (n_true_positives + n_false_positives + n_false_negatives)
# abreviação
tp <- n_true_positives
tn <- n_true_negatives
fp <- n_false_positives
fn <- n_false_negatives
matrix(data=c(tp, fn, fp, tn), byrow=T, ncol=2)
# outras
precision <- tp / (tp + fp) # also called positive predictive value
recall <- tp / (tp + fn) # also called sensitivity or TP rate
specificity <- tn / (tn + fp) # also called TN rate
fp_rate <- fp / (tp + fp)
accuracy <- (tp+tn)/(tp+fp+tn+fn)
f_score <- 2*(precision*recall)/(precision+recall)
#fn_rate <- fn / (tp + fn)


data.frame(precision = precision,
           recall = recall,
           specificity = specificity,
           fp_rate = fp_rate,
           accuracy = accuracy,
           f_score = f_score
)

vinculos_duplicados <- sum(duplicated(pre_tent$`cpf@ldap`)) +
  sum(duplicated(pre_tent$`pef_nu_cpf@ufbadb`))

vinculos_duplicados / n_links_pred

############
# Datasets para inspecionar
set.seed(0)
falso_negativos <- matches %>%
  filter(!is_pred)

if (nrow(falso_negativos) > 0)
  falso_negativos %>% sample_n(20, replace=T) %>% pander()

falso_positivos <- pred %>%
  filter(is_predicted & !is_match)

if (nrow(falso_positivos) > 0)
  falso_positivos %>% sample_n(20, replace=T) %>% pander()

normalize <- function(x) { trimws(toupper(x));  }
verdadeiro_positivos_diferentes <- pred %>%
  filter(is_match & normalize(nome_ldap) != normalize(nome_ufba)) %>%
  mutate(nome_ldap = paste0("[", nome_ldap, "]"),
         nome_ufba = paste0("[", nome_ufba, "]"))

if (nrow(verdadeiro_positivos_diferentes) > 0)
  verdadeiro_positivos_diferentes %>% sample_n(20, replace=T) %>% pander()
