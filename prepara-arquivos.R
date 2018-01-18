#' Para instalar os pacotes, execute:
#' install.packages('dplyr')
#' install.packages('readr')

library(readr)
library(dplyr)

set.seed(0)

TAM_ESTRATO <- 5000

#' Lê LDAP
ldapOriginal <- read_delim("ldap.csv", delim=";")

#' Seleciona registros com CPFs que não aparecem duplicados
ldap <- ldapOriginal %>%
  filter(nchar(displayName) > 0) %>%
  group_by(cpf) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  select(cpf, displayName)

#' Lê UFBADB
ufbadbOriginal <- read_delim("ufbadb.csv", delim=",")

#' Seleciona registros com CPF
ufbadb <- ufbadbOriginal %>%
  filter(nchar(pes_nm_pessoa) > 0) %>%
  filter(!is.na(pef_nu_cpf)) %>%
  select(pef_nu_cpf, pes_nm_pessoa)


#' Faz join
data <- ldap %>%
  full_join(ufbadb, by = c("cpf" = "pef_nu_cpf"))

ambosSample <- data %>%
  filter(!is.na(displayName) & !is.na(pes_nm_pessoa)) %>%
  sample_n(TAM_ESTRATO)

ldapSample <- data %>%
  filter(!is.na(displayName) & is.na(pes_nm_pessoa)) %>%
  sample_n(937) %>%
  rbind(ambosSample) %>%
  select(cpf, displayName)

ufbadbSample <- data %>%
  filter(is.na(displayName) & !is.na(pes_nm_pessoa)) %>%
  sample_n(TAM_ESTRATO) %>%
  rbind(ambosSample) %>%
  select(pef_nu_cpf = cpf, pes_nm_pessoa)

#' Salva arquivos
write_delim(ldapSample, 'ldap-sample.csv', delim=';')
write_delim(ufbadbSample, 'ufbadb-sample.csv', delim=';')

