library(readxl)
library(tidyverse)
# library(geobr)
library(lubridate)
library(incidence)
library(beepr)

setwd("Z:/laura/naturehealth")
source("code/functionsWeatherExtraction.R")
write.dir = "analysis_dfs/"

#Import linelist, muni data ------
full <- readxl::read_excel("data/additional_data_files/bd_OROV_positve&negative_2025-09-23 v.2.xlsx",
                            col_types   = c(
                              "text", "date",
                              rep("text",4),
                              "numeric",  "text","numeric",  "text")
                           )

muni_pops <- read.csv("data/additional_data_files/population_municipalities_2022.csv") |> subset(select = -c(mosturban5leasturban1_2010))
muni_pops$six_digit_id <- as.character(muni_pops$six_digit_id)

full <- left_join(full, muni_pops, join_by(co_municipio == six_digit_id))



full %>% rename(
  # individual_code_maybe = co_seq_examereq,
  # req_st_code = co_uf_requisicao,
  requisition_code = co_requisicao,
  # dt_registratgion = dt_cadastro,
  # dt_request = dt_solicitacao,
  dt_collection = dt_coleta,
  # dt_symp = dt_sintomas,
  # sample_code = co_amostra,
  # sample_material = co_matbio,
  # age_type = tp_idade,
  # age_numeric = nu_idade,
  # dateofbirth = dt_nascimento,
  sex = co_sexo,
  # race_code = co_raca,
  six_digit_id = co_municipio,
  # nationality_code = co_nacionalidade,
  # ethnicity_code = co_etnia,
  # country_code = co_pais,
  # zone_code = co_zona,
  # state_ab = co_estado,
  # test_method_specific = exame_metodo,
  # test_group_maybe = ds_agrupamento_exame,
  # oro_result = aux_resultado,
  # test_method_general = ds_metodo_agrupado,
  # test_method_type = ds_metodo_agrupado_tecnica
) %>% 
  mutate(oro_result_tf = case_match(
  aux_resultado,
  "Negativo" ~ FALSE ,
  "Positivo" ~ TRUE)) |> 
  subset(!is.na(six_digit_id)) -> full
full$dt_collection <- as.Date(full$dt_collection)

## add season etc.-----
full$year <- year(full$dt_collection)
full$month <- month(full$dt_collection)
full$week <- week(full$dt_collection)

full$season <- NA;full$sea_plot_date <- NA
for(y in (min(full$year)-1):(max(full$year)+1)){
  full$season[full$dt_collection >= as.Date(paste0(y-1,"-07-01")) &
                full$dt_collection <= as.Date(paste0(y,"-06-30"))] <- paste0(y-1,"-",y)
  full$sea_plot_date[full$dt_collection >= as.Date(paste0(y-1,"-07-01")) &
                       full$dt_collection <= as.Date(paste0(y,"-06-30"))] <- paste0(y,"-01-01")
}
full$season <- as.factor(full$season)

#save csv with only positive tests----
cases <- full |> 
  subset(oro_result_tf == TRUE) |> 
  subset(select = -c(aux_virus,aux_resultado))

write.csv(cases, paste0("data/additional_data_files/linelist_2025-09-23_positive.csv"), row.names = FALSE)

#Determine isolated vs outbreak, write csv -----
cases <- as.data.frame(cases)
cases$code_muni <- as.factor(cases$code_muni)
muni_inc <- longFormatIncidence(cases, group_for_incidence = "code_muni", int = "day")
muni_periods <- transmissionPeriodsManyUnits(muni_inc,cases,geog_column = "code_muni", minimum_total_cases_for_period = 3, units_for_gap = 21)

per <- muni_periods$periods |> 
  subset(st_date < as.Date("2025-08-01"))

write.csv(per, paste0(write.dir,"outbreaks_through_2025-07-31.csv"), row.names = FALSE)

