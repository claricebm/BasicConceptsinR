##Loading packages
library(readxl)
library(tidyverse)

##Importing datasets

#data on land use in the city of Rio de Janeiro
uso_solo_rio<-read_excel("Classes de Uso do solo e Cobertura Vegetal - RJ.xlsx",sheet = "Dados")

#data on the Protected Areas in the city of Rio de Janeiro
aps_rio<-read_excel("Areas_Protegidas_Rio.xlsx")

##Exploring the data
glimpse(uso_solo_rio)
glimpse(aps_rio)

##Relocating one column
uso_do_solo_categorias<-relocate(uso_solo_rio,Reflorestamento,.after = `Afloramento Rochoso`)

##Relocating columns without pipelines
uso_do_solo_categorias<-relocate(uso_solo_rio,Reflorestamento,.after = `Afloramento Rochoso`)

uso_do_solo_categorias_realocado<-relocate(uso_do_solo_categorias,`Bairros Rio de Janeiro`,.after = `Praia`)

##Relocating columns with pipelines
uso_do_solo_categorias_realocado<-uso_solo_rio%>%
  relocate(Reflorestamento,.after = `Afloramento Rochoso`)%>%
  relocate(`Bairros Rio de Janeiro`,.after = `Praia`)

##Summing the different categories of land use into three big groups: Natural Vegetation + Antropisms + Continental water bodies. By the end, one additional column was created, being the sum of all these three big groups
estimativa_uso_do_solo<-uso_do_solo_categorias%>%
  mutate(Vegetacao_natural = rowSums(pick(`Floresta Ombrófila Densa`:Brejo)),.before = `Floresta Ombrófila Densa`)%>%
  mutate(Antropismos =  rowSums(pick(`Área Urbana`:Reflorestamento)),.before = `Área Urbana`)%>%
  mutate(Corpos_dagua_continental = rowSums(pick(`Corpo d'água continental`:Praia)),.before = `Corpo d'água continental`)%>%
  mutate(Uso_do_solo = rowSums(pick(Vegetacao_natural,Antropismos,Corpos_dagua_continental)),.before = Vegetacao_natural)

##Tidying land use data
uso_do_solo_area_planejamento_rj_tidy<-estimativa_uso_do_solo%>%
  filter(`Bairros Rio de Janeiro` != "Lapa")%>%
  select(`Área de Planejamento (AP)`,Uso_do_solo,Vegetacao_natural,Antropismos,Corpos_dagua_continental)

##Calculating the amount of the three big groups in each Planning region of the city
soma_uso_do_solo_area_planejamento_rj<-uso_do_solo_area_planejamento_rj_tidy%>%
  group_by(`Área de Planejamento (AP)`)%>%
  summarise(across(everything(),list(sum)))

##Transforming these data into percentages
porcentagens_uso_do_solo_area_planejamento_rj<-soma_uso_do_solo_area_planejamento_rj%>%
  mutate(Vegetacao_natural_Porcentagem = (Vegetacao_natural_1/Uso_do_solo_1)*100)%>%
  mutate(Antropismos_Porcentagem = (Antropismos_1/Uso_do_solo_1)*100)%>%
  mutate(Corpos_dagua_Porcentagem = (Corpos_dagua_continental_1/Uso_do_solo_1)*100)%>%
  mutate(across(where(is.numeric), round, 2))%>%
  select(`Área de Planejamento (AP)`,Vegetacao_natural_Porcentagem:Corpos_dagua_Porcentagem)

##Working with the Protected Areas data: calculating how many PAs are in each Planning region of the city
n_areas_protecao_zonas_rj<-aps_rio%>%
  select(area_plane,nome_1)%>%
  distinct(area_plane,nome_1)%>%
  group_by(area_plane)%>%
  count(nome_1)%>%
  summarise(n = sum(n))%>%
  rename("Área de Planejamento (AP)" = "area_plane",
         "Nº de Áreas de Proteção" = "n")

##Integrating all previous information
dados_integrados<-n_areas_protecao_zonas_rj%>%
  left_join(porcentagens_uso_do_solo_area_planejamento_rj,by="Área de Planejamento (AP)")

##What this script should actually look like:

#PAs data
n_areas_protecao_zonas_rj<-aps_rio%>%
  select(nome,area_plane,nome_1)%>%
  distinct(area_plane,nome_1)%>%
  group_by(area_plane)%>%
  count(nome_1)%>%
  summarise(n = sum(n))%>%
  rename("Área de Planejamento (AP)" = "area_plane",
         "Nº de Áreas de Proteção" = "n")

#land use data
dados_integrados_script_completo<-uso_solo_rio%>%
  relocate(Reflorestamento,.after = `Afloramento Rochoso`)%>%
  mutate(Vegetacao_natural = rowSums(pick(`Floresta Ombrófila Densa`:Brejo)),.before = `Floresta Ombrófila Densa`)%>%
  mutate(Antropismos =  rowSums(pick(`Área Urbana`:Reflorestamento)),.before = `Área Urbana`)%>%
  mutate(Corpos_dagua_continental = rowSums(pick(`Corpo d'água continental`:Praia)),.before = `Corpo d'água continental`)%>%
  mutate(Uso_do_solo = rowSums(pick(Vegetacao_natural,Antropismos,Corpos_dagua_continental)),.before = Vegetacao_natural)%>%
  filter(`Bairros Rio de Janeiro` != "Lapa")%>%
  select(`Área de Planejamento (AP)`,Uso_do_solo,Vegetacao_natural,Antropismos,Corpos_dagua_continental)%>%
  group_by(`Área de Planejamento (AP)`)%>%
  summarise(across(everything(),list(sum)))%>%
  mutate(Vegetacao_natural_Porcentagem = (Vegetacao_natural_1/Uso_do_solo_1)*100)%>%
  mutate(Antropismos_Porcentagem = (Antropismos_1/Uso_do_solo_1)*100)%>%
  mutate(Corpos_dagua_Porcentagem = (Corpos_dagua_continental_1/Uso_do_solo_1)*100)%>%
  mutate(across(where(is.numeric), round, 2))%>%
  select(`Área de Planejamento (AP)`,Vegetacao_natural_Porcentagem:Corpos_dagua_Porcentagem)%>%
  left_join(n_areas_protecao_zonas_rj,by="Área de Planejamento (AP)")

##How exactly is the land use in each neighbourhood in Rio?
porcentagens_totais_uso_do_solo<-estimativa_uso_do_solo%>%
  select(`Bairros Rio de Janeiro`:Vegetacao_natural,Antropismos,Corpos_dagua_continental)%>%
  mutate(Vegetacao_natural_Porcentagem = (Vegetacao_natural/Uso_do_solo)*100)%>%
  mutate(Antropismos_Porcentagem = (Antropismos/Uso_do_solo)*100)%>%
  mutate(Corpos_dagua_Porcentagem = (Corpos_dagua_continental/Uso_do_solo)*100)%>%
  filter(`Bairros Rio de Janeiro` != "Lapa")%>%
  mutate(across(where(is.numeric), round, 2))%>%
  select(-(Uso_do_solo:Corpos_dagua_continental))

porcentagens_uso_do_solo_bairros_rj<-porcentagens_totais_uso_do_solo%>%
  select(`Bairros Rio de Janeiro`,Vegetacao_natural_Porcentagem:Corpos_dagua_Porcentagem)%>%
  arrange(`Bairros Rio de Janeiro`)

