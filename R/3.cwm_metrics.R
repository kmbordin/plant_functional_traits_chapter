#precisa carregar esse script antes
here::here("1.load_harmonise.R")

# frequencia de  traits usados para calcular o cwm
cwm.site <- function (x) {
  data <- x %>% 
    mutate(ecosystem = replace(ecosystem, ecosystem == "forest", "Forest")) %>%
    mutate(ecosystem = replace(ecosystem, ecosystem == "grassland", "Grassland")) %>%
    filter(ecosystem != "ecotones") %>%
    select(CWM_LA: CWM_carbon13) %>% 
    rename_all(funs(stringr::str_replace_all( ., "CWM_", ""))) %>% #remove o CWM de todos os titulos
    pivot_longer(everything(), names_to = c("Variables"), values_to = "Valores") %>% #pivota as colunas para linhas
    mutate(Valores = replace(Valores, Valores == "ns", "No relationship")) %>%
    mutate(Valores = replace(Valores, Valores == "positive", "Positive")) %>%
    mutate(Valores = replace(Valores, Valores == "negative", "Negative")) %>%
    group_by(Variables, Valores) %>%
    summarize(`Number of papers` = n()) %>% 
    drop_na(Valores) %>% 
    filter(Valores != "positive(temperate),negative(subtropical)") %>% 
    return(data)
}

# ifelse para mudar o efeito de LMA para SLA (LMA=1/SLA)
data$CWM_LMA <- ifelse(data$CWM_LMA == "negative", "MUDAR", data$CWM_LMA) 
data$CWM_LMA <- ifelse(data$CWM_LMA == "positive", "negative", data$CWM_LMA)
data$CWM_LMA <- ifelse(data$CWM_LMA == "MUDAR", "positive", data$CWM_LMA)

#filta as infos somente para campo
grass <- data %>% 
  filter (ecosystem=="grassland") %>%
  cwm.site() %>% 
  mutate(ecosystem = "Grassland") 

#filtra as infos para floresta
fores <- data %>% 
  filter (ecosystem=="forest") %>% 
  cwm.site() %>% 
  mutate(ecosystem = "Forest")

#efeito dos cwm para todos os dois ecossistemas (total)
all <-  data %>% 
  cwm.site() %>% 
  select(-Valores) %>% 
  mutate(Variables = replace(Variables, Variables == "LMA" , "SLA"))%>%
  summarise(`Number of papers` = sum(`Number of papers`)) %>% 
  mutate(Variables = replace(Variables, Variables=="height" , "Height")) %>%
  mutate(Variables = replace(Variables, Variables=="LCC_LA" , "LCC:LA")) %>%
  mutate(Variables = replace(Variables, Variables=="Chlorophyll" , "Pigments")) %>%
  mutate(Variables = replace(Variables, Variables=="seed.mass" , "Seed size")) %>%
  mutate(Variables = replace(Variables, Variables=="LCC_LNC" , "LCC:LNC")) %>%
  mutate(Variables = replace(Variables, Variables=="leaf.tickness" , "LT")) %>%
  mutate(Variables = replace(Variables, Variables=="root.mass" , "Root quantity")) %>%
  filter(`Number of papers` >= 3) %>% 
  mutate(frequencia = round((`Number of papers`/sum(`Number of papers`) * 100), digits = 0)) %>% 
  rename(`Frequency (%)` = frequencia) %>% 
  arrange(desc(`Number of papers`)) %>% 
  gt() %>% 
  tab_header(title = md("**Functional dominance as predictor of productivity**"),
             subtitle = "Frequency and number of mentions of functional dominance effects on productivity") %>% 
  tab_style(style = cell_fill(color = "gray90"),
            locations = cells_column_labels(columns = everything())) %>% 
  cols_align(align = "center",columns = everything()) #%>% 
  #gtsave(filename = "results/CWM_effects_semecosys.rtf")

dados_contagem <- bind_rows(grass,fores) %>% 
  mutate(Variables = str_remove(Variables, "CWM_")) %>% 
  mutate(Variables = str_remove(Variables, "CWM ")) %>% 
  mutate(Variables = replace(Variables, Variables  %in% lma , "SLA"))%>% # SLA como 1/LMA
  mutate(Variables = replace(Variables, Variables  %in% height , "Height")) %>%
  mutate(Variables = replace(Variables, Variables  %in% seed , "Seed size")) %>%
  mutate(Variables = replace(Variables, Variables  %in% crown , "Crown size")) %>%
  mutate(Variables = replace(Variables, Variables  %in% tree.size , "Tree size")) %>%
  mutate(Variables = replace(Variables, Variables  %in% lcc , "LCC")) %>%
  mutate(Variables = replace(Variables, Variables  %in% root , "Root quantity")) %>%
  mutate(Variables = replace(Variables, Variables  %in% lt , "LT")) %>%
  mutate(Variables = replace(Variables, Variables  %in% vessel , "Vessel quantity")) %>%
  mutate(Variables = replace(Variables, Variables  %in% pigment , "Pigments")) %>%
  mutate(Variables = replace(Variables, Variables  %in% n.p , "LNC:LPC")) %>%
  mutate(Variables = replace(Variables, Variables  %in% lpc , "LPC")) %>%
  mutate(Variables = replace(Variables, Variables  %in% tolerance , "Tolerance")) %>%
  mutate(Variables = replace(Variables, Variables  %in% growth , "Growth rate")) %>%
  mutate(Variables = replace(Variables, Variables  %in% na , "")) %>%
  mutate(Variables = replace(Variables, Variables == "age", "Age")) %>% 
  mutate(Variables = replace(Variables, Variables == "LCC_LA", "LCC:LA")) %>% 
  mutate(Variables = replace(Variables, Variables == "LCC_LNC", "LCC:LNC")) %>% 
  filter(Variables != "Age") %>% 
  group_by(ecosystem, Variables, Valores) %>% 
  summarise(`Number of papers` = sum(`Number of papers`))

# aqui gera a matriz de todos os traits usados para calcular o cwm, com uma linha final de total
dados_contagem <- dados_contagem %>%
  #group_by(ecosystem) %>% 
  group_by(Variables) %>% 
  #mutate(frequencia = round((`Number of papers`/sum(`Number of papers`) * 100), digits = 1)) %>% 
  mutate(frequencia = `Number of papers`) %>% 
  #ungroup() %>% 
  select(Variables, Valores, ecosystem, frequencia) %>% 
  rename(Relationship = Valores, 
         Ecosystem = ecosystem) %>% 
  pivot_wider(names_from = Variables, values_from = frequencia) %>% 
  select(order(colnames(.))) %>% 
  relocate(Ecosystem, .before = CaC) %>% 
  relocate(Relationship, .after = Ecosystem) %>% 
  replace(is.na(.), 0) %>% 
  add_row(Ecosystem = "Total", Relationship = "Total", summarise(., across(where(is.numeric), sum)))

# aqui seleciona apenas os traits que foram citados 3 ou mais vezes
traits.selected = dados_contagem %>% filter (Ecosystem == "Total") %>% 
  select_if(~any(. >= 3)) %>% 
  colnames()

#voltar para a matriz anterior e filtra somente os traits selecionados
dados_contagem <- dados_contagem %>% 
  select(matches(traits.selected)) %>% 
  filter(row_number() <=n()-1) %>% #remove a ultima linha
  arrange(Ecosystem,Relationship)

#numero de papers que citam os cwm e a relacao encontrada
count_cwm <- dados_contagem %>% 
  group_by(Ecosystem) %>% 
  select(-Relationship) %>% 
  #mutate_all(funs((./sum(.))*100)) %>% 
  mutate_if(is.numeric, round, 0)  %>% 
  ungroup() %>% 
  mutate(Relationship = dados_contagem$Relationship) %>% 
  relocate(Relationship, .after = Ecosystem) %>% 
  select(-`LNC:LPC`)

gt(count_cwm) %>%  
  tab_header(title = md("**Functional dominance as predictor of productivity**"),
             subtitle = "Frequency and number of mentions of functional dominance effects on productivity") %>% 
  tab_style(style = cell_fill(color = "gray90"),
            locations = cells_column_labels(columns = everything())) %>% 
  cols_align(align = "center",columns = everything()) #%>% 
  #gtsave(filename = "results/CWM_effects_ecosys.rtf")


