#precisa carregar esse script antes
here::here("1.load_harmonise.R")

# frequencia de  traits usados para calcular o cwm
cwm.site <- function (x) {
  data <- x %>% 
    mutate(ecosystem = replace(ecosystem, ecosystem == "forest", "Forest")) %>%
    mutate(ecosystem = replace(ecosystem, ecosystem == "grassland", "Grassland")) %>%
    filter(ecosystem != "ecotones") %>%
    select(CWM_LA: CWM_carbon13,regiao) %>% 
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

regiao_estudo = c("temperate", "tropical", "subtropical")
dados <- data %>% filter(regiao %in% regiao_estudo)
#filta as infos somente para campo
grass_prod <- dados %>% 
  filter (ecosystem=="grassland") %>%
  filter (prod.metric=="1") %>%
  mutate(regiao = replace(regiao, regiao == "subtropical", "tropical")) %>%
  cwm.site() %>% 
  mutate(ecosystem = "Grassland") %>% 
  mutate(metric = "produc")
grass_stock <- dados %>% 
  filter (ecosystem=="grassland") %>%
  filter (prod.metric=="2") %>%
  mutate(regiao = replace(regiao, regiao == "subtropical", "tropical")) %>%
  cwm.site() %>% 
  mutate(ecosystem = "Grassland") %>% 
  mutate(metric = "stock")

#filtra as infos para floresta
fores_prod <- dados %>% 
  filter (ecosystem=="forest") %>% 
  filter (prod.metric=="1") %>%
  mutate(regiao = replace(regiao, regiao == "subtropical", "tropical")) %>%
  cwm.site() %>% 
  mutate(ecosystem = "Forest") %>% 
  mutate(metric = "produc")

fores_stock <- dados %>% 
  filter (ecosystem=="forest") %>% 
  filter (prod.metric=="2") %>%
  mutate(regiao = replace(regiao, regiao == "subtropical", "tropical")) %>%
  cwm.site() %>% 
  mutate(ecosystem = "Forest") %>% 
  mutate(metric = "stock")

#efeito dos cwm para todos os dois ecossistemas (total)
all = data %>% 
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

dados_contagem <- #bind_rows(grass,fores) %>% 
  bind_rows(grass_prod, grass_stock,fores_prod,fores_stock) %>%  #para regiaocomeçar aqui#
  mutate(Variables = str_remove(Variables, "CWM_")) %>% 
  mutate(Variables = str_remove(Variables, "CWM ")) %>% 
  mutate(Valores = replace(Valores, Valores =="negativa ", "Negative")) %>%
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
  group_by(ecosystem, Variables, Valores, metric) %>% 
  summarise(`Number of papers` = sum(`Number of papers`))

# aqui gera a matriz de todos os traits usados para calcular o cwm, com uma linha final de total
dados_contagem <- dados_contagem %>%
  #group_by(ecosystem) %>% 
  #group_by(Variables) %>% 
  #mutate(frequencia = round((`Number of papers`/sum(`Number of papers`) * 100), digits = 1)) %>% 
  mutate(frequencia = `Number of papers`) %>% 
  #ungroup() %>% 
  select(Variables, Valores, ecosystem, frequencia) %>% 
  rename(Relationship = Valores, 
         Ecosystem = ecosystem) %>% 
  pivot_wider(names_from = Variables, values_from = frequencia) %>% 
  select(order(colnames(.))) %>% 
  relocate(Ecosystem) %>% 
  relocate(Relationship, .after = Ecosystem) %>% 
  replace(is.na(.), 0) %>% 
  add_row(Ecosystem = "Total", Relationship = "Total", summarise(., across(where(is.numeric), sum)))

# aqui seleciona apenas os traits que foram citados 3 ou mais vezes
traits.selected <- dados_contagem %>% filter (Ecosystem == "Total") %>% 
  select_if(~any(. >= 3)) %>% 
  colnames()

#voltar para a matriz anterior e filtra somente os traits selecionados
forest = dados_contagem %>% 
  filter(Ecosystem =="Forest") %>% 
  add_row(Ecosystem = "Forest", Relationship = "Total", .after = 3, summarise(., across(where(is.numeric), sum))) %>% 
  select(matches(traits.selected)) %>% 
  slice(4)
grassland = dados_contagem %>% 
  filter(Ecosystem =="Grassland") %>% 
  add_row(Ecosystem = "Grassland", Relationship = "Total", .after = 3, summarise(., across(where(is.numeric), sum))) %>% 
  select(matches(traits.selected)) %>% 
  slice(4)

dados_contagem <- dados_contagem %>% 
  select(matches(traits.selected)) %>% 
  add_row(forest, .after = 3) %>% 
  add_row(grassland, .after = 7)  %>% 
  #filter(row_number() <=n()-1) %>% #remove a ultima linha
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
             subtitle = "Number of mentions of functional dominance effects on productivity") %>% 
  tab_style(style = cell_fill(color = "gray90"),
            locations = cells_column_labels(columns = everything())) %>% 
  cols_align(align = "center",columns = everything()) #%>% 
  #gtsave(filename = "results/CWM_effects_ecosys.rtf")

num = count_cwm %>% 
  select(Ecosystem, Relationship, Height, SLA, LDMC, LNC, LPC,`Root quantity`,WD) %>% 
  slice(c(1:3,5:7)) %>% 
  group_by(Ecosystem) %>% 
  reshape2:: melt ()
cwm_rel <- count_cwm %>% 
  select(Ecosystem, Relationship, Height, SLA, LDMC, LNC, LPC,`Root quantity`,WD) %>% 
  slice(c(1:3,5:7)) %>% 
  group_by(Ecosystem) %>% 
  mutate(Height = round((Height / sum(Height)*100), digits = 0)) %>% 
  mutate(SLA = round((SLA / sum(SLA)*100), digits = 0)) %>% 
  mutate(LDMC = round((LDMC / sum(LDMC)*100), digits = 0)) %>% 
  mutate(LNC = round((LNC / sum(LNC)*100), digits = 0)) %>% 
  mutate(LPC = round((LPC / sum(LPC)*100), digits = 0)) %>% 
  mutate(`Root quantity` = round((`Root quantity` / sum(`Root quantity`)*100), digits = 0)) %>% 
  mutate(WD = round((WD / sum(WD)*100), digits = 0)) %>% 
  reshape2:: melt () %>% 
  rename(Trait = variable,
         frequencia = value) %>% 
  replace(is.na(.), 0) %>% 
  mutate(count = num$value) %>% 
  mutate_at(c('count'), ~na_if(., 0))



#png("results/CWM_effect_ecosystem.png", units="in", width=11, height=9, res=300)
cwm_rel%>% 
  ggplot(aes(x=Ecosystem, y=frequencia, fill=Relationship))+geom_bar(stat= "identity") +
  labs(x = "", y = "Frequency of papers (%)", title = "Effect of functional dominance on productivity") + 
  scale_fill_manual(values = c("#44AA99","#888888","#AA4499"),guide = guide_legend(
    direction = "horizontal",
    title.position = "top",title.hjust = 0.5))+ 
  facet_grid(facets = ~(Trait), scales="free") + 
  geom_text(aes(label=count), vjust=-0.5, hjust=0, position=position_stack(vjust=0), colour="black", size=5) +
  theme_minimal()  +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5,size = 15, face = "bold"),
        axis.text.x = element_text(size=12),
        axis.title.y =element_text(size=15), 
        axis.title.x =element_text(size=15), 
        strip.text.x = element_text(size = 14))
#dev.off()

reg = c("tropical", "temperate", "subtropical")
n.data = data%>% 
  mutate(ecosystem = replace(ecosystem, ecosystem == "forest", "Forest")) %>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "grassland", "Grassland")) %>%
  filter(ecosystem != "ecotones") %>%
  select(CWM_LA: CWM_carbon13,regiao,prod.metric) %>% 
  filter(regiao %in% reg) %>% 
  mutate(regiao = replace(regiao, regiao == "subtropical", "tropical")) %>% 
  filter (prod.metric != "1,2")

trop = filter (n.data, regiao =="tropical")%>%
  #filter (prod.metric == 1) %>% #incluir esses filtros para os estoques ou npp
  #filter (prod.metric == 2) %>% #incluir esses filtros para os estoques ou npp
  rename_all(funs(stringr::str_replace_all( ., "CWM_", ""))) %>% #remove o CWM de todos os titulos
  pivot_longer(everything(), names_to = c("Variables"), values_to = "Valores") %>% #pivota as colunas para linhas
  mutate(Valores = replace(Valores, Valores == "ns", "No relationship")) %>%
  mutate(Valores = replace(Valores, Valores == "positive", "Positive")) %>%
  mutate(Valores = replace(Valores, Valores == "negative", "Negative")) %>%
  filter(Valores != "positive(temperate),negative(subtropical)") %>% 
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
  group_by(Variables, Valores) %>%
  summarize(`Number of papers` = n()) %>% 
  drop_na(Valores) %>% 
  filter (Variables != "prod.metric") %>% 
  filter (Variables != "regiao") %>% 
  group_by(Variables) %>% 
  mutate(frequencia = round((`Number of papers`/sum(`Number of papers`) * 100), digits = 0))  %>% 
  mutate(Region = "Tropical")
  
  
temp = filter (n.data, regiao == "temperate")%>%
  #filter (prod.metric ==1) %>% npp
  #filter (prod.metric ==2) %>% #estoques
  pivot_longer(everything(), names_to = c("Variables"), values_to = "Valores") %>% #pivota as colunas para linhas
  mutate(Valores = replace(Valores, Valores == "ns", "No relationship")) %>%
  mutate(Valores = replace(Valores, Valores == "positive", "Positive")) %>%
  mutate(Valores = replace(Valores, Valores == "negative", "Negative")) %>%
  filter(Valores != "positive(temperate),negative(subtropical)") %>% 
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
  filter (Variables != "prod.metric") %>% 
  filter (Variables != "regiao")  %>% 
  group_by(Variables, Valores) %>%
  summarize(`Number of papers` = n()) %>% 
  drop_na(Valores) %>% 
  group_by(Variables) %>% 
  mutate(frequencia = round((`Number of papers`/sum(`Number of papers`) * 100), digits = 0))  %>% 
  mutate(Region = "Temperate")  
  
traits = c("Height", "SLA", "LDMC", "LNC", "LPC", "Root quantity", "WD")

both = bind_rows(trop,temp) %>% 
  rename(Relationship = Valores) %>% 
  filter (Variables %in% traits)

prod  =  bind_rows(trop,temp) %>% 
  rename(Relationship = Valores) %>% 
  filter (Variables %in% traits)
estoque = bind_rows(trop,temp) %>% 
  rename(Relationship = Valores) %>% 
  filter (Variables %in% traits)

#p1 = both%>% 
#p2 = prod %>% 
#p3 = estoque %>% 
  ggplot(aes(x=Region, y=frequencia, fill=Relationship))+geom_bar(stat= "identity") +
  labs(x = "", y = "Frequency of papers (%)", title = "Effect of functional dominance on productivity \nacross different regions (stocks)") + 
  scale_fill_manual(values = c("#44AA99","#888888","#AA4499"),guide = guide_legend(
    direction = "horizontal",
    title.position = "top",title.hjust = 0.5))+ 
  facet_grid(facets = ~(Variables), scales="free") + 
  geom_text(aes(label=`Number of papers`), vjust=-0.5, hjust=0, position=position_stack(vjust=0), colour="black", size=5) +
  theme_minimal()  +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5,size = 15, face = "bold"),
        axis.text.x = element_text(size=12),
        axis.title.y =element_text(size=15), 
        axis.title.x =element_text(size=15), 
        strip.text.x = element_text(size = 14))

library(patchwork)
# plots = (p1|(p2/p3)) +plot_annotation(tag_levels = c("A"))+ plot_layout(widths = c(1, 1))
# png('results/CWM_prod_regioes.png', units="in", width=25, height=18, res=300)
# plots
# dev.off()
