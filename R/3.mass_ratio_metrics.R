#precisa carregar esse script antes
here::here("1.load_harmonise.R")
prod_type = c("1","2")
regiao_estudo = c("temperate", "tropical", "subtropical")
ecosys_type= c("forest", "grassland")

# frequencia de  traits usados para calcular o cwm
cwm.site <- function (x) {
  data <- x %>% 
    mutate(ecosystem = replace(ecosystem, ecosystem == "forest", "Forest")) %>%
    mutate(ecosystem = replace(ecosystem, ecosystem == "grassland", "Grassland")) %>%
    filter(ecosystem != "ecotones") %>%
    select(ecosystem, regiao, prod.metric, CWM_LA: CWM_carbon13) %>% 
    pivot_longer(cols = -(ecosystem:prod.metric), names_to = c("Variables"), values_to = "Valores") %>% #pivota as colunas para linhas
    mutate(Variables = sub("^CWM_", "", Variables)) %>% 
    mutate(Valores = replace(Valores, Valores == "ns", "No relationship")) %>%
    mutate(Valores = replace(Valores, Valores == "positive", "Positive")) %>%
    mutate(Valores = replace(Valores, Valores == "negative", "Negative")) %>%
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
    group_by(Variables, Valores, ecosystem, regiao, prod.metric) %>%
    summarize(`Number of papers` = n()) %>% 
    drop_na(Valores) %>% 
    filter(Valores != "positive(temperate),negative(subtropical)") %>% 
    return(data)
}

# ifelse para mudar o efeito de LMA para SLA (LMA=1/SLA)
data$CWM_LMA <- ifelse(data$CWM_LMA == "negative", "MUDAR", data$CWM_LMA) 
data$CWM_LMA <- ifelse(data$CWM_LMA == "positive", "negative", data$CWM_LMA)
data$CWM_LMA <- ifelse(data$CWM_LMA == "MUDAR", "positive", data$CWM_LMA)
data <- data %>% filter(ecosystem %in% ecosys_type)
data <- data %>% filter(regiao %in% regiao_estudo)
data <- data %>% filter(prod.metric %in% prod_type)
data <- data %>% mutate(regiao = replace(regiao, regiao == "subtropical" , "Tropical")) %>% 
  mutate(regiao = replace(regiao, regiao == "temperate" , "Temperate")) %>% 
  mutate(regiao = replace(regiao, regiao == "tropical" , "Tropical")) 

#tomatch categories
cat = readxl::read_excel(here::here("results", "categorisation.xlsx"))

#para tabela total
traits_ecosys = data %>% 
  cwm.site() %>% 
  select(-Valores) %>% 
  mutate(Variables = replace(Variables, Variables == "LMA" , "SLA"))%>%
  summarise(`Number of papers` = sum(`Number of papers`))  %>%
  filter(Variables %in% cat$`Traits used in this study`) %>% 
  mutate(`Number of papers`= as.integer(`Number of papers`),
         Valores = as.factor(Valores),
         Variables = as.factor(Variables),
         ecosystem = as.factor(ecosystem),
         regiao = as.factor(regiao)) %>%
  group_by(ecosystem, Variables) %>% 
  summarise(Value = sum(`Number of papers`)) %>%
  pivot_wider(id_cols = c(Variables), names_from = c(ecosystem),values_from = Value) %>% 
  replace(is.na(.),0) %>% 
  arrange(Variables)

traits_metric <- data %>% 
  cwm.site() %>% 
  mutate(prod.metric = replace(prod.metric, prod.metric == "1" , "Rate")) %>%  
  mutate(prod.metric = replace(prod.metric, prod.metric == "2" , "Stock")) %>%  
    mutate(`Number of papers`= as.integer(`Number of papers`),
           Valores = as.factor(Valores),
           Variables = as.factor(Variables),
           ecosystem = as.factor(ecosystem),
           regiao = as.factor(regiao), 
           prod.metric = as.factor (prod.metric)) %>%
    group_by(prod.metric, Variables) %>% 
    summarise(Value = sum(`Number of papers`)) %>%
  filter(Variables %in% cat$`Traits used in this study`) %>% 
  pivot_wider(id_cols = c(Variables), names_from = c(prod.metric),values_from = Value) %>% 
    replace(is.na(.),0)%>% 
  arrange(Variables)
 
traits_regiao = data %>% 
  cwm.site() %>% 
  select(-Valores) %>% 
  mutate(Variables = replace(Variables, Variables == "LMA" , "SLA"))%>%
  summarise(`Number of papers` = sum(`Number of papers`))  %>%
  filter(Variables %in% cat$`Traits used in this study`) %>% 
  mutate(`Number of papers`= as.integer(`Number of papers`),
         Valores = as.factor(Valores),
         Variables = as.factor(Variables),
         ecosystem = as.factor(ecosystem),
         regiao = as.factor(regiao)) %>%
  group_by(regiao, Variables) %>% 
  summarise(Value = sum(`Number of papers`)) %>%
  pivot_wider(id_cols = c(Variables), names_from = c(regiao),values_from = Value) %>% 
  replace(is.na(.),0) %>% 
  arrange(Variables)

a= bind_cols(traits_ecosys,traits_regiao,traits_metric) %>% 
  select(-c(Variables...4, Variables...7)) %>% 
  rename(Trait = Variables...1)

#efeito dos cwm para todos os dois ecossistemas (total)
all = data %>% 
  cwm.site() %>% 
  select(-Valores) %>% 
  mutate(Variables = replace(Variables, Variables == "LMA" , "SLA"))%>%
  summarise(`Number of papers` = sum(`Number of papers`))  %>%
  filter(Variables %in% cat$`Traits used in this study`) %>% 
  group_by(Variables) %>% 
  summarise(`Number of papers` = sum(`Number of papers`)) %>% 
  filter(`Number of papers` >= 8) %>% 
  mutate(frequencia = round((`Number of papers`/sum(`Number of papers`) * 100), digits = 0)) %>% 
  rename(`Frequency (%)` = frequencia) %>% 
  arrange(desc(`Number of papers`)) 

#filtra as infos somente para campo
grass <- data %>% 
  filter (ecosystem=="grassland") %>%
  cwm.site() %>% 
  mutate(ecosystem = "Grassland") %>% 
  select(-c(regiao,prod.metric)) 

#filtra as infos para floresta
fores <- data %>% 
  filter (ecosystem=="forest") %>% 
  cwm.site() %>% 
  mutate(ecosystem = "Forest")

#filta as infos somente para campo
grass_prod <- data %>% 
  filter (ecosystem=="grassland") %>%
  filter (prod.metric=="1") %>%
  cwm.site() %>% 
  mutate(ecosystem = "Grassland", 
         metric = "produc") 
grass_stock <- data %>% 
  filter (ecosystem=="grassland") %>%
  filter (prod.metric=="2") %>%
  cwm.site() %>% 
  mutate(ecosystem = "Grassland", 
         metric = "stock") 

#filtra as infos para floresta
fores_prod <- data %>% 
  filter (ecosystem=="forest") %>% 
  filter (prod.metric=="1") %>%
  cwm.site() %>% 
  mutate(ecosystem = "Forest", 
         metric = "produc") 
fores_stock <- data %>% 
  filter (ecosystem=="forest") %>% 
  filter (prod.metric=="2") %>%
  cwm.site() %>% 
  mutate(ecosystem = "Forest") %>% 
  mutate(metric = "stock")



all %>% 
  gt() %>% 
  tab_header(title = md("**Functional dominance as predictor of productivity**"),
             subtitle = "Frequency and number of mentions of functional dominance effects on productivity") %>% 
  tab_style(style = cell_fill(color = "gray90"),
            locations = cells_column_labels(columns = everything())) %>% 
  cols_align(align = "center",columns = everything()) #%>% 
#gtsave(filename = "results/CWM_effects_semecosys.rtf")

dados_contagem <- bind_rows(grass,fores) %>% 
#dados_contagem <- bind_rows(grass_prod, grass_stock,fores_prod,fores_stock) %>%  #para estoque ou temporal aqui#
  mutate(Valores = replace(Valores, Valores =="negativa ", "Negative")) %>%
  group_by(ecosystem, Variables, Valores) %>% #usar esse para o total
  #group_by(ecosystem, Variables, Valores, metric) %>% #usar esse para estoque e temporal
  summarise(`Number of papers` = sum(`Number of papers`))

#para plot
traits = c("Height", "SLA", "LDMC", "LNC", "LPC",'Root quantity',"WD")
fores_prod
estoque_temporal_cwm <- dados_contagem %>% 
  group_by(ecosystem,Variables,metric) %>% 
  filter(Variables %in% traits) %>% 
  mutate(frequencia = round((`Number of papers`/sum(`Number of papers`) * 100), digits = 0)) %>% 
  rename(Ecosystem = ecosystem,
         Relationship = Valores,
         Trait = Variables)

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
  #mutate_at(. ~na_if(.0)) %>% 
  mutate(frequencia = replace(frequencia, frequencia == "71", "72")) %>% 
  mutate(frequencia = replace(frequencia, frequencia == "17", "16")) %>% 
  mutate(frequencia = replace(frequencia, frequencia == "38", "39")) %>% 
  mutate(frequencia = as.numeric(frequencia))


#png("results/CWM_effect_ecosystem.png", units="in", width=11, height=9, res=300)
p1 = cwm_rel%>% rename(`Number of papers`=count) %>% 
  #p2 = estoque_temporal_cwm %>% filter(metric=="produc") %>% mutate(frequencia = replace(frequencia, frequencia == "29", "28.5")) %>% mutate(frequencia=replace(frequencia, frequencia == "18", "19")) %>% mutate(frequencia=as.numeric(frequencia)) %>% #temporal
  #p3 = estoque_temporal_cwm %>% filter(metric=="stock") %>% mutate(frequencia = replace(frequencia, frequencia == "29", "28.5")) %>% mutate(frequencia = replace(frequencia, frequencia == "17", "16.5")) %>% mutate(frequencia = replace(frequencia, frequencia == "22", "23")) %>% mutate(frequencia=as.numeric(frequencia)) %>% #stock
  ggplot(aes(x=Ecosystem, y=frequencia, fill=Relationship))+geom_bar(stat= "identity") +
  labs(x = "", y = "Frequency of papers (%)", title = "Effect of functional dominance on productivity") + 
  scale_fill_manual(values = c("#AA4499","#888888","#44AA99"),guide = guide_legend(
    direction = "horizontal",
    title.position = "top",title.hjust = 0.5))+ 
  facet_grid(facets = ~(Trait), scales="free") + 
  #geom_text(aes(label=count), vjust=-0.5, hjust=0, position=position_stack(vjust=0), colour="black", size=5) +
  geom_text(aes(label=`Number of papers`), vjust=-0.5, hjust=0, position=position_stack(vjust=0), colour="black", size=5) +
  theme_minimal()  +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5,size = 15, face = "bold"),
        axis.text.x = element_text(size=12),
        axis.title.y =element_text(size=15), 
        axis.title.x =element_text(size=15), 
        strip.text.x = element_text(size = 14))
#dev.off()
plots = (p1|(p2/p3)) +plot_annotation(tag_levels = c("A"))+ plot_layout(widths = c(1, 1))
# png('results/CWM_estoque_temporal.png', units="in", width=25, height=18, res=300)
# plots
# dev.off()

reg = c("tropical", "temperate", "subtropical")
n.data = data %>% 
  mutate(ecosystem = replace(ecosystem, ecosystem == "forest", "Forest")) %>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "grassland", "Grassland")) %>%
  filter(ecosystem != "ecotones") %>%
  select(CWM_LA: CWM_carbon13,regiao,prod.metric) %>% 
  filter(regiao %in% reg) %>% 
  mutate(regiao = replace(regiao, regiao == "subtropical", "tropical")) %>% 
  filter (prod.metric != "1,2")

filtros <- function(data, reg, metrica) {
  filter (n.data, regiao == reg)%>%
    filter (prod.metric == metrica) %>% #incluir esses filtros para os estoques ou npp
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
    mutate(frequencia = round((`Number of papers`/sum(`Number of papers`) * 100), digits = 0))
  #mutate(Region = "Tropical")
}
trop_prod = filtros(n.data, reg = "tropical", 1) %>% mutate(Region = "Tropical")
trop_estoque = filtros(n.data, reg = "tropical", 2) %>% mutate(Region = "Tropical")
temp_prod = filtros(n.data, reg = "temperate", 1) %>% mutate(Region = "Temperate")  
temp_estoque = filtros(n.data, reg = "temperate", 2) %>% mutate(Region = "Temperate")  
# temp <-n.data %>% 
#   filter (regiao == "temperate")%>%
# trop <-n.data %>% 
#   filter (regiao == "tropical")%>%
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
  mutate(frequencia = round((`Number of papers`/sum(`Number of papers`) * 100), digits = 0)) %>% 
  #mutate(Region = "Temperate")
  #mutate(Region = "Tropical")
  
  
  traits = c("Height", "SLA", "LDMC", "LNC", "LPC", "Root quantity", "WD")

both = bind_rows(trop,temp) %>% 
  rename(Relationship = Valores) %>% 
  filter (Variables %in% traits) %>% 
  mutate(frequencia = replace(frequencia, frequencia == "44", "45")) %>% 
  mutate(frequencia = replace(frequencia, frequencia == "12", "13")) %>% 
  mutate(frequencia = replace(frequencia, frequencia == "27", "27.5")) %>% 
  mutate(frequencia = replace(frequencia, frequencia == "35", "35.5")) %>% 
  mutate(frequencia=as.numeric(frequencia))

prod  =  bind_rows(trop_prod,temp_prod) %>% 
  rename(Relationship = Valores) %>%
  filter (Variables %in% traits)  %>% 
  mutate(frequencia = replace(frequencia, frequencia == "33", "33.3"))%>% 
  mutate(frequencia=as.numeric(frequencia))

estoque = bind_rows(trop_estoque,temp_estoque) %>% 
  rename(Relationship = Valores) %>% 
  filter (Variables %in% traits) %>% 
  mutate(frequencia = replace(frequencia, frequencia == "67", "66"))%>% 
  mutate(frequencia=as.numeric(frequencia))

p1 = both%>% 
  #p2 = prod %>% 
  #p3 = estoque %>% 
  ggplot(aes(x=Region, y=frequencia, fill=Relationship))+geom_bar(stat= "identity") +
  labs(x = "", y = "Frequency of papers (%)", title = "Effect of functional dominance on productivity \nacross different regions") + 
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

plots = (p1|(p2/p3)) +plot_annotation(tag_levels = c("A"))+ plot_layout(widths = c(1, 1))
# png('results/CWM_prod_regioes.png', units="in", width=25, height=13, res=300)
# plots
# dev.off()
