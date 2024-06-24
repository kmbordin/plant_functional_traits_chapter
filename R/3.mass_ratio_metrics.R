# Bordin et al.: The use of functional traits in assessing productivity in natural ecosystems
# Folder to analyse functional dominance (niche-complementarity)

# load these datasets first
here::here("1.load_harmonise.R")
cat = readxl::read_excel(here::here("processed_data", "categorisation.xlsx"))

# regions, ecosystems, and metrics ----
regiao_estudo = c("temperate", "tropical", "subtropical")
prod_type = c("1","2")
ecosys_type= c("forest", "grassland")

# theme for plots -----
my_theme <- theme_minimal()  +
  theme(legend.position = "bottom",
        axis.text=element_text(size=15),
        strip.text = element_text(size = 15), 
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size=13))

# function to obtain the trait frequency and calculate the cwms --------
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
    mutate(Variables = replace(Variables, Variables  %in% lma , "SLA"))%>% # SLA = 1/LMA
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

# ifelse to invert the effect of LMA to SLA
data$CWM_LMA <- ifelse(data$CWM_LMA == "negative", "MUDAR", data$CWM_LMA) 
data$CWM_LMA <- ifelse(data$CWM_LMA == "positive", "negative", data$CWM_LMA)
data$CWM_LMA <- ifelse(data$CWM_LMA == "MUDAR", "positive", data$CWM_LMA)
data <- data %>% filter(ecosystem %in% ecosys_type)
data <- data %>% filter(regiao %in% regiao_estudo)
data <- data %>% filter(prod.metric %in% prod_type)
data <- data %>% mutate(regiao = replace(regiao, regiao == "subtropical" , "Tropical")) %>% 
  mutate(regiao = replace(regiao, regiao == "temperate" , "Temperate")) %>% 
  mutate(regiao = replace(regiao, regiao == "tropical" , "Tropical")) 

# new df - traits and ecosystems ----
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

# new df - traits and metrics  of productivity -----
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

# new df - traits and regions ----
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

# combining dfs and saving data ------
a= bind_cols(traits_ecosys,traits_regiao,traits_metric) %>% 
  select(-c(Variables...4, Variables...7)) %>% 
  rename(Trait = Variables...1)
# save cwm results
#write.table(a, "results/cwm_eval.txt")

# functional dominance across all ecosystems -----
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

filt <- function(data){
  data = data [, -c(4,5)]%>% 
    group_by(Variables, Valores) %>% 
    summarise(`Number of papers` = sum(`Number of papers`)) %>% 
    filter(Variables %in% all$Variables)
} 

# functional dominance across grasslands -----
grass <- data %>% 
  filter (ecosystem=="grassland") %>%
  cwm.site()  %>% filt()%>% 
  mutate(ecosystem = "Grassland")
  
# functional dominance across forests -----
fores <- data %>% 
  filter (ecosystem=="forest") %>% 
  cwm.site()%>% filt() %>% 
  mutate(ecosystem = "Forest")

# grasslands and productivity
grass_prod <- data %>% 
  filter (ecosystem=="grassland") %>%
  filter (prod.metric=="1") %>%
  cwm.site() %>% filt()%>% 
  mutate(ecosystem = "Grassland", 
         metric = "produc") 

# grasslands and stocks
grass_stock <- data %>% 
  filter (ecosystem=="grassland") %>%
  filter (prod.metric=="2") %>%
  cwm.site() %>%  filt() %>% 
  mutate(ecosystem = "Grassland", 
         metric = "stock")

# forest productivity
fores_prod <- data %>% 
  filter (ecosystem=="forest") %>% 
  filter (prod.metric=="1") %>%
  cwm.site()%>% filt() %>% 
  mutate(ecosystem = "Forest", 
         metric = "produc") 

# forest stocks
fores_stock <- data %>% 
  filter (ecosystem=="forest") %>% 
  filter (prod.metric=="2") %>%
  cwm.site() %>% filt()%>% 
  mutate(ecosystem = "Forest", 
         metric = "stock") 

# functional dominance across tropical region -----
trop <- data %>% 
  filter (regiao=="Tropical") %>%
  cwm.site()  %>% filt()%>% 
  mutate(regiao = "Tropical")

# functional dominance across temperate region -----
temp <- data %>% 
  filter (regiao =="Temperate") %>% 
  cwm.site()%>% filt() %>% 
  mutate(regiao = "Temperate")

# tropical productivity
trop_prod <- data %>% 
  filter (regiao=="Tropical") %>%
  filter (prod.metric=="1") %>%
  cwm.site() %>% filt()%>% 
  mutate(regiao = "Tropical", 
         metric = "produc") 

# tropical stocks 
trop_stock <- data %>% 
  filter (regiao=="Tropical") %>%
  filter (prod.metric=="2") %>%
  cwm.site() %>%  filt() %>% 
  mutate(regiao = "Tropical", 
         metric = "stock")

# temperate productivity
temp_prod <- data %>% 
  filter (regiao=="Temperate") %>% 
  filter (prod.metric=="1") %>%
  cwm.site()%>% filt() %>% 
  mutate(regiao = "Temperate", 
         metric = "produc") 

# temperate stocks
temp_stock <- data %>% 
  filter (regiao=="Temperate") %>% 
  filter (prod.metric=="2") %>%
  cwm.site() %>% filt()%>% 
  mutate(regiao = "Temperate", 
         metric = "stock") %>% 
  rename()

# saving data for table 2 -----
all %>% 
  gt() %>% 
  tab_header(title = md("**Functional dominance as predictor of productivity**"),
             subtitle = "Frequency and number of mentions of functional dominance effects on productivity") %>% 
  tab_style(style = cell_fill(color = "gray90"),
            locations = cells_column_labels(columns = everything())) %>% 
  cols_align(align = "center",columns = everything()) #%>% 
#gtsave(filename = "results/CWM_effects_semecosys.rtf")

# plots per ecosystem ----
ecosystem <- bind_rows(grass,fores) %>% 
  group_by(Variables, ecosystem) %>% 
  mutate(frequencia = round((`Number of papers`/sum(`Number of papers`) * 100), digits = 0)) %>% 
  rename(Relationship = Valores) %>% 
  mutate(frequencia = replace(frequencia, frequencia == "45" , "45.5")) %>% 
  mutate(frequencia = replace(frequencia, frequencia == "44" , "45"))%>%
  mutate(frequencia = replace(frequencia, frequencia == "62" , "63"))%>%
  mutate(frequencia = replace(frequencia, frequencia == "23" , "22"))%>%
  mutate(frequencia = as.numeric(frequencia)) %>% 
  mutate(ecosystem = replace(ecosystem, ecosystem == "Forest" , "F")) %>% 
  mutate(ecosystem = replace(ecosystem, ecosystem == "Grassland" , "G"))%>% 
  mutate(Variables = replace(Variables, Variables == "Root quantity" , "Root"))
ecosystem_metric_prod <-  bind_rows(grass_prod,fores_prod) %>%
  group_by(Variables, ecosystem,metric) %>% 
  mutate(frequencia = round((`Number of papers`/sum(`Number of papers`) * 100), digits = 0)) %>% 
  rename(Relationship = Valores)%>% 
  mutate(ecosystem = replace(ecosystem, ecosystem == "Forest" , "F")) %>% 
  mutate(ecosystem = replace(ecosystem, ecosystem == "Grassland" , "G")) %>% 
  mutate(Variables = replace(Variables, Variables == "Root quantity" , "Root"))

ecosystem_metric_stock <-  bind_rows(grass_stock,fores_stock) %>%
  group_by(Variables, ecosystem,metric) %>% 
  mutate(frequencia = round((`Number of papers`/sum(`Number of papers`) * 100), digits = 0)) %>% 
  rename(Relationship = Valores) %>% 
  mutate(frequencia = replace(frequencia, frequencia == "38" , "37.5"))%>%
  mutate(frequencia = as.numeric(frequencia))%>% 
  mutate(ecosystem = replace(ecosystem, ecosystem == "Forest" , "F")) %>% 
  mutate(ecosystem = replace(ecosystem, ecosystem == "Grassland" , "G"))%>% 
  mutate(Variables = replace(Variables, Variables == "Root quantity" , "Root"))

p1 <- ecosystem %>% 
  ggplot(aes(x=ecosystem, y=frequencia, fill=Relationship))+geom_bar(stat= "identity") +
  labs(x = "", y = "Frequency of papers (%)", title = "Relationship between functional dominance \nand productivity") + 
  scale_fill_manual(values = c("#AA4499","#888888","#44AA99"),guide = guide_legend(
    direction = "horizontal",title.position = "top",title.hjust = 0.5))+ 
  facet_grid(facets = ~(Variables), scales="free") + 
  geom_text(aes(label=`Number of papers`), vjust=-0.5, hjust=0.5, position=position_stack(vjust=0), colour="black", size=8) + my_theme+
  theme(legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16),
        axis.text=element_text(size=16))

p2 <- ecosystem_metric_prod %>% 
  ggplot(aes(x=ecosystem, y=frequencia, fill=Relationship))+geom_bar(stat= "identity") +
  labs(x = "", y = "Frequency of papers (%)", title = "Relationship between functional dominance \nand productivity (rate only)") + 
  scale_fill_manual(values = c("#AA4499","#888888","#44AA99"),guide = guide_legend(
    direction = "horizontal",title.position = "top",title.hjust = 0.5))+ 
  facet_grid(facets = ~(Variables), scales="free") + 
  geom_text(aes(label=`Number of papers`), vjust=-0.5, hjust=0.5, position=position_stack(vjust=0), colour="black", size=8) + my_theme+
  theme(legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16),
        axis.text=element_text(size=16))

p3 <- ecosystem_metric_stock %>% 
  ggplot(aes(x=ecosystem, y=frequencia, fill=Relationship))+geom_bar(stat= "identity") +
  labs(x = "", y = "Frequency of papers (%)", title = "Relationship between functional dominance \nand productivity (stock only)") + 
  scale_fill_manual(values = c("#AA4499","#888888","#44AA99"),guide = guide_legend(
    direction = "horizontal",title.position = "top",title.hjust = 0.5))+ 
  facet_grid(facets = ~(Variables), scales="free") + 
  geom_text(aes(label=`Number of papers`), vjust=-0.5, hjust=0.5, position=position_stack(vjust=0), colour="black", size=8) + my_theme+
  theme(legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16),
        axis.text=element_text(size=16))

plots = (p1|(p2/p3)) +plot_annotation(tag_levels = c("A"))+ plot_layout(widths = c(1, 1))
# png('results/CWM_estoque_temporal_ecosystem.png', units="in", width=12, height=10, res=300)
# plots
# dev.off()

# plots per region -----
regiao <- bind_rows(temp,trop)%>% 
  group_by(Variables, regiao) %>% 
  mutate(frequencia = round((`Number of papers`/sum(`Number of papers`) * 100), digits = 0)) %>% 
  rename(Relationship = Valores) %>% 
  mutate(frequencia = replace(frequencia, frequencia == "17" , "16.5"))%>%
  mutate(frequencia = replace(frequencia, frequencia == "22" , "23"))%>%
  mutate(frequencia = replace(frequencia, frequencia == "12" , "13"))%>%
  mutate(frequencia = replace(frequencia, frequencia == "35" , "35.5"))%>%
  mutate(frequencia = replace(frequencia, frequencia == "27" , "27.5"))%>%
  mutate(frequencia = as.numeric(frequencia))%>% 
  mutate(regiao = replace(regiao, regiao == "Temperate" , "Te")) %>% 
  mutate(regiao = replace(regiao, regiao == "Tropical" , "Tr")) %>% 
  mutate(Variables = replace(Variables, Variables == "Root quantity" , "Root"))

regiao_metric_prod <-  bind_rows(temp_prod, trop_prod) %>% 
  group_by(Variables, regiao, metric) %>% 
  mutate(frequencia = round((`Number of papers`/sum(`Number of papers`) * 100), digits = 0)) %>% 
  rename(Relationship = Valores)%>% 
  mutate(frequencia = replace(frequencia, frequencia == "33" , "33.3"))%>%
  mutate(frequencia = as.numeric(frequencia))%>% 
  mutate(regiao = replace(regiao, regiao == "Temperate" , "Te")) %>% 
  mutate(regiao = replace(regiao, regiao == "Tropical" , "Tr")) %>% 
  mutate(Variables = replace(Variables, Variables == "Root quantity" , "Root"))

regiao_metric_stock <-  bind_rows(temp_stock, trop_stock) %>% 
  group_by(Variables, regiao, metric) %>% 
  mutate(frequencia = round((`Number of papers`/sum(`Number of papers`) * 100), digits = 0)) %>% 
  rename(Relationship = Valores)%>% 
  mutate(frequencia = replace(frequencia, frequencia == "67" , "66"))%>%
  mutate(frequencia = as.numeric(frequencia))%>% 
  mutate(regiao = replace(regiao, regiao == "Temperate" , "Te")) %>% 
  mutate(regiao = replace(regiao, regiao == "Tropical" , "Tr")) %>% 
  mutate(Variables = replace(Variables, Variables == "Root quantity" , "Root"))

p1 <- regiao %>% 
  ggplot(aes(x=regiao, y=frequencia, fill=Relationship))+geom_bar(stat= "identity") +
  labs(x = "", y = "Frequency of papers (%)", title = "Relationship between functional dominance \nand productivity") + 
  scale_fill_manual(values = c("#AA4499","#888888","#44AA99"),guide = guide_legend(
    direction = "horizontal",title.position = "top",title.hjust = 0.5))+ 
  facet_grid(facets = ~(Variables), scales="free") + 
  geom_text(aes(label=`Number of papers`), vjust=-0.5, hjust=0.5, position=position_stack(vjust=0), colour="black", size=8) + my_theme+
  theme(legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16),
        axis.text=element_text(size=16))

p2 <- regiao_metric_prod %>% 
  ggplot(aes(x=regiao, y=frequencia, fill=Relationship))+geom_bar(stat= "identity") +
  labs(x = "", y = "Frequency of papers (%)", title = "Relationship between functional dominance \nand productivity (rate only)") + 
  scale_fill_manual(values = c("#AA4499","#888888","#44AA99"),guide = guide_legend(
    direction = "horizontal",title.position = "top",title.hjust = 0.5))+ 
  facet_grid(facets = ~(Variables), scales="free") + 
  geom_text(aes(label=`Number of papers`), vjust=-0.5, hjust=0.5, position=position_stack(vjust=0), colour="black", size=8) + my_theme+
  theme(legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16),
        axis.text=element_text(size=16))

p3 <- regiao_metric_stock %>% 
  ggplot(aes(x=regiao, y=frequencia, fill=Relationship))+geom_bar(stat= "identity") +
  labs(x = "", y = "Frequency of papers (%)", title = "Relationship between functional dominance \nand productivity (stock only)") + 
  scale_fill_manual(values = c("#AA4499","#888888","#44AA99"),guide = guide_legend(
    direction = "horizontal",title.position = "top",title.hjust = 0.5))+ 
  facet_grid(facets = ~(Variables), scales="free") + 
  geom_text(aes(label=`Number of papers`), vjust=-0.5, hjust=0.5, position=position_stack(vjust=0), colour="black", size=8) + my_theme+
  theme(legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16),
        axis.text=element_text(size=16))

plots = (p1|(p2/p3)) +plot_annotation(tag_levels = c("A"))+ plot_layout(widths = c(1, 1))
# png('results/CWM_estoque_temporal_regiao.png', units="in", width=12, height=10, res=300)
# plots
# dev.off()

# all functional dominance results ------
ecosystem <- ecosystem %>% 
  rename(Environment = ecosystem)
regiao <- regiao %>% 
  rename(Environment = regiao)
total = bind_rows(ecosystem,regiao)

cwm = total %>%  select(-frequencia) %>%  pivot_wider(names_from = Variables, values_from = `Number of papers`) %>% 
  relocate(Environment, .before = Relationship) %>% 
  replace(is.na(.),0) %>% 
  relocate(`Root quantity`, .after = WD)

cwm %>% 
  gt() %>% 
  tab_header(title = md("**Functional dominance evaluation**"),
             subtitle = "Relationship between functional dominance and productivity across different ecosystems and regions")  #%>% 
  #gtsave(filename = "results/cwm.rtf")

cwm = total %>%  pivot_wider(names_from = Variables, values_from = `Number of papers`) %>% 
  relocate(Environment, .before = Relationship) %>% 
  replace(is.na(.),0) %>% 
  relocate(`Root quantity`, .after = WD)

cwm %>% 
  gt() %>% 
  tab_header(title = md("**Functional dominance evaluation**"),
             subtitle = "Relationship between functional dominance and productivity across different ecosystems and regions")  #%>% 
  #gtsave(filename = "results/cwm.rtf")

# chi-squares of traits among regions and ecosystems ----- 
chi <- function(data, var, type){
a = data %>%  
  filter(Variables == var) %>% 
  filter(Environment == type)
b = chisq.test(a$`Number of papers`)
print(b)
c = data.frame (Variable = var,
                Environment = type,
                X.squared = b$statistic,
                df = b$parameter,
                p.value = b$p.value)
return(c)
}
a = chi(ecosystem, var = "Height", type = "Grassland")
b = chi(ecosystem, var = "SLA", type = "Grassland")
c = chi(ecosystem, var = "LA", type = "Grassland")
d = chi(ecosystem, var = "LDMC", type = "Grassland")
e = chi(ecosystem, var = "LNC", type = "Grassland")
f = chi(ecosystem, var = "Root quantity", type = "Grassland")
gr = bind_rows(a,b,c,d,e,f)
a = chi(ecosystem, var = "Height", type = "Forest")
b = chi(ecosystem, var = "SLA", type = "Forest")
c = chi(ecosystem, var = "LA", type = "Forest")
d = chi(ecosystem, var = "LDMC", type = "Forest")
e = chi(ecosystem, var = "LNC", type = "Forest")
f = chi(ecosystem, var = "WD", type = "Forest")
fr =  bind_rows(a,b,c,d,e,f)
a = chi(regiao, var = "Height", type = "Temperate")
b = chi(regiao, var = "SLA", type = "Temperate")
c = chi(regiao, var = "LA", type = "Temperate")
d = chi(regiao, var = "LDMC", type = "Temperate")
e = chi(regiao, var = "LNC", type = "Temperate")
f = chi(regiao, var = "Root quantity", type = "Temperate")
te =  bind_rows(a,b,c,d,e,f)
a = chi(regiao, var = "Height", type = "Tropical")
b = chi(regiao, var = "SLA", type = "Tropical")
c = chi(regiao, var = "LA", type = "Tropical")
d = chi(regiao, var = "LDMC", type = "Tropical")
e = chi(regiao, var = "LNC", type = "Tropical")
f = chi(regiao, var = "WD", type = "Tropical")
tr =  bind_rows(a,b,c,d,e,f)

al= bind_rows(gr,fr,te,tr)
#write.csv(al, "results/x2.csv")
