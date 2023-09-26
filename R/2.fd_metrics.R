# fd evaluation 
#precisa carregar esse script antes
here::here("1.load_harmonise.R")
#isso e para criar apenas as regioes tropicais e temperadas
regiao_estudo = c("temperate", "tropical", "subtropical")

#nova matriz, corrigindo e agrupando traits, com a info de regiao
#essa matriz fica ao final com os traits em linhas, separadamente, além da info de ecossistema e regiao

fd <- tibble (traits.fd = data$traits_FD, 
              ecosystem = data$ecosystem,
              region = data$regiao) %>%
  filter(region %in% regiao_estudo) %>% 
  mutate(region = replace(region, region == "subtropical" , "tropical")) %>%
  mutate(across(c(traits.fd), ~str_replace_all(., ";", ","))) %>%
  separate(traits.fd,into = paste0("fd", 1:30), sep = ",")%>%
  gather(key = "variable", value = "driver", starts_with("fd"), na.rm = TRUE)%>%
  mutate(across(c(driver), ~str_replace_all(., " ", ""))) %>%
  mutate(driver = replace(driver, driver  %in% sla , "SLA")) %>%
  mutate(driver = replace(driver, driver  %in% lma , "SLA"))%>% #decidimos tornar LMA para SLA
  mutate(driver = replace(driver, driver  %in% ldmc , "LDMC"))%>%
  mutate(driver = replace(driver, driver  %in% lnc , "LNC")) %>%
  mutate(driver = replace(driver, driver  %in% n.c , "LCC:LNC")) %>%
  mutate(driver = replace(driver, driver  %in% wd , "WD")) %>%
  mutate(driver = replace(driver, driver  %in% height , "Height")) %>%
  mutate(driver = replace(driver, driver  %in% dec , "Deciduousness")) %>%
  mutate(driver = replace(driver, driver  %in% growth , "Growth rate")) %>%
  mutate(driver = replace(driver, driver  %in% seed , "Seed size")) %>%
  mutate(driver = replace(driver, driver  %in% crown , "Crown size")) %>%
  mutate(driver = replace(driver, driver  %in% tree.size , "Tree size")) %>%
  mutate(driver = replace(driver, driver  %in% lcc , "LCC")) %>%
  mutate(driver = replace(driver, driver  %in% form , "Growth form")) %>%
  mutate(driver = replace(driver, driver  %in% tolerance , "Stress tolerance")) %>%
  mutate(driver = replace(driver, driver  %in% root , "Root quantity")) %>%
  mutate(driver = replace(driver, driver  %in% la , "LA")) %>%
  mutate(driver = replace(driver, driver  %in% lt , "LT")) %>%
  mutate(driver = replace(driver, driver  %in% vessel , "Vessel quantity")) %>%
  mutate(driver = replace(driver, driver  %in% pigment , "Pigments")) %>%
  mutate(driver = replace(driver, driver  %in% n.p , "LNC:LPC")) %>%
  mutate(driver = replace(driver, driver  %in% lpc , "LPC")) %>%
  mutate(driver = replace(driver, driver  %in% na , "")) %>%
  mutate(driver = replace(driver, driver == "age", "Age")) %>%
  mutate_all(~na_if(., "")) %>%
  drop_na(driver) %>% 
  filter (driver != "Age")

# aqui remove a info de fd em ecotonos e calcula as frequencias para 3 ou mais citacoes noes estudos

#x <- fd %>% #aqui inclui a regiao de estudo
#group_by(region) %>% 

fd2 <- fd %>%
  group_by(ecosystem) %>% 
  filter(ecosystem != "ecotones") %>% 
  mutate(ecosystem = replace(ecosystem, ecosystem == "forest", "Forest")) %>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "grassland", "Grassland")) %>%
  count(driver) %>%
  filter(n>=3) %>% 
  group_by(ecosystem) %>% 
  mutate(frequencia = round((n / sum(n))*100, digits = 0)) 

fd2 %>% #sem group_by(ecosystem)! para saber os valores totais de traits usados na fd
  rename(`Trait type` = driver,
         `Number of papers` = n,
         `Frequency (%)` = frequencia) %>% 
  gt()%>%  
  tab_header(title = md("**Functional traits used to calculate the functional diversity**"),  subtitle = "Number and frequency of mentions of each trait used to calculate functional diversity ") %>% 
  tab_style(style = cell_fill(color = "gray90"),
            locations = cells_column_labels(columns = everything())) %>% 
  cols_align(align = "center",columns = everything()) #%>% 
  #gtsave(filename = "results/traits_to_FD_semecosys.rtf")
  #gtsave(filename = "results/traits_to_FD_ecosys.rtf") 

#agrupamento dos traits nas classes de Laughlin et al 2014 
roots = c("Root quantity")
seeds = c("Seed size")
life.history = c("Growth rate","Growth form","Tree size")
height = c("Height","Crown size")
stem = c("WD","Vessel quantity","Stress tolerance","Deciduousness")
leaves = c("LDMC","LT","LM","LA","SLA","LT","LCaC","LNC:LPC","Pigments","LCC:LNC","LCC","LPC","LNC")

fd2 <- fd2 %>% 
  mutate(type = ifelse(driver %in% leaves, "Leaf traits",NA),
         type = ifelse(driver %in% seeds, "Height, seed, and root traits",type),
         type = ifelse(driver %in% height, "Height, seed, and root traits",type),
         type = ifelse(driver %in% roots, "Height, seed, and root traits",type),
         type = ifelse(driver %in% life.history, "Life history and stem traits",type),
         type = ifelse(driver %in% stem, "Life history and stem traits",type)) %>% 
  arrange(type, -frequencia) %>% 
  mutate(type = as.factor(type)) %>% 
  rename(`Trait type` = type) %>% 
  rename(Ecosystem = ecosystem)

# cria a gt table com as infos da matriz anterior
fd2 %>% 
  arrange(Ecosystem, `Trait type`,desc(frequencia)) %>% 
  group_by(Ecosystem) %>% 
  mutate(frequencia = round(frequencia, digits = 0)) %>% 
  rename(`Trait used` = driver,
         `Number of papers` = n,
         `Frequency (%)` = frequencia) %>% 
  gt()%>%  
  tab_header(title = md("**Functional traits used to calculate the functional diversity**"),
             subtitle = "Number and frequency of mentions of each trait used to calculate functional diversity") %>% 
  tab_style(style = cell_fill(color = "gray90"),
            locations = cells_column_labels(columns = everything())) %>% 
  cols_align(align = "center",columns = everything()) #%>% 
  #gtsave(filename = "results/traits_to_FD.rtf")

#cria a figura mostrando os traits usados na fd para campo ou floresta
#png("results/traits_FD_ecosystem.png", units="in", width=12, height=8, res=300)
ggplot(fd2, aes(x = reorder(driver, -frequencia), y= frequencia , fill= Ecosystem))+
  facet_wrap(facets = ~(`Trait type`), scales="free_y", ncol = 4) + 
  geom_bar(stat= "identity", width = 0.4, position = "dodge")+
  labs(x = "", y = "Frequency of papers (%)", title = "Effect of functional diversity on productivity") + 
  geom_text(aes(label=n), vjust=0.4, hjust=-0.5, position = position_dodge(width = 0.5), colour="black", size=5) +
  lims(y=c(0,22))+
  coord_flip()+
  scale_x_discrete(limits=rev)+
  scale_fill_manual(values = c("#009E73","#D55E00"))+
  labs(x = "", y = "Frequency (%)", title = "Traits used to calculate functional diversity") +  theme_minimal()  +
  theme(legend.position = "bottom",
    #legend.position = c(0.9,0.05),
    axis.text=element_text(size=15),
    strip.text = element_text(size = 15), 
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
#dev.off()

# matriz para gerar a figura de efeito da fd sobre a produtividade, com ou sem as regioes
# tambem sera usada pra calcular o chi2
#para o chi2 total, apenas remover o group_by(ecosystem)
prod = data %>% filter (prod.metric == 1)
estoque = data %>% filter (prod.metric == 2)
#fd3 <- data %>%
prod <- prod %>% 
#estoque <- estoque %>% 
  filter(ecosystem != "ecotones") %>% 
  drop_na(FD) %>%
  filter(regiao %in% regiao_estudo) %>% 
  mutate(regiao = replace(regiao, regiao == "subtropical" , "tropical")) %>%
  mutate(FD = replace(FD, FD == "negative", "Negative")) %>%
  mutate(FD = replace(FD, FD == "ns", "No relationship")) %>%
  mutate(FD = replace(FD, FD == "positive", "Positive")) %>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "forest", "Forest")) %>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "grassland", "Grassland")) %>%
  rename(Ecosystem = ecosystem) %>% 
  #group_by(region) %>% #region aqui para calcular para as regioes tropical e temperada
  group_by(Ecosystem) %>%#ecosystem aqui para calcular para campo e floresta
  mutate(FD = as.factor(FD)) %>% 
  count(FD) %>%
  mutate(frequencia = (n / sum(n))*100) %>% 
  filter(FD != "yes") 
chisq.test(fd3$n) #X-squared = 6.0541, df = 2, p-value = 0.04846

g = fd3 %>% filter(Ecosystem == "Grassland") 
g1 = chisq.test(g$n) #X-squared = 4.3333, df = 2, p-value = 0.1146

f = fd3 %>% filter(Ecosystem == "Forest")
f1 = chisq.test(f$n) #X-squared = 5.8462, df = 2, p-value = 0.05377


#png("results/FD_effect_ecosystem.png", units="in", width=6, height=5.5, res=300)
#p1 =fd3%>%
#p2 = prod %>% 
#p3 = estoque %>% 
  ggplot(aes(x=Ecosystem, y=frequencia, fill=FD))+geom_bar(stat= "identity") +
  #labs(x = "", y = "Frequency of papers (%)", title = "Relationship between functional diversity \nand productivity") + 
    #labs(x = "", y = "Frequency of papers (%)", title = "Relationship between functional diversity \nand productivity (rate only)") +
  #labs(x = "", y = "Frequency of papers (%)", title = "Relationship between functional diversity \nand productivity (stock only)") +
  scale_fill_manual(values = c("#AA4499","#888888","#44AA99"),guide = guide_legend(
    direction = "horizontal",
    title.position = "top",title.hjust = 0.5))+  
  ylim (0,100) + 
  geom_text(aes(label=n), vjust=-0.5, hjust=0, position=position_stack(vjust=0), colour="black", size=5) +
  theme_minimal()  +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5,size = 15, face = "bold"),
        axis.text=element_text(size=15),
        axis.title.y =element_text(size=15), 
        axis.title.x =element_text(size=15))
#dev.off()
#p1 = total
#p2 = produtividade apenas
#p3 = estoque apenas
# library(patchwork)
plots = (p1|(p2/p3)) +plot_annotation(tag_levels = c("A"))+ plot_layout(widths = c(1, 1))
# png('results/FDonprod.png', units="in", width=13, height=12, res=300)
# plots
# dev.off()
# figura sobre tropical e temperado na fd
data
conjunto <- data %>%
  filter(ecosystem != "ecotones") %>% 
  drop_na(FD) %>%
  filter(regiao %in% regiao_estudo) %>% 
  filter (prod.metric != "1,2")%>% 
  mutate(regiao = replace(regiao, regiao == "subtropical" , "tropical")) %>%
  mutate(FD = replace(FD, FD == "negative", "Negative")) %>%
  mutate(FD = replace(FD, FD == "ns", "No relationship")) %>%
  mutate(FD = replace(FD, FD == "positive", "Positive")) %>%
  mutate(regiao = replace(regiao, regiao == "tropical", "Tropical")) %>%
  mutate(regiao = replace(regiao, regiao == "temperate", "Temperate")) %>%
  rename(Ecosystem = ecosystem) %>% 
  group_by(prod.metric, regiao) %>% 
  #group_by(regiao) %>% 
  mutate(FD = as.factor(FD)) %>% 
  count(FD) %>%
  drop_na(FD) %>%
  mutate(frequencia = round(((n / sum(n))*100), digits=0)) %>% 
  filter(FD != "yes")
all = data %>%
  filter(ecosystem != "ecotones") %>% 
  filter (prod.metric != "1,2") %>% 
  filter(regiao %in% regiao_estudo) %>% 
  mutate(regiao = replace(regiao, regiao == "subtropical" , "tropical")) %>%
  mutate(FD = replace(FD, FD == "negative", "Negative")) %>%
  mutate(FD = replace(FD, FD == "ns", "No relationship")) %>%
  mutate(FD = replace(FD, FD == "positive", "Positive")) %>%
  mutate(regiao = replace(regiao, regiao == "tropical", "Tropical")) %>%
  mutate(regiao = replace(regiao, regiao == "temperate", "Temperate")) %>%
  rename(Ecosystem = ecosystem) %>% 
  group_by(regiao) %>% 
  mutate(FD = as.factor(FD)) %>% 
  count(FD) %>%
  drop_na(FD) %>%
  mutate(frequencia = round(((n / sum(n))*100), digits=0)) 

prod = conjunto %>% filter (prod.metric == 1)
estoque = conjunto %>% filter (prod.metric == 2)
chisq.test(all$n) #X-squared = 7, df = 5, p-value = 0.2206

g = conjunto %>% filter(regiao == "Temperate") 
g1 = chisq.test(g$n) #X-squared = 4.3333, df = 2, p-value = 0.114

f = conjunto %>% filter(regiao == "Tropical")
f1 = chisq.test(f$n) #X-squared = 1.8571, df = 2, p-value = 0.395


#png("results/FD_temperate.tropical.png", units="in", width=9, height=7, res=300)
#p1 = all %>% 
#p2 = prod %>% 
#p3 = estoque %>% 
  ggplot(aes(x = regiao, y= frequencia , fill= FD))+
  geom_bar(stat= "identity")+
  scale_x_discrete(limits=rev)+
  scale_fill_manual(values = c("#AA4499","#888888","#44AA99"),guide = guide_legend(
    direction = "horizontal",
    title.position = "top",title.hjust = 0.5))+ 
  geom_text(aes(label=n), vjust=-0.2, hjust=0.5, position=position_stack(vjust=0), colour="black", size=5) +
  #labs(x = "", y = "Frequency (%)", title = "Relationship between functional diversity and productivity \n across different regions") +  
  #labs(x = "", y = "Frequency (%)", title = "Relationship between functional diversity and productivity \n across different regions (rate only)") +
  #labs(x = "", y = "Frequency (%)", title = "Relationship between functional diversity and productivity \n across different regions (stock only)") +
  theme_minimal()  + 
  theme(legend.position = "bottom",
        #legend.position = c(0.9,0.05),
        axis.text=element_text(size=15),
        strip.text = element_text(size = 15), 
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
#dev.off()
#p1 = total
#p2 = temporal
#p3 = stock
# library(patchwork)
plots = (p1|(p2/p3)) +plot_annotation(tag_levels = c("A"))+ plot_layout(widths = c(1, 1))
# png('results/FDonprod_temptrop.png', units="in",width=13, height=12, res=300)
# plots
# dev.off()

#number of papers
data
summarise = data %>% 
  drop_na(FD) %>% 
  unique() %>% 
  mutate(regiao = replace(regiao, regiao == "subtropical", "tropical")) %>%
  mutate(regiao = replace(regiao, regiao == "temperate", "temperate")) %>% 
  mutate(FD = replace(FD, FD == "negative", "negative")) %>%
  mutate(FD = replace(FD, FD == "ns", "no relationship")) %>%
  mutate(FD = replace(FD, FD == "positive", "positive")) %>%
  mutate(prod.metric = replace(prod.metric, prod.metric == "1", "rate")) %>%
  mutate(prod.metric = replace(prod.metric, prod.metric == "2", "stock")) %>%
  mutate(prod.metric = replace(prod.metric, prod.metric == "1,2", "both")) %>%
  select(Authors,Publication.Year,regiao,ecosystem, FD,prod.metric,traits_FD) %>% 
  relocate(Authors, .after = traits_FD) %>% 
  mutate(aut = unlist(str_split (Authors, ";", simplify = TRUE))) %>% 
  unnest(aut) %>% 
  mutate(aut2 = "et al.") %>% 
  select(Publication.Year,regiao,ecosystem, FD,prod.metric,traits_FD,aut ,aut2) %>% 
  as_tibble() %>% 
  relocate(`Publication.Year`, .after = aut2) %>% 
  as.data.frame() %>% 
  rename(Region = regiao,
         Relationship = FD,
         `Productivity measurement` = prod.metric,
         Year = Publication.Year, 
         Ecosystem = ecosystem,
         Author_name = aut,
         Collabs = aut2) %>% 
  select(-traits_FD)

authors = data.frame(autores =unlist(summarise$Author_name))
summarise <- summarise %>% 
  mutate(Authors = authors$autores.1) %>% 
  relocate(Authors, .before = Collabs) %>% 
  select (Region,Ecosystem,Relationship,`Productivity measurement`,Authors,Collabs,Year) %>% 
  filter (`Productivity measurement` != "both") %>% 
  filter(Ecosystem != "ecotones") %>% 
  mutate(Region = replace(Region, Region == "subtropical;mediterranean", "mediterranean")) %>%  
  mutate(Region = replace(Region, Region == "temperate;boreal", "boreal")) %>%
  mutate(Region = replace(Region, Region == "Temperate, mixed and boreal", "boreal")) %>%
  mutate(Region = replace(Region, Region == "boreal;mediterranean", "boreal and mediterranean")) %>%
  mutate(Region = replace(Region, Region == "subtropical;temperate", "subtropical and temperate")) %>% 
  mutate(Region = replace(Region, Region == "desert-steppe,steppe,semi-steppe,Mediterranean", "steppe and mediterranean")) %>%  
  mutate(Region = replace(Region, Region == "tropical;sub‐tropical;temperate;mediterranean;semi‐arid ", "steppe and mediterranean")) %>%
  mutate(Region = replace(Region, Region == "deciduous;boreal", "boreal")) %>% 
  unite(Authors, c("Authors", "Collabs", "Year"), sep = " ") %>% 
  relocate(Authors, .before = Region)

  
unique(summarise$Authors)

esse <- c("tropical", "temperate")
filter(summarise, Region %in% esse)

gt(summarise)%>%  
  tab_header(title = md("**Functional diversity evaluation**"),
             subtitle = "Description of papers used to evaluate functional diversity and productivity relationship") %>% 
  tab_style(style = cell_fill(color = "gray90"),
            locations = cells_column_labels(columns = everything())) %>% 
  cols_align(align = "center",columns = everything()) #%>% 
gtsave(filename = "results/summary_FD.rtf")
  