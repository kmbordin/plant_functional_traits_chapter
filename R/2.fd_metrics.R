# fd evaluation -----
#precisa carregar esse script antes
here::here("1.load_harmonise.R")
#isso e para criar apenas as regioes tropicais e temperadas
regiao_estudo = c("temperate", "tropical", "subtropical")
#nova matriz, corrigindo e agrupando traits, com a info de regiao
#essa matriz fica ao final com os traits em linhas, separadamente, além da info de ecossistema e regiao
fd <- tibble (traits.fd = data$traits_FD, 
              ecosystem = data$ecosystem,
              region = data$regiao) %>%
  #filter(region %in% regiao_estudo) %>% 
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
fd2 <- fd %>%
  group_by(ecosystem) %>% 
  filter(ecosystem != "ecotones") %>% 
  mutate(ecosystem = replace(ecosystem, ecosystem == "forest", "Forest")) %>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "grassland", "Grassland")) %>%
  count(driver) %>%
  filter(n>=3) %>% 
  mutate(frequencia = (n / sum(n))*100) %>% 
  mutate(frequencia = round(frequencia, digits = 0)) 

fd2 %>% 
  #x %>%  #criada a partir do anterior, sem group_by(ecosystem)! para saber os valores totais de traits usados na fd
  rename(`Trait type` = driver,
         `Number of papers` = n,
         `Frequency (%)` = frequencia) %>% 
  gt()%>%  
  tab_header(title = md("**Functional traits used to calculate the Functional Diversity**"),  subtitle = "Number and frequency of mentions of each trait used to calculate functional diversity ") %>% 
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
         type = ifelse(driver %in% life.history, "Life history traits",type),
         type = ifelse(driver %in% stem, "Stem traits",type)) %>% 
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
  tab_header(title = md("**Functional traits used to calculate the Functional Diversity**"),
             subtitle = "Number and frequency of mentions of each trait used to calculate functional diversity") %>% 
  tab_style(style = cell_fill(color = "gray90"),
            locations = cells_column_labels(columns = everything())) %>% 
  cols_align(align = "center",columns = everything()) %>% 
  gtsave(filename = "results/traits_to_FD.rtf")

#cria a figura mostrando os traits usados na fd para campo ou floresta
# png("results/traits_FD_ecosystem.png", units="in", width=6, height=10, res=300)
ggplot(fd2, aes(x = reorder(driver, -frequencia), y= frequencia , fill= Ecosystem))+
  geom_bar(stat= "identity", width = 0.4)+
  coord_flip()+
  facet_wrap(facets = ~(`Trait type`), scales="free_y", ncol = 1) + 
  scale_x_discrete(limits=rev)+
  scale_fill_manual(values = c("#009E73","#D55E00"))+
  labs(x = "", y = "Frequency (%)", title = "Traits used to calculate functional diversity") +  theme_minimal()  +
  theme(legend.position = "bottom",
    #legend.position = c(0.9,0.05),
    axis.text=element_text(size=15),
    strip.text = element_text(size = 15), 
    plot.title = element_text(size = 20, face = "bold"))
#dev.off()

# matriz para gerar a figura de efeito da fd sobre a produtividade, com ou sem as regioes
# tambem sera usada pra calcular o chi2
#para o chi2 total, apenas remover o group_by(ecosystem)
fd3 <- data %>%
  filter(ecosystem != "ecotones") %>% 
  #filter(regiao %in% regiao_estudo) %>% 
  #mutate(regiao = replace(regiao, regiao == "subtropical" , "tropical")) %>%
  mutate(FD = replace(FD, FD == "negative", "Negative")) %>%
  mutate(FD = replace(FD, FD == "ns", "No relationship")) %>%
  mutate(FD = replace(FD, FD == "positive", "Positive")) %>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "forest", "Forest")) %>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "grassland", "Grassland")) %>%
  rename(Ecosystem = ecosystem) %>% 
  group_by(Ecosystem) %>% 
  mutate(FD = as.factor(FD)) %>% 
  count(FD) %>%
  drop_na(FD) %>%
  mutate(frequencia = (n / sum(n))*100) %>% 
  filter(FD != "yes") 
chisq.test(fd3$n) #X-squared = 4.9545, df = 2, p-value = 0.08397

g = fd3 %>% filter(Ecosystem == "Grassland") 
g1 = chisq.test(g$n) #X-squared = 1.3, df = 2, p-value = 0.522

f = fd3 %>% filter(Ecosystem == "Forest")
f1 = chisq.test(f$n) #X-squared = 4, df = 2, p-value = 0.1353


#png("results/FD_effect_ecosystem.png", units="in", width=6, height=5.5, res=300)
fd3%>% 
  ggplot(aes(x=FD, y=frequencia, fill=Ecosystem))+geom_bar(stat= "identity") +
  labs(x = "", y = "Frequency of papers (%)", title = "Effect of functional diversity on productivity") + 
  scale_fill_manual(values = c("#009E73","#D55E00"),guide = guide_legend(
    direction = "horizontal",
    title.position = "top",title.hjust = 0.5))+  
  ylim (0,100) + 
  theme_minimal()  +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5,size = 15, face = "bold"),
        axis.text=element_text(size=15),
        axis.title.y =element_text(size=15), 
        axis.title.x =element_text(size=15))
#dev.off()

# ----------
data
conjunto <- tibble (traits.fd = data$traits_FD, 
              ecosystem = data$ecosystem,
              region = data$regiao,
              functdiv = data$FD) %>%
  #filter(region %in% regiao_estudo) %>% 
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

conj = conjunto %>%
  group_by(functdiv,ecosystem) %>% 
  filter(ecosystem != "ecotones") %>% 
  mutate(ecosystem = replace(ecosystem, ecosystem == "forest", "Forest")) %>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "grassland", "Grassland")) %>%
  count(driver) %>%
  filter(n>=3) %>% 
  mutate(frequencia = (n / sum(n))*100) %>% 
  mutate(frequencia = round(frequencia, digits = 0)) %>% 
  mutate(functdiv  = replace(functdiv , functdiv  == "negative", "Negative")) %>%
  mutate(functdiv  = replace(functdiv , functdiv  == "positive", "Positive")) %>%
  mutate(functdiv  = replace(functdiv , functdiv  == "ns", "No relationship")) %>%
  rename(Ecosystem = ecosystem) %>% 
  filter (driver != "Tree size")

#png("results/FD_conjunto.png", units="in", width=7, height=9, res=300)
ggplot(conj, aes(x = reorder(driver, +frequencia), y= frequencia , fill= Ecosystem))+
  geom_bar(stat= "identity", width = 0.4)+
  coord_flip()+
  facet_wrap(facets = ~(functdiv), scales="free_y", ncol = 1) + 
  scale_x_discrete(limits=rev)+
  scale_fill_manual(values = c("#009E73","#D55E00"))+
  labs(x = "", y = "Frequency (%)", title = "Trait effect on functional diversity") +  theme_minimal()  +
  theme(legend.position = "bottom",
        #legend.position = c(0.9,0.05),
        axis.text=element_text(size=15),
        strip.text = element_text(size = 15), 
        plot.title = element_text(size = 20, face = "bold"))
#dev.off()

conjunto
trop = filter(conjunto,region =="tropical")
temp = filter(conjunto, region =="temperate")
an = bind_rows(trop, temp)

kk = an %>% 
  group_by(functdiv,ecosystem, region) %>% 
  filter(ecosystem != "ecotones") %>% 
  mutate(ecosystem = replace(ecosystem, ecosystem == "forest", "Forest")) %>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "grassland", "Grassland")) %>%
  count(driver) %>%
  filter(n>=3) %>% 
  mutate(frequencia = (n / sum(n))*100) %>% 
  mutate(frequencia = round(frequencia, digits = 0)) %>% 
  mutate(functdiv  = replace(functdiv , functdiv  == "negative", "Negative")) %>%
  mutate(functdiv  = replace(functdiv , functdiv  == "positive", "Positive")) %>%
  mutate(functdiv  = replace(functdiv , functdiv  == "ns", "No relationship")) %>%
  rename(Ecosystem = ecosystem) %>% 
  filter (driver != "Tree size")
ggplot(kk, aes(x = reorder(driver, +frequencia), y= frequencia , fill= functdiv ))+
  geom_bar(stat= "identity", width = 0.4)+
  coord_flip()+
  facet_wrap(facets = ~(functdiv), scales="free_y", ncol = 1) + 
  scale_x_discrete(limits=rev)+
  scale_fill_manual(values = c("#009E73","#D55E00"))+
  labs(x = "", y = "Frequency (%)", title = "Trait effect on functional diversity") +  theme_minimal()  +
  theme(legend.position = "bottom",
        #legend.position = c(0.9,0.05),
        axis.text=element_text(size=15),
        strip.text = element_text(size = 15), 
        plot.title = element_text(size = 20, face = "bold"))
