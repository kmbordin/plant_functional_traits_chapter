# Bordin et al.: The use of functional traits in assessing productivity in natural ecosystems
# Folder to analyse functional diversity (niche complementarity)

# load these datasets first
here::here("1.load_harmonise.R")
cat = readxl::read_excel(here::here("processed_data", "categorisation.xlsx"))

# tropical and temperate regions ----
regiao_estudo = c("temperate", "tropical", "subtropical")

# grouping traits -----
roots = c("Root quantity")
seeds = c("Seed size")
life.history = c("Growth rate","Growth form","Tree size")
# height = c("Height","Crown size")
stem = c("WD","Vessel quantity","Stress tolerance","Deciduousness")
leaves = c("LDMC","LT","LM","LA","SLA","LT","LCaC","LNC:LPC","Pigments","LCC:LNC","LCC","LPC","LNC")
prod_type = c("1","2") # productivity measure = 1 means rate, 2 means stocks
regiao_estudo = c("temperate", "tropical", "subtropical")
ecosys_type= c("forest", "grassland")

region = c("Tropical","subtropical","Temperate")
ecosys = c("Forest", "Grassland")
metric = c("1","2") #1 means rate, 2 means stocks

# harmonising table for FD analysis ----
data <- data %>% filter(ecosystem %in% ecosys_type) #filter ecosystems
data <- data %>% filter(regiao %in% regiao_estudo) #filter study regions
data <- data %>% filter(prod.metric %in% prod_type) #filter productivity metrics

# new df with variables of interest
fd <- tibble (traits.fd = data$traits_FD, 
              ecosystem = data$ecosystem,
              region = data$regiao, 
              metric = data$prod.metric, 
              relation=data$FD) %>%
  filter(ecosystem %in% ecosys_type) %>% 
  filter(region %in% regiao_estudo) %>% 
  mutate(region = replace(region, region == "subtropical" , "tropical")) %>%
  mutate(across(c(traits.fd), ~str_replace_all(., ";", ","))) %>%
  separate(traits.fd,into = paste0("fd", 1:30), sep = ",")%>%
  gather(key = "variable", value = "driver", starts_with("fd"), na.rm = TRUE)%>%
  mutate(across(c(driver), ~str_replace_all(., " ", ""))) %>%
  mutate(driver = replace(driver, driver  %in% sla , "SLA")) %>%
  mutate(driver = replace(driver, driver  %in% lma , "SLA"))%>% #SLA = 1/LMA; already transformed
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
  filter (driver != "Age") %>% 
  mutate(type = ifelse(driver %in% leaves, "Leaf traits",NA),
         type = ifelse(driver %in% seeds, "Height, seed, and root traits",type),
         type = ifelse(driver %in% height, "Height, seed, and root traits",type),
         type = ifelse(driver %in% roots, "Height, seed, and root traits",type),
         type = ifelse(driver %in% life.history, "Life history and stem traits",type),
         type = ifelse(driver %in% stem, "Life history and stem traits",type)) %>% 
  filter(ecosystem != "ecotones") %>% 
  mutate(ecosystem = replace(ecosystem, ecosystem == "forest", "Forest")) %>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "grassland", "Grassland")) %>% 
  mutate(region = replace(region, region == "tropical", "Tropical")) %>%
  mutate(region = replace(region, region == "temperate", "Temperate")) %>% 
  mutate(relation = replace(relation, relation == "negative", "Negative")) %>%
  mutate(relation = replace(relation, relation == "ns", "No relationship")) %>%
  mutate(relation = replace(relation, relation == "positive", "Positive")) 

# grouping per region ----
region_eval <- fd %>% 
  group_by(region) %>% 
  count(driver) %>%
  mutate(frequencia = round((n / sum(n))*100, digits = 0)) %>% 
  rename(`Trait type` = driver, 
         var = region) 

# grouping per ecosystem ----
ecosys_eval <- fd %>% 
  group_by(ecosystem) %>% 
  count(driver) %>%
  mutate(frequencia = round((n / sum(n))*100, digits = 0)) %>% 
  rename(`Trait type` = driver, 
         var = ecosystem)  

# grouping per metric ----
metric_eval <- fd %>% 
  filter(metric != "1,2") %>% 
  group_by(metric) %>% 
  count(driver) %>%
  mutate(frequencia = round((n / sum(n))*100, digits = 0)) %>% 
  rename(`Trait type` = driver, 
         var = metric)  

# combining datasets and saving table 2 ----
fd.eval <- bind_rows(ecosys_eval,region_eval, metric_eval) %>% 
  select(-frequencia) %>% 
  pivot_wider(names_from = var,values_from = n) %>% 
  rename(Rate = `1`,
         Stock = `2`) %>% 
  replace(is.na(.),0) %>% 
  filter(`Trait type` %in% cat$`Traits used in this study`) %>% 
  arrange()

#write.table(fd.eval, "results/fd.eval.txt")

# number of studies of FD per region -----
n.papers.fd = data %>% 
  drop_na(FD) %>% 
  unique() 
n.p.for = n.papers.fd %>% filter(ecosystem=="forest")   
n.p.gras = n.papers.fd %>% filter(ecosystem=="grassland")  
n.p.trop = n.papers.fd %>%
  mutate(regiao = replace(regiao, regiao == "subtropical" , "tropical")) %>%
  filter(regiao=="tropical")  
n.p.temp = n.papers.fd %>%
  filter(regiao=="temperate") 

# new df to use in plots -----
fd_new <- data %>%  
  mutate(regiao = replace(regiao, regiao == "subtropical" , "tropical")) %>%
  mutate(regiao = replace(regiao, regiao == "tropical" , "Tropical")) %>%
  mutate(regiao = replace(regiao, regiao == "temperate" , "Temperate")) %>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "forest", "Forest")) %>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "grassland", "Grassland")) %>%
  mutate(FD = replace(FD, FD == "negative", "Negative")) %>%
  mutate(FD = replace(FD, FD == "ns", "No relationship")) %>%
  mutate(FD = replace(FD, FD == "positive", "Positive")) %>% 
  drop_na(FD)

eval <- fd_new %>% 
  filter(regiao %in% region) %>%
  filter(ecosystem %in% ecosys) %>% 
  filter(prod.metric %in% metric) %>% 
  rename(Region = regiao,
         Ecosystem = ecosystem)

# tests per region ----
# all regions 
region_all <- eval %>% 
  group_by(Region, FD) %>% 
  count() %>%
  group_by(Region) %>% 
  mutate(`Frequency (%)` = round((n / sum(n))*100, digits = 0)) %>% 
  rename(Relationship = FD) 

# productivity
region_prod <- eval %>% 
  filter(prod.metric == "1") %>% 
  group_by(Region, FD) %>% 
  count() %>%
  group_by(Region) %>% 
  mutate(`Frequency (%)` = round((n / sum(n))*100, digits = 0)) %>% 
  rename(Relationship = FD) 

# stock
region_stock <- eval %>% 
  filter(prod.metric == "2") %>% 
  group_by(Region, FD) %>% 
  count() %>%
  group_by(Region) %>% 
  mutate(`Frequency (%)` = round((n / sum(n))*100, digits = 0)) %>% 
  rename(Relationship = FD) 

# chi-square tests for regions -----
chisq.test(region_all$n) #X-squared = 7, df = 5, p-value = 0.2206
g = region_all %>% filter(Region == "Temperate") 
g1 = chisq.test(g$n) #X-squared = 4.3333, df = 2, p-value = 0.114
f = region_all %>% filter(Region == "Tropical")
f1 = chisq.test(f$n) #X-squared = 1.8571, df = 2, p-value = 0.395

# tests per ecosystem ----
# all ecosystems
ecosys_all <- eval %>% 
  group_by(Ecosystem, FD) %>% 
  count() %>%
  group_by(Ecosystem) %>% 
  mutate(`Frequency (%)` = round((n / sum(n))*100, digits = 0)) %>% 
  rename(Relationship = FD) 

# productivity
ecosys_prod <- eval %>% 
  filter(prod.metric == "1") %>% 
  group_by(Ecosystem, FD) %>% 
  count() %>%
  group_by(Ecosystem) %>% 
  mutate(`Frequency (%)` = round((n / sum(n))*100, digits = 0)) %>% 
  rename(Relationship = FD) 

# stocks
ecosys_stock <- eval %>% 
  filter(prod.metric == "2") %>% 
  group_by(Ecosystem, FD) %>% 
  count() %>%
  group_by(Ecosystem) %>% 
  mutate(`Frequency (%)` = round((n / sum(n))*100, digits = 0)) %>% 
  rename(Relationship = FD) 

# chi-square tests per ecosystems -----
chisq.test(ecosys_all$n) #X-squared = 7.375, df = 5, p-value = 0.1942
g = ecosys_all %>% filter(Ecosystem == "Grassland") 
g1 = chisq.test(g$n) #X-squared = 5.7647, df = 2, p-value = 0.056
f = ecosys_all %>% filter(Ecosystem == "Forest")
f1 = chisq.test(f$n) #X-squared = 1.2, df = 2, p-value = 0.5488

# figures ------
themes <- theme_minimal()  + 
  theme(legend.position = "bottom",
        axis.text=element_text(size=15),
        strip.text = element_text(size = 15), 
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size=13))

# regions ----
p1 = region_all %>% 
ggplot(aes(x = Region, y= `Frequency (%)` , fill= Relationship))+
  geom_bar(stat= "identity") +  geom_text(aes(label=n), vjust=-0.2, hjust=0.5, position=position_stack(vjust=0), colour="black", size=10)+themes+scale_x_discrete(limits=rev)+
  scale_fill_manual(values = c("#AA4499","#888888","#44AA99"),guide = guide_legend(
    direction = "horizontal", title.position = "top",title.hjust = 0.5))+
  labs(x = "", y = "Frequency (%)", title = "Relationship between functional diversity and productivity \n across different regions")

p2 = region_prod %>% 
  ggplot(aes(x = Region, y= `Frequency (%)` , fill= Relationship))+
  geom_bar(stat= "identity") +  geom_text(aes(label=n), vjust=-0.2, hjust=0.5, position=position_stack(vjust=0), colour="black", size=10)+themes+scale_x_discrete(limits=rev)+
  scale_fill_manual(values = c("#AA4499","#888888","#44AA99"),guide = guide_legend(
    direction = "horizontal", title.position = "top",title.hjust = 0.5))+
  labs(x = "", y = "Frequency (%)", title = "Relationship between functional diversity and productivity \n across different regions (rate only)") 

p3 = region_stock %>% 
  ggplot(aes(x = Region, y= `Frequency (%)` , fill= Relationship))+
  geom_bar(stat= "identity") +  geom_text(aes(label=n), vjust=-0.2, hjust=0.5, position=position_stack(vjust=0), colour="black", size=10)+themes+scale_x_discrete(limits=rev)+
  scale_fill_manual(values = c("#AA4499","#888888","#44AA99"),guide = guide_legend(
    direction = "horizontal", title.position = "top",title.hjust = 0.5))+
  labs(x = "", y = "Frequency (%)", title = "Relationship between functional diversity and productivity \n across different regions (stock only)")

#p1 = total
#p2 = temporal
#p3 = stock
# library(patchwork)
plots = (p1|(p2/p3)) +plot_annotation(tag_levels = c("A"))+ plot_layout(widths = c(1, 1))
# png('results/FDonprod_temptrop.png', units="in",width=15, height=10, res=300)
# plots
# dev.off()

# ecosystems ----
p1 = ecosys_all %>% 
  ggplot(aes(x = Ecosystem, y= `Frequency (%)` , fill= Relationship))+
  geom_bar(stat= "identity") +  geom_text(aes(label=n), vjust=-0.2, hjust=0.5, position=position_stack(vjust=0), colour="black", size=10)+themes+scale_x_discrete(limits=rev)+
  scale_fill_manual(values = c("#AA4499","#888888","#44AA99"),guide = guide_legend(
    direction = "horizontal", title.position = "top",title.hjust = 0.5))+
  labs(x = "", y = "Frequency (%)", title = "Relationship between functional diversity and productivity \n across different ecosystems")
p2 = ecosys_prod %>% 
  ggplot(aes(x = Ecosystem, y= `Frequency (%)` , fill= Relationship))+
  geom_bar(stat= "identity") +  geom_text(aes(label=n), vjust=-0.2, hjust=0.5, position=position_stack(vjust=0), colour="black", size=10)+themes+scale_x_discrete(limits=rev)+
  scale_fill_manual(values = c("#AA4499","#888888","#44AA99"),guide = guide_legend(
    direction = "horizontal", title.position = "top",title.hjust = 0.5))+
  labs(x = "", y = "Frequency (%)", title = "Relationship between functional diversity and productivity \n across different ecosystems (rate only)") 
p3 = ecosys_stock %>% 
  ggplot(aes(x = Ecosystem, y= `Frequency (%)` , fill= Relationship))+
  geom_bar(stat= "identity") +  geom_text(aes(label=n), vjust=-0.2, hjust=0.5, position=position_stack(vjust=0), colour="black", size=10)+themes+scale_x_discrete(limits=rev)+
  scale_fill_manual(values = c("#AA4499","#888888","#44AA99"),guide = guide_legend(
    direction = "horizontal", title.position = "top",title.hjust = 0.5))+
  labs(x = "", y = "Frequency (%)", title = "Relationship between functional diversity and productivity \n across different ecosystems (stock only)")
#p1 = total
#p2 = temporal
#p3 = stock
# library(patchwork)
plots = (p1|(p2/p3)) +plot_annotation(tag_levels = c("A"))+ plot_layout(widths = c(1, 1))
# png('results/FDonprod_ecosys.png',units="in",width=15, height=10, res=300)
# plots
# dev.off()
