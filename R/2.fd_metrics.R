# fd evaluation -----

fd <- tibble (traits.fd = data$traits_FD, 
              ecosystem = data$ecosystem) %>%
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

fd2 <- fd %>%
  group_by(ecosystem) %>% 
  filter(ecosystem != "ecotones") %>% 
  mutate(ecosystem = replace(ecosystem, ecosystem == "forest", "Forest")) %>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "grassland", "Grassland")) %>%
  count(driver) %>%
  filter(n>=3) %>% 
  mutate(frequencia = (n / sum(n))*100) %>% 
  mutate(frequencia = round(frequencia, digits = 0)) 


x %>% 
  rename(`Trait type` = driver,
         `Number of papers` = n,
         Frequency = frequencia) %>% 
  gt()%>%  
  tab_header(title = md("**Functional traits used to obtain the Functional Diversity**"),  subtitle = "Number and frequency of mentions of each trait used to calculate functional diversity ") %>% 
  tab_style(style = cell_fill(color = "gray90"),
            locations = cells_column_labels(columns = everything())) %>% 
  cols_align(align = "center",columns = everything()) #%>% 
#gtsave(filename = "results/traits_to_FD_semecosys.rtf")

roots = c("Root quantity")
seeds = c("Seed size")
life.history = c("Growth rate","Growth form","Tree size")
height = c("Height","Crown size")
stem = c("WD","Vessel quantity","Stress tolerance","Deciduousness")
leaves = c("LDMC","LT","LM","LA","SLA","LT","LCaC","LNC:LPC","Pigments","LCC:LNC","LCC","LPC","LNC")

fd2 <- fd2 %>% 
  mutate(type = ifelse(driver %in% leaves, "Leaf traits",NA),
         type = ifelse(driver %in% seeds, "Seed traits",type),
         type = ifelse(driver %in% height, "Height traits",type),
         type = ifelse(driver %in% roots, "Root traits",type),
         type = ifelse(driver %in% life.history, "Life history traits",type),
         type = ifelse(driver %in% stem, "Stem traits",type)) %>% 
  arrange(type, -frequencia) %>% 
  mutate(type = as.factor(type)) %>% 
  rename(`Trait type` = type) %>% 
  rename(Ecosystem = ecosystem)

fd2 %>% 
  arrange(Ecosystem, `Trait type`,desc(frequencia)) %>% 
  group_by(Ecosystem) %>% 
  mutate(frequencia = round(frequencia, digits = 0)) %>% 
  rename(`Trait used` = driver,
         `Number of papers` = n,
         `Frequency (%)` = frequencia) %>% 
  gt()%>%  
  tab_header(title = md("**Functional traits used to obtain the Functional Diversity**"),
             subtitle = "Number and frequency of mentions of each trait used to calculate functional diversity ") %>% 
  tab_style(style = cell_fill(color = "gray90"),
            locations = cells_column_labels(columns = everything())) %>% 
  cols_align(align = "center",columns = everything()) %>% 
  gtsave(filename = "results/traits_to_FD.rtf")

#png("results/traits_FD_ecosystem.png", units="in", width=8, height=10, res=300)

ggplot(fd2, aes(x = driver, y=frequencia, fill= Ecosystem))+
  geom_bar(stat= "identity", width = 0.4)+
  scale_x_discrete(limits=rev)+
  coord_flip()+
  facet_wrap(~ `Trait type`, scales="free_y", ncol = 1, ) + 
  scale_fill_manual(values = c("#009E73","#D55E00"))+
  labs(x = "", y = "Frequency (%)", title = "Traits used to calculate FD") +  theme_minimal()  +
  theme(legend.position = c(0.9,0.05),
        plot.title = element_text(hjust = 0.5, size = 15),
        axis.text=element_text(size=15),
        strip.text = element_text(size = 15))
#dev.off()


fd3 <- data %>%
  filter(ecosystem != "ecotones") %>% 
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
g1 = chisq.test(g$n)

f = fd3 %>% filter(Ecosystem == "Forest")
f1 = chisq.test(f$n)

#png("results/FD_effect_ecosystem.png", units="in", width=6, height=5.5, res=300)
fd3%>% 
  ggplot(aes(x=FD, y=frequencia, fill=Ecosystem))+geom_bar(stat= "identity") +
  labs(x = "", y = "Frequency of papers (%)", title = "Effect of FD on productivity") + 
  scale_fill_manual(values = c("#009E73","#D55E00"),guide = guide_legend(
    direction = "horizontal",
    title.position = "top",title.hjust = 0.5))+  
  ylim (0,100) + 
  theme_minimal()  +
  theme(legend.position = c(0.15,0.95),
        plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=15),
        axis.title.y =element_text(size=15), 
        axis.title.x =element_text(size=15))
#dev.off()