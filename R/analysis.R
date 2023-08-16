# analises from PFT chapter
library(tidyverse)
#data <- read.csv(here::here("processed_data", "analises-08.08.23.csv"))
#data_raw <- read.csv(here::here("processed_data", "total.csv"))
#unique(data_raw$Authors)
data_raw <- read.csv(here::here("processed_data", "analises-14.08.23.csv"))
levels(as.factor(data_raw$tipo_ecossistema.eg.campo.floresta.savana.etc.))
ecosys = c("forest", "grassland","steppe","medow-steppes", "dune grassland","forest-steppe ecotones")
posi = c("positiva", "positive", "positivo")
neg = c("negativa", "negative", "negativo")
outros.neg = c("negative(SLA andLDMC)","negative(SLA,height_rep,LCC)")
outros.posi = c("negativo (trait richness)/positivo (trait divergence)", 
                "positive,ns-for-leaf-traits", "positive,ns-for-seed-mass")
grassland = c("agricultural/semi-natural grasslands","dune grassland", "medow-steppes", "steppe")
forest = c("forest;shrublands", "forest;woodlands","forest;tundra")
ecoton = c("forest-steppe ecotones","forest, shrubland, grassland, savanna,","forests, shrubland and sparse grasslands")
data <- data_raw %>%
  select(Authors, Publication.Year, entra:obs_comentarios) %>%
  filter(X2ndo_filtro != "NAO") %>% 
  rename(ecosystem = tipo_ecossistema.eg.campo.floresta.savana.etc.,
         FD = diversidade_funcional.NA.ns.positiva.negativa.,
         traits_FD = traits_FD.traitsincluidos.,
         CWM_LA = cwmLA.NA.ns.positiva.negativa.,
         CWM_SLA = cwmSLA.NA.ns.positiva.negativa.,
         CWM_LDMC = cwmLDMC.NA.ns.positiva.negativa.,
         CWM_WD = cwmWD.NA.ns.positiva.negativa.,
         CWM_LNC = cwmLNC.NA.ns.positiva.negativa.,
         CWM_LCC = cwmLCC.carbon_content.,
         CWM_CaC = cwmLCaC.calcium_content.,
         CWM_seed.size = cwmSeed_size.NA.ns.positiva.negativa.,
         CWM_seed.mass = cwmSeed_mass.NA.ns.positiva.negativa.,
         CWM_RGR = cwmRelative.growth.rate.NA.ns.positiva.negativa.,
         CWM_LCC_LA = cwmLCC.NA.ns.positiva.negativa.,
         CWM_root.mass = cwmRoot_mass.NA.ns.positiva.negativa.,
         CWM_LPC = cwmLPC.NA.ns.positiva.negativa.,
         CWM_height = cwmHeight.NA.ns.positiva.negativa.,
         CWM_Chlorophyll = cwmChlorophyll.NA.ns.positiva.negativa.,
         CWM_leaf.tissue.density = cwmLeaf_tissue_density.NA.ns.positiva.negativa.,
         CWM_LMA = cwmLeaf_mass_area.NA.ns.positiva.negativa.,
         CWM_Dmax = cwmDmax.diametro_max., 
         CWM_crown.complement = cwmCrown_complementarity,
         CWM_leaf.fresh.weight = cwmLeaf_fresh_weight,
         CWM_leaf.tickness = cwmLeaf_thickness,
         CWM_leaf.lifespan = cwmLeaf_lifespan, 
         CWM_LPC_LNC = cwmLP.N,
         CWM_LCC_LNC = cwmLC.N, 
         CWM_leaf.caloric.value = cwmLeaf_caloric_value, 
         CWM_stomatal.dens = cwmStomatal_density) %>%
  filter(str_detect(ecosystem, paste(ecosys, collapse="|")))%>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "grassland ", "grassland")) %>%
  mutate(ecosystem = replace(ecosystem, ecosystem == "forests", "forest")) %>%
  mutate(regiao = replace(regiao, regiao == "subtropical/temperate", "subtropical;temperate")) %>%
  mutate(regiao = replace(regiao, regiao == "temperate;subtropical", "subtropical;temperate")) %>%
  mutate(regiao = replace(regiao, regiao == "temperate;subtropical", "subtropical;temperate")) %>%
  mutate(across(c(FD:CWM_carbon13), ~ifelse(. %in% neg, "negative", as.character(.)))) %>% #all the descriptions to negative
  mutate(across(c(FD:CWM_carbon13), ~ifelse(. %in% posi,"positive", .))) %>% #all the descriptions to positive
  mutate(Publication.Year = as.character(Publication.Year)) %>% 
  mutate_all(~na_if(., '')) %>% #espaço vazio -> NA
  mutate_all(~na_if(., 'na')) %>% #na = NA
  mutate(across(c(FD:CWM_carbon13), ~str_replace_all(., ";", ","))) %>% #replace all ; to ,
  mutate(across(c(FD), ~ifelse(. %in% outros.posi,"positive", .))) %>%#all the descriptions to negative
  mutate(across(c(FD), ~ifelse(. %in% outros.neg,"negative", .))) %>% #all the descriptions to positive
  mutate(ecosystem = replace(ecosystem, ecosystem  %in% grassland , "grassland")) %>% 
  mutate(ecosystem = replace(ecosystem, ecosystem  %in% ecoton , "ecotones")) %>% 
  mutate(ecosystem = replace(ecosystem, ecosystem  %in% forest , "forest"))
  
levels(as.factor(data$ecosystem))
levels(as.factor(data$CWM_SLA))


sla = c("specificleafarea", "SLA","specificleafarea(SLA)")
lm = c("LEAF_MASS", "leaffreshmass","leaf_mass","LM(leafdrymass)")
lma = c( "leaf_mass_area",  "leafmassperunitarea", "LMA")
lnc = c("leafNperarea", "Nitrogen_fraction(%)", "N", "leafδ15N")
n.c = c("razaoC:Nfolha","leafC:Nratio", "LC:N", "leaf_carbon_nitrogen_ratio", "razãocarbono:nitrogênioofplantlitter", "C:N", "razãoC:Nfolha","leafCN","plantCandNconcentration(CCandNC)")
wd = c("stemspecificdensity.", "stemdrymattercontent", "Wood_specific_gravity(WSG)", "wooddensity", "wd")
height = c("High_understory_layer_cover(h.cover)","plantheight", "Maximum_plant_height", "maximumheight", "plant_height_reproductive", 
           "maximum_height", "H", "height", "canopyheight", "CANOPY_HEIGHT",
           "max_height_growth_rate","plantheight(H)")
dec = c("deciduousness", "deciduousness(Dec)", "leaf_habit", "leafseasonality", "leaf_longevity(LL", "LIFE_SPAN")
crown = c("corwnarea", "Can.\ncover")
growth = c("relative-growth-rate", "diam_growth", "maximum_growth_rate")
seed = c("seedlenght", "dispersalunitlenght", "seed_germination(SG)", "SM(seed_mass)", "seedmass", "seed_mass", "numberofseedsofthereproductiveunit", "SM", "leafprolinecontent", "FSMH")
tree.size = c("maximum_stem_diameter(Dmax)", "DBH", "BA", "Density")
lcc = c("leafCperdrymass", "Carbon_fraction(%)", "carbon_fractions", "LCC","Leaf_carbon_content", "leaf_carbon_content","leafcarbon13")
form = c("growthfrom", "growth_habit", "lateral_spread", "leafangle")
tolerance = c("shade_or_drought_tolerance", "drought.toler", "shade.toler", "fire_resistance", "fire_tolerance", "resprout_ability")
root = c("root_density", "mycorrhizal_association", "root_biomass", "ROOT_DEPTH","specificrootlenght","rootdrymattercontent","RTF","AD","RTD","RA")
la = c("leafsize", "leaf_size", "leaflenght")
lt = c("leafthickness", "Leaf_tissue_density", "Leaf_thickness")
vessel = c("woodvessellenght", "stemconduitdensity")
pigment = c("pigment_composition", "Area_based_pigment_content", "Pigment_concentration")
n.p = c("N/P_ratio", "NPreference")
lpc = c("P", "LPC", "LNP")
na = c("month)", "", "coniferas", "photosynthesis_rate","remotesensingvariables","andplantcellulosecontent(CEC)")
# FD --------
fd <- tibble (traits.fd = data$traits_FD) %>%
  separate(traits.fd,into = paste0("fd", 1:30), sep = ",")%>%
  gather(key = "variable", value = "driver", starts_with("fd"), na.rm = TRUE)%>%
  mutate(across(c(driver), ~str_replace_all(., " ", ""))) %>%
  mutate(driver = replace(driver, driver  %in% sla , "SLA")) %>%
  mutate(driver = replace(driver, driver  %in% lma , "LMA"))%>%
  mutate(driver = replace(driver, driver  %in% lm , "LM"))%>%
  mutate(driver = replace(driver, driver  %in% lnc , "LNC")) %>%
  mutate(driver = replace(driver, driver  %in% n.c , "LC:LN")) %>%
  mutate(driver = replace(driver, driver  %in% wd , "WD")) %>%
  mutate(driver = replace(driver, driver  %in% height , "Height")) %>%
  mutate(driver = replace(driver, driver  %in% dec , "deciduousness")) %>%
  mutate(driver = replace(driver, driver  %in% growth , "growth rate")) %>%
  mutate(driver = replace(driver, driver  %in% seed , "seeds")) %>%
  mutate(driver = replace(driver, driver  %in% crown , "crown")) %>%
  mutate(driver = replace(driver, driver  %in% tree.size , "tree size")) %>%
  mutate(driver = replace(driver, driver  %in% lcc , "LCC")) %>%
  mutate(driver = replace(driver, driver  %in% form , "growth form")) %>%
  mutate(driver = replace(driver, driver  %in% tolerance , "stress tolerance")) %>%
  mutate(driver = replace(driver, driver  %in% root , "root")) %>%
  mutate(driver = replace(driver, driver  %in% la , "LA")) %>%
  mutate(driver = replace(driver, driver  %in% lt , "LT")) %>%
  mutate(driver = replace(driver, driver  %in% vessel , "vessels")) %>%
  mutate(driver = replace(driver, driver  %in% pigment , "pigments")) %>%
  mutate(driver = replace(driver, driver  %in% n.p , "LNC:LPC")) %>%
  mutate(driver = replace(driver, driver  %in% lpc , "LPC")) %>%
  mutate(driver = replace(driver, driver  %in% na , "")) %>%
  mutate_all(~na_if(., "")) %>%
  drop_na(driver)

fd2 <- fd %>%
  count(driver) %>%
  mutate(frequencia = (n / sum(n))*100)

struc = c("Height","LDMC","WD","vessels","LT","growth form","crown","LM","root","LA","SLA","stress tolerance","deciduousness","LT","LMA","seeds")
nutrients = c("LCaC","LNC:LPC","pigments","LC:LN","LCC","LPC","LNC")
demograp = c("age","growth rate","tree size")

fd2 <- fd2 %>% 
  mutate(type = ifelse(driver %in% struc, "Structural traits",NA),
         type = ifelse(driver %in%nutrients, "Nutrient traits",type),
         type = ifelse(driver %in%demograp, "Demographic traits",type)) %>% 
  arrange(type, -frequencia)



#tiff("results/traits_FD.tiff", units="in", width=7, height=12, res=300)

ggplot(fd2, aes(x = driver, y=frequencia, fill=type))+
  geom_bar(stat= "identity", width = 0.4)+
  scale_x_discrete(limits=rev)+
  coord_flip()+
  facet_wrap(~ type, nrow = 3, ncol = 1, scales="free_y") + 
  scale_y_continuous(breaks = seq(0, 14, by = 1.5))+
  scale_fill_manual(values = c("#009E73","#0072B2","#D55E00"))+
  labs(x = "", y = "Frequency (%)", title = "Traits used to calculate FD") +  theme_minimal()  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 15),
        axis.text=element_text(size=15),
        strip.text = element_text(size = 15))
#dev.off()
 
data
#tiff("results/FD_effect.tiff", units="in", width=6.5, height=5.5, res=300)
data %>%
  group_by(FD)%>%
  mutate(FD = as.factor(FD)) %>%
  count(FD) %>%
  drop_na(FD) %>%
  filter(FD != "yes") %>% 
  ggplot(aes(x=FD, y=n, fill=FD))+geom_bar(stat= "identity") +
  labs(x = "", y = "Number of papers", title = "Effect of FD on productivity") + 
  scale_x_discrete(labels=c("Positive", "No relationship","Negative" ))+
  scale_fill_manual(values = c("#882255","#CC6677", "#332288"))+
  theme_minimal()  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=15),
        axis.title.y =element_text(size=15), 
        axis.title.x =element_text(size=15))
#dev.off()
# CWM----

cwm.filter <- function (x, trait) {
  data <- x %>% 
    select(trait)  %>%
    separate(trait,into = paste0(trait, 1:5), sep = ",")%>%
    gather(key = "variable", value = "driver", starts_with(trait), na.rm = TRUE)%>%
    mutate_all(~na_if(., "")) %>%
    drop_na(driver)

fd2 <- data %>%
  count(driver) %>%
  mutate(frequencia = (n / sum(n))*100) %>% 
  mutate(driver = replace(driver, driver == "negative", "Negative")) %>%
  mutate(driver = replace(driver, driver == "positive", "Positive")) %>%
  mutate(driver = replace(driver, driver == "ns", "No relationship")) 
  
fd2$driver <- factor(fd2$driver, levels = c("Positive", "Negative", "No relationship"))
fd2 <- fd2%>%
  drop_na()
# safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
#                              "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
safe_colorblind_palette <- c("#882255","#CC6677", "#332288")

names(safe_colorblind_palette) <- levels(fd2$driver)
colScale <- scale_fill_manual(name = "driver",values = safe_colorblind_palette)
trait = sub("_", " ", trait)
title = paste0("Effect of ",trait," on productivity")
plot <- ggplot(fd2, aes(x = fct_infreq(driver), y=frequencia, fill=driver))+
  geom_bar(stat= "identity")+
  #scale_x_discrete(labels=c( "Positive","Negative", "No relationship"))+
  colScale+
  labs(x = trait, y = "Frequency (%)", title = title) +  theme_minimal()  +
  theme(aspect.ratio = 1,
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=15), 
        axis.title.y =element_text(size=15), 
        axis.title.x =element_text(size=15),
        axis.title = element_text(size=15))
ggsave(filename = paste0("results/grafico_",trait,".png"), plot = plot, units="in", width=5, height=5, dpi=300)

}
names(data)
cwm.filter(data, "CWM_LA")
cwm.filter(data, "CWM_SLA")
cwm.filter(data, "CWM_LDMC")
cwm.filter(data, "CWM_WD")
cwm.filter(data, "CWM_LNC")
cwm.filter(data, "CWM_LCC")
cwm.filter(data, "CWM_CaC")
cwm.filter(data, "CWM_seed.size")
cwm.filter(data, "CWM_seed.mass")
cwm.filter(data, "CWM_LPC")
cwm.filter(data, "CWM_height")
cwm.filter(data, "CWM_Chlorophyll")
cwm.filter(data, "CWM_leaf.tissue.density")
cwm.filter(data, "CWM_LMA")
cwm.filter(data, "CWM_RGR")
cwm.filter(data, "CWM_LCC_LA")
cwm.filter(data, "CWM_LPC_LNC")
cwm.filter(data, "CWM_root.mass")
cwm.filter(data, "CWM_leaf.tickness")
cwm.filter(data, "CWM_Dmax")
cwm.filter(data, "canopy_height")

# tabela com frequencias ---------------

# Carregar os pacotes
library(gt)
library(janitor)
library(dplyr)

# Criar um exemplo de dataframe
dados <- data %>% 
  select(CWM_LA: CWM_carbon13)

# Transformar os dados em um formato adequado para contagem
dados_contagem <- dados %>%
  pivot_longer(everything(), names_to = "Variables", values_to = "Valores") %>%
  mutate(Valores = replace(Valores, Valores == "ns", "No relationship")) %>%
  mutate(Valores = replace(Valores, Valores == "positive", "Positive")) %>%
  mutate(Valores = replace(Valores, Valores == "negative", "Negative")) %>%
  group_by(Variables, Valores) %>%
  summarize(`Number of papers` = n()) %>% 
  drop_na(Valores) %>% 
  group_by(Variables) %>% 
  filter(sum(`Number of papers`)>2) %>% 
  mutate(Variables = replace(Variables, Variables == "CWM_leaf.tickness", "CWM leaf_tickness")) %>%
  mutate(Variables = replace(Variables, Variables == "CWM_root.mass", "CWM_root mass")) %>%
  mutate(Variables = replace(Variables, Variables == "CWM_seed.mass", "CWM_seed mass")) %>% 
  mutate(Variables = replace(Variables, Variables == "CWM_LCC_LNC", "CWM_LCC LNC")) %>% 
  mutate(Variables = replace(Variables, Variables == "CWM_LCC_LA", "CWM_LCC LA")) 
  
  
dados_contagem$Variables <-   sub("_", " ", dados_contagem$Variables)
  
unique(dados_contagem$Variables)

# Criar a tabela com o pacote gt
dados_contagem %>%
  filter(Valores != "positive(temperate),negative(subtropical)") %>% 
  group_by(Variables) %>% 
  mutate(frequencia = round(`Number of papers`/sum(`Number of papers`) * 100, digits = 2)) %>% 
  gt(groupname_col = "Variables", rowname_col = "Valores") %>%
  cols_label(Variables = "Variables", Valores = "Mean traits", `Number of papers` = "Number of papers",frequencia = "Frequency (%)") %>%  
  tab_header(title = md("**Functional traits as predictors of productivity**"),
    subtitle = "Based on community weighted mean values (i.e., mass effect)") %>% 
  tab_style(style = cell_fill(color = "gray90"),
    locations = cells_column_labels(columns = everything())) %>% 
  tab_style(style = cell_text(style = "italic"),
    locations = cells_body()) %>% 
  gt_plt_bar_pct(column = frequencia, scaled = TRUE,fill = "blue", background = "lightblue") %>% 
  cols_align(align = "center",columns = everything()) %>% 
  gtsave(filename = "results/CWM_effects.html")  

dados_contagem %>%
  filter(Valores != "positive(temperate),negative(subtropical)") %>% 
  group_by(Variables) %>% 
  mutate(frequencia = round(`Number of papers`/sum(`Number of papers`) * 100, digits = 2)) %>% 
  gt(groupname_col = "Variables", rowname_col = "Valores") %>%
  cols_label(Variables = "Variables", Valores = "Mean traits", `Number of papers` = "Number of papers",frequencia = "Frequency (%)") %>%  
  tab_header(title = md("**Functional traits as predictors of productivity**"),
             subtitle = "Based on community weighted mean values (i.e., mass effect)") %>% 
  tab_style(style = cell_fill(color = "gray90"),
            locations = cells_column_labels(columns = everything())) %>% 
  tab_style(style = cell_text(style = "italic"),
            locations = cells_body()) %>% 
  #gt_plt_bar_pct(column = frequencia, scaled = TRUE,fill = "blue", background = "lightblue") %>% 
  cols_align(align = "center",columns = everything()) %>% 
  gtsave(filename = "results/CWM_effects.rtf")
