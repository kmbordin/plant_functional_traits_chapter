# analises from PFT chapter
library(tidyverse)
data <- read.csv(here::here("processed_data", "analises-08.08.23.csv"))
ecosys = c("forest", "grassland")
posi = c("positiva", "positive", "positivo")
neg = c("negativa", "negative", "negativo")
outros.neg = c("negative(SLA andLDMC)","negative(SLA,height_rep,LCC)")
outros.posi = c("negativo (trait richness)/positivo (trait divergence)", 
                "positive,ns-for-leaf-traits", "positive,ns-for-seed-mass")

data <- data %>%
  select(produtividade:obs_comentarios) %>%
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
  mutate(across(c(FD:CWM_WD), ~ifelse(. %in% neg, "negative", as.character(.)))) %>%
  mutate(across(c(FD:CWM_stomatal.dens), ~ifelse(. %in% posi,"positive", .))) %>%
  mutate_all(~na_if(., '')) %>%
  mutate_all(~na_if(., 'na')) %>%
  mutate(across(c(FD:CWM_stomatal.dens), ~str_replace_all(., ";", ","))) %>%
  mutate(across(c(FD), ~ifelse(. %in% outros.posi,"positive", .))) %>%
  mutate(across(c(FD), ~ifelse(. %in% outros.neg,"negative", .)))
  
  
sla = c("specificleafarea", "SLA")
lma = c("LEAF_MASS", "leaf_mass_area", "leaf_mass", "leafmassperunitarea", "leaffreshmass", "LMA")
lnc = c("leafNperarea", "Nitrogen_fraction(%)", "N", "leafδ15N")
n.c = c("razaoC:Nfolha","leafC:Nratio", "LC:N", "leaf_carbon_nitrogen_ratio", "razãocarbono:nitrogênioofplantlitter", "C:N", "razãoC:Nfolha")
wd = c("stemspecificdensity.", "stemdrymattercontent", "Wood_specific_gravity(WSG)", "wooddensity", "wd")
height = c("High_understory_layer_cover(h.cover)","plantheight", "Maximum_plant_height", "maximumheight", "plant_height_reproductive", 
           "maximum_height", "H", "height", "canopyheight", "CANOPY_HEIGHT",
           "max_height_growth_rate")
dec = c("deciduousness", "deciduousness(Dec)", "leaf_habit", "leafseasonality", "leaf_longevity(LL", "LIFE_SPAN")
crown = c("corwnarea", "Can.\ncover")
growth = c("relative-growth-rate", "diam_growth", "maximum_growth_rate")
seed = c("seedlenght", "dispersalunitlenght", "seed_germination(SG)", "SM(seed_mass)", "seedmass", "seed_mass", "numberofseedsofthereproductiveunit", "SM", "leafprolinecontent", "FSMH")
tree.size = c("maximum_stem_diameter(Dmax)", "DBH", "BA", "Density")
lcc = c("leafCperdrymass","LCaC", "Carbon_fraction(%)", "carbon_fractions", "LCC","Leaf_carbon_content", "leaf_carbon_content")
form = c("growthfrom", "growth_habit", "lateral_spread", "leafangle")
tolerance = c("shade_or_drought_tolerance", "drought.toler", "shade.toler", "fire_resistance", "fire_tolerance", "resprout_ability")
root = c("root_density", "mycorrhizal_association", "root_biomass", "ROOT_DEPTH")
la = c("leafsize", "leaf_size", "leaflenght")
lt = c("leafthickness", "Leaf_tissue_density", "Leaf_thickness")
vessel = c("woodvessellenght", "stemconduitdensity")
pigment = c("pigment_composition", "Area_based_pigment_content", "Pigment_concentration")
n.p = c("N/P_ratio", "NPreference")
lpc = c("P", "LPC", "LNP")
na = c("month)", "", "coniferas", "photosynthesis_rate")
# FD --------
fd <- tibble (traits.fd = data$traits_FD) %>%
  separate(traits.fd,into = paste0("fd", 1:30), sep = ",")%>%
  gather(key = "variable", value = "driver", starts_with("fd"), na.rm = TRUE)%>%
  mutate(across(c(driver), ~str_replace_all(., " ", ""))) %>%
  mutate(driver = replace(driver, driver  %in% sla , "SLA")) %>%
  mutate(driver = replace(driver, driver  %in% lma , "SLA"))%>%
  mutate(driver = replace(driver, driver  %in% lnc , "LNC")) %>%
  mutate(driver = replace(driver, driver  %in% n.c , "LC:LN")) %>%
  mutate(driver = replace(driver, driver  %in% wd , "WD")) %>%
  mutate(driver = replace(driver, driver  %in% height , "Height")) %>%
  mutate(driver = replace(driver, driver  %in% dec , "deciduousness")) %>%
  mutate(driver = replace(driver, driver  %in% growth , "gr.rate")) %>%
  mutate(driver = replace(driver, driver  %in% seed , "seeds")) %>%
  mutate(driver = replace(driver, driver  %in% crown , "crown")) %>%
  mutate(driver = replace(driver, driver  %in% tree.size , "tree.size")) %>%
  mutate(driver = replace(driver, driver  %in% lcc , "LCC")) %>%
  mutate(driver = replace(driver, driver  %in% form , "gr.form")) %>%
  mutate(driver = replace(driver, driver  %in% tolerance , "stress.tol")) %>%
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


fd2$driver <- reorder(fd2$driver, -fd2$frequencia)

#tiff("results/traits_FD.tiff", units="in", width=6.5, height=8.5, res=300)

ggplot(fd2, aes(x = fct_infreq(driver), y=frequencia), colour="steelblue")+
  geom_bar(stat= "identity", fill="steelblue")+
  scale_y_continuous(breaks = seq(0, 16, by = 2))+
  coord_flip()+
  labs(x = "", y = "Frequency (%)", title = "Traits used to calculate FD") +  theme_minimal()  +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=15))
#dev.off()
 
data
#tiff("results/FD_effect.tiff", units="in", width=6.5, height=5.5, res=300)
data %>%
  group_by(FD)%>%
  mutate(FD = as.factor(FD)) %>%
  count(FD) %>%
  drop_na(FD) %>%
  ggplot(aes(x=FD, y=n))+geom_bar(stat= "identity", fill="steelblue") +
  labs(x = "", y = "Number of papers", title = "Effect of FD on productivity") + 
  scale_x_discrete(labels=c("Negative", "No relationship", "Positive"))+
  theme_minimal()  +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=15))
#dev.off()