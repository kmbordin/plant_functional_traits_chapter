pacman::p_load(tidyverse,gt,gtExtras,janitor,doBy,here)
# load data and harmonise table -----
#data <- read.csv(here::here("processed_data", "analises-08.08.23.csv"))
#data_raw <- read.csv(here::here("processed_data", "total.csv"))

# dataset raw -------
data_raw <- read.csv(here::here("processed_data", "analises-12.09.23.csv"))

levels(as.factor(data_raw$tipo_ecossistema.eg.campo.floresta.savana.etc.))

ecosys = c("forest", "grassland","steppe","medow-steppes", "dune grassland","forest-steppe ecotones","agricultural/semi-natural grasslands","forest;shrublands", "forest;woodlands","forest;tundra", "forest-steppe ecotones","forest, shrubland, grassland, savanna,","forests, shrubland and sparse grasslands")
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
         prod.metric = produtividade_medida.produtividade.1.estoque.2.ouNA,
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
  filter(str_detect(ecosystem, paste(ecosys, collapse="|")))%>% #filter only ecosystems described in ecosys
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

# trait correction -----
sla = c("specificleafarea", "SLA","specificleafarea(SLA)")

lm = c("LEAF_MASS", "leaffreshmass","leaf_mass","LM(leafdrymass)")

lma = c( "leaf_mass_area",  "leafmassperunitarea", "LMA")

lnc = c("leafNperarea", "Nitrogen_fraction(%)", "N", "leafδ15N")

n.c = c("razaoC:Nfolha","leafC:Nratio", "LC:N", "leaf_carbon_nitrogen_ratio", "razãocarbono:nitrogênioofplantlitter", "C:N", "razãoC:Nfolha","leafCN","plantCandNconcentration(CCandNC)", "LCC_LNC")

wd = c("stemspecificdensity.", "stemdrymattercontent", "Wood_specific_gravity(WSG)", "wooddensity", "wd")

height = c("High_understory_layer_cover(h.cover)","plantheight", "Maximum_plant_height", "maximumheight", "plant_height_reproductive", "maximum_height", "H", "height", "canopyheight", "CANOPY_HEIGHT","max_height_growth_rate","plantheight(H)", "canopy_height")

dec = c("deciduousness", "deciduousness(Dec)", "leaf_habit", "leafseasonality", "leaf_longevity(LL", "LIFE_SPAN")

crown = c("corwnarea", "Can.\ncover", "crown.complement")

growth = c("relative-growth-rate", "diam_growth", "maximum_growth_rate", "RGR")

seed = c("seedlenght", "dispersalunitlenght", "seed_germination(SG)", "SM(seed_mass)", "seedmass", "seed_mass", "numberofseedsofthereproductiveunit", "SM", "leafprolinecontent", "FSMH", "seed mass", "seed.mass")

tree.size = c("maximum_stem_diameter(Dmax)", "DBH", "BA", "Density", "Dmax")

lcc = c("leafCperdrymass", "Carbon_fraction(%)", "carbon_fractions", "LCC","Leaf_carbon_content", "leaf_carbon_content","leafcarbon13", "carbon13")

form = c("growthfrom", "growth_habit", "lateral_spread", "leafangle")

tolerance = c("shade_or_drought_tolerance", "drought.toler", "shade.toler", "fire_resistance", "fire_tolerance", "resprout_ability", "waterlogging_tolerance")

root = c("root_density", "mycorrhizal_association", "root_biomass", "ROOT_DEPTH","specificrootlenght","rootdrymattercontent","RTF","AD","RTD","RA", "avg root_diameter", "RLD", "root area", "root mass", "specific root_length", "SRL", "specific_root_length", "root_area", "avg_root_diameter", "root.mass")

la = c("leafsize", "leaf_size", "leaflenght")

lt = c("leafthickness", "Leaf_tissue_density", "Leaf_thickness", "leaf_tickness", "leaf.tissue.density", "leaf.tickness")

vessel = c("woodvessellenght", "stemconduitdensity")

pigment = c("pigment_composition", "Area_based_pigment_content", "Pigment_concentration", "chlorophyllab", "Chlorophyll")

n.p = c("N/P_ratio", "NPreference", "LPC_LNC")

lpc = c("P", "LPC", "LNP")

ldmc = c("LDMC",lm)

na = c("month)", "", "coniferas", "photosynthesis_rate","remotesensingvariables","andplantcellulosecontent(CEC)")
