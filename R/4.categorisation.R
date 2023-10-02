# table of categories ----
cat = readxl::read_excel(here::here("results", "categorisation.xlsx"))

cat %>% 
  gt() %>% 
  tab_header(title = md("**Functional traits grouped into categories**"),
             subtitle = "Traits used to calculate functional diversity and dominance") %>% 
  cols_align(align = "center",columns = everything()) %>% 
  
  tab_style(style = cell_fill(color = "gray90"),
            locations = cells_column_labels(columns = everything())) %>% 
  #gtsave(filename = "results/categorisation.html")
  gtsave(filename = "results/categorisation.rtf")

#final table
summarise = read.csv2(here::here("results", "summary_all_togt.csv"))

summarise %>% 
  gt() %>% 
  tab_header(title = md("**Functional traits evaluated**"),
             subtitle = "Traits used to calculate functional diversity and dominance, based on different ecosystems, regions, and metrics") %>% 
  cols_align(align = "center",columns = everything()) %>% 
  tab_style(style = cell_fill(color = "gray90"),
            locations = cells_column_labels(columns = everything())) %>% 
  cols_merge(columns = c(Niche.complementarity, Niche.complementarity.1)) %>%
  cols_merge(columns = c(Niche.complementarity.2, Niche.complementarity.3)) %>%
  cols_merge(columns = c(Niche.complementarity.4, Niche.complementarity.5)) %>% 
  cols_merge(columns = c(Mass.ratio, Mass.ratio.1)) %>%
  cols_merge(columns = c(Mass.ratio.2, Mass.ratio.3)) %>%
  cols_merge(columns = c(Mass.ratio.4, Mass.ratio.5)) %>% 
  #gtsave(filename = "results/categorisation.html")
  gtsave(filename = "results/summarisation.rtf")


