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


