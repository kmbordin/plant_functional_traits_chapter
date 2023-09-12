
#old figures
#cria a figura mostrando os traits usados na fd para campo ou floresta
# png("results/traits_FD_ecosystem.png", units="in", width=6, height=10, res=300)
ggplot(fd2, aes(x = reorder(driver, -frequencia), y= frequencia , fill= Ecosystem))+
  geom_bar(stat= "identity", width = 0.4)+
  geom_text(aes(label=n), vjust=0.3, hjust=-0.5, position=position_stack(vjust=0), colour="black", size=3) +
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
#p1 = total
#p2 = produtividade apenas
#p3 = estoque apenas
# library(patchwork)
# plots = (p1|(p2/p3)) +plot_annotation(tag_levels = c("A"))+ plot_layout(widths = c(1, 1))
# png('results/FDonprod.png', units="in", width=15, height=10, res=300)
# plots
# dev.off()