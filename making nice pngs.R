library(ggplot2)
library(gridExtra)
library(gtable)


id

pdf(width=10,height=7,pointsize=12, paste0("figures/powerpoint/",subset(file, id == i)$name, ".pdf")[1])

p2<- ggplot()+
  geom_point(data = subset(file, id == i), aes(x=gen, y = div_h, color="H"), size = 1)+
  geom_point(data = subset(file, id == i), aes(x=gen, y = div_p, colour = 'P'), size = 1)+
  #xlim(1000, 3000)+
  scale_colour_manual(name="Lines",
                      breaks=c("H", "P"), #, "PI", "PP"
                      #c=c("chocolate3","cyan4", "green"),
                      values=c("H"="mediumpurple","P"="pink"), #, "PI"="orchid", "PP"="lightgreen"
                      labels=c("Host", "Pathogen"))+ #, "Dif btw PI", "Dif btw Phenotype"
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme_bw() + # setting up the theme
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size=18,colour="Black"),
        text = element_text(size=14),
        panel.grid.minor = element_blank(),
        legend.box.background = element_rect(),
        plot.title = element_text(size=12),
        plot.subtitle = element_text(size=22),
        #panel.grid = element_blank(),
        panel.spacing.x = unit(0.5,"line"))+
  xlab("Generation") + ylab("Pi (Diversity)")

p1<- ggplot()+
  geom_point(data = subset(file, id == i), aes(x=gen, y = pheno_h, color="H"), size = 1)+
  geom_point(data = subset(file, id == i), aes(x=gen, y = pheno_p, colour = 'P'), size = 1)+
  #xlim(1000, 3000)+
  scale_colour_manual(name="Lines",
                      breaks=c("H", "P"), #, "PI",, "PP"
                      #c=c("chocolate3","cyan4", "green"),
                      values=c("H"= "darkmagenta" ,"P"="firebrick"), #, "PI"="orchid" , "PP"="seagreen")
                      labels=c("Host", "Pathogen"))+ #, "Dif btw PI", , "Dif in Pi"
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme_bw() + # setting up the theme
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size=18,colour="Black"),
        text = element_text(size=14),
        panel.grid.minor = element_blank(),
        legend.box.background = element_rect(),
        plot.title = element_text(size=12),
        plot.subtitle = element_text(size=22),
        #panel.grid = element_blank(),
        panel.spacing.x = unit(0.5,"line"))+
  xlab("Generation") + ylab("Phenotype")+
  labs(title = subset(file, id == i)$name, subtitle = "My Plots")

p3<-ggplot()+
  geom_point(data = subset(file, id == i), aes(x=gen, y = dif_pheno, colour = 'PP'), size = 1)+
  geom_point(data = subset(file, id == i), aes(x=gen, y = dif_pi, colour = 'PI'), size = 1)+
  #xlim(1000, 3000)+
  scale_colour_manual(name="Lines",
                      breaks=c("PP", "PI"), #, "PI",, "PP"
                      #c=c("chocolate3","cyan4", "green"),
                      values=c("PP"="seagreen", "PI"="lightgreen"),
                      labels=c("Dif in Pheno", "Dif btw PI"))+ 
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme_bw() + # setting up the theme
  theme(axis.text.x = element_text(size=18,colour="Black"),
        axis.text.y = element_text(size=18,colour="Black"),
        text = element_text(size=14),
        panel.grid.minor = element_blank(),
        legend.box.background = element_rect(),
        plot.title = element_text(size=12),
        plot.subtitle = element_text(size=22),
        #panel.grid = element_blank(),
        panel.spacing.x = unit(0.5,"line"))+
  xlab("Generation") + ylab("Difference")

g1 <- ggplotGrob(p1)  
g2 <- ggplotGrob(p2)  
g3 <- ggplotGrob(p3)  

g<- rbind(g1,g2,g3, size="first")  
print(grid.arrange(g))
dev.off()