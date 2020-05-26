library(ggplot2)

file_info<- list.files("cut/tests", pattern=".txt")


for (i in 1:length(file_info)){
  print(file_info[i])
  if(i == 1){
    file=read.table(paste0("cut/tests/",file_info[1]),header = TRUE)
    file$trial <- strtoi(strsplit(unlist(strsplit(file_info[1], "trial_"))[2], ".txt"))
    file$id <- i
    file$name<-file_info[1]
  }
  tempfile <- read.table(paste0("cut/tests/",file_info[i]), header = TRUE)
  tempfile$trial <- strtoi(strsplit(unlist(strsplit(file_info[i], "trial_"))[2], ".txt"))
  tempfile$id <- i
  tempfile$name<-file_info[i]
  file<-rbind(file,tempfile)
}

file$dif_pheno <- file$pheno_p - file$pheno_h
file$dif_pi <- file$div_p - file$div_h

data<-subset(file, ind_p >900 & ind_h >900 & trial ==1)

ggplot(subset(file, ind_p >900 & ind_h >900), aes(gen,pheno_h))+
  geom_point( color="blue")+
  geom_point(data = subset(file, ind_p >900 & ind_h >900), aes(y = pheno_p), colour = 'red', size = 1)+
  facet_wrap(subset(file, ind_p >900 & ind_h >900)$trial)

ggplot(subset(file, ind_p >900 & ind_h >900), aes(gen,div_h))+
  geom_point( color="lightblue")+
  geom_point(data = subset(file, ind_p >900 & ind_h >900), aes(y = div_p), colour = 'pink', size = 1)+
  facet_wrap(subset(file, ind_p >900 & ind_h >900)$trial)

ggplot(subset(file, ind_p >900 & ind_h >900), aes(gen,pheno_h))+
  geom_point( color="blue")+
  geom_point(data = subset(file, ind_p >900 & ind_h >900), aes(y = pheno_p), colour = 'red', size = 1)+
  facet_wrap(subset(file, ind_p >900 & ind_h >900)$trial)+
  geom_point(data = subset(file, ind_p >900 & ind_h >900), aes(y = div_h), color="lightblue", size = 1)+
  geom_point(data = subset(file, ind_p >900 & ind_h >900), aes(y = div_p), colour = 'pink', size = 1)+
  facet_wrap(subset(file, ind_p >900 & ind_h >900)$trial)



ggplot(subset(file, id == 78), aes(gen,pheno_h))+
  #geom_point( color="blue")+
  #geom_point(data = subset(file, id == 78), aes(y = pheno_p), colour = 'red', size = 1)+
  geom_point(data = subset(file, id == 78), aes(y = div_h), color="lightblue", size = 1)+
  geom_point(data = subset(file, id == 78), aes(y = div_p), colour = 'pink', size = 1)+
  geom_point(data = subset(file, id == 78), aes(y = dif_pheno), colour = 'green', size = 1)+
  xlim(1000, 3000)

ggplot(subset(file, id == 54), aes(gen,pheno_h))+
  #geom_point( color="blue")+
  #geom_point(data = subset(file, id == 78), aes(y = pheno_p), colour = 'red', size = 1)+
  geom_point(data = subset(file, id == 54), aes(y = div_h), color="lightblue", size = 1)+
  geom_point(data = subset(file, id == 54), aes(y = div_p), colour = 'pink', size = 1)+
  geom_point(data = subset(file, id == 54), aes(y = dif_pheno), colour = 'green', size = 1)+
  xlim(1000, 1100)


pdf(width=10,height=7,pointsize=12, paste0("figures/",subset(file, id == 87)$name, ".pdf")[1])

ggplot()+
  #geom_point( color="blue")+
  #geom_point(data = subset(file, id == 78), aes(y = pheno_p), colour = 'red', size = 1)+
  geom_point(data = subset(file, id == 87), aes(x=gen, y = div_h, color="H"), size = 1)+
  geom_point(data = subset(file, id == 87), aes(x=gen, y = div_p, colour = 'P'), size = 1)+
  geom_point(data = subset(file, id == 87), aes(x=gen, y = dif_pheno, colour = 'PP'), size = 1)+
  #geom_point(data = subset(file, id == 87), aes(x=gen, y = dif_pi, colour = 'PI'), size = 1)+
  #xlim(1000, 3000)+
  scale_colour_manual(name="Lines",
                      breaks=c("H", "P", "PP"), #, "PI"
                      #c=c("chocolate3","cyan4", "green"),
                      values=c("H"="lightblue","P"="pink", "PP"="lightgreen"), #, "PI"="orchid"
                      labels=c("Host", "Pathogen", "Dif btw Phenotype"))+ #, "Dif btw PI"
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
  xlab("Generation") + ylab("Pi (Diversity)")+
  labs(title = subset(file, id == 87)$name, subtitle = "My subtitle")
dev.off()

pdf(width=10,height=7,pointsize=12, paste0("figures/",subset(file, id == 54)$name, ".pdf")[1])

ggplot()+
  #geom_point( color="blue")+
  #geom_point(data = subset(file, id == 78), aes(y = pheno_p), colour = 'red', size = 1)+
  geom_point(data = subset(file, id == 54), aes(x=gen, y = div_h, color="H"), size = 1)+
  geom_point(data = subset(file, id == 54), aes(x=gen, y = div_p, colour = 'P'), size = 1)+
  geom_point(data = subset(file, id == 54), aes(x=gen, y = dif_pheno, colour = 'PP'), size = 1)+
  #geom_point(data = subset(file, id == 87), aes(x=gen, y = dif_pi, colour = 'PI'), size = 1)+
  #xlim(1000, 3000)+
  scale_colour_manual(name="Lines",
                      breaks=c("H", "P", "PP"), #, "PI"
                      #c=c("chocolate3","cyan4", "green"),
                      values=c("H"="lightblue","P"="pink", "PP"="lightgreen"), #, "PI"="orchid"
                      labels=c("Host", "Pathogen", "Dif btw Phenotype"))+ #, "Dif btw PI"
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
  xlab("Generation") + ylab("Pi (Diversity)")+
  labs(title = subset(file, id == 54)$name, subtitle = "My subtitle")
dev.off()


pdf(width=10,height=7,pointsize=12, paste0("figures/",subset(file, id == 78)$name, ".pdf")[1])

ggplot()+
  geom_point(data = subset(file, id == 78), aes(x=gen, y = div_h, color="H"), size = 1)+
  geom_point(data = subset(file, id == 78), aes(x=gen, y = div_p, colour = 'P'), size = 1)+
  geom_point(data = subset(file, id == 78), aes(x=gen, y = dif_pheno, colour = 'PP'), size = 1)+
  #geom_point(data = subset(file, id == 87), aes(x=gen, y = dif_pi, colour = 'PI'), size = 1)+
  #xlim(1000, 3000)+
  scale_colour_manual(name="Lines",
                      breaks=c("H", "P", "PP"), #, "PI"
                      #c=c("chocolate3","cyan4", "green"),
                      values=c("H"="lightblue","P"="pink", "PP"="lightgreen"), #, "PI"="orchid"
                      labels=c("Host", "Pathogen", "Dif btw Phenotype"))+ #, "Dif btw PI"
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
  xlab("Generation") + ylab("Pi (Diversity)")+
  labs(title = subset(file, id == 78)$name, subtitle = "My subtitle")
dev.off()


for(i in 1:length(unique(file$id))){
  #dev.off()
  pdf(width=10,height=7,pointsize=12, paste0("figures/",subset(file, id == i)$name, ".pdf")[1])
  
print(ggplot()+
    geom_point(data = subset(file, id == i), aes(x=gen, y = div_h, color="H"), size = 1)+
    geom_point(data = subset(file, id == i), aes(x=gen, y = div_p, colour = 'P'), size = 1)+
    geom_point(data = subset(file, id == i), aes(x=gen, y = dif_pheno, colour = 'PP'), size = 1)+
    #geom_point(data = subset(file, id == 87), aes(x=gen, y = dif_pi, colour = 'PI'), size = 1)+
    #xlim(1000, 3000)+
    scale_colour_manual(name="Lines",
                        breaks=c("H", "P", "PP"), #, "PI"
                        #c=c("chocolate3","cyan4", "green"),
                        values=c("H"="lightblue","P"="pink", "PP"="lightgreen"), #, "PI"="orchid"
                        labels=c("Host", "Pathogen", "Dif btw Phenotype"))+ #, "Dif btw PI"
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
    xlab("Generation") + ylab("Pi (Diversity)")+
    labs(title = subset(file, id == i)$name, subtitle = "My subtitle"))
  dev.off()
  
  }

for(i in 1:length(unique(file$id))){
  #dev.off()
  pdf(width=10,height=7,pointsize=12, paste0("figures/zoom_",subset(file, id == i)$name, ".pdf")[1])
  
  print(ggplot()+
          geom_point(data = subset(file, id == i), aes(x=gen, y = div_h, color="H"), size = 1)+
          geom_point(data = subset(file, id == i), aes(x=gen, y = div_p, colour = 'P'), size = 1)+
          geom_point(data = subset(file, id == i), aes(x=gen, y = dif_pheno, colour = 'PP'), size = 1)+
          #geom_point(data = subset(file, id == 87), aes(x=gen, y = dif_pi, colour = 'PI'), size = 1)+
          xlim(1000, 2000)+
          scale_colour_manual(name="Lines",
                              breaks=c("H", "P", "PP"), #, "PI"
                              #c=c("chocolate3","cyan4", "green"),
                              values=c("H"="lightblue","P"="pink", "PP"="lightgreen"), #, "PI"="orchid"
                              labels=c("Host", "Pathogen", "Dif btw Phenotype"))+ #, "Dif btw PI"
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
          xlab("Generation") + ylab("Pi (Diversity)")+
          labs(title = subset(file, id == i)$name, subtitle = "My subtitle"))
  dev.off()
  
}


for(i in 1:length(unique(file$id))){
  #dev.off()
  pdf(width=10,height=7,pointsize=12, paste0("figures/pheno_",subset(file, id == i)$name, ".pdf")[1])
  
  print(ggplot()+
          geom_point(data = subset(file, id == i), aes(x=gen, y = pheno_h, color="H"), size = 1)+
          geom_point(data = subset(file, id == i), aes(x=gen, y = pheno_p, colour = 'P'), size = 1)+
          geom_point(data = subset(file, id == i), aes(x=gen, y = dif_pi, colour = 'PP'), size = 1)+
          #geom_point(data = subset(file, id == 87), aes(x=gen, y = dif_pi, colour = 'PI'), size = 1)+
          #xlim(1000, 3000)+
          scale_colour_manual(name="Lines",
                              breaks=c("H", "P", "PP"), #, "PI"
                              #c=c("chocolate3","cyan4", "green"),
                              values=c("H"="navy","P"="firebrick", "PP"="seagreen"), #, "PI"="orchid"
                              labels=c("Host", "Pathogen", "Dif in Pi"))+ #, "Dif btw PI"
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
          xlab("Generation") + ylab("Phenotype")+
          labs(title = subset(file, id == i)$name, subtitle = "My subtitle"))
  dev.off()
  
}

for(i in 1:length(unique(file$id))){
  #dev.off()
  pdf(width=10,height=7,pointsize=12, paste0("figures/pheno_zoom_",subset(file, id == i)$name, ".pdf")[1])
  
  print(ggplot()+
          geom_point(data = subset(file, id == i), aes(x=gen, y = pheno_h, color="H"), size = 1)+
          geom_point(data = subset(file, id == i), aes(x=gen, y = pheno_p, colour = 'P'), size = 1)+
          geom_point(data = subset(file, id == i), aes(x=gen, y = dif_pi, colour = 'PP'), size = 1)+
          #geom_point(data = subset(file, id == 87), aes(x=gen, y = dif_pi, colour = 'PI'), size = 1)+
          xlim(1000, 2000)+
          scale_colour_manual(name="Lines",
                              breaks=c("H", "P", "PP"), #, "PI"
                              #c=c("chocolate3","cyan4", "green"),
                              values=c("H"="navy","P"="firebrick", "PP"="seagreen"), #, "PI"="orchid"
                              labels=c("Host", "Pathogen", "Dif in Pi"))+ #, "Dif btw PI"
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
          xlab("Generation") + ylab("Phenotype")+
          labs(title = subset(file, id == i)$name, subtitle = "My subtitle"))
  dev.off()
  
}
