library(ggplot2)
library(gridExtra)
library(gtable)
library(plotrix)
library(viridis) 

file_info<- list.files("data/two_d_size/tests", pattern="exp") #new files that i turned off the remove lines
place<-"data/two_d_size/tests/"
dichotomy_2d<-data.frame(id= integer(length(file_info)), name = character(length(file_info)), v=integer(length(file_info)), h=integer(length(file_info)), sK=integer(length(file_info)), sC=integer(length(file_info)), d_mean = integer(length(file_info)), sd = integer(length(file_info)), se=integer(length(file_info)),stringsAsFactors=FALSE)

for (i in 1:length(file_info)){
  print(file_info[i])
  if(i == 1){
    #file=read.table(paste0("cut/tests/",file_info[1]),header = TRUE)
    file_2d=read.table(paste0(place,file_info[1]),header = TRUE)
    file_2d$trial <- strtoi(strsplit(unlist(strsplit(file_info[1], "trial_"))[2], ".txt"))
    file_2d$id<- i
    file_2d$name<-file_info[1]
    #file_2d$dif_pheno <- file_2d$pheno_p - file_2d$pheno_h
    
    dichotomy_2d$id[1] <- i
    dichotomy_2d$name[1]<-file_info[1]
    dichotomy_2d$v[1]<-as.numeric(unlist(strsplit(unlist(strsplit(file_info[1], "trial_"))[1], "_"))[3])
    dichotomy_2d$h[1]<- as.numeric(unlist(strsplit(unlist(strsplit(file_info[1], "trial_"))[1], "_"))[5])
    dichotomy_2d$sK[1]<-as.numeric(unlist(strsplit(unlist(strsplit(file_info[1], "trial_"))[1], "_"))[8])
    dichotomy_2d$sC[1]<-as.numeric(unlist(strsplit(unlist(strsplit(file_info[1], "trial_"))[1], "_"))[11])
    
    file_2d$dich = NA
    last_0 = 1
    
    for(j in 1:nrow(file_2d)){
      if (file_2d$eud[j] > -5 && file_2d$eud[j] < 5){
        file_2d$dich[j] = file_2d$gen[j] - last_0
        last_0 = file_2d$gen[j]
      }
    }
    dichotomy_2d$d_mean[1]=mean(file_2d$dich,na.rm=TRUE)
    dichotomy_2d$sd[1]<-sd(file_2d$dich,na.rm=TRUE)
    dichotomy_2d$se[1]<-std.error(file_2d$dich, na.rm=TRUE)
  }
  #tempfile <- read.table(paste0("cut/tests/",file_info[i]), header = TRUE)
  tempfile <- read.table(paste0(place,file_info[i]), header = TRUE)
  tempfile$trial <- strtoi(strsplit(unlist(strsplit(file_info[i], "trial_"))[2], ".txt"))
  tempfile$id <- i
  tempfile$name<-file_info[i]
  
  dichotomy_2d$id[i] <- i
  dichotomy_2d$name[i]<-file_info[i]
  dichotomy_2d$v[i]<-as.numeric(unlist(strsplit(unlist(strsplit(file_info[i], "trial_"))[1], "_"))[3])
  dichotomy_2d$h[i]<- as.numeric(unlist(strsplit(unlist(strsplit(file_info[i], "trial_"))[1], "_"))[5])
  dichotomy_2d$sK[i]<-as.numeric(unlist(strsplit(unlist(strsplit(file_info[i], "trial_"))[1], "_"))[8])
  dichotomy_2d$sC[i]<-as.numeric(unlist(strsplit(unlist(strsplit(file_info[i], "trial_"))[1], "_"))[11])
  
  #tempfile$dif_pheno <- tempfile$pheno_p - tempfile$pheno_h
  tempfile$dich = NA
  last_0 = 1
  for(j in 1:nrow(tempfile)){
    if (tempfile$eud[j] > -5 && tempfile$eud[j] < 5){
      tempfile$dich[j] = tempfile$gen[j] - last_0
      last_0 = tempfile$gen[j]
    }
  }
  dichotomy_2d$d_mean[i]=mean(tempfile$dich, na.rm=TRUE)
  dichotomy_2d$sd[i]<-sd(tempfile$dich, na.rm=TRUE)
  dichotomy_2d$se[i]<-std.error(tempfile$dich, na.rm=TRUE)
  file_2d<-rbind(file_2d,tempfile)
}

file_2d$dif_pheno <- file_2d$pheno_p - file_2d$pheno_h
file_2d$dif_pi <- file_2d$div_p - file_2d$div_h


dichotomy_2d<-dichotomy_2d[order(dichotomy$h),] 
dichotomy_2d$idh<-c(1:nrow(dichotomy_2d))
ggplot()+
  geom_point(data = dichotomy_2d, aes(x=id, y = d_mean, color = as.factor(paste0(dichotomy_2d$v,"x", dichotomy_2d$h))), size = 2)+
  geom_errorbar(data = dichotomy_2d, aes(x=idh, ymin=d_mean-sd, ymax=d_mean+sd,color = as.factor(v)), width=.5,
                position=position_dodge(.9))+
  #scale_x_log10()+
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
  xlab("Host Population Size") + ylab("Dichotomy")



pdf(width=10,height=7,pointsize=12, paste0("figures/di2_by_host",subset(file, id == i)$name, ".pdf")[1])

ggplot(data = dichotomy_2d, aes(x=as.factor(h), y = d_mean, fill = as.factor(v) ))+
  geom_boxplot()+
  scale_fill_brewer(name="Virus Population Size", palette="Dark2")+
  theme_bw() + # setting up the theme
  theme(axis.text.x = element_text(size=18,colour="Black"),
        axis.text.y = element_text(size=18,colour="Black"),
        text = element_text(size=18),
        panel.grid.minor = element_blank(),
        legend.box.background = element_rect(),
        legend.title = element_text(size=16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=22),
        plot.subtitle = element_text(size=22),
        #panel.grid = element_blank(),
        panel.spacing.x = unit(0.5,"line"))+
  xlab("Host Population Size") + ylab("Dichotomy")+
  labs(title = "Dichotomy Plot 2D")

dev.off()

pdf(width=10,height=7,pointsize=12, paste0("figures/di2_by_virus",subset(file, id == i)$name, ".pdf")[1])

ggplot(data = dichotomy_2d, aes(x=as.factor(v), y = d_mean, fill = as.factor(h) ))+
  geom_boxplot()+
  scale_fill_viridis_d(name="Host Population Size",option = "C")+
  theme_bw() + # setting up the theme
  theme(axis.text.x = element_text(size=18,colour="Black"),
        axis.text.y = element_text(size=18,colour="Black"),
        text = element_text(size=18),
        panel.grid.minor = element_blank(),
        legend.box.background = element_rect(),
        legend.title = element_text(size=16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=22),
        plot.subtitle = element_text(size=22),
        #panel.grid = element_blank(),
        panel.spacing.x = unit(0.5,"line"))+
  xlab("Virus Population Size") + ylab("Dichotomy")+
  labs(title = "Dichotomy Plot 2D")
dev.off()

scale_x_log10()
#"magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E")

geom_errorbar(data = dichotomy_2d, aes(x=idh, ymin=d_mean-sd, ymax=d_mean+sd,color = as.factor(v)), width=.5,
              position=position_dodge(.9))+
  
  
  
  # geom_point(data = subset(file, id == i), aes(x=gen, y = div_p, colour = 'P'), size = 1)+
  #   #xlim(1000, 3000)+
  #   scale_colour_manual(name="Lines",
  #                       breaks=c("H", "P"), #, "PI", "PP"
  #                       #c=c("chocolate3","cyan4", "green"),
  #                       values=c("H"="lightblue","P"="pink"), #, "PI"="orchid", "PP"="lightgreen"
  #                       labels=c("Host", "Pathogen"))+ #, "Dif btw PI", "Dif btw Phenotype"
  #   guides(colour = guide_legend(override.aes = list(size=4)))+
