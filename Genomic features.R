# Visualise genomic features
#                                                Shingo Miyauchi 25Jan20
#------------------
# Descriptions 
#------------------
# This script generates visual stats plots. 

# <Input files>
# (1) Combined genomic features 
# ###_GF_Step2_GenomicInfoCombined.csv

# <Methods>
# PART 1: Preparation for plots
# PART 2: Make a combined figure

# IMPORTANT: Adjust legends + tree with InDesign after generating combined figures!
#------------------
# IMPORTANT:Global option to switch off factorisation. 
options(stringsAsFactors=FALSE)

library(dplyr)
library(ggplot2)
library(egg)
library(RColorBrewer)

# Insert Porject Name
ProjectName <- "135_Fungi_GF"

# Load Taxonomic info + Genome stats table 
genome.df <- read.csv(list.files(pattern="*_GF_Step2_GenomicInfoCombined.csv"))

# Get shorten ID for JGI_ID for grepping
genome.df$ID <- genome.df$JGI_ID 

#=====================
# PART 1: Preparation 
#=====================
# Factorise Species Name 
genome.df$SpeciesName <- factor(genome.df$FungusName,
                                levels = genome.df$FungusName)

#--------------------------------
#IMPORTANT: Manually set colours
#--------------------------------
genome.df$Ecology %>% unique()
#"Ectomycorrhizae"        "Saprotroph"             "Wood decayer"          
#"Pathogen"               "Endophyte"              "Orchid mycorrhizae"    
#"Ericoid mycorrhizae"    "Parasite"               "Yeast"                 
#"Arbuscular mycorrhizae"

# Set a set of colours (identical to all figures used in manuscript)
iro <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFD92F", 
         "cyan2", "#F781BF", "#999999", "#A65628")

# Check the colours with template above
names(iro) <- c("Ectomycorrhizae", "Saprotroph",
                "Pathogen", "Parasite", 
                "Ericoid mycorrhizae", 
                "Orchid mycorrhizae", 
                "Arbuscular mycorrhizae",
                "Wood decayer", "Endophyte","Yeast")
# Check 
iro

# Check the colours
plot(rep(1,length(iro)),col=iro,pch=19,cex=3)

# Factorise Ecology
genome.df$Ecology <- factor(genome.df$Ecology, 
                         levels= c("Ectomycorrhizae", "Saprotroph",
                                   "Pathogen", "Parasite", 
                                   "Ericoid mycorrhizae", 
                                   "Orchid mycorrhizae", 
                                   "Arbuscular mycorrhizae",
                                   "Endophyte",
                                   "Wood decayer", 
                                   "Yeast" )) 

#======================
# PART 2: Visualisation 
#======================
#-------------------
# Bar plot function
#-------------------
bar.plot.fun <- function(XXX, YYY, ZZZ, IRO, 
                         Y_Title, Limit_value, Leg_Switch) {
  ggplot(XXX, aes(x= SpeciesName, y= YYY, fill= Ecology)) +
    geom_bar(stat="identity", alpha = 0.8) +
    scale_fill_manual(values= IRO) +
    
    # IMPORTANT: Reverse fungal names
    scale_x_discrete(limits= rev(levels(XXX$SpeciesName)),
                     expand = c(0, 1.2)) +
    
    # Set Y axis 0 to 1
    scale_y_continuous(position= "right") +
    
    # Median dot line 
    geom_hline(yintercept=median(YYY), linetype="dotted", color = "grey40") +
  
    # X axis
    ylab(ZZZ) +
    
    # X and Y flip
    coord_flip(ylim = Limit_value) +
    
    ### Legend ###
    theme_minimal() +
    
    # Legend in specified row
    guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
    
    theme(legend.position= Leg_Switch,
          # Remove legend title
          legend.title=element_blank(),
          # Label on X axis 
          axis.title.x= element_text(colour= "grey30", size=9),
          axis.text.x = element_text(colour= "grey30", size=9), 
          
          #-------------------------
          # Fungal IDs on/off - Y axis swich on and off
          #-------------------------
          axis.text.y = Y_Title,
          axis.title.y=element_blank()
          # axis.title.y=element_text(colour= "gray50", size=11),
          #panel.border=element_blank()
    )}

# Point plot function
point.plot.fun <- function(XXX, YYY, ZZZ, IRO, 
                           Y_Title, Limit_value, Leg_Switch) {
  ggplot(XXX, aes(x= SpeciesName, y= YYY, color= Ecology, label= YYY)) +
    # Circle parts
    geom_point(stat="identity", alpha = 0.75, size=2.5) +
    # # Bar parts
    # geom_segment(aes(y = 0, 
    #                  x = SpeciesName, 
    #                  yend = YYY, 
    #                  xend = SpeciesName, 
    #                  color = Ecology)) +
    # Assigned Colours for lifestyle
    scale_color_manual(values= IRO) +
    # Text in circle
    #geom_text(color="white", size=2) +
    
    # IMPORTANT: Reverse fungal names
    scale_x_discrete(limits= rev(levels(XXX$SpeciesName)), 
                     expand = c(0, 1.2)) +
    # Set Y axis 0 to 1
    scale_y_continuous(position= "right", limits = Limit_value) +
    
    # Median dot line 
    geom_hline(yintercept=median(YYY), linetype="dotted", color = "grey40") +
    
    # X axis
    ylab(ZZZ) +
    
    # X and Y flip
    coord_flip() +
    
    ### Legend ###
    theme_minimal() +
    theme(legend.position= Leg_Switch,
          # Remove legend title
          legend.title=element_blank(),
          # Label on X axis 
          axis.title.x= element_text(colour= "grey30", size=9),
          axis.text.x = element_text(colour= "grey30", size=9), 
          
          #-------------------------
          # Fungal IDs on/off - Y axis swich on and off
          #-------------------------
          axis.text.y = Y_Title,
          axis.title.y=element_blank()
    )}

#-------------------
# Generate plots
#-------------------
# Genome size
genome.p <- bar.plot.fun(XXX= genome.df, 
                                  YYY= trunc(genome.df$Genome.size/1000000), 
                                  ZZZ= "Genome (Mb)", 
                                  IRO = iro,
                                  Y_Title = element_text(colour= "gray30", 
                                            size=7, vjust=0.5, hjust=0.5),
                                  Limit_value = NULL,
                                  Leg_Switch= "none")
# TE coverage
TE.p <- bar.plot.fun(XXX= genome.df, 
                              YYY= signif(genome.df$TE.CoverageTotal, 2), 
                              ZZZ= "TE content (%)", 
                              IRO= iro, 
                              Y_Title= element_blank(), 
                              Limit_value= NULL,
                              Leg_Switch= "none")
# Number of genes 
Gene.p <- bar.plot.fun(XXX= genome.df, 
                                YYY= signif(genome.df$Num.Gene/1000, 2), 
                                ZZZ= "Genes (K)", 
                                IRO= iro, 
                                Y_Title= element_blank(), 
                                Limit_value= NULL,
                                Leg_Switch= "none")
# Secreted proteins 
SecPro.p <- bar.plot.fun(XXX= genome.df, 
                                  YYY= signif(genome.df$Secret.protein/1000, 
                                              2),
                                  ZZZ= "Secreted (K)",
                                  IRO= iro,
                                  Y_Title= element_blank(),
                                  Limit_value= NULL,
                                  Leg_Switch= "bottom")

# N50 length (L50) (Mb)
L50.p <- point.plot.fun(XXX= genome.df, 
                             YYY= genome.df$Scaffold.L50.Mbp,
                             ZZZ= "L50 (Mb)",
                             IRO= iro,
                             Y_Title= element_blank(),
                             Limit_value= NULL,
                             Leg_Switch= "none")

# Number of scaffold 
scaf.p <- point.plot.fun(XXX= genome.df, 
                             YYY= genome.df$Num.Scaffold/1000, 
                             ZZZ= "Scaffolds (K)",
                             IRO= iro,
                             Y_Title= element_blank(),
                             Limit_value= c(0, 30),
                             Leg_Switch= "none")

#-----------------------------
# BUSCO completeness of genome (%)
#------------------------------
# complete BUSCO values divide by total BUSCO genes, then make it percentage  
busco.a.val <- (genome.df$CompleteBUSCO/genome.df$Total.BUSCO.group)*100

busco.p <- point.plot.fun(XXX= genome.df,
                              YYY= busco.a.val,
                              ZZZ= "BUSCO (%)",
                              IRO= iro,
                              Y_Title= element_blank(),
                              Limit_value= c(75, 100),
                              Leg_Switch= "none")

#-----------------
# ggarrange figures - Tree + heatmap + total number + ratio  
#----------------
# Print and save
dir <- getwd()
mypath <- paste0(dir,"/" , ProjectName, "_Step3_", "GenomicFeatures", ".pdf")
pdf (file=mypath, height = 13, width = 9, onefile=FALSE)

ggarrange(genome.p, TE.p, Gene.p,
          SecPro.p, scaf.p, L50.p, busco.p, ncol= 7)

dev.off()