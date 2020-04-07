# Visualisation of genome size + TE coverage based on lifestyles
#                                      Shingo Miyauchi 25Jan20
#-------------------------------
#       Descriptions
#-------------------------------
# This script visualises ecololgical style-wise box plot. 

# <Input files>
# 1) Taxonomic info, genome and Transposable elements from TINGO pipeline (Morin et al. 2019) 
# #####_Step5_Taxo_Coverage_Stats.csv

# <Method>
# 1) Set up parameters for visualisation
# 2) Visualisation of box plots.
# 3) Put plots together and generate PDF.
#-------------------------------
# IMPORTANT:Global option to switch off factorisation. 
options(stringsAsFactors=FALSE)

#===============
#  Preparation
#===============
library(ggplot2)
library(dplyr)
library(egg)

# Insert Project name
ProjectName <- "135_Fungi_TINGO"

# Load Taxonomic info + TE Stats table
count.df <- read.csv (list.files(pattern="*_Step5_Taxo_Coverage_Stats.csv"))

#-------------------------------
# Make tables for genome size
#-------------------------------
# Get genome size 
genome.df <- count.df[,c("JGI_FungalID", "FungusName", "Order", "Ecology", "Genome.size", "CladeOrder")]

# Remove duplicated rows
genome.df <- genome.df[!duplicated(genome.df),]

#===========================
# Assign ecology types with colours
#===========================
# Set a set of colours (identical to all figures used in the manuscript)
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

# Factorise Ecology and levels
genome.df$Ecology <- factor(genome.df$Ecology, 
                      levels= c("Ectomycorrhizae", "Saprotroph",
                                "Pathogen", "Parasite", 
                                "Ericoid mycorrhizae", 
                                "Orchid mycorrhizae", 
                                "Arbuscular mycorrhizae",
                                "Endophyte",
                                "Wood decayer", 
                                "Yeast" ))

count.df$Ecology <- factor(count.df$Ecology, 
                            levels= c("Ectomycorrhizae", "Saprotroph",
                                      "Pathogen", "Parasite", 
                                      "Ericoid mycorrhizae", 
                                      "Orchid mycorrhizae", 
                                      "Arbuscular mycorrhizae",
                                      "Endophyte",
                                      "Wood decayer", 
                                      "Yeast" ))

# Factorise FungusName
genome.df$FungusName <- factor(genome.df$FungusName, 
                              levels = unique(genome.df$FungusName))

#=====================
# Visualisation
#=====================
#----------------
# Box plot for ecological groups 
#----------------
# Remove Yeast + Parasite - there are only few and does not help statistics of boxplot
genome.s.df <- genome.df[!genome.df$Ecology %in% c("Yeast", "Parasite"),]

# Re factorise Ecology
genome.s.df$Ecology <- factor(genome.s.df$Ecology,
                              levels= c("Ectomycorrhizae", "Saprotroph", 
                                        "Pathogen",  
                                        "Ericoid mycorrhizae", 
                                        "Orchid mycorrhizae", 
                                        "Arbuscular mycorrhizae",
                                        "Endophyte",
                                        "Wood decayer"))

# Get max count for genome
max.count <- max(genome.s.df$Genome.size)
# Set max.count for 567,950,182

# box Plot function without fungal names
box.fun <- function(Data, MaxCount, TEXT, X_axis_switch, ID_switch, Limit, Colour) {
    ggplot(Data, aes(x = Ecology, y = Genome.size/1000000, fill = Ecology)) + 
        scale_fill_manual(values= Colour) +
        scale_x_discrete(limits = rev(levels(Data$Ecology))) +
        geom_boxplot() + 
        # Limit Y axis 
        coord_flip(ylim = c(0, Limit)) +
        ggtitle(TEXT) +
    theme_minimal() +
        theme(plot.title = element_text(colour= "grey30", size = 11, 
                                        hjust = 0.5),
              legend.position="none",
              # axis.text.x=element_blank(),# This is X axis on and off
              # axis.text.y=element_blank(),# This is Y axis on and off
              axis.title.x=element_blank(), # This is X label on and off
              axis.title.y=element_blank(), # This is Y label on and off
              # IDs on and off
              axis.text.y= ID_switch,
              # axis.title.y= element_text(colour= "gray30", size=12),
              axis.text.x= X_axis_switch
              #axis.title.x=element_text(colour= "gray20", size=11),
        )
}

box.genome.p <- box.fun(genome.s.df, max.count, "Genomes (Mbp)", 
                 # Count measure (X axis) is off
                 element_text(colour= "gray40", size=10), 
                 # Fungal group ID (Y axis) is on
                 element_text(colour= "gray40", size=10), 
                 # Cut off count
                 150, 
                 # Colour  
                 iro)

#----------------
# Box plot for TE coverage according to ecological groups 
#---------------
# Remove Yeast + Parasite - there are only few and does not help statistics of boxplot
count.s.df <- count.df[!count.df$Ecology %in% c("Yeast", "Parasite"),]

# Re factorise Ecology
count.s.df$Ecology <- factor(count.s.df$Ecology,
                              levels= c("Ectomycorrhizae", "Saprotroph", 
                                        "Pathogen",  
                                        "Ericoid mycorrhizae", 
                                        "Orchid mycorrhizae", 
                                        "Arbuscular mycorrhizae",
                                        "Endophyte",
                                        "Wood decayer"))
# Get max count for TE coverage
max.cover <- max(count.s.df$TE.CoverageTotal)

# box Plot function without fungal names
box.te.fun <- function(Data, MaxCount, TEXT, X_axis_switch, ID_switch, CountLimit, Colour) {
  ggplot(Data, aes(x = Ecology, y = TE.CoverageTotal, fill = Ecology)) + 
    scale_fill_manual(values= Colour) +
    scale_x_discrete(limits = rev(levels(Data$Ecology))) +
    geom_boxplot() + 
    coord_flip(ylim = c(0, CountLimit)) +
    ylim(0, MaxCount) +
    ggtitle(TEXT) +
    theme_minimal()+
    theme(plot.title = element_text(colour= "grey30", size = 11, 
                                    hjust = 0.5),
          legend.position="none",
          # axis.text.x=element_blank(),# This is X axis on and off
          # axis.text.y=element_blank(),# This is Y axis on and off
          axis.title.x=element_blank(), # This is X label on and off
          axis.title.y=element_blank(), # This is Y label on and off
          # IDs on and off
          axis.text.y= ID_switch,
          # axis.title.y= element_text(colour= "gray30", size=12),
          axis.text.x= X_axis_switch,
          #axis.title.x=element_text(colour= "gray20", size=11),
          axis.ticks=element_blank()
    )
}

box.te.p <- box.te.fun(count.s.df, 
                 max.count, "Repeat element coverage (%)", 
                 # Count measure (X axis) is off
                 element_text(colour= "gray40", size=10), 
                 # Fungal group ID (Y axis) is off
                 element_blank(), 
                 # Cut off count + colour for symbiont removed
                 max.cover, iro)

#-------------------
# Put plots together
#-------------------
# Box plots - Genome size vs TE coverage 
dir <- getwd()
mypath <- paste0(dir,"/" , ProjectName, "_Step6.5_","LifeStyle_", "Box_GenomeSizeVsTEs", ".pdf")
pdf (file=mypath, height = 5, width = 7,  onefile=FALSE)

# Put all plots together
ggarrange (box.genome.p, box.te.p, ncol = 2)

dev.off()