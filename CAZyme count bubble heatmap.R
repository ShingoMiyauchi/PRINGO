# Visual Total + Secreted CAZyme counts 
#                               Shingo Miyauchi 26Jan20
#------------------
# Descriptions 
#------------------
# This script generates bubble heatmaps for the gene count of interest. 

# <Input files>
# (1) Count of total and secreted genes and sum table for CAZymes  
# 135fungi_PCWDE_Step1_CountOfCAZymes.csv

# <Methods>
# (1) Get colours for plots + factorise variables 
# (2) Make figures 
# (3) Put all figures together  

# IMPORTANT: ggtree objects are not aligned well using "ggarrange" function. You need to manually adjust with InDesign or Illustrator.  
#------------------
# IMPORTANT:Global option to switch off factorisation. 
options(stringsAsFactors=FALSE)

#================
# Preparation
#================
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(gtools)
library(ggpubr)
library(reshape2)

# Insert ProjectName
ProjectName <- "135_fungi_PCWDE"

# Loading list of data frames from Step 2
df <- read.csv("135_fungi_PCWDE_Step1_CountOfCAZymes.csv")

#=====================
# (1) Get colours for plots + factorise variables 
#=====================
#  IMPORTANT: Manually set colours - 11 entries
unique(df$Ecology)
#"Ectomycorrhizae"        "Saprotroph"             "Wood decayer"          
#"Pathogen"               "Endophyte"              "Orchid mycorrhizae"    
#"Ericoid mycorrhizae"    "Parasite"               "Yeast"                 
#"Arbuscular mycorrhizae"

# Set a set of colours (identical to all figures used in TINGO with 122 fungi)
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

# Colour function for heatmap
HotRed <- function(n) {
  col.ls<-colorRampPalette(brewer.pal(9,"Reds"))(n)
  col.ls[1:n]
}

# Check the colours 
heat.color <- HotRed (50)
plot(rep(1,length(heat.color)),col=heat.color,pch=19,cex=3)

#----------------------------
# Factorise Substrate + Species
#----------------------------
# Factorise Class
df$Ecology <- factor(df$Ecology, 
    levels= c("Ectomycorrhizae", "Saprotroph",
              "Pathogen", "Parasite", 
              "Ericoid mycorrhizae", 
              "Orchid mycorrhizae", 
              "Arbuscular mycorrhizae",
              "Wood decayer", "Endophyte","Yeast"))

#-------------------------------
# Convert four data frames into a long format
#------------------------------
# Get unique phyla
df$Phylum %>% unique()
# "Basidiomycota" "Ascomycota"  "Mucoromycota"

# Separate into two data frames  
df.ls <- list(Asco= df[df$Phylum %in% c("Ascomycota", "Mucoromycota"),],
              Basi= df[df$Phylum == "Basidiomycota",])

long.df.ls <- lapply(df.ls, function(X) melt(X,
                # ID variables - all the variables to keep but not split apart on
                id.vars=c("JGI_ID", "Ecology", "FungusName",  "Phylum", "Class",  "Order", "Family",  "Genus",   "CladeOrder" ),
                # The source columns (original column names that insert in a destination column - specify the order and the names) 
                measure.vars=c(names(df)[!names(df) %in% c("JGI_ID", "Ecology", "FungusName",  "Phylum", "Class",  "Order", "Family",  "Genus",   "CladeOrder")]),
                # Name of the destination column that will identify the original column that the measurement came from
                variable.name="CAZy",
                value.name="Count"))

# Remove zero count
for(i in 1:2) long.df.ls[[i]]$Count[long.df.ls[[i]]$Count == 0] <- NA

# Factorise Species
for(i in 1:2) long.df.ls[[i]]$FungusName <- factor(long.df.ls[[i]]$FungusName, levels = unique(long.df.ls[[i]]$FungusName))

#===============
# (2) Make figures 
#===============
# Check max and min counts
min.max.count <- c(1, as.matrix(df[, c(10:20)]) %>% max())

# Function for making heatmap
heat.fun <- function(X, heat_colour, circle_size){
  ggplot(X, aes(x= CAZy, y= FungusName, label= Count)) +
    geom_point(aes(x= CAZy, y= FungusName, colour = Count, 
                   size= Count), alpha = 0.6) +
    # heat colours
    scale_color_gradientn (colors= heat_colour, na.value=NA) +
    
  # Text colour and size
    geom_text(color="grey50", size=2) +
    
    # Circle size
    scale_size(range = circle_size, limits = min.max.count) +
  
    # X axis label on top
    scale_x_discrete(position = "top", expand = c(0, 0.5)) +
    
    # IMPORTANT: Reverse fungal names
    scale_y_discrete(limits= rev(levels(X$FungusName)),
                     expand = c(0, 1.2)) +
    
    # Hide legend for count size
    guides(size = FALSE, colour = guide_legend(override.aes = list(size=8), 
                                               nrow=3, byrow=TRUE)) +
    
    # Titile
    # ggtitle(Odai) +
    
    ### Legend ###
    #theme_minimal() +
    theme(legend.position="none",
          # Remove legend title
          legend.title=element_blank(),
          # Legend background off
          legend.key=element_blank(),
          # Axis ticks off 
          axis.ticks = element_blank(),
          # Label on X axis 
          axis.text.x = element_text(colour= "grey30", 
                                     size=8, angle=-45, vjust=1, hjust=1), 
          # Y axis swich on and off
          #axis.text.y = element_blank(),
          axis.text.y = element_text(colour= "grey30", size=6.5, vjust=0.5, hjust=0.5),
          
          # This is X + Y title on and off
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          # axis.title.y=element_text(colour= "gray50", size=15),
          panel.border=element_blank(),
          # Background off
          panel.background= element_blank()
    )
}

# Function for making bubble plots
bubble.fun <- function(X, bubble_colour, LEGEND){
  
  ggplot(X, aes(x=Lifestyle, y= FungusName)) +
    geom_point(aes(x=Lifestyle, y= FungusName, colour = Ecology), 
               size= 4.5, alpha = 0.7) +
    
    # Bubble colours
    scale_colour_manual(values = bubble_colour) +
    
    # X axis label on top
    scale_x_discrete(position = "top", expand = c(0, 0.5)) +
    
    # IMPORTANT: Reverse fungal names
    scale_y_discrete(limits= rev(levels(X$FungusName)),
                     expand = c(0, 1.2)) +
    
    # Hide legend for count size
    guides(size = FALSE, 
           colour = guide_legend(override.aes = list(size=8), 
                                 nrow=2, byrow=TRUE)) +
    
    # # Hide legend for count size
    # guides(size = FALSE, colour = FALSE) +
    # 
    #ggtitle(Odai) +
    
    ### Legend ###
    # theme_minimal() +
    theme(legend.position= LEGEND,
          # Remove legend title
          legend.title=element_blank(),
          # Legend background off
          legend.key=element_blank(),
          # Axis ticks off 
          axis.ticks = element_blank(),
          # Label on X axis 
          # axis.text.x = element_blank(),
          axis.text.x = element_text(colour= "grey30", 
                                     size=8, angle=-45, vjust=1, hjust=1),
          # Y axis swich on and off
          axis.text.y = element_blank(),
          #axis.text.y = element_text(colour= "grey30", size=9, vjust=0.5, hjust=0.5),
          
          # This is X + Y title on and off
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          # axis.title.y=element_text(colour= "gray50", size=15),
          panel.border=element_blank(),
          # Background off
          panel.background= element_blank()
    )
}

#------------------------
# Make bubble + bar plots
#------------------------
heat.asco <- heat.fun(long.df.ls$Asco,
                   circle_size = c(3, 10),
                    heat_colour = heat.color)

heat.basi <- heat.fun(long.df.ls$Basi,
                      circle_size = c(3, 10),
                      heat_colour = heat.color)  

# Create fake column for Lifestyle dots 
long.df.ls$Asco$Lifestyle <- 1
long.df.ls$Basi$Lifestyle <- 1

# Make colour bubbles for lifestyle
eco.asco <- bubble.fun(long.df.ls$Asco, bubble_colour = iro, 
                       LEGEND = "none")
eco.basi <- bubble.fun(long.df.ls$Basi, bubble_colour = iro,
                       LEGEND = "none")

# Generate same figures with legends 
legend.asco <- bubble.fun(long.df.ls$Asco, bubble_colour = iro, 
                       LEGEND = "bottom")
legend.basi <- bubble.fun(long.df.ls$Basi, bubble_colour = iro,
                       LEGEND = "bottom")

#--------------
# Save figures 
#-------------
asco.p <- ggarrange(eco.asco, heat.asco, align = "h", widths = c(3, 7))
basi.p <- ggarrange(eco.basi, heat.basi, align = "h", widths = c(3, 7))

# Save figures
dir <- getwd()
mypath <- paste0(dir,"/" , ProjectName, "_Step2_", "PCWDEs_Secreted", ".pdf")
pdf (file=mypath, height = 11, width = 11, onefile=FALSE)
ggarrange(asco.p, basi.p, ncol=2)
dev.off()

# Save legend for ecology bubbles
dir <- getwd()
mypath <- paste0(dir,"/" , ProjectName, "_Step2_", "PCWDEs_Secreted_legend", ".pdf")
pdf (file=mypath, height = 8, width = 14, onefile=FALSE)
ggarrange(legend.asco, legend.basi, ncol=2)
dev.off()

#NOTE 30April19: Raw figures are messy because it contains redundant labels. Remove them and add legend with Illustrator. 
