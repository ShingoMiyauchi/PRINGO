# Visualisation of TE coverge and copy number according to ecological styles
#                                                   Shingo Miyauchi 26Jan20

#-------------------------------
#       Descriptions
#-------------------------------
# This script visualises ecololgical style-wise bubble plots for the coverage of TE in genome, copy number of TE types, and total TE coverage. Then, it produces combined bubble plots (Coverage + copy number for each TE) and bar\box plots (Total coverage + each coverage).  

# <Input files>
# 1) Taxonomic info, tree order,  and TE annotations + counts from TINGO pipeline (Morin et al., 2019)
# #####_Step5_Taxo_Coverage_Stats.csv

# <Method>
# 1) Long format transformation
# 2) Set up parameters for visualisation
# 3) Visualisation of bar and bubble plots.
# 4) Put plots together and generate PDF.

# Important note 12Aug19: Removed Tcn760, Repetitive, SAT - they are not informative
#-------------------------------
# IMPORTANT:Global option to switch off factorisation. 
options(stringsAsFactors=FALSE)

#===============
#  Preparation
#===============
library(ggplot2)
library(reshape2)
library(egg)
library(dplyr)

# Insert Project name
ProjectName <- "135_Fungi_TINGO"

#---------------
# Loading files 
#---------------
# Load stats files and combine
count.df <- read.csv (list.files(pattern="*_Step5_Taxo_Coverage_Stats.csv"))

# Calculate max coverage/copy number in count.df
max_coverage <- max(count.df$Coverage, na.rm = T)
max_copy <- max(count.df$CopiesPerFamily, na.rm = T) 

### IMPORTANT: Remove Tcn760, Repetitive, SAT - they are not informative ###
count.df <- count.df[!count.df$TE.family %in% c("Tcn760", "Repetitive", "SAT"),]

#=====================
# PART 1: Preparation for plots
#=====================
#------------------------------
# Factorise Species Name - IMPORTANT: must be identical to the tree!!
#------------------------------
count.df$SpeciesName <- factor(count.df$FungusName, levels= unique(count.df$FungusName))

#------------------------------------
# Modify Ecology for ecological classification 
#------------------------------------
count.df$Ecology %>% unique()
#"Ectomycorrhizae"        "Saprotroph"             "Wood decayer"          
#"Pathogen"               "Endophyte"              "Orchid mycorrhizae"    
#"Ericoid mycorrhizae"    "Parasite"               "Yeast"                 
#"Arbuscular mycorrhizae"

#--------------------------
# Long format transformation
#--------------------------
long.df <- melt(count.df[,c("JGI_FungalID", "SpeciesName", "Ecology", "Family", "TE.family", "Coverage", "CopiesPerFamily","TE.type", "CladeOrder")], 
            # ID variables - all the variables to keep but not split apart on
            id.vars=c("JGI_FungalID","SpeciesName", "Ecology","Family", "TE.family","TE.type", "CladeOrder"),
            # The source columns (original column names that insert in a destination column - specify the order and the names) 
            measure.vars=c("Coverage", "CopiesPerFamily"),
            # Name of the destination column that will identify the original column that the measurement came from
            variable.name="Coverage_Copies",
            value.name="Value")

#===========================
# Assign ecology types with colours
#===========================
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

# Factorise Ecology and levels
long.df$Ecology <- factor(long.df$Ecology,
                        levels= c("Ectomycorrhizae", "Saprotroph",
                                  "Pathogen", "Parasite", 
                                  "Ericoid mycorrhizae", 
                                  "Orchid mycorrhizae", 
                                  "Arbuscular mycorrhizae",
                                  "Endophyte",
                                  "Wood decayer", 
                                  "Yeast" ))

# Factorise TE.type
#long.df$TE.type %>% unique()
long.df$TE.type <- factor(long.df$TE.type,
                          levels= c( "LTR retrotransposon", 
                                     "Non-LTR transposon",
                                     "DNA transposon", 
                                     "Simple repeat", 
                                     "Other"))

# Factorise TE.family grouped by TE.type 
long.df$TE.family <- factor(long.df$TE.family, 
                      levels= unique(long.df$TE.family[order(long.df$TE.type)]))

#=================================
# Make tables for total TE coverage/copy plots
#=================================
# Get total coverage 
cover.df <- count.df[,c("JGI_FungalID", "SpeciesName", "CladeOrder", "Ecology", "TE.CoverageTotal")]
cover.df <- cover.df[!duplicated(cover.df),]

# Get sum of copies per species
copy.df <- aggregate(CopiesPerFamily ~ JGI_FungalID, count.df[,c("JGI_FungalID", "CopiesPerFamily")], sum)

# Merge Phylum and Lifestyle 
stat.df <- merge(cover.df, copy.df)

# IMPORTANT: Order by unique count.df$SpeciesName
stat.df <- stat.df[order(stat.df$SpeciesName),]

# Double check
temp.df <- data.frame(stat.df$SpeciesName, count.df$SpeciesName %>% unique())

View(temp.df)
#NOTE: Yep, same oder

rm(temp.df)

# Factorise Ecology
stat.df$Ecology <- factor(stat.df$Ecology, 
                          levels= c("Ectomycorrhizae", "Saprotroph",
                                      "Pathogen", "Parasite", 
                                      "Ericoid mycorrhizae", 
                                      "Orchid mycorrhizae", 
                                      "Arbuscular mycorrhizae",
                                      "Endophyte",
                                      "Wood decayer", 
                                      "Yeast" ))

#=====================
# PART 2: Visualisation
#=====================
#-------------------------------------
# 1) Make a bubble plot for coverage and copy number
#-------------------------------------
bubble.plot.fun <- function(Data, Label, Colour) {
  ggplot(Data, aes(x = SpeciesName, y = TE.family, 
                  label= as.numeric(format(round(Value, 1), nsmall=1)))) +
    #22Nov17 NOTE: Round up values and 1 digit for decimal. Remove empty space that format function adds by nuericalising.         
    
    geom_point(aes(x = SpeciesName, y = TE.family, colour = Ecology, 
                   size= Value), alpha = 0.75) +
    
    scale_color_manual(values= Colour) +
    geom_text(color="white", size=2) +
    scale_size(range = c(3, 15)) +
    
    # labs(=Label) +
    # Remove bubble legend
    guides(size = FALSE) +
    
    # X and Y flip
    coord_flip() +
    
    # IMPORTANT: Reverse fungal names
    scale_x_discrete(limits= rev(levels(Data$SpeciesName)), 
                     expand = c(0, 1.2)) +
    
    # Set Y axis on top
    scale_y_discrete(position= "right") +
    
    # Title
    ggtitle(Label) + 
    guides(colour = guide_legend(override.aes = list(size=8))) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), # Title position 
          legend.title = element_blank(),
          legend.text=element_text(size=14),
          legend.position="bottom",
          axis.text.x = element_text(colour= "grey50", 
                                     size=10, angle=-40, vjust=1, hjust=1), 
          axis.text.y = element_text(colour= "gray50", size=10, vjust=0.5, hjust=0.5),
          # This is X title on and off
          axis.title.x=element_blank(),
          # This is Y title on and off)
          axis.title.y=element_blank()
          )
}

# Generate figures 
cover.bub <- bubble.plot.fun(long.df[long.df$Coverage_Copies == "Coverage",], "Coverage (%)", iro)
copy.bub <- bubble.plot.fun(long.df[long.df$Coverage_Copies == "CopiesPerFamily",], "Copy number", iro)

#-------------------------------
# 2) Make a bar plot for coverage
#-------------------------------
cover.bar.fun <- function(Data, Colour) { 
  ggplot(Data, aes(x = SpeciesName, y = TE.CoverageTotal, fill=Ecology)) +
    
    scale_fill_manual(values= Colour) +
    
    geom_bar(stat = "identity", alpha= 0.85) +
    
    # IMPORTANT: Reverse fungal names
    scale_x_discrete(limits= rev(levels(Data$SpeciesName)),
                     expand = c(0, 1.2)) +
    
    # Set Y axis 0 to 1
    # This function removes space between labels and plots
    scale_y_continuous(position= "right", expand = c(0, 0)) +
    
    # X and Y flip
    coord_flip() +
    
    ylab("Total") + 
    
    theme_minimal() +
    
    # Control text and titles
    theme(
      # Label on X axis 
      axis.title.x= element_text(colour= "grey30", size=9),
      axis.text.x = element_text(colour= "grey30", size=9),
      
      # This is Y labels on and off    
      axis.text.y=element_blank(), 
      #axis.text.y= element_text(colour= "gray50", size=10),
      axis.title.y= element_blank(),
      legend.position="none"
    )
}

cover.bar <- cover.bar.fun(stat.df, iro)

#---------------------------------
# 3) Make a bar plot for copy number
#---------------------------------
copy.bar.fun <- function(Data, Y_limit, Colour) {
  ggplot(Data, aes(x = SpeciesName, y = CopiesPerFamily, fill= Ecology)) +
    
    scale_fill_manual(values= Colour) +
    geom_bar(stat = "identity", alpha= 0.85) +
    ylab("Total") + 
    
    # IMPORTANT: Reverse fungal names
    scale_x_discrete(limits= rev(levels(Data$SpeciesName)),
                     expand = c(0, 1.2)) +
    
    # Set Y axis 0 to 1
    # This function removes space between labels and plots
    scale_y_continuous(position= "right", expand = c(0, 0)) +
    
    # X and Y flip + Limit Y axis
    coord_flip(ylim = c(0, Y_limit)) +
    
    #  
    # coord_cartesian(ylim = c(0, Y_limit)) +
    
    theme_minimal() +
    theme(
      # Label on X axis 
      axis.title.x= element_text(colour= "grey30", size=9),
      axis.text.x = element_text(colour= "grey30", size=9),
      
      # This is Y labels on and off    
      axis.text.y=element_blank(), 
      #axis.text.y= element_text(colour= "gray50", size=10),
      axis.title.y= element_blank(),
      legend.position="none"
    )
} 

# Max copy number 
max(stat.df$CopiesPerFamily) # 21556 - crazy outlier. Use threshold of 5,000

# Generate a plot
copy.bar <- copy.bar.fun(stat.df, 5000, iro)

#---------------
# 4) Put plots together
#---------------
# Bubble + bar plots
# Coverage 
pdf (file= paste0(getwd(), "/" , ProjectName, "_Step6.4_", "LifeStyle_TE_Coverage", ".pdf"), height = 17, width = 10, onefile=FALSE)
ggarrange (cover.bub, cover.bar, ncol = 2, widths = c(0.8, 0.2))
dev.off()

# Copy number 
pdf (file= paste0(getwd(), "/" , ProjectName, "_Step6.4_", "LifeStyle_TE_CopyNumber", ".pdf"), height = 17, width = 10, onefile=FALSE)
ggarrange (copy.bub, copy.bar, ncol = 2, widths = c(0.8, 0.2))
dev.off()
