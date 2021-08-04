# Visual genomic features with PRINGO & TINGO

                                               Shingo Miyauchi 7April20
# Descriptions

Proteomic Information Navigated Genomic Outlook (PRINGO) and Transposon Identification Nominative Genome Overview (TINGO; Morin et al., 2019) are custom R scripts for data integration and visualisation. This demonstration includes the main Figures, which were used to support the core findings in Miyauchi et al. (2020) in Nature Communications. 

PRINGO enables us to visualise combined output from the established tools and websites including BUSCO (Sima et al., 2015), Genome stats from Mycocosm (http://mycocosm.jgi.doe.gov), Secretome prediction (Pellegrin et al., 2015), identified transposable elements (Morin et al., 2019), the CAZyme repertoire (http://www.cazy.org/). TINGO generates visual bubble charts from identified transposable elements (Morin et al., 2019).  

*Please note that PRINGO and TINGO do not produce bioinformatic results. They are visualisation tools for complex genomic data. 

# Requirement for R and packages
Please install, 
R Studio,
R 3.5.1 

Please install R packages,
egg  0.4.0,
dplyr 0.7.6,
ggplot2 3.0.0,
gtools 3.8.1,
ggpubr 0.2,
reshape2 1.4.3,
RColorBrewer 1.1-2

# How to run the demonstration
1) Download files into a folder
2) Automatically start up R Studio by clicking an R script
3) Make sure the working directory is in the folder so that R recognises the input file. 
4) Read instructions in the scripts and execute the code (line by line).  
5) Figures are generated as output in the same folder. 

*Please note that raw figures generated from the scripts were adjusted with Adobe Illustator for the final reader-friendly version.  

# (1) Genomic feature bar plot

We visualised the combined results of; 1) calculated BUSCO (genome completeness); 2) Genome stats from Mycocosm; 3) identified transposable elements; 4) the number of theoretically secreted proteins from secretome prediction (see Supplementary Table 2).

Input file: 
135_Fungi_GF_Step2_GenomicInfoCombined.csv

Output file:
135_Fungi_GF_Step3_GenomicFeatures.pdf

# (2) Transposable element bubble plot

We visualised the combined results of the genome size and identified transposable elements by grouping 135 fungi based on their lifestyles. 

Input file:
135_Fungi_TINGO_Step5_Taxo_Coverage_Stats.csv

Output file:
135_Fungi_TINGO_Step6.5_LifeStyle_Box_GenomeSizeVsTEs.pdf

# (3) Genome size + TE coverage box plot

We visualised the combined results of identified transposable elements for 135 fungi using TINGO pipeline (Morin et al., 2019). The script produces two plots - The TE coverage in the genomes and the copy number of TE families. Please note that we only used the TE coverage plot in the manuscript.    

Input file: 
135_Fungi_TINGO_Step5_Taxo_Coverage_Stats.csv

Output files:
135_Fungi_TINGO_Step6.4_LifeStyle_TE_Coverage.pdf
135_Fungi_TINGO_Step6.4_LifeStyle_TE_CopyNumber.pdf

# (4) Secreted CAZyme bubble plot

We selected the secreted CAZymes of interest from the count of secretomes and used as an input file. The script makes a heatmap showing the count of genes and legends separately. We put them together using Adobe Illustrator. 

Input file:
135_fungi_PCWDE_Step1_CountOfCAZymes.csv

Output files:
135_fungi_PCWDE_Step2_PCWDEs_Secreted_legend.pdf (legend only)
135_fungi_PCWDE_Step2_PCWDEs_Secreted.pdf (heatmap)

# References 
1. Morin, E., Miyauchi, S., San Clemente, H., Chen, E.C.H., Pelin, A., de la, Providencia, I., Ndikumana, S., Beaudet, D., Hainaut, M., Drula, E., Kuo, A., Tang, N., Roy, S., Viala, J., Henrissat, B., Grigoriev, I.V., Corradi, N., Roux, C. and Martin, F.M. (2019), Comparative genomics of Rhizophagus irregularis, R. cerebriforme, R. diaphanus and Gigaspora rosea highlights specific genetic features in Glomeromycotina. New Phytol, 222: 1584-1598. doi:10.1111/nph.15687 
2. Pellegrin, C., Morin, E., Martin, F. M. & Veneault-Fourrey, C. Comparative analysis of secretomes from ectomycorrhizal fungi with an emphasis on small-secreted proteins. Front. Microbiol. 6, (2015).
3. Sima, F. A., Waterhouse, R. M., Ioannidis, P., Kriventseva, E. V., and Zdobnov, E. M. (2015). Genome analysis BUSCO: assessing genome assembly and annotation completeness with single-copy orthologs. Bioinformatics 31,3210–3212. doi: 10.1093/bioinformatics/btv351 
