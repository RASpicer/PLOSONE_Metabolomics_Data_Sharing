---
title: "Analysis of Data Sharing Practices of Metabolomics Studies Published in PLOS ONE "
author: "Analysis by Rachel Spicer, github:RASpicer"
date: "27/06/2018"
output: 
  html_document:
    code_folding: hide
    number_sections: yes
    theme: cerulean
    keep_md: true
---



# Data Processing
This RMarkdown contains the code used for analysis for the subsection <b>Metabolomics Data Sharing in PLOS ONE</b> of Chapter 3 of the thesis <b>Fit for purpose? A metascientific analysis of metabolomics data in public repositories</b>. 

The data sharing statements of the journal articles were classified into 7 levels: A-G, including six classifications of data availability statement (A-F) and one level (G) indicating no data availability statement was present.


```r
StatementLevels <- data.frame(Levels = c("A", "B", "C", "D", "E", "F", "G"), 
    Description = c("All relevant data are within the paper and its Supporting Information files. This classification includes typos and variations such as 'All relevant data are within the paper and Supporting Information files.' or 'All relevant data are within the paper and supplement.'", 
        "All relevant data are within the paper. This type of statement is used when a paper has no supplementary material.", 
        "Metabolomics data available in a repository.", "All relevant data are within the paper and its Supporting Information files and data for another type of omics is available in a repository or just a repository specific for a different type of omics data is mentioned.", 
        "Data available on request.", "Data cannot be made publicly available.", 
        "No data availability statement."))
kable(StatementLevels, caption = "Data availability statement classification levels.")
```



Table: Data availability statement classification levels.

Levels   Description                                                                                                                                                                                                                                                                   
-------  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
A        All relevant data are within the paper and its Supporting Information files. This classification includes typos and variations such as 'All relevant data are within the paper and Supporting Information files.' or 'All relevant data are within the paper and supplement.' 
B        All relevant data are within the paper. This type of statement is used when a paper has no supplementary material.                                                                                                                                                            
C        Metabolomics data available in a repository.                                                                                                                                                                                                                                  
D        All relevant data are within the paper and its Supporting Information files and data for another type of omics is available in a repository or just a repository specific for a different type of omics data is mentioned.                                                    
E        Data available on request.                                                                                                                                                                                                                                                    
F        Data cannot be made publicly available.                                                                                                                                                                                                                                       
G        No data availability statement.                                                                                                                                                                                                                                               

The level of data sharing of the study was then recorded. Studies that do not include publicly available raw data can have multiple classifications of both level 4 and either level 2, 3 or 5. However they cannot be classified as 2 and 3, 2 and 5 or 3 and 5. This is because raw data (level 1) can be processed and used to generate any of the subsequent types of data: peak lists, tables, figures of spectra and figures of metabolites. Tables and figures of metabolites can be produced from a peak list, but cannot be used to generate a figure of the raw spectra. From some tables (level 3), it is possible to generate some figures (level 5), but not all contain sufficient information.


```r
SharingLevels <- data.frame(Levels = c("1", "1a", "1b", "1c", "2", "3", "4", 
    "5"), Description = c("Raw data available (either commercial format e.g. Thermo .raw, Agilent .d or as an open format e.g. .mzML, .mzXML, .nmrML).", 
    "Raw data available in a specific metabolomics mepository (e.g. MetaboLights, Metabolomics Workbench).", 
    "Raw data available in a general repository (e.g. Figshare, Dryad, Zenodo).", 
    "Raw data available by other means e.g. in supplementary material or on an institutional website.", 
    "Peak list containing relative quantifications, concentrations, etc., on a per sample level (usually in .csv or .xlsx format).", 
    "Table of metabolites, including all identified metabolites or only differentially expressed metabolites.", 
    "Figure of spectra.", "Figure showing differentially expressed metabolites (such as a scatter plot, bar chart, heat map, etc.)"))
kable(SharingLevels, caption = "Data sharing classification levels. If a study does not have raw data publicly available, it can be classified as both level 4 and either level 2, 3 or 5. It cannot be classified as 2 and 3, 2 and 5 or 3 and 5.")
```



Table: Data sharing classification levels. If a study does not have raw data publicly available, it can be classified as both level 4 and either level 2, 3 or 5. It cannot be classified as 2 and 3, 2 and 5 or 3 and 5.

Levels   Description                                                                                                                   
-------  ------------------------------------------------------------------------------------------------------------------------------
1        Raw data available (either commercial format e.g. Thermo .raw, Agilent .d or as an open format e.g. .mzML, .mzXML, .nmrML).   
1a       Raw data available in a specific metabolomics mepository (e.g. MetaboLights, Metabolomics Workbench).                         
1b       Raw data available in a general repository (e.g. Figshare, Dryad, Zenodo).                                                    
1c       Raw data available by other means e.g. in supplementary material or on an institutional website.                              
2        Peak list containing relative quantifications, concentrations, etc., on a per sample level (usually in .csv or .xlsx format). 
3        Table of metabolites, including all identified metabolites or only differentially expressed metabolites.                      
4        Figure of spectra.                                                                                                            
5        Figure showing differentially expressed metabolites (such as a scatter plot, bar chart, heat map, etc.)                       


```r
# Read csv of PLoS One Classification
PLOSONE <- read.csv("../data/OpenDataPLOSONEMetabolomics.csv", stringsAsFactors = FALSE, 
    check.names = FALSE)

# Extract only metabolomics studies
MetabStudies <- PLOSONE$Metabolomics == 1
MetabStudies <- PLOSONE[MetabStudies, ]

# Extract only primary metabolomics studies
PrimaryStudies <- MetabStudies$Primary_study == 1
PrimaryStudies <- MetabStudies[PrimaryStudies, ]

# Split Data sharing Classification, as it is possible that each study has
# more than one
PrimaryStudiesSplit <- PrimaryStudies %>% mutate(Data_sharing_class = strsplit(as.character(Data_sharing_class), 
    ",")) %>% unnest(Data_sharing_class)

# Convert DataSharingClass to Factor to combine split 4 and 5 factors into
# one PrimaryStudiesSplit$Data_sharing_class <-
# as.factor(PrimaryStudiesSplit$Data_sharing_class)

# Produce a table of data sharing statement classification
StatementLevel <- as.data.frame(table(PrimaryStudies$Statement_classification))
colnames(StatementLevel) <- c("Classification", "Frequency")

DataSharingLevel <- as.data.frame(table(PrimaryStudiesSplit$Data_sharing_class))
colnames(DataSharingLevel) <- c("Classification", "Frequency")
```

# Statistical Analysis 
A Pearson's chi-squared ( $\chi^2$) test was used to evaluate the likelihood that the observed differences between the relationship between classification of data availability statements and levels of data sharing arose due to chance. 


```r
# Create a contingency table of statement classification and data sharing
# level
Table = table(PrimaryStudies$Statement_classification, PrimaryStudies$Data_sharing_class)

# As the table function splits the data sharing levels 2, 3 and 4 (into '2',
# '3', '4', '2,4', '3,4', '4,5'), these need to be recombined.

# Create contingency table combining: 2 with 2,4 3 with 3,4 4 with 2,4; 3,4;
# 4,5 5 with 4,5
ConTable <- cbind(Table[, "1a"], Table[, "1b"], Table[, "1c"], Table[, "2"] + 
    Table[, "2,4"], Table[, "3"] + Table[, "3,4"], Table[, "4"] + Table[, "2,4"] + 
    Table[, "3,4"] + Table[, "4,5"], Table[, "5"] + Table[, "4,5"])

# chi-squared test
chisq <- chisq.test(ConTable)
# X-squared = 237.77, df = 36, p-value < 2.2e-16 (1.448768e-31)
```

# Figures
Code that was used to generate raw figures. Figures 3.6A and 3.6B were then combined in 
Adobe Illustrator.

## Figure 3.6A. Frequency of data availability statement levels in primary metabolomics studies

Bar chart showing the frequency of data availability statement levels in primary metabolomics studies. Studies have only a single data availability statement level.


```r
ggplot(StatementLevel, aes(Classification, Frequency, fill = Classification))  + 
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(limits = c(0,250), expand = c(0,0)) +
  xlab("Level") +
  theme_bw() +
  theme(
    axis.text = element_text(colour = "black"),
    #axis.text.x  = element_text(angle=45),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    # Remove gridlines and borders
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(), 
    legend.position = 'none') +
  scale_fill_manual(values = c("#56b4e9","#cc79a7","#e69f00","#f0e442","#009e73", "#d55e00", "#0072b2"))
```

<img src="figs/statementlevel-1.png" style="display: block; margin: auto;" />

## Figure 3.6B. Frequency of data sharing levels in primary metabolomics studies

Bar chart showing the frequency of data sharing levels in primary metabolomics studies. Studies can receive multiple data sharing levels if they are not classified as level 1.


```r
ggplot(DataSharingLevel, aes(Classification, Frequency, fill = Classification))  + 
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(limits = c(0,250), expand = c(0,0)) +
  xlab("Level") +
  theme_bw() +
  theme(
    axis.text = element_text(colour = "black"),
    #axis.text.x  = element_text(angle=45),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    # Remove gridlines and borders
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(), 
    legend.position = c(0.15, 0.8)) +
  scale_fill_manual(breaks=c("1a","2","3","4","5"), values = c("#e41a1c","#e41a1c", "#e41a1c","#f781bf","#984ea3", "#4daf4a", "#ff7f00"), labels = c("1", "2", "3", "4", "5"))
```

<img src="figs/datasharinglevel, statementlevel-1.png" style="display: block; margin: auto;" />

## Figure 3.8. A heat map representation representation of the $\chi^2$ correlation matrix for data statement classification and data sharing level
Maximum positive correlation and negative correlation are respectively indicated in blue and red.


```r
corrplot(chisq$residuals, method = "color", tl.col = "black", tl.srt = 0, is.cor = FALSE)
```

<img src="figs/corrplot-1.png" style="display: block; margin: auto;" />

## Figure 3.9 The frequency of PLOS ONE studies linked to public data on each repository
Studies are coloured in blue if raw data is available, green if it is not, and orange if the publication states that data is available at a given accession number, but no data is publicly available.


```r
# Plot which repositories are used to store metabolomics data

# Extract studies that store metabolomics data in a repository
PrimaryStudies$Repository[PrimaryStudies$Repository == "" ] <- NA
Repositories <- PrimaryStudies[!is.na(PrimaryStudies$Repository),]

# Combine repositories and raw data information for plotting to look at the interaction
Repositories$RepRawdata <- paste(Repositories$Repository, "-", Repositories$Raw_data)

# Create a table showing the number of studies with each combination
RawdataRep <- as.data.frame(table(Repositories$RepRawdata))
# split columns by "-"
RawdataRepsplit <- cbind(data.frame(do.call('rbind', strsplit(as.character(RawdataRep$Var1), '-', fixed=TRUE))), RawdataRep[,2])
colnames(RawdataRepsplit) <- c( "Repository", "Raw Data", "Frequency")

# Alter spaces for plotting
RawdataRepsplit$Repository<- gsub(" $","", RawdataRepsplit$Repository, perl=T)
RawdataRepsplit$Repository <- as.factor(RawdataRepsplit$Repository)
levels(RawdataRepsplit$Repository) <- gsub(" ", "\n", levels(RawdataRepsplit$Repository))

# Plot data
ggplot(RawdataRepsplit, aes(Repository, Frequency, fill = `Raw Data`))  + 
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  scale_y_continuous(limits = c(0,15), expand = c(0,0)) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = "black"),
    #axis.text.x  = element_text(angle=45),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    # Remove gridlines and borders
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) +
  scale_fill_manual(values = c("#66c2a5","#8da0cb","#fc8d62"), labels = c("No", "Yes", "Missing"))
```

<img src="figs/repositories-1.png" style="display: block; margin: auto;" />
