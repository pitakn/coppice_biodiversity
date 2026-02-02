##################################################################################
This README file was generated on [2025-09-18] by [KATIE PITA]
1.0 GENERAL INFORMATION

1.1 Title of dataset: Rstudio raw code for manuscript "Traditional woodland management alters understory floral biodiversity in East Sussex, United Kingdom"

1.2. Author information:

1.2.1 Author/Principal Investigator Information
Name: Katie Pita Institution: University of Waterloo Faculty: Environment Email: kpita@uwaterloo.ca ORCID ID: 0009-0008-1305-6261

1.2.2. Author/Co-investigator/Contributor Information Name: Simon Coppard Institution: University of Greenwich Email: simon.coppaard@greenwich.ac.uk ORCID ID: 0000-0002-8930-2923

1.2.3 Author/Contributor Information Name: Andrew Trant Institution: University of Waterloo Faculty: Environment Email: atrant@uwaterloo.ca 

1.3 Data collection date(s): 2023-11-01 to 2025-09-18
2.0 DATA AND FILE OVERVIEW

2.1 Software Used: RStudio

2.2 File Naming Convention: NA

2.3 Relationship between files: 

2023biov.R and 2024biodiv.R contain first organizational code and computation of biodiversity metrics. These use .csv files biodiv_final_2023.csv and 2024_bd_clean.csv. 

biodiv_visualization.R contains code creating scatter and box plots and uses data in indices_no_ccf.csv. 

canopyclosure.R contains computation of canopy closure fraction from ImageJ pixel counts and uses data from canopyclosure.csv. 

modelling.R contains all regressions and GAMs undertaken in this study and uses data from indices_of_biodiversity_and_ccf_final.csv.

3.0 ACCESS, USAGE, SHARING, AND INTELLECTUAL PROPERTY INFORMATION

3.1 Data licenses and restrictions: All work is not yet peer-reviewed and is still in preparation. Not for reproduction or repurposing of any kind. This code is protected under a 1-year copyright embargo under K. Pita via the University of Waterloo (expires Dec. 5 2026).

3.2 Related publications, outputs, and datasets: Woodland ecosystem services of the past and present in Herstmonceux and south England. K Pita. Thesis.

3.2.1. Publications’ Data Retention Policy information: This work is currently unpublished and has not been peer-reviewed. Not for reproduction or reuse.

3.3 Recommended citation: Pita K. 2025. Rstudio raw code for manuscript "Traditional woodland management alters understory floral biodiversity in East Sussex, United Kingdom" Code in Pita K. Woodland ecosystem services of the past and present in Herstmonceux and south England [Thesis]. University of Waterloo: Waterloo (ON).

4.0 METHODOLOGY

4.1 Methods for collecting/creating data: Data were collected via vegetation sampling protocol (see Puric-Mladenovic and Kenney 2019) and digital photography of coppice wood canopy (see Chiannucci 2020) at Herstmonceux Castle Estate, East Sussex, UK.

4.2 Methods for processing data: Data was cleaned in RStudio and Microsoft Excel.

5.0 DATA-SPECIFIC INFORMATION

5.1 Variable list: character: plotid/cant, plantid, abandonment (y/n), vegetative layer (ground/shrub); numeric: Shannon biodiversity index, Simpson biodiversity index, Pielou's index of evenness, species richness, canopy closure fraction, tree age, year (2023/2024)

5.2 Units of measurement: All are unitless except tree age (years)
