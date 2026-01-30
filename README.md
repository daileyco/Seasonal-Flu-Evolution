# Seasonal Flu Evolution Repo

Genomic epidemiology approaches are increasingly common in influenza surveillance, offering high-resolution insights into transmission patterns. By analyzing genomic data and reconstructing pathogen ancestry through phylogenetic methods, researchers can uncover transmission dynamics that traditional case-based approaches might fail to capture. Particularly, phylogenetic trees can shed light on the underlying transmission dynamics in an outbreak. With the accumulation of genomic data, it may now be possible to systematically characterize spatial variation in transmission dynamics of seasonal influenza in the United States. 
Here, we explore the phylogenetic signals of local influenza outbreaks across the US from 2010-2020, focusing on type A (H3 and H1 subtypes) and type B (Victoria and Yamagata lineages) influenza viruses.

This repo relates to Chapter 4 in [my PhD dissertation](https://esploro.libs.uga.edu/esploro/outputs/9949694128302959).

Feel free to reach out to me (daileyco@gmail.com) or my PhD advisor, Justin Bahl (Justin.Bahl@uga.edu), with any questions. 

## Repo Contents

This repo contains scripts that: 

- read and manage input data, 
- reconstruct influenza phylogenetic trees,
- identify, summarize, and correlate the phylogenetic signals among local transmission clusters, 
- and generate various tables and figures showing important patterns in the data or analytical results.

***NOTE: this repo was a work in progress at last update, so scripts may be broken***

The scripts in this repo (and others of my creation) are highly modular. The scripts are designed to be run in a particular sequence that ensures the output(s) saved from upstream scripts are available for input(s) in downstream scripts. (See the bottom of this readme for a generic description of repo contents/structure.)

There are two files that outline the order of scripts and give details on their individual purposes. 
- "00-Information/script_census-[compile date].xlsx"
- "04-Report/01-Notebook/reproducibility_notebook.rmd"

The script census excel file gives details on scripts in this repo, including its purpose, inputs, package dependencies, and outputs. The creation of this excel file was automated (".02-Scripts/Script_Census.R"), so there are likely some errors in formatting or omitted information. Each script itself has some comments explaining the intent for sections of the code. 
The reproducibility notebook is a combination of (1) a narrative explaining analysis steps and (2) a master script which sources/runs all of the main analysis scripts.

A few of the columns from the script census are shown in the table below. (created with knitr::kable(census[,c(2,1,3)]))


|Script Location     |Script Name                                  |Purpose                                                                                                                                              |
|:-------------------|:--------------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------|
|01-Data-Wrangling   |process_Data_ACS.R                           |ACS Data Processing                                                                                                                                  |
|01-Data-Wrangling   |process_Data_Spatial.R                       |Spatial Data Processing                                                                                                                              |
|01-Data-Wrangling   |process_Data_GISAID.R                        |script to process metadata                                                                                                                           |
|01-Data-Wrangling   |prep_Files_FASTA.R                           |Script to write fasta files for each subtype                                                                                                         |
|01-Data-Wrangling   |process_Data_Alignments.R                    |Script to write fasta files                                                                                                                          |
|01-Data-Wrangling   |prep_Files_FASTA2alt.R                       |Script to write fasta files                                                                                                                          |
|04-Analysis         |reconstruct_Phylogenies_with_IQTREEalt.R     |script to iteratively call iqtree from command line                                                                                                  |
|01-Data-Wrangling   |process_Data_IQTREEalt.R                     |script to read output files of iqtree into an r df                                                                                                   |
|01-Data-Wrangling   |process_Data_Timetrees.R                     |script to partition full phylogenetic trees based on estimated date of internal nodes to identify subtrees whose mrca falls within collection season |
|02-Helper-Functions |root_Tree.R                                  |function to root tree                                                                                                                                |
|02-Helper-Functions |summarize_Tree.R                             |function to calculate summary statistics for trees                                                                                                   |
|01-Data-Wrangling   |generate_Tree_Summaries2.R                   |script to generate tree summaries data frame                                                                                                         |
|01-Data-Wrangling   |process_Data_Tree_Summaries.R                |script to manage the tree summary data                                                                                                               |
|03-Visualization    |generate_Tables_Tree_Summaries.R             |generate summary tables                                                                                                                              |
|03-Visualization    |generate_Flextables_SummaryTables.R          |script to format summary tables and save to word doc                                                                                                 |
|03-Visualization    |generate_Figures_TimeSeries.R                |generate figures showing MPD over time                                                                                                               |
|03-Visualization    |generate_Figures_TimeScalingBoxplots.R       |generate boxplots for mutation rates and ancestor dates                                                                                              |
|03-Visualization    |generate_Figure_Coverage_Heatmap.R           |Figure heatmap showing proportion of cumulative seasonal ILI by week and state                                                                       |
|03-Visualization    |generate_Figure_Coverage_Heatmap_Sequences.R |Figure heatmap                                                                                                                                       |
|03-Visualization    |generate_Figure_Coverage_Heatmap_TreeStats.R |Figure heatmap showing proportion of cumulative seasonal ILI by week and state                                                                       |
|02-Helper-Functions |generate_Spatial_Lag_Variable.R              |calculate the average value among spatial neighbors                                                                                                  |
|04-Analysis         |correlate_Smalltrees.R                       |script to compute correlations using small trees data                                                                                                |
|04-Analysis         |correlate_Smalltrees_subset1520.R            |script to compute correlations using small trees data                                                                                                |
|03-Visualization    |generate_Figure_Cors_Heatmap.R               |Figure heatmap                                                                                                                                       |


# Project Skeleton (my generic repo template)

A basic repo template (directory structure) to be use as starting point for new projects.


  
# Structure
  
## 00-Information
  
Proposals, prompts, outlines, ... anything relevant to the conception of the project.
  
## 01-Data
  
Data in downloaded formats are "raw" (e.g., .xlsx, .dat, .csv, ...). We import and store data in R formats (e.g., .rds, .rdata) during "processing". Data cleaning and recoding finalize "analytic" datasets. *Note: generally, "00-" prefix means don't modify or rewrite the files within.*
  
* Sub-directories
  + 00-Raw-Data
  + 01-Processed-Data
  + 02-Analytic-Data

## 02-Scripts

Envisioned as a linear path from raw data management to final figure creation. Playground to catch everything incomplete; data wrangling to transition through data directories; helper functions to hold project specific functions or those oriented to external programs; visualization is obvious; analysis to fit models, calculate stats, general computation/programming. Each has its own typical set of script types.

* Sub-directories and typical scripts
  + 00-Playground
    - exploratory code dump, in-progress scripts, under construction, placeholder for other scripts
  + 01-Data-Wrangling
    - download_Data_\* : scripts to automate the data download from online
    - process_Data_\* : scripts to process raw data
    - prep_Files_\* : scripts to write files for external program use
    - process_Results_\* : scripts to process results from external program analyses
  + 02-Helper-Functions
    - repeated, modular, specific, useful code
    - run_Program_\* : scripts to automate external programs
  + 03-Visualization
    - plot_\* : pretty self-explanatory, fundamental units to generate_Figure scripts
    - generate_Figure_\* : scripts to create high-level figures, plots, images, interactive visualizations
    - generate_Table_\* : scripts to create tables
  + 04-Analysis
    - analyze_\* : scripts to run analyses within R

## 03-Output

Store the goodies to show everyone else. 

* Sub-directories
  + 01-Tables
  + 02-Figures
  + 03-Non-Static


## 04-Report

* Sub-directories
  + 01-Notebook (detailed reports, write-ups, long-winded, process-oriented)
    - reproducibility_notebook.RMD : quasi-stream-of-thought writing, deeply woven web of scripts and analytical directions
  + 02-Presentation (slide decks)
  + 03-Manuscript (polished for publication)

## 05-Miscellaneous

Whatever else may need to accompany repo.





# Useful Resources

[How to collaborate](https://www.sciencemag.org/careers/2012/07/how-collaborate)

[Ten simple rules for collaboratively writing a multi-authored paper](https://doi.org/10.1371/journal.pcbi.1006508)


[How to write a first-class paper](https://doi.org/10.1038/d41586-018-02404-4)

[Ten simple rules for structuring papers](https://doi.org/10.1371/journal.pcbi.1005619)

[Writing science](https://www.science.org/content/article/writing-science-storys-thing)


[Ten guidelines for effective data visualization in scientific publications](https://doi.org/10.1016/j.envsoft.2010.12.006)


[Three tips for giving a great research talk](https://www.science.org/content/article/three-tips-giving-great-research-talk)

[How to give a great scientific talk](https://doi.org/10.1038/d41586-018-07780-5)


[Science communication, public engagement, and outreach](https://www.informalscience.org/develop-projects/science-communication-public-engagement-and-outreach)

