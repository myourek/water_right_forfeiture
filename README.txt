To regenerate the analyses in this paper, you will need to have R software installed (https://www.r-project.org/). 
The packages ggplot2, gridExtra, grid, and sf will also need to be installed to reproduce the plots. 
This repository contains three scripts written in R computing language (v. 4.3.0). 
These are located in the R_scripts directory:

•	parent_child2.R is the primary script that assembles the water right genealogies and determines diminishment using various decision rules. 
    It contains several hundred, hard-coded instances of diminishment to account for the many instances in which the decision rules incorrectly identified diminishment. 
	As noted in the manuscript, there are many nuances in the way that water quantities are reported in WRTS that necessitate manual review of the scanned documents to make definitive determinations of forfeiture. 
•	load_parent_functions.R is a corollary to the first script, and contains all the functions (e.g., for calculating parent and child quantities) that are called by parent_child2.R. 
•	relinquishment_analysis.R contains the code used for generating all the tables and figures, and for executing the statistical analyses.

To reproduce the results found in this paper, run parent_child2.R and subsequently run relinquishment_analysis.R.

There are three processed data files in the repository. These are located in the cleaned_data directory: 

•	WRChanges.csv is the fully processed dataset for all change authorizations. It was compiled using the script, parent_child2.R. It contains all the child and parent water rights quantities, along with the attributes that were assessed in the manuscript. 
•	show-cause_filled.csv is the descriptive list of certificates of relinquishment in the WRTS database.
•	Yakima_adj.csv contains the pre-adjudication and post-adjudication surface water rights with filled water quantities that were part of the general adjudication of the entire Yakima basin (Acquavella). This file was used to calculate the aggregate forfeiture resulting from that case.
The remaining input files required for running the code are located in the input_data_files and input_maps directories. All these files are loaded by the parent_child2.R and relinquishment_analysis.R scripts.

