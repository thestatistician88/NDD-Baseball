# Nested Dirichlet Analysis for MLB Plate Appearance Data
Last update 09-22-2024

Files included:

batters_by_year.txt: outcomes from plate appearances for all MLB batters from 2000-2010

baseball_cleaning.R: upload batters data and clean. Creates several .csv files for later use.

baseball_analysis.R: does the main analysis

estimateParams.R, treeFinder.R, and getHeatmap.R are helper functions called by baseball_analysis.R to estimate NDD parameters, create a tree structure, and obtain correlations among components from a heatmap, respectively.

LRTresults, pairwiseByAgeResults, and treesByYear give results from running R code.
