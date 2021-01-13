#!/bin/bash

# PIPELINE TO REPLICATE RESULTS

echo "(1) PROCESS THE KRAMER DATA"
Rscript process_kramer.R
# outputs: df_agg_pop.csv, df_pop.csv

echo "(2.A) PROCESS THE DA files"
Rscript process_DAs.R
# outputs: df_PC.csv, df_DA.csv

echo "(2.B) PROCESS GEOGRAPHIES"
Rscript process_geo.R
# outputs: 

echo "(3.A) RUN STATISTICAL ANALYSIS"
#Rscript run_analysis.R

echo "(3.B) RUN GEO ANALYSIS"
Rscript run_geo.R


echo "----- END OF pipeline.sh ------"
