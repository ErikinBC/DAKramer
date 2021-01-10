#!/bin/bash

# PIPELINE TO REPLICATE RESULTS

echo "(1) PROCESS THE KRAMER DATA"
Rscript process_kramer.R
# outputs: df_agg_pop.csv, df_pop.csv

echo "(2) PROCESS THE DA files"
Rscript process_DAs.R


echo "----- END OF pipeline.sh ------"
