
================================================================================
Debt-Stabilizing Properties of GDP-Linked Securities: A Macro-Finance Perspective

Sarah Mouabbi, Jean-Paul Renne, Jean-Guillaume Sahuc

December 2023
================================================================================


The codes attached to this present README file allow to replicate results from the above-cited paper.


--------------------------------------------------------------------------------

A - Requirements

The paper's results have been generated under R version 4.3.1 (2023-06-16) -- "Beagle Scouts".

The name of the R project is "GDP_LB"
To use the codes, run "main.R."

Note that some libraries are required to generate the different results. Make sure the libraries called in "main.R" are available before running the codes.


--------------------------------------------------------------------------------

B - The different folders

The different folders are as follows:

1- data (contains csv data files used at the estimation/calibration stage)

2- estimation (contains R files used to estimate/calibrate the macro-finance models, also contains, in subfolder "results" the previously-estimated parameterizations)

3- prepare_outputs (contains scripts producing figures and tables, also contains these figures and tables)

4- procedures (contains sets of procedures)

5- simulations (contains scripts operating the debt simulations)

In addition to the present README file, the main folder also contains two files "main.R" that has to be sourced in order to replicate the paper's results.


--------------------------------------------------------------------------------

C - Outputs

Figures are stored in "prepare_outputs/PDF_Figures/". Tables are stored in "prepare_outputs/Tables/".


