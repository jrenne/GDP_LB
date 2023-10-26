
========================================================
 TAMING DEBT: CAN GDP-LINKED BONDS DO THE TRICK?

 Sarah Mouabbi, Jean-Paul Renne, Jean-Guillaume Sahuc
 
 This version: May 2020.
========================================================


The codes attached to this present README file allow to replicate results from the above-cited paper.



-----------------------------------------------------------

A - Requirements

The paper's results have been generated under R version 3.5.1 (2018-07-02) -- "Feather Spray".

To use the codes, a first step is to source "main.R" and to change the path after
"# Set working directory:" (line 20)

Note that some libraries are required to generate the different results. Make sure the libraries called in "main.R" are available before running the codes.


-----------------------------------------------------------

B - The different folders

Once the zipped folder has been decompressed, the main folder contains the following sub-folders:

1- data (contains csv data files used at the estimation/calibration stage)

2- estimation (contains R files used to estimate/calibrate the macro-finance models, also contains, in subfolder "results" the previously-estimated parameterizations)

3- prepare_outputs (contains scripts producing figures and tables, also contains these figures and tables)

4- procedures (contains libraries of procedures)

5- simulations (contains scripts operating the debt simulations)

In addition to the present README file, the main folder also contains two files "main.R" that has to be sourced in order to replicate the paper's results.


-----------------------------------------------------------

C - Outputs

Figures are stored in "prepare_outputs/PDF_Figures/". Tables are stored in "prepare_outputs/Tables/".


