# Machine Learning Project

The current zip contains our work for the machine learning project, with topic 
"Exoplanets Habitability".  The contents are organized as following:

## Data/

Folder containing the original dataset, along with the data splits generated for 
our analyses.

## Scripts/

Folder, containing all the R scripts that we used for our work and can be used 
for reproducing our results. Specifically:

- 01-pre-processing.r  ----> script containing all the pre-processed steps followed 
                             prior to the modeling. The scrips contain in comments 
                             the lines that need to be changed in order to generate 
                             the different data splits used in our analyses (training/test, 
                             original/oversampled, numerical/categorical). The final data
                             splits are saved in the fodler data/.


- 02-model-selection.r ----> script containg the modelling process for all the methods 
							 used for our analyses. In order to reproduce the results, 
							 we mention in comments which lines to modify to use the 
							 corresponding data split.

- 03-model-test.r      ----> script containing all the final steps of our analyses, using
							 the test splits of each corresponding data split.
