## Machine Learning Project
# Exoplanet habitability classification
### By Ricard Monge & Amalia Vradi
This is a repository for the final project of the Machine Learning course at Msc MIRI in UPC.

## Introduction

On March 2009 NASA launched the Kepler space telescope to discover planets orbiting other
stars in our galaxy. In particular, the focus was to discover earth-like or habitable planets that could potentially host liquid water and organic life as we know it on Earth. As of March 2019,Kepler has discovered 2349 exoplanets (confirmed) of different types, recording their properties and that of their solar systems.

The current concept of habitability relates to the exoplanet’s relative position to its star, that affects the surface temperature, and the host star’s type, age and composition, that affect itsemissions and the stability of the system. Such measures can be obtained from astronomic observational and modeled data, and could be used to predict a habitability score or
classification.

The Planetary Habitability Laboratory’s Exoplanets Catalog (PHL-EC) from the University of Puerto Rico ([here](http://phl.upr.edu/projects/habitable-exoplanets-catalog/data/database)) contains observed and modeled parameters for all confirmed exoplanets, as for July 2, 2018. The main difference between PHL-EC and other exoplanets databases is that it contains more estimated stellar and planetary parameters, habitability assessments with various
habitability metrics, planetary classifications, and many corrections. In particular, the database
contains a binary variable for its potential habitability and a habitability class for each exoplanet.

Our goal is to model classifiers for the different types of habitability classes in order to automatically detect the habitability of future exoplanets.

Previous work has been done on PHL-EC for habitability classification, for instance in:

* Basak, S., Agrawal, S., Saha, S., Theophilus, A., Bora, K., Deshpande, G., & Murthy, J. (2018). **Habitability Classification of Exoplanets: A Machine Learning Insight** ([here](https://arxiv.org/pdf/1805.08810.pdf)).

* Basak, S., Agrawal, S., Saha, S., Theophilus, A., Bora, K., Rosario-Franco, M. , Routh,
S. (2016). **A Comparative Study in Classification Methods of Exoplanets: Machine Learning Exploration via Mining and Automatic Labeling of the Habitability Catalog** ([here](https://www.researchgate.net/publication/309386054_A_Comparative_Study_in_Classification_Methods_of_Exoplanets_Machine_Learning_Exploration_via_Mining_and_Automatic_Labeling_of_the_Habitability_Catalog)).

* Baxi, Shreyas Shriram (2018). **Machine Learning Approach to Classify Transit Signals and Assessing the Exoplanets Probability for Habitability**. Masters thesis, Dublin, National College of Ireland ([here](http://trap.ncirl.ie/3436/)).


