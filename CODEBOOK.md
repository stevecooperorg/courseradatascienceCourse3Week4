==================================================================
Tidy version of Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Steve Cooper
==================================================================

In this data, analysis, the Human Activity Recognition Using Smartphones Dataset was analysed and summarised.[1]

The raw data records experiments carried out with a group of 30 volunteers. Each performed six activities wearing a smartphone, and the embedded accelerometer and gyroscope recorded multiple readings for each recorded activity. The data was then split into a test set, and a training set.

In this analysis, the test and training sets were recombined, and the accelerometer data was summarised by calculating mean and standard deviations of each of the 9 smartphone variables provided. 

There are two outputs of the analysis. The first set summarises the smartphone readings for each activity, resulting in the same number of observations as the original data but with summary statistics. The second set produces an aggregation by subject and type of activity of those same variables.

In the first set, for each record it is provided:
=================================================

- An identifier of the subject who carried out the experiment.
- A label of the type of activity.
- Arithmetic Mean and Standard Deviation of the total acceleration, body, acceleration, and angular velocity measured during each activity.

In the second set, for each record it is provided:
=========================================

- An identifier for each subject in the experiment.
- A label for each type of activity.
- Arithmetic Mean and Standard Deviation of the total acceleration, body, acceleration, and angular velocity, aggregated across all activities.

Files
=====

- out/firstSet.csv - the first data set
- out/secondSet.csv - the second data set
- run_analysis.R - the analysis script.
- README.md - the detailed description of how the tidying was performed.
- CODEBOOK.md - this file.

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

