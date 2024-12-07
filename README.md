This is a repository for the Urostatical Solutions project. This directory contains the necessary files to tun AI4HealthyAging risk calculator in R studio.

The directory contains three files, the app  R code to run in R studio and two files, the first one provide the XGBoost model in which the risk calculator is based and the second one, render the ROC object to estimate clinical utility curves.

The calculator will offer you the probabilities of Prostate cancer (PCa), and results will be proposed as favorable (green) or unfavorable (red) after applying the Threshold probability selected. Using the threshold point, the PCa wrongly classified and biopsied avoided below the cutoff point are displayed in clinical utility curve for all data.
