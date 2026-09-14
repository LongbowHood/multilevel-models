# Data Dictionary: Asian Children Growth Trajectories

## Dataset Overview
The dataset contains longitudinal weight gains of 568 Asian children in a British community over several weeks after birth. The data is stored in `data/raw_data/ASIAN.DAT` in a fixed-width format. Missing values were originally coded as `-1`.

## Variables

| Variable Name | Type | Description |
| :--- | :--- | :--- |
| `ChildID` | Categorical | Unique identifier for each child (values range from 2 to 5020). |
| `Age` | Numeric | Age of the child at the time of the clinic visit, measured in days. |
| `Weight` | Numeric | Weight of the child at the time of the clinic visit, measured in grams. |
| `Birthweight` | Numeric | The child's weight at birth (baseline covariate), measured in grams. |
| `Gender` | Categorical | Sex of the child. Originally encoded as `1` for Boy and `2` for Girl. |

## Processed Data Variables
In the processed dataset (`cleaned_data_wide.csv` and `cleaned_data_long.csv`), several derived and rescaled variables are added for modeling:

| Variable Name | Type | Description |
| :--- | :--- | :--- |
| `NObs` | Numeric | Total number of observations recorded for the given `ChildID`. |
| `GenderID` | Numeric | Dummy-encoded gender (`0` = Boy, `1` = Girl). |
| `r_Weight` | Numeric | Rescaled weight in kilograms (`Weight / 1000`). |
| `r_Birthweight` | Numeric | Rescaled birth weight in kilograms (`Birthweight / 1000`). |
| `r_Age_weeks` | Numeric | Rescaled age in weeks (`Age / 7`). |
| `r_Age_years` | Numeric | Rescaled age in years (`Age / 365`). |
| `c_Age_years` | Numeric | Mean-centered age in years. |
| `c_Weight_kg` | Numeric | Mean-centered weight in kilograms. |
| `c_Birthweight_kg` | Numeric | Mean-centered birth weight in kilograms. |
