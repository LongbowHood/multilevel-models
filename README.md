# Multilevel Models - Asian Children Growth Trajectories

**Authors**: Maria Levina, Rahul Vishwkarma

## Overview

This project explores an alternative method to analyze longitudinal data using the multilevel modeling framework, which overcomes many limitations of traditional approaches like Repeated Measures ANOVA. We analyze a dataset from the Centre for Multilevel Modelling (University of Bristol), describing the weight gains of 568 Asian children in a British community over several weeks after birth. The children were measured on up to five occasions visiting a clinic roughly at 6 weeks, 8 weeks, 12 weeks, and 27 months.

## Research Questions

We fit different multilevel models of increasing complexity to answer the following research questions: 1. How children’s weight changes in the early stages of life? 2. Whether and how those growth patterns differ between individuals? 3. Are there any differences in development between children of different gender? 4. Is there any influence of birth weight on post-natal development?

## Data Provenance

- **Source**: `ASIAN.DAT`
- **Observations**: Weight measurements across 5 timepoints. Missing values originally coded as -1.
- **Variables**:
  - `ChildID`: Unique identifier (2-5020)
  - `Age`: Measured in days
  - `Weight`: Measured in grams
  - `Birthweight`: Measured in grams (baseline covariate)
  - `Gender`: Boy=1; Girl=2

## Repository Structure

Following the reproducible scientific computing standard, the project is structured as follows:

``` text
.
├── data/
│   ├── raw_data/          # Immutable raw datasets (ASIAN.DAT)
│   └── processed_data/    # Cached parsed CSV datasets
├── R/                     # Reusable modules
│   ├── data_prep.R        # Parsing and standardizing data
│   └── models/            # LME4 hierarchical model specifications
├── scripts/               
│   └── run_analysis.R     # Main execution script
├── output/                # Generated results (git-ignored)
│   ├── figures/           # Rendered model trajectory plots
│   └── models/            # Saved model summaries and comparisons
├── manuscript/            # Final RMarkdown report and presentations
└── references/            # Background literature and assignment plans
```

## Prerequisites & Installation

Ensure you have R installed along with the following packages:

``` r
install.packages(c("lme4", "lmerTest", "ggplot2"))
```

## Usage

To guarantee computational reproducibility, the entire pipeline is automated. From the root directory, simply run:

``` bash
Rscript scripts/run_analysis.R
```

1.  This will parse the fixed-width text data from `data/raw_data/` and cache a structured dataframe into `data/processed_data/`.
2.  It will sequentially fit a series of hierarchical models of increasing complexity (from random intercepts to random quadratic slopes).
3.  The resulting trajectory plots for sampled children will be exported to `output/figures/` and ANOVA comparisons to `output/models/`.

## References

- Goldstein, H. (1986). Efficient statistical modelling of longitudinal data. *Ann. Human Biology*, 13, 129-42.
- Goldstein, H. (1987). *Multilevel Models in Educational and Social Research*. London, Griffin: New York, Oxford University Press.
- Prosser, R., Rasbash, J. and Goldstein, H. (1991). *ML3 Software for Three-level Analysis, Users' Guide for V.2*. Institute of Education, University of London.
