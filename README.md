# Concrete Mix Compressive Strength — Statistical Analysis in R

Statistical consulting analysis for a construction company testing concrete mixes.  
Goal: understand how **compressive strength** relates to mix composition and test specific hypotheses.

## Scenario
A range of concrete mixes is being evaluated. We analyse how ingredients (cement, slag, fly ash, water, superplasticizer, coarse and fine aggregates) and age relate to **compressive strength**, then test hypotheses relevant to the business question.

## Repository Structure
```
concrete-strength-r/
├── data/
│   ├── concrete compressive strength.xlsx   # Dataset
│   └── Concrete_Readme.txt                  # Dataset description/provenance
├── scripts/
│   ├── eda_regression.R                     # EDA, correlation, regression modelling
│   └── hypothesis_testing.R                 # Hypothesis testing (t-test, χ², two-way ANOVA, robust ANOVA)
├── reports/
│   └── report.pdf                           # Methodology, implementation and business-oriented interpretation and conclusions
├── LICENSE
└── README.md
```

## What’s Inside
- **EDA**: structure, missing/duplicates, summary stats, histograms, boxplots, pair plots, correlation matrix.
- **Correlation analysis** among numeric variables.
- **Regression**: simple and multiple linear regression; model selection; diagnostics (linearity, residual normality, homoscedasticity, VIF); interaction terms.
- **Hypothesis tests** (distinct from assumption checks):
  - Two-sample **t‑test** (coarse vs fine aggregate mean strength).
  - **Chi‑square** test (association between fly‑ash presence and aggregate category).
  - **Two‑way ANOVA** for strength by aggregate category × fly‑ash, plus **robust two‑way ANOVA** (WRS2) when assumptions fail.
- **Assumption checks**: Shapiro–Wilk, Levene’s test, residual/Q‑Q/scale‑location plots.
- **Report** in reports/report.pdf explaining methodology, statistical reasoning, results, and business-oriented interpretation and conclusions.

## Dataset
Concrete compressive strength data (1030 rows) with mix components and age; strength measured in MPa.  
Variables include cement, blast furnace slag, fly ash, water, superplasticizer, coarse/fine aggregates, **Age (days)**, concrete category (Coarse/Fine), **Contains Fly Ash**, and **Concrete compressive strength (MPa)**.

**Provenance**: dataset donated by **Prof. I‑Cheng Yeh**, Chung‑Hua University (original work on modelling strength of high‑performance concrete, 1998). See `data/Concrete_Readme.txt` for full attribution and variable descriptions.

## Getting Started
1. **Open in RStudio** and set the working directory to the repo root:
   ```r
   setwd("path/to/concrete-strength-r")
   ```
2. **Install packages** (first run only):
   ```r
   install.packages(c("readxl","ggplot2","corrplot","car","WRS2"))
   ```
3. **Run analyses**:
   - EDA, correlation & regression:
     ```r
     source("scripts/ra.R")
     ```
   - Hypothesis tests (t‑test, χ², two‑way ANOVA, robust ANOVA):
     ```r
     source("scripts/ht.R")
     ```

## Notes
- Scripts assume the Excel file is at `data/concrete compressive strength.xlsx`.
- Plots are created via **ggplot2** and base R; WRS2 is used for robust ANOVA when assumptions are violated.

## Authorship & Contact
Developed by **Jose Wong**  
j.wong@mail.com  
https://www.linkedin.com/in/jose-wongg  
https://github.com/JoseWongg

## License
MIT — see the [LICENSE](LICENSE) file for details.
