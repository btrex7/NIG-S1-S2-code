# NIG-S1-S2-R-Codes-and-Syntax-for-NGX-Listed-Companies-on-SDQ-and-CDQ

Bernard Asanbe (2026)

[DOI](https://doi.org/10.5281/zenodo.22062784)

Companion data repository: NIG-S1-S2: A Dataset for NGX-Listed Companies on SDQ and CDQ — [https://doi.org/10.5281/zenodo.22062525]

---

## Abstract

This repository contains the SPSS syntax and R scripts used to analyze the firm-year panel dataset associated with the Master's thesis: "The Impact of Sustainability Disclosure Quality on Firm Value: Evidence from NGX-Listed Companies Under the IFRS S1/S2 Reporting Framework." Four analysis objectives are covered: the level and trend of disclosure quality (SPSS), the relationship between Sustainability Disclosure Quality (SDQ) and firm value (R), the relationship between Climate-related Disclosure Quality (CDQ) and firm value (R), and a group comparison of early versus non-early IFRS S1/S2 adopters (R). Running the code in this repository against the companion dataset reproduces every statistic reported in the thesis from the original inputs.

## Background & Summary

The companion dataset scores NGX-listed firms' public disclosures against a content-analysis checklist built from IFRS S1 and IFRS S2, alongside the financial data needed to compute Tobin's Q and standard firm-level controls. This repository is the analysis layer on top of that data: panel regression code testing whether disclosure quality predicts firm value, a group-comparison test of whether firms that adopted IFRS S1/S2 early already disclose more than firms that haven't, and the SPSS syntax describing the disclosure data itself. Keeping the code separate from the data lets each analysis be re-run against a corrected or extended version of the dataset without re-publishing the whole project, and lets the code be reused on a comparable dataset from another market.

## Methods

### Code Development

- SPSS syntax (`.sps`) written and run in IBM SPSS Statistics v30, covering data preparation, descriptive statistics, correlation, a firm fixed-effects trend regression, robustness checks, and chart data for Objective 1.
- R scripts (`.R`) written and run under R 4.5.1, one per remaining objective, each self-contained: it installs any package it needs that isn't already present, reads the raw dataset directly, and writes its own CSV/PNG outputs plus a consolidated Excel workbook via `openxlsx`. No script depends on another script's output.
- Two Objective 1 figure inputs (an exact Shapiro-Wilk statistic and a percentile bootstrap confidence interval) were computed in Python/SciPy rather than SPSS, because SPSS's own bootstrap-to-chart pipeline requires a two-pass manual build; the Python code for both is included below.

## Code Description

- **Script 1: `01_Data_Preparation.sps`** — imports the dataset's `Final Sample` sheet, renames all 26 columns by position, builds a provisional-row flag, winsorizes Tobin's Q/Leverage/ROA at the 0.5th/99.5th percentile, and saves `NGX_Panel_Analysis.sav` for every later SPSS script to read.
- **Script 2: `02_Descriptive_Statistics.sps`** — sample profile, SDQ/CDQ descriptives and normality, trend by year, sector breakdown, zero-disclosure prevalence. Requires the Custom Tables add-on module (`CTABLES`).
- **Script 3: `03_Correlation.sps`** — Pearson and Spearman correlation matrices.
- **Script 4: `04_Regression_Base_Model.sps`** — firm fixed-effects trend regression testing whether SDQ/CDQ improved over 2021-2025. Requires the Advanced Statistics add-on module (`GENLIN`).
- **Script 5: `05_Robustness_Tests.sps`** — firm-level Kruskal-Wallis test by sector, and a sensitivity check re-running the headline statistics with provisional-score firm-years excluded.
- **Script 6: `06_Charts.sps`** — trend chart data: a normal-theory 95% CI computed entirely in SPSS, and the bootstrap 95% CI actually used in the thesis figure, read off `BOOTSTRAP`/`EXAMINE` output. Requires the Bootstrapping add-on module (`BOOTSTRAP`).
- **Script 7: `objective2_analysis.R`** — SDQ and firm value: panel fixed-effects vs. random-effects model on Tobin's Q, Hausman test, correlation/VIF/Breusch-Pagan/Breusch-Godfrey diagnostics, four robustness checks, two extended checks (two-way fixed effects, one-year-lagged SDQ), two figures, and a consolidated workbook.
- **Script 8: `objective3_analysis.R`** — same design as Script 7, with CDQ in place of SDQ.
- **Script 9: `objective4_analysis.R`** — early adopters vs. others: Shapiro-Wilk and Levene's test on SDQ by adoption-status group, Mann-Whitney U (primary test) and Welch's t-test (secondary) with effect sizes, plus firm-level and year-restricted robustness checks.

## Usage Notes

**Software and packages.** Objective 1 needs IBM SPSS Statistics (v30 used originally) with the Custom Tables, Advanced Statistics and Bootstrapping add-on modules. Objectives 2-4 need R (4.5.1 used originally; any recent R 4.x works) with `readxl`, `plm`, `lmtest`, `sandwich`, `car`, `ggplot2`, `patchwork`, `scales` (Objectives 2 and 3), `readxl`, `dplyr`, `effectsize`, `car`, `ggplot2` (Objective 4), and `openxlsx` (all three, for the consolidated workbook step). An optional Python 3 environment with `pandas`, `numpy`, `scipy` is needed only to reproduce the two Objective 1 figure inputs described above.

**Folder layout.** Put all six `.sps` files and the dataset in one folder for Objective 1. For Objectives 2-4, use:

```
your-folder/
├── objective2_analysis.R   (or objective3_analysis.R / objective4_analysis.R)
├── data/
│   └── NGX_Sustainability_Panel_Dataset_UPDATED.xlsx
└── outputs/                (created automatically)
```

**SPSS run order**, in the same session, after setting the working directory to the folder above (**File → Change Working Directory**, or a `CD 'path'.` line at the top — every script uses bare filenames):

1. `01_Data_Preparation.sps`
2. `02_Descriptive_Statistics.sps`
3. `03_Correlation.sps`
4. `04_Regression_Base_Model.sps`
5. `05_Robustness_Tests.sps`
6. `06_Charts.sps`

**R run**, from within the folder shown above (in R: **Session → Set Working Directory → To Source File Location**, then confirm `getwd()` and `file.exists("data/NGX_Sustainability_Panel_Dataset_UPDATED.xlsx")` both check out before running anything):

```
Rscript objective2_analysis.R
Rscript objective3_analysis.R
Rscript objective4_analysis.R
```

Each is independent and can be run in any order.

## Example workflow

1. Place the companion dataset (`NGX_Sustainability_Panel_Dataset_UPDATED.xlsx`) alongside the six `.sps` files, and separately inside a `data/` subfolder next to each `objectiveN_analysis.R`.
2. Run the six SPSS scripts in order for Objective 1; check the descriptive, correlation and trend-regression output against the expected values below.
3. Run the three R scripts (any order) for Objectives 2-4; each writes its own `outputs/objectiveN/` folder, ending with `objectiveN_consolidated_workbook.xlsx`.
4. Optionally run the Python code below to reproduce the two figure inputs SPSS doesn't compute natively.
5. Compare every statistic produced against the expected-results tables below to confirm an exact (or, where noted, closely matching) reproduction.

### Objective 1 expected results

| Check | Expected value |
|---|---|
| Winsorization cutoffs (0.5th/99.5th pct) | Tobin's Q 0.43/11.33; Leverage 0.04/3.35; ROA -0.70/0.52 |
| Zero-disclosure prevalence | SDQ 55.65% (340/611); CDQ 82.65% (505/611) |
| Firm-level Kruskal-Wallis | SDQ H=23.84, df=12, p=.021; CDQ H=17.87, p=.120 |
| Firm fixed-effects trend | SDQ b=1.479, within R²=.038; CDQ b=1.2863, within R²=.046 |
| Correlation (SDQ, CDQ) | Pearson r=.828; Spearman rho=.678 |
| Correlation (index, Firm_Size) | Pearson .535 (SDQ) / .508 (CDQ); Spearman .505 / .408 |
| Provisional-row sensitivity (n=603 vs. 611) | SDQ mean 13.151→13.284; CDQ mean 6.370→6.455 |
| Shapiro-Wilk (Python) | SDQ W=.660, p<.001; CDQ W=.407, p<.001 |

Bootstrap 95% CI by year (Python, percentile bootstrap, 10,000 resamples, seed=42):

| Year | SDQ mean | SDQ 95% CI | CDQ mean | CDQ 95% CI |
|---|---|---|---|---|
| 2021 | 9.62 | [6.74, 12.71] | 3.33 | [1.41, 5.76] |
| 2022 | 12.38 | [8.81, 16.11] | 5.36 | [2.79, 8.42] |
| 2023 | 12.87 | [9.10, 17.01] | 7.16 | [3.77, 10.87] |
| 2024 | 15.83 | [11.63, 20.23] | 7.93 | [4.55, 11.77] |
| 2025 | 14.86 | [10.75, 19.21] | 7.91 | [4.67, 11.51] |

```python
import pandas as pd, numpy as np
from scipy import stats

df = pd.read_excel("NGX_Sustainability_Panel_Dataset_UPDATED.xlsx", sheet_name="Final Sample")

w_sdq, p_sdq = stats.shapiro(df["SDQ_Score (0-100)"].dropna())
w_cdq, p_cdq = stats.shapiro(df["CDQ_Score (0-100)"].dropna())
print(f"SDQ Shapiro-Wilk: W={w_sdq:.3f}, p={p_sdq:.3g}")
print(f"CDQ Shapiro-Wilk: W={w_cdq:.3f}, p={p_cdq:.3g}")

def boot_ci(x, n_boot=10000, seed=42):
    rng = np.random.default_rng(seed)
    x = np.asarray(x)
    means = np.array([rng.choice(x, size=len(x), replace=True).mean() for _ in range(n_boot)])
    lo, hi = np.percentile(means, [2.5, 97.5])
    return x.mean(), lo, hi

for yr in [2021, 2022, 2023, 2024, 2025]:
    s = df.loc[df.Year == yr, "SDQ_Score (0-100)"].dropna().values
    c = df.loc[df.Year == yr, "CDQ_Score (0-100)"].dropna().values
    print(yr, boot_ci(s), boot_ci(c))
```

The means reproduce exactly; CI bounds may differ from the table above by a few hundredths of a point depending on your NumPy version, since bootstrap resampling with a fixed seed is sensitive to the exact call sequence, not just the seed number.

### Objective 2 expected results (`objective2_analysis.R`)

| Check | Expected value |
|---|---|
| Model fit | R²=0.1671, Adj R²=-0.0606, N=611 firm-years / 128 firms |
| Hausman test | χ²(4)=83.22, p<.001 (Fixed Effects selected) |
| SDQ coefficient on Tobin's Q | Base FE b=-0.0067, p=.046; Two-way FE b=-0.0059, p=.081; Lagged (t-1) b=-0.0072, p=.037 |
| Firm_Age coefficient | ≈0.28-0.32 across all three specifications, p<.001 |
| VIF | 1.426 / 1.502 / 1.002 / 1.072 |
| Breusch-Pagan | 13.13, p=.011 |
| Breusch-Godfrey/Wooldridge | 15.74, p<.001 |
| Extended F-test (joint year effects) | F(3,476)=4.07, p=.007 |

### Objective 3 expected results (`objective3_analysis.R`)

| Check | Expected value |
|---|---|
| Model fit | R²=0.160, Adj R²=-0.070, N=611 firm-years / 128 firms |
| Hausman test | χ²(4)=120.59, p<.001 (Fixed Effects selected) |
| CDQ coefficient on Tobin's Q | Base FE p=.144; Two-way FE p=.520; Lagged (t-1) p=.186 — not significant under any specification |
| VIF | 1.359 / 1.432 / 1.001 / 1.063 |
| Breusch-Pagan | 13.37, p=.010 |
| Extended F-test (joint year effects) | F(3,476)=4.29, p=.005 |

### Objective 4 expected results (`objective4_analysis.R`)

| Check | Expected value |
|---|---|
| Descriptives | Adopters mean SDQ 67.88 (n=22); non-adopters 11.11 (n=589) |
| Shapiro-Wilk | Adopters W=.853, p=.004; non-adopters W=.648, p<.001 |
| Levene's test | F=3.14, p=.077 |
| Mann-Whitney U (primary) | U=696.5, p<.001, rank-biserial r=.89 |
| Welch t-test (secondary) | t=10.25, df=21.9, p≈8.2e-10, Cohen's d=2.91 |
| Robustness: firm level (n=128) | U=120.5 |
| Robustness: 2023-2025 only (n=371) | U=430.5 |

The rank-biserial sign depends on which group R treats as the reference level — magnitude (.89) is what to check; adopters rank higher either way.

Each of the three R scripts finishes by assembling its own CSV/PNG output into one `objectiveN_consolidated_workbook.xlsx` via `openxlsx` — nothing extra to run, it's produced automatically as the last step of the script.

## Limitations

- The Hausman statistic, the Breusch-Godfrey statistic, and the two-way fixed-effects model's clustered standard errors in Objectives 2 and 3 are the one place in this codebase where results can shift slightly with the installed `plm` package version; the coefficients themselves and every substantive conclusion do not change. A Hausman χ² for Objective 3 landing near 121-124 rather than exactly 120.59 reflects this, not a script error.
- No saved script produces the two Objective 1 figure inputs (exact Shapiro-Wilk statistic, bootstrap CI) inside SPSS itself; the Python code above is the documented substitute, and SPSS's own `EXAMINE`/`BOOTSTRAP` output is a close but not bit-identical alternative.
- All three R scripts assume the companion dataset's `Final Sample` sheet and column headers are unchanged; a renamed or restructured column will break the script at the point it's read rather than silently producing wrong numbers.

## Code & Data Availability

All files are available on Zenodo [https://doi.org/10.5281/zenodo.22062784].
GitHub repository: [https://github.com/btrex7/NIG-S1-S2-code] (placeholder).
Companion data repository: [https://doi.org/10.5281/zenodo.22062525].
## Citation

If using this code, please cite as:

Asanbe, B. (2026). *NIG-S1-S2: R Codes and Syntax for NGX-Listed Companies on SDQ and CDQ*. Zenodo. DOI: [https://doi.org/10.5281/zenodo.22062784].

## License

Code is released under the MIT License.

## Reference

Petersen, M. A. (2009). Estimating standard errors in finance panel data sets: Comparing approaches. *Review of Financial Studies*, 22(1), 435-480.

International Sustainability Standards Board. (2023a). *IFRS S1 general requirements for disclosure of sustainability-related financial information*. IFRS Foundation.

International Sustainability Standards Board. (2023b). *IFRS S2 climate-related disclosures*. IFRS Foundation.
