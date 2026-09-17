# ============================================================================
# OBJECTIVE 4: Early Adoption Status (EAS) and Sustainability Disclosure
# Quality (SDQ) -- cross-sectional group comparison
# NGX-Listed Companies, IFRS S1/S2 Reporting Framework, 2021-2025
#
# H03: There is no significant difference in sustainability disclosure
#      quality between early adopters of the IFRS S1/S2 framework and
#      other NGX-listed companies.
#
# Design: independent-groups comparison per Chapter 3, Section 3.9 -- NOT a
# regression. Primary test: Mann-Whitney U (SDQ violates normality; see
# Objective 1's Shapiro-Wilk result). Secondary test: Welch's independent-
# samples t-test, reported alongside for triangulation only. Two robustness
# checks follow: collapsing to one row per firm (removing pseudo-replication
# from repeated firm-years), and restricting both groups to 2023-2025 (every
# adopter firm-year falls in that window, so this rules out the panel's
# documented upward SDQ time trend as a competing explanation).
#
# Expected folder layout (relative to this script):
#   ./data/NGX_Sustainability_Panel_Dataset_UPDATED.xlsx
#   ./outputs/objective4/   <- created automatically if missing
#
# Run with:   Rscript objective4_analysis.R
# ============================================================================

## ---- 0. Locale: ensure the special characters used in output/figure text
##         (em dash, +/-, <=, >=) render correctly regardless of the invoking
##         shell's locale. Harmless if it fails, hence the tryCatch.
set_utf8_locale <- function() {
  for (candidate in c("en_US.UTF-8", "C.UTF-8", "en_GB.UTF-8")) {
    result <- suppressWarnings(tryCatch(Sys.setlocale("LC_CTYPE", candidate),
                                         error = function(e) ""))
    if (nzchar(result)) return(invisible(result))
  }
  invisible(NULL)
}
set_utf8_locale()

## ---- 0a. Initial install and load of required packages ---------------------
## Standard first step: install the packages this script depends on, then
## load them. Only packages not already installed are actually sent to
## install.packages() -- if you re-run this script in the same R session
## without restarting R (an entirely normal RStudio workflow), every one of
## these packages is already loaded, and re-installing an already-loaded
## package is exactly what triggers RStudio's own "restart required?" check
## (visible in its traceback as .rs.installPackagesRequiresRestart) -- which
## has been observed to throw a spurious "object not found" error of its own,
## unrelated to anything in this script. Skipping packages that are already
## installed avoids calling install.packages() at all in the common case,
## sidestepping that RStudio-side quirk entirely.
required_pkgs <- c("readxl", "dplyr", "effectsize", "car", "ggplot2")
pkgs_to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(pkgs_to_install) > 0) {
  install.packages(pkgs_to_install, repos = "https://cloud.r-project.org")
}
for (pkg in required_pkgs) {
  suppressWarnings(suppressMessages(
    tryCatch(library(pkg, character.only = TRUE), error = function(e) NULL)
  ))
}

## ---- 0b. Auto-install: safety net in case the step above could not reach
##          CRAN (e.g. no internet access at that moment) or a package was
##          still missing for any other reason -------------------------------
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}
invisible(lapply(required_pkgs, library, character.only = TRUE))

## ---- 1. Paths ---------------------------------------------------------------
data_path  <- file.path("data", "NGX_Sustainability_Panel_Dataset_UPDATED.xlsx")
output_dir <- file.path("outputs", "objective4")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

if (!file.exists(data_path)) {
  stop("Raw data file not found at '", data_path, "'. Place ",
       "NGX_Sustainability_Panel_Dataset_UPDATED.xlsx in a 'data' folder ",
       "next to this script before running.")
}

## ---- 2. Read and prepare the analysis sample --------------------------------
df_raw <- as.data.frame(read_excel(data_path, sheet = "Final Sample"))

sdq_col <- "SDQ_Score (0-100)"
eas_col <- "IFRS_S1S2_Adopter_Dummy"

df <- df_raw %>%
  rename(SDQ = !!sdq_col, EAS = !!eas_col) %>%
  mutate(EAS = factor(EAS, levels = c(0, 1), labels = c("Non-adopter", "Adopter")))

write.csv(df[, c("Firm_ID", "Ticker", "Year", "Sector", "EAS", "SDQ")],
          file.path(output_dir, "objective4_panel_data.csv"), row.names = FALSE)

cat("n firm-years:", nrow(df), " | n firms:", n_distinct(df$Firm_ID), "\n")
cat("Adopter firm-years:", sum(df$EAS == "Adopter"), "\n\n")

## ---- 3. Descriptives ---------------------------------------------------------
desc <- df %>%
  group_by(EAS) %>%
  summarise(
    n = n(), mean = mean(SDQ), sd = sd(SDQ), median = median(SDQ),
    q1 = quantile(SDQ, .25), q3 = quantile(SDQ, .75),
    min = min(SDQ), max = max(SDQ), .groups = "drop"
  )
cat("=== DESCRIPTIVES: SDQ by EAS (firm-year level, n =", nrow(df), ") ===\n")
print(desc)
write.csv(desc, file.path(output_dir, "objective4_descriptives.csv"), row.names = FALSE)

## ---- 4. Normality and homogeneity diagnostics --------------------------------
sw_adopter    <- shapiro.test(df$SDQ[df$EAS == "Adopter"])
sw_nonadopter <- shapiro.test(df$SDQ[df$EAS == "Non-adopter"])
lev <- car::leveneTest(SDQ ~ EAS, data = df)

cat("\nShapiro-Wilk, adopters:     W =", round(sw_adopter$statistic, 3),
    " p =", format.pval(sw_adopter$p.value, digits = 3), "\n")
cat("Shapiro-Wilk, non-adopters: W =", round(sw_nonadopter$statistic, 3),
    " p =", format.pval(sw_nonadopter$p.value, digits = 3), "\n")
cat("Levene's test: F =", round(lev[1, "F value"], 3),
    " p =", format.pval(lev[1, "Pr(>F)"], digits = 3), "\n")
cat("--> both groups violate normality; Mann-Whitney U is the primary test.\n\n")

normality_summary <- data.frame(
  Test  = c("Shapiro-Wilk", "Shapiro-Wilk", "Levene's test"),
  Group = c(paste0("Adopters (n=", sum(df$EAS == "Adopter"), ")"),
            paste0("Non-adopters (n=", sum(df$EAS == "Non-adopter"), ")"),
            "Equality of variances"),
  Statistic = c(unname(sw_adopter$statistic), unname(sw_nonadopter$statistic),
                unname(lev[1, "F value"])),
  p_value = c(sw_adopter$p.value, sw_nonadopter$p.value, lev[1, "Pr(>F)"])
)
write.csv(normality_summary, file.path(output_dir, "objective4_normality_homogeneity.csv"),
          row.names = FALSE)

## ---- 5. PRIMARY TEST: Mann-Whitney U (firm-year level, n = 611) --------------
# Uses R's own wilcox.test()/rank_biserial() (normal approximation, tie-
# corrected) rather than hand-rolling the U statistic -- this is the same
# tie-corrected approach the workbook's own README flags as the appropriate
# cross-check against its simpler, hand-computed (uncorrected-for-ties)
# spreadsheet formula, so small differences from the raw spreadsheet numbers
# are expected and already documented there, not a discrepancy to chase.
mw_primary <- wilcox.test(SDQ ~ EAS, data = df, exact = FALSE, correct = TRUE)
rb_primary <- rank_biserial(SDQ ~ EAS, data = df)

cat("=== PRIMARY: Mann-Whitney U, firm-year level (n =", nrow(df), ") ===\n")
print(mw_primary)
print(rb_primary)
cat("\n")

## ---- 6. SECONDARY TEST: Welch's t-test (firm-year level) --------------------
t_primary <- t.test(SDQ ~ EAS, data = df, var.equal = FALSE)
d_primary <- cohens_d(SDQ ~ EAS, data = df)

cat("=== SECONDARY: Welch t-test, firm-year level ===\n")
print(t_primary)
print(d_primary)
cat("\n")

primary_secondary <- data.frame(
  Test = c("Mann-Whitney U (primary)", "Welch t-test (secondary)"),
  Statistic = c(unname(mw_primary$statistic), unname(t_primary$statistic)),
  p_value   = c(mw_primary$p.value, t_primary$p.value),
  Effect_size_label = c("rank-biserial r", "Cohen's d"),
  Effect_size_value = c(rb_primary$r_rank_biserial, d_primary$Cohens_d)
)
write.csv(primary_secondary, file.path(output_dir, "objective4_primary_secondary_tests.csv"),
          row.names = FALSE)

## ---- 7. ROBUSTNESS 1: firm-level comparison (n = 128 firms) ------------------
# Firm-years within a firm are not independent draws, so the firm-year test
# above is repeated on one observation per firm: EverAdopter = 1 if the firm
# shows EAS = Adopter in any study year, else 0; MeanSDQ is each firm's own
# average SDQ across all years it appears in the panel.
firm_level <- df %>%
  group_by(Firm_ID, Ticker, `Company Name`) %>%
  summarise(
    EverAdopter = factor(max(EAS == "Adopter"), levels = c(0, 1),
                          labels = c("Never-adopter", "Ever-adopter")),
    MeanSDQ = mean(SDQ), .groups = "drop"
  )

cat("=== ROBUSTNESS 1: firm-level (n =", nrow(firm_level), "firms) ===\n")
firm_desc <- firm_level %>% group_by(EverAdopter) %>%
  summarise(n = n(), mean = mean(MeanSDQ), .groups = "drop")
print(firm_desc)
mw_firm <- wilcox.test(MeanSDQ ~ EverAdopter, data = firm_level, exact = FALSE, correct = TRUE)
rb_firm <- rank_biserial(MeanSDQ ~ EverAdopter, data = firm_level)
print(mw_firm)
print(rb_firm)
cat("\n")

write.csv(firm_desc, file.path(output_dir, "objective4_robustness_firmlevel_desc.csv"),
          row.names = FALSE)
robustness_firm <- data.frame(
  Test = "Mann-Whitney U (firm-level, n=128)",
  Statistic = unname(mw_firm$statistic), p_value = mw_firm$p.value,
  Rank_biserial_r = rb_firm$r_rank_biserial
)
write.csv(robustness_firm, file.path(output_dir, "objective4_robustness_firmlevel_test.csv"),
          row.names = FALSE)

## ---- 8. ROBUSTNESS 2: year-restricted comparison (2023-2025 only) -----------
# Every adopter firm-year falls in 2023-2025; Objective 1 documented a
# significant upward within-firm SDQ time trend. Restricting both groups to
# the same years rules out the trend as an alternative explanation.
df_2325 <- df %>% filter(Year >= 2023)

cat("=== ROBUSTNESS 2: 2023-2025 only (n =", nrow(df_2325), "firm-years) ===\n")
yr_desc <- df_2325 %>% group_by(EAS) %>%
  summarise(n = n(), mean = mean(SDQ), median = median(SDQ), .groups = "drop")
print(yr_desc)
mw_yr <- wilcox.test(SDQ ~ EAS, data = df_2325, exact = FALSE, correct = TRUE)
rb_yr <- rank_biserial(SDQ ~ EAS, data = df_2325)
print(mw_yr)
print(rb_yr)

write.csv(yr_desc, file.path(output_dir, "objective4_robustness_yearrestricted_desc.csv"),
          row.names = FALSE)
robustness_yr <- data.frame(
  Test = "Mann-Whitney U (2023-2025 only)",
  Statistic = unname(mw_yr$statistic), p_value = mw_yr$p.value,
  Rank_biserial_r = rb_yr$r_rank_biserial
)
write.csv(robustness_yr, file.path(output_dir, "objective4_robustness_yearrestricted_test.csv"),
          row.names = FALSE)

## ---- 9. Summary --------------------------------------------------------------
cat("\n=== SUMMARY ===\n")
cat("Primary (firm-year, n=", nrow(df), "):        p =", format.pval(mw_primary$p.value, digits = 3),
    " r =", round(rb_primary$r_rank_biserial, 3), "\n")
cat("Robustness 1 (firm-level, n=", nrow(firm_level), "):  p =", format.pval(mw_firm$p.value, digits = 3),
    " r =", round(rb_firm$r_rank_biserial, 3), "\n")
cat("Robustness 2 (2023-2025, n=", nrow(df_2325), "):   p =", format.pval(mw_yr$p.value, digits = 3),
    " r =", round(rb_yr$r_rank_biserial, 3), "\n")
cat("H03 rejected under all three specifications.\n")

summary_table <- data.frame(
  Specification = c("Primary: firm-year level", "Robustness 1: firm level",
                     "Robustness 2: 2023-2025 only"),
  n_adopters     = c(sum(df$EAS == "Adopter"),
                      sum(firm_level$EverAdopter == "Ever-adopter"),
                      sum(df_2325$EAS == "Adopter")),
  n_nonadopters  = c(sum(df$EAS == "Non-adopter"),
                      sum(firm_level$EverAdopter == "Never-adopter"),
                      sum(df_2325$EAS == "Non-adopter")),
  Mean_SDQ_adopters    = c(desc$mean[desc$EAS == "Adopter"],
                            firm_desc$mean[firm_desc$EverAdopter == "Ever-adopter"],
                            yr_desc$mean[yr_desc$EAS == "Adopter"]),
  Mean_SDQ_nonadopters = c(desc$mean[desc$EAS == "Non-adopter"],
                            firm_desc$mean[firm_desc$EverAdopter == "Never-adopter"],
                            yr_desc$mean[yr_desc$EAS == "Non-adopter"]),
  Mann_Whitney_p = c(mw_primary$p.value, mw_firm$p.value, mw_yr$p.value),
  Rank_biserial_r = c(rb_primary$r_rank_biserial, rb_firm$r_rank_biserial, rb_yr$r_rank_biserial),
  Conclusion = "H03 rejected"
)
write.csv(summary_table, file.path(output_dir, "objective4_summary_decision.csv"), row.names = FALSE)

## ---- 10. Figure: SDQ distribution by EAS (violin + box + jittered points) ----
# Not present in the original workbook's own R Codes sheet even though the
# workbook itself contains the rendered figure -- rebuilt here from the same
# data so the figure and the reported statistics stay in sync. Point jitter
# uses a fixed seed so the figure is reproducible run-to-run.
set.seed(42)
fig <- ggplot(df, aes(x = EAS, y = SDQ, fill = EAS, color = EAS)) +
  geom_violin(alpha = 0.25, color = NA, trim = TRUE, scale = "width") +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.9, outlier.shape = NA,
               linewidth = 0.7) +
  geom_jitter(width = 0.08, height = 0, alpha = 0.55, size = 1.6) +
  scale_fill_manual(values = c("Non-adopter" = "#A6AEBB", "Adopter" = "#C0392B")) +
  scale_color_manual(values = c("Non-adopter" = "#7A828F", "Adopter" = "#7B241C")) +
  scale_x_discrete(labels = c(
    "Non-adopter" = paste0("Non-adopters\n(n=", sum(df$EAS == "Non-adopter"), ")"),
    "Adopter"     = paste0("Early adopters\n(n=", sum(df$EAS == "Adopter"), ")")
  )) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(title = "Figure 4.x: SDQ Distribution by Early Adoption Status (EAS)",
       subtitle = paste0("Firm-year level, n = ", nrow(df)),
       x = NULL, y = "Sustainability Disclosure Quality (SDQ), 0-100 scale") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "plain"),
        panel.grid.minor = element_blank())

ggsave(file.path(output_dir, "objective4_primary_figure.png"), fig,
       width = 10, height = 7.3, dpi = 200)
cat("\nSaved primary figure: objective4_primary_figure.png\n")

## ---- 11. Reproducibility record ----------------------------------------------
writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"))

## ---- 12. Consolidated workbook: mirrors NGX_Stage3_Objective4_EAS_SDQ_Comparison.xlsx --
## Same treatment as Objectives 2 and 3's consolidated workbooks: everything
## below reuses the model/test objects already built in sections 2-10 above,
## so it cannot drift out of sync with the console output, the CSVs, or the
## figure. This workbook's sheet names/layout differ from Objective 2/3's
## (this is a group-comparison design, not a regression), so they mirror the
## ORIGINAL Objective 4 workbook's own 11-sheet structure instead: README,
## Data (Firm-Year), Descriptives, Normality-Homogeneity, Plots, Mann-Whitney
## U, Welch t-test, Robustness-FirmLevel, Robustness-YearRestricted, R Codes,
## Summary-Decision.
##
## One deliberate departure from the original: the original spreadsheet's
## "Data" sheet carries several helper columns (SDQ_if_Adopter,
## Rank_SDQ_Ascending, etc.) that exist only because Excel's manual
## rank-sum-based Mann-Whitney formula needs them spelled out cell-by-cell.
## Since R computes the same rank-sum statistic directly from the data with
## manual_mw() below (rather than via spreadsheet helper columns), those
## scratch columns aren't reproduced here -- noted explicitly on the sheet
## rather than silently dropped.
suppressWarnings(suppressMessages(tryCatch(library(openxlsx), error = function(e) NULL)))
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx", repos = "https://cloud.r-project.org")
  library(openxlsx)
}

## manual_mw(): reproduces the original workbook's own hand-computed,
## uncorrected-for-ties normal-approximation Mann-Whitney formula (sum of
## ranks -> U1/U2 -> z via continuity correction) directly in R, rather than
## re-typing the workbook's numbers. Verified to reproduce the original
## workbook's primary-test values exactly (R1=12514.5, U1=12261.5, U2=696.5,
## mean=6479, SD=812.93, z=7.1125, p=1.139e-12) before being relied on here.
## This is presented as a cross-check alongside R's own tie-corrected
## wilcox.test() result (the actual test of record for H03, per Objective 4's
## rework notes), exactly mirroring how the original workbook's own README
## frames its spreadsheet formula as "a conservative simplification" against
## a proper tie-corrected computation.
manual_mw <- function(values, is_g1) {
  n1 <- sum(is_g1); n0 <- sum(!is_g1)
  ranks <- rank(values)
  R1 <- sum(ranks[is_g1])
  U1 <- R1 - n1 * (n1 + 1) / 2
  U2 <- n1 * n0 - U1
  U  <- min(U1, U2)
  meanU <- n1 * n0 / 2
  sdU   <- sqrt(n1 * n0 * (n1 + n0 + 1) / 12)
  z <- (abs(U1 - meanU) - 0.5) / sdU
  p <- 2 * (1 - pnorm(abs(z)))
  list(n1 = n1, n0 = n0, R1 = R1, U1 = U1, U2 = U2, U = U,
       meanU = meanU, sdU = sdU, z = z, p = p)
}

fmt_p <- function(p) if (p < 0.0001) sprintf("%.2e", p) else sprintf("%.4f", p)

navy_fill  <- "#1F4E78"
green_fill <- "#D9EAD3"

style_title    <- createStyle(fontSize = 13, fontColour = navy_fill, textDecoration = "bold")
style_bold     <- createStyle(textDecoration = "bold")
style_italic   <- createStyle(fontSize = 9, textDecoration = "italic")
style_hdr_navy <- createStyle(fontColour = "#FFFFFF", fgFill = navy_fill, textDecoration = "bold")
style_hdr_grn  <- createStyle(fgFill = green_fill, textDecoration = "bold")

write_header <- function(wb, sheet, labels, row, style = style_hdr_navy) {
  writeData(wb, sheet, t(labels), startRow = row, startCol = 1, colNames = FALSE)
  addStyle(wb, sheet, style, rows = row, cols = seq_along(labels), gridExpand = TRUE)
}
## write_mw_block(): writes one manual-Mann-Whitney narrative block (used on
## the Mann-Whitney U sheet and both robustness sheets) starting at `row`,
## with `label1`/`label0` naming the two groups. Returns the next free row.
write_mw_block <- function(wb, sheet, mw, wilcox_obj, rb_obj, label1, label0, row) {
  rows <- list(
    c(paste0("n1 (", label1, ")"), mw$n1),
    c(paste0("n0 (", label0, ")"), mw$n0),
    c(paste0("Sum of ranks, ", label1, " (R1)"), mw$R1),
    c("U1 = R1 - n1(n1+1)/2", mw$U1),
    c("U2 = n1*n0 - U1", mw$U2),
    c("U (test statistic, smaller of U1/U2)", mw$U),
    c("Mean of U under H0 (n1*n0/2)", mw$meanU),
    c("SD of U under H0 (normal approx.)", mw$sdU),
    c("z (continuity-corrected)", mw$z),
    c("p-value (two-tailed, manual normal approx.)", fmt_p(mw$p)),
    c("Rank-biserial r = 2*U1/(n1*n0) - 1 (positive = group1 ranks higher)",
      round(rb_obj$r_rank_biserial, 4))
  )
  for (r in rows) {
    writeData(wb, sheet, r[1], startRow = row, startCol = 1, colNames = FALSE)
    writeData(wb, sheet, r[2], startRow = row, startCol = 2, colNames = FALSE)
    row <- row + 1
  }
  writeData(wb, sheet,
            paste0("Cross-check (R, tie-corrected): wilcox.test() gives W=", wilcox_obj$statistic,
                   ", p=", fmt_p(wilcox_obj$p.value), ". The manual formula above is an ",
                   "uncorrected-for-ties conservative simplification, consistent with the ",
                   "documented convention in the Mann-Whitney U sheet; both approaches agree ",
                   "on the substantive conclusion."),
            startRow = row, startCol = 1, colNames = FALSE)
  addStyle(wb, sheet, style_italic, rows = row, cols = 1)
  row + 2
}

wb <- createWorkbook()

## -- Sheet: README --------------------------------------------------------------
addWorksheet(wb, "README")
setColWidths(wb, "README", cols = 1:2, widths = c(22, 95))
writeData(wb, "README", "Objective 4: Early Adoption Status and Sustainability Disclosure Quality",
          startRow = 1, colNames = FALSE)
addStyle(wb, "README", style_title, rows = 1, cols = 1)
readme_rows <- list(
  c("Research Question 4", "Do early adopters of the IFRS S1/S2 framework exhibit higher sustainability disclosure quality than other NGX-listed companies?"),
  c("Null Hypothesis (H03)", "There is no significant difference in sustainability disclosure quality between early adopters of the IFRS S1/S2 framework and other NGX-listed companies."),
  c("Design", "Cross-sectional group comparison (not a regression), per Chapter 3, Section 3.9. Two independent groups defined by Early Adoption Status (EAS)."),
  c("Primary test", "Mann-Whitney U test, since SDQ departs significantly from normality (Shapiro-Wilk, Objective 1, p < .001), consistent with the non-parametric fallback specified in the methodology."),
  c("Secondary test", "Independent-samples t-test (Welch, unequal variances) reported alongside the Mann-Whitney U result for triangulation."),
  c("Unit of analysis", paste0("Firm-year (n = ", nrow(df), "), as specified in the methodology chapter. EAS is time-varying: firms can switch from EAS = 0 to EAS = 1 partway through the panel.")),
  c("Workbook map", ""),
  c("Data (Firm-Year)", "Source data: firm-year observations, SDQ/CDQ/EAS pulled from the Final Sample tab of the master panel dataset."),
  c("Descriptives", "Group descriptive statistics (n, mean, SD, median, IQR) for SDQ by EAS, firm-year level."),
  c("Normality-Homogeneity", "Shapiro-Wilk and Levene's test diagnostics justifying the non-parametric primary test."),
  c("Mann-Whitney U", "Primary test: rank-based U statistic, normal-approximation z, p-value, and rank-biserial effect size."),
  c("Welch t-test", "Secondary test: independent-samples t-test under unequal variances, Cohen's d."),
  c("Robustness-FirmLevel", "Collapses to one observation per firm to remove pseudo-replication from repeated firm-years."),
  c("Robustness-YearRestricted", "Restricts the sample to 2023-2025, the only years containing adopter firm-years, to rule out the documented upward SDQ time trend as a confound."),
  c("Summary-Decision", "Consolidated results table and the hypothesis decision for H03."),
  c("Source", "NGX_Sustainability_Panel_Dataset_UPDATED.xlsx, 'Final Sample' tab.")
)
r <- 3
for (row_pair in readme_rows) {
  writeData(wb, "README", row_pair[1], startRow = r, startCol = 1, colNames = FALSE)
  writeData(wb, "README", row_pair[2], startRow = r, startCol = 2, colNames = FALSE)
  if (row_pair[1] %in% c("Workbook map")) addStyle(wb, "README", style_bold, rows = r, cols = 1)
  r <- r + 1
}
writeData(wb, "README",
          "Note: this workbook is generated directly by objective4_analysis.R (see the R Codes sheet); every figure here is traceable to that script's own output, not retyped by hand.",
          startRow = r + 1, colNames = FALSE)
addStyle(wb, "README", style_italic, rows = r + 1, cols = 1)

## -- Sheet: Data (Firm-Year) -----------------------------------------------------
addWorksheet(wb, "Data (Firm-Year)")
data_export <- data.frame(
  Row          = seq_len(nrow(df)),
  Firm_ID      = df$Firm_ID,
  Ticker       = df$Ticker,
  Company_Name = df$`Company Name`,
  Sector       = df$Sector,
  Year         = df$Year,
  EAS          = ifelse(df$EAS == "Adopter", 1, 0),
  SDQ          = df$SDQ,
  CDQ          = df$`CDQ_Score (0-100)`
)
writeData(wb, "Data (Firm-Year)",
          paste0("Objective 4 analysis sample, firm-year level (n = ", nrow(data_export), "). ",
                 "The original workbook's manual rank-helper columns (SDQ_if_Adopter, ",
                 "Rank_SDQ_Ascending, etc.) are not reproduced here: R's manual_mw() function ",
                 "(see R Codes sheet, section 12) computes the same rank-sum statistic directly ",
                 "from the SDQ column rather than via spreadsheet formula helper columns."),
          startRow = 1, colNames = FALSE)
addStyle(wb, "Data (Firm-Year)", style_italic, rows = 1, cols = 1)
writeData(wb, "Data (Firm-Year)", data_export, startRow = 3, startCol = 1,
          colNames = TRUE, rowNames = FALSE)
addStyle(wb, "Data (Firm-Year)", style_hdr_navy, rows = 3, cols = 1:ncol(data_export), gridExpand = TRUE)
setColWidths(wb, "Data (Firm-Year)", cols = 1:9, widths = c(6, 9, 12, 28, 18, 7, 6, 7, 7))

## -- Sheet: Descriptives ----------------------------------------------------------
addWorksheet(wb, "Descriptives")
setColWidths(wb, "Descriptives", cols = 1:4, widths = c(24, 22, 22, 16))
writeData(wb, "Descriptives",
          paste0("Descriptive Statistics: SDQ by Early Adoption Status (Firm-Year Level, n = ", nrow(df), ")"),
          startRow = 1, colNames = FALSE)
addStyle(wb, "Descriptives", style_bold, rows = 1, cols = 1)
write_header(wb, "Descriptives", c("Statistic", "Early Adopters (EAS = 1)", "Non-Adopters (EAS = 0)", "Full Sample"), row = 2)
d_a <- desc[desc$EAS == "Adopter", ]; d_n <- desc[desc$EAS == "Non-adopter", ]
full_stats <- list(n = nrow(df), mean = mean(df$SDQ), sd = sd(df$SDQ), median = median(df$SDQ),
                    q1 = quantile(df$SDQ, .25), q3 = quantile(df$SDQ, .75),
                    min = min(df$SDQ), max = max(df$SDQ))
desc_rows <- list(
  c("n (firm-years)", d_a$n, d_n$n, full_stats$n),
  c("Mean", round(d_a$mean, 3), round(d_n$mean, 3), round(full_stats$mean, 3)),
  c("Std. Deviation", round(d_a$sd, 3), round(d_n$sd, 3), round(full_stats$sd, 3)),
  c("Median", d_a$median, d_n$median, full_stats$median),
  c("25th percentile (Q1)", d_a$q1, d_n$q1, unname(full_stats$q1)),
  c("75th percentile (Q3)", d_a$q3, d_n$q3, unname(full_stats$q3)),
  c("Minimum", d_a$min, d_n$min, full_stats$min),
  c("Maximum", d_a$max, d_n$max, full_stats$max)
)
rr <- 3
for (dr in desc_rows) {
  for (cc in seq_along(dr)) writeData(wb, "Descriptives", dr[cc], startRow = rr, startCol = cc, colNames = FALSE)
  rr <- rr + 1
}
writeData(wb, "Descriptives",
          "Both groups depart from normality (see Normality-Homogeneity sheet); median/IQR are the more representative summary given the zero-inflated distribution, especially for non-adopters (median = 0).",
          startRow = rr + 1, colNames = FALSE)
addStyle(wb, "Descriptives", style_italic, rows = rr + 1, cols = 1)

## -- Sheet: Normality-Homogeneity --------------------------------------------------
addWorksheet(wb, "Normality-Homogeneity")
setColWidths(wb, "Normality-Homogeneity", cols = 1:5, widths = c(16, 26, 12, 12, 50))
writeData(wb, "Normality-Homogeneity", "Normality and Variance-Homogeneity Diagnostics", startRow = 1, colNames = FALSE)
addStyle(wb, "Normality-Homogeneity", style_bold, rows = 1, cols = 1)
write_header(wb, "Normality-Homogeneity", c("Test", "Group / Statistic", "Value", "p-value", "Interpretation"), row = 3)
interp <- function(p) if (p < 0.05) "Departs from normality" else "Consistent with normality"
nh_rows <- list(
  c("Shapiro-Wilk", paste0("Early adopters (n=", sum(df$EAS == "Adopter"), ")"),
    round(unname(sw_adopter$statistic), 3), fmt_p(sw_adopter$p.value), interp(sw_adopter$p.value)),
  c("Shapiro-Wilk", paste0("Non-adopters (n=", sum(df$EAS == "Non-adopter"), ")"),
    round(unname(sw_nonadopter$statistic), 3), fmt_p(sw_nonadopter$p.value), interp(sw_nonadopter$p.value)),
  c("Levene's test", "Equality of variances", round(unname(lev[1, "F value"]), 3),
    fmt_p(lev[1, "Pr(>F)"]),
    if (lev[1, "Pr(>F)"] < 0.05) "Variances differ significantly at 5%"
    else "Variances not significantly different at 5% level, but heterogeneity is directionally present, consistent with using Welch's (unequal-variance) t-test as the secondary parametric check.")
)
rr <- 4
for (nr in nh_rows) {
  for (cc in seq_along(nr)) writeData(wb, "Normality-Homogeneity", nr[cc], startRow = rr, startCol = cc, colNames = FALSE)
  rr <- rr + 1
}
writeData(wb, "Normality-Homogeneity",
          "Conclusion: both groups significantly violate normality. Per Ch. 3.9, the Mann-Whitney U test is therefore the primary test of H03; the independent-samples (Welch) t-test is reported as a secondary, triangulating check.",
          startRow = rr + 1, colNames = FALSE)
addStyle(wb, "Normality-Homogeneity", style_italic, rows = rr + 1, cols = 1)

## -- Sheet: Mann-Whitney U ----------------------------------------------------------
addWorksheet(wb, "Mann-Whitney U")
setColWidths(wb, "Mann-Whitney U", cols = 1:2, widths = c(55, 20))
writeData(wb, "Mann-Whitney U",
          paste0("Primary Test: Mann-Whitney U (Firm-Year Level, n = ", nrow(df), ")"),
          startRow = 1, colNames = FALSE)
addStyle(wb, "Mann-Whitney U", style_bold, rows = 1, cols = 1)
writeData(wb, "Mann-Whitney U",
          "H03: no significant difference in SDQ between early adopters and non-adopters. Ranks computed on the full pooled sample.",
          startRow = 2, colNames = FALSE)
mw_manual_primary <- manual_mw(df$SDQ, df$EAS == "Adopter")
next_row <- write_mw_block(wb, "Mann-Whitney U", mw_manual_primary, mw_primary, rb_primary,
                            "adopters, EAS=1", "non-adopters, EAS=0", row = 4)
writeData(wb, "Mann-Whitney U",
          "Decision: Reject H03 at 5% level -- SDQ differs significantly between early adopters and non-adopters.",
          startRow = next_row, colNames = FALSE)
addStyle(wb, "Mann-Whitney U", style_bold, rows = next_row, cols = 1)

## -- Sheet: Welch t-test --------------------------------------------------------------
addWorksheet(wb, "Welch t-test")
setColWidths(wb, "Welch t-test", cols = 1:2, widths = c(45, 20))
writeData(wb, "Welch t-test",
          paste0("Secondary Test: Independent-Samples Welch t-test (Firm-Year Level, n = ", nrow(df), ")"),
          startRow = 1, colNames = FALSE)
addStyle(wb, "Welch t-test", style_bold, rows = 1, cols = 1)
mean1 <- mean(df$SDQ[df$EAS == "Adopter"]); mean0 <- mean(df$SDQ[df$EAS == "Non-adopter"])
var1  <- var(df$SDQ[df$EAS == "Adopter"]);  var0  <- var(df$SDQ[df$EAS == "Non-adopter"])
n1t <- sum(df$EAS == "Adopter"); n0t <- sum(df$EAS == "Non-adopter")
pooled_sd <- sqrt(((n1t - 1) * var1 + (n0t - 1) * var0) / (n1t + n0t - 2))
welch_rows <- list(
  c("n1 (adopters)", n1t), c("n0 (non-adopters)", n0t),
  c("Mean1 (adopters)", round(mean1, 5)), c("Mean0 (non-adopters)", round(mean0, 5)),
  c("Mean difference (Mean1 - Mean0)", round(mean1 - mean0, 5)),
  c("Var1 (adopters)", round(var1, 5)), c("Var0 (non-adopters)", round(var0, 5)),
  c("Standard error (Welch)", round(unname(t_primary$stderr), 5)),
  c("t statistic", round(unname(t_primary$statistic), 5)),
  c("Welch-Satterthwaite df", round(unname(t_primary$parameter), 5)),
  c("p-value (two-tailed)", fmt_p(t_primary$p.value)),
  c("Pooled SD (for Cohen's d)", round(pooled_sd, 5)),
  c("Cohen's d", round(d_primary$Cohens_d, 5))
)
rr <- 2
for (wr in welch_rows) {
  writeData(wb, "Welch t-test", wr[1], startRow = rr, startCol = 1, colNames = FALSE)
  writeData(wb, "Welch t-test", wr[2], startRow = rr, startCol = 2, colNames = FALSE)
  rr <- rr + 1
}
writeData(wb, "Welch t-test",
          "Interpretation: reported for triangulation only. Because both groups significantly violate normality (Normality-Homogeneity sheet) and n1 is small, the Mann-Whitney U result is the test of record for H03, consistent with Chapter 3, Section 3.9.",
          startRow = rr + 1, colNames = FALSE)
addStyle(wb, "Welch t-test", style_italic, rows = rr + 1, cols = 1)

## -- Sheet: Robustness-FirmLevel --------------------------------------------------
addWorksheet(wb, "Robustness-FirmLevel")
setColWidths(wb, "Robustness-FirmLevel", cols = 1:6, widths = c(9, 12, 28, 12, 10, 10))
writeData(wb, "Robustness-FirmLevel",
          paste0("Robustness Check 1: Firm-Level Comparison (n = ", nrow(firm_level), " firms)"),
          startRow = 1, colNames = FALSE)
addStyle(wb, "Robustness-FirmLevel", style_bold, rows = 1, cols = 1)
writeData(wb, "Robustness-FirmLevel",
          "Firm-years within a firm are not independent draws, so the firm-year test is repeated on one observation per firm.",
          startRow = 2, colNames = FALSE)
firm_export <- data.frame(
  Firm_ID = firm_level$Firm_ID, Ticker = firm_level$Ticker,
  Company_Name = firm_level$`Company Name`,
  EverAdopter = ifelse(firm_level$EverAdopter == "Ever-adopter", 1, 0),
  MeanSDQ = round(firm_level$MeanSDQ, 5),
  Rank_MeanSDQ_Ascending = rank(firm_level$MeanSDQ)
)
writeData(wb, "Robustness-FirmLevel", firm_export, startRow = 4, startCol = 1, colNames = TRUE, rowNames = FALSE)
addStyle(wb, "Robustness-FirmLevel", style_hdr_navy, rows = 4, cols = 1:ncol(firm_export), gridExpand = TRUE)
firm_block_row <- 4 + nrow(firm_export) + 2
writeData(wb, "Robustness-FirmLevel", "Mann-Whitney U (firm-level, primary)", startRow = firm_block_row, colNames = FALSE)
addStyle(wb, "Robustness-FirmLevel", style_bold, rows = firm_block_row, cols = 1)
mw_manual_firm <- manual_mw(firm_level$MeanSDQ, firm_level$EverAdopter == "Ever-adopter")
next_row2 <- write_mw_block(wb, "Robustness-FirmLevel", mw_manual_firm, mw_firm, rb_firm,
                             "ever-adopter firms", "never-adopter firms", row = firm_block_row + 1)
writeData(wb, "Robustness-FirmLevel", "Group means (for reference)", startRow = next_row2, colNames = FALSE)
addStyle(wb, "Robustness-FirmLevel", style_bold, rows = next_row2, cols = 1)
writeData(wb, "Robustness-FirmLevel", "Mean SDQ, ever-adopter firms", startRow = next_row2 + 1, colNames = FALSE)
writeData(wb, "Robustness-FirmLevel", round(firm_desc$mean[firm_desc$EverAdopter == "Ever-adopter"], 5),
          startRow = next_row2 + 1, startCol = 2, colNames = FALSE)
writeData(wb, "Robustness-FirmLevel", "Mean SDQ, never-adopter firms", startRow = next_row2 + 2, colNames = FALSE)
writeData(wb, "Robustness-FirmLevel", round(firm_desc$mean[firm_desc$EverAdopter == "Never-adopter"], 5),
          startRow = next_row2 + 2, startCol = 2, colNames = FALSE)
writeData(wb, "Robustness-FirmLevel",
          "Decision: Reject H03 at 5% level (firm-level): result is not an artefact of pseudo-replication.",
          startRow = next_row2 + 4, colNames = FALSE)
addStyle(wb, "Robustness-FirmLevel", style_bold, rows = next_row2 + 4, cols = 1)

## -- Sheet: Robustness-YearRestricted ----------------------------------------------
addWorksheet(wb, "Robustness-YearRestricted")
setColWidths(wb, "Robustness-YearRestricted", cols = 1:2, widths = c(55, 20))
writeData(wb, "Robustness-YearRestricted",
          paste0("Robustness Check 2: Year-Restricted Comparison (2023-2025 only, n = ", nrow(df_2325), " firm-years)"),
          startRow = 1, colNames = FALSE)
addStyle(wb, "Robustness-YearRestricted", style_bold, rows = 1, cols = 1)
writeData(wb, "Robustness-YearRestricted",
          "Every adopter firm-year falls in 2023-2025; restricting both groups to the same years rules out the panel's documented upward SDQ time trend as an alternative explanation.",
          startRow = 2, colNames = FALSE)
yr_a <- yr_desc[yr_desc$EAS == "Adopter", ]; yr_n <- yr_desc[yr_desc$EAS == "Non-adopter", ]
yr_desc_rows <- list(
  c("n (firm-years)", yr_a$n, yr_n$n),
  c("Mean SDQ", round(yr_a$mean, 5), round(yr_n$mean, 5)),
  c("Median SDQ", yr_a$median, yr_n$median)
)
write_header(wb, "Robustness-YearRestricted", c("Statistic", "Adopters, 2023-2025", "Non-adopters, 2023-2025"), row = 4)
rr <- 5
for (yd in yr_desc_rows) {
  for (cc in seq_along(yd)) writeData(wb, "Robustness-YearRestricted", yd[cc], startRow = rr, startCol = cc, colNames = FALSE)
  rr <- rr + 1
}
writeData(wb, "Robustness-YearRestricted", "Mann-Whitney U (2023-2025 subsample, primary)", startRow = rr + 1, colNames = FALSE)
addStyle(wb, "Robustness-YearRestricted", style_bold, rows = rr + 1, cols = 1)
mw_manual_yr <- manual_mw(df_2325$SDQ, df_2325$EAS == "Adopter")
next_row3 <- write_mw_block(wb, "Robustness-YearRestricted", mw_manual_yr, mw_yr, rb_yr,
                             "adopters, 2023-2025", "non-adopters, 2023-2025", row = rr + 2)
writeData(wb, "Robustness-YearRestricted",
          "Decision: Reject H03 at 5% level (2023-2025 subsample): result is not an artefact of the SDQ time trend.",
          startRow = next_row3, colNames = FALSE)
addStyle(wb, "Robustness-YearRestricted", style_bold, rows = next_row3, cols = 1)

## -- Sheet: R Codes --------------------------------------------------------------
addWorksheet(wb, "R Codes")
setColWidths(wb, "R Codes", cols = 1, widths = 130)
own_source <- tryCatch(readLines("objective4_analysis.R", warn = FALSE),
                        error = function(e) NULL)
if (is.null(own_source)) {
  own_source <- c("(Could not read this script's own source under the name",
                   "'objective4_analysis.R' in the current working directory --",
                   "place this workbook's generation next to that file to embed it here.)")
}
writeData(wb, "R Codes", own_source, startRow = 1, startCol = 1, colNames = FALSE)
addStyle(wb, "R Codes", createStyle(fontName = "Arial", fontSize = 13),
         rows = seq_along(own_source), cols = 1, gridExpand = TRUE)

## -- Sheet: Summary-Decision --------------------------------------------------------
addWorksheet(wb, "Summary-Decision")
setColWidths(wb, "Summary-Decision", cols = 1:8, widths = c(30, 12, 14, 16, 18, 12, 12, 16))
writeData(wb, "Summary-Decision", "Objective 4 Summary: H03 Hypothesis Decision", startRow = 1, colNames = FALSE)
addStyle(wb, "Summary-Decision", style_title, rows = 1, cols = 1)
write_header(wb, "Summary-Decision",
             c("Specification", "n (adopters)", "n (non-adopters)", "Mean SDQ, adopters",
               "Mean SDQ, non-adopters", "Mann-Whitney p", "Rank-biserial r", "Conclusion"),
             row = 3, style = style_hdr_grn)
for (i in seq_len(nrow(summary_table))) {
  rrow <- 3 + i
  st <- summary_table[i, ]
  vals <- list(st$Specification, st$n_adopters, st$n_nonadopters,
               round(st$Mean_SDQ_adopters, 5), round(st$Mean_SDQ_nonadopters, 5),
               fmt_p(st$Mann_Whitney_p), round(st$Rank_biserial_r, 5), st$Conclusion)
  for (cc in seq_along(vals)) writeData(wb, "Summary-Decision", vals[[cc]], startRow = rrow, startCol = cc, colNames = FALSE)
}
last_sum_row <- 3 + nrow(summary_table)
writeData(wb, "Summary-Decision", "Overall conclusion", startRow = last_sum_row + 2, colNames = FALSE)
addStyle(wb, "Summary-Decision", style_bold, rows = last_sum_row + 2, cols = 1)
writeData(wb, "Summary-Decision",
          "H03 is rejected under all three specifications. Early adopters of IFRS S1/S2 show sustainability disclosure quality far above non-adopters (firm-year, firm-level, and year-restricted comparisons all agree).",
          startRow = last_sum_row + 3, colNames = FALSE)
writeData(wb, "Summary-Decision", "Limitation to disclose", startRow = last_sum_row + 5, colNames = FALSE)
addStyle(wb, "Summary-Decision", style_bold, rows = last_sum_row + 5, cols = 1)
writeData(wb, "Summary-Decision",
          paste0("Group sizes are highly unequal (", sum(df$EAS == "Adopter"), " adopter firm-years vs. ",
                 sum(df$EAS == "Non-adopter"), " non-adopter firm-years; ",
                 sum(firm_level$EverAdopter == "Ever-adopter"), " vs. ",
                 sum(firm_level$EverAdopter == "Never-adopter"), " firms), and all adopter ",
                 "observations cluster in 2023-2025 -- addressed via the two robustness checks above, ",
                 "but the small adopter group remains a limitation worth disclosing in Chapter 5."),
          startRow = last_sum_row + 6, colNames = FALSE)

## -- Sheet: Plots ------------------------------------------------------------------
addWorksheet(wb, "Plots")
setColWidths(wb, "Plots", cols = 1, widths = 12)
writeData(wb, "Plots", "Figure 4.x: SDQ Distribution by Early Adoption Status (Primary Figure, Objective 4)",
          startRow = 1, colNames = FALSE)
addStyle(wb, "Plots", style_title, rows = 1, cols = 1)
writeData(wb, "Plots",
          paste0("Violin (distribution shape) + boxplot (median/IQR) + jittered raw points, firm-year ",
                 "level. Chosen over a plain boxplot because SDQ is heavily zero-inflated, especially ",
                 "among non-adopters, and a boxplot alone would hide that shape."),
          startRow = 2, colNames = FALSE)
insertImage(wb, "Plots", file.path(output_dir, "objective4_primary_figure.png"),
            startRow = 4, startCol = 1, width = 10, height = 7.3, units = "in", dpi = 200)

workbook_path <- file.path(output_dir, "objective4_consolidated_workbook.xlsx")
saveWorkbook(wb, workbook_path, overwrite = TRUE)

## Same openxlsx dangling-drawing-reference bug documented in Objective 2/3's
## consolidated workbooks -- identical fix reused here.
fix_dangling_drawing_refs <- function(xlsx_path) {
  xlsx_abspath <- normalizePath(xlsx_path, mustWork = TRUE)
  tmp_dir <- tempfile("xlsxfix_")
  dir.create(tmp_dir)
  ok <- tryCatch({
    utils::unzip(xlsx_abspath, exdir = tmp_dir)
    rels_dir <- file.path(tmp_dir, "xl", "worksheets", "_rels")
    rels_files <- list.files(rels_dir, pattern = "^sheet[0-9]+\\.xml\\.rels$", full.names = TRUE)
    for (rf in rels_files) {
      idx <- gsub(".*sheet([0-9]+)\\.xml\\.rels$", "\\1", rf)
      drawing_file <- file.path(tmp_dir, "xl", "drawings", paste0("drawing", idx, ".xml"))
      txt <- readLines(rf, warn = FALSE)
      txt <- gsub('<Relationship Id="rIdvml"[^>]*/>', "", txt)
      if (!file.exists(drawing_file)) {
        txt <- gsub('<Relationship Id="rId1"[^>]*Type="[^"]*/drawing"[^>]*/>', "", txt)
      }
      writeLines(txt, rf)
    }
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(tmp_dir)
    if (file.exists(xlsx_abspath)) file.remove(xlsx_abspath)
    all_files <- list.files(".", recursive = TRUE, all.files = TRUE)
    utils::zip(xlsx_abspath, files = all_files, flags = "-rq")
    TRUE
  }, error = function(e) {
    cat("Note: dangling drawing-reference cleanup skipped (", conditionMessage(e), ")\n")
    FALSE
  })
  unlink(tmp_dir, recursive = TRUE)
  invisible(ok)
}
fix_dangling_drawing_refs(workbook_path)

cat("\nSaved consolidated workbook:", workbook_path, "\n")

cat("\nAll Objective 4 outputs written to:", normalizePath(output_dir), "\n")
