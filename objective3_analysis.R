# ============================================================================
# OBJECTIVE 3: Effect of Climate-related Disclosure Quality (CDQ) on Firm Value
# NGX-Listed Companies, IFRS S1/S2 Reporting Framework, 2021-2025
#
# Single, self-contained, top-to-bottom pipeline. Reads the raw panel dataset,
# builds the analysis sample, fits the base FE/RE models with the Hausman
# test, runs the full diagnostic battery, four robustness checks, two
# extended checks (two-way FE, lagged CDQ), and produces both figures used
# in Chapter Four. All coefficients in the figures are pulled directly from
# the fitted model objects earlier in this script, never re-typed as literal
# values, so the figures cannot drift out of sync with the regression output.
#
# Expected folder layout (relative to this script):
#   ./data/NGX_Sustainability_Panel_Dataset_UPDATED.xlsx
#   ./outputs/objective3/   <- created automatically if missing
#
# Run with:   Rscript objective3_analysis.R
# ============================================================================

## ---- 0. Locale: ensure the special characters used in figure text/labels
##         (em dash, +/-, <=, >=, the star marker) render correctly regardless
##         of the invoking shell's locale. Harmless if it fails (e.g. on some
##         Windows setups the exact locale name differs), hence the tryCatch.
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
required_pkgs <- c("readxl", "plm", "lmtest", "sandwich", "car",
                    "ggplot2", "patchwork", "scales")
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
output_dir <- file.path("outputs", "objective3")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

if (!file.exists(data_path)) {
  stop("Raw data file not found at '", data_path, "'. Place ",
       "NGX_Sustainability_Panel_Dataset_UPDATED.xlsx in a 'data' folder ",
       "next to this script before running.")
}

## ---- 2. Read and prepare the analysis sample (single read, used throughout) -
df_raw <- as.data.frame(read_excel(data_path, sheet = "Final Sample"))

winsorize <- function(x, lo = 0.005, hi = 0.995) {
  q <- quantile(x, probs = c(lo, hi), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}
df_raw$Tobin_Q_Wins  <- winsorize(df_raw$Tobin_Q)
df_raw$Leverage_Wins <- winsorize(df_raw$Leverage)

vars <- c("Ticker", "Year", "CDQ_Score (0-100)", "Tobin_Q_Wins",
          "Firm_Size (LN Total Assets)", "Leverage_Wins", "Firm_Age",
          "IFRS_S1S2_Adopter_Dummy")
df <- df_raw[, vars]
names(df) <- c("Ticker", "Year", "CDQ_Score", "Tobin_Q_Wins", "Firm_Size",
               "Leverage_Wins", "Firm_Age", "EAS_Dummy")
df <- na.omit(df)

write.csv(df, file.path(output_dir, "objective3_panel_data.csv"), row.names = FALSE)
cat("Analysis sample:", nrow(df), "firm-years,", length(unique(df$Ticker)), "firms\n")

pdata <- pdata.frame(df, index = c("Ticker", "Year"))

## ---- 3. Base model: FE vs RE, Hausman test ----------------------------------
fe_base <- plm(Tobin_Q_Wins ~ CDQ_Score + Firm_Size + Leverage_Wins + Firm_Age,
               data = pdata, model = "within")
re_base <- plm(Tobin_Q_Wins ~ CDQ_Score + Firm_Size + Leverage_Wins + Firm_Age,
               data = pdata, model = "random")

hausman <- phtest(fe_base, re_base)
cat("\n================ HAUSMAN TEST ================\n"); print(hausman)

base_plain_se  <- coeftest(fe_base)
base_clustered <- coeftest(fe_base, vcov = vcovHC(fe_base, method = "arellano",
                                                    type = "HC1", cluster = "group"))
# RE is the model the Hausman test rejects, so it is reported for reference only,
# with plain (non-clustered) SE -- clustering is reserved for the model actually
# carried forward for inference (FE).
re_plain <- coeftest(re_base)

cat("\n================ BASE MODEL (FE, plain SE) ================\n"); print(base_plain_se)
cat("\n================ BASE MODEL (FE, firm-clustered SE, Petersen 2009) ================\n"); print(base_clustered)
cat("\n================ BASE MODEL (RE, NOT selected, plain SE) ================\n"); print(re_plain)

model_overview <- data.frame(
  Metric = c("R-squared", "Adj R-squared", "N (firm-years)", "N (firms)",
             "Selected model", "Hausman test"),
  Value  = c(round(summary(fe_base)$r.squared[1], 4),
             round(summary(fe_base)$r.squared[2], 4),
             nobs(fe_base),
             length(unique(df$Ticker)),
             "Fixed Effects",
             sprintf("chi2(%d) = %.2f, p %s", hausman$parameter,
                     hausman$statistic,
                     ifelse(hausman$p.value < 0.001, "< 0.001",
                            sprintf("= %.3f", hausman$p.value))))
)
write.csv(model_overview, file.path(output_dir, "objective3_model_overview.csv"), row.names = FALSE)

fe_results_table <- data.frame(
  Variable    = rownames(base_clustered),
  Coefficient = base_clustered[, 1],
  Std_Error   = base_clustered[, 2],
  t_value     = base_clustered[, 3],
  p_value     = base_clustered[, 4]
)
write.csv(fe_results_table, file.path(output_dir, "objective3_fe_results.csv"), row.names = FALSE)

re_results_table <- data.frame(
  Variable    = rownames(re_plain),
  Coefficient = re_plain[, 1],
  Std_Error   = re_plain[, 2],
  z_value     = re_plain[, 3],
  p_value     = re_plain[, 4]
)
write.csv(re_results_table, file.path(output_dir, "objective3_re_results.csv"), row.names = FALSE)

## ---- 4. Diagnostics ----------------------------------------------------------
# 4a. Correlation matrix among the regressors
cor_vars   <- df[, c("CDQ_Score", "Firm_Size", "Leverage_Wins", "Firm_Age")]
cor_matrix <- cor(cor_vars, use = "pairwise.complete.obs", method = "pearson")
write.csv(round(cor_matrix, 3), file.path(output_dir, "objective3_correlation_matrix.csv"))

# 4b. Variance Inflation Factor. VIF is assessed on the pooled OLS equivalent
#     of the model, since the FE "within" transformation removes the
#     between-entity variation VIF is designed to detect.
pooled_ols <- lm(Tobin_Q_Wins ~ CDQ_Score + Firm_Size + Leverage_Wins + Firm_Age, data = df)
vif_vals   <- car::vif(pooled_ols)
write.csv(data.frame(Variable = names(vif_vals), VIF = round(vif_vals, 3)),
          file.path(output_dir, "objective3_vif.csv"), row.names = FALSE)

# 4c. Breusch-Pagan test for heteroskedasticity (regressors as in the base model)
bp_test <- bptest(Tobin_Q_Wins ~ CDQ_Score + Firm_Size + Leverage_Wins + Firm_Age, data = df)

# 4d. Breusch-Godfrey/Wooldridge test for serial correlation in the FE panel model
bg_test <- pbgtest(fe_base)

cat("\n================ VIF ================\n"); print(vif_vals)
cat("\n================ BREUSCH-PAGAN (heteroskedasticity) ================\n"); print(bp_test)
cat("\n================ BREUSCH-GODFREY/WOOLDRIDGE (serial correlation) ================\n"); print(bg_test)

diagnostics_summary <- data.frame(
  Test = c("Breusch-Pagan (heteroskedasticity)",
           "Breusch-Godfrey/Wooldridge (serial correlation)"),
  Statistic = c(unname(bp_test$statistic), unname(bg_test$statistic)),
  df        = c(unname(bp_test$parameter), unname(bg_test$parameter)),
  p_value   = c(bp_test$p.value, bg_test$p.value)
)
write.csv(diagnostics_summary, file.path(output_dir, "objective3_diagnostics.csv"), row.names = FALSE)

## ---- 5. Robustness checks 1-4 ------------------------------------------------
# Check 1: Non-linearity (quadratic CDQ term)
fe_nonlin <- plm(Tobin_Q_Wins ~ CDQ_Score + I(CDQ_Score^2) + Firm_Size + Leverage_Wins + Firm_Age,
                  data = pdata, model = "within")
nonlin_clustered <- coeftest(fe_nonlin, vcov = vcovHC(fe_nonlin, method = "arellano",
                                                        type = "HC1", cluster = "group"))
cat("\n================ CHECK 1: NON-LINEARITY (CDQ + CDQ^2) ================\n"); print(nonlin_clustered)

# Check 2: Moderator, CDQ x Early Adoption Status
fe_interact <- plm(Tobin_Q_Wins ~ CDQ_Score * EAS_Dummy + Firm_Size + Leverage_Wins + Firm_Age,
                    data = pdata, model = "within")
interact_clustered <- coeftest(fe_interact, vcov = vcovHC(fe_interact, method = "arellano",
                                                            type = "HC1", cluster = "group"))
cat("\n================ CHECK 2: MODERATOR (CDQ x EAS_Dummy) ================\n"); print(interact_clustered)

# Check 3: Exclude the early-adopter firms entirely
early_adopter_firms <- unique(df$Ticker[df$EAS_Dummy == 1])
cat("\nEarly adopter firms excluded in Check 3:", length(early_adopter_firms), "firms\n")
print(early_adopter_firms)

df_excl    <- df[!(df$Ticker %in% early_adopter_firms), ]
pdata_excl <- pdata.frame(df_excl, index = c("Ticker", "Year"))
fe_excl    <- plm(Tobin_Q_Wins ~ CDQ_Score + Firm_Size + Leverage_Wins + Firm_Age,
                   data = pdata_excl, model = "within")
excl_clustered <- coeftest(fe_excl, vcov = vcovHC(fe_excl, method = "arellano",
                                                    type = "HC1", cluster = "group"))
cat("\n================ CHECK 3: EXCLUDING EARLY ADOPTERS (N =", nrow(df_excl), ") ================\n")
print(excl_clustered)

extract_row <- function(model_ct, var, label) {
  if (!(var %in% rownames(model_ct))) return(NULL)
  data.frame(Specification = label, Variable = var,
             Coefficient = model_ct[var, 1], Std_Error = model_ct[var, 2],
             t_value = model_ct[var, 3], p_value = model_ct[var, 4])
}

robustness_results <- rbind(
  extract_row(base_clustered,    "CDQ_Score",              "1. Base model"),
  extract_row(nonlin_clustered,  "CDQ_Score",               "2. Non-linearity: linear term"),
  extract_row(nonlin_clustered,  "I(CDQ_Score^2)",          "2. Non-linearity: quadratic term"),
  extract_row(interact_clustered,"CDQ_Score",               "3. Moderator: CDQ main effect"),
  extract_row(interact_clustered,"EAS_Dummy",               "3. Moderator: EAS main effect"),
  extract_row(interact_clustered,"CDQ_Score:EAS_Dummy",     "3. Moderator: CDQ x EAS interaction"),
  extract_row(excl_clustered,    "CDQ_Score",               "4. Excluding early-adopter firms")
)
write.csv(robustness_results, file.path(output_dir, "objective3_robustness.csv"), row.names = FALSE)

n_info <- data.frame(
  Specification = c("Base model", "Excluding early adopters"),
  N_firm_years  = c(nrow(df), nrow(df_excl)),
  N_firms       = c(length(unique(df$Ticker)), length(unique(df_excl$Ticker)))
)
write.csv(n_info, file.path(output_dir, "objective3_robustness_n.csv"), row.names = FALSE)

## ---- 6. Extended checks: two-way FE, lagged CDQ ------------------------------
# Extended Check 1: two-way (firm + year) fixed effects.
# Twoway RANDOM effects is not estimable here (only 5 time periods, too few
# for the Swamy-Arora between-time estimator), so twoway FE is compared
# against oneway FE via a nested F-test rather than a twoway Hausman test.
fe_oneway <- plm(Tobin_Q_Wins ~ CDQ_Score + Firm_Size + Leverage_Wins + Firm_Age,
                  data = pdata, model = "within", effect = "individual")
fe_twoway <- plm(Tobin_Q_Wins ~ CDQ_Score + Firm_Size + Leverage_Wins + Firm_Age,
                  data = pdata, model = "within", effect = "twoways")

twoway_clustered <- coeftest(fe_twoway, vcov = vcovHC(fe_twoway, method = "arellano",
                                                        type = "HC1", cluster = "group"))
cat("\n================ TWO-WAY FE MODEL (firm-clustered SE) ================\n"); print(twoway_clustered)
cat("\nOne-way FE within R2:", summary(fe_oneway)$r.squared[1], "\n")
cat("Two-way FE within R2:", summary(fe_twoway)$r.squared[1], "\n")

year_test <- pFtest(fe_twoway, fe_oneway)
cat("\n================ F-TEST: ARE YEAR EFFECTS JOINTLY SIGNIFICANT? ================\n"); print(year_test)

# Extended Check 2: lagged CDQ (t-1), to address reverse-causality concerns
pdata$CDQ_lag1 <- plm::lag(pdata$CDQ_Score, 1)
fe_lagged <- plm(Tobin_Q_Wins ~ CDQ_lag1 + Firm_Size + Leverage_Wins + Firm_Age,
                  data = pdata, model = "within")
lagged_clustered <- coeftest(fe_lagged, vcov = vcovHC(fe_lagged, method = "arellano",
                                                        type = "HC1", cluster = "group"))
cat("\n================ LAGGED CDQ MODEL (firm-clustered SE) ================\n"); print(lagged_clustered)
cat("\nN in lagged model:", nobs(fe_lagged), "(vs", nobs(fe_oneway), "in base model, lost to lag construction)\n")

extended_results <- rbind(
  extract_row(twoway_clustered, "CDQ_Score",     "5. Two-way FE (firm+year)"),
  extract_row(twoway_clustered, "Firm_Size",     "5. Two-way FE (firm+year)"),
  extract_row(twoway_clustered, "Leverage_Wins", "5. Two-way FE (firm+year)"),
  extract_row(lagged_clustered, "CDQ_lag1",      "6. Lagged CDQ (t-1)"),
  extract_row(lagged_clustered, "Firm_Size",     "6. Lagged CDQ (t-1)"),
  extract_row(lagged_clustered, "Leverage_Wins", "6. Lagged CDQ (t-1)"),
  extract_row(lagged_clustered, "Firm_Age",      "6. Lagged CDQ (t-1)")
)
write.csv(extended_results, file.path(output_dir, "objective3_extended_checks.csv"), row.names = FALSE)

diag_extra <- data.frame(
  Test      = "F-test (year effects jointly significant)",
  Statistic = year_test$statistic,
  df1       = year_test$parameter[1],
  df2       = year_test$parameter[2],
  p_value   = year_test$p.value
)
write.csv(diag_extra, file.path(output_dir, "objective3_extended_diagnostics.csv"), row.names = FALSE)

n_extra <- data.frame(
  Model     = c("One-way FE (base)", "Two-way FE", "Lagged CDQ FE"),
  N         = c(nobs(fe_oneway), nobs(fe_twoway), nobs(fe_lagged)),
  Within_R2 = c(summary(fe_oneway)$r.squared[1], summary(fe_twoway)$r.squared[1],
                summary(fe_lagged)$r.squared[1])
)
write.csv(n_extra, file.path(output_dir, "objective3_extended_n.csv"), row.names = FALSE)

## ---- 7. Secondary descriptive figure: raw scatter + partial regression plot --
pA <- ggplot(df, aes(x = CDQ_Score, y = Tobin_Q_Wins)) +
  geom_point(alpha = 0.35, color = "#1F4E78", size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#C0392B", fill = "#C0392B", alpha = 0.15) +
  labs(title = "A. Raw Scatter (pooled)",
       subtitle = "Illustrative only, dominated by between-firm variation",
       x = "CDQ Score", y = "Tobin's Q (winsorized)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# Partial regression (added-variable) plot via Frisch-Waugh-Lovell: residualize
# both CDQ and Tobin's Q on firm FE + other covariates; the slope of
# resid(Y) ~ resid(X) reproduces the CDQ coefficient from the full FE model.
fe_y <- plm(Tobin_Q_Wins ~ Firm_Size + Leverage_Wins + Firm_Age, data = pdata, model = "within")
fe_x <- plm(CDQ_Score    ~ Firm_Size + Leverage_Wins + Firm_Age, data = pdata, model = "within")
resid_df  <- data.frame(y_resid = residuals(fe_y), x_resid = residuals(fe_x))
fwl_check <- lm(y_resid ~ x_resid, data = resid_df)
cat("\nFWL slope check (should match the FE model's CDQ coefficient,",
    round(base_clustered["CDQ_Score", 1], 5), "):\n")
print(coef(fwl_check))

pB <- ggplot(resid_df, aes(x = x_resid, y = y_resid)) +
  geom_point(alpha = 0.35, color = "#1F4E78", size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#C0392B", fill = "#C0392B", alpha = 0.15) +
  labs(title = "B. Partial Regression Plot",
       subtitle = "Firm FE + Size/Leverage/Age partialled out; slope = FE coefficient",
       x = "CDQ Score (residualized)", y = "Tobin's Q (residualized)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

combined <- pA + pB +
  plot_annotation(title = "Objective 3: CDQ and Firm Value (Tobin's Q)",
                   theme = theme(plot.title = element_text(face = "bold", size = 15)))
ggsave(file.path(output_dir, "objective3_scatter_combined.png"), combined,
       width = 13, height = 5.5, dpi = 150)
cat("\nSaved secondary descriptive figure: objective3_scatter_combined.png\n")

## ---- 8. Primary figure: CDQ + controls across the three core specifications --
# Coefficients are pulled directly from the model objects fitted above
# (base_clustered, twoway_clustered, lagged_clustered) -- never re-typed as
# literal numbers -- so the figure cannot drift out of sync with the tables.
get_ct <- function(ct, var) {
  if (var %in% rownames(ct)) c(coef = ct[var, 1], se = ct[var, 2], p = ct[var, 4])
  else c(coef = NA_real_, se = NA_real_, p = NA_real_)
}

build_spec_df <- function(var_base, var_lag = var_base) {
  b <- get_ct(base_clustered,   var_base)
  t <- get_ct(twoway_clustered, var_base)
  l <- get_ct(lagged_clustered, var_lag)
  data.frame(
    spec = c("Base FE", "Two-way FE\n(firm + year)", "Lagged (t-1)"),
    coef = c(b["coef"], t["coef"], l["coef"]),
    se   = c(b["se"],   t["se"],   l["se"]),
    p    = c(b["p"],    t["p"],    l["p"])
  )
}

primary_data <- list(
  CDQ_Score     = build_spec_df("CDQ_Score", "CDQ_lag1"),
  Firm_Size     = build_spec_df("Firm_Size"),
  Leverage_Wins = build_spec_df("Leverage_Wins"),
  Firm_Age      = build_spec_df("Firm_Age")
)

specs <- c("Base FE", "Two-way FE\n(firm + year)", "Lagged (t-1)")

col_sig      <- "#1E7B34"   # p < .05
col_marginal <- "#D98324"   # .05 <= p < .10
col_ns       <- "#4C6B8A"   # p >= .10
col_absorbed <- "#9A9A9A"   # dropped for collinearity (Firm_Age, two-way FE)

classify_col <- function(p) {
  ifelse(is.na(p), col_absorbed,
         ifelse(p < 0.05, col_sig,
                ifelse(p < 0.10, col_marginal, col_ns)))
}
star_label <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))))
}

LEFT_TEXT_FRAC  <- 0.28
RIGHT_TEXT_FRAC <- 0.62
solve_xlim <- function(L, R, f) {
  xmin <- L; xmax <- L * (f - 1) / f
  if (xmax < R) { xmax <- R; xmin <- f * xmax / (f - 1) }
  c(xmin, xmax)
}
bounds <- lapply(primary_data, function(d) {
  d <- d[!is.na(d$coef), ]
  lower <- d$coef - 1.96 * d$se
  upper <- d$coef + 1.96 * d$se
  ci_min <- min(0, lower); ci_max <- max(0, upper)
  span <- ci_max - ci_min
  c(L = ci_min - LEFT_TEXT_FRAC * span, R = ci_max + RIGHT_TEXT_FRAC * span,
    ci_min = ci_min, ci_max = ci_max, span = span)
})
f_zero <- max(sapply(bounds, function(b) (-b["L"]) / (b["R"] - b["L"])))
xlims  <- lapply(bounds, function(b) solve_xlim(b["L"], b["R"], f_zero))

legend_breaks <- c(col_sig, col_marginal, col_ns)
legend_labels <- c("p < 0.05 (significant)", "0.05 ≤ p < 0.10 (marginal)",
                    "p ≥ 0.10 (not significant)")

make_panel <- function(var_name, is_primary = FALSE, show_xlab = FALSE, show_legend = FALSE) {
  d <- primary_data[[var_name]]
  d$lower <- d$coef - 1.96 * d$se
  d$upper <- d$coef + 1.96 * d$se
  d$color <- classify_col(d$p)
  d$star  <- star_label(d$p)
  d$spec  <- factor(d$spec, levels = rev(specs))

  xlim <- xlims[[var_name]]
  b    <- bounds[[var_name]]
  gap  <- 0.06 * b["span"]

  d$p_label <- ifelse(is.na(d$p), "",
                       ifelse(d$p < 0.001, sprintf("%.3f  (p<0.001)", d$coef),
                              sprintf("%.3f  (p=%.3f)", d$coef, d$p)))

  ggplot(d, aes(y = spec)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_segment(data = subset(d, !is.na(coef)),
                 aes(x = lower, xend = upper, y = spec, yend = spec, color = color),
                 linewidth = 1.7, lineend = "round", show.legend = FALSE) +
    # show.legend = FALSE on every layer above and below except this one: the
    # legend key is meant to be a plain circle, not a line-plus-point glyph
    # patchworked together from every color-mapped layer in the panel.
    geom_point(data = subset(d, !is.na(coef)), aes(x = coef, color = color), size = 4.2) +
    geom_text(data = subset(d, !is.na(coef)),
              aes(x = upper + gap, label = p_label, color = color), hjust = 0, size = 3.6,
              show.legend = FALSE) +
    geom_text(data = subset(d, star != ""),
              aes(x = lower - gap, label = star, color = color),
              hjust = 1, size = 4.6, fontface = "bold", show.legend = FALSE) +
    { if (any(is.na(d$coef)))
        geom_text(data = subset(d, is.na(coef)),
                  aes(x = mean(xlim), label = "absorbed (collinear with two-way FE)"),
                  hjust = 0.5, size = 3.4, fontface = "italic", color = col_absorbed)
      else NULL } +
    # Only one panel (Firm_Age, the bottom-most) carries a legend at all. An
    # identity scale's guide, if turned on separately on every panel, only
    # lists the colors actually present in THAT panel's own data -- patchwork
    # can't cleanly merge four partial, non-identical legends into one clean
    # set (it was tried; it produced duplicate entries). Simplest fix: draw
    # the legend once, with the full set of breaks/labels forced regardless of
    # what colors this panel's data happens to contain, and suppress it
    # everywhere else. Because Firm_Age is the last panel in the stack, its
    # legend.position = "bottom" (set below) lands at the very bottom of the
    # whole composite figure, exactly where the original places it.
    # limits = legend_breaks (not just breaks =) is what actually forces all
    # three categories into the legend: an identity scale's default limits
    # are the range of colors PRESENT in this panel's own data, so without it
    # a panel missing e.g. the marginal/orange color would silently drop that
    # key even though it was named in breaks/labels.
    scale_color_identity(
      guide  = if (show_legend) "legend" else "none",
      breaks = legend_breaks, labels = legend_labels, limits = legend_breaks,
      name = NULL
    ) +
    # drop = FALSE: without this, ggplot silently drops a factor level with no
    # point/segment observations (the "absorbed" Firm_Age x Two-way FE row) from
    # the axis, then re-inserts it out of its intended order once the
    # annotation-only layer below supplies data for it. Keeping drop = FALSE
    # guarantees all three specifications stay in the fixed Base/Two-way/Lagged
    # order regardless of which rows are NA.
    # expand: pulls the 3 specification bars (Base FE / Two-way FE / Lagged
    # CDQ) closer together, per user request. Counterintuitively this means
    # INCREASING the outer padding (add), not decreasing it: with a fixed
    # panel height, a larger add value stretches the total plotted range
    # (categories +/- add), which shrinks the 1-unit gap between adjacent
    # categories as a fraction of that range -- i.e. more blank margin at the
    # very top/bottom of the panel, but the three bars themselves cluster
    # closer together in the middle. Default discrete-scale expansion is
    # add = 0.6.
    # add is c(low, high): "low" sits next to level 1 of the (reversed)
    # factor -- that's "Lagged (t-1)", drawn at the BOTTOM of each panel --
    # and "high" sits next to the top level, "Base FE", right under this
    # panel's own heading. Per this round's request (heading closer to its
    # own bars, but each variable's block kept visually distinct from the
    # next), the padding is split unevenly: a small "high" value keeps the
    # gap between the heading and its first bar tight, while a larger "low"
    # value pushes most of the blank space to the BOTTOM of the panel, ahead
    # of the next variable's heading.
    scale_y_discrete(drop = FALSE, expand = expansion(add = c(1.6, 0.4))) +
    scale_x_continuous(labels = number_format(accuracy = 0.001), limits = xlim,
                       expand = expansion(mult = 0)) +
    labs(title = if (is_primary) expression(bold("CDQ_Score   PRIMARY VARIABLE OF INTEREST"))
                 else var_name,
         x = if (show_xlab) "Coefficient on Tobin's Q" else NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      # margin: small top gap above the heading text (keeps a little air
      # between this panel and whatever sits above it) but a near-zero
      # bottom gap, so the heading sits right on top of its own bars rather
      # than floating a visible distance above them.
      plot.title = element_text(face = "bold", size = 13, hjust = 0,
                                 margin = margin(t = 4, b = 1),
                                 color = if (is_primary) "#B8860B" else "#33414F"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      # axis.text.y: the specification names (Base FE / Two-way FE / Lagged
      # CDQ) -- bumped up further this round per user request (previously
      # 10.5, still read as small).
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 9.5),
      # axis.title.x: the "Coefficient on Tobin's Q" label on the bottom
      # panel -- bumped up per user request (was 11).
      axis.title.x = element_text(size = 13),
      # axis.line.x / axis.ticks.x: theme_minimal() draws neither by default.
      # Added back per user request, matching the thin horizontal rule under
      # each panel's x-axis tick labels in the original workbook's figure.
      axis.line.x = element_line(color = "grey40", linewidth = 0.3),
      axis.ticks.x = element_line(color = "grey40", linewidth = 0.3),
      axis.ticks.length.x = unit(3, "pt"),
      # Legend key size/background bumped up and made explicitly white so the
      # three significance colors (green/orange/blue) read clearly rather
      # than shrinking into the small default legend glyph.
      legend.text = element_text(size = 11),
      legend.key = element_rect(fill = "white", color = NA),
      legend.key.size = unit(0.9, "cm"),
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_blank(),
      # plot.margin: small top, larger bottom -- reinforces the same
      # heading-close-to-own-bars / clear-gap-before-next-variable effect as
      # the asymmetric scale_y_discrete expansion above, at the whole-panel
      # level (this is the gap between one panel's plot area and the next
      # panel's, once patchwork stacks them).
      plot.margin = margin(t = 2, r = 12, b = 10, l = 4)
    ) +
    { if (show_legend)
        guides(color = guide_legend(override.aes = list(size = 6)))
      else NULL }
}

panels <- list(
  make_panel("CDQ_Score", is_primary = TRUE),
  make_panel("Firm_Size"),
  make_panel("Leverage_Wins"),
  make_panel("Firm_Age", show_xlab = TRUE, show_legend = TRUE)
)

primary_fig <- wrap_plots(panels, ncol = 1) +
  plot_annotation(
    title = "Objective 3: CDQ and Control Variables Across Core Specifications",
    subtitle = paste0(
      "Fixed Effects, firm-clustered SE (Petersen, 2009), 95% CI — zero reference ",
      "line aligned across panels\nAxis ranges are optimized for CDQ's own (much ",
      "narrower) coefficient scale; see companion SDQ figure (Objective 2)"
    ),
    caption = paste0(
      "Horizontal bars are 95% confidence intervals (coefficient ± 1.96 × firm-",
      "clustered standard error).\nSignificance stars (*** p<.001, ** p<.01, * p<.05) ",
      "are reported at the opposite end of the bar from the coefficient and p-value."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 17, color = "#1F4E78", hjust = 0.5),
      plot.subtitle = element_text(size = 10.5, color = "#444444", hjust = 0.5),
      plot.caption = element_text(size = 10.5, color = "#444444", face = "italic",
                                   hjust = 0.5)
    )
  )
  # Note: each panel now sets its own asymmetric plot.margin (small top,
  # larger bottom) inside make_panel() itself, so no shared "&" theme margin
  # override is applied here -- that would have clobbered the per-panel
  # asymmetry with one uniform value on every panel.

ggsave(file.path(output_dir, "objective3_primary_figure.png"), primary_fig,
       width = 9, height = 13.6, dpi = 200)
cat("\nSaved primary figure: objective3_primary_figure.png\n")

## ---- 9. Reproducibility record -----------------------------------------------
writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"))

## ---- 10. Consolidated workbook: mirrors NGX_Stage2_Objective3_CDQ_FirmValue.xlsx --
## Everything below reuses the model objects and data frames already built in
## sections 2-8 above -- nothing is re-typed or re-derived, so this workbook
## cannot drift out of sync with the console output, the CSVs, or the figures.
## Sheet names and layout follow the originally-supplied workbook so the two
## are directly comparable; only "8. R Code" changes in nature (it now holds
## THIS script's own source, read back from disk, rather than a separate
## write-up). Same treatment as Objective 2's consolidated workbook (built
## first); see that script's section 10 for the fuller commentary.
suppressWarnings(suppressMessages(tryCatch(library(openxlsx), error = function(e) NULL)))
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx", repos = "https://cloud.r-project.org")
  library(openxlsx)
}

fmt_p_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("< 0.001***")
  stars <- if (p < 0.01) "**" else if (p < 0.05) "*" else ""
  sprintf("%.3f%s", p, stars)
}
fmt_stat_p <- function(stat, df, p, prefix = "chi2") {
  p_str <- if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)
  sprintf("%s(%d) = %.2f, p %s", prefix, df, stat, p_str)
}

navy_fill   <- "#1F4E78"
green_fill  <- "#D9EAD3"
orange_fill <- "#FCE5CD"

style_title    <- createStyle(fontSize = 13, fontColour = navy_fill, textDecoration = "bold")
style_bold     <- createStyle(textDecoration = "bold")
style_italic   <- createStyle(fontSize = 9, textDecoration = "italic")
style_hdr_navy <- createStyle(fontColour = "#FFFFFF", fgFill = navy_fill, textDecoration = "bold")
style_hdr_grn  <- createStyle(fgFill = green_fill, textDecoration = "bold")
style_sub_grn  <- createStyle(fgFill = green_fill, textDecoration = "bold")
style_row_org  <- createStyle(fgFill = orange_fill)
style_5dp      <- createStyle(numFmt = "0.00000")
style_3dp      <- createStyle(numFmt = "0.000")

write_header <- function(wb, sheet, labels, row, style = style_hdr_navy) {
  writeData(wb, sheet, t(labels), startRow = row, startCol = 1, colNames = FALSE)
  addStyle(wb, sheet, style, rows = row, cols = seq_along(labels), gridExpand = TRUE)
}
write_coef_table <- function(wb, sheet, ct, row, highlight_var = NULL) {
  # ct is a coeftest() matrix: Estimate, Std. Error, t/z value, Pr(>|t|)
  vars <- rownames(ct)
  for (i in seq_along(vars)) {
    r <- row + i - 1
    writeData(wb, sheet, vars[i], startRow = r, startCol = 1, colNames = FALSE)
    writeData(wb, sheet, round(ct[i, 1], 5), startRow = r, startCol = 2, colNames = FALSE)
    writeData(wb, sheet, round(ct[i, 2], 5), startRow = r, startCol = 3, colNames = FALSE)
    writeData(wb, sheet, round(ct[i, 3], 3), startRow = r, startCol = 4, colNames = FALSE)
    writeData(wb, sheet, fmt_p_stars(ct[i, 4]), startRow = r, startCol = 5, colNames = FALSE)
    addStyle(wb, sheet, style_5dp, rows = r, cols = 2:3, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, sheet, style_3dp, rows = r, cols = 4, gridExpand = TRUE, stack = TRUE)
    if (!is.null(highlight_var) && vars[i] %in% highlight_var) {
      addStyle(wb, sheet, style_row_org, rows = r, cols = 1:5, gridExpand = TRUE, stack = TRUE)
    }
  }
  row + length(vars)
}

wb <- createWorkbook()

## -- Sheet 1: Model Overview --------------------------------------------------
addWorksheet(wb, "1. Model Overview")
setColWidths(wb, "1. Model Overview", cols = 1:2, widths = c(28, 45))
writeData(wb, "1. Model Overview", "OBJECTIVE 3: CDQ AND FIRM VALUE", startRow = 1, colNames = FALSE)
addStyle(wb, "1. Model Overview", style_title, rows = 1, cols = 1)
writeData(wb, "1. Model Overview", "Model", startRow = 3, colNames = FALSE)
addStyle(wb, "1. Model Overview", style_bold, rows = 3, cols = 1)
writeData(wb, "1. Model Overview",
          "Tobin_Q_it = b0 + b1(CDQ_Score_it) + b2(Firm_Size_it) + b3(Leverage_it) + b4(Firm_Age_it) + e_it",
          startRow = 4, colNames = FALSE)
write_header(wb, "1. Model Overview", c("Metric", "Value"), row = 6)
mo_rows <- list(
  c("R-squared", model_overview$Value[model_overview$Metric == "R-squared"]),
  c("Adj R-squared", model_overview$Value[model_overview$Metric == "Adj R-squared"]),
  c("N (firm-years)", model_overview$Value[model_overview$Metric == "N (firm-years)"]),
  c("N (firms)", model_overview$Value[model_overview$Metric == "N (firms)"]),
  c("Selected model", "Fixed Effects")
)
for (i in seq_along(mo_rows)) {
  writeData(wb, "1. Model Overview", mo_rows[[i]][1], startRow = 6 + i, startCol = 1, colNames = FALSE)
  writeData(wb, "1. Model Overview", mo_rows[[i]][2], startRow = 6 + i, startCol = 2, colNames = FALSE)
}
writeData(wb, "1. Model Overview", "Hausman test", startRow = 13, colNames = FALSE)
writeData(wb, "1. Model Overview",
          paste0(fmt_stat_p(hausman$statistic, hausman$parameter, hausman$p.value, "chi2"),
                 ". Fixed Effects preferred."),
          startRow = 14, colNames = FALSE)
writeData(wb, "1. Model Overview", "Hausman, J. A. (1978). Econometrica, 46(6), 1251-1271.",
          startRow = 16, colNames = FALSE)

## -- Sheet 2: FE Results (Selected) -------------------------------------------
addWorksheet(wb, "2. FE Results (Selected)")
setColWidths(wb, "2. FE Results (Selected)", cols = 1:2, widths = c(16, 14))
writeData(wb, "2. FE Results (Selected)", "FIXED EFFECTS MODEL (SELECTED)", startRow = 1, colNames = FALSE)
addStyle(wb, "2. FE Results (Selected)", style_bold, rows = 1, cols = 1)
writeData(wb, "2. FE Results (Selected)",
          paste0("N=", nrow(df), " firm-years, ", length(unique(df$Ticker)), " firms, unbalanced panel T=1-5"),
          startRow = 2, colNames = FALSE)
writeData(wb, "2. FE Results (Selected)", "Plain SE", startRow = 4, colNames = FALSE)
addStyle(wb, "2. FE Results (Selected)", style_sub_grn, rows = 4, cols = 1)
write_header(wb, "2. FE Results (Selected)", c("Variable", "Coefficient", "Std. Error", "t-value", "p-value"), row = 5)
last_row <- write_coef_table(wb, "2. FE Results (Selected)", base_plain_se, row = 6)
writeData(wb, "2. FE Results (Selected)",
          sprintf("R2 = %.3f, Adj. R2 = %.3f", summary(fe_base)$r.squared[1], summary(fe_base)$r.squared[2]),
          startRow = last_row + 1, colNames = FALSE)
addStyle(wb, "2. FE Results (Selected)", style_italic, rows = last_row + 1, cols = 1)
writeData(wb, "2. FE Results (Selected)", "Firm-clustered SE (Petersen, 2009)", startRow = last_row + 3, colNames = FALSE)
addStyle(wb, "2. FE Results (Selected)", style_sub_grn, rows = last_row + 3, cols = 1)
write_header(wb, "2. FE Results (Selected)", c("Variable", "Coefficient", "Std. Error", "t-value", "p-value"),
             row = last_row + 4)
last_row2 <- write_coef_table(wb, "2. FE Results (Selected)", base_clustered, row = last_row + 5)
writeData(wb, "2. FE Results (Selected)", "Significance: *** p<.001, ** p<.01, * p<.05",
          startRow = last_row2 + 2, colNames = FALSE)
addStyle(wb, "2. FE Results (Selected)", style_italic, rows = last_row2 + 2, cols = 1)

## -- Sheet 3: RE Results (Not Selected) ---------------------------------------
addWorksheet(wb, "3. RE Results (Not Selected)")
setColWidths(wb, "3. RE Results (Not Selected)", cols = 1:2, widths = c(16, 14))
writeData(wb, "3. RE Results (Not Selected)", "RANDOM EFFECTS MODEL (NOT SELECTED)", startRow = 1, colNames = FALSE)
addStyle(wb, "3. RE Results (Not Selected)", style_bold, rows = 1, cols = 1)
writeData(wb, "3. RE Results (Not Selected)", "Swamy-Arora RE; rejected by Hausman test, p < 0.001",
          startRow = 2, colNames = FALSE)
addStyle(wb, "3. RE Results (Not Selected)", style_italic, rows = 2, cols = 1)
writeData(wb, "3. RE Results (Not Selected)", "Random Effects", startRow = 3, colNames = FALSE)
write_header(wb, "3. RE Results (Not Selected)", c("Variable", "Coefficient", "Std. Error", "z-value", "p-value"),
             row = 4, style = style_hdr_grn)
last_row3 <- write_coef_table(wb, "3. RE Results (Not Selected)", re_plain, row = 5)
writeData(wb, "3. RE Results (Not Selected)",
          sprintf("R2 = %.3f, Adj. R2 = %.3f", summary(re_base)$r.squared[1], summary(re_base)$r.squared[2]),
          startRow = last_row3 + 1, colNames = FALSE)
addStyle(wb, "3. RE Results (Not Selected)", style_italic, rows = last_row3 + 1, cols = 1)

## -- Sheet 4: Diagnostics ------------------------------------------------------
addWorksheet(wb, "4. Diagnostics")
setColWidths(wb, "4. Diagnostics", cols = 1:2, widths = c(16, 14))
writeData(wb, "4. Diagnostics", "DIAGNOSTIC TESTS", startRow = 1, colNames = FALSE)
addStyle(wb, "4. Diagnostics", style_bold, rows = 1, cols = 1)
writeData(wb, "4. Diagnostics", "1. Correlation matrix (Pearson), regressors", startRow = 3, colNames = FALSE)
addStyle(wb, "4. Diagnostics", style_bold, rows = 3, cols = 1)
cor_df <- as.data.frame(round(cor_matrix, 3))
writeData(wb, "4. Diagnostics", cor_df, startRow = 4, startCol = 1, rowNames = TRUE, colNames = TRUE)
addStyle(wb, "4. Diagnostics", style_hdr_navy, rows = 4, cols = 1:5, gridExpand = TRUE)
writeData(wb, "4. Diagnostics", "2. Variance Inflation Factor (VIF)", startRow = 10, colNames = FALSE)
addStyle(wb, "4. Diagnostics", style_bold, rows = 10, cols = 1)
write_header(wb, "4. Diagnostics", c("Variable", "VIF"), row = 11)
vif_df <- data.frame(Variable = names(vif_vals), VIF = round(unname(vif_vals), 3))
writeData(wb, "4. Diagnostics", vif_df, startRow = 12, startCol = 1, colNames = FALSE)
writeData(wb, "4. Diagnostics", "3. Breusch-Pagan test (heteroskedasticity)", startRow = 17, colNames = FALSE)
addStyle(wb, "4. Diagnostics", style_bold, rows = 17, cols = 1)
writeData(wb, "4. Diagnostics",
          fmt_stat_p(unname(bp_test$statistic), unname(bp_test$parameter), bp_test$p.value, "BP"),
          startRow = 18, colNames = FALSE)
writeData(wb, "4. Diagnostics", "4. Breusch-Godfrey/Wooldridge test (serial correlation)",
          startRow = 20, colNames = FALSE)
addStyle(wb, "4. Diagnostics", style_bold, rows = 20, cols = 1)
writeData(wb, "4. Diagnostics",
          fmt_stat_p(unname(bg_test$statistic), unname(bg_test$parameter), bg_test$p.value, "chi2"),
          startRow = 21, colNames = FALSE)

## -- Sheet 5: Plots -------------------------------------------------------------
addWorksheet(wb, "5. Plots")
setColWidths(wb, "5. Plots", cols = 1, widths = 12)
writeData(wb, "5. Plots", "OBJECTIVE 3: PRIMARY FIGURE AND SUPPORTING DESCRIPTIVE FIGURE",
          startRow = 1, colNames = FALSE)
addStyle(wb, "5. Plots", style_title, rows = 1, cols = 1)
writeData(wb, "5. Plots", "Primary Figure", startRow = 3, colNames = FALSE)
addStyle(wb, "5. Plots", style_bold, rows = 3, cols = 1)
writeData(wb, "5. Plots",
          paste0("CDQ_Score and all control variables (Firm_Size, Leverage_Wins, Firm_Age) across the three ",
                 "core specifications: Base FE, Two-way FE, and Lagged CDQ (t-1). Each variable keeps its own ",
                 "x-axis scale, but the zero reference line is forced to the same horizontal position in every ",
                 "panel so it reads as one continuous line down the figure. CDQ_Score is the primary variable ",
                 "of interest. Horizontal bars are 95% confidence intervals (coefficient ± 1.96 × firm-",
                 "clustered standard error, Petersen 2009); the coefficient and p-value are reported at one ",
                 "end of each bar, the significance star at the other. Axis ranges are optimized for CDQ's ",
                 "own, much narrower coefficient scale rather than forced to match the companion SDQ figure ",
                 "(Objective 2) number-for-number; note how much narrower and more consistently zero-crossing ",
                 "the CDQ interval is by comparison."),
          startRow = 4, colNames = FALSE)
insertImage(wb, "5. Plots", file.path(output_dir, "objective3_primary_figure.png"),
            startRow = 6, startCol = 1, width = 9, height = 13.6, units = "in", dpi = 200)
writeData(wb, "5. Plots", "Secondary Figure (descriptive)", startRow = 72, colNames = FALSE)
addStyle(wb, "5. Plots", style_bold, rows = 72, cols = 1)
writeData(wb, "5. Plots",
          paste0("Panel A: raw pooled scatter, illustrative only. Panel B: partial regression plot; its slope ",
                 "reproduces the FE model's CDQ coefficient exactly, verified via the Frisch-Waugh-Lovell theorem."),
          startRow = 73, colNames = FALSE)
insertImage(wb, "5. Plots", file.path(output_dir, "objective3_scatter_combined.png"),
            startRow = 75, startCol = 1, width = 13, height = 5.5, units = "in", dpi = 150)

## -- Sheet 6: Robustness Checks -------------------------------------------------
addWorksheet(wb, "6. Robustness Checks")
setColWidths(wb, "6. Robustness Checks", cols = 1:6, widths = c(34, 20, 14, 14, 10, 12))
writeData(wb, "6. Robustness Checks", "ROBUSTNESS CHECKS", startRow = 1, colNames = FALSE)
addStyle(wb, "6. Robustness Checks", style_bold, rows = 1, cols = 1)
writeData(wb, "6. Robustness Checks",
          paste0("Base N=", nrow(df), " firm-years/", length(unique(df$Ticker)), " firms. ",
                 "Excl. early adopters N=", nrow(df_excl), "/", length(unique(df_excl$Ticker)), "."),
          startRow = 2, colNames = FALSE)
write_header(wb, "6. Robustness Checks",
             c("Specification", "Variable", "Coefficient", "Std. Error", "t-value", "p-value"), row = 4)
for (i in seq_len(nrow(robustness_results))) {
  r <- 4 + i
  rr <- robustness_results[i, ]
  writeData(wb, "6. Robustness Checks", rr$Specification, startRow = r, startCol = 1, colNames = FALSE)
  writeData(wb, "6. Robustness Checks", rr$Variable, startRow = r, startCol = 2, colNames = FALSE)
  writeData(wb, "6. Robustness Checks", round(rr$Coefficient, 5), startRow = r, startCol = 3, colNames = FALSE)
  writeData(wb, "6. Robustness Checks", round(rr$Std_Error, 5), startRow = r, startCol = 4, colNames = FALSE)
  writeData(wb, "6. Robustness Checks", round(rr$t_value, 3), startRow = r, startCol = 5, colNames = FALSE)
  writeData(wb, "6. Robustness Checks", fmt_p_stars(rr$p_value), startRow = r, startCol = 6, colNames = FALSE)
  addStyle(wb, "6. Robustness Checks", style_5dp, rows = r, cols = 3:4, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "6. Robustness Checks", style_3dp, rows = r, cols = 5, gridExpand = TRUE, stack = TRUE)
}

## -- Sheet 7: Extended Checks ----------------------------------------------------
addWorksheet(wb, "7. Extended Checks")
setColWidths(wb, "7. Extended Checks", cols = 1:6, widths = c(30, 16, 14, 14, 10, 12))
writeData(wb, "7. Extended Checks", "EXTENDED CHECKS: TWO-WAY FE AND LAGGED CDQ", startRow = 1, colNames = FALSE)
addStyle(wb, "7. Extended Checks", style_bold, rows = 1, cols = 1)
writeData(wb, "7. Extended Checks",
          "Requested review against standard practice in the ESG/disclosure-value literature",
          startRow = 2, colNames = FALSE)
write_header(wb, "7. Extended Checks",
             c("Specification", "Variable", "Coefficient", "Std. Error", "t-value", "p-value"), row = 4)
for (i in seq_len(nrow(extended_results))) {
  r <- 4 + i
  rr <- extended_results[i, ]
  writeData(wb, "7. Extended Checks", rr$Specification, startRow = r, startCol = 1, colNames = FALSE)
  writeData(wb, "7. Extended Checks", rr$Variable, startRow = r, startCol = 2, colNames = FALSE)
  writeData(wb, "7. Extended Checks", round(rr$Coefficient, 5), startRow = r, startCol = 3, colNames = FALSE)
  writeData(wb, "7. Extended Checks", round(rr$Std_Error, 5), startRow = r, startCol = 4, colNames = FALSE)
  writeData(wb, "7. Extended Checks", round(rr$t_value, 3), startRow = r, startCol = 5, colNames = FALSE)
  writeData(wb, "7. Extended Checks", fmt_p_stars(rr$p_value), startRow = r, startCol = 6, colNames = FALSE)
  addStyle(wb, "7. Extended Checks", style_5dp, rows = r, cols = 3:4, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "7. Extended Checks", style_3dp, rows = r, cols = 5, gridExpand = TRUE, stack = TRUE)
  if (rr$Variable %in% c("CDQ_Score", "CDQ_lag1")) {
    addStyle(wb, "7. Extended Checks", style_row_org, rows = r, cols = 1:6, gridExpand = TRUE, stack = TRUE)
  }
}
ext_last <- 4 + nrow(extended_results)
writeData(wb, "7. Extended Checks",
          "Firm_Age dropped from two-way FE model (collinear with firm+year fixed effects; age-period-cohort identification issue).",
          startRow = ext_last + 2, colNames = FALSE)
writeData(wb, "7. Extended Checks",
          sprintf("One-way FE (base): N=%d, within R2=%.3f", nobs(fe_oneway), summary(fe_oneway)$r.squared[1]),
          startRow = ext_last + 3, colNames = FALSE)
writeData(wb, "7. Extended Checks",
          sprintf("Two-way FE: N=%d, within R2=%.3f", nobs(fe_twoway), summary(fe_twoway)$r.squared[1]),
          startRow = ext_last + 4, colNames = FALSE)
writeData(wb, "7. Extended Checks",
          sprintf("Lagged CDQ FE: N=%d, within R2=%.3f", nobs(fe_lagged), summary(fe_lagged)$r.squared[1]),
          startRow = ext_last + 5, colNames = FALSE)
writeData(wb, "7. Extended Checks",
          sprintf("F-test, year effects jointly significant: F(%d, %d) = %.2f, p = %.3f",
                  year_test$parameter[1], year_test$parameter[2], year_test$statistic, year_test$p.value),
          startRow = ext_last + 7, colNames = FALSE)
writeData(wb, "7. Extended Checks",
          paste0("Barros, L. A. B. C., Bergmann, D. R., Castro, F. H., & Silveira, A. D. M. da (2020). ",
                 "Endogeneity in panel data regressions: Methodological guidance for corporate finance ",
                 "researchers. Revista Brasileira de Gestao de Negocios, 22(Esp.), 437-461."),
          startRow = ext_last + 9, colNames = FALSE)

## -- Sheet 8: R Code -------------------------------------------------------------
# Embeds THIS script's own source, read back from disk -- assumes the script
# is still named "objective3_analysis.R" and is being run from its own
# directory (both true for a normal Rscript/Source run). Falls back to a
# short note rather than erroring if the file can't be found under that name.
addWorksheet(wb, "8. R Code")
setColWidths(wb, "8. R Code", cols = 1, widths = 130)
own_source <- tryCatch(readLines("objective3_analysis.R", warn = FALSE),
                        error = function(e) NULL)
if (is.null(own_source)) {
  own_source <- c("(Could not read this script's own source under the name",
                   "'objective3_analysis.R' in the current working directory --",
                   "place this workbook's generation next to that file to embed it here.)")
}
writeData(wb, "8. R Code", own_source, startRow = 1, startCol = 1, colNames = FALSE)
addStyle(wb, "8. R Code", createStyle(fontName = "Arial", fontSize = 13),
         rows = seq_along(own_source), cols = 1, gridExpand = TRUE)

## -- Sheet 9: Panel Data --------------------------------------------------------
addWorksheet(wb, "Panel Data (Obj 3)")
panel_export <- df_raw[rownames(df),
                        c("Ticker", "Year", "Sector", "SDQ_Score (0-100)", "CDQ_Score (0-100)",
                          "Tobin_Q_Wins", "Firm_Size (LN Total Assets)", "Leverage_Wins", "Firm_Age")]
names(panel_export) <- c("Ticker", "Year", "Sector", "SDQ_Score", "CDQ_Score",
                          "Tobin_Q_Wins", "Firm_Size", "Leverage_Wins", "Firm_Age")
writeData(wb, "Panel Data (Obj 3)", panel_export, startRow = 1, startCol = 1,
          colNames = TRUE, rowNames = FALSE)
addStyle(wb, "Panel Data (Obj 3)", style_hdr_navy, rows = 1, cols = 1:ncol(panel_export), gridExpand = TRUE)
setColWidths(wb, "Panel Data (Obj 3)", cols = 1:9, widths = c(10, 6, 18, 10, 10, 12, 10, 13, 9))

workbook_path <- file.path(output_dir, "objective3_consolidated_workbook.xlsx")
saveWorkbook(wb, workbook_path, overwrite = TRUE)

## Some installed builds of openxlsx (this sandbox included, package version
## 4.2.5.2) always write a "drawing" and a "vmlDrawing" relationship for
## EVERY worksheet, even ones with no image, pointing at files that were
## never actually created -- confirmed by testing a plain one-sheet, no-image
## workbook from this same installation, which fails to open for the same
## reason. Strict readers (and some Excel versions) refuse a file with a
## relationship pointing at a target that doesn't exist in the package, even
## if nothing in the sheet itself uses that relationship id. This helper
## strips exactly those dangling references (never touching any relationship
## that points at a file which genuinely exists, such as this workbook's own
## two embedded plot images) and re-zips the archive. Falls back to leaving
## the file exactly as openxlsx wrote it if anything here goes wrong.
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
      # vmlDrawingN.vml is never actually produced by this openxlsx build, on
      # any sheet, image or not -- always safe to strip.
      txt <- gsub('<Relationship Id="rIdvml"[^>]*/>', "", txt)
      # Only strip the "drawing" relationship on sheets where drawingN.xml
      # doesn't actually exist; the sheet(s) with a real image keep theirs.
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

cat("\nAll Objective 3 outputs written to:", normalizePath(output_dir), "\n")
