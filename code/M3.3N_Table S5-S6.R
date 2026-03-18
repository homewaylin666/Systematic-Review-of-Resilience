library(broom)
library(stringr)

# safe numeric conversion
safe_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

# -----------------------------
# 1) CHI-SQUARE: details table
# -----------------------------
# Extract robustly and produce 'direction' column: NS / EXCESS / DEFICIT
extract_chi_details_simple <- function(chi_res, test_label) {
  df <- chi_res$data
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      Test = test_label,
      var1 = NA_character_,
      var2 = NA_character_,
      count = NA_real_,
      std_residual = NA_real_,
      direction = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  # determine first two data columns (their values)
  orig_names <- names(df)
  col1 <- if (length(orig_names) >= 1) orig_names[1] else NA_character_
  col2 <- if (length(orig_names) >= 2) orig_names[2] else NA_character_
  # get values for var1/var2 robustly
  get_val <- function(d, nm, i) {
    if (!is.na(nm) && nm %in% names(d)) return(as.character(d[[nm]][i]))
    return(NA_character_)
  }
  # find std residual column if present
  stdres_col <- intersect(c("std_residual","std.residual","stdres","std_residuals","stdresidual"), names(df))
  count_col <- intersect(c("count","Count","Freq","freq","frequency"), names(df))
  n <- nrow(df)
  out <- data.frame(
    Test = rep(test_label, n),
    var1 = sapply(seq_len(n), function(i) get_val(df, col1, i)),
    var2 = sapply(seq_len(n), function(i) get_val(df, col2, i)),
    count = if (length(count_col) >= 1) safe_num(df[[count_col[1]]]) else NA_real_,
    std_residual = if (length(stdres_col) >= 1) safe_num(df[[stdres_col[1]]]) else NA_real_,
    stringsAsFactors = FALSE
  )
  # compute direction using std_residual threshold ±1.96
  out$direction <- sapply(out$std_residual, function(x) {
    if (is.na(x)) return(NA_character_)
    if (abs(x) < 1.96) return("NS")
    if (x > 0) return("EXCESS") else return("DEFICIT")
  })
  out
}

chi1_fixed <- extract_chi_details_simple(chi_result1, "Manipulation_vs_Type")
chi2_fixed <- extract_chi_details_simple(chi_result2, "Manipulation_vs_Pattern")
chi3_fixed <- extract_chi_details_simple(chi_result3, "Type_vs_Pattern")

chi_details_fixed <- do.call(rbind, list(chi1_fixed, chi2_fixed, chi3_fixed))

# Write chi details CSV
write.csv(chi_details_fixed, "Table S5_ChiTable.details.csv", row.names = FALSE)

# -----------------------------
# 2) CHI-SQUARE: summary table
# -----------------------------
chi_summary <- data.frame(
  Test = c("Manipulation_vs_Type", "Manipulation_vs_Pattern", "Type_vs_Pattern"),
  statistic = c(safe_num(chi_result1$chi_test$statistic),
                safe_num(chi_result2$chi_test$statistic),
                safe_num(chi_result3$chi_test$statistic)),
  df = c(safe_num(chi_result1$chi_test$parameter),
         safe_num(chi_result2$chi_test$parameter),
         safe_num(chi_result3$chi_test$parameter)),
  p.value = c(safe_num(chi_result1$p_value),
              safe_num(chi_result2$p_value),
              safe_num(chi_result3$p_value)),
  stringsAsFactors = FALSE
)

format_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 1e-4) return("<0.0001")
  if (p < 0.001) return("<0.001")
  sprintf("%.4f", round(p, 4))
}
p_stars <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 1e-4) return("****")
  if (p < 1e-3) return("***")
  if (p < 1e-2) return("**")
  if (p < 0.05) return("*")
  ""
}
chi_summary$p.value_formatted <- sapply(chi_summary$p.value, format_p)
chi_summary$significance <- sapply(chi_summary$p.value, p_stars)

# Write chi summary CSV
write.csv(chi_summary, "Table S5_ChiSquare.summary.csv", row.names = FALSE)

# -----------------------------
# 3) ANOVA + Tukey pairwise table (simplified)
# -----------------------------
# ANOVA rows: tidy aov output; no group1/group2/significance columns
safe_tidy_aov_simple <- function(aov_model, label) {
  td <- broom::tidy(aov_model)
  if (is.null(td) || nrow(td) == 0) {
    return(data.frame(
      Test_Type = "ANOVA",
      Factor = label,
      term = NA_character_,
      statistic = NA_real_,
      df = NA_real_,
      p.value = NA_real_,
      p.value_formatted = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  names(td) <- make.names(names(td))
  pcol <- intersect(c("p.value","Pr...F.","Pr..F.","Pr..F..","p.value."), names(td))
  statcol <- intersect(c("statistic","F.value","F"), names(td))
  dfcol <- intersect(c("df","Df","df1"), names(td))
  out <- data.frame(
    Test_Type = rep("ANOVA", nrow(td)),
    Factor = rep(label, nrow(td)),
    term = if ("term" %in% names(td)) as.character(td$term) else rownames(td),
    statistic = if (length(statcol) >= 1) safe_num(td[[statcol[1]]]) else NA_real_,
    df = if (length(dfcol) >= 1) safe_num(td[[dfcol[1]]]) else NA_real_,
    p.value = if (length(pcol) >= 1) safe_num(td[[pcol[1]]]) else NA_real_,
    stringsAsFactors = FALSE
  )
  out$p.value_formatted <- sapply(out$p.value, format_p)
  out
}

anova_rows1 <- safe_tidy_aov_simple(tukey_result1$aov_model, "Manipulation_vs_logDuration")
anova_rows2 <- safe_tidy_aov_simple(tukey_result2$aov_model, "Type_vs_logDuration")
anova_rows3 <- safe_tidy_aov_simple(tukey_result3$aov_model, "Pattern_vs_logDuration")

anova_rows <- do.call(rbind, list(anova_rows1, anova_rows2, anova_rows3))

# Tukey extraction: only keep significant comparisons (p < 0.05)
extract_tukey_significant <- function(tukey_hsd_obj) {
  if (is.null(tukey_hsd_obj) || length(tukey_hsd_obj) == 0) return(NULL)
  out_list <- list()
  factor_names <- names(tukey_hsd_obj)
  for (fac in factor_names) {
    mat <- tukey_hsd_obj[[fac]]
    if (is.null(mat) || nrow(mat) == 0) next
    dfm <- as.data.frame(mat, stringsAsFactors = FALSE)
    dfm$comparison <- rownames(dfm)
    names(dfm) <- make.names(names(dfm))
    # find p and estimate/conf columns robustly
    pcol_candidates <- intersect(c("p.adj","p.adj.","p.adj.value","p.value","adj.p.value","p.value.","P.adj"), names(dfm))
    pcol <- if (length(pcol_candidates) >= 1) pcol_candidates[1] else NA
    diffcol_candidates <- intersect(c("diff","difference","estimate"), names(dfm))
    diffcol <- if (length(diffcol_candidates) >= 1) diffcol_candidates[1] else NA
    lowcol_candidates <- intersect(c("lwr","conf.low","Lower"), names(dfm))
    lowcol <- if (length(lowcol_candidates) >= 1) lowcol_candidates[1] else NA
    highcol_candidates <- intersect(c("upr","conf.high","Upper"), names(dfm))
    highcol <- if (length(highcol_candidates) >= 1) highcol_candidates[1] else NA
    for (i in seq_len(nrow(dfm))) {
      pval <- if (!is.na(pcol)) safe_num(dfm[i, pcol]) else NA_real_
      # keep only significant
      if (is.na(pval) || pval >= 0.05) next
      comp <- dfm$comparison[i]
      estimate <- if (!is.na(diffcol)) safe_num(dfm[i, diffcol]) else NA_real_
      conf_low <- if (!is.na(lowcol)) safe_num(dfm[i, lowcol]) else NA_real_
      conf_high <- if (!is.na(highcol)) safe_num(dfm[i, highcol]) else NA_real_
      out_list[[length(out_list) + 1]] <- data.frame(
        Test_Type = "Tukey_HSD",
        Factor = fac,
        term = comp,
        estimate = estimate,
        conf.low = conf_low,
        conf.high = conf_high,
        p.value = pval,
        p.value_formatted = format_p(pval),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(out_list) == 0) return(NULL)
  do.call(rbind, out_list)
}

tuk1_signif <- extract_tukey_significant(tukey_result1$tukey_result)
tuk2_signif <- extract_tukey_significant(tukey_result2$tukey_result)
tuk3_signif <- extract_tukey_significant(tukey_result3$tukey_result)

tukey_signif_rows <- do.call(rbind, Filter(Negate(is.null), list(tuk1_signif, tuk2_signif, tuk3_signif)))

# Ensure same columns for rbind with anova_rows
# anova_rows columns: Test_Type, Factor, term, statistic, df, p.value, p.value_formatted
# tukey_signif_rows: Test_Type, Factor, term, estimate, conf.low, conf.high, p.value, p.value_formatted
# Create unified column set: Test_Type, Factor, term, statistic, df, estimate, conf.low, conf.high, p.value, p.value_formatted
# For ANOVA fill estimate/conf columns NA; for Tukey fill statistic/df NA

# prepare ANOVA for bind
anova_for_bind <- data.frame(
  Test_Type = anova_rows$Test_Type,
  Factor = anova_rows$Factor,
  term = anova_rows$term,
  statistic = anova_rows$statistic,
  df = anova_rows$df,
  estimate = NA_real_,
  conf.low = NA_real_,
  conf.high = NA_real_,
  p.value = anova_rows$p.value_formatted,
  stringsAsFactors = FALSE
)

# prepare Tukey for bind
if (!is.null(tukey_signif_rows) && nrow(tukey_signif_rows) > 0) {
  tukey_for_bind <- data.frame(
    Test_Type = tukey_signif_rows$Test_Type,
    Factor = tukey_signif_rows$Factor,
    term = tukey_signif_rows$term,
    statistic = NA_real_,
    df = NA_real_,
    estimate = tukey_signif_rows$estimate,
    conf.low = tukey_signif_rows$conf.low,
    conf.high = tukey_signif_rows$conf.high,
    p.value = tukey_signif_rows$p.value_formatted,
    stringsAsFactors = FALSE
  )
  anova_tukey_combined <- rbind(anova_for_bind, tukey_for_bind)
} else {
  anova_tukey_combined <- anova_for_bind
}

# Write ANOVA + significant Tukey CSV
write.csv(anova_tukey_combined, "table/Table S6_ANOVA_Tukey_pairwise.csv", row.names = FALSE)
