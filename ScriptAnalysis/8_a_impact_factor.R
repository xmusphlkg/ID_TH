
# packages ----------------------------------------------------------------

library(tidyverse)
library(purrr)
library(patchwork)
library(ggrepel)
library(broom)
library(factoextra)

# data --------------------------------------------------------------------

#' Calculate Rebound Metrics
#' We need to quantify "How strong was the surge?" to test the immunity debt hypothesis.
#' Metric: Peak Ratio (Max Observed / Median Forecast) during the post-trough period.
df_rebound <- map_dfr(outcome, function(item) {
     od <- item$outcome_data
     shortname <- unique(od$Shortname)[1]
     
     # Find the Trough Date from your existing results (df_metrics needs to exist)
     # If df_metrics isn't in global env, we recalculate or look it up.
     # Assuming 'df_metrics' exists from previous step:
     trough_date <- df_metrics$Trough_Date[df_metrics$Shortname == shortname]
     
     if (length(trough_date) == 0 || is.na(trough_date)) return(NULL)
     
     # Filter for Post-Trough data
     df_post <- od %>% filter(date >= trough_date)
     
     if (nrow(df_post) == 0) return(NULL)
     
     # Calculate Rebound Intensity
     # 1. Peak Ratio: How many times higher than normal?
     peak_ratio <- max(df_post$value / (df_post$median + 1), na.rm = TRUE)
     
     # 2. Rebound Speed: Slope of the surge (Cases per month) - Optional
     # Simple Peak Ratio is usually sufficient for this analysis.
     
     tibble(Shortname = shortname, Rebound_Intensity = peak_ratio)
})

# Merge everything into a Master Analysis Dataframe
df_analysis <- df_metrics %>%
     left_join(df_rebound, by = "Shortname") %>%
     left_join(data_class %>% select(Shortname, Group), by = "Shortname") %>%
     # Filter out diseases with no deficit (can't analyze debt for them)
     filter(Max_Deficit < 0) %>%
     mutate(
          # Convert Deficit to positive "Debt" for log-scale plotting
          Immunity_Debt = abs(Max_Deficit),
          Log_Debt = log10(Immunity_Debt),
          # Ensure Months is numeric
          Months_to_Rec = as.numeric(Months_Trough_to_Recovery)
     ) %>%
     # Remove rows with NAs in key columns for modeling
     na.omit()

# Determinants of Recovery ------------------------------------------------

# Model: Linear Regression (Recovery Time ~ Disease Group)
# We set "Respiratory IDs" as the reference level if possible, or just run basic lm
model_rec <- lm(Months_to_Rec ~ Group, data = df_analysis)

# Tidy the results for plotting
df_model <- tidy(model_rec, conf.int = TRUE) %>%
     filter(term != "(Intercept)") %>%
     mutate(term = gsub("Group", "", term)) # Clean label names

# Plot A: Forest Plot
p1 <- ggplot(df_model, aes(x = estimate, y = reorder(term, estimate))) +
     geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
     geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "#2c3e50") +
     geom_point(size = 3, color = "#e74c3c") +
     labs(
          title = "A: Determinants of Recovery Duration",
          subtitle = "Regression Coefficients (vs. Reference Group)",
          x = "Additional Months to Recover (Estimate)",
          y = NULL
     ) +
     theme_minimal() +
     theme(plot.title = element_text(face = "bold"))

# Immunity Debt Verification ----------------------------------------------

# Hypothesis: Deeper Debt (X) -> Stronger Rebound (Y)
p2 <- ggplot(df_analysis, aes(x = Immunity_Debt, y = Rebound_Intensity)) +
     # Use log scale for Debt because it spans orders of magnitude
     scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
     geom_smooth(method = "lm", color = "gray80", alpha = 0.2, linetype = "dashed") +
     geom_point(aes(color = Group, size = Months_to_Rec), alpha = 0.7) +
     geom_text_repel(aes(label = Shortname), size = 3, max.overlaps = 10) +
     scale_color_brewer(palette = "Set1") +
     labs(
          title = "B: Testing the 'Immunity Debt' Hypothesis",
          subtitle = "Relationship between Cumulative Deficit and Rebound Intensity",
          x = "Cumulative Immunity Debt (Log Scale)",
          y = "Rebound Intensity (Peak Observed / Expected)",
          size = "Rec. Months",
          color = "Disease Group"
     ) +
     theme_minimal() +
     theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# Resilience Clustering ---------------------------------------------------

# 1. Prepare Data for Clustering (Numerical columns only)
# We use: Log_Debt, Rebound_Intensity, Months_to_Rec
df_clus_raw <- df_analysis %>%
     select(Shortname, Log_Debt, Rebound_Intensity, Months_to_Rec) %>%
     column_to_rownames("Shortname")

# 2. Scale/Normalize
df_scaled <- scale(df_clus_raw)

# 3. Run K-means (Let's assume k=3 for High/Medium/Low impact)
set.seed(123)
km_res <- kmeans(df_scaled, centers = 3, nstart = 25)

# 4. Perform PCA for visualization dimension reduction (2D)
pca_res <- prcomp(df_scaled, center = FALSE, scale. = FALSE)
df_pca <- as.data.frame(pca_res$x) %>%
     mutate(
          Cluster = as.factor(km_res$cluster),
          Shortname = rownames(df_scaled)
     )

# Add Cluster info back to main dataframe for interpretation
df_analysis$Cluster <- as.factor(km_res$cluster)

# Plot C: PCA Cluster Map
p3 <- ggplot(df_pca, aes(x = PC1, y = PC2, color = Cluster)) +
     geom_hline(yintercept = 0, linetype = "dashed", color = "gray90") +
     geom_vline(xintercept = 0, linetype = "dashed", color = "gray90") +
     # Draw hull (optional, makes clusters clearer)
     stat_ellipse(aes(fill = Cluster), geom = "polygon", alpha = 0.1, show.legend = FALSE) +
     geom_point(size = 3) +
     geom_text_repel(aes(label = Shortname), size = 3) +
     labs(
          title = "C: Resilience Typology (Clustering)",
          subtitle = "K-means Clustering based on Debt, Recovery Speed & Rebound",
          x = paste0("PC1 (", round(summary(pca_res)$importance[2,1]*100, 1), "%)"),
          y = paste0("PC2 (", round(summary(pca_res)$importance[2,2]*100, 1), "%)"),
          color = "Resilience Type"
     ) +
     theme_minimal() +
     theme(plot.title = element_text(face = "bold"))

# save --------------------------------------------------------------------

# Layout: 
# Row 1: Forest Plot (A) | Clustering (C)
# Row 2: Scatter Plot (B) (Width spanning full)
layout_design <- "
AAACC
BBBBB
"

final_plot <- p1 + p2 + p3 + 
     plot_layout(design = layout_design)

# Save
ggsave("../Outcome/Publish/fig6.pdf",
       plot = final_plot, 
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 14, height = 12)

ggsave("../Outcome/Publish/fig6.png",
       final_plot,
       limitsize = FALSE,
       width = 14, height = 12)

