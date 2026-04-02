# ==============================================================================
# Figure 5: Dashboard Screenshots
# Captures key interface panels from the deployed Shiny dashboard
# ==============================================================================
#
# Prerequisites:
#   install.packages("webshot2")
#   # webshot2 requires chromote (bundled Chrome)
#
# Usage:
#   source("fig5_dashboard.R")
#   # Or capture manually from https://lkg1116.shinyapps.io/TH_ID/
# ==============================================================================

library(webshot2)
library(magick)

dashboard_url <- "https://lkg1116.shinyapps.io/TH_ID/"
out_dir <- "../Outcome/Publish"

# --- Option A: Automated capture (requires chromote/headless Chrome) ---
capture_dashboard <- function() {
  # Full-page overview screenshot
  webshot2::webshot(
    url = dashboard_url,
    file = file.path(out_dir, "fig5_dashboard_overview.png"),
    vwidth = 1400,
    vheight = 900,
    delay = 5,      # Wait for Shiny app to render
    zoom = 2         # Retina resolution
  )
  cat("Dashboard overview screenshot captured.\n")
}

# Try automated capture; fall back to manual instructions
tryCatch(
  capture_dashboard(),
  error = function(e) {
    message("Automated capture failed: ", e$message)
    message("\n--- Manual Screenshot Instructions ---")
    message("1. Open browser to: ", dashboard_url)
    message("2. Capture 5 screenshots (one per module tab):")
    message("   - Overview tab   -> fig5_dashboard_a.png")
    message("   - Recovery tab   -> fig5_dashboard_b.png")
    message("   - Time Series tab -> fig5_dashboard_c.png")
    message("   - Seasonality tab -> fig5_dashboard_d.png")
    message("   - Reference tab  -> fig5_dashboard_e.png")
    message("3. Save to: ", out_dir)
    message("4. Run the composite assembly below after saving screenshots.")
  }
)

# --- Option B: Assemble multi-panel composite from individual screenshots ---
assemble_composite <- function() {
  panel_files <- c(
    file.path(out_dir, "fig5_dashboard_a.png"),
    file.path(out_dir, "fig5_dashboard_b.png"),
    file.path(out_dir, "fig5_dashboard_c.png"),
    file.path(out_dir, "fig5_dashboard_d.png")
  )

  existing <- file.exists(panel_files)
  if (!all(existing)) {
    message("Missing panel files: ", paste(panel_files[!existing], collapse = ", "))
    message("Please capture screenshots first.")
    return(invisible(NULL))
  }

  panels <- lapply(panel_files, image_read)
  # Resize to uniform width
  panels <- lapply(panels, image_resize, geometry = "1400x")

  # Add labels
  labels <- c("A. Overview", "B. Recovery", "C. Time Series", "D. Seasonality")
  for (i in seq_along(panels)) {
    panels[[i]] <- image_annotate(
      panels[[i]], labels[i],
      size = 40, weight = 700, color = "black",
      location = "+20+10", font = "Helvetica"
    )
  }

  # 2x2 grid
  top_row <- image_append(c(panels[[1]], panels[[2]]))
  bot_row <- image_append(c(panels[[3]], panels[[4]]))
  composite <- image_append(c(top_row, bot_row), stack = TRUE)

  image_write(composite, path = file.path(out_dir, "fig5_dashboard.png"), format = "png")
  image_write(composite, path = file.path(out_dir, "fig5_dashboard.pdf"), format = "pdf")
  cat("Figure 5 composite saved.\n")
}

# Uncomment to assemble after manual screenshots:
# assemble_composite()
