# Thailand Infectious Disease Recovery Dashboard

This Shiny dashboard is written as a manuscript-facing analytical interface rather than a generic KPI screen. The goal is to support co-author discussion, result interpretation, and presentation of the study's main conceptual finding: incidence recovery, cumulative balance, and seasonal timing should be interpreted as distinct surveillance dimensions.

## Page design

1. `Overview`
   Opens with the national surveillance landscape, the disease-flow counts `72 -> 43 -> 24`, and a publication-style summary of the main findings.
2. `Recovery Explorer`
   Focuses on the manuscript's main contribution: the decoupling between recovery period (RP) and balance period (BP).
3. `Seasonal Shift`
   Compares prepandemic, observed post-PHSM, and counterfactual post-PHSM seasonal signatures in formal analytical language suitable for presentation.
4. `Methods & Data`
   Keeps the operational definitions visible so the dashboard remains interpretable without opening the manuscript text.

## Data sources used by the app

The dashboard now supports a bundled local data layout inside the app directory so it can run more reliably with `shiny::runApp()` and be shared more easily.

Bundled files expected by the app:

- `data/temp/month.RData`
- `data/temp/outcome.RData`
- `data/Outcome/TotalCasesDeaths.csv`

If the local bundled files are present, the app will use them first. If not, it falls back to the original repository paths.

## Bundled app structure

```text
ShinyDashboard/
   app.R
   global.R
   R/
   data/
      temp/
         month.RData
         outcome.RData
      Outcome/
         TotalCasesDeaths.csv
```

## Live app

Public deployment:

[https://lkg1116.shinyapps.io/TH_ID/](https://lkg1116.shinyapps.io/TH_ID/)

## Run

From the repository root in R:

```r
shiny::runApp("ScriptAnalysis/ShinyDashboard")
```

Or from inside `ScriptAnalysis/ShinyDashboard`:

```r
shiny::runApp()
```

## Notes

- The app now preloads modules through `global.R` so direct `shiny::runApp()` works more reliably.
- The Overview status plot was adjusted to avoid the `figure margins too large` error in small plotting devices such as the RStudio Viewer.
