# Thailand Infectious Disease Recovery Dashboard

This Shiny dashboard presents the Thailand infectious disease recovery analysis as a polished review and presentation interface. The app is designed around three distinct surveillance dimensions: incidence recovery, cumulative balance recovery, and seasonal re-alignment after the pandemic shock.

## Live app

Public deployment:

[https://lkg1116.shinyapps.io/TH_ID/](https://lkg1116.shinyapps.io/TH_ID/)

## Dashboard pages

1. `Overview`
   Summarizes the national surveillance landscape with a dashboard landing page, headline KPIs, burden ranking, and recovery-class mix across diseases.

   ![Overview](./lkg1116.shinyapps.io%20(1).jpeg)

2. `Recovery`
   Focuses on disease-level trajectories, recovery period (RP), balance period (BP), deficit depth, and cross-disease comparison.

   ![Recovery](./lkg1116.shinyapps.io%20(5).jpeg)

3. `Time Series`
   Provides filtered monthly observed and counterfactual series for inspection, faceted comparison, and export.

   ![Time Series](./lkg1116.shinyapps.io%20(4).jpeg)

4. `Seasonality`
   Compares prepandemic, observed post-PHSM, and counterfactual post-PHSM seasonal signatures, including peak timing and normalized seasonal shape.

   ![Seasonality](./lkg1116.shinyapps.io%20(3).jpeg)

5. `Reference`
   Keeps operational definitions, study flow, source files, and reading guidance visible for manuscript and presentation use.

   ![Reference](./lkg1116.shinyapps.io%20(2).jpeg)

## Core definitions

- `RP (Recovery Period)`: first stable return to expected monthly incidence.
- `BP (Balance Period)`: first month when the cumulative observed-minus-expected deviation closes back to zero.
- `Counterfactual`: expected post-2020 trajectory estimated from prepandemic patterns.

## Data used by the app

The app uses bundled cached outputs so it can run directly without rerunning the full analytical pipeline.

Expected local files:

- `data/temp/month.RData`
- `data/temp/outcome.RData`
- `data/Outcome/TotalCasesDeaths.csv`

These files support the fast-loading dashboard used for review, presentation, and manuscript discussion.

## App structure

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
   rsconnect/
```

## Run locally

From the repository root in R:

```r
shiny::runApp("ShinyDashboard")
```

Or from inside `ShinyDashboard`:

```r
shiny::runApp()
```

## Notes

- The app preloads modules through `global.R`, which makes direct `shiny::runApp()` more reliable.
- The current UI is optimized for a mature dashboard presentation rather than a manuscript-style notes page.
- The screenshots in this README reflect the deployed dashboard layout.
