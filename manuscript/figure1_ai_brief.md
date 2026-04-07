# Figure 1 AI Brief

Use this as a direct prompt for AI-assisted Figure 1 generation.

## Goal

Create a journal-ready main-text Figure 1 for the npj Digital Medicine manuscript. The figure should show that the Thailand workflow is not just a surveillance display pipeline, but a digital decision-support system that converts routine surveillance data into auditable review priorities.

## Preferred format

- Prefer a two-panel figure.
- Panel A: operational review flow.
- Panel B: simplified analytical engine.
- A strong single-panel alternative is acceptable only if all required content below remains clear.

## Required content

### Data scope
- Thailand national infectious disease surveillance data
- 72 monitored series
- 43 retained for descriptive analysis
- 24 retained for counterfactual modelling
- Time span: 2008-2025

### Data processing and validation
- Weekly-to-monthly temporal disaggregation
- Overlap-year validation against official monthly totals
- Show these values somewhere in the figure:
  - Pearson r = 0.998
  - 2,064 overlap observations
  - median absolute error = 3 cases
  - median absolute percentage error = 3.90%

### Forecasting engine
- Seven candidate model families: NNAR, ETS, SARIMA, TBATS, Hybrid, BSTS, ARIMA + Fourier
- Three rolling pre-pandemic hold-out splits
- Composite model selection using sMAPE, RMSE, and MASE

### Robustness and sensitivity
- rank aggregation: 16/24
- sMAPE-only: 15/24
- horizon-weighted composite: 21/24
- no disease-level classification changed under 2020-03 or 2020-04 interruption choices

### Counterfactual simulation
- Best model family refit on the 2008-2019 baseline
- 5,000 simulated forecast paths per disease
- 80% and 95% predictive intervals

### Analytical modules
- RP/BP recovery classification
- uncertainty-aware phenotype probabilities
- alternative endpoint checks
- seasonal displacement detection
- contextual triangulation
- operational synthesis into a disease-review queue

### Review outputs
- dashboard or web-based review layer
- disease-level queue
- drill-down evidence
- frozen monthly review snapshots or other auditable review outputs

## Suggested panel logic

### Panel A: Operational overview
- data sources
- data processing
- model selection
- counterfactual generation
- analytical modules
- interactive review layer
- validation / robustness / endpoint checks can appear as side callouts

### Panel B: Technical engine
- data preparation
- seven-family model-selection engine
- counterfactual simulation
- recovery and endpoint module
- seasonal and contextual module
- final review outputs
- do not simply repeat Panel A verbatim

## Style constraints

- Editorial infographic, not a software diagram or presentation slide
- No black background
- No excessive gradients, 3D effects, or dense text blocks
- Maintain whitespace for readability after journal scaling
- Use short phrases inside boxes
- Use restrained color coding and only necessary arrows

## Caption seed

"Two-panel overview of the counterfactual digital decision-support pipeline." Panel A can describe the operational review-facing workflow. Panel B can describe the simplified technical engine supporting disease-level queue assignment.

## Source files for facts

- `manuscript/manuscript.md`
- `Outcome/Appendix/Tables/Appendix_S6_overlap_summary.csv`
- `Outcome/Appendix/Tables/Appendix_S7_overlap_examples.csv`
- `Outcome/Appendix/Tables/Recovery_uncertainty_summary.csv`
- `Outcome/Appendix/Tables/Joint_operational_summary.csv`
- `Outcome/Appendix/Tables/Temporal_utility_freeze_summary.csv`
- `Outcome/Appendix/Tables/Placebo_interruption_portfolio_summary.csv`

## Note

Existing exported files in `Outcome/Publish/npjDM/fig1.pdf` and `fig1.png` can be used only as rough visual references. They are not the authoritative design source.