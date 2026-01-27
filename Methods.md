
# Methods

## Data sources, inclusion criteria and case definitions

We compiled routine case and death notifications from Thailand’s national infectious‑disease surveillance systems for 2008–2022. To focus analysis on diseases with sufficient data density for reliable time‑series modelling, diseases were eligible if they met two prespecified criteria: (1) continuous reporting across the study period with no entire calendar years missing, and (2) a substantive burden defined by a minimum cumulative case count across the study period (threshold chosen a priori to exclude very rare conditions). Administrative changes that affect denominators (for example the separation of new provinces) were handled by treating affected locations consistently and, where necessary, flagging series that could not be reliably linked across boundary changes. Cases and deaths were aggregated to calendar months for the primary analyses. Population denominators used official annual mid‑year estimates; when 2023 age‑group population distributions were not yet available we used 2022 age distributions as an approximation for 2023. Age‑specific and province‑specific rates were calculated by dividing counts by the relevant population denominators and scaling per 100,000 population.

| Raw disease name | Exclusion category |
|---|---|
| Anthrax | Zero reported incidence over the study period |
| Avian Influenza | Zero reported incidence over the study period |
| Poliomyelitis | Zero reported incidence over the study period |
| D.H.F | Overlapping surveillance categories |
| D.H.F.shock syndrome | Overlapping surveillance categories |
| Dengue fever | Overlapping surveillance categories |
| Encephalitis uns | Overlapping surveillance categories |
| Eosinophilic Meningitis | Overlapping surveillance categories |
| Measles | Overlapping surveillance categories |
| Measles c Complication | Overlapping surveillance categories |
| Pulmonary T.B | Overlapping surveillance categories |
| T.B. other organs | Overlapping surveillance categories |
| T.B.meningitis | Overlapping surveillance categories |
| STI Total | Overlapping surveillance categories |
| TB Total | Structural changes in surveillance definitions |
| Botulism | Not aligned with the transmissible infectious-disease framework |
| Food_Poisoning | Not aligned with the transmissible infectious-disease framework |
| Mushroom poisoning | Not aligned with the transmissible infectious-disease framework |
| Rabies | Not aligned with the transmissible infectious-disease framework |
| Tetanus exc.Neo | Not aligned with the transmissible infectious-disease framework |
| Tetanus neonatorum | Not aligned with the transmissible infectious-disease framework |
| Meningococcal Meningitis | Incomplete reporting in the most recent surveillance year |
| Pediculosis Pubis | Incomplete reporting in the most recent surveillance year |
| Vaginal trichomoniasis | Incomplete reporting in the most recent surveillance year |
| Genital Molluscum Contagiosum | Ill-defined or residual surveillance categories |
| Hepatitis_uns | Ill-defined or residual surveillance categories |
| L.G.V.&other&unsp.V.D | Ill-defined or residual surveillance categories |
| N.S.U._V | Ill-defined or residual surveillance categories |
| Other STI | Ill-defined or residual surveillance categories |

## Data cleaning and pre‑processing

Time series from weekly and monthly feeds were harmonized by standardizing disease names, consolidating duplicate or overlapping sources, and converting calendar week references to consistent ISO week representations. Observations with non‑numeric or clearly erroneous counts were treated as missing; sporadically missing single months were retained for modelling, whereas series with long continuous gaps were excluded from forecasting analyses. For week→month reconstruction (see below) we used an explicit mapping of ISO year–week to constituent daily dates to ensure alignment across cross‑year ISO week assignments.

## Reconstruction of monthly series from weekly data

Where monthly counts were missing but weekly totals existed we reconstructed daily profiles and aggregated to months with the following reproducible procedure. For each disease–year we mapped ISO year–week to a representative week midpoint and the list of daily dates belonging to the week. A natural spline was fitted to the series of week‑midpoint dates (numerical) versus weekly totals and evaluated at daily resolution to obtain smooth daily predictions. Negative predicted values were set to zero. To preserve observed weekly aggregates the daily predictions were rescaled within each week so that their sum equalled the observed weekly total. Fractional daily values were then floored to integers and the remaining counts (week total minus sum of floors) were distributed to days with the largest fractional parts; this preserves exact integer weekly totals while producing a smooth within‑week profile. When fewer than two observed weeks were available or spline extrapolation proved unreliable, the procedure defaulted to uniform distribution over a week. For deaths, which are rarer and often zero‑inflated, we apportioned weekly totals to months by weighting the week’s days by how many fall in each calendar month (proportional day allocation). The reconstruction yields monthly aggregates that are consistent with weekly reporting while avoiding negative daily estimates or spurious high‑frequency artifacts.

## Disease selection rules for analysis

To produce a robust comparative analysis similar in spirit to prior multi‑disease surveillance studies, we excluded diseases with sparse reporting or incomplete multi‑year coverage. Specifically, diseases with any full calendar year of missing data during 2008–2022 or with cumulative counts below a prespecified threshold were excluded from forecasting and comparative disruption analyses. Diseases with known reporting artefacts (for example changes in case definition or laboratory confirmation practices) were reviewed and, where appropriate, excluded or analysed separately with additional sensitivity checks. These selection steps reduce bias from intermittent surveillance and ensure the models are trained on stable historical baselines.

## Forecasting framework and candidate models

We estimated counterfactual expected trajectories using a unified forecasting pipeline that compared six model families chosen to cover a broad range of time‑series behaviour: (i) neural‑network autoregression (for non‑linear autocorrelation), (ii) ETS (exponential smoothing state‑space models for level and seasonal dynamics), (iii) SARIMA (auto‑selected seasonal ARIMA for autoregressive integrated structures), (iv) TBATS (to handle multiple or complex seasonalities), (v) a hybrid ensemble that combines component models with cross‑validated weights, and (vi) Bayesian structural time‑series (BSTS) to capture trend/seasonal structure in a probabilistic framework. Prior to fitting, monthly counts were transformed by log(y + c) where c = 0.01 (Laplace smoothing) to stabilise variance and avoid log(0). Models were fitted on the transformed scale and point and interval forecasts were back‑transformed for evaluation and reporting.

## Cross‑validation, performance metrics and model selection

To prioritise out‑of‑sample predictive robustness we evaluated candidate models using three predefined pre‑pandemic hold‑out schemes: (A) train through 2008–2018, test on 2019; (B) train through 2008–2017, test on 2018–2019; and (C) train through 2008–2016, test on 2017–2019. For each disease, model and split we computed predictive accuracy on the original scale using symmetric mean absolute percentage error (sMAPE), root‑mean‑square error (RMSE) and mean absolute scaled error (MASE). To synthesize these complementary metrics into a single selection criterion we z‑standardized each metric (within disease and split), inverted the sign so that better performance gave larger scores, and summed the three standardized scores to form a composite index per split; indices were summed across splits and the model with the highest total index was chosen as the disease’s best model. This composite criterion penalises models that perform inconsistently across different error notions or hold‑out periods.

## Forecast uncertainty quantification

Uncertainty was characterised via Monte Carlo simulation of forecast paths. For standard models we used model‑native simulation/bootstrapping to generate replicate paths; for the hybrid ensemble we simulated by adding bootstrap residuals to the fitted ensemble mean; for BSTS we used posterior predictive draws from the model’s MCMC output. Default simulation settings used in the pipeline were 1,000 independent simulation paths for Monte Carlo summaries and 1,000 BSTS MCMC iterations with standard burn‑in trimming; hybrid ensemble fitting used a programmatically selected window size with a minimum of 24 months and an upper practical bound proportional to 70% of the training series length to stabilise cross‑validation when evaluating component weights. Whenever parallel computation was applicable we used multi‑core processing to accelerate ensemble fitting and simulation. Forecast distributions were summarised by median and central 80% and 95% credible/interval bands on the original scale.

## Interrupted‑time comparisons and derived metrics

We compared observed monthly counts after the public‑health intervention onset to counterfactual medians and intervals to identify the timing and magnitude of disruptions. We computed a stabilized relative change metric (observed + 0.01)/(counterfactual_median + 0.01) to avoid undefined ratios when counts are zero. To summarise prolonged effects we computed the cumulative deficit series cum_diff(t) = Σ_{s ≤ t} (observed_s − expected_median_s), and derived dates and quantities from cum_diff: (i) start of deficit (first date cum_diff < 0); (ii) trough date (date of minimum cum_diff); (iii) accumulated deficit at trough; (iv) relative deficit = accumulated_deficit_at_trough / cumulative_expected_at_trough; and (v) balance (payback) date defined as the first date after trough when cum_diff ≥ 0. Rebound intensity was measured as the maximum observed/expected ratio after trough. Suppression and payback durations were reported in months. To reduce false positives when declaring recovery we required observed values to exceed 95% of expected median in a sustained manner, operationalised via a three‑month rolling requirement.

## Seasonality analysis

To identify disruptions in seasonal shape and timing we compared mean monthly profiles across three scenarios: pre‑pandemic observed (≤2019), post‑PHSM observed (≥2023) and post‑PHSM counterfactual (predicted median ≥2023). For shape comparison we scaled monthly profiles to the unit interval and calculated amplitude metrics (peak, trough, peak‑to‑trough ratio and amplitude_ratio = (peak − trough)/mean). Peak timing was estimated using both the month of maximal mean and a centre‑of‑mass circular method (mapping months to angles and computing a weighted circular mean) to accommodate broad or multiple peaks. Circular differences were mapped onto [−6, +6] months for interpretation of phase shifts.

## Clustering of resilience phenotypes

We clustered diseases by their suppression and rebound characteristics using k‑means on scaled feature pairs (relative deficit and rebound intensity). Prior to clustering, features with heavy skew were log‑transformed. A three‑cluster solution (k = 3) with nstart = 25 was used to define broad resilience phenotypes; we examined sensitivity to k and assessed cluster stability, excluding extreme outliers when necessary to preserve interpretability.

## Determinants of recovery: survival analysis

To examine determinants of recovery speed we used time‑to‑event analysis where the event was recovery as defined above and time was measured in months from the start of suppression. Candidate predictors included transmission category, vaccine availability/programmatic status, incubation period category, infectious period category and the typical duration of immunity following vaccination or natural infection. We fitted Cox proportional hazards models and present hazard ratios with 95% confidence intervals and two‑sided p‑values. Covariate categorisations followed epidemiological convention and were selected a priori; models were inspected for proportional hazards violations and small‑sample instability.

## Implementation and reproducibility

Analyses were implemented in a reproducible pipeline with fixed random seeds for all stochastic processes to permit replication of results. Important numeric settings used throughout the pipeline included: Laplace smoothing constant c = 0.01 for log transforms; simulation paths n = 1,000 for Monte Carlo summaries; BSTS MCMC iterations = 1,000; hybrid ensemble window selection with minimum 24 months and practical upper bound selected at ≈70% of training length; k‑means nstart = 25. Parallel computation was used where applicable to accelerate ensemble fitting and simulation. Intermediate outputs and final summary tables were produced to support auditability and sensitivity checks.


