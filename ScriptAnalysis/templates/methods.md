<div style="text-align:center;">
  <h3 style="font-family: inherit; font-weight: normal; margin-bottom: 0;">Supplementary information:</h3>
  <h1 style="font-family: inherit; font-weight: bold; font-size: 1.5em;">A counterfactual framework for post-disruption recovery assessment in infectious disease surveillance in Thailand</h1>
  <br>
  <br>
  Kangguo Li et al. (2026)
</div>


<div style="page-break-after: always;"></div>

**Contents**

[toc]

<div style="page-break-after: always;"></div>

## Supplementary Methods

### Overview of the analytical workflow

The supplementary analyses were generated from a modular R and Python workflow that mirrored the main paper pipeline and wrote intermediate analytical objects to a version-locked cache before figure and table production. Raw monthly surveillance extracts were cleaned in Python and stored in disease-specific case, death, age-stratified, and rate files. The analytical workflow then proceeded in R through four linked stages: construction of the national monthly analytical cache; generation of descriptive outputs for overall burden, category-specific trends, age structure, and province-level patterns; selection and fitting of disease-specific counterfactual forecasting models; and derivation of recovery, balance, seasonal-shift, suppression, rebound, and exploratory recovery-speed metrics. All supplementary tables and figures were exported from the same cached analytical objects used for the manuscript results, so the appendix reports the operational implementation rather than a separate post hoc analysis stream.

### Disease-series curation and analytical subsets

The source registry comprised 72 nationally notifiable disease series. Series were first screened for inclusion in the descriptive portfolio using explicit metadata flags and manual disease-name harmonization tables stored in the project workbook. Exclusion before the 43-disease descriptive analysis followed six operational categories: zero incidence throughout follow-up, overlapping aggregate surveillance categories, diseases not aligned with the infectious-disease focus of the paper, unstable or residual categories, incomplete recent reporting, and structural changes in surveillance definition. **Supplementary Table S2** lists the series removed at this stage. The retained 43 series were then reviewed for counterfactual suitability using a minimum requirement of continuous monthly coverage across all 144 pre-pandemic months from January 2008 to December 2019, plus disease-specific screening for sparse counts, non-seasonal or weak signal, and residual or unspecified disease definitions. Nineteen diseases were kept for descriptive trend, age, and spatial analyses but excluded from counterfactual forecasting because of insufficient pre-pandemic duration, sparse counts, non-seasonal or weak signal, or residual/unspecified disease definitions. The final forecasting subset therefore contained 24 diseases, as summarized in **Supplementary Table S1**, and **Supplementary Table S3**.

### Construction of the monthly analytical cache

Monthly national counts for 2008 to 2024 were imported from cleaned Bureau of Epidemiology surveillance tables and restricted to national totals. Annual denominators were linked from the United Nations World Population Prospects file, and annual province-level denominators for recent weekly surveillance outputs were linked from province population tables. Incidence and mortality rates were recalculated as counts per 100 000 population after all replacements and harmonization steps, rather than being inherited directly from the raw rate files.

For disease $d$ in year or month $t$, incidence and mortality rates were calculated as

$$
\mathrm{Incidence}_{d,t} = \frac{\mathrm{Cases}_{d,t}}{\mathrm{Population}_t} \times 100000,
\qquad
\mathrm{Mortality}_{d,t} = \frac{\mathrm{Deaths}_{d,t}}{\mathrm{Population}_t} \times 100000.
$$

To support complete follow-up through December 2025, weekly surveillance extracts were collected separately for cases and deaths from the Department of Disease Control dashboard and harmonized to a common disease-name dictionary, province-name dictionary, and International Organization for Standardization (ISO) week-year calendar. In the final analytical cache used by the main models, official monthly data were retained for the stable overlap period and reconstructed weekly-based monthly values were used to fill the incompletely updated late period, especially 2024–2025. This design allowed the main analyses to preserve the historical monthly reporting backbone while extending the series to a complete endpoint for forecasting comparisons.

### Monthly reconstruction and validation

Case counts were reconstructed from weekly totals by a constrained temporal disaggregation procedure implemented at the disease-year level. First, each ISO week was mapped to its set of contributing calendar dates and to a representative week-midpoint date, with explicit handling of ISO week 1 in late December and ISO weeks 52–53 in early January. Second, a natural cubic spline was fitted to the sequence of observed weekly totals against week-midpoint dates. Third, the spline was evaluated on each day of the year to obtain a preliminary daily trajectory, negative values were truncated to zero, and the daily predictions were rescaled within each ISO week so that the reconstructed daily values summed exactly to the original observed weekly total. Fourth, fractional daily values were converted to integer counts by flooring and then redistributing the remaining counts to days with the largest fractional remainders. When there were too few observed weeks to support spline fitting, or when spline fitting was unstable, the weekly total was distributed evenly across the contributing days as a fallback. This algorithm therefore preserved observed weekly sums exactly while producing a smooth day-level bridge to calendar months.

If week $w$ had observed total $W_w$, spline-based non-negative preliminary daily weights $g_{w,d}$ for days $d \in w$, and $\sum_{d \in w} g_{w,d} > 0$, the rescaled daily counts were

$$
x_{w,d} = W_w \times \frac{g_{w,d}}{\sum_{d \in w} g_{w,d}}.
$$

Monthly reconstructed cases were then obtained by summing the daily values belonging to month $m$:

$$
\widehat{C}_m = \sum_{d \in m} x_d.
$$

Deaths were reconstructed more conservatively. Weekly death totals were not spline-smoothed; instead, each week was decomposed into its contributing dates and the weekly total was allocated to months in direct proportion to the number of days of that week falling within each calendar month. Monthly deaths were then summed after rounding the apportioned values. This distinction between cases and deaths was prespecified because deaths were much sparser for many diseases and did not justify an additional smoothing layer.

For weekly deaths $D_w$, if week $w$ contributed $n_{w,m}$ days to month $m$ and $n_w$ days in total, the apportioned deaths for that month were

$$
\widehat{D}_{w,m} = D_w \times \frac{n_{w,m}}{n_w},
\qquad
\widehat{D}_m = \sum_w \widehat{D}_{w,m}.
$$

Validation of the weekly-to-monthly reconstruction was conducted in the overlap period for which both official monthly data and weekly surveillance data were available. Disease-specific panels in **Supplementary Fig. S1-S86** compare observed weekly totals, reconstructed weekly totals, and official versus reconstructed monthly totals. Summary metrics in **Supplementary Tables S6 and S7** were computed from disease-month pairs in 2020–2023 and included Pearson correlation, mean and median absolute error, and mean and median absolute percentage error. The main text reports the overlap-period summaries for the full set of retained disease-month observations and for selected high-burden diseases that materially drive the national results.

### Descriptive trend, ranking, age, and spatial analyses

For descriptive national summaries, monthly disease-specific counts were aggregated to annual totals and to monthly all-disease totals. Disease rankings by cumulative burden and fatal burden were obtained directly from summed counts across the study period. Category-specific temporal panels in **Supplementary Fig. S114-S123** were produced by aggregating counts within transmission group and extracting smooth trend components using seasonal-trend decomposition using Loess (STL). The category heatmaps displayed within-disease normalized incidence or mortality rates to emphasize within-series temporal structure rather than absolute scale.

Age-specific analyses required additional harmonization because historical monthly age files and recent weekly dashboard age outputs used different age-bin definitions. Historical age-stratified data were therefore converted to single-year age estimates with a penalized composite link model and then re-aggregated into seven target groups: 0–4, 5–9, 10–14, 15–19, 20–39, 40–59, and 60+ years. To stabilize estimation, a small constant was added before fitting when needed, fitted values were rescaled to preserve the original group total exactly, and a simple uniform split was used when the total count in a disease-year cell was too sparse to support reliable smoothing. Recent weekly age data were collapsed into the same seven target groups and appended to the harmonized historical series. Validation in 2020–2023 compared reconstructed age-group totals against directly observed weekly age totals; the corresponding scatterplots are shown in **Supplementary Fig. S113**. Age-ranking figures were then summarized in two-year blocks to reduce visual noise and to emphasize shifts in the leading diseases within each age stratum.

The age-harmonization validation is shown explicitly in **Supplementary Fig. S113**, which compares estimated and directly observed age-group totals for both cases and deaths during the 2020–2023 overlap period. This figure was included to document that the age-reconstruction step was adequate for the downstream age-specific descriptive analyses rather than to support a separate inferential result.

Province-level analyses combined two sources. For 2008–2023, province-specific incidence and mortality rates were obtained from the cleaned rate files after excluding national, zone-level, and region-level records. For the recent weekly period, province-specific case and death totals were aggregated from dashboard extracts and converted to province-year incidence and mortality rates using province population denominators. Province names were harmonized to the Database of Global Administrative Areas (GADM) Thailand shapefile before mapping. **Supplementary Fig. S124-S125** presents the overview province maps for the leading disease by incidence and mortality across calendar years; the full disease-specific spatial panels were generated in the same appendix output set.

### Trend decomposition and breakpoint estimation

National monthly incidence and mortality rates were decomposed with seasonal-trend decomposition using Loess (STL) using periodic seasonal windows and robust fitting. To estimate breakpoint structure without overfitting monthly noise, the number of joinpoints was first selected from annual aggregate incidence and mortality using the callable Joinpoint implementation with a log-linear model, Bayesian information criterion model selection, and up to four joinpoints. The corresponding monthly STL trend component was then fitted with segmented log-linear regression, using the number of annual joinpoints to determine how many monthly breakpoints to estimate. Those fixed breakpoint locations were finally imposed on segmented log-linear models fitted to the raw monthly incidence and mortality series, from which segment-specific annual percent change (APC) and 95% confidence intervals were calculated by transforming the monthly slope to the annual scale. This two-stage design was intended to stabilize breakpoint location while preserving monthly resolution for APC estimation.

If the segmented model estimated a monthly log-slope $\beta$, the annual percent change was calculated as

$$
\mathrm{APC} = \left(e^{12\beta} - 1\right) \times 100\%.
$$

### Counterfactual model selection and forecasting

This section records the notation used by the appendix exports for the disease-specific forecasting workflow described in the main Methods. The pre-pandemic training window was January 2008 to December 2019, January 2020 was treated as the common national interruption date, and monthly counts were square-root transformed after adding a constant of 0.01. Seven candidate model families were compared: autoregressive neural networks, information-criterion-selected exponential-smoothing models, information-criterion-selected seasonal autoregressive integrated moving-average models, trigonometric state-space models for complex seasonality with a fixed 12-month cycle, hybrid ensembles combining autoregressive, exponential-smoothing, neural-network, and complex-seasonality components, Bayesian structural time-series models with local linear trend and seasonal states, and autoregressive models augmented with Fourier harmonic terms.

For observed monthly counts $Y_t$, the transformed modelling scale was

$$
Y_t^* = \sqrt{Y_t + 0.01}.
$$

Within each disease and hold-out split, back-transformed symmetric mean absolute percentage error (sMAPE), root mean squared error (RMSE), and mean absolute scaled error (MASE) values were standardized across the seven candidate models, multiplied by -1 so that larger values indicated better performance, and summed to give the split-specific composite score. These split-specific scores were then summed across the three hold-out schemes exactly as described in the main Methods. The resulting cross-validation outputs were exported to **Supplementary Fig. S87-S110**, and robustness of the selected family to alternative aggregation rules was summarized in **Supplementary Table S11**.

Let $e_{m,s,k}$ denote the error metric for model $m$, split $s$, and metric $k \in \{\mathrm{sMAPE},\mathrm{RMSE},\mathrm{MASE}\}$. Standardization was performed within each disease and split across the seven candidate models:

$$
z_{m,s,k} = -\frac{e_{m,s,k} - \bar e_{s,k}}{\mathrm{sd}(e_{s,k})}.
$$

The split-specific composite score was then

$$
\mathrm{Score}_{m,s} = \sum_k z_{m,s,k},
$$

and the final selection score for each model was

$$
\mathrm{Score}_m^{\mathrm{total}} = \sum_s \mathrm{Score}_{m,s}.
$$

The model with the largest $\mathrm{Score}_m^{\mathrm{total}}$ was selected.

After model selection, the winning specification for each disease was refitted to the full pre-pandemic series and simulated 5,000 times through December 2025. Forecast uncertainty was summarized from these simulated trajectories. Residual-bootstrap forward simulation was used for the autoregressive neural-network, seasonal autoregressive integrated moving-average, trigonometric state-space, and Fourier-augmented autoregressive families; transformed-scale one-step residual bootstraps around the deterministic forecast were used for the exponential-smoothing family to avoid pathological right tails in strongly seasonal series; historical-residual resampling around the mean forecast was used for the hybrid ensemble family; and posterior predictive draws after burn-in were used for the Bayesian structural family. The appendix exports the cross-validation accuracy summaries, interval summaries, and disease-level forecast-versus-observed tables that support **Fig. 2** and **Supplementary Fig. S87-S110**.

### Recovery, balance, suppression, and rebound metrics

This section records the notation used by the supplementary tables and figures for the Return to Pre-pandemic baseline (RP) and Balance of Pre-pandemic cumulative deficit (BP) workflow described in the main Methods.

Formally, if observed monthly cases were $O_t$ and the counterfactual median was $E_t$, then

$$
D_t = O_t - E_t,
\qquad
C_t = \sum_{\tau=1}^{t} D_{\tau}.
$$

Recovery period was implemented as the first month in a left-aligned 3-month window for which two conditions held throughout the window: observed counts were at least 95% of the counterfactual median, and the cumulative deviation was no longer decreasing from month to month. Balance period was defined independently as the first month after the trough when cumulative deviation returned to zero or greater. A disease could therefore satisfy RP without satisfying BP.

Equivalently, RP was the earliest month $t$ such that for $j=t,t+1,t+2$,

$$
O_j \ge 0.95 E_j
$$

and

$$
C_j - C_{j-1} \ge 0.
$$

BP was the earliest post-trough month $t$ for which

$$
C_t \ge 0.
$$

Additional disease-level metrics were derived from the same cumulative-deviation process. Suppression duration was the number of months from disruption onset to RP, or to the end of follow-up for diseases that never recovered. Payback duration was the number of months from trough to BP. Relative suppression and rebound intensity were defined as follows.

If $t^*$ denotes the trough month, relative suppression was

$$
\mathrm{Relative\ suppression} = \frac{|C_{t^*}|}{\sum_{\tau=1}^{t^*} E_{\tau}},
$$

and rebound intensity was

$$
\mathrm{Rebound\ intensity} = \max_{t \ge t^*} \frac{O_t}{E_t + 1}.
$$

### Seasonal displacement and rebound typology analyses

This section retains only the appendix notation for the seasonal-comparison workflow described in the main Methods. Monthly counts were averaged across years within each disease-scenario to create a mean annual profile, and seasonal shape was assessed by min-max normalization so that within-year timing could be compared independently of absolute burden.

If $M_m$ denotes the mean count in calendar month $m$, the normalized seasonal shape was

$$
M_m^{\mathrm{norm}} = \frac{M_m - \min(M)}{\max(M) - \min(M)}.
$$

Timing was evaluated in two ways: empirical peak month and a weighted circular center-of-mass estimator.

For the circular center-of-mass estimator, month $m$ was mapped to angle

$$
\theta_m = \frac{2\pi(m-1)}{12},
$$

and the weighted phase centre was obtained from

$$
x = \sum_m M_m \cos(\theta_m),
\qquad
y = \sum_m M_m \sin(\theta_m),
$$

followed by

$$
\theta_{\mathrm{COM}} = \mathrm{atan2}(y, x).
$$

Phase shifts were then corrected to the minimal signed displacement on a 12-month cycle, restricted to the interval $[-6, 6]$ months.

Among diseases with measurable cumulative deficit, suppression-rebound patterns were summarized by relative suppression magnitude and rebound intensity. The association between suppression duration and rebound intensity was examined with Pearson correlation and a linear trend overlay used only as a visualization aid. Disease typologies were then explored by k-means clustering after standardizing relative suppression and rebound intensity, but this step was retained only as a descriptive visual grouping aid and was not used in the main decision framework. A three-cluster solution was retained for the displayed typology figure (Supplementary Fig. S130) on the basis of the elbow method.

### Exploratory disease-level recovery-speed analyses

Exploratory determinants of recovery speed were analysed at the disease level using univariable Cox proportional-hazards models. The time scale was suppression duration, defined from the first month of cumulative deficit to RP; diseases that had not recovered by December 2025 were right-censored at the end of follow-up. Predictor metadata were curated in advance in the project workbook and included transmission category, incubation period, infectious period, vaccination availability, vaccine-induced protection, and natural protection. Because only a small number of diseases contributed to these analyses and several categories were imbalanced, these models were treated strictly as descriptive exploratory comparisons and were not used for formal etiological inference.

### Sensitivity and robustness analyses

Appendix sensitivity outputs include alternative RP thresholds and persistence windows in **Supplementary Table S5**, an exploratory threshold-tolerance stress test in **Supplementary Fig. S129**, alternative model-selection rules in **Supplementary Table S11**, weekly-to-monthly overlap validation in **Supplementary Tables S6 and S7** and **Supplementary Fig. S1-S86**, and age-harmonization validation in **Supplementary Fig. S113**.

### Uncertainty-aware recovery and operational synthesis

Appendix exports from the main uncertainty workflow include disease-specific RP/BP probabilities, no-deficit probabilities, empirical 95% intervals for RP and BP month, and retention of the deterministic median-based phenotype (**Supplementary Table S9**). Deterministic interruption-date reruns are summarized in **Supplementary Table S10**, and alternative model-selection-rule outputs are summarized in **Supplementary Table S11**.

To connect the recovery and seasonality modules to practical surveillance review, we created a retrospective operational synthesis that combined deterministic RP/BP phenotype with the center-of-mass phase-shift summaries. For this synthesis only, a substantial seasonal displacement flag was defined pragmatically as an absolute phase shift of at least 2 months relative to either the pre-pandemic observed seasonal profile or the post-public-health and social measures (PHSM) counterfactual seasonal profile. The resulting portfolio-level prioritization table is shown in **Supplementary Table S12**.

### Alternative endpoints, contextual triangulation, and task-based interface assessment

Appendix-only extensions include three prespecified alternative recovery endpoints (**Supplementary Table S13**), descriptive temporal triangulation against national Oxford COVID-19 Government Response Tracker indicators and World Health Organization (WHO) COVID-19 case counts (**Supplementary Tables S14–S16**), and an author-side heuristic walkthrough of six public-health review tasks scored on discoverability, interpretability, and auditability (**Supplementary Table S17**).

<div style="page-break-after: always;"></div>

