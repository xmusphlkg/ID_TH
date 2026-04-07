<div style="text-align:center;">
  <h3 style="font-family: inherit; font-weight: normal; margin-bottom: 0;">Supplementary Appendix:</h3>
  <h1 style="font-family: inherit; font-weight: bold; font-size: 1.5em;">Counterfactual digital decision support for infectious disease recovery in Thailand</h1>
  <br>
  <br>
  Kangguo Li et al. (2026)
</div>


<div style="page-break-after: always;"></div>

## Supplementary Methods

### Overview of the analytical workflow

The supplementary analyses were generated from a modular R and Python workflow that mirrored the main paper pipeline and wrote intermediate analytical objects to a version-locked cache before figure and table production. Raw monthly surveillance extracts were cleaned in Python and stored in disease-specific case, death, age-stratified, and rate files. The analytical workflow then proceeded in R through four linked stages: construction of the national monthly analytical cache; generation of descriptive outputs for overall burden, category-specific trends, age structure, and province-level patterns; selection and fitting of disease-specific counterfactual forecasting models; and derivation of recovery, balance, seasonal-shift, suppression, rebound, and exploratory recovery-speed metrics. All supplementary tables and figures were exported from the same cached analytical objects used for the manuscript results, so the appendix reports the operational implementation rather than a separate post hoc analysis stream.

### Disease-series curation and analytical subsets

The source registry comprised 72 nationally notifiable disease series. Series were first screened for inclusion in the descriptive portfolio using explicit metadata flags and manual disease-name harmonization tables stored in the project workbook. Exclusion before the 43-disease descriptive analysis followed six operational categories: zero incidence throughout follow-up, overlapping aggregate surveillance categories, diseases not aligned with the infectious-disease focus of the paper, unstable or residual categories, incomplete recent reporting, and structural changes in surveillance definition. **Supplementary Table S2** lists the series removed at this stage. The retained 43 series were then reviewed for counterfactual suitability using a minimum requirement of continuous monthly coverage across all 144 prepandemic months from January, 2008 to December, 2019, plus disease-specific screening for sparse counts, non-seasonal or weak signal, and residual or unspecified disease definitions. Nineteen diseases were kept for descriptive trend, age, and spatial analyses but excluded from counterfactual forecasting because of insufficient prepandemic duration, sparse counts, non-seasonal or weak signal, or residual/unspecified disease definitions. The final forecasting subset therefore contained 24 diseases, as summarized in **Supplementary Table S1**, **Supplementary Fig. S0**, and **Supplementary Table S3**.

### Construction of the monthly analytical cache

Monthly national counts for 2008–2024 were imported from cleaned Bureau of Epidemiology surveillance tables and restricted to national totals. Annual denominators were linked from the United Nations World Population Prospects file, and annual province-level denominators for recent weekly surveillance outputs were linked from province population tables. Incidence and mortality rates were recalculated as counts per 100 000 population after all replacements and harmonization steps, rather than being inherited directly from the raw rate files.

For disease $d$ in year or month $t$, incidence and mortality rates were calculated as

$$
\mathrm{Incidence}_{d,t} = \frac{\mathrm{Cases}_{d,t}}{\mathrm{Population}_t} \times 100000,
\qquad
\mathrm{Mortality}_{d,t} = \frac{\mathrm{Deaths}_{d,t}}{\mathrm{Population}_t} \times 100000.
$$

To support complete follow-up through December, 2025, weekly surveillance extracts were collected separately for cases and deaths from the Department of Disease Control dashboard and harmonized to a common disease-name dictionary, province-name dictionary, and ISO week-year calendar. In the final analytical cache used by the main models, official monthly data were retained for the stable overlap period and reconstructed weekly-based monthly values were used to fill the incompletely updated late period, especially 2024–2025. This design allowed the main analyses to preserve the historical monthly reporting backbone while extending the series to a complete endpoint for forecasting comparisons.

### Weekly-to-monthly reconstruction and validation

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

For descriptive national summaries, monthly disease-specific counts were aggregated to annual totals and to monthly all-disease totals. Disease rankings by cumulative burden and fatal burden were obtained directly from summed counts across the study period. Category-specific temporal panels in **Supplementary Fig. S114-S123** were produced by aggregating counts within transmission group and extracting smooth trend components using STL decomposition. The category heatmaps displayed within-disease normalized incidence or mortality rates to emphasize within-series temporal structure rather than absolute scale.

Age-specific analyses required additional harmonization because historical monthly age files and recent weekly dashboard age outputs used different age-bin definitions. Historical age-stratified data were therefore converted to single-year age estimates with a penalized composite link model and then re-aggregated into seven target groups: 0–4, 5–9, 10–14, 15–19, 20–39, 40–59, and 60+ years. To stabilize estimation, a small constant was added before fitting when needed, fitted values were rescaled to preserve the original group total exactly, and a simple uniform split was used when the total count in a disease-year cell was too sparse to support reliable smoothing. Recent weekly age data were collapsed into the same seven target groups and appended to the harmonized historical series. Validation in 2020–2023 compared reconstructed age-group totals against directly observed weekly age totals; the corresponding scatterplots are shown in **Supplementary Fig. S113**. Age-ranking figures were then summarized in two-year blocks to reduce visual noise and to emphasize shifts in the leading diseases within each age stratum.

The age-harmonization validation is shown explicitly in **Supplementary Fig. S113**, which compares estimated and directly observed age-group totals for both cases and deaths during the 2020–2023 overlap period. This figure was included to document that the age-reconstruction step was adequate for the downstream age-specific descriptive analyses rather than to support a separate inferential result.

Province-level analyses combined two sources. For 2008–2023, province-specific incidence and mortality rates were obtained from the cleaned rate files after excluding national, zone-level, and region-level records. For the recent weekly period, province-specific case and death totals were aggregated from dashboard extracts and converted to province-year incidence and mortality rates using province population denominators. Province names were harmonized to the GADM Thailand shapefile before mapping. **Supplementary Fig. S124-S125** presents the overview province maps for the leading disease by incidence and mortality across calendar years; the full disease-specific spatial panels were generated in the same appendix output set.

### Trend decomposition and breakpoint estimation

National monthly incidence and mortality rates were decomposed with STL using periodic seasonal windows and robust fitting. To estimate breakpoint structure without overfitting monthly noise, the number of joinpoints was first selected from annual aggregate incidence and mortality using the callable Joinpoint implementation with a log-linear model, Bayesian information criterion model selection, and up to four joinpoints. The corresponding monthly STL trend component was then fitted with segmented log-linear regression, using the number of annual joinpoints to determine how many monthly breakpoints to estimate. Those fixed breakpoint locations were finally imposed on segmented log-linear models fitted to the raw monthly incidence and mortality series, from which segment-specific annual percent change and 95% confidence intervals were calculated by transforming the monthly slope to the annual scale. This two-stage design was intended to stabilize breakpoint location while preserving monthly resolution for APC estimation.

If the segmented model estimated a monthly log-slope $\beta$, the annual percent change was calculated as

$$
\mathrm{APC} = \left(e^{12\beta} - 1\right) \times 100\%.
$$

### Counterfactual model selection and forecasting

Counterfactual forecasts were estimated separately for each of the 24 diseases retained for modelling. The prepandemic training window was January, 2008 to December, 2019, and January, 2020 was treated as the common national interruption date for all diseases. Monthly counts were square-root transformed after adding a constant of 0.01 so that zero-count months remained estimable while preserving a positive-support transformation for low-count series. Seven candidate model families were compared: neural network autoregression, ETS, seasonal ARIMA, TBATS, a weighted hybrid model combining autoregressive and exponential-smoothing families, Bayesian structural time-series models with local linear trend and seasonal states, and autoregressive integrated moving average with Fourier terms.

For observed monthly counts $Y_t$, the transformed modelling scale was

$$
Y_t^* = \sqrt{Y_t + 0.01}.
$$

Model selection was based on three rolling prepandemic hold-out schemes: 2019 alone, 2018–2019, and 2017–2019. Within each split and disease, every model was fitted on the corresponding training segment and forecast over the withheld period. Forecast performance was summarized by sMAPE, RMSE, and MASE-type absolute-error metrics on the back-transformed scale. For each disease and split, these metrics were z-standardized across the seven candidate models and multiplied by −1 so that better performance corresponded to larger values; the standardized metrics were then summed with equal weight to give a split-specific composite score. Composite scores were summed across the three hold-out schemes, and the highest-scoring model was selected as the primary disease-specific counterfactual specification. Absolute cross-validation outputs for each model family and hold-out split were exported to **Supplementary Fig. S87-S110**, and robustness of the selected family to alternative aggregation rules was summarized in **Supplementary Table S11**.

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

After model selection, the winning specification for each disease was refitted to the full prepandemic series and forecast forward from January, 2020 to December, 2025. Forecast uncertainty was summarized from 5000 simulated trajectories. For neural network, SARIMA, TBATS, and ARIMA-with-Fourier models, future trajectories were generated by residual-bootstrap simulation. For ETS, future trajectories were generated by bootstrapping transformed-scale one-step residuals around the deterministic forecast to avoid pathological right tails in strongly seasonal series. For the hybrid model, forecast uncertainty was approximated by resampling historical residuals around the model mean forecast. For BSTS, posterior predictive draws were taken directly from the predictive distribution after burn-in. The appendix exports both the simulated interval summaries and the disease-level forecast-versus-observed tables used in **Fig. 2** and in **Supplementary Fig. S87-S110**.

### Recovery, balance, suppression, and rebound metrics

All recovery metrics were derived from disease-specific monthly observed counts and the corresponding counterfactual median forecast beginning in January, 2020. For month $t$, the monthly deviation was defined as observed minus counterfactual cases and the cumulative deviation as the running sum of those monthly deviations. Disruption onset was defined operationally as the first month after January, 2020 when cumulative deviation became negative. The trough was the month of minimum cumulative deviation.

Formally, if observed monthly cases were $O_t$ and the counterfactual median was $E_t$, then

$$
D_t = O_t - E_t,
\qquad
C_t = \sum_{\tau=1}^{t} D_{\tau}.
$$

Recovery period was implemented as the first month in a left-aligned 3-month window for which two conditions held throughout the window: observed counts were at least 95% of the counterfactual median, and the cumulative deviation was no longer decreasing from month to month. Diseases meeting this criterion were labelled recovered. Balance period was defined independently as the first month after the trough when cumulative deviation returned to zero or greater. Diseases that reached this zero-crossing were labelled balanced in the exported analytical tables. A disease could therefore satisfy RP without satisfying BP, which is the basis of the decoupling emphasized in the manuscript.

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

Additional disease-level metrics were derived from the same cumulative-deviation process. Relative suppression was the absolute trough deficit divided by the cumulative expected burden up to the trough. Rebound intensity was the maximum post-trough ratio of observed to expected incidence, calculated from the observed monthly count divided by the counterfactual median plus one case to stabilize very small denominators. Suppression duration was the number of months from disruption onset to RP, or to the end of follow-up for diseases that never recovered. Payback duration was the number of months from trough to BP. The sensitivity analyses in **Supplementary Table S5** repeated the RP rule under alternative thresholds of 90%, 95%, and 100% and persistence windows of 2, 3, and 4 months. Additional robustness summaries compared interruption dates and alternative model-selection aggregation rules against the primary disease-specific best-model workflow (Supplementary Tables S10-S11).

If $t^*$ denotes the trough month, relative suppression was

$$
\mathrm{Relative\ suppression} = \frac{|C_{t^*}|}{\sum_{\tau=1}^{t^*} E_{\tau}},
$$

and rebound intensity was

$$
\mathrm{Rebound\ intensity} = \max_{t \ge t^*} \frac{O_t}{E_t + 1}.
$$

### Seasonal displacement and rebound typology analyses

Seasonal comparisons were based on three scenario-specific monthly profiles: prepandemic observed data through 2019, post-PHSM observed data from 2023 onward, and post-PHSM counterfactual predictions from 2023 onward. For each disease and scenario, monthly counts were averaged across years to create a mean annual profile. Two complementary quantities were derived. First, seasonal shape was assessed by min-max normalization within each disease-period so that the monthly profile ranged from 0 to 1, allowing cross-scenario comparison of within-year timing independent of absolute burden. Second, seasonal amplitude was quantified with peak-to-trough and mean-standardized amplitude ratios.

If $M_m$ denotes the mean count in calendar month $m$, the normalized seasonal shape was

$$
M_m^{\mathrm{norm}} = \frac{M_m - \min(M)}{\max(M) - \min(M)}.
$$

Timing was evaluated in two ways. The first was the empirical peak month defined by the month with the largest average count. The second was a weighted circular centre-of-mass estimator in which months were mapped onto a 12-month circle and weighted by the monthly mean count. Phase shifts were calculated as the difference in peak timing between post-PHSM observed profiles and either prepandemic observed or post-PHSM counterfactual profiles, then mapped onto the minimal signed displacement on a 12-month cycle so that positive and negative shifts remained interpretable. These quantities underpin the seasonal plots and peak markers shown in **Fig. 4**.

For the circular centre-of-mass estimator, month $m$ was mapped to angle

$$
theta_m = \frac{2\pi(m-1)}{12},
$$

and the weighted phase centre was obtained from

$$
x = \sum_m M_m \cos(\theta_m),
\qquad
y = \sum_m M_m \sin(\theta_m),
$$

followed by

$$
theta_{\mathrm{COM}} = \mathrm{atan2}(y, x).
$$

Phase shifts were then corrected to the minimal signed displacement on a 12-month cycle, restricted to the interval $[-6, 6]$ months.

Among diseases with measurable cumulative deficit, suppression-rebound patterns were summarized by relative suppression magnitude and rebound intensity. The association between suppression duration and rebound intensity was examined with Pearson correlation and a linear trend overlay used only as a visualization aid. Disease typologies were then explored by k-means clustering after standardizing relative suppression and rebound intensity. A three-cluster solution was retained for the displayed typology figure on the basis of the elbow method.

### Exploratory disease-level recovery-speed analyses

Exploratory determinants of recovery speed were analysed at the disease level using univariable Cox proportional-hazards models. The time scale was suppression duration, defined from the first month of cumulative deficit to RP; diseases that had not recovered by December, 2025 were right-censored at the end of follow-up. Predictor metadata were curated in advance in the project workbook and included transmission category, incubation period, infectious period, vaccination availability, vaccine-induced protection, and natural protection. Because only a small number of diseases contributed to these analyses and several categories were imbalanced, these models were treated strictly as descriptive exploratory comparisons and were not used for formal etiological inference.

### Sensitivity and robustness analyses

Several supplementary sensitivity analyses were prespecified to test whether the main qualitative interpretation depended on a single operational definition or a single model-selection rule. First, the RP rule was recalculated under alternative recovery thresholds of 90%, 95%, and 100% of the counterfactual median and persistence windows of 2, 3, and 4 consecutive months. These analyses yielded the reclassification summary reported in **Supplementary Table S5**. Second, we compared the disease-specific best model selected by the primary equal-weight composite with alternatives based on rank aggregation, sMAPE-only aggregation, and a horizon-weighted composite that emphasized the shortest extrapolation window. These model-selection rule checks are summarized in **Supplementary Table S11**.

In addition to these classification-focused checks, two reconstruction-validation exercises were retained in the appendix because they support the validity of upstream processing choices. Weekly-to-monthly overlap validation for case reconstruction is summarized in **Supplementary Tables S6 and S7** and visualized in **Supplementary Fig. S1-S86**, whereas age-harmonization validation is shown in **Supplementary Fig. S113**. Together, these checks were intended to show that the main findings were not driven solely by a specific reconstruction rule, RP threshold, or counterfactual model-family choice.

### Uncertainty-aware recovery and operational synthesis

To move beyond deterministic recovery labels, we reran the RP/BP algorithm across the 5000 simulated counterfactual trajectories already generated for each disease. This produced disease-specific probabilities of achieving RP and BP by the end of follow-up, a probability of no sustained cumulative deficit, empirical 95% intervals for RP and BP month, and the probability that the deterministic median-based phenotype was retained. These outputs are summarized in **Supplementary Table S9** and were used in the revised main text to distinguish stable RP-only or BP-positive classifications from diseases with substantial state uncertainty. For operational interpretation only, diseases with primary-status probability below 0.80 were flagged as uncertainty-sensitive.

We also evaluated whether the main recovery phenotypes depended materially on a single analytical interruption date. Deterministic RP/BP classifications were therefore recalculated with analytical start dates of January 2020, March 2020, and April 2020. The corresponding disease-level results are summarized in **Supplementary Table S10**. Finally, because the main model-selection procedure uses an equal-weight composite across three error metrics and three validation windows, we compared the selected model family against rank aggregation, sMAPE-only aggregation, and a horizon-weighted composite that emphasized the shortest extrapolation window. Those comparisons are summarized in **Supplementary Table S11**.

To connect the recovery and seasonality modules to practical surveillance review, we created a retrospective operational synthesis that combined deterministic RP/BP phenotype with the center-of-mass phase-shift summaries. For this synthesis only, a substantial seasonal displacement flag was defined pragmatically as an absolute phase shift of at least 2 months relative to either the pre-pandemic observed seasonal profile or the post-PHSM counterfactual seasonal profile. Monthly-normalized diseases were then separated into cumulative-review, recalibration, and low-priority groups, whereas non-recovered diseases were flagged for manual review. The resulting portfolio-level prioritization table is shown in **Supplementary Table S12**.

### Alternative endpoints, contextual triangulation, and task-based interface assessment

To test whether the principal interpretation depended on one monthly recovery endpoint, we evaluated three prespecified alternatives using the deterministic median forecast for each disease. The first was the earliest 3-month window in which observed counts re-entered the disease-specific 95% predictive interval. The second was the earliest 3-month window in which the observed-to-expected ratio was at least 1.0 throughout. The third was a cumulative intermediate milestone defined as the first month after the trough when the cumulative deficit had halved, that is, when cumulative deviation rose to at least half of the trough deficit. These outputs are summarized in **Supplementary Table S13** and were used to distinguish endpoints that preserved the main RP signal from endpoints that were too permissive for operational triage.

External context was incorporated only for descriptive temporal triangulation, not as part of the predictive engine. National Oxford COVID-19 Government Response Tracker indicators for Thailand were aggregated from daily values to calendar months for January, 2020 to December, 2022 and included stringency, government response intensity, school closing, internal movement restrictions, international travel controls, and testing policy. Weekly WHO COVID-19 case counts for Thailand were aggregated to calendar months through June, 2024. These series were compared against the portfolio-level observed-to-expected ratio across the 24 modelled diseases. Period summaries are shown in **Supplementary Table S14**, and correlation and milestone summaries are shown in **Supplementary Table S15**. Because policy coverage ended in December, 2022, later follow-up was contextualized only by WHO COVID-19 burden and the internal surveillance outputs.

The digital decision-support layer was additionally assessed with an author-side task-based heuristic walkthrough of a frozen dashboard build after the prioritization tab had been added. Six public-health review tasks were prespecified and scored on three 1-4 dimensions: discoverability, interpretability, and auditability. This assessment was intended to document whether the implemented interface covered the main decision tasks implied by the manuscript, not to substitute for formal end-user usability testing. The task-level results are presented in **Supplementary Table S16**.

<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S1 -->
**Table S1. Disease flow from 72 monitored series to the 43-disease descriptive analysis and 24-disease counterfactual analysis.**

| Stage | N |
| --- | --- |
| All monitored notifiable diseases | 72 |
| Included in descriptive 43-disease analysis | 43 |
| Included in 24-disease counterfactual analysis | 24 |

**Supplementary Fig. S0. Two-stage disease-selection flow for the analytical subsets.**

```text
72 monitored notifiable disease series
  -> Excluded before descriptive analysis (n = 29)
     11 overlapping surveillance categories
     6 conditions not aligned with the transmissible infectious-disease framework
     5 ill-defined or residual categories
     3 zero-incidence series
     3 incompletely reported recent series
     1 series with structural surveillance-definition change
  -> Retained for descriptive 43-disease analysis (n = 43)
     -> Excluded from counterfactual modelling (n = 19)
        12 sparse or insufficient-count series
        3 non-seasonal series
        2 insufficient-duration series
        2 residual or unspecified series
     -> Retained for 24-disease counterfactual analysis (n = 24)
```

<!-- END TABLE_S1 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S2 -->
**Table S2. Excluded disease series and exclusion category.**

| Raw disease name | Exclusion category |
| --- | --- |
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
| STI Total | Overlapping surveillance categories |
| T.B. other organs | Overlapping surveillance categories |
| T.B.meningitis | Overlapping surveillance categories |
| Botulism | Not aligned with the transmissible infectious-disease framework |
| Food_Poisoning | Not aligned with the transmissible infectious-disease framework |
| Mushroom poisoning | Not aligned with the transmissible infectious-disease framework |
| Rabies | Not aligned with the transmissible infectious-disease framework |
| Tetanus exc.Neo | Not aligned with the transmissible infectious-disease framework |
| Tetanus neonatorum | Not aligned with the transmissible infectious-disease framework |
| Genital Molluscum Contagiosum | Ill-defined or residual surveillance categories |
| Hepatitis_uns | Ill-defined or residual surveillance categories |
| L.G.V.&other&unsp.V.D | Ill-defined or residual surveillance categories |
| N.S.U._V | Ill-defined or residual surveillance categories |
| Other STI | Ill-defined or residual surveillance categories |
| Meningococcal Meningitis | Incomplete reporting in the most recent surveillance year |
| Pediculosis Pubis | Incomplete reporting in the most recent surveillance year |
| Vaginal trichomoniasis | Incomplete reporting in the most recent surveillance year |
| TB Total | Structural changes in surveillance definitions |

These 29 excluded series were concentrated in overlapping surveillance categories (11), diseases outside the transmissible infectious-disease framework (6), ill-defined or residual categories (5), zero-incidence series (3), incompletely reported recent series (3), and one series with a structural surveillance-definition change.

<!-- END TABLE_S2 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S3 -->
**Table S3. Diseases retained in the 43-disease descriptive analysis but not modelled counterfactually, with direct reason for descriptive-only retention.**

| Disease | Shortname | Group | Reason for descriptive-only retention |
| --- | --- | --- | --- |
| Cholera | Cholera | Gastrointestinal IDs | Insufficient cases |
| Enterovirus Fever | Enterovirus | Gastrointestinal IDs | Insufficient duration |
| HepatitisE | HEV | Gastrointestinal IDs | Insufficient cases |
| Liver fluke | Liver fluke | Gastrointestinal IDs | Non-seasonal trend |
| Paratyphoid | Paratyphoid | Gastrointestinal IDs | Insufficient cases |
| Encephalitis Total | Encephalitis | Other IDs | Unspecifed disease |
| Meningitis,uns | Other meningitis | Other IDs | Unspecifed disease |
| Diphtheria | Diphtheria | Respiratory IDs | Insufficient cases |
| Leprosy | Leprosy | Respiratory IDs | Insufficient cases |
| Measles Total | Measles | Respiratory IDs | Non-seasonal trend |
| Pertussis | Pertussis | Respiratory IDs | Insufficient cases |
| HepatitisD | HDV | Sexually IDs | Insufficient cases |
| Brucellosis | Brucellosis | Vector-borne and zoonotic IDs | Insufficient cases |
| Chikungunya | Chikungunya | Vector-borne and zoonotic IDs | Non-seasonal trend |
| Filariasis | Filariasis | Vector-borne and zoonotic IDs | Insufficient cases |
| Japanese B encephalitis | JE | Vector-borne and zoonotic IDs | Insufficient cases |
| Kala azar | Leishmaniasis | Vector-borne and zoonotic IDs | Insufficient cases |
| Trichinosis | Trichinosis | Vector-borne and zoonotic IDs | Insufficient cases |
| Zika virus | Zika virus | Vector-borne and zoonotic IDs | Insufficient duration |

Across these 19 diseases, the main reasons for descriptive-only retention were insufficient prepandemic counts or sparse long-horizon signal (12 diseases), non-seasonal prepandemic structure (3 diseases), insufficient time coverage (2 diseases), and ill-defined residual categories (2 diseases). This pattern indicates that the 24-disease forecasting subset was selected primarily on time-series suitability rather than on a single transmission category, although vector-borne and respiratory pathogens remained differentially represented after this second-stage restriction.

<!-- END TABLE_S3 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S4 -->
**Table S4. Predictor definitions used in time-to-recovery analyses**

| Disease | Category | Incubation period, days | Infectious period | Vaccine | Vaccine protection | Natural protection |
| --- | --- | --- | --- | --- | --- | --- |
| Amebiasis | Gastrointestinal IDs |  21 | 1 month - 1 year | Unavailable | None | Short-term |
| Shigellosis | Gastrointestinal IDs |   2 | <1 month | Unavailable | None | Short-term |
| HAV | Gastrointestinal IDs |  28 | 1 month - 1 year | Optional | Long-term | Long-term |
| HFMD | Gastrointestinal IDs |   4 | <1 month | Optional | Short-term | Short-term |
| Typhoid | Gastrointestinal IDs |  10 | 1 month - 1 year | Optional | Short-term | Short-term |
| Chickenpox | Respiratory IDs |  15 | <1 month | Optional | Long-term | Long-term |
| Influenza | Respiratory IDs |   2 | <1 month | EPI | Short-term | Short-term |
| Mumps | Respiratory IDs |  17 | <1 month | EPI | Long-term | Long-term |
| Pneumonia | Respiratory IDs |  14 | <1 month | Optional | Short-term | Short-term |
| Rubella | Respiratory IDs |  17 | <1 month | EPI | Long-term | Long-term |
| Scarlet fever | Respiratory IDs |   3 | <1 month | Unavailable | None | Short-term |
| Chancroid | Sexually IDs |   6 | <1 month | Unavailable | None | None |
| CA (HPV) | Sexually IDs |  90 | 1 month - 1 year | EPI | Long-term | None |
| Genital herpes | Sexually IDs |   7 | >1 year | Unavailable | None | None |
| Gonorrhoea | Sexually IDs |   4 | 1 month - 1 year | Unavailable | None | None |
| HBV | Sexually IDs |  90 | >1 year | EPI | Long-term | Long-term |
| HCV | Sexually IDs | 100 | >1 year | Unavailable | None | None |
| Syphilis | Sexually IDs |  21 | 1 month - 1 year | Unavailable | None | None |
| Dengue fever | Vector-borne and zoonotic IDs |   6 | <1 month | Optional | Long-term | Long-term |
| Leptospirosis | Vector-borne and zoonotic IDs |   8 | 1 month - 1 year | Unavailable | None | Short-term |
| Malaria | Vector-borne and zoonotic IDs |  13 | 1 month - 1 year | Optional | Short-term | Short-term |
| Melioidosis | Vector-borne and zoonotic IDs |   9 | NA | Unavailable | None | Short-term |
| Scrub Typhus | Vector-borne and zoonotic IDs |  10 | NA | Unavailable | None | Short-term |
| S. suis | Vector-borne and zoonotic IDs |   2 | NA | Unavailable | None | Short-term |

This lookup table covers all 24 modelled diseases and supplied the disease-level predictors used in the recovery-timing analyses. Vaccine status was classified as unavailable for 12 diseases, optional for 7 diseases, and part of the national EPI schedule for 5 diseases.

<!-- END TABLE_S4 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S5 -->
**Table S5. Sensitivity of RP/BP classifications to alternative RP thresholds and persistence requirements.**

| Threshold | Consecutive months | Balanced | RP achieved without BP | Suppressed | No deficit | Diseases reclassified vs primary analysis |
| --- | --- | --- | --- | --- | --- | --- |
| 0.95 | 3 | 14 | 8 | 1 | 1 | None |
| 0.90 | 2 | 14 | 8 | 1 | 1 | None |
| 0.90 | 3 | 14 | 8 | 1 | 1 | None |
| 0.90 | 4 | 14 | 8 | 1 | 1 | None |
| 0.95 | 2 | 14 | 8 | 1 | 1 | None |
| 0.95 | 4 | 14 | 8 | 1 | 1 | None |
| 1.00 | 2 | 14 | 8 | 1 | 1 | None |
| 1.00 | 3 | 14 | 8 | 1 | 1 | None |
| 1.00 | 4 | 14 | 8 | 1 | 1 | None |

These sensitivity checks were computed from the exported disease-specific outcome tables underlying Fig. 3. No disease changed RP/BP classification when the RP threshold was varied across 90%, 95%, and 100% with persistence requirements of 2, 3, or 4 months, indicating that the principal recovery typology was stable to plausible operational definition changes.

<!-- END TABLE_S5 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S6 -->
**Table S6. Summary metrics for overlap-period validation of weekly-to-monthly reconstruction.**

| Metric | Value |
| --- | --- |
| Overlap years retained in analytical cache | 2020-2023 |
| Disease-month observations | 2064 |
| Overall Pearson correlation | 0.9982 |
| Mean absolute error, cases | 55.21 |
| Median absolute error, cases | 3.00 |
| Mean absolute percentage error, % | 14.16 |
| Median absolute percentage error, % | 3.90 |

These overlap-period validation summaries were recalculated directly from the disease-month comparison cache used to validate the weekly-to-monthly reconstruction. The refreshed cache retains 2064 disease-month pairs across 2020-2023.

<!-- END TABLE_S6 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S7 -->
**Table S7. Illustrative disease-specific overlap-period reconstruction error metrics.**

| Disease | Mean absolute error | Median absolute error | Mean absolute percentage error, % | Median absolute percentage error, % | Pearson correlation |
| --- | --- | --- | --- | --- | --- |
| Pneumonia | 617.38 | 378.50 | 3.47 | 1.97 | 0.9890 |
| Influenza | 872.10 | 156.50 | 5.58 | 4.31 | 0.9974 |
| Dengue fever | 211.94 | 75.00 | 4.36 | 3.09 | 0.9988 |
| HFMD | 271.90 | 103.50 | 6.10 | 3.72 | 0.9977 |
| HAV | 2.52 | 2.00 | 10.42 | 8.17 | 0.9148 |
| HCV | 3.25 | 3.00 | 7.76 | 5.34 | 0.9942 |

High-burden diseases that materially contribute to the main analyses showed low relative reconstruction error across these 6 illustrative examples, whereas some low-count series had larger percentage error because small absolute monthly differences inflate relative measures. Together with the disease-specific visual comparisons in Part 1, these summaries support the robustness of the reconstructed monthly series for the principal RP/BP and seasonal analyses.

<!-- END TABLE_S7 -->
<div style="page-break-after: always;"></div>

**Table S8. Legacy fixed-family robustness table not rerun in the refreshed square-root, 5000-path primary workflow.**

In the refreshed primary analysis, robustness emphasis shifted to uncertainty propagation (**Table S9**), interruption-date sensitivity (**Table S10**), and alternative model-selection aggregation rules (**Table S11**). Uniform ETS and uniform SARIMA refits were not re-run as part of the updated end-to-end workflow and are therefore not re-reported here.

<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S9 -->
**Table S9. Uncertainty-aware RP/BP classification from 5000 simulated counterfactual trajectories.**

| Shortname | Group | Primary deterministic phenotype | Pr(RP) | Pr(BP) | Primary phenotype stability | RP month, median (95% interval) | BP month, median (95% interval) |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Pneumonia | Respiratory IDs | Balanced | 1.000 | 0.835 | 0.835 | 34 (26-54) | 59 (43-70) |
| Influenza | Respiratory IDs | Recovered but not balanced | 1.000 | 0.000 | 1.000 | 44 (44-44) | NA |
| Chickenpox | Respiratory IDs | Recovered but not balanced | 0.889 | 0.000 | 0.889 | 54 (7-67) | NA |
| Mumps | Respiratory IDs | Recovered but not balanced | 1.000 | 0.000 | 1.000 | 68 (68-68) | NA |
| Scarlet fever | Respiratory IDs | Recovered but not balanced | 0.812 | 0.332 | 0.480 | 60 (6-66) | 65 (34.5-71) |
| Rubella | Respiratory IDs | Suppressed | 0.000 | 0.000 | 1.000 | NA | NA |
| Dengue fever | Vector-borne and zoonotic IDs | Recovered but not balanced | 0.723 | 0.339 | 0.383 | 30 (2-46) | 42 (3-65) |
| Malaria | Vector-borne and zoonotic IDs | No deficit | 0.475 | 0.475 | 0.525 | 3 (0-24) | 4 (1-28) |
| Scrub Typhus | Vector-borne and zoonotic IDs | Balanced | 1.000 | 1.000 | 1.000 | 28 (28-28) | 50 (50-51) |
| Leptospirosis | Vector-borne and zoonotic IDs | Balanced | 0.858 | 0.773 | 0.773 | 11 (1-37) | 32 (3-64.3) |
| Melioidosis | Vector-borne and zoonotic IDs | Balanced | 1.000 | 1.000 | 1.000 | 28 (6-30) | 41 (35-45) |
| S. suis | Vector-borne and zoonotic IDs | Balanced | 1.000 | 1.000 | 1.000 | 5 (5-5) | 7 (7-8) |
| HFMD | Gastrointestinal IDs | Balanced | 1.000 | 1.000 | 1.000 | 9 (9-9) | 37 (32-55) |
| Amebiasis | Gastrointestinal IDs | Balanced | 0.707 | 0.628 | 0.628 | 29 (4-48) | 38 (24-63) |
| Shigellosis | Gastrointestinal IDs | Recovered but not balanced | 0.715 | 0.351 | 0.364 | 7 (3-67) | 23 (3-68) |
| Typhoid | Gastrointestinal IDs | Balanced | 1.000 | 1.000 | 1.000 | 31 (28-36) | 55 (52-61) |
| HAV | Gastrointestinal IDs | Recovered but not balanced | 0.923 | 0.000 | 0.923 | 47 (1-61) | NA |
| Gonorrhoea | Sexually IDs | Balanced | 0.999 | 0.822 | 0.822 | 36 (8-48) | 56 (43-70) |
| Syphilis | Sexually IDs | Balanced | 0.429 | 0.300 | 0.300 | 34 (9-60) | 41 (14-66) |
| HBV | Sexually IDs | Balanced | 1.000 | 1.000 | 1.000 | 38 (36-41) | 52 (51-52) |
| CA (HPV) | Sexually IDs | Balanced | 0.956 | 0.773 | 0.773 | 41 (4-53) | 52 (32-68) |
| Genital herpes | Sexually IDs | Balanced | 1.000 | 1.000 | 1.000 | 36 (31-41) | 52 (51-53) |
| Chancroid | Sexually IDs | Recovered but not balanced | 0.996 | 0.224 | 0.772 | 48 (8-50) | 66 (55-71) |
| HCV | Sexually IDs | Balanced | 0.955 | 0.947 | 0.947 | 25 (0-41) | 38 (1-55) |

Here, primary phenotype stability denotes the probability that the deterministic median-based phenotype was retained across the 5000 simulated trajectories; values below 0.80 were treated as uncertainty-sensitive in the revised main-text review layer. Under that pragmatic flag, 9 diseases were uncertainty-sensitive: Scarlet fever, Dengue fever, Malaria, Leptospirosis, Amebiasis, Shigellosis, Syphilis, CA (HPV), Chancroid.

<!-- END TABLE_S9 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S10 -->
**Table S10. Sensitivity of deterministic RP/BP classification to alternative interruption dates.**

| Shortname | Group | 2020-01 status | 2020-03 status | 2020-04 status | Changed vs January in March analysis | Changed vs January in April analysis |
| --- | --- | --- | --- | --- | --- | --- |
| Pneumonia | Respiratory IDs | Balanced | Balanced | Balanced | No | No |
| Influenza | Respiratory IDs | Recovered but not balanced | Recovered but not balanced | Recovered but not balanced | No | No |
| Chickenpox | Respiratory IDs | Recovered but not balanced | Recovered but not balanced | Recovered but not balanced | No | No |
| Mumps | Respiratory IDs | Recovered but not balanced | Recovered but not balanced | Recovered but not balanced | No | No |
| Scarlet fever | Respiratory IDs | Recovered but not balanced | Recovered but not balanced | Recovered but not balanced | No | No |
| Rubella | Respiratory IDs | Suppressed | Suppressed | Suppressed | No | No |
| Dengue fever | Vector-borne and zoonotic IDs | Recovered but not balanced | Recovered but not balanced | Recovered but not balanced | No | No |
| Malaria | Vector-borne and zoonotic IDs | No deficit | No deficit | No deficit | No | No |
| Scrub Typhus | Vector-borne and zoonotic IDs | Balanced | Balanced | Balanced | No | No |
| Leptospirosis | Vector-borne and zoonotic IDs | Balanced | Balanced | Balanced | No | No |
| Melioidosis | Vector-borne and zoonotic IDs | Balanced | Balanced | Balanced | No | No |
| S. suis | Vector-borne and zoonotic IDs | Balanced | Balanced | Balanced | No | No |
| HFMD | Gastrointestinal IDs | Balanced | Balanced | Balanced | No | No |
| Amebiasis | Gastrointestinal IDs | Balanced | Balanced | Balanced | No | No |
| Shigellosis | Gastrointestinal IDs | Recovered but not balanced | Recovered but not balanced | Recovered but not balanced | No | No |
| Typhoid | Gastrointestinal IDs | Balanced | Balanced | Balanced | No | No |
| HAV | Gastrointestinal IDs | Recovered but not balanced | Recovered but not balanced | Recovered but not balanced | No | No |
| Gonorrhoea | Sexually IDs | Balanced | Balanced | Balanced | No | No |
| Syphilis | Sexually IDs | Balanced | Balanced | Balanced | No | No |
| HBV | Sexually IDs | Balanced | Balanced | Balanced | No | No |
| CA (HPV) | Sexually IDs | Balanced | Balanced | Balanced | No | No |
| Genital herpes | Sexually IDs | Balanced | Balanced | Balanced | No | No |
| Chancroid | Sexually IDs | Recovered but not balanced | Recovered but not balanced | Recovered but not balanced | No | No |
| HCV | Sexually IDs | Balanced | Balanced | Balanced | No | No |

No disease changed classification when the analytical start date was moved from January 2020 to March 2020 or April 2020 (March changes: 0; April changes: 0), supporting the use of January 2020 as a pragmatic portfolio-level interruption anchor.

<!-- END TABLE_S10 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S11 -->
**Table S11. Alternative model-selection rules compared with the primary equal-weight composite rule.**

| Shortname | Group | Primary best model | Rank-aggregation best model | sMAPE-only best model | Horizon-weighted best model | Match under rank aggregation | Match under sMAPE-only | Match under horizon weighting |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| HCV | Sexually IDs | ARIMA + Fourier | ARIMA + Fourier | ARIMA + Fourier | ARIMA + Fourier | Yes | Yes | Yes |
| Chancroid | Sexually IDs | ARIMA + Fourier | ARIMA + Fourier | Hybrid | ARIMA + Fourier | Yes | No | Yes |
| Genital herpes | Sexually IDs | Hybrid | ETS | Hybrid | Hybrid | No | Yes | Yes |
| CA (HPV) | Sexually IDs | Bayesian structural | Bayesian structural | Bayesian structural | Bayesian structural | Yes | Yes | Yes |
| HBV | Sexually IDs | ETS | Bayesian structural | ETS | ETS | No | Yes | Yes |
| Syphilis | Sexually IDs | Bayesian structural | Bayesian structural | Bayesian structural | Bayesian structural | Yes | Yes | Yes |
| Gonorrhoea | Sexually IDs | TBATS | ETS | ETS | TBATS | No | No | Yes |
| HAV | Gastrointestinal IDs | Hybrid | Hybrid | Hybrid | Hybrid | Yes | Yes | Yes |
| Typhoid | Gastrointestinal IDs | Hybrid | ARIMA + Fourier | Neural Network | Hybrid | No | No | Yes |
| Shigellosis | Gastrointestinal IDs | Bayesian structural | Bayesian structural | Bayesian structural | Bayesian structural | Yes | Yes | Yes |
| Amebiasis | Gastrointestinal IDs | TBATS | TBATS | SARIMA | SARIMA | Yes | No | No |
| HFMD | Gastrointestinal IDs | Hybrid | Hybrid | Hybrid | Hybrid | Yes | Yes | Yes |
| S. suis | Vector-borne and zoonotic IDs | ETS | Hybrid | Hybrid | ETS | No | No | Yes |
| Melioidosis | Vector-borne and zoonotic IDs | Hybrid | Hybrid | Hybrid | Hybrid | Yes | Yes | Yes |
| Leptospirosis | Vector-borne and zoonotic IDs | ARIMA + Fourier | ARIMA + Fourier | ARIMA + Fourier | ARIMA + Fourier | Yes | Yes | Yes |
| Scrub Typhus | Vector-borne and zoonotic IDs | ETS | Bayesian structural | Bayesian structural | Bayesian structural | No | No | No |
| Malaria | Vector-borne and zoonotic IDs | Hybrid | Hybrid | Neural Network | Hybrid | Yes | No | Yes |
| Dengue fever | Vector-borne and zoonotic IDs | TBATS | TBATS | TBATS | TBATS | Yes | Yes | Yes |
| Rubella | Respiratory IDs | ETS | ETS | ETS | TBATS | Yes | Yes | No |
| Scarlet fever | Respiratory IDs | TBATS | TBATS | TBATS | TBATS | Yes | Yes | Yes |
| Mumps | Respiratory IDs | ETS | SARIMA | Hybrid | ETS | No | No | Yes |
| Chickenpox | Respiratory IDs | SARIMA | SARIMA | SARIMA | SARIMA | Yes | Yes | Yes |
| Influenza | Respiratory IDs | ETS | ETS | Bayesian structural | ETS | Yes | No | Yes |
| Pneumonia | Respiratory IDs | TBATS | ARIMA + Fourier | TBATS | TBATS | No | Yes | Yes |

Across the 24 diseases, the primary selected family was also recovered for 16 diseases under rank aggregation, 15 diseases under sMAPE-only selection, and 21 diseases under the horizon-weighted composite. Most disagreements were concentrated in a small subset of diseases rather than a single model family, suggesting that the principal conclusions were not driven by one aggregation formula.

<!-- END TABLE_S11 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S12 -->
**Table S12. Joint operational synthesis of recovery phenotype and seasonal displacement.**

| Shortname | Group | Primary deterministic phenotype | Shift vs pre (months) | Shift vs predicted (months) | Seasonal displacement | Operational priority |
| --- | --- | --- | --- | --- | --- | --- |
| Pneumonia | Respiratory IDs | Balanced | 1 | 1 | Stable | Low priority routine review |
| Influenza | Respiratory IDs | Recovered but not balanced | 1 | 0 | Stable | Cumulative review needed |
| Chickenpox | Respiratory IDs | Recovered but not balanced | -1 | -1 | Stable | Cumulative review needed |
| Mumps | Respiratory IDs | Recovered but not balanced | 6 | 4 | Shifted | Recalibrate and monitor |
| Scarlet fever | Respiratory IDs | Recovered but not balanced | -1 | -1 | Stable | Cumulative review needed |
| Rubella | Respiratory IDs | Suppressed | 3 | 2 | Shifted | High priority manual review |
| Dengue fever | Vector-borne and zoonotic IDs | Recovered but not balanced | 0 | 0 | Stable | Cumulative review needed |
| Malaria | Vector-borne and zoonotic IDs | No deficit | 0 | 0 | Stable | No deficit monitoring |
| Scrub Typhus | Vector-borne and zoonotic IDs | Balanced | 0 | 0 | Stable | Low priority routine review |
| Leptospirosis | Vector-borne and zoonotic IDs | Balanced | 1 | 1 | Stable | Low priority routine review |
| Melioidosis | Vector-borne and zoonotic IDs | Balanced | -2 | -2 | Shifted | Recovered but recalibrate seasonality |
| S. suis | Vector-borne and zoonotic IDs | Balanced | 0 | 0 | Stable | Low priority routine review |
| HFMD | Gastrointestinal IDs | Balanced | 1 | 0 | Stable | Low priority routine review |
| Amebiasis | Gastrointestinal IDs | Balanced | 0 | 0 | Stable | Low priority routine review |
| Shigellosis | Gastrointestinal IDs | Recovered but not balanced | 1 | 1 | Stable | Cumulative review needed |
| Typhoid | Gastrointestinal IDs | Balanced | 0 | -1 | Stable | Low priority routine review |
| HAV | Gastrointestinal IDs | Recovered but not balanced | -5 | -5 | Shifted | Recalibrate and monitor |
| Gonorrhoea | Sexually IDs | Balanced | 0 | 1 | Stable | Low priority routine review |
| Syphilis | Sexually IDs | Balanced | -2 | -3 | Shifted | Recovered but recalibrate seasonality |
| HBV | Sexually IDs | Balanced | 2 | 2 | Shifted | Recovered but recalibrate seasonality |
| CA (HPV) | Sexually IDs | Balanced | 4 | 4 | Shifted | Recovered but recalibrate seasonality |
| Genital herpes | Sexually IDs | Balanced | 2 | 3 | Shifted | Recovered but recalibrate seasonality |
| Chancroid | Sexually IDs | Recovered but not balanced | 0 | 0 | Stable | Cumulative review needed |
| HCV | Sexually IDs | Balanced | 4 | 4 | Shifted | Recovered but recalibrate seasonality |

This joint table clarifies the retrospective decision utility of the framework. A monthly-incidence-only interpretation would have marked 22 diseases as monthly-normalized, but the integrated RP/BP-seasonality synthesis separated them into 8 low-priority routine-review cases, 6 cumulative-review cases, 6 recovered-but-recalibrate-seasonality cases, and 2 recalibrate-and-monitor cases, while 1 disease remained in high-priority manual review and 1 disease remained in no-deficit monitoring.

<!-- END TABLE_S12 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S13 -->
**Table S13. Alternative endpoint sensitivity analyses for the 24 modelled diseases.** Month values are counted from January 2020, so month 0 corresponds to January 2020.

| Shortname | Group | Primary phenotype | Primary RP month | PI95 month | Ratio>=1 month | Half-deficit month | PI95 achieved | Ratio>=1 achieved | Half-deficit achieved |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Pneumonia | Respiratory IDs | Balanced | 34 | 26 | 34 | 54 | Yes | Yes | Yes |
| Influenza | Respiratory IDs | Recovered but not balanced | 44 | NA | 44 | 68 | No | Yes | Yes |
| Chickenpox | Respiratory IDs | Recovered but not balanced | 66 | 6 | 66 | NA | Yes | Yes | No |
| Mumps | Respiratory IDs | Recovered but not balanced | 68 | NA | 68 | NA | No | Yes | No |
| Scarlet fever | Respiratory IDs | Recovered but not balanced | 61 | 0 | 61 | 68 | Yes | Yes | Yes |
| Rubella | Respiratory IDs | Suppressed | NA | NA | NA | NA | No | No | No |
| Dengue fever | Vector-borne and zoonotic IDs | Recovered but not balanced | 35 | 1 | 35 | 47 | Yes | Yes | Yes |
| Malaria | Vector-borne and zoonotic IDs | No deficit | NA | NA | NA | NA | No | No | No |
| Scrub Typhus | Vector-borne and zoonotic IDs | Balanced | 28 | NA | 28 | 44 | No | Yes | Yes |
| Leptospirosis | Vector-borne and zoonotic IDs | Balanced | 11 | 5 | 11 | 30 | Yes | Yes | Yes |
| Melioidosis | Vector-borne and zoonotic IDs | Balanced | 28 | 4 | 28 | 32 | Yes | Yes | Yes |
| S. suis | Vector-borne and zoonotic IDs | Balanced | 5 | NA | 5 | 6 | No | Yes | Yes |
| HFMD | Gastrointestinal IDs | Balanced | 9 | 34 | 9 | 31 | Yes | Yes | Yes |
| Amebiasis | Gastrointestinal IDs | Balanced | 29 | 24 | 29 | 33 | Yes | Yes | Yes |
| Shigellosis | Gastrointestinal IDs | Recovered but not balanced | 31 | 0 | 31 | NA | Yes | Yes | No |
| Typhoid | Gastrointestinal IDs | Balanced | 30 | 0 | 30 | 48 | Yes | Yes | Yes |
| HAV | Gastrointestinal IDs | Recovered but not balanced | 47 | 0 | 47 | NA | Yes | Yes | No |
| Gonorrhoea | Sexually IDs | Balanced | 36 | 8 | 36 | 51 | Yes | Yes | Yes |
| Syphilis | Sexually IDs | Balanced | 34 | 19 | 34 | 37 | Yes | Yes | Yes |
| HBV | Sexually IDs | Balanced | 38 | 5 | 38 | 49 | Yes | Yes | Yes |
| CA (HPV) | Sexually IDs | Balanced | 40 | 2 | 40 | 51 | Yes | Yes | Yes |
| Genital herpes | Sexually IDs | Balanced | 36 | 31 | 36 | 48 | Yes | Yes | Yes |
| Chancroid | Sexually IDs | Recovered but not balanced | 48 | 7 | 48 | 66 | Yes | Yes | Yes |
| HCV | Sexually IDs | Balanced | 28 | 0 | 28 | 31 | Yes | Yes | Yes |

Among the 23 diseases that entered a sustained cumulative deficit, 18 re-entered the disease-specific 95% predictive interval for at least 3 months, 22 met the sustained observed-to-expected ratio endpoint of at least 1.0, and 18 halved their cumulative deficit by end follow-up. The ratio endpoint preserved the same achieved-versus-not-achieved distinction as the primary RP definition for all 23 deficit-entering diseases, whereas the half-deficit milestone was reached by 4 of the 8 recovered-but-not-balanced diseases.

<!-- END TABLE_S13 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S14 -->
**Table S14. External contextual triangulation period summary.**

| Period | Mean_Portfolio_Ratio | Median_Portfolio_Ratio | Mean_Stringency | Mean_SchoolClosing | Mean_InternalMovement | Mean_InternationalTravel | Mean_TestingPolicy | Mean_WHO_COVID_Cases |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2020-2021 restriction-intensive | 0.413 | 0.368 | 50.839 | 1.712 | 1.351 | 2.578 | 1.267 | 92082.083 |
| 2022 transition | 0.531 | 0.536 | 27.128 | 1.126 | 0.392 | 1.027 | 2.935 | 209319.833 |
| 2023-2024 WHO-context only | 1.187 | 1.093 | NA | NA | NA | NA | NA | 4024.222 |
| Late follow-up without external context | 1.876 | 1.832 | NA | NA | NA | NA | NA | NA |

The portfolio-level observed-to-expected ratio rose across the restriction-intensive, transition, and post-PHSM periods. Policy indicators were available through December, 2022, whereas WHO COVID-19 burden was available through June, 2024.

<!-- END TABLE_S14 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S15 -->
**Table S15. External contextual triangulation correlations and milestone dates.**

Panel A. Monthly correlation between the portfolio observed-to-expected ratio and external indicators.

| Indicator | SpearmanRho |
| --- | --- |
| StringencyIndex | -0.677 |
| GovernmentResponseIndex | -0.778 |
| SchoolClosing | -0.577 |
| InternalMovement | -0.741 |
| InternationalTravel | -0.638 |
| TestingPolicy | -0.143 |
| log1p(WHO_COVID_Cases) | -0.422 |

Panel B. Selected milestone months from the contextual triangulation.

| Milestone | month | PortfolioRatio | StringencyIndex | SchoolClosing | InternalMovement | InternationalTravel | WHO_COVID_Cases |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Peak stringency month | 2020-04 | 0.444 | 76.264 | 3.000 | 2.000 | 4.000 | 1534 |
| Peak WHO COVID-19 case month | 2021-08 | 0.220 | 73.223 | 2.452 | 2.000 | 2.968 | 676789 |
| First sustained low-restriction month | 2022-10 | 0.649 | 5.560 | 0.000 | 0.000 | 0.000 | 14096 |
| First sustained portfolio normalization month | 2023-07 | 1.060 | NA | NA | NA | NA | 3612 |

These contextual summaries were used descriptively to anchor the timing of portfolio suppression and normalization. They were not used as predictive covariates and do not support causal attribution.

<!-- END TABLE_S15 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S16 -->
**Table S16. Task-based heuristic assessment of the final dashboard build.**

| TaskID | PublicHealthTask | PrimaryModule | MinimumInteractions | Discoverability | Interpretability | Auditability | SupportStatus | ResidualFriction | MeanHeuristicScore |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| T1 | Identify which diseases are balanced, RP-only, suppressed, or no-deficit at portfolio level. | Overview | 1 | 4 | 4 | 3 | Fully supported | The overview gives counts but not the full disease list in the same card. | 3.67 |
| T2 | Inspect one disease's RP, BP, deficit depth, and trajectory against its counterfactual. | Recovery | 1 | 4 | 4 | 4 | Fully supported | None beyond disease selection. | 4.00 |
| T3 | Compare several diseases side by side and export the filtered time series for review meetings. | Time Series | 3 | 3 | 3 | 4 | Fully supported | Users must decide which subset to prefilter before export. | 3.33 |
| T4 | Determine whether apparent recovery is accompanied by persistent seasonal timing shift. | Seasonality | 1 | 3 | 4 | 4 | Fully supported | The user still has to combine timing and magnitude mentally. | 3.67 |
| T5 | Check definitions, study flow, and source files before interpreting outputs operationally. | Reference | 1 | 4 | 4 | 3 | Fully supported | Reference material is descriptive rather than interactive. | 3.67 |
| T6 | Translate recovery plus seasonality into an action-oriented prioritization category. | Prioritization | 1 | 4 | 4 | 4 | Fully supported | The action labels remain rule-based and should still be reviewed against local workflow. | 4.00 |

All six prespecified surveillance-review tasks were directly supported in the final build, with a mean heuristic score of 3.72 and median minimum interaction count of 1. This assessment documents functional interface coverage but should not be interpreted as a substitute for prospective end-user usability testing.

<!-- END TABLE_S16 -->
<div style="page-break-after: always;"></div>

![**Fig. S0. disease_flow.**](Supplementary%20Appendix%201_0/flow_diagram.png)

**Fig. S0. Two-stage disease-selection flow from 72 monitored series to the 43-disease descriptive portfolio and 24-disease counterfactual subset.** The first stage excluded 29 series because of overlapping aggregate categories (11), non-infectious scope (6), residual or ill-defined categories (5), zero incidence (3), incomplete recent reporting (3), or surveillance-definition shifts (1). The second stage excluded 19 descriptively retained diseases because of insufficient long-horizon signal (12), non-seasonal or weak seasonality (3), insufficient prepandemic duration (2), or residual or unspecified disease definitions (2). The right-hand criteria box summarizes the operational requirements for inclusion in counterfactual modelling.

<div style="page-break-after: always;"></div>

## Part 1: Validation of data reconstruction

![**Fig. S1. Pneumonia.**](Supplementary%20Appendix%201_1/cases/Pneumonia.png)

**Fig. S1. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Pneumonia, 2020-2025.**

![**Fig. S2. Influenza.**](Supplementary%20Appendix%201_1/cases/Influenza.png)

**Fig. S2. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Influenza, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S3. Chickenpox.**](Supplementary%20Appendix%201_1/cases/Chickenpox.png)

**Fig. S3. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Chickenpox, 2020-2025.**

![**Fig. S4. Mumps.**](Supplementary%20Appendix%201_1/cases/Mumps.png)

**Fig. S4. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Mumps, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S5. Measles.**](Supplementary%20Appendix%201_1/cases/Measles.png)

**Fig. S5. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Measles, 2020-2025.**

![**Fig. S6. Scarlet fever.**](Supplementary%20Appendix%201_1/cases/Scarlet%20fever.png)

**Fig. S6. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Scarlet fever, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S7. Rubella.**](Supplementary%20Appendix%201_1/cases/Rubella.png)

**Fig. S7. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Rubella, 2020-2025.**

![**Fig. S8. Pertussis.**](Supplementary%20Appendix%201_1/cases/Pertussis.png)

**Fig. S8. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Pertussis, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S9. Leprosy.**](Supplementary%20Appendix%201_1/cases/Leprosy.png)

**Fig. S9. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Leprosy, 2020-2025.**

![**Fig. S10. Diphtheria.**](Supplementary%20Appendix%201_1/cases/Diphtheria.png)

**Fig. S10. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Diphtheria, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S11. Dengue fever.**](Supplementary%20Appendix%201_1/cases/Dengue%20fever.png)

**Fig. S11. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Dengue fever, 2020-2025.**

![**Fig. S12. Malaria.**](Supplementary%20Appendix%201_1/cases/Malaria.png)

**Fig. S12. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Malaria, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S13. Scrub Typhus.**](Supplementary%20Appendix%201_1/cases/Scrub%20Typhus.png)

**Fig. S13. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Scrub Typhus, 2020-2025.**

![**Fig. S14. Chikungunya.**](Supplementary%20Appendix%201_1/cases/Chikungunya.png)

**Fig. S14. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Chikungunya, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S15. Leptospirosis.**](Supplementary%20Appendix%201_1/cases/Leptospirosis.png)

**Fig. S15. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Leptospirosis, 2020-2025.**

![**Fig. S16. Melioidosis.**](Supplementary%20Appendix%201_1/cases/Melioidosis.png)

**Fig. S16. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Melioidosis, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S17. S. suis.**](Supplementary%20Appendix%201_1/cases/S.%20suis.png)

**Fig. S17. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of S. suis, 2020-2025.**

![**Fig. S18. Zika virus.**](Supplementary%20Appendix%201_1/cases/Zika%20virus.png)

**Fig. S18. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Zika virus, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S19. Filariasis.**](Supplementary%20Appendix%201_1/cases/Filariasis.png)

**Fig. S19. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Filariasis, 2020-2025.**

![**Fig. S20. Trichinosis.**](Supplementary%20Appendix%201_1/cases/Trichinosis.png)

**Fig. S20. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Trichinosis, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S21. Brucellosis.**](Supplementary%20Appendix%201_1/cases/Brucellosis.png)

**Fig. S21. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Brucellosis, 2020-2025.**

![**Fig. S22. JE.**](Supplementary%20Appendix%201_1/cases/JE.png)

**Fig. S22. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of JE, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S23. Leishmaniasis.**](Supplementary%20Appendix%201_1/cases/Leishmaniasis.png)

**Fig. S23. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Leishmaniasis, 2020-2025.**

![**Fig. S24. HFMD.**](Supplementary%20Appendix%201_1/cases/HFMD.png)

**Fig. S24. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of HFMD, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S25. Amebiasis.**](Supplementary%20Appendix%201_1/cases/Amebiasis.png)

**Fig. S25. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Amebiasis, 2020-2025.**

![**Fig. S26. Shigellosis.**](Supplementary%20Appendix%201_1/cases/Shigellosis.png)

**Fig. S26. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Shigellosis, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S27. Typhoid.**](Supplementary%20Appendix%201_1/cases/Typhoid.png)

**Fig. S27. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Typhoid, 2020-2025.**

![**Fig. S28. Liver fluke.**](Supplementary%20Appendix%201_1/cases/Liver%20fluke.png)

**Fig. S28. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Liver fluke, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S29. HAV.**](Supplementary%20Appendix%201_1/cases/HAV.png)

**Fig. S29. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of HAV, 2020-2025.**

![**Fig. S30. Paratyphoid.**](Supplementary%20Appendix%201_1/cases/Paratyphoid.png)

**Fig. S30. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Paratyphoid, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S31. Cholera.**](Supplementary%20Appendix%201_1/cases/Cholera.png)

**Fig. S31. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Cholera, 2020-2025.**

![**Fig. S32. HEV.**](Supplementary%20Appendix%201_1/cases/HEV.png)

**Fig. S32. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of HEV, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S33. Enterovirus.**](Supplementary%20Appendix%201_1/cases/Enterovirus.png)

**Fig. S33. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Enterovirus, 2020-2025.**

![**Fig. S34. Gonorrhoea.**](Supplementary%20Appendix%201_1/cases/Gonorrhoea.png)

**Fig. S34. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Gonorrhoea, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S35. Syphilis.**](Supplementary%20Appendix%201_1/cases/Syphilis.png)

**Fig. S35. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Syphilis, 2020-2025.**

![**Fig. S36. HBV.**](Supplementary%20Appendix%201_1/cases/HBV.png)

**Fig. S36. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of HBV, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S37. CA (HPV).**](Supplementary%20Appendix%201_1/cases/CA%20%28HPV%29.png)

**Fig. S37. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of CA (HPV), 2020-2025.**

![**Fig. S38. Genital herpes.**](Supplementary%20Appendix%201_1/cases/Genital%20herpes.png)

**Fig. S38. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Genital herpes, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S39. Chancroid.**](Supplementary%20Appendix%201_1/cases/Chancroid.png)

**Fig. S39. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Chancroid, 2020-2025.**

![**Fig. S40. HCV.**](Supplementary%20Appendix%201_1/cases/HCV.png)

**Fig. S40. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of HCV, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S41. HDV.**](Supplementary%20Appendix%201_1/cases/HDV.png)

**Fig. S41. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of HDV, 2020-2025.**

![**Fig. S42. Other meningitis.**](Supplementary%20Appendix%201_1/cases/Other%20meningitis.png)

**Fig. S42. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Other meningitis, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S43. Encephalitis.**](Supplementary%20Appendix%201_1/cases/Encephalitis.png)

**Fig. S43. Comparison of observed and reconstructed (A) weekly and (B) monthly cases of Encephalitis, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S44. Pneumonia.**](Supplementary%20Appendix%201_1/deaths/Pneumonia.png)

**Fig. S44. Comparison of observed and reconstructed monthly deaths of Pneumonia, 2020-2025.**

![**Fig. S45. Influenza.**](Supplementary%20Appendix%201_1/deaths/Influenza.png)

**Fig. S45. Comparison of observed and reconstructed monthly deaths of Influenza, 2020-2025.**

![**Fig. S46. Chickenpox.**](Supplementary%20Appendix%201_1/deaths/Chickenpox.png)

**Fig. S46. Comparison of observed and reconstructed monthly deaths of Chickenpox, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S47. Mumps.**](Supplementary%20Appendix%201_1/deaths/Mumps.png)

**Fig. S47. Comparison of observed and reconstructed monthly deaths of Mumps, 2020-2025.**

![**Fig. S48. Measles.**](Supplementary%20Appendix%201_1/deaths/Measles.png)

**Fig. S48. Comparison of observed and reconstructed monthly deaths of Measles, 2020-2025.**

![**Fig. S49. Scarlet fever.**](Supplementary%20Appendix%201_1/deaths/Scarlet%20fever.png)

**Fig. S49. Comparison of observed and reconstructed monthly deaths of Scarlet fever, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S50. Rubella.**](Supplementary%20Appendix%201_1/deaths/Rubella.png)

**Fig. S50. Comparison of observed and reconstructed monthly deaths of Rubella, 2020-2025.**

![**Fig. S51. Pertussis.**](Supplementary%20Appendix%201_1/deaths/Pertussis.png)

**Fig. S51. Comparison of observed and reconstructed monthly deaths of Pertussis, 2020-2025.**

![**Fig. S52. Leprosy.**](Supplementary%20Appendix%201_1/deaths/Leprosy.png)

**Fig. S52. Comparison of observed and reconstructed monthly deaths of Leprosy, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S53. Diphtheria.**](Supplementary%20Appendix%201_1/deaths/Diphtheria.png)

**Fig. S53. Comparison of observed and reconstructed monthly deaths of Diphtheria, 2020-2025.**

![**Fig. S54. Dengue fever.**](Supplementary%20Appendix%201_1/deaths/Dengue%20fever.png)

**Fig. S54. Comparison of observed and reconstructed monthly deaths of Dengue fever, 2020-2025.**

![**Fig. S55. Malaria.**](Supplementary%20Appendix%201_1/deaths/Malaria.png)

**Fig. S55. Comparison of observed and reconstructed monthly deaths of Malaria, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S56. Scrub Typhus.**](Supplementary%20Appendix%201_1/deaths/Scrub%20Typhus.png)

**Fig. S56. Comparison of observed and reconstructed monthly deaths of Scrub Typhus, 2020-2025.**

![**Fig. S57. Chikungunya.**](Supplementary%20Appendix%201_1/deaths/Chikungunya.png)

**Fig. S57. Comparison of observed and reconstructed monthly deaths of Chikungunya, 2020-2025.**

![**Fig. S58. Leptospirosis.**](Supplementary%20Appendix%201_1/deaths/Leptospirosis.png)

**Fig. S58. Comparison of observed and reconstructed monthly deaths of Leptospirosis, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S59. Melioidosis.**](Supplementary%20Appendix%201_1/deaths/Melioidosis.png)

**Fig. S59. Comparison of observed and reconstructed monthly deaths of Melioidosis, 2020-2025.**

![**Fig. S60. S. suis.**](Supplementary%20Appendix%201_1/deaths/S.%20suis.png)

**Fig. S60. Comparison of observed and reconstructed monthly deaths of S. suis, 2020-2025.**

![**Fig. S61. Zika virus.**](Supplementary%20Appendix%201_1/deaths/Zika%20virus.png)

**Fig. S61. Comparison of observed and reconstructed monthly deaths of Zika virus, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S62. Filariasis.**](Supplementary%20Appendix%201_1/deaths/Filariasis.png)

**Fig. S62. Comparison of observed and reconstructed monthly deaths of Filariasis, 2020-2025.**

![**Fig. S63. Trichinosis.**](Supplementary%20Appendix%201_1/deaths/Trichinosis.png)

**Fig. S63. Comparison of observed and reconstructed monthly deaths of Trichinosis, 2020-2025.**

![**Fig. S64. Brucellosis.**](Supplementary%20Appendix%201_1/deaths/Brucellosis.png)

**Fig. S64. Comparison of observed and reconstructed monthly deaths of Brucellosis, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S65. JE.**](Supplementary%20Appendix%201_1/deaths/JE.png)

**Fig. S65. Comparison of observed and reconstructed monthly deaths of JE, 2020-2025.**

![**Fig. S66. Leishmaniasis.**](Supplementary%20Appendix%201_1/deaths/Leishmaniasis.png)

**Fig. S66. Comparison of observed and reconstructed monthly deaths of Leishmaniasis, 2020-2025.**

![**Fig. S67. HFMD.**](Supplementary%20Appendix%201_1/deaths/HFMD.png)

**Fig. S67. Comparison of observed and reconstructed monthly deaths of HFMD, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S68. Amebiasis.**](Supplementary%20Appendix%201_1/deaths/Amebiasis.png)

**Fig. S68. Comparison of observed and reconstructed monthly deaths of Amebiasis, 2020-2025.**

![**Fig. S69. Shigellosis.**](Supplementary%20Appendix%201_1/deaths/Shigellosis.png)

**Fig. S69. Comparison of observed and reconstructed monthly deaths of Shigellosis, 2020-2025.**


<div style="page-break-after: always;"></div>

![**Fig. S70. Typhoid.**](Supplementary%20Appendix%201_1/deaths/Typhoid.png)

**Fig. S70. Comparison of observed and reconstructed monthly deaths of Typhoid, 2020-2025.**

![**Fig. S71. Liver fluke.**](Supplementary%20Appendix%201_1/deaths/Liver%20fluke.png)

**Fig. S71. Comparison of observed and reconstructed monthly deaths of Liver fluke, 2020-2025.**

![**Fig. S72. HAV.**](Supplementary%20Appendix%201_1/deaths/HAV.png)

**Fig. S72. Comparison of observed and reconstructed monthly deaths of HAV, 2020-2025.**


<div style="page-break-after: always;"></div>

![**Fig. S73. Paratyphoid.**](Supplementary%20Appendix%201_1/deaths/Paratyphoid.png)

**Fig. S73. Comparison of observed and reconstructed monthly deaths of Paratyphoid, 2020-2025.**

![**Fig. S74. Cholera.**](Supplementary%20Appendix%201_1/deaths/Cholera.png)

**Fig. S74. Comparison of observed and reconstructed monthly deaths of Cholera, 2020-2025.**

![**Fig. S75. HEV.**](Supplementary%20Appendix%201_1/deaths/HEV.png)

**Fig. S75. Comparison of observed and reconstructed monthly deaths of HEV, 2020-2025.**


<div style="page-break-after: always;"></div>

![**Fig. S76. Enterovirus.**](Supplementary%20Appendix%201_1/deaths/Enterovirus.png)

**Fig. S76. Comparison of observed and reconstructed monthly deaths of Enterovirus, 2020-2025.**

![**Fig. S77. Gonorrhoea.**](Supplementary%20Appendix%201_1/deaths/Gonorrhoea.png)

**Fig. S77. Comparison of observed and reconstructed monthly deaths of Gonorrhoea, 2020-2025.**

![**Fig. S78. Syphilis.**](Supplementary%20Appendix%201_1/deaths/Syphilis.png)

**Fig. S78. Comparison of observed and reconstructed monthly deaths of Syphilis, 2020-2025.**


<div style="page-break-after: always;"></div>

![**Fig. S79. HBV.**](Supplementary%20Appendix%201_1/deaths/HBV.png)

**Fig. S79. Comparison of observed and reconstructed monthly deaths of HBV, 2020-2025.**

![**Fig. S80. CA (HPV).**](Supplementary%20Appendix%201_1/deaths/CA%20%28HPV%29.png)

**Fig. S80. Comparison of observed and reconstructed monthly deaths of CA (HPV), 2020-2025.**

![**Fig. S81. Genital herpes.**](Supplementary%20Appendix%201_1/deaths/Genital%20herpes.png)

**Fig. S81. Comparison of observed and reconstructed monthly deaths of Genital herpes, 2020-2025.**

<div style="page-break-after: always;"></div>

![**Fig. S82. Chancroid.**](Supplementary%20Appendix%201_1/deaths/Chancroid.png)

**Fig. S82. Comparison of observed and reconstructed monthly deaths of Chancroid, 2020-2025.**

![**Fig. S83. HCV.**](Supplementary%20Appendix%201_1/deaths/HCV.png)

**Fig. S83. Comparison of observed and reconstructed monthly deaths of HCV, 2020-2025.**

![**Fig. S84. HDV.**](Supplementary%20Appendix%201_1/deaths/HDV.png)

**Fig. S84. Comparison of observed and reconstructed monthly deaths of HDV, 2020-2025.**


<div style="page-break-after: always;"></div>

![**Fig. S85. Other meningitis.**](Supplementary%20Appendix%201_1/deaths/Other%20meningitis.png)

**Fig. S85. Comparison of observed and reconstructed monthly deaths of Other meningitis, 2020-2025.**

![**Fig. S86. Encephalitis.**](Supplementary%20Appendix%201_1/deaths/Encephalitis.png)

**Fig. S86. Comparison of observed and reconstructed monthly deaths of Encephalitis, 2020-2025.**

<div style="page-break-after: always;"></div>

## Part 2: Detailed forecasting diagnostics and model verification

![**Fig. S87. Pneumonia.**](Supplementary%20Appendix%201_5/Pneumonia.png)

**Fig. S87. Model selection and cross‑validation performance for Pneumonia: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S88. Influenza.**](Supplementary%20Appendix%201_5/Influenza.png)

**Fig. S88. Model selection and cross‑validation performance for Influenza: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

![**Fig. S89. Chickenpox.**](Supplementary%20Appendix%201_5/Chickenpox.png)

**Fig. S89. Model selection and cross‑validation performance for Chickenpox: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S90. Mumps.**](Supplementary%20Appendix%201_5/Mumps.png)

**Fig. S90. Model selection and cross‑validation performance for Mumps: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S91. Scarlet fever.**](Supplementary%20Appendix%201_5/Scarlet%20fever.png)

**Fig. S91. Model selection and cross‑validation performance for Scarlet fever: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>


![**Fig. S92. Rubella.**](Supplementary%20Appendix%201_5/Rubella.png)

**Fig. S92. Model selection and cross‑validation performance for Rubella: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S93. Dengue fever.**](Supplementary%20Appendix%201_5/Dengue%20fever.png)

**Fig. S93. Model selection and cross‑validation performance for Dengue fever: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S94. Malaria.**](Supplementary%20Appendix%201_5/Malaria.png)

**Fig. S94. Model selection and cross‑validation performance for Malaria: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S95. Scrub Typhus.**](Supplementary%20Appendix%201_5/Scrub%20Typhus.png)

**Fig. S95. Model selection and cross‑validation performance for Scrub Typhus: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S96. Leptospirosis.**](Supplementary%20Appendix%201_5/Leptospirosis.png)

**Fig. S96. Model selection and cross‑validation performance for Leptospirosis: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S97. Melioidosis.**](Supplementary%20Appendix%201_5/Melioidosis.png)

**Fig. S97. Model selection and cross‑validation performance for Melioidosis: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S98. S. suis.**](Supplementary%20Appendix%201_5/S.%20suis.png)

**Fig. S98. Model selection and cross‑validation performance for S. suis: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S99. HFMD.**](Supplementary%20Appendix%201_5/HFMD.png)

**Fig. S99. Model selection and cross‑validation performance for HFMD: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S100. Amebiasis.**](Supplementary%20Appendix%201_5/Amebiasis.png)

**Fig. S100. Model selection and cross‑validation performance for Amebiasis: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S101. Shigellosis.**](Supplementary%20Appendix%201_5/Shigellosis.png)

**Fig. S101. Model selection and cross‑validation performance for Shigellosis: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S102. Typhoid.**](Supplementary%20Appendix%201_5/Typhoid.png)

**Fig. S102. Model selection and cross‑validation performance for Typhoid: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S103. HAV.**](Supplementary%20Appendix%201_5/HAV.png)

**Fig. S103. Model selection and cross‑validation performance for HAV: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S104. Gonorrhoea.**](Supplementary%20Appendix%201_5/Gonorrhoea.png)

**Fig. S104. Model selection and cross‑validation performance for Gonorrhoea: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S105. Syphilis.**](Supplementary%20Appendix%201_5/Syphilis.png)

**Fig. S105. Model selection and cross‑validation performance for Syphilis: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S106. HBV.**](Supplementary%20Appendix%201_5/HBV.png)

**Fig. S106. Model selection and cross‑validation performance for HBV: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S107. CA (HPV).**](Supplementary%20Appendix%201_5/CA%20%28HPV%29.png)

**Fig. S107. Model selection and cross‑validation performance for CA (HPV): multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S108. Genital herpes.**](Supplementary%20Appendix%201_5/Genital%20herpes.png)

**Fig. S108. Model selection and cross‑validation performance for Genital herpes: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S109. Chancroid.**](Supplementary%20Appendix%201_5/Chancroid.png)

**Fig. S109. Model selection and cross‑validation performance for Chancroid: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

![**Fig. S110. HCV.**](Supplementary%20Appendix%201_5/HCV.png)

**Fig. S110. Model selection and cross‑validation performance for HCV: multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables.

<div style="page-break-after: always;"></div>

## Part 3: Disease ranking and age distribution validation

![**Fig. S111. ranking_all.**](Supplementary%20Appendix%201_3/ranking_all.png)

**Fig. S111. Biennial rankings of infectious diseases (A) cases and (B) deaths by reported cases within each time window (2008–2009 to 2024–2025).** Nodes are coloured by disease category, and connecting arrows indicate changes in rank between consecutive windows (increase, decrease, or unchanged).

<div style="page-break-after: always;"></div>

![**Fig. S112. ranking_age.**](./Supplementary%20Appendix%201_3/age_patterns_main.png)

**Fig. S112. Shifts in age-specific disease patterns in Thailand, 2008–2025.** (A) Biennial rankings of the top 10 diseases by reported cases within each time window (2008–2009 to 2024–2025). Nodes are coloured by disease category, and connecting arrows indicate changes in rank between consecutive windows (increase, decrease, or unchanged); diseases outside the top 10 are grouped as “Others”. (B) Cumulative reported cases by age group for selected leading diseases across 2008–2025. (C) Dominant disease by cases for each age group and biennial window. (D) Biennial rankings of the top 10 diseases by reported deaths, displayed as in panel A. (E) Cumulative reported deaths by age group for selected leading diseases across 2008–2025 (inset shows expanded scale for younger age groups). (F) Dominant disease by deaths for each age group and biennial window. Disease categories are indicated by colour: respiratory, vector-borne and zoonotic, gastrointestinal, sexually transmitted, and other infectious diseases.

<div style="page-break-after: always;"></div>

![**Fig. S113. validation_age.**](Supplementary%20Appendix%201_3/validation_age.png)

**Fig. S113. Validation of age‑reconstruction: agreement between estimated and observed age‑stratified counts (2020–2023).**
This figure evaluates the performance of the age‑reconstruction procedure by comparing harmonized estimates (derived from historical grouped age reports) with independently observed, fine‑scale age‑stratified data over the 2020–2023 overlap period. Each panel presents a scatterplot of aggregated counts per disease–year–age group (one panel for cases, one for deaths): points are colored by seven target age bands, and a dashed 1:1 line indicates perfect agreement. The subtitle reports the Pearson correlation (r) summarizing linear concordance across all points. Close clustering of points around the identity line and high r values indicate that the reconstruction preserves the relative age distribution and magnitude of burden across diseases and years; systematic deviations (consistent over- or under‑estimation for particular age bands) reveal where disaggregation may introduce bias and warrant further inspection. This validation supports combining harmonized historical and recent age‑stratified data for downstream age‑specific analyses.

<div style="page-break-after: always;"></div>

## Part 4: Temporal trends of infectious diseases by category

![**Fig. S114. Cases Gastrointestinal IDs.**](Supplementary%20Appendix%201_2/Cases%20Gastrointestinal%20IDs.png)

**Fig. S114. Temporal trends of gastrointestinal infectious diseases by category.** (A) Monthly observed case counts together with a smoothed long‑term trend derived from decomposition of the monthly time series. (B) Heatmap of standardized incidence values (each disease scaled relative to its own historical distribution). (C-L) The combination of trajectory plots and standardized heatmaps facilitates comparison of long‑term trend behavior and temporal clustering of anomalies across disease groups, aiding interpretation of post‑pandemic re‑equilibration patterns.

<div style="page-break-after: always;"></div>

![**Fig. S115. Cases Other IDs.**](Supplementary%20Appendix%201_2/Cases%20Other%20IDs.png)

**Fig. S115. Temporal trends of other infectious diseases by category.** (A) Monthly observed case counts together with a smoothed long‑term trend derived from decomposition of the monthly time series. (B) Heatmap of standardized incidence values (each disease scaled relative to its own historical distribution). (C-D) The combination of trajectory plots and standardized heatmaps facilitates comparison of long‑term trend behavior and temporal clustering of anomalies across disease groups, aiding interpretation of post‑pandemic re‑equilibration patterns.

<div style="page-break-after: always;"></div>

![**Fig. S116. Cases Respiratory IDs.**](Supplementary%20Appendix%201_2/Cases%20Respiratory%20IDs.png)

**Fig. S116. Temporal trends of respiratory infectious diseases by category.** (A) Monthly observed case counts together with a smoothed long‑term trend derived from decomposition of the monthly time series. (B) Heatmap of standardized incidence values (each disease scaled relative to its own historical distribution). (C-L) The combination of trajectory plots and standardized heatmaps facilitates comparison of long‑term trend behavior and temporal clustering of anomalies across disease groups, aiding interpretation of post‑pandemic re‑equilibration patterns.

<div style="page-break-after: always;"></div>

![**Fig. S117. Cases Sexually IDs.**](Supplementary%20Appendix%201_2/Cases%20Sexually%20IDs.png)

**Fig. S117. Temporal trends of sexually transmitted infectious diseases by category.** (A) Monthly observed case counts together with a smoothed long‑term trend derived from decomposition of the monthly time series. (B) Heatmap of standardized incidence values (each disease scaled relative to its own historical distribution). (C-J) The combination of trajectory plots and standardized heatmaps facilitates comparison of long‑term trend behavior and temporal clustering of anomalies across disease groups, aiding interpretation of post‑pandemic re‑equilibration patterns.

<div style="page-break-after: always;"></div>

![**Fig. S118. Cases Vector-borne and zoonotic IDs.**](Supplementary%20Appendix%201_2/Cases%20Vector-borne%20and%20zoonotic%20IDs.png)

**Fig. S118. Temporal trends of vector-borne and zoonotic infectious diseases by category.** (A) Monthly observed case counts together with a smoothed long‑term trend derived from decomposition of the monthly time series. (B) Heatmap of standardized incidence values (each disease scaled relative to its own historical distribution). (C-O) The combination of trajectory plots and standardized heatmaps facilitates comparison of long‑term trend behavior and temporal clustering of anomalies across disease groups, aiding interpretation of post‑pandemic re‑equilibration patterns.

<div style="page-break-after: always;"></div>

![**Fig. S119. Deaths Gastrointestinal IDs.**](Supplementary%20Appendix%201_2/Deaths%20Gastrointestinal%20IDs.png)

**Fig. S119. Temporal trends of deaths from gastrointestinal infectious diseases by category.** (A) Monthly observed death counts together with a smoothed long‑term trend derived from decomposition of the monthly time series. (B) Heatmap of standardized mortality values (each disease scaled relative to its own historical distribution). (C-I) The combination of trajectory plots and standardized heatmaps facilitates comparison of long‑term trend behavior and temporal clustering of anomalies across disease groups, aiding interpretation of post‑pandemic re‑equilibration patterns.

<div style="page-break-after: always;"></div>

![**Fig. S120. Deaths Other IDs.**](Supplementary%20Appendix%201_2/Deaths%20Other%20IDs.png)

**Fig. S120. Temporal trends of deaths from other infectious diseases by category.** (A) Monthly observed death counts together with a smoothed long‑term trend derived from decomposition of the monthly time series. (B) Heatmap of standardized mortality values (each disease scaled relative to its own historical distribution). (C-D) The combination of trajectory plots and standardized heatmaps facilitates comparison of long‑term trend behavior and temporal clustering of anomalies across disease groups, aiding interpretation of post‑pandemic re‑equilibration patterns.

<div style="page-break-after: always;"></div>

![**Fig. S121. Deaths Respiratory IDs.**](Supplementary%20Appendix%201_2/Deaths%20Respiratory%20IDs.png)

**Fig. S121. Temporal trends of deaths from respiratory infectious diseases by category.** (A) Monthly observed death counts together with a smoothed long‑term trend derived from decomposition of the monthly time series. (B) Heatmap of standardized mortality values (each disease scaled relative to its own historical distribution). (C-I) The combination of trajectory plots and standardized heatmaps facilitates comparison of long‑term trend behavior and temporal clustering of anomalies across disease groups, aiding interpretation of post‑pandemic re‑equilibration patterns.

<div style="page-break-after: always;"></div>

![**Fig. S122. Deaths Sexually IDs.**](Supplementary%20Appendix%201_2/Deaths%20Sexually%20IDs.png)

**Fig. S122. Temporal trends of deaths from sexually transmitted infectious diseases by category.** (A) Monthly observed death counts together with a smoothed long‑term trend derived from decomposition of the monthly time series. (B) Heatmap of standardized mortality values (each disease scaled relative to its own historical distribution). (C-G) The combination of trajectory plots and standardized heatmaps facilitates comparison of long‑term trend behavior and temporal clustering of anomalies across disease groups, aiding interpretation of post‑pandemic re‑equilibration patterns.

<div style="page-break-after: always;"></div>

![**Fig. S123. Deaths Vector-borne and zoonotic IDs.**](Supplementary%20Appendix%201_2/Deaths%20Vector-borne%20and%20zoonotic%20IDs.png)

**Fig. S123. Temporal trends of deaths from vector-borne and zoonotic infectious diseases by category.** (A) Monthly observed death counts together with a smoothed long‑term trend derived from decomposition of the monthly time series. (B) Heatmap of standardized mortality values (each disease scaled relative to its own historical distribution). (C-M) The combination of trajectory plots and standardized heatmaps facilitates comparison of long‑term trend behavior and temporal clustering of anomalies across disease groups, aiding interpretation of post‑pandemic re‑equilibration patterns.

<div style="page-break-after: always;"></div>

## Part 5: Spatial distribution of incidence and mortality

![**Fig. S124. incidence.**](Supplementary%20Appendix%201_4/incidence.png)

**Fig. S124. Leading disease by province-year for incidence.** Panels summarize the infectious disease with the highest annual incidence in each Thai province for each calendar year included in the provincial dataset. Colors denote the dominant disease category assignment used in the spatial workflow, allowing visual comparison of how the leading incidence burden shifted across provinces and over time.

<div style="page-break-after: always;"></div>

![**Fig. S125. mortality.**](Supplementary%20Appendix%201_4/mortality.png)

**Fig. S125. Leading disease by province-year for mortality.** Panels summarize the infectious disease with the highest annual mortality in each Thai province for each calendar year included in the provincial dataset. Provinces with no recorded deaths in a given year are retained as a separate map class, so the figure distinguishes true zero-mortality settings from changes in the dominant fatal disease elsewhere.

<div style="page-break-after: always;"></div>

<!-- BEGIN EXTERNAL_PERTUSSIS_CASE_STUDY -->
## Part 6: External pertussis decision-support case study

**Table S17. Country-level counterfactual median and 95% predictive-interval summary for the external pertussis case study.**
| Country | Cadence | Best model | Follow-up median forecast | Follow-up median 95% PI | End date | End forecast median | End 95% PI | Normalization date | Balance date |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Australia | Monthly surveillance | ARIMA + Fourier |  807.8 | 56.1 to 2506.1 | 2025-12-01 | 1084.2 | 4.6 to 3881.1 | 2020-01-01 | 2024-11-01 |
| China | Monthly surveillance | ARIMA + Fourier | 1916.4 | 191.2 to 5413.1 | 2025-12-01 | 1346.3 | 0 to 6502.6 | 2021-12-01 | 2024-01-01 |
| Japan | Weekly surveillance | ARIMA + Fourier |  217.2 | 23.2 to 551.3 | 2025-12-22 |  165.4 | 0 to 636.8 | 2025-01-20 | 2025-08-04 |
| New Zealand | Monthly surveillance | ARIMA + Fourier |   35.5 | 0 to 700.3 | 2025-12-01 |   56.9 | 0 to 1259.7 | 2024-05-01 | 2024-12-01 |
| Sweden | Monthly surveillance | ARIMA + Fourier |   58.3 | 29.6 to 109.5 | 2025-12-01 |   74.7 | 39.9 to 131.5 | 2024-03-01 | Not reached |
| United States | Weekly surveillance | TBATS |  109.2 | 25.3 to 257.3 | 2025-12-28 |   87.9 | 2 to 269 | 2020-01-05 | Not reached |

<div style="page-break-after: always;"></div>

![**Fig. S126. External pertussis decision-support case study.**](Supplementary%20Appendix%201_6/external_pertussis_decision_support.png)

**Fig. S126. External pertussis decision-support case study across six countries.** Panel A compares candidate model performance using the same rolling hold-out composite-selection logic used in the main manuscript. Panels B-G show observed pertussis incidence and the selected counterfactual median forecast for Australia, China, Japan, New Zealand, Sweden, and the United States. Shaded blue and gold bars mark the recovery-review and balance-review windows implied by the RP/BP logic, while green/red fills mark months or weeks in which observed incidence is above or below the counterfactual median.
<!-- END EXTERNAL_PERTUSSIS_CASE_STUDY -->
