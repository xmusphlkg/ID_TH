# A digital surveillance framework reveals decoupled and heterogeneous recovery of infectious diseases in Thailand after COVID-19 disruption

Kangguo Li^1^, Yulun Xie^1^, Yunzhi Zenghuang^1^, Tao Chen^1^, Yanhua Su^1^, Zeyu Zhao^2,^&^, Qiuping Chen^1,^&^, Jia Rui^3,^&^, Tianmu Chen^1,^&^

^1^State Key Laboratory of Vaccines for Infectious Diseases, Xiang An Biomedicine Laboratory, School of Public Health, Xiamen University, Xiamen, China.  
^2^WorldPop, School of Geography and Environmental Science, University of Southampton, Southampton, UK.  
^3^Department of Epidemiology and Health Statistics, Xiangya School of Public Health, Central South University, Changsha, China.  

**Correspondence:** Tianmu Chen (chentianmu@xmu.edu.cn), Jia Rui (ruijia5345@163.com), Qiuping Chen (chenqp241@xmu.edu.cn), Zeyu Zhao (zz1e25@soton.ac.uk)

---

## Abstract

Routine infectious disease dashboards usually track current incidence but may fail to show whether cumulative burden has truly recovered after major system shocks. We developed and validated a digital surveillance framework to assess post-disruption recovery using Thailand’s national notifiable disease surveillance data for 43 infectious diseases (2008–2025). Disease-specific machine learning and statistical counterfactual models were selected through rolling hold-out validation. We defined two complementary recovery metrics: recovery period (RP), the return of monthly incidence to ≥95% of expected levels for three consecutive months, and balance period (BP), the time required for cumulative observed-minus-expected deviation to return to zero. Among 24 diseases eligible for recovery profiling, 13 achieved both RP and BP, 7 achieved RP without BP, 3 remained suppressed, and malaria showed no sustained cumulative deficit. Recovery in monthly incidence often preceded by restoration of cumulative burden. This framework offers a scalable, interpretable tool for post-pandemic surveillance prioritization.

---

## Introduction

National infectious disease surveillance systems are increasingly digital, producing high-volume longitudinal data that can support timely public health decision-making1. In practice, however, most operational dashboards rely on single-dimensional indicators such as current incidence, case counts, or short-term trends2,3. While useful for situational awareness, these metrics can be misleading after large-scale disruptions: a return to expected monthly incidence does not necessarily imply that cumulative population burden has been restored4. This limitation is particularly critical in low- and middle-income countries, where constrained surveillance capacity and resources require simple but decision-relevant analytic tools5–9.

The COVID-19 pandemic exposed this limitation. Public health and social measures (PHSMs), mobility restrictions, school closures, healthcare disruption, diagnostic reprioritization, and changes in care-seeking behavior altered both true transmission and observed reporting across a wide range of infectious diseases. As a result, infectious diseases exhibited heterogeneous dynamics, with sharp declines during periods of intensive control followed by uneven rebounds and, in some cases, persistent shifts in seasonal timing10,11. However, most post-pandemic analyses have remained disease-specific, descriptive, or focused on high-income settings4,10,12,13. Consequently, it remains unclear whether normalization in reported incidence reflects true epidemiological recovery or masks unresolved cumulative deficits across a broader disease portfolio.

A key unresolved challenge is how to define and measure recovery as a multidimensional process. Recent studies have increasingly adopted counterfactual baselines to estimate expected incidence in the absence of disruption4,10,13. However, emerging evidence suggests that recovery is not a single event but comprises at least two distinct processes: the realignment of current incidence with expected trajectories, and the closure of cumulative deficits accumulated during disruption14,15. These processes may not occur synchronously, such that apparent normalization can coexist with substantial residual burden. In addition, post-pandemic observations of altered seasonality indicate that recovery may involve structural changes in transmission dynamics rather than simple reversion to pre-pandemic patterns17. Capturing these dimensions is essential for surveillance interpretation and public health prioritization18.

To address this gap, we developed a digital counterfactual surveillance framework that integrates statistical and machine-learning models within an interpretable pipeline. The framework quantifies two complementary recovery indicators: a recovery period (RP), representing the return of monthly incidence to expected levels, and a balance period (BP), representing the restoration of cumulative observed-minus-expected burden. We applied this framework to Thailand’s national notifiable disease surveillance system, analyzing monthly nationwide data for 43 infectious diseases from 2008 to 20255,6. We aimed to determine whether a dual-metric surveillance approach could distinguish apparent from cumulative recovery after COVID-19 disruption, identify pathogen-specific recovery phenotypes across a national disease portfolio, and detect persistent seasonal displacement relevant to post-pandemic preparedness and resource allocation.

---

## Results

### Ensemble model selection and framework validation

The automated model selection pipeline evaluated six candidate families—neural network autoregression (NNAR), exponential smoothing (ETS), seasonal ARIMA (SARIMA), trigonometric seasonality with Box-Cox transformation (TBATS), a hybrid statistical-ML composite, and Bayesian structural time series (BSTS)—across three rolling pre-pandemic hold-out schemes for each of the 24 diseases eligible for counterfactual modelling (Fig. 1). No single model family dominated: NNAR was selected for 7 diseases, ETS for 5, SARIMA for 4, TBATS for 3, hybrid for 3, and BSTS for 2 (Fig. 2A). This heterogeneity validates the adaptive, disease-specific design of the framework over one-size-fits-all approaches. Composite z-standardized performance scores (sMAPE, RMSE, MASE) across hold-out splits showed that the selected best model improved median sMAPE by 12–38% relative to the worst-performing family for the same disease (Supplementary Fig. S87–S110).

To validate the data processing pipeline, we compared reconstructed monthly case counts with official monthly totals in overlap years (2020–2023). Across 1968 disease-month observations, reconstructed and official totals showed Pearson r=0·999, a median absolute error of 3 cases, and a median absolute percentage error (MAPE) of 3·9%. Corresponding MAPEs for high-burden sentinel diseases—pneumonia, influenza, dengue fever, and HFMD—were 3·27%, 5·44%, 4·12%, and 5·91%, respectively (Supplementary Tables S4–S5, Supplementary Fig. S1–S86). Counterfactual forecast uncertainty was quantified from 1000 Monte Carlo simulated trajectories per disease; monthly medians and 95% predictive intervals are reported throughout.

### Dual-metric recovery classification

The framework's RP/BP classification algorithm assigned each of the 24 modelled diseases to one of four recovery phenotypes based on post-January 2020 counterfactual trajectories (Fig. 3A). Thirteen diseases achieved both RP and BP within follow-up, seven achieved RP but remained cumulatively unbalanced by December 2025, three remained suppressed without recovery (chickenpox, rubella, and syphilis), and malaria showed no sustained cumulative deficit (Fig. 3).

Median RP was 32·5 months, whereas median BP was 38 months, and the two metrics were only weakly correlated, demonstrating that recovery in monthly incidence and restoration of cumulative burden represent distinct surveillance dimensions. HFMD illustrated this decoupling: monthly incidence returned to expected levels relatively early (RP 9 months), yet the cumulative observed-minus-expected deviation did not return to zero until BP 32 months (Fig. 3N). Similar lagged or incomplete cumulative correction was detected for dengue fever (Fig. 3H), influenza (Fig. 3C), mumps (Fig. 3E), scarlet fever (Fig. 3F), HAV (Fig. 3R), and chancroid (Fig. 3X), all of which met RP criteria but remained cumulatively below expectation at end of follow-up. By contrast, vector-borne and zoonotic infections—including scrub typhus and leptospirosis (Fig. 3J,K)—reached cumulative zero-crossing earlier. Recovery durations varied widely: S. suis infection reached both RP and BP within the first year (Fig. 3M), whereas pneumonia and gonorrhea required more than 5 years (Fig. 3B,S).

RP/BP classifications were robust to threshold variation: assignments were unchanged when the recovery threshold was varied from 95% to 90% with 3- or 4-month persistence requirements (Supplementary Table S5). Under uniform model-family sensitivity analyses, 19 of 24 classifications were preserved under ETS, and 16 of 24 under SARIMA, relative to the primary best-model specification (Supplementary Table S8).

### Seasonal displacement detection

The framework's seasonal analysis module detected post-PHSM phase shifts and amplitude changes that would be invisible to conventional incidence dashboards (Fig. 4). Observed-to-expected ratios were below 1 for most diseases during 2020–2022 and increased from 2023 onwards across all transmission categories (Fig. 4A–D).

Using the circular center-of-mass metric, the largest consistent phase shifts were detected for mumps (+6 months relative to pre-pandemic observed timing and +5 months relative to the post-PHSM counterfactual profile, Fig. 4H), HAV (−5 and −5 months, Fig. 4U), HCV (+4 and +4 months, Fig. 4AB), and condyloma acuminatum (CA, HPV, +4 and +4 months, Fig. 4Y). Intermediate shifts were detected for rubella (+3 and +2 months, Fig. 4J), HBV (+2 and +2 months, Fig. 4X), and genital herpes (+2 and +2 months, Fig. 4Z), whereas dengue fever, malaria, scrub typhus, S. suis infection, and typhoid showed 0-month phase shifts (Fig. 4E–AB).

Seasonal amplitude varied independently of phase shift, with post-PHSM observed peak-to-trough amplitude ratios of 0·50 (mumps), 0·72 (HAV), 0·70 (HCV), and 0·38 (CA, HPV), compared with pre-pandemic values of 0·77, 2·11, 0·94, and 0·57, respectively. By contrast, vector-borne infections showed maintained or increased amplitude: dengue fever 1·65 vs 1·50, malaria 1·98 vs 1·35, scrub typhus 1·05 vs 1·03. These findings demonstrate that the framework can identify diseases requiring recalibration of seasonality-based early warning algorithms.

### Interactive surveillance dashboard

To translate the framework from a research pipeline into an operational surveillance tool, we developed a modular interactive dashboard (available at https://lkg1116.shinyapps.io/TH_ID/) built on R Shiny with five functional modules (Fig. 5). The *Overview* module provides disease portfolio filtering, national burden ranking, and transmission category summaries. The *Recovery* module displays precomputed RP/BP classifications with observed-versus-counterfactual trajectory visualization and deficit depth metrics per disease. The *Time Series* module enables interactive exploration of monthly observed incidence against counterfactual forecasts with prediction intervals. The *Seasonality* module presents pre-pandemic versus post-PHSM seasonal signatures with peak timing comparisons. A *Reference* module documents operational definitions and data provenance.

The dashboard loads cached analytical outputs (`.RData` files) to minimize computational latency, enabling near-instantaneous disease-level exploration without requiring server-side recomputation. This architecture prioritizes accessibility for LMIC surveillance teams with limited computing infrastructure. The complete dashboard code is open-source and deployable on standard R Shiny hosting platforms.

---

## Discussion

Our framework demonstrates that post-pandemic recovery of infectious diseases is a multidimensional process that conventional surveillance dashboards fail to capture. By coupling automated ensemble forecasting with a dual-metric classification algorithm and an interactive dashboard, the framework distinguishes transient monthly normalization from genuine cumulative burden restoration—a distinction directly relevant to digital surveillance design and public health decision-making.

The adaptive, disease-specific model selection represents a key methodological advance for digital surveillance infrastructure. No single model family dominated across the 24 diseases, confirming that heterogeneous transmission dynamics require flexible algorithmic pipelines rather than fixed statistical specifications. The hybrid integration of classical statistical models (ETS, SARIMA) with machine learning approaches (NNAR, TBATS, BSTS) through rigorous rolling hold-out validation balances interpretability, data efficiency, and predictive flexibility—priorities increasingly recognized in digital medicine. Our composite scoring protocol with z-standardized metrics provides a transparent, reproducible procedure for automated model selection that could be embedded in routine surveillance platforms without requiring manual expert input for each pathogen.

The decoupling between monthly recovery (RP) and cumulative balance (BP) has direct implications for digital surveillance system design. Seven of 24 diseases showed apparent monthly normalization while remaining cumulatively unbalanced, meaning that dashboards displaying only current incidence would incorrectly signal full recovery. This finding argues for next-generation surveillance platforms to incorporate cumulative deviation tracking alongside traditional incidence metrics. The RP/BP framework provides a decision-support triage tool: diseases achieving RP without BP warrant investigation of testing intensity, case ascertainment quality, and residual cumulative shortfall, enabling resource prioritization in settings where comprehensive disease-by-disease review is operationally infeasible.

Seasonal displacement detection addresses an underappreciated vulnerability in digital early warning systems. Phase shifts exceeding 5 months for mumps and HAV indicate that anomaly detection algorithms trained on pre-pandemic seasonality risk both false alarms and missed outbreaks if seasonal baselines are not updated. Our framework's circular center-of-mass approach provides an automated, quantitative metric for detecting such shifts and flagging diseases requiring baseline recalibration—a capability absent from most operational surveillance dashboards.

The interactive dashboard translates the analytical pipeline into an accessible surveillance tool deployable in resource-limited settings. By caching precomputed analytical outputs and using modular open-source architecture, the dashboard minimizes computational requirements while providing on-demand disease-level exploration of recovery status, counterfactual trajectories, and seasonal patterns. This design reflects a deliberate trade-off between analytical depth and operational accessibility, following digital health principles that prioritize interpretability and low-resource deployability for LMIC surveillance systems. The open-source codebase and standardized data pipeline enable adaptation to other national surveillance systems once foundational data infrastructure exists.

The scalability of this framework extends beyond Thailand. The analytical pipeline requires only longitudinal monthly case counts with consistent case definitions—a data maturity level achievable by many national surveillance systems. Critical prerequisites include: (1) at least 10 years of pre-disruption data for stable counterfactual estimation, (2) consistent pathogen-level reporting without major case-definition changes during the training period, and (3) timely reporting to support actionable surveillance outputs. Future multi-country implementations could leverage the disease-specific model selection to accommodate surveillance systems with different data characteristics, reporting frequencies, and pathogen portfolios.

Several limitations should be considered alongside the digital-specific constraints of the framework. Routine surveillance data inherently reflect pandemic-era changes in healthcare-seeking, testing practices, and reporting completeness that varied across pathogens. Counterfactual modelling reduces but cannot eliminate this ascertainment uncertainty. The RP/BP classification uses point forecasts rather than full predictive distributions, meaning classification uncertainty is not propagated into the decision rule; future implementations should incorporate probabilistic thresholds, particularly for borderline diseases or high-stakes policy decisions. The common January 2020 interruption point and ≥2023 post-PHSM window improve cross-pathogen comparability but represent temporal simplifications. Monthly reconstruction from weekly data preserved totals but may introduce minor allocation errors. Exploratory Cox regression analyses of recovery speed determinants, presented in the Supplementary Appendix, were underpowered (n=24 diseases) and should be interpreted as hypothesis-generating.

Future development should integrate additional digital data streams—mobility data, climate variables, genomic surveillance, and syndromic signals—to enhance mechanistic understanding of pathogen-specific recovery drivers and extend forecast horizons. Probabilistic RP/BP classification incorporating full predictive distributions would improve decision confidence for borderline cases. Real-time deployment with automated data ingestion and continuous model updating would transform the framework from retrospective analysis to prospective surveillance intelligence. Multi-country validation expanding the disease portfolio across diverse surveillance systems could also enable advanced statistical approaches (Bayesian model averaging, hierarchical models) that our single-country sample size currently precludes.

---

## Methods

### Study design and data sources

We conducted a retrospective time-series study of nationally notifiable infectious diseases in Thailand using publicly available surveillance data from the Bureau of Epidemiology, Ministry of Public Health, Thailand (2008–2025). The study was reported in accordance with STROBE guidance.[10] Among 72 notifiable diseases, we retained 43 for descriptive analyses after excluding conditions with zero incidence, overlapping categories, case-definition changes, non-communicable conditions, or incomplete reporting (Supplementary Tables S1–S3). For counterfactual modelling, 24 diseases with continuous pre-pandemic monthly series and sufficient signal for stable long-horizon forecasting were retained. All data and code are publicly accessible at https://github.com/xmusphlkg/ID_TH.

### Digital data processing pipeline

Annual population denominators were obtained from UN World Population Prospects mid-year estimates.[11] Monthly incidence rates were expressed per 100 000 population. For 2020–2025, we reconstructed monthly counts from weekly data using constrained spline-based temporal disaggregation that preserved observed weekly totals, because the official monthly series was not consistently updated beyond 2024 (Supplementary Method). Pipeline validation against official monthly totals in overlap years (2020–2023) yielded Pearson r=0·999, median absolute error of 3 cases, and MAPE of 3·9% across 1968 disease-month observations (Supplementary Tables S4–S5, Supplementary Fig. S1–S86).

### Ensemble forecasting framework

For each of the 24 modelled diseases, we fitted the pre-pandemic period (January 2008 to December 2019) and forecast monthly counterfactual trajectories for January 2020 to December 2025, treating January 2020 as the common portfolio-level interruption point. Counts were log-transformed after addition of a small constant (Supplementary Appendix). This common interruption date coincided with the onset of COVID-19-related surveillance disruption in Thailand; we did not interpret it as a disease-specific biological switch point.

Six candidate model families were evaluated on log-transformed monthly series (with a Laplace smoothing constant of 0·01 to handle zero counts):

- **NNAR**: Feed-forward neural network autoregression via `nnetar()` with automatic lag and hidden-node selection.
- **ETS**: Error-trend-seasonal exponential smoothing via `ets()` with AICc-based model selection.
- **SARIMA**: Seasonal autoregressive integrated moving average via `auto.arima()` with seasonal period 12 and AICc selection.
- **TBATS**: Trigonometric seasonality with Box-Cox transformation and ARMA errors, seasonal period fixed at 12 months.
- **Hybrid**: Four-model ensemble (ARIMA + ETS + NNAR + TBATS) via `hybridModel()` with cross-validation error-based RMSE weighting and a rolling window of max(24, 70% of training length) months.
- **BSTS**: Bayesian structural time series with local linear trend and seasonal components (period 12), fitted via 1000 Markov chain Monte Carlo iterations with 10% burn-in.

For each disease, models were assessed under three rolling pre-pandemic hold-out schemes: training through December 2018 with test on 2019 (1-year horizon), training through December 2017 with test on 2018–2019 (2-year horizon), and training through December 2016 with test on 2017–2019 (3-year horizon). Per-disease, per-split forecast performance was quantified using three complementary accuracy metrics:

$$\text{sMAPE} = \text{mean}\left(200 \times \frac{|O_t - E_t|}{|O_t| + |E_t|}\right)$$

$$\text{RMSE} = \sqrt{\frac{1}{n}\sum(O_t - E_t)^2}$$

$$\text{MASE} = \frac{\text{mean}(|O_t - E_t|)}{\text{mean}(|\Delta O_t|)}$$

For each disease and hold-out split, all three metrics were z-standardized across the six candidate models (sign-reversed so that higher values indicate better performance) and summed with equal weight to form a split-specific composite index. These split-level composites were then summed across the three hold-out schemes, and the model family with the highest aggregate index was selected as the disease-specific best model.[15–17] Disease-specific performance tables are provided in Supplementary Fig. S87–S110.

Forecast uncertainty was quantified from 1000 simulated trajectories per disease. For NNAR, ETS, SARIMA, and TBATS, we used bootstrap residual simulation (`simulate(model, future=TRUE, bootstrap=TRUE)`); for the hybrid model, we resampled residuals around the mean forecast; and for BSTS, we drew from the posterior predictive distribution. All simulated paths were back-transformed from the log scale, and monthly medians, 80%, and 95% predictive intervals were computed from the empirical quantiles of the 1000-path ensemble (Supplementary Method). As robustness analyses, we re-estimated all outcomes under uniform ETS and uniform SARIMA families across all 24 diseases and compared disease-level reclassification against the primary best-model specification (Supplementary Table S7).

### Recovery metric algorithms

We compared observed monthly case counts (*O_t*) after January 2020 with disease-specific counterfactual medians (*E_t*). Monthly deviation was defined as *D_t = O_t − E_t* and cumulative deviation as *C_t = Σ D_τ*. Disruption onset was the first month with *C_t < 0*; the trough was the month with min(*C_t*).

**Recovery period (RP)** was defined algorithmically as the first month *t* satisfying: (1) *O_t ≥ 0.95 × E_t* for three consecutive months, and (2) *C_t* was non-decreasing over the same window. **Balance period (BP)** was defined as the first month after the trough at which *C_t ≥ 0*. BP is interpreted as a surveillance summary of cumulative observed-minus-expected mismatch, not as a measure of biological compensation. The 95% threshold and 3-month persistence requirement were prespecified as conservative operational criteria; sensitivity was evaluated by varying the threshold to 90% and persistence to 2 or 4 months (Supplementary Table S6). Additional metrics included relative suppression at trough, rebound intensity, suppression duration, and payback duration.

The RP/BP classification relies on point forecasts rather than full predictive distributions. This prioritizes interpretability for surveillance dashboards but means that forecast uncertainty is not propagated into the decision rule. Future implementations could incorporate probabilistic thresholds or report classification probabilities given predictive intervals.

### Seasonal analysis module

Seasonal shape, amplitude, and timing were compared across three scenarios: pre-pandemic observed (≤2019), post-PHSM observed (≥2023), and post-PHSM counterfactual (predicted ≥2023). We use *post-PHSM* for the ≥2023 period after relaxation of major national control measures and border restrictions. Seasonal profiles were constructed from mean monthly counts. The weighted circular center-of-mass phase shift was prespecified as the primary timing metric, with the empirical peak month as sensitivity check.[18,19] For each disease, post-PHSM observed minus pre-pandemic observed and post-PHSM observed minus post-PHSM counterfactual timing shifts were mapped to minimal signed displacements on a 12-month cycle.

### Interactive dashboard implementation

The surveillance dashboard was implemented in R Shiny with a modular architecture comprising five functional modules: overview, recovery, time series, seasonality, and reference. Each module uses separate UI and server components to enable independent development and testing. The dashboard loads precomputed analytical outputs (cached `.RData` files) rather than performing real-time model fitting, ensuring sub-second response times suitable for resource-limited settings. The complete dashboard source code is available at https://github.com/xmusphlkg/ID_TH and is deployed at https://lkg1116.shinyapps.io/TH_ID/.

### Supplementary analyses

Trend decomposition used seasonal and Loess decomposition with segmented log-linear breakpoint regression.[12–14] Suppression-rebound associations were examined using Spearman correlation and generalized additive models; disruption-recovery typologies were identified using k-means clustering on standardized suppression magnitude and rebound intensity (Supplementary Appendix). Exploratory Cox proportional-hazards models assessed recovery speed determinants using transmission category, incubation period, infectious period, and immunity characteristics as predictors; these are reported in the Supplementary Appendix given the small number of events (n=24).[21,22] All analyses were performed in R version 4·5·2.

### Ethics approval

This study used publicly available, de-identified, aggregated surveillance data from Thailand's Bureau of Epidemiology. As the research involved secondary analysis of anonymized routine surveillance data without individual participant identifiers, ethical approval was not required according to institutional policies. No patient consent was required.

---

## Data Availability

All data used in this study are publicly available from the Bureau of Epidemiology, Ministry of Public Health, Thailand. The processed analytical datasets, reconstructed monthly data, and complete metadata are deposited in the GitHub repository at https://github.com/xmusphlkg/ID_TH under an open data license. Original aggregated surveillance data can also be accessed directly from the Thai Ministry of Public Health website or upon reasonable request to the corresponding authors.

## Code Availability

All statistical analyses were performed using R version 4·5·2. The complete source code implementing the six-model ensemble (NNAR via nnetar package, ETS via forecast package, SARIMA via forecast package, TBATS via tbats package, hybrid models, and BSTS via bsts package), RP/BP classification algorithms, data preprocessing scripts, and visualization code are publicly available at https://github.com/xmusphlkg/ID_TH. The repository includes comprehensive documentation, example workflows, and instructions for reproducing all main figures and supplementary analyses.

## Author Contributions

K.L.: conceptualization, data curation, visualization, methodology, writing – original draft. Y.X.: methodology validation, formal analysis, investigation, writing – original draft. Y.Z.: methodology validation, formal analysis, investigation. T.C.: methodology validation, formal analysis, investigation. Y.S.: supervision, project administration. Z.Z.: supervision, project administration. Q.C.: supervision, project administration. J.R.: supervision, project administration, writing – review & editing. T.C.: supervision, project administration, writing – review & editing. All authors had full access to the aggregated study data, accepted responsibility for the decision to submit for publication, and approved the final version of the manuscript.

## Acknowledgments

We thank the Bureau of Epidemiology, Ministry of Public Health, Thailand, for maintaining and providing access to long-term national infectious disease surveillance data, without which this study would not have been possible. This work was supported by the Self-supporting Program of Guangzhou Laboratory (GZNL2024A01004), the National Natural Science Foundation of China (825B2104), and the National Key Research and Development Program of China (2024YFC2311404).

## Competing Interests

All authors declare no competing interests.

---

## References

1. Brett TS, Rohani P. Collateral effects of COVID-19 pandemic control on the US infectious disease landscape. Science. 2025;390: 510–515. doi:10.1126/science.adw4964
2. Olsen SJ, Azzizz-Baumgartner E, Budd AP, Brammer L, Sullivan S, Pineda RF, et al. Decreased influenza activity during the COVID-19 pandemic-United States, Australia, Chile, and South Africa, 2020. Am J Transplant. 2020;20: 3681–3685. doi:10.1111/ajt.16382
3. Huang QS, Wood T, Jelley L, Jennings T, Jefferies S, Daniells K, et al. Impact of the COVID-19 nonpharmaceutical interventions on influenza and other respiratory viral infections in New Zealand. Nat Commun. 2021;12: 1001. doi:10.1038/s41467-021-21157-9
4. Li K, Rui J, Song W, Luo L, Zhao Y, Qu H, et al. Temporal shifts in 24 notifiable infectious diseases in China before and during the COVID-19 pandemic. Nat Commun. 2024;15: 3891. doi:10.1038/s41467-024-48201-8
5. Chongsuvivatwong V, Phua KH, Yap MT, Pocock NS, Hashim JH, Chhem R, et al. Health and health-care systems in southeast Asia: diversity and transitions. The Lancet. 2011;377: 429–437. doi:10.1016/S0140-6736(10)61507-3
6. Sittimart M, Rachatan C, Muenkaew P, Dabak SV. Past, present, and future: a situational analysis of infectious disease modelling in Thailand. The Lancet Regional Health - Southeast Asia. 2025;39. doi:10.1016/j.lansea.2025.100618
7. Baker RE, Park SW, Yang W, Vecchi GA, Metcalf CJE, Grenfell BT. The impact of COVID-19 nonpharmaceutical interventions on the future dynamics of endemic infections. PNAS. 2020;117: 30547–30553. doi:10.1073/pnas.2013182117
8. Chen Y, Li N, Lourenço J, Wang L, Cazelles B, Dong L, et al. Measuring the effects of COVID-19-related disruption on dengue transmission in southeast Asia and Latin America: a statistical modelling study. The Lancet Infectious Diseases. 2022;22: 657–667. doi:10.1016/S1473-3099(22)00025-1
9. Gyapong JO, Gohoho M, Manyeh AK, Immurana M, Gyapong M. Current state and future directions of interventions for neglected tropical diseases. Nature Human Behaviour. 2025;9: 1557–1570. doi:10.1038/s41562-025-02219-0
10. Cuschieri S. The STROBE guidelines. Saudi J Anaesth. 2019;13: S31–S34. doi:10.4103/sja.SJA_543_18
11. United Nations, Department of Economic and Social Affairs, Population Division. World Population Prospects 2024, Online Edition. 2024 [cited 27 Jan 2026]. Available: https://population.un.org/wpp/
12. Cleveland RB, Cleveland WS. STL: A seasonal-trend decomposition procedure based on Loess. Journal of official statistics. 1990;6. 
13. Kim HJ, Fay MP, Feuer EJ, Midthune DN. Permutation tests for joinpoint regression with applications to cancer rates. Stat Med. 2000;19: 335–351. doi:10.1002/(sici)1097-0258(20000215)19:3<335::aid-sim336>3.0.co;2-z
14. Mugdeo VMR. Estimating regression models with unknown break-points. Stat Med. 2003;22: 3055–3071. doi:10.1002/sim.1545

---

## Figure Legends

**Figure 1. Framework architecture of the digital counterfactual surveillance system.** The pipeline comprises six layers: (1) data ingestion from Thailand's Bureau of Epidemiology weekly surveillance reports; (2) automated digital data processing with weekly-to-monthly temporal disaggregation and overlap-year validation; (3) an ensemble forecasting framework evaluating six candidate model families (NNAR, ETS, SARIMA, TBATS, hybrid, BSTS) through rolling hold-out cross-validation with z-standardized composite scoring; (4) counterfactual trajectory generation via 1000 Monte Carlo simulations per disease; (5) two parallel analytical modules—the RP/BP recovery classification algorithm and the seasonal displacement detection module; and (6) an interactive Shiny dashboard for operational surveillance deployment. The validation protocol (right) summarizes data pipeline concordance and model sensitivity analyses.

**Figure 2. Disease-specific model selection performance.** (A) Heatmap of composite z-standardized performance indices (sum of normalized sMAPE, RMSE, and MASE across three hold-out splits) for each model family (columns) and disease (rows). Warmer colours indicate higher (better) composite scores. Asterisks mark the selected best model for each disease. Diseases are grouped by transmission category (colour bar, left). No single model family dominated across the 24 diseases, validating the adaptive ensemble design.

**Figure 3. Dual-metric recovery classification of 24 infectious diseases.** (A) Summary of recovery phenotype assignments: both RP and BP achieved (green), RP without BP (yellow), persistently suppressed (red), and no sustained cumulative deficit (grey). (B–Y) Disease-specific panels showing monthly observed incidence (red line) versus counterfactual median forecast (teal line) with 95% predictive intervals (teal ribbon), January 2008 to December 2025. Blue shading indicates the recovery period (disruption onset to RP); gold shading indicates the balance period (RP to BP). The area between observed and expected curves is shaded to highlight cumulative surplus (green) or deficit (red).

**Figure 4. Seasonal displacement detection.** (A–D) Heatmaps of monthly observed-to-expected incidence rate ratios for respiratory, vector-borne and zoonotic, gastrointestinal, and sexually transmitted infections. Values below 1 (blue) indicate suppression; values above 1 (red) indicate exceedance. (E–AB) Radar charts comparing seasonal profiles across three scenarios: pre-pandemic observed (brown), post-PHSM observed (red), and post-PHSM counterfactual (teal). Circular centre-of-mass peak months are indicated by triangles; empirical peak months by circles. Grey shading denotes the rainy season (May–October). Phase shifts (months) relative to pre-pandemic timing and counterfactual timing are annotated for each disease.

**Figure 5. Interactive surveillance dashboard.** Screenshots of the four principal modules of the R Shiny dashboard deployed at https://lkg1116.shinyapps.io/TH_ID/. (A) Overview module with disease portfolio filtering, burden ranking, and transmission category summaries. (B) Recovery module displaying precomputed RP/BP classifications with observed-versus-counterfactual trajectory visualization and deficit metrics. (C) Time Series module enabling interactive exploration of monthly observed incidence against counterfactual forecasts with prediction intervals. (D) Seasonality module presenting pre-pandemic versus post-PHSM seasonal signatures with peak timing comparisons. The dashboard loads cached analytical outputs to ensure sub-second response times suitable for resource-limited surveillance settings.
