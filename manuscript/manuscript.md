# Counterfactual digital decision support for infectious disease recovery in Thailand

Kangguo Li^1^, Yulun Xie^1^, Yunzhi Zenghuang^1^, Tao Chen^1^, Yanhua Su^1^, Zeyu Zhao^2,^&^, Qiuping Chen^1,^&^, Jia Rui^3,^&^, Tianmu Chen^1,^&^

^1^State Key Laboratory of Vaccines for Infectious Diseases, Xiang An Biomedicine Laboratory, School of Public Health, Xiamen University, Xiamen, China.  
^2^WorldPop, School of Geography and Environmental Science, University of Southampton, Southampton, UK.  
^3^Department of Epidemiology and Health Statistics, Xiangya School of Public Health, Central South University, Changsha, China.  

**Correspondence:** Tianmu Chen (chentianmu@xmu.edu.cn), Jia Rui (ruijia5345@163.com), Qiuping Chen (chenqp241@xmu.edu.cn), Zeyu Zhao (zz1e25@soton.ac.uk)

---

## Abstract

Routine infectious disease dashboards display current incidence but not whether reported burden has rejoined its expected trajectory after disruption. We developed and retrospectively evaluated a counterfactual digital decision-support framework for Thailand's national surveillance system, analysing 43 diseases and modelling 24 with stable pre-pandemic series. The framework combines disease-specific forecasting, dual recovery milestones, uncertainty propagation, seasonal displacement detection, and an interface for portfolio review. Under incidence-only review, 20 diseases appeared normalized; the framework retained only 8 in routine review and reassigned 12 to cumulative review or recalibration queues. RP/BP classifications were moderately stable under threshold, interruption-timing, and simulation-time sensitivity checks (Monte Carlo path counts and BSTS iteration settings), and uncertainty analysis identified eight uncertainty-sensitive diseases. This framework supports auditable post-disruption triage and seasonal surveillance recalibration using routine digital public health data.

---

## Introduction

National infectious disease surveillance systems are increasingly digital data infrastructures, but many operational dashboards remain display layers rather than decision layers.<sup>1,2</sup> They report current incidence or short-term deviation yet often do not translate those signals into auditable review priorities after major system shocks. From a digital medicine perspective, the unmet need is not only to visualize surveillance data, but to convert them into reproducible triage logic that helps public-health teams decide which diseases require routine review, cumulative follow-up, or seasonal recalibration.<sup>3-5</sup>

The COVID-19 pandemic exposed this gap. Non-pharmaceutical interventions, mobility restrictions, school closures, altered healthcare seeking, diagnostic reprioritization, and interruptions to routine services changed both true transmission and reported surveillance incidence across many infections.<sup>6-10</sup> Yet most post-pandemic studies have remained pathogen-specific, descriptive, or focused on a narrow group of respiratory infections.<sup>3,6,7</sup> They therefore provide limited support for portfolio-level digital review in which surveillance teams must compare many diseases, rank follow-up needs, and document why a disease is escalated despite apparently normalized monthly counts.

A second challenge is conceptual. Recovery is often discussed as a single endpoint and sometimes framed mechanistically through immunity debt.<sup>11</sup> However, routine surveillance data are better suited to deriving surveillance-defined operational states than to proving true infection burden recovery, biological compensation, or causal mechanisms of rebound. For digital review workflows, at least two distinct milestones are relevant: realignment of monthly reported incidence with expected levels (reported-incidence normalization), and closure of the cumulative observed-minus-expected deficit generated during disruption (cumulative deficit closure). These processes need not occur simultaneously, and either may coexist with persistent seasonal displacement that requires recalibration of alert timing.

We therefore framed this study as the development and retrospective evaluation of a digital public-health decision-support workflow for routine national surveillance data. Using Thailand's notifiable disease surveillance system, we analysed 43 infectious diseases from 2008 to 2025 and modelled 24 diseases with sufficiently stable pre-pandemic monthly series. We asked whether an uncertainty-aware dual-metric framework could transform retrospective surveillance data into an auditable disease-review queue by distinguishing monthly normalization from cumulative reconciliation, quantifying classification uncertainty, identifying diseases needing cumulative follow-up or seasonal recalibration, and supporting reproducible portfolio prioritization.

---

## Results

### Transparent disease inclusion, model specification, and pre-pandemic validation

We screened 72 monitored notifiable disease series, retained 43 for descriptive analysis, and carried 24 forward to counterfactual modelling through a two-stage disease-selection flow summarized in Supplementary Table S1, Supplementary Fig. S0, and Supplementary Tables S2-S3. Eligible modelled diseases required a continuous 144-month pre-pandemic monthly series and sufficient stability to support 72-month counterfactual extrapolation without domination by structural zeros, weak non-seasonal signal, or residual surveillance definitions. The model-selection pipeline then evaluated six candidate forecasting families across three rolling pre-pandemic hold-out schemes for each of these 24 diseases (Fig. 1). No single family dominated: neural network autoregression was selected for 7 diseases, exponential smoothing for 5, seasonal autoregressive integrated moving average for 4, TBATS for 3, a weighted hybrid ensemble for 3, and Bayesian structural time series for 2 (Fig. 2A). Median sMAPE improved by 12-38 percentage points relative to the worst-performing family for the same disease, supporting disease-specific rather than one-size-fits-all counterfactual specification within the pre-pandemic validation window (Supplementary Figures S87-S110).

The model-selection rule was robust to alternative aggregation strategies. The primary selected family was also recovered for 18 of 24 diseases by rank aggregation, 19 of 24 by sMAPE-only aggregation, and 21 of 24 by a horizon-weighted composite that placed greater weight on the shortest extrapolation window (Supplementary Table S11). Fixed-family sensitivity analyses were more stringent but preserved the qualitative recovery pattern: 19 of 24 disease-level RP/BP classifications were retained under a uniform exponential-smoothing specification and 16 of 24 under a uniform seasonal autoregressive integrated moving average specification (Supplementary Table S8). We did not perform nested or external validation because the objective was to estimate a no-disruption statistical baseline from pre-pandemic structure rather than to optimize prediction of observed post-disruption incidence; accordingly, these cross-validation results should be interpreted as baseline-specification checks rather than prospective forecasting guarantees.

To validate the upstream data-processing pipeline, we compared reconstructed monthly case counts with official monthly totals in overlap years from 2020 to 2023. Across 1,968 disease-month observations, reconstructed and official totals showed Pearson r = 0.999, a median absolute error of 3 cases, and a median absolute percentage error of 3.9%. Corresponding MAPEs for high-burden sentinel diseases were 3.27% for pneumonia, 5.44% for influenza, 4.12% for dengue fever, and 5.91% for hand, foot, and mouth disease (Supplementary Tables S6 and S7; Supplementary Figures S1-S86). Forecast uncertainty was summarized from 1,000 simulated trajectories per disease and propagated into disease-level recovery probabilities; additional simulation-time sensitivity checks (varying path counts and BSTS iterations) are summarized in Supplementary Table S17.

### Surveillance-defined recovery was frequently decoupled across monthly and cumulative dimensions

Using the deterministic counterfactual median as the primary summary, the RP/BP algorithm assigned the 24 modelled diseases to four surveillance-defined phenotypes (Fig. 3). Thirteen diseases achieved both RP and BP within follow-up, seven achieved RP without BP, three remained suppressed without RP, and malaria showed no sustained cumulative deficit. RP and BP were operational milestones rather than biological constants, but they captured meaningfully different states of reported-incidence recovery. Monthly normalization often preceded cumulative reconciliation: hand, foot, and mouth disease reached RP 9 months after January 2020 but did not achieve BP until month 32, and several other diseases, including influenza, mumps, dengue fever, HAV, shigellosis, scarlet fever, and chancroid, also met RP criteria while remaining cumulatively unresolved at the end of follow-up.

Uncertainty-aware classification showed that these deterministic labels were best interpreted as point summaries with heterogeneous stability (Supplementary Table S9). HAV and chancroid had Pr(RP) = 1.000 but Pr(BP) of 0 and 0.016, respectively, indicating near-certain monthly normalization without cumulative reconciliation. In contrast, HFMD, typhoid, leptospirosis, melioidosis, HBV, and genital herpes all had Pr(BP) = 1.000, supporting stable cumulative closure. More ambiguous diseases included influenza (Pr(RP) = 0.656; Pr(BP) = 0.388), dengue fever (0.723; 0.340), shigellosis (0.716; 0.419), and pneumonia (1.000; 0.785). RP and BP timing uncertainty was similarly heterogeneous: influenza had an RP interval of 25-68 months and a BP interval of 38-70 months, dengue fever had corresponding intervals of 2-46 and 3-63 months, whereas leptospirosis and HBV had much tighter BP intervals of 33-35 and 52-53 months. Using a pragmatic instability flag based on deterministic phenotype stability below 0.5, eight diseases were uncertainty-sensitive in the review layer: influenza, chickenpox, scarlet fever, dengue fever, malaria, scrub typhus, shigellosis, and syphilis. Thus, uncertainty did not erase the distinction between RP and BP; rather, it quantified how strongly the data supported each disease-specific phenotype.

RP/BP assignments were also stable under predefined operational and simulation settings. Classifications were unchanged when the monthly recovery threshold was lowered from 95% to 90% while maintaining 3- or 4-month persistence requirements; only the most permissive 2-month rule reclassified chickenpox from suppressed to RP only (Supplementary Table S5). Changing the analytical interruption point from January 2020 to March 2020 did not alter any disease-level classification, and shifting it to April 2020 changed only two diseases: malaria moved from no sustained cumulative deficit to balanced, and shigellosis moved from RP only to balanced (Supplementary Table S10). Varying simulation-time settings (Monte Carlo paths and BSTS iterations) produced limited additional reclassification and concentrated instability in the same uncertainty-sensitive subset (Supplementary Table S17).

Alternative endpoint analyses supported the same operational interpretation but showed that not all plausible endpoints were equally discriminating (Supplementary Table S13). Among the 23 diseases that entered a sustained cumulative deficit, the stricter sustained observed-to-expected ratio endpoint of at least 1.0 yielded the same achieved-versus-not-achieved distinction as the primary RP rule for all 23 diseases, although recovery timing shifted for some diseases. By contrast, a definition based on first sustained return within the 95% predictive interval was achieved by all 23 diseases, including chickenpox and rubella, indicating that predictive-interval return alone is too permissive to separate persistent suppression from operational normalization. A half-deficit endpoint lay between RP and BP: 18 of the 23 diseases halved their cumulative deficit by end follow-up, including 5 of the 7 RP-without-BP diseases, whereas mumps and HAV did not.

### External contextual triangulation supported the timing of portfolio disruption

External contextual indicators were used only for interpretive triangulation rather than prediction. During the restriction-intensive period of 2020-2021, the mean observed-to-expected portfolio ratio across the 24 modelled diseases was 0.399, increasing to 0.493 in the 2022 transition period and 1.000 in the January 2023 to June 2024 WHO-context period (Supplementary Table S14). Across 2020-2022, the monthly portfolio ratio was negatively correlated with national stringency (Spearman rho = -0.590), government response intensity (-0.703), school closing (-0.628), internal movement restrictions (-0.628), international travel controls (-0.493), and log WHO COVID-19 case counts (-0.534) (Supplementary Table S15). Peak stringency occurred in April 2020, when the portfolio ratio was 0.386, and the WHO COVID-19 case peak occurred in August 2021, when the portfolio ratio was 0.262. The first sustained low-restriction month was October 2022, whereas the first sustained portfolio-level normalization month was July 2023 (Supplementary Table S15). These contextual signals were treated as surveillance-pressure proxies (healthcare utilization constraints, diagnostic reprioritization, and reporting stress) to improve interpretation of RP/BP timing under possible ascertainment bias.

### Decision utility and operational prioritization

A monthly-incidence-only surveillance review would have treated 20 of the 24 modelled diseases as operationally normalized by the end of follow-up: 13 balanced diseases plus 7 diseases that achieved RP without BP. In retrospective workflow emulation, the digital framework retained only 8 of these 20 diseases in the low-priority routine-review queue and reassigned the remaining 12 (60%) to action-triggering queues with explicit reasons (Supplementary Table S12). This reassignment rate represents the proportion of diseases for which a conventional incidence-only dashboard would have produced a different review decision from the framework.

The seven diseases that appeared recovered on monthly incidence but remained cumulatively unresolved were influenza, mumps, scarlet fever, dengue fever, shigellosis, HAV, and chancroid. Within this group, scarlet fever, dengue fever, shigellosis, and chancroid entered the cumulative-review queue because seasonal timing remained broadly stable, whereas influenza, mumps, and HAV were assigned to the recalibrate-and-monitor queue because unresolved cumulative deficit coincided with phase shifts of at least 2 months. Conversely, five balanced diseases, namely melioidosis, HBV, condyloma acuminatum, genital herpes, and HCV, were removed from the routine-recovered group and placed in a balanced-but-recalibrate-seasonality queue, showing that BP achievement alone did not guarantee readiness for unchanged seasonal surveillance thresholds.

At the opposite end of the decision spectrum, chickenpox, rubella, and syphilis remained in the high-priority manual-review queue because they had not achieved RP, whereas malaria was more appropriately interpreted as a no-deficit monitoring case than as a delayed-recovery case. The disease-level digital review queue used for routine surveillance review is summarized in Table 1.

To quantify the operational divergence between conventional and framework-based review, we computed three summary measures across the 24-disease portfolio. First, the decision discordance rate was 50%, meaning that half of all diseases received different review assignments under the two approaches. Second, among the 12 reassigned diseases, 4 would have been deprioritized under incidence-only review despite carrying unresolved cumulative deficits (influenza, mumps, HAV, and chancroid), representing potential false-negative triage errors. Third, the framework-generated review rationales were fully traceable to quantitative rules, whereas incidence-only review would have required ad hoc justification for each escalation or deprioritization decision.

### Seasonal displacement identified recalibration needs beyond cumulative recovery

The seasonal analysis module detected post-PHSM phase shifts and amplitude changes that would not be apparent from incidence trajectories alone (Fig. 4). Observed-to-expected ratios were below 1 for most diseases during 2020-2022 and increased from 2023 onward across all transmission categories (Fig. 4A-D).

Using the weighted circular center-of-mass metric, the largest consistent phase shifts were detected for mumps (+6 months relative to pre-pandemic observed timing and +5 months relative to the post-PHSM counterfactual profile, Fig. 4H), HAV (-5 and -5 months, Fig. 4U), HCV (+4 and +4 months, Fig. 4AB), and condyloma acuminatum (+4 and +4 months, Fig. 4Y). Intermediate shifts were observed for rubella (+3 and +2 months, Fig. 4J), HBV (+2 and +2 months, Fig. 4X), and genital herpes (+2 and +2 months, Fig. 4Z), whereas dengue fever, malaria, scrub typhus, *S. suis* infection, and typhoid showed no measurable shift on this metric.

Seasonal amplitude varied independently of phase shift. Post-PHSM observed peak-to-trough amplitude ratios were 0.50 for mumps, 0.72 for HAV, 0.70 for HCV, and 0.38 for condyloma acuminatum, compared with pre-pandemic values of 0.77, 2.11, 0.94, and 0.57, respectively. By contrast, vector-borne infections showed maintained or increased amplitude, including dengue fever (1.65 vs 1.50), malaria (1.98 vs 1.35), and scrub typhus (1.05 vs 1.03). These findings show why seasonal displacement should sit in the same decision layer as RP/BP classification: cumulative reconciliation alone does not ensure that surveillance thresholds or preparedness calendars remain aligned.

Joint interpretation of recovery phenotype and seasonal displacement revealed three operationally distinct patterns that would be invisible under either metric alone (Supplementary Table S12). First, diseases that appeared recovered under both RP and BP but remained seasonally misaligned, including melioidosis, HBV, condyloma acuminatum, genital herpes, and HCV, require recalibration of seasonal alert thresholds despite having resolved their cumulative deficit. Second, diseases that achieved monthly normalization but remained both cumulatively unresolved and seasonally displaced, including influenza, mumps, and HAV, represent the highest-priority recalibration targets because they carry both unresolved burden and shifted timing. Third, diseases that remained suppressed without RP and also showed amplitude compression, including chickenpox and rubella, suggest that neither conventional nor recalibrated seasonal thresholds would be adequate until monthly recovery is achieved.

## Discussion

The key contribution of this study is a digital public-health decision layer, not simply a retrospective description of national surveillance trends. We developed a framework that converts routine surveillance streams, counterfactual expectations, uncertainty summaries, and seasonal diagnostics into an auditable disease-review queue across a national infectious-disease portfolio. Across Thailand's 24 modelled diseases, monthly normalization, cumulative reconciliation, and seasonal realignment were frequently decoupled. That multidimensionality matters because display-oriented dashboards that emphasize only current incidence can understate unresolved cumulative disruption or miss the need to recalibrate seasonal surveillance expectations.

The counterfactual component proved robust enough to support this operational framing, although not model-invariant. Disease-specific model selection improved fit relative to one-size-fits-all specification, and most selected families were retained under alternative rank-based, single-metric, or horizon-weighted aggregation rules. At the same time, the uncertainty-aware analyses showed why deterministic RP/BP labels should not be treated as hard endpoints. For some diseases, such as HAV and chancroid, the distinction between monthly normalization and cumulative non-reconciliation was extremely stable. For others, including influenza, dengue fever, shigellosis, chickenpox, and syphilis, the simulated trajectories supported broader ranges of plausible recovery states and low deterministic phenotype stability. Reporting classification probabilities, timing intervals, and uncertainty-sensitive flags alongside deterministic labels therefore improves methodological transparency without removing the practical value of the framework.

The decision-utility analysis is the clearest reason this work should be interpreted as decision support rather than retrospective observation. An incidence-only workflow would have treated 20 of 24 modelled diseases as normalized by the end of follow-up, but the framework retained only 8 of those 20 in a low-priority routine queue and reassigned 12 to explicit follow-up categories. The 50% decision discordance rate and the identification of 4 potential false-negative triage errors under conventional review demonstrate that the framework changes surveillance prioritization rather than merely reproducing it. In operational terms, the system produces reviewable action states: cumulative review needed, recalibrate and monitor, balanced but recalibrate seasonality, high-priority manual review, and no-deficit monitoring. Each state is linked to transparent quantitative rules. The digital value therefore lies in structuring surveillance review, escalation, and auditability, not merely in presenting plots interactively.

The framework also yields direct public-health workflow scenarios. First, diseases in RP-only states with stable seasonality can be prioritized for cumulative-deficit follow-up to avoid false reassurance from monthly normalization. Second, diseases with RP-only plus seasonal displacement can trigger recalibration of alert timing and preparedness calendars before routine thresholds are reinstated. Third, diseases that are balanced but seasonally shifted can move to lower clinical concern while remaining in method-calibration queues. These use-cases are implementable as meeting-level triage rules for surveillance teams and provide an explicit bridge from analytics to resource allocation decisions.

The alternative endpoint analysis sharpened rather than weakened this interpretation. A strict sustained observed-to-expected ratio of at least 1.0 reproduced the same achieved-versus-not-achieved RP pattern as the primary 95% rule, suggesting that the main classification is not an artifact of a lenient monthly threshold. In contrast, return within the 95% predictive interval was too permissive to function as a decision endpoint because even persistently suppressed diseases eventually re-entered the interval. The half-deficit analysis was informative as an intermediate cumulative milestone, showing that partial cumulative catch-up was more common than full BP closure.

The framework is suitable for surveillance triage, identification of persistent departure from expected reported incidence, prioritization of disease-level review meetings, and recalibration of seasonal preparedness calendars. It is not suitable for estimating true infection burden, proving biological compensation, demonstrating immunity debt, quantifying under-ascertainment directly, or attributing rebound patterns to specific causal mechanisms. We therefore interpret RP and BP as surveillance-defined operational milestones rather than as natural epidemiological constants.<sup>11</sup>

Seasonal displacement deserves equal weight in that interpretation. The large phase shifts observed for mumps, HAV, HCV, and condyloma acuminatum show that even diseases classified as monthly-normalized or cumulatively balanced may still challenge pre-pandemic alert calendars. The joint interpretation of RP/BP phenotype with seasonal displacement revealed three distinct operational patterns that would be invisible under either metric alone, reinforcing why these components should be integrated into a single decision layer rather than treated as parallel analyses. This is especially relevant for surveillance systems that rely on expected peak timing or seasonal thresholds for response planning. The current interface should therefore be understood as the implementation layer of this workflow: it turns precomputed analytics into a portfolio queue, disease drill-down, and traceable supporting evidence for surveillance meetings. We do not claim prospective proof that use of the interface changed public-health outcomes, but the system is designed to support that next stage of evaluation.

External contextual triangulation provided a descriptive bridge between the portfolio trajectories and the broader pandemic environment. The deepest portfolio suppression aligned with months of highest national stringency and COVID-19 burden, and the first sustained portfolio normalization did not occur until July 2023, after the late-2022 low-restriction transition. Because these indicators were not included as predictive covariates and policy-series coverage ended in December 2022, we use them only to contextualize timing rather than to infer causality.

The framework is potentially transferable to other surveillance systems that have multi-year monthly series, reasonably stable case definitions, and documented reporting continuity. These prerequisites are attainable in many middle-income settings, but transferability should not be assumed where denominators, ascertainment, or case definitions changed materially over time. Thailand should therefore be interpreted as a national case study demonstrating a portfolio-level workflow rather than as a universal template.

Several limitations should be considered. First, routine surveillance data reflect changes in healthcare seeking, testing, and reporting as well as transmission, and these influences almost certainly differed across pathogens.<sup>3,6-10</sup> Second, although we propagated forecast uncertainty into RP/BP probabilities, those probabilities remain conditional on the selected model family, the assumed interruption structure, and the simulation scheme used for each model class. Third, monthly reconstruction from weekly data preserved totals and showed high overlap-period concordance, but residual allocation error may still affect low-count series and fine seasonal timing. Fourth, the common January 2020 interruption point and the post-PHSM comparison window from 2023 onward simplified heterogeneous real-world transitions, even though interruption-timing sensitivity showed only limited reclassification. Fifth, simulation-time sensitivity analyses reduced concern that RP/BP outputs were artifacts of a single simulation budget, but residual Monte Carlo error remains possible for borderline diseases. Sixth, the operational prioritization categories and the 2-month seasonal-shift flag were pragmatic surveillance rules rather than externally validated decision thresholds. Finally, the interface underwent structured heuristic workflow assessment rather than prospective end-user deployment, so the current evidence supports implementation readiness and auditability more directly than downstream effect on real-world surveillance actions. Future work should focus on prospective deployment with end-user engagement, formal usability testing, and evaluation of whether framework-guided review decisions lead to earlier detection of persistent surveillance anomalies or more efficient resource allocation compared with conventional incidence-only review.

An additional methodological boundary is generalizability. This analysis is a single-country case study and does not constitute external validation across health systems. Transfer to other settings requires comparable historical depth, reporting continuity, and disease definitions, plus local recalibration of operational thresholds and queue rules. The modular architecture is technically transferable, but epidemiological transferability must be re-evaluated in each implementation context.

---

## Methods

### Study design and data sources

We conducted a retrospective development and evaluation study of a digital public-health decision-support framework using publicly available aggregated surveillance data for nationally notifiable infectious diseases in Thailand. The study was reported in accordance with STROBE guidance for the observational data component.<sup>12</sup> Monthly national case counts were assembled for 2008-2024 from the Bureau of Epidemiology surveillance portal (https://doe1.moph.go.th/surdata/index.php), and weekly surveillance data for 2020-2025 were obtained from the Department of Disease Control dashboard (https://dvis3.ddc.moph.go.th/). The source registry comprised 72 monitored notifiable disease series. We retained 43 for descriptive analyses after excluding conditions with zero incidence, overlapping categories, major case-definition changes, non-communicable conditions, incomplete reporting, or unstable residual categories, and then retained 24 for counterfactual modelling after screening for pre-pandemic continuity and time-series suitability (Supplementary Tables S1-S3). The surveillance data were aggregated by disease and time; sex assigned at birth, gender identity, race, and ethnicity were not consistently available and were therefore not analysed.

### Data processing and disease selection

Annual population denominators were obtained from the United Nations World Population Prospects 2024 mid-year estimates.<sup>13</sup> Monthly incidence rates were expressed per 100,000 population. Because the official monthly series was not consistently updated beyond 2024, we reconstructed monthly counts for 2020-2025 from weekly data using constrained spline-based temporal disaggregation that preserved observed weekly totals. These procedures were intended to preserve weekly totals and temporal continuity rather than to infer individual event dates. Pipeline validation against official monthly totals in overlap years from 2020 to 2023 yielded Pearson r = 0.999, a median absolute error of 3 cases, and a median absolute percentage error of 3.9% across 1,968 disease-month observations (Supplementary Tables S6 and S7; Supplementary Figures S1-S86).

The 24-disease counterfactual subset was selected with explicit pre-specified operational criteria. Eligible diseases required a continuous 144-month monthly pre-pandemic series from January 2008 to December 2019, a sufficiently stable non-residual surveillance definition, and enough pre-pandemic signal to support 72-month counterfactual extrapolation without domination by structural zeros or erratic low-count noise. The two-stage disease-selection flow is summarized in Supplementary Table S1 and Supplementary Fig. S0, and all excluded series are itemized in Supplementary Tables S2 and S3 with exclusion category labels (sparse, duration-limited, non-seasonal, residual/unspecified). Diseases retained descriptively but excluded from counterfactual modelling were categorized by these criteria rather than removed for substantive public health unimportance.

### Counterfactual forecasting framework

For each of the 24 modelled diseases, we used the pre-pandemic period from January 2008 to December 2019 to forecast monthly counterfactual trajectories for January 2020 to December 2025, treating January 2020 as a common portfolio-level interruption point. This date coincided with the onset of major COVID-19-related disruption in Thailand and was used as an analytical anchor rather than as a disease-specific biological switch point. The resulting counterfactual trajectories are interpreted as statistical baselines under continuation of pre-pandemic dynamics, not as epidemiological ground truth. Counts were analysed on the log scale after adding 0.01 so that zero-count months remained estimable while introducing negligible distortion for higher-burden series.

We compared six candidate model families: neural network autoregression, error-trend-seasonal exponential smoothing, seasonal autoregressive integrated moving average, TBATS, a weighted hybrid ensemble, and Bayesian structural time series.<sup>14,15</sup> All models were fit with monthly seasonality fixed at 12 months. The hybrid ensemble combined autoregressive integrated moving average, exponential smoothing, neural-network autoregression, and TBATS components using weights derived from cross-validated root mean squared error. The Bayesian structural time-series model included local linear trend and seasonal components and was estimated with 1,000 Markov chain Monte Carlo iterations after a 10% burn-in period.

Model performance was compared under three rolling pre-pandemic hold-out schemes: training through December 2018 with validation on 2019, training through December 2017 with validation on 2018-2019, and training through December 2016 with validation on 2017-2019. The primary best-performing model family for each disease was selected by combining standardized forecast-accuracy measures across these three validation schemes. Equal weighting across sMAPE, RMSE, and MASE was used to avoid domination by a single scale-dependent criterion, and equal weighting across hold-out windows was used to balance short- and longer-horizon extrapolation performance. As robustness analyses, we repeated model selection using rank aggregation, sMAPE-only aggregation, and a horizon-weighted composite that gave greater weight to the shortest extrapolation window (Supplementary Table S11).

No nested cross-validation loop or external-country validation cohort was used. This was a deliberate design choice because the inferential target was counterfactual baseline construction under continuation of pre-2020 structure, not prospective prediction of post-2020 observed incidence. To mitigate model-selection instability under this design, we reported agreement across multiple aggregation rules and fixed-family sensitivity analyses, and we propagated forecast uncertainty into RP/BP classification.

### Recovery metrics

We compared observed monthly case counts after January 2020 with disease-specific counterfactual medians. For month $t$, observed cases were denoted by $O_t$ and the counterfactual median by $E_t$. Monthly deviation and cumulative deviation were defined as

$$
D_t = O_t - E_t
$$

and

$$
C_t = \sum_{\tau=1}^{t} D_{\tau},
$$

respectively. Disruption onset was defined as the first month for which $C_t < 0$, and the trough was the month at which $C_t$ reached its minimum. Operationally, the algorithm then proceeded in four steps: compute $D_t$ and $C_t$ from January 2020 onward; identify the first negative cumulative month and the trough; scan forward for the first 3-month window satisfying the RP rule; and finally scan from the trough for the first month at which $C_t \ge 0$ to define BP.

RP was defined as the first month $t$ for which observed incidence satisfied $O_t \ge 0.95E_t$ for three consecutive months and $C_t$ was non-decreasing over the same interval. BP was defined as the first month after the trough at which $C_t \ge 0$. We treated RP as an operational normalization milestone and BP as an operational cumulative reconciliation milestone rather than as universal epidemiological constants or direct measures of biological compensation. The 95% threshold and 3-month persistence rule were pre-specified as conservative surveillance-operational criteria to avoid single-month noise triggering recovery declarations; these choices were not tuned post hoc to maximize any one result. Additional descriptors included relative suppression at the trough, rebound intensity, suppression duration, and payback duration. Sensitivity analyses varied the RP threshold from 95% to 90% and the persistence requirement from 3 months to 2 or 4 months (Supplementary Table S5), and alternative endpoint checks included a sustained $O_t/E_t \ge 1.0$ rule and predictive-interval return.

### Uncertainty-aware classification, alternative endpoints, and interruption sensitivity

To propagate uncertainty into recovery classification, we applied the RP/BP algorithm to each of the 1,000 simulated counterfactual trajectories generated for each disease. This yielded disease-specific probabilities of achieving RP by the end of follow-up, probabilities of achieving BP by the end of follow-up, a probability of no sustained cumulative deficit, empirical 95% intervals for RP and BP month, and the probability that the deterministic median-based phenotype was retained. For operational interpretation only, we flagged diseases with deterministic phenotype stability below 0.5 as uncertainty-sensitive. We used 1,000 trajectories because this provided stable empirical quantiles and disease-level classification probabilities while remaining computationally feasible for all 24 diseases. We then varied simulation-time settings (Monte Carlo path count and BSTS chain length) to assess whether RP/BP probabilities and deterministic phenotypes changed materially under alternative simulation budgets (Supplementary Table S17).

We also evaluated interruption-timing sensitivity by repeating the deterministic RP/BP classification with analytical start dates of January 2020, March 2020, and April 2020. These dates were chosen to test whether the main conclusions depended strongly on a single national interruption anchor. The primary January 2020 specification was retained in the main analysis for cross-disease comparability.

To test whether the main conclusions depended on one particular recovery endpoint, we additionally computed three prespecified alternatives. First, we identified the first 3-month window in which observed counts lay within the disease-specific 95% predictive interval. Second, we identified the first 3-month window in which the observed-to-expected ratio satisfied $O_t/E_t \ge 1.0$. Third, we calculated the first month after the trough for which the cumulative deficit had halved, that is, $C_t \ge C_{\mathrm{trough}}/2$. These alternatives were intended as sensitivity analyses rather than replacements for the primary RP/BP framework.

### Decision-utility synthesis

For retrospective workflow emulation, a baseline incidence-only queue considered a disease operationally normalized if RP had been achieved; no-deficit series were treated separately as monitoring cases. We then generated a framework-based queue by combining RP/BP phenotype with seasonal displacement into pragmatic review categories: low-priority routine review, cumulative review needed, recalibrate and monitor, balanced but recalibrate seasonality, high-priority manual review, and no-deficit monitoring. The primary decision-utility summaries were the number of diseases reassigned away from routine review and the disease-specific rationale for each reassignment. For this synthesis only, substantial seasonal displacement was flagged when the absolute center-of-mass phase shift was at least 2 months relative to either the pre-pandemic observed profile or the post-PHSM counterfactual profile. This threshold was used as a practical surveillance flag rather than as a biological threshold.

### Contextual triangulation and task-based interface assessment

For descriptive temporal triangulation, we aggregated national Oxford COVID-19 Government Response Tracker indicators for Thailand to calendar months from January 2020 to December 2022, including stringency, government response intensity, school closing, internal movement restrictions, international travel controls, and testing policy. Weekly WHO COVID-19 case counts for Thailand were aggregated to calendar months through June 2024. These contextual series were summarized against the portfolio-level observed-to-expected ratio and were used only to contextualize timing, not as predictive covariates or causal drivers.

We also conducted an author-side task-based heuristic assessment of a frozen dashboard build after the prioritization tab had been added to evaluate whether the interface could operationalize the review-queue workflow. Six surveillance-review tasks were prespecified, covering portfolio overview, disease-level RP/BP inspection, filtered time-series comparison, seasonal review, source-tracing, and prioritization. Each task was scored on discoverability, interpretability, and auditability using a 1-4 rubric, and the assessment was intended to document interface coverage rather than to substitute for formal end-user usability testing.

### Seasonal analysis

Seasonal shape, amplitude, and timing were compared across three scenarios: pre-pandemic observed (through 2019), post-PHSM observed (from 2023 onward), and post-PHSM counterfactual (predicted from 2023 onward). We used *post-PHSM* to denote the period after relaxation of major national control measures and border restrictions. Seasonal profiles were constructed from mean monthly counts. The weighted circular center-of-mass phase shift was prespecified as the primary timing metric, with empirical peak month used as a sensitivity check.<sup>17-19</sup> For each disease, post-PHSM observed minus pre-pandemic observed and post-PHSM observed minus post-PHSM counterfactual timing shifts were mapped to the minimal signed displacement on a 12-month cycle.

### Statistics and reproducibility

Forecast performance for each disease and hold-out split was quantified using symmetric mean absolute percentage error, root mean squared error, and mean absolute scaled error.<sup>16</sup> These metrics were calculated as

$$
\mathrm{sMAPE} = \frac{100}{n}\sum_{t=1}^{n}\frac{2|O_t - E_t|}{|O_t| + |E_t|},
$$

$$
\mathrm{RMSE} = \sqrt{\frac{1}{n}\sum_{t=1}^{n}(O_t - E_t)^2},
$$

and

$$
\mathrm{MASE} = \frac{\frac{1}{n}\sum_{t=1}^{n}|O_t - E_t|}{\frac{1}{n-1}\sum_{t=2}^{n}|O_t - O_{t-1}|}.
$$

For each disease and hold-out split, the three metrics were z-standardized across the six candidate models, sign-reversed so that larger values indicated better performance, and summed with equal weight to form a split-specific composite index. Composite indices were then summed across the three hold-out schemes to select the disease-specific best model.

Forecast uncertainty was summarized from 1,000 simulated trajectories per disease generated from the fitted models. Monthly medians and 80% and 95% predictive intervals were obtained from the empirical quantiles of these simulated trajectories. As robustness analyses, we re-estimated all recovery classifications under fixed exponential smoothing and fixed seasonal autoregressive integrated moving average families and compared disease-level reclassification against the primary disease-specific best-model analysis (Supplementary Table S8). We additionally evaluated simulation-time sensitivity by varying Monte Carlo path counts and BSTS iterations and comparing RP/BP probabilities and phenotype changes against the primary setting (Supplementary Table S17). We further summarized uncertainty-aware RP/BP probabilities, interruption-timing sensitivity, alternative model-selection rules, joint operational prioritization, alternative endpoint sensitivity, contextual triangulation, and the task-based interface assessment in Supplementary Tables S9-S17.

Additional supplementary analyses used seasonal-trend decomposition by Loess and segmented log-linear regression to characterize broad temporal trends, generalized additive models to visualize suppression-rebound associations, and k-means clustering with the gap statistic to summarize disruption-recovery typologies.<sup>20-24</sup> Because this study analysed complete national surveillance time series rather than repeated laboratory or animal experiments, reproducibility pertains to transparent data processing, model specification, and sensitivity analysis rather than to biological replicates. All analyses were conducted in R version 4.5.2.

### Operational implementation

A web-based surveillance interface was developed to operationalize the framework as a portfolio review system rather than as a static report. The interface displays precomputed disease-level outputs, including recovery classifications, uncertainty summaries, observed and counterfactual time series, prioritization tables, seasonal comparisons, and source-tracing views. It was designed to separate offline model fitting from interactive review, thereby minimizing response time and enabling auditable drill-down from queue assignment to underlying evidence. All analytical outputs presented through the interface were generated offline from the reproducible pipeline described above.

### Ethics approval

This study used publicly available, de-identified, aggregated surveillance data and did not involve individual participant contact or intervention. Ethical approval and individual informed consent were therefore not required under institutional policy for secondary analysis of anonymized public surveillance data.

---

## Data Availability

The raw aggregated surveillance data used in this study are publicly available from the Bureau of Epidemiology and Department of Disease Control, Ministry of Public Health, Thailand (https://doe1.moph.go.th/surdata/index.php; https://dvis3.ddc.moph.go.th/). Processed aggregated analytical datasets, metadata used for disease classification, and data dictionaries required to reproduce the reported analyses are available in the project repository at https://github.com/xmusphlkg/ID_TH.

## Code Availability

Code for data processing, counterfactual forecasting, recovery classification, figure generation, and the web-based surveillance interface is available at https://github.com/xmusphlkg/ID_TH.

## Author Contributions

K.L. conceptualized the study, developed the methodology, curated data, produced visualizations, and drafted the manuscript. Y.X. contributed to methodology development, validation, and formal analysis. Y.Z. and Ta.C. contributed to data curation, formal analysis, and investigation. Y.S., Z.Z., Q.C., J.R., and Ti.C. supervised the study. J.R. and Ti.C. reviewed and edited the manuscript. All authors interpreted the results, approved the final manuscript, and accept responsibility for the decision to submit.

## Acknowledgements

We thank the Bureau of Epidemiology, Ministry of Public Health, Thailand, for maintaining and providing access to long-term national infectious disease surveillance data. This work was supported by the Self-supporting Program of Guangzhou Laboratory (GZNL2024A01004), the National Natural Science Foundation of China (825B2104), and the National Key Research and Development Program of China (2024YFC2311404). The funders had no role in study design, data collection, data analysis, data interpretation, or writing of the manuscript.

## Competing Interests

The authors declare no competing interests.

---

## References

1. Hu, W.-H., Sun, H.-M., Wei, Y.-Y. & Hao, Y.-T. Global infectious disease early warning models: an updated review and lessons from the COVID-19 pandemic. *Infect. Dis. Model.* **10**, 410-422 (2025).
2. Eales, O. et al. Key challenges for respiratory virus surveillance while transitioning out of acute phase of COVID-19 pandemic. *Emerg. Infect. Dis.* **30**, e230768 (2024).
3. Li, K. et al. Temporal shifts in 24 notifiable infectious diseases in China before and during the COVID-19 pandemic. *Nat. Commun.* **15**, 3891 (2024).
4. Chongsuvivatwong, V. et al. Health and health-care systems in southeast Asia: diversity and transitions. *Lancet* **377**, 429-437 (2011).
5. Sittimart, M., Rachatan, C., Muenkaew, P. & Dabak, S. V. Past, present, and future: a situational analysis of infectious disease modelling in Thailand. *Lancet Reg. Health Southeast Asia* **39**, 100684 (2025).
6. Olsen, S. J. et al. Decreased influenza activity during the COVID-19 pandemic: United States, Australia, Chile, and South Africa, 2020. *Am. J. Transplant.* **20**, 3681-3685 (2020).
7. Huang, Q. S. et al. Impact of the COVID-19 nonpharmaceutical interventions on influenza and other respiratory viral infections in New Zealand. *Nat. Commun.* **12**, 1001 (2021).
8. Baker, R. E. et al. The impact of COVID-19 nonpharmaceutical interventions on the future dynamics of endemic infections. *Proc. Natl Acad. Sci. USA* **117**, 30547-30553 (2020).
9. Chen, Y. et al. Measuring the effects of COVID-19-related disruption on dengue transmission in southeast Asia and Latin America: a statistical modelling study. *Lancet Infect. Dis.* **22**, 657-667 (2022).
10. Shet, A. et al. Impact of the SARS-CoV-2 pandemic on routine immunisation services: evidence of disruption and recovery from 170 countries and territories. *Lancet Glob. Health* **10**, e186-e194 (2022).
11. Munro, A. P. S. & House, T. Cycles of susceptibility: immunity debt explains altered infectious disease dynamics post-pandemic. *Clin. Infect. Dis.* ciae493 (2024).
12. Cuschieri, S. The STROBE guidelines. *Saudi J. Anaesth.* **13**, S31-S34 (2019).
13. United Nations Department of Economic and Social Affairs, Population Division. *World Population Prospects 2024 Online Edition* (United Nations, 2024); https://population.un.org/wpp/
14. Hyndman, R. J. & Athanasopoulos, G. *Forecasting: Principles and Practice* 3rd edn (OTexts, 2021).
15. Scott, S. L. & Varian, H. S. Predicting the present with Bayesian structural time series. *Int. J. Math. Model. Numer. Optim.* **5**, 4-23 (2014).
16. Hyndman, R. J. & Koehler, A. B. Another look at measures of forecast accuracy. *Int. J. Forecast.* **22**, 679-688 (2006).
17. Naumova, E. N. Mystery of seasonality: getting the rhythm of nature. *J. Public Health Policy* **27**, 2-12 (2006).
18. Mardia, K. V. & Jupp, P. E. *Directional Statistics* (Wiley, 1999).
19. Fisher, N. I. *Statistical Analysis of Circular Data* (Cambridge Univ. Press, 1993).
20. Cleveland, R. B., Cleveland, W. S., McRae, J. E. & Terpenning, I. STL: a seasonal-trend decomposition procedure based on Loess. *J. Off. Stat.* **6**, 3-73 (1990).
21. Muggeo, V. M. R. Estimating regression models with unknown break-points. *Stat. Med.* **22**, 3055-3071 (2003).
22. Wood, S. N. *Generalized Additive Models: An Introduction with R* 2nd edn (Chapman & Hall/CRC, 2017).
23. Hartigan, J. A. & Wong, M. A. A K-means clustering algorithm. *J. R. Stat. Soc. C Appl. Stat.* **28**, 100-108 (1979).
24. Tibshirani, R., Walther, G. & Hastie, T. Estimating the number of clusters in a data set via the gap statistic. *J. R. Stat. Soc. B* **63**, 411-423 (2001).

---

## Figure Legends

**Figure 1. Framework overview of the counterfactual digital decision-support pipeline.** The pipeline comprises six layers: (1) data ingestion from Thailand's national infectious disease surveillance system; (2) automated data processing with weekly-to-monthly temporal disaggregation and overlap-year validation; (3) disease-specific model selection across six forecasting families using rolling hold-out validation and composite accuracy scoring; (4) generation of counterfactual trajectories, predictive intervals, and simulated forecast paths; (5) analytical modules for RP/BP recovery classification, uncertainty-aware phenotype probabilities, alternative endpoint checks, seasonal displacement detection, and contextual triangulation; and (6) a web-based review layer for operational surveillance prioritization. The right-hand workflow summarizes data validation, robustness checks, and endpoint sensitivity analyses.

**Figure 2. Disease-specific model selection performance.** Heatmap of composite z-standardized performance indices, calculated from sMAPE, RMSE, and MASE across three hold-out schemes for each model family and disease. Warmer colours indicate better performance. Asterisks mark the selected best model for each disease. Diseases are grouped by transmission category.

**Figure 3. Dual-metric surveillance-defined recovery classification across 24 infectious diseases.** (A) Summary of recovery phenotypes: both RP and BP achieved, RP achieved without BP, persistently suppressed, and no sustained cumulative deficit. (B-Y) Disease-specific panels showing monthly observed incidence and the counterfactual median forecast with 95% predictive intervals from January 2008 to December 2025. Blue shading denotes the operational recovery period and gold shading denotes the cumulative balance period. Areas between observed and expected trajectories are shaded to indicate cumulative deficit or surplus. For readability in the main text, interpretation emphasizes phenotype summary plus representative disease trajectories, while complete disease-level uncertainty summaries are provided in Supplementary Table S9.

**Figure 4. Seasonal displacement after COVID-19 disruption.** (A-D) Heatmaps of monthly observed-to-expected incidence ratios for respiratory, vector-borne and zoonotic, gastrointestinal, and sexually transmitted infections. Values below 1 indicate suppression and values above 1 indicate exceedance. (E-AB) Seasonal profiles comparing pre-pandemic observed, post-PHSM observed, and post-PHSM counterfactual patterns. Triangles denote circular center-of-mass timing estimates and circles denote empirical peak months. Grey shading denotes the rainy season in Thailand. Joint interpretation of RP/BP phenotype and seasonal displacement for operational prioritization is provided in Supplementary Table S12.

**Figure 5. Operational decision-support interface.** Screenshots of representative analytical views within the web-based interface. (A) Disease portfolio overview and filtering tools. (B) Recovery view showing RP/BP classification with observed-versus-counterfactual trajectories. (C) Time-series view for disease-specific trajectory inspection with predictive intervals. (D) Seasonality view showing pre-pandemic and post-PHSM seasonal signatures and timing comparisons. The current build additionally includes a prioritization tab that joins recovery phenotype, seasonal displacement, and uncertainty summaries into action-oriented categories. The interface serves precomputed analytical outputs and is designed for rapid disease-level review rather than real-time model fitting.

---

## Tables

**Table 1. Disease-level digital review queue summary across the 24 modelled diseases.** RP and BP month values are counted from January 2020, so month 0 corresponds to January 2020. `Uncertainty-sensitive` denotes deterministic phenotype stability below 0.5 in Supplementary Table S9.

| Disease | Model | Phenotype | Pr(RP) | Pr(BP) | RP month | BP month | Shift vs pre | Amp ratio | Review queue | Stable in sensitivity | Uncertainty-sensitive |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Chickenpox | NNAR | Suppressed | - | - | -1 | 0.4 | High priority manual review | Yes | Yes |
| Influenza | SARIMA | RP only | 43 | - | 1 | 0.95 | Recalibrate and monitor | Yes | Yes |
| Mumps | ETS | RP only | 58 | - | 6 | 0.74 | Recalibrate and monitor | Yes | No |
| Pneumonia | TBATS | Balanced | 34 | 61 | 1 | 0.97 | Low priority routine review | Yes | No |
| Rubella | TBATS | Suppressed | - | - | 3 | 2.02 | High priority manual review | Yes | No |
| Scarlet fever | TBATS | RP only | 61 | - | -1 | 3.97 | Cumulative review needed | Yes | Yes |
| Dengue fever | TBATS | RP only | 35 | - | 0 | 1.1 | Cumulative review needed | Yes | Yes |
| Leptospirosis | Hybrid | Balanced | 28 | 33 | 1 | 0.81 | Low priority routine review | Yes | No |
| Malaria | SARIMA | No deficit | - | - | 0 | 1.56 | No deficit monitoring | Partial | Yes |
| Melioidosis | Hybrid | Balanced | 28 | 41 | -2 | 1.13 | Balanced but recalibrate seasonality | Yes | No |
| S. suis | ETS | Balanced | 5 | 6 | 0 | 0.8 | Low priority routine review | Yes | No |
| Scrub Typhus | BSTS | Balanced | 27 | 32 | 0 | 1.01 | Low priority routine review | Yes | Yes |
| Amebiasis | TBATS | Balanced | 29 | 36 | 0 | 1.03 | Low priority routine review | Yes | No |
| HAV | Hybrid | RP only | 46 | - | -5 | 0.38 | Recalibrate and monitor | Yes | No |
| HFMD | Hybrid | Balanced | 9 | 32 | 1 | 1.23 | Low priority routine review | Yes | No |
| Shigellosis | BSTS | RP only | 31 | - | 1 | 1.32 | Cumulative review needed | Partial | Yes |
| Typhoid | Hybrid | Balanced | 30 | 54 | 0 | 0.81 | Low priority routine review | Yes | No |
| CA (HPV) | BSTS | Balanced | 43 | 56 | 4 | 0.85 | Balanced but recalibrate seasonality | Yes | No |
| Chancroid | Hybrid | RP only | 48 | - | 0 | 1.06 | Cumulative review needed | Partial | No |
| Genital herpes | Hybrid | Balanced | 36 | 51 | 2 | 1.07 | Balanced but recalibrate seasonality | Yes | No |
| Gonorrhoea | TBATS | Balanced | 36 | 61 | 0 | 1.06 | Low priority routine review | Yes | No |
| HBV | Hybrid | Balanced | 41 | 52 | 2 | 1.01 | Balanced but recalibrate seasonality | Partial | No |
| HCV | ETS | Balanced | 28 | 34 | 4 | 0.87 | Balanced but recalibrate seasonality | Yes | No |
| Syphilis | BSTS | Suppressed | - | - | -2 | 1.02 | High priority manual review | Yes | Yes |
