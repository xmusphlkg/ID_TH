Weaknesses
Technical limitations or concerns

Forecasting families are not count-data specific and may have calibration issues for low-incidence series; sMAPE can be unstable with small denominators.
The square-root transform with constant and bootstrapped residuals may yield miscalibrated predictive intervals; no formal assessment of interval coverage is reported.
A common interruption date (January 2020) may not reflect disease-specific onset of disruption; though tested in sensitivity analyses, this remains an operational simplification.
Seasonal displacement threshold (≥2 months) and RP threshold (95% for ≥3 consecutive months) are pragmatic but ad hoc; limited justification or data-driven calibration.
Experimental gaps or methodological issues

Validation is primarily internal to the rule system (freeze-point emulation) rather than against independent outcomes or alternative triage frameworks (e.g., CUSUM/EARS, causal-impact baselines, or hierarchical Bayesian approaches with explicit reporting models).
Placebo windows reportedly produce “non-rare false alerts,” yet the paper does not quantify false-positive/false-negative rates of RP/BP classifications under no-disruption scenarios.
No simulation study injecting synthetic disruptions to benchmark timeliness, specificity, and robustness of RP/BP decisions versus established surveillance algorithms.
Limited exploration of covariates (e.g., mobility, PHSM measures) or hierarchical models that could improve counterfactual stability and interpretability.
Clarity or presentation issues

Some implementation details of the hybrid ensemble and uncertainty generation for each family are concise; calibration diagnostics for predictive distributions are not shown.
Disease selection and coverage are explained well, but representativeness and implications for diseases excluded from modeling (beyond the rationale) could be more explicit in the main text.
Missing related work or comparisons

Minimal engagement with classic surveillance/quality-control baselines (EARS, CUSUM, Farrington-like methods) and modern hierarchical models for reporting delays/nowcasting (e.g., Bayesian spatio-temporal approaches), which could serve as comparators or complements.
BSTS is used as a candidate forecasting family, but discussion of causal-impact-style counterfactual designs for interruption detection is limited.
Detailed Comments
Technical soundness evaluation

The dual-metric design is a conceptually sound and practically meaningful innovation: RP captures whether current incidence is “back on track,” while BP recognizes that cumulative backlog can persist, motivating continued monitoring or targeted action.
Disease-specific model selection is appropriate given heterogeneous seasonality and signal strength across infections; cross-validated aggregation across sMAPE/RMSE/MASE is reasonable, though the equal-weight z-scoring across error metrics may imbalance contributions if distributions differ; sMAPE instability near zero can bias model ranking.
The simulation-based uncertainty propagation is a strength; however, without empirical coverage checks, users cannot gauge whether RP/BP probabilities are well-calibrated, especially given diverse bootstrap schemes across models. A calibration analysis (e.g., PIT histograms, coverage by horizon) would add confidence.
The square-root transform and residual bootstraps for ETS/TBATS and ensembles are standard but may under-represent overdispersion or time-varying volatility post-disruption.
Experimental evaluation assessment

The framework’s effect on triage decisions is clearly demonstrated relative to an “incidence-only” comparator. The retrospective freeze-point emulation adds realism, though it remains internal validation anchored to the same decision rules.
Robustness analyses indicate that core findings are not sensitive to modest variations in thresholds or interruption dates. This is important for policy portability.
The placebo analyses are a critical inclusion but are underutilized; quantifying the rate of spurious RP/BP signals would be highly informative for operational risk.
Comparison with related work (using the summaries provided)

Compared with hierarchical nowcasting models for reporting delays (e.g., the Ohio Bayesian spatio-temporal approach), this framework tackles a different but complementary problem: post-disruption counterfactual recovery and triage, not primarily delay correction. Incorporating explicit reporting-delay models could further strengthen the counterfactual foundation where delays were dynamic.
BSTS/CausalImpact-style counterfactual methods have been applied to policy shocks in other domains; while BSTS appears among candidate models, the paper does not leverage causal-impact designs to estimate intervention effects or validate RP onset. A richer discussion of such causal baselines (and why they were not used here) would improve methodological positioning.
Broader discussions on surveillance requirements and governance (e.g., lessons from U.S. pandemic surveillance upgrades and ASEAN governance) underscore the importance of decision-ready, auditable outputs; this work contributes a practical instantiation of such principles for a national portfolio but would benefit from explicit alignment with governance frameworks and equity considerations.
Prior works on digital resilience and operational monitoring emphasize usability, transparency, and trust; the Shiny implementations and auditability aim in that direction, but user testing or prospective deployment outcomes are not yet reported.
Discussion of broader impact and significance

The core contribution is an operational decision-support layer that recasts familiar time-series analytics into auditable, reproducible states and queues. This is a valuable step for ministries aiming to institutionalize post-shock surveillance triage.
Claims are appropriately scoped: the authors refrain from asserting downstream health-outcome improvements and instead emphasize implementation readiness and prioritization benefits.
The openness of data, code, and apps advances reproducibility and will likely accelerate adoption and adaptation in other LMIC/UMIC contexts with long-standing surveillance systems.
To maximize real-world impact, additional benchmarking (classical surveillance baselines), calibration diagnostics, and prospective or quasi-prospective evaluations with end users would be essential next steps.
Questions for Authors
How well calibrated are your predictive distributions across diseases and horizons? Can you report empirical coverage and PIT diagnostics for the simulated trajectories, and relate these to the stability of RP/BP probabilities?
Did you evaluate RP/BP false-positive/false-negative rates under placebo (no-disruption) windows quantitatively? If so, what are the rates by disease class, and how would you recommend operational thresholds to manage these risks?
Why were classical surveillance comparators (e.g., EARS, CUSUM, Farrington-like methods) not included? Could you benchmark RP/BP-based triage against these to contextualize performance and to position your framework within established public health practice?
Can you elaborate on the hybrid ensemble (component models, weighting, and uncertainty construction) and discuss potential overfitting given limited pre-pandemic data for some series?
The 95% RP threshold with 3-month persistence and 2-month seasonal shift cutoff are pragmatic. Did you explore data-driven calibration (e.g., optimizing prospective capture of “needs follow-up” status at freeze points) or expert-elicited thresholds? How sensitive are triage errors to these choices?
Given heterogeneous disruption onsets by disease, have you considered data-adaptive interruption detection (e.g., structural break detection or causal-impact tests) to complement the fixed January/March/April 2020 starts?
How would you incorporate covariates (mobility, PHSM indicators) or hierarchical models (across diseases or provinces) to stabilize counterfactuals and improve interpretability (e.g., distinguishing reporting vs transmission changes)?
For diseases with low amplitude or sparse counts, how do you guard against spurious seasonality shifts (e.g., center-of-mass instability)? Would alternative seasonality metrics (e.g., circular regression or harmonic-phase estimation with uncertainty) yield more robust flags?
What were the computational costs for model selection, simulation, and app generation at national scale, and what are your recommendations for operationalization (e.g., re-fit cadence, monitoring alerts) in a ministry environment?
Have you conducted any formative usability testing with surveillance staff to assess whether the queues and uncertainty flags improve prioritization decisions or reduce time-to-action compared with standard dashboards?
Overall Assessment
This is a timely, thoughtful, and practically oriented contribution to digital public health surveillance. The paper’s central innovation—codifying recovery along two operational dimensions (monthly normalization vs cumulative reconciliation) and integrating seasonal displacement—addresses a real gap in post-disruption portfolio management. The methodology is solid for retrospective decision support, with careful disease-specific model selection, uncertainty propagation, and sensitivity analyses. The findings are well-communicated, the code/data are open, and the authors responsibly scope their claims.

However, the empirical validation is largely internal to the proposed rules, and several methodological choices (thresholds, seasonal displacement cutoff, reliance on standard forecasting families without explicit delay/reporting models) would benefit from deeper benchmarking and calibration. In particular, prospective or quasi-prospective evaluation, explicit false-alert characterization under placebo scenarios, and comparisons with standard surveillance baselines (EARS/CUSUM/Farrington, causal-impact approaches) would materially strengthen the case for adoption and help readers assess trade-offs.

I view this work as publishable in npj Digital Medicine after substantial revision that addresses benchmarking, calibration, and validation gaps and adds more guidance for operational deployment. The framework’s decision-support orientation and open implementations make it valuable to the community; bolstering methodological comparisons and empirical calibration would elevate it to the journal’s standards.