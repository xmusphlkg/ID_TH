# Manuscript and Project Improvement Brief for GPT Agent
**Target journal:** *npj Digital Medicine*  
**Project/manuscript title:** *A digital surveillance framework identifies decoupled and heterogeneous infectious-disease recovery in Thailand after COVID-19*

## Purpose of this brief
This document summarizes high-value, concrete, and feasible improvement directions for the manuscript and the broader project. The goal is to help a GPT agent strengthen the study for a more competitive submission to *npj Digital Medicine*, while preserving scientific rigor and avoiding overstatement.

---

## 1. Executive summary

The project has real potential. Its strongest contribution is not any single forecasting model, but the **portfolio-level digital surveillance framework** that distinguishes between:

- **monthly normalization** of observed incidence relative to counterfactual expectation, and
- **closure of cumulative deficit** after pandemic-related disruption.

This distinction is scientifically meaningful and operationally useful. The manuscript is already reasonably well structured and the problem is important. However, in its current form, it is **not yet maximally competitive for *npj Digital Medicine***.

The main issues are:

1. The **digital medicine positioning is still not strong enough**.
2. The **counterfactual foundation needs stronger robustness and uncertainty handling**.
3. The **recovery metrics are interpretable but still appear partly threshold-driven and somewhat arbitrary**.
4. The manuscript must more clearly distinguish **surveillance-defined recovery** from **true epidemiological or biological recovery**.
5. The **seasonality module is interesting but not yet fully integrated into the central decision-support story**.

The next stage should focus on **upgrading rigor and sharpening positioning**, rather than adding more loosely connected analyses.

---

## 2. Main strengths to preserve

### 2.1 Strong problem framing
The manuscript addresses a real gap: routine infectious disease dashboards often show current incidence, but they do not reveal whether cumulative burden has returned to its expected trajectory after a major disruption.

### 2.2 Portfolio-level perspective
The study moves beyond single-pathogen analyses and evaluates a broad national infectious disease portfolio. This is one of the most valuable aspects of the work.

### 2.3 Operationally oriented framework
The framework is relatively interpretable and potentially deployable:
- disease-specific counterfactual forecasting,
- dual recovery metrics,
- seasonal displacement analysis,
- precomputed interface for review.

### 2.4 Writing quality is already fairly good
The manuscript is generally coherent and well organized. The authors already acknowledge several limitations honestly, which is a good foundation.

---

## 3. Highest-priority issues to fix

## 3.1 Strengthen the *npj Digital Medicine* positioning

### Problem
At present, the work reads more like a **digital public health surveillance / infectious disease modeling paper** than a clearly high-impact **digital medicine / digital decision-support** paper.

The interface is described mostly as an implementation layer, not as a demonstrated contributor to improved decision-making.

### Risk
Reviewers may ask:
- Why is this best suited for *npj Digital Medicine* rather than a surveillance, epidemiology, or public health journal?
- What does the digital system actually improve beyond conventional surveillance summaries?
- Does the interface change decisions, triage, prioritization, or response workflows?

### Recommended improvements
#### A. Add a **decision utility** analysis
Show what the framework changes in practice. For example:
- Which diseases would look “recovered” if only monthly incidence were used?
- Which diseases are reclassified as still unresolved once BP is considered?
- Which diseases would require surveillance recalibration because seasonal phase has shifted?

A practical comparison between:
- **standard dashboard interpretation**, and
- **framework-guided prioritization**
would greatly strengthen the paper.

#### B. Add a minimal **expert or user evaluation**
Even a small structured assessment from public health professionals would help. For example:
- interpretability,
- usefulness,
- confidence in the outputs,
- likely use in surveillance review meetings.

Even a modest expert feedback exercise would improve fit for *npj Digital Medicine*.

#### C. Reframe the system as a **digital decision-support framework**
The paper should be clearer about:
- who uses the system,
- when it is used,
- what decisions it informs,
- what output is actionable.

---

## 3.2 Strengthen the counterfactual foundation

### Problem
The entire study depends on the credibility of the counterfactual expected trajectories. Since these are not directly observable, the manuscript must do everything possible to reduce reviewer concern.

### Key weaknesses to address

#### A. Single interruption point at January 2020
Using January 2020 as a common anchor is understandable for comparability, but it is still a simplification. Different diseases and surveillance processes were affected at different times.

#### B. Model selection logic is reasonable, but not yet fully defended
The current approach uses:
- six model families,
- three rolling hold-out schemes,
- multiple error metrics,
- z-standardization and equal-weight composite scoring.

This is workable, but reviewers may ask:
- Why these metrics?
- Why equal weighting?
- Why is short pre-pandemic hold-out performance the best basis for long post-2020 counterfactual extrapolation?

#### C. RP/BP classification currently relies on counterfactual medians
The study simulates 1,000 trajectories, but the final classification uses the median trajectory and deterministic rules. This underuses the uncertainty information already available.

### Recommended improvements
#### A. Add **interruption timing sensitivity analyses**
At minimum, compare results using:
- January 2020,
- March 2020,
- April 2020.

Report how many disease classifications change.

#### B. Add **model-selection sensitivity analyses**
Compare the current composite selection rule with alternatives such as:
- rank-based aggregation,
- single-metric selection,
- horizon-weighted selection.

The goal is not to replace the main method, but to show that the conclusions are not driven by one specific scoring rule.

#### C. Propagate uncertainty into RP/BP classification
This is probably the single most valuable methodological upgrade.

Use the simulated counterfactual trajectories to estimate:
- probability that RP is achieved by end of follow-up,
- probability that BP is achieved by end of follow-up,
- uncertainty intervals for RP month and BP month,
- classification stability.

Then report both:
- the deterministic median-based classification, and
- the uncertainty-aware classification probabilities.

This would substantially improve the methodological credibility of the framework.

---

## 3.3 Clarify what “recovery” means and what it does **not** mean

### Problem
The paper mostly handles this issue responsibly, but some readers may still interpret the findings as reflecting “true disease burden recovery” or “biological compensation.”

That is not what the data can support.

### What should be made clearer
This study primarily evaluates:
- **reported surveillance incidence relative to counterfactual expectation**,
not directly:
- true infection burden,
- true transmission intensity,
- biological immunity repayment,
- causal mechanisms of rebound.

### Recommended improvements
#### A. Tighten terminology throughout
Prefer language such as:
- “surveillance-defined recovery”
- “reported incidence normalization”
- “cumulative deficit closure”
- “cumulative reconciliation”

Use with more caution:
- “disease burden recovery”
- “payback”
- “biological compensation”

#### B. Add a dedicated paragraph in the Discussion
Explicitly state:
- what the framework is suitable for,
- what it is not suitable for.

For example:

**Suitable for:**
- surveillance triage,
- identifying persistent departure from expected reported incidence,
- supporting surveillance recalibration,
- highlighting diseases needing manual review.

**Not suitable for:**
- estimating true infections,
- inferring causal biological mechanisms,
- proving immunity debt,
- estimating under-ascertainment directly.

#### C. If possible, add contextual triangulation for representative diseases
For example, use one or two external contextual timelines:
- mobility restrictions,
- school reopening,
- border reopening,
- health service utilization proxies,
- testing volume proxies.

This would not turn the study into a causal analysis, but it would help interpret recovery phenotypes more credibly.

---

## 3.4 Make the RP/BP framework look less arbitrary

### Problem
The RP/BP framework is conceptually useful, but it may still look threshold-driven:
- 95% of expected incidence,
- 3 consecutive months,
- cumulative non-decrease condition,
- BP as first cumulative return to zero after trough.

These choices are interpretable, but reviewers may still question whether they were chosen somewhat ad hoc.

### Recommended improvements
#### A. State clearly that RP and BP are **operational milestones**
Do not present them as natural or universal epidemiological constants.

Suggested framing:
- **RP** = operational normalization milestone
- **BP** = cumulative reconciliation milestone

#### B. Expand threshold sensitivity analyses
In addition to the existing sensitivity work, consider:
- 90%, 95%, 100% thresholds,
- 2-, 3-, and 4-month persistence,
- alternative sustained-return rules,
- disease-stratified sensitivity by burden or seasonality strength.

#### C. Compare against at least one alternative endpoint definition
Examples:
- first sustained return within predictive interval,
- first sustained month with observed/expected ratio ≥ 1,
- time to halve cumulative deficit.

This does not mean the main definition must change, but it helps demonstrate that the main conclusions are not uniquely dependent on one arbitrary rule.

---

## 3.5 Better integrate seasonal displacement into the core story

### Problem
The seasonality module is interesting and potentially valuable, but currently it risks feeling like a parallel analysis rather than an integral part of the framework.

### Recommended improvements
#### A. Explicitly connect seasonality to surveillance recalibration
Make the logic more explicit:
- a disease may appear recovered on monthly magnitude,
- but its within-year timing may remain shifted,
- so traditional early warning thresholds and preparedness calendars may still be misaligned.

#### B. Jointly summarize recovery phenotype and seasonal displacement
Create a synthesis figure or table showing, for each disease:
- RP/BP phenotype,
- phase shift magnitude,
- amplitude change.

This would allow identification of diseases that are:
- normalized but still seasonally displaced,
- unresolved and seasonally displaced,
- fully stabilized.

#### C. Highlight diseases where seasonality changes would matter operationally
For example:
- diseases for which preparedness timing could be shifted,
- diseases for which peak-season surveillance thresholds may need recalibration.

---

## 4. Priority additional analyses

## Tier 1: Highest-value additions
These are the most important upgrades.

### 4.1 Uncertainty-aware RP/BP classification
Use the 1,000 simulated trajectories to estimate:
- probability of achieving RP,
- probability of achieving BP,
- uncertainty in timing,
- uncertainty in phenotype assignment.

### 4.2 Interruption timing sensitivity
Test at least:
- 2020-01,
- 2020-03,
- 2020-04.

### 4.3 Transparent disease inclusion/exclusion flow
Provide:
- full disease screening flowchart,
- list of excluded diseases,
- quantitative eligibility criteria,
- rationale for exclusions,
- whether exclusions may bias the interpretation of the portfolio.

## Tier 2: Strongly recommended
### 4.4 Decision utility / prioritization impact analysis
Demonstrate what the framework changes compared with incidence-only surveillance interpretation.

### 4.5 Joint recovery-seasonality summary
Integrate the two main analytical outputs into one operational synthesis.

## Tier 3: Nice if feasible
### 4.6 Small expert/usability assessment
A lightweight public health user review could help justify the digital decision-support aspect of the system.

---

## 5. Section-by-section manuscript revision guidance

## 5.1 Title
The current title is acceptable, but it could better emphasize the conceptual novelty.

Possible directions:
- emphasize the counterfactual framework,
- emphasize the distinction between monthly normalization and cumulative recovery,
- emphasize surveillance recalibration relevance.

Examples:
- **A counterfactual digital surveillance framework reveals decoupled normalization and cumulative recovery across infectious diseases in Thailand after COVID-19**
- **Digital surveillance of post-pandemic infectious disease recovery in Thailand reveals decoupled monthly normalization and cumulative deficit closure**

---

## 5.2 Abstract
### What to improve
- Make the operational significance more explicit.
- Clarify that this is surveillance-defined recovery.
- Avoid overinterpreting cumulative balance as biological compensation.
- Briefly mention uncertainty or robustness if new analyses are added.

### Goal
The abstract should make clear that the contribution is not merely descriptive forecasting, but an interpretable framework for identifying unresolved cumulative disruption and seasonal misalignment.

---

## 5.3 Introduction
### What to strengthen
The Introduction should sharpen three points:
1. Existing studies are often pathogen-specific.
2. Routine dashboards capture current incidence but not cumulative recovery.
3. Public health systems need an operational framework for portfolio-level post-disruption surveillance review.

### Additional recommendation
The “immunity debt” concept should remain secondary. The paper is strongest as an operational surveillance framework, not as a mechanism paper.

---

## 5.4 Results
### Main issue
There is a lot of material, but the most important take-home messages are not compressed forcefully enough.

### Recommended improvements
For each Results subsection, make the key message explicit in the first or last sentence.

Suggested core messages:
- **Model selection:** disease-specific forecasting materially improved counterfactual fit.
- **Recovery classification:** monthly normalization and cumulative deficit closure were frequently decoupled.
- **Seasonality:** some diseases showed persistent timing displacement even after apparent normalization.
- **Operational implementation:** outputs can support disease-level surveillance review and prioritization.

### Add a summary table
A main-text summary table would help substantially. For each modeled disease, report:
- selected model family,
- RP month,
- BP month,
- final recovery phenotype,
- phase shift,
- amplitude change,
- whether classification remained stable in sensitivity analyses.

---

## 5.5 Discussion
This section should be both more careful and more strategic.

### Be more careful by:
- reducing mechanism speculation,
- more clearly distinguishing surveillance signal from true burden,
- avoiding overclaiming regarding the interface.

### Be more strategic by:
- emphasizing that the framework offers an operational vocabulary for post-disruption recovery,
- highlighting public health decision-support value,
- clearly defining appropriate and inappropriate use cases.

A specific Discussion paragraph should explicitly state:
- what this framework can answer,
- what it cannot answer.

This would improve credibility.

---

## 5.6 Methods
This section should be strengthened materially.

### Add or clarify:
1. quantitative inclusion criteria for the 24 modeled diseases,
2. detailed weekly-to-monthly reconstruction logic,
3. rationale for log(count + 0.01),
4. rationale for using 1,000 simulated trajectories,
5. RP/BP algorithm pseudocode or flowchart,
6. more formal mathematical definition of phase shift calculation,
7. whether sensitivity analyses were prespecified or post hoc.

### Additional note
Check all software/version information carefully before submission.

---

## 5.7 Figures
### Figure 1
The framework overview is useful, but it should better show the path from analytics to operational output.

### Figure 2
If the full heatmap is visually dense, consider moving the most detailed version to the Supplement and keeping a cleaner summary in the main text.

### Figure 3
This is the core figure. It should be optimized for clarity and take-home value:
- clearly mark RP and BP timing,
- highlight representative diseases,
- ensure the reader can quickly understand phenotype differences.

### Figure 4
A summary panel integrating phase shift and amplitude change would make the seasonality results more interpretable.

### Figure 5
If no user evaluation is added, consider moving the interface figure to the Supplement. If user evaluation is added, retain it in the main text.

---

## 6. Recommendations for the broader project

## 6.1 Move from retrospective framework toward near-real-time surveillance support
The long-term project value will be much higher if the pipeline eventually supports:
- automated data ingestion,
- periodic updating,
- automatic refresh of RP/BP probabilities,
- disease ranking for manual review.

## 6.2 Add an action layer
The system should ultimately do more than visualize metrics. It should support practical categories such as:
- monitor,
- recalibrate threshold,
- review reporting pathway,
- investigate unusual seasonal shift.

## 6.3 Add an explanation layer
Future versions could include contextual covariates or annotations:
- mobility,
- school calendar,
- border policies,
- rainfall,
- service utilization,
- testing intensity.

These do not need to be the main predictive engine, but they could help users interpret why patterns changed.

## 6.4 Consider external validation or transferability testing
A replication in another country or another subnational setting would markedly strengthen the broader scientific value of the framework.

---

## 7. Suggested reviewer-style summary

### Overall assessment
This is a promising and potentially impactful study. Its central contribution is the introduction of an interpretable, portfolio-level surveillance framework that distinguishes apparent monthly normalization from unresolved cumulative deficit after pandemic disruption.

### Main concern
The manuscript would be substantially stronger if it:
- better justified its fit for *npj Digital Medicine*,
- strengthened counterfactual robustness,
- propagated uncertainty into recovery classification,
- clarified the meaning and limits of “recovery,”
- and more clearly demonstrated operational decision-support value.

### Overall recommendation
The project is worth pushing forward, but the next stage should focus on:
1. **uncertainty-aware RP/BP analysis**,  
2. **counterfactual sensitivity analyses**, and  
3. **clearer demonstration of practical surveillance value**.

---

## 8. Action checklist for GPT agent

Please prioritize the following tasks:

### Highest priority
- [ ] Rework the manuscript framing to better match *npj Digital Medicine*.
- [ ] Add uncertainty-aware RP/BP classification using simulated trajectories.
- [ ] Add interruption timing sensitivity analyses.
- [ ] Improve justification of model selection strategy.
- [ ] Make the language around “recovery” more precise and conservative.

### Second priority
- [ ] Add a decision utility / prioritization impact analysis.
- [ ] Jointly summarize RP/BP phenotype with seasonal displacement.
- [ ] Improve transparency of disease inclusion/exclusion criteria.

### Third priority
- [ ] Strengthen the Discussion with a “what the framework can and cannot answer” paragraph.
- [ ] Revise the title and abstract for sharper conceptual novelty.
- [ ] Consider adding a lightweight expert/usability assessment for the interface.

---

## 9. Final positioning advice

The manuscript should not be optimized by simply adding more analyses. The main goal should be to make the existing framework:
- **more robust**,
- **more interpretable**,
- **more operationally meaningful**, and
- **more clearly aligned with digital decision-support in public health practice**.

The strongest version of this paper is not “a study that used several models and built a dashboard,” but rather:

> **an uncertainty-aware digital surveillance decision-support framework that reveals when apparent post-pandemic normalization masks unresolved cumulative disruption and seasonal misalignment across a national infectious disease portfolio.**
