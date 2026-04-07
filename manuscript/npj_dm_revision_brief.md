# Revision Brief for *npj Digital Medicine*

## Project and Manuscript
**Title:** *Counterfactual digital decision support for infectious disease recovery in Thailand*  
**Purpose of this brief:** Provide an objective, high-value, and implementation-oriented revision plan that can be used by an AI editor or co-author team to improve both the manuscript and the underlying project before submission to *npj Digital Medicine*.

---

## 1. Overall editorial judgment

This is a strong and genuinely interesting project with a clear conceptual core. Its main strength is not simply retrospective epidemiologic description, but the conversion of routine surveillance data into an auditable review workflow using a counterfactual baseline, dual recovery milestones, uncertainty propagation, seasonal displacement detection, and a dashboard-style review layer.

However, in its current form, the paper is still stronger as a **digital public-health analytics framework** than as a fully convincing **digital decision-support study** for *npj Digital Medicine*. The key reason is not weak methodology, but an incomplete evidence chain around **decision benefit, end-user validation, implementation realism, and submission-grade reproducibility**.

### Bottom-line positioning
- **Current strength:** original framework, sensible operational problem definition, transparent disease screening, disease-specific forecasting, uncertainty-aware classification, and practical surveillance review logic.
- **Current weakness:** limited proof that the framework improves real surveillance decision-making rather than merely changing retrospective classification.
- **Submission recommendation:** revise substantially before submission.
- **Best framing for revision:** this is not a paper that should be rebuilt from scratch; it is a paper that should be strengthened by adding a better validation layer, end-user evidence, and a cleaner reproducibility package.

---

## 2. Core strategic message for revision

The most important change is this:

> The revised paper should demonstrate not only that the framework produces a different disease-review queue than an incidence-only approach, but also that the framework is more useful, more defensible, and more operationally informative for real surveillance review.

That means the revision should move from **retrospective reclassification** toward **decision-support validation**.

---

## 3. Major revision priorities

### Major Comment 1. Demonstrate decision value, not only queue reclassification

#### Problem
The current decision-utility section shows that the framework changes the disease queue relative to an incidence-only workflow. This is interesting, but it does not yet prove that the new queue is more correct, more useful, or more predictive of future review needs.

The manuscript should avoid language that implicitly treats the framework itself as the gold standard. Phrases such as **"potential false-negative triage errors"** are too strong unless an independent external criterion is available.

#### Why this matters for *npj Digital Medicine*
A digital medicine journal will ask whether the system improves decisions, not only whether it creates a more elaborate classification layer.

#### Required revision
Upgrade the current decision-utility analysis into a **temporal utility validation**.

#### Recommended analysis
At one or more predefined decision freeze points (for example, December 2023 or June 2024):
1. Use only data available up to that freeze point to generate the disease review queue.
2. Evaluate subsequent data over the next 12 to 18 months.
3. Compare whether the framework-based queue versus the incidence-only queue better identifies diseases that later show:
   - persistent cumulative deficit,
   - persistent seasonal misalignment,
   - delayed rejoining of expected trajectory,
   - renewed divergence from counterfactual expectations.

#### Minimum acceptable version
- One freeze point
- One future validation window
- One simple comparative performance summary

#### Manuscript edits
- Replace **"potential false-negative triage errors"** with:
  - **"potential under-triage under an incidence-only review rule"**
- Reframe "decision utility" in parts of the manuscript as:
  - **operational differentiation**
  - **triage reclassification performance**
  - **future review prioritization value**

---

### Major Comment 2. Add real end-user evidence

#### Problem
The manuscript currently includes a dashboard and an author-side task-based heuristic assessment, but not a real end-user evaluation. This makes the work feel closer to a retrospective analytics system than a properly evaluated decision-support tool.

#### Why this matters for *npj Digital Medicine*
For a paper positioned as digital decision support, some form of end-user evidence is highly valuable, even if early-stage and small.

#### Required revision
Add a **small but real end-user study**.

#### Recommended study design
- **Participants:** 5 to 10 public-health or surveillance users
  - disease surveillance staff
  - epidemiologists
  - outbreak response analysts
  - public-health informatics users
- **Tasks:** 4 to 6 predefined tasks
  - identify high-priority diseases
  - identify diseases requiring cumulative follow-up
  - identify diseases requiring seasonal recalibration
  - trace the evidence supporting a queue assignment
  - compare review actions under the baseline dashboard versus the framework dashboard
- **Primary outcomes:**
  - task completion time
  - agreement with expert reference answers
  - perceived interpretability and auditability
- **Secondary outcomes:**
  - user confidence
  - System Usability Scale or a short structured usability score
  - qualitative feedback on workflow fit and confusion points

#### Minimum acceptable version
- 6 to 8 users
- task-based evaluation
- simple quantitative summary plus short qualitative synthesis

#### How to write it
Position it as:
- **early-stage end-user evaluation**
- **task-based decision-support usability assessment**
- **implementation-oriented evaluation**, not large-scale effectiveness proof

#### Important boundary
The current author-side heuristic assessment should be retained only as a **pre-deployment interface coverage check**, not as a substitute for end-user testing.

---

### Major Comment 3. Strengthen falsification and robustness at the system level

#### Problem
The current model-selection and sensitivity analyses are thoughtful, but they are still more about choosing forecasting models than about proving that the framework does not overcall disruption or misclassify stable systems.

#### Required revision
Add **system-level falsification checks**.

#### Recommended analyses
1. **Placebo interruption test**
   - pick one or more pre-pandemic pseudo-interruption dates
   - run the RP/BP and seasonal workflow as if a major disruption had occurred
   - quantify how often the framework would have generated prolonged deficits or recalibration flags in an otherwise normal period

2. **Long-horizon backtest**
   - fit on an earlier pre-pandemic window
   - forecast a later pre-pandemic window at a horizon similar to the pandemic extrapolation problem
   - evaluate whether long-horizon statistical baselines remain stable enough for operational use

3. **Interval coverage calibration**
   - report predictive interval coverage in backtest windows
   - do not rely only on point forecast accuracy

#### Minimum acceptable version
- placebo interruption test
- predictive interval coverage summary

#### Why this matters
These analyses directly answer the question: **Does the system generate false operational alerts when no real major disruption exists?**

---

### Major Comment 4. Better justify thresholds and queue rules

#### Problem
The 95% threshold, 3-month persistence rule, BP definition, and 2-month seasonal shift flag are reasonable operational rules, but they still appear mainly author-defined.

#### Why this matters
Reviewers may ask why these thresholds should matter operationally and whether they are externally grounded.

#### Required revision
Explicitly anchor these thresholds as **operational review rules**, not biological truths or universally optimal cutoffs.

#### Recommended options
**Best option:**
- mini-Delphi or structured expert elicitation with surveillance stakeholders

**Acceptable option:**
- 3 to 5 expert reviewers provide structured feedback on the rule set

**Minimum option:**
- add a supplementary **rule-rationale table** explaining:
  - why 95% was chosen instead of 90% or 100%
  - why 3 months was chosen as the persistence requirement
  - why a 2-month seasonal shift has operational relevance in this surveillance context
  - why BP is defined as cumulative closure rather than partial recovery

#### Manuscript edits
Use phrasing such as:
- **operational rules for surveillance review prioritization**
- **pragmatic thresholds for digital triage**
- **not externally validated biological thresholds**

---

### Major Comment 5. Clarify portfolio coverage and fallback workflow

#### Problem
The disease-selection process is transparent, but readers may still ask how much of the surveillance portfolio is actually covered by the modeling framework and what happens to diseases that cannot be modeled.

#### Required revision
Clarify the framework as a **portfolio system**, not just a framework for a subset of model-friendly series.

#### Recommended additions
1. **Coverage summary**
   - proportion of descriptive diseases covered by the modelled set
   - proportion of total case burden represented by the modelled diseases
   - proportion of high-frequency or operationally important diseases represented

2. **Fallback workflow for excluded diseases**
   - manual review
   - simple descriptive monitoring queue
   - separate low-information anomaly monitoring
   - explicit statement that exclusion from counterfactual modeling does not imply lack of public-health importance

#### Minimum acceptable version
Add a supplementary subsection titled something like:
- **Portfolio coverage and fallback workflow**

---

### Major Comment 6. Upgrade reproducibility to submission-grade standard

#### Problem
The project appears analytically serious, but the submission package may still feel more like a research repository or demonstration dashboard than a fully frozen, manuscript-aligned reproducibility package.

#### Why this matters
A high-profile journal will expect clear code availability, reproducibility logic, and consistency between manuscript claims and repository structure.

#### Required revision
Prepare a **submission freeze**.

#### Required actions
- freeze a specific code version with a tag or release
- archive the frozen release with a DOI (for example via Zenodo)
- include environment lock files
  - `renv.lock`
  - `requirements.txt`
  - session information
- make the analytical workflow explicit:
  - raw data
  - preprocessing
  - monthly reconstruction
  - model selection
  - simulation
  - RP/BP classification
  - seasonal analysis
  - figure and table generation
  - dashboard output generation
- ensure all main-text figures and tables can be reproduced from the frozen package
- align README language with the manuscript
- clearly state which outputs are precomputed for interface responsiveness and which are fully reproducible offline

#### Minimum acceptable version
- frozen release
- DOI-backed archive
- locked environment
- manuscript-repository wording consistency

---

### Major Comment 7. Add an operational governance layer

#### Problem
The manuscript has a review layer and an implementation narrative, but it still lacks a sufficiently explicit governance description for real surveillance use.

#### Required revision
Add an **operational governance** subsection or supplement.

#### Recommended contents
- data refresh cadence
- data freeze rules for monthly review
- data completeness and anomaly checks
- handling of reporting delays or case-definition changes
- conditions for suspending automatic queue assignment
- override rules and audit trail
- model retraining triggers
- situations in which system outputs should not be trusted

#### Why this matters
This will make the project feel more like a deployable decision-support workflow and less like a one-off retrospective analysis.

---

### Major Comment 8. Tighten claim framing and avoid overstatement

#### Problem
The manuscript is generally cautious, especially in distinguishing surveillance-defined recovery from true infection burden recovery. That is a major strength. However, a few expressions still sound too strong.

#### Required revision
Systematically reduce overclaiming.

#### Recommended edits
- avoid language implying that the framework provides truth rather than structured operational interpretation
- avoid causal language about public-health impact unless directly tested
- avoid treating reclassification as equivalent to decision improvement

#### Stronger manuscript framing
Use language such as:
- **auditable surveillance prioritization**
- **implementation readiness**
- **operational review support**
- **surveillance-defined milestones**
- **portfolio-level digital review workflow**

#### Optional title revision
A more conservative and journal-aligned title could be:

**A counterfactual-baseline decision-support framework for post-disruption infectious disease surveillance review in Thailand**

---

## 4. Minor revision priorities

### Minor Comment 1. Clarify count, incidence, and transformed scale
Explain clearly and consistently:
- what quantity is modeled
- what quantity is visualized
- what quantity determines RP/BP
- what quantity is used for seasonal analysis

The distinction between counts, incidence rates, and square-root transformed counts should not require close inference from multiple sections.

### Minor Comment 2. Clarify the role of uncertainty
State more explicitly that the uncertainty analysis propagates forecast uncertainty conditional on the selected model family and does not fully propagate model-class uncertainty.

### Minor Comment 3. Clarify why nested CV and external validation were not the primary design
The current rationale is defensible, but it should be made more explicit that the inferential target is a no-disruption statistical baseline rather than prediction of observed post-2020 incidence.

### Minor Comment 4. Strengthen the abstract boundary statement
Add one sentence near the end of the abstract clarifying that current evidence supports auditable surveillance prioritization and implementation readiness more directly than real-world improvement in downstream public-health outcomes.

### Minor Comment 5. Keep transferability claims modest
Retain Thailand as a national case study and treat transportability as conditional on reporting continuity, definition stability, and local recalibration of rules.

---

## 5. Recommended additions to supplementary material

Add the following supplementary components:

1. **Temporal utility validation**
   - freeze-point analysis
   - future follow-up comparison

2. **Falsification and calibration**
   - placebo interruption test
   - predictive interval coverage
   - long-horizon backtest if feasible

3. **Rule-rationale appendix**
   - explanation of operational thresholds
   - expert feedback summary if available

4. **Portfolio coverage and fallback workflow**
   - what is covered
   - what is not covered
   - how excluded diseases are handled operationally

5. **Operational governance appendix**
   - update cadence
   - override and retraining rules
   - data quality and failure modes

6. **End-user evaluation appendix**
   - participant roles
   - tasks
   - metrics
   - usability findings
   - structured qualitative themes

---

## 6. Specific guidance on the end-user study question

### Can a small end-user study use users from other countries or data with different temporal frequency?

### Short answer
Yes, **but only for some purposes**.

### What is acceptable
Using users from other countries and/or data with different frequency can provide useful evidence for:
- usability
- interpretability
- auditability
- workflow clarity
- task completion under a digital decision-support interface
- early evidence of transportability

### What it cannot fully replace
It should **not** be presented as a full substitute for Thailand-specific validation if the manuscript’s core claim remains that this framework supports review decisions in the Thailand national surveillance context.

### Recommended interpretation
Use external users or external datasets as:
- **supportive usability evidence**
- **cross-setting formative evaluation**
- **transportability-oriented demonstration**

Do **not** describe such a study as complete external validation unless the full analytical framework is rebuilt and assessed in that setting.

### Best-case design
- Thailand-relevant users and Thailand disease outputs as the main study
- a small number of external users or settings as supplementary evidence

### If Thailand users are not feasible
A practical alternative is:
- conduct the usability study with surveillance practitioners from another country
- clearly describe it as an **early-stage usability evaluation** rather than a Thailand-specific effectiveness test
- state explicitly that this supports interface generalizability more than local rule validation

### Different temporal frequency: weekly versus monthly
Because the current framework is operationalized primarily at the **monthly** level, a direct weekly-data usability study should be handled carefully.

#### Best practice
If the external dataset is weekly:
- aggregate or harmonize it to monthly review outputs
- preserve the same review logic users see in the manuscript
- evaluate the same RP/BP and seasonal decision layer

#### Less desirable approach
If a weekly interface is evaluated without harmonizing to the monthly workflow, the study may look like a different system rather than a usability test of the same one.

### Recommended wording if external users or external data are used
Use phrasing such as:
- **supplementary cross-setting usability assessment**
- **formative evaluation of workflow transferability**
- **early-stage end-user testing in an external surveillance context**

Avoid claims such as:
- **external validation of the Thailand decision thresholds**
- **proof of effectiveness across surveillance systems**

---

## 7. Recommended end-user study design template

### Participants
6 to 8 users is sufficient for an early-stage study.

Suggested composition:
- 3 to 4 Thailand-relevant surveillance or public-health users if feasible
- 3 to 4 external surveillance or epidemiology users as supplementary participants if useful

### Tasks
Use four core tasks:
1. Identify diseases that should not be considered routine based only on current incidence.
2. Identify diseases requiring cumulative-deficit follow-up.
3. Identify diseases requiring seasonal recalibration.
4. Trace the evidence supporting a queue assignment.

Optional fifth task:
5. Compare review conclusions under a basic incidence display versus the framework dashboard.

### Primary outcomes
- task completion time
- agreement with expert answer key
- interpretability score
- auditability score

### Secondary outcomes
- user confidence
- SUS or a brief structured usability scale
- short interview or open-text feedback

### How to report it
Describe this as:

**An early-stage task-based end-user evaluation of a surveillance decision-support interface**

If external settings are included, add:

**with supplementary cross-setting usability assessment**

---

## 8. Project-level revision tasks beyond the manuscript

The project itself should also be improved, not just the wording of the paper.

### Required project tasks
1. Freeze the analytical pipeline and repository for submission.
2. Align repository documentation with manuscript claims.
3. Add an explicit reproducibility workflow.
4. Add a governance note for dashboard operation.
5. Add a reviewer-facing README explaining:
   - what is precomputed
   - what can be reproduced end-to-end
   - which analyses correspond to which manuscript sections
6. Add or plan an end-user study module.
7. Add falsification and temporal utility validation modules if not already implemented.

---

## 9. Suggested wording changes for the manuscript

### Replace overstrong phrasing
Instead of:
- **false-negative triage errors**

Use:
- **potential under-triage under an incidence-only review rule**

Instead of:
- **decision utility** in contexts where benefit is not externally validated

Use:
- **operational differentiation**
- **review-prioritization value**
- **triage reclassification performance**

Instead of:
- implying improved public-health outcomes

Use:
- **supports auditable surveillance review**
- **improves traceability of review logic**
- **structures disease-level prioritization for surveillance teams**

### Keep and strengthen these good framings
- **surveillance-defined operational milestones**
- **not a measure of true infection burden recovery**
- **not proof of biological compensation**
- **implementation layer**
- **portfolio-level digital review workflow**

---

## 10. Suggested structure for the revised manuscript

A stronger revision would keep the main conceptual architecture but rebalance the paper toward validation and implementation.

### Recommended high-level structure
1. **Introduction**
   - emphasize the gap between display dashboards and decision layers
   - define RP and BP as surveillance-defined operational milestones
   - state that the goal is auditable review prioritization

2. **Methods**
   - keep forecasting and RP/BP methods
   - add temporal utility validation methods
   - add falsification methods
   - add end-user study methods
   - add governance or deployment methods if feasible

3. **Results**
   - keep the core phenotype findings
   - strengthen future-oriented utility results
   - report user study findings
   - report placebo and calibration results
   - retain seasonal and uncertainty findings as supporting logic

4. **Discussion**
   - emphasize decision-support contribution
   - state clearly what is and is not validated
   - discuss transferability carefully
   - discuss implementation next steps without overclaiming

---

## 11. Priority order if time is limited

If only a small number of revisions can be completed before submission, prioritize them in this order:

1. **Temporal utility validation**
2. **Real end-user study**
3. **Placebo interruption / falsification analyses**
4. **Submission-grade reproducibility freeze**
5. **Rule-rationale and governance documentation**

These five steps will create the largest increase in credibility for *npj Digital Medicine*.

---

## 12. Instructions for an AI editor or co-author team

Use this brief to revise the manuscript and project according to the following principles:

1. Preserve the core conceptual contribution: a counterfactual-baseline, uncertainty-aware surveillance review framework.
2. Do not overstate causal impact, clinical benefit, or public-health outcome improvement unless directly demonstrated.
3. Treat RP and BP as operational milestones rather than biological truths.
4. Strengthen the evidence chain around decision support, end-user value, and implementation readiness.
5. Improve reproducibility, auditability, and repository-manuscript alignment.
6. Keep Thailand as the primary case study unless a true external implementation is added.
7. If external users or external datasets are used, describe them as supportive usability or transportability evidence unless a full external validation is performed.
8. Rewrite overstated phrases conservatively and consistently across the abstract, Results, and Discussion.
9. Add missing validation layers before polishing language.
10. When editing, prefer specific operational claims over broad aspirational claims.

---

## 13. Final assessment

This is a promising paper with clear originality and strong practical potential. Its main limitation is not conceptual weakness, but that the current evidence most strongly supports **retrospective digital public-health analytics with implementation potential**, rather than a fully validated **digital decision-support system**. The revision should therefore focus on strengthening validation, usability evidence, governance, and reproducibility rather than changing the core scientific idea.
