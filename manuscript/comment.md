

# **Reviewer Report (for npj Digital Medicine submission)**

## **Manuscript Title**

*A digital surveillance framework reveals decoupled and heterogeneous recovery of infectious diseases in Thailand after COVID-19 disruption*

---

## **Overall Assessment**

This manuscript presents a national-scale digital surveillance framework integrating counterfactual forecasting, dual recovery metrics (recovery period, RP; balance period, BP), seasonal displacement detection, and an interactive dashboard to assess post-pandemic infectious disease recovery in Thailand.

The study addresses an important and timely problem: conventional surveillance dashboards emphasize contemporaneous incidence but fail to capture cumulative recovery dynamics after large-scale disruptions. The proposed distinction between **monthly normalization (RP)** and **cumulative deficit closure (BP)** is conceptually meaningful and has potential operational relevance.

Compared with earlier versions, the manuscript has improved in methodological transparency, system framing, and alignment with digital surveillance applications. The integration of an interpretable analytical pipeline with an accessible dashboard is a strength.

However, in its current form, the manuscript still sits at the boundary between a **retrospective observational surveillance study** and a **digital decision-support framework**, and does not yet fully meet the expectations of *npj Digital Medicine*, which prioritizes demonstrable advances in digital health systems, decision-making, or clinical/public health practice.

The manuscript would be substantially strengthened by addressing several key issues related to:

* **demonstration of practical decision-support value**,
* **propagation of uncertainty in the core classification framework**, and
* **robustness of counterfactual assumptions**.

---

## **Major Comments**

### **1. Limited evidence of digital decision-support impact**

While the manuscript positions the framework as a *digital surveillance tool* and includes an interactive dashboard, the contribution remains largely descriptive and retrospective.

At present, the digital component is primarily:

* visualization of precomputed analytical outputs,
* modular dashboard implementation,
* low-latency deployment design.

However, there is limited evidence that the system:

* changes surveillance prioritization,
* improves decision-making,
* or leads to different public health actions compared with conventional dashboards.

Given the journal’s scope, this is a critical gap.

#### **Recommendation**

The authors should incorporate a **decision utility or prioritization analysis**, for example:

* compare disease classification based on conventional incidence-only surveillance vs. RP/BP framework,
* identify diseases that would be misclassified as “recovered” under standard dashboards,
* demonstrate how the framework changes triage or review priorities.

Even a retrospective simulation of decision impact would significantly strengthen the manuscript’s relevance to digital medicine.

---

### **2. Lack of uncertainty propagation in RP/BP classification**

The study generates 1000 simulated counterfactual trajectories per disease, but the RP/BP classification relies on **median forecasts and deterministic thresholds**.

This creates a key methodological limitation:

* classification uncertainty is not quantified,
* borderline diseases may be unstable,
* phenotype counts (e.g., 13 vs. 7 diseases) may be sensitive to stochastic variation.

Although acknowledged as a limitation, this affects the core results.

#### **Recommendation**

The authors should incorporate **uncertainty-aware classification**, including:

* probability of achieving RP and BP,
* uncertainty intervals for RP and BP timing,
* classification stability metrics,
* identification of “uncertain” or borderline diseases.

This is likely the single most impactful methodological improvement.

---

### **3. Counterfactual assumptions require further robustness testing**

The framework depends critically on the validity of counterfactual forecasts.

While model selection is carefully described, one key assumption remains insufficiently tested:

#### **Common interruption point (January 2020)**

Although justified as a portfolio-level anchor, this choice simplifies heterogeneous disruption timing across diseases.

#### **Recommendation**

The authors should perform **interruption timing sensitivity analyses**, e.g.:

* January 2020,
* March 2020,
* April 2020.

The robustness of RP/BP classifications to these alternative specifications should be reported.

---

### **4. RP/BP definitions remain somewhat threshold-driven**

The RP/BP framework is interpretable and useful, but still relies on fixed thresholds:

* 95% of expected incidence,
* 3 consecutive months,
* cumulative non-decreasing condition.

Although sensitivity analyses are included, the framework may still appear somewhat arbitrary.

#### **Recommendation**

* Expand threshold sensitivity analyses (e.g., 90%, 95%, 100%; different persistence windows),
* Compare with at least one alternative recovery definition,
* Emphasize more clearly that RP/BP are **operational metrics**, not universal epidemiological constants.

---

### **5. Seasonal displacement module not fully integrated with core framework**

The seasonal analysis is valuable and well executed, but currently appears as a parallel component rather than a fully integrated part of the recovery framework.

#### **Recommendation**

* Provide a joint synthesis linking:

  * RP/BP phenotype,
  * phase shift,
  * amplitude change.
* Highlight diseases that:

  * appear recovered (RP achieved),
  * but remain seasonally misaligned,
  * and therefore require recalibration of early warning systems.

This would strengthen the operational coherence of the framework.

---

## **Minor Comments**

### **1. Title does not conform to journal requirements**

The journal specifies titles:

* up to 15 words,
* free of punctuation.

The current title exceeds this limit and includes punctuation (“COVID-19”).

#### **Recommendation**

Revise to meet journal formatting requirements.

---

### **2. Abstract likely exceeds word limit**

The journal requires abstracts of **≤150 words**.

#### **Recommendation**

Condense the abstract while preserving:

* problem statement,
* method,
* key findings,
* operational implication.

---

### **3. Terminology around “recovery” should be tightened**

Some expressions (e.g., “genuine cumulative burden restoration”) may be interpreted as reflecting true epidemiological recovery rather than surveillance-defined metrics.

#### **Recommendation**

Use more precise language:

* “surveillance-defined recovery”,
* “reported incidence normalization”,
* “cumulative deficit closure”.

---

### **4. Reference consistency**

Ensure all in-text citations correspond to complete reference entries. Some citation numbering appears inconsistent.

---

### **5. Clarify disease inclusion criteria**

The selection of 24 diseases for modeling is described qualitatively (“sufficient signal”).

#### **Recommendation**

Provide:

* explicit quantitative criteria,
* a flow diagram of inclusion/exclusion,
* list of excluded diseases.

---

## **Recommendation**

**Major revision**

This manuscript addresses an important problem and proposes a promising framework with potential relevance for digital surveillance systems. However, to meet the standards of *npj Digital Medicine*, the authors should:

1. Demonstrate **practical decision-support value** of the framework,
2. Incorporate **uncertainty-aware RP/BP classification**,
3. Strengthen **counterfactual robustness analyses**, and
4. Improve integration of analytical components into a coherent operational narrative.

With these improvements, the manuscript could make a meaningful contribution to digital public health surveillance and decision-support systems.
