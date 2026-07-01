# Manuscript Blueprint

> 故事线锁定版本。写 Discussion 之前，一切以此为锚。
> 如果某个数字与这里不一致，先改这里（讨论），再改正文。

---

## Title (tentative)

**Metabolic dysfunction–associated steatotic liver disease and stroke risk:**
**A systematic review and meta-analysis exploring AF-associated and fibrosis-related pathways**

---

## Central Message

MASLD is associated with a 37% increased stroke risk (HR 1.37, 95% CI 1.28–1.46, k=21, n≈29.8M), with an AF-associated pathway explaining 16.4% of between-study heterogeneity (p=0.040) and fibrosis severity/dynamics representing potential contributors to residual risk variation — together suggesting that stroke risk in MASLD is not monolithic but modulated by hepatic and arrhythmic pathways.

---

## Three Key Findings

### Finding 1 — Main Effect

**MASLD is independently associated with increased stroke risk.**

| Metric | Value |
|--------|-------|
| Pooled HR (REML) | **1.37** (95% CI 1.28–1.46) |
| Studies (k) | 21 |
| Total N | ≈29.8 million |
| I² | 98.6% (τ²=0.0221) |
| Sensitivity (excl. NHIS overlaps) | Direction unchanged — see Methods footnote |
| AF subgroup (random effects) | Directionally consistent, but k=2 — see Constitution Rule 3 |
| Fibrosis subgroup (random effects) | Not assessed (k=14): HR 1.42 | Assessed (k=7): HR 1.26 | p=0.078 — trend only |

**NHIS overlap handling (Methods footnote + Discussion one-liner):**

> Several studies were derived from the Korean National Health Insurance Service database and contained partially overlapping source populations. To avoid potential double-counting, only the largest and most comprehensive cohort (Jang et al., 2026) was retained in the primary quantitative synthesis. Park et al. (2022) was included descriptively to evaluate fibrosis dynamics but was not entered into pooled analyses.

**Supporting Figures:** Figure 2 (main forest), Figure S1–S8 (sensitivity/subgroup forests)

**One-liner for Discussion:** "Across 21 studies including approximately 29.8 million participants, MASLD was associated with a 37% increased risk of stroke (HR 1.37, 95% CI 1.28–1.46), with substantial between-study heterogeneity (I²=98.6%)."

---

### Finding 2 — AF-Associated Pathway

**AF adjustment explains part of between-study heterogeneity, consistent with an AF-associated pathway linking MASLD and stroke risk.**

| Metric | Value |
|--------|-------|
| Meta-regression R² | **16.4%** |
| Meta-regression p | **0.040** |
| β (AF adjustment) | −0.214 |
| Predicted HR (without AF adjustment) | 1.40 (95% CI 1.31–1.49) |
| Predicted HR (with AF adjustment) | 1.13 (95% CI 0.93–1.37) |
| Attenuation | (1.40 − 1.13) / (1.40 − 1) = **67.7%** |
| Patient-level interaction (Jang 2026) | Without AF: HR **1.10**; With AF: HR **1.03** (0.95–1.12) |
| Within-study AF adjustment (Jang 2026) | Before AF: HR 1.14 → After AF: HR 1.10 (Δ=28.6%) |
| Bundle adjustment (Lee 2021) | Before bundle: 1.55 → After bundle: 1.43 (Δ=21.8%) |
| Supporting pathway evidence (Ohno 2023) | MASLD → incident AF: HR **1.51** (1.46–1.57), n=8.2M |

**Evidence hierarchy & Results priority (Table S1):**

| Priority | Level | Evidence | Star |
|----------|-------|----------|------|
| 1st | I | Between-study meta-regression (R²=16.4%, p=0.040) | ★★★★★ |
| 2nd | II | Patient-level AF subgroup interaction (Jang 2026: HR 1.10 → 1.03) | ★★★★☆ |
| 3rd | IV | Bundle adjustment (Lee 2021: 1.55 → 1.43) | ★★★☆☆ |
| Last | III | Within-study AF covariate adjustment (Jang 2026: 1.14 → 1.10) | ★★☆☆☆ |
| — | S1 | MASLD → AF pathway link (Ohno 2023: 1.51) | Supporting |

**Results paragraph structure:**
1. Meta-regression first: "AF adjustment explained 16.4% of between-study heterogeneity (p=0.040)"
2. Jang interaction second: "Patient-level interaction analysis (Jang 2026)..."
3. Subgroup closing line: "Subgroup analyses yielded directionally consistent findings, although the number of studies was limited."

AF subgroup (k=2) must NOT be elevated to a core result. The meta-regression is the primary between-study evidence.

**Supporting Figures:** Figure 6 (AF pathway 3-panel), Table S1 (evidence hierarchy)

**One-liner for Discussion:** "Meta-regression identified AF adjustment as a significant moderator of between-study heterogeneity (R²=16.4%, p=0.040), with predicted stroke risk attenuating from HR 1.40 to 1.13 after AF adjustment — a pattern corroborated by patient-level interaction data (Jang 2026: HR 1.10 without baseline AF vs 1.03 with AF) and supported by large-scale evidence linking MASLD to incident AF (Ohno 2023: HR 1.51, n=8.2M)."

**Terminology lock:** "AF-associated pathway" (NOT "AF-mediated" — no mediation analysis was performed). Use "explains part of" (NOT "partially mediates"). Use "effect modification" for the Jang 2026 patient-level interaction.

---

### Finding 3 — Fibrosis Severity & Dynamics

**Higher fibrosis severity is associated with greater stroke risk, and fibrosis regression may be protective.**

| Metric | Value |
|--------|-------|
| **Individual study estimates** | |
| Histology (Simon 2022) — Low / Intermediate / Advanced | HR 1.50 / 1.96 / 1.91 |
| NFS (Chen 2023) — Low / Intermediate / Advanced | HR 1.29 / 1.38 / **2.08** |
| AAR (Chung 2022) — Ref / Intermediate / High | HR 1.00 / 1.13 / 1.29 |
| BARD (Jang 2026) — Low/Ref / Advanced | HR 1.03 / 1.11 |
| **Pooled estimates (exploratory — Supplementary only)** | |
| Non-Advanced/Low fibrosis (4 studies) | HR **1.21** (Supplementary only) |
| Advanced/High fibrosis (4 studies) | HR **1.58** (Supplementary only) |
| ⚠️ **Do NOT enter main Results.** Mixed tools (Histology + NFS + BARD), categories not harmonized. "Interpret with caution" mandatory. |
| → Rationale: Figure 7 (individual studies + Park 2022 dynamics) is already strong. Pooled estimate is the weakest link — reviewers will attack here if elevated to main text. |
| **Fibrosis dynamics (Park 2022)** | |
| Fibrosis regression | HR **0.58** (protective) |
| Persistent fibrosis | HR **1.31** |

**Fibrosis-assessed subgroup (random effects):**

| Subgroup | k | HR (95% CI) |
|----------|---|-------------|
| Fibrosis assessed | 7 | **1.26** (1.15–1.39) |
| Fibrosis not assessed | 14 | **1.42** (1.30–1.55) |
| p-between | — | **0.078** |

**Mandatory framing (verbatim):**

> Although subgroup differences did not reach conventional statistical significance (p = 0.078), a consistent increase in risk estimates was observed across studies reporting more advanced fibrosis.

Do NOT write: "Fibrosis severity significantly modified risk." Do NOT write: "Advanced fibrosis explained heterogeneity." The correct posture is: acknowledge non-significance + preserve biological signal.

**Supporting Figures:** Figure 7 (fibrosis severity + dynamics), Figure S9 (fibrosis subgroup forest)

**One-liner for Discussion:** "Among four studies reporting fibrosis-stratified stroke risk, an exploratory pooled analysis suggested a severity gradient (non-advanced: HR 1.21; advanced: HR 1.58), with the strongest signal from NFS-based stratification (Chen 2023: low HR 1.29, intermediate HR 1.38, advanced HR 2.08). Longitudinal data from Park et al. (2022) further demonstrated that fibrosis regression was associated with substantially reduced stroke risk (HR 0.58) compared to persistent fibrosis (HR 1.31), suggesting that hepatic fibrosis may represent a dynamic, potentially modifiable risk modifier rather than a static confounder."

**Caveat:** Individual fibrosis-stratified estimates each come from single studies with different tools (histology, NFS, AAR, BARD) — heterogeneity in measurement limits formal pooling. The exploratory pooled estimates (1.21 vs 1.58) should be presented in supplementary material with appropriate caution.

---

## Narrative Arc (Discussion Flow)

```
Finding 1 (Main Effect)
    "MASLD → stroke: the association is real and consistent"
    │
    ├─→ Finding 2 (AF Pathway)
    │   "But part of this risk travels through AF"
    │   Evidence: meta-regression + patient-level interaction + pathway link
    │   Terminology: AF-associated pathway, NOT AF-mediated
    │
    └─→ Finding 3 (Fibrosis)
        "And fibrosis severity/dynamics modulate what remains"
        Evidence: severity gradient + regression data
        Implication: fibrosis as a dynamic risk modifier, not a static confounder
```

---

## Terminology Discipline

| Use | Don't Use | Reason |
|-----|-----------|--------|
| AF-associated pathway | AF-mediated / AF mediator | No mediation analysis performed |
| Explains part of heterogeneity | Partially mediates | Observational meta-regression, not causal decomposition |
| Effect modification | AF subgroup effect | Patient-level interaction term (Jang 2026) |
| Potential AF-associated pathway | Likely AF-mediated | Hedge appropriately for observational evidence |
| Attenuation after AF adjustment | AF mediation effect | Attenuation ≠ mediation |
| Between-study AF moderation | AF explains heterogeneity | Moderation is the correct meta-regression term |

---

## Figure-to-Story Mapping

| Figure | Story Function | Key Number |
|--------|---------------|------------|
| Figure 2 | Main finding: MASLD → stroke | HR **1.37** (1.28–1.46) |
| Figure 6 | AF-associated pathway (3-panel) | R²=**16.4%**, p=**0.040** |
| Figure 7 | Fibrosis severity + dynamics | NFS-high HR **2.08**; regression HR **0.58** |
| Table S1 | AF evidence hierarchy (5-level) | Attenuation: Level I 67.7%, Level II 70.0% |
| Graphical Abstract | Visual synthesis of three findings | — |
| Figure S9 | Fibrosis subgroup forest | Assessed HR 1.26 vs Not assessed HR 1.42, p=0.078 |

---

## All Decisions — Resolved

1. [x] Fibrosis subgroup: Assessed k=7, HR 1.26; Not assessed k=14, HR 1.42; p=0.078 → **trend only**
2. [x] Main HR estimator: REML, τ²=0.0221, HR 1.3657 → 1.37
3. [x] Lee 2021: **1.55 is canonical** (AF Attenuation Matrix). 1.52 in AF Mediation Data is a different extraction.
4. [x] NFS-high 2.08: **single study** (Chen 2023). Pooled 1.58 is exploratory.
5. [x] **NHIS overlap → Methods footnote + Discussion one sentence.** Do not enter Results main text. Verbatim language provided above.
6. [x] **Fibrosis p=0.078 → "Although subgroup differences did not reach conventional statistical significance (p=0.078), a consistent increase in risk estimates was observed across studies reporting more advanced fibrosis."** Do NOT claim significance. Do NOT claim "explained heterogeneity."
7. [x] **AF subgroup k=2 → last sentence of AF Results paragraph.** "Subgroup analyses yielded directionally consistent findings, although the number of studies was limited." Do not elevate.
8. [x] **Exploratory pooled fibrosis → Supplementary only.** "Interpret with caution because fibrosis categories were not harmonized across studies." Figure 7 is already strong enough for the main text.

---

## Manuscript Constitution

> 以下五条为 Discussion 的宪法。任何文字不得违反。

### Article 1 — Main Finding

**MASLD was associated with a 37% increased risk of stroke.**
(HR 1.37, 95% CI 1.28–1.46, k=21, n≈29.8M)

### Article 2 — Innovation 1: AF-Associated Pathway

Supported by:
- AF meta-regression (R²=16.4%, p=0.040) ★★★★★
- Jang 2026 patient-level interaction (HR 1.10 → 1.03) ★★★★☆
- AF attenuation evidence (Lee 2021, within-study adjustment) ★★★☆☆
- Incident AF evidence (Ohno 2023: HR 1.51) — supporting

**Mandatory interpretation:** "AF-associated pathway"
**Forbidden:** "AF mediation confirmed"

### Article 3 — Innovation 2: Fibrosis-Related Pathway

Supported by:
- Histology (Simon 2022)
- NFS (Chen 2023)
- BARD (Jang 2026)
- Fibrosis dynamics (Park 2022: regression HR 0.58 vs persistent HR 1.31)

**Mandatory interpretation:** "Fibrosis severity and regression provide prognostic information beyond AF."
**Forbidden:** "Fibrosis → AF → Stroke" causal chain

### Article 4 — Forbidden Claims

| Do NOT write | Use instead |
|-------------|-------------|
| AF mediation proven / confirmed | AF-associated pathway |
| Liver–heart–brain axis confirmed | Potential mechanism |
| Fibrosis → AF → Stroke (causal chain) | Fibrosis severity provides prognostic information |
| Fibrosis subgroup significant | Trend toward higher risk (p=0.078, not significant) |
| AF adjustment explains the majority of risk | AF adjustment explains part of between-study heterogeneity (R²=16.4%) |
| Partially mediates / mediated by AF | Explains part of / associated with |
| Pooled fibrosis HR 1.58 (in main text) | Individual study estimates (Figure 7); pooled in Supplementary |
| AF subgroup confirms pathway | Subgroup analyses yielded directionally consistent findings, although the number of studies was limited |

### Article 5 — Amendment Process

If any number or claim must change during writing:
1. Amend this Constitution first
2. Then propagate to all affected sections (Results, Discussion, Figures, Tables)
3. Never change a number in Discussion without updating the source here

---

*Last updated: 2026-06-26*
*Constitution ratified. All Discussion text must comply.*
