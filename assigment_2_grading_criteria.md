
## Introduction

This grading checklist is used to assess the second Medical Statistics assignment. The grade is based on three main parts (ANCOVA, Logistic Regression & DAG, Prediction Model) and a general section on data handling and reproducibility.

This checklist is aligned with the published rubric but provides more detailed guidance for grading. Use this document together with the **grading form** (`_Assignment_2_grading_form.qmd`), which contains the detailed checklist items to be scored.

For each item in the grading form, add a score after the colon for that item of either +, +/- (see comments), or - (see comments). A score of "+" should be assigned if there are no issues for that item; a score of "+/- (see comments)" if there are minor issues; a score of "- (see comments)" if there are major issues or the item is missing. In case a score other than + is being assigned, a comment regarding this item should be added to the comments section at the end of the grading form. The comments section can be omitted for students scoring an overall grade of 10.

Use the scoring tables under each criterion below to decide the overall level for that criterion.

## Criterion 1: Part A – ANCOVA (30%)

Analysis of sex differences in resting blood pressure while adjusting for age.

| Level | Score | Description |
|-------|-------|-------------|
| Insufficient | 3 | One or more required components are missing, or the additive/interaction model is wrong, or the diagnostics/tests are misinterpreted. |
| Sufficient | 6 | Some required components are present, but there are multiple minor issues or one major issue (e.g., missing a required plot/test or unclear/incorrect conclusion). |
| Good | 8 | All required components are present with at most one minor issue (e.g., small reporting omission or slightly unclear interpretation). |
| Excellent | 10 | All required components are present and correct; reporting is clear and complete (estimate, 95% CI, p-value; correct use/interpretation of plots and Type III interaction test). |

## Criterion 2: Part B – Logistic Regression & DAG (30%)

Investigation of the causal effect of cholesterol on heart disease using a DAG for adjustment.

| Level | Score | Description |
|-------|-------|-------------|
| Insufficient | 3 | One or more required components are missing, or the adjustment set is wrong, or ORs/models are wrong/misinterpreted. |
| Sufficient | 6 | Some required components are present, but there are multiple minor issues or one major issue (e.g., unclear DAG logic, weak/incorrect comparison). |
| Good | 8 | All required components are present with at most one minor issue (e.g., slight weakness in explanation or a small reporting omission). |
| Excellent | 10 | All required components are present and correct; DAG logic is clear; OR and aOR are correctly reported/interpreted with a clear confounding comparison. |

## Criterion 3: Part C – Prediction Model (30%)

Building a model for maximum heart rate using manual backward elimination.

| Level | Score | Description |
|-------|-------|-------------|
| Insufficient | 3 | One or more required components are missing, or backward elimination is not correctly performed (wrong p-values, wrong threshold, or wrong final model). |
| Sufficient | 6 | Some required components are present, but there are multiple minor issues or one major issue (e.g., incomplete documentation or inconsistent use of Type III ANOVA). |
| Good | 8 | All required components are present with at most one minor issue (e.g., a small reporting omission or slightly unclear step table). |
| Excellent | 10 | All required components are present and correct; the removal process is transparent and consistent with Type III p-values and the 0.10 threshold; final model reporting is complete. |

## Criterion 4: Data handling and reproducibility (10%)

Reproducibility and correctness of submitted analysis files.

| Level | Score | Description |
|-------|-------|-------------|
| Insufficient | 3 | One or more required components are missing, or results cannot be verified from the submitted script/output, or the dataset creation is wrong/undocumented (R users). |
| Sufficient | 6 | Some required components are present, but there are multiple minor issues or one major issue (e.g., omissions that make verification difficult). |
| Good | 8 | All required components are present with at most one minor issue (e.g., slight structure/clarity issues). |
| Excellent | 10 | All required components are present and correct; work is clearly structured and fully reproducible; dataset creation (R users) is correctly documented. |

## Quick Reference: Common Issues by Score Level

### Minor Issues (one = Good; multiple = Sufficient)
- Small reporting omission (e.g., missing CI or df)
- Slightly unclear wording that doesn't change conclusion
- Minor formatting inconsistency
- Diagnostic plot included but not explicitly interpreted
- Comparison of OR vs aOR mentioned but not elaborated

### Major Issues (one = Sufficient; multiple = Insufficient)
- Wrong model specified (e.g., missing covariate in ANCOVA, wrong adjustment set in DAG)
- Diagnostics/assumptions not checked at all
- Interaction test missing or misinterpreted
- Backward elimination not performed correctly (wrong criterion or threshold)
- Misinterpretation that changes the conclusion
- Missing or incorrect confidence intervals
- Dataset not correctly created (R users)

