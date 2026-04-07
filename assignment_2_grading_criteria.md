
## Introduction

This grading checklist is used to assess the second Medical Statistics assignment. The grade is based on three main parts (ANCOVA, Logistic Regression & DAG, Prediction Model) and a general section on data handling and reproducibility.

This checklist is aligned with the published rubric but provides more detailed guidance for grading. Use this document together with the **grading form** (`_Assignment_2_grading_form.qmd`), which contains the detailed checklist items to be scored.

For each item in the grading form, add a score after the colon for that item of either +, +/- (see comments), or - (see comments). A score of "+" should be assigned if there are no issues for that item; a score of "+/- (see comments)" if there are minor issues; a score of "- (see comments)" if there are major issues or the item is missing. In case a score other than + is being assigned, a comment regarding this item should be added to the comments section at the end of the grading form. The comments section can be omitted for students scoring an overall grade of 10.

Completed grading forms for each student are saved in the `Assignment part 2 submissions/Evaluations/` folder.

### Style note for comments

When writing comments in the grading form, avoid em dashes (\u2014), en dashes (\u2013), and arrows (\u2192). Use commas or restructure sentences instead. Keep comments factual and concise.

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

### Grading note: Testing submitted R scripts

For R users, the submitted R script should be tested by running it locally. The only change permitted is adjusting the file path in the `read_sav()` call to point to the local copy of the dataset (e.g., `datasets/heart_disease_cleveland.sav`). No other changes should be made. If the script runs without errors and its output matches the reported results, this confirms reproducibility. If the script fails or produces different results, this should be noted in the comments and reflected in the Criterion 4 score.

| Level | Score | Description |
|-------|-------|-------------|
| Insufficient | 3 | One or more required components are missing, or results cannot be verified from the submitted script/output, or the dataset creation is wrong/undocumented (R users). |
| Sufficient | 6 | Some required components are present, but there are multiple minor issues or one major issue (e.g., omissions that make verification difficult). |
| Good | 8 | All required components are present with at most one minor issue (e.g., slight structure/clarity issues). |
| Excellent | 10 | All required components are present and correct; work is clearly structured and fully reproducible; dataset creation (R users) is correctly documented. |

## Grading note: Confounding assessment in Part B

The comparison of OR vs aOR (Criterion 2, Item 4) asks whether adjustment changed the estimated effect of cholesterol on heart disease. The key question is whether the point estimate (OR) changes meaningfully after adjustment.

- If the OR changes meaningfully after adjustment (rule of thumb: >10% change on the absolute scale), this suggests confounding.
- If the OR barely changes (e.g., 1.005 to 1.006 per 1 mg/dL), there is no meaningful evidence of confounding, even if the adjustment set is correct.

Precision (CI width change) is not a required part of the confounding assessment. Students are not penalized for omitting a discussion of CI width. However, students who mistake a CI or significance change for confounding when the point estimate barely moved should be penalized, because this reflects a misunderstanding of confounding.

- Correct confounding assessment based on whether the point estimate changed: **+**
- Claiming confounding based on a significance or CI change when the OR barely moved: **+/-**
- No comparison at all, or completely wrong interpretation: **-**

## Quick Reference: Common Issues by Score Level

### Minor Issues (one = Good; multiple = Sufficient)
- Small reporting omission (e.g., missing CI or df)
- Slightly unclear wording that doesn't change conclusion
- Minor formatting inconsistency
- Diagnostic plot included but not explicitly interpreted
- Comparison of OR vs aOR mentioned but not elaborated
- Claiming confounding based on a significance change when the OR barely moved

### Major Issues (one = Sufficient; multiple = Insufficient)
- Wrong model specified (e.g., missing covariate in ANCOVA, wrong adjustment set in DAG)
- Diagnostics/assumptions not checked at all
- Interaction test missing or misinterpreted
- Backward elimination not performed correctly (wrong criterion or threshold)
- Misinterpretation that changes the conclusion
- Missing or incorrect confidence intervals
- Dataset not correctly created (R users)

