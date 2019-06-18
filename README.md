# gambling-sibling-study

Analysis scripts for [insert publication DOI link].
For each analysis there are two scripts, one for the comparison of GD and conGD, one for the comparison of siblings and conSIB.

1. Group characteristics
Non-parametric tests comparing groups on demographic and clinical charactersitics. Results reported in table 1a.

2. Impulsivity UPPS-P MCQ
Kirby MCQ: Mixed linear regression to predict log(k). Reported in table 1c.
UPPS-P: unpaired t-tests of subscales Reported in table 1b

3. Impulsivity SST
Non-parametric comparison of groups on SST variables. Reported in table 1d.

4. CGT
- _decision_quality.R
Mixed logistic regression models predicting decision quality. 

- _bet_ascending.R and _bet_descending.R
Two files, one for ascending model, one for descending model. Mixed linear regression.

5. fMRI behaviour
Analysis of ratings during near miss task. Chance of winning and continue to play. Descriptives and mixed linear models.

6. fMRI ROI
Unlike previous scripts, bothe GD and SIB analyses are contained within the same file. One file for Near miss>full miss ROI anlaysis, one for win > all miss. Each file contains 6 models (3 ROIs x SIBHC, 3 ROIS x GDHC). Used lme instead of lmer.
- GDHC_and_SIBHC_WIN.R
Resported in supplemental materials.
- GDHC_and_SIBHC_NM.R
Resported in supplemental materials.

7. Between task correlations
Resported in supplemental materials.
Correlation between negative urgency and CGT bet and between negative urgency and fMRI win response. Within GD and SIB participants only. 
