sysuse auto, replace

putdocx clear 
putdocx begin

 putdocx paragraph, style(Title)
 putdocx text ("Education Level and Place of Work Among Women in Ghana Using Demographic and Health Survey Data")

 putdocx paragraph, style(Heading1)
 putdocx text ("Abstract")
 
 putdocx paragraph
 putdocx text ("Objective: "), bold
 putdocx text ("State the objective or study question starting with To determine whether..."), linebreak
 putdocx text ("Methods: "), bold 
 putdocx text ("Provide the basic design, place, year(s), setting, and number of participants of the study. If applicable, include the name of the study, the duration of follow-up. Indicate exposure and outcomes."), linebreak
 putdocx text ("Results: "), bold
 putdocx text ("Include quantitative results."), linebreak
 putdocx text ("Conclusions: "), bold
 putdocx text ("Provide only conclusions of the study that are directly supported by the results, whether positive or negative."), linebreak
 
  putdocx paragraph, style(Heading1)
 putdocx text ("Introduction")
 
 putdocx paragraph
 putdocx text ("In Ghana, women face several obstacles to career advancement. Some of these obstacles include women's responsibility of raising children and maintaining a household, which stem from soecietal gender norms. Women also have limited access to opportunities that may lead to career advancement, such as education, assests, and owning land. Using Demographic and Health Survey Data collected in Ghana in 2008, we explore whether women's education level is associated with their place of work, either at home or away from home. We assume that a place of work away from home is indicative of greater career advancement compared to working at home and hypothesize that a higher education level is associated with working away from the home.")
 
  putdocx paragraph, style(Heading1)
 putdocx text ("Methods")
 
 putdocx paragraph
 putdocx text ("We used data from the 2008 Demographic and Healthy Survey (DHS) in Ghana, which is a national survey that collects data on topics such as maternal and child health, household characteristics, education, HIV/AIDS, and domestic violence. The 2008 DHS survey is the fifth such survey administered in Ghana and is uses a two-stage sample design. In the first stage, clusters were selected from a sampling frame based off of the 2000 Ghana Population and Housing Census, with probability of selection based on cluster size. 412 clusters were selected. In the second stage, 30 households were sampled from each cluster. In total, 12,323 households were selected and 11,778 were sampled in practice. All selected households participated in the Household Questionnaire, and women aged 15-49 and men aged 15-59 in half of the selected households participated in the Women's and Men's Questionnaire, respectively. 4,916 women and 4,568 men were interviewed. Sampling weights were computed to ensure the sample is nationally representative and accounts for non-response at the cluster, household, and individual level.")
 
 putdocx paragraph
 putdocx text ("In this analysis, the main predictor variable of interest is women's education level, which is a categorical variable comprised of 4 categories indicating the highest level of education achieved: no education, primary schooling, secondary schooling, and higher education. Place of work is the outcome variable of interest and is a binary variable indicating a work environment either at home or away from home. We also adjusted for 4 covariates we believe may confound this relationship: age (continuous), number of children born (continuous), place of residence (binary: urban or rural), and husband's education level (categorical, same categories as women's education level). Using these covariates, we restricted the analysis to married women who work, of which there were 2,404 individuals in the data.")
 
 putdocx paragraph
 putdocx text ("We used design-based analysis to account for the sampling structure when analyzing the data to answer the research question. First, we assessed missingness in the data and found a small amount of missing data in the following variables: place of work (outcome), respondent's education level, and husband's education level. To handle the missing data, we employed multiple imputation by chained equations (MICE) with 20 imputations on those three variables, using the other variables in the analysis as covariates to predict the missing values of these variables. The outcome regression model was a logistic regression model using weights. We conducted an exploratory analysis and found that using weights did not result in a significant loss of efficiency, so we decided to proceed with the weighted regression to achieve the most unbiased and representative results. With our relatively large sample size, we had adequate power to observe even a small effect size.")
 
  use "GHKR5HFL_copy-1.DTA", clear
  
  drop if v501 == 0
  keep if inlist(v731, 1, 2)
 
  label var v721 "Place of Work"

. label define v721 1 "At Home" 2 "Away", modify

. label var v012 "Age"

. label var v201 "Number of Children Born"

. label var v025 "Type of Place of Residence"

. label define v025 1 "Urban" 2 "Rural", modify

. label var v106 "Respondent's Education Level"

. label define v106 0 "No Education" 1 "Primary" 2 "Secondary" 3 "Higher" 9 "Missing", modify

. label var v701 "Husband's Education Level"

. label define v701 0 "No Education" 1 "Primary" 2 "Secondary" 3 "Higher" 8 "Don't Know" 9 "Missing", modify
 
  putdocx paragraph, style(Heading1)
  putdocx text ("Results")
  
   putdocx paragraph
  putdocx text ("Table 1 shows the distribution of demographic characteristics for each category of education level, the primary predictor of this anlaysis. Women with a higher education level tend to have fewer children, with an average of 2.3 children for women with higher education and an average of 4.75 children for women with no education. Additionally, women with higher education level are more lkely to live in an urban environment compared to a rural environment.")
 
  gen wt=v005/1000000 
 
 svyset v021 [pweight=wt], strata(v022)
 
 recode v721 (1 = 0) (2 = 1)
 
 replace v721 = . if v721 == 9
replace v106 = . if v106 == 9
replace v701 = . if v701 == 9

dtable, svy continuous(v012) continuous(v201) factor(v025)  factor(v701)  by(v106, nototal) /// 
 nformat(%9.2f mean) ///
 nformat(%9.2f sd) title(Table 1. Respondent's Education Level by Demographics) note(Total sample: N = 2,404; for continuous variables: mean (sd), for categorical variables: n (%))
 
putdocx collect

  rename (v721 v201 v701 v012 v025 v106) (Place_of_Work Number_of_Children_Born Husband_Edu_Level Age Place_of_Residence Edu_Level) 
  
  mi set wide
 mi register imputed Place_of_Work Husband_Edu_Level Edu_Level
 mi register regular Age Number_of_Children_Born Place_of_Residence
 mi impute chained (mlogit) Husband_Edu_Level Edu_Level (logit) Place_of_Work = Age Number_of_Children_Born Place_of_Residence, add(20) rseed(12345) augment
 mi xeq 0 5 10 20: summarize Place_of_Work Husband_Edu_Level Edu_Level
 mi svyset v021 [pw=wt], str(v022)
 mi estimate, dots or: svy:logistic Place_of_Work i.Edu_Level Age Number_of_Children_Born i.Place_of_Residence i.Husband_Edu_Level
 
 putdocx table mytable = etable
 
 putdocx paragraph 
 putdocx text ("Table 2 shows the results from the regression model fit using MICE. The odds of working away from home for women with at most a primary school education are 0.56 (95% CI: 0.39, 0.85) times the odds of working away from home for women with no education (the reference category). This means that women with primary school education are less likely to work away from home compared to women with no education. Interestingly, the comparison between women with secondary school education and no education is non-significant. The odds ratio comparing these groups is 0.75 (95% CI: 0.51, 1.11). However, the odds of working away from home among women with higher education is 16.13 (95% CI: 1.83, 142.37) times the odds of working away from home among women with no education. All of these odds ratios are adjusted for age, number of children born, place of residence, and husband's education level, as shown in Table 2.")
 
  putdocx paragraph, style(Heading1)
 putdocx text ("Discussion")
 
   putdocx paragraph 
   putdocx text ("This study examines the relationship between education level and place of work among married working women in Ghana using 2008 DHS data. The main finding is that completing higher education is associated with significantly increased odds of working away from the home. A potential limitaiton of this study is that there may be additional confounders we did not adjust for. In the future, a more extensive literature review and consultation with experts in education and career advancement in Ghana or similar countries may be beneficial to ensure that all relevant confounders are adjusted for. The implication of this finding is that higher education may serve as a gateway to career advancement for women in Ghana, and that implimenting policies or campaigns that lead more women to achieve higher education could improve career advancement for women on a broad scale in Ghana.")

  putdocx paragraph, style(Heading1)
 putdocx text ("References")


putdocx save FinalProject.docx, replace
