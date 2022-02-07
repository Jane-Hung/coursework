# Provider Segmentation
**Author:** Jane Hung  
**Date:** 2018 Aug 17  
**Program:** Optum Data Science University - Machine Learning Methods (3 month period)  

## Table of Contents
- [CoNVO](#convo)
- [Question](#question)
- [Hypothesis](#hypothesis)
- [Data Lineage](#data-lineage)
- [Cohort Definition](#cohort-definition)
- [Execution Instructions](#execution-instructions)

## CoNVO
**Context, Need, Value, Outcome**  
Although we have multiple ways to understand and group our providers (i.e. 
through line of business, size, etc.), we still do not have a full understanding 
of how to segment our providers. In an environment where our providers feel
like we do not know who they are and how to best serve their needs, it is 
imperative to gain more intuition into the true provider personas. As a 
healthcare insurance company, creating these provider segments allows us to
understand a provider group's common concerns, background information, etc. and
grants us the ability to interact with individual providers with unique 
insights into their issues. This analysis directly adds value to the company
because we are then able to positively influence Net Promoter Scores (NPS) 
and develop a trusting relationship between us and our provider partners. As
an outcome, this analysis could be used in call centers so our agents can 
tailor their conversation to the provider persona. Furthermore, this analysis
could be used within our Advocate Network to help facilitate stronger 
communication and empathy.

## Question
What provider personas can be formed through unsupervised clustering methods?  

## Hypothesis
Based on the data and completing exploratory data analysis, below are expected
features that may determine provider clusters.  
* Providers accepting Medicare assignments  
* Providers using electronic health records  
* Providers with a secondary specialty  
* Providers with a hospital affiliation  
* Providers in a hospital network  
   
## Data Lineage
For this analysis, open source provider data was used from the Centers for 
Medicare & Medicaid Services. Please vist [here](https://data.medicare.gov/Physician-Compare/Physician-Compare-National-Downloadable-File/mj5m-pzi6) for the most up-to-date source. This data
was used because it has robust documentation, available SMEs, and current 
data refreshes. Furthermore, the data was fairly clean and was in a format 
that was easy to complete feature engineering. The total file contains 2.67
million rows; however, due to issues with high performance computing using
R on a local machine, I opted to compute a simple random sample to get 5% of
the total data, which amounted to ~133 thousand observations. This dataset
contains demographic and Medicare quality program participation information 
for individual eligible professionals.  
  
## Cohort Definition  
Within this analysis, I opted to include all providers taken from the SRS of 
5% of the total CMS dataset because these were all active and eligible 
providers. Furthermore, every provider in this dataset was represented as a 
single observation, and only individual providers (not organizations) were 
represented as a row. In addition, one additional constraint to this dataset
is that it was taken from the CMS website, which indicates that these providers
have had some affiliation with Medicare and Medicaid. Therefore, providers 
not in our cohort are those that *do not* have any past affiliation with CMS.

## Execution Instructions
Click [here](https://htmlpreview.github.io/?https://github.com/jane-hung/provider_segmentation/blob/master/MLM1805_JaneHung.html) for full HTML report.
