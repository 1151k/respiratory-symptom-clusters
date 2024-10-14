##### R scripts used in 
**All-cause and cause-specific mortality in respiratory symptom clusters: a population-based multicohort study**
##### Daniil Lisik, Helena Backman, Hannu Kankaanranta, Rani Basna, Linnea Hedman, Linda Ekerljung, Fredrik Nyberg, Anne Lindberg, Göran Wennergren, Eva Rönmark, Bright I. Nwaru, Lowie Vanfleteren
##### *Under review*

<br>

#### Description of R scripts
- **VARIABLES.r**: define variables across cohorts and their characteristics (e.g., variables for cluster analysis)  
- **CONSTANTS.r**: define constants used in multiple R scripts (e.g., folder paths)  
- **draw-dag.r**: draw a directed acyclic graph (DAG)  
- **clean-and-combine-datasets.r**: do cleaning/preprocessing and merge data from the cohorts with standardized variable names  
- **assess-pre-imputation-data.r**: tabulate data pre-imputation by cohort  
- **assess-missingness.r**: quantify and visualize missingness in the data  
- **select-random-dataset.r** select random dataset for testing stability by random removal of subjects
- **impute.r**: impute missing data (and assess validity of imputation), and set composite variables  
- **pool-imputations.r**: pool imputed datasets  
- **add-comorbidity-data.r**: add comorbidity register data and tabulate the results  
- **assess-pre-post-imputation-data.r**: tabulate data pre- and post-imputation (as well as tabulation of relevant register [comorbidity] data) 
- **do-descriptive-statistics.r**: generate descriptive statistics, primarily regarding respiratory symptoms frequency  
- **assess-correlation.r**: quantify and visualize correlation between potential cluster-defining variables  
- **prepare-data-for-cluster-analysis.r**: based on above findings, prepare data to be used in cluster analysis  
- **lsh.r**: perform cluster analysis  
- **pool-cluster-labels.r**: pool cluster labels  
- **assess-clusters.r**: tabulate data by clusters  
- **add-mortality-data.r**: prepare data to be used in mortality analysis  
- **mortality-analysis.r**: perform mortality analysis and extract results therefrom  
- **pool-hazard-ratios-and-confidence-intervals.r**: pool hazard ratios (HR) and corresponding 95% confidence intervals (95%CI)
- **catboost.r**: assess feature importance by gradient boosting on decision trees
- **plot-cost.r**: across all the cluster analyses across all datasets, pool and plot the clustering cost (total within-cluster distance)
<br>

#### Data availability
Due to regulations and contractual agreement with study participants, the underlying data are not available.

<br>

#### Contact
For any inquiries, please contact [Daniil Lisik](https://www.gu.se/en/about/find-staff/daniillisik) ([daniil.lisik@gmail.com](mailto:daniil.lisik@gmail.com)).

<br>
