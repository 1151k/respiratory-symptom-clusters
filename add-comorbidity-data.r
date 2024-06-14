# add comorbidity data



# load external packages
packages_all = c("data.table", "haven", "dplyr", "flextable", "gtsummary")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
# load internal constants
source('CONSTANTS.r')
source('VARIABLES.r')



# Charlson Comorbidity Index (CCI) calculator
cci_calculator <- function(patients, Matrix) {
    #########################################################################################################
    # This script was based on the publication:
    #  
    #  Adaptation of the Charlson comorbidity index for register-based research in Sweden. 
    #  Jonas F. Ludvigsson, Peter Appelros, Johan Askling, Liisa Byberg, Juan-Jesus Carrero, Anna Mia Ekström, Magnus Ekström, Karin Ekström Smedby, 
    #  Hannes Hagström, Stefan James, Bengt Järvholm, Karl Michaelsson, Nancy L. Pedersen, Helene Sundelin, Kristina Sundquist, Johan Sundström. Clinical Epidemiology, 2021:13.

    # The actual scripts (available in R, STATA and SAS) were created by Bjorn Roelstraete and Jonas Söderling. Data management: Mariam Lashkariani

    # Original code can be found at https://github.com/bjoroeKI/Charlson-comorbidity-index-revisited/blob/main/Charlson_R
    # The code was here modified by excluding combinations of asthma, COPD, and other chronic pulmonary diseeases from the calculation (instead, these were calculated explicitly)
    ##########################################################################################################
    # Myocardial_infarction
    icd7  <- "\\<420,1"
    icd8  <- "\\<410|\\<411|\\<412,01|\\<412,91"
    icd9  <- "\\<410|\\<412"
    icd10 <- "\\<I21|\\<I22|\\<I252"
    
    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9  <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10 <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Myocardial_infarction=datum,diagnos.Myocardial_infarction=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Myocardial_infarction=if_else(!is.na(date.Myocardial_infarction),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Congestive_heart_failure
    icd7  <- "\\<422,21|\\<422,22|\\<434,1|\\<434,2"
    icd8  <- "\\<425,08|\\<425,09|\\<427,0|\\<427,1|\\<428"
    icd9  <- paste(c("\\<402A", "402B", "402X", "404A","404B","404X","425E","425F","425H","425W","425X","428"),collapse="|\\<")
    icd10 <- "\\<I110|\\<I130|\\<I132|\\<I255|\\<I420|\\<I426|\\<I427|\\<I428|\\<I429|\\<I43|\\<I50"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10 <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Congestive_heart_failure=datum,diagnos.Congestive_heart_failure=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Congestive_heart_failure=if_else(!is.na(date.Congestive_heart_failure),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Peripheral_vascular_disease
    icd7  <- "\\<450,1|\\<451|\\<453"
    icd8  <- "\\<440|\\<441|\\<443,1|\\<443,9"
    icd9  <- "\\<440|\\<441|\\<443B|\\<443X|\\<447B|\\<557"
    icd10 <- "\\<I70|\\<I71|\\<I731|\\<I738|\\<I739|\\<I771|\\<I790|\\<I792|\\<K55"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Peripheral_vascular_disease=datum,diagnos.Peripheral_vascular_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Peripheral_vascular_disease=if_else(!is.na(date.Peripheral_vascular_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Cerebrovascular_disease
    icd7  <- paste(c("\\<330",331:334),collapse="|\\<")
    icd8  <- "\\<430|\\<431|\\<432|\\<433|\\<434|\\<435|\\<436|\\<437|\\<438"
    icd9  <- "\\<430|\\<431|\\<432|\\<433|\\<434|\\<435|\\<436|\\<437|\\<438"
    icd10 <- "\\<G45|\\<I60|\\<I61|\\<I62|\\<I63|\\<I64|\\<I67|\\<I69"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Cerebrovascular_disease=datum,diagnos.Cerebrovascular_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Cerebrovascular_disease=if_else(!is.na(date.Cerebrovascular_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Asthma
    icd7  <- "\\<241"
    icd8  <- "\\<493"
    icd9  <- "\\<493"
    icd10 <- "\\<J45|\\<J46"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Asthma=datum,diagnos.Asthma=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Asthma=if_else(!is.na(date.Asthma),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Chronic_obstructive_pulmonary_disease
    icd7  <- "\\<502|\\<527,1"
    icd8  <- "\\<491|\\<492"
    icd9  <- "\\<491|\\<492|\\<496"
    icd10 <- "\\<J43|\\<J44"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Chronic_obstructive_pulmonary_disease=datum,diagnos.Chronic_obstructive_pulmonary_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Chronic_obstructive_pulmonary_disease=if_else(!is.na(date.Chronic_obstructive_pulmonary_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Chronic_other_pulmonary_disease
    icd7  <- paste(c("\\<501",523:526),collapse="|\\<") # excluding 241 (asthma)
    icd8  <- paste(c("\\<490",515:518),collapse="|\\<") # excluding 493 (asthma)
    icd9  <- paste(c("\\<490",494:495,500:508,516,517),collapse="|\\<") # excluding 493 (asthma)
    icd10 <- paste(c("\\<J41",42,47,60:70),collapse="|\\<J") # excluding J45 and J46 (asthma and acute severe asthma)

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Chronic_other_pulmonary_disease=datum,diagnos.Chronic_other_pulmonary_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Chronic_other_pulmonary_disease=if_else(!is.na(date.Chronic_other_pulmonary_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Rheumatic_disease
    icd7  <- paste(c("\\<722,00","722,01","722,10","722,20","722,23","456,0","456,1","456,2","456,3"),collapse="|\\<")
    icd8  <- paste(c("\\<446",696,"712,0","712,1","712,2","712,3","712,5", 716, "734,0", "734,1", "734,9"),collapse="|\\<")
    icd9  <- paste(c("\\<446","696A","710A","710B","710C","710D","710E",714,"719D",720,725),collapse="|\\<")
    icd10 <- paste(c("\\<M05","06",123,"070","071","072","073","08",13,30,313:316,32:34,350:351,353,45:46),collapse="|\\<M")

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Rheumatic_disease=datum,diagnos.Rheumatic_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Rheumatic_disease=if_else(!is.na(date.Rheumatic_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Dementia
    icd7  <- "\\<304|\\<305"
    icd8  <- "\\<290"
    icd9  <- "\\<290|\\<294B|\\<331A|\\<331B|\\<331C|\\<331X"
    icd10 <- "\\<F00|\\<F01|\\<F02|\\<F03|\\<F051|\\<G30|\\<G311|\\<G319"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Dementia=datum,diagnos.Dementia=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Dementia=if_else(!is.na(date.Dementia),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Hemiplegia
    icd7 <- "\\<351|\\<352|\\<357,00"
    icd8 <- "\\<343|\\<344"
    icd9 <- "\\<342|\\<343|\\<344A|\\<344B|\\<344C|\\<344D|\\<344E|\\<344F"
    icd10 <- "\\<G114|\\<G80|\\<G81|\\<G82|\\<G830|\\<G831|\\<G832|\\<G833|\\<G838"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Hemiplegia=datum,diagnos.Hemiplegia=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Hemiplegia=if_else(!is.na(date.Hemiplegia),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Diabetes_without_chronic_complication
    icd7 <- "\\<260,09"
    icd8 <-  "\\<250,00|\\<250,07|\\<250,08"
    icd9 <- "\\<250A|\\<250B|\\<250C"
    icd10 <- "\\<E100|\\<E101|\\<E110|\\<E111|\\<E120|\\<E121|\\<E130|\\<E131|\\<E140|\\<E141"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Diabetes_without_chronic_complication=datum,diagnos.Diabetes_without_chronic_complication=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Diabetes_without_chronic_complication=if_else(!is.na(date.Diabetes_without_chronic_complication),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Diabetes_with_chronic_complication
    icd7 <- "\\<260,2|\\<260,21|\\<260,29|\\<260,3|\\<260,4|\\<260,49|\\<260,99"
    icd8 <- "\\<250,01|\\<250,02|\\<250,03|\\<250,04|\\<250,05"
    icd9 <- "\\<250D|\\<250E|\\<250F|\\<250G"
    icd10 <- "\\<E102|\\<E103|\\<E104|\\<E105|\\<E107|\\<E112|\\<E113|\\<E114|\\<E115|\\<E116|\\<E117|\\<E122|\\<E123|\\<E124|\\<E125|\\<E126|\\<E127|\\<E132|\\<E133|\\<E134|\\<E135|\\<E136|\\<E137|\\<E142|\\<E143|\\<E144|\\<E145|\\<E146|\\<E147"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Diabetes_with_chronic_complication=datum,diagnos.Diabetes_with_chronic_complication=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Diabetes_with_chronic_complication=if_else(!is.na(date.Diabetes_with_chronic_complication),1,0,missing=0))
    Matrix <- Matrix %>% mutate(Diabetes_without_chronic_complication=if_else(Diabetes_with_chronic_complication==1,0,Diabetes_without_chronic_complication))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Renal_disease
    icd7 <- "\\<592|\\<593|\\<792"
    icd8 <- "\\<582|\\<583|\\<584|\\<792|\\<593|\\<403,99|\\<404,99|\\<792,99|\\<Y29,01"
    icd9 <- "\\<403A|\\<403B|\\<403X|\\<582|\\<583|\\<585|\\<586|\\<588A|\\<V42A|\\<V45B|\\<V56"
    icd10 <- "\\<I120|\\<I131|\\<N032|\\<N033|\\<N034|\\<N035|\\<N036|\\<N037|\\<N052|\\<N053|\\<N054|\\<N055|\\<N056|\\<N057|\\<N11|\\<N18|\\<N19|\\<N250|\\<Q611|\\<Q612|\\<Q613|\\<Q614|\\<Z49|\\<Z940|\\<Z992"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Renal_disease=datum,diagnos.Renal_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Renal_disease=if_else(!is.na(date.Renal_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Mild_liver_disease
    icd7  <- "\\<581"
    icd8  <- "\\<070|\\<571|\\<573"
    icd9 <-  "\\<070|\\<571C|\\<571E|\\<571F|\\<573"
    icd10 <- "\\<B15|\\<B16|\\<B17|\\<B18|\\<B19|\\<K703|\\<K709|\\<K73|\\<K746|\\<K754"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Mild_liver_disease=datum,diagnos.Mild_liver_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Mild_liver_disease=if_else(!is.na(date.Mild_liver_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # liver special
    icd8  <- "\\<785,3"
    icd9 <- "\\<789F"
    icd10 <- "\\<R18"

    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.liver_special=datum,diagnos.liver_special=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Liver_special=if_else(!is.na(date.liver_special),1,0,missing=0))
    rm(icd8, icd9, icd10, ICD8, ICD9, ICD10, ptnts)

    # moderate severe liver disease
    icd7 <- "\\<462,1"
    icd8 <- "\\<456,0|\\<571,9|\\<573,02"
    icd9 <- "\\<456A|\\<456B|\\<456C|\\<572C|\\<572D|\\<572E"
    icd10 <-  "\\<I850|\\<I859|\\<I982|\\<I983"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Severe_liver_disease=datum,diagnos.Severe_liver_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Severe_liver_disease=if_else(!is.na(date.Severe_liver_disease),1,0,missing=0))
    Matrix <- Matrix %>% mutate(Severe_liver_disease=if_else(Mild_liver_disease==1 & Liver_special==1,1,Severe_liver_disease))
    Matrix <- Matrix %>% mutate(Mild_liver_disease=if_else(Severe_liver_disease==1,0,Mild_liver_disease))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Peptic_ulcer_disease
    icd7  <- "\\<540|\\<541|\\<542"
    icd8  <- "\\<531|\\<532|\\<533|\\<534"
    icd9 <- "\\<531|\\<532|\\<533|\\<534"
    icd10 <-"\\<K25|\\<K26|\\<K27|\\<K28"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Peptic_ulcer_disease=datum,diagnos.Peptic_ulcer_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Peptic_ulcer_disease=if_else(!is.na(date.Peptic_ulcer_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Malignancy
    icd7   <- paste(paste("\\<",paste(140:190,collapse = "|\\<"),sep=""), paste("|\\<",paste(192:197,collapse = "|\\<"),sep=""), paste("|\\<",paste(200:204,collapse = "|\\<"),sep=""),sep="")
    icd8   <- paste(paste("\\<",paste(c(140:172,174),collapse = "|\\<"),sep=""), paste("|\\<",paste(c(180:207,209),collapse = "|\\<"),sep=""),sep="")
    icd9   <- paste(paste("\\<",paste(140:172,collapse = "|\\<"),sep=""), paste("|\\<",paste(174:208,collapse = "|\\<"),sep=""),sep="")
    icd10  <- paste("\\<C00|\\<C0",paste(1:9,collapse = "|\\<C0",sep=""),paste("|\\<C",paste(c(10:41,43,45:58,60:76,81:86,88:97),collapse = "|\\<C"),sep=""),sep="")

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.malignancy=datum,diagnos.malignancy=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Malignancy=if_else(!is.na(date.malignancy),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Metastatic_cancer
    icd7 <- "\\<156,91|\\<198|\\<199"
    icd8 <- "\\<196|\\<197|\\<198|\\<199"
    icd9 <- "\\<196|\\<197|\\<198|\\<199A|\\<199B"
    icd10 <- "\\<C77|\\<C78|\\<C79|\\<C80"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Metastatic_solid_tumor=datum,diagnos.Metastatic_solid_tumor=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Metastatic_solid_tumor=if_else(!is.na(date.Metastatic_solid_tumor),1,0,missing=0))
    Matrix <- Matrix %>% mutate(Malignancy=if_else(Metastatic_solid_tumor==1,0,Malignancy))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Aids
    icd9  <- "\\<079J|\\<279K"
    icd10 <- "\\<B20|\\<B21|\\<B22|\\<B23|\\<B24|\\<F024|\\<O987|\\<R75|\\<Z219|\\<Z717"

    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Aids=datum,diagnos.Aids=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Aids=if_else(!is.na(date.Aids),1,0,missing=0))
    rm(icd9, icd10, ICD9, ICD10, ptnts)

    # Calculate the weighted comorbidity index (CCI, including all respiratory diseases)
    Matrix$CCI <- Matrix$Myocardial_infarction + Matrix$Congestive_heart_failure + Matrix$Peripheral_vascular_disease + 
                Matrix$Cerebrovascular_disease + Matrix$Rheumatic_disease + Matrix$Dementia + 2*Matrix$Hemiplegia + Matrix$Diabetes_without_chronic_complication + 
                2*Matrix$Diabetes_with_chronic_complication + 2*Matrix$Renal_disease + Matrix$Mild_liver_disease + 3*Matrix$Severe_liver_disease + 
                Matrix$Peptic_ulcer_disease + 2*Matrix$Malignancy + 6*Matrix$Metastatic_solid_tumor + 6*Matrix$Aids + Matrix$Asthma + Matrix$Chronic_other_pulmonary_disease + Matrix$Chronic_obstructive_pulmonary_disease

    # Calculate the weighted comorbidity index (CCI, including Chronic_other_pulmonary_disease but not COPD or asthma)
    Matrix$CCI_no_asthma_COPD <- Matrix$Myocardial_infarction + Matrix$Congestive_heart_failure + Matrix$Peripheral_vascular_disease + 
                Matrix$Cerebrovascular_disease + Matrix$Rheumatic_disease + Matrix$Dementia + 2*Matrix$Hemiplegia + Matrix$Diabetes_without_chronic_complication + 
                2*Matrix$Diabetes_with_chronic_complication + 2*Matrix$Renal_disease + Matrix$Mild_liver_disease + 3*Matrix$Severe_liver_disease + 
                Matrix$Peptic_ulcer_disease + 2*Matrix$Malignancy + 6*Matrix$Metastatic_solid_tumor + 6*Matrix$Aids + Matrix$Chronic_other_pulmonary_disease # excluding Matrix$Chronic_obstructive_pulmonary_disease and Matrix$Asthma

    # Calculate the weighted comorbidity index (CCI, including no respiratory disease)
    Matrix$CCI_no_respiratory <- Matrix$Myocardial_infarction + Matrix$Congestive_heart_failure + Matrix$Peripheral_vascular_disease + 
                Matrix$Cerebrovascular_disease + Matrix$Rheumatic_disease + Matrix$Dementia + 2*Matrix$Hemiplegia + Matrix$Diabetes_without_chronic_complication + 
                2*Matrix$Diabetes_with_chronic_complication + 2*Matrix$Renal_disease + Matrix$Mild_liver_disease + 3*Matrix$Severe_liver_disease + 
                Matrix$Peptic_ulcer_disease + 2*Matrix$Malignancy + 6*Matrix$Metastatic_solid_tumor + 6*Matrix$Aids # excluding Matrix$Chronic_other_pulmonary_disease, Matrix$Chronic_obstructive_pulmonary_disease and Matrix$Asthma

    # Calculat only congestive heart failure
    Matrix$CCI_CHF <- Matrix$Congestive_heart_failure

    # Calculate only peripheral vascular disease
    Matrix$CCI_PVD <- Matrix$Peripheral_vascular_disease

    # Calculate only cerebrovascular disease
    Matrix$CCI_CVD <- Matrix$Cerebrovascular_disease

    # Calculate only rheumatic disease
    Matrix$CCI_RD <- Matrix$Rheumatic_disease

    # Calculate only dementia
    Matrix$CCI_Dementia <- Matrix$Dementia

    # Calculate only hemiplegia
    Matrix$CCI_Hemiplegia <- Matrix$Hemiplegia

    # Calculate only diabetes without chronic complication
    Matrix$CCI_Diabetes_without_chronic_complication <- Matrix$Diabetes_without_chronic_complication

    # Calculate only diabetes with chronic complication
    Matrix$CCI_Diabetes_with_chronic_complication <- Matrix$Diabetes_with_chronic_complication

    # Calculate only renal disease
    Matrix$CCI_Renal_disease <- Matrix$Renal_disease

    # Calculate only mild liver disease
    Matrix$CCI_Mild_liver_disease <- Matrix$Mild_liver_disease

    # Calculate only severe liver disease
    Matrix$CCI_Severe_liver_disease <- Matrix$Severe_liver_disease

    # Calculate only peptic ulcer disease
    Matrix$CCI_Peptic_ulcer_disease <- Matrix$Peptic_ulcer_disease

    # Calculate only malignancy
    Matrix$CCI_Malignancy <- Matrix$Malignancy

    # Calculate only metastatic solid tumor
    Matrix$CCI_Metastatic_solid_tumor <- Matrix$Metastatic_solid_tumor

    # Calculate only aids
    Matrix$CCI_Aids <- Matrix$Aids    

    # Calculate asthma
    Matrix$CCI_Asthma <- Matrix$Asthma

    # Calculate COPD
    Matrix$CCI_COPD <- Matrix$Chronic_obstructive_pulmonary_disease

    # Calculate Chronic_other_pulmonary_disease
    Matrix$CCI_Chronic_other_pulmonary_disease <- Matrix$Chronic_other_pulmonary_disease

    # Delete date and diagnos information in case not needed 
    Matrix <- select(Matrix, -contains("."))

    return(Matrix)
}



# local variables
raw_or_pooled_imputed <- 'pooled_imputed' # set to 'raw' or 'pooled_imputed'
# MAIN DATA TO APPEND COMORBIDITY DATA TO
if (raw_or_pooled_imputed == 'raw') {
    load(paste0(folder_path, 'output/rda/', 'raw-data', '.Rda')) # raw data
    dt_working <- copy(dt)
} else {
    load(paste0(folder_path, 'output/rda/', 'imputed-data--pooled', '.Rda')) # pooled imputed data
    dt_working <- copy(dt_pooled_imputed)
}
print(paste('loaded', raw_or_pooled_imputed, 'data'))
# cohort-specific years of baseline
cohort_years <- c(
    "OLIN-IV-1996" = 1996,
    "OLIN-VI-2006" = 2006,
    "OLIN-VII-2016" = 2016,
    "WSAS-I-2008" = 2008,
    "WSAS-II-2016" = 2016
)
# create "extra" comorbidity columns (asthma and COPD, which will be defined separately)
dt_working$comorbidity_asthma <- 0
# definition based on https://openres.ersjournals.com/content/6/3/00258-2019
dt_working[asthma_physician_diagnosed == 1 & (asthma_medication_use == 1 | wheezing_12m == 1 | attack_12m == 1), comorbidity_asthma := 1]
dt_working$comorbidity_copd <- 0
dt_working$comorbidity_copd[dt_working$copd_physician_diagnosed == 1] <- 1

# COMORBIDITY DATA HANDLING
# WSAS
# wsas cohort data.table (person_id and cohort from pooled imputed data where cohort == 'WSAS-I-2008' or 'WSAS-II-2016')
wsas_cohort <- dt_working[cohort %in% c('WSAS-I-2008', 'WSAS-II-2016'), .(person_id, cohort)]
# wsas inpatient data
# get data
wsas_inpatient <- setDT(read_sav(paste0(folder_path, 'input/sav/WSAS-inpatient-care.sav')))
# combine diagnosis columns into one
wsas_inpatient[, diagnos := do.call(paste, c(.SD, sep = " ")), .SDcols = paste0('DIA', 1:30)]
# select only relevant columns
wsas_inpatient <- wsas_inpatient[, .(AR, LopNr, diagnos, UTDATUMA)]
# change column names
setnames(wsas_inpatient, 'LopNr', 'person_id')
setnames(wsas_inpatient, 'UTDATUMA', 'datum')
# merge with cohort data (thereby attaining the cohort column)
wsas_inpatient <- merge(wsas_inpatient, wsas_cohort, by = 'person_id', all.x = TRUE)
# filter out data from before the cohort year
print(dim(wsas_inpatient))
wsas_inpatient <- wsas_inpatient[AR <= cohort_years[cohort], ]
print(dim(wsas_inpatient))
# wsas outpatient data
# get data
wsas_outpatient <- setDT(read_sav(paste0(folder_path, 'input/sav/WSAS-outpatient-care.sav')))
# combine diagnosis columns into one
wsas_outpatient[, diagnos := do.call(paste, c(.SD, sep = " ")), .SDcols = paste0('DIA', 1:30)]
# select only relevant columns
wsas_outpatient <- wsas_outpatient[, .(AR, LopNr, diagnos, INDATUMA)] # OUTDATUMA not available since outpatient
# change column names
setnames(wsas_outpatient, 'LopNr', 'person_id')
setnames(wsas_outpatient, 'INDATUMA', 'datum')
# merge with cohort data (thereby attaining the cohort column)
wsas_outpatient <- merge(wsas_outpatient, wsas_cohort, by = 'person_id', all.x = TRUE)
# filter out data from before the cohort year
print(dim(wsas_outpatient))
wsas_outpatient <- wsas_outpatient[AR <= cohort_years[cohort], ]
print(dim(wsas_outpatient))
# combine wsas inpatient and outpatient data and preprocess before CCI calculation
wsas_comorbidity <- rbind(wsas_inpatient, wsas_outpatient)
# remove AR column
wsas_comorbidity <- wsas_comorbidity[, AR := NULL]
# rename group column to person_id
setnames(wsas_comorbidity, 'person_id', 'group')
wsas_comorbidity[, group := as.numeric(group)]
# select the columns group, datum, diagnos
wsas_comorbidity_without_cohort <- wsas_comorbidity[, .(group, datum, diagnos)]
# remove leading and trailing whitespaces from diagnos
wsas_comorbidity_without_cohort$diagnos <- trimws(wsas_comorbidity_without_cohort$diagnos)
# one subject per row to populate with comorbidity data
Matrix <- distinct(wsas_comorbidity_without_cohort, group)
# calculate comorbidity data
Matrix <- cci_calculator(wsas_comorbidity_without_cohort, Matrix)
# select only the relevant columns
Matrix <- select(Matrix, group, CCI, CCI_no_asthma_COPD, CCI_no_respiratory, CCI_CHF, CCI_PVD, CCI_CVD, CCI_RD, CCI_Dementia, CCI_Hemiplegia, CCI_Diabetes_without_chronic_complication, CCI_Diabetes_with_chronic_complication, CCI_Renal_disease, CCI_Mild_liver_disease, CCI_Severe_liver_disease, CCI_Peptic_ulcer_disease, CCI_Malignancy, CCI_Metastatic_solid_tumor, CCI_Aids, CCI_Asthma, CCI_COPD, CCI_Chronic_other_pulmonary_disease)
# rename group to person_id
Matrix <- rename(Matrix, person_id = group)
# merge with pooled imputed data
wsas_data <- copy(dt_working[dt_working$cohort == 'WSAS-I-2008' | dt_working$cohort == 'WSAS-II-2016', ])
wsas_data <- merge(wsas_data, Matrix, by = 'person_id', all.x = TRUE)
wsas_data[, CCI := ifelse(!is.na(CCI), CCI, 0)]
wsas_data[, CCI_no_asthma_COPD := ifelse(!is.na(CCI_no_asthma_COPD), CCI_no_asthma_COPD, 0)]
wsas_data[, CCI_no_respiratory := ifelse(!is.na(CCI_no_respiratory), CCI_no_respiratory, 0)]
wsas_data[, CCI_CHF := ifelse(!is.na(CCI_CHF), CCI_CHF, 0)]
wsas_data[, CCI_PVD := ifelse(!is.na(CCI_PVD), CCI_PVD, 0)]
wsas_data[, CCI_CVD := ifelse(!is.na(CCI_CVD), CCI_CVD, 0)]
wsas_data[, CCI_RD := ifelse(!is.na(CCI_RD), CCI_RD, 0)]
wsas_data[, CCI_Dementia := ifelse(!is.na(CCI_Dementia), CCI_Dementia, 0)]
wsas_data[, CCI_Hemiplegia := ifelse(!is.na(CCI_Hemiplegia), CCI_Hemiplegia, 0)]
wsas_data[, CCI_Diabetes_without_chronic_complication := ifelse(!is.na(CCI_Diabetes_without_chronic_complication), CCI_Diabetes_without_chronic_complication, 0)]
wsas_data[, CCI_Diabetes_with_chronic_complication := ifelse(!is.na(CCI_Diabetes_with_chronic_complication), CCI_Diabetes_with_chronic_complication, 0)]
wsas_data[, CCI_Renal_disease := ifelse(!is.na(CCI_Renal_disease), CCI_Renal_disease, 0)]
wsas_data[, CCI_Mild_liver_disease := ifelse(!is.na(CCI_Mild_liver_disease), CCI_Mild_liver_disease, 0)]
wsas_data[, CCI_Severe_liver_disease := ifelse(!is.na(CCI_Severe_liver_disease), CCI_Severe_liver_disease, 0)]
wsas_data[, CCI_Peptic_ulcer_disease := ifelse(!is.na(CCI_Peptic_ulcer_disease), CCI_Peptic_ulcer_disease, 0)]
wsas_data[, CCI_Malignancy := ifelse(!is.na(CCI_Malignancy), CCI_Malignancy, 0)]
wsas_data[, CCI_Metastatic_solid_tumor := ifelse(!is.na(CCI_Metastatic_solid_tumor), CCI_Metastatic_solid_tumor, 0)]
wsas_data[, CCI_Aids := ifelse(!is.na(CCI_Aids), CCI_Aids, 0)]
wsas_data[, CCI_Asthma := ifelse(!is.na(CCI_Asthma), CCI_Asthma, 0)]
wsas_data[, CCI_COPD := ifelse(!is.na(CCI_COPD), CCI_COPD, 0)]
wsas_data[, CCI_Chronic_other_pulmonary_disease := ifelse(!is.na(CCI_Chronic_other_pulmonary_disease), CCI_Chronic_other_pulmonary_disease, 0)]

# OLIN
# olin cohort data.table (person_id and cohort from pooled imputed data where cohort == 'OLIN-IV-1996' or 'OLIN-VI-2006' or 'OLIN-VII-2016')
olin_cohort <- dt_working[cohort %in% c('OLIN-IV-1996', 'OLIN-VI-2006', 'OLIN-VII-2016'), .(person_id, cohort)]
olin_cohort <- olin_cohort[order(person_id)]
# olin inpatient data
# get data
olin_inpatient <- setDT(read_sav(paste0(folder_path, 'input/sav/OLIN-inpatient-care.sav')))
# select only relevant columns
olin_inpatient <- olin_inpatient[, .(AR, LopNr, DIAGNOS, UTDATUMA)]
# change column names
setnames(olin_inpatient, 'LopNr', 'person_id')
setnames(olin_inpatient, 'UTDATUMA', 'datum')
# merge with cohort data (thereby attaining the cohort column)
olin_inpatient <- merge(olin_inpatient, olin_cohort, by = 'person_id', all.x = TRUE)
# filter out data from before the cohort year
print(dim(olin_inpatient))
olin_inpatient <- olin_inpatient[AR <= cohort_years[cohort], ]
print(dim(olin_inpatient))
# olin outpatient data
# get data
olin_outpatient <- setDT(read_sav(paste0(folder_path, 'input/sav/OLIN-outpatient-care.sav')))
# select only relevant columns
olin_outpatient <- olin_outpatient[, .(AR, LopNr, DIAGNOS, INDATUMA)] # OUTDATUMA not available since outpatient
# change column names
setnames(olin_outpatient, 'LopNr', 'person_id')
setnames(olin_outpatient, 'INDATUMA', 'datum')
# merge with cohort data (thereby attaining the cohort column)
olin_outpatient <- merge(olin_outpatient, olin_cohort, by = 'person_id', all.x = TRUE)
# filter out data from before the cohort year
print(dim(olin_outpatient))
olin_outpatient <- olin_outpatient[AR <= cohort_years[cohort], ]
print(dim(olin_outpatient))
# combine olin inpatient and outpatient data and preprocess before CCI calculation
olin_comorbidity <- rbind(olin_inpatient, olin_outpatient)
# remove AR column
olin_comorbidity <- olin_comorbidity[, AR := NULL]
# rename group column to person_id
setnames(olin_comorbidity, 'person_id', 'group')
olin_comorbidity[, group := as.numeric(group)]
# rename DIAGNOS to diagnos
setnames(olin_comorbidity, 'DIAGNOS', 'diagnos')
# select the columns group, datum, diagnos
olin_comorbidity_without_cohort <- olin_comorbidity[, .(group, datum, diagnos)]
# remove leading and trailing whitespaces from diagnos
olin_comorbidity_without_cohort$diagnos <- trimws(olin_comorbidity_without_cohort$diagnos)
# one subject per row to populate with comorbidity data
Matrix <- distinct(olin_comorbidity_without_cohort, group)
# calculate comorbidity data
Matrix <- cci_calculator(olin_comorbidity_without_cohort, Matrix)
# select only the relevant columns
Matrix <- select(Matrix, group, CCI, CCI_no_asthma_COPD, CCI_no_respiratory, CCI_CHF, CCI_PVD, CCI_CVD, CCI_RD, CCI_Dementia, CCI_Hemiplegia, CCI_Diabetes_without_chronic_complication, CCI_Diabetes_with_chronic_complication, CCI_Renal_disease, CCI_Mild_liver_disease, CCI_Severe_liver_disease, CCI_Peptic_ulcer_disease, CCI_Malignancy, CCI_Metastatic_solid_tumor, CCI_Aids, CCI_Asthma, CCI_COPD, CCI_Chronic_other_pulmonary_disease)
# rename group to person_id
Matrix <- rename(Matrix, person_id = group)
# merge with pooled imputed data
olin_data <- copy(dt_working[dt_working$cohort == 'OLIN-IV-1996' | dt_working$cohort == 'OLIN-VI-2006' | dt_working$cohort == 'OLIN-VII-2016', ])
olin_data <- merge(olin_data, Matrix, by = 'person_id', all.x = TRUE)
olin_data[, CCI := ifelse(!is.na(CCI), CCI, 0)]
olin_data[, CCI_no_asthma_COPD := ifelse(!is.na(CCI_no_asthma_COPD), CCI_no_asthma_COPD, 0)]
olin_data[, CCI_no_respiratory := ifelse(!is.na(CCI_no_respiratory), CCI_no_respiratory, 0)]
olin_data[, CCI_CHF := ifelse(!is.na(CCI_CHF), CCI_CHF, 0)]
olin_data[, CCI_PVD := ifelse(!is.na(CCI_PVD), CCI_PVD, 0)]
olin_data[, CCI_CVD := ifelse(!is.na(CCI_CVD), CCI_CVD, 0)]
olin_data[, CCI_RD := ifelse(!is.na(CCI_RD), CCI_RD, 0)]
olin_data[, CCI_Dementia := ifelse(!is.na(CCI_Dementia), CCI_Dementia, 0)]
olin_data[, CCI_Hemiplegia := ifelse(!is.na(CCI_Hemiplegia), CCI_Hemiplegia, 0)]
olin_data[, CCI_Diabetes_without_chronic_complication := ifelse(!is.na(CCI_Diabetes_without_chronic_complication), CCI_Diabetes_without_chronic_complication, 0)]
olin_data[, CCI_Diabetes_with_chronic_complication := ifelse(!is.na(CCI_Diabetes_with_chronic_complication), CCI_Diabetes_with_chronic_complication, 0)]
olin_data[, CCI_Renal_disease := ifelse(!is.na(CCI_Renal_disease), CCI_Renal_disease, 0)]
olin_data[, CCI_Mild_liver_disease := ifelse(!is.na(CCI_Mild_liver_disease), CCI_Mild_liver_disease, 0)]
olin_data[, CCI_Severe_liver_disease := ifelse(!is.na(CCI_Severe_liver_disease), CCI_Severe_liver_disease, 0)]
olin_data[, CCI_Peptic_ulcer_disease := ifelse(!is.na(CCI_Peptic_ulcer_disease), CCI_Peptic_ulcer_disease, 0)]
olin_data[, CCI_Malignancy := ifelse(!is.na(CCI_Malignancy), CCI_Malignancy, 0)]
olin_data[, CCI_Metastatic_solid_tumor := ifelse(!is.na(CCI_Metastatic_solid_tumor), CCI_Metastatic_solid_tumor, 0)]
olin_data[, CCI_Aids := ifelse(!is.na(CCI_Aids), CCI_Aids, 0)]
olin_data[, CCI_Asthma := ifelse(!is.na(CCI_Asthma), CCI_Asthma, 0)]
olin_data[, CCI_COPD := ifelse(!is.na(CCI_COPD), CCI_COPD, 0)]
olin_data[, CCI_Chronic_other_pulmonary_disease := ifelse(!is.na(CCI_Chronic_other_pulmonary_disease), CCI_Chronic_other_pulmonary_disease, 0)]

# MERGE WSAS AND OLIN DATA
# merge wsas and olin data
dt_working_comorbidity <- rbind(wsas_data, olin_data)
print(dim(dt_working_comorbidity))
print(colnames(dt_working_comorbidity))

# set comorbidity_asthma to 1 if either CCI_Asthma or comorbidity_asthma is 1
print(table(dt_working_comorbidity$CCI_Asthma, useNA = 'always'))
dt_working_comorbidity$comorbidity_asthma <- ifelse(dt_working_comorbidity$CCI_Asthma == 1 | dt_working_comorbidity$comorbidity_asthma == 1, 1, 0)
print(table(dt_working_comorbidity$comorbidity_asthma, useNA = 'always'))
# set comorbidity_copd to 1 if either CCI_COPD or comorbidity_copd is 1
dt_working_comorbidity$comorbidity_copd <- ifelse(dt_working_comorbidity$CCI_COPD == 1 | dt_working_comorbidity$comorbidity_copd == 1, 1, 0)

# save the data
if (raw_or_pooled_imputed == 'raw') {
    save(dt_working_comorbidity, file = paste0(folder_path, 'output/rda/', 'raw-data--comorbidity', '.Rda'))
} else {
    save(dt_working_comorbidity, file = paste0(folder_path, 'output/rda/', 'imputed-data--pooled--comorbidity', '.Rda'))
}



# tabulate comorbidity data
# select only the relevant columns (comorbidity)
table_data <- dt_working_comorbidity %>% select(CCI, CCI_no_asthma_COPD, CCI_no_respiratory, CCI_CHF, CCI_PVD, CCI_CVD, CCI_RD, CCI_Dementia, CCI_Hemiplegia, CCI_Diabetes_without_chronic_complication, CCI_Diabetes_with_chronic_complication, CCI_Renal_disease, CCI_Mild_liver_disease, CCI_Severe_liver_disease, CCI_Peptic_ulcer_disease, CCI_Malignancy, CCI_Metastatic_solid_tumor, CCI_Aids, CCI_Asthma, CCI_COPD, CCI_Chronic_other_pulmonary_disease)
# make levels 0, 1-2, and ≥3 for the CCI columns
table_data$CCI <- cut(table_data$CCI, breaks = c(-1, 0, 2, 100), labels = c('0', '1-2', '≥3'))
table_data$CCI_no_asthma_COPD <- cut(table_data$CCI_no_asthma_COPD, breaks = c(-1, 0, 2, 100), labels = c('0', '1-2', '≥3'))
table_data$CCI_no_respiratory <- cut(table_data$CCI_no_respiratory, breaks = c(-1, 0, 2, 100), labels = c('0', '1-2', '≥3'))
# make all columns factor
table_data <- table_data %>% mutate_if(is.numeric, as.factor)
table <- table_data %>%
    gtsummary::tbl_summary(
        ) %>%
    bold_labels() %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = paste0(folder_path, 'output/docx/', 'characteristics--comorbidities', '.docx'))