# Internet Anonymity: Pew: Privacy.Importance regression:: Anonymity2
bdanalytics  

**  **    
**Date: (Fri) Jun 12, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/AnonymityPoll.csv  
    New:        <newdt_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:
- Organization:
    - Categorize by chunk
    - Priority criteria:
        0. Ease of change
        1. Impacts report
        2. Cleans innards
        3. Bug report
        
- all chunks:
    - at chunk-end rm(!glb_<var>)
    
- manage.missing.data chunk:
    - cleaner way to manage re-splitting of training vs. new entity

- extract.features chunk:
    - Add n-grams for glb_txt_vars
        - "RTextTools", "tau", "RWeka", and "textcat" packages
    - Convert user-specified mutate code to config specs
    
- fit.models chunk:
    - Prediction accuracy scatter graph:
    -   Add tiles (raw vs. PCA)
    -   Use shiny for drop-down of "important" features
    -   Use plot.ly for interactive plots ?
    
    - Change .fit suffix of model metrics to .mdl if it's data independent (e.g. AIC, Adj.R.Squared - is it truly data independent ?, etc.)
    - move model_type parameter to myfit_mdl before indep_vars_vctr (keep all model_* together)
    - create a custom model for rpart that has minbucket as a tuning parameter
    - varImp for randomForest crashes in caret version:6.0.41 -> submit bug report

- Probability handling for multinomials vs. desired binomial outcome
-   ROCR currently supports only evaluation of binary classification tasks (version 1.0.7)
-   extensions toward multiclass classification are scheduled for the next release

- Skip trControl.method="cv" for dummy classifier ?
- Add custom model to caret for a dummy (baseline) classifier (binomial & multinomial) that generates proba/outcomes which mimics the freq distribution of glb_rsp_var values; Right now glb_dmy_glm_mdl always generates most frequent outcome in training data
- glm_dmy_mdl should use the same method as glm_sel_mdl until custom dummy classifer is implemented

- fit.all.training chunk:
    - myplot_prediction_classification: displays 'x' instead of '+' when there are no prediction errors 
- Compare glb_sel_mdl vs. glb_fin_mdl:
    - varImp
    - Prediction differences (shd be minimal ?)

- Move glb_analytics_diag_plots to mydsutils.R: (+) Easier to debug (-) Too many glb vars used
- Add print(ggplot.petrinet(glb_analytics_pn) + coord_flip()) at the end of every major chunk
- Parameterize glb_analytics_pn
- Move glb_impute_missing_data to mydsutils.R: (-) Too many glb vars used; glb_<>_df reassigned
- Replicate myfit_mdl_classification features in myfit_mdl_regression
- Do non-glm methods handle interaction terms ?
- f-score computation for classifiers should be summation across outcomes (not just the desired one ?)
- Add accuracy computation to glb_dmy_mdl in predict.data.new chunk
- Why does splitting fit.data.training.all chunk into separate chunks add an overhead of ~30 secs ? It's not rbind b/c other chunks have lower elapsed time. Is it the number of plots ?
- Incorporate code chunks in print_sessionInfo
- Test against 
    - projects in github.com/bdanalytics
    - lectures in jhu-datascience track

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/myscript.R")
source("~/Dropbox/datascience/R/mydsutils.R")
```

```
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
source("~/Dropbox/datascience/R/myplclust.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(doMC))
registerDoMC(4) # max(length(glb_txt_vars), glb_n_cv_folds) + 1
#packageVersion("snow")

#require(sos); findFn("cosine", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_trnng_url <- "https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/AnonymityPoll.csv"
glb_newdt_url <- "<newdt_url>"
glb_out_pfx <- "Anonymity2_"
glb_save_envir <- FALSE # or TRUE

glb_is_separate_newent_dataset <- FALSE    # or TRUE
glb_split_entity_newent_datasets <- TRUE   # or FALSE
glb_split_newdata_method <- "sample"          # "condition" or "sample" or "copy"
glb_split_newdata_condition <- NULL # or "is.na(<var>)"; "<var> <condition_operator> <value>"
glb_split_newdata_size_ratio <- 0.3               # > 0 & < 1
glb_split_sample.seed <- 123               # or any integer
glb_drop_vars <- c(NULL) # or c("<col_name>")

glb_max_fitent_obs <- NULL # or any integer                         
glb_is_regression <- TRUE; glb_is_classification <- FALSE; glb_is_binomial <- TRUE

glb_rsp_var_raw <- "Privacy.Importance"

# for classification, the response variable has to be a factor
glb_rsp_var <- glb_rsp_var_raw # or "Privacy.Importance.fctr"

# if the response factor is based on numbers/logicals e.g (0/1 OR TRUE/FALSE vs. "A"/"B"), 
#   or contains spaces (e.g. "Not in Labor Force")
#   caret predict(..., type="prob") crashes
glb_map_rsp_raw_to_var <- NULL # or function(raw) {
    #relevel(factor(ifelse(raw == 1, "Y", "N")), as.factor(c("Y", "N")), ref="N")
    #as.factor(paste0("B", raw))
    #as.factor(gsub(" ", "\\.", raw))    
#}
#glb_map_rsp_raw_to_var(c(1, 1, 0, 0, 0))

glb_map_rsp_var_to_raw <- NULL # or function(var) {
    #as.numeric(var) - 1
    #as.numeric(var)
    #gsub("\\.", " ", levels(var)[as.numeric(var)])
    #c(" <=50K", " >50K")[as.numeric(var)]
    #c(FALSE, TRUE)[as.numeric(var)]
#}
#glb_map_rsp_var_to_raw(glb_map_rsp_raw_to_var(c(1, 1, 0, 0, 0)))

if ((glb_rsp_var != glb_rsp_var_raw) & is.null(glb_map_rsp_raw_to_var))
    stop("glb_map_rsp_raw_to_var function expected")
glb_rsp_var_out <- paste0(glb_rsp_var, ".predict.") # model_id is appended later

# List info gathered for various columns
# <col_name>:   <description>; <notes>
# Internet.Use: A binary variable indicating if the interviewee uses the Internet, at least occasionally (equals 1 if the interviewee uses the Internet, and equals 0 if the interviewee does not use the Internet).
# Smartphone: A binary variable indicating if the interviewee has a smartphone (equals 1 if they do have a smartphone, and equals 0 if they don't have a smartphone).
# Sex: Male or Female.
# Age: Age in years.
# State: State of residence of the interviewee.
# Region: Census region of the interviewee (Midwest, Northeast, South, or West).
# Conservativeness: Self-described level of conservativeness of interviewee, from 1 (very liberal) to 5 (very conservative).
# Info.On.Internet: Number of the following items this interviewee believes to be available on the Internet for others to see: (1) Their email address; (2) Their home address; (3) Their home phone number; (4) Their cell phone number; (5) The employer/company they work for; (6) Their political party or political affiliation; (7) Things they've written that have their name on it; (8) A photo of them; (9) A video of them; (10) Which groups or organizations they belong to; and (11) Their birth date.
# Worry.About.Info: A binary variable indicating if the interviewee worries about how much information is available about them on the Internet (equals 1 if they worry, and equals 0 if they don't worry).
# Privacy.Importance: A score from 0 (privacy is not too important) to 100 (privacy is very important), which combines the degree to which they find privacy important in the following: (1) The websites they browse; (2) Knowledge of the place they are located when they use the Internet; (3) The content and files they download; (4) The times of day they are online; (5) The applications or programs they use; (6) The searches they perform; (7) The content of their email; (8) The people they exchange email with; and (9) The content of their online chats or hangouts with others.
# Anonymity.Possible: A binary variable indicating if the interviewee thinks it's possible to use the Internet anonymously, meaning in such a way that online activities can't be traced back to them (equals 1 if he/she believes you can, and equals 0 if he/she believes you can't).
# Tried.Masking.Identity: A binary variable indicating if the interviewee has ever tried to mask his/her identity when using the Internet (equals 1 if he/she has tried to mask his/her identity, and equals 0 if he/she has not tried to mask his/her identity).
# Privacy.Laws.Effective: A binary variable indicating if the interviewee believes United States law provides reasonable privacy protection for Internet users (equals 1 if he/she believes it does, and equals 0 if he/she believes it doesn't).

# If multiple vars are parts of id, consider concatenating them to create one id var
# If glb_id_var == NULL, ".rownames <- row.names()" is the default
glb_id_var <- NULL # or c("<var1>")
glb_category_vars <- NULL # or c("<var1>", "<var2>")

glb_map_vars <- NULL # or c("<var1>", "<var2>")
glb_map_urls <- list();
# glb_map_urls[["<var1>"]] <- "<var1.url>"

glb_assign_pairs_lst <- NULL; 
glb_assign_pairs_lst[["Internet.Use"]] <- list(from=c(NA),
                                                 to=c(0))
glb_assign_vars <- names(glb_assign_pairs_lst)

glb_transform_lst <- NULL;
glb_transform_lst[["Age"]] <- list(
    mapfn=function(raw) { tfr_raw <- as.character(cut(raw, 5)); 
                          tfr_raw[is.na(tfr_raw)] <- "NA.my";
                          return(as.factor(tfr_raw)) }
    , sfx=".my.fctr")
# mapfn(glb_allobs_df$Age)
glb_transform_lst[["Anonymity.Possible"]] <- list(
    mapfn=function(raw) { tfr_raw <- as.character(raw); tfr_raw[is.na(tfr_raw)] <- "NA.my";
                            return(as.factor(tfr_raw)) }
    , sfx=".my.fctr")
# mapfn(glb_allobs_df$Anonymity.Possible)
glb_transform_lst[["Conservativeness"]] <- list(
    mapfn=function(raw) { tfr_raw <- as.character(raw); tfr_raw[is.na(tfr_raw)] <- "NA.my";
                            return(as.factor(tfr_raw)) }
    , sfx=".my.fctr")
# mapfn(glb_allobs_df$Conservativeness)
glb_transform_lst[["Info.On.Internet"]] <- list(
    mapfn=function(raw) { tfr_raw <- as.character(raw); tfr_raw[is.na(tfr_raw)] <- "NA.my";
                            return(as.factor(tfr_raw)) }
    , sfx=".my.fctr")
# mapfn(glb_allobs_df$Info.On.Internet)
glb_transform_lst[["Privacy.Laws.Effective"]] <- list(
    mapfn=function(raw) { tfr_raw <- as.character(raw); tfr_raw[is.na(tfr_raw)] <- "NA.my";
                            return(as.factor(tfr_raw)) }
    , sfx=".my.fctr")
# mapfn(glb_allobs_df$Privacy.Laws.Effective)
glb_transform_lst[["Smartphone"]] <- list(
    mapfn=function(raw) { tfr_raw <- as.character(raw); tfr_raw[is.na(tfr_raw)] <- "NA.my";
                            return(as.factor(tfr_raw)) }
    , sfx=".my.fctr")
# mapfn(glb_allobs_df$Smartphone)
glb_transform_lst[["Tried.Masking.Identity"]] <- list(
    mapfn=function(raw) { tfr_raw <- as.character(raw); tfr_raw[is.na(tfr_raw)] <- "NA.my";
                            return(as.factor(tfr_raw)) }
    , sfx=".my.fctr")
# mapfn(glb_allobs_df$Tried.Masking.Identity)
glb_transform_lst[["Worry.About.Info"]] <- list(
    mapfn=function(raw) { tfr_raw <- as.character(raw); tfr_raw[is.na(tfr_raw)] <- "NA.my";
                            return(as.factor(tfr_raw)) }
    , sfx=".my.fctr")
# mapfn(glb_allobs_df$Worry.About.Info)
# Add logs of numerics that are not distributed normally ->  do automatically ???
glb_transform_vars <- names(glb_transform_lst)

glb_date_vars <- NULL # or c("<date_var>")
glb_date_fmts <- list(); #glb_date_fmts[["<date_var>"]] <- "%m/%e/%y"
glb_date_tzs <- list();  #glb_date_tzs[["<date_var>"]] <- "America/New_York"
#grep("America/New", OlsonNames(), value=TRUE)

glb_txt_vars <- NULL # or c("<txt_var1>", "<txt_var2>")   
#Sys.setlocale("LC_ALL", "C") # For english

glb_append_stop_words <- list()
# Remember to use unstemmed words
#orderBy(~ -cor.y.abs, subset(glb_feats_df, grepl("[HSA]\\.T\\.", id) & !is.na(cor.high.X)))
#dsp_obs(Headline.contains="polit")
#subset(glb_allobs_df, H.T.compani > 0)[, c("UniqueID", "Headline", "H.T.compani")]
# glb_append_stop_words[["<txt_var1>"]] <- c(NULL
# #                             ,"<word1>" # <reason1>
#                             )
#subset(glb_allobs_df, S.T.newyorktim > 0)[, c("UniqueID", "Snippet", "S.T.newyorktim")]
#glb_txt_lst[["Snippet"]][which(glb_allobs_df$UniqueID %in% c(8394, 8317, 8339, 8350, 8307))]

glb_important_terms <- list()
# Remember to use stemmed terms 

glb_sprs_thresholds <- NULL # or c(0.988, 0.970, 0.970) # Generates 29, 22, 22 terms
# Properties:
#   numrows(glb_feats_df) << numrows(glb_fitobs_df)
#   Select terms that appear in at least 0.2 * O(FP/FN(glb_OOBobs_df))
#       numrows(glb_OOBobs_df) = 1.1 * numrows(glb_newobs_df)
names(glb_sprs_thresholds) <- glb_txt_vars

# User-specified exclusions  
glb_exclude_vars_as_features <- c("State.fctr") # or c("<var_name>") 
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)

# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c(NULL)) # or c("<col_name>")

glb_impute_na_data <- FALSE # or TRUE
glb_mice_complete.seed <- 144 # or any integer

glb_cluster <- FALSE # or TRUE

glb_interaction_only_features <- NULL # or ???

glb_models_lst <- list(); glb_models_df <- data.frame()
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "bayesglm", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        glb_models_method_vctr <- c("glm", "bayesglm", "rpart", "rf") else  
        glb_models_method_vctr <- c("rpart", "rf")

# Baseline prediction model feature(s)
glb_Baseline_mdl_var <- NULL # or c("<col_name>")

glb_model_metric_terms <- NULL # or matrix(c(
#                               0,1,2,3,4,
#                               2,0,1,2,3,
#                               4,2,0,1,2,
#                               6,4,2,0,1,
#                               8,6,4,2,0
#                           ), byrow=TRUE, nrow=5)
glb_model_metric <- NULL # or "<metric_name>"
glb_model_metric_maximize <- NULL # or FALSE (TRUE is not the default for both classification & regression) 
glb_model_metric_smmry <- NULL # or function(data, lev=NULL, model=NULL) {
#     confusion_mtrx <- t(as.matrix(confusionMatrix(data$pred, data$obs)))
#     #print(confusion_mtrx)
#     #print(confusion_mtrx * glb_model_metric_terms)
#     metric <- sum(confusion_mtrx * glb_model_metric_terms) / nrow(data)
#     names(metric) <- glb_model_metric
#     return(metric)
# }

glb_tune_models_df <- 
   rbind(
    #data.frame(parameter="cp", min=0.00005, max=0.00005, by=0.000005),
                            #seq(from=0.01,  to=0.01, by=0.01)
    #data.frame(parameter="mtry",  min=080, max=100, by=10),
    #data.frame(parameter="mtry",  min=08, max=10, by=1),    
    data.frame(parameter="dummy", min=2, max=4, by=1)
        ) 
# or NULL
glb_n_cv_folds <- 3 # or NULL

glb_clf_proba_threshold <- NULL # 0.5

# Model selection criteria
if (glb_is_regression)
    glb_model_evl_criteria <- c("min.RMSE.OOB", "max.R.sq.OOB", "max.Adj.R.sq.fit")
if (glb_is_classification) {
    if (glb_is_binomial)
        glb_model_evl_criteria <- 
            c("max.Accuracy.OOB", "max.auc.OOB", "max.Kappa.OOB", "min.aic.fit") else
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB")
}

glb_sel_mdl_id <- NULL # or "<model_id_prefix>.<model_method>"
glb_fin_mdl_id <- glb_sel_mdl_id # or "Final"

# Depict process
glb_analytics_pn <- petrinet(name="glb_analytics_pn",
                        trans_df=data.frame(id=1:6,
    name=c("data.training.all","data.new",
           "model.selected","model.final",
           "data.training.all.prediction","data.new.prediction"),
    x=c(   -5,-5,-15,-25,-25,-35),
    y=c(   -5, 5,  0,  0, -5,  5)
                        ),
                        places_df=data.frame(id=1:4,
    name=c("bgn","fit.data.training.all","predict.data.new","end"),
    x=c(   -0,   -20,                    -30,               -40),
    y=c(    0,     0,                      0,                 0),
    M0=c(   3,     0,                      0,                 0)
                        ),
                        arcs_df=data.frame(
    begin=c("bgn","bgn","bgn",        
            "data.training.all","model.selected","fit.data.training.all",
            "fit.data.training.all","model.final",    
            "data.new","predict.data.new",
            "data.training.all.prediction","data.new.prediction"),
    end  =c("data.training.all","data.new","model.selected",
            "fit.data.training.all","fit.data.training.all","model.final",
            "data.training.all.prediction","predict.data.new",
            "predict.data.new","data.new.prediction",
            "end","end")
                        ))
#print(ggplot.petrinet(glb_analytics_pn))
print(ggplot.petrinet(glb_analytics_pn) + coord_flip())
```

```
## Loading required package: grid
```

![](Pew_Anonymity2_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor   bgn end elapsed
## 1 import.data          1          0 9.118  NA      NA
```

## Step `1.0: import data`
#### chunk option: eval=<r condition>

```r
glb_trnobs_df <- myimport_data(url=glb_trnng_url, comment="glb_trnobs_df", 
                                force_header=TRUE)
```

```
## [1] "Reading file ./data/AnonymityPoll.csv..."
## [1] "dimensions of data in ./data/AnonymityPoll.csv: 1,002 rows x 13 cols"
##   Internet.Use Smartphone    Sex Age          State    Region
## 1            1          0   Male  62  Massachusetts Northeast
## 2            1          0   Male  45 South Carolina     South
## 3            0          1 Female  70     New Jersey Northeast
## 4            1          0   Male  70        Georgia     South
## 5            0         NA Female  80        Georgia     South
## 6            1          1   Male  49      Tennessee     South
##   Conservativeness Info.On.Internet Worry.About.Info Privacy.Importance
## 1                4                0                1          100.00000
## 2                1                1                0            0.00000
## 3                4                0                0                 NA
## 4                4                3                1           88.88889
## 5                4               NA               NA                 NA
## 6                4                6                0           88.88889
##   Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 1                  0                      0                      0
## 2                  1                      0                      1
## 3                  0                      0                     NA
## 4                  1                      0                      0
## 5                 NA                     NA                     NA
## 6                  1                      1                      0
##     Internet.Use Smartphone    Sex Age       State    Region
## 35             1          0 Female  74     Florida     South
## 153            0          0 Female  77      Oregon      West
## 511            1          1   Male  19    Virginia     South
## 729            0          1   Male  52 Connecticut Northeast
## 734            1          1   Male  26   Wisconsin   Midwest
## 990            1          1 Female  36    Missouri   Midwest
##     Conservativeness Info.On.Internet Worry.About.Info Privacy.Importance
## 35                 3                0                0               6.25
## 153                3               NA               NA                 NA
## 511                3                7                0             100.00
## 729                2                1                0              50.00
## 734                5                2                0             100.00
## 990                3                6                0             100.00
##     Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 35                   0                      0                      0
## 153                 NA                     NA                     NA
## 511                  1                      0                      0
## 729                  1                      0                      1
## 734                  0                      0                      0
## 990                  0                      0                      1
##      Internet.Use Smartphone    Sex Age      State Region Conservativeness
## 997             1          1   Male  29 California   West                3
## 998             1          1 Female  57       Utah   West                4
## 999             0         NA   Male  29   Colorado   West                3
## 1000            1          1   Male  22 California   West                4
## 1001            0          0 Female  63 California   West                4
## 1002            1          1 Female  26      Texas  South                3
##      Info.On.Internet Worry.About.Info Privacy.Importance
## 997                 7                1           77.77778
## 998                 7                1           27.77778
## 999                NA               NA                 NA
## 1000                6                0           11.11111
## 1001               NA               NA                 NA
## 1002                3                1           55.55556
##      Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 997                   1                      1                      1
## 998                   0                      0                      1
## 999                  NA                     NA                      0
## 1000                  0                      0                      1
## 1001                 NA                     NA                      1
## 1002                  0                      0                      0
## 'data.frame':	1002 obs. of  13 variables:
##  $ Internet.Use          : int  1 1 0 1 0 1 1 0 0 1 ...
##  $ Smartphone            : int  0 0 1 0 NA 1 0 0 NA 0 ...
##  $ Sex                   : chr  "Male" "Male" "Female" "Male" ...
##  $ Age                   : int  62 45 70 70 80 49 52 76 75 76 ...
##  $ State                 : chr  "Massachusetts" "South Carolina" "New Jersey" "Georgia" ...
##  $ Region                : chr  "Northeast" "South" "Northeast" "South" ...
##  $ Conservativeness      : int  4 1 4 4 4 4 3 3 4 4 ...
##  $ Info.On.Internet      : int  0 1 0 3 NA 6 3 NA NA 0 ...
##  $ Worry.About.Info      : int  1 0 0 1 NA 0 1 NA NA 0 ...
##  $ Privacy.Importance    : num  100 0 NA 88.9 NA ...
##  $ Anonymity.Possible    : int  0 1 0 1 NA 1 0 NA NA 1 ...
##  $ Tried.Masking.Identity: int  0 0 0 0 NA 1 0 NA NA 0 ...
##  $ Privacy.Laws.Effective: int  0 1 NA 0 NA 0 1 NA 0 1 ...
##  - attr(*, "comment")= chr "glb_trnobs_df"
## NULL
```

```r
# glb_trnobs_df <- data.frame()
# for (symbol in c("Boeing", "CocaCola", "GE", "IBM", "ProcterGamble")) {
#     sym_trnobs_df <- 
#         myimport_data(url=gsub("IBM", symbol, glb_trnng_url), comment="glb_trnobs_df", 
#                                     force_header=TRUE)
#     sym_trnobs_df$Symbol <- symbol
#     glb_trnobs_df <- myrbind_df(glb_trnobs_df, sym_trnobs_df)
# }
                                
if (glb_is_separate_newent_dataset) {
    glb_newobs_df <- myimport_data(url=glb_newdt_url, comment="glb_newobs_df", 
                                   force_header=TRUE)
    
    # To make plots / stats / checks easier in chunk:inspectORexplore.data
    glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df); 
    comment(glb_allobs_df) <- "glb_allobs_df"
} else {
    glb_allobs_df <- glb_trnobs_df; comment(glb_allobs_df) <- "glb_allobs_df"
    if (!glb_split_entity_newent_datasets) {
        stop("Not implemented yet") 
        glb_newobs_df <- glb_trnobs_df[sample(1:nrow(glb_trnobs_df),
                                          max(2, nrow(glb_trnobs_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=glb_split_newdata_condition)))
            glb_trnobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
                                      SplitRatio=(1-glb_split_newdata_size_ratio))
                glb_newobs_df <- glb_trnobs_df[!split, ] 
                glb_trnobs_df <- glb_trnobs_df[split ,]
        } else if (glb_split_newdata_method == "copy") {  
            glb_trnobs_df <- glb_allobs_df
            comment(glb_trnobs_df) <- "glb_trnobs_df"
            glb_newobs_df <- glb_allobs_df
            comment(glb_newobs_df) <- "glb_newobs_df"
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample', 'copy')")   

    comment(glb_newobs_df) <- "glb_newobs_df"
    myprint_df(glb_newobs_df)
    str(glb_newobs_df)

    if (glb_split_entity_newent_datasets) {
        myprint_df(glb_trnobs_df)
        str(glb_trnobs_df)        
    }
}         
```

```
## Loading required package: caTools
```

```
##   Internet.Use Smartphone    Sex Age          State    Region
## 3            0          1 Female  70     New Jersey Northeast
## 4            1          0   Male  70        Georgia     South
## 5            0         NA Female  80        Georgia     South
## 7            1          0 Female  52       Michigan   Midwest
## 8            0          0 Female  76       New York Northeast
## 9            0         NA   Male  75 North Carolina     South
##   Conservativeness Info.On.Internet Worry.About.Info Privacy.Importance
## 3                4                0                0                 NA
## 4                4                3                1           88.88889
## 5                4               NA               NA                 NA
## 7                3                3                1           33.33333
## 8                3               NA               NA                 NA
## 9                4               NA               NA                 NA
##   Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 3                  0                      0                     NA
## 4                  1                      0                      0
## 5                 NA                     NA                     NA
## 7                  0                      0                      1
## 8                 NA                     NA                     NA
## 9                 NA                     NA                      0
##     Internet.Use Smartphone    Sex Age         State    Region
## 13             0          0   Male  72      New York Northeast
## 179            0          0 Female  90      Michigan   Midwest
## 230            0          0 Female  80      Kentucky     South
## 499            0          0 Female  72      Kentucky     South
## 905            1          1 Female  61 Massachusetts Northeast
## 923            1          0   Male  55    California      West
##     Conservativeness Info.On.Internet Worry.About.Info Privacy.Importance
## 13                 5               NA               NA                 NA
## 179                4               NA               NA                 NA
## 230               NA               NA               NA                 NA
## 499                3               NA               NA                 NA
## 905                3                6                1           61.11111
## 923                4                6                0           33.33333
##     Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 13                  NA                     NA                      1
## 179                 NA                     NA                     NA
## 230                 NA                     NA                     NA
## 499                 NA                     NA                      0
## 905                  0                      0                      1
## 923                  1                      0                      0
##      Internet.Use Smartphone    Sex Age      State Region Conservativeness
## 988             1          1   Male  45  Louisiana  South                3
## 994             1          1   Male  18   Oklahoma  South                3
## 996             1          1 Female  30    Arizona   West                2
## 997             1          1   Male  29 California   West                3
## 999             0         NA   Male  29   Colorado   West                3
## 1001            0          0 Female  63 California   West                4
##      Info.On.Internet Worry.About.Info Privacy.Importance
## 988                 2                1          100.00000
## 994                 7                0           44.44444
## 996                 5                0           94.44444
## 997                 7                1           77.77778
## 999                NA               NA                 NA
## 1001               NA               NA                 NA
##      Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 988                   0                      0                      0
## 994                   0                      0                      0
## 996                   1                      0                      0
## 997                   1                      1                      1
## 999                  NA                     NA                      0
## 1001                 NA                     NA                      1
## 'data.frame':	446 obs. of  13 variables:
##  $ Internet.Use          : int  0 1 0 1 0 0 1 0 0 0 ...
##  $ Smartphone            : int  1 0 NA 0 0 NA 0 0 0 0 ...
##  $ Sex                   : chr  "Female" "Male" "Female" "Female" ...
##  $ Age                   : int  70 70 80 52 76 75 76 69 72 63 ...
##  $ State                 : chr  "New Jersey" "Georgia" "Georgia" "Michigan" ...
##  $ Region                : chr  "Northeast" "South" "South" "Midwest" ...
##  $ Conservativeness      : int  4 4 4 3 3 4 4 4 5 3 ...
##  $ Info.On.Internet      : int  0 3 NA 3 NA NA 0 NA NA NA ...
##  $ Worry.About.Info      : int  0 1 NA 1 NA NA 0 NA NA NA ...
##  $ Privacy.Importance    : num  NA 88.9 NA 33.3 NA ...
##  $ Anonymity.Possible    : int  0 1 NA 0 NA NA 1 NA NA NA ...
##  $ Tried.Masking.Identity: int  0 0 NA 0 NA NA 0 NA NA NA ...
##  $ Privacy.Laws.Effective: int  NA 0 NA 1 NA 0 1 0 1 0 ...
##  - attr(*, "comment")= chr "glb_newobs_df"
##    Internet.Use Smartphone    Sex Age          State    Region
## 1             1          0   Male  62  Massachusetts Northeast
## 2             1          0   Male  45 South Carolina     South
## 6             1          1   Male  49      Tennessee     South
## 12            1          1 Female  50       Virginia     South
## 14            1          1 Female  47 North Carolina     South
## 15            1          0   Male  69       New York Northeast
##    Conservativeness Info.On.Internet Worry.About.Info Privacy.Importance
## 1                 4                0                1          100.00000
## 2                 1                1                0            0.00000
## 6                 4                6                0           88.88889
## 12                3                1                0           27.77778
## 14                3                0                0            0.00000
## 15                3                9                0           77.77778
##    Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 1                   0                      0                      0
## 2                   1                      0                      1
## 6                   1                      1                      0
## 12                  0                      0                      0
## 14                  1                      0                      0
## 15                  0                      0                      0
##     Internet.Use Smartphone    Sex Age      State  Region Conservativeness
## 121            1          1 Female  55 California    West                3
## 322            1          1 Female  43   Michigan Midwest                3
## 393            0          1   Male  72 Washington    West                3
## 439            1         NA Female  67     Kansas Midwest                4
## 521            1          1   Male  34  Minnesota Midwest                4
## 815            1          1   Male  46 California    West               NA
##     Info.On.Internet Worry.About.Info Privacy.Importance
## 121                6                1           61.11111
## 322                8                1          100.00000
## 393                2                1          100.00000
## 439                2                0           43.75000
## 521                6                0            0.00000
## 815                7                1          100.00000
##     Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 121                  0                      0                      1
## 322                  0                      0                      1
## 393                  0                      0                      0
## 439                  0                      0                     NA
## 521                  1                      0                      1
## 815                  0                      1                     NA
##      Internet.Use Smartphone    Sex Age      State  Region
## 992             1          1   Male  37      Texas   South
## 993             1          1 Female  63  Wisconsin Midwest
## 995             1          1 Female  55   Colorado    West
## 998             1          1 Female  57       Utah    West
## 1000            1          1   Male  22 California    West
## 1002            1          1 Female  26      Texas   South
##      Conservativeness Info.On.Internet Worry.About.Info Privacy.Importance
## 992                 1                2                0           50.00000
## 993                 2                0                0           83.33333
## 995                 3                3                1           88.88889
## 998                 4                7                1           27.77778
## 1000                4                6                0           11.11111
## 1002                3                3                1           55.55556
##      Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 992                   1                      0                      0
## 993                   0                      0                      1
## 995                   0                      0                      0
## 998                   0                      0                      1
## 1000                  0                      0                      1
## 1002                  0                      0                      0
## 'data.frame':	556 obs. of  13 variables:
##  $ Internet.Use          : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ Smartphone            : int  0 0 1 1 1 0 0 0 0 0 ...
##  $ Sex                   : chr  "Male" "Male" "Male" "Female" ...
##  $ Age                   : int  62 45 49 50 47 69 91 66 61 58 ...
##  $ State                 : chr  "Massachusetts" "South Carolina" "Tennessee" "Virginia" ...
##  $ Region                : chr  "Northeast" "South" "South" "South" ...
##  $ Conservativeness      : int  4 1 4 3 3 3 5 5 3 5 ...
##  $ Info.On.Internet      : int  0 1 6 1 0 9 5 2 1 1 ...
##  $ Worry.About.Info      : int  1 0 0 0 0 0 1 0 0 0 ...
##  $ Privacy.Importance    : num  100 0 88.9 27.8 0 ...
##  $ Anonymity.Possible    : int  0 1 1 0 1 0 NA 1 0 0 ...
##  $ Tried.Masking.Identity: int  0 0 1 0 0 0 0 0 0 0 ...
##  $ Privacy.Laws.Effective: int  0 1 0 0 0 0 0 0 0 0 ...
##  - attr(*, "comment")= chr "glb_trnobs_df"
```

```r
if ((num_nas <- sum(is.na(glb_trnobs_df[, glb_rsp_var_raw]))) > 0)
    stop("glb_trnobs_df$", glb_rsp_var_raw, " contains NAs for ", num_nas, " obs")

if (nrow(glb_trnobs_df) == nrow(glb_allobs_df))
    warning("glb_trnobs_df same as glb_allobs_df")
if (nrow(glb_newobs_df) == nrow(glb_allobs_df))
    warning("glb_newobs_df same as glb_allobs_df")

if (length(glb_drop_vars) > 0) {
    warning("dropping vars: ", paste0(glb_drop_vars, collapse=", "))
    glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), glb_drop_vars)]
    glb_trnobs_df <- glb_trnobs_df[, setdiff(names(glb_trnobs_df), glb_drop_vars)]    
    glb_newobs_df <- glb_newobs_df[, setdiff(names(glb_newobs_df), glb_drop_vars)]    
}

#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Check for duplicates in glb_id_var
if (length(glb_id_var) == 0) {
    warning("using .rownames as identifiers for observations")
    glb_allobs_df$.rownames <- rownames(glb_allobs_df)
    glb_trnobs_df$.rownames <- rownames(glb_trnobs_df)
    glb_newobs_df$.rownames <- rownames(glb_newobs_df)    
    glb_id_var <- ".rownames"
}
```

```
## Warning: using .rownames as identifiers for observations
```

```r
if (sum(duplicated(glb_allobs_df[, glb_id_var, FALSE])) > 0)
    stop(glb_id_var, " duplicated in glb_allobs_df")
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_var)

# Combine trnent & newent into glb_allobs_df for easier manipulation
glb_trnobs_df$.src <- "Train"; glb_newobs_df$.src <- "Test"; 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, ".src")
glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df)
comment(glb_allobs_df) <- "glb_allobs_df"
glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
glb_trnobs_df <- glb_newobs_df <- NULL

glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=TRUE)
```

```
##          label step_major step_minor   bgn   end elapsed
## 1  import.data          1          0 9.118 9.966   0.848
## 2 inspect.data          2          0 9.966    NA      NA
```

## Step `2.0: inspect data`

```r
#print(str(glb_allobs_df))
#View(glb_allobs_df)

dsp_class_dstrb <- function(var) {
    xtab_df <- mycreate_xtab_df(glb_allobs_df, c(".src", var))
    rownames(xtab_df) <- xtab_df$.src
    xtab_df <- subset(xtab_df, select=-.src)
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Performed repeatedly in other chunks
glb_chk_data <- function() {
    # Histogram of predictor in glb_trnobs_df & glb_newobs_df
    print(myplot_histogram(glb_allobs_df, glb_rsp_var_raw) + facet_wrap(~ .src))
    
    if (glb_is_classification) 
        dsp_class_dstrb(var=ifelse(glb_rsp_var %in% names(glb_allobs_df), 
                                   glb_rsp_var, glb_rsp_var_raw))
    mycheck_problem_data(glb_allobs_df)
}
glb_chk_data()
```

![](Pew_Anonymity2_files/figure-html/inspect.data-1.png) 

```
## [1] "numeric data missing in glb_allobs_df: "
##           Internet.Use             Smartphone                    Age 
##                      1                     43                     27 
##       Conservativeness       Info.On.Internet       Worry.About.Info 
##                     62                    210                    212 
##     Privacy.Importance     Anonymity.Possible Tried.Masking.Identity 
##                    215                    249                    218 
## Privacy.Laws.Effective 
##                    108 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##           Internet.Use             Smartphone       Info.On.Internet 
##                    226                    472                    105 
##       Worry.About.Info     Privacy.Importance     Anonymity.Possible 
##                    404                     43                    475 
## Tried.Masking.Identity Privacy.Laws.Effective 
##                    656                    660 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##       Sex     State    Region .rownames 
##         0         0         0         0
```

```r
# Create new features that help diagnostics
if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_allobs_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_allobs_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_allobs_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    if (glb_is_classification) dsp_class_dstrb(glb_rsp_var)
}

#   Convert dates to numbers 
#       typically, dates come in as chars; 
#           so this must be done before converting chars to factors

myextract_dates_df <- function(df, vars, id_vars, rsp_var) {
    keep_feats <- c(NULL)
    for (var in vars) {
        dates_df            <- df[, id_vars, FALSE]        
        dates_df[, rsp_var] <- df[, rsp_var, FALSE]
        #dates_df <- data.frame(.date=strptime(df[, var], "%Y-%m-%d %H:%M:%S"))
        dates_df <- cbind(dates_df, data.frame(.date=strptime(df[, var], 
            glb_date_fmts[[var]], tz=glb_date_tzs[[var]])))
#         print(dates_df[is.na(dates_df$.date), c("ID", "Arrest.fctr", ".date")])
#         print(glb_allobs_df[is.na(dates_df$.date), c("ID", "Arrest.fctr", "Date")])     
#         print(head(glb_allobs_df[grepl("4/7/02 .:..", glb_allobs_df$Date), c("ID", "Arrest.fctr", "Date")]))
#         print(head(strptime(glb_allobs_df[grepl("4/7/02 .:..", glb_allobs_df$Date), "Date"], "%m/%e/%y %H:%M"))
        # Wrong data during EST->EDT transition
#         tmp <- strptime("4/7/02 2:00","%m/%e/%y %H:%M:%S"); print(tmp); print(is.na(tmp))
#         dates_df[dates_df$ID == 2068197, .date] <- tmp
#         grep("(.*?) 2:(.*)", glb_allobs_df[is.na(dates_df$.date), "Date"], value=TRUE)
#         dates_df[is.na(dates_df$.date), ".date"] <- 
#             data.frame(.date=strptime(gsub("(.*?) 2:(.*)", "\\1 3:\\2",
#                 glb_allobs_df[is.na(dates_df$.date), "Date"]), "%m/%e/%y %H:%M"))$.date
        if (sum(is.na(dates_df$.date)) > 0) {
            stop("NA POSIX dates for ", var)
            print(df[is.na(dates_df$.date), c(id_vars, rsp_var, var)])
        }    
        
        .date <- dates_df$.date
        dates_df[, paste0(var, ".POSIX")] <- .date
        dates_df[, paste0(var, ".year")] <- as.numeric(format(.date, "%Y"))
        dates_df[, paste0(var, ".year.fctr")] <- as.factor(format(.date, "%Y")) 
        dates_df[, paste0(var, ".month")] <- as.numeric(format(.date, "%m"))
        dates_df[, paste0(var, ".month.fctr")] <- as.factor(format(.date, "%m"))
        dates_df[, paste0(var, ".date")] <- as.numeric(format(.date, "%d"))
        dates_df[, paste0(var, ".date.fctr")] <- 
            cut(as.numeric(format(.date, "%d")), 5) # by month week  
        dates_df[, paste0(var, ".juliandate")] <- as.numeric(format(.date, "%j"))        
        
        # wkday Sun=0; Mon=1; ...; Sat=6
        dates_df[, paste0(var, ".wkday")] <- as.numeric(format(.date, "%w"))
        dates_df[, paste0(var, ".wkday.fctr")] <- as.factor(format(.date, "%w"))
        
        # Get US Federal Holidays for relevant years
        require(XML)
        doc.html = htmlTreeParse('http://about.usps.com/news/events-calendar/2012-federal-holidays.htm', useInternal = TRUE)
        
#         # Extract all the paragraphs (HTML tag is p, starting at
#         # the root of the document). Unlist flattens the list to
#         # create a character vector.
#         doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
#         # Replace all \n by spaces
#         doc.text = gsub('\\n', ' ', doc.text)
#         # Join all the elements of the character vector into a single
#         # character string, separated by spaces
#         doc.text = paste(doc.text, collapse = ' ')
        
        # parse the tree by tables
        txt <- unlist(strsplit(xpathSApply(doc.html, "//*/table", xmlValue), "\n"))
        # do some clean up with regular expressions
        txt <- grep("day, ", txt, value=TRUE)
        txt <- trimws(gsub("(.*?)day, (.*)", "\\2", txt))
#         txt <- gsub("\t","",txt)
#         txt <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", txt, perl=TRUE)
#         txt <- txt[!(txt %in% c("", "|"))]
        hldays <- strptime(paste(txt, ", 2012", sep=""), "%B %e, %Y")
        dates_df[, paste0(var, ".hlday")] <- 
            ifelse(format(.date, "%Y-%m-%d") %in% hldays, 1, 0)
        
        # NYState holidays 1.9., 13.10., 11.11., 27.11., 25.12.
        
        dates_df[, paste0(var, ".wkend")] <- as.numeric(
            (dates_df[, paste0(var, ".wkday")] %in% c(0, 6)) | 
            dates_df[, paste0(var, ".hlday")] )
        
        dates_df[, paste0(var, ".hour")] <- as.numeric(format(.date, "%H"))
        dates_df[, paste0(var, ".hour.fctr")] <- 
            if (length(unique(vals <- as.numeric(format(.date, "%H")))) <= 1)
                   vals else cut(vals, 3) # by work-shift    
        dates_df[, paste0(var, ".minute")] <- as.numeric(format(.date, "%M")) 
        dates_df[, paste0(var, ".minute.fctr")] <- 
            if (length(unique(vals <- as.numeric(format(.date, "%M")))) <= 1)
                   vals else cut(vals, 4) # by quarter-hours    
        dates_df[, paste0(var, ".second")] <- as.numeric(format(.date, "%S")) 
        dates_df[, paste0(var, ".second.fctr")] <- 
            if (length(unique(vals <- as.numeric(format(.date, "%S")))) <= 1)
                   vals else cut(vals, 4) # by quarter-minutes

        dates_df[, paste0(var, ".day.minutes")] <- 
            60 * dates_df[, paste0(var, ".hour")] + 
                 dates_df[, paste0(var, ".minute")]
        if ((unq_vals_n <- length(unique(dates_df[, paste0(var, ".day.minutes")]))) > 1) {
            max_degree <- min(unq_vals_n, 5)
            dates_df[, paste0(var, ".day.minutes.poly.", 1:max_degree)] <- 
                as.matrix(poly(dates_df[, paste0(var, ".day.minutes")], max_degree))
        } else max_degree <- 0   
        
#         print(gp <- myplot_box(df=dates_df, ycol_names="PubDate.day.minutes", 
#                                xcol_name=rsp_var))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name=".rownames", 
#                         ycol_name="PubDate.day.minutes", colorcol_name=rsp_var))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name="PubDate.day.minutes.poly.1", colorcol_name=rsp_var))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.day.minutes", 
#                         ycol_name="PubDate.day.minutes.poly.4", colorcol_name=rsp_var))
# 
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name="PubDate.day.minutes", colorcol_name=rsp_var, smooth=TRUE))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name="PubDate.day.minutes.poly.4", colorcol_name=rsp_var, smooth=TRUE))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name=c("PubDate.day.minutes", "PubDate.day.minutes.poly.4"), 
#                         colorcol_name=rsp_var))
        
#         print(gp <- myplot_scatter(df=subset(dates_df, Popular.fctr=="Y"), 
#                                    xcol_name=paste0(var, ".juliandate"), 
#                         ycol_name=paste0(var, ".day.minutes", colorcol_name=rsp_var))
#         print(gp <- myplot_box(df=dates_df, ycol_names=paste0(var, ".hour"), 
#                                xcol_name=rsp_var))
#         print(gp <- myplot_bar(df=dates_df, ycol_names=paste0(var, ".hour.fctr"), 
#                                xcol_name=rsp_var, 
#                                colorcol_name=paste0(var, ".hour.fctr")))                
        keep_feats <- paste(var, 
            c(".POSIX", ".year.fctr", ".month.fctr", ".date.fctr", ".wkday.fctr", 
              ".wkend", ".hour.fctr", ".minute.fctr", ".second.fctr"), sep="")
        if (max_degree > 0)
            keep_feats <- union(keep_feats, paste(var, 
              paste0(".day.minutes.poly.", 1:max_degree), sep=""))
        keep_feats <- intersect(keep_feats, names(dates_df))        
    }
    #myprint_df(dates_df)
    return(dates_df[, keep_feats])
}

if (!is.null(glb_date_vars)) {
    glb_allobs_df <- cbind(glb_allobs_df, 
        myextract_dates_df(df=glb_allobs_df, vars=glb_date_vars, 
                           id_vars=glb_id_var, rsp_var=glb_rsp_var))
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          paste(glb_date_vars, c("", ".POSIX"), sep=""))

    for (feat in glb_date_vars) {
        glb_allobs_df <- orderBy(reformulate(paste0(feat, ".POSIX")), glb_allobs_df)
#         print(myplot_scatter(glb_allobs_df, xcol_name=paste0(feat, ".POSIX"),
#                              ycol_name=glb_rsp_var, colorcol_name=glb_rsp_var))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[, paste0(feat, ".POSIX")] >=
                                               strptime("2012-12-01", "%Y-%m-%d"), ], 
                             xcol_name=paste0(feat, ".POSIX"),
                             ycol_name=glb_rsp_var, colorcol_name=paste0(feat, ".wkend")))

        # Create features that measure the gap between previous timestamp in the data
        require(zoo)
        z <- zoo(as.numeric(as.POSIXlt(glb_allobs_df[, paste0(feat, ".POSIX")])))
        glb_allobs_df[, paste0(feat, ".zoo")] <- z
        print(head(glb_allobs_df[, c(glb_id_var, feat, paste0(feat, ".zoo"))]))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[,  paste0(feat, ".POSIX")] >
                                            strptime("2012-10-01", "%Y-%m-%d"), ], 
                            xcol_name=paste0(feat, ".zoo"), ycol_name=glb_rsp_var,
                            colorcol_name=glb_rsp_var))
        b <- zoo(, seq(nrow(glb_allobs_df)))
        
        last1 <- as.numeric(merge(z-lag(z, -1), b, all=TRUE)); last1[is.na(last1)] <- 0
        glb_allobs_df[, paste0(feat, ".last1.log")] <- log(1 + last1)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last1.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last1.log"), 
                               xcol_name=glb_rsp_var))
        
        last10 <- as.numeric(merge(z-lag(z, -10), b, all=TRUE)); last10[is.na(last10)] <- 0
        glb_allobs_df[, paste0(feat, ".last10.log")] <- log(1 + last10)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last10.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last10.log"), 
                               xcol_name=glb_rsp_var))
        
        last100 <- as.numeric(merge(z-lag(z, -100), b, all=TRUE)); last100[is.na(last100)] <- 0
        glb_allobs_df[, paste0(feat, ".last100.log")] <- log(1 + last100)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last100.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last100.log"), 
                               xcol_name=glb_rsp_var))
        
        glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
        glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                                c(paste0(feat, ".zoo")))
        # all2$last3 = as.numeric(merge(z-lag(z, -3), b, all = TRUE))
        # all2$last5 = as.numeric(merge(z-lag(z, -5), b, all = TRUE))
        # all2$last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
        # all2$last20 = as.numeric(merge(z-lag(z, -20), b, all = TRUE))
        # all2$last50 = as.numeric(merge(z-lag(z, -50), b, all = TRUE))
        # 
        # 
        # # order table
        # all2 = all2[order(all2$id),]
        # 
        # ## fill in NAs
        # # count averages
        # na.avg = all2 %>% group_by(weekend, hour) %>% dplyr::summarise(
        #     last1=mean(last1, na.rm=TRUE),
        #     last3=mean(last3, na.rm=TRUE),
        #     last5=mean(last5, na.rm=TRUE),
        #     last10=mean(last10, na.rm=TRUE),
        #     last20=mean(last20, na.rm=TRUE),
        #     last50=mean(last50, na.rm=TRUE)
        # )
        # 
        # # fill in averages
        # na.merge = merge(all2, na.avg, by=c("weekend","hour"))
        # na.merge = na.merge[order(na.merge$id),]
        # for(i in c("last1", "last3", "last5", "last10", "last20", "last50")) {
        #     y = paste0(i, ".y")
        #     idx = is.na(all2[[i]])
        #     all2[idx,][[i]] <- na.merge[idx,][[y]]
        # }
        # rm(na.avg, na.merge, b, i, idx, n, pd, sec, sh, y, z)
    }
}

# check distribution of all numeric data
dsp_numeric_feats_dstrb <- function(feats_vctr) {
    for (feat in feats_vctr) {
        print(sprintf("feat: %s", feat))
        if (glb_is_regression)
            gp <- myplot_scatter(df=glb_allobs_df, ycol_name=glb_rsp_var, xcol_name=feat,
                                 smooth=TRUE)
        if (glb_is_classification)
            gp <- myplot_box(df=glb_allobs_df, ycol_names=feat, xcol_name=glb_rsp_var)
        if (inherits(glb_allobs_df[, feat], "factor"))
            gp <- gp + facet_wrap(reformulate(feat))
        print(gp)
    }
}
# dsp_numeric_vars_dstrb(setdiff(names(glb_allobs_df), 
#                                 union(myfind_chr_cols_df(glb_allobs_df), 
#                                       c(glb_rsp_var_raw, glb_rsp_var))))                                      

add_new_diag_feats <- function(obs_df, ref_df=glb_allobs_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<ref_val>"), 
#         <col2_name>.fctr=relevel(factor(ifelse(<col1_name> == <val>, "<oth_val>", "<ref_val>")), 
#                               as.factor(c("R", "<ref_val>")),
#                               ref="<ref_val>"),

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

#         <col_name> = trunc(<col2_name> / 100),

        .rnorm = rnorm(n=nrow(obs_df))
                        )

    # If levels of a factor are different across obs_df & glb_newobs_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    #print(summary(obs_df))
    #print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}
glb_allobs_df <- add_new_diag_feats(glb_allobs_df)
```

```
## Loading required package: plyr
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Merge some <descriptor>
# glb_allobs_df$<descriptor>.my <- glb_allobs_df$<descriptor>
# glb_allobs_df[grepl("\\bAIRPORT\\b", glb_allobs_df$<descriptor>.my),
#               "<descriptor>.my"] <- "AIRPORT"
# glb_allobs_df$<descriptor>.my <-
#     plyr::revalue(glb_allobs_df$<descriptor>.my, c(
#         "ABANDONED BUILDING" = "OTHER",
#         "##"                      = "##"
#     ))
# print(<descriptor>_freq_df <- mycreate_sqlxtab_df(glb_allobs_df, c("<descriptor>.my")))
# # print(dplyr::filter(<descriptor>_freq_df, grepl("(MEDICAL|DENTAL|OFFICE)", <descriptor>.my)))
# # print(dplyr::filter(dplyr::select(glb_allobs_df, -<var.zoo>), 
# #                     grepl("STORE", <descriptor>.my)))
# glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, "<descriptor>")

# Check distributions of newly transformed / extracted vars
#   Enhancement: remove vars that were displayed ealier
dsp_numeric_feats_dstrb(feats_vctr=setdiff(names(glb_allobs_df), 
        c(myfind_chr_cols_df(glb_allobs_df), glb_rsp_var_raw, glb_rsp_var, 
          glb_exclude_vars_as_features)))
```

```
## [1] "feat: Internet.Use"
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/inspect.data-2.png) 

```
## [1] "feat: Smartphone"
```

```
## Warning in loop_apply(n, do.ply): Removed 235 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 235 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 235 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: Age"
```

```
## Warning in loop_apply(n, do.ply): Removed 237 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 237 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 237 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/inspect.data-4.png) 

```
## [1] "feat: Conservativeness"
```

```
## Warning in loop_apply(n, do.ply): Removed 259 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 259 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 259 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/inspect.data-5.png) 

```
## [1] "feat: Info.On.Internet"
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/inspect.data-6.png) 

```
## [1] "feat: Worry.About.Info"
```

```
## Warning in loop_apply(n, do.ply): Removed 217 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 217 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 217 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/inspect.data-7.png) 

```
## [1] "feat: Anonymity.Possible"
```

```
## Warning in loop_apply(n, do.ply): Removed 254 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 254 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 254 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/inspect.data-8.png) 

```
## [1] "feat: Tried.Masking.Identity"
```

```
## Warning in loop_apply(n, do.ply): Removed 223 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 223 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 223 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/inspect.data-9.png) 

```
## [1] "feat: Privacy.Laws.Effective"
```

```
## Warning in loop_apply(n, do.ply): Removed 279 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 279 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 279 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/inspect.data-10.png) 

```
## [1] "feat: .rnorm"
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/inspect.data-11.png) 

```r
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

#pairs(subset(glb_trnobs_df, select=-c(col_symbol)))
# Check for glb_newobs_df & glb_trnobs_df features range mismatches

# Other diagnostics:
# print(subset(glb_trnobs_df, <col1_name> == max(glb_trnobs_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_trnobs_df$<col1_name>, na.rm=TRUE)))

# print(glb_trnobs_df[which.max(glb_trnobs_df$<col_name>),])

# print(<col_name>_freq_glb_trnobs_df <- mycreate_tbl_df(glb_trnobs_df, "<col_name>"))
# print(which.min(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>)[, 2]))
# print(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>))
# print(table(is.na(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(table(sign(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(mycreate_xtab_df(glb_trnobs_df, <col1_name>))
# print(mycreate_xtab_df(glb_trnobs_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mycreate_xtab_df(glb_trnobs_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_trnobs_df[is.na(<col1_name>_<col2_name>_xtab_glb_trnobs_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_trnobs_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 
# print(mycreate_sqlxtab_df(glb_allobs_df, c("<col1_name>", "<col2_name>")))

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>.NA, glb_trnobs_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_trnobs_df, Symbol %in% c("CocaCola", "ProcterGamble")), 
#                   "Date.POSIX", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.POSIXlt("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1983-01-01")))        
#         )
# print(myplot_line(subset(glb_trnobs_df, Date.POSIX > as.POSIXct("2004-01-01")), 
#                   "Date.POSIX", "StockPrice") +
#     geom_line(aes(color=Symbol)) + 
#     coord_cartesian(xlim=c(as.POSIXct("1990-01-01"),
#                            as.POSIXct("2000-01-01"))) +     
#     coord_cartesian(ylim=c(0, 250)) +     
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-09-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-11-01")))        
#         )
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>") + 
#         geom_point(data=subset(glb_allobs_df, <condition>), 
#                     mapping=aes(x=<x_var>, y=<y_var>), color="red", shape=4, size=5))

rm(srt_allobs_df, last1, last10, last100, pd)
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object
## 'srt_allobs_df' not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'last1'
## not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'last10'
## not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'last100'
## not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'pd' not
## found
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "scrub.data", major.inc=FALSE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 2 inspect.data          2          0  9.966 15.619   5.653
## 3   scrub.data          2          1 15.620     NA      NA
```

### Step `2.1: scrub data`

```r
# Options:
#   1. Not fill missing vars
#   2. Fill missing numerics with a different algorithm
#   3. Fill missing chars with data based on clusters 

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##           Internet.Use             Smartphone                    Age 
##                      1                     43                     27 
##       Conservativeness       Info.On.Internet       Worry.About.Info 
##                     62                    210                    212 
##     Privacy.Importance     Anonymity.Possible Tried.Masking.Identity 
##                    215                    249                    218 
## Privacy.Laws.Effective 
##                    108 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##           Internet.Use             Smartphone       Info.On.Internet 
##                    226                    472                    105 
##       Worry.About.Info     Privacy.Importance     Anonymity.Possible 
##                    404                     43                    475 
## Tried.Masking.Identity Privacy.Laws.Effective 
##                    656                    660 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##       Sex     State    Region .rownames 
##         0         0         0         0
```

```r
# if (!is.null(glb_force_0_to_NA_vars)) {
#     for (feat in glb_force_0_to_NA_vars) {
#         warning("Forcing ", sum(glb_allobs_df[, feat] == 0),
#                 " obs with ", feat, " 0s to NAs")
#         glb_allobs_df[glb_allobs_df[, feat] == 0, feat] <- NA
#     }
# }

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##           Internet.Use             Smartphone                    Age 
##                      1                     43                     27 
##       Conservativeness       Info.On.Internet       Worry.About.Info 
##                     62                    210                    212 
##     Privacy.Importance     Anonymity.Possible Tried.Masking.Identity 
##                    215                    249                    218 
## Privacy.Laws.Effective 
##                    108 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##           Internet.Use             Smartphone       Info.On.Internet 
##                    226                    472                    105 
##       Worry.About.Info     Privacy.Importance     Anonymity.Possible 
##                    404                     43                    475 
## Tried.Masking.Identity Privacy.Laws.Effective 
##                    656                    660 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##       Sex     State    Region .rownames 
##         0         0         0         0
```

```r
dsp_catgs <- function() {
    print("NewsDesk:")
    print(table(glb_allobs_df$NewsDesk))
    print("SectionName:")    
    print(table(glb_allobs_df$SectionName))
    print("SubsectionName:")        
    print(table(glb_allobs_df$SubsectionName))
}

# sel_obs <- function(Popular=NULL, 
#                     NewsDesk=NULL, SectionName=NULL, SubsectionName=NULL,
#         Headline.contains=NULL, Snippet.contains=NULL, Abstract.contains=NULL,
#         Headline.pfx=NULL, NewsDesk.nb=NULL, .clusterid=NULL, myCategory=NULL,
#         perl=FALSE) {
sel_obs <- function(vars_lst) {
    tmp_df <- glb_allobs_df
    # Does not work for Popular == NAs ???
    if (!is.null(Popular)) {
        if (is.na(Popular))
            tmp_df <- tmp_df[is.na(tmp_df$Popular), ] else   
            tmp_df <- tmp_df[tmp_df$Popular == Popular, ]    
    }    
    if (!is.null(NewsDesk)) 
        tmp_df <- tmp_df[tmp_df$NewsDesk == NewsDesk, ]
    if (!is.null(SectionName)) 
        tmp_df <- tmp_df[tmp_df$SectionName == SectionName, ]
    if (!is.null(SubsectionName)) 
        tmp_df <- tmp_df[tmp_df$SubsectionName == SubsectionName, ]
    if (!is.null(Headline.contains))
        tmp_df <- 
            tmp_df[grep(Headline.contains, tmp_df$Headline, perl=perl), ]
    if (!is.null(Snippet.contains))
        tmp_df <- 
            tmp_df[grep(Snippet.contains, tmp_df$Snippet, perl=perl), ]
    if (!is.null(Abstract.contains))
        tmp_df <- 
            tmp_df[grep(Abstract.contains, tmp_df$Abstract, perl=perl), ]
    if (!is.null(Headline.pfx)) {
        if (length(grep("Headline.pfx", names(tmp_df), fixed=TRUE, value=TRUE))
            > 0) tmp_df <- 
                tmp_df[tmp_df$Headline.pfx == Headline.pfx, ] else
        warning("glb_allobs_df does not contain Headline.pfx; ignoring that filter")                    
    }    
    if (!is.null(NewsDesk.nb)) {
        if (any(grepl("NewsDesk.nb", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$NewsDesk.nb == NewsDesk.nb, ] else
        warning("glb_allobs_df does not contain NewsDesk.nb; ignoring that filter")                    
    }    
    if (!is.null(.clusterid)) {
        if (any(grepl(".clusterid", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$clusterid == clusterid, ] else
        warning("glb_allobs_df does not contain clusterid; ignoring that filter")                       }
    if (!is.null(myCategory)) {    
        if (!(myCategory %in% names(glb_allobs_df)))
            tmp_df <-
                tmp_df[tmp_df$myCategory == myCategory, ] else
        warning("glb_allobs_df does not contain myCategory; ignoring that filter")                    
    }    
    
    return(glb_allobs_df$UniqueID %in% tmp_df$UniqueID)
}

dsp_obs <- function(..., cols=c(NULL), all=FALSE) {
    tmp_df <- glb_allobs_df[sel_obs(...), 
                            union(c("UniqueID", "Popular", "myCategory", "Headline"), cols), FALSE]
    if(all) { print(tmp_df) } else { myprint_df(tmp_df) }
}
#dsp_obs(Popular=1, NewsDesk="", SectionName="", Headline.contains="Boehner")
# dsp_obs(Popular=1, NewsDesk="", SectionName="")
# dsp_obs(Popular=NA, NewsDesk="", SectionName="")

dsp_tbl <- function(...) {
    tmp_entity_df <- glb_allobs_df[sel_obs(...), ]
    tmp_tbl <- table(tmp_entity_df$NewsDesk, 
                     tmp_entity_df$SectionName,
                     tmp_entity_df$SubsectionName, 
                     tmp_entity_df$Popular, useNA="ifany")
    #print(names(tmp_tbl))
    #print(dimnames(tmp_tbl))
    print(tmp_tbl)
}

dsp_hdlxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
                           c("Headline.pfx", "Headline", glb_rsp_var)))
#dsp_hdlxtab("(1914)|(1939)")

dsp_catxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# dsp_catxtab("1914)|(1939)")
# dsp_catxtab("19(14|39|64):")
# dsp_catxtab("19..:")

# Create myCategory <- NewsDesk#SectionName#SubsectionName
#   Fix some data before merging categories
# glb_allobs_df[sel_obs(Headline.contains="Your Turn:", NewsDesk=""),
#               "NewsDesk"] <- "Styles"
# glb_allobs_df[sel_obs(Headline.contains="School", NewsDesk="", SectionName="U.S.",
#                       SubsectionName=""),
#               "SubsectionName"] <- "Education"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SectionName"] <- "Business Day"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SubsectionName"] <- "Small Business"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SectionName"] <- "Opinion"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SubsectionName"] <- "Room For Debate"

# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName="", Popular=NA),
#               "SubsectionName"] <- "Small Business"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(7973), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName=""),
#               "SectionName"] <- "Technology"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5076, 5736, 5924, 5911, 6532), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(SectionName="Health"),
#               "NewsDesk"] <- "Science"
# glb_allobs_df[sel_obs(SectionName="Travel"),
#               "NewsDesk"] <- "Travel"
# 
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SectionName"] <- ""
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SubsectionName"] <- ""
# glb_allobs_df[sel_obs(NewsDesk="Styles", SectionName="", SubsectionName="", Popular=1),
#               "SectionName"] <- "U.S."
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5486), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df$myCategory <- paste(glb_allobs_df$NewsDesk, 
#                                   glb_allobs_df$SectionName,
#                                   glb_allobs_df$SubsectionName,
#                                   sep="#")

# dsp_obs( Headline.contains="Music:"
#         #,NewsDesk=""
#         #,SectionName=""  
#         #,SubsectionName="Fashion & Style"
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
# dsp_obs( Headline.contains="."
#         ,NewsDesk=""
#         ,SectionName="Opinion"  
#         ,SubsectionName=""
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
                                        
# Merge some categories
# glb_allobs_df$myCategory <-
#     plyr::revalue(glb_allobs_df$myCategory, c(      
#         "#Business Day#Dealbook"            = "Business#Business Day#Dealbook",
#         "#Business Day#Small Business"      = "Business#Business Day#Small Business",
#         "#Crosswords/Games#"                = "Business#Crosswords/Games#",
#         "Business##"                        = "Business#Technology#",
#         "#Open#"                            = "Business#Technology#",
#         "#Technology#"                      = "Business#Technology#",
#         
#         "#Arts#"                            = "Culture#Arts#",        
#         "Culture##"                         = "Culture#Arts#",        
#         
#         "#World#Asia Pacific"               = "Foreign#World#Asia Pacific",        
#         "Foreign##"                         = "Foreign#World#",    
#         
#         "#N.Y. / Region#"                   = "Metro#N.Y. / Region#",  
#         
#         "#Opinion#"                         = "OpEd#Opinion#",                
#         "OpEd##"                            = "OpEd#Opinion#",        
# 
#         "#Health#"                          = "Science#Health#",
#         "Science##"                         = "Science#Health#",        
#         
#         "Styles##"                          = "Styles##Fashion",                        
#         "Styles#Health#"                    = "Science#Health#",                
#         "Styles#Style#Fashion & Style"      = "Styles##Fashion",        
# 
#         "#Travel#"                          = "Travel#Travel#",                
#         
#         "Magazine#Magazine#"                = "myOther",
#         "National##"                        = "myOther",
#         "National#U.S.#Politics"            = "myOther",        
#         "Sports##"                          = "myOther",
#         "Sports#Sports#"                    = "myOther",
#         "#U.S.#"                            = "myOther",        
#         
# 
# #         "Business##Small Business"        = "Business#Business Day#Small Business",        
# #         
# #         "#Opinion#"                       = "#Opinion#Room For Debate",        
#         "##"                                = "##"
# #         "Business##" = "Business#Business Day#Dealbook",
# #         "Foreign#World#" = "Foreign##",
# #         "#Open#" = "Other",
# #         "#Opinion#The Public Editor" = "OpEd#Opinion#",
# #         "Styles#Health#" = "Styles##",
# #         "Styles#Style#Fashion & Style" = "Styles##",
# #         "#U.S.#" = "#U.S.#Education",
#     ))

# ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
#                           mycreate_sqlxtab_df(glb_allobs_df,
#     c("myCategory", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# myprint_df(ctgry_xtab_df)
# write.table(ctgry_xtab_df, paste0(glb_out_pfx, "ctgry_xtab.csv"), 
#             row.names=FALSE)

# ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
#                        myCategory + NewsDesk + SectionName + SubsectionName ~ 
#                            Popular.fctr, sum, value.var=".n"))
# myprint_df(ctgry_cast_df)
# write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_cast.csv"), 
#             row.names=FALSE)

# print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df[, glb_rsp_var], 
#                              useNA="ifany"))

dsp_chisq.test <- function(...) {
    sel_df <- glb_allobs_df[sel_obs(...) & 
                            !is.na(glb_allobs_df$Popular), ]
    sel_df$.marker <- 1
    ref_df <- glb_allobs_df[!is.na(glb_allobs_df$Popular), ]
    mrg_df <- merge(ref_df[, c(glb_id_var, "Popular")],
                    sel_df[, c(glb_id_var, ".marker")], all.x=TRUE)
    mrg_df[is.na(mrg_df)] <- 0
    print(mrg_tbl <- table(mrg_df$.marker, mrg_df$Popular))
    print("Rows:Selected; Cols:Popular")
    #print(mrg_tbl)
    print(chisq.test(mrg_tbl))
}
# dsp_chisq.test(Headline.contains="[Ee]bola")
# dsp_chisq.test(Snippet.contains="[Ee]bola")
# dsp_chisq.test(Abstract.contains="[Ee]bola")

# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola"), ], 
#                           c(glb_rsp_var, "NewsDesk", "SectionName", "SubsectionName")))

# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName))
# print(table(glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))
# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))

# glb_allobs_df$myCategory.fctr <- as.factor(glb_allobs_df$myCategory)
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("myCategory", "NewsDesk", "SectionName", "SubsectionName"))

# Copy Headline into Snipper & Abstract if they are empty
# print(glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, c("Headline", "Snippet")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Snippet, 
#                     c("UniqueID", "Headline", "Snippet")])
# glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Snippet"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Headline"]
# 
# print(glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, c("Headline", "Abstract")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Abstract, 
#                     c("UniqueID", "Headline", "Abstract")])
# glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Abstract"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Headline"]

# WordCount_0_df <- subset(glb_allobs_df, WordCount == 0)
# table(WordCount_0_df$Popular, WordCount_0_df$WordCount, useNA="ifany")
# myprint_df(WordCount_0_df[, 
#                 c("UniqueID", "Popular", "WordCount", "Headline")])
```

### Step `2.1: scrub data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "transform.data", major.inc=FALSE)
```

```
##            label step_major step_minor   bgn    end elapsed
## 3     scrub.data          2          1 15.62 17.419   1.799
## 4 transform.data          2          2 17.42     NA      NA
```

```r
### Mapping dictionary
#sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_map_vars)) {
    for (feat in glb_map_vars) {
        map_df <- myimport_data(url=glb_map_urls[[feat]], 
                                            comment="map_df", 
                                           print_diagn=TRUE)
        glb_allobs_df <- mymap_codes(glb_allobs_df, feat, names(map_df)[2], 
                                     map_df, map_join_col_name=names(map_df)[1], 
                                     map_tgt_col_name=names(map_df)[2])
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_map_vars)
}

### Forced Assignments
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (feat in glb_assign_vars) {
    new_feat <- paste0(feat, ".my")
    print(sprintf("Forced Assignments for: %s -> %s...", feat, new_feat))
    glb_allobs_df[, new_feat] <- glb_allobs_df[, feat]
    
    pairs <- glb_assign_pairs_lst[[feat]]
    for (pair_ix in 1:length(pairs$from)) {
        if (is.na(pairs$from[pair_ix]))
            nobs <- nrow(filter(glb_allobs_df, 
                                is.na(eval(parse(text=feat),
                                            envir=glb_allobs_df)))) else
            nobs <- sum(glb_allobs_df[, feat] == pairs$from[pair_ix])
        #nobs <- nrow(filter(glb_allobs_df, is.na(Married.fctr)))    ; print(nobs)
        
        if ((is.na(pairs$from[pair_ix])) && (is.na(pairs$to[pair_ix])))
            stop("what are you trying to do ???")
        if (is.na(pairs$from[pair_ix]))
            glb_allobs_df[is.na(glb_allobs_df[, feat]), new_feat] <- 
                pairs$to[pair_ix] else
            glb_allobs_df[glb_allobs_df[, feat] == pairs$from[pair_ix], new_feat] <- 
                pairs$to[pair_ix]
                    
        print(sprintf("    %s -> %s for %s obs", 
                      pairs$from[pair_ix], pairs$to[pair_ix], format(nobs, big.mark=",")))
    }

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_assign_vars)
}
```

```
## [1] "Forced Assignments for: Internet.Use -> Internet.Use.my..."
## [1] "    NA -> 0 for 1 obs"
```

```r
### Transformations using mapping functions
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (feat in glb_transform_vars) {
    new_feat <- paste0(feat, glb_transform_lst[[feat]]$sfx)
    print(sprintf("Applying mapping function for: %s -> %s...", feat, new_feat))
    glb_allobs_df[, new_feat] <- glb_transform_lst[[feat]]$mapfn(glb_allobs_df[, feat])

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_transform_vars)
}
```

```
## [1] "Applying mapping function for: Age -> Age.my.fctr..."
## [1] "Applying mapping function for: Anonymity.Possible -> Anonymity.Possible.my.fctr..."
## [1] "Applying mapping function for: Conservativeness -> Conservativeness.my.fctr..."
## [1] "Applying mapping function for: Info.On.Internet -> Info.On.Internet.my.fctr..."
## [1] "Applying mapping function for: Privacy.Laws.Effective -> Privacy.Laws.Effective.my.fctr..."
## [1] "Applying mapping function for: Smartphone -> Smartphone.my.fctr..."
## [1] "Applying mapping function for: Tried.Masking.Identity -> Tried.Masking.Identity.my.fctr..."
## [1] "Applying mapping function for: Worry.About.Info -> Worry.About.Info.my.fctr..."
```

### Step `2.2: transform data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 4      transform.data          2          2 17.420 17.556   0.136
## 5 manage.missing.data          2          3 17.556     NA      NA
```

```r
# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))
# glb_trnobs_df <- na.omit(glb_trnobs_df)
# glb_newobs_df <- na.omit(glb_newobs_df)
# df[is.na(df)] <- 0

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##           Internet.Use             Smartphone                    Age 
##                      1                     43                     27 
##       Conservativeness       Info.On.Internet       Worry.About.Info 
##                     62                    210                    212 
##     Privacy.Importance     Anonymity.Possible Tried.Masking.Identity 
##                    215                    249                    218 
## Privacy.Laws.Effective 
##                    108 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##                   Internet.Use                     Smartphone 
##                            226                            472 
##               Info.On.Internet               Worry.About.Info 
##                            105                            404 
##             Privacy.Importance             Anonymity.Possible 
##                             43                            475 
##         Tried.Masking.Identity         Privacy.Laws.Effective 
##                            656                            660 
##                Internet.Use.my     Anonymity.Possible.my.fctr 
##                            227                            475 
##       Info.On.Internet.my.fctr Privacy.Laws.Effective.my.fctr 
##                            105                            660 
##             Smartphone.my.fctr Tried.Masking.Identity.my.fctr 
##                            472                            656 
##       Worry.About.Info.my.fctr 
##                            404 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##       Sex     State    Region .rownames 
##         0         0         0         0
```

```r
# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function() {
    
    require(mice)
    set.seed(glb_mice_complete.seed)
    inp_impent_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                union(glb_exclude_vars_as_features, glb_rsp_var))]
    print("Summary before imputation: ")
    print(summary(inp_impent_df))
    out_impent_df <- complete(mice(inp_impent_df))
    print(summary(out_impent_df))
    
    # complete(mice()) changes attributes of factors even though values don't change
    ret_vars <- sapply(names(out_impent_df), 
                       function(col) ifelse(!identical(out_impent_df[, col], inp_impent_df[, col]), 
                                            col, ""))
    ret_vars <- ret_vars[ret_vars != ""]
    return(out_impent_df[, ret_vars])
}

if (glb_impute_na_data && 
    (length(myfind_numerics_missing(glb_allobs_df)) > 0) &&
    (ncol(nonna_df <- glb_impute_missing_data()) > 0)) {
    for (col in names(nonna_df)) {
        glb_allobs_df[, paste0(col, ".nonNA")] <- nonna_df[, col]
        glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, col)        
    }
}    
    
mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##           Internet.Use             Smartphone                    Age 
##                      1                     43                     27 
##       Conservativeness       Info.On.Internet       Worry.About.Info 
##                     62                    210                    212 
##     Privacy.Importance     Anonymity.Possible Tried.Masking.Identity 
##                    215                    249                    218 
## Privacy.Laws.Effective 
##                    108 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##                   Internet.Use                     Smartphone 
##                            226                            472 
##               Info.On.Internet               Worry.About.Info 
##                            105                            404 
##             Privacy.Importance             Anonymity.Possible 
##                             43                            475 
##         Tried.Masking.Identity         Privacy.Laws.Effective 
##                            656                            660 
##                Internet.Use.my     Anonymity.Possible.my.fctr 
##                            227                            475 
##       Info.On.Internet.my.fctr Privacy.Laws.Effective.my.fctr 
##                            105                            660 
##             Smartphone.my.fctr Tried.Masking.Identity.my.fctr 
##                            472                            656 
##       Worry.About.Info.my.fctr 
##                            404 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##       Sex     State    Region .rownames 
##         0         0         0         0
```

## Step `2.3: manage missing data`

```r
#```{r extract_features, cache=FALSE, eval=!is.null(glb_txt_vars)}
glb_chunks_df <- myadd_chunk(glb_chunks_df, "extract.features", major.inc=TRUE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 5 manage.missing.data          2          3 17.556 17.639   0.083
## 6    extract.features          3          0 17.639     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 17.645  NA      NA
```

```r
# Options:
#   Select Tf, log(1 + Tf), Tf-IDF or BM25Tf-IDf

# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_trnobs_df$<col_name>), -2, na.pad=TRUE)
# glb_trnobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newobs_df$<col_name>), -2, na.pad=TRUE)
# glb_newobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newobs_df[1, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df) - 1, 
#                                                    "<col_name>"]
# glb_newobs_df[2, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df), 
#                                                    "<col_name>"]
                                                   
# glb_allobs_df <- mutate(glb_allobs_df,
#     A.P.http=ifelse(grepl("http",Added,fixed=TRUE), 1, 0)
#                     )
# 
# glb_trnobs_df <- mutate(glb_trnobs_df,
#                     )
# 
# glb_newobs_df <- mutate(glb_newobs_df,
#                     )

#   Create factors of string variables
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "factorize.str.vars"), major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 1                extract.features_bgn          1          0 17.645 17.653
## 2 extract.features_factorize.str.vars          2          0 17.653     NA
##   elapsed
## 1   0.008
## 2      NA
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; #glb_allobs_df <- sav_allobs_df
print(str_vars <- myfind_chr_cols_df(glb_allobs_df))
```

```
##         Sex       State      Region   .rownames        .src 
##       "Sex"     "State"    "Region" ".rownames"      ".src"
```

```r
if (length(str_vars <- setdiff(str_vars, 
                               glb_exclude_vars_as_features)) > 0) {
    for (var in str_vars) {
        warning("Creating factors of string variable: ", var, 
                ": # of unique values: ", length(unique(glb_allobs_df[, var])))
        glb_allobs_df[, paste0(var, ".fctr")] <- factor(glb_allobs_df[, var], 
                        as.factor(unique(glb_allobs_df[, var])))
#         glb_trnobs_df[, paste0(var, ".fctr")] <- factor(glb_trnobs_df[, var], 
#                         as.factor(unique(glb_allobs_df[, var])))
#         glb_newobs_df[, paste0(var, ".fctr")] <- factor(glb_newobs_df[, var], 
#                         as.factor(unique(glb_allobs_df[, var])))
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
}
```

```
## Warning: Creating factors of string variable: Sex: # of unique values: 2
```

```
## Warning: Creating factors of string variable: State: # of unique values: 49
```

```
## Warning: Creating factors of string variable: Region: # of unique values: 4
```

```r
if (!is.null(glb_txt_vars)) {
    require(foreach)
    require(gsubfn)
    require(stringr)
    require(tm)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text"), major.inc=TRUE)
    
    chk_pattern_freq <- function(re_str, ignore.case=TRUE) {
        match_mtrx <- str_extract_all(txt_vctr, regex(re_str, ignore_case=ignore.case), 
                                      simplify=TRUE)
        match_df <- as.data.frame(match_mtrx[match_mtrx != ""])
        names(match_df) <- "pattern"
        return(mycreate_sqlxtab_df(match_df, "pattern"))        
    }
    #tmp_freq_df <- chk_pattern_freq("\\bNew (\\w)+", ignore.case=FALSE)
    #subset(chk_pattern_freq("\\bNew (\\w)+", ignore.case=FALSE), grepl("New [[:upper:]]", pattern))
    #chk_pattern_freq("\\bnew (\\W)+")

    chk_subfn <- function(pos_ix) {
        re_str <- gsubfn_args_lst[["re_str"]][[pos_ix]]
        print("re_str:"); print(re_str)
        rp_frmla <- gsubfn_args_lst[["rp_frmla"]][[pos_ix]]        
        print("rp_frmla:"); print(rp_frmla, showEnv=FALSE)
        tmp_vctr <- grep(re_str, txt_vctr, value=TRUE, ignore.case=TRUE)[1:5]
        print("Before:")
        print(tmp_vctr)
        print("After:")            
        print(gsubfn(re_str, rp_frmla, tmp_vctr, ignore.case=TRUE))
    }
    #chk_subfn(1)

    myapply_gsub <- function(...) {
        if ((length_lst <- length(names(gsub_map_lst))) == 0)
            return(txt_vctr)
        for (ptn_ix in 1:length_lst) {
            print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                            length(names(gsub_map_lst)), names(gsub_map_lst)[ptn_ix]))
            txt_vctr <- gsub(names(gsub_map_lst)[ptn_ix], gsub_map_lst[[ptn_ix]], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    myapply_txtmap <- function(txt_vctr, ...) {
        nrows <- nrow(glb_txt_map_df)
        for (ptn_ix in 1:nrows) {
            print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                            nrows, glb_txt_map_df[ptn_ix, "rex_str"]))
            txt_vctr <- gsub(glb_txt_map_df[ptn_ix, "rex_str"], 
                             glb_txt_map_df[ptn_ix, "rpl_str"], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    chk.equal <- function(bgn, end) {
        print(all.equal(sav_txt_lst[["Headline"]][bgn:end], glb_txt_lst[["Headline"]][bgn:end]))
    }    
    dsp.equal <- function(bgn, end) {
        print(sav_txt_lst[["Headline"]][bgn:end])
        print(glb_txt_lst[["Headline"]][bgn:end])
    }    
#sav_txt_lst <- glb_txt_lst; all.equal(sav_txt_lst, glb_txt_lst)
#all.equal(sav_txt_lst[["Headline"]][1:4200], glb_txt_lst[["Headline"]][1:4200])
#all.equal(sav_txt_lst[["Headline"]][1:2000], glb_txt_lst[["Headline"]][1:2000])
#all.equal(sav_txt_lst[["Headline"]][1:1000], glb_txt_lst[["Headline"]][1:1000])
#all.equal(sav_txt_lst[["Headline"]][1:500], glb_txt_lst[["Headline"]][1:500])
#all.equal(sav_txt_lst[["Headline"]][1:200], glb_txt_lst[["Headline"]][1:200])
#all.equal(sav_txt_lst[["Headline"]][1:100], glb_txt_lst[["Headline"]][1:100])
#chk.equal( 1, 100)
#chk.equal(51, 100)
#chk.equal(81, 100)
#chk.equal(81,  90)
#chk.equal(81,  85)
#chk.equal(86,  90)
#chk.equal(96, 100)

#dsp.equal(86, 90)
    
    glb_txt_map_df <- read.csv("mytxt_map.csv", comment.char="#", strip.white=TRUE)
    glb_txt_lst <- list(); 
    print(sprintf("Building glb_txt_lst..."))
    glb_txt_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_vctr <- glb_allobs_df[, txt_var]
        
        # myapply_txtmap shd be created as a tm_map::content_transformer ?
        #print(glb_txt_map_df)
        #txt_var=glb_txt_vars[3]; txt_vctr <- glb_txt_lst[[txt_var]]
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rex_str == "\\bWall St\\.", "rex_str"])
        #print(rex_str <- glb_txt_map_df[grepl("du Pont", glb_txt_map_df$rex_str), "rex_str"])        
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rpl_str == "versus", "rex_str"])             
        #print(tmp_vctr <- grep(rex_str, txt_vctr, value=TRUE, ignore.case=FALSE))
        #ret_lst <- regexec(rex_str, txt_vctr, ignore.case=FALSE); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])
        #gsub(rex_str, glb_txt_map_df[glb_txt_map_df$rex_str == rex_str, "rpl_str"], tmp_vctr, ignore.case=FALSE)
        #grep("Hong Hong", txt_vctr, value=TRUE)
    
        txt_vctr <- myapply_txtmap(txt_vctr, ignore.case=FALSE)    
    }
    names(glb_txt_lst) <- glb_txt_vars

    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining Acronyms in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        print(tmp_vctr <- grep("[[:upper:]]\\.", txt_vctr, value=TRUE, ignore.case=FALSE))
    }

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(Fort|Ft\\.|Hong|Las|Los|New|Puerto|Saint|San|St\\.)( |-)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl("( |-)[[:upper:]]", pattern))))
        print("    consider cleaning if relevant to problem domain; geography name; .n > 1")
        #grep("New G", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Wins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }        
        
    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(N|S|E|W|C)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("N Weaver", txt_vctr, value=TRUE, ignore.case=FALSE)        
    }    

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(North|South|East|West|Central)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("Central (African|Bankers|Cast|Italy|Role|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("East (Africa|Berlin|London|Poland|Rivals|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("North (American|Korean|West)", txt_vctr, value=TRUE, ignore.case=FALSE)        
        #grep("South (Pacific|Street)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Martins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }    

    find_cmpnd_wrds <- function(txt_vctr) {
        txt_corpus <- Corpus(VectorSource(txt_vctr))
        txt_corpus <- tm_map(txt_corpus, tolower)
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation, 
                             preserve_intra_word_dashes=TRUE)
        full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTf))
        print("   Full TermMatrix:"); print(full_Tf_DTM)
        full_Tf_mtrx <- as.matrix(full_Tf_DTM)
        rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_Tf_vctr <- colSums(full_Tf_mtrx)
        names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
        #grep("year", names(full_Tf_vctr), value=TRUE)
        #which.max(full_Tf_mtrx[, "yearlong"])
        full_Tf_df <- as.data.frame(full_Tf_vctr)
        names(full_Tf_df) <- "Tf.full"
        full_Tf_df$term <- rownames(full_Tf_df)
        #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
        full_Tf_df <- orderBy(~ -Tf.full, full_Tf_df)
        cmpnd_Tf_df <- full_Tf_df[grep("-", full_Tf_df$term, value=TRUE) ,]
        
        filter_df <- read.csv("mytxt_compound.csv", comment.char="#", strip.white=TRUE)
        cmpnd_Tf_df$filter <- FALSE
        for (row_ix in 1:nrow(filter_df))
            cmpnd_Tf_df[!cmpnd_Tf_df$filter, "filter"] <- 
            grepl(filter_df[row_ix, "rex_str"], 
                  cmpnd_Tf_df[!cmpnd_Tf_df$filter, "term"], ignore.case=TRUE)
        cmpnd_Tf_df <- subset(cmpnd_Tf_df, !filter)
        # Bug in tm_map(txt_corpus, removePunctuation, preserve_intra_word_dashes=TRUE) ???
        #   "net-a-porter" gets converted to "net-aporter"
        #grep("net-a-porter", txt_vctr, ignore.case=TRUE, value=TRUE)
        #grep("maser-laser", txt_vctr, ignore.case=TRUE, value=TRUE)
        #txt_corpus[[which(grepl("net-a-porter", txt_vctr, ignore.case=TRUE))]]
        #grep("\\b(across|longer)-(\\w)", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        #grep("(\\w)-(affected|term)\\b", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        
        print(sprintf("nrow(cmpnd_Tf_df): %d", nrow(cmpnd_Tf_df)))
        myprint_df(cmpnd_Tf_df)
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text_reporting_compound_terms"), major.inc=FALSE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining compound terms in %s: ", txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
#         find_cmpnd_wrds(txt_vctr)
        #grep("thirty-five", txt_vctr, ignore.case=TRUE, value=TRUE)
        #rex_str <- glb_txt_map_df[grepl("hirty", glb_txt_map_df$rex_str), "rex_str"]
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "build.corpus"), major.inc=TRUE)
    
    glb_corpus_lst <- list()
    print(sprintf("Building glb_corpus_lst..."))
    glb_corpus_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_corpus <- Corpus(VectorSource(glb_txt_lst[[txt_var]]))
        txt_corpus <- tm_map(txt_corpus, tolower) #nuppr
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation) #npnct<chr_ix>
#         txt-corpus <- tm_map(txt_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x))   

        # Not to be run in production
        inspect_terms <- function() {
            full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                              control=list(weighting=weightTf))
            print("   Full TermMatrix:"); print(full_Tf_DTM)
            full_Tf_mtrx <- as.matrix(full_Tf_DTM)
            rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
            full_Tf_vctr <- colSums(full_Tf_mtrx)
            names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
            #grep("year", names(full_Tf_vctr), value=TRUE)
            #which.max(full_Tf_mtrx[, "yearlong"])
            full_Tf_df <- as.data.frame(full_Tf_vctr)
            names(full_Tf_df) <- "Tf.full"
            full_Tf_df$term <- rownames(full_Tf_df)
            #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
            full_Tf_df <- orderBy(~ -Tf.full +term, full_Tf_df)
            print(myplot_histogram(full_Tf_df, "Tf.full"))
            myprint_df(full_Tf_df)
            #txt_corpus[[which(grepl("zun", txt_vctr, ignore.case=TRUE))]]
            digit_terms_df <- subset(full_Tf_df, grepl("[[:digit:]]", term))
            myprint_df(digit_terms_df)
            return(full_Tf_df)
        }    
        #print("RemovePunct:"); remove_punct_Tf_df <- inspect_terms()

        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words[[txt_var]], 
                               stopwords("english"))) #nstopwrds
        #print("StoppedWords:"); stopped_words_Tf_df <- inspect_terms()
        txt_corpus <- tm_map(txt_corpus, stemDocument) #Features for lost information: Difference/ratio in density of full_TfIdf_DTM ???
        #txt_corpus <- tm_map(txt_corpus, content_transformer(stemDocument))        
        #print("StemmedWords:"); stemmed_words_Tf_df <- inspect_terms()
        #stemmed_stopped_Tf_df <- merge(stemmed_words_Tf_df, stopped_words_Tf_df, by="term", all=TRUE, suffixes=c(".stem", ".stop"))
        #myprint_df(stemmed_stopped_Tf_df)
        #print(subset(stemmed_stopped_Tf_df, grepl("compan", term)))
        #glb_corpus_lst[[txt_var]] <- txt_corpus
    }
    names(glb_corpus_lst) <- glb_txt_vars
        
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "extract.DTM"), major.inc=TRUE)

    glb_full_DTM_lst <- list(); glb_sprs_DTM_lst <- list();
    for (txt_var in glb_txt_vars) {
        print(sprintf("Extracting TfIDf terms for %s...", txt_var))        
        txt_corpus <- glb_corpus_lst[[txt_var]]
        
#         full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
#                                           control=list(weighting=weightTf))
        full_TfIdf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTfIdf))
        sprs_TfIdf_DTM <- removeSparseTerms(full_TfIdf_DTM, 
                                            glb_sprs_thresholds[txt_var])
        
#         glb_full_DTM_lst[[txt_var]] <- full_Tf_DTM
#         glb_sprs_DTM_lst[[txt_var]] <- sprs_Tf_DTM
        glb_full_DTM_lst[[txt_var]] <- full_TfIdf_DTM
        glb_sprs_DTM_lst[[txt_var]] <- sprs_TfIdf_DTM
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "report.DTM"), major.inc=TRUE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Reporting TfIDf terms for %s...", txt_var))        
        full_TfIdf_DTM <- glb_full_DTM_lst[[txt_var]]
        sprs_TfIdf_DTM <- glb_sprs_DTM_lst[[txt_var]]        

        print("   Full TermMatrix:"); print(full_TfIdf_DTM)
        full_TfIdf_mtrx <- as.matrix(full_TfIdf_DTM)
        rownames(full_TfIdf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_TfIdf_vctr <- colSums(full_TfIdf_mtrx)
        names(full_TfIdf_vctr) <- dimnames(full_TfIdf_DTM)[[2]]
        #grep("scene", names(full_TfIdf_vctr), value=TRUE)
        #which.max(full_TfIdf_mtrx[, "yearlong"])
        full_TfIdf_df <- as.data.frame(full_TfIdf_vctr)
        names(full_TfIdf_df) <- "TfIdf.full"
        full_TfIdf_df$term <- rownames(full_TfIdf_df)
        full_TfIdf_df$freq.full <- colSums(full_TfIdf_mtrx != 0)
        full_TfIdf_df <- orderBy(~ -TfIdf.full, full_TfIdf_df)

        print("   Sparse TermMatrix:"); print(sprs_TfIdf_DTM)
        sprs_TfIdf_vctr <- colSums(as.matrix(sprs_TfIdf_DTM))
        names(sprs_TfIdf_vctr) <- dimnames(sprs_TfIdf_DTM)[[2]]
        sprs_TfIdf_df <- as.data.frame(sprs_TfIdf_vctr)
        names(sprs_TfIdf_df) <- "TfIdf.sprs"
        sprs_TfIdf_df$term <- rownames(sprs_TfIdf_df)
        sprs_TfIdf_df$freq.sprs <- colSums(as.matrix(sprs_TfIdf_DTM) != 0)        
        sprs_TfIdf_df <- orderBy(~ -TfIdf.sprs, sprs_TfIdf_df)
        
        terms_TfIdf_df <- merge(full_TfIdf_df, sprs_TfIdf_df, all.x=TRUE)
        terms_TfIdf_df$in.sprs <- !is.na(terms_TfIdf_df$freq.sprs)
        plt_TfIdf_df <- subset(terms_TfIdf_df, 
                               TfIdf.full >= min(terms_TfIdf_df$TfIdf.sprs, na.rm=TRUE))
        plt_TfIdf_df$label <- ""
        plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "label"] <- 
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"]
        glb_important_terms[[txt_var]] <- union(glb_important_terms[[txt_var]],
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"])
        print(myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full", 
                             colorcol_name="in.sprs") + 
                  geom_text(aes(label=label), color="Black", size=3.5))
        
        melt_TfIdf_df <- orderBy(~ -value, melt(terms_TfIdf_df, id.var="term"))
        print(ggplot(melt_TfIdf_df, aes(value, color=variable)) + stat_ecdf() + 
                  geom_hline(yintercept=glb_sprs_thresholds[txt_var], 
                             linetype = "dotted"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, !is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(melt_TfIdf_df, "term", "value", 
                          colorcol_name="variable"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(head(melt_TfIdf_df, 10), "term", "value", 
                          colorcol_name="variable"))
    }

#     sav_full_DTM_lst <- glb_full_DTM_lst
#     sav_sprs_DTM_lst <- glb_sprs_DTM_lst
#     print(identical(sav_glb_corpus_lst, glb_corpus_lst))
#     print(all.equal(length(sav_glb_corpus_lst), length(glb_corpus_lst)))
#     print(all.equal(names(sav_glb_corpus_lst), names(glb_corpus_lst)))
#     print(all.equal(sav_glb_corpus_lst[["Headline"]], glb_corpus_lst[["Headline"]]))

#     print(identical(sav_full_DTM_lst, glb_full_DTM_lst))
#     print(identical(sav_sprs_DTM_lst, glb_sprs_DTM_lst))
        
    rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df, terms_TfIdf_df)

    # Create txt features
    if ((length(glb_txt_vars) > 1) &&
        (length(unique(pfxs <- sapply(glb_txt_vars, 
                    function(txt) toupper(substr(txt, 1, 1))))) < length(glb_txt_vars)))
            stop("Prefixes for corpus freq terms not unique: ", pfxs)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DTM"), 
                                         major.inc=TRUE)
    for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DTM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))
        txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
        colnames(txt_X_df) <- paste(txt_var_pfx, ".T.",
                                    make.names(colnames(txt_X_df)), sep="")
        rownames(txt_X_df) <- rownames(glb_allobs_df) # warning otherwise
#         plt_X_df <- cbind(txt_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today", xcol_name=glb_rsp_var))

#         log_X_df <- log(1 + txt_X_df)
#         colnames(log_X_df) <- paste(colnames(txt_X_df), ".log", sep="")
#         plt_X_df <- cbind(log_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today.log", xcol_name=glb_rsp_var))
        glb_allobs_df <- cbind(glb_allobs_df, txt_X_df) # TfIdf is normalized
        #glb_allobs_df <- cbind(glb_allobs_df, log_X_df) # if using non-normalized metrics 
    }
    #identical(chk_entity_df, glb_allobs_df)
    #chk_entity_df <- glb_allobs_df

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DXM"), 
                                         major.inc=TRUE)

#sav_allobs_df <- glb_allobs_df
    glb_punct_vctr <- c("!", "\"", "#", "\\$", "%", "&", "'", 
                        "\\(|\\)",# "\\(", "\\)", 
                        "\\*", "\\+", ",", "-", "\\.", "/", ":", ";", 
                        "<|>", # "<", 
                        "=", 
                        # ">", 
                        "\\?", "@", "\\[", "\\\\", "\\]", "^", "_", "`", 
                        "\\{", "\\|", "\\}", "~")
    txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
    txt_X_df <- foreach(txt_var=glb_txt_vars, .combine=cbind) %dopar% {   
    #for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DXM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))        
        #txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
        
        txt_full_DTM_mtrx <- as.matrix(glb_full_DTM_lst[[txt_var]])
        rownames(txt_full_DTM_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        #print(txt_full_DTM_mtrx[txt_full_DTM_mtrx[, "ebola"] != 0, "ebola"])
        
        # Create <txt_var>.T.<term> for glb_important_terms
        for (term in glb_important_terms[[txt_var]])
            txt_X_df[, paste0(txt_var_pfx, ".T.", make.names(term))] <- 
                txt_full_DTM_mtrx[, term]
                
        # Create <txt_var>.nwrds.log & .nwrds.unq.log
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")] <- 
            log(1 + mycount_pattern_occ("\\w+", glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.unq.log")] <- 
            log(1 + rowSums(txt_full_DTM_mtrx != 0))
        txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] <- 
            rowSums(txt_full_DTM_mtrx) 
        txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 
            txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] / 
            (exp(txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")]) - 1)
        txt_X_df[is.nan(txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")]),
                 paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 0

        # Create <txt_var>.nchrs.log
        txt_X_df[, paste0(txt_var_pfx, ".nchrs.log")] <- 
            log(1 + mycount_pattern_occ(".", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".nuppr.log")] <- 
            log(1 + mycount_pattern_occ("[[:upper:]]", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".ndgts.log")] <- 
            log(1 + mycount_pattern_occ("[[:digit:]]", glb_allobs_df[, txt_var]))

        # Create <txt_var>.npnct?.log
        # would this be faster if it's iterated over each row instead of 
        #   each created column ???
        for (punct_ix in 1:length(glb_punct_vctr)) { 
#             smp0 <- " "
#             smp1 <- "! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~"
#             smp2 <- paste(smp1, smp1, sep=" ")
#             print(sprintf("Testing %s pattern:", glb_punct_vctr[punct_ix])) 
#             results <- mycount_pattern_occ(glb_punct_vctr[punct_ix], c(smp0, smp1, smp2))
#             names(results) <- NULL; print(results)
            txt_X_df[, 
                paste0(txt_var_pfx, ".npnct", sprintf("%02d", punct_ix), ".log")] <-
                log(1 + mycount_pattern_occ(glb_punct_vctr[punct_ix], 
                                            glb_allobs_df[, txt_var]))
        }
#         print(head(glb_allobs_df[glb_allobs_df[, "A.npnct23.log"] > 0, 
#                                     c("UniqueID", "Popular", "Abstract", "A.npnct23.log")]))    
        
        # Create <txt_var>.nstopwrds.log & <txt_var>ratio.nstopwrds.nwrds
        stop_words_rex_str <- paste0("\\b(", paste0(c(glb_append_stop_words[[txt_var]], 
                                       stopwords("english")), collapse="|"),
                                     ")\\b")
        txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] <-
            log(1 + mycount_pattern_occ(stop_words_rex_str, glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".ratio.nstopwrds.nwrds")] <-
            exp(txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] - 
                txt_X_df[, paste0(txt_var_pfx, ".nwrds", ".log")])

        # Create <txt_var>.P.http
        txt_X_df[, paste(txt_var_pfx, ".P.http", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("http", glb_allobs_df[, txt_var]))    
    
        # Create user-specified pattern vectors 
        #   <txt_var>.P.year.colon
        txt_X_df[, paste0(txt_var_pfx, ".P.year.colon")] <-
            as.integer(0 + mycount_pattern_occ("[0-9]{4}:", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".P.daily.clip.report")] <-
            as.integer(0 + mycount_pattern_occ("Daily Clip Report", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".P.fashion.week")] <-
            as.integer(0 + mycount_pattern_occ("Fashion Week", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".P.first.draft")] <-
            as.integer(0 + mycount_pattern_occ("First Draft", glb_allobs_df[, txt_var]))

#sum(mycount_pattern_occ("Metropolitan Diary:", glb_allobs_df$Abstract) > 0)
        if (txt_var %in% c("Snippet", "Abstract")) {
            txt_X_df[, paste0(txt_var_pfx, ".P.metropolitan.diary.colon")] <-
                as.integer(0 + mycount_pattern_occ("Metropolitan Diary:", 
                                                   glb_allobs_df[, txt_var]))
        }

#sum(mycount_pattern_occ("[0-9]{4}:", glb_allobs_df$Headline) > 0)
#sum(mycount_pattern_occ("Quandary(.*)(?=:)", glb_allobs_df$Headline, perl=TRUE) > 0)
#sum(mycount_pattern_occ("No Comment(.*):", glb_allobs_df$Headline) > 0)
#sum(mycount_pattern_occ("Friday Night Music:", glb_allobs_df$Headline) > 0)
        if (txt_var %in% c("Headline")) {
            txt_X_df[, paste0(txt_var_pfx, ".P.facts.figures")] <-
                as.integer(0 + mycount_pattern_occ("Facts & Figures:", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.friday.night.music")] <-
                as.integer(0 + mycount_pattern_occ("Friday Night Music", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.no.comment.colon")] <-
                as.integer(0 + mycount_pattern_occ("No Comment(.*):", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.on.this.day")] <-
                as.integer(0 + mycount_pattern_occ("On This Day", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.quandary")] <-
                as.integer(0 + mycount_pattern_occ("Quandary(.*)(?=:)", glb_allobs_df[, txt_var], perl=TRUE))
            txt_X_df[, paste0(txt_var_pfx, ".P.readers.respond")] <-
                as.integer(0 + mycount_pattern_occ("Readers Respond", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.recap.colon")] <-
                as.integer(0 + mycount_pattern_occ("Recap:", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.s.notebook")] <-
                as.integer(0 + mycount_pattern_occ("s Notebook", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.today.in.politic")] <-
                as.integer(0 + mycount_pattern_occ("Today in Politic", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.today.in.smallbusiness")] <-
                as.integer(0 + mycount_pattern_occ("Today in Small Business:", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.verbatim.colon")] <-
                as.integer(0 + mycount_pattern_occ("Verbatim:", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.what.we.are")] <-
                as.integer(0 + mycount_pattern_occ("What We're", glb_allobs_df[, txt_var]))
        }

#summary(glb_allobs_df[ ,grep("P.on.this.day", names(glb_allobs_df), value=TRUE)])
        txt_X_df <- subset(txt_X_df, select=-.rnorm)
        txt_X_df <- txt_X_df[, -grep(glb_id_var, names(txt_X_df), fixed=TRUE), FALSE]
        #glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    }
    glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    #myplot_box(glb_allobs_df, "A.sum.TfIdf", glb_rsp_var)

    # Generate summaries
#     print(summary(glb_allobs_df))
#     print(sapply(names(glb_allobs_df), function(col) sum(is.na(glb_allobs_df[, col]))))
#     print(summary(glb_trnobs_df))
#     print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
#     print(summary(glb_newobs_df))
#     print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          glb_txt_vars)
    rm(log_X_df, txt_X_df)
}

# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

# print(myplot_scatter(glb_trnobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))

rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr, 
   glb_full_DTM_lst, glb_sprs_DTM_lst, txt_corpus, txt_vctr)
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'corpus_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_DTM' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_vctr' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_full_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_sprs_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_corpus' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_vctr' not found
```

```r
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, "extract.features_end", 
                                     major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 17.653 17.676
## 3                extract.features_end          3          0 17.677     NA
##   elapsed
## 2   0.024
## 3      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 17.653 17.676
## 1                extract.features_bgn          1          0 17.645 17.653
##   elapsed duration
## 2   0.024    0.023
## 1   0.008    0.008
## [1] "Total Elapsed Time: 17.676 secs"
```

![](Pew_Anonymity2_files/figure-html/extract.features-1.png) 

```r
# if (glb_save_envir)
#     save(glb_feats_df, 
#          glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
#          file=paste0(glb_out_pfx, "extract_features_dsk.RData"))
# load(paste0(glb_out_pfx, "extract_features_dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all","data.new")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0
```

![](Pew_Anonymity2_files/figure-html/extract.features-2.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn   end elapsed
## 6 extract.features          3          0 17.639 18.98   1.342
## 7     cluster.data          4          0 18.981    NA      NA
```

## Step `4.0: cluster data`

```r
if (glb_cluster) {
    require(proxy)
    #require(hash)
    require(dynamicTreeCut)

#     glb_hash <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#     glb_hash_lst <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#stophere; sav_allobs_df <- glb_allobs_df; 
    print("Clustering features: ")
    print(cluster_vars <- grep("[HSA]\\.[PT]\\.", names(glb_allobs_df), value=TRUE))
    #print(cluster_vars <- grep("[HSA]\\.", names(glb_allobs_df), value=TRUE))
    glb_allobs_df$.clusterid <- 1    
    #print(max(table(glb_allobs_df$myCategory.fctr) / 20))
    for (myCategory in c("##", "Business#Business Day#Dealbook", "OpEd#Opinion#", 
                         "Styles#U.S.#", "Business#Technology#", "Science#Health#",
                         "Culture#Arts#")) {
        ctgry_allobs_df <- glb_allobs_df[glb_allobs_df$myCategory == myCategory, ]
        
        dstns_dist <- dist(ctgry_allobs_df[, cluster_vars], method = "cosine")
        dstns_mtrx <- as.matrix(dstns_dist)
        print(sprintf("max distance(%0.4f) pair:", max(dstns_mtrx)))
        row_ix <- ceiling(which.max(dstns_mtrx) / ncol(dstns_mtrx))
        col_ix <- which.max(dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])
    
        min_dstns_mtrx <- dstns_mtrx
        diag(min_dstns_mtrx) <- 1
        print(sprintf("min distance(%0.4f) pair:", min(min_dstns_mtrx)))
        row_ix <- ceiling(which.min(min_dstns_mtrx) / ncol(min_dstns_mtrx))
        col_ix <- which.min(min_dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])                          
    
        clusters <- hclust(dstns_dist, method = "ward.D2")
        #plot(clusters, labels=NULL, hang=-1)
        myplclust(clusters, lab.col=unclass(ctgry_allobs_df[, glb_rsp_var]))
        
        #clusterGroups = cutree(clusters, k=7)
        clusterGroups <- cutreeDynamic(clusters, minClusterSize=20, method="tree", deepSplit=0)
        # Unassigned groups are labeled 0; the largest group has label 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")   
        #print(ctgry_allobs_df[which(clusterGroups == 1), c("UniqueID", "Popular", "Headline")])
        #print(ctgry_allobs_df[(clusterGroups == 1) & !is.na(ctgry_allobs_df$Popular) & (ctgry_allobs_df$Popular==1), c("UniqueID", "Popular", "Headline")])
        clusterGroups[clusterGroups == 0] <- 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
        #summary(factor(clusterGroups))
#         clusterGroups <- clusterGroups + 
#                 100 * # has to be > max(table(glb_allobs_df$myCategory.fctr) / minClusterSize=20)
#                             which(levels(glb_allobs_df$myCategory.fctr) == myCategory)
#         table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
    
        # add to glb_allobs_df - then split the data again
        glb_allobs_df[glb_allobs_df$myCategory==myCategory,]$.clusterid <- clusterGroups
        #print(unique(glb_allobs_df$.clusterid))
        #print(glb_feats_df[glb_feats_df$id == ".clusterid.fctr", ])
    }
    
    ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
                              mycreate_sqlxtab_df(glb_allobs_df,
        c("myCategory", ".clusterid", glb_rsp_var)))
    ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
                           myCategory + .clusterid ~ 
                               Popular.fctr, sum, value.var=".n"))
    print(ctgry_cast_df)
    #print(orderBy(~ myCategory -Y -NA, ctgry_cast_df))
    # write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_clst.csv"), 
    #             row.names=FALSE)
    
    print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df$.clusterid, 
                                 glb_allobs_df[, glb_rsp_var], 
                                 useNA="ifany"))
#     dsp_obs(.clusterid=1, myCategory="OpEd#Opinion#", 
#             cols=c("UniqueID", "Popular", "myCategory", ".clusterid", "Headline"),
#             all=TRUE)
    
    glb_allobs_df$.clusterid.fctr <- as.factor(glb_allobs_df$.clusterid)
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      ".clusterid")
    glb_interaction_only_features["myCategory.fctr"] <- c(".clusterid.fctr")
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      cluster_vars)
}

# Re-partition
glb_trnobs_df <- subset(glb_allobs_df, .src == "Train")
glb_newobs_df <- subset(glb_allobs_df, .src == "Test")

glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##             label step_major step_minor    bgn    end elapsed
## 7    cluster.data          4          0 18.981 19.252   0.271
## 8 select.features          5          0 19.253     NA      NA
```

## Step `5.0: select features`

```r
print(glb_feats_df <- myselect_features(entity_df=glb_trnobs_df, 
                       exclude_vars_as_features=glb_exclude_vars_as_features, 
                       rsp_var=glb_rsp_var))
```

```
##                                                            id
## Worry.About.Info                             Worry.About.Info
## Privacy.Laws.Effective                 Privacy.Laws.Effective
## Worry.About.Info.my.fctr             Worry.About.Info.my.fctr
## Privacy.Laws.Effective.my.fctr Privacy.Laws.Effective.my.fctr
## Tried.Masking.Identity                 Tried.Masking.Identity
## Anonymity.Possible                         Anonymity.Possible
## Age.my.fctr                                       Age.my.fctr
## Tried.Masking.Identity.my.fctr Tried.Masking.Identity.my.fctr
## Internet.Use                                     Internet.Use
## Internet.Use.my                               Internet.Use.my
## Anonymity.Possible.my.fctr         Anonymity.Possible.my.fctr
## Age                                                       Age
## Sex.fctr                                             Sex.fctr
## Conservativeness                             Conservativeness
## Conservativeness.my.fctr             Conservativeness.my.fctr
## Region.fctr                                       Region.fctr
## State.fctr                                         State.fctr
## Smartphone                                         Smartphone
## Info.On.Internet.my.fctr             Info.On.Internet.my.fctr
## Info.On.Internet                             Info.On.Internet
## .rnorm                                                 .rnorm
## Smartphone.my.fctr                         Smartphone.my.fctr
##                                        cor.y exclude.as.feat    cor.y.abs
## Worry.About.Info                0.2731098859               1 0.2731098859
## Privacy.Laws.Effective         -0.2646162683               1 0.2646162683
## Worry.About.Info.my.fctr        0.2590726457               0 0.2590726457
## Privacy.Laws.Effective.my.fctr -0.2089672738               0 0.2089672738
## Tried.Masking.Identity          0.0917151958               1 0.0917151958
## Anonymity.Possible             -0.0900636172               1 0.0900636172
## Age.my.fctr                     0.0784486053               0 0.0784486053
## Tried.Masking.Identity.my.fctr  0.0738452219               0 0.0738452219
## Internet.Use                    0.0716537180               1 0.0716537180
## Internet.Use.my                 0.0716537180               0 0.0716537180
## Anonymity.Possible.my.fctr     -0.0705772584               0 0.0705772584
## Age                             0.0586258396               1 0.0586258396
## Sex.fctr                        0.0568208798               0 0.0568208798
## Conservativeness                0.0475530736               1 0.0475530736
## Conservativeness.my.fctr        0.0380593579               0 0.0380593579
## Region.fctr                    -0.0373439487               0 0.0373439487
## State.fctr                     -0.0326536598               1 0.0326536598
## Smartphone                      0.0112852450               1 0.0112852450
## Info.On.Internet.my.fctr       -0.0052073661               0 0.0052073661
## Info.On.Internet                0.0045589909               1 0.0045589909
## .rnorm                         -0.0036694388               0 0.0036694388
## Smartphone.my.fctr             -0.0007627864               0 0.0007627864
```

```r
# sav_feats_df <- glb_feats_df; glb_feats_df <- sav_feats_df
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, obs_df=glb_trnobs_df, 
                              rsp_var=glb_rsp_var)))
```

```
## Loading required package: reshape2
```

```
##                                id         cor.y exclude.as.feat
## 21               Worry.About.Info  0.2731098859               1
## 22       Worry.About.Info.my.fctr  0.2590726457               0
## 19         Tried.Masking.Identity  0.0917151958               1
## 3                     Age.my.fctr  0.0784486053               0
## 20 Tried.Masking.Identity.my.fctr  0.0738452219               0
## 10                   Internet.Use  0.0716537180               1
## 11                Internet.Use.my  0.0716537180               0
## 2                             Age  0.0586258396               1
## 15                       Sex.fctr  0.0568208798               0
## 6                Conservativeness  0.0475530736               1
## 7        Conservativeness.my.fctr  0.0380593579               0
## 16                     Smartphone  0.0112852450               1
## 8                Info.On.Internet  0.0045589909               1
## 17             Smartphone.my.fctr -0.0007627864               0
## 1                          .rnorm -0.0036694388               0
## 9        Info.On.Internet.my.fctr -0.0052073661               0
## 18                     State.fctr -0.0326536598               1
## 14                    Region.fctr -0.0373439487               0
## 5      Anonymity.Possible.my.fctr -0.0705772584               0
## 4              Anonymity.Possible -0.0900636172               1
## 13 Privacy.Laws.Effective.my.fctr -0.2089672738               0
## 12         Privacy.Laws.Effective -0.2646162683               1
##       cor.y.abs cor.high.X freqRatio percentUnique zeroVar   nzv myNearZV
## 21 0.2731098859         NA  1.102273     0.3597122   FALSE FALSE    FALSE
## 22 0.2590726457         NA  1.102273     0.5395683   FALSE FALSE    FALSE
## 19 0.0917151958         NA  5.122222     0.3597122   FALSE FALSE    FALSE
## 3  0.0784486053         NA  1.239726     1.0791367   FALSE FALSE    FALSE
## 20 0.0738452219         NA  5.122222     0.5395683   FALSE FALSE    FALSE
## 10 0.0716537180         NA 54.600000     0.3597122   FALSE  TRUE    FALSE
## 11 0.0716537180         NA 54.600000     0.3597122   FALSE  TRUE    FALSE
## 2  0.0586258396         NA  1.055556    12.2302158   FALSE FALSE    FALSE
## 15 0.0568208798         NA  1.114068     0.3597122   FALSE FALSE    FALSE
## 6  0.0475530736         NA  1.069364     0.8992806   FALSE FALSE    FALSE
## 7  0.0380593579         NA  1.069364     1.0791367   FALSE FALSE    FALSE
## 16 0.0112852450         NA  1.737374     0.3597122   FALSE FALSE    FALSE
## 8  0.0045589909         NA  1.027397     2.1582734   FALSE FALSE    FALSE
## 17 0.0007627864         NA  1.737374     0.5395683   FALSE FALSE    FALSE
## 1  0.0036694388         NA  1.000000   100.0000000   FALSE FALSE    FALSE
## 9  0.0052073661         NA  1.027397     2.1582734   FALSE FALSE    FALSE
## 18 0.0326536598         NA  1.441860     8.6330935   FALSE FALSE    FALSE
## 14 0.0373439487         NA  1.369128     0.7194245   FALSE FALSE    FALSE
## 5  0.0705772584         NA  1.671717     0.5395683   FALSE FALSE    FALSE
## 4  0.0900636172         NA  1.671717     0.3597122   FALSE FALSE    FALSE
## 13 0.2089672738         NA  2.762963     0.5395683   FALSE FALSE    FALSE
## 12 0.2646162683         NA  2.762963     0.3597122   FALSE FALSE    FALSE
##    is.cor.y.abs.low
## 21            FALSE
## 22            FALSE
## 19            FALSE
## 3             FALSE
## 20            FALSE
## 10            FALSE
## 11            FALSE
## 2             FALSE
## 15            FALSE
## 6             FALSE
## 7             FALSE
## 16            FALSE
## 8             FALSE
## 17             TRUE
## 1             FALSE
## 9             FALSE
## 18            FALSE
## 14            FALSE
## 5             FALSE
## 4             FALSE
## 13            FALSE
## 12            FALSE
```

```r
#subset(glb_feats_df, id %in% c("A.nuppr.log", "S.nuppr.log"))
print(myplot_scatter(glb_feats_df, "percentUnique", "freqRatio", 
                     colorcol_name="myNearZV", jitter=TRUE) + 
          geom_point(aes(shape=nzv)) + xlim(-5, 25))
```

```
## Warning in myplot_scatter(glb_feats_df, "percentUnique", "freqRatio",
## colorcol_name = "myNearZV", : converting myNearZV to class:factor
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

![](Pew_Anonymity2_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##  [1] id               cor.y            exclude.as.feat  cor.y.abs       
##  [5] cor.high.X       freqRatio        percentUnique    zeroVar         
##  [9] nzv              myNearZV         is.cor.y.abs.low
## <0 rows> (or 0-length row.names)
```

```r
glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                         subset(glb_feats_df, myNearZV)$id)]

if (!is.null(glb_interaction_only_features))
    glb_feats_df[glb_feats_df$id %in% glb_interaction_only_features, "interaction.feat"] <-
        names(glb_interaction_only_features) else
    glb_feats_df$interaction.feat <- NA        

mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in : "
##           Internet.Use             Smartphone                    Age 
##                      1                     43                     27 
##       Conservativeness       Info.On.Internet       Worry.About.Info 
##                     62                    210                    212 
##     Privacy.Importance     Anonymity.Possible Tried.Masking.Identity 
##                    215                    249                    218 
## Privacy.Laws.Effective 
##                    108 
## [1] "numeric data w/ 0s in : "
##                   Internet.Use                     Smartphone 
##                            226                            472 
##               Info.On.Internet               Worry.About.Info 
##                            105                            404 
##             Privacy.Importance             Anonymity.Possible 
##                             43                            475 
##         Tried.Masking.Identity         Privacy.Laws.Effective 
##                            656                            660 
##                Internet.Use.my     Anonymity.Possible.my.fctr 
##                            227                            475 
##       Info.On.Internet.my.fctr Privacy.Laws.Effective.my.fctr 
##                            105                            660 
##             Smartphone.my.fctr Tried.Masking.Identity.my.fctr 
##                            472                            656 
##       Worry.About.Info.my.fctr 
##                            404 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
##       Sex     State    Region .rownames 
##         0         0         0         0
```

```r
# glb_allobs_df %>% filter(is.na(Married.fctr)) %>% tbl_df()
# glb_allobs_df %>% count(Married.fctr)
# levels(glb_allobs_df$Married.fctr)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor    bgn    end elapsed
## 8         select.features          5          0 19.253 19.878   0.626
## 9 partition.data.training          6          0 19.879     NA      NA
```

## Step `6.0: partition data training`

```r
if (all(is.na(glb_newobs_df[, glb_rsp_var]))) {
    require(caTools)
    
    set.seed(glb_split_sample.seed)
    split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
        SplitRatio=1 - (nrow(glb_newobs_df) * 1.1 / nrow(glb_trnobs_df)))
    glb_fitobs_df <- glb_trnobs_df[split, ] 
    glb_OOBobs_df <- glb_trnobs_df[!split ,]    
} else {
    print(sprintf("Newdata contains non-NA data for %s; setting OOB to Newdata", 
                  glb_rsp_var))
    glb_fitobs_df <- glb_trnobs_df; glb_OOBobs_df <- glb_newobs_df
}
```

```
## [1] "Newdata contains non-NA data for Privacy.Importance; setting OOB to Newdata"
```

```r
if (!is.null(glb_max_fitent_obs) && (nrow(glb_fitobs_df) > glb_max_fitent_obs)) {
    warning("glb_fitobs_df restricted to glb_max_fitent_obs: ", 
            format(glb_max_fitent_obs, big.mark=","))
    org_fitent_df <- glb_fitobs_df
    glb_fitobs_df <- 
        org_fitent_df[split <- sample.split(org_fitent_df[, glb_rsp_var_raw], 
                                            SplitRatio=glb_max_fitent_obs), ]
    org_fitent_df <- NULL
}

glb_allobs_df$.lcn <- ""
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_fitobs_df[, glb_id_var], ".lcn"] <- "Fit"
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_OOBobs_df[, glb_id_var], ".lcn"] <- "OOB"

dsp_class_dstrb <- function(obs_df, location_var, partition_var) {
    xtab_df <- mycreate_xtab_df(obs_df, c(location_var, partition_var))
    rownames(xtab_df) <- xtab_df[, location_var]
    xtab_df <- xtab_df[, -grepl(location_var, names(xtab_df))]
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Ensure proper splits by glb_rsp_var_raw & user-specified feature for OOB vs. new
if (!is.null(glb_category_vars)) {
    if (glb_is_classification)
        dsp_class_dstrb(glb_allobs_df, ".lcn", glb_rsp_var_raw)
    newent_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .src == "Test"), 
                                           glb_category_vars)
    OOBobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .lcn == "OOB"), 
                                           glb_category_vars)
    glb_ctgry_df <- merge(newent_ctgry_df, OOBobs_ctgry_df, by=glb_category_vars
                          , all=TRUE, suffixes=c(".Tst", ".OOB"))
    glb_ctgry_df$.freqRatio.Tst <- glb_ctgry_df$.n.Tst / sum(glb_ctgry_df$.n.Tst, na.rm=TRUE)
    glb_ctgry_df$.freqRatio.OOB <- glb_ctgry_df$.n.OOB / sum(glb_ctgry_df$.n.OOB, na.rm=TRUE)
    print(orderBy(~-.freqRatio.Tst-.freqRatio.OOB, glb_ctgry_df))
}

# Run this line by line
print("glb_feats_df:");   print(dim(glb_feats_df))
```

```
## [1] "glb_feats_df:"
```

```
## [1] 22 12
```

```r
sav_feats_df <- glb_feats_df
glb_feats_df <- sav_feats_df

glb_feats_df[, "rsp_var_raw"] <- FALSE
glb_feats_df[glb_feats_df$id == glb_rsp_var_raw, "rsp_var_raw"] <- TRUE 
glb_feats_df$exclude.as.feat <- (glb_feats_df$exclude.as.feat == 1)
if (!is.null(glb_id_var) && glb_id_var != ".rownames")
    glb_feats_df[glb_feats_df$id %in% glb_id_var, "id_var"] <- TRUE 
add_feats_df <- data.frame(id=glb_rsp_var, exclude.as.feat=TRUE, rsp_var=TRUE)
row.names(add_feats_df) <- add_feats_df$id; print(add_feats_df)
```

```
##                                    id exclude.as.feat rsp_var
## Privacy.Importance Privacy.Importance            TRUE    TRUE
```

```r
glb_feats_df <- myrbind_df(glb_feats_df, add_feats_df)
if (glb_id_var != ".rownames")
    print(subset(glb_feats_df, rsp_var_raw | rsp_var | id_var)) else
    print(subset(glb_feats_df, rsp_var_raw | rsp_var))    
```

```
##                                    id cor.y exclude.as.feat cor.y.abs
## Privacy.Importance Privacy.Importance    NA            TRUE        NA
##                    cor.high.X freqRatio percentUnique zeroVar nzv myNearZV
## Privacy.Importance         NA        NA            NA      NA  NA       NA
##                    is.cor.y.abs.low interaction.feat rsp_var_raw rsp_var
## Privacy.Importance               NA               NA          NA    TRUE
```

```r
print("glb_feats_df vs. glb_allobs_df: "); 
```

```
## [1] "glb_feats_df vs. glb_allobs_df: "
```

```r
print(setdiff(glb_feats_df$id, names(glb_allobs_df)))
```

```
## character(0)
```

```r
print("glb_allobs_df vs. glb_feats_df: "); 
```

```
## [1] "glb_allobs_df vs. glb_feats_df: "
```

```r
# Ensure these are only chr vars
print(setdiff(setdiff(names(glb_allobs_df), glb_feats_df$id), 
                myfind_chr_cols_df(glb_allobs_df)))
```

```
## character(0)
```

```r
#print(setdiff(setdiff(names(glb_allobs_df), glb_exclude_vars_as_features), 
#                glb_feats_df$id))

print("glb_allobs_df: "); print(dim(glb_allobs_df))
```

```
## [1] "glb_allobs_df: "
```

```
## [1] 1002   29
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 556  28
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 556  28
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 446  28
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 446  28
```

```r
# # Does not handle NULL or length(glb_id_var) > 1
# glb_allobs_df$.src.trn <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_trnobs_df[, glb_id_var], 
#                 ".src.trn"] <- 1 
# glb_allobs_df$.src.fit <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_fitobs_df[, glb_id_var], 
#                 ".src.fit"] <- 1 
# glb_allobs_df$.src.OOB <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_OOBobs_df[, glb_id_var], 
#                 ".src.OOB"] <- 1 
# glb_allobs_df$.src.new <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_newobs_df[, glb_id_var], 
#                 ".src.new"] <- 1 
# #print(unique(glb_allobs_df[, ".src.trn"]))
# write_cols <- c(glb_feats_df$id, 
#                 ".src.trn", ".src.fit", ".src.OOB", ".src.new")
# glb_allobs_df <- glb_allobs_df[, write_cols]
# 
# tmp_feats_df <- glb_feats_df
# tmp_entity_df <- glb_allobs_df

if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         file=paste0(glb_out_pfx, "blddfs_dsk.RData"))
# load(paste0(glb_out_pfx, "blddfs_dsk.RData"))

# if (!all.equal(tmp_feats_df, glb_feats_df))
#     stop("glb_feats_df r/w not working")
# if (!all.equal(tmp_entity_df, glb_allobs_df))
#     stop("glb_allobs_df r/w not working")

rm(split)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=TRUE)
```

```
##                      label step_major step_minor    bgn    end elapsed
## 9  partition.data.training          6          0 19.879 20.175   0.296
## 10              fit.models          7          0 20.175     NA      NA
```

## Step `7.0: fit models`

```r
# load(paste0(glb_out_pfx, "dsk.RData"))
# keep_cols <- setdiff(names(glb_allobs_df), 
#                      grep("^.src", names(glb_allobs_df), value=TRUE))
# glb_trnobs_df <- glb_allobs_df[glb_allobs_df$.src.trn == 1, keep_cols]
# glb_fitobs_df <- glb_allobs_df[glb_allobs_df$.src.fit == 1, keep_cols]
# glb_OOBobs_df <- glb_allobs_df[glb_allobs_df$.src.OOB == 1, keep_cols]
# glb_newobs_df <- glb_allobs_df[glb_allobs_df$.src.new == 1, keep_cols]
# 
# glb_models_lst <- list(); glb_models_df <- data.frame()
# 
if (glb_is_classification && glb_is_binomial && 
        (length(unique(glb_fitobs_df[, glb_rsp_var])) < 2))
    stop("glb_fitobs_df$", glb_rsp_var, ": contains less than 2 unique values: ",
         paste0(unique(glb_fitobs_df[, glb_rsp_var]), collapse=", "))

max_cor_y_x_vars <- orderBy(~ -cor.y.abs, 
        subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low & 
                                is.na(cor.high.X)))[1:2, "id"]
# while(length(max_cor_y_x_vars) < 2) {
#     max_cor_y_x_vars <- c(max_cor_y_x_vars, orderBy(~ -cor.y.abs, 
#             subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low))[3, "id"])    
# }
if (!is.null(glb_Baseline_mdl_var)) {
    if ((max_cor_y_x_vars[1] != glb_Baseline_mdl_var) & 
        (glb_feats_df[max_cor_y_x_vars[1], "cor.y.abs"] > 
         glb_feats_df[glb_Baseline_mdl_var, "cor.y.abs"]))
        stop(max_cor_y_x_vars[1], " has a lower correlation with ", glb_rsp_var, 
             " than the Baseline var: ", glb_Baseline_mdl_var)
}

glb_model_type <- ifelse(glb_is_regression, "regression", "classification")
    
# Baseline
if (!is.null(glb_Baseline_mdl_var)) 
    ret_lst <- myfit_mdl_fn(model_id="Baseline", model_method="mybaseln_classfr",
                            indep_vars_vctr=glb_Baseline_mdl_var,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)

# Most Frequent Outcome "MFO" model: mean(y) for regression
#   Not using caret's nullModel since model stats not avl
#   Cannot use rpart for multinomial classification since it predicts non-MFO
ret_lst <- myfit_mdl(model_id="MFO", 
                     model_method=ifelse(glb_is_regression, "lm", "myMFO_classfr"), 
                     model_type=glb_model_type,
                        indep_vars_vctr=".rnorm",
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: MFO.lm"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
```

![](Pew_Anonymity2_files/figure-html/fit.models_0-1.png) ![](Pew_Anonymity2_files/figure-html/fit.models_0-2.png) ![](Pew_Anonymity2_files/figure-html/fit.models_0-3.png) ![](Pew_Anonymity2_files/figure-html/fit.models_0-4.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -62.833 -23.580   4.284  26.478  37.758 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  62.5402     1.3390  46.706   <2e-16 ***
## .rnorm       -0.1148     1.3288  -0.086    0.931    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.57 on 554 degrees of freedom
## Multiple R-squared:  1.346e-05,	Adjusted R-squared:  -0.001792 
## F-statistic: 0.00746 on 1 and 554 DF,  p-value: 0.9312
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##   model_id model_method  feats max.nTuningRuns min.elapsedtime.everything
## 1   MFO.lm           lm .rnorm               0                      0.442
##   min.elapsedtime.final max.R.sq.fit min.RMSE.fit max.R.sq.OOB
## 1                 0.002 1.346478e-05     31.51269           NA
##   min.RMSE.OOB max.Adj.R.sq.fit
## 1           NA     -0.001791565
```

```r
if (glb_is_classification)
    # "random" model - only for classification; 
    #   none needed for regression since it is same as MFO
    ret_lst <- myfit_mdl(model_id="Random", model_method="myrandom_classfr",
                            model_type=glb_model_type,                         
                            indep_vars_vctr=".rnorm",
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)

# Any models that have tuning parameters has "better" results with cross-validation
#   (except rf) & "different" results for different outcome metrics

# Max.cor.Y
#   Check impact of cv
#       rpart is not a good candidate since caret does not optimize cp (only tuning parameter of rpart) well
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Max.cor.Y.cv.0.rpart"
## [1] "    indep_vars: Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.0758 on full training set
```

```
## Loading required package: rpart.plot
```

![](Pew_Anonymity2_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 556 
## 
##          CP nsplit rel error
## 1 0.0757997      0         1
## 
## Node number 1: 556 observations
##   mean=62.54204, MSE=993.0631 
## 
## n= 556 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 556 552143.1 62.54204 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##               model_id model_method
## 1 Max.cor.Y.cv.0.rpart        rpart
##                                                      feats max.nTuningRuns
## 1 Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr               0
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      0.532                 0.016            0
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB
## 1      31.5129           NA           NA
```

```r
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0.cp.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=0, 
            tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
```

```
## [1] "fitting model: Max.cor.Y.cv.0.cp.0.rpart"
## [1] "    indep_vars: Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr"
## Fitting cp = 0 on full training set
```

![](Pew_Anonymity2_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 556 
## 
##             CP nsplit rel error
## 1 0.0757997006      0 1.0000000
## 2 0.0349025295      1 0.9242003
## 3 0.0058478990      2 0.8892978
## 4 0.0057792960      3 0.8834499
## 5 0.0005067675      4 0.8776706
## 6 0.0000000000      5 0.8771638
## 
## Variable importance
##           Worry.About.Info.my.fctr1     Privacy.Laws.Effective.my.fctr1 
##                                  59                                  36 
## Privacy.Laws.Effective.my.fctrNA.my 
##                                   5 
## 
## Node number 1: 556 observations,    complexity param=0.0757997
##   mean=62.54204, MSE=993.0631 
##   left son=2 (292 obs) right son=3 (264 obs)
##   Primary splits:
##       Worry.About.Info.my.fctr1           < 0.5 to the left,  improve=0.075799700, (0 missing)
##       Privacy.Laws.Effective.my.fctr1     < 0.5 to the right, improve=0.055656000, (0 missing)
##       Privacy.Laws.Effective.my.fctrNA.my < 0.5 to the right, improve=0.003566702, (0 missing)
##   Surrogate splits:
##       Privacy.Laws.Effective.my.fctr1 < 0.5 to the right, agree=0.556, adj=0.064, (0 split)
## 
## Node number 2: 292 observations,    complexity param=0.03490253
##   mean=54.29244, MSE=1062.076 
##   left son=4 (90 obs) right son=5 (202 obs)
##   Primary splits:
##       Privacy.Laws.Effective.my.fctr1     < 0.5 to the right, improve=0.062139860, (0 missing)
##       Privacy.Laws.Effective.my.fctrNA.my < 0.5 to the right, improve=0.001576476, (0 missing)
## 
## Node number 3: 264 observations,    complexity param=0.005779296
##   mean=71.66659, MSE=758.1997 
##   left son=6 (45 obs) right son=7 (219 obs)
##   Primary splits:
##       Privacy.Laws.Effective.my.fctr1     < 0.5 to the right, improve=0.0159418600, (0 missing)
##       Privacy.Laws.Effective.my.fctrNA.my < 0.5 to the right, improve=0.0005340128, (0 missing)
## 
## Node number 4: 90 observations
##   mean=42.12169, MSE=959.2431 
## 
## Node number 5: 202 observations,    complexity param=0.005847899
##   mean=59.71505, MSE=1012.49 
##   left son=10 (33 obs) right son=11 (169 obs)
##   Primary splits:
##       Privacy.Laws.Effective.my.fctrNA.my < 0.5 to the right, improve=0.01578735, (0 missing)
## 
## Node number 6: 45 observations
##   mean=63.99691, MSE=791.7729 
## 
## Node number 7: 219 observations,    complexity param=0.0005067675
##   mean=73.24255, MSE=736.7303 
##   left son=14 (15 obs) right son=15 (204 obs)
##   Primary splits:
##       Privacy.Laws.Effective.my.fctrNA.my < 0.5 to the right, improve=0.001734234, (0 missing)
## 
## Node number 10: 33 observations
##   mean=50.66739, MSE=1221.87 
## 
## Node number 11: 169 observations
##   mean=61.48176, MSE=952.4997 
## 
## Node number 14: 15 observations
##   mean=69.07407, MSE=867.7767 
## 
## Node number 15: 204 observations
##   mean=73.54906, MSE=725.7229 
## 
## n= 556 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
##  1) root 556 552143.10 62.54204  
##    2) Worry.About.Info.my.fctr1< 0.5 292 310126.10 54.29244  
##      4) Privacy.Laws.Effective.my.fctr1>=0.5 90  86331.88 42.12169 *
##      5) Privacy.Laws.Effective.my.fctr1< 0.5 202 204523.00 59.71505  
##       10) Privacy.Laws.Effective.my.fctrNA.my>=0.5 33  40321.70 50.66739 *
##       11) Privacy.Laws.Effective.my.fctrNA.my< 0.5 169 160972.40 61.48176 *
##    3) Worry.About.Info.my.fctr1>=0.5 264 200164.70 71.66659  
##      6) Privacy.Laws.Effective.my.fctr1>=0.5 45  35629.78 63.99691 *
##      7) Privacy.Laws.Effective.my.fctr1< 0.5 219 161343.90 73.24255  
##       14) Privacy.Laws.Effective.my.fctrNA.my>=0.5 15  13016.65 69.07407 *
##       15) Privacy.Laws.Effective.my.fctrNA.my< 0.5 204 148047.50 73.54906 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##                    model_id model_method
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart
##                                                      feats max.nTuningRuns
## 1 Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr               0
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      0.466                 0.015    0.1228362
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB
## 1     29.51405           NA           NA
```

```r
if (glb_is_regression || glb_is_binomial) # For multinomials this model will be run next by default
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.rpart"
## [1] "    indep_vars: Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr"
```

```
## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info =
## trainInfo, : There were missing values in resampled performance measures.
```

```
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.00585 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](Pew_Anonymity2_files/figure-html/fit.models_0-7.png) ![](Pew_Anonymity2_files/figure-html/fit.models_0-8.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 556 
## 
##            CP nsplit rel error
## 1 0.075799701      0 1.0000000
## 2 0.034902530      1 0.9242003
## 3 0.005847899      2 0.8892978
## 
## Variable importance
##       Worry.About.Info.my.fctr1 Privacy.Laws.Effective.my.fctr1 
##                              66                              34 
## 
## Node number 1: 556 observations,    complexity param=0.0757997
##   mean=62.54204, MSE=993.0631 
##   left son=2 (292 obs) right son=3 (264 obs)
##   Primary splits:
##       Worry.About.Info.my.fctr1           < 0.5 to the left,  improve=0.075799700, (0 missing)
##       Privacy.Laws.Effective.my.fctr1     < 0.5 to the right, improve=0.055656000, (0 missing)
##       Privacy.Laws.Effective.my.fctrNA.my < 0.5 to the right, improve=0.003566702, (0 missing)
##   Surrogate splits:
##       Privacy.Laws.Effective.my.fctr1 < 0.5 to the right, agree=0.556, adj=0.064, (0 split)
## 
## Node number 2: 292 observations,    complexity param=0.03490253
##   mean=54.29244, MSE=1062.076 
##   left son=4 (90 obs) right son=5 (202 obs)
##   Primary splits:
##       Privacy.Laws.Effective.my.fctr1     < 0.5 to the right, improve=0.062139860, (0 missing)
##       Privacy.Laws.Effective.my.fctrNA.my < 0.5 to the right, improve=0.001576476, (0 missing)
## 
## Node number 3: 264 observations
##   mean=71.66659, MSE=758.1997 
## 
## Node number 4: 90 observations
##   mean=42.12169, MSE=959.2431 
## 
## Node number 5: 202 observations
##   mean=59.71505, MSE=1012.49 
## 
## n= 556 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 556 552143.10 62.54204  
##   2) Worry.About.Info.my.fctr1< 0.5 292 310126.10 54.29244  
##     4) Privacy.Laws.Effective.my.fctr1>=0.5 90  86331.88 42.12169 *
##     5) Privacy.Laws.Effective.my.fctr1< 0.5 202 204523.00 59.71505 *
##   3) Worry.About.Info.my.fctr1>=0.5 264 200164.70 71.66659 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##          model_id model_method
## 1 Max.cor.Y.rpart        rpart
##                                                      feats max.nTuningRuns
## 1 Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr               3
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      0.947                 0.016    0.1107022
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit min.RMSESD.fit
## 1     29.87947           NA           NA        0.1033123      0.8361541
##   max.RsquaredSD.fit
## 1         0.02379652
```

```r
# Used to compare vs. Interactions.High.cor.Y and/or Max.cor.Y.TmSrs
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.lm"
## [1] "    indep_vars: Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](Pew_Anonymity2_files/figure-html/fit.models_0-9.png) ![](Pew_Anonymity2_files/figure-html/fit.models_0-10.png) ![](Pew_Anonymity2_files/figure-html/fit.models_0-11.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](Pew_Anonymity2_files/figure-html/fit.models_0-12.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -74.720 -22.506   3.058  25.280  55.470 
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                           60.068      2.082  28.850  < 2e-16
## Worry.About.Info.my.fctr1             14.652      2.573   5.694 2.02e-08
## Worry.About.Info.my.fctrNA.my        -52.944     29.960  -1.767   0.0778
## Privacy.Laws.Effective.my.fctr1      -15.539      3.027  -5.134 3.95e-07
## Privacy.Laws.Effective.my.fctrNA.my   -7.124      4.624  -1.541   0.1240
##                                        
## (Intercept)                         ***
## Worry.About.Info.my.fctr1           ***
## Worry.About.Info.my.fctrNA.my       .  
## Privacy.Laws.Effective.my.fctr1     ***
## Privacy.Laws.Effective.my.fctrNA.my    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.63 on 551 degrees of freedom
## Multiple R-squared:  0.1236,	Adjusted R-squared:  0.1172 
## F-statistic: 19.43 on 4 and 551 DF,  p-value: 5.761e-15
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##       model_id model_method
## 1 Max.cor.Y.lm           lm
##                                                      feats max.nTuningRuns
## 1 Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr               1
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      0.841                 0.004    0.1235906
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit max.Rsquared.fit
## 1     29.77477           NA           NA        0.1172283        0.1089247
##   min.RMSESD.fit max.RsquaredSD.fit
## 1      0.8402025         0.01758807
```

```r
if (!is.null(glb_date_vars) && 
    (sum(grepl(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
               names(glb_allobs_df))) > 0)) {
# ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly1", 
#                         model_method=ifelse(glb_is_regression, "lm", 
#                                         ifelse(glb_is_binomial, "glm", "rpart")),
#                      model_type=glb_model_type,
#                         indep_vars_vctr=c(max_cor_y_x_vars, paste0(glb_date_vars, ".day.minutes")),
#                         rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                         fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                         n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
# 
ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=c(max_cor_y_x_vars, 
            grep(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
                        names(glb_allobs_df), value=TRUE)),
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
}

# Interactions.High.cor.Y
if (length(int_feats <- setdiff(unique(glb_feats_df$cor.high.X), NA)) > 0) {
    # lm & glm handle interaction terms; rpart & rf do not
    if (glb_is_regression || glb_is_binomial) {
        indep_vars_vctr <- 
            c(max_cor_y_x_vars, paste(max_cor_y_x_vars[1], int_feats, sep=":"))            
    } else { indep_vars_vctr <- union(max_cor_y_x_vars, int_feats) }
    
    ret_lst <- myfit_mdl(model_id="Interact.High.cor.Y", 
                            model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                         model_type=glb_model_type,
                            indep_vars_vctr,
                            glb_rsp_var, glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)                        
}    

# Low.cor.X
# if (glb_is_classification && glb_is_binomial)
#     indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
#                                             is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"] else
indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & !myNearZV & 
                              (exclude.as.feat != 1))[, "id"]  
myadjust_interaction_feats <- function(vars_vctr) {
    for (feat in subset(glb_feats_df, !is.na(interaction.feat))$id)
        if (feat %in% vars_vctr)
            vars_vctr <- union(setdiff(vars_vctr, feat), 
                paste0(glb_feats_df[glb_feats_df$id == feat, "interaction.feat"], ":", feat))
    return(vars_vctr)
}
indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)
ret_lst <- myfit_mdl(model_id="Low.cor.X", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                        indep_vars_vctr=indep_vars_vctr,
                        model_type=glb_model_type,                     
                        glb_rsp_var, glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Low.cor.X.lm"
## [1] "    indep_vars: Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](Pew_Anonymity2_files/figure-html/fit.models_0-13.png) ![](Pew_Anonymity2_files/figure-html/fit.models_0-14.png) ![](Pew_Anonymity2_files/figure-html/fit.models_0-15.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -76.048 -19.249   4.149  21.387  65.612 
## 
## Coefficients: (1 not defined because of singularities)
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                          34.9170    12.4594   2.802  0.00526
## Worry.About.Info.my.fctr1            13.7799     2.6606   5.179 3.20e-07
## Worry.About.Info.my.fctrNA.my       -57.2029    30.1759  -1.896  0.05856
## `Age.my.fctr(33.6,49.2]`             11.0518     3.7471   2.949  0.00333
## `Age.my.fctr(49.2,64.8]`             10.9517     3.4654   3.160  0.00167
## `Age.my.fctr(64.8,80.4]`              1.8203     4.3536   0.418  0.67604
## `Age.my.fctr(80.4,96.1]`              2.4955     9.2478   0.270  0.78738
## Age.my.fctrNA.my                     14.7688     8.7521   1.687  0.09212
## Tried.Masking.Identity.my.fctr1       4.6212     3.5949   1.285  0.19920
## Tried.Masking.Identity.my.fctrNA.my  -6.8003    13.4700  -0.505  0.61388
## Internet.Use.my                      17.9850     9.8992   1.817  0.06982
## Sex.fctrFemale                        2.2626     2.6248   0.862  0.38909
## Conservativeness.my.fctr2             0.8528     6.4467   0.132  0.89481
## Conservativeness.my.fctr3             5.3331     5.9749   0.893  0.37249
## Conservativeness.my.fctr4             4.5422     5.9929   0.758  0.44884
## Conservativeness.my.fctr5             3.9890     7.0603   0.565  0.57232
## Conservativeness.my.fctrNA.my         5.1013     7.9028   0.646  0.51888
## Smartphone.my.fctr1                   2.6720     2.9600   0.903  0.36709
## Smartphone.my.fctrNA.my               0.6564     8.4128   0.078  0.93784
## .rnorm                               -0.1930     1.2831  -0.150  0.88049
## Info.On.Internet.my.fctr1            -0.7005     5.4397  -0.129  0.89759
## Info.On.Internet.my.fctr10          -20.7025    10.8093  -1.915  0.05601
## Info.On.Internet.my.fctr11           16.5327    13.7849   1.199  0.23095
## Info.On.Internet.my.fctr2           -10.2710     5.0344  -2.040  0.04184
## Info.On.Internet.my.fctr3            -7.8068     5.1808  -1.507  0.13245
## Info.On.Internet.my.fctr4            -8.5032     5.0476  -1.685  0.09267
## Info.On.Internet.my.fctr5            -6.3418     5.4519  -1.163  0.24527
## Info.On.Internet.my.fctr6            -7.9571     5.8301  -1.365  0.17290
## Info.On.Internet.my.fctr7            -9.1325     5.8907  -1.550  0.12167
## Info.On.Internet.my.fctr8             4.3286     6.6599   0.650  0.51602
## Info.On.Internet.my.fctr9            -7.5259     9.1857  -0.819  0.41299
## Info.On.Internet.my.fctrNA.my             NA         NA      NA       NA
## Region.fctrSouth                      2.2245     4.0273   0.552  0.58094
## Region.fctrWest                       0.1380     4.2571   0.032  0.97414
## Region.fctrMidwest                   -0.2835     4.3720  -0.065  0.94832
## Anonymity.Possible.my.fctr1          -3.8693     2.6915  -1.438  0.15115
## Anonymity.Possible.my.fctrNA.my       1.9033     6.1261   0.311  0.75617
## Privacy.Laws.Effective.my.fctr1     -14.8815     3.1009  -4.799 2.09e-06
## Privacy.Laws.Effective.my.fctrNA.my  -6.7398     4.7663  -1.414  0.15795
##                                        
## (Intercept)                         ** 
## Worry.About.Info.my.fctr1           ***
## Worry.About.Info.my.fctrNA.my       .  
## `Age.my.fctr(33.6,49.2]`            ** 
## `Age.my.fctr(49.2,64.8]`            ** 
## `Age.my.fctr(64.8,80.4]`               
## `Age.my.fctr(80.4,96.1]`               
## Age.my.fctrNA.my                    .  
## Tried.Masking.Identity.my.fctr1        
## Tried.Masking.Identity.my.fctrNA.my    
## Internet.Use.my                     .  
## Sex.fctrFemale                         
## Conservativeness.my.fctr2              
## Conservativeness.my.fctr3              
## Conservativeness.my.fctr4              
## Conservativeness.my.fctr5              
## Conservativeness.my.fctrNA.my          
## Smartphone.my.fctr1                    
## Smartphone.my.fctrNA.my                
## .rnorm                                 
## Info.On.Internet.my.fctr1              
## Info.On.Internet.my.fctr10          .  
## Info.On.Internet.my.fctr11             
## Info.On.Internet.my.fctr2           *  
## Info.On.Internet.my.fctr3              
## Info.On.Internet.my.fctr4           .  
## Info.On.Internet.my.fctr5              
## Info.On.Internet.my.fctr6              
## Info.On.Internet.my.fctr7              
## Info.On.Internet.my.fctr8              
## Info.On.Internet.my.fctr9              
## Info.On.Internet.my.fctrNA.my          
## Region.fctrSouth                       
## Region.fctrWest                        
## Region.fctrMidwest                     
## Anonymity.Possible.my.fctr1            
## Anonymity.Possible.my.fctrNA.my        
## Privacy.Laws.Effective.my.fctr1     ***
## Privacy.Laws.Effective.my.fctrNA.my    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.24 on 518 degrees of freedom
## Multiple R-squared:  0.1977,	Adjusted R-squared:  0.1404 
## F-statistic: 3.449 on 37 and 518 DF,  p-value: 2.31e-10
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

![](Pew_Anonymity2_files/figure-html/fit.models_0-16.png) 

```
##       model_id model_method
## 1 Low.cor.X.lm           lm
##                                                                                                                                                                                                                                                       feats
## 1 Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.983                 0.015
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1    0.1976727     31.31739           NA           NA        0.1403636
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1       0.07528015       1.469406         0.02687989
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 10 fit.models          7          0 20.175 32.709  12.535
## 11 fit.models          7          1 32.710     NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor    bgn end elapsed
## 1 fit.models_1_bgn          1          0 34.331  NA      NA
```

```r
# Options:
#   1. rpart & rf manual tuning
#   2. rf without pca (default: with pca)

#stop(here); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
#glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df

# All X that is not user excluded
# if (glb_is_classification && glb_is_binomial) {
#     model_id_pfx <- "Conditional.X"
# # indep_vars_vctr <- setdiff(names(glb_fitobs_df), union(glb_rsp_var, glb_exclude_vars_as_features))
#     indep_vars_vctr <- subset(glb_feats_df, is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"]
# } else {
    model_id_pfx <- "All.X"
    indep_vars_vctr <- subset(glb_feats_df, !myNearZV &
                                            (exclude.as.feat != 1))[, "id"]
# }

indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)

for (method in glb_models_method_vctr) {
    fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, 
                                paste0("fit.models_1_", method), major.inc=TRUE)
    if (method %in% c("rpart", "rf")) {
        # rpart:    fubar's the tree
        # rf:       skip the scenario w/ .rnorm for speed
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(".rnorm"))
        model_id <- paste0(model_id_pfx, ".no.rnorm")
    } else model_id <- model_id_pfx
    
    if (method %in% c("glm")) # for a "robust" glm model
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(NULL
                                    ,"A.nchrs.log"      # correlated to "S.*"                                                      
                                    ,"A.ndgts.log"      # correlated to "S.*"
                                    ,"A.nuppr.log"      # correlated to "S.*"
                                    ,"A.npnct01.log" # identical  to "S.npnct01.log"
                                    ,"A.npnct03.log" # correlated to "S.npnct03.log"
                                    ,"A.npnct04.log" # correlated to "S.npnct04.log"
                                    ,"A.npnct06.log" # identical  to "S.npnct06.log"
                                    ,"A.npnct07.log" # identical  to "S.npnct07.log"
                                    ,"A.npnct08.log" # correlated to "S.npnct08.log"
                                    ,"A.npnct11.log" # correlated to "S.*"
                                    ,"A.npnct12.log" # correlated to "S.*"
                                    ,"S.npnct14.log" # correlated to "A.*"
                                    ,"A.npnct15.log" # correlated to "S.npnct15.log"
                                    ,"A.npnct16.log" # correlated to "S.npnct16.log"
                                    ,"A.npnct19.log" # correlated to "S.*"
                                    ,"A.npnct20.log" # identical  to "S.npnct20.log"
                                    ,"A.npnct21.log" # correlated to "S.npnct21.log"
                                    ,"A.P.daily.clip.report" # identical  to "S.*"
                                    ,"S.P.daily.clip.report" # identical  to "H.*"
                                    ,"A.P.http" # correlated  to "A.npnct14.log"
                                    ,"A.P.fashion.week" # identical  to "S.*"
                                    ,"H.P.first.draft" # correlated  to "H.T.first"
                                    ,"A.P.first.draft" # identical  to "S.*"
                                    ,"A.P.metropolitan.diary.colon" # identical  to "S.*"
                                    ,"A.P.year.colon" # identical  to "S.P.year.colon"
                                                      ))
    
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                            indep_vars_vctr=indep_vars_vctr,
                            model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # If All.X.glm is less accurate than Low.Cor.X.glm
    #   check NA coefficients & filter appropriate terms in indep_vars_vctr
#     if (method == "glm") {
#         orig_glm <- glb_models_lst[[paste0(model_id, ".", model_method)]]$finalModel
#         orig_glm <- glb_models_lst[["All.X.glm"]]$finalModel; print(summary(orig_glm))
#           vif_orig_glm <- vif(orig_glm); print(vif_orig_glm)
#           print(vif_orig_glm[!is.na(vif_orig_glm) & (vif_orig_glm == Inf)])
#           print(which.max(vif_orig_glm))
#           print(sort(vif_orig_glm[vif_orig_glm >= 1.0e+03], decreasing=TRUE))
#           glb_fitobs_df[c(1143, 3637, 3953, 4105), c("UniqueID", "Popular", "H.P.quandary", "Headline")]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE), ]
#           all.equal(glb_allobs_df$S.nuppr.log, glb_allobs_df$A.nuppr.log)
#           all.equal(glb_allobs_df$S.npnct19.log, glb_allobs_df$A.npnct19.log)
#           all.equal(glb_allobs_df$S.P.year.colon, glb_allobs_df$A.P.year.colon)
#           all.equal(glb_allobs_df$S.T.share, glb_allobs_df$A.T.share)
#           all.equal(glb_allobs_df$H.T.clip, glb_allobs_df$H.P.daily.clip.report)
#           cor(glb_allobs_df$S.T.herald, glb_allobs_df$S.T.tribun)
#           dsp_obs(Abstract.contains="[Dd]iar", cols=("Abstract"), all=TRUE)
#           dsp_obs(Abstract.contains="[Ss]hare", cols=("Abstract"), all=TRUE)
#           subset(glb_feats_df, cor.y.abs <= glb_feats_df[glb_feats_df$id == ".rnorm", "cor.y.abs"])
#         corxx_mtrx <- cor(data.matrix(glb_allobs_df[, setdiff(names(glb_allobs_df), myfind_chr_cols_df(glb_allobs_df))]), use="pairwise.complete.obs"); abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
#           which.max(abs_corxx_mtrx["S.T.tribun", ])
#           abs_corxx_mtrx["A.npnct08.log", "S.npnct08.log"]
#         step_glm <- step(orig_glm)
#     }
    # Since caret does not optimize rpart well
#     if (method == "rpart")
#         ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".cp.0"), model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,        
#             n_cv_folds=0, tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
}
```

```
##              label step_major step_minor    bgn    end elapsed
## 1 fit.models_1_bgn          1          0 34.331 34.345   0.014
## 2  fit.models_1_lm          2          0 34.345     NA      NA
## [1] "fitting model: All.X.lm"
## [1] "    indep_vars: Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](Pew_Anonymity2_files/figure-html/fit.models_1-1.png) ![](Pew_Anonymity2_files/figure-html/fit.models_1-2.png) ![](Pew_Anonymity2_files/figure-html/fit.models_1-3.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -76.048 -19.249   4.149  21.387  65.612 
## 
## Coefficients: (1 not defined because of singularities)
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                          34.9170    12.4594   2.802  0.00526
## Worry.About.Info.my.fctr1            13.7799     2.6606   5.179 3.20e-07
## Worry.About.Info.my.fctrNA.my       -57.2029    30.1759  -1.896  0.05856
## `Age.my.fctr(33.6,49.2]`             11.0518     3.7471   2.949  0.00333
## `Age.my.fctr(49.2,64.8]`             10.9517     3.4654   3.160  0.00167
## `Age.my.fctr(64.8,80.4]`              1.8203     4.3536   0.418  0.67604
## `Age.my.fctr(80.4,96.1]`              2.4955     9.2478   0.270  0.78738
## Age.my.fctrNA.my                     14.7688     8.7521   1.687  0.09212
## Tried.Masking.Identity.my.fctr1       4.6212     3.5949   1.285  0.19920
## Tried.Masking.Identity.my.fctrNA.my  -6.8003    13.4700  -0.505  0.61388
## Internet.Use.my                      17.9850     9.8992   1.817  0.06982
## Sex.fctrFemale                        2.2626     2.6248   0.862  0.38909
## Conservativeness.my.fctr2             0.8528     6.4467   0.132  0.89481
## Conservativeness.my.fctr3             5.3331     5.9749   0.893  0.37249
## Conservativeness.my.fctr4             4.5422     5.9929   0.758  0.44884
## Conservativeness.my.fctr5             3.9890     7.0603   0.565  0.57232
## Conservativeness.my.fctrNA.my         5.1013     7.9028   0.646  0.51888
## Smartphone.my.fctr1                   2.6720     2.9600   0.903  0.36709
## Smartphone.my.fctrNA.my               0.6564     8.4128   0.078  0.93784
## .rnorm                               -0.1930     1.2831  -0.150  0.88049
## Info.On.Internet.my.fctr1            -0.7005     5.4397  -0.129  0.89759
## Info.On.Internet.my.fctr10          -20.7025    10.8093  -1.915  0.05601
## Info.On.Internet.my.fctr11           16.5327    13.7849   1.199  0.23095
## Info.On.Internet.my.fctr2           -10.2710     5.0344  -2.040  0.04184
## Info.On.Internet.my.fctr3            -7.8068     5.1808  -1.507  0.13245
## Info.On.Internet.my.fctr4            -8.5032     5.0476  -1.685  0.09267
## Info.On.Internet.my.fctr5            -6.3418     5.4519  -1.163  0.24527
## Info.On.Internet.my.fctr6            -7.9571     5.8301  -1.365  0.17290
## Info.On.Internet.my.fctr7            -9.1325     5.8907  -1.550  0.12167
## Info.On.Internet.my.fctr8             4.3286     6.6599   0.650  0.51602
## Info.On.Internet.my.fctr9            -7.5259     9.1857  -0.819  0.41299
## Info.On.Internet.my.fctrNA.my             NA         NA      NA       NA
## Region.fctrSouth                      2.2245     4.0273   0.552  0.58094
## Region.fctrWest                       0.1380     4.2571   0.032  0.97414
## Region.fctrMidwest                   -0.2835     4.3720  -0.065  0.94832
## Anonymity.Possible.my.fctr1          -3.8693     2.6915  -1.438  0.15115
## Anonymity.Possible.my.fctrNA.my       1.9033     6.1261   0.311  0.75617
## Privacy.Laws.Effective.my.fctr1     -14.8815     3.1009  -4.799 2.09e-06
## Privacy.Laws.Effective.my.fctrNA.my  -6.7398     4.7663  -1.414  0.15795
##                                        
## (Intercept)                         ** 
## Worry.About.Info.my.fctr1           ***
## Worry.About.Info.my.fctrNA.my       .  
## `Age.my.fctr(33.6,49.2]`            ** 
## `Age.my.fctr(49.2,64.8]`            ** 
## `Age.my.fctr(64.8,80.4]`               
## `Age.my.fctr(80.4,96.1]`               
## Age.my.fctrNA.my                    .  
## Tried.Masking.Identity.my.fctr1        
## Tried.Masking.Identity.my.fctrNA.my    
## Internet.Use.my                     .  
## Sex.fctrFemale                         
## Conservativeness.my.fctr2              
## Conservativeness.my.fctr3              
## Conservativeness.my.fctr4              
## Conservativeness.my.fctr5              
## Conservativeness.my.fctrNA.my          
## Smartphone.my.fctr1                    
## Smartphone.my.fctrNA.my                
## .rnorm                                 
## Info.On.Internet.my.fctr1              
## Info.On.Internet.my.fctr10          .  
## Info.On.Internet.my.fctr11             
## Info.On.Internet.my.fctr2           *  
## Info.On.Internet.my.fctr3              
## Info.On.Internet.my.fctr4           .  
## Info.On.Internet.my.fctr5              
## Info.On.Internet.my.fctr6              
## Info.On.Internet.my.fctr7              
## Info.On.Internet.my.fctr8              
## Info.On.Internet.my.fctr9              
## Info.On.Internet.my.fctrNA.my          
## Region.fctrSouth                       
## Region.fctrWest                        
## Region.fctrMidwest                     
## Anonymity.Possible.my.fctr1            
## Anonymity.Possible.my.fctrNA.my        
## Privacy.Laws.Effective.my.fctr1     ***
## Privacy.Laws.Effective.my.fctrNA.my    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.24 on 518 degrees of freedom
## Multiple R-squared:  0.1977,	Adjusted R-squared:  0.1404 
## F-statistic: 3.449 on 37 and 518 DF,  p-value: 2.31e-10
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

![](Pew_Anonymity2_files/figure-html/fit.models_1-4.png) 

```
##   model_id model_method
## 1 All.X.lm           lm
##                                                                                                                                                                                                                                                       feats
## 1 Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.906                 0.015
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1    0.1976727     31.31739           NA           NA        0.1403636
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1       0.07528015       1.469406         0.02687989
##              label step_major step_minor    bgn    end elapsed
## 2  fit.models_1_lm          2          0 34.345 36.768   2.423
## 3 fit.models_1_glm          3          0 36.768     NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](Pew_Anonymity2_files/figure-html/fit.models_1-5.png) ![](Pew_Anonymity2_files/figure-html/fit.models_1-6.png) ![](Pew_Anonymity2_files/figure-html/fit.models_1-7.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -76.048  -19.249    4.149   21.387   65.612  
## 
## Coefficients: (1 not defined because of singularities)
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                          34.9170    12.4594   2.802  0.00526
## Worry.About.Info.my.fctr1            13.7799     2.6606   5.179 3.20e-07
## Worry.About.Info.my.fctrNA.my       -57.2029    30.1759  -1.896  0.05856
## `Age.my.fctr(33.6,49.2]`             11.0518     3.7471   2.949  0.00333
## `Age.my.fctr(49.2,64.8]`             10.9517     3.4654   3.160  0.00167
## `Age.my.fctr(64.8,80.4]`              1.8203     4.3536   0.418  0.67604
## `Age.my.fctr(80.4,96.1]`              2.4955     9.2478   0.270  0.78738
## Age.my.fctrNA.my                     14.7688     8.7521   1.687  0.09212
## Tried.Masking.Identity.my.fctr1       4.6212     3.5949   1.285  0.19920
## Tried.Masking.Identity.my.fctrNA.my  -6.8003    13.4700  -0.505  0.61388
## Internet.Use.my                      17.9850     9.8992   1.817  0.06982
## Sex.fctrFemale                        2.2626     2.6248   0.862  0.38909
## Conservativeness.my.fctr2             0.8528     6.4467   0.132  0.89481
## Conservativeness.my.fctr3             5.3331     5.9749   0.893  0.37249
## Conservativeness.my.fctr4             4.5422     5.9929   0.758  0.44884
## Conservativeness.my.fctr5             3.9890     7.0603   0.565  0.57232
## Conservativeness.my.fctrNA.my         5.1013     7.9028   0.646  0.51888
## Smartphone.my.fctr1                   2.6720     2.9600   0.903  0.36709
## Smartphone.my.fctrNA.my               0.6564     8.4128   0.078  0.93784
## .rnorm                               -0.1930     1.2831  -0.150  0.88049
## Info.On.Internet.my.fctr1            -0.7005     5.4397  -0.129  0.89759
## Info.On.Internet.my.fctr10          -20.7025    10.8093  -1.915  0.05601
## Info.On.Internet.my.fctr11           16.5327    13.7849   1.199  0.23095
## Info.On.Internet.my.fctr2           -10.2710     5.0344  -2.040  0.04184
## Info.On.Internet.my.fctr3            -7.8068     5.1808  -1.507  0.13245
## Info.On.Internet.my.fctr4            -8.5032     5.0476  -1.685  0.09267
## Info.On.Internet.my.fctr5            -6.3418     5.4519  -1.163  0.24527
## Info.On.Internet.my.fctr6            -7.9571     5.8301  -1.365  0.17290
## Info.On.Internet.my.fctr7            -9.1325     5.8907  -1.550  0.12167
## Info.On.Internet.my.fctr8             4.3286     6.6599   0.650  0.51602
## Info.On.Internet.my.fctr9            -7.5259     9.1857  -0.819  0.41299
## Info.On.Internet.my.fctrNA.my             NA         NA      NA       NA
## Region.fctrSouth                      2.2245     4.0273   0.552  0.58094
## Region.fctrWest                       0.1380     4.2571   0.032  0.97414
## Region.fctrMidwest                   -0.2835     4.3720  -0.065  0.94832
## Anonymity.Possible.my.fctr1          -3.8693     2.6915  -1.438  0.15115
## Anonymity.Possible.my.fctrNA.my       1.9033     6.1261   0.311  0.75617
## Privacy.Laws.Effective.my.fctr1     -14.8815     3.1009  -4.799 2.09e-06
## Privacy.Laws.Effective.my.fctrNA.my  -6.7398     4.7663  -1.414  0.15795
##                                        
## (Intercept)                         ** 
## Worry.About.Info.my.fctr1           ***
## Worry.About.Info.my.fctrNA.my       .  
## `Age.my.fctr(33.6,49.2]`            ** 
## `Age.my.fctr(49.2,64.8]`            ** 
## `Age.my.fctr(64.8,80.4]`               
## `Age.my.fctr(80.4,96.1]`               
## Age.my.fctrNA.my                    .  
## Tried.Masking.Identity.my.fctr1        
## Tried.Masking.Identity.my.fctrNA.my    
## Internet.Use.my                     .  
## Sex.fctrFemale                         
## Conservativeness.my.fctr2              
## Conservativeness.my.fctr3              
## Conservativeness.my.fctr4              
## Conservativeness.my.fctr5              
## Conservativeness.my.fctrNA.my          
## Smartphone.my.fctr1                    
## Smartphone.my.fctrNA.my                
## .rnorm                                 
## Info.On.Internet.my.fctr1              
## Info.On.Internet.my.fctr10          .  
## Info.On.Internet.my.fctr11             
## Info.On.Internet.my.fctr2           *  
## Info.On.Internet.my.fctr3              
## Info.On.Internet.my.fctr4           .  
## Info.On.Internet.my.fctr5              
## Info.On.Internet.my.fctr6              
## Info.On.Internet.my.fctr7              
## Info.On.Internet.my.fctr8              
## Info.On.Internet.my.fctr9              
## Info.On.Internet.my.fctrNA.my          
## Region.fctrSouth                       
## Region.fctrWest                        
## Region.fctrMidwest                     
## Anonymity.Possible.my.fctr1            
## Anonymity.Possible.my.fctrNA.my        
## Privacy.Laws.Effective.my.fctr1     ***
## Privacy.Laws.Effective.my.fctrNA.my    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 855.2114)
## 
##     Null deviance: 552143  on 555  degrees of freedom
## Residual deviance: 442999  on 518  degrees of freedom
## AIC: 5370.2
## 
## Number of Fisher Scoring iterations: 2
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                                                                                       feats
## 1 Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.977                 0.067
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB min.aic.fit
## 1    0.1976727     31.31739           NA           NA    5370.249
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1       0.07528015       1.469406         0.02687989
##                   label step_major step_minor    bgn    end elapsed
## 3      fit.models_1_glm          3          0 36.768 39.446   2.678
## 4 fit.models_1_bayesglm          4          0 39.446     NA      NA
## [1] "fitting model: All.X.bayesglm"
## [1] "    indep_vars: Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr"
```

```
## Loading required package: arm
## Loading required package: MASS
## 
## Attaching package: 'MASS'
## 
## The following object is masked from 'package:dplyr':
## 
##     select
## 
## Loading required package: Matrix
## Loading required package: lme4
## Loading required package: Rcpp
## 
## Attaching package: 'lme4'
## 
## The following object is masked from 'package:nlme':
## 
##     lmList
## 
## 
## arm (Version 1.8-5, built: 2015-05-13)
## 
## Working directory is /Users/bbalaji-2012/Documents/Work/Courses/MIT/Analytics_Edge_15_071x/Assignments/HW1_Pew_Anonymity
```

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -75.98  -19.29    4.22   21.42   65.50  
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                          35.0459    12.4214   2.821  0.00497
## Worry.About.Info.my.fctr1            13.7815     2.6619   5.177 3.23e-07
## Worry.About.Info.my.fctrNA.my       -53.8001    29.2871  -1.837  0.06679
## `Age.my.fctr(33.6,49.2]`             11.0295     3.7464   2.944  0.00339
## `Age.my.fctr(49.2,64.8]`             10.9181     3.4641   3.152  0.00172
## `Age.my.fctr(64.8,80.4]`              1.8000     4.3514   0.414  0.67929
## `Age.my.fctr(80.4,96.1]`              2.4664     9.2236   0.267  0.78926
## Age.my.fctrNA.my                     14.7009     8.7317   1.684  0.09286
## Tried.Masking.Identity.my.fctr1       4.6089     3.5960   1.282  0.20053
## Tried.Masking.Identity.my.fctrNA.my  -6.7242    13.3854  -0.502  0.61563
## Internet.Use.my                      17.8089     9.8665   1.805  0.07166
## Sex.fctrFemale                        2.2844     2.6259   0.870  0.38474
## Conservativeness.my.fctr2             0.7745     6.4167   0.121  0.90398
## Conservativeness.my.fctr3             5.2966     5.9457   0.891  0.37343
## Conservativeness.my.fctr4             4.5163     5.9637   0.757  0.44922
## Conservativeness.my.fctr5             3.9506     7.0301   0.562  0.57439
## Conservativeness.my.fctrNA.my         5.0581     7.8700   0.643  0.52070
## Smartphone.my.fctr1                   2.6593     2.9596   0.899  0.36931
## Smartphone.my.fctrNA.my               0.6621     8.3961   0.079  0.93718
## .rnorm                               -0.1916     1.2839  -0.149  0.88142
## Info.On.Internet.my.fctr1            -0.6843     5.4239  -0.126  0.89965
## Info.On.Internet.my.fctr10          -20.4873    10.7602  -1.904  0.05747
## Info.On.Internet.my.fctr11           16.3989    13.6899   1.198  0.23151
## Info.On.Internet.my.fctr2           -10.1889     5.0189  -2.030  0.04286
## Info.On.Internet.my.fctr3            -7.7262     5.1648  -1.496  0.13528
## Info.On.Internet.my.fctr4            -8.4232     5.0321  -1.674  0.09476
## Info.On.Internet.my.fctr5            -6.2591     5.4346  -1.152  0.24997
## Info.On.Internet.my.fctr6            -7.8700     5.8120  -1.354  0.17629
## Info.On.Internet.my.fctr7            -9.0423     5.8731  -1.540  0.12427
## Info.On.Internet.my.fctr8             4.3869     6.6399   0.661  0.50911
## Info.On.Internet.my.fctr9            -7.4100     9.1526  -0.810  0.41854
## Info.On.Internet.my.fctrNA.my         0.0000   157.8871   0.000  1.00000
## Region.fctrSouth                      2.2574     4.0245   0.561  0.57509
## Region.fctrWest                       0.1721     4.2542   0.040  0.96774
## Region.fctrMidwest                   -0.2539     4.3694  -0.058  0.95368
## Anonymity.Possible.my.fctr1          -3.8571     2.6929  -1.432  0.15266
## Anonymity.Possible.my.fctrNA.my       1.9082     6.1221   0.312  0.75541
## Privacy.Laws.Effective.my.fctr1     -14.8770     3.1023  -4.796 2.13e-06
## Privacy.Laws.Effective.my.fctrNA.my  -6.7867     4.7639  -1.425  0.15488
##                                        
## (Intercept)                         ** 
## Worry.About.Info.my.fctr1           ***
## Worry.About.Info.my.fctrNA.my       .  
## `Age.my.fctr(33.6,49.2]`            ** 
## `Age.my.fctr(49.2,64.8]`            ** 
## `Age.my.fctr(64.8,80.4]`               
## `Age.my.fctr(80.4,96.1]`               
## Age.my.fctrNA.my                    .  
## Tried.Masking.Identity.my.fctr1        
## Tried.Masking.Identity.my.fctrNA.my    
## Internet.Use.my                     .  
## Sex.fctrFemale                         
## Conservativeness.my.fctr2              
## Conservativeness.my.fctr3              
## Conservativeness.my.fctr4              
## Conservativeness.my.fctr5              
## Conservativeness.my.fctrNA.my          
## Smartphone.my.fctr1                    
## Smartphone.my.fctrNA.my                
## .rnorm                                 
## Info.On.Internet.my.fctr1              
## Info.On.Internet.my.fctr10          .  
## Info.On.Internet.my.fctr11             
## Info.On.Internet.my.fctr2           *  
## Info.On.Internet.my.fctr3              
## Info.On.Internet.my.fctr4           .  
## Info.On.Internet.my.fctr5              
## Info.On.Internet.my.fctr6              
## Info.On.Internet.my.fctr7              
## Info.On.Internet.my.fctr8              
## Info.On.Internet.my.fctr9              
## Info.On.Internet.my.fctrNA.my          
## Region.fctrSouth                       
## Region.fctrWest                        
## Region.fctrMidwest                     
## Anonymity.Possible.my.fctr1            
## Anonymity.Possible.my.fctrNA.my        
## Privacy.Laws.Effective.my.fctr1     ***
## Privacy.Laws.Effective.my.fctrNA.my    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 856.889)
## 
##     Null deviance: 552143  on 555  degrees of freedom
## Residual deviance: 443012  on 517  degrees of freedom
## AIC: 5372.3
## 
## Number of Fisher Scoring iterations: 8
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                                                                                                                                                                                       feats
## 1 Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.709                 0.112
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB min.aic.fit
## 1    0.1976507     31.28241           NA           NA    5372.264
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1       0.07570071       1.446939          0.0266417
##                   label step_major step_minor    bgn    end elapsed
## 4 fit.models_1_bayesglm          4          0 39.446 42.138   2.692
## 5    fit.models_1_rpart          5          0 42.139     NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr"
```

```
## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info =
## trainInfo, : There were missing values in resampled performance measures.
```

![](Pew_Anonymity2_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0349 on full training set
```

![](Pew_Anonymity2_files/figure-html/fit.models_1-9.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 556 
## 
##           CP nsplit rel error
## 1 0.07579970      0 1.0000000
## 2 0.03490253      1 0.9242003
## 
## Variable importance
##       Worry.About.Info.my.fctr1 Tried.Masking.Identity.my.fctr1 
##                              76                               9 
## Privacy.Laws.Effective.my.fctr1                 Region.fctrWest 
##                               5                               4 
##          Age.my.fctr(33.6,49.2]       Info.On.Internet.my.fctr5 
##                               4                               2 
## 
## Node number 1: 556 observations,    complexity param=0.0757997
##   mean=62.54204, MSE=993.0631 
##   left son=2 (292 obs) right son=3 (264 obs)
##   Primary splits:
##       Worry.About.Info.my.fctr1       < 0.5 to the left,  improve=0.075799700, (0 missing)
##       Privacy.Laws.Effective.my.fctr1 < 0.5 to the right, improve=0.055656000, (0 missing)
##       Age.my.fctr(33.6,49.2]          < 0.5 to the left,  improve=0.009163670, (0 missing)
##       Tried.Masking.Identity.my.fctr1 < 0.5 to the left,  improve=0.008489602, (0 missing)
##       Info.On.Internet.my.fctr8       < 0.5 to the left,  improve=0.007772879, (0 missing)
##   Surrogate splits:
##       Tried.Masking.Identity.my.fctr1 < 0.5 to the left,  agree=0.579, adj=0.114, (0 split)
##       Privacy.Laws.Effective.my.fctr1 < 0.5 to the right, agree=0.556, adj=0.064, (0 split)
##       Region.fctrWest                 < 0.5 to the left,  agree=0.552, adj=0.057, (0 split)
##       Age.my.fctr(33.6,49.2]          < 0.5 to the left,  agree=0.549, adj=0.049, (0 split)
##       Info.On.Internet.my.fctr5       < 0.5 to the left,  agree=0.540, adj=0.030, (0 split)
## 
## Node number 2: 292 observations
##   mean=54.29244, MSE=1062.076 
## 
## Node number 3: 264 observations
##   mean=71.66659, MSE=758.1997 
## 
## n= 556 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 556 552143.1 62.54204  
##   2) Worry.About.Info.my.fctr1< 0.5 292 310126.1 54.29244 *
##   3) Worry.About.Info.my.fctr1>=0.5 264 200164.7 71.66659 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                               feats
## 1 Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.071                 0.065
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit
## 1    0.0757997     30.69813           NA           NA       0.05886582
##   min.RMSESD.fit max.RsquaredSD.fit
## 1      0.8694026         0.03641824
##                label step_major step_minor    bgn    end elapsed
## 5 fit.models_1_rpart          5          0 42.139 45.066   2.927
## 6    fit.models_1_rf          6          0 45.066     NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## 
## The following object is masked from 'package:dplyr':
## 
##     combine
```

![](Pew_Anonymity2_files/figure-html/fit.models_1-10.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 2 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: mtry
```

![](Pew_Anonymity2_files/figure-html/fit.models_1-11.png) ![](Pew_Anonymity2_files/figure-html/fit.models_1-12.png) 

```
##                 Length Class      Mode     
## call              4    -none-     call     
## type              1    -none-     character
## predicted       556    -none-     numeric  
## mse             500    -none-     numeric  
## rsq             500    -none-     numeric  
## oob.times       556    -none-     numeric  
## importance       37    -none-     numeric  
## importanceSD      0    -none-     NULL     
## localImportance   0    -none-     NULL     
## proximity         0    -none-     NULL     
## ntree             1    -none-     numeric  
## mtry              1    -none-     numeric  
## forest           11    -none-     list     
## coefs             0    -none-     NULL     
## y               556    -none-     numeric  
## test              0    -none-     NULL     
## inbag             0    -none-     NULL     
## xNames           37    -none-     character
## problemType       1    -none-     character
## tuneValue         1    data.frame list     
## obsLevels         1    -none-     logical  
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                               feats
## 1 Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      7.635                 0.627
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit
## 1    0.2674834     30.40884           NA           NA       0.08078154
##   min.RMSESD.fit max.RsquaredSD.fit
## 1       0.667417        0.005363173
```

```r
# User specified
    # easier to exclude features
#model_id_pfx <- "";
# indep_vars_vctr <- setdiff(names(glb_fitobs_df), 
#                         union(union(glb_rsp_var, glb_exclude_vars_as_features), 
#                                 c("<feat1_name>", "<feat2_name>")))
# method <- ""                                

    # easier to include features
# sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df; sav_featsimp_df <- glb_featsimp_df
# glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df; glm_featsimp_df <- sav_featsimp_df
#table(glb_allobs_df$myCategory, glb_allobs_df$H.P.readers.respond, glb_allobs_df[, glb_rsp_var], useNA="ifany")
#model_id <- "Rank9.2"; indep_vars_vctr <- c(NULL
#    ,"<feat1>"
#    ,"<feat1>*<feat2>"
#    ,"<feat1>:<feat2>"
#                                            )
# for (method in c("bayesglm")) {
#     ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                     n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
# #     csm_mdl_id <- paste0(model_id, ".", method)
# #     csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".", method)]]);         print(head(csm_featsimp_df))
# }
#csm_featsimp_df[grepl("H.npnct19.log", row.names(csm_featsimp_df)), , FALSE]
#csm_OOBobs_df <- glb_get_predictions(glb_OOBobs_df, mdl_id=csm_mdl_id, rsp_var_out=glb_rsp_var_out, prob_threshold_def=glb_models_df[glb_models_df$model_id == csm_mdl_id, "opt.prob.threshold.OOB"])
#print(sprintf("%s OOB confusion matrix & accuracy: ", csm_mdl_id)); print(t(confusionMatrix(csm_OOBobs_df[, paste0(glb_rsp_var_out, csm_mdl_id)], csm_OOBobs_df[, glb_rsp_var])$table))

#glb_models_df[, "max.Accuracy.OOB", FALSE]
#varImp(glb_models_lst[["Low.cor.X.glm"]])
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.2.glm"]])$importance)
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.3.glm"]])$importance)
#glb_feats_df[grepl("npnct28", glb_feats_df$id), ]
#print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id)); print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], glb_OOBobs_df[, glb_rsp_var])$table))

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_fitobs_df), 
#                          union(glb_rsp_var, glb_exclude_vars_as_features)))
#         indep_vars_vctr_lst[["feat"]] <- feat

    # User specified combinatorial models
#     indep_vars_vctr_lst <- list()
#     combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), 
#                           <num_feats_to_choose>)
#     for (combn_ix in 1:ncol(combn_mtrx))
#         #print(combn_mtrx[, combn_ix])
#         indep_vars_vctr_lst[[combn_ix]] <- combn_mtrx[, combn_ix]
    
    # template for myfit_mdl
    #   rf is hard-coded in caret to recognize only Accuracy / Kappa evaluation metrics
    #       only for OOB in trainControl ?
    
#     ret_lst <- myfit_mdl_fn(model_id=paste0(model_id_pfx, ""), model_method=method,
#                             indep_vars_vctr=indep_vars_vctr,
#                             rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                             fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                             n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df,
#                             model_loss_mtrx=glb_model_metric_terms,
#                             model_summaryFunction=glb_model_metric_smmry,
#                             model_metric=glb_model_metric,
#                             model_metric_maximize=glb_model_metric_maximize)

# Simplify a model
# fit_df <- glb_fitobs_df; glb_mdl <- step(<complex>_mdl)

# Non-caret models
#     rpart_area_mdl <- rpart(reformulate("Area", response=glb_rsp_var), 
#                                data=glb_fitobs_df, #method="class", 
#                                control=rpart.control(cp=0.12),
#                            parms=list(loss=glb_model_metric_terms))
#     print("rpart_sel_wlm_mdl"); prp(rpart_sel_wlm_mdl)
# 

print(glb_models_df)
```

```
##                                            model_id model_method
## MFO.lm                                       MFO.lm           lm
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart        rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart        rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart        rpart
## Max.cor.Y.lm                           Max.cor.Y.lm           lm
## Low.cor.X.lm                           Low.cor.X.lm           lm
## All.X.lm                                   All.X.lm           lm
## All.X.glm                                 All.X.glm          glm
## All.X.bayesglm                       All.X.bayesglm     bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart        rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                               feats
## MFO.lm                                                                                                                                                                                                                                                                       .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                       Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                  Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr
## Max.cor.Y.rpart                                                                                                                                                                                                            Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr
## Max.cor.Y.lm                                                                                                                                                                                                               Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr
## Low.cor.X.lm              Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
## All.X.lm                  Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
## All.X.glm                 Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
## All.X.bayesglm            Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
## All.X.no.rnorm.rpart              Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
## All.X.no.rnorm.rf                 Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
##                           max.nTuningRuns min.elapsedtime.everything
## MFO.lm                                  0                      0.442
## Max.cor.Y.cv.0.rpart                    0                      0.532
## Max.cor.Y.cv.0.cp.0.rpart               0                      0.466
## Max.cor.Y.rpart                         3                      0.947
## Max.cor.Y.lm                            1                      0.841
## Low.cor.X.lm                            1                      0.983
## All.X.lm                                1                      0.906
## All.X.glm                               1                      0.977
## All.X.bayesglm                          1                      1.709
## All.X.no.rnorm.rpart                    3                      1.071
## All.X.no.rnorm.rf                       3                      7.635
##                           min.elapsedtime.final max.R.sq.fit min.RMSE.fit
## MFO.lm                                    0.002 1.346478e-05     31.51269
## Max.cor.Y.cv.0.rpart                      0.016 0.000000e+00     31.51290
## Max.cor.Y.cv.0.cp.0.rpart                 0.015 1.228362e-01     29.51405
## Max.cor.Y.rpart                           0.016 1.107022e-01     29.87947
## Max.cor.Y.lm                              0.004 1.235906e-01     29.77477
## Low.cor.X.lm                              0.015 1.976727e-01     31.31739
## All.X.lm                                  0.015 1.976727e-01     31.31739
## All.X.glm                                 0.067 1.976727e-01     31.31739
## All.X.bayesglm                            0.112 1.976507e-01     31.28241
## All.X.no.rnorm.rpart                      0.065 7.579970e-02     30.69813
## All.X.no.rnorm.rf                         0.627 2.674834e-01     30.40884
##                           max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## MFO.lm                              NA           NA     -0.001791565
## Max.cor.Y.cv.0.rpart                NA           NA               NA
## Max.cor.Y.cv.0.cp.0.rpart           NA           NA               NA
## Max.cor.Y.rpart                     NA           NA               NA
## Max.cor.Y.lm                        NA           NA      0.117228253
## Low.cor.X.lm                        NA           NA      0.140363573
## All.X.lm                            NA           NA      0.140363573
## All.X.glm                           NA           NA               NA
## All.X.bayesglm                      NA           NA               NA
## All.X.no.rnorm.rpart                NA           NA               NA
## All.X.no.rnorm.rf                   NA           NA               NA
##                           max.Rsquared.fit min.RMSESD.fit
## MFO.lm                                  NA             NA
## Max.cor.Y.cv.0.rpart                    NA             NA
## Max.cor.Y.cv.0.cp.0.rpart               NA             NA
## Max.cor.Y.rpart                 0.10331231      0.8361541
## Max.cor.Y.lm                    0.10892470      0.8402025
## Low.cor.X.lm                    0.07528015      1.4694062
## All.X.lm                        0.07528015      1.4694062
## All.X.glm                       0.07528015      1.4694062
## All.X.bayesglm                  0.07570071      1.4469388
## All.X.no.rnorm.rpart            0.05886582      0.8694026
## All.X.no.rnorm.rf               0.08078154      0.6674170
##                           max.RsquaredSD.fit min.aic.fit
## MFO.lm                                    NA          NA
## Max.cor.Y.cv.0.rpart                      NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                 NA          NA
## Max.cor.Y.rpart                  0.023796516          NA
## Max.cor.Y.lm                     0.017588068          NA
## Low.cor.X.lm                     0.026879886          NA
## All.X.lm                         0.026879886          NA
## All.X.glm                        0.026879886    5370.249
## All.X.bayesglm                   0.026641704    5372.264
## All.X.no.rnorm.rpart             0.036418243          NA
## All.X.no.rnorm.rf                0.005363173          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 6  fit.models_1_rf          6          0 45.066 55.002   9.936
## 7 fit.models_1_end          7          0 55.002     NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 11 fit.models          7          1 32.710 55.008  22.298
## 12 fit.models          7          2 55.008     NA      NA
```


```r
if (!is.null(glb_model_metric_smmry)) {
    stats_df <- glb_models_df[, "model_id", FALSE]

    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_fitobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "fit",
        						glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_OOBobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "OOB",
            					glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
#     tmp_models_df <- orderBy(~model_id, glb_models_df)
#     rownames(tmp_models_df) <- seq(1, nrow(tmp_models_df))
#     all.equal(subset(tmp_models_df[, names(stats_df)], model_id != "Random.myrandom_classfr"),
#               subset(stats_df, model_id != "Random.myrandom_classfr"))
#     print(subset(tmp_models_df[, names(stats_df)], model_id != "Random.myrandom_classfr")[, c("model_id", "max.Accuracy.fit")])
#     print(subset(stats_df, model_id != "Random.myrandom_classfr")[, c("model_id", "max.Accuracy.fit")])

    print("Merging following data into glb_models_df:")
    print(stats_mrg_df <- stats_df[, c(1, grep(glb_model_metric, names(stats_df)))])
    print(tmp_models_df <- orderBy(~model_id, glb_models_df[, c("model_id", grep(glb_model_metric, names(stats_df), value=TRUE))]))

    tmp2_models_df <- glb_models_df[, c("model_id", setdiff(names(glb_models_df), grep(glb_model_metric, names(stats_df), value=TRUE)))]
    tmp3_models_df <- merge(tmp2_models_df, stats_mrg_df, all.x=TRUE, sort=FALSE)
    print(tmp3_models_df)
    print(names(tmp3_models_df))
    print(glb_models_df <- subset(tmp3_models_df, select=-model_id.1))
}

plt_models_df <- glb_models_df[, -grep("SD|Upper|Lower", names(glb_models_df))]
for (var in grep("^min.", names(plt_models_df), value=TRUE)) {
    plt_models_df[, sub("min.", "inv.", var)] <- 
        #ifelse(all(is.na(tmp <- plt_models_df[, var])), NA, 1.0 / tmp)
        1.0 / plt_models_df[, var]
    plt_models_df <- plt_models_df[ , -grep(var, names(plt_models_df))]
}
print(plt_models_df)
```

```
##                                            model_id model_method
## MFO.lm                                       MFO.lm           lm
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart        rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart        rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart        rpart
## Max.cor.Y.lm                           Max.cor.Y.lm           lm
## Low.cor.X.lm                           Low.cor.X.lm           lm
## All.X.lm                                   All.X.lm           lm
## All.X.glm                                 All.X.glm          glm
## All.X.bayesglm                       All.X.bayesglm     bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart        rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                               feats
## MFO.lm                                                                                                                                                                                                                                                                       .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                       Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                  Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr
## Max.cor.Y.rpart                                                                                                                                                                                                            Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr
## Max.cor.Y.lm                                                                                                                                                                                                               Worry.About.Info.my.fctr, Privacy.Laws.Effective.my.fctr
## Low.cor.X.lm              Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
## All.X.lm                  Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
## All.X.glm                 Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
## All.X.bayesglm            Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
## All.X.no.rnorm.rpart              Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
## All.X.no.rnorm.rf                 Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
##                           max.nTuningRuns max.R.sq.fit max.R.sq.OOB
## MFO.lm                                  0 1.346478e-05           NA
## Max.cor.Y.cv.0.rpart                    0 0.000000e+00           NA
## Max.cor.Y.cv.0.cp.0.rpart               0 1.228362e-01           NA
## Max.cor.Y.rpart                         3 1.107022e-01           NA
## Max.cor.Y.lm                            1 1.235906e-01           NA
## Low.cor.X.lm                            1 1.976727e-01           NA
## All.X.lm                                1 1.976727e-01           NA
## All.X.glm                               1 1.976727e-01           NA
## All.X.bayesglm                          1 1.976507e-01           NA
## All.X.no.rnorm.rpart                    3 7.579970e-02           NA
## All.X.no.rnorm.rf                       3 2.674834e-01           NA
##                           max.Adj.R.sq.fit max.Rsquared.fit
## MFO.lm                        -0.001791565               NA
## Max.cor.Y.cv.0.rpart                    NA               NA
## Max.cor.Y.cv.0.cp.0.rpart               NA               NA
## Max.cor.Y.rpart                         NA       0.10331231
## Max.cor.Y.lm                   0.117228253       0.10892470
## Low.cor.X.lm                   0.140363573       0.07528015
## All.X.lm                       0.140363573       0.07528015
## All.X.glm                               NA       0.07528015
## All.X.bayesglm                          NA       0.07570071
## All.X.no.rnorm.rpart                    NA       0.05886582
## All.X.no.rnorm.rf                       NA       0.08078154
##                           inv.elapsedtime.everything inv.elapsedtime.final
## MFO.lm                                     2.2624434            500.000000
## Max.cor.Y.cv.0.rpart                       1.8796992             62.500000
## Max.cor.Y.cv.0.cp.0.rpart                  2.1459227             66.666667
## Max.cor.Y.rpart                            1.0559662             62.500000
## Max.cor.Y.lm                               1.1890606            250.000000
## Low.cor.X.lm                               1.0172940             66.666667
## All.X.lm                                   1.1037528             66.666667
## All.X.glm                                  1.0235415             14.925373
## All.X.bayesglm                             0.5851375              8.928571
## All.X.no.rnorm.rpart                       0.9337068             15.384615
## All.X.no.rnorm.rf                          0.1309758              1.594896
##                           inv.RMSE.fit inv.RMSE.OOB  inv.aic.fit
## MFO.lm                      0.03173325           NA           NA
## Max.cor.Y.cv.0.rpart        0.03173303           NA           NA
## Max.cor.Y.cv.0.cp.0.rpart   0.03388217           NA           NA
## Max.cor.Y.rpart             0.03346779           NA           NA
## Max.cor.Y.lm                0.03358548           NA           NA
## Low.cor.X.lm                0.03193114           NA           NA
## All.X.lm                    0.03193114           NA           NA
## All.X.glm                   0.03193114           NA 0.0001862111
## All.X.bayesglm              0.03196684           NA 0.0001861413
## All.X.no.rnorm.rpart        0.03257527           NA           NA
## All.X.no.rnorm.rf           0.03288518           NA           NA
```

```r
print(myplot_radar(radar_inp_df=plt_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 11. Consider specifying shapes manually. if you must have them.
```

```
## Warning in loop_apply(n, do.ply): Removed 7 rows containing missing values
## (geom_path).
```

```
## Warning in loop_apply(n, do.ply): Removed 48 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 19 rows containing missing values
## (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 11. Consider specifying shapes manually. if you must have them.
```

![](Pew_Anonymity2_files/figure-html/fit.models_2-1.png) 

```r
# print(myplot_radar(radar_inp_df=subset(plt_models_df, 
#         !(model_id %in% grep("random|MFO", plt_models_df$model_id, value=TRUE)))))

# Compute CI for <metric>SD
glb_models_df <- mutate(glb_models_df, 
                max.df = ifelse(max.nTuningRuns > 1, max.nTuningRuns - 1, NA),
                min.sd2ci.scaler = ifelse(is.na(max.df), NA, qt(0.975, max.df)))
for (var in grep("SD", names(glb_models_df), value=TRUE)) {
    # Does CI alredy exist ?
    var_components <- unlist(strsplit(var, "SD"))
    varActul <- paste0(var_components[1],          var_components[2])
    varUpper <- paste0(var_components[1], "Upper", var_components[2])
    varLower <- paste0(var_components[1], "Lower", var_components[2])
    if (varUpper %in% names(glb_models_df)) {
        warning(varUpper, " already exists in glb_models_df")
        # Assuming Lower also exists
        next
    }    
    print(sprintf("var:%s", var))
    # CI is dependent on sample size in t distribution; df=n-1
    glb_models_df[, varUpper] <- glb_models_df[, varActul] + 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
    glb_models_df[, varLower] <- glb_models_df[, varActul] - 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
}
```

```
## [1] "var:min.RMSESD.fit"
## [1] "var:max.RsquaredSD.fit"
```

```r
# Plot metrics with CI
plt_models_df <- glb_models_df[, "model_id", FALSE]
pltCI_models_df <- glb_models_df[, "model_id", FALSE]
for (var in grep("Upper", names(glb_models_df), value=TRUE)) {
    var_components <- unlist(strsplit(var, "Upper"))
    col_name <- unlist(paste(var_components, collapse=""))
    plt_models_df[, col_name] <- glb_models_df[, col_name]
    for (name in paste0(var_components[1], c("Upper", "Lower"), var_components[2]))
        pltCI_models_df[, name] <- glb_models_df[, name]
}

build_statsCI_data <- function(plt_models_df) {
    mltd_models_df <- melt(plt_models_df, id.vars="model_id")
    mltd_models_df$data <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) tail(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), "[.]")), 1))
    mltd_models_df$label <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) head(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), 
            paste0(".", mltd_models_df[row_ix, "data"]))), 1))
    #print(mltd_models_df)
    
    return(mltd_models_df)
}
mltd_models_df <- build_statsCI_data(plt_models_df)

mltdCI_models_df <- melt(pltCI_models_df, id.vars="model_id")
for (row_ix in 1:nrow(mltdCI_models_df)) {
    for (type in c("Upper", "Lower")) {
        if (length(var_components <- unlist(strsplit(
                as.character(mltdCI_models_df[row_ix, "variable"]), type))) > 1) {
            #print(sprintf("row_ix:%d; type:%s; ", row_ix, type))
            mltdCI_models_df[row_ix, "label"] <- var_components[1]
            mltdCI_models_df[row_ix, "data"] <- 
                unlist(strsplit(var_components[2], "[.]"))[2]
            mltdCI_models_df[row_ix, "type"] <- type
            break
        }
    }    
}
#print(mltdCI_models_df)
# castCI_models_df <- dcast(mltdCI_models_df, value ~ type, fun.aggregate=sum)
# print(castCI_models_df)
wideCI_models_df <- reshape(subset(mltdCI_models_df, select=-variable), 
                            timevar="type", 
        idvar=setdiff(names(mltdCI_models_df), c("type", "value", "variable")), 
                            direction="wide")
#print(wideCI_models_df)
mrgdCI_models_df <- merge(wideCI_models_df, mltd_models_df, all.x=TRUE)
#print(mrgdCI_models_df)

# Merge stats back in if CIs don't exist
goback_vars <- c()
for (var in unique(mltd_models_df$label)) {
    for (type in unique(mltd_models_df$data)) {
        var_type <- paste0(var, ".", type)
        # if this data is already present, next
        if (var_type %in% unique(paste(mltd_models_df$label, mltd_models_df$data,
                                       sep=".")))
            next
        #print(sprintf("var_type:%s", var_type))
        goback_vars <- c(goback_vars, var_type)
    }
}

if (length(goback_vars) > 0) {
    mltd_goback_df <- build_statsCI_data(glb_models_df[, c("model_id", goback_vars)])
    mltd_models_df <- rbind(mltd_models_df, mltd_goback_df)
}

mltd_models_df <- merge(mltd_models_df, glb_models_df[, c("model_id", "model_method")], 
                        all.x=TRUE)

png(paste0(glb_out_pfx, "models_bar.png"), width=480*3, height=480*2)
print(gp <- myplot_bar(mltd_models_df, "model_id", "value", colorcol_name="model_method") + 
        geom_errorbar(data=mrgdCI_models_df, 
            mapping=aes(x=model_id, ymax=value.Upper, ymin=value.Lower), width=0.5) + 
          facet_grid(label ~ data, scales="free") + 
          theme(axis.text.x = element_text(angle = 90,vjust = 0.5)))
```

```
## Warning in loop_apply(n, do.ply): Removed 3 rows containing missing values
## (position_stack).
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(gp)
```

```
## Warning in loop_apply(n, do.ply): Removed 3 rows containing missing values
## (position_stack).
```

![](Pew_Anonymity2_files/figure-html/fit.models_2-2.png) 

```r
# used for console inspection
model_evl_terms <- c(NULL)
for (metric in glb_model_evl_criteria)
    model_evl_terms <- c(model_evl_terms, 
                         ifelse(length(grep("max", metric)) > 0, "-", "+"), metric)
if (glb_is_classification && glb_is_binomial)
    model_evl_terms <- c(model_evl_terms, "-", "opt.prob.threshold.OOB")
model_sel_frmla <- as.formula(paste(c("~ ", model_evl_terms), collapse=" "))
dsp_models_cols <- c("model_id", glb_model_evl_criteria) 
if (glb_is_classification && glb_is_binomial) 
    dsp_models_cols <- c(dsp_models_cols, "opt.prob.threshold.OOB")
print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
```

```
##                     model_id min.RMSE.OOB max.R.sq.OOB max.Adj.R.sq.fit
## 6               Low.cor.X.lm           NA           NA      0.140363573
## 7                   All.X.lm           NA           NA      0.140363573
## 5               Max.cor.Y.lm           NA           NA      0.117228253
## 1                     MFO.lm           NA           NA     -0.001791565
## 2       Max.cor.Y.cv.0.rpart           NA           NA               NA
## 3  Max.cor.Y.cv.0.cp.0.rpart           NA           NA               NA
## 4            Max.cor.Y.rpart           NA           NA               NA
## 8                  All.X.glm           NA           NA               NA
## 9             All.X.bayesglm           NA           NA               NA
## 10      All.X.no.rnorm.rpart           NA           NA               NA
## 11         All.X.no.rnorm.rf           NA           NA               NA
```

```r
print(myplot_radar(radar_inp_df=dsp_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 11. Consider specifying shapes manually. if you must have them.
```

```
## Warning in loop_apply(n, do.ply): Removed 11 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 7 rows containing missing values
## (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 11. Consider specifying shapes manually. if you must have them.
```

![](Pew_Anonymity2_files/figure-html/fit.models_2-3.png) 

```r
print("Metrics used for model selection:"); print(model_sel_frmla)
```

```
## [1] "Metrics used for model selection:"
```

```
## ~+min.RMSE.OOB - max.R.sq.OOB - max.Adj.R.sq.fit
```

```r
print(sprintf("Best model id: %s", dsp_models_df[1, "model_id"]))
```

```
## [1] "Best model id: Low.cor.X.lm"
```

```r
if (is.null(glb_sel_mdl_id)) { 
    glb_sel_mdl_id <- dsp_models_df[1, "model_id"]
    if (glb_sel_mdl_id == "Interact.High.cor.Y.glm") {
        warning("glb_sel_mdl_id: Interact.High.cor.Y.glm; myextract_mdl_feats does not currently support interaction terms")
        glb_sel_mdl_id <- dsp_models_df[2, "model_id"]
    }
} else 
    print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

![](Pew_Anonymity2_files/figure-html/fit.models_2-4.png) ![](Pew_Anonymity2_files/figure-html/fit.models_2-5.png) ![](Pew_Anonymity2_files/figure-html/fit.models_2-6.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](Pew_Anonymity2_files/figure-html/fit.models_2-7.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -76.048 -19.249   4.149  21.387  65.612 
## 
## Coefficients: (1 not defined because of singularities)
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                          34.9170    12.4594   2.802  0.00526
## Worry.About.Info.my.fctr1            13.7799     2.6606   5.179 3.20e-07
## Worry.About.Info.my.fctrNA.my       -57.2029    30.1759  -1.896  0.05856
## `Age.my.fctr(33.6,49.2]`             11.0518     3.7471   2.949  0.00333
## `Age.my.fctr(49.2,64.8]`             10.9517     3.4654   3.160  0.00167
## `Age.my.fctr(64.8,80.4]`              1.8203     4.3536   0.418  0.67604
## `Age.my.fctr(80.4,96.1]`              2.4955     9.2478   0.270  0.78738
## Age.my.fctrNA.my                     14.7688     8.7521   1.687  0.09212
## Tried.Masking.Identity.my.fctr1       4.6212     3.5949   1.285  0.19920
## Tried.Masking.Identity.my.fctrNA.my  -6.8003    13.4700  -0.505  0.61388
## Internet.Use.my                      17.9850     9.8992   1.817  0.06982
## Sex.fctrFemale                        2.2626     2.6248   0.862  0.38909
## Conservativeness.my.fctr2             0.8528     6.4467   0.132  0.89481
## Conservativeness.my.fctr3             5.3331     5.9749   0.893  0.37249
## Conservativeness.my.fctr4             4.5422     5.9929   0.758  0.44884
## Conservativeness.my.fctr5             3.9890     7.0603   0.565  0.57232
## Conservativeness.my.fctrNA.my         5.1013     7.9028   0.646  0.51888
## Smartphone.my.fctr1                   2.6720     2.9600   0.903  0.36709
## Smartphone.my.fctrNA.my               0.6564     8.4128   0.078  0.93784
## .rnorm                               -0.1930     1.2831  -0.150  0.88049
## Info.On.Internet.my.fctr1            -0.7005     5.4397  -0.129  0.89759
## Info.On.Internet.my.fctr10          -20.7025    10.8093  -1.915  0.05601
## Info.On.Internet.my.fctr11           16.5327    13.7849   1.199  0.23095
## Info.On.Internet.my.fctr2           -10.2710     5.0344  -2.040  0.04184
## Info.On.Internet.my.fctr3            -7.8068     5.1808  -1.507  0.13245
## Info.On.Internet.my.fctr4            -8.5032     5.0476  -1.685  0.09267
## Info.On.Internet.my.fctr5            -6.3418     5.4519  -1.163  0.24527
## Info.On.Internet.my.fctr6            -7.9571     5.8301  -1.365  0.17290
## Info.On.Internet.my.fctr7            -9.1325     5.8907  -1.550  0.12167
## Info.On.Internet.my.fctr8             4.3286     6.6599   0.650  0.51602
## Info.On.Internet.my.fctr9            -7.5259     9.1857  -0.819  0.41299
## Info.On.Internet.my.fctrNA.my             NA         NA      NA       NA
## Region.fctrSouth                      2.2245     4.0273   0.552  0.58094
## Region.fctrWest                       0.1380     4.2571   0.032  0.97414
## Region.fctrMidwest                   -0.2835     4.3720  -0.065  0.94832
## Anonymity.Possible.my.fctr1          -3.8693     2.6915  -1.438  0.15115
## Anonymity.Possible.my.fctrNA.my       1.9033     6.1261   0.311  0.75617
## Privacy.Laws.Effective.my.fctr1     -14.8815     3.1009  -4.799 2.09e-06
## Privacy.Laws.Effective.my.fctrNA.my  -6.7398     4.7663  -1.414  0.15795
##                                        
## (Intercept)                         ** 
## Worry.About.Info.my.fctr1           ***
## Worry.About.Info.my.fctrNA.my       .  
## `Age.my.fctr(33.6,49.2]`            ** 
## `Age.my.fctr(49.2,64.8]`            ** 
## `Age.my.fctr(64.8,80.4]`               
## `Age.my.fctr(80.4,96.1]`               
## Age.my.fctrNA.my                    .  
## Tried.Masking.Identity.my.fctr1        
## Tried.Masking.Identity.my.fctrNA.my    
## Internet.Use.my                     .  
## Sex.fctrFemale                         
## Conservativeness.my.fctr2              
## Conservativeness.my.fctr3              
## Conservativeness.my.fctr4              
## Conservativeness.my.fctr5              
## Conservativeness.my.fctrNA.my          
## Smartphone.my.fctr1                    
## Smartphone.my.fctrNA.my                
## .rnorm                                 
## Info.On.Internet.my.fctr1              
## Info.On.Internet.my.fctr10          .  
## Info.On.Internet.my.fctr11             
## Info.On.Internet.my.fctr2           *  
## Info.On.Internet.my.fctr3              
## Info.On.Internet.my.fctr4           .  
## Info.On.Internet.my.fctr5              
## Info.On.Internet.my.fctr6              
## Info.On.Internet.my.fctr7              
## Info.On.Internet.my.fctr8              
## Info.On.Internet.my.fctr9              
## Info.On.Internet.my.fctrNA.my          
## Region.fctrSouth                       
## Region.fctrWest                        
## Region.fctrMidwest                     
## Anonymity.Possible.my.fctr1            
## Anonymity.Possible.my.fctrNA.my        
## Privacy.Laws.Effective.my.fctr1     ***
## Privacy.Laws.Effective.my.fctrNA.my    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.24 on 518 degrees of freedom
## Multiple R-squared:  0.1977,	Adjusted R-squared:  0.1404 
## F-statistic: 3.449 on 37 and 518 DF,  p-value: 2.31e-10
```

```
## [1] TRUE
```

```r
# From here to save(), this should all be in one function
#   these are executed in the same seq twice more:
#       fit.data.training & predict.data.new chunks
glb_get_predictions <- function(df, mdl_id, rsp_var_out, prob_threshold_def=NULL) {
    mdl <- glb_models_lst[[mdl_id]]
    rsp_var_out <- paste0(rsp_var_out, mdl_id)

    if (glb_is_regression) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        print(myplot_scatter(df, glb_rsp_var, rsp_var_out, smooth=TRUE))
        df[, paste0(rsp_var_out, ".err")] <- 
            abs(df[, rsp_var_out] - df[, glb_rsp_var])
        print(head(orderBy(reformulate(c("-", paste0(rsp_var_out, ".err"))), 
                           df)))                             
    }

    if (glb_is_classification && glb_is_binomial) {
        prob_threshold <- glb_models_df[glb_models_df$model_id == mdl_id, 
                                        "opt.prob.threshold.OOB"]
        if (is.null(prob_threshold) || is.na(prob_threshold)) {
            warning("Using default probability threshold: ", prob_threshold_def)
            if (is.null(prob_threshold <- prob_threshold_def))
                stop("Default probability threshold is NULL")
        }
        
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")[, 2]
        df[, rsp_var_out] <- 
        		factor(levels(df[, glb_rsp_var])[
    				(df[, paste0(rsp_var_out, ".prob")] >=
    					prob_threshold) * 1 + 1], levels(df[, glb_rsp_var]))
    
        # prediction stats already reported by myfit_mdl ???
    }    
    
    if (glb_is_classification && !glb_is_binomial) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")
    }

    return(df)
}    
glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out)
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/fit.models_2-8.png) 

```
##     Internet.Use Smartphone    Sex Age          State Region
## 794            1          1 Female  49       Virginia  South
## 80             1          0   Male  21        Florida  South
## 683            1          1   Male  43 North Carolina  South
## 460            1          0 Female  62       Virginia  South
## 954            1          1 Female  62        Florida  South
## 632            1          0 Female  55        Alabama  South
##     Conservativeness Info.On.Internet Worry.About.Info Privacy.Importance
## 794                3                0                1           11.11111
## 80                 3                8                1            0.00000
## 683                3                0                0            0.00000
## 460                2                8                1            0.00000
## 954                4                3                0            0.00000
## 632                2                0                0            0.00000
##     Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 794                  1                      0                      0
## 80                   1                      0                      0
## 683                  0                      0                      0
## 460                  0                      0                      1
## 954                 NA                      0                      0
## 632                  0                      0                      0
##     .rownames .src     .rnorm Internet.Use.my Age.my.fctr
## 794       794 Test -0.3440475               1 (33.6,49.2]
## 80         80 Test  0.5643370               1 (17.9,33.6]
## 683       683 Test -0.3761202               1 (33.6,49.2]
## 460       460 Test -1.1358528               1 (49.2,64.8]
## 954       954 Test  0.3327613               1 (49.2,64.8]
## 632       632 Test -0.6707990               1 (49.2,64.8]
##     Anonymity.Possible.my.fctr Conservativeness.my.fctr
## 794                          1                        3
## 80                           1                        3
## 683                          0                        3
## 460                          0                        2
## 954                      NA.my                        4
## 632                          0                        2
##     Info.On.Internet.my.fctr Privacy.Laws.Effective.my.fctr
## 794                        0                              0
## 80                         8                              0
## 683                        0                              0
## 460                        8                              1
## 954                        3                              0
## 632                        0                              0
##     Smartphone.my.fctr Tried.Masking.Identity.my.fctr
## 794                  1                              0
## 80                   0                              0
## 683                  1                              0
## 460                  0                              0
## 954                  1                              0
## 632                  0                              0
##     Worry.About.Info.my.fctr Sex.fctr     State.fctr Region.fctr
## 794                        1   Female       Virginia       South
## 80                         1     Male        Florida       South
## 683                        0     Male North Carolina       South
## 460                        1   Female       Virginia       South
## 954                        0   Female        Florida       South
## 632                        0   Female        Alabama       South
##     Privacy.Importance.predict.Low.cor.X.lm
## 794                                86.42286
## 80                                 74.58973
## 683                                74.25595
## 460                                72.63961
## 954                                69.58716
## 632                                69.32296
##     Privacy.Importance.predict.Low.cor.X.lm.err
## 794                                    75.31175
## 80                                     74.58973
## 683                                    74.25595
## 460                                    72.63961
## 954                                    69.58716
## 632                                    69.32296
```

```r
predct_accurate_var_name <- paste0(glb_rsp_var_out, glb_sel_mdl_id, ".accurate")
glb_OOBobs_df[, predct_accurate_var_name] <-
                    (glb_OOBobs_df[, glb_rsp_var] == 
                     glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)])

#stop(here"); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
glb_featsimp_df <- 
    myget_feats_importance(mdl=glb_sel_mdl, featsimp_df=NULL)
glb_featsimp_df[, paste0(glb_sel_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##                                      importance Low.cor.X.lm.importance
## Worry.About.Info.my.fctr1           100.0000000             100.0000000
## Privacy.Laws.Effective.my.fctr1      92.6128304              92.6128304
## `Age.my.fctr(49.2,64.8]`             60.7732556              60.7732556
## `Age.my.fctr(33.6,49.2]`             56.6756161              56.6756161
## Info.On.Internet.my.fctr2            39.0097635              39.0097635
## Info.On.Internet.my.fctr10           36.5824483              36.5824483
## Worry.About.Info.my.fctrNA.my        36.2015544              36.2015544
## Internet.Use.my                      34.6695694              34.6695694
## Age.my.fctrNA.my                     32.1563634              32.1563634
## Info.On.Internet.my.fctr4            32.1007392              32.1007392
## Info.On.Internet.my.fctr7            29.4920157              29.4920157
## Info.On.Internet.my.fctr3            28.6475914              28.6475914
## Anonymity.Possible.my.fctr1          27.3021584              27.3021584
## Privacy.Laws.Effective.my.fctrNA.my  26.8441377              26.8441377
## Info.On.Internet.my.fctr6            25.8877833              25.8877833
## Tried.Masking.Identity.my.fctr1      24.3459375              24.3459375
## Info.On.Internet.my.fctr11           22.6724300              22.6724300
## Info.On.Internet.my.fctr5            21.9709901              21.9709901
## Smartphone.my.fctr1                  16.9094201              16.9094201
## Conservativeness.my.fctr3            16.7124321              16.7124321
## Sex.fctrFemale                       16.1181262              16.1181262
## Info.On.Internet.my.fctr9            15.2886330              15.2886330
## Conservativeness.my.fctr4            14.0961369              14.0961369
## Info.On.Internet.my.fctr8            11.9980054              11.9980054
## Conservativeness.my.fctrNA.my        11.9118764              11.9118764
## Conservativeness.my.fctr5            10.3474601              10.3474601
## Region.fctrSouth                     10.1019949              10.1019949
## Tried.Masking.Identity.my.fctrNA.my   9.1788768               9.1788768
## `Age.my.fctr(64.8,80.4]`              7.4936724               7.4936724
## Anonymity.Possible.my.fctrNA.my       5.4063607               5.4063607
## `Age.my.fctr(80.4,96.1]`              4.6130643               4.6130643
## .rnorm                                2.2924783               2.2924783
## Conservativeness.my.fctr2             1.9402339               1.9402339
## Info.On.Internet.my.fctr1             1.8719519               1.8719519
## Smartphone.my.fctrNA.my               0.8859963               0.8859963
## Region.fctrMidwest                    0.6299939               0.6299939
## Region.fctrWest                       0.0000000               0.0000000
```

```r
# Used again in fit.data.training & predict.data.new chunks
glb_analytics_diag_plots <- function(obs_df, mdl_id, prob_threshold=NULL) {
    featsimp_df <- glb_featsimp_df
    featsimp_df$feat <- gsub("`(.*?)`", "\\1", row.names(featsimp_df))    
    featsimp_df$feat.interact <- gsub("(.*?):(.*)", "\\2", featsimp_df$feat)
    featsimp_df$feat <- gsub("(.*?):(.*)", "\\1", featsimp_df$feat)    
    featsimp_df$feat.interact <- ifelse(featsimp_df$feat.interact == featsimp_df$feat, 
                                        NA, featsimp_df$feat.interact)
    featsimp_df$feat <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat)
    featsimp_df$feat.interact <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat.interact) 
    featsimp_df <- orderBy(~ -importance.max, summaryBy(importance ~ feat + feat.interact, 
                                                        data=featsimp_df, FUN=max))    
    #rex_str=":(.*)"; txt_vctr=tail(featsimp_df$feat); ret_lst <- regexec(rex_str, txt_vctr); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])    
    if (nrow(featsimp_df) > 5) {
        warning("Limiting important feature scatter plots to 5 out of ", nrow(featsimp_df))
        featsimp_df <- head(featsimp_df, 5)
    }
#     if (!all(is.na(featsimp_df$feat.interact)))
#         stop("not implemented yet")
    rsp_var_out <- paste0(glb_rsp_var_out, mdl_id)
    for (var in featsimp_df$feat) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, rsp_var_out))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", colorcol_name="variable",
                                 facet_colcol_name="variable", jitter=TRUE) + 
                      guides(color=FALSE))
    }
    
    if (glb_is_regression) {
        if (nrow(featsimp_df) == 0)
            warning("No important features in glb_fin_mdl") else
            print(myplot_prediction_regression(df=obs_df, 
                        feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2],
                                      ".rownames"), 
                                               feat_y=featsimp_df$feat[1],
                        rsp_var=glb_rsp_var, rsp_var_out=rsp_var_out,
                        id_vars=glb_id_var)
    #               + facet_wrap(reformulate(featsimp_df$feat[2])) # if [1 or 2] is a factor
    #               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
                  )
    }    
    
    if (glb_is_classification) {
        if (nrow(featsimp_df) == 0)
            warning("No features in selected model are statistically important")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2], 
                              ".rownames"),
                                               feat_y=featsimp_df$feat[1],
                     rsp_var=glb_rsp_var, 
                     rsp_var_out=rsp_var_out, 
                     id_vars=glb_id_var,
                    prob_threshold=prob_threshold)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id)                  
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_OOBobs_df, mdl_id =
## glb_sel_mdl_id): Limiting important feature scatter plots to 5 out of 12
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/fit.models_2-9.png) 

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/fit.models_2-10.png) 

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/fit.models_2-11.png) 

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/fit.models_2-12.png) 

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/fit.models_2-13.png) 

```
##     Internet.Use Smartphone    Sex Age          State Region
## 794            1          1 Female  49       Virginia  South
## 80             1          0   Male  21        Florida  South
## 683            1          1   Male  43 North Carolina  South
## 460            1          0 Female  62       Virginia  South
## 954            1          1 Female  62        Florida  South
##     Conservativeness Info.On.Internet Worry.About.Info Privacy.Importance
## 794                3                0                1           11.11111
## 80                 3                8                1            0.00000
## 683                3                0                0            0.00000
## 460                2                8                1            0.00000
## 954                4                3                0            0.00000
##     Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 794                  1                      0                      0
## 80                   1                      0                      0
## 683                  0                      0                      0
## 460                  0                      0                      1
## 954                 NA                      0                      0
##     .rownames .src     .rnorm Internet.Use.my Age.my.fctr
## 794       794 Test -0.3440475               1 (33.6,49.2]
## 80         80 Test  0.5643370               1 (17.9,33.6]
## 683       683 Test -0.3761202               1 (33.6,49.2]
## 460       460 Test -1.1358528               1 (49.2,64.8]
## 954       954 Test  0.3327613               1 (49.2,64.8]
##     Anonymity.Possible.my.fctr Conservativeness.my.fctr
## 794                          1                        3
## 80                           1                        3
## 683                          0                        3
## 460                          0                        2
## 954                      NA.my                        4
##     Info.On.Internet.my.fctr Privacy.Laws.Effective.my.fctr
## 794                        0                              0
## 80                         8                              0
## 683                        0                              0
## 460                        8                              1
## 954                        3                              0
##     Smartphone.my.fctr Tried.Masking.Identity.my.fctr
## 794                  1                              0
## 80                   0                              0
## 683                  1                              0
## 460                  0                              0
## 954                  1                              0
##     Worry.About.Info.my.fctr Sex.fctr     State.fctr Region.fctr
## 794                        1   Female       Virginia       South
## 80                         1     Male        Florida       South
## 683                        0     Male North Carolina       South
## 460                        1   Female       Virginia       South
## 954                        0   Female        Florida       South
##     Privacy.Importance.predict.Low.cor.X.lm
## 794                                86.42286
## 80                                 74.58973
## 683                                74.25595
## 460                                72.63961
## 954                                69.58716
##     Privacy.Importance.predict.Low.cor.X.lm.err
## 794                                    75.31175
## 80                                     74.58973
## 683                                    74.25595
## 460                                    72.63961
## 954                                    69.58716
##     Privacy.Importance.predict.Low.cor.X.lm.accurate .label
## 794                                            FALSE    794
## 80                                             FALSE     80
## 683                                            FALSE    683
## 460                                            FALSE    460
## 954                                            FALSE    954
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/fit.models_2-14.png) 

```r
# gather predictions from models better than MFO.*
#mdl_id <- "Conditional.X.rf"
#mdl_id <- "Conditional.X.cp.0.rpart"
#mdl_id <- "Conditional.X.rpart"
# glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id,
#                                      glb_rsp_var_out)
# print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, mdl_id)], 
#                         glb_OOBobs_df[, glb_rsp_var])$table))
FN_OOB_ids <- c(4721, 4020, 693, 92)
print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                    grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
```

```
## [1] Privacy.Importance                              
## [2] Privacy.Importance.predict.Low.cor.X.lm         
## [3] Privacy.Importance.predict.Low.cor.X.lm.err     
## [4] Privacy.Importance.predict.Low.cor.X.lm.accurate
## <0 rows> (or 0-length row.names)
```

```r
print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                    glb_feats_df$id[1:5]])
```

```
## [1] Worry.About.Info               Worry.About.Info.my.fctr      
## [3] Tried.Masking.Identity         Age.my.fctr                   
## [5] Tried.Masking.Identity.my.fctr
## <0 rows> (or 0-length row.names)
```

```r
print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                    glb_txt_vars])
```

```
## data frame with 0 columns and 0 rows
```

```r
write.csv(glb_OOBobs_df[, c(glb_id_var, 
                grep(glb_rsp_var, names(glb_OOBobs_df), fixed=TRUE, value=TRUE))], 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_sel_mdl_id), fixed=TRUE), 
           "_OOBobs.csv"), row.names=FALSE)

# print(glb_allobs_df[glb_allobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
# dsp_tbl(Headline.contains="[Ee]bola")
# sum(sel_obs(Headline.contains="[Ee]bola"))
# ftable(xtabs(Popular ~ NewsDesk.fctr, data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,]))
# xtabs(NewsDesk ~ Popular, #Popular ~ NewsDesk.fctr, 
#       data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,],
#       exclude=NULL)
# print(mycreate_xtab_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], 
#                       tbl_col_names=c("Popular", "NewsDesk")))

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 12 fit.models          7          2 55.008 65.646  10.638
## 13 fit.models          7          3 65.646     NA      NA
```


```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## [1] "Privacy.Importance.predict.Low.cor.X.lm"         
## [2] "Privacy.Importance.predict.Low.cor.X.lm.err"     
## [3] "Privacy.Importance.predict.Low.cor.X.lm.accurate"
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_sel_mdl, glb_sel_mdl_id,
         glb_model_type,
        file=paste0(glb_out_pfx, "selmdl_dsk.RData"))
#load(paste0(glb_out_pfx, "selmdl_dsk.RData"))

rm(ret_lst)
```

```
## Warning in rm(ret_lst): object 'ret_lst' not found
```

```r
replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "model.selected")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0
```

![](Pew_Anonymity2_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor    bgn    end elapsed
## 13        fit.models          7          3 65.646 70.767   5.121
## 14 fit.data.training          8          0 70.768     NA      NA
```

## Step `8.0: fit data training`

```r
#load(paste0(glb_inp_pfx, "dsk.RData"))

# To create specific models
# glb_fin_mdl_id <- NULL; glb_fin_mdl <- NULL; 
# glb_sel_mdl_id <- "Conditional.X.cp.0.rpart"; 
# glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]]; print(glb_sel_mdl)
    
if (!is.null(glb_fin_mdl_id) && (glb_fin_mdl_id %in% names(glb_models_lst))) {
    warning("Final model same as user selected model")
    glb_fin_mdl <- glb_sel_mdl
} else {    
#     print(mdl_feats_df <- myextract_mdl_feats(sel_mdl=glb_sel_mdl, 
#                                               entity_df=glb_fitobs_df))
    
    if ((model_method <- glb_sel_mdl$method) == "custom")
        # get actual method from the model_id
        model_method <- tail(unlist(strsplit(glb_sel_mdl_id, "[.]")), 1)
        
    tune_finmdl_df <- NULL
    if (nrow(glb_sel_mdl$bestTune) > 0) {
        for (param in names(glb_sel_mdl$bestTune)) {
            #print(sprintf("param: %s", param))
            if (glb_sel_mdl$bestTune[1, param] != "none")
                tune_finmdl_df <- rbind(tune_finmdl_df, 
                    data.frame(parameter=param, 
                               min=glb_sel_mdl$bestTune[1, param], 
                               max=glb_sel_mdl$bestTune[1, param], 
                               by=1)) # by val does not matter
        }
    } 
    
    # Sync with parameters in mydsutils.R
    require(gdata)
    ret_lst <- myfit_mdl(model_id="Final", model_method=model_method,
        indep_vars_vctr=trim(unlist(strsplit(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id,
                                                    "feats"], "[,]"))), 
                         model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out, 
                            fit_df=glb_trnobs_df, OOB_df=NULL,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=tune_finmdl_df,
                         # Automate from here
                         #  Issues if glb_sel_mdl$method == "rf" b/c trainControl is "oob"; not "cv"
                            model_loss_mtrx=glb_model_metric_terms,
                            model_summaryFunction=glb_sel_mdl$control$summaryFunction,
                            model_metric=glb_sel_mdl$metric,
                            model_metric_maximize=glb_sel_mdl$maximize)
    glb_fin_mdl <- glb_models_lst[[length(glb_models_lst)]] 
    glb_fin_mdl_id <- glb_models_df[length(glb_models_lst), "model_id"]
}
```

```
## Loading required package: gdata
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## 
## The following object is masked from 'package:randomForest':
## 
##     combine
## 
## The following objects are masked from 'package:dplyr':
## 
##     combine, first, last
## 
## The following object is masked from 'package:stats':
## 
##     nobs
## 
## The following object is masked from 'package:utils':
## 
##     object.size
```

```
## [1] "fitting model: Final.lm"
## [1] "    indep_vars: Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](Pew_Anonymity2_files/figure-html/fit.data.training_0-1.png) ![](Pew_Anonymity2_files/figure-html/fit.data.training_0-2.png) ![](Pew_Anonymity2_files/figure-html/fit.data.training_0-3.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -76.048 -19.249   4.149  21.387  65.612 
## 
## Coefficients: (1 not defined because of singularities)
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                          34.9170    12.4594   2.802  0.00526
## Worry.About.Info.my.fctr1            13.7799     2.6606   5.179 3.20e-07
## Worry.About.Info.my.fctrNA.my       -57.2029    30.1759  -1.896  0.05856
## `Age.my.fctr(33.6,49.2]`             11.0518     3.7471   2.949  0.00333
## `Age.my.fctr(49.2,64.8]`             10.9517     3.4654   3.160  0.00167
## `Age.my.fctr(64.8,80.4]`              1.8203     4.3536   0.418  0.67604
## `Age.my.fctr(80.4,96.1]`              2.4955     9.2478   0.270  0.78738
## Age.my.fctrNA.my                     14.7688     8.7521   1.687  0.09212
## Tried.Masking.Identity.my.fctr1       4.6212     3.5949   1.285  0.19920
## Tried.Masking.Identity.my.fctrNA.my  -6.8003    13.4700  -0.505  0.61388
## Internet.Use.my                      17.9850     9.8992   1.817  0.06982
## Sex.fctrFemale                        2.2626     2.6248   0.862  0.38909
## Conservativeness.my.fctr2             0.8528     6.4467   0.132  0.89481
## Conservativeness.my.fctr3             5.3331     5.9749   0.893  0.37249
## Conservativeness.my.fctr4             4.5422     5.9929   0.758  0.44884
## Conservativeness.my.fctr5             3.9890     7.0603   0.565  0.57232
## Conservativeness.my.fctrNA.my         5.1013     7.9028   0.646  0.51888
## Smartphone.my.fctr1                   2.6720     2.9600   0.903  0.36709
## Smartphone.my.fctrNA.my               0.6564     8.4128   0.078  0.93784
## .rnorm                               -0.1930     1.2831  -0.150  0.88049
## Info.On.Internet.my.fctr1            -0.7005     5.4397  -0.129  0.89759
## Info.On.Internet.my.fctr10          -20.7025    10.8093  -1.915  0.05601
## Info.On.Internet.my.fctr11           16.5327    13.7849   1.199  0.23095
## Info.On.Internet.my.fctr2           -10.2710     5.0344  -2.040  0.04184
## Info.On.Internet.my.fctr3            -7.8068     5.1808  -1.507  0.13245
## Info.On.Internet.my.fctr4            -8.5032     5.0476  -1.685  0.09267
## Info.On.Internet.my.fctr5            -6.3418     5.4519  -1.163  0.24527
## Info.On.Internet.my.fctr6            -7.9571     5.8301  -1.365  0.17290
## Info.On.Internet.my.fctr7            -9.1325     5.8907  -1.550  0.12167
## Info.On.Internet.my.fctr8             4.3286     6.6599   0.650  0.51602
## Info.On.Internet.my.fctr9            -7.5259     9.1857  -0.819  0.41299
## Info.On.Internet.my.fctrNA.my             NA         NA      NA       NA
## Region.fctrSouth                      2.2245     4.0273   0.552  0.58094
## Region.fctrWest                       0.1380     4.2571   0.032  0.97414
## Region.fctrMidwest                   -0.2835     4.3720  -0.065  0.94832
## Anonymity.Possible.my.fctr1          -3.8693     2.6915  -1.438  0.15115
## Anonymity.Possible.my.fctrNA.my       1.9033     6.1261   0.311  0.75617
## Privacy.Laws.Effective.my.fctr1     -14.8815     3.1009  -4.799 2.09e-06
## Privacy.Laws.Effective.my.fctrNA.my  -6.7398     4.7663  -1.414  0.15795
##                                        
## (Intercept)                         ** 
## Worry.About.Info.my.fctr1           ***
## Worry.About.Info.my.fctrNA.my       .  
## `Age.my.fctr(33.6,49.2]`            ** 
## `Age.my.fctr(49.2,64.8]`            ** 
## `Age.my.fctr(64.8,80.4]`               
## `Age.my.fctr(80.4,96.1]`               
## Age.my.fctrNA.my                    .  
## Tried.Masking.Identity.my.fctr1        
## Tried.Masking.Identity.my.fctrNA.my    
## Internet.Use.my                     .  
## Sex.fctrFemale                         
## Conservativeness.my.fctr2              
## Conservativeness.my.fctr3              
## Conservativeness.my.fctr4              
## Conservativeness.my.fctr5              
## Conservativeness.my.fctrNA.my          
## Smartphone.my.fctr1                    
## Smartphone.my.fctrNA.my                
## .rnorm                                 
## Info.On.Internet.my.fctr1              
## Info.On.Internet.my.fctr10          .  
## Info.On.Internet.my.fctr11             
## Info.On.Internet.my.fctr2           *  
## Info.On.Internet.my.fctr3              
## Info.On.Internet.my.fctr4           .  
## Info.On.Internet.my.fctr5              
## Info.On.Internet.my.fctr6              
## Info.On.Internet.my.fctr7              
## Info.On.Internet.my.fctr8              
## Info.On.Internet.my.fctr9              
## Info.On.Internet.my.fctrNA.my          
## Region.fctrSouth                       
## Region.fctrWest                        
## Region.fctrMidwest                     
## Anonymity.Possible.my.fctr1            
## Anonymity.Possible.my.fctrNA.my        
## Privacy.Laws.Effective.my.fctr1     ***
## Privacy.Laws.Effective.my.fctrNA.my    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.24 on 518 degrees of freedom
## Multiple R-squared:  0.1977,	Adjusted R-squared:  0.1404 
## F-statistic: 3.449 on 37 and 518 DF,  p-value: 2.31e-10
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

![](Pew_Anonymity2_files/figure-html/fit.data.training_0-4.png) 

```
##   model_id model_method
## 1 Final.lm           lm
##                                                                                                                                                                                                                                                       feats
## 1 Worry.About.Info.my.fctr, Age.my.fctr, Tried.Masking.Identity.my.fctr, Internet.Use.my, Sex.fctr, Conservativeness.my.fctr, Smartphone.my.fctr, .rnorm, Info.On.Internet.my.fctr, Region.fctr, Anonymity.Possible.my.fctr, Privacy.Laws.Effective.my.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.029                 0.015
##   max.R.sq.fit min.RMSE.fit max.Adj.R.sq.fit max.Rsquared.fit
## 1    0.1976727     31.31739        0.1403636       0.07528015
##   min.RMSESD.fit max.RsquaredSD.fit
## 1       1.469406         0.02687989
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor    bgn    end elapsed
## 14 fit.data.training          8          0 70.768 78.034   7.266
## 15 fit.data.training          8          1 78.035     NA      NA
```


```r
glb_trnobs_df <- glb_get_predictions(df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

![](Pew_Anonymity2_files/figure-html/fit.data.training_1-1.png) 

```
##     Internet.Use Smartphone    Sex Age          State Region
## 487            1          0 Female  NA       Kentucky  South
## 262            1          0   Male  66      Tennessee  South
## 14             1          1 Female  47 North Carolina  South
## 967            1          1   Male  50     California   West
## 698            1          0   Male  60      Louisiana  South
## 408            1          1 Female  60      Tennessee  South
##     Conservativeness Info.On.Internet Worry.About.Info Privacy.Importance
## 487                4                1                0                0.0
## 262                4                1                1                0.0
## 14                 3                0                0                0.0
## 967                4                1                1               12.5
## 698                5                0                0                0.0
## 408                5                4                0                0.0
##     Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 487                  0                      0                      0
## 262                  0                      0                      0
## 14                   1                      0                      0
## 967                  0                      0                      0
## 698                  0                      0                      0
## 408                  0                      0                      0
##     .rownames  .src     .rnorm Internet.Use.my Age.my.fctr
## 487       487 Train -0.2531848               1       NA.my
## 262       262 Train -0.5622947               1 (64.8,80.4]
## 14         14 Train  0.7036249               1 (33.6,49.2]
## 967       967 Train  0.7633980               1 (49.2,64.8]
## 698       698 Train -1.0376304               1 (49.2,64.8]
## 408       408 Train -2.2727122               1 (49.2,64.8]
##     Anonymity.Possible.my.fctr Conservativeness.my.fctr
## 487                          0                        4
## 262                          0                        4
## 14                           1                        3
## 967                          0                        4
## 698                          0                        5
## 408                          0                        5
##     Info.On.Internet.my.fctr Privacy.Laws.Effective.my.fctr
## 487                        1                              0
## 262                        1                              0
## 14                         0                              0
## 967                        1                              0
## 698                        0                              0
## 408                        4                              0
##     Smartphone.my.fctr Tried.Masking.Identity.my.fctr
## 487                  0                              0
## 262                  0                              0
## 14                   1                              0
## 967                  1                              0
## 698                  0                              0
## 408                  1                              0
##     Worry.About.Info.my.fctr Sex.fctr     State.fctr Region.fctr
## 487                        0   Female       Kentucky       South
## 262                        1     Male      Tennessee       South
## 14                         0   Female North Carolina       South
## 967                        1     Male     California        West
## 698                        0     Male      Louisiana       South
## 408                        0   Female      Tennessee       South
##     Privacy.Importance.predict.Final.lm
## 487                            76.04835
## 262                            74.67681
## 14                             72.44079
## 967                            84.13790
## 698                            70.26737
## 408                            66.93714
##     Privacy.Importance.predict.Final.lm.err
## 487                                76.04835
## 262                                74.67681
## 14                                 72.44079
## 967                                71.63790
## 698                                70.26737
## 408                                66.93714
```

```r
sav_featsimp_df <- glb_featsimp_df
#glb_feats_df <- sav_feats_df
# glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
#                                                entity_df=glb_trnobs_df)
glb_featsimp_df <- myget_feats_importance(mdl=glb_fin_mdl, featsimp_df=glb_featsimp_df)
glb_featsimp_df[, paste0(glb_fin_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##                                     Low.cor.X.lm.importance  importance
## Worry.About.Info.my.fctr1                       100.0000000 100.0000000
## Privacy.Laws.Effective.my.fctr1                  92.6128304  92.6128304
## `Age.my.fctr(49.2,64.8]`                         60.7732556  60.7732556
## `Age.my.fctr(33.6,49.2]`                         56.6756161  56.6756161
## Info.On.Internet.my.fctr2                        39.0097635  39.0097635
## Info.On.Internet.my.fctr10                       36.5824483  36.5824483
## Worry.About.Info.my.fctrNA.my                    36.2015544  36.2015544
## Internet.Use.my                                  34.6695694  34.6695694
## Age.my.fctrNA.my                                 32.1563634  32.1563634
## Info.On.Internet.my.fctr4                        32.1007392  32.1007392
## Info.On.Internet.my.fctr7                        29.4920157  29.4920157
## Info.On.Internet.my.fctr3                        28.6475914  28.6475914
## Anonymity.Possible.my.fctr1                      27.3021584  27.3021584
## Privacy.Laws.Effective.my.fctrNA.my              26.8441377  26.8441377
## Info.On.Internet.my.fctr6                        25.8877833  25.8877833
## Tried.Masking.Identity.my.fctr1                  24.3459375  24.3459375
## Info.On.Internet.my.fctr11                       22.6724300  22.6724300
## Info.On.Internet.my.fctr5                        21.9709901  21.9709901
## Smartphone.my.fctr1                              16.9094201  16.9094201
## Conservativeness.my.fctr3                        16.7124321  16.7124321
## Sex.fctrFemale                                   16.1181262  16.1181262
## Info.On.Internet.my.fctr9                        15.2886330  15.2886330
## Conservativeness.my.fctr4                        14.0961369  14.0961369
## Info.On.Internet.my.fctr8                        11.9980054  11.9980054
## Conservativeness.my.fctrNA.my                    11.9118764  11.9118764
## Conservativeness.my.fctr5                        10.3474601  10.3474601
## Region.fctrSouth                                 10.1019949  10.1019949
## Tried.Masking.Identity.my.fctrNA.my               9.1788768   9.1788768
## `Age.my.fctr(64.8,80.4]`                          7.4936724   7.4936724
## Anonymity.Possible.my.fctrNA.my                   5.4063607   5.4063607
## `Age.my.fctr(80.4,96.1]`                          4.6130643   4.6130643
## .rnorm                                            2.2924783   2.2924783
## Conservativeness.my.fctr2                         1.9402339   1.9402339
## Info.On.Internet.my.fctr1                         1.8719519   1.8719519
## Smartphone.my.fctrNA.my                           0.8859963   0.8859963
## Region.fctrMidwest                                0.6299939   0.6299939
## Region.fctrWest                                   0.0000000   0.0000000
##                                     Final.lm.importance
## Worry.About.Info.my.fctr1                   100.0000000
## Privacy.Laws.Effective.my.fctr1              92.6128304
## `Age.my.fctr(49.2,64.8]`                     60.7732556
## `Age.my.fctr(33.6,49.2]`                     56.6756161
## Info.On.Internet.my.fctr2                    39.0097635
## Info.On.Internet.my.fctr10                   36.5824483
## Worry.About.Info.my.fctrNA.my                36.2015544
## Internet.Use.my                              34.6695694
## Age.my.fctrNA.my                             32.1563634
## Info.On.Internet.my.fctr4                    32.1007392
## Info.On.Internet.my.fctr7                    29.4920157
## Info.On.Internet.my.fctr3                    28.6475914
## Anonymity.Possible.my.fctr1                  27.3021584
## Privacy.Laws.Effective.my.fctrNA.my          26.8441377
## Info.On.Internet.my.fctr6                    25.8877833
## Tried.Masking.Identity.my.fctr1              24.3459375
## Info.On.Internet.my.fctr11                   22.6724300
## Info.On.Internet.my.fctr5                    21.9709901
## Smartphone.my.fctr1                          16.9094201
## Conservativeness.my.fctr3                    16.7124321
## Sex.fctrFemale                               16.1181262
## Info.On.Internet.my.fctr9                    15.2886330
## Conservativeness.my.fctr4                    14.0961369
## Info.On.Internet.my.fctr8                    11.9980054
## Conservativeness.my.fctrNA.my                11.9118764
## Conservativeness.my.fctr5                    10.3474601
## Region.fctrSouth                             10.1019949
## Tried.Masking.Identity.my.fctrNA.my           9.1788768
## `Age.my.fctr(64.8,80.4]`                      7.4936724
## Anonymity.Possible.my.fctrNA.my               5.4063607
## `Age.my.fctr(80.4,96.1]`                      4.6130643
## .rnorm                                        2.2924783
## Conservativeness.my.fctr2                     1.9402339
## Info.On.Internet.my.fctr1                     1.8719519
## Smartphone.my.fctrNA.my                       0.8859963
## Region.fctrMidwest                            0.6299939
## Region.fctrWest                               0.0000000
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id)                  
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_trnobs_df, mdl_id =
## glb_fin_mdl_id): Limiting important feature scatter plots to 5 out of 12
```

![](Pew_Anonymity2_files/figure-html/fit.data.training_1-2.png) ![](Pew_Anonymity2_files/figure-html/fit.data.training_1-3.png) ![](Pew_Anonymity2_files/figure-html/fit.data.training_1-4.png) ![](Pew_Anonymity2_files/figure-html/fit.data.training_1-5.png) ![](Pew_Anonymity2_files/figure-html/fit.data.training_1-6.png) 

```
##     Internet.Use Smartphone    Sex Age          State Region
## 487            1          0 Female  NA       Kentucky  South
## 262            1          0   Male  66      Tennessee  South
## 14             1          1 Female  47 North Carolina  South
## 967            1          1   Male  50     California   West
## 698            1          0   Male  60      Louisiana  South
##     Conservativeness Info.On.Internet Worry.About.Info Privacy.Importance
## 487                4                1                0                0.0
## 262                4                1                1                0.0
## 14                 3                0                0                0.0
## 967                4                1                1               12.5
## 698                5                0                0                0.0
##     Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 487                  0                      0                      0
## 262                  0                      0                      0
## 14                   1                      0                      0
## 967                  0                      0                      0
## 698                  0                      0                      0
##     .rownames  .src     .rnorm Internet.Use.my Age.my.fctr
## 487       487 Train -0.2531848               1       NA.my
## 262       262 Train -0.5622947               1 (64.8,80.4]
## 14         14 Train  0.7036249               1 (33.6,49.2]
## 967       967 Train  0.7633980               1 (49.2,64.8]
## 698       698 Train -1.0376304               1 (49.2,64.8]
##     Anonymity.Possible.my.fctr Conservativeness.my.fctr
## 487                          0                        4
## 262                          0                        4
## 14                           1                        3
## 967                          0                        4
## 698                          0                        5
##     Info.On.Internet.my.fctr Privacy.Laws.Effective.my.fctr
## 487                        1                              0
## 262                        1                              0
## 14                         0                              0
## 967                        1                              0
## 698                        0                              0
##     Smartphone.my.fctr Tried.Masking.Identity.my.fctr
## 487                  0                              0
## 262                  0                              0
## 14                   1                              0
## 967                  1                              0
## 698                  0                              0
##     Worry.About.Info.my.fctr Sex.fctr     State.fctr Region.fctr
## 487                        0   Female       Kentucky       South
## 262                        1     Male      Tennessee       South
## 14                         0   Female North Carolina       South
## 967                        1     Male     California        West
## 698                        0     Male      Louisiana       South
##     Privacy.Importance.predict.Final.lm
## 487                            76.04835
## 262                            74.67681
## 14                             72.44079
## 967                            84.13790
## 698                            70.26737
##     Privacy.Importance.predict.Final.lm.err .label
## 487                                76.04835    487
## 262                                74.67681    262
## 14                                 72.44079     14
## 967                                71.63790    967
## 698                                70.26737    698
```

![](Pew_Anonymity2_files/figure-html/fit.data.training_1-7.png) 

```r
dsp_feats_vctr <- c(NULL)
for(var in grep(".importance", names(glb_feats_df), fixed=TRUE, value=TRUE))
    dsp_feats_vctr <- union(dsp_feats_vctr, 
                            glb_feats_df[!is.na(glb_feats_df[, var]), "id"])

print(glb_trnobs_df[glb_trnobs_df$UniqueID %in% FN_OOB_ids, 
                    grep(glb_rsp_var, names(glb_trnobs_df), value=TRUE)])
```

```
## [1] Privacy.Importance                     
## [2] Privacy.Importance.predict.Final.lm    
## [3] Privacy.Importance.predict.Final.lm.err
## <0 rows> (or 0-length row.names)
```

```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## [1] "Privacy.Importance.predict.Final.lm"    
## [2] "Privacy.Importance.predict.Final.lm.err"
```

```r
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all.prediction","model.final")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1
```

![](Pew_Anonymity2_files/figure-html/fit.data.training_1-8.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor    bgn    end elapsed
## 15 fit.data.training          8          1 78.035 82.865    4.83
## 16  predict.data.new          9          0 82.865     NA      NA
```

## Step `9.0: predict data new`

```r
# Compute final model predictions
glb_newobs_df <- glb_get_predictions(glb_newobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/predict.data.new-1.png) 

```
##     Internet.Use Smartphone    Sex Age          State Region
## 794            1          1 Female  49       Virginia  South
## 80             1          0   Male  21        Florida  South
## 683            1          1   Male  43 North Carolina  South
## 460            1          0 Female  62       Virginia  South
## 954            1          1 Female  62        Florida  South
## 632            1          0 Female  55        Alabama  South
##     Conservativeness Info.On.Internet Worry.About.Info Privacy.Importance
## 794                3                0                1           11.11111
## 80                 3                8                1            0.00000
## 683                3                0                0            0.00000
## 460                2                8                1            0.00000
## 954                4                3                0            0.00000
## 632                2                0                0            0.00000
##     Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 794                  1                      0                      0
## 80                   1                      0                      0
## 683                  0                      0                      0
## 460                  0                      0                      1
## 954                 NA                      0                      0
## 632                  0                      0                      0
##     .rownames .src     .rnorm Internet.Use.my Age.my.fctr
## 794       794 Test -0.3440475               1 (33.6,49.2]
## 80         80 Test  0.5643370               1 (17.9,33.6]
## 683       683 Test -0.3761202               1 (33.6,49.2]
## 460       460 Test -1.1358528               1 (49.2,64.8]
## 954       954 Test  0.3327613               1 (49.2,64.8]
## 632       632 Test -0.6707990               1 (49.2,64.8]
##     Anonymity.Possible.my.fctr Conservativeness.my.fctr
## 794                          1                        3
## 80                           1                        3
## 683                          0                        3
## 460                          0                        2
## 954                      NA.my                        4
## 632                          0                        2
##     Info.On.Internet.my.fctr Privacy.Laws.Effective.my.fctr
## 794                        0                              0
## 80                         8                              0
## 683                        0                              0
## 460                        8                              1
## 954                        3                              0
## 632                        0                              0
##     Smartphone.my.fctr Tried.Masking.Identity.my.fctr
## 794                  1                              0
## 80                   0                              0
## 683                  1                              0
## 460                  0                              0
## 954                  1                              0
## 632                  0                              0
##     Worry.About.Info.my.fctr Sex.fctr     State.fctr Region.fctr
## 794                        1   Female       Virginia       South
## 80                         1     Male        Florida       South
## 683                        0     Male North Carolina       South
## 460                        1   Female       Virginia       South
## 954                        0   Female        Florida       South
## 632                        0   Female        Alabama       South
##     Privacy.Importance.predict.Final.lm
## 794                            86.42286
## 80                             74.58973
## 683                            74.25595
## 460                            72.63961
## 954                            69.58716
## 632                            69.32296
##     Privacy.Importance.predict.Final.lm.err
## 794                                75.31175
## 80                                 74.58973
## 683                                74.25595
## 460                                72.63961
## 954                                69.58716
## 632                                69.32296
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id)                  
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_newobs_df, mdl_id =
## glb_fin_mdl_id): Limiting important feature scatter plots to 5 out of 12
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/predict.data.new-2.png) 

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/predict.data.new-3.png) 

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/predict.data.new-4.png) 

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/predict.data.new-5.png) 

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/predict.data.new-6.png) 

```
##     Internet.Use Smartphone    Sex Age          State Region
## 794            1          1 Female  49       Virginia  South
## 80             1          0   Male  21        Florida  South
## 683            1          1   Male  43 North Carolina  South
## 460            1          0 Female  62       Virginia  South
## 954            1          1 Female  62        Florida  South
##     Conservativeness Info.On.Internet Worry.About.Info Privacy.Importance
## 794                3                0                1           11.11111
## 80                 3                8                1            0.00000
## 683                3                0                0            0.00000
## 460                2                8                1            0.00000
## 954                4                3                0            0.00000
##     Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 794                  1                      0                      0
## 80                   1                      0                      0
## 683                  0                      0                      0
## 460                  0                      0                      1
## 954                 NA                      0                      0
##     .rownames .src     .rnorm Internet.Use.my Age.my.fctr
## 794       794 Test -0.3440475               1 (33.6,49.2]
## 80         80 Test  0.5643370               1 (17.9,33.6]
## 683       683 Test -0.3761202               1 (33.6,49.2]
## 460       460 Test -1.1358528               1 (49.2,64.8]
## 954       954 Test  0.3327613               1 (49.2,64.8]
##     Anonymity.Possible.my.fctr Conservativeness.my.fctr
## 794                          1                        3
## 80                           1                        3
## 683                          0                        3
## 460                          0                        2
## 954                      NA.my                        4
##     Info.On.Internet.my.fctr Privacy.Laws.Effective.my.fctr
## 794                        0                              0
## 80                         8                              0
## 683                        0                              0
## 460                        8                              1
## 954                        3                              0
##     Smartphone.my.fctr Tried.Masking.Identity.my.fctr
## 794                  1                              0
## 80                   0                              0
## 683                  1                              0
## 460                  0                              0
## 954                  1                              0
##     Worry.About.Info.my.fctr Sex.fctr     State.fctr Region.fctr
## 794                        1   Female       Virginia       South
## 80                         1     Male        Florida       South
## 683                        0     Male North Carolina       South
## 460                        1   Female       Virginia       South
## 954                        0   Female        Florida       South
##     Privacy.Importance.predict.Final.lm
## 794                            86.42286
## 80                             74.58973
## 683                            74.25595
## 460                            72.63961
## 954                            69.58716
##     Privacy.Importance.predict.Final.lm.err .label
## 794                                75.31175    794
## 80                                 74.58973     80
## 683                                74.25595    683
## 460                                72.63961    460
## 954                                69.58716    954
```

```
## Warning in loop_apply(n, do.ply): Removed 215 rows containing missing
## values (geom_point).
```

![](Pew_Anonymity2_files/figure-html/predict.data.new-7.png) 

```r
if (glb_is_classification && glb_is_binomial) {
    submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id, ".prob"))]
    names(submit_df)[2] <- "Probability1"
} else submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id))]
write.csv(submit_df, 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_fin_mdl_id), fixed=TRUE), 
           "_submit.csv"), row.names=FALSE)

# print(orderBy(~ -max.auc.OOB, glb_models_df[, c("model_id", 
#             "max.auc.OOB", "max.Accuracy.OOB")]))
if (glb_is_classification && glb_is_binomial)
    print(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                        "opt.prob.threshold.OOB"])
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: Low.cor.X.lm"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.lm"
```

```r
print(dim(glb_fitobs_df))
```

```
## [1] 556  28
```

```r
print(dsp_models_df)
```

```
##                     model_id min.RMSE.OOB max.R.sq.OOB max.Adj.R.sq.fit
## 6               Low.cor.X.lm           NA           NA      0.140363573
## 7                   All.X.lm           NA           NA      0.140363573
## 5               Max.cor.Y.lm           NA           NA      0.117228253
## 1                     MFO.lm           NA           NA     -0.001791565
## 2       Max.cor.Y.cv.0.rpart           NA           NA               NA
## 3  Max.cor.Y.cv.0.cp.0.rpart           NA           NA               NA
## 4            Max.cor.Y.rpart           NA           NA               NA
## 8                  All.X.glm           NA           NA               NA
## 9             All.X.bayesglm           NA           NA               NA
## 10      All.X.no.rnorm.rpart           NA           NA               NA
## 11         All.X.no.rnorm.rf           NA           NA               NA
```

```r
if (glb_is_classification) {
    print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id))
    print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                            glb_OOBobs_df[, glb_rsp_var])$table))

    if (!is.null(glb_category_vars)) {
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
}    

dsp_myCategory_conf_mtrx <- function(myCategory) {
    print(sprintf("%s OOB::myCategory=%s confusion matrix & accuracy: ", 
                  glb_sel_mdl_id, myCategory))
    print(t(confusionMatrix(
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                      paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, glb_rsp_var])$table))
    print(sum(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                            predct_accurate_var_name]) / 
         nrow(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, ]))
    err_ids <- glb_OOBobs_df[(glb_OOBobs_df$myCategory == myCategory) & 
                             (!glb_OOBobs_df[, predct_accurate_var_name]), glb_id_var]

    OOB_FNerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 1), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FN errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FNerr_df)))
    print(OOB_FNerr_df)

    OOB_FPerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 0), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FP errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FPerr_df)))
    print(OOB_FPerr_df)
}
#dsp_myCategory_conf_mtrx(myCategory="OpEd#Opinion#")
#dsp_myCategory_conf_mtrx(myCategory="Business#Business Day#Dealbook")
#dsp_myCategory_conf_mtrx(myCategory="##")

if (glb_is_classification) {
    print("FN_OOB_ids:")
    print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                        grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
    print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                        glb_txt_vars])
    print(dsp_vctr <- colSums(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                        setdiff(grep("[HSA].", names(glb_OOBobs_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_OOBobs_df),
                    grep(".fctr", names(glb_OOBobs_df), fixed=TRUE, value=TRUE)))]))
}

dsp_hdlpfx_results <- function(hdlpfx) {
    print(hdlpfx)
    print(glb_OOBobs_df[glb_OOBobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_newobs_df), value=TRUE)])
    print(dsp_vctr <- colSums(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        setdiff(grep("[HSA]\\.", names(glb_newobs_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_newobs_df),
                    grep(".fctr", names(glb_newobs_df), fixed=TRUE, value=TRUE)))]))
    print(dsp_vctr <- dsp_vctr[dsp_vctr != 0])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        union(names(dsp_vctr), myfind_chr_cols_df(glb_newobs_df))])
}
#dsp_hdlpfx_results(hdlpfx="Ask Well::")

# print("myMisc::|OpEd|blank|blank|1:")
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% c(6446), 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])

# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     c("WordCount", "WordCount.log", "myMultimedia",
#                       "NewsDesk", "SectionName", "SubsectionName")])
# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), ], 
#                           c(glb_rsp_var, "myMultimedia")))
# dsp_chisq.test(Headline.contains="[Vi]deo")
# print(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline")])
# print(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola", Popular=1), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline",
#                             "NewsDesk", "SectionName", "SubsectionName")])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.ConditionalX.y & is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
print(orderBy(as.formula(paste0("~ -", glb_sel_mdl_id, ".importance")), glb_featsimp_df))
```

```
##                                     Low.cor.X.lm.importance  importance
## Worry.About.Info.my.fctr1                       100.0000000 100.0000000
## Privacy.Laws.Effective.my.fctr1                  92.6128304  92.6128304
## `Age.my.fctr(49.2,64.8]`                         60.7732556  60.7732556
## `Age.my.fctr(33.6,49.2]`                         56.6756161  56.6756161
## Info.On.Internet.my.fctr2                        39.0097635  39.0097635
## Info.On.Internet.my.fctr10                       36.5824483  36.5824483
## Worry.About.Info.my.fctrNA.my                    36.2015544  36.2015544
## Internet.Use.my                                  34.6695694  34.6695694
## Age.my.fctrNA.my                                 32.1563634  32.1563634
## Info.On.Internet.my.fctr4                        32.1007392  32.1007392
## Info.On.Internet.my.fctr7                        29.4920157  29.4920157
## Info.On.Internet.my.fctr3                        28.6475914  28.6475914
## Anonymity.Possible.my.fctr1                      27.3021584  27.3021584
## Privacy.Laws.Effective.my.fctrNA.my              26.8441377  26.8441377
## Info.On.Internet.my.fctr6                        25.8877833  25.8877833
## Tried.Masking.Identity.my.fctr1                  24.3459375  24.3459375
## Info.On.Internet.my.fctr11                       22.6724300  22.6724300
## Info.On.Internet.my.fctr5                        21.9709901  21.9709901
## Smartphone.my.fctr1                              16.9094201  16.9094201
## Conservativeness.my.fctr3                        16.7124321  16.7124321
## Sex.fctrFemale                                   16.1181262  16.1181262
## Info.On.Internet.my.fctr9                        15.2886330  15.2886330
## Conservativeness.my.fctr4                        14.0961369  14.0961369
## Info.On.Internet.my.fctr8                        11.9980054  11.9980054
## Conservativeness.my.fctrNA.my                    11.9118764  11.9118764
## Conservativeness.my.fctr5                        10.3474601  10.3474601
## Region.fctrSouth                                 10.1019949  10.1019949
## Tried.Masking.Identity.my.fctrNA.my               9.1788768   9.1788768
## `Age.my.fctr(64.8,80.4]`                          7.4936724   7.4936724
## Anonymity.Possible.my.fctrNA.my                   5.4063607   5.4063607
## `Age.my.fctr(80.4,96.1]`                          4.6130643   4.6130643
## .rnorm                                            2.2924783   2.2924783
## Conservativeness.my.fctr2                         1.9402339   1.9402339
## Info.On.Internet.my.fctr1                         1.8719519   1.8719519
## Smartphone.my.fctrNA.my                           0.8859963   0.8859963
## Region.fctrMidwest                                0.6299939   0.6299939
## Region.fctrWest                                   0.0000000   0.0000000
##                                     Final.lm.importance
## Worry.About.Info.my.fctr1                   100.0000000
## Privacy.Laws.Effective.my.fctr1              92.6128304
## `Age.my.fctr(49.2,64.8]`                     60.7732556
## `Age.my.fctr(33.6,49.2]`                     56.6756161
## Info.On.Internet.my.fctr2                    39.0097635
## Info.On.Internet.my.fctr10                   36.5824483
## Worry.About.Info.my.fctrNA.my                36.2015544
## Internet.Use.my                              34.6695694
## Age.my.fctrNA.my                             32.1563634
## Info.On.Internet.my.fctr4                    32.1007392
## Info.On.Internet.my.fctr7                    29.4920157
## Info.On.Internet.my.fctr3                    28.6475914
## Anonymity.Possible.my.fctr1                  27.3021584
## Privacy.Laws.Effective.my.fctrNA.my          26.8441377
## Info.On.Internet.my.fctr6                    25.8877833
## Tried.Masking.Identity.my.fctr1              24.3459375
## Info.On.Internet.my.fctr11                   22.6724300
## Info.On.Internet.my.fctr5                    21.9709901
## Smartphone.my.fctr1                          16.9094201
## Conservativeness.my.fctr3                    16.7124321
## Sex.fctrFemale                               16.1181262
## Info.On.Internet.my.fctr9                    15.2886330
## Conservativeness.my.fctr4                    14.0961369
## Info.On.Internet.my.fctr8                    11.9980054
## Conservativeness.my.fctrNA.my                11.9118764
## Conservativeness.my.fctr5                    10.3474601
## Region.fctrSouth                             10.1019949
## Tried.Masking.Identity.my.fctrNA.my           9.1788768
## `Age.my.fctr(64.8,80.4]`                      7.4936724
## Anonymity.Possible.my.fctrNA.my               5.4063607
## `Age.my.fctr(80.4,96.1]`                      4.6130643
## .rnorm                                        2.2924783
## Conservativeness.my.fctr2                     1.9402339
## Info.On.Internet.my.fctr1                     1.8719519
## Smartphone.my.fctrNA.my                       0.8859963
## Region.fctrMidwest                            0.6299939
## Region.fctrWest                               0.0000000
```

```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "prdnew_dsk.RData"))

rm(submit_df, tmp_OOBobs_df)
```

```
## Warning in rm(submit_df, tmp_OOBobs_df): object 'tmp_OOBobs_df' not found
```

```r
# tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
#     replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
#         "data.new.prediction")), flip_coord=TRUE)
# print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())

glb_chunks_df <- myadd_chunk(glb_chunks_df, "display.session.info", major.inc=TRUE)
```

```
##                   label step_major step_minor    bgn   end elapsed
## 16     predict.data.new          9          0 82.865 88.41   5.545
## 17 display.session.info         10          0 88.410    NA      NA
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 
#```{r q1, cache=FALSE}
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
#```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
##                      label step_major step_minor    bgn    end elapsed
## 11              fit.models          7          1 32.710 55.008  22.298
## 10              fit.models          7          0 20.175 32.709  12.535
## 12              fit.models          7          2 55.008 65.646  10.638
## 14       fit.data.training          8          0 70.768 78.034   7.266
## 2             inspect.data          2          0  9.966 15.619   5.653
## 16        predict.data.new          9          0 82.865 88.410   5.545
## 13              fit.models          7          3 65.646 70.767   5.121
## 15       fit.data.training          8          1 78.035 82.865   4.830
## 3               scrub.data          2          1 15.620 17.419   1.799
## 6         extract.features          3          0 17.639 18.980   1.342
## 1              import.data          1          0  9.118  9.966   0.848
## 8          select.features          5          0 19.253 19.878   0.626
## 9  partition.data.training          6          0 19.879 20.175   0.296
## 7             cluster.data          4          0 18.981 19.252   0.271
## 4           transform.data          2          2 17.420 17.556   0.136
## 5      manage.missing.data          2          3 17.556 17.639   0.083
##    duration
## 11   22.298
## 10   12.534
## 12   10.638
## 14    7.266
## 2     5.653
## 16    5.545
## 13    5.121
## 15    4.830
## 3     1.799
## 6     1.341
## 1     0.848
## 8     0.625
## 9     0.296
## 7     0.271
## 4     0.136
## 5     0.083
## [1] "Total Elapsed Time: 88.41 secs"
```

![](Pew_Anonymity2_files/figure-html/display.session.info-1.png) 

```
##                   label step_major step_minor    bgn    end elapsed
## 6       fit.models_1_rf          6          0 45.066 55.002   9.936
## 5    fit.models_1_rpart          5          0 42.139 45.066   2.927
## 4 fit.models_1_bayesglm          4          0 39.446 42.138   2.692
## 3      fit.models_1_glm          3          0 36.768 39.446   2.678
## 2       fit.models_1_lm          2          0 34.345 36.768   2.423
## 1      fit.models_1_bgn          1          0 34.331 34.345   0.014
##   duration
## 6    9.936
## 5    2.927
## 4    2.692
## 3    2.678
## 2    2.423
## 1    0.014
## [1] "Total Elapsed Time: 55.002 secs"
```

![](Pew_Anonymity2_files/figure-html/display.session.info-2.png) 

```
## R version 3.2.0 (2015-04-16)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] grid      parallel  stats     graphics  grDevices utils     datasets 
## [8] methods   base     
## 
## other attached packages:
##  [1] gdata_2.16.1        randomForest_4.6-10 arm_1.8-5          
##  [4] lme4_1.1-7          Rcpp_0.11.6         Matrix_1.2-1       
##  [7] MASS_7.3-40         rpart.plot_1.5.2    rpart_4.1-9        
## [10] reshape2_1.4.1      mgcv_1.8-6          nlme_3.1-120       
## [13] dplyr_0.4.1         plyr_1.8.2          caTools_1.17.1     
## [16] doMC_1.3.3          iterators_1.0.7     foreach_1.4.2      
## [19] doBy_4.5-13         survival_2.38-1     caret_6.0-47       
## [22] ggplot2_1.0.1       lattice_0.20-31    
## 
## loaded via a namespace (and not attached):
##  [1] gtools_3.5.0        splines_3.2.0       colorspace_1.2-6   
##  [4] htmltools_0.2.6     yaml_2.1.13         nloptr_1.0.4       
##  [7] DBI_0.3.1           RColorBrewer_1.1-2  stringr_1.0.0      
## [10] munsell_0.4.2       gtable_0.1.2        codetools_0.2-11   
## [13] coda_0.17-1         evaluate_0.7        labeling_0.3       
## [16] knitr_1.10.5        SparseM_1.6         quantreg_5.11      
## [19] pbkrtest_0.4-2      proto_0.3-10        scales_0.2.4       
## [22] formatR_1.2         BradleyTerry2_1.0-6 abind_1.4-3        
## [25] digest_0.6.8        stringi_0.4-1       brglm_0.5-9        
## [28] tools_3.2.0         bitops_1.0-6        magrittr_1.5       
## [31] lazyeval_0.1.10     car_2.0-25          assertthat_0.1     
## [34] minqa_1.2.4         rmarkdown_0.6.1     nnet_7.3-9         
## [37] compiler_3.2.0
```
