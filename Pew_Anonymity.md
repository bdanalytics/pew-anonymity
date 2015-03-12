# Pew-Anonymity: <Predicted attribute> <regression/classification>
bdanalytics  

**  **    
**Date: (Thu) Mar 12, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/c4x/MITx/15.071x_2/asset/AnonymityPoll.csv  
    New:        <prdct_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
#suppressPackageStartupMessages(require())

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis specific global variables
glb_separate_predict_dataset <- FALSE

script_df <- data.frame(chunk_label="import_data", chunk_step_major=1, chunk_step_minor=0)
print(script_df)
```

```
##   chunk_label chunk_step_major chunk_step_minor
## 1 import_data                1                0
```

## Step `1`: import data

```r
entity_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/AnonymityPoll.csv", 
    comment="entity_df", print_diagn=TRUE)
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
##     Internet.Use Smartphone    Sex Age          State  Region
## 166            1          1 Female  61 North Carolina   South
## 456            1          1 Female  24       Delaware   South
## 723            1          1   Male  72       Illinois Midwest
## 761            1          1 Female  22     California    West
## 877            0          0   Male  70       Illinois Midwest
## 886            1          1   Male  31         Kansas Midwest
##     Conservativeness Info.On.Internet Worry.About.Info Privacy.Importance
## 166                3                1                0           88.88889
## 456                3                4                0           22.22222
## 723                4                2                1          100.00000
## 761                3                4                0           33.33333
## 877                2               NA               NA                 NA
## 886                4                0                0           16.66667
##     Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 166                  1                      0                      1
## 456                  1                      0                      1
## 723                  0                      0                     NA
## 761                  1                      0                      0
## 877                 NA                     NA                      0
## 886                  0                      0                      0
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
##  - attr(*, "comment")= chr "entity_df"
## NULL
```

```r
if (glb_separate_predict_dataset) {
    predct_df <- myimport_data(
        url="<prdct_url>", 
        comment="predct_df", print_diagn=TRUE)
} else {
    predct_df <- entity_df[sample(1:nrow(entity_df), nrow(entity_df) / 1000),]
    comment(predct_df) <- "predct_df"
    myprint_df(predct_df)
    str(predct_df)
}         
```

```
##     Internet.Use Smartphone    Sex Age    State    Region Conservativeness
## 326            1          0 Female  55 New York Northeast                3
##     Info.On.Internet Worry.About.Info Privacy.Importance
## 326                3                1           61.11111
##     Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
## 326                  0                      0                      0
## 'data.frame':	1 obs. of  13 variables:
##  $ Internet.Use          : int 1
##  $ Smartphone            : int 0
##  $ Sex                   : chr "Female"
##  $ Age                   : int 55
##  $ State                 : chr "New York"
##  $ Region                : chr "Northeast"
##  $ Conservativeness      : int 3
##  $ Info.On.Internet      : int 3
##  $ Worry.About.Info      : int 1
##  $ Privacy.Importance    : num 61.1
##  $ Anonymity.Possible    : int 0
##  $ Tried.Masking.Identity: int 0
##  $ Privacy.Laws.Effective: int 0
##  - attr(*, "comment")= chr "predct_df"
```

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="inspect_data", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=1))
print(script_df)
```

```
##    chunk_label chunk_step_major chunk_step_minor
## 1  import_data                1                0
## 2 inspect_data                2                1
```

### Step `2`.`1`: inspect data

```r
#print(str(entity_df))
#View(entity_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>

# Create new features that help diagnostics
#   Convert factors to dummy variables
#   Potential Enhancements:
#       One code chunk to cycle thru entity_df & predct_df ?
#           Use with / within ?
#           for (df in c(entity_df, predct_df)) cycles thru column names
#           for (df in list(entity_df, predct_df)) does not change the actual dataframes
#
#       Build splines   require(splines); bsBasis <- bs(training$age, df=3)

# entity_df <- mutate(entity_df,
#     <col_name>.NA=is.na(<col_name>) 
#     <col_name>_fctr=as.factor(<col_name>),
#     
#     Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#     Year=year(Date.my),
#     Month=months(Date.my),
#     Weekday=weekdays(Date.my)
#     
#                     )
# 
# predct_df <- mutate(predct_df, 
#                     )

print(summary(entity_df))
```

```
##   Internet.Use      Smartphone         Sex                 Age       
##  Min.   :0.0000   Min.   :0.0000   Length:1002        Min.   :18.00  
##  1st Qu.:1.0000   1st Qu.:0.0000   Class :character   1st Qu.:37.00  
##  Median :1.0000   Median :1.0000   Mode  :character   Median :55.00  
##  Mean   :0.7742   Mean   :0.5078                      Mean   :52.37  
##  3rd Qu.:1.0000   3rd Qu.:1.0000                      3rd Qu.:66.00  
##  Max.   :1.0000   Max.   :1.0000                      Max.   :96.00  
##  NA's   :1        NA's   :43                          NA's   :27     
##     State              Region          Conservativeness Info.On.Internet
##  Length:1002        Length:1002        Min.   :1.000    Min.   : 0.000  
##  Class :character   Class :character   1st Qu.:3.000    1st Qu.: 2.000  
##  Mode  :character   Mode  :character   Median :3.000    Median : 4.000  
##                                        Mean   :3.277    Mean   : 3.795  
##                                        3rd Qu.:4.000    3rd Qu.: 6.000  
##                                        Max.   :5.000    Max.   :11.000  
##                                        NA's   :62       NA's   :210     
##  Worry.About.Info Privacy.Importance Anonymity.Possible
##  Min.   :0.0000   Min.   :  0.00     Min.   :0.0000    
##  1st Qu.:0.0000   1st Qu.: 41.43     1st Qu.:0.0000    
##  Median :0.0000   Median : 68.75     Median :0.0000    
##  Mean   :0.4886   Mean   : 62.85     Mean   :0.3692    
##  3rd Qu.:1.0000   3rd Qu.: 88.89     3rd Qu.:1.0000    
##  Max.   :1.0000   Max.   :100.00     Max.   :1.0000    
##  NA's   :212      NA's   :215        NA's   :249       
##  Tried.Masking.Identity Privacy.Laws.Effective
##  Min.   :0.0000         Min.   :0.0000        
##  1st Qu.:0.0000         1st Qu.:0.0000        
##  Median :0.0000         Median :0.0000        
##  Mean   :0.1633         Mean   :0.2617        
##  3rd Qu.:0.0000         3rd Qu.:1.0000        
##  Max.   :1.0000         Max.   :1.0000        
##  NA's   :218            NA's   :108
```

```r
print(sapply(names(entity_df), function(col) sum(is.na(entity_df[, col]))))
```

```
##           Internet.Use             Smartphone                    Sex 
##                      1                     43                      0 
##                    Age                  State                 Region 
##                     27                      0                      0 
##       Conservativeness       Info.On.Internet       Worry.About.Info 
##                     62                    210                    212 
##     Privacy.Importance     Anonymity.Possible Tried.Masking.Identity 
##                    215                    249                    218 
## Privacy.Laws.Effective 
##                    108
```

```r
print(summary(predct_df))
```

```
##   Internet.Use   Smartphone     Sex                 Age    
##  Min.   :1     Min.   :0    Length:1           Min.   :55  
##  1st Qu.:1     1st Qu.:0    Class :character   1st Qu.:55  
##  Median :1     Median :0    Mode  :character   Median :55  
##  Mean   :1     Mean   :0                       Mean   :55  
##  3rd Qu.:1     3rd Qu.:0                       3rd Qu.:55  
##  Max.   :1     Max.   :0                       Max.   :55  
##     State              Region          Conservativeness Info.On.Internet
##  Length:1           Length:1           Min.   :3        Min.   :3       
##  Class :character   Class :character   1st Qu.:3        1st Qu.:3       
##  Mode  :character   Mode  :character   Median :3        Median :3       
##                                        Mean   :3        Mean   :3       
##                                        3rd Qu.:3        3rd Qu.:3       
##                                        Max.   :3        Max.   :3       
##  Worry.About.Info Privacy.Importance Anonymity.Possible
##  Min.   :1        Min.   :61.11      Min.   :0         
##  1st Qu.:1        1st Qu.:61.11      1st Qu.:0         
##  Median :1        Median :61.11      Median :0         
##  Mean   :1        Mean   :61.11      Mean   :0         
##  3rd Qu.:1        3rd Qu.:61.11      3rd Qu.:0         
##  Max.   :1        Max.   :61.11      Max.   :0         
##  Tried.Masking.Identity Privacy.Laws.Effective
##  Min.   :0              Min.   :0             
##  1st Qu.:0              1st Qu.:0             
##  Median :0              Median :0             
##  Mean   :0              Mean   :0             
##  3rd Qu.:0              3rd Qu.:0             
##  Max.   :0              Max.   :0
```

```r
print(sapply(names(predct_df), function(col) sum(is.na(predct_df[, col]))))
```

```
##           Internet.Use             Smartphone                    Sex 
##                      0                      0                      0 
##                    Age                  State                 Region 
##                      0                      0                      0 
##       Conservativeness       Info.On.Internet       Worry.About.Info 
##                      0                      0                      0 
##     Privacy.Importance     Anonymity.Possible Tried.Masking.Identity 
##                      0                      0                      0 
## Privacy.Laws.Effective 
##                      0
```

```r
#pairs(subset(entity_df, select=-c(col_symbol)))

#   Histogram of predictor in entity_df & predct_df
# Check for predct_df & entity_df features range mismatches

# Other diagnostics:
# print(subset(entity_df, <col1_name> == max(entity_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(entity_df$<col1_name>, na.rm=TRUE)))

print(Smartphone_freq_entity_df <- mycreate_tbl_df(entity_df, "Smartphone"))
```

```
##   Smartphone .freq
## 1          0   472
## 2          1   487
```

```r
print(State_freq_entity_df <- mycreate_tbl_df(subset(entity_df, Region == "South"), 
                                              "State"))
```

```
##                   State .freq
## 1  District of Columbia     2
## 2         West Virginia     5
## 3              Delaware     6
## 4              Arkansas    10
## 5               Alabama    11
## 6           Mississippi    11
## 7        South Carolina    12
## 8              Oklahoma    14
## 9             Louisiana    17
## 10            Tennessee    17
## 11             Maryland    18
## 12             Kentucky    25
## 13             Virginia    31
## 14       North Carolina    32
## 15              Georgia    34
## 16              Florida    42
## 17                Texas    72
```

```r
# print(which.min(table(entity_df$<col_name>)))
# print(which.max(table(entity_df$<col_name>)))
# print(which.max(table(entity_df$<col1_name>, entity_df$<col2_name>)[, 2]))
print(table(entity_df$Sex, entity_df$Region))
```

```
##         
##          Midwest Northeast South West
##   Female     123        90   176  116
##   Male       116        76   183  122
```

```r
print(table(entity_df$State, entity_df$Region))
```

```
##                       
##                        Midwest Northeast South West
##   Alabama                    0         0    11    0
##   Arizona                    0         0     0   24
##   Arkansas                   0         0    10    0
##   California                 0         0     0  103
##   Colorado                   0         0     0   19
##   Connecticut                0         8     0    0
##   Delaware                   0         0     6    0
##   District of Columbia       0         0     2    0
##   Florida                    0         0    42    0
##   Georgia                    0         0    34    0
##   Idaho                      0         0     0    8
##   Illinois                  32         0     0    0
##   Indiana                   27         0     0    0
##   Iowa                      14         0     0    0
##   Kansas                    14         0     0    0
##   Kentucky                   0         0    25    0
##   Louisiana                  0         0    17    0
##   Maine                      0         4     0    0
##   Maryland                   0         0    18    0
##   Massachusetts              0        19     0    0
##   Michigan                  31         0     0    0
##   Minnesota                 15         0     0    0
##   Mississippi                0         0    11    0
##   Missouri                  26         0     0    0
##   Montana                    0         0     0    5
##   Nebraska                  11         0     0    0
##   Nevada                     0         0     0    8
##   New Hampshire              0         7     0    0
##   New Jersey                 0        16     0    0
##   New Mexico                 0         0     0    5
##   New York                   0        60     0    0
##   North Carolina             0         0    32    0
##   North Dakota               5         0     0    0
##   Ohio                      38         0     0    0
##   Oklahoma                   0         0    14    0
##   Oregon                     0         0     0   20
##   Pennsylvania               0        45     0    0
##   Rhode Island               0         4     0    0
##   South Carolina             0         0    12    0
##   South Dakota               3         0     0    0
##   Tennessee                  0         0    17    0
##   Texas                      0         0    72    0
##   Utah                       0         0     0   11
##   Vermont                    0         3     0    0
##   Virginia                   0         0    31    0
##   Washington                 0         0     0   28
##   West Virginia              0         0     5    0
##   Wisconsin                 23         0     0    0
##   Wyoming                    0         0     0    7
```

```r
# print(table(is.na(entity_df$<col1_name>), entity_df$<col2_name>))
# print(xtabs(~ <col1_name>, entity_df))
# print(xtabs(~ <col1_name> + <col2_name>, entity_df))
# print(<col1_name>_<col2_name>_xtab_entity_df <- 
#   mycreate_xtab(entity_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_entity_df[is.na(<col1_name>_<col2_name>_xtab_entity_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_entity_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_entity_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(entity_df$<col1_name>, entity_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(entity_df$<col1_name>.NA, entity_df$<col2_name>, mean, na.rm=TRUE)))

print(sum((entity_df$Internet.Use == 0) & (entity_df$Smartphone == 0), na.rm=TRUE))
```

```
## [1] 186
```

```r
print(sum((entity_df$Internet.Use == 1) & (entity_df$Smartphone == 1), na.rm=TRUE))
```

```
## [1] 470
```

```r
print(sum((entity_df$Internet.Use == 1) & (entity_df$Smartphone == 0), na.rm=TRUE))
```

```
## [1] 285
```

```r
print(sum((entity_df$Internet.Use == 0) & (entity_df$Smartphone == 1), na.rm=TRUE))
```

```
## [1] 17
```

```r
entity_limited_df <- subset(entity_df, (Internet.Use == 1) | (Smartphone == 1))
print(nrow(entity_limited_df))
```

```
## [1] 792
```

```r
print(sapply(names(entity_limited_df), function(col) sum(is.na(entity_limited_df[, col]))))
```

```
##           Internet.Use             Smartphone                    Sex 
##                      0                     20                      0 
##                    Age                  State                 Region 
##                     22                      0                      0 
##       Conservativeness       Info.On.Internet       Worry.About.Info 
##                     45                      0                      2 
##     Privacy.Importance     Anonymity.Possible Tried.Masking.Identity 
##                      5                     39                      8 
## Privacy.Laws.Effective 
##                     65
```

```r
#print(sum(is.na(entity_limited_df$Internet.Use)))

print(summary(entity_limited_df))
```

```
##   Internet.Use      Smartphone         Sex                 Age       
##  Min.   :0.0000   Min.   :0.0000   Length:792         Min.   :18.00  
##  1st Qu.:1.0000   1st Qu.:0.0000   Class :character   1st Qu.:33.00  
##  Median :1.0000   Median :1.0000   Mode  :character   Median :51.00  
##  Mean   :0.9785   Mean   :0.6308                      Mean   :48.57  
##  3rd Qu.:1.0000   3rd Qu.:1.0000                      3rd Qu.:62.00  
##  Max.   :1.0000   Max.   :1.0000                      Max.   :93.00  
##                   NA's   :20                          NA's   :22     
##     State              Region          Conservativeness Info.On.Internet
##  Length:792         Length:792         Min.   :1.000    Min.   : 0.000  
##  Class :character   Class :character   1st Qu.:3.000    1st Qu.: 2.000  
##  Mode  :character   Mode  :character   Median :3.000    Median : 4.000  
##                                        Mean   :3.237    Mean   : 3.795  
##                                        3rd Qu.:4.000    3rd Qu.: 6.000  
##                                        Max.   :5.000    Max.   :11.000  
##                                        NA's   :45                       
##  Worry.About.Info Privacy.Importance Anonymity.Possible
##  Min.   :0.0000   Min.   :  0.00     Min.   :0.0000    
##  1st Qu.:0.0000   1st Qu.: 41.43     1st Qu.:0.0000    
##  Median :0.0000   Median : 68.75     Median :0.0000    
##  Mean   :0.4886   Mean   : 62.85     Mean   :0.3692    
##  3rd Qu.:1.0000   3rd Qu.: 88.89     3rd Qu.:1.0000    
##  Max.   :1.0000   Max.   :100.00     Max.   :1.0000    
##  NA's   :2        NA's   :5          NA's   :39        
##  Tried.Masking.Identity Privacy.Laws.Effective
##  Min.   :0.0000         Min.   :0.0000        
##  1st Qu.:0.0000         1st Qu.:0.0000        
##  Median :0.0000         Median :0.0000        
##  Mean   :0.1633         Mean   :0.2559        
##  3rd Qu.:0.0000         3rd Qu.:1.0000        
##  Max.   :1.0000         Max.   :1.0000        
##  NA's   :8              NA's   :65
```

```r
print(Info_On_Iternet_freq_entity_limited_df <- mycreate_tbl_df(entity_limited_df, "Info.On.Internet"))
```

```
##    Info.On.Internet .freq
## 1                11     8
## 2                10    13
## 3                 9    18
## 4                 8    40
## 5                 7    63
## 6                 6    67
## 7                 1    84
## 8                 5    94
## 9                 2    95
## 10                3   101
## 11                4   104
## 12                0   105
```

```r
print(Worry_About_Info_freq_entity_limited_df <- mycreate_tbl_df(entity_limited_df, "Worry.About.Info"))
```

```
##   Worry.About.Info .freq
## 1                1   386
## 2                0   404
```

```r
print(Worry_About_Info_freq_entity_limited_df[1, ".freq"] * 1.0 / sum(Worry_About_Info_freq_entity_limited_df[, ".freq"]))
```

```
##         1 
## 0.4886076
```

```r
print(Anonymity_Possible_freq_entity_limited_df <- mycreate_tbl_df(entity_limited_df, "Anonymity.Possible"))
```

```
##   Anonymity.Possible .freq
## 1                  1   278
## 2                  0   475
```

```r
print(Anonymity_Possible_freq_entity_limited_df[1, ".freq"] * 1.0 / sum(Anonymity_Possible_freq_entity_limited_df[, ".freq"]))
```

```
##         1 
## 0.3691899
```

```r
print(Tried_Masking_Identity_freq_entity_limited_df <- mycreate_tbl_df(entity_limited_df, "Tried.Masking.Identity"))
```

```
##   Tried.Masking.Identity .freq
## 1                      1   128
## 2                      0   656
```

```r
print(Tried_Masking_Identity_freq_entity_limited_df[1, ".freq"] * 1.0 / sum(Tried_Masking_Identity_freq_entity_limited_df[, ".freq"]))
```

```
##         1 
## 0.1632653
```

```r
print(Privacy_Laws_Effective_freq_entity_limited_df <- mycreate_tbl_df(entity_limited_df, "Privacy.Laws.Effective"))
```

```
##   Privacy.Laws.Effective .freq
## 1                      1   186
## 2                      0   541
```

```r
print(Privacy_Laws_Effective_freq_entity_limited_df[1, ".freq"] * 1.0 / sum(Privacy_Laws_Effective_freq_entity_limited_df[, ".freq"]))
```

```
##         1 
## 0.2558459
```

```r
# Other plots:
# print(myplot_histogram(entity_df, "<col1_name>"))
# print(myplot_box(df=entity_df, ycol_names="<col1_name>"))
# print(myplot_box(df=entity_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(entity_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(entity_df, "<col1_name>", "<col2_name>"))

script_df <- rbind(script_df, 
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[which.max(script_df$chunk_step_major), 
                                   "chunk_step_minor"]+1))
print(script_df)
```

```
##           chunk_label chunk_step_major chunk_step_minor
## 1         import_data                1                0
## 2        inspect_data                2                1
## 3 manage_missing_data                2                2
```

### Step `2`.`2`: manage missing data

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="encode_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[which.max(script_df$chunk_step_major), 
                                   "chunk_step_minor"]+1))
print(script_df)
```

```
##           chunk_label chunk_step_major chunk_step_minor
## 1         import_data                1                0
## 2        inspect_data                2                1
## 3 manage_missing_data                2                2
## 4         encode_data                2                2
```

### Step `2`.`2`: encode data

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# 
# entity_df <- mymap_codes(entity_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
script_df <- rbind(script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##           chunk_label chunk_step_major chunk_step_minor
## 1         import_data                1                0
## 2        inspect_data                2                1
## 3 manage_missing_data                2                2
## 4         encode_data                2                2
## 5    extract_features                3                0
```

## Step `3`: extract features

```r
# script_df <- rbind(script_df, 
#                    data.frame(chunk_label="extract_features", 
#                               chunk_step_major=max(script_df$chunk_step_major)+1, 
#                               chunk_step_minor=0))
print(script_df)
```

```
##           chunk_label chunk_step_major chunk_step_minor
## 1         import_data                1                0
## 2        inspect_data                2                1
## 3 manage_missing_data                2                2
## 4         encode_data                2                2
## 5    extract_features                3                0
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 

```r
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.

## remove nearZeroVar features (not much variance)
#require(reshape)
#var_features_df <- melt(summaryBy(. ~ factor(0), data=entity_df[, features_lst], 
#                             FUN=var, keep.names=TRUE), 
#                             variable_name=c("feature"))
#names(var_features_df)[2] <- "var"
#print(var_features_df[order(var_features_df$var), ])
# summaryBy ignores factors whereas nearZeroVar inspects factors

# k_fold <- 5
# entity_df[order(entity_df$classe, 
#                   entity_df$user_name, 
#                   entity_df$my.rnorm),"my.cv_ix"] <- 
#     rep(1:k_fold, length.out=nrow(entity_df))
# summaryBy(X ~ my.cv_ix, data=entity_df, FUN=length)
# tapply(entity_df$X, list(entity_df$classe, entity_df$user_name, 
#                            entity_df$my.cv_ix), length)

#require(DAAG)
#entity_df$classe.proper <- as.numeric(entity_df$classe == "A")
#rnorm.glm <- glm(classe.proper ~ rnorm, family=binomial, data=entity_df)
#cv.binary(rnorm.glm, nfolds=k_fold, print.details=TRUE)
#result <- cv.lm(df=entity_df, form.lm=formula(classe ~ rnorm), 
#                    m=k_fold, seed=12345, printit=TRUE)

#plot(mdl_1$finalModel, uniform=TRUE, main="base")
#text(mdl_1$finalModel, use.n=TRUE, all=TRUE, cex=0.8)



```
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] doBy_4.5-13     survival_2.38-1 ggplot2_1.0.0  
## 
## loaded via a namespace (and not attached):
##  [1] codetools_0.2-10 colorspace_1.2-5 digest_0.6.8     evaluate_0.5.5  
##  [5] formatR_1.0      grid_3.1.2       gtable_0.1.2     htmltools_0.2.6 
##  [9] knitr_1.9        lattice_0.20-30  MASS_7.3-39      Matrix_1.1-5    
## [13] munsell_0.4.2    plyr_1.8.1       proto_0.3-10     Rcpp_0.11.4     
## [17] reshape2_1.4.1   rmarkdown_0.5.1  scales_0.2.4     splines_3.1.2   
## [21] stringr_0.6.2    tcltk_3.1.2      tools_3.1.2      yaml_2.1.13
```
