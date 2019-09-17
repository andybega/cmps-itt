
# Examining Repressive and Oppressive State Violence using the Ill-Treatement Contents and Torture Data

This repo contains replication materials for:

Beger, Andreas and Daniel W. Hill, Jr., 20XX, “Examining Repressive and
Oppressive State Violence using the Ill-Treatement Contents and Torture
Data”, *Conflict Managament and Peace Science*.

``` bibtex
@article{beger20XXexamining,
  author = {Andreas Beger and Daniel W.\ Hill, Jr.\},
  title = {Examining Repressive and Oppressive State Violence using the Ill-Treatement Contents and Torture Data},
  year = ,
  journal = {Conflict Management and Peace Science},
  volume = {},
  number = {},
  doi = {}
}
```

## Code

The R scripts in the `R/` folder replicate the figures and tables in the
main paper and supplemental appendix. The `R/functions.R` file contains
helper functions used in some of the other scripts, otherwise all the
other files can be run in the order they sort alphabetically.

Files that start with `si`…pertain to the SI. All other files are
related to the main article.

All output generated by the scripts will be saved in the `output/`
folder and sub-folders. It should have the following structure:

    - output/
      - figures/
      - figures-si/
      - models/
      - tables/

Training the XGBoost model (`2-xgboost.R`) and the SI expanded model set
(`si1-estimate-all-models.R`) takes a while. We have included the
trained XGBoost model in `output/mdl-xgboost.rds` and thus it is
possible to replicate the main results without re-training it. There are
1,008 models in the SI. These are not included, but estimating them does
not take quite as long as training the XGBoost model.

## Data

The data is included in the `data` directory both in R’s native RDS
format and in CSV form.

Most variables have prefixes indicating the data source:

  - `itt_`: Ill-Treatment and Torture data; also binary indicators
    starting with `yy_` and used in an earlier version
  - `NY.GDP.MKTP.KD` and subequent, including `pop`: World Bank World
    Development Indicators
  - `v2x_`: V-Dem
  - `regime` and `dd_democracy` from Cheibub, Gandhi, and Vreeland
    Democracy and dictatorships data
  - `epr_`: Ethnic Power Relations
  - variables from `internal_confl` to `ext_conf_minor`: UCDP ACD
  - `gtd_`: Global terrorism database
  - `ccp_`: Comparative Constitutions Project
  - `gmfd_functionallyfree`: media freedom; see SI
  - `igo_`: COW IGO membership dataset; see SI
  - `NE.TRD.GNFS.ZS`: trade as % of GDP; see SI
  - Human rights organization-related (`hro_`); see SI
  - Time trends (`year_`); see SI

<!-- end list -->

``` r
cy <- readRDS("data/cy.rds")
str(cy)
```

    ## 'data.frame':    1654 obs. of  86 variables:
    ##  $ gwcode                             : num  2 2 2 2 2 2 2 2 2 2 ...
    ##  $ year                               : num  1995 1996 1997 1998 1999 ...
    ##  $ date                               : Date, format: "1995-01-01" "1996-01-01" ...
    ##  $ itt_alleg_vtall                    : num  119 107 191 159 73 42 41 72 94 93 ...
    ##  $ itt_alleg_vtcriminal               : num  70 69 99 91 41 21 15 25 31 34 ...
    ##  $ itt_alleg_vtdissident              : num  4 1 9 1 2 0 4 13 20 13 ...
    ##  $ itt_alleg_vtmarginalized           : num  29 30 78 55 15 20 9 9 14 18 ...
    ##  $ itt_alleg_vtpow                    : num  0 0 0 0 0 0 0 0 1 1 ...
    ##  $ itt_alleg_vtstateagent             : num  1 0 0 0 0 0 0 0 0 0 ...
    ##  $ itt_alleg_vtunst                   : num  15 7 5 12 15 1 13 25 28 27 ...
    ##  $ itt_LoTUnknown                     : chr  "Widespread" "Several" "Several" "Systematic" ...
    ##  $ itt_LoTCriminal                    : chr  "Widespread" "Widespread" "Widespread" "Systematic" ...
    ##  $ itt_LoTDissident                   : chr  "No Allegations" "No Allegations" "No Allegations" "No Allegations" ...
    ##  $ itt_LoTMarginalized                : chr  "Several" "Several" "Routinely" "Systematic" ...
    ##  $ itt_LoTStateAgent                  : chr  "No Allegations" "No Allegations" "No Allegations" "No Allegations" ...
    ##  $ itt_RstrctAccess                   : int  1 1 1 1 0 0 0 0 0 0 ...
    ##  $ NY.GDP.MKTP.KD                     : num  10299 10690 11170 11667 12213 ...
    ##  $ NY.GDP.MKTP.KD.ZG                  : num  2.72 3.8 4.49 4.45 4.69 ...
    ##  $ NY.GDP.PCAP.KD                     : num  38768 39769 41044 42341 43797 ...
    ##  $ NY.GDP.PCAP.KD.ZG                  : num  1.59 2.58 3.21 3.16 3.44 ...
    ##  $ pop                                : num  265659 268803 272137 275543 278862 ...
    ##  $ ln_NY.GDP.MKTP.KD                  : num  9.24 9.28 9.32 9.36 9.41 ...
    ##  $ norm_ln_NY.GDP.MKTP.KD             : num  2.72 2.74 2.76 2.78 2.81 ...
    ##  $ norm_ln_pop                        : num  2.24 2.25 2.25 2.26 2.27 ...
    ##  $ v2x_elecoff                        : num  1 1 1 1 1 1 1 1 1 1 ...
    ##   ..- attr(*, "label")= chr "Elected officials index"
    ##   ..- attr(*, "format.stata")= chr "%9.0g"
    ##  $ v2xel_frefair                      : num  0.907 0.908 0.908 0.91 0.91 ...
    ##   ..- attr(*, "label")= chr "Clean elections index"
    ##   ..- attr(*, "format.stata")= chr "%9.0g"
    ##  $ v2asuffrage                        : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ v2x_jucon                          : num  0.935 0.935 0.935 0.935 0.935 ...
    ##   ..- attr(*, "label")= chr "Judicial constraints on the executive index"
    ##   ..- attr(*, "format.stata")= chr "%9.0g"
    ##  $ v2xlg_legcon                       : num  0.946 0.946 0.946 0.946 0.946 ...
    ##   ..- attr(*, "label")= chr "Legislative constraints on the executive index"
    ##   ..- attr(*, "format.stata")= chr "%9.0g"
    ##  $ v2clacjust                         : num  1.81 1.81 1.81 1.81 1.81 ...
    ##   ..- attr(*, "label")= chr "Social class equality in respect for civil liberty"
    ##   ..- attr(*, "format.stata")= chr "%9.0g"
    ##  $ v2clsocgrp                         : num  0.594 0.594 0.594 0.594 0.594 ...
    ##   ..- attr(*, "label")= chr "Social group equality in respect for civil liberties"
    ##   ..- attr(*, "format.stata")= chr "%9.0g"
    ##  $ v2pepwrses                         : num  0.786 0.786 0.786 0.786 0.786 ...
    ##   ..- attr(*, "label")= chr "Power distributed by socioeconomic position"
    ##   ..- attr(*, "format.stata")= chr "%9.0g"
    ##  $ v2pepwrsoc                         : num  1.58 1.58 1.58 1.58 1.58 ...
    ##   ..- attr(*, "label")= chr "Power distributed by social group"
    ##   ..- attr(*, "format.stata")= chr "%9.0g"
    ##  $ regime                             : Factor w/ 6 levels "Parliamentary democracy",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ dd_democracy                       : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ LJI                                : num  0.99 0.991 0.991 0.991 0.99 ...
    ##  $ post.sd                            : num  0.009 0.0084 0.0082 0.0082 0.0089 0.0092 0.0099 0.0107 0.0109 0.0114 ...
    ##  $ ht_colonial                        : Factor w/ 11 levels "Never colonized",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ mrs_legalsys                       : Factor w/ 4 levels "Civil","Common",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ epr_groups                         : int  6 6 6 6 6 6 6 6 6 6 ...
    ##  $ epr_elf                            : num  0.51 0.51 0.51 0.51 0.51 ...
    ##  $ epr_excluded_groups_count          : int  5 5 5 5 5 5 5 5 5 5 ...
    ##  $ epr_excluded_group_pop             : num  0.297 0.297 0.297 0.297 0.297 0.297 0.297 0.297 0.297 0.297 ...
    ##  $ epr_inpower_groups_count           : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ epr_inpower_groups_pop             : num  0.691 0.691 0.691 0.691 0.691 0.691 0.691 0.691 0.691 0.691 ...
    ##  $ epr_regaut_groups_count            : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ epr_regaut_group_pop               : num  0.0078 0.0078 0.0078 0.0078 0.0078 0.0078 0.0078 0.0078 0.0078 0.0078 ...
    ##  $ norm_ln1p_epr_excluded_groups_count: num  1.14 1.14 1.14 1.14 1.14 ...
    ##  $ norm_sqrt_epr_excluded_group_pop   : num  0.998 0.998 0.998 0.998 0.998 ...
    ##  $ internal_confl                     : num  0 0 0 0 0 0 1 1 1 1 ...
    ##  $ internal_confl_major               : num  0 0 0 0 0 0 1 0 0 0 ...
    ##  $ internal_confl_minor               : num  0 0 0 0 0 0 0 1 1 1 ...
    ##  $ internal_confl_part                : num  0 0 0 0 1 0 1 1 1 1 ...
    ##  $ internal_confl_part_major          : num  0 0 0 0 1 0 1 0 0 1 ...
    ##  $ internal_confl_part_minor          : num  0 0 0 0 0 0 0 1 1 1 ...
    ##  $ war                                : num  0 0 0 0 0 0 1 0 1 0 ...
    ##  $ war_major                          : num  0 0 0 0 0 0 1 0 1 0 ...
    ##  $ war_minor                          : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ any_conflict                       : num  0 0 0 0 1 0 1 1 1 1 ...
    ##  $ any_conflict_major                 : num  0 0 0 0 1 0 1 0 1 1 ...
    ##  $ any_conflict_minor                 : num  0 0 0 0 0 0 0 1 1 1 ...
    ##  $ ext_conf                           : num  0 0 0 0 1 0 1 0 1 1 ...
    ##  $ ext_conf_major                     : num  0 0 0 0 1 0 1 0 1 1 ...
    ##  $ ext_conf_minor                     : num  0 0 0 0 0 0 0 0 1 1 ...
    ##  $ gtd_events                         : int  60 35 40 31 53 32 41 33 33 9 ...
    ##  $ gtd_killed                         : num  178 2 2 4 20 ...
    ##  $ ccp_torture                        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ccp_prerel                         : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ ccp_habcorp                        : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ ccp_dueproc                        : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ ccp_speedtri                       : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ gmfd_functionallyfree              : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ igo_n                              : num  90 89 90 91 89 88 89 88 87 87 ...
    ##  $ igo_total                          : num  323 325 328 337 330 330 323 327 327 327 ...
    ##  $ igo_p                              : num  0.279 0.274 0.274 0.27 0.27 ...
    ##  $ NE.TRD.GNFS.ZS                     : num  22.5 22.7 23.4 22.8 23.3 ...
    ##  $ norm_ln_NE.TRD.GNFS.ZS             : num  -1.63 -1.61 -1.56 -1.6 -1.57 ...
    ##  $ hro_n                              : num  83 90 97 94 91 110 109 108 107 NA ...
    ##  $ hro_secloc                         : num  70 71 71 71 71 71 71 71 71 71 ...
    ##  $ year_poly1                         : num  -0.0388 -0.03105 -0.0233 -0.01556 -0.00781 ...
    ##  $ year_poly2                         : num  0.04167 0.01668 -0.00276 -0.01665 -0.02498 ...
    ##  $ yy_Unknown                         : logi  TRUE FALSE FALSE TRUE TRUE TRUE ...
    ##  $ yy_Criminal                        : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##  $ yy_Dissident                       : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ yy_Marginalized                    : logi  FALSE FALSE FALSE TRUE TRUE FALSE ...
    ##  $ yy_StateAgent                      : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  - attr(*, "yvars")= chr  "yy_Unknown" "yy_Criminal" "yy_Dissident" "yy_Marginalized" ...

## R session info

``` r
sessionInfo()
```

    ## R version 3.6.0 (2019-04-26)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS Mojave 10.14.6
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_3.6.0  magrittr_1.5    tools_3.6.0     htmltools_0.3.6
    ##  [5] yaml_2.2.0      Rcpp_1.0.2      stringi_1.4.3   rmarkdown_1.14 
    ##  [9] knitr_1.24      stringr_1.4.0   xfun_0.8        digest_0.6.20  
    ## [13] evaluate_0.14
