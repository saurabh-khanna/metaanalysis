L\&L Meta-analysis
================
Saurabh Khanna
2020-04-10

  - [Reading in data](#reading-in-data)
  - [Calculate effect sizes](#calculate-effect-sizes)
      - [R studies](#r-studies)

``` r
# Libraries
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
    ## ✓ tibble  3.0.0     ✓ dplyr   0.8.5
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(metafor)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## Loading 'metafor' package (version 2.4-0). For an overview 
    ## and introduction to the package please type: help(metafor).

``` r
library(robumeta)

# Parameters
data_file <- here::here("data/L&L Data Set Means SDs.xlsx")
```

## Reading in data

``` r
# join checks
read_xlsx(data_file, sheet = "VR") %>% 
  select(AUTYR) %>% 
  drop_na(AUTYR) %>% 
  anti_join(
    read_xlsx(data_file, sheet = "StudyChar") %>% 
      select(AUTYR) %>% 
      drop_na(AUTYR),
    by = "AUTYR"
  )
```

    ## # A tibble: 0 x 1
    ## # … with 1 variable: AUTYR <chr>

``` r
read_xlsx(data_file, sheet = "VS") %>% 
  select(AUTYR) %>% 
  drop_na(AUTYR) %>% 
  anti_join(
    read_xlsx(data_file, sheet = "StudyChar") %>% 
      select(AUTYR) %>% 
      drop_na(AUTYR),
    by = "AUTYR"
  )
```

    ## # A tibble: 0 x 1
    ## # … with 1 variable: AUTYR <chr>

``` r
read_xlsx(data_file, sheet = "LR") %>% 
  select(AUTYR) %>% 
  drop_na(AUTYR) %>% 
  anti_join(
    read_xlsx(data_file, sheet = "StudyChar") %>% 
      select(AUTYR) %>% 
      drop_na(AUTYR),
    by = "AUTYR"
  )
```

    ## # A tibble: 0 x 1
    ## # … with 1 variable: AUTYR <chr>

``` r
read_xlsx(data_file, sheet = "LS") %>% 
  select(AUTYR) %>% 
  drop_na(AUTYR) %>% 
  anti_join(
    read_xlsx(data_file, sheet = "StudyChar") %>% 
      select(AUTYR) %>% 
      drop_na(AUTYR),
    by = "AUTYR"
  )
```

    ## # A tibble: 0 x 1
    ## # … with 1 variable: AUTYR <chr>

``` r
read_xlsx(data_file, sheet = "RR") %>% 
  select(AUTYR) %>% 
  drop_na(AUTYR) %>% 
  anti_join(
    read_xlsx(data_file, sheet = "StudyChar") %>% 
      select(AUTYR) %>% 
      drop_na(AUTYR),
    by = "AUTYR"
  )
```

    ## # A tibble: 0 x 1
    ## # … with 1 variable: AUTYR <chr>

``` r
read_xlsx(data_file, sheet = "RS") %>% 
  select(AUTYR) %>% 
  drop_na(AUTYR) %>% 
  anti_join(
    read_xlsx(data_file, sheet = "StudyChar") %>% 
      select(AUTYR) %>% 
      drop_na(AUTYR),
    by = "AUTYR"
  )
```

    ## New names:
    ## * TSRS1delay -> TSRS1delay...8
    ## * TSRS1delay -> TSRS1delay...9
    ## * TSRS2delay -> TSRS2delay...24
    ## * TSRS2delay -> TSRS2delay...25

    ## # A tibble: 0 x 1
    ## # … with 1 variable: AUTYR <chr>

All good\!

## Calculate effect sizes

### R studies

#### Post only

``` r
df_r_post <-
  read_xlsx(data_file, sheet = "RR") %>% 
  rename_at(vars(-AUTYR), ~ str_replace(., "RR", "")) %>%
  bind_rows(
    "RR" = .,
    "RS" = read_xlsx(data_file, sheet = "RS") %>% rename_at(vars(-AUTYR), ~ str_replace(., "RS", "")),
    .id = "type"
  ) %>% 
  drop_na(AUTYR) %>%
  filter(is.na(TM1pre)) %>% 
  select_if(~ any(!is.na(.))) %>%
  select(AUTYR, type, sort(current_vars()))
```

    ## New names:
    ## * TSRS1delay -> TSRS1delay...8
    ## * TSRS1delay -> TSRS1delay...9
    ## * TSRS2delay -> TSRS2delay...24
    ## * TSRS2delay -> TSRS2delay...25

    ## Warning: current_vars() is deprecated. 
    ## Please use tidyselect::peek_vars() instead
    ## This warning is displayed once per session.

``` r
for (mt in 1:4) {
  for (mc in 1:4) {
    if (
      !(str_glue("TM{mt}post") %in% colnames(df_r_post)) | 
      !(str_glue("CM{mc}post") %in% colnames(df_r_post))
    ) {
      next
    }
    df_r_post <-
      escalc(
        data = df_r_post,
        measure = "SMD",
        m1i = df_r_post[, str_c("TM", mt, "post")] %>% unlist(),
        m2i = df_r_post[, str_c("CM", mc, "post")] %>% unlist(),
        sd1i = df_r_post[, str_c("TS", mt, "post")] %>% unlist(),
        sd2i = df_r_post[, str_c("CM", mc, "post")] %>% unlist(),
        n1i = df_r_post[, str_c("TN", mt, "post")] %>% unlist(),
        n2i = df_r_post[, str_c("CN", mc, "post")] %>% unlist(),
        var.names = c(str_glue("ES_TM{mt}_CM{mc}"), str_glue("EV_TM{mt}_CM{mc}"))
      ) 
  }
}


df_r_post <-
  df_r_post %>% 
  transmute(
    AUTYR, 
    type,
    ES =
      pmap_dbl(
        select(., starts_with("ES_")),
        ~ mean(c(...), na.rm = TRUE)
      ),
    EV =
      pmap_dbl(
        select(., starts_with("EV_")),
        ~ mean(c(...), na.rm = TRUE)
      )
  )
```

#### Pre and Post

``` r
df_r_prepost <-
  read_xlsx(data_file, sheet = "RR") %>% 
  rename_at(vars(-AUTYR), ~ str_replace(., "RR", "")) %>%
  bind_rows(
    "RR" = .,
    "RS" = read_xlsx(data_file, sheet = "RS") %>% rename_at(vars(-AUTYR), ~ str_replace(., "RS", "")),
    .id = "type"
  ) %>% 
  drop_na(AUTYR) %>%
  filter(!is.na(TM1pre)) %>% 
  select_if(~ any(!is.na(.))) %>%
  select(AUTYR, type, sort(current_vars()))
```

    ## New names:
    ## * TSRS1delay -> TSRS1delay...8
    ## * TSRS1delay -> TSRS1delay...9
    ## * TSRS2delay -> TSRS2delay...24
    ## * TSRS2delay -> TSRS2delay...25

``` r
# treatment (post-pre)
for (mt in 1:4) {
  for (mc in 1:4) {
    if (
      !(str_glue("TM{mt}post") %in% colnames(df_r_prepost)) | 
      !(str_glue("TM{mt}pre") %in% colnames(df_r_prepost))
    ) {
      next
    }
    df_r_prepost <-
      escalc(
        data = df_r_prepost,
        measure = "SMCR",
        m1i = df_r_prepost[, str_c("TM", mt, "post")] %>% unlist(),
        m2i = df_r_prepost[, str_c("TM", mt, "pre")] %>% unlist(),
        sd1i = df_r_prepost[, str_c("TS", mt, "pre")] %>% unlist(),
        ni = df_r_prepost[, str_c("TN", mt, "post")] %>% unlist(),
        ri = rep(0.7, 19),
        var.names = c(str_glue("TES_TM{mt}_CM{mc}"), str_glue("TEV_TM{mt}_CM{mc}"))
      ) 
  }
}

# control (post-pre)
for (mt in 1:4) {
  for (mc in 1:4) {
    if (
      !(str_glue("CM{mc}post") %in% colnames(df_r_prepost)) | 
      !(str_glue("CM{mc}pre") %in% colnames(df_r_prepost))
    ) {
      next
    }
    df_r_prepost <-
      escalc(
        data = df_r_prepost,
        measure = "SMCR",
        m1i = df_r_prepost[, str_c("CM", mc, "post")] %>% unlist(),
        m2i = df_r_prepost[, str_c("CM", mc, "pre")] %>% unlist(),
        sd1i = df_r_prepost[, str_c("CS", mc, "pre")] %>% unlist(),
        ni = df_r_prepost[, str_c("CN", mc, "post")] %>% unlist(),
        ri = rep(0.7, 19),
        var.names = c(str_glue("CES_TM{mt}_CM{mc}"), str_glue("CEV_TM{mt}_CM{mc}"))
      ) 
  }
}

# ES and EV taken together
for (mt in 1:4) {
  for (mc in 1:4) {
    if (
      !(str_glue("TES_TM{mt}_CM{mc}") %in% colnames(df_r_prepost)) | 
      !(str_glue("TEV_TM{mt}_CM{mc}") %in% colnames(df_r_prepost)) |
      !(str_glue("CES_TM{mt}_CM{mc}") %in% colnames(df_r_prepost)) | 
      !(str_glue("CEV_TM{mt}_CM{mc}") %in% colnames(df_r_prepost))
    ) {
      next
    }
    # subtracting effect size
    df_r_prepost[, str_c("ES_TM", mt, "_CM", mc)] <- 
      (df_r_prepost[, str_c("TES_TM", mt, "_CM", mc)] %>% unlist()) -
      (df_r_prepost[, str_c("CES_TM", mt, "_CM", mc)] %>% unlist())
    # adding variance
    df_r_prepost[, str_c("EV_TM", mt, "_CM", mc)] <- 
      (df_r_prepost[, str_c("TEV_TM", mt, "_CM", mc)] %>% unlist()) +
      (df_r_prepost[, str_c("CEV_TM", mt, "_CM", mc)] %>% unlist())
  }
}


df_r_prepost <- 
  df_r_prepost %>% 
  transmute(
    AUTYR, 
    type,
    ES =
      pmap_dbl(
        select(., starts_with("ES_")),
        ~ mean(c(...), na.rm = TRUE)
      ),
    EV =
      pmap_dbl(
        select(., starts_with("EV_")),
        ~ mean(c(...), na.rm = TRUE)
      )
  )
```

Combining all R studies in a single tibble:

``` r
df_r <- bind_rows(df_r_post, df_r_prepost)

df_r %>% knitr::kable()
```

| AUTYR            | type |          ES |        EV |
| :--------------- | :--: | ----------: | --------: |
| Dalton11\_V      |  RR  |   0.0995073 | 0.0543501 |
| Dalton11\_VC     |  RR  |   0.1255728 | 0.0579221 |
| Connor18\_3\_COM |  RS  |   0.0074417 | 0.0208056 |
| Connor18\_3\_ERC |  RS  | \-0.0044229 | 0.0201864 |
| Connor18\_3\_LIM |  RS  | \-0.0039575 | 0.0198170 |
| Connor18\_4\_ERC |  RS  | \-0.0055516 | 0.0179116 |
| Dalton11\_V      |  RS  |   0.0289196 | 0.0540992 |
| Dalton11\_VC     |  RS  |   0.0059258 | 0.0575660 |
| Berry13          |  RR  | \-0.3571446 | 1.2383362 |
| Graham15         |  RR  |   0.1519858 | 0.0106017 |
| Silverman17a\_4  |  RR  |   0.8508342 | 0.0139246 |
| VadSanHer15      |  RR  |   0.2178669 | 0.0020603 |
| Apel14\_1        |  RS  |   0.3604871 | 0.0768706 |
| Apel14\_2        |  RS  | \-0.0919161 | 0.0553390 |
| Daunic13         |  RS  | \-0.2206484 | 0.0596250 |
| Jones19\_1       |  RS  |   0.0537309 | 0.0010934 |
| Jones19\_2       |  RS  |   0.1929975 | 0.0011084 |
| Morris12         |  RS  |   0.2364356 | 0.0196849 |
| Proctor11        |  RS  | \-0.0436775 | 0.0105416 |
| Proctor19        |  RS  |   0.2331418 | 0.0124730 |
| Silverman17b\_4  |  RS  | \-0.1127521 | 0.0114530 |
| Silverman17a\_4  |  RS  | \-0.0014823 | 0.0045630 |
| Simmons10\_CBAU  |  RS  | \-0.0727496 | 0.0054170 |
| Simmons10\_CALT  |  RS  | \-0.0568601 | 0.0044384 |
| Tong10\_B        |  RS  |   0.0172740 | 0.0777953 |
| Tong10\_G        |  RS  |   0.2822964 | 0.0908513 |
| VadSanHer15      |  RS  |   0.0500486 | 0.0020736 |

``` r
# All R studies (REML)
df_r %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    slab = AUTYR
  ) %>% 
  forest(
    order = "obs",
    xlab = "Reading Comprehension",
    addcred = T, 
    header = T
  )
```

<img src="analysis_files/figure-gfm/unnamed-chunk-5-1.png" width="100%" height="100%" />

``` r
# All R studies (RVE)
df_r %>% 
  robu(
    formula = ES ~ 1, 
    var.eff.size = EV, 
    studynum = AUTYR,
    data = ., 
    modelweights = "CORR",
    rho = 0.8
  ) %>% 
  sensitivity()
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## Model: ES ~ 1 
    ## 
    ## Sensitivity Analysis 
    ## 
    ##                           Rho = 0 Rho = 0.2 Rho = 0.4 Rho = 0.6 Rho = 0.8
    ##  X.Intercept. Coefficient 0.0749  0.0749    0.0749    0.0749    0.0749   
    ##               Std. Error  0.0345  0.0345    0.0345    0.0345    0.0345   
    ##  Tau.sq       Estimate    0.0163  0.0163    0.0163    0.0163    0.0163   
    ##  Rho = 1
    ##  0.0749 
    ##  0.0345 
    ##  0.0163

#### Moderator effects

``` r
df_r %>% 
  left_join(
    read_xlsx(data_file, sheet = "StudyChar") %>% 
      drop_na(AUTYR),
    by = "AUTYR"
  ) %>%
  mutate(
    Hours = Hours %>% parse_number()
  ) %>% 
  robu(
    formula = ES ~ TMULT + WholeCl + SmallGr + Indiv + Duration + TVOC + TMOR + TPAD + TLRC + TDD + TTEC + TSTR, 
    var.eff.size = EV, 
    studynum = AUTYR,
    data = ., 
    modelweights = "CORR",
    rho = 0.8
  )
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ TMULT + WholeCl + SmallGr + Indiv + Duration + TVOC + TMOR + TPAD + TLRC + TDD + TTEC + TSTR 
    ## 
    ## Number of studies = 23 
    ## Number of outcomes = 27 (min = 1 , mean = 1.17 , median = 1 , max = 2 )
    ## Rho = 0.8 
    ## I.sq = 72.04371 
    ## Tau.sq = 0.01920417 
    ## 
    ##                 Estimate StdErr t-value  dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1  X.Intercept.  0.04837 0.6296  0.0768 2.89   0.944   -1.998    2.095    
    ## 2         TMULT  0.01944 0.0636  0.3056 1.81   0.791   -0.283    0.322    
    ## 3       WholeCl -0.02266 0.3718 -0.0610 2.15   0.957   -1.522    1.476    
    ## 4       SmallGr -0.02729 0.2616 -0.1043 2.66   0.924   -0.924    0.869    
    ## 5         Indiv -0.20681 0.3331 -0.6209 3.70   0.571   -1.162    0.748    
    ## 6      Duration -0.10546 0.1311 -0.8046 2.40   0.493   -0.588    0.377    
    ## 7          TVOC  0.23162 0.3911  0.5922 4.55   0.582   -0.804    1.268    
    ## 8          TMOR  0.08466 0.3384  0.2502 2.82   0.820   -1.032    1.202    
    ## 9          TPAD  0.17841 0.1953  0.9135 2.71   0.435   -0.482    0.839    
    ## 10         TLRC -0.18886 0.1225 -1.5415 2.36   0.244   -0.646    0.269    
    ## 11          TDD -0.00125 0.3040 -0.0041 2.20   0.997   -1.200    1.197    
    ## 12         TTEC  0.17191 0.1457  1.1801 2.90   0.326   -0.301    0.644    
    ## 13         TSTR -0.19332 0.0699 -2.7654 1.73   0.128   -0.543    0.156    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results
