L\&L Metaanalysis
================
Saurabh Khanna
2020-04-08

  - [Reading in data](#reading-in-data)
  - [Calculate effect sizes](#calculate-effect-sizes)
      - [R studies](#r-studies)

``` r
# Libraries
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
    ## ✓ tibble  3.0.0     ✓ dplyr   0.8.5
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
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
# treatment
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

# control
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
| Connor18\_3\_COM |  RS  | \-0.1074613 | 0.0207957 |
| Connor18\_3\_ERC |  RS  | \-0.1103352 | 0.0201836 |
| Connor18\_3\_LIM |  RS  | \-0.1118355 | 0.0198132 |
| Connor18\_4\_ERC |  RS  |   0.0901937 | 0.0179108 |
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
    ##  X.Intercept. Coefficient 0.0653  0.0653    0.0653    0.0653    0.0653   
    ##               Std. Error  0.0365  0.0365    0.0365    0.0365    0.0365   
    ##  Tau.sq       Estimate    0.0178  0.0178    0.0178    0.0178    0.0178   
    ##  Rho = 1
    ##  0.0653 
    ##  0.0365 
    ##  0.0178
