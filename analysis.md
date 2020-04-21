Metaanalysis Script
================
Saurabh Khanna
2020-04-21

  - [Reading in data](#reading-in-data)
  - [Calculate effect sizes](#calculate-effect-sizes)
      - [Post only](#post-only)
      - [Pre and Post](#pre-and-post)
      - [Combining pre and prepost](#combining-pre-and-prepost)
      - [Summary stats](#summary-stats)
  - [Synthesizing effect sizes](#synthesizing-effect-sizes)
      - [Vocabulary](#vocabulary)
      - [Listening Comprehension](#listening-comprehension)
      - [Reading Comprehension](#reading-comprehension)
      - [Morphology](#morphology)
      - [Syntax](#syntax)
      - [AS Studies](#as-studies)
  - [Moderator effects](#moderator-effects)
      - [Vocabulary](#vocabulary-1)
      - [Listening Comprehension](#listening-comprehension-1)
      - [Reading Comprehension](#reading-comprehension-1)

``` r
# Libraries
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.0     ✓ dplyr   0.8.5
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
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
#library(robumeta)
library(MAd)
library(metaforest)
```

    ## Loading required package: ranger

    ## Loading required package: data.table

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

``` r
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

    ## # A tibble: 0 x 1
    ## # … with 1 variable: AUTYR <chr>

All good\!

## Calculate effect sizes

### Post only

``` r
df_post <-
  bind_rows(
    "VR" = read_xlsx(data_file, sheet = "VR") %>% rename_at(vars(-AUTYR), ~ str_replace(., "VR", "")),
    "VS" = read_xlsx(data_file, sheet = "VS") %>% rename_at(vars(-AUTYR), ~ str_replace(., "VS", "")),
    "RR" = read_xlsx(data_file, sheet = "RR") %>% rename_at(vars(-AUTYR), ~ str_replace(., "RR", "")),
    "RS" = read_xlsx(data_file, sheet = "RS") %>% rename_at(vars(-AUTYR), ~ str_replace(., "RS", "")),
    "LR" = read_xlsx(data_file, sheet = "LR") %>% rename_at(vars(-AUTYR), ~ str_replace(., "LR", "")),
    "LS" = read_xlsx(data_file, sheet = "LS") %>% rename_at(vars(-AUTYR), ~ str_replace(., "LS", "")),
    "MR" = read_xlsx(data_file, sheet = "MR") %>% rename_at(vars(-AUTYR), ~ str_replace(., "MR", "")),
    "MS" = read_xlsx(data_file, sheet = "MS") %>% rename_at(vars(-AUTYR), ~ str_replace(., "MS", "")),
    "SR" = read_xlsx(data_file, sheet = "SR") %>% rename_at(vars(-AUTYR), ~ str_replace(., "SR", "")),
    "SS" = read_xlsx(data_file, sheet = "SS") %>% rename_at(vars(-AUTYR), ~ str_replace(., "SS", "")),
    "AS" = read_xlsx(data_file, sheet = "AS") %>% rename_at(vars(-AUTYR), ~ str_replace(., "AS", "")),
    .id = "type"
  ) %>% 
  drop_na(AUTYR) %>%
  filter(is.na(TM1pre)) %>% 
  select_if(~ any(!is.na(.))) %>%
  select(AUTYR, type, sort(current_vars()))


for (mt in 1:4) {
  for (mc in 1:4) {
    if (
      !(str_glue("TM{mt}post") %in% colnames(df_post)) | 
      !(str_glue("CM{mc}post") %in% colnames(df_post))
    ) {
      next
    }
    df_post <-
      escalc(
        data = df_post,
        measure = "SMD",
        m1i = df_post[, str_c("TM", mt, "post")] %>% unlist(),
        m2i = df_post[, str_c("CM", mc, "post")] %>% unlist(),
        sd1i = df_post[, str_c("TS", mt, "post")] %>% unlist(),
        sd2i = df_post[, str_c("CS", mc, "post")] %>% unlist(),
        n1i = df_post[, str_c("TN", mt, "post")] %>% unlist(),
        n2i = df_post[, str_c("CN", mc, "post")] %>% unlist(),
        var.names = c(str_glue("ES_TM{mt}_CM{mc}"), str_glue("EV_TM{mt}_CM{mc}"))
      ) 
  }
}


df_post <-
  df_post %>% 
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

### Pre and Post

``` r
df_prepost <-
  bind_rows(
    "VR" = read_xlsx(data_file, sheet = "VR") %>% rename_at(vars(-AUTYR), ~ str_replace(., "VR", "")),
    "VS" = read_xlsx(data_file, sheet = "VS") %>% rename_at(vars(-AUTYR), ~ str_replace(., "VS", "")),
    "RR" = read_xlsx(data_file, sheet = "RR") %>% rename_at(vars(-AUTYR), ~ str_replace(., "RR", "")),
    "RS" = read_xlsx(data_file, sheet = "RS") %>% rename_at(vars(-AUTYR), ~ str_replace(., "RS", "")),
    "LR" = read_xlsx(data_file, sheet = "LR") %>% rename_at(vars(-AUTYR), ~ str_replace(., "LR", "")),
    "LS" = read_xlsx(data_file, sheet = "LS") %>% rename_at(vars(-AUTYR), ~ str_replace(., "LS", "")),
    "MR" = read_xlsx(data_file, sheet = "MR") %>% rename_at(vars(-AUTYR), ~ str_replace(., "MR", "")),
    "MS" = read_xlsx(data_file, sheet = "MS") %>% rename_at(vars(-AUTYR), ~ str_replace(., "MS", "")),
    "SR" = read_xlsx(data_file, sheet = "SR") %>% rename_at(vars(-AUTYR), ~ str_replace(., "SR", "")),
    "SS" = read_xlsx(data_file, sheet = "SS") %>% rename_at(vars(-AUTYR), ~ str_replace(., "SS", "")),
    "AS" = read_xlsx(data_file, sheet = "AS") %>% rename_at(vars(-AUTYR), ~ str_replace(., "AS", "")),
    .id = "type"
  ) %>% 
  drop_na(AUTYR) %>%
  filter(!is.na(TM1pre)) %>% 
  select_if(~ any(!is.na(.))) %>%
  select(AUTYR, type, sort(current_vars())) %>% 
  mutate(
    TN1post = if_else(is.na(TN1post) & !is.na(TN1pre), TN1pre, TN1post),
    TN2post = if_else(is.na(TN2post) & !is.na(TN2pre), TN2pre, TN2post),
    CN1post = if_else(is.na(CN1post) & !is.na(CN1pre), CN1pre, CN1post),
    CN2post = if_else(is.na(CN2post) & !is.na(CN2pre), CN2pre, CN2post)
  )


# treatment (post-pre)
for (mt in 1:4) {
  if (
    !(str_glue("TM{mt}post") %in% colnames(df_prepost)) | 
    !(str_glue("TM{mt}pre") %in% colnames(df_prepost))
  ) {
    next
  }
  df_prepost <-
    escalc(
      data = df_prepost,
      measure = "SMCR",
      m1i = df_prepost[, str_c("TM", mt, "post")] %>% unlist(),
      m2i = df_prepost[, str_c("TM", mt, "pre")] %>% unlist(),
      sd1i = df_prepost[, str_c("TS", mt, "pre")] %>% unlist(),
      ni = df_prepost[, str_c("TN", mt, "post")] %>% unlist(),
      ri = rep(0.5, 95),
      var.names = c(str_glue("TES_TM{mt}"), str_glue("TEV_TM{mt}"))
    ) 
}

# control (post-pre)
for (mc in 1:4) {
  if (
    !(str_glue("CM{mc}post") %in% colnames(df_prepost)) | 
    !(str_glue("CM{mc}pre") %in% colnames(df_prepost))
  ) {
    next
  }
  df_prepost <-
    escalc(
      data = df_prepost,
      measure = "SMCR",
      m1i = df_prepost[, str_c("CM", mc, "post")] %>% unlist(),
      m2i = df_prepost[, str_c("CM", mc, "pre")] %>% unlist(),
      sd1i = df_prepost[, str_c("CS", mc, "pre")] %>% unlist(),
      ni = df_prepost[, str_c("CN", mc, "post")] %>% unlist(),
      ri = rep(0.5, 95),
      var.names = c(str_glue("CES_CM{mc}"), str_glue("CEV_CM{mc}"))
    ) 
}


# ES and EV taken together
for (m in 1:4) {
  if (
    !(str_glue("TES_TM{m}") %in% colnames(df_prepost)) | 
    !(str_glue("TEV_TM{m}") %in% colnames(df_prepost)) |
    !(str_glue("CES_CM{m}") %in% colnames(df_prepost)) | 
    !(str_glue("CEV_CM{m}") %in% colnames(df_prepost))
  ) {
    next
  }
  # subtracting effect size
  df_prepost[, str_c("ES_TM", m, "_CM", m)] <- 
    (df_prepost[, str_c("TES_TM", m)] %>% unlist()) -
    (df_prepost[, str_c("CES_CM", m)] %>% unlist())
  # adding variance
  df_prepost[, str_c("EV_TM", m, "_CM", m)] <- 
    (df_prepost[, str_c("TEV_TM", m)] %>% unlist()) +
    (df_prepost[, str_c("CEV_CM", m)] %>% unlist())
}


df_prepost <- 
  df_prepost %>% 
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

### Combining pre and prepost

``` r
df_es_direct <-
  tribble(
    ~AUTYR,        ~type, ~ES, ~EV,
    
    # "Apthorp12_K", "VR",  0.98, 0,
    # "Apthorp12_1", "VR",  1.00, 0,
    # "Apthorp12_3", "VR",  0.95, 0,
    # "Apthorp12_4", "VR",  1.24, 0,
    
    # "Apthorp12_P", "VS",  0.06, 0,
    # "Apthorp12_I", "VS", -0.14, 0,
    "Gersten10",   "VS",  0.33, 0.05,
    "Jayanthi18",  "VS", -0.043, 0.001,

    # "Apthorp12_K", "LR",  0.24, 0,
    # "Apthorp12_1", "LR",  0.21, 0,
    # 
    # "Apthorp12_P", "LS",  0.05, 0,
    # 
    # "Apthorp12_3", "RR",  0.09, 0,
    # "Apthorp12_4", "RR",  0.44, 0,
    # 
    # "Apthorp12_I", "RS", -0.11, 0,
    "Gersten10",   "RS",  0.13, 0.04
  )


df_append <- 
  bind_rows(df_post, df_prepost, df_es_direct) %>% 
  left_join(
    read_xlsx(data_file, sheet = "StudyChar") %>% 
      drop_na(AUTYR),
    by = "AUTYR"
  ) %>%
  left_join(
    read_xlsx(data_file, sheet = "Notes") %>% select(AUTYR, Citation),
    by = "AUTYR"
  ) %>% 
  mutate(
    Hours = Hours %>% parse_number(),
    CONT = recode(CONT, "BAU" = "0", "ALT" = "1") %>% as.integer()
  ) %>%
  rename(cont_alt = CONT, citation = Citation) %>% 
  arrange(type, stdid) %>%
  select(type, stdid, citation, AUTYR, everything()) %>% 
  filter(!str_detect(stdid, "^Apth"))
```

    ## New names:
    ## * Notes -> Notes...3
    ## * Notes -> Notes...4
    ## * `` -> ...5
    ## * `` -> ...6
    ## * `` -> ...7
    ## * ...

``` r
cor_es <-
  df_append %>%
  unite("type_stdid", c("type", "stdid")) %>% 
  agg(id = type_stdid, es = ES, var = EV, method = "BHHR", data = .) %>% 
  separate(id, c("type", "stdid")) %>% 
  rename(ES = es, EV = var)

df_clean <-
  df_append %>%
  group_by(type, stdid) %>% 
  summarize_at(vars(TMULT:Hours), ~ round(mean(.))) %>%
  ungroup() %>%
  left_join(cor_es, by = c("type", "stdid")) %>% 
  select(type, stdid, ES, EV, everything())

rm(df_post, df_prepost, df_es_direct, df_append, cor_es)
#df_clean %>% knitr::kable()
```

### Summary stats

``` r
df_clean %>% count(stdid)
```

    ## # A tibble: 56 x 2
    ##    stdid     n
    ##    <chr> <int>
    ##  1 Apel1     2
    ##  2 Apel2     2
    ##  3 ApelK     1
    ##  4 Arth1     1
    ##  5 Arth2     1
    ##  6 Arth3     1
    ##  7 ArthK     1
    ##  8 Aug       1
    ##  9 Baker     3
    ## 10 Berry     2
    ## # … with 46 more rows

``` r
#df_clean %>% writexl::write_xlsx("df_clean.xlsx")
df_clean %>% count(type, stdid)
```

    ## # A tibble: 114 x 3
    ##    type  stdid       n
    ##    <chr> <chr>   <int>
    ##  1 AS    Jones1      1
    ##  2 AS    Jones2      1
    ##  3 AS    Proc19      1
    ##  4 LR    Baker       1
    ##  5 LR    Conn183     1
    ##  6 LR    Conn184     1
    ##  7 LR    Coyne10     1
    ##  8 LR    Coyne19     1
    ##  9 LR    Jiang1      1
    ## 10 LR    Jiang2      1
    ## # … with 104 more rows

``` r
df_v <- df_clean %>% filter(type %in% c("VR", "VS"))
df_l <- df_clean %>% filter(type %in% c("LR", "LS"))
df_r <- df_clean %>% filter(type %in% c("RR", "RS"))
df_m <- df_clean %>% filter(type %in% c("MR", "MS"))
df_s <- df_clean %>% filter(type %in% c("SR", "SS"))
df_a <- df_clean %>% filter(type == "AS")
```

## Synthesizing effect sizes

### Vocabulary

``` r
model_1 <-
  df_v %>%
  arrange(desc(type), desc(ES)) %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    slab = stdid
  )

model_2 <-
  df_v %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    subset = (type == "VR"),
    slab = stdid
  )

model_3 <-
  df_v %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    subset = (type == "VS"),
    slab = stdid
  )

model_1
```

    ## 
    ## Random-Effects Model (k = 66; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 2.4970 (SE = 0.4547)
    ## tau (square root of estimated tau^2 value):      1.5802
    ## I^2 (total heterogeneity / total variability):   99.36%
    ## H^2 (total variability / sampling variability):  155.90
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 65) = 2283.9471, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub 
    ##   1.0599  0.1984  5.3413  <.0001  0.6710  1.4489  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_2
```

    ## 
    ## Random-Effects Model (k = 42; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 3.2308 (SE = 0.7421)
    ## tau (square root of estimated tau^2 value):      1.7974
    ## I^2 (total heterogeneity / total variability):   99.30%
    ## H^2 (total variability / sampling variability):  143.67
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 41) = 1764.6384, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub 
    ##   1.6238  0.2832  5.7328  <.0001  1.0686  2.1789  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_3
```

    ## 
    ## Random-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0026 (SE = 0.0039)
    ## tau (square root of estimated tau^2 value):      0.0510
    ## I^2 (total heterogeneity / total variability):   17.28%
    ## H^2 (total variability / sampling variability):  1.21
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 23) = 31.8420, p-val = 0.1035
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.0337  0.0278  1.2121  0.2255  -0.0208  0.0883    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>% count(type)
```

    ## # A tibble: 2 x 2
    ##   type      n
    ##   <chr> <int>
    ## 1 VR       42
    ## 2 VS       24

``` r
forest(
  model_1,
  xlab = "Vocabulary",
  addcred = T, 
  header = T,
  ylim = c(-1, 80),
  rows = c(3:26, 33:74),
  pch = 21,
  bg = "grey",
  lwd = 1.5
)
op <- par(cex = 0.75, font = 4)
text(-16, c(28, 76), pos = 4, c("Standardized Measure", "Custom Measure"))
addpoly(model_2, row = 31, cex = 1.25, col = "white", lwd = 3)
addpoly(model_3, row = 1, cex = 1.25, col = "white", lwd = 3)
```

<img src="analysis_files/figure-gfm/unnamed-chunk-8-1.png" width="100%" height="100%" />

### Listening Comprehension

``` r
model_1 <-
  df_l %>%
  arrange(desc(type), desc(ES)) %>%  
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    slab = stdid
  )

model_2 <-
  df_l %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    subset = (type == "LR"),
    slab = stdid
  )

model_3 <-
  df_l %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    subset = (type == "LS"),
    slab = stdid
  )

model_1
```

    ## 
    ## Random-Effects Model (k = 16; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.2546 (SE = 0.1114)
    ## tau (square root of estimated tau^2 value):      0.5046
    ## I^2 (total heterogeneity / total variability):   90.77%
    ## H^2 (total variability / sampling variability):  10.83
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 15) = 107.9416, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.1519  0.1386  1.0960  0.2731  -0.1198  0.4236    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_2
```

    ## 
    ## Random-Effects Model (k = 10; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0424 (SE = 0.0333)
    ## tau (square root of estimated tau^2 value):      0.2060
    ## I^2 (total heterogeneity / total variability):   68.17%
    ## H^2 (total variability / sampling variability):  3.14
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 9) = 26.8296, p-val = 0.0015
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub 
    ##   0.2987  0.0852  3.5059  0.0005  0.1317  0.4656  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_3
```

    ## 
    ## Random-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.5617 (SE = 0.4132)
    ## tau (square root of estimated tau^2 value):      0.7494
    ## I^2 (total heterogeneity / total variability):   89.07%
    ## H^2 (total variability / sampling variability):  9.15
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 5) = 51.6442, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval    ci.lb   ci.ub 
    ##  -0.1614  0.3302  -0.4889  0.6249  -0.8087  0.4858    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>% count(type)
```

    ## # A tibble: 2 x 2
    ##   type      n
    ##   <chr> <int>
    ## 1 LR       10
    ## 2 LS        6

``` r
forest(
  model_1,
  xlab = "Listening Comprehension",
  addcred = T, 
  header = T,
  ylim = c(-1, 30),
  rows = c(3:8, 15:24),
  pch = 21,
  bg = "grey",
  lwd = 1.5
)
op <- par(cex = 0.75, font = 4)
text(-6.3, c(10, 26), pos = 4, c("Standardized Measure", "Custom Measure"))
addpoly(model_2, row = 12, cex = 1.25, col = "white", lwd = 3)
addpoly(model_3, row = 1, cex = 1.25, col = "white", lwd = 3)
```

<img src="analysis_files/figure-gfm/unnamed-chunk-10-1.png" width="100%" height="100%" />

### Reading Comprehension

``` r
model_1 <-
  df_r %>%
  arrange(desc(type), desc(ES)) %>%  
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    slab = stdid
  )

model_2 <-
  df_r %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    subset = (type == "RR"),
    slab = stdid
  )

model_3 <-
  df_r %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    subset = (type == "RS"),
    slab = stdid
  )

model_1
```

    ## 
    ## Random-Effects Model (k = 23; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.1247 (SE = 0.0472)
    ## tau (square root of estimated tau^2 value):      0.3531
    ## I^2 (total heterogeneity / total variability):   92.60%
    ## H^2 (total variability / sampling variability):  13.51
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 22) = 106.6186, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub 
    ##   0.2031  0.0839  2.4203  0.0155  0.0386  0.3676  * 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_2
```

    ## 
    ## Random-Effects Model (k = 5; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.5327 (SE = 0.4501)
    ## tau (square root of estimated tau^2 value):      0.7299
    ## I^2 (total heterogeneity / total variability):   96.31%
    ## H^2 (total variability / sampling variability):  27.14
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 4) = 46.6163, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.6633  0.3614  1.8354  0.0664  -0.0450  1.3716  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_3
```

    ## 
    ## Random-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0115 (SE = 0.0087)
    ## tau (square root of estimated tau^2 value):      0.1071
    ## I^2 (total heterogeneity / total variability):   54.35%
    ## H^2 (total variability / sampling variability):  2.19
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 17) = 39.3585, p-val = 0.0016
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.0756  0.0405  1.8649  0.0622  -0.0039  0.1550  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>% count(type)
```

    ## # A tibble: 2 x 2
    ##   type      n
    ##   <chr> <int>
    ## 1 RR        5
    ## 2 RS       18

``` r
forest(
  model_1,
  xlab = "Reading Comprehension",
  addcred = T, 
  header = T,
  ylim = c(-1, 37),
  rows = c(3:20, 27:31),
  pch = 21,
  bg = "grey",
  lwd = 1.5
)
op <- par(cex = 0.75, font = 4)
text(-8.7, c(22, 33), pos = 4, c("Standardized Measure", "Custom Measure"))
addpoly(model_2, row = 24, cex = 1.25, col = "white", lwd = 3)
addpoly(model_3, row = 1, cex = 1.25, col = "white", lwd = 3)
```

<img src="analysis_files/figure-gfm/unnamed-chunk-12-1.png" width="100%" height="100%" />

### Morphology

``` r
df_m %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    slab = stdid
  )
```

    ## 
    ## Random-Effects Model (k = 4; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0340 (SE = 0.1776)
    ## tau (square root of estimated tau^2 value):      0.1844
    ## I^2 (total heterogeneity / total variability):   14.06%
    ## H^2 (total variability / sampling variability):  1.16
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 3) = 5.0583, p-val = 0.1676
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub 
    ##   0.9660  0.2362  4.0892  <.0001  0.5030  1.4290  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_m %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    slab = stdid
  ) %>% 
  forest(
    order = "obs",
    xlab = "Morphology",
    addcred = T, 
    header = T,
    pch = 21,
    bg = "grey",
    lwd = 1.5
  )
```

<img src="analysis_files/figure-gfm/unnamed-chunk-13-1.png" width="672" />

### Syntax

``` r
df_s %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    slab = stdid
  )
```

    ## 
    ## Random-Effects Model (k = 2; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0 (SE = 0.0292)
    ## tau (square root of estimated tau^2 value):      0
    ## I^2 (total heterogeneity / total variability):   0.00%
    ## H^2 (total variability / sampling variability):  1.00
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 1) = 0.2039, p-val = 0.6516
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.0314  0.1003  0.3133  0.7540  -0.1651  0.2280    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_s %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    slab = stdid
  ) %>% 
  forest(
    order = "obs",
    xlab = "Syntax",
    addcred = T, 
    header = T,
    pch = 21,
    bg = "grey",
    lwd = 1.5
  )
```

<img src="analysis_files/figure-gfm/unnamed-chunk-14-1.png" width="672" />

### AS Studies

``` r
df_a %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    slab = stdid
  )
```

    ## 
    ## Random-Effects Model (k = 3; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0147 (SE = 0.0202)
    ## tau (square root of estimated tau^2 value):      0.1213
    ## I^2 (total heterogeneity / total variability):   79.61%
    ## H^2 (total variability / sampling variability):  4.90
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 2) = 11.5331, p-val = 0.0031
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub 
    ##   0.2010  0.0817  2.4596  0.0139  0.0408  0.3612  * 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_a %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    slab = stdid
  ) %>% 
  forest(
    order = "obs",
    xlab = "Academic Learning",
    addcred = T, 
    header = T,
    pch = 21,
    bg = "grey",
    lwd = 1.5
  )
```

<img src="analysis_files/figure-gfm/unnamed-chunk-15-1.png" width="672" />

## Moderator effects

### Vocabulary

Checking variable correlations:

``` r
p.mat <- ggcorrplot::cor_pmat(df_v %>% select(TMULT:Hours))
df_v %>% 
  select(TMULT:Hours) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  ggcorrplot::ggcorrplot(type = "lower", p.mat = p.mat, insig = "blank")
```

<img src="analysis_files/figure-gfm/unnamed-chunk-16-1.png" width="672" />

MetaForest:

``` r
df_v %>% 
  rename(vi = EV) %>%
  mutate(stdid = stdid %>% as_factor()) %>% 
  drop_na(Hours) %>% 
  MetaForest(
    formula = ES ~ .,
    data = .,
    study = "stdid"
  ) %>%
  preselect(replications = 500L) %>% 
  plot()
```

<img src="analysis_files/figure-gfm/unnamed-chunk-17-1.png" width="672" />

Meta-regressions:

``` r
df_v %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ type
  )
```

    ## 
    ## Mixed-Effects Model (k = 66; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     1.9802 (SE = 0.3663)
    ## tau (square root of estimated tau^2 value):             1.4072
    ## I^2 (residual heterogeneity / unaccounted variability): 99.12%
    ## H^2 (unaccounted variability / sampling variability):   114.14
    ## R^2 (amount of heterogeneity accounted for):            20.70%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 64) = 1796.4804, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 16.6018, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb    ci.ub 
    ## intrcpt    1.6106  0.2241   7.1878  <.0001   1.1714   2.0498  *** 
    ## typeVS    -1.4957  0.3671  -4.0745  <.0001  -2.2152  -0.7762  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ scale(Hours)
  )
```

    ## 
    ## Mixed-Effects Model (k = 64; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.5844 (SE = 0.4818)
    ## tau (square root of estimated tau^2 value):             1.6076
    ## I^2 (residual heterogeneity / unaccounted variability): 99.17%
    ## H^2 (unaccounted variability / sampling variability):   120.45
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 62) = 2087.8946, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.3269, p-val = 0.5675
    ## 
    ## Model Results:
    ## 
    ##               estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt         1.0888  0.2050  5.3110  <.0001   0.6870  1.4907  *** 
    ## scale(Hours)    0.1181  0.2065  0.5717  0.5675  -0.2867  0.5229      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TPAD
  )
```

    ## 
    ## Mixed-Effects Model (k = 66; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.4536 (SE = 0.4506)
    ## tau (square root of estimated tau^2 value):             1.5664
    ## I^2 (residual heterogeneity / unaccounted variability): 99.35%
    ## H^2 (unaccounted variability / sampling variability):   153.49
    ## R^2 (amount of heterogeneity accounted for):            1.74%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 64) = 2282.2492, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 2.1890, p-val = 0.1390
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.1835  0.2139   5.5335  <.0001   0.7643  1.6027  *** 
    ## TPAD      -0.8076  0.5458  -1.4795  0.1390  -1.8774  0.2622      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TDD
  )
```

    ## 
    ## Mixed-Effects Model (k = 66; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.4672 (SE = 0.4530)
    ## tau (square root of estimated tau^2 value):             1.5707
    ## I^2 (residual heterogeneity / unaccounted variability): 99.30%
    ## H^2 (unaccounted variability / sampling variability):   142.52
    ## R^2 (amount of heterogeneity accounted for):            1.19%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 64) = 2277.2851, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.8229, p-val = 0.1770
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.1904  0.2198   5.4160  <.0001   0.7596  1.6212  *** 
    ## TDD       -0.6734  0.4987  -1.3502  0.1770  -1.6509  0.3041      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ Grade5
  )
```

    ## 
    ## Mixed-Effects Model (k = 66; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.4788 (SE = 0.4552)
    ## tau (square root of estimated tau^2 value):             1.5744
    ## I^2 (residual heterogeneity / unaccounted variability): 99.30%
    ## H^2 (unaccounted variability / sampling variability):   142.62
    ## R^2 (amount of heterogeneity accounted for):            0.73%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 64) = 2283.7461, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.5363, p-val = 0.2152
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.1651  0.2152   5.4132  <.0001   0.7432  1.5869  *** 
    ## Grade5    -0.6756  0.5451  -1.2395  0.2152  -1.7440  0.3927      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TTEC
  )
```

    ## 
    ## Mixed-Effects Model (k = 66; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.5322 (SE = 0.4646)
    ## tau (square root of estimated tau^2 value):             1.5913
    ## I^2 (residual heterogeneity / unaccounted variability): 99.35%
    ## H^2 (unaccounted variability / sampling variability):   153.94
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 64) = 2257.9700, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.2458, p-val = 0.6201
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.1317  0.2462   4.5957  <.0001   0.6490  1.6143  *** 
    ## TTEC      -0.2088  0.4213  -0.4958  0.6201  -1.0345  0.6168      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Only `type` is significant.

### Listening Comprehension

Checking variable correlations:

``` r
p.mat <- ggcorrplot::cor_pmat(df_l %>% select(TMULT:Hours))
df_l %>% 
  select(TMULT:Hours) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  ggcorrplot::ggcorrplot(type = "lower", p.mat = p.mat, insig = "blank")
```

<img src="analysis_files/figure-gfm/unnamed-chunk-19-1.png" width="672" />

MetaForest:

``` r
df_l %>% 
  rename(vi = EV) %>%
  mutate(stdid = stdid %>% as_factor()) %>% 
  drop_na(Hours) %>% 
  MetaForest(
    formula = ES ~ .,
    data = .,
    study = "stdid"
  ) %>%
  preselect(replications = 500L) %>% 
  plot()
```

<img src="analysis_files/figure-gfm/unnamed-chunk-20-1.png" width="672" />

Meta-regressions:

``` r
df_l %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TRC
  )
```

    ## 
    ## Mixed-Effects Model (k = 16; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1629 (SE = 0.0798)
    ## tau (square root of estimated tau^2 value):             0.4036
    ## I^2 (residual heterogeneity / unaccounted variability): 84.79%
    ## H^2 (unaccounted variability / sampling variability):   6.57
    ## R^2 (amount of heterogeneity accounted for):            36.03%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 14) = 64.9286, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 7.1928, p-val = 0.0073
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb    ci.ub 
    ## intrcpt    0.3834  0.1447   2.6494  0.0081   0.0998   0.6670  ** 
    ## TRC       -0.6440  0.2401  -2.6819  0.0073  -1.1147  -0.1734  ** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ scale(Hours)
  )
```

    ## 
    ## Mixed-Effects Model (k = 16; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.2407 (SE = 0.1092)
    ## tau (square root of estimated tau^2 value):             0.4907
    ## I^2 (residual heterogeneity / unaccounted variability): 89.85%
    ## H^2 (unaccounted variability / sampling variability):   9.86
    ## R^2 (amount of heterogeneity accounted for):            5.44%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 14) = 86.4545, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.3945, p-val = 0.2377
    ## 
    ## Model Results:
    ## 
    ##               estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt         0.1626  0.1357  1.1984  0.2308  -0.1034  0.4286    
    ## scale(Hours)    0.1768  0.1498  1.1809  0.2377  -0.1167  0.4704    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ type
  )
```

    ## 
    ## Mixed-Effects Model (k = 16; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1982 (SE = 0.0933)
    ## tau (square root of estimated tau^2 value):             0.4452
    ## I^2 (residual heterogeneity / unaccounted variability): 88.18%
    ## H^2 (unaccounted variability / sampling variability):   8.46
    ## R^2 (amount of heterogeneity accounted for):            22.15%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 14) = 78.4738, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 3.8417, p-val = 0.0500
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb    ci.ub 
    ## intrcpt    0.3219  0.1525   2.1110  0.0348   0.0230   0.6208  * 
    ## typeLS    -0.5213  0.2660  -1.9600  0.0500  -1.0426  -0.0000  * 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TMOR
  )
```

    ## 
    ## Mixed-Effects Model (k = 16; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.2155 (SE = 0.1001)
    ## tau (square root of estimated tau^2 value):             0.4642
    ## I^2 (residual heterogeneity / unaccounted variability): 89.32%
    ## H^2 (unaccounted variability / sampling variability):   9.36
    ## R^2 (amount of heterogeneity accounted for):            15.37%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 14) = 100.1503, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 3.1326, p-val = 0.0767
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0214  0.1486  0.1440  0.8855  -0.2698  0.3126    
    ## TMOR       0.5336  0.3015  1.7699  0.0767  -0.0573  1.1246  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TMULT
  )
```

    ## 
    ## Mixed-Effects Model (k = 16; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.2155 (SE = 0.1001)
    ## tau (square root of estimated tau^2 value):             0.4642
    ## I^2 (residual heterogeneity / unaccounted variability): 89.32%
    ## H^2 (unaccounted variability / sampling variability):   9.36
    ## R^2 (amount of heterogeneity accounted for):            15.37%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 14) = 100.1503, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 3.1326, p-val = 0.0767
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0214  0.1486  0.1440  0.8855  -0.2698  0.3126    
    ## TMULT      0.5336  0.3015  1.7699  0.0767  -0.0573  1.1246  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ SmallGr
  )
```

    ## 
    ## Mixed-Effects Model (k = 16; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.2124 (SE = 0.0992)
    ## tau (square root of estimated tau^2 value):             0.4609
    ## I^2 (residual heterogeneity / unaccounted variability): 88.37%
    ## H^2 (unaccounted variability / sampling variability):   8.60
    ## R^2 (amount of heterogeneity accounted for):            16.56%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 14) = 105.0285, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 3.3346, p-val = 0.0678
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.4120  0.1923   2.1430  0.0321   0.0352  0.7889  * 
    ## SmallGr   -0.4721  0.2585  -1.8261  0.0678  -0.9788  0.0346  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ WholeCl
  )
```

    ## 
    ## Mixed-Effects Model (k = 16; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.2039 (SE = 0.0960)
    ## tau (square root of estimated tau^2 value):             0.4515
    ## I^2 (residual heterogeneity / unaccounted variability): 87.34%
    ## H^2 (unaccounted variability / sampling variability):   7.90
    ## R^2 (amount of heterogeneity accounted for):            19.92%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 14) = 106.7320, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 4.0553, p-val = 0.0440
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt   -0.1707  0.2036  -0.8384  0.4018  -0.5697  0.2283    
    ## WholeCl    0.5229  0.2597   2.0138  0.0440   0.0140  1.0319  * 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

TRC, type, TMOR, TMULT, SmallGr, WholeCl are significant.

### Reading Comprehension

Checking variable correlations:

``` r
p.mat <- ggcorrplot::cor_pmat(df_r %>% select(TMULT:Hours))
df_r %>% 
  select(TMULT:Hours) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  ggcorrplot::ggcorrplot(type = "lower", p.mat = p.mat, insig = "blank")
```

<img src="analysis_files/figure-gfm/unnamed-chunk-22-1.png" width="672" />

MetaForest:

``` r
df_r %>% 
  rename(vi = EV) %>%
  mutate(stdid = stdid %>% as_factor()) %>% 
  drop_na(Hours) %>% 
  MetaForest(
    formula = ES ~ .,
    data = .,
    study = "stdid"
  ) %>%
  preselect(replications = 500L) %>% 
  plot()
```

<img src="analysis_files/figure-gfm/unnamed-chunk-23-1.png" width="672" />

Meta-regressions:

``` r
df_r %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ type
  )
```

    ## 
    ## Mixed-Effects Model (k = 23; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0845 (SE = 0.0351)
    ## tau (square root of estimated tau^2 value):             0.2907
    ## I^2 (residual heterogeneity / unaccounted variability): 88.75%
    ## H^2 (unaccounted variability / sampling variability):   8.89
    ## R^2 (amount of heterogeneity accounted for):            32.22%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 21) = 85.9748, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 7.8663, p-val = 0.0050
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb    ci.ub 
    ## intrcpt    0.6082  0.1637   3.7155  0.0002   0.2873   0.9290  *** 
    ## typeRS    -0.5106  0.1821  -2.8047  0.0050  -0.8675  -0.1538   ** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TTEC
  )
```

    ## 
    ## Mixed-Effects Model (k = 23; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1072 (SE = 0.0427)
    ## tau (square root of estimated tau^2 value):             0.3274
    ## I^2 (residual heterogeneity / unaccounted variability): 91.41%
    ## H^2 (unaccounted variability / sampling variability):   11.64
    ## R^2 (amount of heterogeneity accounted for):            14.05%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 21) = 101.4828, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 4.8599, p-val = 0.0275
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0803  0.0958  0.8380  0.4020  -0.1075  0.2681    
    ## TTEC       0.3718  0.1687  2.2045  0.0275   0.0412  0.7024  * 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

`type` and `TTEC` are significant.
