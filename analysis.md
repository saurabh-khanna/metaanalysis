Meta-analysis Script
================
Saurabh Khanna
2020-04-23

  - [Reading in data](#reading-in-data)
  - [Calculate effect sizes](#calculate-effect-sizes)
      - [Post only](#post-only)
      - [Pre and Post](#pre-and-post)
      - [Directly entered ES](#directly-entered-es)
      - [Combining all](#combining-all)
      - [Summary stats](#summary-stats)
  - [Synthesizing effect sizes](#synthesizing-effect-sizes)
      - [Vocabulary](#vocabulary)
      - [Listening Comprehension](#listening-comprehension)
      - [Reading Comprehension](#reading-comprehension)
      - [Morphology](#morphology)
      - [Syntax](#syntax)
      - [Academic Learning](#academic-learning)
  - [Moderator effects](#moderator-effects)
      - [Vocabulary (Taken together)](#vocabulary-taken-together)
      - [Vocabulary (VR only)](#vocabulary-vr-only)
      - [Vocabulary (VS only)](#vocabulary-vs-only)
      - [Listening Comprehension (Taken
        together)](#listening-comprehension-taken-together)
      - [Listening Comprehension (LR
        only)](#listening-comprehension-lr-only)
      - [Listening Comprehension (LS
        only)](#listening-comprehension-ls-only)
      - [Reading Comprehension (Taken
        together)](#reading-comprehension-taken-together)
      - [Reading Comprehension (RR
        only)](#reading-comprehension-rr-only)
      - [Reading Comprehension (RS
        only)](#reading-comprehension-rs-only)

``` r
# Libraries
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.0     ✓ dplyr   0.8.5
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
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
#library(metaforest)

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

df_post %>% arrange(type, AUTYR) %>% filter(str_detect(AUTYR, "Apthorp"))
```

    ##         AUTYR type          ES          EV
    ## 1 Apthorp12_1   LR 0.231252029 0.002414391
    ## 2 Apthorp12_K   LR 0.377183665 0.002583990
    ## 3 Apthorp12_P   LS 0.017617993 0.001431251
    ## 4 Apthorp12_3   RR 0.137701863 0.002299693
    ## 5 Apthorp12_1   VR 0.767929262 0.002427400
    ## 6 Apthorp12_3   VR 0.903274726 0.002817972
    ## 7 Apthorp12_K   VR 0.796399595 0.002846865
    ## 8 Apthorp12_P   VS 0.008084674 0.001551961

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

### Directly entered ES

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
```

### Combining all

``` r
df_append <- 
  bind_rows(df_post, df_prepost, df_es_direct) %>% 
  left_join(
    read_xlsx(data_file, sheet = "StudyChar") %>% 
      drop_na(AUTYR),
    by = "AUTYR"
  ) %>%
  mutate(
    Hours = Hours %>% parse_number(),
    CONT = recode(CONT, "BAU" = "0", "ALT" = "1") %>% as.integer()
  ) %>%
  arrange(type, stdid) %>%
  select(type, stdid, AUTYR, everything())
  
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
  select(type, stdid, ES, EV, everything()) %>% 
  mutate(
    design = case_when(
      RCT == 1 ~ "RCT",
      QED == 1 ~ "QED",
      WSD == 1 ~ "WSD",
      TRUE ~ NA_character_
    ) %>% as_factor(),
    grade = case_when(
      (GradeK + Grade1 + Grade2) > 0 & (Grade3 + Grade4 + Grade5) == 0 ~ "K-2",
      (Grade3 + Grade4 + Grade5) > 0 & (GradeK + Grade1 + Grade2) == 0 ~ "3-5",
      TRUE ~ "Both"
    ) %>% as_factor(),
    grouping = case_when(
      (WholeCl + SmallGr + Indiv) > 1 ~ "Combination",
      WholeCl == 1 ~ "Whole class",
      SmallGr == 1 ~ "Small group",
      Indiv == 1 ~ "Individual",
      TRUE ~ NA_character_
    ) %>% as_factor(),
    type = type %>% as_factor,
    CONT = CONT %>% factor(labels = c("BAU", "ALT"))
  ) %>%
  select(-c(RCT, QED, WSD, WholeCl, SmallGr, Indiv), -starts_with("Grade", ignore.case = F))

df_clean %>% count(CONT)
```

    ## # A tibble: 2 x 2
    ##   CONT      n
    ##   <fct> <int>
    ## 1 BAU      92
    ## 2 ALT      30

``` r
rm(df_post, df_prepost, df_es_direct, df_append, cor_es)

df_clean %>% knitr::kable()
```

| type | stdid       |          ES |        EV | TMULT | TVOC | TSYN | TMOR | TLC | TRC | TPAD | TDD | TTEC | TSTR | CONT | Duration | Hours | design | grade | grouping    |
| :--: | :---------- | ----------: | --------: | ----: | ---: | ---: | ---: | --: | --: | ---: | --: | ---: | ---: | :--- | -------: | ----: | :----- | :---- | :---------- |
|  AS  | Jones1      |   0.3371586 | 0.0025073 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    0 |    0 | BAU  |        1 |    95 | RCT    | 3-5   | Combination |
|  AS  | Jones2      |   0.1152593 | 0.0019759 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    0 |    0 | BAU  |        1 |    95 | RCT    | 3-5   | Combination |
|  AS  | Proc19      |   0.1139215 | 0.0167638 |     1 |    1 |    1 |    1 |   1 |   1 |    0 |   1 |    1 |    0 | BAU  |        1 |    20 | QED    | 3-5   | Small group |
|  LR  | Apth1       |   0.2312520 | 0.0024144 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        1 |    30 | RCT    | K-2   | Whole class |
|  LR  | ApthK       |   0.3771837 | 0.0025840 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        1 |    30 | RCT    | K-2   | Whole class |
|  LR  | Baker       |   0.2651806 | 0.0312088 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | ALT  |        1 |    38 | RCT    | K-2   | Whole class |
|  LR  | Conn183     |   0.0681798 | 0.0135373 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |        1 |    22 | RCT    | 3-5   | Small group |
|  LR  | Conn184     | \-0.0865421 | 0.0178965 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |        1 |    22 | RCT    | 3-5   | Small group |
|  LR  | Coyne10     |   0.3683911 | 0.0357745 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |    18 | QED    | K-2   | Whole class |
|  LR  | Coyne19     |   0.4067562 | 0.0025440 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |        1 |    44 | RCT    | K-2   | Small group |
|  LR  | Jiang1      |   0.7951549 | 0.0554539 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |        1 |    50 | RCT    | K-2   | Whole class |
|  LR  | Jiang2      |   0.1679589 | 0.0657273 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |        1 |    50 | RCT    | K-2   | Whole class |
|  LR  | Jiang3      |   0.5428986 | 0.0708219 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |        1 |    50 | RCT    | 3-5   | Whole class |
|  LR  | JiangK      |   0.6955669 | 0.0484587 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |        1 |    50 | RCT    | K-2   | Whole class |
|  LR  | Silver17aK  |   0.1225492 | 0.0222134 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    1 |    0 | BAU  |        0 |    10 | QED    | K-2   | Combination |
|  LS  | ApthP       |   0.0176180 | 0.0014313 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        1 |    30 | RCT    | K-2   | Whole class |
|  LS  | Baker       |   0.0990000 | 0.0241636 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | ALT  |        1 |    38 | RCT    | K-2   | Whole class |
|  LS  | Conn183     | \-0.2189914 | 0.0653648 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |        1 |    22 | RCT    | 3-5   | Small group |
|  LS  | Conn184     | \-1.6440105 | 0.0492648 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |        1 |    22 | RCT    | 3-5   | Small group |
|  LS  | Nielsen     |   0.5156824 | 0.1941841 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |    18 | QED    | K-2   | Small group |
|  LS  | TongB       |   0.3306846 | 0.1133714 |     0 |    1 |    0 |    0 |   1 |   1 |    1 |   0 |    0 |    0 | BAU  |        1 |   100 | RCT    | K-2   | Combination |
|  LS  | TongG       |   0.1531120 | 0.1388452 |     0 |    1 |    0 |    0 |   1 |   1 |    1 |   0 |    0 |    0 | BAU  |        1 |   100 | RCT    | K-2   | Combination |
|  MR  | Apel1       |   2.4249393 | 0.5364876 |     0 |    0 |    0 |    1 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |    13 | RCT    | K-2   | Small group |
|  MR  | Apel2       |   0.6689962 | 0.1026682 |     0 |    0 |    0 |    1 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |    13 | RCT    | K-2   | Small group |
|  MR  | ApelK       |   0.9939616 | 0.1029765 |     0 |    0 |    0 |    1 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |    13 | RCT    | K-2   | Small group |
|  MR  | Brimo       |   0.5860487 | 0.6119338 |     0 |    0 |    0 |    1 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |    12 | QED    | 3-5   | Small group |
|  RR  | Apth3       |   0.1377019 | 0.0022997 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        1 |    30 | RCT    | 3-5   | Whole class |
|  RR  | Berry       | \-0.3571446 | 1.2659224 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   1 |    0 |    1 | ALT  |        0 |     6 | QED    | 3-5   | Whole class |
|  RR  | Dalt11      |   1.9049363 | 0.0906114 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    1 |    0 | ALT  |        1 |    20 | RCT    | 3-5   | Individual  |
|  RR  | Graham      |   0.1519858 | 0.0156276 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    1 | BAU  |        0 |     9 | RCT    | 3-5   | Whole class |
|  RR  | Silver17a4  |   0.8508342 | 0.0206818 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    1 |    0 | BAU  |        0 |    10 | QED    | 3-5   | Combination |
|  RR  | VadSanHer15 |   0.2178669 | 0.0033594 |     0 |    1 |    0 |    0 |   0 |   1 |    0 |   0 |    0 |    0 | BAU  |        1 |    35 | RCT    | 3-5   | Whole class |
|  RS  | Apel1       |   0.3604871 | 0.1141001 |     0 |    0 |    0 |    1 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |    13 | RCT    | K-2   | Small group |
|  RS  | Apel2       | \-0.0919161 | 0.0850093 |     0 |    0 |    0 |    1 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |    13 | RCT    | K-2   | Small group |
|  RS  | Conn183     | \-0.0203567 | 0.0137530 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |        1 |    22 | RCT    | 3-5   | Small group |
|  RS  | Conn184     | \-0.0222525 | 0.0180729 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |        1 |    22 | RCT    | 3-5   | Small group |
|  RS  | Dalt11      |   1.0331095 | 0.0493694 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    1 |    0 | ALT  |        1 |    20 | RCT    | 3-5   | Individual  |
|  RS  | Daunic      | \-0.2206484 | 0.0877731 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   1 |    0 |    0 | BAU  |        0 |     5 | QED    | K-2   | Small group |
|  RS  | Gersten     |   0.1300000 | 0.0400000 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |        1 |    NA | RCT    | K-2   | Whole class |
|  RS  | Jones1      |   0.0537309 | 0.0018209 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    0 |    0 | BAU  |        1 |    95 | RCT    | 3-5   | Combination |
|  RS  | Jones2      |   0.1929975 | 0.0018416 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    0 |    0 | BAU  |        1 |    95 | RCT    | 3-5   | Combination |
|  RS  | Morris      |   0.2364356 | 0.0312791 |     1 |    1 |    1 |    1 |   0 |   0 |    1 |   0 |    0 |    1 | ALT  |        1 |    70 | RCT    | Both  | Small group |
|  RS  | Proc11      | \-0.0436775 | 0.0172460 |     1 |    1 |    1 |    0 |   1 |   1 |    0 |   0 |    1 |    0 | BAU  |        1 |    27 | QED    | 3-5   | Individual  |
|  RS  | Proc19      |   0.2331418 | 0.0191677 |     1 |    1 |    1 |    1 |   1 |   1 |    0 |   1 |    1 |    0 | BAU  |        1 |    20 | QED    | 3-5   | Small group |
|  RS  | Silver17a4  | \-0.0014823 | 0.0075817 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    1 |    0 | BAU  |        0 |    10 | QED    | 3-5   | Combination |
|  RS  | Silver17b4  | \-0.1127521 | 0.0182102 |     0 |    1 |    0 |    0 |   0 |   1 |    0 |   1 |    1 |    1 | BAU  |        1 |    20 | QED    | 3-5   | Combination |
|  RS  | Simmons     | \-0.0648048 | 0.0056414 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    1 | BAU  |        1 |    27 | RCT    | 3-5   | Whole class |
|  RS  | TongB       |   0.0172740 | 0.0931362 |     0 |    1 |    0 |    0 |   1 |   1 |    1 |   0 |    0 |    0 | BAU  |        1 |   100 | RCT    | K-2   | Combination |
|  RS  | TongG       |   0.2822964 | 0.1091847 |     0 |    1 |    0 |    0 |   1 |   1 |    1 |   0 |    0 |    0 | BAU  |        1 |   100 | RCT    | K-2   | Combination |
|  RS  | VadSanHer15 |   0.0500486 | 0.0033727 |     0 |    1 |    0 |    0 |   0 |   1 |    0 |   0 |    0 |    0 | BAU  |        1 |    35 | RCT    | 3-5   | Whole class |
|  SS  | Conn183     |   0.0700015 | 0.0173574 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |        1 |    22 | RCT    | 3-5   | Small group |
|  SS  | Conn184     | \-0.0217323 | 0.0239139 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |        1 |    22 | RCT    | 3-5   | Small group |
|  VR  | Apth1       |   0.7679293 | 0.0024274 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        1 |    30 | RCT    | K-2   | Whole class |
|  VR  | Apth3       |   0.9032747 | 0.0028180 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        1 |    30 | RCT    | 3-5   | Whole class |
|  VR  | ApthK       |   0.7963996 | 0.0028469 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        1 |    30 | RCT    | K-2   | Whole class |
|  VR  | Arth1       |   0.4018396 | 0.1512635 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |     9 | QED    | K-2   | Whole class |
|  VR  | Arth2       |   1.0824260 | 0.3962804 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |     9 | QED    | K-2   | Whole class |
|  VR  | Arth3       |   0.8095698 | 0.1674121 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |     9 | QED    | 3-5   | Whole class |
|  VR  | ArthK       |   0.0786293 | 0.2387246 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |     9 | QED    | K-2   | Whole class |
|  VR  | Aug         |   2.1695243 | 0.0192149 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |        1 |    25 | WSD    | K-2   | Whole class |
|  VR  | Baker       |   1.0516546 | 0.0525750 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | ALT  |        1 |    38 | RCT    | K-2   | Whole class |
|  VR  | Berry       |   0.8616891 | 2.8024506 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   1 |    0 |    1 | ALT  |        0 |     6 | QED    | 3-5   | Whole class |
|  VR  | Conn19      | \-0.1659631 | 0.0143786 |     1 |    1 |    0 |    1 |   0 |   0 |    0 |   0 |    1 |    1 | BAU  |        0 |     4 | RCT    | 3-5   | Individual  |
|  VR  | Coyne10     |   6.9739699 | 0.4738991 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |    18 | QED    | K-2   | Whole class |
|  VR  | Coyne19     |   8.0164868 | 0.1170897 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |        1 |    44 | RCT    | K-2   | Small group |
|  VR  | Dalt11      |   1.2606480 | 0.0501652 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    1 |    0 | ALT  |        1 |    20 | RCT    | 3-5   | Individual  |
|  VR  | Fillippini  |   0.6088208 | 0.0802535 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |        0 |     7 | RCT    | K-2   | Small group |
|  VR  | Goldstein   |   2.8023543 | 0.1218541 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    1 |    1 | ALT  |        1 |    72 | RCT    | Both  | Individual  |
|  VR  | Graham      |   1.6509504 | 0.0373717 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    1 | BAU  |        0 |     9 | RCT    | 3-5   | Whole class |
|  VR  | Hass        |   1.6710769 | 0.1039257 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |    12 | RCT    | K-2   | Small group |
|  VR  | Jiang1      |   3.3726157 | 0.0798665 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |        1 |    50 | RCT    | K-2   | Whole class |
|  VR  | Jiang2      |   2.2260077 | 0.0525373 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |        1 |    50 | RCT    | K-2   | Whole class |
|  VR  | Jiang3      |   2.5913270 | 0.0562224 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |        1 |    50 | RCT    | 3-5   | Whole class |
|  VR  | JiangK      |   2.9412143 | 0.0592122 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |        1 |    50 | RCT    | K-2   | Whole class |
|  VR  | Jones1      |   0.2283174 | 0.0021398 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    0 |    0 | BAU  |        1 |    95 | RCT    | 3-5   | Combination |
|  VR  | Jones2      |   0.1901639 | 0.0020985 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    0 |    0 | BAU  |        1 |    95 | RCT    | 3-5   | Combination |
|  VR  | Mancilla    |   1.0153708 | 0.1377186 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    0 |    0 | BAU  |        1 |    25 | QED    | 3-5   | Whole class |
|  VR  | McK         |   3.2932945 | 0.2459157 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |        0 |     2 | WSD    | K-2   | Whole class |
|  VR  | Nelson      |   0.5405571 | 0.0580920 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |        1 |    33 | RCT    | K-2   | Small group |
|  VR  | Neuman      |   0.5346265 | 0.0288756 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    1 |    0 | BAU  |        1 |    20 | RCT    | K-2   | Whole class |
|  VR  | Nielsen     |   2.7051948 | 0.5095005 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |    18 | QED    | K-2   | Small group |
|  VR  | Pow         |   0.2253011 | 0.0526278 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |     3 | RCT    | K-2   | Individual  |
|  VR  | Proc11      |   0.9242303 | 0.0256500 |     1 |    1 |    1 |    0 |   1 |   1 |    0 |   0 |    1 |    0 | BAU  |        1 |    27 | QED    | 3-5   | Individual  |
|  VR  | Puhal       |   2.6937576 | 0.5142966 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |        0 |     5 | RCT    | K-2   | Combination |
|  VR  | Pullen      |   0.4786691 | 0.0500405 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |        0 |     2 | QED    | K-2   | Combination |
|  VR  | Silver17a4  |   0.4220216 | 0.0079434 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    1 |    0 | BAU  |        0 |    10 | QED    | 3-5   | Combination |
|  VR  | Silver17aK  |   0.3868128 | 0.0091241 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    1 |    0 | BAU  |        0 |    10 | QED    | K-2   | Combination |
|  VR  | Silver17b4  |   0.8424442 | 0.0222265 |     0 |    1 |    0 |    0 |   0 |   1 |    0 |   1 |    1 |    1 | BAU  |        1 |    20 | QED    | 3-5   | Combination |
|  VR  | Silver17bK  |   1.4421285 | 0.0345476 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   1 |    1 |    1 | BAU  |        1 |    20 | QED    | K-2   | Combination |
|  VR  | Simmons     |   7.0009329 | 0.1391228 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    1 | BAU  |        1 |    27 | RCT    | 3-5   | Whole class |
|  VR  | VadSan15    |   0.2613394 | 0.1065636 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |        0 |     1 | RCT    | K-2   | Individual  |
|  VR  | VadSan16    |   0.3089192 | 0.0572620 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |        0 |    14 | RCT    | K-2   | Individual  |
|  VR  | VadSanHer15 |   1.2747785 | 0.0053252 |     0 |    1 |    0 |    0 |   0 |   1 |    0 |   0 |    0 |    0 | BAU  |        1 |    35 | RCT    | 3-5   | Whole class |
|  VR  | VadSanNel15 |   1.2277100 | 0.0464508 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |        1 |    40 | RCT    | K-2   | Small group |
|  VR  | Wood18      |   0.2654639 | 0.0236715 |     1 |    1 |    0 |    1 |   0 |   0 |    0 |   0 |    1 |    0 | ALT  |        1 |    30 | RCT    | K-2   | Whole class |
|  VR  | Wright      |   1.5070059 | 0.0483573 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   1 |    0 |    0 | BAU  |        0 |    15 | QED    | K-2   | Whole class |
|  VR  | Zipoli      |   0.8411806 | 0.0204632 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |        0 |    18 | WSD    | K-2   | Combination |
|  VS  | ApthP       |   0.0080847 | 0.0015520 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        1 |    30 | RCT    | K-2   | Whole class |
|  VS  | Conn183     | \-0.0164879 | 0.0629945 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |        1 |    22 | RCT    | 3-5   | Small group |
|  VS  | Conn184     |   0.1197656 | 0.0951981 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |        1 |    22 | RCT    | 3-5   | Small group |
|  VS  | Coyne10     |   0.1061789 | 0.0361622 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |    18 | QED    | K-2   | Whole class |
|  VS  | Coyne19     |   0.1034697 | 0.0049060 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |        1 |    44 | RCT    | K-2   | Small group |
|  VS  | Dalt11      |   0.1024531 | 0.0419712 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    1 |    0 | ALT  |        1 |    20 | RCT    | 3-5   | Individual  |
|  VS  | Daunic      |   0.0580749 | 0.0898036 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   1 |    0 |    0 | BAU  |        0 |     5 | QED    | K-2   | Small group |
|  VS  | Gersten     |   0.3300000 | 0.0500000 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |        1 |    NA | RCT    | K-2   | Whole class |
|  VS  | Hass        |   0.4835901 | 0.0534860 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |    12 | RCT    | K-2   | Small group |
|  VS  | Huang       |   0.4786637 | 0.1040172 |     1 |    1 |    1 |    0 |   0 |   0 |    0 |   0 |    1 |    0 | ALT  |        1 |    80 | QED    | K-2   | Whole class |
|  VS  | Jayanthi    | \-0.0430000 | 0.0010000 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    1 | BAU  |        1 |    NA | RCT    | K-2   | Whole class |
|  VS  | Nelson      | \-0.4798887 | 0.0317414 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |        1 |    33 | RCT    | K-2   | Small group |
|  VS  | Neuman      | \-0.0712942 | 0.0256859 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    1 |    0 | BAU  |        1 |    20 | RCT    | K-2   | Whole class |
|  VS  | Nielsen     |   0.1435089 | 0.2058622 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |        0 |    18 | QED    | K-2   | Small group |
|  VS  | Proc11      |   0.0343903 | 0.0171422 |     1 |    1 |    1 |    0 |   1 |   1 |    0 |   0 |    1 |    0 | BAU  |        1 |    27 | QED    | 3-5   | Individual  |
|  VS  | Silver17aK  | \-0.0393292 | 0.0079704 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    1 |    0 | BAU  |        0 |    10 | QED    | K-2   | Combination |
|  VS  | Silver17b4  |   0.1398774 | 0.0182383 |     0 |    1 |    0 |    0 |   0 |   1 |    0 |   1 |    1 |    1 | BAU  |        1 |    20 | QED    | 3-5   | Combination |
|  VS  | Silver17bK  | \-0.1159971 | 0.0215981 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   1 |    1 |    1 | BAU  |        1 |    20 | QED    | K-2   | Combination |
|  VS  | Simmons     | \-0.0106594 | 0.0054480 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    1 | BAU  |        1 |    27 | RCT    | 3-5   | Whole class |
|  VS  | TongB       |   0.6892086 | 0.1115444 |     0 |    1 |    0 |    0 |   1 |   1 |    1 |   0 |    0 |    0 | BAU  |        1 |   100 | RCT    | K-2   | Combination |
|  VS  | TongG       |   0.3193906 | 0.1387398 |     0 |    1 |    0 |    0 |   1 |   1 |    1 |   0 |    0 |    0 | BAU  |        1 |   100 | RCT    | K-2   | Combination |
|  VS  | VadSan16    |   0.2884231 | 0.0438190 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |        0 |    14 | RCT    | K-2   | Individual  |
|  VS  | VadSanHer15 |   0.0648028 | 0.0033742 |     0 |    1 |    0 |    0 |   0 |   1 |    0 |   0 |    0 |    0 | BAU  |        1 |    35 | RCT    | 3-5   | Whole class |
|  VS  | VadSanNel15 |   0.0111751 | 0.0142347 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |        1 |    40 | RCT    | K-2   | Small group |
|  VS  | Wood18      |   0.1214477 | 0.0151603 |     1 |    1 |    0 |    1 |   0 |   0 |    0 |   0 |    1 |    0 | ALT  |        1 |    30 | RCT    | K-2   | Whole class |

### Summary stats

``` r
df_clean %>% count(stdid)
```

    ## # A tibble: 60 x 2
    ##    stdid     n
    ##    <chr> <int>
    ##  1 Apel1     2
    ##  2 Apel2     2
    ##  3 ApelK     1
    ##  4 Apth1     2
    ##  5 Apth3     2
    ##  6 ApthK     2
    ##  7 ApthP     2
    ##  8 Arth1     1
    ##  9 Arth2     1
    ## 10 Arth3     1
    ## # … with 50 more rows

``` r
#df_clean %>% arrange(type, ES) %>% writexl::write_xlsx("df_clean.xlsx")
#df_clean %>% select(stdid, type) %>% arrange(stdid, type) %>% writexl::write_xlsx("temp.xlsx")
df_clean %>% count(type, stdid)
```

    ## # A tibble: 122 x 3
    ##    type  stdid       n
    ##    <fct> <chr>   <int>
    ##  1 AS    Jones1      1
    ##  2 AS    Jones2      1
    ##  3 AS    Proc19      1
    ##  4 LR    Apth1       1
    ##  5 LR    ApthK       1
    ##  6 LR    Baker       1
    ##  7 LR    Conn183     1
    ##  8 LR    Conn184     1
    ##  9 LR    Coyne10     1
    ## 10 LR    Coyne19     1
    ## # … with 112 more rows

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
    ## Random-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 2.3508 (SE = 0.4153)
    ## tau (square root of estimated tau^2 value):      1.5332
    ## I^2 (total heterogeneity / total variability):   99.49%
    ## H^2 (total variability / sampling variability):  197.63
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 69) = 2607.4754, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub 
    ##   1.0322  0.1869  5.5218  <.0001  0.6658  1.3986  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_2
```

    ## 
    ## Random-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 3.0285 (SE = 0.6710)
    ## tau (square root of estimated tau^2 value):      1.7403
    ## I^2 (total heterogeneity / total variability):   99.50%
    ## H^2 (total variability / sampling variability):  199.74
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 44) = 1783.1225, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub 
    ##   1.5668  0.2649  5.9159  <.0001  1.0477  2.0859  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_3
```

    ## 
    ## Random-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0014 (SE = 0.0025)
    ## tau (square root of estimated tau^2 value):      0.0374
    ## I^2 (total heterogeneity / total variability):   12.56%
    ## H^2 (total variability / sampling variability):  1.14
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 24) = 31.8504, p-val = 0.1307
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.0243  0.0230  1.0564  0.2908  -0.0208  0.0693    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>% count(type)
```

    ## # A tibble: 2 x 2
    ##   type      n
    ##   <fct> <int>
    ## 1 VR       45
    ## 2 VS       25

``` r
forest(
  model_1,
  xlab = "Vocabulary",
  addcred = T, 
  header = T,
  ylim = c(-1, 85),
  rows = c(3:27, 34:78),
  pch = 21,
  bg = "grey",
  lwd = 1.5
)
op <- par(cex = 0.75, font = 4)
text(-16, c(29, 80), pos = 4, c("Standardized Measure", "Custom Measure"))
addpoly(model_2, row = 32, cex = 1.25, col = "white", lwd = 3)
addpoly(model_3, row = 1.5, cex = 1.25, col = "white", lwd = 3)
```

<img src="analysis_files/figure-gfm/unnamed-chunk-9-1.png" width="100%" height="100%" />

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
    ## Random-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.1999 (SE = 0.0798)
    ## tau (square root of estimated tau^2 value):      0.4471
    ## I^2 (total heterogeneity / total variability):   95.40%
    ## H^2 (total variability / sampling variability):  21.73
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 18) = 145.5857, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.1615  0.1129  1.4306  0.1525  -0.0598  0.3828    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_2
```

    ## 
    ## Random-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0253 (SE = 0.0185)
    ## tau (square root of estimated tau^2 value):      0.1590
    ## I^2 (total heterogeneity / total variability):   72.91%
    ## H^2 (total variability / sampling variability):  3.69
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 11) = 31.2272, p-val = 0.0010
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub 
    ##   0.2944  0.0621  4.7397  <.0001  0.1726  0.4161  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_3
```

    ## 
    ## Random-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.4540 (SE = 0.3054)
    ## tau (square root of estimated tau^2 value):      0.6738
    ## I^2 (total heterogeneity / total variability):   93.20%
    ## H^2 (total variability / sampling variability):  14.70
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 6) = 58.4876, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval    ci.lb   ci.ub 
    ##  -0.1369  0.2753  -0.4972  0.6191  -0.6766  0.4028    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>% count(type)
```

    ## # A tibble: 2 x 2
    ##   type      n
    ##   <fct> <int>
    ## 1 LR       12
    ## 2 LS        7

``` r
forest(
  model_1,
  xlab = "Listening Comprehension",
  addcred = T, 
  header = T,
  ylim = c(-1, 33),
  rows = c(3:9, 16:27),
  pch = 21,
  bg = "grey",
  lwd = 1.5
)
op <- par(cex = 0.75, font = 4)
text(-6.3, c(11, 29), pos = 4, c("Standardized Measure", "Custom Measure"))
addpoly(model_2, row = 14, cex = 1.25, col = "white", lwd = 3)
addpoly(model_3, row = 1.5, cex = 1.25, col = "white", lwd = 3)
```

<img src="analysis_files/figure-gfm/unnamed-chunk-11-1.png" width="100%" height="100%" />

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
    ## Random-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.1127 (SE = 0.0419)
    ## tau (square root of estimated tau^2 value):      0.3357
    ## I^2 (total heterogeneity / total variability):   92.79%
    ## H^2 (total variability / sampling variability):  13.87
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 23) = 106.7166, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub 
    ##   0.1979  0.0783  2.5268  0.0115  0.0444  0.3513  * 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_2
```

    ## 
    ## Random-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.4488 (SE = 0.3304)
    ## tau (square root of estimated tau^2 value):      0.6699
    ## I^2 (total heterogeneity / total variability):   97.92%
    ## H^2 (total variability / sampling variability):  48.11
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 5) = 54.0068, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.5606  0.2995  1.8718  0.0612  -0.0264  1.1476  . 
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
    ##   <fct> <int>
    ## 1 RR        6
    ## 2 RS       18

``` r
forest(
  model_1,
  xlab = "Reading Comprehension",
  addcred = T, 
  header = T,
  ylim = c(-1, 38),
  rows = c(3:20, 27:32),
  pch = 21,
  bg = "grey",
  lwd = 1.5
)
op <- par(cex = 0.75, font = 4)
text(-8.7, c(22, 34), pos = 4, c("Standardized Measure", "Custom Measure"))
addpoly(model_2, row = 24, cex = 1.25, col = "white", lwd = 3)
addpoly(model_3, row = 1, cex = 1.25, col = "white", lwd = 3)
```

<img src="analysis_files/figure-gfm/unnamed-chunk-13-1.png" width="100%" height="100%" />

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

<img src="analysis_files/figure-gfm/unnamed-chunk-14-1.png" width="672" />

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

<img src="analysis_files/figure-gfm/unnamed-chunk-15-1.png" width="672" />

### Academic Learning

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

<img src="analysis_files/figure-gfm/unnamed-chunk-16-1.png" width="672" />

## Moderator effects

### Vocabulary (Taken together)

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
    ## Mixed-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     1.8709 (SE = 0.3355)
    ## tau (square root of estimated tau^2 value):             1.3678
    ## I^2 (residual heterogeneity / unaccounted variability): 99.33%
    ## H^2 (unaccounted variability / sampling variability):   148.17
    ## R^2 (amount of heterogeneity accounted for):            20.42%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 68) = 1814.9729, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 17.2117, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.1104  0.2770  0.3985  0.6902  -0.4325  0.6532      
    ## typeVR     1.4426  0.3477  4.1487  <.0001   0.7611  2.1241  *** 
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
    mods = ~ CONT
  )
```

    ## 
    ## Mixed-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.3795 (SE = 0.4233)
    ## tau (square root of estimated tau^2 value):             1.5426
    ## I^2 (residual heterogeneity / unaccounted variability): 99.50%
    ## H^2 (unaccounted variability / sampling variability):   198.42
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 68) = 2580.4295, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.2393, p-val = 0.6247
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.9690  0.2286  4.2391  <.0001   0.5210  1.4170  *** 
    ## CONTALT    0.1967  0.4020  0.4892  0.6247  -0.5913  0.9846      
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
    mods = ~ Duration
  )
```

    ## 
    ## Mixed-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.3864 (SE = 0.4245)
    ## tau (square root of estimated tau^2 value):             1.5448
    ## I^2 (residual heterogeneity / unaccounted variability): 99.50%
    ## H^2 (unaccounted variability / sampling variability):   199.29
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 68) = 2605.7855, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0319, p-val = 0.8582
    ## 
    ## Model Results:
    ## 
    ##           estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt     0.9890  0.3085  3.2058  0.0013   0.3843  1.5936  ** 
    ## Duration    0.0696  0.3895  0.1787  0.8582  -0.6937  0.8329     
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
    ## Mixed-Effects Model (k = 68; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.4281 (SE = 0.4386)
    ## tau (square root of estimated tau^2 value):             1.5582
    ## I^2 (residual heterogeneity / unaccounted variability): 99.40%
    ## H^2 (unaccounted variability / sampling variability):   166.18
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 66) = 2364.2352, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.3363, p-val = 0.5620
    ## 
    ## Model Results:
    ## 
    ##               estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt         1.0585  0.1928  5.4914  <.0001   0.6807  1.4363  *** 
    ## scale(Hours)    0.1127  0.1944  0.5799  0.5620  -0.2682  0.4936      
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
    mods = ~ design
  )
```

    ## 
    ## Mixed-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.3635 (SE = 0.4237)
    ## tau (square root of estimated tau^2 value):             1.5374
    ## I^2 (residual heterogeneity / unaccounted variability): 99.50%
    ## H^2 (unaccounted variability / sampling variability):   198.20
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 67) = 2448.8901, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 2:3):
    ## QM(df = 2) = 1.6137, p-val = 0.4463
    ## 
    ## Model Results:
    ## 
    ##            estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt      1.0535  0.2345   4.4925  <.0001   0.5939  1.5131  *** 
    ## designQED   -0.2030  0.4067  -0.4991  0.6177  -1.0000  0.5941      
    ## designWSD    1.0125  0.9344   1.0836  0.2785  -0.8188  2.8439      
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
    mods = ~ grade
  )
```

    ## 
    ## Mixed-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.3738 (SE = 0.4255)
    ## tau (square root of estimated tau^2 value):             1.5407
    ## I^2 (residual heterogeneity / unaccounted variability): 99.48%
    ## H^2 (unaccounted variability / sampling variability):   191.71
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 67) = 2545.8243, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 2:3):
    ## QM(df = 2) = 1.4022, p-val = 0.4960
    ## 
    ## Model Results:
    ## 
    ##            estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt      0.9076  0.3355  2.7051  0.0068   0.2500  1.5652  ** 
    ## gradeK-2     0.1459  0.4062  0.3592  0.7195  -0.6503  0.9421     
    ## gradeBoth    1.8948  1.6150  1.1732  0.2407  -1.2706  5.0601     
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
    mods = ~ grouping
  )
```

    ## 
    ## Mixed-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.3402 (SE = 0.4230)
    ## tau (square root of estimated tau^2 value):             1.5298
    ## I^2 (residual heterogeneity / unaccounted variability): 99.46%
    ## H^2 (unaccounted variability / sampling variability):   185.29
    ## R^2 (amount of heterogeneity accounted for):            0.45%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 66) = 2580.4672, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 2:4):
    ## QM(df = 3) = 3.4026, p-val = 0.3336
    ## 
    ## Model Results:
    ## 
    ##                      estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt                0.5819  0.4143  1.4046  0.1601  -0.2301  1.3939    
    ## groupingSmall group    0.4739  0.5881  0.8057  0.4204  -0.6789  1.6266    
    ## groupingWhole class    0.7815  0.4986  1.5675  0.1170  -0.1957  1.7587    
    ## groupingIndividual     0.0149  0.6410  0.0233  0.9814  -1.2415  1.2713    
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
    mods = ~ TMULT
  )
```

    ## 
    ## Mixed-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.3876 (SE = 0.4247)
    ## tau (square root of estimated tau^2 value):             1.5452
    ## I^2 (residual heterogeneity / unaccounted variability): 99.50%
    ## H^2 (unaccounted variability / sampling variability):   201.19
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 68) = 2578.4358, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0193, p-val = 0.8894
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.0195  0.2106  4.8417  <.0001   0.6068  1.4323  *** 
    ## TMULT      0.0655  0.4709  0.1391  0.8894  -0.8574  0.9884      
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
    mods = ~ TVOC
  )
```

    ## 
    ## Random-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 2.3508 (SE = 0.4153)
    ## tau (square root of estimated tau^2 value):      1.5332
    ## I^2 (total heterogeneity / total variability):   99.49%
    ## H^2 (total variability / sampling variability):  197.63
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 69) = 2607.4754, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub 
    ##   1.0322  0.1869  5.5218  <.0001  0.6658  1.3986  *** 
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
    mods = ~ TSYN
  )
```

    ## 
    ## Mixed-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.3601 (SE = 0.4199)
    ## tau (square root of estimated tau^2 value):             1.5363
    ## I^2 (residual heterogeneity / unaccounted variability): 99.50%
    ## H^2 (unaccounted variability / sampling variability):   199.76
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 68) = 2469.4882, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.6087, p-val = 0.4353
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.9696  0.2038  4.7567  <.0001   0.5701  1.3691  *** 
    ## TSYN       0.4030  0.5166  0.7802  0.4353  -0.6094  1.4155      
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
    mods = ~ TMOR
  )
```

    ## 
    ## Mixed-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.3455 (SE = 0.4176)
    ## tau (square root of estimated tau^2 value):             1.5315
    ## I^2 (residual heterogeneity / unaccounted variability): 99.49%
    ## H^2 (unaccounted variability / sampling variability):   197.84
    ## R^2 (amount of heterogeneity accounted for):            0.23%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 68) = 2568.7086, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.0838, p-val = 0.2978
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.9666  0.1971  4.9046  <.0001   0.5803  1.3528  *** 
    ## TMOR       0.6418  0.6165  1.0411  0.2978  -0.5665  1.8500      
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
    mods = ~ TLC
  )
```

    ## 
    ## Mixed-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.3890 (SE = 0.4249)
    ## tau (square root of estimated tau^2 value):             1.5456
    ## I^2 (residual heterogeneity / unaccounted variability): 99.50%
    ## H^2 (unaccounted variability / sampling variability):   201.86
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 68) = 2499.8257, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0012, p-val = 0.9720
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.0372  0.2282   4.5444  <.0001   0.5898  1.4845  *** 
    ## TLC       -0.0142  0.4044  -0.0351  0.9720  -0.8067  0.7783      
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
    mods = ~ TRC
  )
```

    ## 
    ## Mixed-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.3295 (SE = 0.4148)
    ## tau (square root of estimated tau^2 value):             1.5263
    ## I^2 (residual heterogeneity / unaccounted variability): 99.48%
    ## H^2 (unaccounted variability / sampling variability):   191.35
    ## R^2 (amount of heterogeneity accounted for):            0.91%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 68) = 2598.2999, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.7201, p-val = 0.1897
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.1328  0.2014   5.6257  <.0001   0.7381  1.5274  *** 
    ## TRC       -0.6920  0.5276  -1.3115  0.1897  -1.7262  0.3421      
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
    ## Mixed-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.3152 (SE = 0.4123)
    ## tau (square root of estimated tau^2 value):             1.5216
    ## I^2 (residual heterogeneity / unaccounted variability): 99.49%
    ## H^2 (unaccounted variability / sampling variability):   195.80
    ## R^2 (amount of heterogeneity accounted for):            1.51%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 68) = 2602.8465, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 2.1105, p-val = 0.1463
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.1426  0.2006   5.6953  <.0001   0.7494  1.5358  *** 
    ## TPAD      -0.7668  0.5278  -1.4528  0.1463  -1.8013  0.2677      
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
    ## Mixed-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.3283 (SE = 0.4146)
    ## tau (square root of estimated tau^2 value):             1.5259
    ## I^2 (residual heterogeneity / unaccounted variability): 99.46%
    ## H^2 (unaccounted variability / sampling variability):   186.42
    ## R^2 (amount of heterogeneity accounted for):            0.96%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 68) = 2582.8290, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.7157, p-val = 0.1902
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.1472  0.2058   5.5738  <.0001   0.7438  1.5506  *** 
    ## TDD       -0.6307  0.4815  -1.3098  0.1902  -1.5745  0.3130      
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
    ## Mixed-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.3852 (SE = 0.4244)
    ## tau (square root of estimated tau^2 value):             1.5444
    ## I^2 (residual heterogeneity / unaccounted variability): 99.50%
    ## H^2 (unaccounted variability / sampling variability):   198.57
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 68) = 2598.9961, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.1629, p-val = 0.6865
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.0850  0.2286   4.7462  <.0001   0.6369  1.5330  *** 
    ## TTEC      -0.1626  0.4030  -0.4036  0.6865  -0.9525  0.6272      
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
    mods = ~ TSTR
  )
```

    ## 
    ## Mixed-Effects Model (k = 70; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.3747 (SE = 0.4225)
    ## tau (square root of estimated tau^2 value):             1.5410
    ## I^2 (residual heterogeneity / unaccounted variability): 99.45%
    ## H^2 (unaccounted variability / sampling variability):   180.91
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 68) = 2453.3431, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.3618, p-val = 0.5475
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.9845  0.2041  4.8234  <.0001   0.5844  1.3845  *** 
    ## TSTR       0.3140  0.5220  0.6015  0.5475  -0.7091  1.3370      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Vocabulary (VR only)

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ CONT
  )
```

    ## 
    ## Mixed-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     3.0969 (SE = 0.6937)
    ## tau (square root of estimated tau^2 value):             1.7598
    ## I^2 (residual heterogeneity / unaccounted variability): 99.51%
    ## H^2 (unaccounted variability / sampling variability):   204.67
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 1688.7735, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0768, p-val = 0.7817
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.5131  0.3317  4.5619  <.0001   0.8630  2.1632  *** 
    ## CONTALT    0.1557  0.5619  0.2771  0.7817  -0.9456  1.2569      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ Duration
  )
```

    ## 
    ## Mixed-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     3.0046 (SE = 0.6736)
    ## tau (square root of estimated tau^2 value):             1.7334
    ## I^2 (residual heterogeneity / unaccounted variability): 99.49%
    ## H^2 (unaccounted variability / sampling variability):   194.60
    ## R^2 (amount of heterogeneity accounted for):            0.79%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 1767.5599, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.2150, p-val = 0.2703
    ## 
    ## Model Results:
    ## 
    ##           estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt     1.2468  0.3922  3.1788  0.0015   0.4780  2.0155  ** 
    ## Duration    0.5843  0.5301  1.1023  0.2703  -0.4546  1.6232     
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ scale(Hours)
  )
```

    ## 
    ## Mixed-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     3.0362 (SE = 0.6807)
    ## tau (square root of estimated tau^2 value):             1.7425
    ## I^2 (residual heterogeneity / unaccounted variability): 99.44%
    ## H^2 (unaccounted variability / sampling variability):   179.61
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 1594.2441, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.9106, p-val = 0.3400
    ## 
    ## Model Results:
    ## 
    ##               estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt         1.5618  0.2652  5.8884  <.0001   1.0419  2.0816  *** 
    ## scale(Hours)    0.2543  0.2665  0.9542  0.3400  -0.2680  0.7766      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ design
  )
```

    ## 
    ## Mixed-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     3.1302 (SE = 0.7092)
    ## tau (square root of estimated tau^2 value):             1.7692
    ## I^2 (residual heterogeneity / unaccounted variability): 99.51%
    ## H^2 (unaccounted variability / sampling variability):   203.35
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 42) = 1701.4471, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 2:3):
    ## QM(df = 2) = 0.6383, p-val = 0.7268
    ## 
    ## Model Results:
    ## 
    ##            estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt      1.6557  0.3441   4.8117  <.0001   0.9813  2.3301  *** 
    ## designQED   -0.3626  0.5866  -0.6181  0.5365  -1.5122  0.7871      
    ## designWSD    0.4185  1.0920   0.3833  0.7015  -1.7217  2.5587      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ grade
  )
```

    ## 
    ## Mixed-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     3.1222 (SE = 0.7075)
    ## tau (square root of estimated tau^2 value):             1.7670
    ## I^2 (residual heterogeneity / unaccounted variability): 99.48%
    ## H^2 (unaccounted variability / sampling variability):   193.99
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 42) = 1642.0793, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 2:3):
    ## QM(df = 2) = 0.8114, p-val = 0.6665
    ## 
    ## Model Results:
    ## 
    ##            estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt      1.3211  0.4669  2.8292  0.0047   0.4059  2.2363  ** 
    ## gradeK-2     0.3302  0.5743  0.5750  0.5653  -0.7953  1.4558     
    ## gradeBoth    1.4813  1.8607  0.7961  0.4260  -2.1656  5.1281     
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ grouping
  )
```

    ## 
    ## Mixed-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.8478 (SE = 0.6554)
    ## tau (square root of estimated tau^2 value):             1.6875
    ## I^2 (residual heterogeneity / unaccounted variability): 99.42%
    ## H^2 (unaccounted variability / sampling variability):   173.21
    ## R^2 (amount of heterogeneity accounted for):            5.97%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 41) = 1305.4188, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 2:4):
    ## QM(df = 3) = 5.5384, p-val = 0.1364
    ## 
    ## Model Results:
    ## 
    ##                      estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt                0.8041  0.5690   1.4132  0.1576  -0.3111  1.9194    
    ## groupingSmall group    1.6366  0.9069   1.8046  0.0711  -0.1409  3.4141  . 
    ## groupingWhole class    1.0823  0.6744   1.6047  0.1086  -0.2396  2.4042    
    ## groupingIndividual    -0.0085  0.8598  -0.0099  0.9921  -1.6937  1.6767    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TMULT
  )
```

    ## 
    ## Mixed-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     3.0899 (SE = 0.6922)
    ## tau (square root of estimated tau^2 value):             1.7578
    ## I^2 (residual heterogeneity / unaccounted variability): 99.51%
    ## H^2 (unaccounted variability / sampling variability):   204.14
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 1769.4336, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.2597, p-val = 0.6103
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.6451  0.3079   5.3424  <.0001   1.0415  2.2486  *** 
    ## TMULT     -0.3165  0.6212  -0.5096  0.6103  -1.5340  0.9009      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TVOC
  )
```

    ## 
    ## Random-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 3.0285 (SE = 0.6710)
    ## tau (square root of estimated tau^2 value):      1.7403
    ## I^2 (total heterogeneity / total variability):   99.50%
    ## H^2 (total variability / sampling variability):  199.74
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 44) = 1783.1225, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub 
    ##   1.5668  0.2649  5.9159  <.0001  1.0477  2.0859  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TSYN
  )
```

    ## 
    ## Mixed-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     3.1024 (SE = 0.6948)
    ## tau (square root of estimated tau^2 value):             1.7614
    ## I^2 (residual heterogeneity / unaccounted variability): 99.52%
    ## H^2 (unaccounted variability / sampling variability):   206.92
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 1627.6835, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0108, p-val = 0.9173
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.5535  0.2995  5.1863  <.0001   0.9664  2.1405  *** 
    ## TSYN       0.0696  0.6702  0.1039  0.9173  -1.2439  1.3832      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TMOR
  )
```

    ## 
    ## Mixed-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     3.0894 (SE = 0.6923)
    ## tau (square root of estimated tau^2 value):             1.7577
    ## I^2 (residual heterogeneity / unaccounted variability): 99.51%
    ## H^2 (unaccounted variability / sampling variability):   204.24
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 1767.9277, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.1931, p-val = 0.6604
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.5205  0.2878  5.2829  <.0001   0.9564  2.0846  *** 
    ## TMOR       0.3420  0.7782  0.4394  0.6604  -1.1834  1.8673      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TLC
  )
```

    ## 
    ## Mixed-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     3.1022 (SE = 0.6947)
    ## tau (square root of estimated tau^2 value):             1.7613
    ## I^2 (residual heterogeneity / unaccounted variability): 99.52%
    ## H^2 (unaccounted variability / sampling variability):   206.39
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 1600.0473, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0056, p-val = 0.9404
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.5542  0.3203  4.8524  <.0001   0.9265  2.1820  *** 
    ## TLC        0.0437  0.5846  0.0748  0.9404  -1.1020  1.1895      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TRC
  )
```

    ## 
    ## Mixed-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     3.0837 (SE = 0.6912)
    ## tau (square root of estimated tau^2 value):             1.7560
    ## I^2 (residual heterogeneity / unaccounted variability): 99.49%
    ## H^2 (unaccounted variability / sampling variability):   196.78
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 1729.3324, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.3176, p-val = 0.5730
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.6082  0.2769   5.8080  <.0001   1.0655  2.1510  *** 
    ## TRC       -0.5939  1.0538  -0.5636  0.5730  -2.6593  1.4715      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TPAD
  )
```

    ## 
    ## Mixed-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.9783 (SE = 0.6681)
    ## tau (square root of estimated tau^2 value):             1.7258
    ## I^2 (residual heterogeneity / unaccounted variability): 99.50%
    ## H^2 (unaccounted variability / sampling variability):   199.34
    ## R^2 (amount of heterogeneity accounted for):            1.66%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 1782.7097, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.7604, p-val = 0.1846
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.6910  0.2790   6.0611  <.0001   1.1442  2.2378  *** 
    ## TPAD      -1.1001  0.8291  -1.3268  0.1846  -2.7251  0.5250      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TDD
  )
```

    ## 
    ## Mixed-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     2.9400 (SE = 0.6597)
    ## tau (square root of estimated tau^2 value):             1.7146
    ## I^2 (residual heterogeneity / unaccounted variability): 99.43%
    ## H^2 (unaccounted variability / sampling variability):   176.37
    ## R^2 (amount of heterogeneity accounted for):            2.92%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 1428.8656, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 2.3299, p-val = 0.1269
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.7625  0.2911   6.0555  <.0001   1.1920  2.3330  *** 
    ## TDD       -1.0052  0.6585  -1.5264  0.1269  -2.2959  0.2855      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TTEC
  )
```

    ## 
    ## Mixed-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     3.0948 (SE = 0.6934)
    ## tau (square root of estimated tau^2 value):             1.7592
    ## I^2 (residual heterogeneity / unaccounted variability): 99.50%
    ## H^2 (unaccounted variability / sampling variability):   200.00
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 1781.3381, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.1654, p-val = 0.6842
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.6419  0.3245   5.0601  <.0001   1.0059  2.2779  *** 
    ## TTEC      -0.2334  0.5739  -0.4067  0.6842  -1.3581  0.8914      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TSTR
  )
```

    ## 
    ## Mixed-Effects Model (k = 45; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     3.0384 (SE = 0.6809)
    ## tau (square root of estimated tau^2 value):             1.7431
    ## I^2 (residual heterogeneity / unaccounted variability): 99.50%
    ## H^2 (unaccounted variability / sampling variability):   200.67
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 1768.4970, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.7533, p-val = 0.3854
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    1.4708  0.2874  5.1171  <.0001   0.9075  2.0342  *** 
    ## TSTR       0.6479  0.7464  0.8679  0.3854  -0.8152  2.1109      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Vocabulary (VS only)

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ CONT
  )
```

    ## 
    ## Mixed-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0008 (SE = 0.0021)
    ## tau (square root of estimated tau^2 value):             0.0278
    ## I^2 (residual heterogeneity / unaccounted variability): 7.10%
    ## H^2 (unaccounted variability / sampling variability):   1.08
    ## R^2 (amount of heterogeneity accounted for):            44.73%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 23) = 30.3362, p-val = 0.1400
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.9413, p-val = 0.3319
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0105  0.0234  0.4469  0.6550  -0.0354  0.0564    
    ## CONTALT    0.0540  0.0557  0.9702  0.3319  -0.0551  0.1632    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ Duration
  )
```

    ## 
    ## Mixed-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0016 (SE = 0.0027)
    ## tau (square root of estimated tau^2 value):             0.0398
    ## I^2 (residual heterogeneity / unaccounted variability): 13.92%
    ## H^2 (unaccounted variability / sampling variability):   1.16
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 23) = 31.0406, p-val = 0.1218
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.6474, p-val = 0.4210
    ## 
    ## Model Results:
    ## 
    ##           estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt     0.0810  0.0731   1.1082  0.2678  -0.0623  0.2243    
    ## Duration   -0.0621  0.0772  -0.8046  0.4210  -0.2134  0.0892    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ scale(Hours)
  )
```

    ## 
    ## Mixed-Effects Model (k = 23; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0 (SE = 0.0023)
    ## tau (square root of estimated tau^2 value):             0
    ## I^2 (residual heterogeneity / unaccounted variability): 0.00%
    ## H^2 (unaccounted variability / sampling variability):   1.00
    ## R^2 (amount of heterogeneity accounted for):            100.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 21) = 22.4240, p-val = 0.3754
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 3.2901, p-val = 0.0697
    ## 
    ## Model Results:
    ## 
    ##               estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt         0.0439  0.0229  1.9129  0.0558  -0.0011  0.0889  . 
    ## scale(Hours)    0.0941  0.0519  1.8139  0.0697  -0.0076  0.1959  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ design
  )
```

    ## 
    ## Mixed-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0017 (SE = 0.0028)
    ## tau (square root of estimated tau^2 value):             0.0409
    ## I^2 (residual heterogeneity / unaccounted variability): 14.51%
    ## H^2 (unaccounted variability / sampling variability):   1.17
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 23) = 31.7848, p-val = 0.1047
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0018, p-val = 0.9658
    ## 
    ## Model Results:
    ## 
    ##            estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt      0.0253  0.0259  0.9759  0.3291  -0.0255  0.0761    
    ## designQED    0.0027  0.0632  0.0429  0.9658  -0.1211  0.1265    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ grade
  )
```

    ## 
    ## Mixed-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0012 (SE = 0.0026)
    ## tau (square root of estimated tau^2 value):             0.0350
    ## I^2 (residual heterogeneity / unaccounted variability): 10.38%
    ## H^2 (unaccounted variability / sampling variability):   1.12
    ## R^2 (amount of heterogeneity accounted for):            12.32%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 23) = 30.7940, p-val = 0.1279
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.4247, p-val = 0.5146
    ## 
    ## Model Results:
    ## 
    ##           estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt     0.0477  0.0438   1.0898  0.2758  -0.0381  0.1335    
    ## gradeK-2   -0.0333  0.0511  -0.6517  0.5146  -0.1334  0.0668    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ grouping
  )
```

    ## 
    ## Mixed-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0013 (SE = 0.0027)
    ## tau (square root of estimated tau^2 value):             0.0366
    ## I^2 (residual heterogeneity / unaccounted variability): 11.66%
    ## H^2 (unaccounted variability / sampling variability):   1.13
    ## R^2 (amount of heterogeneity accounted for):            4.20%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 21) = 30.0347, p-val = 0.0913
    ## 
    ## Test of Moderators (coefficients 2:4):
    ## QM(df = 3) = 1.0072, p-val = 0.7995
    ## 
    ## Model Results:
    ## 
    ##                      estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt                0.0287  0.0677   0.4232  0.6721  -0.1041  0.1614    
    ## groupingSmall group    0.0156  0.0881   0.1767  0.8598  -0.1572  0.1883    
    ## groupingWhole class   -0.0169  0.0732  -0.2306  0.8176  -0.1604  0.1267    
    ## groupingIndividual     0.0782  0.1211   0.6458  0.5184  -0.1591  0.3154    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TMULT
  )
```

    ## 
    ## Mixed-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0012 (SE = 0.0024)
    ## tau (square root of estimated tau^2 value):             0.0341
    ## I^2 (residual heterogeneity / unaccounted variability): 10.75%
    ## H^2 (unaccounted variability / sampling variability):   1.12
    ## R^2 (amount of heterogeneity accounted for):            16.89%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 23) = 30.5073, p-val = 0.1354
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.0305, p-val = 0.3101
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0170  0.0231  0.7341  0.4629  -0.0283  0.0623    
    ## TMULT      0.0936  0.0922  1.0151  0.3101  -0.0872  0.2744    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TVOC
  )
```

    ## 
    ## Random-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0014 (SE = 0.0025)
    ## tau (square root of estimated tau^2 value):      0.0374
    ## I^2 (total heterogeneity / total variability):   12.56%
    ## H^2 (total variability / sampling variability):  1.14
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 24) = 31.8504, p-val = 0.1307
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.0243  0.0230  1.0564  0.2908  -0.0208  0.0693    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TSYN
  )
```

    ## 
    ## Mixed-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0014 (SE = 0.0025)
    ## tau (square root of estimated tau^2 value):             0.0376
    ## I^2 (residual heterogeneity / unaccounted variability): 12.87%
    ## H^2 (unaccounted variability / sampling variability):   1.15
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 23) = 31.3366, p-val = 0.1147
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.3839, p-val = 0.5355
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0217  0.0234  0.9270  0.3539  -0.0242  0.0676    
    ## TSYN       0.0792  0.1278  0.6196  0.5355  -0.1713  0.3296    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TMOR
  )
```

    ## 
    ## Mixed-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0013 (SE = 0.0024)
    ## tau (square root of estimated tau^2 value):             0.0360
    ## I^2 (residual heterogeneity / unaccounted variability): 11.83%
    ## H^2 (unaccounted variability / sampling variability):   1.13
    ## R^2 (amount of heterogeneity accounted for):            7.63%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 23) = 31.0318, p-val = 0.1220
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.6000, p-val = 0.4386
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0205  0.0231  0.8877  0.3747  -0.0248  0.0657    
    ## TMOR       0.1010  0.1303  0.7746  0.4386  -0.1545  0.3564    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TLC
  )
```

    ## 
    ## Mixed-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0013 (SE = 0.0025)
    ## tau (square root of estimated tau^2 value):             0.0360
    ## I^2 (residual heterogeneity / unaccounted variability): 11.89%
    ## H^2 (unaccounted variability / sampling variability):   1.13
    ## R^2 (amount of heterogeneity accounted for):            7.42%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 23) = 31.0145, p-val = 0.1224
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.6016, p-val = 0.4380
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0181  0.0238  0.7603  0.4471  -0.0286  0.0648    
    ## TLC        0.0615  0.0792  0.7756  0.4380  -0.0938  0.2167    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TRC
  )
```

    ## 
    ## Mixed-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0006 (SE = 0.0021)
    ## tau (square root of estimated tau^2 value):             0.0253
    ## I^2 (residual heterogeneity / unaccounted variability): 5.73%
    ## H^2 (unaccounted variability / sampling variability):   1.06
    ## R^2 (amount of heterogeneity accounted for):            54.47%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 23) = 28.9695, p-val = 0.1813
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 2.2619, p-val = 0.1326
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0045  0.0229  0.1971  0.8437  -0.0403  0.0494    
    ## TRC        0.0826  0.0549  1.5039  0.1326  -0.0251  0.1903    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TPAD
  )
```

    ## 
    ## Mixed-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0016 (SE = 0.0027)
    ## tau (square root of estimated tau^2 value):             0.0403
    ## I^2 (residual heterogeneity / unaccounted variability): 14.44%
    ## H^2 (unaccounted variability / sampling variability):   1.17
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 23) = 31.8449, p-val = 0.1034
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0438, p-val = 0.8343
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0269  0.0244   1.1002  0.2713  -0.0210  0.0748    
    ## TPAD      -0.0190  0.0907  -0.2092  0.8343  -0.1968  0.1588    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TDD
  )
```

    ## 
    ## Mixed-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0019 (SE = 0.0029)
    ## tau (square root of estimated tau^2 value):             0.0434
    ## I^2 (residual heterogeneity / unaccounted variability): 16.03%
    ## H^2 (unaccounted variability / sampling variability):   1.19
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 23) = 31.7508, p-val = 0.1054
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.2583, p-val = 0.6113
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0313  0.0257   1.2172  0.2235  -0.0191  0.0816    
    ## TDD       -0.0378  0.0745  -0.5082  0.6113  -0.1838  0.1081    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TTEC
  )
```

    ## 
    ## Mixed-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0017 (SE = 0.0028)
    ## tau (square root of estimated tau^2 value):             0.0409
    ## I^2 (residual heterogeneity / unaccounted variability): 14.37%
    ## H^2 (unaccounted variability / sampling variability):   1.17
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 23) = 31.7204, p-val = 0.1061
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0079, p-val = 0.9291
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0246  0.0266  0.9261  0.3544  -0.0275  0.0768    
    ## TTEC       0.0052  0.0579  0.0890  0.9291  -0.1083  0.1186    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_v %>%
  filter(type == "VS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TSTR
  )
```

    ## 
    ## Mixed-Effects Model (k = 25; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0000 (SE = 0.0021)
    ## tau (square root of estimated tau^2 value):             0.0019
    ## I^2 (residual heterogeneity / unaccounted variability): 0.03%
    ## H^2 (unaccounted variability / sampling variability):   1.00
    ## R^2 (amount of heterogeneity accounted for):            99.76%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 23) = 27.4054, p-val = 0.2391
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 4.4360, p-val = 0.0352
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb    ci.ub 
    ## intrcpt    0.0446  0.0242   1.8466  0.0648  -0.0027   0.0919  . 
    ## TSTR      -0.0778  0.0369  -2.1062  0.0352  -0.1502  -0.0054  * 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Listening Comprehension (Taken together)

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
    ## Mixed-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1486 (SE = 0.0638)
    ## tau (square root of estimated tau^2 value):             0.3855
    ## I^2 (residual heterogeneity / unaccounted variability): 92.19%
    ## H^2 (unaccounted variability / sampling variability):   12.81
    ## R^2 (amount of heterogeneity accounted for):            25.68%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 17) = 89.7148, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 5.0717, p-val = 0.0243
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt   -0.1649  0.1757  -0.9387  0.3479  -0.5093  0.1794    
    ## typeLR     0.4807  0.2134   2.2520  0.0243   0.0623  0.8990  * 
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
    mods = ~ CONT
  )
```

    ## 
    ## Mixed-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.2135 (SE = 0.0874)
    ## tau (square root of estimated tau^2 value):             0.4620
    ## I^2 (residual heterogeneity / unaccounted variability): 94.98%
    ## H^2 (unaccounted variability / sampling variability):   19.92
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 17) = 128.0728, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.1567, p-val = 0.6922
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.1406  0.1277  1.1006  0.2711  -0.1098  0.3910    
    ## CONTALT    0.1212  0.3061  0.3959  0.6922  -0.4787  0.7211    
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
    mods = ~ Duration
  )
```

    ## 
    ## Mixed-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.2121 (SE = 0.0864)
    ## tau (square root of estimated tau^2 value):             0.4605
    ## I^2 (residual heterogeneity / unaccounted variability): 95.77%
    ## H^2 (unaccounted variability / sampling variability):   23.66
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 17) = 145.4587, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.2569, p-val = 0.6123
    ## 
    ## Model Results:
    ## 
    ##           estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt     0.3046  0.3048   0.9993  0.3176  -0.2928  0.9020    
    ## Duration   -0.1670  0.3295  -0.5068  0.6123  -0.8128  0.4788    
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
    ## Mixed-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1865 (SE = 0.0767)
    ## tau (square root of estimated tau^2 value):             0.4318
    ## I^2 (residual heterogeneity / unaccounted variability): 94.98%
    ## H^2 (unaccounted variability / sampling variability):   19.94
    ## R^2 (amount of heterogeneity accounted for):            6.73%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 17) = 120.8494, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.6338, p-val = 0.2012
    ## 
    ## Model Results:
    ## 
    ##               estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt         0.1742  0.1101  1.5822  0.1136  -0.0416  0.3899    
    ## scale(Hours)    0.1598  0.1250  1.2782  0.2012  -0.0852  0.4048    
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
    mods = ~ design
  )
```

    ## 
    ## Mixed-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.2121 (SE = 0.0864)
    ## tau (square root of estimated tau^2 value):             0.4605
    ## I^2 (residual heterogeneity / unaccounted variability): 95.77%
    ## H^2 (unaccounted variability / sampling variability):   23.66
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 17) = 145.4587, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.2569, p-val = 0.6123
    ## 
    ## Model Results:
    ## 
    ##            estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt      0.1376  0.1251  1.0995  0.2716  -0.1077  0.3829    
    ## designQED    0.1670  0.3295  0.5068  0.6123  -0.4788  0.8128    
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
    mods = ~ grade
  )
```

    ## 
    ## Mixed-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1396 (SE = 0.0606)
    ## tau (square root of estimated tau^2 value):             0.3736
    ## I^2 (residual heterogeneity / unaccounted variability): 93.60%
    ## H^2 (unaccounted variability / sampling variability):   15.63
    ## R^2 (amount of heterogeneity accounted for):            30.18%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 17) = 120.6914, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 6.7540, p-val = 0.0094
    ## 
    ## Model Results:
    ## 
    ##           estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    -0.2624  0.1896  -1.3839  0.1664  -0.6341  0.1092     
    ## gradeK-2    0.5741  0.2209   2.5989  0.0094   0.1411  1.0071  ** 
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
    mods = ~ grouping
  )
```

    ## 
    ## Mixed-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1703 (SE = 0.0737)
    ## tau (square root of estimated tau^2 value):             0.4126
    ## I^2 (residual heterogeneity / unaccounted variability): 94.03%
    ## H^2 (unaccounted variability / sampling variability):   16.75
    ## R^2 (amount of heterogeneity accounted for):            14.84%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 16) = 144.9844, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 2:3):
    ## QM(df = 2) = 4.6548, p-val = 0.0975
    ## 
    ## Model Results:
    ## 
    ##                      estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt                0.1922  0.2892   0.6646  0.5063  -0.3746  0.7590    
    ## groupingSmall group   -0.3609  0.3453  -1.0451  0.2960  -1.0377  0.3159    
    ## groupingWhole class    0.1476  0.3221   0.4582  0.6468  -0.4837  0.7789    
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
    ## Mixed-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1716 (SE = 0.0718)
    ## tau (square root of estimated tau^2 value):             0.4142
    ## I^2 (residual heterogeneity / unaccounted variability): 94.88%
    ## H^2 (unaccounted variability / sampling variability):   19.53
    ## R^2 (amount of heterogeneity accounted for):            14.20%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 17) = 135.7075, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 3.3430, p-val = 0.0675
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0662  0.1179  0.5615  0.5744  -0.1649  0.2973    
    ## TMULT      0.4897  0.2678  1.8284  0.0675  -0.0352  1.0147  . 
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
    mods = ~ TVOC
  )
```

    ## 
    ## Random-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.1999 (SE = 0.0798)
    ## tau (square root of estimated tau^2 value):      0.4471
    ## I^2 (total heterogeneity / total variability):   95.40%
    ## H^2 (total variability / sampling variability):  21.73
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 18) = 145.5857, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.1615  0.1129  1.4306  0.1525  -0.0598  0.3828    
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
    mods = ~ TSYN
  )
```

    ## 
    ## Mixed-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1716 (SE = 0.0718)
    ## tau (square root of estimated tau^2 value):             0.4142
    ## I^2 (residual heterogeneity / unaccounted variability): 94.88%
    ## H^2 (unaccounted variability / sampling variability):   19.53
    ## R^2 (amount of heterogeneity accounted for):            14.20%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 17) = 135.7075, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 3.3430, p-val = 0.0675
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0662  0.1179  0.5615  0.5744  -0.1649  0.2973    
    ## TSYN       0.4897  0.2678  1.8284  0.0675  -0.0352  1.0147  . 
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
    ## Mixed-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1716 (SE = 0.0718)
    ## tau (square root of estimated tau^2 value):             0.4142
    ## I^2 (residual heterogeneity / unaccounted variability): 94.88%
    ## H^2 (unaccounted variability / sampling variability):   19.53
    ## R^2 (amount of heterogeneity accounted for):            14.20%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 17) = 135.7075, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 3.3430, p-val = 0.0675
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0662  0.1179  0.5615  0.5744  -0.1649  0.2973    
    ## TMOR       0.4897  0.2678  1.8284  0.0675  -0.0352  1.0147  . 
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
    mods = ~ TLC
  )
```

    ## 
    ## Mixed-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.2093 (SE = 0.0859)
    ## tau (square root of estimated tau^2 value):             0.4575
    ## I^2 (residual heterogeneity / unaccounted variability): 95.59%
    ## H^2 (unaccounted variability / sampling variability):   22.69
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 17) = 139.0451, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.3569, p-val = 0.5502
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.2530  0.1914   1.3220  0.1862  -0.1221  0.6281    
    ## TLC       -0.1431  0.2396  -0.5975  0.5502  -0.6127  0.3264    
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
    mods = ~ TRC
  )
```

    ## 
    ## Mixed-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1266 (SE = 0.0558)
    ## tau (square root of estimated tau^2 value):             0.3558
    ## I^2 (residual heterogeneity / unaccounted variability): 92.99%
    ## H^2 (unaccounted variability / sampling variability):   14.27
    ## R^2 (amount of heterogeneity accounted for):            36.69%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 17) = 117.7441, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 8.2761, p-val = 0.0040
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb    ci.ub 
    ## intrcpt    0.3314  0.1108   2.9914  0.0028   0.1143   0.5485  ** 
    ## TRC       -0.5943  0.2066  -2.8768  0.0040  -0.9992  -0.1894  ** 
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
    mods = ~ TPAD
  )
```

    ## 
    ## Mixed-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.2111 (SE = 0.0851)
    ## tau (square root of estimated tau^2 value):             0.4595
    ## I^2 (residual heterogeneity / unaccounted variability): 95.84%
    ## H^2 (unaccounted variability / sampling variability):   24.04
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 17) = 145.5386, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0451, p-val = 0.8319
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.1545  0.1204  1.2829  0.1995  -0.0815  0.3905    
    ## TPAD       0.0908  0.4276  0.2123  0.8319  -0.7474  0.9289    
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
    mods = ~ TDD
  )
```

    ## 
    ## Mixed-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.2166 (SE = 0.0884)
    ## tau (square root of estimated tau^2 value):             0.4654
    ## I^2 (residual heterogeneity / unaccounted variability): 95.85%
    ## H^2 (unaccounted variability / sampling variability):   24.11
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 17) = 145.3326, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0068, p-val = 0.9341
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.1641  0.1203   1.3644  0.1724  -0.0716  0.3999    
    ## TDD       -0.0416  0.5032  -0.0826  0.9341  -1.0279  0.9448    
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
    mods = ~ TTEC
  )
```

    ## 
    ## Mixed-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1826 (SE = 0.0759)
    ## tau (square root of estimated tau^2 value):             0.4274
    ## I^2 (residual heterogeneity / unaccounted variability): 95.13%
    ## H^2 (unaccounted variability / sampling variability):   20.53
    ## R^2 (amount of heterogeneity accounted for):            8.65%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 17) = 141.0038, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 2.4976, p-val = 0.1140
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0614  0.1257  0.4889  0.6249  -0.1849  0.3078    
    ## TTEC       0.3953  0.2501  1.5804  0.1140  -0.0949  0.8856    
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
    mods = ~ TSTR
  )
```

    ## 
    ## Random-Effects Model (k = 19; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.1999 (SE = 0.0798)
    ## tau (square root of estimated tau^2 value):      0.4471
    ## I^2 (total heterogeneity / total variability):   95.40%
    ## H^2 (total variability / sampling variability):  21.73
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 18) = 145.5857, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.1615  0.1129  1.4306  0.1525  -0.0598  0.3828    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Listening Comprehension (LR only)

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ CONT
  )
```

    ## 
    ## Mixed-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0289 (SE = 0.0222)
    ## tau (square root of estimated tau^2 value):             0.1700
    ## I^2 (residual heterogeneity / unaccounted variability): 70.32%
    ## H^2 (unaccounted variability / sampling variability):   3.37
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 10) = 26.8944, p-val = 0.0027
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.2377, p-val = 0.6258
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.2796  0.0727  3.8439  0.0001   0.1370  0.4222  *** 
    ## CONTALT    0.0785  0.1611  0.4876  0.6258  -0.2371  0.3942      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ Duration
  )
```

    ## 
    ## Mixed-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0300 (SE = 0.0218)
    ## tau (square root of estimated tau^2 value):             0.1732
    ## I^2 (residual heterogeneity / unaccounted variability): 77.02%
    ## H^2 (unaccounted variability / sampling variability):   4.35
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 10) = 30.5503, p-val = 0.0007
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.1686, p-val = 0.6814
    ## 
    ## Model Results:
    ## 
    ##           estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt     0.2313  0.1706  1.3559  0.1751  -0.1031  0.5657    
    ## Duration    0.0759  0.1849  0.4106  0.6814  -0.2864  0.4383    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ scale(Hours)
  )
```

    ## 
    ## Mixed-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0056 (SE = 0.0075)
    ## tau (square root of estimated tau^2 value):             0.0749
    ## I^2 (residual heterogeneity / unaccounted variability): 35.31%
    ## H^2 (unaccounted variability / sampling variability):   1.55
    ## R^2 (amount of heterogeneity accounted for):            77.80%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 10) = 16.2970, p-val = 0.0914
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 10.2556, p-val = 0.0014
    ## 
    ## Model Results:
    ## 
    ##               estimate      se    zval    pval   ci.lb   ci.ub 
    ## intrcpt         0.3207  0.0412  7.7793  <.0001  0.2399  0.4015  *** 
    ## scale(Hours)    0.1731  0.0540  3.2024  0.0014  0.0672  0.2790   ** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ design
  )
```

    ## 
    ## Mixed-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0300 (SE = 0.0218)
    ## tau (square root of estimated tau^2 value):             0.1732
    ## I^2 (residual heterogeneity / unaccounted variability): 77.02%
    ## H^2 (unaccounted variability / sampling variability):   4.35
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 10) = 30.5503, p-val = 0.0007
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.1686, p-val = 0.6814
    ## 
    ## Model Results:
    ## 
    ##            estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt      0.3073  0.0712   4.3152  <.0001   0.1677  0.4468  *** 
    ## designQED   -0.0759  0.1849  -0.4106  0.6814  -0.4383  0.2864      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ grade
  )
```

    ## 
    ## Mixed-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0074 (SE = 0.0085)
    ## tau (square root of estimated tau^2 value):             0.0859
    ## I^2 (residual heterogeneity / unaccounted variability): 44.34%
    ## H^2 (unaccounted variability / sampling variability):   1.80
    ## R^2 (amount of heterogeneity accounted for):            70.86%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 10) = 20.7980, p-val = 0.0225
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 6.2415, p-val = 0.0125
    ## 
    ## Model Results:
    ## 
    ##           estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt     0.0677  0.0999  0.6772  0.4983  -0.1281  0.2635    
    ## gradeK-2    0.2769  0.1108  2.4983  0.0125   0.0597  0.4942  * 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ grouping
  )
```

    ## 
    ## Mixed-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0290 (SE = 0.0233)
    ## tau (square root of estimated tau^2 value):             0.1702
    ## I^2 (residual heterogeneity / unaccounted variability): 71.79%
    ## H^2 (unaccounted variability / sampling variability):   3.54
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 9) = 29.4983, p-val = 0.0005
    ## 
    ## Test of Moderators (coefficients 2:3):
    ## QM(df = 2) = 3.1527, p-val = 0.2067
    ## 
    ## Model Results:
    ## 
    ##                      estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt                0.1225  0.2262  0.5418  0.5880  -0.3208  0.5659    
    ## groupingSmall group    0.0428  0.2534  0.1689  0.8658  -0.4539  0.5395    
    ## groupingWhole class    0.2679  0.2414  1.1098  0.2671  -0.2052  0.7409    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TMULT
  )
```

    ## 
    ## Mixed-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0184 (SE = 0.0151)
    ## tau (square root of estimated tau^2 value):             0.1357
    ## I^2 (residual heterogeneity / unaccounted variability): 67.71%
    ## H^2 (unaccounted variability / sampling variability):   3.10
    ## R^2 (amount of heterogeneity accounted for):            27.16%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 10) = 26.3529, p-val = 0.0033
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 4.6503, p-val = 0.0310
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval   ci.lb   ci.ub 
    ## intrcpt    0.2388  0.0614  3.8897  0.0001  0.1185  0.3591  *** 
    ## TMULT      0.3282  0.1522  2.1565  0.0310  0.0299  0.6264    * 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TVOC
  )
```

    ## 
    ## Random-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0253 (SE = 0.0185)
    ## tau (square root of estimated tau^2 value):      0.1590
    ## I^2 (total heterogeneity / total variability):   72.91%
    ## H^2 (total variability / sampling variability):  3.69
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 11) = 31.2272, p-val = 0.0010
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub 
    ##   0.2944  0.0621  4.7397  <.0001  0.1726  0.4161  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TSYN
  )
```

    ## 
    ## Mixed-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0184 (SE = 0.0151)
    ## tau (square root of estimated tau^2 value):             0.1357
    ## I^2 (residual heterogeneity / unaccounted variability): 67.71%
    ## H^2 (unaccounted variability / sampling variability):   3.10
    ## R^2 (amount of heterogeneity accounted for):            27.16%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 10) = 26.3529, p-val = 0.0033
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 4.6503, p-val = 0.0310
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval   ci.lb   ci.ub 
    ## intrcpt    0.2388  0.0614  3.8897  0.0001  0.1185  0.3591  *** 
    ## TSYN       0.3282  0.1522  2.1565  0.0310  0.0299  0.6264    * 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TMOR
  )
```

    ## 
    ## Mixed-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0184 (SE = 0.0151)
    ## tau (square root of estimated tau^2 value):             0.1357
    ## I^2 (residual heterogeneity / unaccounted variability): 67.71%
    ## H^2 (unaccounted variability / sampling variability):   3.10
    ## R^2 (amount of heterogeneity accounted for):            27.16%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 10) = 26.3529, p-val = 0.0033
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 4.6503, p-val = 0.0310
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval   ci.lb   ci.ub 
    ## intrcpt    0.2388  0.0614  3.8897  0.0001  0.1185  0.3591  *** 
    ## TMOR       0.3282  0.1522  2.1565  0.0310  0.0299  0.6264    * 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TLC
  )
```

    ## 
    ## Mixed-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0299 (SE = 0.0222)
    ## tau (square root of estimated tau^2 value):             0.1728
    ## I^2 (residual heterogeneity / unaccounted variability): 76.21%
    ## H^2 (unaccounted variability / sampling variability):   4.20
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 10) = 28.3212, p-val = 0.0016
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0498, p-val = 0.8235
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.3093  0.0887   3.4864  0.0005   0.1354  0.4831  *** 
    ## TLC       -0.0294  0.1318  -0.2231  0.8235  -0.2877  0.2289      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TRC
  )
```

    ## 
    ## Mixed-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0059 (SE = 0.0075)
    ## tau (square root of estimated tau^2 value):             0.0768
    ## I^2 (residual heterogeneity / unaccounted variability): 38.90%
    ## H^2 (unaccounted variability / sampling variability):   1.64
    ## R^2 (amount of heterogeneity accounted for):            76.68%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 10) = 17.6535, p-val = 0.0611
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 9.6606, p-val = 0.0019
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb    ci.ub 
    ## intrcpt    0.3486  0.0445   7.8324  <.0001   0.2614   0.4358  *** 
    ## TRC       -0.3500  0.1126  -3.1082  0.0019  -0.5707  -0.1293   ** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TPAD
  )
```

    ## 
    ## Random-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0253 (SE = 0.0185)
    ## tau (square root of estimated tau^2 value):      0.1590
    ## I^2 (total heterogeneity / total variability):   72.91%
    ## H^2 (total variability / sampling variability):  3.69
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 11) = 31.2272, p-val = 0.0010
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub 
    ##   0.2944  0.0621  4.7397  <.0001  0.1726  0.4161  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TDD
  )
```

    ## 
    ## Mixed-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0280 (SE = 0.0208)
    ## tau (square root of estimated tau^2 value):             0.1675
    ## I^2 (residual heterogeneity / unaccounted variability): 75.79%
    ## H^2 (unaccounted variability / sampling variability):   4.13
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 10) = 29.5849, p-val = 0.0010
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.6471, p-val = 0.4211
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.3108  0.0671   4.6345  <.0001   0.1793  0.4422  *** 
    ## TDD       -0.1882  0.2340  -0.8044  0.4211  -0.6468  0.2704      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TTEC
  )
```

    ## 
    ## Mixed-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0275 (SE = 0.0204)
    ## tau (square root of estimated tau^2 value):             0.1657
    ## I^2 (residual heterogeneity / unaccounted variability): 75.43%
    ## H^2 (unaccounted variability / sampling variability):   4.07
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 10) = 30.3959, p-val = 0.0007
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.6692, p-val = 0.1964
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.2450  0.0747  3.2810  0.0010   0.0987  0.3914  ** 
    ## TTEC       0.1857  0.1438  1.2920  0.1964  -0.0960  0.4675     
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TSTR
  )
```

    ## 
    ## Random-Effects Model (k = 12; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0253 (SE = 0.0185)
    ## tau (square root of estimated tau^2 value):      0.1590
    ## I^2 (total heterogeneity / total variability):   72.91%
    ## H^2 (total variability / sampling variability):  3.69
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 11) = 31.2272, p-val = 0.0010
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub 
    ##   0.2944  0.0621  4.7397  <.0001  0.1726  0.4161  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Listening Comprehension (LS only)

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ CONT
  )
```

    ## 
    ## Mixed-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.5483 (SE = 0.4014)
    ## tau (square root of estimated tau^2 value):             0.7405
    ## I^2 (residual heterogeneity / unaccounted variability): 92.23%
    ## H^2 (unaccounted variability / sampling variability):   12.87
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 5) = 57.8991, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.1113, p-val = 0.7387
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt   -0.1758  0.3256  -0.5400  0.5892  -0.8139  0.4623    
    ## CONTALT    0.2748  0.8237   0.3336  0.7387  -1.3396  1.8892    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ Duration
  )
```

    ## 
    ## Mixed-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.4732 (SE = 0.3375)
    ## tau (square root of estimated tau^2 value):             0.6879
    ## I^2 (residual heterogeneity / unaccounted variability): 94.21%
    ## H^2 (unaccounted variability / sampling variability):   17.27
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 5) = 57.0162, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.7212, p-val = 0.3958
    ## 
    ## Model Results:
    ## 
    ##           estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt     0.5157  0.8170   0.6312  0.5279  -1.0855  2.1169    
    ## Duration   -0.7386  0.8698  -0.8492  0.3958  -2.4434  0.9661    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ scale(Hours)
  )
```

    ## 
    ## Mixed-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.4560 (SE = 0.3319)
    ## tau (square root of estimated tau^2 value):             0.6753
    ## I^2 (residual heterogeneity / unaccounted variability): 93.61%
    ## H^2 (unaccounted variability / sampling variability):   15.65
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 5) = 54.2042, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.8326, p-val = 0.3615
    ## 
    ## Model Results:
    ## 
    ##               estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt        -0.1265  0.2761  -0.4583  0.6468  -0.6676  0.4146    
    ## scale(Hours)    0.2813  0.3083   0.9124  0.3615  -0.3229  0.8855    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ design
  )
```

    ## 
    ## Mixed-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.4732 (SE = 0.3375)
    ## tau (square root of estimated tau^2 value):             0.6879
    ## I^2 (residual heterogeneity / unaccounted variability): 94.21%
    ## H^2 (unaccounted variability / sampling variability):   17.27
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 5) = 57.0162, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.7212, p-val = 0.3958
    ## 
    ## Model Results:
    ## 
    ##            estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt     -0.2230  0.2985  -0.7470  0.4550  -0.8079  0.3620    
    ## designQED    0.7386  0.8698   0.8492  0.3958  -0.9661  2.4434    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ grade
  )
```

    ## 
    ## Mixed-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1855 (SE = 0.1614)
    ## tau (square root of estimated tau^2 value):             0.4307
    ## I^2 (residual heterogeneity / unaccounted variability): 83.35%
    ## H^2 (unaccounted variability / sampling variability):   6.01
    ## R^2 (amount of heterogeneity accounted for):            59.14%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 5) = 20.1406, p-val = 0.0012
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 7.4646, p-val = 0.0063
    ## 
    ## Model Results:
    ## 
    ##           estimate      se     zval    pval    ci.lb    ci.ub 
    ## intrcpt    -0.9551  0.3483  -2.7426  0.0061  -1.6377  -0.2726  ** 
    ## gradeK-2    1.1377  0.4164   2.7321  0.0063   0.3216   1.9539  ** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ grouping
  )
```

    ## 
    ## Mixed-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.4876 (SE = 0.4002)
    ## tau (square root of estimated tau^2 value):             0.6983
    ## I^2 (residual heterogeneity / unaccounted variability): 93.08%
    ## H^2 (unaccounted variability / sampling variability):   14.45
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 4) = 28.8693, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 2:3):
    ## QM(df = 2) = 1.4008, p-val = 0.4964
    ## 
    ## Model Results:
    ## 
    ##                      estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt                0.2437  0.5538   0.4401  0.6598  -0.8417  1.3292    
    ## groupingSmall group   -0.7696  0.7080  -1.0870  0.2770  -2.1574  0.6181    
    ## groupingWhole class   -0.1864  0.7462  -0.2498  0.8028  -1.6488  1.2761    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TMULT
  )
```

    ## 
    ## Random-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.4540 (SE = 0.3054)
    ## tau (square root of estimated tau^2 value):      0.6738
    ## I^2 (total heterogeneity / total variability):   93.20%
    ## H^2 (total variability / sampling variability):  14.70
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 6) = 58.4876, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval    ci.lb   ci.ub 
    ##  -0.1369  0.2753  -0.4972  0.6191  -0.6766  0.4028    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TVOC
  )
```

    ## 
    ## Random-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.4540 (SE = 0.3054)
    ## tau (square root of estimated tau^2 value):      0.6738
    ## I^2 (total heterogeneity / total variability):   93.20%
    ## H^2 (total variability / sampling variability):  14.70
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 6) = 58.4876, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval    ci.lb   ci.ub 
    ##  -0.1369  0.2753  -0.4972  0.6191  -0.6766  0.4028    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TSYN
  )
```

    ## 
    ## Random-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.4540 (SE = 0.3054)
    ## tau (square root of estimated tau^2 value):      0.6738
    ## I^2 (total heterogeneity / total variability):   93.20%
    ## H^2 (total variability / sampling variability):  14.70
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 6) = 58.4876, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval    ci.lb   ci.ub 
    ##  -0.1369  0.2753  -0.4972  0.6191  -0.6766  0.4028    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TMOR
  )
```

    ## 
    ## Random-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.4540 (SE = 0.3054)
    ## tau (square root of estimated tau^2 value):      0.6738
    ## I^2 (total heterogeneity / total variability):   93.20%
    ## H^2 (total variability / sampling variability):  14.70
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 6) = 58.4876, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval    ci.lb   ci.ub 
    ##  -0.1369  0.2753  -0.4972  0.6191  -0.6766  0.4028    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TLC
  )
```

    ## 
    ## Mixed-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.5617 (SE = 0.4132)
    ## tau (square root of estimated tau^2 value):             0.7494
    ## I^2 (residual heterogeneity / unaccounted variability): 89.07%
    ## H^2 (unaccounted variability / sampling variability):   9.15
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 5) = 51.6442, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0477, p-val = 0.8271
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0176  0.7504   0.0235  0.9813  -1.4531  1.4884    
    ## TLC       -0.1791  0.8198  -0.2184  0.8271  -1.7859  1.4278    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TRC
  )
```

    ## 
    ## Mixed-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.4450 (SE = 0.3309)
    ## tau (square root of estimated tau^2 value):             0.6671
    ## I^2 (residual heterogeneity / unaccounted variability): 91.72%
    ## H^2 (unaccounted variability / sampling variability):   12.07
    ## R^2 (amount of heterogeneity accounted for):            1.98%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 5) = 37.5168, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.0590, p-val = 0.3034
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.1781  0.4105   0.4339  0.6643  -0.6264  0.9826    
    ## TRC       -0.5656  0.5496  -1.0291  0.3034  -1.6428  0.5116    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TPAD
  )
```

    ## 
    ## Mixed-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.4800 (SE = 0.3480)
    ## tau (square root of estimated tau^2 value):             0.6928
    ## I^2 (residual heterogeneity / unaccounted variability): 93.91%
    ## H^2 (unaccounted variability / sampling variability):   16.41
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 5) = 57.3133, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.6446, p-val = 0.4221
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt   -0.2708  0.3285  -0.8245  0.4097  -0.9147  0.3730    
    ## TPAD       0.5146  0.6410   0.8029  0.4221  -0.7417  1.7709    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TDD
  )
```

    ## 
    ## Random-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.4540 (SE = 0.3054)
    ## tau (square root of estimated tau^2 value):      0.6738
    ## I^2 (total heterogeneity / total variability):   93.20%
    ## H^2 (total variability / sampling variability):  14.70
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 6) = 58.4876, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval    ci.lb   ci.ub 
    ##  -0.1369  0.2753  -0.4972  0.6191  -0.6766  0.4028    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TTEC
  )
```

    ## 
    ## Random-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.4540 (SE = 0.3054)
    ## tau (square root of estimated tau^2 value):      0.6738
    ## I^2 (total heterogeneity / total variability):   93.20%
    ## H^2 (total variability / sampling variability):  14.70
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 6) = 58.4876, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval    ci.lb   ci.ub 
    ##  -0.1369  0.2753  -0.4972  0.6191  -0.6766  0.4028    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_l %>%
  filter(type == "LS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TSTR
  )
```

    ## 
    ## Random-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.4540 (SE = 0.3054)
    ## tau (square root of estimated tau^2 value):      0.6738
    ## I^2 (total heterogeneity / total variability):   93.20%
    ## H^2 (total variability / sampling variability):  14.70
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 6) = 58.4876, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval    ci.lb   ci.ub 
    ##  -0.1369  0.2753  -0.4972  0.6191  -0.6766  0.4028    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Reading Comprehension (Taken together)

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
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0874 (SE = 0.0348)
    ## tau (square root of estimated tau^2 value):             0.2957
    ## I^2 (residual heterogeneity / unaccounted variability): 90.10%
    ## H^2 (unaccounted variability / sampling variability):   10.10
    ## R^2 (amount of heterogeneity accounted for):            22.41%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 22) = 93.3653, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 5.8483, p-val = 0.0156
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0979  0.0808  1.2110  0.2259  -0.0605  0.2562    
    ## typeRR     0.4017  0.1661  2.4183  0.0156   0.0761  0.7273  * 
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
    mods = ~ CONT
  )
```

    ## 
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0525 (SE = 0.0226)
    ## tau (square root of estimated tau^2 value):             0.2292
    ## I^2 (residual heterogeneity / unaccounted variability): 86.11%
    ## H^2 (unaccounted variability / sampling variability):   7.20
    ## R^2 (amount of heterogeneity accounted for):            53.38%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 22) = 79.5082, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 16.0375, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.1047  0.0610  1.7171  0.0860  -0.0148  0.2242    . 
    ## CONTALT    0.7768  0.1940  4.0047  <.0001   0.3966  1.1570  *** 
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
    mods = ~ Duration
  )
```

    ## 
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1229 (SE = 0.0462)
    ## tau (square root of estimated tau^2 value):             0.3506
    ## I^2 (residual heterogeneity / unaccounted variability): 93.35%
    ## H^2 (unaccounted variability / sampling variability):   15.04
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 22) = 105.7406, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0077, p-val = 0.9301
    ## 
    ## Model Results:
    ## 
    ##           estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt     0.1867  0.1659  1.1256  0.2603  -0.1384  0.5118    
    ## Duration    0.0167  0.1902  0.0877  0.9301  -0.3561  0.3895    
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
    mods = ~ scale(Hours)
  )
```

    ## 
    ## Mixed-Effects Model (k = 23; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1311 (SE = 0.0500)
    ## tau (square root of estimated tau^2 value):             0.3621
    ## I^2 (residual heterogeneity / unaccounted variability): 93.26%
    ## H^2 (unaccounted variability / sampling variability):   14.83
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 21) = 106.7052, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.1906, p-val = 0.6625
    ## 
    ## Model Results:
    ## 
    ##               estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt         0.2047  0.0852   2.4020  0.0163   0.0377  0.3717  * 
    ## scale(Hours)   -0.0392  0.0899  -0.4365  0.6625  -0.2154  0.1370    
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
    mods = ~ design
  )
```

    ## 
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1231 (SE = 0.0464)
    ## tau (square root of estimated tau^2 value):             0.3508
    ## I^2 (residual heterogeneity / unaccounted variability): 93.35%
    ## H^2 (unaccounted variability / sampling variability):   15.05
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 22) = 106.6937, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.3208, p-val = 0.5711
    ## 
    ## Model Results:
    ## 
    ##            estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt      0.2276  0.0953   2.3895  0.0169   0.0409  0.4143  * 
    ## designQED   -0.1033  0.1824  -0.5664  0.5711  -0.4609  0.2542    
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
    mods = ~ grade
  )
```

    ## 
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1290 (SE = 0.0489)
    ## tau (square root of estimated tau^2 value):             0.3592
    ## I^2 (residual heterogeneity / unaccounted variability): 94.04%
    ## H^2 (unaccounted variability / sampling variability):   16.78
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 21) = 106.1104, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 2:3):
    ## QM(df = 2) = 0.5547, p-val = 0.7578
    ## 
    ## Model Results:
    ## 
    ##            estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt      0.2300  0.0948   2.4275  0.0152   0.0443  0.4157  * 
    ## gradeK-2    -0.1562  0.2114  -0.7390  0.4599  -0.5705  0.2581    
    ## gradeBoth    0.0064  0.4115   0.0156  0.9876  -0.8000  0.8129    
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
    mods = ~ grouping
  )
```

    ## 
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0967 (SE = 0.0398)
    ## tau (square root of estimated tau^2 value):             0.3110
    ## I^2 (residual heterogeneity / unaccounted variability): 91.09%
    ## H^2 (unaccounted variability / sampling variability):   11.22
    ## R^2 (amount of heterogeneity accounted for):            14.13%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 20) = 96.6246, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 2:4):
    ## QM(df = 3) = 9.1280, p-val = 0.0276
    ## 
    ## Model Results:
    ## 
    ##                      estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt                0.1786  0.1322   1.3505  0.1768  -0.0806  0.4377    
    ## groupingSmall group   -0.1086  0.1937  -0.5605  0.5751  -0.4883  0.2711    
    ## groupingWhole class   -0.0821  0.1873  -0.4385  0.6610  -0.4493  0.2850    
    ## groupingIndividual     0.6258  0.2553   2.4509  0.0143   0.1253  1.1263  * 
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
    mods = ~ TMULT
  )
```

    ## 
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1228 (SE = 0.0462)
    ## tau (square root of estimated tau^2 value):             0.3504
    ## I^2 (residual heterogeneity / unaccounted variability): 93.47%
    ## H^2 (unaccounted variability / sampling variability):   15.30
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 22) = 106.7153, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0886, p-val = 0.7660
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.2090  0.0873   2.3934  0.0167   0.0378  0.3801  * 
    ## TMULT     -0.0704  0.2366  -0.2976  0.7660  -0.5342  0.3933    
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
    mods = ~ TVOC
  )
```

    ## 
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1194 (SE = 0.0445)
    ## tau (square root of estimated tau^2 value):             0.3456
    ## I^2 (residual heterogeneity / unaccounted variability): 93.42%
    ## H^2 (unaccounted variability / sampling variability):   15.20
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 22) = 106.7060, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0618, p-val = 0.8036
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.1193  0.3302  0.3612  0.7179  -0.5279  0.7664    
    ## TVOC       0.0846  0.3404  0.2487  0.8036  -0.5825  0.7518    
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
    mods = ~ TSYN
  )
```

    ## 
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1228 (SE = 0.0462)
    ## tau (square root of estimated tau^2 value):             0.3504
    ## I^2 (residual heterogeneity / unaccounted variability): 93.47%
    ## H^2 (unaccounted variability / sampling variability):   15.30
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 22) = 106.7153, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0886, p-val = 0.7660
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.2090  0.0873   2.3934  0.0167   0.0378  0.3801  * 
    ## TSYN      -0.0704  0.2366  -0.2976  0.7660  -0.5342  0.3933    
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
    mods = ~ TMOR
  )
```

    ## 
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1220 (SE = 0.0458)
    ## tau (square root of estimated tau^2 value):             0.3493
    ## I^2 (residual heterogeneity / unaccounted variability): 93.46%
    ## H^2 (unaccounted variability / sampling variability):   15.30
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 22) = 105.9429, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0030, p-val = 0.9561
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.2011  0.0877   2.2932  0.0218   0.0292  0.3731  * 
    ## TMOR      -0.0125  0.2276  -0.0550  0.9561  -0.4586  0.4335    
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
    mods = ~ TLC
  )
```

    ## 
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1112 (SE = 0.0424)
    ## tau (square root of estimated tau^2 value):             0.3335
    ## I^2 (residual heterogeneity / unaccounted variability): 92.78%
    ## H^2 (unaccounted variability / sampling variability):   13.86
    ## R^2 (amount of heterogeneity accounted for):            1.27%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 22) = 104.0924, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 2.0100, p-val = 0.1563
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.2736  0.0946   2.8938  0.0038   0.0883  0.4590  ** 
    ## TLC       -0.2365  0.1668  -1.4177  0.1563  -0.5633  0.0904     
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
    mods = ~ TRC
  )
```

    ## 
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1069 (SE = 0.0411)
    ## tau (square root of estimated tau^2 value):             0.3270
    ## I^2 (residual heterogeneity / unaccounted variability): 91.97%
    ## H^2 (unaccounted variability / sampling variability):   12.45
    ## R^2 (amount of heterogeneity accounted for):            5.10%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 22) = 105.3698, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 2.1272, p-val = 0.1447
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.2881  0.0989   2.9129  0.0036   0.0942  0.4819  ** 
    ## TRC       -0.2282  0.1565  -1.4585  0.1447  -0.5349  0.0785     
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
    mods = ~ TPAD
  )
```

    ## 
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1211 (SE = 0.0453)
    ## tau (square root of estimated tau^2 value):             0.3480
    ## I^2 (residual heterogeneity / unaccounted variability): 93.47%
    ## H^2 (unaccounted variability / sampling variability):   15.30
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 22) = 106.4170, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0042, p-val = 0.9482
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.2009  0.0851   2.3605  0.0183   0.0341  0.3677  * 
    ## TPAD      -0.0174  0.2673  -0.0650  0.9482  -0.5413  0.5065    
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
    mods = ~ TDD
  )
```

    ## 
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1239 (SE = 0.0467)
    ## tau (square root of estimated tau^2 value):             0.3520
    ## I^2 (residual heterogeneity / unaccounted variability): 92.74%
    ## H^2 (unaccounted variability / sampling variability):   13.77
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 22) = 106.6665, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.2002, p-val = 0.6546
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.2252  0.0996   2.2607  0.0238   0.0300  0.4204  * 
    ## TDD       -0.0775  0.1731  -0.4474  0.6546  -0.4167  0.2618    
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
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0970 (SE = 0.0378)
    ## tau (square root of estimated tau^2 value):             0.3114
    ## I^2 (residual heterogeneity / unaccounted variability): 91.71%
    ## H^2 (unaccounted variability / sampling variability):   12.07
    ## R^2 (amount of heterogeneity accounted for):            13.94%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 22) = 101.9262, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 5.0867, p-val = 0.0241
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0850  0.0883  0.9623  0.3359  -0.0881  0.2581    
    ## TTEC       0.3606  0.1599  2.2554  0.0241   0.0472  0.6740  * 
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
    mods = ~ TSTR
  )
```

    ## 
    ## Mixed-Effects Model (k = 24; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1155 (SE = 0.0439)
    ## tau (square root of estimated tau^2 value):             0.3398
    ## I^2 (residual heterogeneity / unaccounted variability): 92.87%
    ## H^2 (unaccounted variability / sampling variability):   14.02
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 22) = 100.8564, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.0008, p-val = 0.3171
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.2371  0.0881   2.6912  0.0071   0.0644  0.4097  ** 
    ## TSTR      -0.2004  0.2003  -1.0004  0.3171  -0.5930  0.1922     
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Reading Comprehension (RR only)

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ CONT
  )
```

    ## 
    ## Mixed-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1076 (SE = 0.0957)
    ## tau (square root of estimated tau^2 value):             0.3281
    ## I^2 (residual heterogeneity / unaccounted variability): 93.21%
    ## H^2 (unaccounted variability / sampling variability):   14.73
    ## R^2 (amount of heterogeneity accounted for):            76.01%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 4) = 26.1314, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 8.2608, p-val = 0.0041
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.3257  0.1715  1.8997  0.0575  -0.0103  0.6618   . 
    ## CONTALT    1.2939  0.4502  2.8742  0.0041   0.4115  2.1762  ** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ Duration
  )
```

    ## 
    ## Mixed-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.5936 (SE = 0.4911)
    ## tau (square root of estimated tau^2 value):             0.7705
    ## I^2 (residual heterogeneity / unaccounted variability): 98.44%
    ## H^2 (unaccounted variability / sampling variability):   64.25
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 4) = 47.8162, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.2220, p-val = 0.6375
    ## 
    ## Model Results:
    ## 
    ##           estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt     0.3789  0.5125  0.7393  0.4597  -0.6256  1.3834    
    ## Duration    0.3231  0.6858  0.4712  0.6375  -1.0211  1.6674    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ scale(Hours)
  )
```

    ## 
    ## Mixed-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.5834 (SE = 0.4849)
    ## tau (square root of estimated tau^2 value):             0.7638
    ## I^2 (residual heterogeneity / unaccounted variability): 98.38%
    ## H^2 (unaccounted variability / sampling variability):   61.77
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 4) = 45.4764, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0391, p-val = 0.8433
    ## 
    ## Model Results:
    ## 
    ##               estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt         0.5699  0.3419   1.6668  0.0956  -0.1003  1.2400  . 
    ## scale(Hours)   -0.0742  0.3757  -0.1976  0.8433  -0.8106  0.6621    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ design
  )
```

    ## 
    ## Mixed-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.5906 (SE = 0.4836)
    ## tau (square root of estimated tau^2 value):             0.7685
    ## I^2 (residual heterogeneity / unaccounted variability): 98.55%
    ## H^2 (unaccounted variability / sampling variability):   68.91
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 4) = 35.0993, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0002, p-val = 0.9894
    ## 
    ## Model Results:
    ## 
    ##            estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt      0.5620  0.3926   1.4315  0.1523  -0.2075  1.3315    
    ## designQED   -0.0104  0.7836  -0.0132  0.9894  -1.5461  1.5254    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ grade
  )
```

    ## 
    ## Random-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.4488 (SE = 0.3304)
    ## tau (square root of estimated tau^2 value):      0.6699
    ## I^2 (total heterogeneity / total variability):   97.92%
    ## H^2 (total variability / sampling variability):  48.11
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 5) = 54.0068, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.5606  0.2995  1.8718  0.0612  -0.0264  1.1476  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ grouping
  )
```

    ## 
    ## Mixed-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0 (SE = 0.0039)
    ## tau (square root of estimated tau^2 value):             0
    ## I^2 (residual heterogeneity / unaccounted variability): 0.00%
    ## H^2 (unaccounted variability / sampling variability):   1.00
    ## R^2 (amount of heterogeneity accounted for):            100.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 3) = 1.3736, p-val = 0.7117
    ## 
    ## Test of Moderators (coefficients 2:3):
    ## QM(df = 2) = 52.6332, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##                      estimate      se     zval    pval    ci.lb    ci.ub 
    ## intrcpt                0.8508  0.1438   5.9163  <.0001   0.5690   1.1327  *** 
    ## groupingWhole class   -0.6825  0.1481  -4.6084  <.0001  -0.9728  -0.3923  *** 
    ## groupingIndividual     1.0541  0.3336   3.1597  0.0016   0.4002   1.7080   ** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TMULT
  )
```

    ## 
    ## Random-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.4488 (SE = 0.3304)
    ## tau (square root of estimated tau^2 value):      0.6699
    ## I^2 (total heterogeneity / total variability):   97.92%
    ## H^2 (total variability / sampling variability):  48.11
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 5) = 54.0068, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.5606  0.2995  1.8718  0.0612  -0.0264  1.1476  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TVOC
  )
```

    ## 
    ## Random-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.4488 (SE = 0.3304)
    ## tau (square root of estimated tau^2 value):      0.6699
    ## I^2 (total heterogeneity / total variability):   97.92%
    ## H^2 (total variability / sampling variability):  48.11
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 5) = 54.0068, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.5606  0.2995  1.8718  0.0612  -0.0264  1.1476  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TSYN
  )
```

    ## 
    ## Random-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.4488 (SE = 0.3304)
    ## tau (square root of estimated tau^2 value):      0.6699
    ## I^2 (total heterogeneity / total variability):   97.92%
    ## H^2 (total variability / sampling variability):  48.11
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 5) = 54.0068, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.5606  0.2995  1.8718  0.0612  -0.0264  1.1476  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TMOR
  )
```

    ## 
    ## Random-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.4488 (SE = 0.3304)
    ## tau (square root of estimated tau^2 value):      0.6699
    ## I^2 (total heterogeneity / total variability):   97.92%
    ## H^2 (total variability / sampling variability):  48.11
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 5) = 54.0068, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.5606  0.2995  1.8718  0.0612  -0.0264  1.1476  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TLC
  )
```

    ## 
    ## Mixed-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.4753 (SE = 0.3533)
    ## tau (square root of estimated tau^2 value):             0.6894
    ## I^2 (residual heterogeneity / unaccounted variability): 98.42%
    ## H^2 (unaccounted variability / sampling variability):   63.24
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 4) = 53.7350, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.5115, p-val = 0.4745
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.6133  0.3162   1.9396  0.0524  -0.0064  1.2330  . 
    ## TLC       -0.9704  1.3569  -0.7152  0.4745  -3.6299  1.6891    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TRC
  )
```

    ## 
    ## Mixed-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.5618 (SE = 0.4726)
    ## tau (square root of estimated tau^2 value):             0.7495
    ## I^2 (residual heterogeneity / unaccounted variability): 96.71%
    ## H^2 (unaccounted variability / sampling variability):   30.36
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 4) = 53.9484, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.2570, p-val = 0.6122
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.6427  0.3701   1.7366  0.0825  -0.0827  1.3681  . 
    ## TRC       -0.4248  0.8379  -0.5070  0.6122  -2.0672  1.2175    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TPAD
  )
```

    ## 
    ## Random-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.4488 (SE = 0.3304)
    ## tau (square root of estimated tau^2 value):      0.6699
    ## I^2 (total heterogeneity / total variability):   97.92%
    ## H^2 (total variability / sampling variability):  48.11
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 5) = 54.0068, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.5606  0.2995  1.8718  0.0612  -0.0264  1.1476  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TDD
  )
```

    ## 
    ## Mixed-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.5906 (SE = 0.4836)
    ## tau (square root of estimated tau^2 value):             0.7685
    ## I^2 (residual heterogeneity / unaccounted variability): 98.55%
    ## H^2 (unaccounted variability / sampling variability):   68.91
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 4) = 35.0993, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0002, p-val = 0.9894
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.5620  0.3926   1.4315  0.1523  -0.2075  1.3315    
    ## TDD       -0.0104  0.7836  -0.0132  0.9894  -1.5461  1.5254    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TTEC
  )
```

    ## 
    ## Mixed-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0023 (SE = 0.0068)
    ## tau (square root of estimated tau^2 value):             0.0483
    ## I^2 (residual heterogeneity / unaccounted variability): 21.21%
    ## H^2 (unaccounted variability / sampling variability):   1.27
    ## R^2 (amount of heterogeneity accounted for):            99.48%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 4) = 11.3574, p-val = 0.0228
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 38.3064, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval   ci.lb   ci.ub 
    ## intrcpt    0.1700  0.0472  3.5994  0.0003  0.0775  0.2626  *** 
    ## TTEC       0.8900  0.1438  6.1892  <.0001  0.6082  1.1718  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RR") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TSTR
  )
```

    ## 
    ## Mixed-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.4982 (SE = 0.4143)
    ## tau (square root of estimated tau^2 value):             0.7058
    ## I^2 (residual heterogeneity / unaccounted variability): 98.21%
    ## H^2 (unaccounted variability / sampling variability):   55.76
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 4) = 53.5193, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.9154, p-val = 0.3387
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.7331  0.3623   2.0235  0.0430   0.0230  1.4432  * 
    ## TSTR      -0.6960  0.7274  -0.9567  0.3387  -2.1217  0.7298    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Reading Comprehension (RS only)

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ CONT
  )
```

    ## 
    ## Mixed-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0062 (SE = 0.0060)
    ## tau (square root of estimated tau^2 value):             0.0789
    ## I^2 (residual heterogeneity / unaccounted variability): 40.24%
    ## H^2 (unaccounted variability / sampling variability):   1.67
    ## R^2 (amount of heterogeneity accounted for):            45.67%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 16) = 27.8223, p-val = 0.0332
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 11.0755, p-val = 0.0009
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0455  0.0355  1.2808  0.2003  -0.0241  0.1151      
    ## CONTALT    0.5119  0.1538  3.3280  0.0009   0.2104  0.8134  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ Duration
  )
```

    ## 
    ## Mixed-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0141 (SE = 0.0105)
    ## tau (square root of estimated tau^2 value):             0.1189
    ## I^2 (residual heterogeneity / unaccounted variability): 59.11%
    ## H^2 (unaccounted variability / sampling variability):   2.45
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 16) = 38.0725, p-val = 0.0015
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.5737, p-val = 0.4488
    ## 
    ## Model Results:
    ## 
    ##           estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    -0.0047  0.1165  -0.0407  0.9675  -0.2330  0.2235    
    ## Duration    0.0949  0.1253   0.7574  0.4488  -0.1507  0.3406    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ scale(Hours)
  )
```

    ## 
    ## Mixed-Effects Model (k = 17; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0121 (SE = 0.0099)
    ## tau (square root of estimated tau^2 value):             0.1099
    ## I^2 (residual heterogeneity / unaccounted variability): 53.46%
    ## H^2 (unaccounted variability / sampling variability):   2.15
    ## R^2 (amount of heterogeneity accounted for):            8.54%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 15) = 34.3860, p-val = 0.0030
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.9134, p-val = 0.3392
    ## 
    ## Model Results:
    ## 
    ##               estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt         0.0692  0.0421  1.6413  0.1007  -0.0134  0.1517    
    ## scale(Hours)    0.0419  0.0438  0.9557  0.3392  -0.0440  0.1277    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ design
  )
```

    ## 
    ## Mixed-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0142 (SE = 0.0105)
    ## tau (square root of estimated tau^2 value):             0.1194
    ## I^2 (residual heterogeneity / unaccounted variability): 59.24%
    ## H^2 (unaccounted variability / sampling variability):   2.45
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 16) = 37.1127, p-val = 0.0020
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.2458, p-val = 0.2644
    ## 
    ## Model Results:
    ## 
    ##            estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt      0.1070  0.0507   2.1113  0.0347   0.0077  0.2063  * 
    ## designQED   -0.1078  0.0965  -1.1162  0.2644  -0.2970  0.0815    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ grade
  )
```

    ## 
    ## Mixed-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0138 (SE = 0.0100)
    ## tau (square root of estimated tau^2 value):             0.1175
    ## I^2 (residual heterogeneity / unaccounted variability): 61.00%
    ## H^2 (unaccounted variability / sampling variability):   2.56
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 15) = 38.5738, p-val = 0.0007
    ## 
    ## Test of Moderators (coefficients 2:3):
    ## QM(df = 2) = 0.5876, p-val = 0.7454
    ## 
    ## Model Results:
    ## 
    ##            estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt      0.0701  0.0465  1.5055  0.1322  -0.0212  0.1613    
    ## gradeK-2     0.0018  0.1335  0.0134  0.9893  -0.2599  0.2635    
    ## gradeBoth    0.1664  0.2174  0.7653  0.4441  -0.2597  0.5924    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ grouping
  )
```

    ## 
    ## Mixed-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0241 (SE = 0.0166)
    ## tau (square root of estimated tau^2 value):             0.1551
    ## I^2 (residual heterogeneity / unaccounted variability): 69.16%
    ## H^2 (unaccounted variability / sampling variability):   3.24
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 14) = 34.6689, p-val = 0.0016
    ## 
    ## Test of Moderators (coefficients 2:4):
    ## QM(df = 3) = 3.0077, p-val = 0.3904
    ## 
    ## Model Results:
    ## 
    ##                      estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt                0.0611  0.0820   0.7450  0.4562  -0.0997  0.2219    
    ## groupingSmall group    0.0108  0.1237   0.0871  0.9306  -0.2316  0.2532    
    ## groupingWhole class   -0.0416  0.1356  -0.3069  0.7589  -0.3074  0.2242    
    ## groupingIndividual     0.2828  0.1821   1.5532  0.1204  -0.0741  0.6397    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TMULT
  )
```

    ## 
    ## Mixed-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0137 (SE = 0.0101)
    ## tau (square root of estimated tau^2 value):             0.1172
    ## I^2 (residual heterogeneity / unaccounted variability): 59.24%
    ## H^2 (unaccounted variability / sampling variability):   2.45
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 16) = 39.1185, p-val = 0.0010
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.2690, p-val = 0.6040
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0675  0.0464  1.4549  0.1457  -0.0234  0.1585    
    ## TMULT      0.0612  0.1180  0.5187  0.6040  -0.1701  0.2926    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TVOC
  )
```

    ## 
    ## Mixed-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0124 (SE = 0.0092)
    ## tau (square root of estimated tau^2 value):             0.1113
    ## I^2 (residual heterogeneity / unaccounted variability): 57.54%
    ## H^2 (unaccounted variability / sampling variability):   2.36
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 16) = 39.3502, p-val = 0.0010
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0155, p-val = 0.9009
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.1049  0.2346   0.4472  0.6548  -0.3549  0.5646    
    ## TVOC      -0.0297  0.2383  -0.1245  0.9009  -0.4968  0.4374    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TSYN
  )
```

    ## 
    ## Mixed-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0137 (SE = 0.0101)
    ## tau (square root of estimated tau^2 value):             0.1172
    ## I^2 (residual heterogeneity / unaccounted variability): 59.24%
    ## H^2 (unaccounted variability / sampling variability):   2.45
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 16) = 39.1185, p-val = 0.0010
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.2690, p-val = 0.6040
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0675  0.0464  1.4549  0.1457  -0.0234  0.1585    
    ## TSYN       0.0612  0.1180  0.5187  0.6040  -0.1701  0.2926    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TMOR
  )
```

    ## 
    ## Mixed-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0098 (SE = 0.0080)
    ## tau (square root of estimated tau^2 value):             0.0989
    ## I^2 (residual heterogeneity / unaccounted variability): 51.12%
    ## H^2 (unaccounted variability / sampling variability):   2.05
    ## R^2 (amount of heterogeneity accounted for):            14.69%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 16) = 37.5819, p-val = 0.0017
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.4522, p-val = 0.2282
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0576  0.0413  1.3948  0.1631  -0.0233  0.1385    
    ## TMOR       0.1456  0.1208  1.2051  0.2282  -0.0912  0.3825    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TLC
  )
```

    ## 
    ## Mixed-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0156 (SE = 0.0111)
    ## tau (square root of estimated tau^2 value):             0.1249
    ## I^2 (residual heterogeneity / unaccounted variability): 61.87%
    ## H^2 (unaccounted variability / sampling variability):   2.62
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 16) = 38.6395, p-val = 0.0012
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.3713, p-val = 0.5423
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0970  0.0541   1.7933  0.0729  -0.0090  0.2031  . 
    ## TLC       -0.0574  0.0943  -0.6094  0.5423  -0.2422  0.1273    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TRC
  )
```

    ## 
    ## Mixed-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0143 (SE = 0.0106)
    ## tau (square root of estimated tau^2 value):             0.1196
    ## I^2 (residual heterogeneity / unaccounted variability): 57.58%
    ## H^2 (unaccounted variability / sampling variability):   2.36
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 16) = 37.1187, p-val = 0.0020
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.0977, p-val = 0.2948
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.1170  0.0574   2.0375  0.0416   0.0044  0.2295  * 
    ## TRC       -0.0913  0.0871  -1.0477  0.2948  -0.2620  0.0795    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TPAD
  )
```

    ## 
    ## Mixed-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0115 (SE = 0.0088)
    ## tau (square root of estimated tau^2 value):             0.1073
    ## I^2 (residual heterogeneity / unaccounted variability): 55.47%
    ## H^2 (unaccounted variability / sampling variability):   2.25
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 16) = 38.6193, p-val = 0.0012
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.6286, p-val = 0.4279
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0669  0.0420  1.5931  0.1111  -0.0154  0.1493    
    ## TPAD       0.1279  0.1613  0.7929  0.4279  -0.1883  0.4441    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TDD
  )
```

    ## 
    ## Mixed-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0181 (SE = 0.0125)
    ## tau (square root of estimated tau^2 value):             0.1347
    ## I^2 (residual heterogeneity / unaccounted variability): 62.75%
    ## H^2 (unaccounted variability / sampling variability):   2.68
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 16) = 37.7111, p-val = 0.0017
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0677, p-val = 0.7948
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0902  0.0618   1.4605  0.1442  -0.0308  0.2112    
    ## TDD       -0.0243  0.0935  -0.2601  0.7948  -0.2076  0.1590    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TTEC
  )
```

    ## 
    ## Mixed-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0174 (SE = 0.0120)
    ## tau (square root of estimated tau^2 value):             0.1318
    ## I^2 (residual heterogeneity / unaccounted variability): 63.91%
    ## H^2 (unaccounted variability / sampling variability):   2.77
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 16) = 39.3489, p-val = 0.0010
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.4064, p-val = 0.5238
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0604  0.0544  1.1109  0.2666  -0.0462  0.1670    
    ## TTEC       0.0642  0.1006  0.6375  0.5238  -0.1331  0.2614    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df_r %>%
  filter(type == "RS") %>% 
  rma(
    yi = ES, 
    vi = EV, 
    data = ., 
    method = "REML",
    mods = ~ TSTR
  )
```

    ## 
    ## Mixed-Effects Model (k = 18; tau^2 estimator: REML)
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0090 (SE = 0.0079)
    ## tau (square root of estimated tau^2 value):             0.0948
    ## I^2 (residual heterogeneity / unaccounted variability): 47.30%
    ## H^2 (unaccounted variability / sampling variability):   1.90
    ## R^2 (amount of heterogeneity accounted for):            21.70%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 16) = 35.0446, p-val = 0.0039
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.4399, p-val = 0.2302
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0959  0.0420   2.2810  0.0226   0.0135  0.1783  * 
    ## TSTR      -0.1167  0.0973  -1.1999  0.2302  -0.3073  0.0739    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
