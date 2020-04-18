Metaanalysis Script
================
Saurabh Khanna
2020-04-18

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

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
    ## ✓ tibble  3.0.0     ✓ dplyr   0.8.5
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
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

#df_prepost %>% select(type, contains(c("N1p", "N2p"))) %>% summary()

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
df_clean <- 
  bind_rows(df_post, df_prepost) %>% 
  arrange(type, AUTYR) %>% 
  separate(AUTYR, c("stdid", "case"), remove = F, extra = "merge") %>% 
  left_join(
    read_xlsx(data_file, sheet = "StudyChar") %>% 
      drop_na(AUTYR),
    by = "AUTYR"
  ) %>%
  mutate(
    Hours = Hours %>% parse_number()
  ) %>% 
  mutate_at(vars(type, CONT), as.factor) %>% 
  arrange(type, AUTYR) %>% 
  mutate_at(vars(case), replace_na, replace = "Default")

df_clean %>% knitr::kable()
```

| AUTYR             | stdid        | case     | type |          ES |        EV | TMULT | TVOC | TSYN | TMOR | TLC | TRC | TPAD | TDD | TTEC | TSTR | CONT | GradeK | Grade1 | Grade2 | Grade3 | Grade4 | Grade5 | QED | RCT | WSD | WholeCl | SmallGr | Indiv | Duration |  Hours |
| :---------------- | :----------- | :------- | :--: | ----------: | --------: | ----: | ---: | ---: | ---: | --: | --: | ---: | --: | ---: | ---: | :--- | -----: | -----: | -----: | -----: | -----: | -----: | --: | --: | --: | ------: | ------: | ----: | -------: | -----: |
| Jones19\_1        | Jones19      | 1        |  AS  |   0.3371586 | 0.0025073 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      1 |   0 |   1 |   0 |       1 |       1 |     0 |        1 |  95.00 |
| Jones19\_2        | Jones19      | 2        |  AS  |   0.1152593 | 0.0019759 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      1 |   0 |   1 |   0 |       1 |       1 |     0 |        1 |  95.00 |
| Proctor19         | Proctor19    | Default  |  AS  |   0.1139215 | 0.0167638 |     1 |    1 |    1 |    1 |   1 |   1 |    0 |   1 |    1 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      1 |   1 |   0 |   0 |       0 |       1 |     0 |        1 |  20.00 |
| Baker13           | Baker13      | Default  |  LR  |   0.2651806 | 0.0312088 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | ALT  |      0 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  38.00 |
| Connor18\_3\_COM  | Connor18     | 3\_COM   |  LR  |   0.3136444 | 0.0209808 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Connor18\_3\_ERC  | Connor18     | 3\_ERC   |  LR  |   0.0667638 | 0.0201244 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Connor18\_3\_LIM  | Connor18     | 3\_LIM   |  LR  | \-0.1758689 | 0.0198194 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Connor18\_4\_ERC  | Connor18     | 4\_ERC   |  LR  | \-0.0865421 | 0.0178965 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Coyne10           | Coyne10      | Default  |  LR  |   0.3683911 | 0.0357745 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        0 |  18.00 |
| Coyne19           | Coyne19      | Default  |  LR  |   0.4067562 | 0.0025440 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  44.00 |
| Jiang17\_1\_Basic | Jiang17      | 1\_Basic |  LR  |   0.9288143 | 0.0805088 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      0 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Jiang17\_1\_Deep  | Jiang17      | 1\_Deep  |  LR  |   0.6614954 | 0.0675574 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      0 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Jiang17\_2\_Basic | Jiang17      | 2\_Basic |  LR  |   0.1703855 | 0.0859893 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      0 |      0 |      1 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Jiang17\_2\_Deep  | Jiang17      | 2\_Deep  |  LR  |   0.1655324 | 0.0892940 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      0 |      0 |      1 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Jiang17\_3\_Basic | Jiang17      | 3\_Basic |  LR  |   0.3488033 | 0.0863644 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Jiang17\_3\_Deep  | Jiang17      | 3\_Deep  |  LR  |   0.7369940 | 0.1027305 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Jiang17\_K\_Basic | Jiang17      | K\_Basic |  LR  |   0.9437457 | 0.0716192 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Jiang17\_K\_Deep  | Jiang17      | K\_Deep  |  LR  |   0.4473880 | 0.0578489 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Silverman17a\_K   | Silverman17a | K        |  LR  |   0.1225492 | 0.0222134 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    1 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       1 |     0 |        0 |  10.00 |
| Baker13           | Baker13      | Default  |  LS  |   0.0990000 | 0.0241636 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | ALT  |      0 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  38.00 |
| Connor18\_3\_COM  | Connor18     | 3\_COM   |  LS  | \-0.2036594 | 0.0969489 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Connor18\_3\_ERC  | Connor18     | 3\_ERC   |  LS  | \-0.1336838 | 0.1006798 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Connor18\_3\_LIM  | Connor18     | 3\_LIM   |  LS  | \-0.3196310 | 0.0965330 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Connor18\_4\_ERC  | Connor18     | 4\_ERC   |  LS  | \-1.6440105 | 0.0492648 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Nielsen12         | Nielsen12    | Default  |  LS  |   0.5156824 | 0.1941841 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       0 |       1 |     0 |        0 |  18.00 |
| Tong10\_B         | Tong10       | B        |  LS  |   0.3306846 | 0.1133714 |     0 |    1 |    0 |    0 |   1 |   1 |    1 |   0 |    0 |    0 | BAU  |      1 |      1 |      1 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 | 100.00 |
| Tong10\_G         | Tong10       | G        |  LS  |   0.1531120 | 0.1388452 |     0 |    1 |    0 |    0 |   1 |   1 |    1 |   0 |    0 |    0 | BAU  |      1 |      1 |      1 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 | 100.00 |
| Apel14\_1         | Apel14       | 1        |  MR  |   2.4249393 | 0.5364876 |     0 |    0 |    0 |    1 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        0 |  13.00 |
| Apel14\_2         | Apel14       | 2        |  MR  |   0.6689962 | 0.1026682 |     0 |    0 |    0 |    1 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      1 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        0 |  13.00 |
| Apel14\_K         | Apel14       | K        |  MR  |   0.9939616 | 0.1029765 |     0 |    0 |    0 |    1 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        0 |  13.00 |
| Brimo16           | Brimo16      | Default  |  MR  |   0.5860487 | 0.6119338 |     0 |    0 |    0 |    1 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   1 |   0 |   0 |       0 |       1 |     0 |        0 |  12.50 |
| Berry13           | Berry13      | Default  |  RR  | \-0.3571446 | 1.2659224 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   1 |    0 |    1 | ALT  |      0 |      0 |      0 |      1 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        0 |   6.00 |
| Dalton11\_V       | Dalton11     | V        |  RR  |   1.7674223 | 0.1076674 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    1 |    0 | ALT  |      0 |      0 |      0 |      0 |      0 |      1 |   0 |   1 |   0 |       0 |       0 |     1 |        1 |  20.00 |
| Dalton11\_VC      | Dalton11     | VC       |  RR  |   2.0424503 | 0.1344585 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    1 |    0 | ALT  |      0 |      0 |      0 |      0 |      0 |      1 |   0 |   1 |   0 |       0 |       0 |     1 |        1 |  20.00 |
| Graham15          | Graham15     | Default  |  RR  |   0.1519858 | 0.0156276 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    1 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        0 |   9.00 |
| Silverman17a\_4   | Silverman17a | 4        |  RR  |   0.8508342 | 0.0206818 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    1 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   1 |   0 |   0 |       1 |       1 |     0 |        0 |  10.00 |
| VadSanHer15       | VadSanHer15  | Default  |  RR  |   0.2178669 | 0.0033594 |     0 |    1 |    0 |    0 |   0 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      1 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  35.00 |
| Apel14\_1         | Apel14       | 1        |  RS  |   0.3604871 | 0.1141001 |     0 |    0 |    0 |    1 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        0 |  13.00 |
| Apel14\_2         | Apel14       | 2        |  RS  | \-0.0919161 | 0.0850093 |     0 |    0 |    0 |    1 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      1 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        0 |  13.00 |
| Connor18\_3\_COM  | Connor18     | 3\_COM   |  RS  | \-0.0078114 | 0.0211752 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Connor18\_3\_ERC  | Connor18     | 3\_ERC   |  RS  | \-0.0317346 | 0.0205454 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Connor18\_3\_LIM  | Connor18     | 3\_LIM   |  RS  | \-0.0215240 | 0.0201725 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Connor18\_4\_ERC  | Connor18     | 4\_ERC   |  RS  | \-0.0222525 | 0.0180729 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Dalton11\_V       | Dalton11     | V        |  RS  |   1.6876576 | 0.0733381 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    1 |    0 | ALT  |      0 |      0 |      0 |      0 |      0 |      1 |   0 |   1 |   0 |       0 |       0 |     1 |        1 |  20.00 |
| Dalton11\_VC      | Dalton11     | VC       |  RS  |   0.3785614 | 0.0585894 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    1 |    0 | ALT  |      0 |      0 |      0 |      0 |      0 |      1 |   0 |   1 |   0 |       0 |       0 |     1 |        1 |  20.00 |
| Daunic13          | Daunic13     | Default  |  RS  | \-0.2206484 | 0.0877731 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   1 |    0 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       0 |       1 |     0 |        0 |   5.30 |
| Jones19\_1        | Jones19      | 1        |  RS  |   0.0537309 | 0.0018209 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      1 |   0 |   1 |   0 |       1 |       1 |     0 |        1 |  95.00 |
| Jones19\_2        | Jones19      | 2        |  RS  |   0.1929975 | 0.0018416 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      1 |   0 |   1 |   0 |       1 |       1 |     0 |        1 |  95.00 |
| Morris12          | Morris12     | Default  |  RS  |   0.2364356 | 0.0312791 |     1 |    1 |    1 |    1 |   0 |   0 |    1 |   0 |    0 |    1 | ALT  |      0 |      0 |      1 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  70.00 |
| Proctor11         | Proctor11    | Default  |  RS  | \-0.0436775 | 0.0172460 |     1 |    1 |    1 |    0 |   1 |   1 |    0 |   0 |    1 |    0 | BAU  |      0 |      0 |      0 |      0 |      0 |      1 |   1 |   0 |   0 |       0 |       0 |     1 |        1 |  26.67 |
| Proctor19         | Proctor19    | Default  |  RS  |   0.2331418 | 0.0191677 |     1 |    1 |    1 |    1 |   1 |   1 |    0 |   1 |    1 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      1 |   1 |   0 |   0 |       0 |       1 |     0 |        1 |  20.00 |
| Silverman17a\_4   | Silverman17a | 4        |  RS  | \-0.0014823 | 0.0075817 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    1 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   1 |   0 |   0 |       1 |       1 |     0 |        0 |  10.00 |
| Silverman17b\_4   | Silverman17b | 4        |  RS  | \-0.1127521 | 0.0182102 |     0 |    1 |    0 |    0 |   0 |   1 |    0 |   1 |    1 |    1 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   1 |   0 |   0 |       1 |       1 |     0 |        1 |  20.00 |
| Simmons10\_CALT   | Simmons10    | CALT     |  RS  | \-0.0568601 | 0.0067998 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    1 | ALT  |      0 |      0 |      0 |      0 |      1 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  27.00 |
| Simmons10\_CBAU   | Simmons10    | CBAU     |  RS  | \-0.0727496 | 0.0082679 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    1 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  27.00 |
| Tong10\_B         | Tong10       | B        |  RS  |   0.0172740 | 0.0931362 |     0 |    1 |    0 |    0 |   1 |   1 |    1 |   0 |    0 |    0 | BAU  |      1 |      1 |      1 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 | 100.00 |
| Tong10\_G         | Tong10       | G        |  RS  |   0.2822964 | 0.1091847 |     0 |    1 |    0 |    0 |   1 |   1 |    1 |   0 |    0 |    0 | BAU  |      1 |      1 |      1 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 | 100.00 |
| VadSanHer15       | VadSanHer15  | Default  |  RS  |   0.0500486 | 0.0033727 |     0 |    1 |    0 |    0 |   0 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      1 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  35.00 |
| Connor18\_3\_COM  | Connor18     | 3\_COM   |  SS  |   0.1654445 | 0.0269010 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Connor18\_3\_ERC  | Connor18     | 3\_ERC   |  SS  |   0.1207297 | 0.0259816 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Connor18\_3\_LIM  | Connor18     | 3\_LIM   |  SS  | \-0.0761698 | 0.0252357 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Connor18\_4\_ERC  | Connor18     | 4\_ERC   |  SS  | \-0.0217323 | 0.0239139 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Arthur16\_DD\_1   | Arthur16     | DD\_1    |  VR  |   0.3345986 | 0.2034808 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      1 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        0 |   9.00 |
| Arthur16\_DD\_2   | Arthur16     | DD\_2    |  VR  |   0.7966542 | 0.2142997 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      1 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        0 |   9.00 |
| Arthur16\_DD\_3   | Arthur16     | DD\_3    |  VR  |   0.9914873 | 0.2410833 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        0 |   9.00 |
| Arthur16\_DD\_K   | Arthur16     | DD\_K    |  VR  |   0.9653333 | 0.3938890 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        0 |   9.00 |
| Arthur16\_SD\_1   | Arthur16     | SD\_1    |  VR  |   0.4690806 | 0.1998939 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      1 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        0 |   9.00 |
| Arthur16\_SD\_2   | Arthur16     | SD\_2    |  VR  |   1.3681978 | 0.9254794 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      1 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        0 |   9.00 |
| Arthur16\_SD\_3   | Arthur16     | SD\_3    |  VR  |   0.6276522 | 0.2058135 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        0 |   9.00 |
| Arthur16\_SD\_K   | Arthur16     | SD\_K    |  VR  | \-0.8080746 | 0.2482847 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        0 |   9.00 |
| August18\_EM      | August18     | EM       |  VR  |   1.6576141 | 0.0195257 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |      0 |      0 |      1 |      0 |      0 |      0 |   0 |   0 |   1 |       1 |       0 |     0 |        1 |  25.00 |
| August18\_EX      | August18     | EX       |  VR  |   2.6814346 | 0.0322427 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |      0 |      0 |      1 |      0 |      0 |      0 |   0 |   0 |   1 |       1 |       0 |     0 |        1 |  25.00 |
| Baker13           | Baker13      | Default  |  VR  |   1.0516546 | 0.0525750 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | ALT  |      0 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  38.00 |
| Berry13           | Berry13      | Default  |  VR  |   0.8616891 | 2.8024506 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   1 |    0 |    1 | ALT  |      0 |      0 |      0 |      1 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        0 |   6.00 |
| Connor19\_E       | Connor19     | E        |  VR  | \-0.1234557 | 0.0180285 |     1 |    1 |    0 |    1 |   0 |   0 |    0 |   0 |    1 |    1 | BAU  |      0 |      0 |      0 |      1 |      1 |      1 |   0 |   1 |   0 |       0 |       0 |     1 |        0 |   4.50 |
| Connor19\_EBC     | Connor19     | EBC      |  VR  | \-0.2084705 | 0.0203376 |     1 |    1 |    0 |    1 |   0 |   0 |    0 |   1 |    1 |    1 | ALT  |      0 |      0 |      0 |      1 |      1 |      1 |   0 |   1 |   0 |       0 |       1 |     1 |        0 |   4.50 |
| Coyne10           | Coyne10      | Default  |  VR  |   6.9739699 | 0.4738991 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        0 |  18.00 |
| Coyne19           | Coyne19      | Default  |  VR  |   8.0164868 | 0.1170897 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  44.00 |
| Dalton11\_V       | Dalton11     | V        |  VR  |   8.1075333 | 0.4982294 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    1 |    0 | ALT  |      0 |      0 |      0 |      0 |      0 |      1 |   0 |   1 |   0 |       0 |       0 |     1 |        1 |  20.00 |
| Dalton11\_VC      | Dalton11     | VC       |  VR  |   6.8664358 | 0.3943368 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    1 |    0 | ALT  |      0 |      0 |      0 |      0 |      0 |      1 |   0 |   1 |   0 |       0 |       0 |     1 |        1 |  20.00 |
| Filippini12\_VMOR | Filippini12  | VMOR     |  VR  |   0.8227023 | 0.1120031 |     1 |    1 |    0 |    1 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |      0 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        0 |   7.25 |
| Filippini12\_VSEM | Filippini12  | VSEM     |  VR  |   0.3949393 | 0.1020828 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |      0 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        0 |   7.25 |
| Goldstein17       | Goldstein17  | Default  |  VR  |   2.8023543 | 0.1218541 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    1 |    1 | ALT  |      0 |      1 |      1 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       0 |     1 |        1 |  72.00 |
| Graham15          | Graham15     | Default  |  VR  |   1.6509504 | 0.0373717 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    1 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        0 |   9.00 |
| Hassinger15\_CALT | Hassinger15  | CALT     |  VR  |   1.8251531 | 0.1357037 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        0 |  12.00 |
| Hassinger15\_CBAU | Hassinger15  | CBAU     |  VR  |   1.5170007 | 0.1414513 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        0 |  12.00 |
| Jiang17\_1\_Basic | Jiang17      | 1\_Basic |  VR  |   3.4324900 | 0.1150367 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      0 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Jiang17\_1\_Deep  | Jiang17      | 1\_Deep  |  VR  |   3.3127415 | 0.0981635 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      0 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Jiang17\_2\_Basic | Jiang17      | 2\_Basic |  VR  |   1.3461024 | 0.0497289 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      0 |      0 |      1 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Jiang17\_2\_Deep  | Jiang17      | 2\_Deep  |  VR  |   3.1059129 | 0.0925712 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      0 |      0 |      1 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Jiang17\_3\_Basic | Jiang17      | 3\_Basic |  VR  |   2.2761737 | 0.0647487 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Jiang17\_3\_Deep  | Jiang17      | 3\_Deep  |  VR  |   2.9064802 | 0.0856647 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Jiang17\_K\_Basic | Jiang17      | K\_Basic |  VR  |   2.5358793 | 0.0684081 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Jiang17\_K\_Deep  | Jiang17      | K\_Deep  |  VR  |   3.3465493 | 0.0899831 |     1 |    1 |    1 |    1 |   1 |   0 |    0 |   0 |    1 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  50.00 |
| Jones19\_1        | Jones19      | 1        |  VR  |   0.2283174 | 0.0021398 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      1 |   0 |   1 |   0 |       1 |       1 |     0 |        1 |  95.00 |
| Jones19\_2        | Jones19      | 2        |  VR  |   0.1901639 | 0.0020985 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      1 |   0 |   1 |   0 |       1 |       1 |     0 |        1 |  95.00 |
| Mancilla10        | Mancilla10   | Default  |  VR  |   1.0153708 | 0.1377186 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      0 |      1 |   1 |   0 |   0 |       1 |       0 |     0 |        1 |  25.00 |
| McKeown14\_INT    | McKeown14    | INT      |  VR  |   3.5425526 | 0.3173367 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   0 |   1 |       1 |       0 |     0 |        0 |   2.50 |
| McKeown14\_REP    | McKeown14    | REP      |  VR  |   3.0440364 | 0.3385530 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   0 |   1 |       1 |       0 |     0 |        0 |   2.50 |
| Nelson11          | Nelson11     | Default  |  VR  |   0.5405571 | 0.0580920 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  33.00 |
| Neuman18          | Neuman18     | Default  |  VR  |   0.5346265 | 0.0288756 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    1 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  20.00 |
| Nielsen12         | Nielsen12    | Default  |  VR  |   2.7051948 | 0.5095005 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       0 |       1 |     0 |        0 |  18.00 |
| Powell15\_CALT    | Powell15     | CALT     |  VR  | \-0.0634246 | 0.0699734 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |      0 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       0 |     1 |        0 |   3.00 |
| Powell15\_CBAU    | Powell15     | CBAU     |  VR  |   0.5140269 | 0.0703675 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       0 |     1 |        0 |   3.00 |
| Proctor11         | Proctor11    | Default  |  VR  |   0.9242303 | 0.0256500 |     1 |    1 |    1 |    0 |   1 |   1 |    0 |   0 |    1 |    0 | BAU  |      0 |      0 |      0 |      0 |      0 |      1 |   1 |   0 |   0 |       0 |       0 |     1 |        1 |  26.67 |
| Puhalla11         | Puhalla11    | Default  |  VR  |   2.6937576 | 0.5142966 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |      0 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       1 |     0 |        0 |   5.00 |
| Pullen10          | Pullen10     | Default  |  VR  |   0.4786691 | 0.0500405 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |      0 |      1 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       1 |     0 |        0 |   2.00 |
| Silverman17a\_4   | Silverman17a | 4        |  VR  |   0.4220216 | 0.0079434 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    1 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   1 |   0 |   0 |       1 |       1 |     0 |        0 |  10.00 |
| Silverman17a\_K   | Silverman17a | K        |  VR  |   0.3868128 | 0.0091241 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    1 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       1 |     0 |        0 |  10.00 |
| Silverman17b\_4   | Silverman17b | 4        |  VR  |   0.8424442 | 0.0222265 |     0 |    1 |    0 |    0 |   0 |   1 |    0 |   1 |    1 |    1 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   1 |   0 |   0 |       1 |       1 |     0 |        1 |  20.00 |
| Silverman17b\_K   | Silverman17b | K        |  VR  |   1.4421285 | 0.0345476 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   1 |    1 |    1 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       1 |     0 |        1 |  20.00 |
| Simmons10\_CALT   | Simmons10    | CALT     |  VR  |   7.4878594 | 0.1748973 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    1 | ALT  |      0 |      0 |      0 |      0 |      1 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  27.00 |
| Simmons10\_CBAU   | Simmons10    | CBAU     |  VR  |   6.5140063 | 0.1963028 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    1 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  27.00 |
| VadSan15          | VadSan15     | Default  |  VR  |   0.2613394 | 0.1065636 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       0 |     1 |        0 |   1.00 |
| VadSan16          | VadSan16     | Default  |  VR  |   0.3089192 | 0.0572620 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       0 |     1 |        0 |  14.00 |
| VadSanHer15       | VadSanHer15  | Default  |  VR  |   1.2747785 | 0.0053252 |     0 |    1 |    0 |    0 |   0 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      1 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  35.00 |
| VadSanNel15       | VadSanNel15  | Default  |  VR  |   1.2277100 | 0.0464508 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |      1 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  40.00 |
| Wood18            | Wood18       | Default  |  VR  |   0.2654639 | 0.0236715 |     1 |    1 |    0 |    1 |   0 |   0 |    0 |   0 |    1 |    0 | ALT  |      1 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  30.00 |
| Wright17\_U1      | Wright17     | U1       |  VR  |   1.6948761 | 0.0626681 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   1 |    0 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        0 |  15.00 |
| Wright17\_U2      | Wright17     | U2       |  VR  |   1.3191358 | 0.0663016 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   1 |    0 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        0 |  15.00 |
| Zipoli11\_EmRev   | Zipoli11     | EmRev    |  VR  |         NaN |       NaN |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   0 |   1 |       1 |       1 |     0 |        0 |  18.00 |
| Zipoli11\_SemRev  | Zipoli11     | SemRev   |  VR  |         NaN |       NaN |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   0 |   1 |       1 |       1 |     0 |        0 |  18.00 |
| Connor18\_3\_COM  | Connor18     | 3\_COM   |  VS  | \-0.1258972 | 0.0916727 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Connor18\_3\_ERC  | Connor18     | 3\_ERC   |  VS  |   0.1150151 | 0.0977410 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Connor18\_3\_LIM  | Connor18     | 3\_LIM   |  VS  | \-0.0385816 | 0.0940984 |     1 |    1 |    1 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      1 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Connor18\_4\_ERC  | Connor18     | 4\_ERC   |  VS  |   0.1197656 | 0.0951981 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  22.00 |
| Coyne10           | Coyne10      | Default  |  VS  |   0.1061789 | 0.0361622 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        0 |  18.00 |
| Coyne19           | Coyne19      | Default  |  VS  |   0.1034697 | 0.0049060 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  44.00 |
| Dalton11\_V       | Dalton11     | V        |  VS  |   0.0052469 | 0.0540938 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    1 |    0 | ALT  |      0 |      0 |      0 |      0 |      0 |      1 |   0 |   1 |   0 |       0 |       0 |     1 |        1 |  20.00 |
| Dalton11\_VC      | Dalton11     | VC       |  VS  |   0.1996593 | 0.0578505 |     0 |    1 |    0 |    0 |   1 |   1 |    0 |   0 |    1 |    0 | ALT  |      0 |      0 |      0 |      0 |      0 |      1 |   0 |   1 |   0 |       0 |       0 |     1 |        1 |  20.00 |
| Daunic13          | Daunic13     | Default  |  VS  |   0.0580749 | 0.0898036 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   1 |    0 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       0 |       1 |     0 |        0 |   5.30 |
| Hassinger15\_CALT | Hassinger15  | CALT     |  VS  |   0.5506831 | 0.0694178 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | ALT  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        0 |  12.00 |
| Hassinger15\_CBAU | Hassinger15  | CBAU     |  VS  |   0.4164971 | 0.0732284 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        0 |  12.00 |
| Huang15           | Huang15      | Default  |  VS  |   0.4786637 | 0.1040172 |     1 |    1 |    1 |    0 |   0 |   0 |    0 |   0 |    1 |    0 | ALT  |      0 |      0 |      1 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       0 |     0 |        1 |  80.00 |
| Nelson11          | Nelson11     | Default  |  VS  | \-0.4798887 | 0.0317414 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  33.00 |
| Neuman18          | Neuman18     | Default  |  VS  | \-0.0712942 | 0.0256859 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    1 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  20.00 |
| Nielsen12         | Nielsen12    | Default  |  VS  |   0.1435089 | 0.2058622 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   0 |    0 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       0 |       1 |     0 |        0 |  18.00 |
| Proctor11         | Proctor11    | Default  |  VS  |   0.0343903 | 0.0171422 |     1 |    1 |    1 |    0 |   1 |   1 |    0 |   0 |    1 |    0 | BAU  |      0 |      0 |      0 |      0 |      0 |      1 |   1 |   0 |   0 |       0 |       0 |     1 |        1 |  26.67 |
| Silverman17a\_K   | Silverman17a | K        |  VS  | \-0.0393292 | 0.0079704 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   1 |    1 |    0 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       1 |     0 |        0 |  10.00 |
| Silverman17b\_4   | Silverman17b | 4        |  VS  |   0.1398774 | 0.0182383 |     0 |    1 |    0 |    0 |   0 |   1 |    0 |   1 |    1 |    1 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   1 |   0 |   0 |       1 |       1 |     0 |        1 |  20.00 |
| Silverman17b\_K   | Silverman17b | K        |  VS  | \-0.1159971 | 0.0215981 |     0 |    1 |    0 |    0 |   1 |   0 |    0 |   1 |    1 |    1 | BAU  |      1 |      0 |      0 |      0 |      0 |      0 |   1 |   0 |   0 |       1 |       1 |     0 |        1 |  20.00 |
| Simmons10\_CALT   | Simmons10    | CALT     |  VS  | \-0.0004577 | 0.0065726 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    1 | ALT  |      0 |      0 |      0 |      0 |      1 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  27.00 |
| Simmons10\_CBAU   | Simmons10    | CBAU     |  VS  | \-0.0208612 | 0.0079782 |     0 |    1 |    0 |    0 |   0 |   0 |    0 |   0 |    0 |    1 | BAU  |      0 |      0 |      0 |      0 |      1 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  27.00 |
| Tong10\_B         | Tong10       | B        |  VS  |   0.6892086 | 0.1115444 |     0 |    1 |    0 |    0 |   1 |   1 |    1 |   0 |    0 |    0 | BAU  |      1 |      1 |      1 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 | 100.00 |
| Tong10\_G         | Tong10       | G        |  VS  |   0.3193906 | 0.1387398 |     0 |    1 |    0 |    0 |   1 |   1 |    1 |   0 |    0 |    0 | BAU  |      1 |      1 |      1 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 | 100.00 |
| VadSan16          | VadSan16     | Default  |  VS  |   0.2884231 | 0.0438190 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |      1 |      0 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       0 |     1 |        0 |  14.00 |
| VadSanHer15       | VadSanHer15  | Default  |  VS  |   0.0648028 | 0.0033742 |     0 |    1 |    0 |    0 |   0 |   1 |    0 |   0 |    0 |    0 | BAU  |      0 |      0 |      0 |      0 |      1 |      1 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  35.00 |
| VadSanNel15       | VadSanNel15  | Default  |  VS  |   0.0111751 | 0.0142347 |     0 |    1 |    0 |    0 |   0 |   0 |    1 |   0 |    0 |    0 | ALT  |      1 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       0 |       1 |     0 |        1 |  40.00 |
| Wood18            | Wood18       | Default  |  VS  |   0.1214477 | 0.0151603 |     1 |    1 |    0 |    1 |   0 |   0 |    0 |   0 |    1 |    0 | ALT  |      1 |      1 |      0 |      0 |      0 |      0 |   0 |   1 |   0 |       1 |       0 |     0 |        1 |  30.00 |

### Summary stats

``` r
df_clean %>% count(AUTYR) %>% count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    74

``` r
df_clean %>% count(stdid) %>% count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    41

``` r
df_clean %>% 
  group_by(type) %>%
  summarize(n = n_distinct(stdid))
```

    ## # A tibble: 9 x 2
    ##   type      n
    ##   <fct> <int>
    ## 1 AS        2
    ## 2 LR        6
    ## 3 LS        4
    ## 4 MR        2
    ## 5 RR        5
    ## 6 RS       13
    ## 7 SS        1
    ## 8 VR       33
    ## 9 VS       19

``` r
df_v <- df_clean %>% filter(type %in% c("VR", "VS"))
df_l <- df_clean %>% filter(type %in% c("LR", "LS"))
df_r <- df_clean %>% filter(type %in% c("RR", "RS"))
df_m <- df_clean %>% filter(type %in% c("MR", "MS"))
df_s <- df_clean %>% filter(type %in% c("SR", "SS"))
df_a <- df_clean %>% filter(type == "AS")

rm(df_post, df_prepost)
```

## Synthesizing effect sizes

### Vocabulary

``` r
robu(
  formula = ES ~ 1, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_v, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ 1 
    ## 
    ## Number of studies = 36 
    ## Number of outcomes = 85 (min = 1 , mean = 2.36 , median = 2 , max = 8 )
    ## Rho = 0.5 
    ## I.sq = 97.53935 
    ## Tau.sq = 1.183327 
    ## 
    ##                Estimate StdErr t-value  dfs    P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.     1.18  0.207    5.69 34.3 0.00000211    0.758      1.6 ***
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
#forest.robu(es.lab = "stdid", study.lab = "stdid", "Effect Size" = ES)

rma(
  yi = ES, 
  vi = EV, 
  data = df_v, 
  method = "REML",
  slab = AUTYR
) %>% 
  forest(
    order = "obs",
    xlab = "Vocabulary",
    addcred = T, 
    header = T
  )
```

<img src="analysis_files/figure-gfm/unnamed-chunk-7-1.png" width="100%" height="100%" />

### Listening Comprehension

``` r
robu(
  formula = ES ~ 1, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_l, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ 1 
    ## 
    ## Number of studies = 8 
    ## Number of outcomes = 24 (min = 1 , mean = 3 , median = 1.5 , max = 8 )
    ## Rho = 0.5 
    ## I.sq = 64.39267 
    ## Tau.sq = 0.04988895 
    ## 
    ##                Estimate StdErr t-value  dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.     0.25 0.0874    2.86 5.79    0.03   0.0341    0.466  **
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
#forest.robu(es.lab = "stdid", study.lab = "stdid", "Effect Size" = ES)

rma(
  yi = ES, 
  vi = EV, 
  data = df_l, 
  method = "REML",
  slab = AUTYR
) %>% 
  forest(
    order = "obs",
    xlab = "Listening Comprehension",
    addcred = T, 
    header = T
  )
```

<img src="analysis_files/figure-gfm/unnamed-chunk-8-1.png" width="100%" height="100%" />

### Reading Comprehension

``` r
robu(
  formula = ES ~ 1, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_r, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ 1 
    ## 
    ## Number of studies = 15 
    ## Number of outcomes = 27 (min = 1 , mean = 1.8 , median = 2 , max = 4 )
    ## Rho = 0.5 
    ## I.sq = 76.91086 
    ## Tau.sq = 0.04317138 
    ## 
    ##                Estimate StdErr t-value  dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.    0.142 0.0672    2.11 11.5  0.0578 -0.00551    0.289   *
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
#forest.robu(es.lab = "stdid", study.lab = "stdid", "Effect Size" = ES)

rma(
  yi = ES, 
  vi = EV, 
  data = df_r, 
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

<img src="analysis_files/figure-gfm/unnamed-chunk-9-1.png" width="100%" height="100%" />

### Morphology

``` r
robu(
  formula = ES ~ 1, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_m, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ 1 
    ## 
    ## Number of studies = 2 
    ## Number of outcomes = 4 (min = 1 , mean = 2 , median = 2 , max = 3 )
    ## Rho = 0.5 
    ## I.sq = 59.48202 
    ## Tau.sq = 0.7804755 
    ## 
    ##                Estimate StdErr t-value dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.     1.03  0.384    2.69   1   0.227    -3.84     5.91    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
#forest.robu(es.lab = "stdid", study.lab = "stdid", "Effect Size" = ES)

rma(
  yi = ES, 
  vi = EV, 
  data = df_m, 
  method = "REML",
  slab = AUTYR
) %>% 
  forest(
    order = "obs",
    xlab = "Morphology",
    addcred = T, 
    header = T
  )
```

![](analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Syntax

``` r
# robu(
#   formula = ES ~ 1, 
#   var.eff.size = EV, 
#   studynum = stdid,
#   data = df_s, 
#   modelweights = "CORR",
#   rho = 0.5
# )
#forest.robu(es.lab = "stdid", study.lab = "stdid", "Effect Size" = ES)

rma(
  yi = ES, 
  vi = EV, 
  data = df_s, 
  method = "REML",
  slab = AUTYR
) %>% 
  forest(
    order = "obs",
    xlab = "Syntax",
    addcred = T, 
    header = T
  )
```

![](analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### AS Studies

``` r
robu(
  formula = ES ~ 1, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_a, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ 1 
    ## 
    ## Number of studies = 2 
    ## Number of outcomes = 3 (min = 1 , mean = 1.5 , median = 1.5 , max = 2 )
    ## Rho = 0.5 
    ## I.sq = 80.17026 
    ## Tau.sq = 0.04689072 
    ## 
    ##                Estimate StdErr t-value dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.    0.177 0.0557    3.18   1   0.194    -0.53    0.885    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
#forest.robu(es.lab = "stdid", study.lab = "stdid", "Effect Size" = ES)

rma(
  yi = ES, 
  vi = EV, 
  data = df_a, 
  method = "REML",
  slab = AUTYR
) %>% 
  forest(
    order = "obs",
    xlab = "AS Studies",
    addcred = T, 
    header = T
  )
```

![](analysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Moderator effects

### Vocabulary

Checking variable correlations:

``` r
p.mat <- ggcorrplot::cor_pmat(df_v %>% select(TMULT:TSTR, GradeK:Hours))
df_v %>% 
  select(TMULT:TSTR, GradeK:Hours) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(type = "lower", p.mat = p.mat, insig = "blank")
```

![](analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Meta-regressions:

``` r
# Custom vs Std
robu(
  formula = ES ~ type, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_v, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ type 
    ## 
    ## Number of studies = 36 
    ## Number of outcomes = 85 (min = 1 , mean = 2.36 , median = 2 , max = 8 )
    ## Rho = 0.5 
    ## I.sq = 97.35011 
    ## Tau.sq = 1.151654 
    ## 
    ##                Estimate StdErr t-value  dfs    P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.     1.68  0.311     5.4 27.7 0.00000958     1.04     2.32 ***
    ## 2       typeVS    -1.55  0.316    -4.9 25.8 0.00004425    -2.20    -0.90 ***
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
# Single vs Multiple
robu(
  formula = ES ~ TMULT, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_v, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ TMULT 
    ## 
    ## Number of studies = 36 
    ## Number of outcomes = 85 (min = 1 , mean = 2.36 , median = 2 , max = 8 )
    ## Rho = 0.5 
    ## I.sq = 97.57653 
    ## Tau.sq = 1.250312 
    ## 
    ##                Estimate StdErr t-value  dfs    P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.    1.296  0.237    5.47 28.1 0.00000758    0.811    1.781 ***
    ## 2        TMULT   -0.599  0.448   -1.34  9.3 0.21272138   -1.607    0.409    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
# Hours
robu(
  formula = ES ~ scale(Hours), 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_v, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ scale(Hours) 
    ## 
    ## Number of studies = 36 
    ## Number of outcomes = 85 (min = 1 , mean = 2.36 , median = 2 , max = 8 )
    ## Rho = 0.5 
    ## I.sq = 97.52379 
    ## Tau.sq = 1.536479 
    ## 
    ##                Estimate StdErr t-value  dfs    P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.   1.1849  0.209   5.657 33.5 0.00000253    0.759    1.611 ***
    ## 2 scale.Hours.   0.0474  0.181   0.262  6.3 0.80197707   -0.391    0.486    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
# Control group
robu(
  formula = ES ~ factor(CONT), 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_v, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ factor(CONT) 
    ## 
    ## Number of studies = 36 
    ## Number of outcomes = 85 (min = 1 , mean = 2.36 , median = 2 , max = 8 )
    ## Rho = 0.5 
    ## I.sq = 97.47389 
    ## Tau.sq = 1.209645 
    ## 
    ##                   Estimate StdErr t-value  dfs  P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1    X.Intercept.    1.407  0.330    4.27 17.6 0.000484    0.714    2.101 ***
    ## 2 factor.CONT.BAU   -0.445  0.382   -1.16 33.5 0.252854   -1.222    0.333    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
# Design
robu(
  formula = ES ~ RCT + WSD, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_v, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ RCT + WSD 
    ## 
    ## Number of studies = 36 
    ## Number of outcomes = 85 (min = 1 , mean = 2.36 , median = 2 , max = 8 )
    ## Rho = 0.5 
    ## I.sq = 97.42449 
    ## Tau.sq = 1.239853 
    ## 
    ##                Estimate StdErr t-value   dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.    0.907  0.267    3.39 10.49 0.00639    0.316     1.50 ***
    ## 2          RCT    0.282  0.391    0.72 21.00 0.47970   -0.532     1.10    
    ## 3          WSD    1.764  0.619    2.85  1.37 0.15762   -2.507     6.04    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
# Grade
robu(
  formula = ES ~ GradeK + Grade1 + Grade2 + Grade3 + Grade4, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_v, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ GradeK + Grade1 + Grade2 + Grade3 + Grade4 
    ## 
    ## Number of studies = 36 
    ## Number of outcomes = 85 (min = 1 , mean = 2.36 , median = 2 , max = 8 )
    ## Rho = 0.5 
    ## I.sq = 97.54135 
    ## Tau.sq = 1.396483 
    ## 
    ##                Estimate StdErr t-value   dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.    1.513  0.485   3.120  9.99  0.0109    0.432    2.594  **
    ## 2       GradeK   -0.325  0.487  -0.667 15.06  0.5150   -1.363    0.713    
    ## 3       Grade1   -0.424  0.447  -0.948 12.96  0.3602   -1.390    0.542    
    ## 4       Grade2    0.336  0.656   0.512  5.59  0.6285   -1.299    1.971    
    ## 5       Grade3   -0.333  0.815  -0.408  5.30  0.6991   -2.394    1.728    
    ## 6       Grade4   -0.449  0.663  -0.678 13.29  0.5095   -1.877    0.979    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

### Listening Comprehension

Checking variable correlations:

``` r
p.mat <- ggcorrplot::cor_pmat(df_l %>% select(TMULT:TSTR, GradeK:Hours))
df_l %>% 
  select(TMULT:TSTR, GradeK:Hours) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(type = "lower", p.mat = p.mat, insig = "blank")
```

![](analysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Meta-regressions:

``` r
# Custom vs Std
robu(
  formula = ES ~ type, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_l, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ type 
    ## 
    ## Number of studies = 8 
    ## Number of outcomes = 24 (min = 1 , mean = 3 , median = 1.5 , max = 8 )
    ## Rho = 0.5 
    ## I.sq = 57.48429 
    ## Tau.sq = 0.03938658 
    ## 
    ##                Estimate StdErr t-value  dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.    0.313 0.0701    4.46 3.85  0.0122    0.115    0.510  **
    ## 2       typeLS   -0.257 0.1964   -1.31 4.24  0.2569   -0.791    0.276    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
# Single vs Multiple
robu(
  formula = ES ~ TMULT, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_l, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ TMULT 
    ## 
    ## Number of studies = 8 
    ## Number of outcomes = 24 (min = 1 , mean = 3 , median = 1.5 , max = 8 )
    ## Rho = 0.5 
    ## I.sq = 66.75497 
    ## Tau.sq = 0.05591496 
    ## 
    ##                Estimate StdErr t-value  dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.    0.234 0.0824   2.836 4.92  0.0371   0.0208    0.447  **
    ## 2        TMULT    0.126 0.3233   0.391 1.28  0.7510  -2.3476    2.600    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
# Hours
robu(
  formula = ES ~ scale(Hours), 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_l, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ scale(Hours) 
    ## 
    ## Number of studies = 8 
    ## Number of outcomes = 24 (min = 1 , mean = 3 , median = 1.5 , max = 8 )
    ## Rho = 0.5 
    ## I.sq = 62.89634 
    ## Tau.sq = 0.05283849 
    ## 
    ##                Estimate StdErr t-value  dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.   0.2633 0.0858   3.070 4.51  0.0318   0.0355    0.491  **
    ## 2 scale.Hours.   0.0676 0.0853   0.792 2.10  0.5078  -0.2830    0.418    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
# Control group
robu(
  formula = ES ~ factor(CONT), 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_l, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ factor(CONT) 
    ## 
    ## Number of studies = 8 
    ## Number of outcomes = 24 (min = 1 , mean = 3 , median = 1.5 , max = 8 )
    ## Rho = 0.5 
    ## I.sq = 61.77399 
    ## Tau.sq = 0.06434803 
    ## 
    ##                   Estimate StdErr t-value  dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1    X.Intercept.   0.3122  0.111    2.81 1.00   0.217   -1.097    1.722    
    ## 2 factor.CONT.BAU  -0.0989  0.168   -0.59 2.27   0.609   -0.744    0.546    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
# Design
robu(
  formula = ES ~ RCT, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_l, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ RCT 
    ## 
    ## Number of studies = 8 
    ## Number of outcomes = 24 (min = 1 , mean = 3 , median = 1.5 , max = 8 )
    ## Rho = 0.5 
    ## I.sq = 67.65557 
    ## Tau.sq = 0.07169885 
    ## 
    ##                Estimate StdErr t-value  dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.   0.2814  0.113   2.488 1.66   0.156   -0.315    0.878    
    ## 2          RCT  -0.0507  0.176  -0.288 3.48   0.789   -0.569    0.467    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
# Grade
robu(
  formula = ES ~ GradeK + Grade1 + Grade2 + Grade3 + Grade4, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_l, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ GradeK + Grade1 + Grade2 + Grade3 + Grade4 
    ## 
    ## Number of studies = 8 
    ## Number of outcomes = 24 (min = 1 , mean = 3 , median = 1.5 , max = 8 )
    ## Rho = 0.5 
    ## I.sq = 51.67547 
    ## Tau.sq = 0.03333617 
    ## 
    ##                Estimate StdErr t-value  dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.  0.25971  0.109   2.391 1.58  0.1719   -0.350    0.869    
    ## 2       GradeK  0.07897  0.105   0.753 1.58  0.5473   -0.509    0.667    
    ## 3       Grade1 -0.00503  0.105  -0.048 1.58  0.9671   -0.593    0.583    
    ## 4       Grade2 -0.09175  0.109  -0.845 1.58  0.5069   -0.701    0.518    
    ## 5       Grade3 -0.21037  0.254  -0.829 1.37  0.5268   -1.960    1.539    
    ## 6       Grade4 -1.12499  0.109 -10.356 1.58  0.0194   -1.735   -0.515  **
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

### Reading Comprehension

Checking variable correlations:

``` r
p.mat <- ggcorrplot::cor_pmat(df_r %>% select(TMULT:TSTR, GradeK:Hours))
df_r %>% 
  select(TMULT:TSTR, GradeK:Hours) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(type = "lower", p.mat = p.mat, insig = "blank")
```

![](analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Meta-regressions:

``` r
# Custom vs Std
robu(
  formula = ES ~ type, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_r, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ type 
    ## 
    ## Number of studies = 15 
    ## Number of outcomes = 27 (min = 1 , mean = 1.8 , median = 2 , max = 4 )
    ## Rho = 0.5 
    ## I.sq = 71.8202 
    ## Tau.sq = 0.03524594 
    ## 
    ##                Estimate StdErr t-value  dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.    0.455  0.240    1.90 2.42   0.175   -0.422    1.331    
    ## 2       typeRS   -0.395  0.234   -1.69 3.59   0.175   -1.076    0.286    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
# Single vs Multiple
robu(
  formula = ES ~ TMULT, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_r, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ TMULT 
    ## 
    ## Number of studies = 15 
    ## Number of outcomes = 27 (min = 1 , mean = 1.8 , median = 2 , max = 4 )
    ## Rho = 0.5 
    ## I.sq = 78.39222 
    ## Tau.sq = 0.0473733 
    ## 
    ##                Estimate StdErr t-value  dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.    0.150 0.0876    1.71 8.63   0.123  -0.0498    0.349    
    ## 2        TMULT   -0.027 0.1230   -0.22 4.12   0.836  -0.3646    0.311    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
# Hours
robu(
  formula = ES ~ scale(Hours), 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_r, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ scale(Hours) 
    ## 
    ## Number of studies = 15 
    ## Number of outcomes = 27 (min = 1 , mean = 1.8 , median = 2 , max = 4 )
    ## Rho = 0.5 
    ## I.sq = 78.19902 
    ## Tau.sq = 0.06370063 
    ## 
    ##                Estimate StdErr t-value  dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.  0.14777 0.0752  1.9644 11.2  0.0747  -0.0174    0.313   *
    ## 2 scale.Hours. -0.00422 0.0430 -0.0982  2.9  0.9282  -0.1437    0.135    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
# Control group
robu(
  formula = ES ~ factor(CONT), 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_r, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ factor(CONT) 
    ## 
    ## Number of studies = 15 
    ## Number of outcomes = 27 (min = 1 , mean = 1.8 , median = 2 , max = 4 )
    ## Rho = 0.5 
    ## I.sq = 77.81854 
    ## Tau.sq = 0.04614438 
    ## 
    ##                   Estimate StdErr t-value  dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1    X.Intercept.    0.424  0.353   1.201 2.03   0.351    -1.07    1.921    
    ## 2 factor.CONT.BAU   -0.332  0.351  -0.945 2.78   0.420    -1.50    0.839    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
# Design
robu(
  formula = ES ~ RCT, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_r, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ RCT 
    ## 
    ## Number of studies = 15 
    ## Number of outcomes = 27 (min = 1 , mean = 1.8 , median = 2 , max = 4 )
    ## Rho = 0.5 
    ## I.sq = 78.36818 
    ## Tau.sq = 0.04891271 
    ## 
    ##                Estimate StdErr t-value  dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.   0.0865  0.118   0.734 3.85   0.505   -0.245    0.418    
    ## 2          RCT   0.0899  0.150   0.601 8.06   0.564   -0.254    0.434    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results

``` r
# Grade
robu(
  formula = ES ~ GradeK + Grade1 + Grade2 + Grade3 + Grade4, 
  var.eff.size = EV, 
  studynum = stdid,
  data = df_r, 
  modelweights = "CORR",
  rho = 0.5
)
```

    ## RVE: Correlated Effects Model with Small-Sample Corrections 
    ## 
    ## Model: ES ~ GradeK + Grade1 + Grade2 + Grade3 + Grade4 
    ## 
    ## Number of studies = 15 
    ## Number of outcomes = 27 (min = 1 , mean = 1.8 , median = 2 , max = 4 )
    ## Rho = 0.5 
    ## I.sq = 82.53889 
    ## Tau.sq = 0.05220973 
    ## 
    ##                Estimate StdErr t-value  dfs P(|t|>) 95% CI.L 95% CI.U Sig
    ## 1 X.Intercept.   0.3437  0.480   0.716 1.71   0.559    -2.10     2.79    
    ## 2       GradeK  -0.4983  0.393  -1.269 1.35   0.380    -3.25     2.25    
    ## 3       Grade1   0.1601  0.236   0.679 1.24   0.601    -1.75     2.07    
    ## 4       Grade2   0.0719  0.228   0.315 1.34   0.794    -1.55     1.69    
    ## 5       Grade3  -0.2794  0.482  -0.580 2.18   0.616    -2.20     1.64    
    ## 6       Grade4  -0.2231  0.484  -0.461 2.53   0.681    -1.94     1.49    
    ## ---
    ## Signif. codes: < .01 *** < .05 ** < .10 *
    ## ---
    ## Note: If df < 4, do not trust the results
