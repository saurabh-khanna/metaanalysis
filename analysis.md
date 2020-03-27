L\&L Metaanalysis
================
Saurabh Khanna
2020-03-26

  - [Reading in data](#reading-in-data)
  - [Calculate effect sizes](#calculate-effect-sizes)
      - [R studies](#r-studies)

``` r
# Libraries
library(tidyverse)
library(metafor)
library(readxl)

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

    ## New names:
    ## * `` -> ...40

    ## # A tibble: 0 x 1
    ## # … with 1 variable: AUTYR <chr>

All good\!

## Calculate effect sizes

### R studies

#### RR studies

``` r
df_rr_post <-
  read_xlsx(data_file, sheet = "RR") %>% 
  rename_at(vars(-AUTYR), ~ str_replace(., "RR", "")) %>% 
  filter(is.na(T1M1pre), is.na(T1S1pre)) %>%
  select_if(~ any(!is.na(.))) %>%
  select(AUTYR, sort(current_vars()))
```

    ## Warning: current_vars() is deprecated. 
    ## Please use tidyselect::peek_vars() instead
    ## This warning is displayed once per session.

``` r
for (t in 1:4) {
  for (c in 1:4) {
    for (m in 1:4) {
      if (
        !(str_glue("T{t}M{m}post") %in% colnames(df_rr_post)) | 
        !(str_glue("C{c}M{m}post") %in% colnames(df_rr_post))
      ) {
        next
      }
      df_rr_post <-
        escalc(
          data = df_rr_post,
          measure = "SMD",
          m1i = df_rr_post[, str_c("T", t, "M", m, "post")] %>% unlist(),
          m2i = df_rr_post[, str_c("C", c, "M", m, "post")] %>% unlist(),
          sd1i = df_rr_post[, str_c("T", t, "S", m, "post")] %>% unlist(),
          sd2i = df_rr_post[, str_c("C", c, "M", m, "post")] %>% unlist(),
          n1i = df_rr_post[, str_c("T", t, "N", m, "post")] %>% unlist(),
          n2i = df_rr_post[, str_c("C", c, "N", m, "post")] %>% unlist(),
          var.names = c(str_glue("ES_T{t}M{m}_C{c}M{m}"), str_glue("EV_T{t}M{m}_C{c}M{m}"))
        ) 
    }
  }
}

df_rr_post <- 
  df_rr_post %>% 
  select(AUTYR, starts_with("E"))

df_rr_post
```

    ##        AUTYR ES_T1M1_C1M1 EV_T1M1_C1M1 ES_T1M2_C1M2 EV_T1M2_C1M2 ES_T2M1_C1M1 
    ## 1 Connor18_3       0.0165       0.0207      -0.0178       0.0207       0.0155 
    ## 2 Connor18_3       0.0140       0.0179      -0.0354       0.0179           NA 
    ## 3   Dalton11       0.0999       0.0649       0.0726       0.0541       0.1047 
    ##   EV_T2M1_C1M1 ES_T2M2_C1M2 EV_T2M2_C1M2 ES_T3M1_C1M1 EV_T3M1_C1M1 ES_T3M2_C1M2 
    ## 1       0.0201      -0.0398       0.0201       0.0143       0.0197      -0.0400 
    ## 2           NA           NA           NA           NA           NA           NA 
    ## 3       0.0576       0.1234       0.0577           NA           NA           NA 
    ##   EV_T3M2_C1M2 
    ## 1       0.0197 
    ## 2           NA 
    ## 3           NA
