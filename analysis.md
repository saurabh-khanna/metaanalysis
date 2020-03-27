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

df_rr_post %>% 
  knitr::kable()
```

| AUTYR       | ES\_T1M1\_C1M1 | EV\_T1M1\_C1M1 | ES\_T1M2\_C1M2 | EV\_T1M2\_C1M2 | ES\_T2M1\_C1M1 | EV\_T2M1\_C1M1 | ES\_T2M2\_C1M2 | EV\_T2M2\_C1M2 | ES\_T3M1\_C1M1 | EV\_T3M1\_C1M1 | ES\_T3M2\_C1M2 | EV\_T3M2\_C1M2 |
| :---------- | -------------: | -------------: | -------------: | -------------: | -------------: | -------------: | -------------: | -------------: | -------------: | -------------: | -------------: | -------------: |
| Connor18\_3 |      0.0164822 |      0.0207266 |    \-0.0177789 |      0.0207268 |      0.0155116 |      0.0201138 |    \-0.0398454 |      0.0201172 |      0.0142697 |      0.0197437 |    \-0.0399563 |      0.0197472 |
| Connor18\_3 |      0.0140436 |      0.0178803 |    \-0.0354091 |      0.0178826 |             NA |             NA |             NA |             NA |             NA |             NA |             NA |             NA |
| Dalton11    |      0.0998691 |      0.0648552 |      0.0725982 |      0.0541292 |      0.1046956 |      0.0576441 |      0.1234006 |      0.0576746 |             NA |             NA |             NA |             NA |
