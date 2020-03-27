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
for (t in 1:3) {
  for (c in 1:2) {
    for (m in 1:2) {
      if (
        !(str_glue("T{t}M{m}post") %in% colnames(df_rr_post)) | 
        !(str_glue("C{c}M{m}post") %in% colnames(df_rr_post))
      ) {
        next
      }
      #print(t)
      #print(c)
      #print(m)
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
          var.names = c(str_glue("yi_t{t}m{m}_c{c}m{m}"), str_glue("vi_t{t}m{m}_c{c}m{m}"))
        )
    }
  }
}
```
