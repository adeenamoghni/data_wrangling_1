Data Tidy
================

This document will practice how to tidy data

``` r
pulse_df = read_sas("data/public_pulse_data.sas7bdat")
janitor::clean_names(pulse_df)
```

    ## # A tibble: 1,087 × 7
    ##       id   age sex    bdi_score_bl bdi_score_01m bdi_score_06m bdi_score_12m
    ##    <dbl> <dbl> <chr>         <dbl>         <dbl>         <dbl>         <dbl>
    ##  1 10003  48.0 male              7             1             2             0
    ##  2 10015  72.5 male              6            NA            NA            NA
    ##  3 10022  58.5 male             14             3             8            NA
    ##  4 10026  72.7 male             20             6            18            16
    ##  5 10035  60.4 male              4             0             1             2
    ##  6 10050  84.7 male              2            10            12             8
    ##  7 10078  31.3 male              4             0            NA            NA
    ##  8 10088  56.9 male              5            NA             0             2
    ##  9 10091  76.0 male              0             3             4             0
    ## 10 10092  74.2 female           10             2            11             6
    ## # ℹ 1,077 more rows

``` r
pulse_tidy_df = 
  pulse_df %>% 
  pivot_longer(
    cols = BDIScore_BL:BDIScore_12m,
    names_to = "visit",
    names_prefix = "bdi_score_",
    values_to = "bdi_score" 
    ) %>% 
    mutate(
      visit = replace(visit, visit == "bl", "00m"),
       visit = factor(visit)
  )
```

``` r
litters_df = read_csv("data/FAS_litters.csv", na = c("NA", "", "."))
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litters_df = janitor::clean_names(litters_df) %>% 
pivot_longer(
  cols = gd0_weight:gd18_weight,
  names_to = "gd_time",
  values_to = "weight"
  ) %>% 
  mutate(
  gd_time = case_match(
    gd_time,
    "gd0_weight" ~ 0,
    "gd18_weight" ~ 18
)
)
```

\##Pivot Wider

``` r
analysis_df = 
  tibble(
    group = c("treatment", "treatment", "control", "control"),
    time = c("pre", "post", "pre", "post"),
    mean = c(4, 10, 4.2, 5)
  )
```

``` r
analysis_df %>% 
  pivot_wider (
    names_from = time,
    values_from = mean
  ) %>% 
  knitr::kable()
```

| group     | pre | post |
|:----------|----:|-----:|
| treatment | 4.0 |   10 |
| control   | 4.2 |    5 |

## Bind Tables

``` r
fellowship_ring = 
  read_excel("data/LotR_Words.xlsx", range = "B3:D6") %>% 
  mutate(movie = "fellowship_ring")
  
two_towers = 
  read_excel("data/LotR_Words.xlsx", range = "F3:H6") %>% 
  mutate(movie = "two_towers")
  
return_king = 
  read_excel("data/LotR_Words.xlsx", range = "J3:L6") %>% 
  mutate(movie = "return_king")

lotr_df =
  bind_rows(fellowship_ring, two_towers, return_king) %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    cols = female:male,
    names_to = "sex",
    values_to = "words"
  ) %>% 
  relocate(movie) %>% 
  mutate(race = str_to_lower(race))
```

## Join FAS datasets

``` r
litters_df = read_csv("data/FAS_litters.csv", na = c("NA", "", ".")) %>% 
  janitor::clean_names() %>% 
  mutate(
    wt_gain = gd18_weight - gd0_weight
  ) %>% 
  separate(
    group, into = c("dose", "day_of_treatment"), sep = 3
  )
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pups_df = read_csv("data/FAS_pups.csv", , na = c("NA", "", ".")) %>% 
  janitor::clean_names() %>% 
  mutate(
    sex = case_match(
      sex,
      1 ~ "male",
      2 ~ "female"
    )
  )
```

    ## Rows: 313 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Join data sets

``` r
fas_df =
  left_join(pups_df, litters_df, by = "litter_number") %>% 
  relocate(litter_number, dose, day_of_treatment)
```
