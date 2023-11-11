# ST 558 Project 3

## Introduction

The goal of this R project is to build and compare models for predicting diabetes outcome using data from the CDC Behavioral Risk Factor Surveillance System (BRFSS) survey. We perform separate analyses for five distinct groups of individuals, subset by education level. The six types of models we consider are logistic regression, LASSO regression, decision tree, random forests, (5), and (6).

## R Packages

The following is a list of packages we use for data processing, model building, and generating output:

1.  `readr`
2.  `dplyr`
3.  `forcats`
4.  `tidyr`
5.  `ggplot2`
6.  `caret`
7.  `stringr`
8.  `rmarkdown`
9.  `purrr`

## Report Automation

We apply the same analysis techniques across all five education level groups. To do this, we build one R Markdown document automated for the parameter `edu_level`. We use the following `render` function to generate the final reports:

```         
reports <- diabetes |> 
  distinct(Education) |>
  mutate(output_file = paste0(Education, ".md")) |>
  mutate(output_file = str_replace_all(Education, " ", "_")) |>
  mutate(params = lapply(Education,
                         FUN = function(x){list(edu_level = x)})) |>
  select(-Education)

pwalk(reports, render, input = "analysis.Rmd")
```

## Analysis

Final reports for each education level are linked below: 

**TO DO: add links**

1. No School or Elementary 
2. Some High School
3. High School Graduate
4. Some College or Technical School
5. College Graduate





