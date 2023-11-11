library(purrr)
library(rmarkdown)
library(stringr)

reports <- diabetes |> 
  distinct(Education) |>
  mutate(output_file = paste0(Education, ".md")) |>
  mutate(output_file = str_replace_all(Education, " ", "_")) |>
  mutate(params = lapply(Education,
                         FUN = function(x){list(edu_level = x)})) |>
  select(-Education)

pwalk(reports, render, input = "analysis.Rmd")



