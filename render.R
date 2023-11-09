library(purrr)
library(rmarkdown)

reports <- diabetes |> 
  distinct(Education) |>
  mutate(output_file = paste0(Education, ".md")) |>
  mutate(params = lapply(Education,
                         FUN = function(x){list(edu_level = x)})) |>
  select(-Education)

pwalk(reports, render, input = "test.md")


