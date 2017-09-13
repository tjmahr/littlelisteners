trials <- lookr::Session("../lookr/tests/testthat/data/RWL_WFFArea_Long/001P00XS1/")
lookr::AddAOIData(trials) %>%
  lookr::MeltLooks(other_attrs = "TargetOnset") %>%
  as_tibble() %>%
  select(-Subj, -Task, -Condition, -WordGroup, -DateTime, -TargetEnd) %>%
  rename(Subj = Subject) %>%
  select(-Basename) %>%
  readr::write_csv("data-raw/four-image.csv")

four_image_data <- readr::read_csv("./data-raw/four-image.csv")
devtools::use_data(four_image_data, overwrite = TRUE)
