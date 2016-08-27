dashboard_data <- polloi::read_dataset("portal/language_destination.tsv")

dir.create("data")
readr::write_tsv(dplyr::filter(dashboard_data, prefix %in% c("uk", "ru", "crh", "en", "de")),
                 "data/language_destination.tsv")
