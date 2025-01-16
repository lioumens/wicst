# qa book freezes

library(readxl)
library(purrr)
library(dplyr)
qa_fp <- "~/Library/CloudStorage/OneDrive-UW-Madison/Database development/Michael/QA BOOK.xlsx"
sheet_names <- excel_sheets(qa_fp)

xl_qa <- sheet_names |> 
  set_names() |>
  map(\(x) {
      read_xlsx(qa_fp, sheet = x, na = c("-", "."),
                     skip = case_when(x == 'counts'~1,
                                      .default = 0))
    })


date_string <- Sys.Date() %>% format(format = "%Y%m%d")
filepath_date <- paste0("data/qa_", date_string, ".Rdata")

save(xl_qa, file = filepath_date)
