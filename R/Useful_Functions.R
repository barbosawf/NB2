# Function for creating dir with the same name of the R document ----------


create_dir_as_doc <-
  function() {
    rstudioapi::documentPath() |>
      stringr::str_split_i("/", -1) |>
      stringr::str_remove("\\.R") |>
      (\(x) {
        if (!dir.exists(x)) {
          dir.create(x)
        } else {
          cat("\033[31mThe", x, "folder already exists.\033[0m")
        }
      })()
  }


# create_dir_as_doc()


create_dir_as_doc |>
  saveRDS("Useful_Functions/create_dir_as_doc.rds")



# create_full_dir ---------------------------------------------------------


create_full_dir <- function(path) {
  if (!str_ends(path, pattern = "/")) {
    path <- str_c(path, "/")

  }

  path |>
    str_split_1("/") |>
    (\(x) x[-length(x)])() |>
    accumulate(\(x, y)  {
      paste(x, y, sep = "/")


    }) |>
    map(\(x) {
      if (!isTRUE(dir.exists(x))) {
        dir.create(x)
      }
    })

}


create_full_dir |>
  saveRDS("Useful_Functions/create_full_dir.rds")
