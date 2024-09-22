#' Parse messages
#'
#'
#' @param m A dataframe containing message field, now limited to "m"
#' @param TYPE A character, containing messge type
#'
#' @return A tibble containg parsed messge
#' @export
hr_message_parse <- function(m, TYPE = "ATW") {

  # This should possibly be part of the package as like list data
  #  At least this does not belong here
  cn_integer   <- hr_fields  |> dplyr::filter(class == "integer")   |> dplyr::pull(name) |> unique()
  cn_double    <- hr_fields  |> dplyr::filter(class == "double")    |> dplyr::pull(name) |> unique()
  cn_character <- hr_fields  |> dplyr::filter(class == "character") |> dplyr::pull(name) |> unique()
  cn_datestamp <- hr_fields  |> dplyr::filter(class == "datestamp") |> dplyr::pull(name) |> unique()
  cn_timestamp <- hr_fields  |> dplyr::filter(class == "timestamp") |> dplyr::pull(name) |> unique()

  cn <- hr_fields |> dplyr::filter(type == TYPE) |> dplyr::pull(name)
  # Some messages do not have a checksum
  if(TYPE %in% c("DTM", "HBT")) {
    cn <- c(".id", cn)
  } else {
    cn <- c(".id", cn, ".check")
  }

  ret <-
    m |>
    dplyr::filter(type == TYPE) |>
    dplyr::mutate(m = stringr::str_replace(m, "\\*", ","),
                  m = stringr::str_remove(m, "\\$")) |>
    tidyr::separate_wider_delim(cols = m, names = cn, cols_remove = TRUE, delim = ",", too_many = "merge", too_few = "align_start") |>
    # if string blank ("") then NA
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.,"")))

  var <-
    ret |>
    dplyr::select(-c(id:message_type, json_message:type)) |>
    colnames()



  var.int <- var[var %in% cn_integer]
  var.chr <- var[var %in% cn_character]
  var.dbl <- var[var %in% cn_double]
  var.dmy <- var[var %in% cn_datestamp]
  var.tms <- var[var %in% cn_timestamp]

  if(!is_empty(var.int)) ret <- ret |> dplyr::mutate(dplyr::across(dplyr::all_of(var.int), as.integer))
  #if(!is_empty(res.chr)) res[[i]] <- res[[i]] |> mutate(across(all_of(res.chr), as.character))
  if(!is_empty(var.dbl)) ret <- ret |> dplyr::mutate(dplyr::across(dplyr::all_of(var.dbl), as.numeric))
  if(!is_empty(var.dmy)) ret <- ret |> dplyr::mutate(dplyr::across(dplyr::all_of(var.dmy), lubridate::dmy))

  if(!is_empty(var.tms)) {
    # Find a solution to this, this is dependent of if name is "timestamp" or "time_hhmmss"
    #  solution should be agnostic to the exact variable name
    if(!TYPE %in% c("MET", "POS", "TSS")) {
      ret <-
        ret |>
        dplyr::mutate(timestamp = paste(stringr::str_sub(timestamp, 1, 2), stringr::str_sub(timestamp, 3, 4), stringr::str_sub(timestamp, 5, 6),
                                        sep = ":")) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(var.tms), lubridate::hms))
    } else {
      ret <-
        ret |>
        dplyr::mutate(time_hhmmss = paste(stringr::str_sub(time_hhmmss, 1, 2), stringr::str_sub(time_hhmmss, 3, 4), stringr::str_sub(time_hhmmss, 5, 6),
                                        sep = ":")) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(var.tms), lubridate::hms))
    }

  }
  return(ret)
}



# promising function, need to think about talker
#  works on the message worker

message_type <- function(x) {

  type <-
    tibble::tibble(m = x) |>
    dplyr::mutate(m = stringr::str_remove(m, "\\$"),
                  m = stringr::str_replace(m, "\\*", ",")) |>
    dplyr::mutate(comma_location = stringr::str_locate_all(m, ",")) |>
    dplyr::mutate(
      type = dplyr::case_when(stringr::str_starts(m, "PSXN") ~ stringr::str_sub(m, 1, purrr::map_int(comma_location, 2) - 1),
                              .default = stringr::str_sub(m, 1, purrr::map_int(comma_location, 1) - 1)),
      type = stringr::str_remove(type, ","),
      talker = dplyr::case_when(nchar(type) == 5 ~ stringr::str_sub(type, 1,2),
                                .default = stringr::str_sub(type, 1, 4)),
      type = stringr::str_sub(type, nchar(talker) + 1)) |>
    dplyr::select(talker, type, m)

  return(type)
}
