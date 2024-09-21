# promising function, need to think about talker
#  works on the message worker
message_parse <- function(d) {

}

message_type <- function(x) {

  type <-
    tibble::tibble(m = arni$m) |>
    dplyr::mutate(m = stringr::str_remove(m, "\\$"),
                  m = stringr::str_replace(m, "\\*", ",")) |>
    dplyr::mutate(comma_location = stringr::str_locate_all(m, ",")) |>
    dplyr::mutate(
      type = dplyr::case_when(stringr::str_starts(m, "PSXN") ~ stringr::str_sub(m, 1, purrr::map_int(comma_location, 2) - 1),
                              .default = stringr::str_sub(m, 1, purrr::map_int(comma_location, 1) - 1)),
      type = stringr::str_remove(type, ","),
      talker = dplyr::case_when(nchar(m) == 5 ~ stringr::str_sub(x, 1,2),
                                .default = stringr::str_sub(x, 1, 4)))
}
