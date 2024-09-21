#' Hafriti sensor data
#'
#' Arni and Bjarni are in separate table
#'
#' @param con a database connection
#' @param skip vessel name (default: 'arni')
#'
#' @return A SQL connection
#' @export
#'
hr_sensor <- function(con, skip = "arni") {
  q <-
    dplyr::tbl(con, dbplyr::in_schema(skip, "sensor_data")) |>
    dplyr::mutate(
      talker = dplyr::case_when(nchar(message_type) == 5 ~ stringr::str_sub(message_type, 1,2),
                                .default = stringr::str_sub(message_type, 1, 4)),
      type = dplyr::case_when(nchar(message_type) == 5 ~ stringr::str_sub(message_type, 3, 5),
                              talker %in% c("PSCM", "PSDG") ~ stringr::str_sub(message_type, 5),
                              talker == "PSXN" ~ stringr::str_sub(nmea_message, 1, 8),
                              .default = "NO IDEA"),
      type = stringr::str_remove(type, "\\$"),
      type = stringr::str_remove(type, ","))

  return(q)
}

# cn <- hr_fields |> filter(type == types[i]) |> pull(field_name)

hr_sensor_hdt <- function(q) {
  TP <- "HDT"
  q |>
    dplyr::filter(stringr::str_sub(type, 3) == TP)
}

