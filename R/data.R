#' Informations on talkers
#'
#' @format ## `hr_talkers`
#' A data frame with 50 rows and 3 columns:
#' \describe{
#'   \item{talker}{talker code}
#'   \item{talker_desc}{talker description}
#'   \item{source}{data source}
#' }
"hr_talkers"

#' Informations on types
#'
#' @format ## `hr_types`
#' A data frame with 104 rows and 3 columns:
#' \describe{
#'   \item{type}{type code}
#'   \item{type_desc}{type description}
#'   \item{source}{data source}
#' }
"hr_types"

#' Informations on fields
#'
#' @format ## `hr_fields`
#' A data frame with 104 rows and 3 columns:
#' \describe{
#'   \item{type}{type code}
#'   \item{label}{label name}
#'   \item{name}{variable name}
#'   \item{class}{variable class}
#'   \item{details}{information on values}
#' }
"hr_fields"


#' postres test data
#'
#' Dataset that contains dumps from Arni and Bjarni, 100 records for each
#' message type
#'
#' @format ## `postgres_test_data`
#' A data frame with 104 rows and 3 columns:
#' \describe{
#'   \item{id}{type code}
#'   \item{channel_id}{label name}
#'   \item{date-time}{variable name}
#'   \item{message_type}{variable class}
#'   \item{nmea_message}{information on values}
#'   \item{json_message}{xxx}
#'   \item{talker}{derived from message_type and nmea_message}
#'   \item{type}{derived from message_type and nmea_message}
#' }
"postgres_test_data"
