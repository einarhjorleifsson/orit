

#' Read position
#'
#' @param vid x
#' @param t1 x
#' @param t2 x
#' @param mean_by x
#' @param path x
#'
#' @return A tibble
#' @export
#'
read_positions <- function(vid = 2350, t1 = "2024-02-27", t2 = "2024-03-21", mean_by = "sec", path = "/home/haf/einarhj/stasi/hafriti/data/type=GGA") {

  V <- vid
  t1 <- lubridate::ymd(t1)
  t2 <- lubridate::ymd(t2)
  Y <- c(year(t1):year(t2))
  Q <- unique(c(quarter(t1), quarter(t2)))

  lh <- function(x) {
    x <-
      x |>
      as.character() |>
      stringr::str_remove("\\.")
    paste0(stringr::str_sub(x, 1, 6)) |>  #, ".", str_sub(x, 5, 6)) |>
      as.numeric() |>
      geoconvert.1()
  }
  geoconvert.1 <- function (x) {
    i <- sign(x)
    x <- abs(x)
    x1 <- x%%10000
    k <- c(1:length(x1))
    k <- k[x1 > 5999 & !is.na(x1)]
    #if (length(k) > 0)
    #  print(paste("error > 60 min nr", k, x[k]))
    min <- (x/100) - trunc(x/10000) * 100
    return((i * (x + (200/3) * min))/10000)
  }

  arrow::open_dataset(path) |>
    dplyr::filter(vid == V,
                  year %in% Y,
                  quarter %in% Q) |>
    dplyr::select(time = date_time,
                  lon,
                  lat) |>
    dplyr::collect() |>
    dplyr::filter(dplyr::between(time, t1, t2 + lubridate::days(1))) |>
    tidyr::drop_na() |>
    dplyr::mutate(lon = -lh(lon),
                  lat = lh(lat)) |>
    tidyr::drop_na() |>
    dplyr::mutate(time = lubridate::floor_date(time, unit = mean_by)) |>
    dplyr::reframe(lon = mean(lon),
                   lat = mean(lat),
                   n_pos = dplyr::n(),
                   .by = time)
}

#' Title
#'
#' @param vid x
#' @param t1 x
#' @param t2 x
#' @param mean_by x
#' @param path x
#'
#' @return A tibble
#' @export
#'
read_speed <- function(vid = 2350, t1 = "2024-02-27", t2 = "2024-03-21", mean_by = "sec", path = "/home/haf/einarhj/stasi/hafriti/data/type=VTG") {

  V <- vid
  t1 <- lubridate::ymd(t1)
  t2 <- lubridate::ymd(t2)
  Y <- c(year(t1):year(t2))
  Q <- unique(c(quarter(t1), quarter(t2)))
  arrow::open_dataset(path) |>
    dplyr::filter(vid == V,
                  year %in% Y,
                  quarter %in% Q) |>
    dplyr::select(time = date_time, speed = spd_over_grnd_kts) |>
    dplyr::collect() |>
    dplyr::filter(dplyr::between(time, t1, t2 + lubridate::days(1))) |>
    tidyr::drop_na() |>
    dplyr::mutate(time = floor_date(time, unit = mean_by)) |>
    dplyr::reframe(speed = mean(speed),
                   n_speed = dplyr::n(),
                   .by = time)
}


#' Title
#'
#' @param vid x
#' @param t1 x
#' @param t2 x
#' @param mean_by x
#' @param path x
#'
#' @return A tibble
#' @export
read_heading <- function(vid = 2350, t1 = "2024-02-27", t2 = "2024-03-21", mean_by = "sec", path = "/home/haf/einarhj/stasi/hafriti/data/type=HDT") {

  V <- vid
  t1 <- lubridate::ymd(t1)
  t2 <- lubridate::ymd(t2)
  Y <- c(year(t1):year(t2))
  Q <- unique(c(quarter(t1), quarter(t2)))

  arrow::open_dataset(path) |>
    dplyr::filter(vid == 2350,
                  year %in% Y,
                  quarter %in% Q) |>
    dplyr::select(time = date_time, heading) |>
    dplyr::collect() |>
    dplyr::filter(dplyr::between(time, t1, t1 + lubridate::days(1))) |>
    tidyr::drop_na() |>
    dplyr::mutate(time = lubridate::floor_date(time, unit = mean_by)) |>
    dplyr::reframe(heading = mean(heading),
                   n_heading = dplyr::n(),
                   .by = time)
}



#' Title
#'
#' @param vid x
#' @param t1 x
#' @param t2 x
#' @param mean_by x
#' @param path x
#'
#' @return A tibble
#' @export
read_winch <- function(vid = 2350, t1 = "2024-02-27", t2 = "2024-03-21", mean_by = "sec", path = "/home/haf/einarhj/stasi/hafriti/data/type=ATW") {

  V <- vid
  t1 <- lubridate::ymd(t1)
  t2 <- lubridate::ymd(t2)
  Y <- c(year(t1):year(t2))
  Q <- unique(c(quarter(t1), quarter(t2)))

  arrow::open_dataset(path) |>
    dplyr::filter(vid == V,
                  year %in% Y,
                  quarter %in% Q) |>
    dplyr::collect() |>
    dplyr::rename(time = date_time) |>
    dplyr::filter(dplyr::between(time, t1, t2)) |>
    dplyr::mutate(time = lubridate::floor_date(time, unit = mean_by)) |>
    dplyr::reframe(winch_star_ten = mean(winch_star_ten),
            winch_port_ten = mean(winch_port_ten),
            winch_star_len = mean(winch_star_len),
            winch_port_len = mean(winch_port_len),
            RPM_star = mean(RPM_star),
            RPM_port = mean(RPM_port),
            line_speed_star = mean(line_speed_star),
            line_speed_port = mean(line_speed_port),
            n_winch = dplyr::n(),
            .by = time)
}


