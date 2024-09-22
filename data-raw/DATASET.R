# Compilation of metadata from different sources
#
# talkers: Information about the type of instrument sending the message
# types:   Information about the message type, may
# fields:  Information about field names in the message
#
# Processing
#  1. First get available data for NMEA types from official or "reliable" sources
#  2. Add missing stuff based on message in Arni and Bjarni using
#     various ad-hoc sources

library(tidyverse)
library(RPostgres)
library(readxl)

# 1. Import available digitized data -------------------------------------------

## NMEA 0183 standards ---------------------------------------------------------
#  File: data-raw/NMEA0183.xlsx
#  Contains ONLY information of standard NMEA 0183 talkers and message types
#  README: Information about the source of the data
#   NMEA 0183 Standard For Interfacing Marine Electronic Devices
#   Version 3.01 January 1, 2002
#   pdf document: data-raw/documents/NMEA0183-2.pdf)
#   Information of talker and message type meta information manually (copy-paste)
#   obtained from the pdf-document and stored in the excel document
### NMEA 0183 talkers ----------------------------------------------------------
talkers <-
  read_excel("data-raw/NMEA0183.xlsx", sheet = "talkers") |>
  mutate(talker = str_squish(talker),
         description = str_squish(description)) |>
  select(talker, talker_desc = description, source)
### NMEA 0183 message types ----------------------------------------------------
types <-
  read_excel("data-raw/NMEA0183.xlsx", sheet = "types") |>
  mutate(type = str_squish(type),
         description = str_squish(description)) |>
  select(type, type_desc = description, source)
### NMEA 0183 fields -----------------------------------------------------------
# The data from the nmea-package were dumped to csv on 2024--09-20
if(FALSE) {
  nmea::nmea_fields |>
    write_csv("data-raw/nmea-package_fields.csv")
}
fields <-
  read_csv("data-raw/nmea-package_fields.csv",
           show_col_types = FALSE) |>
  rename(type = message_type,
         label = field_label,
         name = field_name,
         class = field_type) |>
  # NOTE: This should be double checked
  mutate(class = case_when(name %in% c("lon", "lat", "altitude") ~ "double",
                           name %in% c("rate_of_turn") ~ "double",
                           name %in% c("mag_variation") ~ "double",
                           .default = class))
## Talkers and message types in Arni and Bjarni --------------------------------

### Get summary data from database ---------------------------------------------
# The data from postgres hafriti were dumped to csv on 2024--09-20
#  These data contain talkers and/or types that may not be in the
#   NMEA 0183 standards
#  This pertains also to that the talker identifier is more than one letter and
#   that a message type may not only be distinguished from the first field
#   but also the second field in the message string (the PSXN message)
if(FALSE) {
  con <- dbConnect(drv = 'PostgreSQL',
                   user = 'hafriti_user',
                   password = 'XXXX',
                   host = 'hfs-lipgsql01.hafogvatn.is',
                   dbname = 'skip')
  arni <-
    dplyr::tbl(con, dbplyr::in_schema("arni", "sensor_data")) |>
    rename(sentence_id = message_type) |>
    mutate(cn2 = case_when(sentence_id %in% c("PSXN") ~ str_sub(nmea_message, 7, 8),
                           .default = ""),
           sentence_id = paste0(sentence_id, cn2)) |>
    count(sentence_id) |>
    collect() |>
    mutate(vessel = "Árni")
  bjarni <-
    dplyr::tbl(con, dbplyr::in_schema("bjarni", "sensor_data")) |>
    rename(sentence_id = message_type) |>
    mutate(cn2 = case_when(sentence_id %in% c("PSXN") ~ str_sub(nmea_message, 7, 8),
                           .default = ""),
           sentence_id = paste0(sentence_id, cn2)) |>
    count(sentence_id) |>
    collect() |>
    mutate(vessel = "Bjarni")
  meta <-
    bind_rows(arni, bjarni) |>
    group_by(sentence_id) |>
    reframe(n = sum(n)) |>
    mutate(talker = case_when(nchar(sentence_id) == 5 ~ str_sub(sentence_id, 1,2),
                              str_starts(sentence_id, "PSCM") ~ "PSCM",
                              str_starts(sentence_id, "PSDG") ~ "PSDG",
                              str_starts(sentence_id, "PSXN") ~ "PSXN",
                              .default = sentence_id),
           talker_standard = case_when(talker %in% talkers$talker ~ "yes",
                                       .default = "no"),
           type = case_when(nchar(sentence_id) == 5 ~ str_sub(sentence_id, 3, 5),
                            talker == "PSCM" ~ str_sub(sentence_id, 5),
                            talker == "PSDG" ~ str_sub(sentence_id, 5),
                            talker == "PSXN" ~ sentence_id,
                            .default = NA),
           type_standard = case_when(type %in% types$type ~ "yes",
                                     .default = "no"))

  meta |>
    write_csv("data-raw/hafriti_meta.csv")
}

# 2. Add additional (missing) talkers, types and fields ------------------------
meta <- read_csv("data-raw/hafriti_meta.csv")
## Some info -------------------------------------------------------------------

# From: Björn Sigurðarson <bjorn.sigurdarson@hafogvatn.is>
#   Sent: föstudagur, 5. október 2018 16:58
# To: Rafn Sigurðsson <rafn.sigurdsson@hafogvatn.is>
#   Subject: NMEA format
#
# Sæll Rafn
# Skilgreining NMEA staðals í viðhengi.

# Hérna er listi yfir þau skeyti sem munu verða í netsendingum. XDR skeytið er
# sérstakt þar sem það er notað fyrir marga mismunandi nema. Skilgreining á
# hvaða nema er átt við er undir mér/okkur komið.
# DBT - Depth Below Transducer
# DPT – Depth
# GGA - Global Positioning System Fix Data
# GLL - Geographic Position - Latitude/Longitude
# HDT - Heading, True
# MTW - Water Temperature
# MWD - Wind Direction & Speed
# MWV - Wind Speed and Angle
# RMC - Recommended Minimum Specific GNSS Data
# VLW - Dual Ground/Water Distance
# VTG - Course Over Ground and Ground Speed
# XDR - Transducer Measurements
# ZDA - Time & Date
#
# Kv,
# Björn

# From: Björn Sigurðarson - HAFRO <bjorn.sigurdarson@hafogvatn.is>
#   Sent: miðvikudagur, 14. febrúar 2024 15:30
# To: Rafn Sigurðsson - HAFRO <rafn.sigurdsson@hafogvatn.is>
#   Subject: RE: NMEA Staðall ofl.
#
# Sæll Rafn.
# Var ég ekki búin að senda þér pdf skjal með NMEA staðlinum?  Hengi það við.
#
# Ekki horfa á BS hlutan.  BS stendur fyrir Björn Sig.  Þú getur fundið
# útskýringu á hverjum hluta fyrir sig í kafla 5.2 í staðlinum.
#
# Í stuttu máli þýðir $ að skeyti samkvæmt skilgreiningum staðalsins.
# Fyrstu tveir stafirnir (´BS´, ´GP´ eða ´IN´ sem dæmi) eru kóði sem segir
# hvers konar tæki er að senda.
# Og svo ´XDR´ sem skilgreinir hvaða skeyti er um að ræða.
#
# Þú skalt ekki hafa áhyggjur af fyrstu tveimur stöfunum.  Í Árna geta skeyti
# frá Staðsetningarbúnaði haft annaðhvort ´IN´ eða ´GP´ sem kóða fyrir hver
# sendir (sama verður í nýja skipinu).
# ´IN´ stendur fyrir Integrated Navigation og
# ´GP´ fyrir GPS.
# Í þessu tilviki setti ég ´BS´ þar sem skilgreindir kóðar áttu ekki við.
# Gæti valdið þér vandræðum ef þú ert að nota aðfengið tól til að lesa NMEA
# strengina.  Við getum rætt breytingu á því ef með þarf
#
# ´XDR´ er skeyti til að senda upplýsingar frá ýmsum nemum sem ekki hafa sín
# eigin skilgreind skeyti.  Hver nemi hefur 3 parametra:  Gildið, Kvarði
# (C fyrir hita, S fyrir seltu osfv.) og Nafn eða annað auðkenni fyrir nemann.
# Skeytið er hannað til að hægt sé að senda upplýsingar frá fleiri en einum nema
# í einu eða að nemar séu tengdir í seríu og hver nemi bæti sínum upplýsingum
# við og áframsendi.  Auðu sætin eru bara til þess að upplýsingar frá sama nema
# endi alltaf í sama sæti í skeytinu.  Það getur einfaldað forritum að finna
# upplýsingar í skeytinu.
#
# Aðeins um dýpisskeyti.
#
# Dýpisskeyti eru mismunandi.  Í fyrsta lagi getur kóði fyrir sendanda verið
# mismunandi (almennt ´DS´ fyrir ´Depth Sounder´).  Svo eru til mismunandi skeyti:
#
#   DPT  Depth,
# DBT  Depth Below Transducer,
# DBS  Depth Below Surface
# DBK  Depth Below Keel.
#
# DPT er það skeyti sem ætti að nota.  Í því er dýpi frá botnstykki og
# upplýsingar um hversu djúpt botnstykkið er miðað við kjöl eða yfirborð.
# Hin skeytin gefa dýpi miðað við uppgefin punkt (T, S eða K) í metrum,
# föðmum og fetum.  DPT er í raun ekki nothæft nema settar hafa verið inn
# upplýsingar í dýptarmæli um hvar botnstykkin eru á skrokknum og hversu mikið
# skipið ristir.  Í Árna þarf einnig stöðu á fellikili sem er breytileg.
#
# Í Árna þarf að nota DPT skeytið.  Ég skal athuga með Bjarna.
#
# Í Skjalið sem er hengt við vantar viðauka I þar sem skilgreind eru skeyti sem
# ekki er mælt með að séu notuð í nýjum tækjum.  Þú gætir lent í að fá
# slík skeyti en ég á ekki skýringar á þeim á pdf formi.
#
# Kv.
# Björn




## Talkers ---------------------------------------------------------------------
talkers2 <-
  meta |>
  filter(talker_standard == "no") |>
  mutate(
    talker_desc = case_when(talker == "BS" ~ "Björn Sigurðsson",
                            talker == "DB" ~ "Depth sounder",
                            talker == "IMWV" ~ "ERROR",                         # Seems to be an error
                            talker == "JSON" ~ "TO DO",                         # Not solved yet
                            talker == "MP" ~ "Marport's proprietary sentence", # https://marport.com/doc_web/scala/topics/r-NMEAOutputSentences.html
                            talker == "NM" ~ "Naust Marine - winch stuff",
                            talker == "PFEC" ~ "HELP NEEDED",
                            talker == "PSCM" ~ "Scanmar stuff",            # https://www.kongsberg.com/contentassets/4cce87e469c14794bcde42406bd99ec4/361017ad_mdm500_reference_manual.pdf
                            talker == "PSDG" ~ "Pos, temp, weather",
                            str_starts(talker, "PSXN") ~ "Various stuff",
                            .default = NA),
    source = "misc"
  )

hr_talkers <-
  talkers |>
  bind_rows(talkers2 |>
              select(talker, talker_desc, source) |>
              distinct())
hr_talkers |> knitr::kable()
# Before saving, may need to filter out errors
usethis::use_data(hr_talkers, overwrite = TRUE)


## Types -----------------------------------------------------------------------
types2 <-
  meta |>
  filter(type_standard == "no") |>
  mutate(type_desc =
           case_when(type == "HBT" ~ "HELP NEEDED",
                     type == "MSD" ~ "HELP NEEDED",
                     type == "ATW" ~ "HELP NEEDED",
                     type == "DBS" ~ "Depth below surface",
                     type == "SM2" ~ "HELP NEEDED",
                     type == "MET" ~ "Meterology",
                     type == "POS" ~ "Position",
                     type == "TSS" ~ "Thermosalinograph",
                     type == "PSXN20" ~ "Quality",
                     type == "PSXN21" ~ "Event",
                     type == "PSXN22" ~ "Calibration",
                     type == "PSXN23" ~ "Degrees",
                     type == "PSXN24" ~ "Rate",
                     type == "MDA" ~ "NMEA 0183 standard Meteorological Composite",  # http://www.nuovamarea.net/blog/wimda
                     sentence_id == "IMWV" ~ "ERROR",
                     sentence_id == "JSON" ~ "TO DO",
                     sentence_id == "PFEC" ~ "HELP NEEDED",
                     .default = "NOT DEALT WITH ABOVE"),
         source = "misc")

hr_types <-
  types |>
  bind_rows(types2 |>
              select(type, type_desc, source))

# Before saving, may need to filter out errors
usethis::use_data(hr_types, overwrite = TRUE)



## fields ----------------------------------------------------------------------

### PSXN specs ------------------------------------------------------------------
# https://github.com/Knio/pynmea2/blob/master/pynmea2/types/proprietary/sxn.py

# NOTE: The sentence_id is PSXN. But the number of variables and their names
#       are dictated by the first true variable column (here 'sentence_id'
#       This means that these data have to be processed a bit different from
#       the rest

# horiz-qual: Horizontal position and velocity quality: 0 = normal, 1 = reduced performance, 2= invalid data.
# hgt-qual: Height and vertical velocity quality: 0 = normal, 1 = reduced performance, 2 =invalid data.
# head-qual: Heading quality: 0 = normal, 1 = reduced performance, 2 = invalid data.
# rp-qual: Roll and pitch quality: 0 = normal, 1 = reduced performance, 2 = invalid data.
# gyro-calib: Gyro calibration value since system start-up in degrees on format d.dd.
# gyro-offs: Short-term gyro offset in degrees on format d.dd.
# roll: Roll in degrees on format d.dd. Positive with port side up.
# pitch: Pitch in degrees on format d.dd. Positive with bow up.
# heave: Heave in metres on format d.dd. Positive down.
# roll-rate: Roll rate in degrees per second on format d.dd. Positive when port side is moving upwards.
# pitch-rate: Pitch rate in degrees per second on format d.dd. Positive when bow is moving upwards.
# yaw-rate: Yaw rate in degrees per second on format d.dd. Positive when bow is moving towards starboard.
# vertical-vel: Vertical velocity in metres per second on format d.dd. Positive when moving downwards.
# event: Event code: 1 = system restart.
# csum: Checksum (exclusive or) of all characters between, but not including, the preceding $ and * , hexadecimal (00 - FF).
# term: CR-LF (2 bytes, values 13 and 10).
psxn20 <-
  tribble(~type, ~label, ~name, ~class, ~details,
          'PSXN20', 'Message Type', 'sentence_id', 'integer', NA,
          'PSXN20', 'Horizontal position and velocity quality', 'horiz_qual', 'integer', '0 = normal, 1 = reduced performance, 2 = invalid data',
          'PSXN20', 'Height and vertical velocity quality', 'hgt_qual', 'integer', '0 = normal, 1 = reduced performance, 2 = invalid data',
          'PSXN20', 'Heading quality', 'head_qual', 'integer', '0 = normal, 1 = reduced performance, 2 = invalid data',
          'PSXN20', 'Roll and pitch quality', 'rp_qual', 'integer', '0 = normal, 1 = reduced performance, 2 = invalid data')

psxn21 <-
  tribble(~type, ~label, ~name, ~class, ~details,
          'PSXN21', 'Message Type', 'sentence_id', 'integer', NA,
          'PSXN21', 'Event code', 'event', 'integer', '1 = system restart')

psxn22 <-
  tribble(~type, ~label, ~name, ~class, ~details,
          'PSXN22', 'Message Type', 'sentence_id', 'integer', NA,
          'PSXN22', 'Gyro calibration value', 'gyro_calib', 'double', 'calibration value since system start-up in degree',
          'PSXN22', 'Short-term gyro offset in degrees', 'gyro_offs', 'double', NA)

psxn23 <-
  tribble(~type, ~label, ~name, ~class, ~details,
          'PSXN23', 'Message Type', 'sentence_id', 'integer', NA,
          'PSXN23', 'Roll in degrees', 'roll', 'double', 'Positive with port side up',
          'PSXN23', 'Pitch in degrees', 'pitch', 'double', 'Positive with bow up',
          'PSXN23', 'Heading, degrees true', 'head', 'double', ' (0.00 - 359.99)',
          'PSXN23', 'Heave in metres', 'heave', 'double', 'Positive down')
psxn24 <-
  tribble(~type, ~label, ~name, ~class, ~details,
          'PSXN24', 'Message Type', 'sentence_id', 'integer', NA,
          'PSXN24', 'Roll rate in degrees/second', 'roll_rate', 'double', 'Positive when port side is moving upwards',
          'PSXN24', 'Pitch rate in degrees/second', 'pitch_rate', 'double', 'Positive when bow is moving upwards',
          'PSXN24', 'Yaw rate in degrees/second', 'yaw_rate', 'double', 'Positive when bow is moving towards starboard',
          'PSXN24', 'Vertical velocity in metres/second', 'vertical_vel', 'double', ' Positive when moving downwards')
pxsn_specs <-
  bind_rows(psxn20, psxn21, psxn22, psxn23, psxn24)

### PSDG specs ------------------------------------------------------------------

# https://www.eurofleets.eu/download/Deliverables/EurofleetsPlus-D3.3_EARS-V2-Deployment-Report-V1_MARIS.pdf

# Datagram description navigation data (POS)
psdgpos <-
  tribble(~type, ~label, ~name, ~class,
          "POS", "Date of position Format ddmmyy", "date_ddmmyy", "datestamp",
          "POS", "UTC time of position Format hhmmss", "time_hhmmss", "timestamp",
          "POS", "Longitude in decimal degrees", "lat_deg", "double",
          "POS", "Latitude in decimal degrees", "lon_deg", "double",
          "POS", "Ship heading in °", "heading_true", "double",
          "POS", "FO/AF speed in kn", "foaf_speed_kn", "double",
          "POS", "Water depth in m", "depth_m", "double",
          "POS", "Course over ground in °", "course", "double",
          "POS", "Speed over ground in kn", "speed_kn", "double")

# Datagram description thermosalinograph data (TSS)
psdgtss <-
  tribble(~type, ~label, ~name, ~class,
          "TSS", "Date of position Format ddmmyy", "date_ddmmyy", "datestamp",
          "TSS", "UTC time of position Format hhmmss", "time_hhmmss", "timestamp",
          "TSS", "Sea water temperature in °C", 'temp_seawate', 'double',
          "TSS", "Salinity in PSU", 'salinity', 'double',
          "TSS", "Sigma theta in kg/m³", 'sigma_theta', 'double',
          "TSS", "Conductivity in S/m", 'conductivity', 'double',
          "TSS", "Raw fluorometry in V", 'fluorometry', 'double')

# Datagram description MET (meteo data)
psdgmet <-
  tribble(~type, ~label, ~name, ~class,
          "MET", "Date of position Format ddmmyy", "date_ddmmyy", "datestamp",
          "MET", "UTC time of position Format hhmmss", "time_hhmmss", "timestamp",
          "MET", "Mean wind speed in m/s", "wind_speed_ms", "double",
          "MET", "Wind gust speed in m/s", "gust_speed_ms", "double",
          "MET", "Wind direction in °", "wind_direction", "double",
          "MET", "Air temperature in °C", "air_temp", "double",
          "MET", "Humidity in %", "rel_humidity", "double",
          "MET", "Solar radiation in W/m²", "solar_rad_wm2", "double",
          "MET", "Atmospheric pressure in hPa", "pressure_hpa", "double",
          "MET", "Sea water temperature in °C", "water_temp", "double")

psdg_specs <-
  bind_rows(psdgmet, psdgpos, psdgtss)

### BSXDR -----------------------------------------------------------------------
xdr_specs <-
  tribble(~type, ~label, ~name, ~class,
          'XDR', 'trans_type1', 'trans_type1', 'character',
          'XDR', 'measure1', 'measure1', 'double',
          'XDR', 'unit1', 'unit1', 'double',
          'XDR', 'trans_type2', 'trans_type2', 'character',
          'XDR', 'measure2', 'measure2', 'double',
          'XDR', 'unit2', 'unit2', 'double',
          'XDR', 'trans_type3', 'trans_type3', 'character',
          'XDR', 'measure3', 'measure3', 'double',
          'XDR', 'unit3', 'unit3', 'double',
          'XDR', 'trans_type4', 'trans_type4', 'character',
          'XDR', 'measure4', 'measure4', 'double',
          'XDR', 'unit4', 'unit4', 'double',
          'XDR', 'trans_type5', 'trans_type5', 'character',
          'XDR', 'measure5', 'measure5', 'double',
          'XDR', 'unit5', 'unit5', 'double')

### ATW -------------------------------------------------------------------------
# Source ????
atw_specs <-
  tribble(~type, ~label, ~name, ~class,
          'ATW', 'winch_star_ten', 'winch_star_ten', 'double',
          'ATW', 'winch_port_ten', 'winch_port_ten', 'double',
          'ATW', 'winch_mid_ten', 'winch_mid_ten', 'double',
          'ATW', 'winch_star_len', 'winch_star_len', 'double',
          'ATW', 'winch_port_len', 'winch_port_len', 'double',
          'ATW', 'winch_mid_len', 'winch_mid_len', 'double',
          'ATW', 'RPM_star', 'RPM_star', 'double',
          'ATW', 'RPM_port', 'RPM_port', 'double',
          'ATW', 'RPM_mid', 'RPM_mid', 'double',
          'ATW', 'line_speed_star', 'line_speed_star', 'double',
          'ATW', 'line_speed_port', 'line_speed_port', 'double',
          'ATW', 'line_speed_mid', 'line_speed_mid', 'double',
          'ATW', 'tow_time', 'tow_time', 'double')


### Bind all --------------------------------------------------------------------

hr_fields <-
  bind_rows(pxsn_specs,
            psdg_specs,
            xdr_specs,
            atw_specs,
            fields |>
              # This stuff needs checking
              filter(type != "XDR"))

# Expect zero rows
hr_fields |>
  select(name, type) |>
  distinct() |>
  count(name, type) |>
  filter(n > 1) |>
  arrange(name)

# temporary shit mix
hr_fields <-
  hr_fields |>
  mutate(name = case_when(name == "month" ~ "month0",
                                name == "year" ~ "year0",
                                .default = name))

usethis::use_data(hr_fields, overwrite = TRUE)
