#' Estimates stay times from log data
#'
#' @param logs Tibble. Must be a logs tibble retrieved with `get_logs()` or `read_logs()`.
#'
#' @return Contains the estimated stay times of units and pages.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Returns estimated stay times for units and unit pages.
#'
#' @export
#' @importFrom dplyr setdiff

# TODO: Loading times in Fällen berechnen, in denen mehrmals hintereinander dasselbe 
# Unit versucht wurde zu laden. 
# Derzeit wird nur der letzte (und erfolgreiche) Ladeversuch beachtet.

estimate_unit_times <- function(logs) {
  cli_setting()
  groups_booklet <- setdiff(names(logs), c("unit_key", "unit_alias", "ts", "log_entry"))
  groups_unit <- setdiff(names(logs), c("ts", "log_entry"))
  
  all_logs <-
    logs %>%
    dplyr::filter(
      # Delete duplicate page identifiers as these would contaminate page time estimation
      !log_entry %>% stringr::str_detect("(CURRENT_PAGE_NR|PAGE_COUNT)"),
      # This is only a constant message stream that is not interaction-based
      !log_entry %>% stringr::str_detect("TESTLETS_TIMELEFT")
    ) %>%
    dplyr::mutate(ts = as.numeric(ts)) 
  
  all_logs <- all_logs %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(groups_booklet, "ts")))) %>%
    # Unusable timestamps
    dplyr::filter(ts != 0) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet)))) %>%
    dplyr::mutate(
      unit_key = dplyr::case_when(
        # For legacy reasons
        stringr::str_detect(log_entry, "CURRENT_UNIT_ID") ~
          stringr::str_extract(log_entry, "\"(.+)\"", group = TRUE),
        .default = unit_key
      ),
      is_max_ts = ts == max(ts)
    ) %>%
    dplyr::filter((!is.na(unit_key) & unit_key != "") | is_max_ts) %>%
    tidyr::fill(unit_key, .direction = "downup") %>%
    dplyr::ungroup()
  
  all_ts <-
    all_logs %>%
    dplyr::mutate(
      ts_name = dplyr::case_when(
        # For the previous unit
        stringr::str_detect(log_entry, "CURRENT_UNIT_ID") ~ "unit_current_ts",
        stringr::str_detect(log_entry, "PLAYER = LOADING") ~ "unit_load_ts",
        stringr::str_detect(log_entry, "PLAYER = RUNNING") ~ "unit_start_ts",
        stringr::str_detect(log_entry, "CURRENT_PAGE_ID") ~ "page_start_ts",
        is_max_ts ~ "session_end_ts",
        log_entry == "PLAYER = PAUSED" ~ "n_paused",
        log_entry == "FOCUS : \"HAS_NOT\"" ~ "n_lost_focus",
        .default = NA_character_
      ),
      page_id = dplyr::case_when(
        # For legacy reasons
        log_entry == "CURRENT_PAGE_ID" ~ 0L,
        ts_name == "page_start_ts" ~ log_entry %>% stringr::str_extract("\\d+") %>% as.integer(),
        .default = NA_integer_
      )
    ) %>%
    dplyr::mutate(
      ts_name = dplyr::case_when(
        is_max_ts ~ "session_end_ts",
        .default = ts_name
      )
    )
  
  
  all_ts <- all_ts %>%
    # dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_unit, "ts_name")))) %>%
    # dplyr::mutate(
    #   n_play = ifelse(ts_name == "unit_start_ts", seq_along(ts_name), NA_integer_)
    # ) %>%
    # dplyr::group_by(dplyr::across(dplyr::all_of(groups_unit))) %>%
    # dplyr::arrange("ts", by_group=TRUE) %>%
    # tidyr::fill(n_play) %>%
    # dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_unit, "n_play")))) %>% # n_play m. E. 
    # fehlerhaft, 
    # weil tw. PLAYER=LOADING als Ende des letzten Units gewertet wird
    # dplyr::mutate(
    #   n_ts = seq_along(ts_name),
    #   ts_name = ifelse((n_ts == max(n_ts) & n_ts != 1), "unitplay_last_ts", ts_name), # Lea: 
    #   # Nicht nutzbar für Berechnung der Spielzeiten
    #   # einzelner Unit-Plays, weil dazwischen tw. andere Units eingespielt wurden.
    #   # Ferner wird tw. Der Start-ts auch als End-ts (letzter ts im Play) markiert.
    # ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(ts_name))
  
  unit_logs_prep <-
    all_ts %>%
    dplyr::filter(
      ts_name == "unit_start_ts" | ts_name == "unit_load_ts" | ts_name == "session_end_ts"
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_booklet))) %>%
    dplyr::arrange("ts", by_group=TRUE) %>%  
    dplyr::mutate(
      ts_next = dplyr::lead(ts),
      unit_time = ts_next - ts # Unit time hier definiert als Zeitspanne von Unit RUNNING 
      # bis zur nächsten Aktion innerhalb des Booklets
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_unit))) %>%
    dplyr::arrange("ts", by_group=TRUE) %>%
    dplyr::mutate(
      ts_prev = dplyr::lag(ts),
      unit_loadtime = ts - ts_prev # Unit Loadtime hier definiert als Zeitspanne von der 
      # letzten Aktion innerhalb des Units 
      # (fast immer LOADING) bis zum Unit RUNNING. D. h. nur der letzte (erfolgreiche) 
      # Ladeversuch wird gezählt.
    ) %>%
    dplyr::filter(ts_name %in% c("unit_start_ts")) %>%
    dplyr::mutate(
      unit_start_i = seq_along(unit_time)
    )
  
  # Multiple Unit plays
  unit_logs_starts <-
    unit_logs_prep %>%
    dplyr::select(dplyr::all_of(c(groups_unit,
                                  "unit_start_i",
                                  "unit_time_i" = "unit_time",
                                  "unit_start_time_i" = "ts",
                                  "unit_end_time_i" = "ts_next",
                                  "unit_loadtime_i" = "unit_loadtime",
                                  "unit_loadstart_i" = "ts_prev"))) %>%
    tidyr::nest(
      unit_logs_i = c("unit_start_i", "unit_time_i", "unit_end_time_i", 
                      "unit_start_time_i", "unit_loadtime_i", "unit_loadstart_i")
    )
  
  # Total start and stay times
  unit_logs <-
    unit_logs_prep %>%
    dplyr::summarise(
      unit_start_time = min(ts),
      unit_n_play = length(unit_time),
      unit_time = sum(unit_time, na.rm = TRUE), # Ohne Ladezeiten
      all_load_time =  sum(unit_loadtime, na.rm = TRUE), #Reine Ladezeiten (ohne 
      # erfolglose Ladeversuche)
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      unit_logs_starts,
      by = dplyr::join_by(!!! groups_unit)
    )
  
  # Page times
  if (any(!is.na(all_ts$page_id))) {
    unit_page_logs_prep <-
      all_ts %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet, "unit_key")))) %>%
      dplyr::mutate(
        is_max_ts = ts == max(ts)
      ) %>%
      dplyr::filter(
        ts_name %>% stringr::str_detect("^page_") | ts_name == "unit_start_ts" | is_max_ts
      ) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet)))) %>%
      dplyr::arrange("ts", by_group=TRUE) %>%  
      dplyr::mutate(
        ts_prev = dplyr::lag(ts),
        ts_next = dplyr::lead(ts),
        page_time = ts_next - ts
        # ts = ifelse(ts_name == "page_start_ts", ts, ts_next)
      ) %>%
      dplyr::group_by(dplyr::across(groups_unit)) %>%
      dplyr::filter(
        ts_name != "unit_current_ts" & ts_name != "unit_start_ts" # These are only
        # used as endpoint of last page
      ) %>%
      # The first page is not logged before completion...
      tidyr::fill(page_id, .direction = "up") %>%
      dplyr::filter(!is.na(page_id)) %>%
      dplyr::group_by(dplyr::across(c(groups_unit, "page_id"))) %>%
      dplyr::mutate(
        page_start_i = seq_along(page_time)
      )
    
    # Separate Unit start and stay times
    unit_page_logs_start <-
      unit_page_logs_prep %>%
      dplyr::select(dplyr::all_of(c(groups_unit, "page_id",
                                    "page_start_i",
                                    "page_time_i" = "page_time",
                                    "page_start_time_i" = "ts",
                                    "page_end_time_i" = "ts_next"))) %>%
      tidyr::nest(
        page_logs_i = c("page_start_i", "page_time_i", "page_end_time_i", "page_start_time_i")
      )
    
    unit_page_logs <-
      unit_page_logs_prep %>%
      dplyr::summarise(
        page_start_time = min(ts),
        page_n_start = length(page_time),
        page_time = sum(page_time),
        .groups = "drop"
      ) %>%
      dplyr::left_join(
        unit_page_logs_start,
        by = dplyr::join_by(!!! c(groups_unit, "page_id"))
      ) %>%
      tidyr::nest(unit_page_logs = dplyr::any_of(c("page_id", "page_start_time", 
                                                   "page_n_start", "page_time", "page_logs_i")))
    
    # unit_page_logs$unit_page_logs[[4]]$page_logs_i
    
    unit_logs <- unit_logs %>%
      dplyr::left_join(
        unit_page_logs,
        by = dplyr::join_by(!!! groups_unit)
      ) %>%
      dplyr::mutate(
        unit_has_pages = purrr::map_lgl(unit_page_logs, function(x) !is.null(x))
      )
  }
  return(unit_logs)
}