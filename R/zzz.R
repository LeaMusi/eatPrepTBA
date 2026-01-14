.onAttach <- function(libname, pkgname) {
  cli_setting()

  version <- utils::packageVersion(pkgname)

  cli::cli_alert_info("{.pkg {pkgname}} v{version}",
                      class = "packageStartupMessage")
  # rlang::inform(message = msg, class = "packageStartupMessage")
}

utils::globalVariables(c(
  "unit_key",
  "variable_id",
  "schemer",
  "link",
  "rule_parameter",
  "code_score",
  "code_type",
  "code_id",
  "variable_source_type",
  "Test",
  "print_cols",
  "n",
  "id",
  "code",
  "score",
  "status",
  "insert_manual"
))
