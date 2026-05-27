## --------------------------------------------------------------#
## Script name: function01-01_helper_functions.R
##
## Purpose of script:
##    Provide reusable helper functions for workflow management
##    and common operations across analysis scripts.
##
## Dependencies:
##    - None (standalone function library)
##
## Author: Paul Bzonek
##
## Date Created: 2024-11-27
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#


#-------------------------------------------------------------#
#####func_source_clean ####################################----
#-------------------------------------------------------------#
#' Source R scripts with verbosity control
#'
#' @description
#' A wrapper around `source()` that provides flexible control over console output
#' verbosity. Useful for cleaning up console output when sourcing multiple scripts
#' in a workflow, while maintaining the ability to debug individual scripts when needed.
#'
#' @param file Character string specifying the path to the R script file to source.
#'   Accepts both relative and absolute paths.
#' @param level Character string controlling verbosity level. Must be one of:
#'   \itemize{
#'     \item \code{"debug"} - Shows code expressions, output, and progress messages
#'       (most verbose, for troubleshooting)
#'     \item \code{"full"} - Shows output and progress messages but not code expressions
#'       (default interactive mode)
#'     \item \code{"minimal"} - Shows cat() output and errors, suppresses warnings,
#'       messages like "Joining with...", and plots (recommended for production runs)
#'     \item \code{"silent"} - Completely silent execution with no console output
#'       (for automated pipelines)
#'     \item \code{"code_only"} - Shows code expressions but suppresses output
#'       (rare debugging use case)
#'   }
#'   Defaults to \code{"minimal"}.
#' @param beep_on_complete Logical; if TRUE, plays a sound after script completes (requires beepr package).
#'   Useful for marking the end of a selected code block. Defaults to FALSE.
#' @param beep_on_error Logical; if TRUE, plays an alarm sound when an error occurs (requires beepr package).
#'   Useful for audio notification of script failures. Defaults to FALSE.
#'
#' @return
#' Invisibly returns the result of `source()`. Side effects include loading
#' objects, functions, and variables from the sourced script into the current
#' environment, and optionally printing progress/output to console based on
#' verbosity level.
#'
#' @details
#' The function uses `switch()` to control both the `echo` argument of `source()`
#' (which controls code visibility) and `capture.output()` (which suppresses
#' output like `cat()`, `print()`, and plot displays).
#'
#' Progress messages show only the basename of the file for cleaner output.
#'
#' @examples
#' # Minimal output (just progress)
#' func_source_clean("02 - Scripts/Script1-1_format_data.R", level = "minimal")
#'
#' # Full debugging with code and output
#' func_source_clean("02 - Scripts/Script1-1_format_data.R", level = "debug")
#'
#' # Completely silent
#' func_source_clean("02 - Scripts/Script1-1_format_data.R", level = "silent")
#'
#' # Using with a global parameter
#' param_verbose <- "minimal"
#' func_source_clean("02 - Scripts/Script1-1_format_data.R", level = param_verbose)
#'
#' @export
func_source_clean <- function(file, level = "minimal", beep_on_complete = FALSE, beep_on_error = FALSE) {

  #----------------------------
  # 1. Validate inputs
  #----------------------------
  valid_levels <- c("debug", "full", "minimal", "silent", "code_only")
  if (!level %in% valid_levels) {
    stop("`level` must be one of: ", paste(valid_levels, collapse = ", "))
  }

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  if ((beep_on_complete || beep_on_error) && !requireNamespace("beepr", quietly = TRUE)) {
    warning("beepr package not installed. Beep will not sound.")
    beep_on_complete <- FALSE
    beep_on_error <- TRUE
  }

  #----------------------------
  # 2. Execute source with verbosity control and error handling
  #----------------------------
  tryCatch(
    {
       switch(level,

              "debug" = {
                # Shows code + output + progress message
                cat("\n=== SOURCING:", basename(file), "===\n")
                source(file, echo = TRUE)
                cat("=== COMPLETE ===\n\n")
              },

              "full" = {
                # Shows output but NOT code, with progress message
                cat("Loading:", basename(file), "...")
                source(file, echo = FALSE)
                cat(" Done.\n")
              },

              "minimal" = {
                # Shows cat output and errors, suppresses warnings and messages
                cat("\n---", basename(file), "---\n")
                suppressMessages(suppressWarnings(source(file, echo = FALSE)))
                cat("--- COMPLETE ---\n")
              },

              "silent" = {
                # Completely silent - no progress, no output, no code
                cat("\n---", basename(file), "---\n")
                pdf(NULL)  # Redirect plots to null device
                suppressMessages(suppressWarnings(
                  invisible(capture.output(source(file, echo = FALSE)))
                ))
                dev.off()  # Close null device
              },

              "code_only" = {
                # Shows code but suppresses output (rare use case)
                cat("\n>>> Code from:", basename(file), "\n")
                invisible(capture.output(source(file, echo = TRUE)))
              }
            ) #End switch
    }, #End main try catch
    error = function(e) {
      # Play error beep if requested
      if (beep_on_error) {
        beepr::beep(sound = 7)  # sound = 7 is alarm sound
     }
      # Re-throw the error to stop execution
      stop(e)
    }
  )

  #----------------------------
  # 3. Play completion sound if requested
  #----------------------------
  if (beep_on_complete) {
    beepr::beep(sound = 1)  # Completion beep
  }
}
