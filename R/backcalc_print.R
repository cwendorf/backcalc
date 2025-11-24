#' Custom Print Method for `backcalc` Objects
#'
#' This function provides a tailored print method for objects of class \code{backcalc}.
#' It optionally displays additional attributes such as notes and approximation messages
#' alongside the main data output.
#'
#' @param x An object of class \code{backcalc}, typically a matrix or data frame with
#'   attached attributes "Notes" and "Approximations".
#' @param ... Additional arguments passed to the base \code{print} function.
#'
#' @details
#' If the attribute \code{"attr"} is set to \code{TRUE} on the object, this method
#' prints the object along with its "Notes" and "Approximations" attributes.
#' Otherwise, it prints only the main data content.
#'
#' @return Invisibly returns the original object \code{x}.
#'
#' @export
print.backcalc <- function(x, ...) {
  attr_show <- isTRUE(attr(x, "attr"))
  
  # Copy for display only
  df_for_display <- x
  attr(df_for_display, "Notes") <- NULL
  attr(df_for_display, "Approximations") <- NULL
  attr(df_for_display, "attr") <- NULL
  class(df_for_display) <- setdiff(class(df_for_display), "backcalc")
  
  # Function to count digits after decimal point in a number stored as numeric
  count_decimal_digits <- function(num) {
    if (is.na(num) || floor(num) == num) return(0)
    # Convert to string without scientific notation
    s <- format(num, scientific = FALSE, trim = TRUE)
    # Extract part after decimal
    parts <- strsplit(s, "\\.", fixed = FALSE)[[1]]
    if (length(parts) < 2) return(0)
    # Count number of digits after decimal, ignoring trailing zeros
    decimal_part <- sub("0+$", "", parts[2])
    nchar(decimal_part)
  }
  
  # Find the max digits after decimal in all numeric columns
  is_num <- vapply(df_for_display, is.numeric, logical(1))
  numeric_cols <- df_for_display[is_num]
  
  if (length(numeric_cols) == 0) {
    digits <- 0
  } else {
    decimal_counts <- unlist(lapply(numeric_cols, function(col) sapply(col, count_decimal_digits)))
    digits <- max(decimal_counts, na.rm = TRUE)
  }
  
  # Format numeric columns with inferred digits and trailing zeros
  df_for_display[is_num] <- lapply(df_for_display[is_num], function(col) {
    format(round(col, digits), nsmall = digits, trim = TRUE)
  })
  
  if (!attr_show) {
    print(df_for_display, ...)
    return(invisible(df_for_display))
  }
  
  approximations <- attr(x, "Approximations")
  # Determine if all effects are insufficient
  all_insufficient <- FALSE
  if (!is.null(approximations) && length(approximations) && is.list(approximations)) {
    insuff_flags <- vapply(approximations, function(notes) any(grepl("^Insufficient Input:", notes)), logical(1))
    all_insufficient <- all(insuff_flags) && length(insuff_flags) > 0
  }
  
  if (!all_insufficient) {
    cat("\n")
    print(df_for_display, ...)
    cat("\n")
  }
  
  if (!is.null(approximations) && length(approximations)) {
    rn <- rownames(x)
    if (all_insufficient) {
      # Specialized header for complete insufficiency
      cat("\nInsufficient Input:\n")
      insuff_lines <- character()
      if (is.list(approximations)) {
        for (i in seq_along(approximations)) {
          notes_i <- approximations[[i]]
          insuff_i <- notes_i[grepl("^Insufficient Input:", notes_i)]
          if (length(insuff_i)) {
            trimmed <- sub("^Insufficient Input:\\s*", "", insuff_i)
            for (tmsg in trimmed) {
              insuff_lines <- c(insuff_lines, paste0(rn[i], ": ", tmsg))
            }
          }
        }
      } else if (is.character(approximations)) {
        insuff_raw <- approximations[grepl("^Insufficient Input:", approximations)]
        trimmed <- sub("^Insufficient Input:\\s*", "", insuff_raw)
        for (tmsg in trimmed) {
          insuff_lines <- c(insuff_lines, paste0(rn[1], ": ", tmsg))
        }
      }
      if (length(insuff_lines)) cat(paste(insuff_lines, collapse = "\n"), "\n\n")
    } else {
      # Standard notes header
      cat("Notes:\n")
      note_lines <- character()
      if (is.list(approximations)) {
        for (i in seq_along(approximations)) {
          notes_i <- approximations[[i]]
          if (length(notes_i)) {
            for (n_i in notes_i) {
              note_lines <- c(note_lines, paste0(rn[i], ": ", n_i))
            }
          }
        }
      } else if (is.character(approximations)) {
        note_lines <- approximations
      }
      if (length(note_lines)) cat(paste(note_lines, collapse = "\n"), "\n\n")
    }
  }
  
  invisible(x)
}

#' Knit print method for backcalc objects
#' Ensures Notes and Approximations print in Rmd
#' @export
knit_print.backcalc <- function(x, ...) {
  out <- capture.output(print(x, ...))  # This captures ALL printed output including cat()
  knitr::asis_output(paste0("```\n", paste(out, collapse = "\n"), "\n```"))
}
