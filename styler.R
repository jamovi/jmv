options(styler.indent_by = 4)

# Ignore folders
exclude_dirs <- c(
    "renv",
    "build",
    "temp"
)

# Ignore files with specific pattern
exclude_files_pattern <- c(
    "\\h\\.R$" # Exclude header files
)

# Function to apply styler
format <- function() {
    path <- "."

    exclude_files_pattern_regex <- paste0("(", paste(exclude_files_pattern, collapse = "|"), ")")
    exclude_files <- list.files(path, pattern = exclude_files_pattern_regex, recursive = TRUE, full.names = TRUE)

    styler::style_dir(
        path = path,
        exclude_files = exclude_files,
        exclude_dirs = exclude_dirs,
        indent_by = getOption("styler.indent_by")
    )
}

# Function for dry run
format_check <- function() {
    path <- "."

    exclude_files_pattern_regex <- paste0("(", paste(exclude_files_pattern, collapse = "|"), ")")
    exclude_files <- list.files(path, pattern = exclude_files_pattern_regex, recursive = TRUE, full.names = TRUE)

    result <- styler::style_dir(
        path = path,
        exclude_files = exclude_files,
        exclude_dirs = exclude_dirs,
        indent_by = getOption("styler.indent_by"),
        dry = "on"
    )

    # Check if there are any differences
    if (any(result$changed)) {
        message("Code needs formatting. Run 'make format' to apply changes.")
    } else {
        message("All files are formatted correctly.")
    }
}

# Command line arguments to decide which function to call
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0 || args[1] == "format") {
    format()
} else if (args[1] == "check") {
    format_check()
} else {
    stop("Unknown argument. Use 'format' or 'check'.")
}
