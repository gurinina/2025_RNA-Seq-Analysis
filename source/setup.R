# Global options for code visibility, formatting, and output width
options(width = 80)

knitr::opts_chunk$set(
  echo = TRUE,                         # Show code by default
  tidy.opts = list(width.cutoff = 80),  # Format code width
  tidy = TRUE,
  message = FALSE,                      # Silence messages by default
  warning = FALSE                       # Silence warnings by default
)

# Extra settings for non-HTML builds (e.g., PDF, EPUB) to avoid LaTeX errors
if (!knitr::is_html_output()) {
  # Comprehensive color and ANSI escape sequence disabling
  options(
    crayon.enabled = FALSE,
    cli.num_colors = 1,
    cli.ansi = FALSE,
    cli.unicode = FALSE,
    pillar.subtle = FALSE,
    pillar.neg = FALSE,
    pillar.bold = FALSE,
    pillar.min_title_chars = Inf,
    tidyverse.quiet = TRUE,              # Suppress tidyverse startup messages
    dplyr.summarise.inform = FALSE,      # Suppress dplyr grouping messages
    readr.show_progress = FALSE,         # Disable readr progress bars
    readr.show_col_types = FALSE         # Disable readr column type messages
  )
  
  # Environment variables to disable colors
  Sys.setenv(
    NO_COLOR = "1",
    CLICOLOR = "0",
    CLICOLOR_FORCE = "0",
    R_CLI_NUM_COLORS = "1",
    TERM = "dumb"                        # Force dumb terminal
  )
  
  # More aggressive output cleaning
  knitr::knit_hooks$set(
    output = function(x, options) {
      # Remove all ANSI escape sequences
      x <- gsub("\033\\[[0-9;]*[mK]", "", x)      # Color codes
      x <- gsub("\033\\[\\?[0-9]*[hl]", "", x)    # Cursor codes
      x <- gsub("\033\\[[0-9;]*[JH]", "", x)      # Clear codes
      x <- gsub("\033\\[[0-9]*[ABCD]", "", x)     # Movement codes
      x <- gsub("\033\\[\\d*;\\d*[Hf]", "", x)    # Position codes
      x <- gsub("\033\\[[sSDK]", "", x)           # Save/restore codes
      x <- gsub("\r", "", x)                      # Remove carriage returns
      x
    },
    message = function(x, options) {
      # Clean messages too
      x <- gsub("\033\\[[0-9;]*[mK]", "", x)
      x <- gsub("\033\\[\\?[0-9]*[hl]", "", x)
      x <- gsub("\r", "", x)
      x
    }
  )
  
  # Disable fansi colors if available
  if (requireNamespace("fansi", quietly = TRUE)) {
    options(fansi.term.cap = "")
  }
}

# Use suppressPackageStartupMessages() in your Rmd files when loading packages:
# suppressPackageStartupMessages(library(tidyverse))
# suppressPackageStartupMessages(library(DESeq2))