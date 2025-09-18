# library(details)
evaluate_and_display <- function(expr, title = "Click to Show/Hide") {
  # Evaluate the expression
  result <- eval(expr, envir = parent.frame())

  # Capture the original code for display
  code <- paste(deparse(expr), collapse = "\n")
  formatted_code <- paste0("```r\n", code, "\n```")

  # Create collapsible HTML using your CSS styles
  html <- paste0(
    '<details>',
    '<summary>', title, '</summary>',
    '<pre><code>', formatted_code, '</code></pre>',
    '<div>',
    print(result), # Render the evaluated result
    '</div>',
    '</details>'
  )

  return(knitr::asis_output(html))
}


folded <- function(solution_text, evaluated_output) {
  output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  if (output_format == "html") {
    # For GitBook (HTML), output as raw HTML and wrap the solution in <details><pre><code> tags
    knitr::asis_output(paste0(
      '<details>\n',
      '<summary><i>Click here to see the solution</i></summary>\n',
      '<pre><code>',
      "```r\n",  # Open backticks for R code block
      solution_text, '\n',  # Include the original solution text
      "```\n",  # Close backticks for R code block
      "```r\n",  # Open new backticks for evaluated output
      evaluated_output, '\n',  # Include the evaluated output
      "```\n",  # Close backticks for evaluated output
      '</code></pre>\n',
      '</details>\n'
    ))
  } else {
    # For non-HTML formats (EPUB, PDF), hide the solution
    cat("")  # No output for non-HTML formats
  }
}


wrap_in_quotes <- function(...) {
  # Capture expressions without evaluating them
  code_lines <- sapply(substitute(list(...))[-1], deparse)

  # Return the lines wrapped in single quotes
  return(paste0("'", code_lines, "'", collapse = ",\n"))
}
# 
# perfect, why doesn't this produce a duplicate line? Determine the sources explaining the variation represented by PC1 and PC2. 
# {r, results = "asis", echo = FALSE}
# solutions(list(quote({
#   # The sample groups 'Ctrl' and 'Sci' separate well on PC1 and PC2
#   cat("The sample groups 'Ctrl' and 'Sci' separate well on PC1 and PC2")
# })))
# 
# This doesn't produce a duplicate line because of the logic in the updated solutions() function:
#   
#   Detection phase: The function recognizes this as a "pure cat" expression - it only contains a cat() call (and a comment)
# Single execution: When is_pure_cat is TRUE, the function takes this path:
#   r
# if (is_pure_cat) {
#   # Just execute the cat() statements without showing code
#   eval(expr, envir = .GlobalEnv)
# }
# No second execution: It skips the else block that would show code and execute again
# No output section: There's no separate "output" section that would run the cat() command a second time
# The key difference from your previous function is that the old version would:
# 
# Execute during line-by-line processing
# Execute again in the final output section
# But the new version recognizes "this is just text output" and executes only once. The cat() statement runs exactly one time, producing exactly one line of output.
# 
# That's why you get clean, single-line text output instead of duplicated results.

# this works for cat and print and no parentheses in answers
solutions <- function(code_expr) {
  if (!knitr::is_html_output()) {
    return("")
  }
  
  cat('<details>\n')
  cat('<summary><strong>Solution</strong></summary>\n\n')
  
  for (i in seq_along(code_expr)) {
    expr <- code_expr[[i]]
    
    # Check if this is a pure cat() expression block
    is_pure_cat <- FALSE
    if (is.call(expr) && length(expr) > 0 && expr[[1]] == as.name("{")) {
      # Check if all non-comment lines are cat() calls
      all_cat <- TRUE
      for (j in 2:length(expr)) {
        line_expr <- expr[[j]]
        if (is.call(line_expr) && length(line_expr) > 0) {
          tryCatch({
            func_name <- as.character(line_expr[[1]])
            if (!func_name %in% c("cat", "print")) {
              all_cat <- FALSE
              break
            }
          }, error = function(e) {
            all_cat <- FALSE
          })
        }
      }
      is_pure_cat <- all_cat
    }
    
    if (is_pure_cat) {
      # Just execute the cat() statements without showing code
      eval(expr, envir = .GlobalEnv)
    } else {
      # Handle expressions wrapped in {}
      if (is.call(expr) && expr[[1]] == as.name("{")) {
        # Extract all expressions inside the {} using proper indexing
        code_lines <- character()
        for (j in 2:length(expr)) {  # Start from 2 to skip the { symbol
          inner_expr <- expr[[j]]
          code_lines <- c(code_lines, deparse(inner_expr, width.cutoff = 200))
        }
        code_line <- code_lines
      } else {
        # Show code and execute normally
        code_line <- deparse(expr, width.cutoff = 200)
      }
      
      cat('```r\n')
      cat(paste(code_line, collapse = '\n'), '\n')
      cat('```\n\n')
      
      cat('```\n')
      tryCatch({
        result <- eval(expr, envir = .GlobalEnv)
        if (!is.null(result)) {
          output <- capture.output(print(result))
          cat(paste(output, collapse = '\n'), '\n')
        }
      }, error = function(e) {
        cat("Error:", e$message, "\n")
      })
      cat('```\n\n')
    }
    
    if (i < length(code_expr)) {
      cat('---\n\n')
    }
  }
  
  cat('</details>\n')
}

solution3 <- function(solution_text) {
  output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  
  # Handle NULL or empty output_format
  if (is.null(output_format) || length(output_format) == 0) {
    output_format <- "html"  # Default to HTML
  }
  
  if (output_format == "html") {
    # For GitBook (HTML), output as raw HTML and wrap the solution in <pre><code> tags
    knitr::asis_output(paste0(
      '<details>\n',
      '<summary><i>Click here to see the solution</i></summary>\n',
      '<pre><code>', solution_text, '</code></pre>\n',
      '</details>\n'
    ))
  } else {
    # For non-HTML formats (EPUB, PDF), hide the solution
    cat("")  # No output for non-HTML formats
  }
}