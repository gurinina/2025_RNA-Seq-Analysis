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

# solutions expressiion works to plot igraph outide code block
solutions <- function(code_expr) {
  output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  if (output_format == "html") {
    # Convert expressions to strings for display
    code_lines <- sapply(code_expr, deparse)  # Convert each expression to a string
    wrapped_code <- paste(code_lines, collapse = "\n")  # Combine all lines into a single string

    # Execute the code and capture the output or result
    eval_env <- new.env(parent = globalenv())  # Create a new environment for execution
    result_value <- tryCatch(
      {
        # Evaluate the code and capture printed output
        capture.output(eval(parse(text = paste(code_lines, collapse = "\n")), envir = eval_env))
      },
      error = function(e) paste("Error:", e$message)
    )

    # Generate collapsible HTML
    output <- paste0(
      '<details>\n',
      '<summary><i>Click here to see the code and the result</i></summary>\n',
      '<pre><code>', htmltools::htmlEscape(wrapped_code), '</code></pre>\n'
    )

    # Include the result (if any)
    if (length(result_value) > 0) {
      result_html <- paste(result_value, collapse = "<br>")
      output <- paste0(output, '<strong>Result:</strong><br>', result_html, '<br>\n')
    }

    output <- paste0(output, '</details>\n')
    return(knitr::asis_output(output))
  } else {
    # Fallback for non-HTML formats
    cat("Code:\n", paste(sapply(code_expr, deparse), collapse = "\n"), "\n")
    result_value <- tryCatch(
      eval(parse(text = paste(sapply(code_expr, deparse), collapse = "\n")), envir = globalenv()),
      error = function(e) cat("Error:", e$message, "\n")
    )
    if (!is.null(result_value)) cat("Result:\n", result_value, "\n")
    return(invisible(NULL))
  }
}


solution3 <- function(solution_text) {
  output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

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
