# student_version_generator.R

library(stringr)

# student_version_generator.R

library(stringr)

create_student_version <- function(instructor_file, student_file) {
  
  # Read the instructor file
  content <- readLines(instructor_file, warn = FALSE)
  content <- paste(content, collapse = "\n")
  
  # Remove solution chunks with results="asis" - make it multiline and non-greedy
  content <- str_replace_all(content, 
                             regex("```\\{r, results = \"asis\", echo = FALSE\\}.*?```", dotall = TRUE), 
                             "```{r}\n# Your code here\n\n```")
  
  # Remove manual HTML solutions (from <details> to </details>)
  content <- str_replace_all(content, 
                             regex("<details>.*?</details>", dotall = TRUE), 
                             "```{r}\n# Your code here\n\n```")
  
  # Convert back to lines and write
  content_lines <- str_split(content, "\n")[[1]]
  writeLines(content_lines, student_file)
  
  cat("Student version created:", student_file, "\n")
}

# List of files to convert
files_to_convert <- c(
  "lessons/01-DGE_setup_and_overview.Rmd",
  "lessons/02-DGE_count_normalization.Rmd",
  "lessons/03-DGE_QC_analysis.Rmd",
  "lessons/04-DGE_DESeq2_analysis.Rmd",
  "lessons/05-DGE_DESeq2_analysis.Rmd",
  "lessons/06-DGE_visualizing_results.Rmd",
  "lessons/07-DGE_summarizing_workflow.Rmd",
  "lessons/08-GO_enrichment_analysis.Rmd"
)

# Convert all files
for (file in files_to_convert) {
  instructor_file <- file.path("/Users/guri/RProjects/2025_PHAR520-instructor/2025_RNA-Seq-Analysis-instructor", file)
  student_file <- file.path("/Users/guri/RProjects/2025_PHAR520/2025_RNA-Seq-Analysis", file)
  
  if (file.exists(instructor_file)) {
    create_student_version(instructor_file, student_file)
  } else {
    cat("Warning: File not found:", instructor_file, "\n")
  }
}
