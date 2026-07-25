#| eval: true
#| echo: false
#| include: false

# 1. Get the absolute path of the current .qmd file (returns NULL if interactive)
current_file <- knitr::current_input(dir = TRUE)

# 2. Only run the extraction if the document is actively being rendered
if (!is.null(current_file)) {
  
  # Extract the milestone directory
  milestone_dir <- dirname(current_file)
  
  # Replace .qmd with a more descriptive suffix (e.g., _R_code.R)
  base_name <- sub("\\.qmd$", "_R_code.R", basename(current_file))
  
  # Construct the target path
  output_script <- file.path(milestone_dir, base_name)
  
  # Extract the code
  knitr::purl(
    input = current_file, 
    output = output_script, 
    documentation = 0,
    quiet = TRUE
  )
}



# ------------------------ INTERACTIVE EXTRACTION --------------------
# 1. Get the path of your active Quarto document interactively
current_file <- rstudioapi::getSourceEditorContext()$path

if (!is.null(current_file)) {
  # 2. Extract directory and construct output path
  milestone_dir <- dirname(current_file)
  base_name <- sub("\\.qmd$", "_R_code.R", basename(current_file))
  output_script <- file.path(milestone_dir, base_name)
  
  # 3. Purl the document
  knitr::purl(
    input = current_file, 
    output = output_script, 
    documentation = 0,
    quiet = TRUE
  )
  
  message("Code successfully extracted to: ", output_script)
}