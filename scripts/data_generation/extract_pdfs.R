# PDF Text Extraction Script
# This script extracts text from PDF files in the articles folder

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Install and load required packages
if (!require("pdftools")) {
  install.packages("pdftools")
  library(pdftools)
}

if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}

# Function to extract text from a single PDF and save as text file
extract_pdf_text <- function(pdf_path, output_dir = "articles/extracted_text") {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Extract filename without extension
  filename <- tools::file_path_sans_ext(basename(pdf_path))
  
  # Extract text from PDF
  cat("Extracting text from:", filename, "\n")
  
  tryCatch({
    # Read PDF text
    text <- pdf_text(pdf_path)
    
    # Combine all pages
    full_text <- paste(text, collapse = "\n\n--- PAGE BREAK ---\n\n")
    
    # Create output filename
    output_file <- file.path(output_dir, paste0(filename, ".txt"))
    
    # Write to text file
    writeLines(full_text, output_file, useBytes = TRUE)
    
    cat("✓ Successfully extracted:", filename, "\n")
    cat("  Output:", output_file, "\n")
    cat("  Pages:", length(text), "\n")
    cat("  Characters:", nchar(full_text), "\n\n")
    
    return(TRUE)
    
  }, error = function(e) {
    cat("✗ Error extracting", filename, ":", e$message, "\n\n")
    return(FALSE)
  })
}

# Get all PDF files in the articles directory
pdf_files <- list.files("articles", pattern = "\\.pdf$", full.names = TRUE)

cat("Found", length(pdf_files), "PDF files:\n")
for (file in pdf_files) {
  cat("-", basename(file), "\n")
}
cat("\n")

# Extract text from all PDFs
cat("Starting extraction...\n\n")
results <- sapply(pdf_files, extract_pdf_text)

# Summary
successful <- sum(results, na.rm = TRUE)
total <- length(pdf_files)

cat("=== EXTRACTION COMPLETE ===\n")
cat("Successfully extracted:", successful, "out of", total, "files\n")

if (successful > 0) {
  cat("\nExtracted text files are saved in: articles/extracted_text/\n")
  cat("You can now read these text files to analyze the content.\n")
} 