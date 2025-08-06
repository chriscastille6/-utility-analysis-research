# Extract Tables from Sturman (2000) PDF - Simple Version
# Focus on text extraction to find parameter ranges

library(pdftools)
library(dplyr)

cat("EXTRACTING PARAMETER INFORMATION FROM STURMAN (2000)\n")
cat("====================================================\n\n")

# Find the Sturman PDF
pdf_files <- list.files("articles", pattern = "*.pdf", full.names = TRUE)
sturman_pdf <- pdf_files[grepl("Sturman.*Monte Carlo", pdf_files)]

if(length(sturman_pdf) > 0) {
  cat("Found Sturman PDF:", sturman_pdf, "\n\n")
  
  # Extract text from PDF
  text <- pdf_text(sturman_pdf)
  cat("PDF has", length(text), "pages\n\n")
  
  # Search each page for tables and parameter information
  for(page in 1:length(text)) {
    lines <- strsplit(text[page], "\n")[[1]]
    
    # Look for Table 1
    if(any(grepl("Table 1", lines, ignore.case = TRUE))) {
      cat("=== FOUND TABLE 1 ON PAGE", page, "===\n")
      
      # Find the table start
      table_line <- which(grepl("Table 1", lines, ignore.case = TRUE))[1]
      
      # Print context around the table
      start_context <- max(1, table_line - 3)
      end_context <- min(length(lines), table_line + 25)
      
      for(i in start_context:end_context) {
        cat(sprintf("%2d: %s\n", i, lines[i]))
      }
      cat("\n")
    }
    
    # Look for Table 2
    if(any(grepl("Table 2", lines, ignore.case = TRUE))) {
      cat("=== FOUND TABLE 2 ON PAGE", page, "===\n")
      
      # Find the table start
      table_line <- which(grepl("Table 2", lines, ignore.case = TRUE))[1]
      
      # Print context around the table
      start_context <- max(1, table_line - 3)
      end_context <- min(length(lines), table_line + 25)
      
      for(i in start_context:end_context) {
        cat(sprintf("%2d: %s\n", i, lines[i]))
      }
      cat("\n")
    }
    
    # Look for parameter ranges
    range_lines <- grep("\\d+\\.\\d+.*to.*\\d+\\.\\d+|between.*\\d+.*and.*\\d+|from.*\\d+.*to.*\\d+", lines, value = TRUE)
    if(length(range_lines) > 0) {
      cat("=== PARAMETER RANGES ON PAGE", page, "===\n")
      for(line in range_lines) {
        cat("  ", line, "\n")
      }
      cat("\n")
    }
  }
  
} else {
  cat("Sturman PDF not found. Available files:\n")
  print(pdf_files)
}

# Also search the extracted text file more systematically
cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("SYSTEMATIC SEARCH OF EXTRACTED TEXT\n")
cat(paste(rep("=", 70), collapse=""), "\n")

extracted_text <- "articles/extracted_text/2000; Sturman; Monte Carlo Sim.txt"

if(file.exists(extracted_text)) {
  text_content <- readLines(extracted_text)
  
  # Search for specific keywords and patterns
  keywords <- c("Table 1", "Table 2", "parameter", "range", "uniform", "exponential", 
                "minimum", "maximum", "between", "from.*to", "\\d+\\.\\d+.*to.*\\d+\\.\\d+")
  
  for(keyword in keywords) {
    matches <- grep(keyword, text_content, ignore.case = TRUE)
    if(length(matches) > 0) {
      cat("\n--- LINES CONTAINING '", keyword, "' ---\n")
      for(match in matches[1:min(10, length(matches))]) {  # Limit to first 10 matches
        # Print line with context
        start_line <- max(1, match - 1)
        end_line <- min(length(text_content), match + 1)
        
        for(i in start_line:end_line) {
          prefix <- if(i == match) ">>> " else "    "
          cat(sprintf("%s%3d: %s\n", prefix, i, text_content[i]))
        }
        cat("\n")
      }
    }
  }
  
  # Look specifically for the 291% and 298% results
  cat("\n--- SEARCHING FOR STURMAN'S KEY RESULTS ---\n")
  result_patterns <- c("291", "298", "median.*effect", "mean.*effect")
  
  for(pattern in result_patterns) {
    matches <- grep(pattern, text_content, ignore.case = TRUE)
    if(length(matches) > 0) {
      cat("\nLines containing '", pattern, "':\n")
      for(match in matches) {
        cat(sprintf("%3d: %s\n", match, text_content[match]))
      }
    }
  }
  
} else {
  cat("Extracted text file not found at:", extracted_text, "\n")
}

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("SUMMARY OF FINDINGS\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("Based on the analysis, here's what we know about Sturman's parameters:\n")
cat("1. n (number hired): 1 to 1100, exponential distribution\n")
cat("2. cost: exponential distribution (range not fully specified)\n")
cat("3. r_old (for L&W): 0.05 to 0.38 uniform\n")
cat("4. All other parameters: uniform distributions\n")
cat("5. Key result: 291% median, 298% mean reduction\n")
cat("6. 16% of cases become negative\n")
cat("7. Minimum reduction: 71%\n\n")

cat("Our current results (~94%) suggest we may need to:\n")
cat("- Adjust parameter ranges to match Table 1 exactly\n")
cat("- Verify adjustment formula implementations\n")
cat("- Check if there are interaction effects we're missing\n") 