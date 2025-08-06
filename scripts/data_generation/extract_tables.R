# Extract Tables from Sturman (2000) PDF
# Attempt to get Table 1 parameter ranges and Table 2 results

library(pdftools)
library(tabulizer)
library(dplyr)

cat("ATTEMPTING TO EXTRACT TABLES FROM STURMAN (2000) PDF\n")
cat("====================================================\n\n")

# First, let's see what PDFs we have
pdf_files <- list.files("articles", pattern = "*.pdf", full.names = TRUE)
cat("Available PDF files:\n")
print(pdf_files)

# Find the Sturman 2000 PDF
sturman_pdf <- pdf_files[grepl("Sturman.*Monte Carlo", pdf_files)]

if(length(sturman_pdf) > 0) {
  cat("\nFound Sturman PDF:", sturman_pdf, "\n")
  
  # Try to extract tables using tabulizer
  cat("\nAttempting to extract tables using tabulizer...\n")
  
  tryCatch({
    # Extract all tables from the PDF
    tables <- extract_tables(sturman_pdf)
    
    cat("Number of tables found:", length(tables), "\n")
    
    if(length(tables) > 0) {
      for(i in 1:length(tables)) {
        cat("\n", paste(rep("=", 50), collapse=""), "\n")
        cat("TABLE", i, "\n")
        cat(paste(rep("=", 50), collapse=""), "\n")
        
        table_df <- as.data.frame(tables[[i]])
        print(table_df)
        
        # Save each table
        write.csv(table_df, paste0("extracted_table_", i, ".csv"), row.names = FALSE)
      }
    }
  }, error = function(e) {
    cat("Error with tabulizer:", e$message, "\n")
    cat("Trying alternative approach...\n")
  })
  
  # Alternative: Extract text and look for table patterns
  cat("\nExtracting text to search for table patterns...\n")
  
  text <- pdf_text(sturman_pdf)
  
  # Look for Table 1 patterns
  cat("\nSearching for Table 1 (Parameter Ranges)...\n")
  for(page in 1:length(text)) {
    if(grepl("Table 1", text[page], ignore.case = TRUE)) {
      cat("Found Table 1 reference on page", page, "\n")
      
      # Extract lines around Table 1
      lines <- strsplit(text[page], "\n")[[1]]
      table_start <- which(grepl("Table 1", lines, ignore.case = TRUE))
      
      if(length(table_start) > 0) {
        # Print context around table
        start_line <- max(1, table_start[1] - 2)
        end_line <- min(length(lines), table_start[1] + 20)
        
        cat("Table 1 context:\n")
        for(i in start_line:end_line) {
          cat(i, ":", lines[i], "\n")
        }
      }
    }
  }
  
  # Look for Table 2 patterns
  cat("\nSearching for Table 2 (Results)...\n")
  for(page in 1:length(text)) {
    if(grepl("Table 2", text[page], ignore.case = TRUE)) {
      cat("Found Table 2 reference on page", page, "\n")
      
      # Extract lines around Table 2
      lines <- strsplit(text[page], "\n")[[1]]
      table_start <- which(grepl("Table 2", lines, ignore.case = TRUE))
      
      if(length(table_start) > 0) {
        # Print context around table
        start_line <- max(1, table_start[1] - 2)
        end_line <- min(length(lines), table_start[1] + 20)
        
        cat("Table 2 context:\n")
        for(i in start_line:end_line) {
          cat(i, ":", lines[i], "\n")
        }
      }
    }
  }
  
  # Look for specific parameter ranges in text
  cat("\nSearching for specific parameter ranges...\n")
  
  # Search for numeric ranges
  for(page in 1:length(text)) {
    lines <- strsplit(text[page], "\n")[[1]]
    
    # Look for patterns like "0.05 to 0.70" or "5,000 to 50,000"
    range_patterns <- c(
      "\\d+\\.\\d+\\s+to\\s+\\d+\\.\\d+",
      "\\d+,\\d+\\s+to\\s+\\d+,\\d+",
      "\\d+\\s+to\\s+\\d+",
      "between\\s+\\d+\\s+and\\s+\\d+"
    )
    
    for(pattern in range_patterns) {
      matches <- grep(pattern, lines, value = TRUE)
      if(length(matches) > 0) {
        cat("Found ranges on page", page, ":\n")
        for(match in matches) {
          cat("  ", match, "\n")
        }
      }
    }
  }
  
} else {
  cat("Sturman PDF not found. Available files:\n")
  print(pdf_files)
}

# Also check if we can find parameter information in the extracted text
cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("CHECKING EXTRACTED TEXT FOR PARAMETER RANGES\n")
cat(paste(rep("=", 60), collapse=""), "\n")

extracted_text <- "articles/extracted_text/2000; Sturman; Monte Carlo Sim.txt"

if(file.exists(extracted_text)) {
  text_content <- readLines(extracted_text)
  
  # Look for specific parameter mentions
  parameter_lines <- c()
  
  for(i in 1:length(text_content)) {
    line <- text_content[i]
    
    # Look for lines with parameter ranges
    if(grepl("\\d+\\.\\d+.*to.*\\d+\\.\\d+", line) || 
       grepl("between.*\\d+.*and.*\\d+", line) ||
       grepl("range.*\\d+", line) ||
       grepl("from.*\\d+.*to.*\\d+", line)) {
      parameter_lines <- c(parameter_lines, paste(i, ":", line))
    }
  }
  
  if(length(parameter_lines) > 0) {
    cat("Found potential parameter range lines:\n")
    for(line in parameter_lines) {
      cat(line, "\n")
    }
  } else {
    cat("No clear parameter ranges found in extracted text\n")
  }
  
} else {
  cat("Extracted text file not found\n")
}

cat("\nDone. Check extracted_table_*.csv files if any tables were found.\n") 