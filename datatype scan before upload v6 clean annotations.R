# Author: Christian Blum, member of Barbara Klump lab
# Created on: 2025-11-13
# Description:
# This script helps in identifying suitable SQL column types before uploading data to the database.
# It also scans for blank cells, extreme values, values of a different type (e.g., "false" in a column with otherwise only 1s and 0s).
# It does not replace human attention, so always check your data in detail!

# --- Relevant SQL Data Types ---
# TINYINT: For binary or small categorical data (e.g., flags, Likert scales).
# SMALLINT: For small numeric ranges (e.g., counts, small scores).
# INT: General-purpose integer type for IDs or larger numeric ranges.
# BIGINT: Necessary for very large integers (e.g., large counts or IDs).
# DECIMAL(p, s): Essential for exact numeric values requiring precision (e.g., reaction times, financial data).
# DOUBLE: Important for high-precision approximate numeric values (e.g., scientific measurements).
# VARCHAR(n): Flexible and efficient for variable-length text fields (e.g., participant IDs, experiment names).
# TEXT: Ideal for long text fields (e.g., descriptions, open-ended responses).
# DATE: Suitable for date-only values (e.g., experiment dates).
# TIMESTAMP: Critical for time-sensitive data (e.g., event timestamps).
# BOOLEAN: Perfect for binary flags (e.g., "is_active", "is_correct").
# ENUM: Useful for categorical data with predefined values (e.g., stimulus categories).

# Load required libraries
library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(tidyr)

# CHAPTER 1: Load Data ####

## Create example data using nycflights13 ####
# If you have existing data you want to scan, skip this step 

# Create data
library(nycflights13)
data <- flights %>%
  select(carrier, flight, tailnum, origin, dest, distance, year, month, day, hour, minute, time_hour) %>%
  left_join(weather %>% select(origin, time_hour, temp, humid), by = c("origin", "time_hour")) %>%
  slice(1:15) %>%  # Keep only the first 15 rows, enough for testing purposes
  mutate(
    `distance. .metRic` = distance * 1609.34  # Convert miles to meters, give strange name
  )

# Add NA, blank, and suspicious contents to example data
data$carrier[1] <- NA       # Set row 1 of 'carrier' column to NA
data$carrier[2] <- ""       # Set row 2 of 'carrier' column to an empty string
data$carrier[3] <- "Robert');DROP TABLE Students;--"  # SQL injection attempt, see https://xkcd.com/327/
data$carrier[4] <- "SELECT * FROM users;"  # Another SQL injection attempt
data$carrier[5] <- as.numeric("5")  # Different data type

data$flight[4] <- NA        # Set row 4 of 'flight' column to NA
data$flight[8] <- NA        # Set row 8 of 'flight' column to NA
data$flight[10] <- -99999   # Extreme negative value (anomaly)

data$origin[1] <- ""        # Set row 1 of 'origin' column to an empty string
data$origin[2] <- ""        # Set row 2 of 'origin' column to an empty string
data$origin[5] <- "DROP DATABASE test;"  # SQL injection attempt

data$dest[7] <- "New York; DELETE FROM flights;"  # SQL injection attempt
data$dest[9] <- "Los Angeles--"  # Suspicious double dash

data$distance[3] <- -5000   # Negative distance (anomaly)
data$distance[6] <- 999999999999  # Extremely large value (anomaly)

data$temp[2] <- -273.15     # Absolute zero temperature (anomaly)
data$temp[5] <- 1000        # Extremely high temperature (anomaly)
data$temp[7] <- "Text Typo" # Different data type

data$humid[4] <- -10        # Negative humidity (anomaly)
data$humid[8] <- 200        # Humidity above 100% (anomaly)

# create duplicate rows
idx <- c(2L, 4L, 33L)

data <- data %>%
  mutate(n = if_else(row_number() %in% idx, 2L, 1L)) %>%
  uncount(n)

# Clean column names using janitor
data <- data %>% janitor::clean_names()

## Load existing data ####
#data <- read_csv("path/to/your/data.csv")
# data = read_csv("C:/ucloud/Work/TA Palmyre/Loopy Coding/Loop API Scripts/Loopy_HH_Video_Summary_full.csv")


# CHAPTER 2: Define Helper Functions ####

# but first, check for duplicate rows
data[duplicated(data), ]

# if there are any, remove them
data <- data %>% distinct(.keep_all = TRUE)


# Function to convert snake_case to camelCase using janitor
convert_to_camel_case <- function(name) {
  # Use janitor to clean names first
  name <- janitor::make_clean_names(name)
  
  # Convert snake_case to camelCase
  words <- unlist(strsplit(name, "_"))
  camel_case <- paste0(words[1], paste0(toupper(substring(words[-1], 1, 1)), substring(words[-1], 2), collapse = ""))
  return(camel_case)
}

convert_to_camel_case <- function(name) {
  name <- janitor::make_clean_names(name)          # use janitor to normalize to clean snake_case
  words <- unlist(strsplit(name, "_"))             # split into words by underscore
  camel_case <- paste0(                            # convert to to camelCase in the following steps:
    words[1],                                      # first word stays lowercase
    paste0(                                        # append transformed subsequent words
      toupper(substring(words[-1], 1, 1)),         # make uppercase each first letter of each subsequent word
      substring(words[-1], 2),                     # keep the rest of each subsequent word as is
      collapse = ""                                # join without separators
    )
  )
  return(camel_case)
}


# Function to detect SQL data type
detect_sql_type <- function(column) {
  if (all(is.numeric(column), na.rm = TRUE)) {                       # if numeric (integer or decimal)
    if (all(column == floor(column), na.rm = TRUE)) {                # if all values are whole numbers -> integer types
      max_val <- max(column, na.rm = TRUE)                           # largest observed integer
      min_val <- min(column, na.rm = TRUE)                           # smallest observed integer
      if (max_val <= 2147483647 && min_val >= -2147483648) {         # if range falls within 32-bit signed int range -> INT
        return("INT")
      } else {
        return("BIGINT")                                             # if larger -> 64-bit integer range (BIGINT)
      }
    } else {
      # Helper: count significant digits for each value (from string, no scientific notation)
      non_na <- column[!is.na(column)]
      str_vals <- format(non_na, scientific = FALSE, trim = TRUE)    # preserve literal decimals
      # Compute per-value significant digits by removing non-digits and leading zeros appropriately
      sig_digits <- vapply(str_vals, function(s) {
        s <- sub("^-", "", s)                                        # drop sign HELP
        if (!grepl("\\.", s)) {                                      # integer-like string HELP
          nchar(gsub("^0+", "", s))                                  # strip leading zeros; remaining digits are significant HELP
        } else {
          parts <- strsplit(s, "\\.", fixed = FALSE)[[1]]
          int <- gsub("^0+", "", parts[1])                           # drop leading zeros in integer part HELP
          frac <- parts[2]
          if (nzchar(int)) {
            nchar(int) + nchar(frac)                                 # int digits + all provided fractional digits HELP
          } else {
            nchar(gsub("^0+", "", frac))                             # numbers < 1: skip leading fractional zeros HELP
          }
        }
      }, integer(1))
      sig_required <- if (length(sig_digits)) max(sig_digits) else 0  # maximum significant digits needed
      
      # Also estimate decimal places across original values (for DECIMAL fallback)
      decimal_places <- max(
        nchar(gsub(".*\\.", "", as.character(column))),              # count digits after decimal point
        na.rm = TRUE
      )
      max_val <- max(column, na.rm = TRUE)
      min_val <- min(column, na.rm = TRUE)
      # Estimate total precision for DECIMAL fallback (digits in extremes + scale)
      precision <- max(
        nchar(gsub("[^0-9]", "", as.character(max_val))),
        nchar(gsub("[^0-9]", "", as.character(min_val)))
      ) + decimal_places
      
      # Choose FLOAT/DOUBLE if they can represent the needed significant digits
      # FLOAT (~7 sig figs), DOUBLE (~15-16 sig figs)
      if (sig_required <= 7) {
        return("FLOAT")                                                 # up to 7 significant digits -> FLOAT
      } else if (sig_required <= 15) {
        return("DOUBLE")                                                # up to 15 significant digits -> DOUBLE
      } else {
        return(paste0("DECIMAL(", precision, ",", decimal_places, ")")) # otherwise DECIMAL with exact specifications
      }
    }
  } else if (all(is.character(column), na.rm = TRUE)) {              # if character/text data
    max_length <- max(nchar(column), na.rm = TRUE)                   # longest string length
    headroom <- ceiling(max_length * 1.3)                            # add ~30% headroom
    if (headroom <= 255) {                                            
      return(paste0("VARCHAR(", headroom, ")"))                      # if less than 255 character -> VARCHAR(n)
    } else {
      return("TEXT")                                                 # if more -> TEXT
    }
  } else if (inherits(column, "POSIXct") || inherits(column, "POSIXlt")) { # if datetime type -> DATETIME
    return("DATETIME")
  } else if (all(is.logical(column), na.rm = TRUE)) {                # if TRUE/FALSE -> BOOLEAN
    return("BOOLEAN")
  } else {
    return("TEXT")                                                   # otherwise TEXT as fallback for unsupported/mixed types
  }
}


# Function to detect anomalies in a column
detect_anomalies <- function(column) {
  if (is.numeric(column)) {
    non_na <- column[!is.na(column)]
    if (length(non_na) == 0) return(list(values = numeric(0), percentage = "0%"))
    lower_bound <- quantile(non_na, 0.01, na.rm = TRUE)
    upper_bound <- quantile(non_na, 0.99, na.rm = TRUE)
    mask <- !is.na(column) & (column < lower_bound | column > upper_bound)
    anomalies <- column[mask]
    anomaly_percentage <- round((sum(mask) / length(column)) * 100, 2)  # percent of all rows
    return(list(values = anomalies, percentage = paste0(anomaly_percentage, "%")))
  } else if (is.character(column)) {
    # Detect non-character values
    anomalies <- column[!is.character(column)]
    anomaly_percentage <- round((length(anomalies) / length(column)) * 100, 2)
    return(list(values = anomalies, percentage = paste0(anomaly_percentage, "%")))
  } else if (inherits(column, "POSIXct") || inherits(column, "POSIXlt")) {
    # Detect invalid datetime values
    anomalies <- column[!lubridate::is.POSIXct(column)]
    anomaly_percentage <- round((length(anomalies) / length(column)) * 100, 2)
    return(list(values = anomalies, percentage = paste0(anomaly_percentage, "%")))
  } else {
    return(NULL)
  }
}

# Function to check for missing values
check_missing_values <- function(data) {
  missing_info <- sapply(data, function(column) {
    percent_missing <- round(sum(is.na(column)) / length(column) * 100, 2)  # Round to 2 decimal places
    return(paste0(percent_missing, "%"))
  })
  return(missing_info[as.numeric(sub("%", "", missing_info)) > 0])  # Return columns with missing values
}

# Function to detect SQL injection patterns
detect_sql_injection <- function(column) {
  # Define a list of suspicious patterns
  suspicious_patterns <- c("';", "--",
                           "\\bDROP\\b", "\\bDELETE\\b", "\\bINSERT\\b",
                           "\\bSELECT\\b", "\\bUPDATE\\b", "\\bCREATE\\b", "\\bALTER\\b")
  
  # Check for matches in the column
  matches <- sapply(suspicious_patterns, function(pattern) {
    grep(pattern, column, ignore.case = TRUE, value = TRUE, perl = TRUE)
  })
  
  # Combine all matches into a single vector
  matches <- unlist(matches)
  
  # Return unique matches
  return(unique(matches))
}

# Function to detect out-of-place entries in a column
detect_out_of_place_entries <- function(column) {
  if (is.numeric(column)) {
    # Detect non-numeric values in a numeric column
    non_numeric_values <- column[!is.numeric(column)]
    if (length(non_numeric_values) > 0) {
      return(list(type = "Non-numeric values in numeric column", values = non_numeric_values))
    }
  } else if (is.character(column)) {
    # Detect unusually long strings in a character column
    string_lengths <- nchar(column)
    max_length <- max(string_lengths, na.rm = TRUE)
    typical_length <- quantile(string_lengths, 0.95, na.rm = TRUE)  # 95th percentile
    outliers <- column[string_lengths > typical_length]
    if (length(outliers) > 0) {
      return(list(type = "Unusually long strings", values = outliers))
    }
  }
  return(NULL)  # Return NULL if no out-of-place entries are detected
}

# Function to generate SQL code
generate_sql <- function(data) {
  # Use the name of the data object as the table name
  table_name <- deparse(substitute(data))
  
  # Clean column names
  cleaned_column_names <- sapply(colnames(data), convert_to_camel_case)
  
  # Infer SQL data types for each column
  data_types <- sapply(data, detect_sql_type)
  
  # Generate SQL CREATE TABLE statement
  create_table_sql <- paste0(
    "CREATE TABLE ", table_name, " (\n", 
    paste0("    ", cleaned_column_names, " ", data_types, collapse = ",\n"),
    "\n);"
  )
  
  # Generate SQL LOAD DATA INFILE statement
  load_data_sql <- paste0(
    "LOAD DATA INFILE '", "path/to/your/file.csv", "'\n",
    "INTO TABLE ", table_name, "\n",
    "FIELDS TERMINATED BY ','\n",
    "LINES TERMINATED BY '\\n'\n",
    "IGNORE 1 ROWS;"
  )
  
  # Print SQL code
  cat("----SQL Code:----\n\n")
  cat(create_table_sql, "\n\n")
  cat(load_data_sql, "\n\n")
  
  # Detect anomalies
  anomalies <- lapply(data, detect_anomalies)
  anomalies <- anomalies[sapply(anomalies, function(x) length(x$values) > 0)]  # Keep only columns with anomalies
  
  # Check for missing values
  missing_values <- check_missing_values(data)
  
  # Detect SQL injection patterns
  suspicious_values <- lapply(data %>% select(where(is.character)), detect_sql_injection)
  suspicious_values <- suspicious_values[sapply(suspicious_values, length) > 0]  # Keep only columns with suspicious values
  
  # Detect out-of-place entries
  out_of_place_entries <- lapply(data, detect_out_of_place_entries)
  out_of_place_entries <- out_of_place_entries[!sapply(out_of_place_entries, is.null)]  # Keep only columns with out-of-place entries
  
  # Print anomalies separately
  if (length(anomalies) > 0) {
    cat("\n----END OF SQL CODE----\nDetected anomalies (extreme values):\n")
    for (col in names(anomalies)) {
      cat(paste0(col, " (", anomalies[[col]]$percentage, "):\n"))
      print(utils::head(unique(na.omit(anomalies[[col]]$values)), num_error))
    }
  } else {
    cat("\n----END OF SQL CODE----\nNo anomalies detected (extreme values).\n")
  }
  
  # Print missing values separately
  if (length(missing_values) > 0) {
    cat("\nColumns with missing values:\n")
    print(utils::head(missing_values, num_error))
  } else {
    cat("\nNo missing values detected.\n")
  }
  
  # Print suspicious values separately
  if (length(suspicious_values) > 0) {
    cat("\nSuspicious values detected (potential SQL injection):\n")
    for (col in names(suspicious_values)) {
      cat(paste0("Column: ", col, "\n"))
      print(utils::head(unique(na.omit(suspicious_values[[col]])), num_error))
    }
  } else {
    cat("\nNo suspicious values detected (SQL injection).\n")
  }
  
  # Print out-of-place entries separately
  if (length(out_of_place_entries) > 0) {
    cat("\nOut-of place entries detected:\n")
    for (col in names(out_of_place_entries)) {
      cat(paste0("Column: ", col, " (", out_of_place_entries[[col]]$type, "):\n"))
      print(utils::head(unique(na.omit(out_of_place_entries[[col]]$values)), num_error))
    }
  } else {
    cat("\nNo out-of-place entries detected.\n")
  }
  # Store full lists of issues 
  return(list(
    anomalies = anomalies,
    missing_values = missing_values,
    suspicious_values = suspicious_values,
    out_of_place_entries = out_of_place_entries
  ))
}

# CHAPTER 4: Run the Script ####

## Decide how many lines of errors should be depicted: ####
num_error = 2

## Generate SQL code ####
results <- generate_sql(data)


# Print lists of issues ####
cat("\n----FULL LISTS----\n")


## Anomalies (extreme values) ####
# Get column names, number of affected rows and unique values
if (length(results$anomalies) > 0) {
  cat("\nFull anomalies (extreme values):\n")
  for (col in names(results$anomalies)) {
    vals <- results$anomalies[[col]]$values
    affected_rows <- length(vals)
    unique_count <- length(unique(na.omit(vals)))
    cat(sprintf("%s (%s) — rows affected: %d, unique values: %d\n",
                col, results$anomalies[[col]]$percentage, affected_rows, unique_count))
  }
} else {cat("None detected")}
  
# Get values per column
if (length(results$anomalies) > 0) {
  cat("\nFull anomalies (extreme values):\n")
  for (col in names(results$anomalies)) {
    cat(paste0(col, " (", results$anomalies[[col]]$percentage, "):\n"))
    print(unique(na.omit(results$anomalies[[col]]$values)))
  }
} else {cat("None detected")}

## Missing values ####
# Get column names, number of affected rows and unique values
if (length(results$missing_values) > 0) {
  cat("\nFull missing values:\n")
  for (col in names(results$missing_values)) {
    pct <- results$missing_values[[col]]
    # Without original data here we can only report the percentage; NA is the only "value"
    cat(sprintf("%s (%s) — rows affected: (unknown), unique values: 1 (NA)\n", col, pct))
  }
} else {cat("None detected")}


# Get values per column
if (length(results$missing_values) > 0) {
  cat("\nMissing values per column:\n")
  for (col in names(results$missing_values)) {
    cat(paste0(col, " (", results$missing_values[[col]], "):\n"))
    print("NA")
  }
} else {cat("None detected")}

## Suspicious values (SQL injection) ####
# Get column names, number of affected rows and unique values
if (length(results$suspicious_values) > 0) {
  cat("\nFull suspicious values (potential SQL injection):\n")
  for (col in names(results$suspicious_values)) {
    vals <- results$suspicious_values[[col]]
    affected_rows <- length(vals)
    unique_count <- length(unique(na.omit(vals)))
    cat(sprintf("%s — rows affected: %d, unique values: %d\n", col, affected_rows, unique_count))
  }
} else {cat("None detected")}

# Get values per column
if (length(results$suspicious_values) > 0) {
  cat("\nSuspicious values per column (potential SQL injection):\n")
  for (col in names(results$suspicious_values)) {
    cat(paste0("Column: ", col, "\n"))
    print(unique(na.omit(results$suspicious_values[[col]])))
  }
} else {cat("None detected")}

## Out of place entries ####
# Get column names, number of affected rows and unique values
if (length(results$out_of_place_entries) > 0) {
  cat("\nFull out-of-place entries:\n")
  for (col in names(results$out_of_place_entries)) {
    vals <- results$out_of_place_entries[[col]]$values
    affected_rows <- length(vals)
    unique_count <- length(unique(na.omit(vals)))
    cat(sprintf("%s (%s) — rows affected: %d, unique values: %d\n",
                col, results$out_of_place_entries[[col]]$type, affected_rows, unique_count))
  }
} else {cat("None detected")}

# Get values per column
if (length(results$out_of_place_entries) > 0) {
  cat("\nOut-of-place entries per column:\n")
  for (col in names(results$out_of_place_entries)) {
    cat(paste0("Column: ", col, " (", results$out_of_place_entries[[col]]$type, "):\n"))
    print(unique(na.omit(results$out_of_place_entries[[col]]$values)))
  }
} else {cat("None detected")}


