-- Author: Christian Blum, member of Klump Lab
-- Created on: 2025-11-12
-- This basic data upload script takes a CSV file from a specified path and uploads it to the database.

-- Quick tips on coding in SQL:
-- Use "-- " to denote single-line comments. "#" does not work with other database systems.
-- Use single quotes '' for string literals. Double quotes "" do not work in SQL for this purpose.
-- Always end SQL statements with a semicolon ";" for clarity and compatibility across systems.
-- Use descriptive table and column names to make your database easier to understand and maintain.
-- Test your script on a small dataset before running it on large files to avoid unexpected issues.
-- Ensure your table schema matches the structure and data types of your CSV file.

-- Step 1: Create the table in the database
-- Define the table structure based on the CSV file. Ensure column names and data types match the file exaclty (case sensitive).
-- Ensure you use appropriate column types, see end of the script for details, or use my R script "datatype scan before upload".
CREATE TABLE FOC_rawDataCombined (
    Rownumber INT, -- Row number (integer)
    Source_Name VARCHAR(255), -- Source name (text, max length 255 characters)
    Scoring VARCHAR(255), -- Scoring information (text)
    Start FLOAT, -- Start time (floating-point number)
    Start_Frame INT, -- Start frame (integer)
    Stop FLOAT, -- Stop time (floating-point number)
    Stop_Frame INT, -- Stop frame (integer)
    Subject VARCHAR(255), -- Subject name (text)
    Behaviour VARCHAR(255), -- Behaviour description (text)
    Value VARCHAR(255), -- Value (text)
    Partner VARCHAR(255), -- Partner name (text)
    Initiation VARCHAR(255), -- Initiation type (text)
    Termination VARCHAR(255), -- Termination type (text)
    Label_Focal_Crow_SO VARCHAR(255), -- Label for focal crow (text)
    Modifiers VARCHAR(255) -- Modifiers (text)
);




-- Step 2: Load data from the CSV file into this newly created table
-- Adjust the file path and delimiters as needed.
LOAD DATA INFILE 'C:/ucloud/Work/Database files/datasets/FOC_raw_data_sheets_combined.csv' -- Specify full path of your csv file
INTO TABLE FOC_rawDataCombined -- ensure this matches the newly created table above
FIELDS TERMINATED BY ',' -- Specify delimiter (',' for comma-delimited CSV files (most common) or '\t' for tab-delimited files)
LINES TERMINATED BY '\n' -- Standard terminator for line endings
IGNORE 1 ROWS; -- Ignore the header row in the CSV file, otherwise they will be imported as standard data

-- Step 3: Verify the data was uploaded correctly
-- Display the first 10 rows of the table to confirm the data was imported successfully.
SELECT * FROM FOC_rawDataCombined LIMIT 10;

-- Additional tips:
-- If you encounter errors during the import, check the following:
-- 1. Ensure the file path is correct ( Windows uses '\', SQL uses '/')
-- 2. Verify that the table schema matches the structure of the CSV file (e.g., column order and data types).
-- 3. Check for empty fields in the CSV file. MySQL will insert NULL values if the table allows them. If you don't want that, replace them with NA before importing.
--    Be aware of the distinction between "subject didn't show behaviour X" and "subject was not present". Likewise, "no partner was present" and "partner could not be identified".
--    This is an important distinction, but goes beyond database management and should be discussed when setting up your data structures, statistical anaylsis plans etc.
-- 4. If using LOCAL files, ensure 'local_infile' is enabled:
-- 	  SHOW VARIABLES LIKE 'local_infile'; -- if this says 'no', you can enable it with the following command:
--    SET GLOBAL local_infile = 1;
-- 5. Use GRANT FILE privileges if you encounter permission issues:
--    GRANT FILE ON *.* TO 'your_username'@'localhost';


-- MySQL Column Types Summary:

-- Numeric Data Types:
-- 
-- TINYINT(size)    : Very small integer (-128 to 127 or 0 to 255 for UNSIGNED). Use for small numbers like flags (e.g., 0/1).
-- SMALLINT(size)   : Small integer (-32,768 to 32,767 or 0 to 65,535 for UNSIGNED).
-- MEDIUMINT(size)  : Medium integer (-8,388,608 to 8,388,607 or 0 to 16,777,215 for UNSIGNED).
-- INT(size)        : Standard integer (-2,147,483,648 to 2,147,483,647 or 0 to 4,294,967,295 for UNSIGNED).
-- BIGINT(size)     : Large integer (-9 quintillion to 9 quintillion). Use for very large numbers like IDs or counters.
-- DECIMAL(size, d) : Fixed-point number. Use for precise values like currency (e.g., DECIMAL(10,2) for 10 digits, 2 after decimal).
-- FLOAT(size, d)   : Approximate floating-point number. Use for less precise values like scientific data. Precise for approximately 6 digits.
-- DOUBLE(size, d)  : Double-precision floating-point number. More precise than FLOAT. Precise for approximately 15 digits.

-- Examples on differences between FLOAT, DOUBLE and DECIMAL (identical to NUMERIC):
SELECT CAST(1234567.890123 AS FLOAT); -- Output: '1234570', approximate precision, use for numbers around 7 digits
SELECT CAST(0.1234567890123 AS FLOAT); -- Output: '0.123457'
SELECT CAST(1234567890123456.7890123 AS DOUBLE); -- Output: '1234567890123456.8' , better precision (around 15 digits)
SELECT CAST(0.12345678901234567890123 AS DOUBLE); -- Output: '0.12345678901234568'
SELECT CAST(1234567890123456.7890123 AS DECIMAL(20, 10)); -- Output: '9999999999.9999999999' because 16 digits before decimal and 7 after is a total of 23 digits. i only specified 20 digits total, 10 after the decimal
SELECT CAST(1234567890123456.7890123 AS DECIMAL(23, 5)); -- Oputput: '1234567890123456.78901' rounds to 5 post decimals, as specified
SELECT CAST(1234567890123456.7890123 AS DECIMAL(23, 7)); -- Output: '1234567890123456.7890123' stores complete number
SELECT CAST(1234567890123456.7890123 AS DECIMAL(30, 10)); -- Output: '1234567890123456.7890123000' adds 0s after the decimal (padding), but not before, to specification

-- Date and Time Data Types:
-- DATE             : Stores a date in 'YYYY-MM-DD' format (e.g., '2023-10-15').
-- DATETIME         : Stores date and time in 'YYYY-MM-DD HH:MM:SS' format. Use for timestamps.
-- TIMESTAMP        : Similar to DATETIME, but auto-updates to the current timestamp on insert/update.
-- TIME             : Stores time in 'HH:MM:SS' format (e.g., '12:30:45').
-- YEAR             : Stores a year in 'YYYY' format (e.g., '2023').

-- String Data Types:
-- most common:
-- VARCHAR(size)    : Variable-length string (0-65,535 characters, depending on row size). Use for general text. 
-- 					  255 is the largest value that can bes tored in 1 byte, thus very common to set here.
-- alternatives:
-- CHAR(size)       : Fixed-length string (0-255 characters). Use for data with consistent length (e.g., country codes).
-- TEXT             : Large text (up to 65,535 characters). Cannot be indexed efficiently.
-- TINYTEXT         : Very small text (up to 255 characters).
-- MEDIUMTEXT       : Medium text (up to 16,777,215 characters).
-- LONGTEXT         : Very large text (up to 4GB). Use for large documents or logs.

-- Binary Data Types:
-- BLOB             : Binary Large Object. Use for storing binary data like images or files (up to 65,535 bytes).
-- TINYBLOB         : Very small binary data (up to 255 bytes).
-- MEDIUMBLOB       : Medium binary data (up to 16,777,215 bytes).
-- LONGBLOB         : Very large binary data (up to 4GB).

-- Enumerated and Set Data Types:
-- ENUM('value1', 'value2', ...) : A string object with predefined values. Use for limited options (e.g., ENUM('Male', 'Female')).
-- SET('value1', 'value2', ...)  : A string object that can store multiple predefined values (e.g., SET('A', 'B', 'C')).

-- Spatial Data Types (for GIS):
-- GEOMETRY         : Stores geometric data (e.g., points, lines, polygons).
-- POINT            : Stores a single point in 2D space.
-- LINESTRING       : Stores a line of points in 2D space.
-- POLYGON          : Stores a polygon in 2D space.

-- JSON Data Type:
-- JSON             : Stores JSON-formatted data. Use for semi-structured data (e.g., '{"key": "value"}').

-- Notes:
-- 1. Always choose the smallest data type that fits your data to optimize storage and performance.
-- 2. Use UNSIGNED for numeric types if negative values are not needed (e.g., IDs, counters).
-- 3. Use TEXT or BLOB types only when necessary, as they are stored separately and can impact performance.
-- 4. Use ENUM or SET for predefined categories to enforce consistency and save space.
-- 5. Use TIMESTAMP for automatic time tracking (e.g., created_at, updated_at).