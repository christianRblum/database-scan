Author: Christian Blum, member of Barbara Klump lab

Created on: 2025-11-13

Description:
This R script helps in identifying suitable SQL column types before uploading data to the database.
It also scans for blank cells, extreme values, values of a different type (e.g., "false" in a column with otherwise only 1s and 0s) and some basic code injections.
It does not replace human attention, so always check your data in detail!

Also included an SQL script as example for data upload.
