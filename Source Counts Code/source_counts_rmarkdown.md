---
title: "IHME Source Counts"
author: "Christian Hernandez (chrish47@uw.edu), Helen Ippolito (helenipp@uw.edu)"
date: '2022-11-22'
output:
  html_document:
    keep_md: yes
    df_print: paged
    theme: paper #journal,paper,lumen,united,cosmo
    code_folding: hide
---
## **Data Landscaping - Source Counts**
<hr style="border:2px solid gray">
#### <span style="color:red">**Using the Source Counts Function**</span>  
(1) Make sure you are on the cluster for this, and that have you have an R interactive session opened. Please see the HUB for more details.  
(2) Script: "/ihme/scratch/projects/rgud/Source Counts Tool 2022/Code/Source Counts Code/source_counts_master.R"  
    • Run this on your console: `source("/ihme/scratch/projects/rgud/Source Counts Tool 2022/Code/Source Counts Code/source_counts_master.R")`  
    [ You don't have to run anything else. Simply source in the above script. ]  
    • Source counts function: `source_counts()`  
    • You can enter a `crosswalk_version_id, bundle_version_id, or a custom data table`.  
    • Please make sure your custom data table input contains the following columns: `nid, location_id, location_name, year_start, and year_end`.  
(3) Source in your location_data. REQUIRED.   
    • Function: `get_location_metadata()`  
    • Most common *location_set_id: 35*  
    • Please use the *location_data* object name.  
    `location_data <- ...`  
(4) When using the source_counts(), please make sure your out_path filepath exists.  
(6) For example calls and explanations, please see the section below Title 'Example Calls'.    

* If you run into the following error:  
`Error in .local(drv, ...) : Cannot allocate a new connection: 16 connections already opened`  
  - Run the following line on your console: `disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)`  

<hr style="border:2px solid gray">
#### <span style="color:red">**Source Counts Strategy - Mechanics**</span>  
##### **We Can Only Count 'Sources' That Meet The Following Criteria**
• It is a GBD project with data that comes through our sources data ingestion pipeline. We don't count custom data outside our sources database.  
• The source data corresponds to valid NIDs that are published in the GHDx. Our source counts are all based on GHDx Record NIDs.  
• The source counts requested can be fulfilled with the data we have in our sources database / source counts tables. Data we have in-house. 

##### **General NID strategy Overview**
(1) ***NID Relationships***  
    • If a Record NID or replaced underlying NID is a Merged Citation, we break apart the Merged Citation into its associated component NIDs.  
    • If a Record NID has an underlying NID, the Underlying NID is used.[Currently Being Investigated!]    
(2) ***NID’s Data Type***  
    • Every GHDx Record NID has a data type (it is required).  
    • In some counting strategies, records are counted differently depending on data type. 
(3) ***NID Validity***  
    • NIDs must be valid, published, Record NIDs from the GHDx. We do not count “file” NIDs, “series or system” NIDs, aggregate NIDs, or any fake NIDs.
    
##### **Source Counting Method**  
• Source counts will be generated using the following Counting Strategy Combination  

`NID + location + year_start + year_end`

• The above combination strategy is applied accross all data types.  
• According to the GBD strategy Scientific literature, Financial records, Survey, Census, and Clinical trial should be counted by NID only. `source_counts()` doesn't count these data types by NID only - It counts them by the above combination as with all the other data types. We believe implementing this combination counting method across all data types is more accurate than doing it different accross different data types.  

[For more details on the GBD Source Counts strategy, please click here](https://hub.ihme.washington.edu/display/SCDS/Source+Counts+-+Source+Counting+Strategy)

<hr style="border:2px solid gray">
#### <span style="color:red">**Settup**</span> 

```r
rm(list=ls())
os <- .Platform$OS.type
if (os == "windows") {
  j <- "J:/"
  h <- "H:/"
  l <- "L:/"
} else {
  j <- "/home/j/"
  user <- Sys.info()[["user"]]
  h <- paste0("/ihme/homes/", user, "/")
  l <- "/ihme/limited_use/"
}
# Libraries
library(stringr)
library(openxlsx)
library(RMySQL)
library(dplyr)
library(tidyr)
library(data.table)
library(reshape)
library(reshape2)
library(knitr)
```


```r
user <- Sys.getenv("USER")
#Sourcing in Shared Functions
invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
```
#### <span style="color:red">**Sourcing in Location Data for Source Counts**</span>  
• In order to use the Source Counts code and generate accurate outputs. <ins>You must source in your Location Data</ins>.  
• Please source in the adequate location data that will accurately incorporate all locations ids in your dataset.  
• If for some reason your final source count ouput(s) contain empty location_id or location_name, this signals that the location data your sourced in didn't fully cover all the location in your  

**Please make sure you use the 'location_data' object name or else the source counts code will not work.**  

`location_data <- ...`



```r
#(1) Most common Location Data sourced in:
location_data <- get_location_metadata(location_set_id=35, gbd_round_id=7) #Make sure you use the location_data object name

#(2) Custom Location Data sourced in:
location_data_1 <- get_location_metadata(location_set_id=35, gbd_round_id=7)
location_data_2 <- get_location_metadata(location_set_id=21, gbd_round_id=7) %>% filter(location_id %in% c(44794, 44793, 44795, 44797, 44798, 44799, 44800))
location_data <- rbind(location_data_1, location_data_2) #Make sure you use the location_data object name
```

#### <span style="color:red">**GHDx Helper Functions**</span> 
• In the Source Counts code, the ghdx_search_by_nid and ghdx_merged_citation_component functions are used to adequately Identify the Component NIDs from Merged NIDs.  
• ***PLEASE READ IN THE GHDx FUNCTIONS BEFORE USING THE SOURCE COUNTS CODE.***

```r
ghdx_search_by_nid <- function(nid = NULL, full = FALSE){ #Replicate the same function here -- > http://internal-ghdx.healthdata.org/search-by-nids-all-types/
  # Sourcing GHDx databases
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  # Mimicking the GHDx csv output column names
  query_data <- dbGetQuery(mydb, "SELECT nid, title, type FROM node;")
  query_data <- query_data %>% dplyr::rename('NID' = 'nid', 'Title' = 'title', 'Content Type' = 'type') 
  query_data <- query_data %>% transform(NID = as.numeric(NID))
  #NID subset or full list
  if(full == FALSE){
    nid_t <- data.table(nid)
    subset <- subset(query_data, NID %in% nid_t$nid)
    return(subset) 
  } else if (full == TRUE){
    return(query_data)
  }
  #Disconnecting from Database
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
}

ghdx_merged_citation_component <- function(nid, full = FALSE){ #Replicate the same function here -- > http://internal-ghdx.healthdata.org/merged-citations-components
  # Sourcing GHDx databases
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  # Mimicking the GHDx csv output column names
  query_data_merged <- dbGetQuery(mydb, "SELECT field_merged_records_target_id, entity_id FROM field_data_field_merged_records;")
  query_data <- dbGetQuery(mydb, "SELECT nid, title, type FROM node;")
  # Merging to get Title
  query_data_merged <- query_data_merged %>% dplyr::rename('nid' = 'field_merged_records_target_id')
  query_data_merged <- merge(query_data_merged, query_data, by = 'nid')
  query_data_merged <- query_data_merged %>% dplyr::select(nid, title, entity_id)
  #NID subset or full list
  if(full == FALSE){
    nid_t <- data.table(nid)
    subset <- subset(query_data_merged, entity_id %in% nid_t$nid)
    subset <- subset %>% dplyr::rename('Component NID' = 'nid', 'Merged Citation NID' = 'entity_id')
    return(subset)
  } else if(full == TRUE) {
    query_data_merged <- query_data_merged %>% dplyr::rename('Component NID' = 'nid', 'Merged Citation NID' = 'entity_id')
    return(query_data_merged)
  }
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
}

# This functions pulls in Data Type Values for NID inputs, something similar to one of the internal tools in the GHDx.
# The following function output Data Type for each NID, including Merged NIDs.
ghdx_search_nid_data_type <- function(nid = NULL, full = FALSE){
  # Sourcing GHDx databases
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  # Mimicking the GHDx csv output column names
  query_data <- dbGetQuery(mydb, "
  SELECT c.type, c.nid AS NID, c.title AS Title, ftype.name AS Data_Type     
  FROM ghdx.node AS c 
  LEFT JOIN ghdx.field_data_field_type ft ON ft.entity_id = c.nid 
  LEFT JOIN ghdx.taxonomy_term_data AS ftype ON ftype.tid = ft.field_type_tid
  WHERE c.type = 'record' OR c.type = 'merged_citation'
  ORDER BY c.nid;")
  #query_data <- query_data %>% dplyr::rename('Data Type(Detailed)' = 'Data_Type') 
  query_data <- query_data %>% transform(NID = as.numeric(NID))
  #NID subset or full list
  if(full == FALSE){
    nid_t <- data.table(nid)
    subset <- subset(query_data, NID %in% nid_t$nid)
    return(subset) 
  } else if (full == TRUE){
    return(query_data)
  }
  #Disconnecting from Database
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
}

# Pulling GHDx year data for all NIDs(Merged, Component, etc.)
# The following function was used to accurately depict the year spread in the Maternal Data Landscaping Plots.
ghdx_search_nid_time_period <- function(nid = NULL, full = FALSE){
  # Sourcing GHDx databases
  mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
  # Mimicking the GHDx csv output column names
  query_data <- dbGetQuery(mydb, "SELECT entity_id, field_time_value, field_time_value2 FROM field_data_field_time;")
  query_data <- query_data %>% dplyr::rename('NID' = 'entity_id', 'time_start' = 'field_time_value', 'time_end' = 'field_time_value2') 
  query_data <- query_data %>% transform(NID = as.numeric(NID))
  #NID subset or full list
  if(full == FALSE){
    nid_t <- data.table(nid)
    subset <- subset(query_data, NID %in% nid_t$nid)
    return(subset) 
  } else if (full == TRUE){
    return(query_data)
  }
  #Disconnecting from Database
  disconnection <- lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
}
```


#### <span style="color:red">**Source Counts Code**</span>
**General Requirements**  
• Please make sure that you either input a cw_id or a custom_data_table.  
• REQUIRED COLUMNS for custom_data_table: nid, location_id, year_start, and year_end.  
• Please avoid having duplicate column names or variant column names. This can corrupt the final source counts repots.  
• ***PLEASE MAKE SURE YOU READ IN THE MASTER AND HELPER FUNCTIONS***

```r
###################################
###################################
####### - Master Function - ####### 
###################################
###################################
#' @param cw_id REQUIRED(Either a cw_id, bv_id, or custom_data_table is required) Please enter your numeric Crosswalk Version ID here. Please only enter one at a time.
#' @param bv_id REQUIRED(Either a cw_id, bv_id, or custom_data_table is required) Please enter your numeric Bundle Version ID here. Please only enter one at a time.
#' @param custom_data_table REQUIRED(Either custom_data_table or cw_id is required) Source in your desired dataset object here, with the required columns:nid, location_id, year_start, and year_end. Your table can have as many columns as you like, but make sure the required columns are included. Or else the code will break. Also, please be careful to only include unique column names and no duplicates.
#' @param source_type OPTIONAL. Acceptable character entries as of Nov, 2022: "inpatient", "claims", "prevalence", and "incidence". For Maternal Landscaping usage. For other uses please use the custom_data_table option instead. 
#' @param clinical_data_only OPTIONAL. Please enter a logical(TRUE/FALSE) answer here. Default input: FALSE. 
#' @param nonclinical_data_only OPTIONAL. Please enter a logical(TRUE/FALSE) answer here. Default input: FALSE. 
#' @param custom_subset_column_names OPTIONAL. Please enter the column name(s) you would like to use to create the subset custom source counts. You can enter a single or multiple columns, but please make sure they appropriately align with the values from custom_subset_column_values.
#' @param custom_subset_column_values OPTIONAL. Please enter the value(s) you would like to use to create the subset custom source counts. You can enter a single or multiple values, but please make sure they appropriately align with their assign column from custom_subset_column_names.
#' @param out_path REQUIRED. Please enter an existing file dir here where you want your ouputs. Please keep in mind that the file dir must exist, or else you won't get your source counts report outputs.
source_counts <- function(
  cw_id = NULL, 
  bv_id = NULL,
  custom_data_table = NULL,
  source_type= NULL,
  clinical_data_only = FALSE,
  nonclinical_data_only = FALSE,
  custom_subset_column_names = NULL,
  custom_subset_column_values = NULL,
  strategy_detail = FALSE,
  out_path = NULL
  ){
  # Creating Directories Automatically, in the out_path.
  # This is to avoid file_path/file errors.
  # For better organization and clarity for users.
  if(file.exists(paste0(out_path, 'source_counts'))){
    counts_path <- paste0(out_path, 'source_counts')
  } else {
    dir.create(paste0(out_path, 'source_counts'))
    counts_path <- paste0(out_path, 'source_counts')
  }
  if(file.exists(paste0(counts_path, '/vetting'))){
    vetting_path <- paste0(counts_path, '/vetting')
  } else {
    dir.create(paste0(counts_path, '/vetting'))
    vetting_path <- paste0(counts_path, '/vetting')
  }
  if(file.exists(paste0(counts_path, '/final_counts'))){
    final_counts <- paste0(counts_path, '/final_counts')
  } else {
    dir.create(paste0(counts_path, '/final_counts'))
    final_counts <- paste0(counts_path, '/final_counts')
  }
  if(file.exists(paste0(counts_path, '/unmerged_data_bins'))){
    unmerged_path <- paste0(counts_path, '/unmerged_data_bins')
  } else {
    dir.create(paste0(counts_path, '/unmerged_data_bins'))
    unmerged_path <- paste0(counts_path, '/unmerged_data_bins')
  }
  # Sourcing in your input data: crosswalk version id, bundle version id, or custom data table
  if(is.null(cw_id) == FALSE){
    input_data <- get_crosswalk_version(cw_id)
    input_data[input_data == ""] <- NA #Converting all empty cells to NA. 
    id_input <- cw_id
  } else if(is.null(custom_data_table) == FALSE){
    input_data <- custom_data_table
    id_input <- NULL
  } else if(is.null(bv_id) == FALSE){
    input_data <- get_bundle_version(bv_id)
    input_data[input_data == ""] <- NA #Converting all empty cells to NA.
    id_input <- bv_id
  } else {
    stop("Please input a get_crosswalk_version or get_bundle_version id OR a custom df/dt.")
  }
  
  if (!"ihme_loc_id" %in% colnames(input_data)) {
    input_data <- input_data %>% dplyr::mutate(ihme_loc_id = NA)
  }
  if("location_name" %in% colnames(input_data)) {
    input_data <- input_data %>% dplyr::select(-location_name) 
    #This was to avoid overlapping errors for data inputs a crosswalk_version_id and a custom table #Additionally, this is more accurate, as location_name will be used from location_data, instead what the data bins, as location_name can sometimes be succestible to errors.
  }
  #Ran into issues when data inputs had these columns. Ex) crosswalk_version_id: 31994
  if("region_id" %in% colnames(input_data)) {
    input_data <- input_data %>% dplyr::select(-region_id)
  }
  if("region_name" %in% colnames(input_data)) {
    input_data <- input_data %>% dplyr::select(-region_name)
  }
  
  # The following is to check that custom data inpit doesn't have variant of location_id, location_name, year_start, or year_end.
  # From testing different dt's, the variant names tested below generated a corrupted file, when the columns were included.
  if(is.null(custom_data_table) == FALSE){
    if(!"nid" %in% colnames(input_data) | !"location_id" %in% colnames(input_data) | !"year_start" %in% colnames(input_data) | !"year_end" %in% colnames(input_data)){
      stop("Please make sure your custom data input contains the following required columns: nid, location_id, year_start, and year_end")
    }
  } 
  
  if(is.null(custom_data_table) == FALSE){
    if(any(grepl('location_name.', colnames(input_data))) == TRUE | any(grepl('location_id.', colnames(input_data))) == TRUE | any(grepl('year_start.', colnames(input_data))) == TRUE | any(grepl('year_end.', colnames(input_data))) == TRUE | any(grepl('nid.', colnames(input_data))) == TRUE){
      stop('Please make sure your custom data input doesnt contain column variants to these columns: nid, location_id, location_name, year_start, and year_end.')
    }
  } 
  
  input_data <- as.data.table(input_data)
  
  # Unmerged Function - Counts
  unmerged_nid_count_unadjusted(id_input = id_input, input_data = input_data, source_type = source_type, custom_subset_column_names = custom_subset_column_names, custom_subset_column_values = custom_subset_column_values, clinical_data_only = clinical_data_only, nonclinical_data_only = nonclinical_data_only, unmerged_path = unmerged_path)
  # Merged Function - Counts
  merged_nid_count_adjustment(id_input = id_input, input_data = input_data, source_type = source_type, custom_subset_column_names = custom_subset_column_names, custom_subset_column_values = custom_subset_column_values, clinical_data_only = clinical_data_only, nonclinical_data_only = nonclinical_data_only, unmerged_path = unmerged_path, strategy_detail = strategy_detail, final_counts = final_counts)
}
###################################
###################################
###### - Helper Function 1 - ######
###################################
###################################
unmerged_nid_count_unadjusted <- function(input_data, id_input = NULL, source_type = NULL, custom_subset_column_names = NULL, custom_subset_column_values = NULL, clinical_data_only = FALSE, nonclinical_data_only = FALSE, strategy_detail = FALSE, unmerged_path){
  if(is.null(source_type) == FALSE) {
    if (grepl("claims", source_type, fixed = TRUE) == TRUE) {
      source_type_t <- data.table(source_type)
      locs <- input_data %>% left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        filter(clinical_data_type %in% source_type_t$source_type) %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
      } else if (grepl("inpatient", source_type, fixed = TRUE) == TRUE) {
        source_type_t <- data.table(source_type)
        locs <- input_data %>% 
        left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        filter(clinical_data_type %in% source_type_t$source_type) %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
        } else if (source_type == "incidence") {
          source_type_t <- data.table(source_type)
          locs <- input_data %>% 
          filter(is.na(clinical_data_type) & measure == "incidence") %>% 
          left_join(location_data %>% select(location_id, location_name, path_to_top_parent, level, region_id, region_name), by = "location_id") %>%
          mutate(country_id = case_when(level == 3 ~ as.character(location_id), level > 3 ~ str_match(path_to_top_parent, 
                                                                                                  "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2], TRUE ~ "OTHER")) %>%
            filter(!country_id=="OTHER") %>%
            mutate(country_id = as.integer(country_id)) %>%
            left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
            dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
            # One PUD EMR source (NID 416752) is labeled with location_id == 1 and ihme_loc_id==IDN. Excluded this record from counts (this is the
            # only "IHME" source in this bundle).
            # Review the above filter(!country_id == "OTHER") for further details on why these were filtered out, connect to the above comment on 
            # NID: 416752
            # This particular NID is a dummy NID or an NID for other purposes!
            # It will be important to properly ver this with other crosswalk version ids.
            } else if (source_type == "prevalence") {
              source_type_t <- data.table(source_type)
              locs <- input_data %>% 
              filter(is.na(clinical_data_type) & measure == "prevalence") %>% 
              left_join(location_data %>% select(location_id, location_name, path_to_top_parent, level, region_id, region_name), by = "location_id") %>%
              mutate(country_id = case_when(level == 3 ~ as.character(location_id), level > 3 ~ str_match(path_to_top_parent, 
                                                                                                  "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2], TRUE ~ "OTHER")) %>%
              filter(!country_id=="OTHER") %>%
              mutate(country_id = as.integer(country_id)) %>%
              left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
              dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
              }
    } else if(is.null(custom_subset_column_names) == FALSE && is.null(custom_subset_column_values) == FALSE){
      subset_table <- data.table(custom_subset_column_names, custom_subset_column_values)
      subset_table <- dcast(subset_table, custom_subset_column_values~custom_subset_column_names)
      subset_table <- select(subset_table, -custom_subset_column_values)
      subset_table <- subset_table %>% summarise_all(list(~ sum(., na.rm = T)))
      subset_table <- as.data.table(subset_table)
      # # change_columns <- custom_subset_column_names
      # # subset_table[,(change_columns) := lapply(.SD, as.character),.SDcols = change_columns] # Changing Column Class Type#For Obstruction Draft Table
      # subset_table_test <- sapply(subset_table, class)
      # if(subset_table_test == 'numeric'){
      #   subset_table <- subset_table %>% mutate(across(where(is.numeric), as.character)) #An attempt to resolve the class types issues.
      # } #An empty data table, as with some data inputs will produce errors, a NULL or empty table can't join properly with a filed table.
      
      locs <- input_data %>% 
        left_join(location_data %>% 
                    select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        inner_join(subset_table, by = custom_subset_column_names) %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>% 
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>% 
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if(is.null(custom_subset_column_names) == TRUE && is.null(custom_subset_column_values) == TRUE && clinical_data_only == FALSE && nonclinical_data_only == FALSE && is.null(source_type) == TRUE) {
      #subset_table <- data.table(custom_subset_column_names, custom_subset_column_values)
      #subset_table <- dcast(subset_table, custom_subset_column_values~custom_subset_column_names)
      #subset_table <- select(subset_table, -custom_subset_column_values)
      #subset_table <- subset_table %>% summarise_all(list(~ sum(., na.rm = T)))
      locs <- input_data %>% left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>% 
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>% 
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if(clinical_data_only == TRUE) {
      locs <- input_data %>%
        left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>%
        filter(!clinical_data_type %in% NA) %>% #Better to use for General Use!
        #filter(!clinical_data_type %in% NA & !clinical_data_type %in% "literature") %>% #For maternal clinical data specifically!
        mutate(country = substr(ihme_loc_id.y, 1, 3),
                ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if(nonclinical_data_only == TRUE) {
      locs <- input_data %>%
        left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        filter(clinical_data_type %in% NA) %>% #Better to use for General Use!
        #filter(clinical_data_type %in% NA | clinical_data_type %in% "literature" & !extractor %in% NA) %>% #For maternal clinical data specifically!
        mutate(country = substr(ihme_loc_id.y, 1, 3),
                ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
      } else {
        message("Invalid source_type argument.")
        message("OR the subset inputted for processing is not valid. Please review the arguments for this function!")
      } 
  
  if (nrow(locs) == 0) {
    if(is.null(id_input) == FALSE){
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_id_", id, "_counts_unadj.xlsx")
      empty_table <- data.table(region_id = NA, location_name = NA, n = 0, hierarchy_key = NA)
      write.xlsx(empty_table, paste0(unmerged_path, "/", out_filename))
      message(cat(paste0("id_input ", id_input, ", ", source_type), ": no sources of this data type. An appropriate unadjusted table with zero counts has been generated."))
    } else {
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_","counts_unadj.xlsx")
      empty_table <- data.table(region_id = NA, location_name = NA, n = 0, hierarchy_key = NA)
      write.xlsx(empty_table, paste0(unmerged_path, "/", out_filename))
      message(cat(paste0("Custom Table, ", source_type), ": no sources of this data type. An appropriate unadjusted table with zero counts has been generated."))
    }
  } else {
    # Unique NID-location-year_start combinations [most granular locs]
    # Unique counts for all locations #
    count_loc <- locs %>% dplyr::distinct(nid, year_start, year_end, location_name) %>%
      group_by(location_name) %>% 
      dplyr::tally() %>%
      left_join(location_data %>% select(location_id, location_name), by = "location_name")
    # The below notes and instructions were automated. This was done manually before.
    # Unique NID-loc-year_start combos [most granular locs] - collapsed NID list
    # Only to feed into custom GHDx tool to identify merged NIDs for manual
    # adjustments: http://internal-ghdx.healthdata.org/search-by-nids-all-types
    
      count_loc_merged <- locs %>% 
        dplyr::distinct(nid, year_start, year_end, location_name) %>%
        dplyr::pull(nid) %>%
        unique()
      count_loc_merged_message <- count_loc_merged %>% paste0(collapse=",")
    # Message, providing the NID list for processing OR Searching for Merged NIDs and their NID components
    #message(cat(paste0("cw_id ", cw_id, ", ", source_type, " NID list for identifying merged NIDs: ")), count_loc_merged_message)
    if(is.null(id_input) == FALSE){
      message(cat(paste0("id_input ", id_input, ", ", source_type, " NID list for identifying merged NIDs: ")), count_loc_merged_message)
      } else {
        message(cat(paste0("Custom Table, ", source_type, " NID list for identifying merged NIDs: ")), count_loc_merged_message)
        }
    
    # Sourcing GHDx database and creating appropriate files with Merged NIDs and their NID components
    dt_merged_types <- ghdx_search_by_nid(nid = count_loc_merged)
    dt_component_records <- ghdx_merged_citation_component(full = TRUE)
    
    # Creates the Merge NIDs and component NIDs for Processing. This mimicks what the original manual processes.
    if(is.null(id_input) == FALSE){
      #print(source_type)
      file_extension_merged_types <- paste0(unmerged_path,"/",id_input,"_",ifelse(is.null(source_type) == FALSE, source_type,""),"merge_types.xlsx")
      } else {
        file_extension_merged_types <- paste0(unmerged_path,"/","Custom_Table",ifelse(is.null(source_type) == FALSE, source_type,""),"_merge_types.xlsx")
        }
    file_extension_merged_component_records <- paste0(unmerged_path,"/", "merged_citation_component_records.xlsx")
    
    write.xlsx(dt_merged_types, file_extension_merged_types)
    write.xlsx(dt_component_records, file_extension_merged_component_records)
    
    # Unique NID-country-year_start combinations [country-level location counts] # 
    if(strategy_detail == TRUE){
      count_country <- locs %>% distinct(nid, year_start, year_end, ihme_loc_id, country_name, region_id)
    } else if(strategy_detail == FALSE) {
      count_country <- locs %>% distinct(nid, year_start, year_end, country_name, region_id)      
    }
    count_country <- count_country %>% 
        group_by(region_id, country_name) %>% 
        tally() %>%
        left_join(location_data %>% filter(location_type == 'admin0' | level == 3) %>% 
                    select(location_name, location_id), by = c('country_name'='location_name'))
      
    # Unique NID-GBD-Region_year_start combinations # 
    if(strategy_detail == TRUE){
      gbd_region <- locs %>% distinct(nid, year_start, year_end, ihme_loc_id, region_id, region_name)
    } else if(strategy_detail == FALSE) {
      gbd_region <- locs %>% distinct(nid, year_start, year_end, region_id, region_name)      
    }
      gbd_region <- gbd_region %>% 
        group_by(region_id, region_name) %>% 
        tally() 
    
    # Unique GBD region and country counts #
    dt_g <- gbd_region %>% dplyr::rename('location_name' = 'region_name') %>% mutate(hierarchy_key = 'gbd_region')
    dt_c <- count_country %>% dplyr::rename('location_name' = 'country_name') %>% select(-location_id) %>% mutate(hierarchy_key = 'gbd_country')
    country_gbd_regions <- rbind(dt_g, dt_c) %>% setorder('region_id', -'hierarchy_key', 'location_name')
    
    # Create new xlsx workbook and add source counts (separate sheets for country and most granular counts)
    if(is.null(id_input) == FALSE){
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_id_", id_input, "_counts_unadj.xlsx")
    } else {
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_","counts_unadj.xlsx")
    }
    wb <- createWorkbook(title = out_filename)
    
    addWorksheet(wb, "Counts_All Locations_Unadj")
    writeData(wb, "Counts_All Locations_Unadj", count_loc)
    
    addWorksheet(wb, "counts_by_country")
    writeData(wb, "counts_by_country", count_country)
    
    addWorksheet(wb, "counts_by_gbd_region")
    writeData(wb, "counts_by_gbd_region", gbd_region)
    
    addWorksheet(wb, "counts_by_gbd_region_country")
    writeData(wb, "counts_by_gbd_region_country", country_gbd_regions)
    
    saveWorkbook(wb, paste0(unmerged_path, "/", out_filename), overwrite = TRUE)
  }
}
###################################
###################################
###### - Helper Function 2 - ###### 
###################################
###################################
merged_nid_count_adjustment <- function(input_data, id_input = NULL, source_type = NULL, final_counts, custom_subset_column_names = NULL, custom_subset_column_values = NULL, clinical_data_only = FALSE, nonclinical_data_only = FALSE, strategy_detail = FALSE, unmerged_path){
  # Read in source counts from source_counts() function (unadjusted for merged NIDs)
  if(!file.exists(paste0(unmerged_path,"/", ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_id_", id_input, "_counts_unadj.xlsx")) && !file.exists(paste0(unmerged_path,"/", ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_", "counts_unadj.xlsx")) || !file.exists(paste0(unmerged_path,"/", "merged_citation_component_records.xlsx"))){
    if(is.null(id_input) == FALSE){
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_counts_id_", id_input, "_counts_adj.xlsx")
      empty_table <- data.table(region_id = NA, location_name = NA, n_adj = 0, hierarchy_key = NA)
      #write.xlsx(empty_table, paste0(final_counts, "/", out_filename))
      message(cat(paste0("No ", source_type, " NIDs for id_input ", id_input,".An appropriate adjusted table with zero counts has been generated.")))
    } else {
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_","counts_adj.xlsx")
      empty_table <- data.table(region_id = NA, location_name = NA, n_adj = 0, hierarchy_key = NA)
      #write.xlsx(empty_table, paste0(final_counts, "/", out_filename))
      message(cat(paste0("No NIDs for Custom table. An appropriate adjusted table with zero counts has been generated.")))
      }
    } else {
      # The following lines of code are not necessary, as the tables don't processed anywhere else this function. The data wrangling and
      # unique counts that takes place in the unmerged_nid_count_unadjusted functions gets repeated in this function, in addition to the
      # merged nid adjustments.
      # The following lines of code were included, as they were included in the original code!
      if(is.null(id_input) == FALSE) {
        #print(source_type)
        source_type_nids <- read.xlsx(paste0(unmerged_path, "/",id_input, "_",ifelse(is.null(source_type) == FALSE, source_type,""), "merge_types.xlsx"))
        unadj_counts_subnat <- read.xlsx(paste0(unmerged_path, "/",ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_id_", id_input, "_counts_unadj.xlsx"), sheet = 1) 
        unadj_counts_country <- read.xlsx(paste0(unmerged_path, "/",ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_id_", id_input, "_counts_unadj.xlsx"), sheet = 2) 
        # Read in list of all NIDs for each source_type (prev, inpatient, claims) with identifiers for merged NIDs and normal record NIDs. 
        # subset_cw_version_19694_counts_unadj
        # Read in list of all merged NIDs and their component NIDs (downloaded from GHDx)
        merged_comp_list <- read.xlsx(paste0(unmerged_path, "/","merged_citation_component_records.xlsx")) %>%
          group_by(Merged.Citation.NID) %>%
          tally() %>%
          dplyr::rename(n_comp_nids = n) # number of component NIDs associated with each merged NID
      } else {
        unadj_counts_subnat <- read.xlsx(paste0(unmerged_path, "/",ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_", "counts_unadj.xlsx"), sheet = 1) 
        unadj_counts_country <- read.xlsx(paste0(unmerged_path, "/",ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_", "counts_unadj.xlsx"), sheet = 2) 
        unadj_counts_region <- read.xlsx(paste0(unmerged_path, "/",ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_", "counts_unadj.xlsx"), sheet = 3) 
        unadj_counts_country_region <- read.xlsx(paste0(unmerged_path, "/",ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_", "counts_unadj.xlsx"), sheet = 4) 
        # Read in list of all NIDs for each source_type (prev, inpatient, claims) with identifiers for merged NIDs and normal record NIDs. 
        source_type_nids <- read.xlsx(paste0(unmerged_path, "/","Custom_Table",ifelse(is.null(source_type) == FALSE, source_type,""),"_merge_types.xlsx"))
        
        # Read in list of all merged NIDs and their component NIDs (downloaded from GHDx)
        merged_comp_list <- read.xlsx(paste0(unmerged_path, "/","merged_citation_component_records.xlsx")) %>%
          group_by(Merged.Citation.NID) %>%
          tally() %>%
          dplyr::rename(n_comp_nids = n) # number of component NIDs associated with each merged NID
      }
    
    # Match all merged NIDs of the given source_type from the PUD/GD crosswalk versions with the # of component NIDs
    key <- source_type_nids %>% left_join(merged_comp_list, by = c("NID" = "Merged.Citation.NID")) %>% 
      filter(!is.na(n_comp_nids))
    # Format crosswalk version data to prep for source counting 
    if(is.null(source_type) == FALSE) {
      if (grepl("claims", source_type, fixed = TRUE) == TRUE) {
        source_type_t <- data.table(source_type)
        locs <- input_data %>%
        left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        filter(clinical_data_type %in% source_type_t$source_type) %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3),
               ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if (grepl("inpatient", source_type, fixed = TRUE) == TRUE) {
      source_type_t <- data.table(source_type)
      locs <- input_data %>% 
        left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        filter(clinical_data_type %in% source_type_t$source_type) %>% 
        mutate(country = substr(ihme_loc_id.y, 1, 3),
               ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if (source_type == "incidence") {
      locs <- input_data %>% 
        filter(is.na(clinical_data_type) & measure == "incidence") %>% 
        left_join(location_data %>% select(location_id, location_name, path_to_top_parent, level, region_id, region_name), by = "location_id") %>%
        mutate(country_id = case_when(level == 3 ~ as.character(location_id), 
                                      level > 3 ~ str_match(path_to_top_parent, "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2], 
                                      TRUE ~ "OTHER")) %>%
        filter(!country_id=="OTHER") %>% 
        # one PUD EMR source (NID 416752) is labeled with location_id == 1 and ihme_loc_id==IDN. Excluded this record from counts (this is the only
        # "IHME" source in this bundle).
        mutate(country_id = as.integer(country_id)) %>%
        left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if (source_type == "prevalence") {
      locs <- input_data %>% 
        filter(is.na(clinical_data_type) & measure == "prevalence") %>% 
        left_join(location_data %>% select(location_id, location_name, path_to_top_parent, level, region_id, region_name), by = "location_id") %>%
        mutate(country_id = case_when(level == 3 ~ as.character(location_id), 
                                      level > 3 ~ str_match(path_to_top_parent, "1,[0-9]*[0-9]*[0-9],[0-9]*[0-9]*[0-9],\\s*(.*?)\\s*,[0-9]*[0-9]*[0-9]")[,2], 
                                      TRUE ~ "OTHER")) %>%
        filter(!country_id=="OTHER") %>% 
        # One PUD EMR source (NID 416752) is labeled with location_id == 1 and ihme_loc_id==IDN. Excluded this record from counts (this is the only
        # "IHME" source in this bundle).
        mutate(country_id = as.integer(country_id)) %>%
        left_join(location_data %>% select(location_name, location_id), by = c("country_id" = "location_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    }
    } else if(is.null(custom_subset_column_names) == FALSE && is.null(custom_subset_column_values) == FALSE){

        subset_table <- data.table(custom_subset_column_names, custom_subset_column_values)
        subset_table <- dcast(subset_table, custom_subset_column_values~custom_subset_column_names)
        subset_table <- select(subset_table, -custom_subset_column_values)
        subset_table <- subset_table %>% summarise_all(list(~ sum(., na.rm = T)))
        subset_table <- as.data.table(subset_table) #
        # #change_columns <- custom_subset_column_names
        # #subset_table[,(change_columns) := lapply(.SD, as.character),.SDcols = change_columns] # Changing Column Class Type#For Obstruction Draft Table
        # subset_table_test <- sapply(subset_table, class)
        # if(subset_table_test == 'numeric'){
        #   subset_table <- subset_table %>% mutate(across(where(is.numeric), as.character)) #An attempt to resolve the class types issues.
        # }
        
        locs <- input_data %>% left_join(location_data %>% 
                                              select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
          inner_join(subset_table, by = custom_subset_column_names) %>% 
          mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>% 
          left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>% 
          dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if(is.null(custom_subset_column_names) == TRUE && is.null(custom_subset_column_values) == TRUE && clinical_data_only == FALSE && nonclinical_data_only == FALSE && is.null(source_type) == TRUE) {
      #subset_table <- data.table(custom_subset_column_names, custom_subset_column_values)
      #subset_table <- dcast(subset_table, custom_subset_column_values~custom_subset_column_names)
      #subset_table <- select(subset_table, -custom_subset_column_values)
      #subset_table <- subset_table %>% summarise_all(list(~ sum(., na.rm = T)))

      locs <- input_data %>% left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% mutate(country = substr(ihme_loc_id.y, 1, 3), ihme_loc_id = ihme_loc_id.y) %>% 
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>% 
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if(clinical_data_only == TRUE){
      #print('test clinical')
      locs <- input_data %>%
        left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        filter(!clinical_data_type %in% NA) %>% #Better to use for General Use!
        #filter(!clinical_data_type %in% NA & !clinical_data_type %in% "literature") %>% #For maternal clinical data specifically!
        mutate(country = substr(ihme_loc_id.y, 1, 3),
               ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
    } else if(nonclinical_data_only == TRUE) {
      #print('test nonclinical')
      locs <- input_data %>%
        left_join(location_data %>% select(location_id, location_name, ihme_loc_id, region_id, region_name), by = "location_id") %>% 
        filter(clinical_data_type %in% NA) %>% #Better to use for General Use!
        #filter(clinical_data_type %in% NA | clinical_data_type %in% "literature" & !extractor %in% NA) %>% #For maternal clinical data specifically!
        mutate(country = substr(ihme_loc_id.y, 1, 3),
               ihme_loc_id = ihme_loc_id.y) %>%
        left_join(location_data %>% select(location_name, ihme_loc_id), by = c("country" = "ihme_loc_id")) %>%
        dplyr::rename(., country_name = location_name.y, location_name = location_name.x)
      } else {
        message("Invalid source_type argument.")
        message("OR the subset inputted for processing is not valid. Please review the arguments for this function!")
      } 
    
    # Unique NID-location-year_start combinations [most granular locs] accounting for component NIDs #
    locs <- transform(locs, nid = as.numeric(nid)) # These are important to avoid incompability issues! ~ chrish47
    key <- transform(key, NID = as.numeric(NID)) # These are important to avoid incompability issues! ~ chrish47
    # 
    count_loc <- locs %>% distinct(nid, year_start, year_end, location_id, location_name) %>%
      left_join(key %>% select(NID, n_comp_nids), by = (c("nid" = "NID"))) %>% # This is the line with the incompatible types problems ~ chrish47
      mutate(n_comp_nids = ifelse(is.na(n_comp_nids), 1, n_comp_nids)) %>% 
      # NIDs without component NIDs are assigned '1' == a single NID-loc-year_start combo.
      group_by(location_name) %>%
      mutate(n_adj = sum(n_comp_nids)) %>% 
      arrange(location_name) %>%
      distinct(location_name, location_id, n_adj)
    
    # Unique NID-country-year_start combinations [country-level counts] accounting for component NIDs #
    if(strategy_detail == TRUE){
      count_country <- locs %>% distinct(nid, year_start, year_end, ihme_loc_id, country_name, region_id)
    } else if(strategy_detail == FALSE) {
      count_country <- locs %>% distinct(nid, year_start, year_end, country_name, region_id)      
    }
    count_country <- count_country %>% #For testing purposes exclude/inclide region_id!
      left_join(key %>% select(NID, n_comp_nids), by = (c("nid" = "NID"))) %>%
      mutate(n_comp_nids = ifelse(is.na(n_comp_nids), 1, n_comp_nids)) %>% 
      # NIDs without component NIDs are assigned '1' == a single NID-loc-year_start combo.
      group_by(country_name, region_id) %>%
      mutate(n_adj = sum(n_comp_nids)) %>% 
      arrange(country_name, region_id) %>%
      distinct(region_id, country_name, n_adj) %>% 
      left_join(location_data %>% filter(location_type == 'admin0' | level == 3) %>% 
                  select(location_name, location_id), by = c('country_name'='location_name'))
    #location_data was subsetted to only include admin0(country) locations. This was necessary in order to avoid duplicates.
    # Ex: Georgia(US state) vs. Georgia(Country) is an issues if doesn't get properly filtered to admin0 or countries only.
    
    # Unique NID-GBD-Region combinations [GBD_region_name] accounting for component NIDs #
    #detail <- ifelse(strategy_detail == TRUE, 'ihme_loc_id', NULL) 
    if(strategy_detail == TRUE){
      gbd_region_counts <- locs %>% distinct(nid, year_start, year_end, ihme_loc_id, region_id, region_name)
    } else if(strategy_detail == FALSE) {
      gbd_region_counts <- locs %>% distinct(nid, year_start, year_end, region_id, region_name)      
    }
    gbd_region_counts <- gbd_region_counts %>% 
      left_join(key %>% select(NID, n_comp_nids), by = (c("nid" = "NID"))) %>%
      mutate(n_comp_nids = ifelse(is.na(n_comp_nids), 1, n_comp_nids)) %>% 
      # NIDs without component NIDs are assigned '1' == a single NID-loc-year_start combo.
      group_by(region_id, region_name) %>%
      mutate(n_adj = sum(n_comp_nids)) %>% 
      arrange(region_id, region_name) %>%
      distinct(region_id, region_name, n_adj)
    
    # Lanscaping table, gbd_region and country, ascending by region_id and location_name and descending by hierarchy_key # 
    dt_g <- gbd_region_counts %>% dplyr::rename('location_name' = 'region_name') %>% mutate(hierarchy_key = 'gbd_region')
    dt_c <- count_country %>% dplyr::rename('location_name' = 'country_name') %>% select(-location_id) %>% mutate(hierarchy_key = 'gbd_country')
    country_gbd_regions <- rbind(dt_g, dt_c) %>% setorder('region_id', -'hierarchy_key', 'location_name')
    
    # Create xlsx file with sheets for most granular and country-level source counts. 
    if(is.null(id_input) == FALSE){
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_counts_id_", id_input, "_counts_adj.xlsx")
    } else {
      out_filename <- paste0(ifelse(is.null(source_type) == FALSE, source_type,"subset"), "_Custom_Table_","counts_adj.xlsx")
    }
    wb <- createWorkbook(title = out_filename)
    
    addWorksheet(wb, "Counts_All Locations_Adj")
    writeData(wb, "Counts_All Locations_Adj", count_loc)
    
    addWorksheet(wb, "Counts_Country_Adj")
    writeData(wb, "Counts_Country_Adj", count_country)
    
    addWorksheet(wb, "Counts_GBD_Region_Adj")
    writeData(wb, "Counts_GBD_Region_Adj", gbd_region_counts)
    
    addWorksheet(wb, "Counts_GBD_Region_Country_Adj")
    writeData(wb, "Counts_GBD_Region_Country_Adj", country_gbd_regions)
    
    saveWorkbook(wb, paste0(final_counts, "/", out_filename), overwrite = TRUE)
  }  
}
```
<hr style="border:2px solid gray">
#### <span style="color:red">**Example Calls**</span> 
**With Crosswalk Version ID**  
• Source Counts for the whole crosswalk version ID you enter.  
`source_counts(cw_id = 19694, out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/cw_id_19694/")`  
• Source Counts for Clinical Data only, for the crosswalk version ID you enter. ~ This has not has been developed for the custom table option yet.  
`source_counts(cw_id = 19694, clinical_data_only = TRUE, out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/cw_id_19694/")`  
• Source Counts for Non-Clinical Data only, for the crosswalk version ID you enter. ~ This has not has been developed for the custom table option yet.  
`source_counts(cw_id = 19694, nonclinical_data_only = TRUE, out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/cw_id_19694/")`  
• Source Counts by Source Type or Measure Type ~ Please keep in mind that the source_type argument is column-specific and with the use cw_id(It hasn't yet been tested with custom_data_table). If you want to use this this option, please make sure your crosswalk version ID and/or data inputs have 'source_type' and 'measure_type'.  
• For the measure_type column, only 'incidence' and 'prevalence' has been tested.  
• For source_type column, only 'inpatient' and 'claims' have been tested.   
• If you want Source Counts for specific subsets of your main data input. I would recommend to do the prep work before, that includes the desired data, and use the custom_data_table option instead.
`source_counts(cw_id = 12221, source_type = 'inpatient', out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/cw_id_19694/")`
`source_counts(cw_id = 12221, source_type = 'claims', out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/cw_id_19694/")`
`source_counts(cw_id = 12221, source_type = 'prevalence', out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/cw_id_19694/")`
`source_counts(cw_id = 9890, source_type = 'inpatient', out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/cw_id_19694/")`
`source_counts(cw_id = 9890, source_type = 'claims', out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/cw_id_19694/")`
`source_counts(cw_id = 9890, source_type = 'prevalence', out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/cw_id_19694/")`

**With Custom Data Input**  
• Prepping custom dataset:  
`dt <- read.xlsx("/ihme/scratch/projects/rgud/Source Counts Tool 2022/Testing/testing_dt.xlsx") %>% select(-'location_name.x', -'location_name.y')`  
• Total/All source counts for custom data input.  
`source_counts(custom_data_table = dt, out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/custom_table/")`  
• Subset source counts for 'All_data'(column) for custom data input. All rows(data) with a value of 1 under 'All data' will be processed.  
`source_counts(custom_data_table = dt, custom_subset_column_names = 'All_data', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/custom_table/")`  
• Subset source counts for 'Vital_Reg'(column) for custom data input. All rows(data) with a value of 1 under 'Vital_Reg' will be processed.  
`source_counts(custom_data_table = dt, custom_subset_column_names = 'Vital_Reg', custom_subset_column_values = 1, out_path = "/ihme/scratch/users/chrish47/Source_Counts_Testing_Nov2022/custom_table/")`

<hr style="border:2px solid gray">
#### <span style="color:red">**Source Count File outputs in your out_path**</span> 

**A source_counts folder automatically gets created if it doesn't exist already in your out_path**  
![Source Counts Folder](/ihme/homes/chrish47/source_counts_utility/Source Counts Code/source_counts.jpg)

**Inside the source_counts folder, you will have three different subfolders**  
• *final_counts*  
    - This folder contains your final source counts report. These source counts take into account merge nids.   
• *unmerged_data_bins*  
    - This folder contains your unmerged source counts report and other files containing GHDx sourced in data that are used to create your final source counts report.  
• *vetting*  
    - Empty folder ~ Included for conducting any vetting work by the user.    
![](/ihme/homes/chrish47/source_counts_utility/Source Counts Code/source_counts_folders.jpg)

**Both the Final and Unmerged source counts files contain the same tabs and columns.**  
![](/ihme/homes/chrish47/source_counts_utility/Source Counts Code/final_source_counts_tab.jpg)
![](/ihme/homes/chrish47/source_counts_utility/Source Counts Code/final_source_counts_columns.jpg)

**Notes**  
• **Counts_All_Locations_Ajd**, contains source counts at the most detailed location_id level. This includes all source counts by location_id/location_name.  
    - Using the below tables: Each table at the location_id/location_name has 5 total source counts.  
• **Counts_Country_Adj**, contains source counts at the country level. All non-country locations are assigned to their corresponding Countries for source counts.  
    - Using the below tables: Table 1, at the country level, would have 2 total source counts. Table 2, at the country level, would have 5 total source counts. Pay close attention to the year spread.  
• **Counts_GBD_Region**, contains source counts at the GBD regional level. Counntry/subnational  
    - Using the below tables: Table 1, at the regional level, would have 1 total source count. Table 2, at the regional level, would have 5 total source counts. Pay close attention to the year spread.  
• **Counts_GBD_Region_Country_Adj**, combines the source counts from the Counts_Country_Adj and Counts_GBD_Region tabs.  

| location_id|location_name |country                 |region_name               | year_start| year_end|
|-----------:|:-------------|:-----------------------|:-------------------------|----------:|--------:|
|         523|Alaska        |United State of America |High-income North America |       2010|     2011|
|         524|Alabama       |United State of America |High-income North America |       2010|     2011|
|         560|Oregon        |United State of America |High-income North America |       2010|     2011|
|         570|Washington    |United State of America |High-income North America |       2010|     2011|
|         101|Canada        |Canada                  |High-income North America |       2010|     2011|



| location_id|location_name |country                 |region_name               | year_start| year_end|
|-----------:|:-------------|:-----------------------|:-------------------------|----------:|--------:|
|         523|Alaska        |United State of America |High-income North America |       2010|     2011|
|         524|Alabama       |United State of America |High-income North America |       2015|     2016|
|         560|Oregon        |United State of America |High-income North America |       2021|     2021|
|         570|Washington    |United State of America |High-income North America |       2020|     2020|
|         101|Canada        |Canada                  |High-income North America |       2023|     2023|











