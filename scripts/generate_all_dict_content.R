
t <- proc.time()

# set github commit message here
github_commit_message <- "New COVID surveys and other updates"


library(opalr)
library(dplyr)
library(openxlsx)
library(knitr)
library(stringr)
library(readr)
library(haven)
library(labelled)
library(here)
library(writexl)
library(readxl)
library(git2r)


### SETUP

source("H:/MyDocuments/R/opal_opts.R")

dict_csv_path <- file.path("U:", "Born In Bradford - Confidential - Opal", "data_dict", "opal_blogdown_dict", "docs", "csv")

dict_content_path <- file.path("U:", "Born In Bradford - Confidential - Opal", "data_dict", "opal_blogdown_dict", "content")

dict_output_path <- file.path("U:", "Born In Bradford - Confidential - Opal", "data_dict", "opal_blogdown_dict", "docs")

github_csv_path <- "https://github.com/BornInBradford/datadict/blob/main/docs/csv/"
github_csv_raw_path <- "https://raw.githubusercontent.com/BornInBradford/datadict/main/docs/csv/"

base_url <- "https://borninbradford.github.io/datadict/"

source("U:\\Born In Bradford - Confidential - Opal\\opal_load\\utils\\bibopal_user_defined_functions.R")


o <- opal.login()

# get the table and project groupings info we need from Opal
dd_groups <- get_opal_data(o, "DataDictionary", "dd_groups")
dd_projects <- get_opal_data(o, "DataDictionary", "dd_projects")
dd_tables <- get_opal_data(o, "DataDictionary", "dd_tables")

# get project info table
project_info <- get_opal_project_table(o, dd_projects$id)

# join project title and description
project_groups <- dd_projects %>% inner_join(project_info, by = c("id" = "Project name"))
project_groups <- project_groups %>% rename(project = id)
groups <- dd_groups %>% select(project_group = id, project_group_index, project_group_desc) %>% arrange(project_group_index)



### MAIN DICTIONARY WEB CONTENT AND TABLE CSVS

message("Generating data dictionary web content and table csvs")

# clear html output folder
if(dir.exists(dict_output_path)) unlink(dict_output_path, recursive = TRUE)
dir.create(dict_output_path, showWarnings = FALSE)
dir.create(dict_csv_path, showWarnings = FALSE)
# add .nojekyll file so github doesn't try to rebuild the site on upload
file.create(file.path(dict_output_path, ".nojekyll"), showWarnings = FALSE)

# do group level output
for(g in 1:nrow(groups)) {
  
  message(paste0("  Generating web content for group: `", groups$project_group[g], "'"))
  
  # (delete and) recreate group level content directory
  group_path <- file.path(dict_content_path, groups$project_group[g])
  if(dir.exists(group_path)) unlink(group_path, recursive = TRUE)
  dir.create(group_path, showWarnings = FALSE)
  
  # create group project table with urls
  group_projects <- project_groups %>% filter(project_group == groups$project_group[g]) %>%
    arrange(project_index) %>%
    mutate(url = paste0(base_url, tolower(project_group), "/", tolower(project), "/")) %>%
    mutate(project = paste0("[", project, "]", "(", url, ")")) %>%
    select(`Project name` = project, `Project title`, Description)
  
  # create project level _index.md
  content <- c("---", 
               paste0("title: ", groups$project_group[g]), 
               paste0("weight: ", groups$project_group_index[g]),
               "---", 
               "", 
               paste0("Container data dictionary: ",
                      "[github](", github_csv_path, groups$project_group[g], "__full_data_dictionary.csv) | ",
                      "[csv](", base_url, "csv/", groups$project_group[g], "__full_data_dictionary.csv) | ",
                      "[xlsx](", base_url, "csv/", groups$project_group[g], "__full_data_dictionary.xlsx)"
               ),
               "",
               groups$project_group_desc[g],
               "", 
               kable(group_projects, align = "l")
  )
  
  writeLines(content, file.path(group_path, "_index.md"))
  
}

# do project level output
for(p in 1:nrow(project_groups)) {
    
  message(paste0("      Generating web content for project: `", project_groups$project[p], "'"))
  
  dd_tables_project <- dd_tables %>% filter(project_name == project_groups$project[p])
  
  table_info <- get_opal_table_info(o, project_name = project_groups$project[p], table_dd = dd_tables_project)
  
  project_url <- paste0(base_url, tolower(project_groups$project_group[p]), "/", tolower(project_groups$project[p]), "/")
  
  # add table urls
  table_info <- table_info %>% mutate(url = paste0(project_url, 
                                                   ifelse(!is.na(parent), paste0(tolower(parent), "/"), ""),
                                                   tolower(Table)))
    
  table_info <- table_info %>% mutate(table_name = Table, Table = paste0("[", Table, "]", "(", url, ")"))
                                      
  # take just the "core" tables, i.e. no parent, and order
  core_table_info <- table_info %>% filter(is.na(parent)) %>% arrange(index) %>% select(-parent, -index, -url, -Description)
  
  # add the related tables to each core table
  related_tables <- table_info %>% filter(!is.na(parent)) %>% group_by(parent) %>% 
    summarise(`Related tables` = paste0(Table, collapse = "<br>")) %>% ungroup()
  related_tables$parent = as.character(related_tables$parent)
  core_table_info <- core_table_info %>% left_join(related_tables, by = c("table_name" = "parent")) %>% select(-table_name)
  
  # create project level content directory
  project_path <- file.path(dict_content_path, project_groups$project_group[p], project_groups$project[p])
  dir.create(project_path, showWarnings = FALSE)
  
  # create project level _index.md
  content <- c("---", 
               paste0("title: ", project_groups$`Project title`[p]), 
               paste0("weight: ", project_groups$project_index[p]),
               "---", 
               "", 
               project_groups$Description[p], 
               "", 
               kable(core_table_info, align = "l")
  )
  
  writeLines(content, file.path(project_path, "_index.md"))
  
  # create table level content
  for (i in 1:nrow(table_info)) {
    
    message(paste0("      Generating web content for table: `", table_info$table_name[i], "'"))
    
    table_stats <- table_info[i,] %>% select(`Entity type`, Variables, Entities, Updated)
    
    # is it a core table?
    if(is.na(table_info$parent[i])) {
      table_stats$`Related tables` <- core_table_info$`Related tables`[core_table_info$Table == table_info$Table[i]]
    }
    
    table_dd <- get_opal_summary_table(o, project_name = project_groups$project[p], table = table_info$table_name[i])
    
    content <- c("---",
                 paste0("title: ", table_info$Name[i]),
                 paste0("weight: ", table_info$index[i]),
                 "---",
                 "",
                 paste0("Table ID: ", project_groups$project[p], ".", table_info$table_name[i]),
                 "",
                 paste0("[github](", github_csv_path, project_groups$project[p], "/", project_groups$project[p], "__", table_info$table_name[i], "_data_dictionary.csv) | ",
                        "[csv](", base_url, "csv/", project_groups$project[p], "/", project_groups$project[p], "__", table_info$table_name[i], "_data_dictionary.csv)"),
                 "",
                 kable(table_stats, align = "l"),
                 "",
                 table_info$Description[i],
                 "",
                 kable(table_dd, align = "l")
    )
    
    # create table level directory and set file name
    # table directory must be a core table or the parent
    # if we are writing a core table we write an _index.md
    if(is.na(table_info$parent[i])) {
      table_path <- file.path(project_path, table_info$table_name[i])
      file_name <- "_index.md"
    } else {
      table_path <- file.path(project_path, table_info$parent[i])
      file_name <- paste0(table_info$table_name[i], ".md")
    }

    dir.create(file.path(table_path), showWarnings = FALSE)
    
    writeLines(content, file.path(table_path, file_name))
    
    # write dict table csv
    message(paste0("      Generating csv for table: `", table_info$table_name[i], "'"))
    make_opal_table_csv(o, project_groups$project[p], table_info$table_name[i]) %>% write_dict_csv(project_groups$project[p], table_info$table_name[i])
    
  }
  


}




### TOP-LEVEL DICTIONARY CSVS/SPREADSHEETS

message("Generating data dictionary master spreadsheets")

# group level loop
for(g in 1:nrow(groups)) {
  
  # open new workbook for group
  wb <- createWorkbook(groups$project_group[g], groups$project_group[g])
  
  group_dd <- tibble()
  
  xl_projects <- project_groups %>% filter(project_group == groups$project_group[g])
  
  # project level loop
  for(p in 1:nrow(xl_projects)) {
    
    project_dd <- tibble()
    
    message(paste0("  Processing dictionary content for project: `", xl_projects$project[p], "'"))
    
    dd_tables_project <- dd_tables %>% filter(project_name == xl_projects$project[p])
    
    table_info <- get_opal_table_info(o, project = xl_projects$project[p], table_dd = dd_tables_project) %>% arrange(index)
    
    # table level loop
    for (i in 1:nrow(table_info)) {
      
      message(paste0("    Processing dictionary content for table: `", table_info$Table[i], "'"))
      
      # get dict table dd
      table_dd <- make_opal_table_csv(o, xl_projects$project[p], table_info$Table[i])
      table_dd$project <- xl_projects$project[p]
      table_dd$table <- table_info$Table[i]
      table_dd <- table_dd %>% select(project, table, everything())
      
      # add to project dd
      project_dd <- project_dd %>% bind_rows(table_dd)
      
    }
    
    # add to group dd
    group_dd <- group_dd %>% bind_rows(project_dd)
    
    # add to excel tab
    addWorksheet(wb, xl_projects$project[p])
    setColWidths(wb, xl_projects$project[p], cols = 1:100, widths = "auto")
    writeDataTable(wb, xl_projects$project[p], project_dd, colNames = TRUE, withFilter = TRUE)
    
  }
  
  # save group csv
  write_csv(group_dd, 
            path = file.path(dict_csv_path, paste0(groups$project_group[g], "__full_data_dictionary.csv")),
            na = "")
  
  # save group excel
  saveWorkbook(wb, file.path(dict_csv_path, paste0(groups$project_group[g], "__full_data_dictionary.xlsx")), overwrite = TRUE)
  
}




opal.logout(o)


## build site
library(blogdown)

build_site(local = FALSE, run_hugo = TRUE, build_rmd = TRUE)


# push to github

commit(all = TRUE, message = github_commit_message)

#git config http.postBuffer 524288000
#git add --all
#git commit -m "Major update to PSY"
#git push origin main




proc.time() - t

