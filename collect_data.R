## @knitr collect_data

#This should be easy to generalize
#option to set directory and search path
#option to rad in label file

library(data.table)
library(tidyr)
if(is.na(ncores <- as.numeric(Sys.getenv('SLURM_CPUS_ON_NODE')))){
  ncores <- 1
  message('No environment variable specifying number of cores. Setting to ', ncores, '.')
}
setDTthreads(ncores)

if(file.exists(fname)){
  adt_labels <- readRDS(fname)
  schaefer_400_7_net_ids <- readRDS('schaefer_ids.RDS')
} else {
  message('Creating file list...')
  files_list <- lapply(files, function(x) ifelse(file.exists(x), x, ''))
  
  message('Reading rsfc files...')
  if(ncores > 1){
    message('Using ', ncores, ' cores to read files.')
    cl <- parallel::makePSOCKcluster(max(c(ncores - 1), 1))
    
    adt_list <- unlist(parallel::parLapply(cl = cl, split(files, 1:ncores), function(part){
      lapply(part, function(f){
        if(file.exists(f)){
          data.table::fread(f, skip = 2, col.names = c('node1', 'node2', 'r'))
        } else {
          data.table::data.table()
        }
      })
    }), recursive = FALSE)
    try(parallel::stopCluster(cl))
  } else {
    adt_list <- lapply(files, function(f){
      if(file.exists(f)){
        data.table::fread(f, skip = 2, col.names = c('node1', 'node2', 'r'))
      } else {
        data.table::data.table()
      }
    }) 
  }
  names(adt_list) <- file_id
  
  message('Combining data, assigning labels...')
  adt <- rbindlist(adt_list, idcol = 'id')
  
  #https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/MNI
  schaefer_400_7_net_ids <- fread('Schaefer2018_400Parcels_7Networks_order.txt', 
                                  col.names = c('node1', 'network1', 'V1', 'V2', 'V3', 'V4'))
  schaefer_400_7_net_ids[, c('V1', 'V2', 'V3', 'V4') := NULL]
  schaefer_400_7_net_ids <- schaefer_400_7_net_ids %>% 
    tidyr::extract(network1, c('network1', 'roi1'), '7Networks_([RL]H_.*)_(\\d+)')
  
  setkey(adt, node1)
  setkey(schaefer_400_7_net_ids, node1)
  adt_labels1 <- schaefer_400_7_net_ids[adt]
  setnames(schaefer_400_7_net_ids, c('node2', 'network2', 'roi2'))
  setkey(adt_labels1, node2)
  setkey(schaefer_400_7_net_ids, node2)
  adt_labels <- schaefer_400_7_net_ids[adt_labels1]
  adt_labels[, c('id', 'sess') := tstrsplit(id, '_', fixed = TRUE)]
  message('Saving RDS file to: ', fname)
  saveRDS(adt_labels, fname)
  message('Saving Schaefer ID data table to schaefer_ids.rds')
  saveRDS(schaefer_400_7_net_ids, 'schaefer_ids.RDS')
}

if(FALSE){
  adt_labels_old <- readRDS('sea_rsfc_schaefer400x7.RDS')
  missing_files_csv <- data.table(file = files, does_not_exist = files_list == '')
  View(missing_files_csv[does_not_exist == TRUE])
  readr::write_csv(data.table(file = files, does_not_exist = files_list == ''), 
                   '~/sea_rsfc-missing_fcon_output.csv')
  
  a <- data.table(file = files, does_not_exist = files_list == '')
  a[, fcon_missing := lapply(gsub('(.*/fcon)/.*', '\\1', file), function(x) !file.exists(x))]
  a[, missmatch := does_not_exist != fcon_missing ]
  a[(missmatch), file]
}