## @knitr collect_data

#This should be easy to generalize
#option to set directory and search path
#option to rad in label file

library(data.table)
library(tidyr)
if(is.na(ncores <- as.numeric(Sys.getenv('SLURM_CPUS_ON_NODE')))){
  ncores <- 8
  message('No environment variable specifying number of cores. Setting to ', ncores, '.')
}
setDTthreads(ncores)

fname <- 'sea_rsfc_schaefer400x7.RDS'
if(file.exists(fname)){
  adt_labels <- readRDS(fname)
} else {
  data_dir <- '/net/holynfs01/srv/export/mclaughlin/share_root/stressdevlab/SEA_BIDS/derivatives/1.4.1-final/xcpengine-default'
  subs <- sprintf('sub-10%02d', c(1:16,18:31))
  sess <- sprintf('ses-%02d', 1:10)
  subses <- expand.grid(sess, subs)
  files <- paste0(data_dir, '/', subses[,2], '/', subses[,1], '/fcon/schaefer400x7/', subses[,2], '_', subses[,1], '_schaefer400x7.net')
  file_id <- paste0(subses[,2], '_', subses[,1])
  
  message('Creating file list...')
  files_list <- lapply(files, function(x) ifelse(file.exists(x), x, ''))
  
  message('Reading rsfc files...')
  if(ncores > 1){
    message('Using ', ncores, ' cores to read files.')
    cl <- parallel::makePSOCKcluster(ncores)
    
    adt_list <- unlist(parallel::parLapply(cl = cl, split(files, 1:ncores), function(part){
      lapply(part, function(f){
        if(file.exists(f)){
          data.table::fread(f, skip = 2, col.names = c('node1', 'node2', 'r'))
        } else {
          data.table::data.table()
        }
      })
    }), recursive = FALSE)
    try(stop(cl))
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
}
