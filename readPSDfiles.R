readPSDfiles<-function(datapath){# Get a list of filenames (psd only)
  # reads in all the files in a directory and saves them into separate cal and med R.data files, with separate table for each channel. 
#datapath = "./data_20180930/"

files_psd <- list.files(datapath, pattern="*psds.json", recursive = TRUE, full.names = TRUE)

all_psd_med_ch1 = NULL
all_psd_med_ch2 = NULL
all_psd_med_ch3 = NULL
all_psd_med_ch4 = NULL
all_psd_cal_ch1 = NULL
all_psd_cal_ch2 = NULL
all_psd_cal_ch3 = NULL
all_psd_cal_ch4 = NULL

start_time <- Sys.time()
for(ii in 1:100){ # loop per session file   length(files_psd)
  # Give the input file name to the function.
  curfile = files_psd[ii]
  fname = strsplit(curfile, '/')
  session_id = strsplit(fname[[1]][length(fname[[1]])],'_')[[1]][1]
  
  print(paste0('loading file ',ii, ' of ', length(files_psd), ' : ', curfile))
  raw_psd <- fromJSON(file =curfile)
  
  # read the meditation data in table, separate for each channel. 
  for(ee in 1:length(raw_psd$psd_mean_meditation)){ # loop through number of epochs per subj
    psd_med_ch1 =tibble(session_id = session_id, 
                        type = 'med',
                        channel = 1,
                        epoch = ee,
                        numPs = raw_psd$psd_num_calibration,
                        freq = raw_psd$freqs_meditation,
                        meanP = raw_psd$psd_mean_meditation[[ee]][[1]],
                        stdP = raw_psd$psd_std_meditation[[ee]][[1]],
                        maxP = raw_psd$psd_max_meditation[[ee]][[2]])
    
    
    psd_med_ch2 =tibble(session_id = session_id, 
                        channel = 1,
                        type = 'med',
                        epoch = ee,
                        numPs = raw_psd$psd_num_calibration,
                        freq = raw_psd$freqs_meditation,
                        meanP = raw_psd$psd_mean_meditation[[ee]][[2]],
                        stdP = raw_psd$psd_std_meditation[[ee]][[2]],
                        maxP = raw_psd$psd_max_meditation[[ee]][[2]])
    psd_med_ch3 =tibble(session_id = session_id, 
                        channel = 3,
                        type = 'med',
                        epoch = ee,
                        numPs = raw_psd$psd_num_calibration,
                        freq = raw_psd$freqs_meditation,
                        meanP = raw_psd$psd_mean_meditation[[ee]][[3]],
                        stdP = raw_psd$psd_std_meditation[[ee]][[3]],
                        maxP = raw_psd$psd_max_meditation[[ee]][[3]])
    psd_med_ch4 =tibble(session_id = session_id, 
                        channel = 4,
                        type = 'med',
                        epoch = ee,
                        numPs = raw_psd$psd_num_calibration,
                        freq = raw_psd$freqs_meditation,
                        meanP = raw_psd$psd_mean_meditation[[ee]][[4]],
                        stdP = raw_psd$psd_std_meditation[[ee]][[4]],
                        maxP = raw_psd$psd_max_meditation[[ee]][[4]])
    
    all_psd_med_ch1 = bind_rows(all_psd_med_ch1,psd_med_ch1, .id = ) # append the rows. 
    all_psd_med_ch2 = bind_rows(all_psd_med_ch2,psd_med_ch2) # append the rows. 
    all_psd_med_ch3 = bind_rows(all_psd_med_ch3,psd_med_ch3) # append the rows. 
    all_psd_med_ch4 = bind_rows(all_psd_med_ch4,psd_med_ch4) # append the rows. 
  }
  
  if(ii%%10==0){
    save(file = 'spectral_20180930_psd_med.Rdata', list = ls(pattern = 'all_psd_med_*'))
  }
  
  # read the calibration data in tables, separate for each channel. 
  for(ee in 1:length(raw_psd$psd_mean_calibration)){ # loop through number of epochs per subj
    
    psd_cal_ch1 =tibble(session_id = session_id, 
                        channel = 1,
                        type = 'cal',
                        epoch = ee,
                        numPs = raw_psd$psd_num_calibration,
                        freq = raw_psd$freqs_calibration[[ee]],
                        meanP = raw_psd$psd_mean_calibration[[ee]][[1]],
                        stdP = raw_psd$psd_std_calibration[[ee]][[1]],
                        maxP = raw_psd$psd_max_calibration[[ee]][[1]])
    
    psd_cal_ch2 =tibble(session_id = session_id, 
                        channel = 2,
                        type = 'cal',
                        epoch = ee,
                        numPs = raw_psd$psd_num_calibration,
                        freq = raw_psd$freqs_calibration[[ee]],
                        meanP = raw_psd$psd_mean_calibration[[ee]][[2]],
                        stdP = raw_psd$psd_std_calibration[[ee]][[2]],
                        maxP = raw_psd$psd_max_calibration[[ee]][[2]])
    
    psd_cal_ch3 =tibble(session_id = session_id, 
                        channel = 3,
                        type = 'cal',
                        epoch = ee,
                        numPs = raw_psd$psd_num_calibration,
                        freq = raw_psd$freqs_calibration[[ee]],
                        meanP = raw_psd$psd_mean_calibration[[ee]][[3]],
                        stdP = raw_psd$psd_std_calibration[[ee]][[3]],
                        maxP = raw_psd$psd_max_calibration[[ee]][[3]])
    
    psd_cal_ch4 =tibble(session_id = session_id, 
                        channel = 4,
                        type = 'cal',
                        epoch = ee,
                        numPs = raw_psd$psd_num_calibration,
                        freq = raw_psd$freqs_calibration[[ee]],
                        meanP = raw_psd$psd_mean_calibration[[ee]][[4]],
                        stdP = raw_psd$psd_std_calibration[[ee]][[4]],
                        maxP = raw_psd$psd_max_calibration[[ee]][[4]])
    all_psd_cal_ch1 = bind_rows(all_psd_cal_ch1, psd_cal_ch1)
    all_psd_cal_ch2 = bind_rows(all_psd_cal_ch2, psd_cal_ch2)
    all_psd_cal_ch3 = bind_rows(all_psd_cal_ch3, psd_cal_ch3)
    all_psd_cal_ch4 = bind_rows(all_psd_cal_ch4, psd_cal_ch4)
  }
  if(ii%%100==0){
  save(file = 'spectral_20180930_psd_cal.Rdata', list = ls(pattern = 'all_psd_cal_*'))
  }
} # end session file loop. 
end_time <- Sys.time()
(total_time = end_time-start_time)
out = list(all_psd_cal_ch1,all_psd_cal_ch2, all_psd_cal_ch3, all_psd_cal_ch4, 
           all_psd_med_ch1, all_psd_med_ch2, all_psd_med_ch3, all_psd_med_ch4)
return(out)
}