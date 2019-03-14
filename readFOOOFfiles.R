readFOOOFjson2Rdata<-function(fooofpath){
  library(purrr) # required for unlisting fooof to find best peak
  all_fooof_med_ch1 = tibble(session_id = NA, 
                             bg_int = NA,
                             bg_slope = NA,
                             pk_freq = NA,
                             pk_amp = NA,
                             pk_bw = NA,
                             r2 = NA,
                             err = NA)
  all_fooof_med_ch1 = all_fooof_med_ch1[-1,]
  all_fooof_med_ch4 = all_fooof_med_ch1
  all_fooof_cal_ch1 = all_fooof_med_ch1
  all_fooof_cal_ch4 = all_fooof_med_ch1
  
  start_time <- Sys.time()
 for( ii in 1:length(files_fooof_med_ch1) ){ # loop per session file
  curfile = files_fooof_med_ch1[ii]
  fname = strsplit(curfile, '/')
  session_id = strsplit(fname[[1]][length(fname[[1]])], '_')[[1]][1]

  # Give the input file name to the function.
  curfooof <-tryCatch(
    {
      curfooof <- fromJSON(file = curfile)
    },
    error = function(cond){
      message(cond)
      message(paste0('cannot read', curfile, ' skipping......'))
      curfooof = NULL
      return(NULL)
    }
  )
  if (is.null(curfooof)){
    next
  }

  print(paste0('loaded ', curfile))
  curpeaks <- map_df(curfooof$peak_params_, ~as.data.frame(t(.)))
  names(curpeaks) <- c("freq","amp","bw")
  #pkIX <- which.max(curpeaks[curpeaks$freq<14&curpeaks$freq>7,2])  # going to keep all peaks.




  curmat <- tibble(session_id = session_id,
                   type = 'med',
                   channel = 1,
                   bg_int=curfooof$background_params_[1],
                   bg_slope=curfooof$background_params_[2],
                   pk_freq = curpeaks$freq,
                   pk_amp = curpeaks$amp,
                   pk_bw = curpeaks$bw,
                   r2 = curfooof$r_squared_,
                   err = curfooof$error_)
  #curmat <- tibble(session_id = substr(curfile,1,nchar(curfile)-10), channel = cc, epoch = ss, power = psd_num, freq = freq_num)

  all_fooof_med_ch1 <- bind_rows(all_fooof_med_ch1, curmat)
}
save(file = 'spectral_20180930_fooof.Rdata', list = ls(pattern = "all_fooof*"))

  
  for( ii in 1:length(files_fooof_cal_ch1) ){ # loop per session file 
    curfile = files_fooof_cal_ch1[ii]
    
    fname = strsplit(curfile, '/')
    session_id = strsplit(fname[[1]][length(fname[[1]])], '_')[[1]][1]
    
    # Give the input file name to the function.
    curfooof <-tryCatch(
      {
        curfooof <- fromJSON(file = curfile)
      }, 
      error = function(cond){
        message(cond)
        message(paste0('cannot read', curfile, ' skipping......'))
        curfooof = NULL
        return(NULL)
      }
    )
    if (is.null(curfooof)){
      next
    }
    print(paste0('loaded ', curfile))
    curpeaks <- map_df(curfooof$peak_params_, ~as.data.frame(t(.)))
    names(curpeaks) <- c("freq","amp","bw")
    #pkIX <- which.max(curpeaks[curpeaks$freq<14&curpeaks$freq>7,2])  # going to keep all peaks. 
    
    curmat <- tibble(session_id = session_id, 
                     type = 'cal',
                     channel = 1,
                     bg_int=curfooof$background_params_[1],
                     bg_slope=curfooof$background_params_[2],
                     pk_freq = curpeaks$freq,
                     pk_amp = curpeaks$amp,
                     pk_bw = curpeaks$bw,
                     r2 = curfooof$r_squared_,
                     err = curfooof$error_)
    #curmat <- tibble(session_id = substr(curfile,1,nchar(curfile)-10), channel = cc, epoch = ss, power = psd_num, freq = freq_num)
    
    all_fooof_cal_ch1 <- bind_rows(all_fooof_cal_ch1, curmat)
  }
  save(file = 'spectral_20180930_fooof.Rdata', list = ls(pattern = "all_fooof*"))
  
  
  for( ii in 1:length(files_fooof_med_ch4) ){ # loop per session file 
    curfile = files_fooof_med_ch4[ii]
    fname = strsplit(curfile, '/')
    session_id = strsplit(fname[[1]][length(fname[[1]])], '_')[[1]][1]
    
    # Give the input file name to the function.
    curfooof <-tryCatch(
      {
        curfooof <- fromJSON(file = curfile)
      }, 
      error = function(cond){
        message(cond)
        message(paste0('cannot read', curfile, ' skipping......'))
        curfooof = NULL
        return(NULL)
      }
    )
    if (is.null(curfooof)){
      next
    }
    print(paste0('loaded ', curfile))
    curpeaks <- map_df(curfooof$peak_params_, ~as.data.frame(t(.)))
    names(curpeaks) <- c("freq","amp","bw")
    #pkIX <- which.max(curpeaks[curpeaks$freq<14&curpeaks$freq>7,2])  # going to keep all peaks. 
    
    curmat <- tibble(session_id = session_id, 
                     type = 'med',
                     channel = 4,
                     bg_int=curfooof$background_params_[1],
                     bg_slope=curfooof$background_params_[2],
                     pk_freq = curpeaks$freq,
                     pk_amp = curpeaks$amp,
                     pk_bw = curpeaks$bw,
                     r2 = curfooof$r_squared_,
                     err = curfooof$error_)
    #curmat <- tibble(session_id = substr(curfile,1,nchar(curfile)-10), channel = cc, epoch = ss, power = psd_num, freq = freq_num)
    
    all_fooof_med_ch4 <- bind_rows(all_fooof_med_ch4, curmat)
  }
  save(file = 'spectral_20180930_fooof.Rdata', list = ls(pattern = "all_fooof*"))
  
  for( ii in 1:length(files_fooof_cal_ch4) ){ # loop per session file 
    curfile = files_fooof_cal_ch4[ii]
    fname = strsplit(curfile, '/')
    session_id = strsplit(fname[[1]][length(fname[[1]])], '_')[[1]][1]
    
    # Give the input file name to the function.
    curfooof <-tryCatch(
      {
        curfooof <- fromJSON(file = curfile)
      }, 
      error = function(cond){
        message(cond)
        message(paste0('cannot read', curfile, ' skipping......'))
        curfooof = NULL
        return(NULL)
      }
    )
    if (is.null(curfooof)){
      next
    }
    print(paste0('loaded ', curfile))
    curpeaks <- map_df(curfooof$peak_params_, ~as.data.frame(t(.)))
    names(curpeaks) <- c("freq","amp","bw")
    #pkIX <- which.max(curpeaks[curpeaks$freq<14&curpeaks$freq>7,2])  # going to keep all peaks. 
    
    curmat <- tibble(session_id = session_id, 
                     type = 'cal',
                     channel = 4,
                     bg_int=curfooof$background_params_[1],
                     bg_slope=curfooof$background_params_[2],
                     pk_freq = curpeaks$freq,
                     pk_amp = curpeaks$amp,
                     pk_bw = curpeaks$bw,
                     r2 = curfooof$r_squared_,
                     err = curfooof$error_)
    #curmat <- tibble(session_id = substr(curfile,1,nchar(curfile)-10), channel = cc, epoch = ss, power = psd_num, freq = freq_num)
    
    all_fooof_cal_ch4 <- bind_rows(all_fooof_cal_ch4, curmat)
  }
  save(file = 'spectral_20180930_fooof.Rdata', list = ls(pattern = "all_fooof*"))
  
  end_time <- Sys.time()
  (total_time = end_time-start_time)
  
  return(list(ls(pattern="all_fooof*")))
}