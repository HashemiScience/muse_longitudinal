# install.packages("rjson")
library(rjson)
library(data.table)
library(ggplot2)
library(dplyr)

# Get a list of filenames (psd only)
getwd()
datapath = "~/Documents/MATLAB/muse_longitudinal/sample data/"
files_psd <- dir(datapath, pattern="*psds.json")
files_raw = dir(datapath, pattern="*raw_data.json")
files_ch1med <- dir(datapath, pattern="*fg_meditation_ch1.json")
file_subjdetails <- dir(datapath, pattern="*.csv")
subjdets <- read.csv(paste0(datapath,file_subjdetails))

head(subjdets)

meditate_psd = tibble(session_id = NA, channel = NA, epoch = NA, power = NA, freq = NA)
meditate_psd = meditate_psd[-1,]

#######
## To do still: for efficiency when dealing with larger dataset, convert the epoch and channel loops to pipe-lines if possible. Needs some fiddling around.
######
for(curfile in files_psd){ # loop per session file

  # Give the input file name to the function.
  raw_psd <- fromJSON(file = paste0(datapath, curfile))
  # raw_eeg <- fromJSON(file = paste0(datapath, files_raw[1]))
  # data_ch1_med <- fromJSON(file = paste0(datapath, files_ch1med[1]))
  
  for(ss in 1:length(raw_psd$psds_meditation)){ # loop through number of epochs per subj

    freq_num <- as.numeric(raw_psd$freqs_meditation)
    
    for(cc in 1:4){ # loop through 4 channels
      
      # Convert JSON file to a data frame.
      psd_num <- as.numeric(raw_psd$psds_meditation[[ss]][[cc]])
      
      curmat <- tibble(session_id = substr(curfile,1,nchar(curfile)-10), channel = cc, epoch = ss, power = psd_num, freq = freq_num)
      
      meditate_psd <- rbind(meditate_psd, curmat)
      
    }
  }
}


# convert channel an epochs to factors
meditate_psd$channel = as.factor(meditate_psd$channel)
meditate_psd$epoch = as.factor(meditate_psd$epoch)

# merge actual PSD with user/session info dataset according to matching "session_id" value
merged_df <- merge(meditate_psd, subjdets, by="session_id")

# the FFT was not percent, off by 0.01 Hz often, so round to nearest 1 decimal
merged_df$freq <- round(merged_df$freq,1) 

# check that the merge worked
unique(merged_df$session_id)
unique(meditate_psd$session_id)
unique(subjdets$session_id)

# plot 4 channels per session, average across all epochs per session
ggplot(data=merged_df, aes(x=freq, y=power, col=channel), na.rm=T) +
  stat_summary_bin(aes(), fun.y=mean, geom="line") +
  facet_wrap(~user_id+session_timestamp) +
  xlim(c(0,30)) +
  scale_y_log10()
  

## extract parameters (e.g., alpha amp) to compare across sessions:

# create separate data.frame for each parameter
alphaAmp <- subset(merged_df, freq>7&freq<13)
alphaAmp %>% group_by(user_id,session_id,channel,epoch) %>% summarize(alphaAmp = mean(power))

# alternatively, and better, label each freq with band label, then categorize by them
merged_df$band <- NA
merged_df[merged_df$freq<30.5,"band"] = "beta"
merged_df[merged_df$freq<13.5,"band"] = "alpha"
merged_df[merged_df$freq<8,"band"] = "theta"
merged_df[merged_df$freq<3,"band"] = "delta"

# calculate band power for each channel, epoch, session
bandpower_df <- merged_df[merged_df$freq<30.5,] %>% group_by(user_id,session_id,session_timestamp,channel,epoch,band) %>% summarize(power = mean(power))
