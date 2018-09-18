# install.packages("rjson")
library(rjson)
library(data.table)
library(ggplot2)

# Get a list of filenames (psd only)
getwd()
datapath = "~/Desktop/MATLAB/muse_longitudinal/sample data/"
files_psd <- dir(datapath, pattern="*psds.json")
files_raw = dir(datapath, pattern="*raw_data.json")
files_ch1med <- dir(datapath, pattern="*fg_meditation_ch1.json")

meditate_psd = tibble(sessID = NA, channel = NA, epoch = NA, power = NA, freq = NA)
ii=0
for(curfile in files_psd){
  # Give the input file name to the function.
  raw_psd <- fromJSON(file = paste0(datapath, curfile))
  # raw_eeg <- fromJSON(file = paste0(datapath, files_raw[1]))
  # data_ch1_med <- fromJSON(file = paste0(datapath, files_ch1med[1]))
  
  for(ss in 1:length(raw_psd$psds_meditation)){

    freq_num <- as.numeric(raw_psd$freqs_meditation)
    
    for(cc in 1:4){
      
      # Convert JSON file to a data frame.
      psd_num <- as.numeric(raw_psd$psds_meditation[[ss]][[cc]])
      
      curmat <- tibble(sessID = curfile, channel = cc, epoch = ss, power = psd_num, freq = freq_num)
      
      meditate_psd <- rbind(meditate_psd, curmat)
      
    }
  }
}

meditate_psd = meditate_psd[-1,]

meditate_psd$channel = as.factor(meditate_psd$channel)
meditate_psd$epoch = as.factor(meditate_psd$epoch)

ggplot(data=subset(meditate_psd, channel==1|channel==4)) +
  geom_line(aes(x=freq, y=power, col=epoch)) +
  facet_wrap(~sessID+channel) +
  xlim(c(0,30)) +
  scale_y_log10()
  
  