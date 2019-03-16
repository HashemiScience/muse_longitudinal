
#psds -- are _1 1 to 100
#psds -- are _1 101 to end

# extract each data frame separately. 


psd_cal_ch1_1 = psds[[1]]
psd_cal_ch2_1 = psds[[2]]
psd_cal_ch3_1 = psds[[3]]
psd_cal_ch4_1 = psds[[4]]
psd_med_ch1_1 = psds[[5]]
psd_med_ch2_1 = psds[[6]]
psd_med_ch3_1 = psds[[7]]
psd_med_ch4_1 = psds[[8]]


psd_cal_ch1_2 = psds2[[1]]
psd_cal_ch2_2 = psds2[[2]]
psd_cal_ch3_2 = psds2[[3]]
psd_cal_ch4_2 = psds2[[4]]
psd_med_ch1_2 = psds2[[5]]
psd_med_ch2_2 = psds2[[6]]
psd_med_ch3_2 = psds2[[7]]
psd_med_ch4_2 = psds2[[8]]



# combine the correct ones. 

psd_cal_ch1_part1 = bind_rows(psd_cal_ch1_1,psd_cal_ch1_2)
psd_cal_ch2_part1 = bind_rows(psd_cal_ch2_1,psd_cal_ch2_2)
psd_cal_ch3_part1 = bind_rows(psd_cal_ch3_1,psd_cal_ch3_2)
psd_cal_ch4_part1 = bind_rows(psd_cal_ch4_1,psd_cal_ch4_2)

psd_med_ch1_part1 = bind_rows(psd_med_ch1_1,psd_med_ch1_2)
psd_med_ch2_part1 = bind_rows(psd_med_ch2_1,psd_med_ch2_2)
psd_med_ch3_part1 = bind_rows(psd_med_ch3_1,psd_med_ch3_2)
psd_med_ch4_part1 = bind_rows(psd_med_ch4_1,psd_med_ch4_2)

# save separate Rdata files with _pt1 suffix.

save(psd_cal_ch1_part1, 
  psd_cal_ch2_part1,
  psd_cal_ch3_part1, 
  psd_cal_ch4_part1, 
  psd_med_ch1_part1 ,
  psd_med_ch2_part1,
  psd_med_ch3_part1,
  psd_med_ch4_part1, file = './data_20180930/psds_part1_med_cal_allchans.Rdata')
  
  # save separate Rdata files with _pt1 suffix.