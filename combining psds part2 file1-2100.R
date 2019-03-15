load("~/github/muse_longitudinal/spectral_20180930_psd_cal_1to100.Rdata")
load("~/github/muse_longitudinal/spectral_20180930_psd_med_1to100.Rdata")

# these are the first 100
all_psd_cal_ch1_a = all_psd_cal_ch1
all_psd_cal_ch2_a = all_psd_cal_ch2
all_psd_cal_ch3_a = all_psd_cal_ch3
all_psd_cal_ch4_a = all_psd_cal_ch4


all_psd_med_ch1_a = all_psd_med_ch1
all_psd_med_ch2_a = all_psd_med_ch2
all_psd_med_ch3_a = all_psd_med_ch3
all_psd_med_ch4_a = all_psd_med_ch4

load("~/github/muse_longitudinal/spectral_20180930_psd_2_cal_100toNotEnd.Rdata")
load("~/github/muse_longitudinal/spectral_20180930_psd_2_med_101toNotEnd.Rdata")
# this overwrote the all_psd_cal_ch1.. etc. 

length(unique(all_psd_cal_ch1$session_id))

psd_cal_ch1_part2a = bind_rows(all_psd_cal_ch1_a, all_psd_cal_ch1)
psd_cal_ch2_part2a = bind_rows(all_psd_cal_ch2_a, all_psd_cal_ch2)
psd_cal_ch3_part2a = bind_rows(all_psd_cal_ch3_a, all_psd_cal_ch3)
psd_cal_ch4_part2a = bind_rows(all_psd_cal_ch4_a, all_psd_cal_ch4)

psd_med_ch1_part2a = bind_rows(all_psd_med_ch1_a, all_psd_med_ch1)
psd_med_ch2_part2a = bind_rows(all_psd_med_ch2_a, all_psd_med_ch2)
psd_med_ch3_part2a = bind_rows(all_psd_med_ch3_a, all_psd_med_ch3)
psd_med_ch4_part2a = bind_rows(all_psd_med_ch4_a, all_psd_med_ch4)


save(psd_cal_ch1_part2a, 
     psd_cal_ch2_part2a,
     psd_cal_ch3_part2a, 
     psd_cal_ch4_part2a, 
     psd_med_ch1_part2a, 
     psd_med_ch2_part2a, 
     psd_med_ch3_part2a, 
     psd_med_ch4_part2a, file = './data_20180930/psds_part2a_med_cal_allchans.Rdata')

save(psd_cal_ch1_part2a, 
     psd_med_ch1_part2a, file = './data_20180930/psds_part2a_med_cal_ch1.Rdata')

save(psd_cal_ch2_part2a, 
     psd_med_ch2_part2a, file = './data_20180930/psds_part2a_med_cal_ch2.Rdata')

save(psd_cal_ch3_part2a, 
     psd_med_ch3_part2a, file = './data_20180930/psds_part2a_med_cal_ch3.Rdata')

save(psd_cal_ch4_part2a, 
     psd_med_ch4_part2a, file = './data_20180930/psds_part2a_med_cal_ch4.Rdata')
#####################
# combining part 1 and 2a

psd_cal_ch1_part1to2a = bind_rows(psd_cal_ch1_part1, psd_cal_ch1_part2a)
psd_cal_ch2_part1to2a = bind_rows(psd_cal_ch2_part1, psd_cal_ch2_part2a)
psd_cal_ch3_part1to2a = bind_rows(psd_cal_ch3_part1, psd_cal_ch3_part2a)
psd_cal_ch4_part1to2a = bind_rows(psd_cal_ch4_part1, psd_cal_ch4_part2a)

psd_med_ch1_part1to2a = bind_rows(psd_med_ch1_part1, psd_med_ch1_part2a)
psd_med_ch2_part1to2a = bind_rows(psd_med_ch2_part1, psd_med_ch2_part2a)
psd_med_ch3_part1to2a = bind_rows(psd_med_ch3_part1, psd_med_ch3_part2a)
psd_med_ch4_part1to2a = bind_rows(psd_med_ch4_part1, psd_med_ch4_part2a)

save(psd_cal_ch1_part1to2a,file = './data_20180930/psd_cal_ch1_part1to2a.Rdata')
save(psd_cal_ch2_part1to2a,file = './data_20180930/psd_cal_ch2_part1to2a.Rdata')
save(psd_cal_ch3_part1to2a,file = './data_20180930/psd_cal_ch3_part1to2a.Rdata')
save(psd_cal_ch4_part1to2a,file = './data_20180930/psd_cal_ch4_part1to2a.Rdata')

save(psd_med_ch1_part1to2a,file = './data_20180930/psd_med_ch1_part1to2a.Rdata')
save(psd_med_ch2_part1to2a,file = './data_20180930/psd_med_ch2_part1to2a.Rdata')
save(psd_med_ch3_part1to2a,file = './data_20180930/psd_med_ch3_part1to2a.Rdata')
save(psd_med_ch4_part1to2a,file = './data_20180930/psd_med_ch4_part1to2a.Rdata')
