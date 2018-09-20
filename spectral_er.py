#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Spectral study: PSD characterization with FOOOF
===============================================

"""
from functools import partial

import numpy as np
import mne
from fooof import FOOOFGroup, fit_fooof_group_3d

from musetools.inout import load_musings, exporters
from cloud_runner import cloud_runner

np.warnings.filterwarnings('ignore', 'All-NaN slice encountered')


def process_file(filename):
    """
    Args:
        filename (str): path to muse file

    Returns:
        (string) : csv of the result dataframes
    """
    CONDITIONS = {'session_stage': ['calibration', 'meditation']}
    WIN_LEN = 5  # seconds
    STEP = WIN_LEN / 2 # 50% overlap 
    AVG_NUM = 30 # psds to average prior to FOOOF extraction ER: Might as well average all of them, since we'll 
    fmin, fmax = 2, 80  # look at PSD between 2 and 80 Hz
    PSD_TIMES = [120, 300] # times during meditation to extract PSD, 2 - 5min

    fooof_settings = {'peak_width_limits' : [1, 6],
                      'max_n_peaks' : 6,
                      'min_peak_amplitude' : 0.1,
                      'peak_threshold' : 1.5,
                      'background_mode' : 'fixed'}

    raw_data_dict = dict()
    psds_dict = dict()
    fg_dict = dict()

    # load muse file
    md = load_musings(filename)
    eeg_raw = md.raw_data.get('eeg')

    # add "meditation" annotations from 2-5 min of NFB
    nfb_times = md.annotations.get_timestamps('session_stage', 'nfb')
    md.annotations.add('meditation', nfb_times[0][0] + PSD_TIMES[0],
                       end_ts=nfb_times[0][0] + PSD_TIMES[1],
                       kind='session_stage')

    # export to mne
    raw = exporters.to_mne(md)

    for id, stage in enumerate(['calibration', 'meditation']):
        raw_eeg_stage = eeg_raw.filter_by_annotation(
            md.annotations, conditions={'session_stage': stage})
        raw_data_dict['raw_eeg_{}'.format(stage)] = raw_eeg_stage.samples
        raw_data_dict['ts_{}'.format(stage)] = raw_eeg_stage.timestamps

        # epoch raw data
        stage_ind = np.where(raw.annotations.description == stage)[0][0]
        stage_onset = raw.annotations.onset[stage_ind]
        events = mne.make_fixed_length_events(
            raw, id=id, start=stage_onset, duration=STEP,
            stop=stage_onset+raw.annotations.duration[stage_ind])
        epochs = mne.Epochs(raw, events, event_id=id, tmin=0, tmax=WIN_LEN,
                            baseline=None, reject=None, picks=[0,1,2,3])
                            # reject={'eeg':275e-6}

        # compute epoch psds
        psds, freqs = mne.time_frequency.psd_multitaper(epochs, fmin=fmin,
                                                        fmax=fmax, bandwidth=0.5)
        psd_means = [np.mean(psds[n:n+AVG_NUM, :, :], keepdims=True, axis=0)
                     for n in np.arange(0, psds.shape[0]-AVG_NUM, AVG_NUM)]              
        psd_means = np.concatenate(psd_means, axis=0)
        
        # ER: pls also get variability of PSDs. (st dev) and output
        psds_dict['psds_{}'.format(stage)] = psd_means
        psds_dict['freqs_{}'.format(stage)] = freqs

        #to be added: old alpha method - er: was it just max between 8 - 13? 

        # compute FOOOF psd characterization
        fg = FOOOFGroup(**fooof_settings)
        fgs = fit_fooof_group_3d(fg, freqs, np.swapaxes(psd_means[:, [0, 3], :], 0, 1),
                                 freq_range=[3, 40]) 
        fg_dict['fg_{}_{}'.format(stage, 'ch1')]= fgs[0]
        fg_dict['fg_{}_{}'.format(stage, 'ch4')]= fgs[1]

    return [raw_data_dict, psds_dict, fg_dict]

# spectral_pipeline = partial(cloud_runner.data_pipeline, process_func=process_file)

if __name__ == "__main__":
    path_name = 'spectral-study'
    file_path = '/Users/nicoleproulx/Documents/data/spectral_study_data/test_data/' \
        '8ed8050c55fd4d79b53d126fe9ce653f.muse'
    data = {'session_id': '8ed8050c55fd4d79b53d126fe9ce653f'}
    result = process_file(file_path)
    cloud_runner.output_dict(result[0], 'raw_data', data, path_name)
    cloud_runner.output_dict(result[1], 'psds', data, path_name)
    cloud_runner.output_fooofgroup(result[2], data, path_name)