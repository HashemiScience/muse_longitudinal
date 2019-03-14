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
    STEP = 2 # seconds
    AVG_NUM = 14 # psds to average prior to FOOOF extraction
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
            md.annotations, conditions={'session_stage': stage},
            concat_chunks=True)
        print(raw_eeg_stage.timestamps[-1]-raw_eeg_stage.timestamps[0])
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

        n_avg = np.arange(0, psds.shape[0]-AVG_NUM, AVG_NUM)
        if n_avg.shape[0] == 0:
            print(stage, n_avg, psds.shape)
            psd_means = np.nanmean(psds, keepdims=True, axis=0)
            psd_stds = np.nanstd(psds, keepdims=True, axis=0)
            psd_maxs = np.nanmax(psds, keepdims=True, axis=0)
            avg_n = psds.shape[0]
        else:
            psd_means = [np.nanmean(psds[n:n+AVG_NUM, :, :], keepdims=True, axis=0)
                        for n in n_avg]
            psd_means = np.concatenate(psd_means, axis=0)
            psd_stds = [np.nanstd(psds[n:n+AVG_NUM, :, :], keepdims=True, axis=0)
                        for n in n_avg]
            psd_stds = np.concatenate(psd_stds, axis=0)
            psd_maxs = [np.nanmax(psds[n:n+AVG_NUM, :, :], keepdims=True, axis=0)
                        for n in n_avg]
            psd_maxs = np.concatenate(psd_maxs, axis=0)
            avg_n = AVG_NUM

        psds_dict['psd_mean_{}'.format(stage)] = psd_means
        psds_dict['psd_std_{}'.format(stage)] = psd_stds
        psds_dict['psd_max_{}'.format(stage)] = psd_maxs
        psds_dict['psd_num_{}'.format(stage)] = int(avg_n)
        psds_dict['freqs_{}'.format(stage)] = freqs

        #to be added: old alpha method - er: was it just max between 8 - 13?

        # compute FOOOF psd characterization
        fg = FOOOFGroup(**fooof_settings)
        fgs = fit_fooof_group_3d(fg, freqs, np.swapaxes(psd_means[:, [0, 3], :], 0, 1),
                                 freq_range=[3, 40])
        fg_dict['fg_{}_{}'.format(stage, 'ch1')]= fgs[0]
        fg_dict['fg_{}_{}'.format(stage, 'ch4')]= fgs[1]

    return [psds_dict, fg_dict]

spectral_pipeline = partial(cloud_runner.data_pipeline, process_func=process_file)