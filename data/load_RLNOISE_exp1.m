
%% Load data from RLNOISE/exp1 behavioral+fMRI dataset
%
%  The RLNOISE/exp1 behavioral+fMRI dataset (N = 30) uses a blocked design
%  with partial and complete outcome conditions. In both conditions, the drifts
%  in the mean values associated with the two options are uncorrelated between
%  options. Note that the sampling variance is rather high in this behavioral
%  dataset, as in all experiments performed in the RLNOISE study. The task also
%  features cued trials in which the subject was required to select a pre-cued
%  option rather than choosing the option he/she wanted to sample.
%
%  Valentin Wyart <valentin.wyart@ens.fr>

% clear workspace
clear all
close all
clc

% set list of subjects
subjlist = 01:30;
nsubj = numel(subjlist);

for isubj = 1:nsubj

    % load file
    fname = sprintf('./RLNOISE_exp1/RLNOISE_exp1_S%02d_data.mat',subjlist(isubj));
    fname_csv = sprintf('./RLNOISE_exp1/RLNOISE_exp1_S%02d_data.csv',subjlist(isubj));
    load(fname,'expe');

    % get data
    fbtype = []; % feedback type
    resp   = []; % response
    rew    = []; % reward values
    trl    = []; % trial number in current block
    cue    = []; % cued trial?
    for iblk = 3:10
        fbtype = cat(1,fbtype,expe(iblk).cfg.feedback(ones(56,1)));
        resp = cat(1,resp,expe(iblk).rslt.resp(:));
        rew = cat(1,rew,expe(iblk).vs'/100);
        trl = cat(1,trl,(1:56)');
        cue = cat(1,cue,expe(iblk).act(:) == 0);
    end

    % do whatever you want to do with the data
    x = [fbtype, resp, rew, trl, cue]
    csvwrite(fname_csv,x)

end



