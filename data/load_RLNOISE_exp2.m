
%% Load data from RLNOISE/exp2 behavioral+pupillometry dataset
%
%  The RLNOISE/exp2 behavioral+pupillometry dataset (N = 30) uses a blocked
%  design with partial and complete outcome conditions. In both conditions, the
%  drifts in the mean values associated with the two options can either be
%  uncorrelated or anticorrelated between options. The presence or absence of
%  correlation between option values was cued explicitly at the beginning of
%  each block. Here we ignore the blocks with anticorrelated drifts in option
%  values. Note that the sampling variance is rather high in this behavioral
%  dataset, as in all experiments performed in the RLNOISE study.
%
%  Valentin Wyart <valentin.wyart@ens.fr>

% clear workspace
clear all
close all
clc

% set list of subjects
subjlist = 01:30;
nsubj = numel(subjlist);

for isubj = 15:nsubj

    % load file
    fname = sprintf('./RLNOISE_exp2/RLNOISE_exp2_S%02d_data.mat',subjlist(isubj));
    fname_csv = sprintf('./RLNOISE_exp2/RLNOISE_exp2_S%02d_data.csv',subjlist(isubj));
    load(fname,'expe');

    % get data
    fbtype = []; % feedback type
    resp   = []; % response
    rew    = []; % reward values
    trl    = []; % trial number in current block
    for iblk = 5:12
        % ignore blocks with anticorrelated rewards
        if expe(iblk).cfg.anticorr == 1
            continue
        end
        fbtype = cat(1,fbtype,expe(iblk).cfg.feedback(ones(96,1)));
        resp = cat(1,resp,expe(iblk).rslt.resp(:));
        rew = cat(1,rew,expe(iblk).vs'/100);
        trl = cat(1,trl,(1:96)');
    end

    x = [fbtype, resp, rew, trl]
    csvwrite(fname_csv,x)

end
