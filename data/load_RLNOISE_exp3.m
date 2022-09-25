
%% Load data from RLNOISE/exp3 behavioral dataset
%
%  The RLNOISE/exp3 behavioral dataset (N = 30) uses a repeated-blocks design
%  in the complete outcome condition. Note that the sampling variance is rather
%  high in this behavioral dataset, as in all experiments performed in the
%  RLNOISE study.
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
    fname = sprintf('./data/RLNOISE_exp4/RLNOISE_exp4_S%02d_data.mat',subjlist(isubj));
    load(fname,'expe');
    
    % get data
    resp = []; % response
    rew  = []; % reward values
    trl  = []; % trial number in current block
    blk  = []; % block number
    blk_seed = []; % seed block number for current block
    for iblk = 3:18
        resp = cat(1,resp,expe(iblk).rslt.resp(:));
        rew = cat(1,rew,expe(iblk).vs'/100);
        trl = cat(1,trl,(1:56)');
        blk = cat(1,blk,(iblk-2)*ones(56,1));
        bseed = expe(iblk).origblck;
        if isempty(bseed), bseed = iblk; end
        blk_seed = cat(1,blk_seed,(bseed-2)*ones(56,1));
    end
    
    % do whatever you want to do with the data
    
end
