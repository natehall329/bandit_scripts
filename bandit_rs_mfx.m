%% bandit MFX script: adapted from https://github.com/PennStateDEPENdLab/spott_modeling/blob/master/vba_timeseries/fit_group_vba_mfx.m

%loads in subjects' data and fits bandit data using Q-learning model from Dombrovski et al 2019 using VBA;
close all; clear;

dir_str='Bandit_withrest';
% dir_str='subjects';
dirs=dir(dir_str);


%addpath(genpath('~/Documents/MATLAB/VBA-toolbox'))
%addpath('~/Documents/MATLAB/VBA-toolbox-master/utils')
addpath('vba/')
addpath('behav_scripts/')
%addpath('~/Box/temporal_instrumental_agent')
%addpath('~/Box/spott_modeling/vba_timeseries/') %for toying with MH's options, priors etc

mh_vba_path='~/Data_Analysis/mh_vba'; %MH helper scripts
vba_path = '~/Documents/MATLAB/VBA-toolbox';

addpath(mh_vba_path); %add MH VBA helpers (e.g., extract_group_statistics)
addpath(genpath_safe(vba_path)); %add VBA functions

if ~exist('vba_output','dir')
    mkdir('vba_output');
end


%The vanilla version is currently valence=1 decay=1 utility=0

%Set up input arguements
graphics = 0;
plot_subject=0;
save_results=0;
parameterization.valence=1;
parameterization.fix_decay=0; %The logic surrounds decay is kind of confusing
parameterization.utility=0;
parameterization.fix_all_params=0;
parameterization.disappointment = 0;
parameterization.regret = 0;
parameterization.use_reward_vec=0;

plot_subject=1;
save_results=1;

no_mri = 1;
    


%elements 1:3 are '.', '..', and '.DS_Store' -- verify this on your machine, then drop
ids = {dirs(4:length(dirs)-1).name};

dirs = dirs(4:length(dirs)-1); %for input to Alex's script.

ids_filt = {'73'}; %terrible subject; others can be added to the vec if desired

bad_ids = ismember(ids, ids_filt);
ids = ids(~bad_ids);
dirs = dirs(~bad_ids); %for input to Alex's script.

%extract IDs for record keeping
% [~,fnames]=cellfun(@fileparts, behavfiles, 'UniformOutput', false);
% ids=cellfun(@(x) char(regexp(x,'(?<=MEG_|fMRIEmoClock_)[\d_]+(?=_tc|_concat)','match')), fnames, 'UniformOutput', false);

%puts a nested cell in each element
%ids=regexp(fnames,'(?<=MEG_|fMRIEmoClock_)[\d_]+(?=_tc|_concat)','match'); %use lookahead and lookbehind to make id more flexible (e.g., 128_1)

ns = length(ids);
y_all = cell(ns, 1);
u_all = cell(ns, 1);
b_all = cell(ns, 1);
options_all = cell(ns, 1);

n_t=NaN(1,ns);

% get relevant vars from bandit script, but dont run NLstatespace
% [options,dim,priors] = bandit_vba_setup(id,graphics,plot_subject,save_results,parameterization,dir_str, no_mri);

%settings for output files
so.output_dir = '/Users/mnh5174/Data_Analysis/bandit_scripts/vba_output';
so.dataset = 'specc';
so.model = 'twolr_decay';

if ~exist([so.output_dir, filesep, 'b_outputs'],'dir')
    mkdir(['vba_output' filesep, 'b_outputs']);
end

%% Load data for each subject into a group structure
for sub = 1:ns
    
    fprintf('Loading subject %d id: %s \n', sub, ids{sub});
    id = str2double(ids{sub});
    
    [options,dim,priors,u, y, b] = bandit_vba_setup(id,graphics,plot_subject,save_results,parameterization,dir_str, no_mri);
    options.verbose=0; %prevent lots of text output for each subject during MFX fitting
    n_t(sub) = dim.n_t; % allow for variation in number of trials across subjects
  
    delete(strcat('Bandit_withrest/',ids{sub}, '.mat'))  %get rid of superfluous .mat files that are hanging around for some reason.
    
    %output b statistics (now corrected eprimeread)
    error_struct = b.sub_proc.errors;
    error_struct = rmfield(error_struct, {'before', 'after'});
    btmp = rmfield(b.sub_proc, {'delta_index', 'errors', 'counts_to_first_C', 'b'});
    btmp = horzcat(struct2table(btmp), struct2table(error_struct));
    btmp.id = repmat(ids(sub), size(btmp,1),1);
    writetable(btmp, sprintf('%s/b_outputs/b_subject%s.csv', so.output_dir, ids{sub}));
    
    y_all{sub} = y;
    u_all{sub} = u;
    b_all{sub} = b;
    
    %hard coding some things here so that mh_vba extractor functions work (rather than reimplement the code base into the new framework)
    options.inF.hidden_states = 4; %three Q values plus PE tagalong state
    options.inF.state_names = {'QA', 'QB', 'QC', 'PE'};
    
    options.inF.n_outputs = 3; %three choices (multinomial)
    options.inF.y_names = {'pA', 'pB', 'pC'}; %three choices (multinomial)
    
    options.inF.n_theta = 3;
    options.inF.theta_names = {'alpha_win', 'alpha_loss', 'decay'};
    
    options.inF.n_phi = 3;
    options.inF.phi_names = {'beta'};
    
    options.inF.evo_fname = @f_bandit_Qlearn;
    options.inF.obs_fname = @g_bandit_softmax;
    
    options.inF.model = so.model;
    options.inF.dataset = so.dataset;
    
    options_all{sub} = options; % Pass subject options into group structure
    
end

% add the n_t vector to dim before passing to MFX
dim.n_t=n_t;
dim.n_s=length(y_all);

% options for MFX.. this all comes from MH. Priors may need to be tweaked slightly
options_group.TolFun=1e-2;
options_group.MaxIter=50;
options_group.DisplayWin=1;
options_group.verbose=1;

% priors_group.muPhi = 0; %temperature -- exp(phi(1))
% priors_group.SigmaPhi = 10; %variance on temperature (before exponential transform)
% 
% priors_group.muTheta = zeros(dim.n_theta,1); %valenced learning rate (alpha_win, alpha_lose), decay (lambda) -- before logistic transform
% priors_group.SigmaTheta =  1e1*eye(dim.n_theta); %variance of 10 on alpha and gamma
% 
% priors_group.muX0 = zeros(dim.n,1);
% priors_group.SigmaX0 = zeros(dim.n, dim.n);

priors_group = options_all{1}.priors; %use first subject as representative of group priors on phi, theta, and X0

priors_group.a_vX0 = Inf([1, dim.n]); %use infinite precision prior on gamma for X0 to treat as fixed (a = Inf; b = 0)
priors_group.b_vX0 = zeros([1, dim.n]);

[p_sub, o_sub, p_group, o_group] = VBA_MFX_parallel(y_all, u_all, @f_bandit_Qlearn, @g_bandit_softmax, dim, options_all, priors_group, options_group);
%[p_sub, o_sub, p_group, o_group] = VBA_MFX(y_all, u_all, @f_bandit_Qlearn, @g_bandit_softmax, dim, options_all, priors_group, options_group);

delete(gcp('nocreate'));

%populate subject ids into output.options.inF structure since these are added to subject statistics below
for s=1:length(o_sub)
    o_sub{s}.options.inF.id = ids{s};
    o_sub{s}.options.inG.id = ids{s};

    %add bandit trial-level and global model-free statistics
    o_sub{s} = add_model_statistics(o_sub{s}, b_all{s});
    
    %populate ffx parameters from o_group structure to p_sub structure for extraction
    p_sub{s}.ffx = o_group.initVBA.p_sub{s};
    p_sub{s}.ffx = rmfield(p_sub{s}.ffx, {'SigmaX', 'iQx', 'iQy'}); %large matrices not needed for anything (they use lots of disk space)
end

%create a structure with just the barebones useful parameters from subjects
s_all = cellfun(@(p, o) extract_subject_statistics(p, o), p_sub, o_sub, 'UniformOutput', false);

%output MFX results

[group_global, group_trial_level] = extract_group_statistics(s_all, ...
    sprintf('%s/%s_%s_mfx_bandit_global_statistics.csv', so.output_dir, so.dataset, so.model), ...
    sprintf('%s/%s_%s_mfx_bandit_trial_outputs.csv', so.output_dir, so.dataset, so.model));

%save group outputs
save(sprintf('%s/group_fits_%s_%s', so.output_dir, so.model, so.dataset), 'ids', 'so', 's_all', 'group_global', 'group_trial_level');

%too huge to save into one .mat file
save([so.output_dir, '/', so.dataset, '_', so.model, '_vba_mfx_results_psub.mat'], 'p_sub', '-v7.3');
save([so.output_dir, '/', so.dataset, '_', so.model, '_vba_mfx_results_pgroup.mat'], 'p_group', '-v7.3');
save([so.output_dir, '/', so.dataset, '_', so.model, '_vba_mfx_results_ogroup.mat'], 'o_group', '-v7.3');
save([so.output_dir, '/', so.dataset, '_', so.model, '_vba_mfx_results_osub.mat'], 'o_sub', '-v7.3');
save([so.output_dir, '/', so.dataset, '_', so.model, '_vba_mfx_results_settings.mat'], 'priors_group', 'options_group', 'y_all', 'u_all', 'b_all', 'ids', '-v7.3');

%free energy (log evidence)
L = o_group.within_fit.F;

% save just the log evidence, L
save([so.output_dir, '/', so.dataset, '_', so.model, '_vba_mfx_L.mat'], 'L', '-v7.3');





