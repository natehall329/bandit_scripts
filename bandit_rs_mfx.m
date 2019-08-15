%% bandit MFX script: adapted from https://github.com/PennStateDEPENdLab/spott_modeling/blob/master/vba_timeseries/fit_group_vba_mfx.m

%loads in subjects' data and fits bandit data using Q-learning model from Dombrovski et al 2019 using VBA;
close all; clear;

dir_str='Bandit_withrest';
% dir_str='subjects';
dirs=dir(dir_str);


addpath(genpath('~/Documents/MATLAB/VBA-toolbox-master/'))
%addpath('~/Documents/MATLAB/VBA-toolbox-master/utils')
addpath('vba/')
addpath('behav_scripts/')
addpath('~/Box/temporal_instrumental_agent')
addpath('~/Box/spott_modeling/vba_timeseries/') %for toying with MH's options, priors etc


if ~exist('vba_output','dir')
    mkdir('vba_output');
end

%Loading in the subjects that still need processed
%load('fMRI_ids_to_run_vba_on.mat')

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
    


ids = {dirs(4:length(dirs)-1).name};

dirs = dirs(4:length(dirs)-1); %for input to Alex's script.

% % Note: this function looks for 'sceptic_dataset' and 'sceptic_model'
% % as environment variables so that this script can be scaled easily for batch processing
% 
% so = []; %you can set custom sceptic options here that will override the function below.
% so = sceptic_validate_options(so); %initialize and validate sceptic fitting settings
% % 
% %setup paths, parallel pools, etc. based on user environment
% [so, poolobj, behavfiles] = sceptic_setup_environment(so);

%extract IDs for record keeping
% [~,fnames]=cellfun(@fileparts, behavfiles, 'UniformOutput', false);
% ids=cellfun(@(x) char(regexp(x,'(?<=MEG_|fMRIEmoClock_)[\d_]+(?=_tc|_concat)','match')), fnames, 'UniformOutput', false);

%puts a nested cell in each element
%ids=regexp(fnames,'(?<=MEG_|fMRIEmoClock_)[\d_]+(?=_tc|_concat)','match'); %use lookahead and lookbehind to make id more flexible (e.g., 128_1)

ns = length(ids);
y_all = cell(ns, 1);
u_all = cell(ns, 1);
options_all = cell(ns, 1);

n_t=NaN(1,ns);

% get relevant vars from bandit script, but dont run NLstatespace


% [options,dim,priors] = bandit_vba_setup(id,graphics,plot_subject,save_results,parameterization,dir_str, no_mri);


for sub = 1:ns
    
    fprintf('Loading subject %d id: %s \n', sub, ids{sub});
    id = str2double(ids{sub});
    
    [options,dim,priors] = bandit_vba_setup(id,graphics,plot_subject,save_results,parameterization,dir_str, no_mri);

    
%     [data, y, u] = sceptic_get_data(behavfiles{sub}, so);
%   [options, dim] = sceptic_get_vba_options(data, so);
%     n_t(sub) = dim.n_t; % allow for variation in number of trials across subjects
%     


    % populate data structures for VBA_MFX
    y_all{sub} = textread(sprintf('%s/%d/choices.txt', dir_str, id), '%f');
    u_all{sub} = textread(sprintf('%s/%d/outcomes.txt', dir_str, id), '%f');;
    options_all{sub} = options; %%N.B. probably going to have to come
%     back to this. 

end


%options for MFX.. this all comes from MH. Priors may need to be tweaked
%slightly
options_group.TolFun=1e-2;
options_group.MaxIter=50;
options_group.DisplayWin=0;
options_group.verbose=1;

priors_group.muPhi = 0; %temperature -- exp(phi(1))
priors_group.SigmaPhi = 10; %variance on temperature (before exponential transform)

priors_group.muTheta = zeros(dim.n_theta,1); %valenced learning rate (alpha_win, alpha_lose), decay (lambda) -- before logistic transform
priors_group.SigmaTheta =  1e1*eye(dim.n_theta); %variance of 10 on alpha and gamma

priors_group.muX0 = zeros(dim.n,1);
priors_group.SigmaX0 = zeros(dim.n, dim.n);

priors_group.a_vX0 = Inf([1, dim.n]); %use infinite precision prior on gamma for X0 to treat as fixed (a = Inf; b = 0)
priors_group.b_vX0 = zeros([1, dim.n]);


% priors_group.muX0 = zeros(so.nbasis*so.hidden_states,1); %have PE and decay as tag-along states
% priors_group.SigmaX0 = zeros(so.nbasis*so.hidden_states, so.nbasis*so.hidden_states); %have PE and decay as tag-along states
% 
% priors_group.a_vX0 = Inf([1, so.nbasis*so.hidden_states]); %use infinite precision prior on gamma for X0 to treat as fixed (a = Inf; b = 0)
% priors_group.b_vX0 = zeros([1, so.nbasis*so.hidden_states]);

%[p_sub, o_sub, p_group, o_group] = VBA_MFX_parallel(y_all, u_all, @f_bandit_Qlearn, @g_bandit_softmax, dim, options_all, priors_group, options_group);
[p_sub, o_sub, p_group, o_group] = VBA_MFX(y_all, u_all, @f_bandit_Qlearn, @g_bandit_softmax, dim, options_all, priors_group, options_group);
%[p_sub, o_sub, p_group, o_group] = VBA_MFX(y_all, u_all, @h_sceptic_fixed_decay_fmri, @g_sceptic, dim, options_all, priors_group, options_group);

delete(poolobj);

%populate subject ids into output.options.inF structure since these are added to subject statistics below
for s=1:length(o_sub)
    o_sub{s}.options.inF.id = ids{s};
    o_sub{s}.options.inG.id = ids{s};
    
    %populate ffx parameters from o_group structure to p_sub structure for extraction
    p_sub{s}.ffx = o_group.initVBA.p_sub{s};
    p_sub{s}.ffx = rmfield(p_sub{s}.ffx, {'SigmaX', 'iQx', 'iQy'}); %large matrices not needed for anything (they use lots of disk space)
end

%create a structure with just the barebones useful parameters from subjects
s_all = cellfun(@(p, o) extract_subject_statistics(p, o), p_sub, o_sub, 'UniformOutput', false);

%output MFX results

[group_global, group_trial_level] = extract_group_statistics(s_all, ...
    sprintf('%s/%s_%s_mfx_sceptic_global_statistics.csv', so.output_dir, so.dataset, so.model), ...
    sprintf('%s/%s_%s_mfx_sceptic_trial_outputs_by_timestep.csv', so.output_dir, so.dataset, so.model));

%save the basis as a csv file
vnames = cellfun(@(x) strcat('Time_', num2str(x)), num2cell(1:so.ntimesteps), 'UniformOutput', false);
basis_mat = array2table(o_sub{1}.options.inF.gaussmat, 'VariableNames', vnames);

writetable(basis_mat, sprintf('%s/%s_%s_mfx_sceptic_basis.csv', so.output_dir, so.dataset, so.model));

%save group outputs
save(sprintf('%s/group_fits_%s_%s', so.output_dir, so.model, so.dataset), 'ids', 'so', 's_all', 'group_global', 'group_trial_level');

%too huge to save into one .mat file
save([so.output_dir, '/', so.dataset, '_', so.model, '_vba_mfx_results_psub.mat'], 'p_sub', '-v7.3');
save([so.output_dir, '/', so.dataset, '_', so.model, '_vba_mfx_results_pgroup.mat'], 'p_group', '-v7.3');
save([so.output_dir, '/', so.dataset, '_', so.model, '_vba_mfx_results_ogroup.mat'], 'o_group', '-v7.3');
save([so.output_dir, '/', so.dataset, '_', so.model, '_vba_mfx_results_osub.mat'], 'o_sub', '-v7.3');
save([so.output_dir, '/', so.dataset, '_', so.model, '_vba_mfx_results_settings.mat'], 'priors_group', 'options_group', 'y_all', 'u_all', 'ids', '-v7.3');

%free energy (log evidence)
L = o_group.within_fit.F;

% save just the log evidence, L
save([so.output_dir, '/', so.dataset, '_', so.model, '_vba_mfx_L.mat'], 'L', '-v7.3');





