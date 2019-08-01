function [options, dim, priors] = bandit_vba(id,graphics,plot_subject,save_results,parameterization,dir_str, no_mri)

% fits BANDIT rl model to 3 armed bandit subject data using VBA toolbox
%
close all

valence = parameterization.valence;
fix_decay = parameterization.fix_decay; %The logic is fixed
utility = parameterization.utility;
disappointment = parameterization.disappointment;
regret = parameterization.regret;
fix_all_params = parameterization.fix_all_params ;
use_reward_vec = parameterization.use_reward_vec;

%If we only want to use the first 150 trials
use_first_150 = 0;


%% Where to look for data
%Quick username check, and path setting

if save_results
    file_path = 'vba_output';
else
    fprintf('You are not saving the data!\n\n')
end

%Evolution options
options.inF.utility = 0;
options.inF.valence = 0;
options.inF.decay = 0;


%Turn graphics on or off
if ~graphics
    options.DisplayWin = 0;
    options.GnFigs = 0;
end
%% set up dim defaults
if valence && ~disappointment
    n_theta = 3; %Number of evolution params (AlphaWin AlphaLoss Beta)
    options.inF.valence = 1;
    options.inF.disappointment= 0;

elseif valence && disappointment
    n_theta = 4; %Number of evolution params (AlphaWin AlphaLoss Beta Omega)
    options.inF.valence = 1;
    options.inF.disappointment= 1;

else
    n_theta = 2;
end

if utility
    n_theta = n_theta +1; %Add in steepness parameter
    options.inF.utility = 1;
end

if fix_decay
    n_theta = n_theta-1;
    options.inF.fix_decay = 1;
else
    options.inF.fix_decay = 0;
end

n_phi = 1; %Number of observation params (Beta)
f_name = @f_bandit_Qlearn; %Evolution function
g_name = @g_bandit_softmax; %Observation function
n_t = 300; %Total number of trials
% n_runs = 3; %3 blocks total
n_hidden_states = 4; %Track value for each arm of the bandit + PE

%% Fixed parameters		
 if fix_all_params				
     n_theta=0;		
     n_phi=0;		
     options.inF.fixed_params=1;		
     options.inG.fixed_params=1;		
 else		
     options.inF.fixed_params=0;		
     options.inG.fixed_params=0;		
 end


%% Load in the subject's data
%u is 2 x ntrials where first row is actions and second row is reward
b = bandit_vba_read_in_data( 'id',id,'data_dir',dir_str); %REPLACE subjects with local dir
b.id = id;
censor = b.chosen_stim==999; %Censor some trials first
subjects_actions = b.chosen_stim;
subjects_actions(censor)=nan;


if no_mri
%%override subject actions in behavioral setup. unclear why but the
%%eprimeread script botches this, and I've rerun chosen actions and
%%outcomes into R. 

subjects_actions = textread(sprintf('%s/%d/choices.txt', dir_str, id), '%f');
b.chosen_stim = subjects_actions;
%actions_R = type (sprintf('%s/%d/choices.txt', dir_str, id))
end


u(1,:) = subjects_actions; %Chosen action [1 2 3]
if use_reward_vec
    u(2,:) = b.rewardVec; %Reward has actual value [10 25 50]
    u(3,:) = b.stakeVec; %Stake 
else
    u(2,:) = textread(sprintf('%s/%d/outcomes.txt', dir_str, id), '%f');
    %u(2,:) = b.stim_ACC; %Reward or not [1 0]
    u(3,:) = NaN;
end
u = [zeros(size(u,1),1) u(:,1:end-1)]; %Shift the u!

%Only use the first 150 trials
if use_first_150==1
   n_t = n_t/2; %Should take care of the y
   u = u(:,1:n_t);
   censor = censor(1:n_t);
end




y = zeros(3, n_t);
for i = 1:n_t
    try
        y(subjects_actions(i), i) = 1;
    catch
        y(:,i) = nan;
    end
end

%% set up models within evolution/observation Fx
options.inF.b = b;
options.inG.b = b;

%% skip first trial
options.skipf = zeros(1,n_t);
options.skipf(1) = 1;

%options.binomial = 1;

%% split into conditions/runs
% if multisession %improves fits moderately
%     options.multisession.split = repmat(n_t/n_runs,1,n_runs); % three runs of 100 datapoints each
%     %% fix parameters
%     if fixed_params_across_runs
%         options.multisession.fixed.theta = 'all';
%         options.multisession.fixed.phi = 'all';
%         %
%         % allow unique initial values for each run?x
%         options.multisession.fixed.X0 = 'all';
%     end
%
% end

%% defined number of hidden states and parameters
dim = struct('n',n_hidden_states,'n_theta',n_theta,'n_phi',n_phi, 'n_t', n_t);

%% priors
priors.muPhi = zeros(dim.n_phi,1);
priors.muTheta = zeros(dim.n_theta,1);

if utility
    priors.muTheta(end) = -2;
end

priors.muX0 = zeros(n_hidden_states,1);
priors.SigmaTheta = 1e1*eye(dim.n_theta);
%priors.SigmaPhi = diag([1,1,1]);
priors.SigmaPhi = 1e1*eye(dim.n_phi);
priors.SigmaX0 = 0*eye(dim.n);
priors.a_alpha = Inf;
priors.b_alpha = 0;
priors.a_sigma = 1;     % Jeffrey's prior
priors.b_sigma = 1;     % Jeffrey's prior

options.priors = priors;
%options.inG.priors = priors; %copy priors into inG for parameter transformation (e.g., Gaussian -> uniform)

%% Last bit of option declarations
options.TolFun = 1e-6;
options.GnTolFun = 1e-6;
options.verbose=1;

%Censor any bad trials
options.isYout = repmat(censor,1,3)';
options.inF.Yout = options.isYout;


%%set to multinomial, remove binomial

options.sources(1) = struct('out', 1:3, 'type', 2); %choice is multinomial (with no response)
options.inF.decay = 1;


%% Run the vba model
% [posterior,out] = VBA_NLStateSpaceModel(y,u,f_name,g_name,dim,options);
return