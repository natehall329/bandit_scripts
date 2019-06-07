% 
%
% Jan Kalkus
% 13 Nov 2012

function b = bandit_sub_proc(id, varargin )
% process behavioral data from variable-schedule 3-armed bandit task
% more details at: http://bit.ly/HvBdby

% parse optional 'varargin' arguments
fig_flag = false;
alt_data_path = false;
if(any(strcmpi('figure',varargin))), fig_flag = true; end



%  --  get the data  --  %
get_vars = {'showstim.RESP','showstim.RT','showstim.ACC'};
[b, data_out_path] = getData(id,get_vars,varargin{:});

% recode chosen position numerically, counterclockwise 1=top, 2=left, 3=right
% there are multiple versions of this task which have differing output,
% we need to search for matches for both old and new versions of the task. 
% OR with SRBOX 4=left 5=top 6=right
choice_right = strcmp(b.showstim_RESP,'right') | strcmp(b.showstim_RESP,'{RIGHTARROW}') | isequal(b.showstim_RESP,6);%b.showstim_RESP==6;
choice_left  = strcmp(b.showstim_RESP,'left')  | strcmp(b.showstim_RESP,'{LEFTARROW}')  | isequal(b.showstim_RESP,4);%b.showstim_RESP==4;
choice_top   = strcmp(b.showstim_RESP,'top')   | strcmp(b.showstim_RESP,'{UPARROW}')    | isequal(b.showstim_RESP,5);%b.showstim_RESP==5;

if(any(strcmp('nonUPMC',varargin)))
    choice_right = b.showstim_RESP==6;
    choice_left  = b.showstim_RESP==4;
    choice_top   = b.showstim_RESP==5;
end

% code responses of chosen position
b.chosen_position = (choice_top + (2*choice_left) + (3*choice_right));


%  --  get our design file  --  %
% I'm pretty sure this is all in the E-prime output file as well,
% it would be a good idea to update this in the future to just
% pull this data from each file, I believe this is the same as the file
% named crdt-sched-vrbl072911.
load([pathroot 'db/bandit/designwithaposition.mat'], ...
    'aposition','bposition','cposition', ...
    'arew','brew','crew');



%  --  find probabiliy of reward  --  % 
% stimulus with objectively highest reward probability, based on a 
% 10-trial (last 10 samples of that stimulus) moving average 

% translate position chosen to stimulus chosen
b.achoice = ( b.chosen_position == aposition );
b.bchoice = ( b.chosen_position == bposition );
b.cchoice = ( b.chosen_position == cposition );
b.stim_choice = char('A'*b.achoice + 'B'*b.bchoice + 'C'*b.cchoice); 
%A=1 B=2 C=3
b.stim_choice_numeric = (b.achoice + 2*b.bchoice + 3*b.cchoice);

% calculate probability of reward based on last 10 trials of the
% stimulus. the average is based only on trials in which a given
% stimulus was chosen (hence indexing by b.[x]choice).
n_prev_trials = 10;

%prob.a = obs_prob(arew,b.achoice,n_prev_trials);
%prob.b = obs_prob(brew,b.bchoice,n_prev_trials);
%prob.c = obs_prob(crew,b.cchoice,n_prev_trials);
prob.a = obs_prob(double(b.showstim_ACC & b.achoice),[],n_prev_trials);
prob.b = obs_prob(double(b.showstim_ACC & b.bchoice),[],n_prev_trials);
prob.c = obs_prob(double(b.showstim_ACC & b.cchoice),[],n_prev_trials);


b.prob = [prob.a prob.b prob.c]; prob = b.prob;

% did the subject make the best choice possible; did the subject
% choose the stimulus which, based upon the sampling history, had
% the highest probability of reward?
b.best_choice = zeros(size(prob,1),1); % pre-alloc. (with a struct?)
for row_i = 1:size(prob,1)
    % this method is more thorough; it is possible to have more
    % than one "good choice" (i.e., maximum probability option)
    % as some choices have the same (highest) probability. the
    % previous method forced one option, potentially falsely
    % coding correct/good choices as bad ones. 
	tmp = find(prob(row_i,:) == nanmax(prob(row_i,:)));
    picked_stim = stimChar2Num(b.stim_choice(row_i));
    b.best_choice(row_i,1) = any( tmp == picked_stim );
end

% TODO:
% - trials to an 8-trial criterion (may relax) based on b.goodchoice
% - what (see above)!?

% additional measures
b.above_chance_diff = aboveChanceDelta(b.prob,[arew,brew,crew]);
b.delta_index = windowedDeltaIndex(b.prob,b.stim_choice,'A',[160 200]);

% parse and code errors subject made
b.errors = errorParser(b.stim_choice,b.best_choice,b.showstim_ACC,b.prob);
b.errors.perseverative = persevErrProc(b.stim_choice,b.best_choice,b.prob,b.showstim_ACC);

% first half (before reversal)
b.errors.before = ...
    errorParser(b.stim_choice(1:150),b.best_choice(1:150),b.showstim_ACC(1:150),b.prob(1:150,:));
b.errors.before.perseverative = ...
    persevErrProc(b.stim_choice(1:150),b.best_choice(1:150),b.prob(1:150,:),b.showstim_ACC(1:150));

% second half (after reversal)
b.errors.after = ...
    errorParser(b.stim_choice(151:end),b.best_choice(151:end),b.showstim_ACC(151:end),b.prob(151:end,:));
b.errors.after.perseverative = ...
    persevErrProc(b.stim_choice(151:end),b.best_choice(151:end),b.prob(151:end,:),b.showstim_ACC(151:end));

% count the number of trials after the reversal until the first stim C is chosen
b.counts_to_first_C = countTrialsToStim(b.stim_choice(151:end),'C');

% save individual file
if ~exist(data_out_path,'dir')
    mkdir(data_out_path);
end
save([data_out_path sprintf('/%d.mat',id)],'b');

% plots if we want them
if(fig_flag)
 	showGraphs(id,prob,n_prev_trials,b,b.errors.perseverative);
    if(any(b.errors.perseverative))
        keyboard
    end
end

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
function stim_id_num = stimChar2Num(stim_char)
% converts stimulus character to stimulus number (e.g., 'A' --> 1,
% 'B' --> 2, 'C' --> 3, etc.)

stim_id_num = cast(stim_char,'double')-64;

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
function [xout, fout] = getData(id,vars,varargin)
% reads eprime data for a given bandit subject based on ID
alt_path = false;
if(any(strcmp('nonUPMC',varargin))), alt_path = true; end

if ~alt_path
    % Find the eprime file
    data_dir  = [pathroot 'analysis/bandit/data/raw/'];
    file_name = ls([data_dir sprintf('%d/*.txt',id)]);
    tmp       = sprintf('analysis/bandit/data/raw/%d/%s',id,file_name);
    fpath     = @(~) [pathroot tmp];
    fout      = 'data';
else
    file_path = glob([varargin{3} '*\' sprintf('*%d*.txt',id)]);
    fpath     = @(~) file_path{:};
    fout      = [varargin{3} 'processed_data'];
end
    
% read in the data
xout = eprimeread(fpath(),'trialproc',vars,0,-10,10); 

% put ID in structure and make it the first field
xout.id = id;
reorderstructure(xout,'id');

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
function ob_out = obs_prob(rew,sampled_stim,binsize)
% calculate sliding average on only observed trials. a stimulus'
% probability remains the same when it is not chosen; 
%
% if a stimulus has not yet been sampled, its probability of
% reward is set to NaN. this might be changed to 0, but at the
% moment setting it as NaN is informative. 

if(isempty(sampled_stim)), sampled_stim = ones(size(rew)); end

ob_out = nan(size(sampled_stim)); ob_out(1) = NaN; % no trials before first one
rew(~sampled_stim) = NaN; % filter out unsampled stimuli

for qtrial = 2:length(ob_out)
    % find reward status of up to 'binsize' previous trials (exclude NaN's)
    foo = find(~isnan(rew(1:qtrial-1)),binsize,'last');
    ob_out(qtrial) = mean(rew(foo)); % store it in the main output var.
end

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
function [chance_delta,stim_delta] = aboveChanceDelta(observed_prob,reward_matrix)
% this function calculates the difference between the subject's
% performance and a completely random performance
%
% not sure if this is ever even used

chance_performance = sum(reward_matrix,2)/size(reward_matrix,2);
chance_delta = max(observed_prob,[],2) - chance_performance;

if(nargout > 1)
    stim_delta = nan(size(reward_matrix));
    for n_stim = 1:size(reward_matrix,2)
        stim_delta(:,n_stim) = observed_prob(:,n_stim) - chance_performance;
    end
end

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
function delta_index = windowedDeltaIndex(stim_reward_prob,stim_choice,stim,epoch_window)
% this function calculates an index defined by the following equation
% 
%       delta_index = nA_choice - pA_rew
% 
%       where: 
%           nA_choice: number of A choices within 'epoch_window'
%              pA_rew: reward probability for A choices based on
%                      on a 10-trial moving average


% check input arguments
if(~exist('epoch_window','var') || isempty(epoch_window))
    qb = 1; 
    qe = size(stim_reward_prob,1);
else
    qb = epoch_window(1); 
    qe = epoch_window(2);
end

% we prefer numbers to characters
if(ischar(stim)), stim = stimChar2Num(stim); end

% get stimulus choice counts within the specified epoch
stim_count = sum(arrayfun(@stimChar2Num,stim_choice(qb:qe)) == stim);

% calculate and output 
delta_index = stim_count - mean(stim_reward_prob(qb:qe,stim));

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
function errstruct = errorParser(stim_choice,best_choice,reward,prob)
% searches for probabilistic and spontaneous switch errors.
% perseverative erros are delt with in another subfunction as
% they are more complex to clasify and identify. 

% pre-allocate memory
prob_switch          = false(size(stim_choice));
spont_switch         = false(size(stim_choice));
erratic_spont_switch = false(size(stim_choice));
noncat_err           = false(size(stim_choice));
explore_switch       = nan(size(stim_choice));

% calculate running correct response count
count = 0; running_sum = zeros(size(stim_choice));
for n = 1:length(stim_choice)
    if(best_choice(n) == 1)
        count = count+1;
        running_sum(n) = count;
    else
        count = 0;
    end
end

% find trial with errors
q_error = ( reward == false );

for error_n = find(q_error');
    
    if(error_n > 1) % no trials before the first one

        % get current and prior trial stimuli
        this_stim = stim_choice(error_n);
        prev_stim = stim_choice(error_n-1);

        % was the previous stimulus the same as the current one?
        if(ne(this_stim,prev_stim))
            if(q_error(error_n-1)) % prev. trial not rewarded: prob. switch
                % did the previous choice have the highest reward probability
                max_prob_stim_id = find(prob(error_n-1,:) == max(prob(error_n-1,:)));
                if(any(max_prob_stim_id == stimChar2Num(prev_stim)));
                    % then this was a probabilistic switch error
                    prob_switch(error_n) = true;
                else
                    noncat_err(error_n) = false;
                end
            else % spontaneous switch
                spont_switch(error_n) = true;
                
                % did the previous choice have the highest reward probability
                max_prob_stim_id = find(prob(error_n-1,:) == max(prob(error_n-1,:)));
                if(any(max_prob_stim_id == stimChar2Num(prev_stim)))
                    % if so, it is an ERRATIC SPONTANEOUS SWITCH
                    erratic_spont_switch(error_n) = true;
                end
                
                % was this switched somehow exploratory; does this stimulus
                % still have a reward probability greated than chance?
                stim_id = stimChar2Num(this_stim);
                if(stim_id > 0) % make sure subject didn't forget to respond
                    if(prob(error_n,stim_id) > (1/3))

                        % subject probably made the switch because in the past
                        % it delivered reward consistenly enough when chosen
                        explore_switch(error_n) = 1;
                    else
                        explore_switch(error_n) = 0;
                    end
                end
            end
        else
            noncat_err(error_n) = true;
        end
    end
end

% store errors in data structure
errstruct.prob_switch_err  = prob_switch;
errstruct.spont_switch_err = spont_switch;
errstruct.erratic_spont    = erratic_spont_switch;
errstruct.explore_switch   = explore_switch;
if(any(noncat_err))
    errstruct.error_NOS = noncat_err;
end

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
function persev_errs = persevErrProc(stim_choice,best_choice,reward_prob_array,reward)
% process errors to find when subject made a perseverative error

persev_errs = false(size(best_choice)); % pre-alloc.

% subjects must choose the best stimulus (based on their prior
% sampling) at least 'learning_criterion' times in a row to meet 
% the learning criterion.
%
% we are skipping the learning criterion because of low occurrence 
% of runs of AAAA and such
learning_criterion = 0;

% subjects must also choose the same stimulus without any reward 
% more than 'persev_criterion' times for an error to be considered 
% a perseverative error.
persever_criterion = 2;

% threshold below which subjects could make what we will define
% as perseverative errors. the thinking behind this is that if the 
% probability of reward (based on the subject's sampling and reward
% history) for a given stimulus is large enough, it makes sense to
% keep choosing that stimulus. 
%probability_threshold = 0.8;
probability_threshold = 1/3; 
%probability_threshold = 0.5;


% get running total of correct answers 
running_count = runningCount(best_choice);
learn_crit_array = ( running_count >= learning_criterion );

% for each error (defined as a trial for which the subject did not
% receive reward), determine if it was a perseverative error
for error_n = find(reshape(~reward,1,length(best_choice)))
    
    if(error_n > 1) % otherwise, it's beginner's luck
            
        % find choice outcome(s) since last best choice. 'best choice'
        % is defined as the stimulus with the highest reward probability. 
        %last_best_choice = find(best_choice(1:error_n-1),1,'last');
        
        % redefined 'best_choice':
        %   1) define a window that is a stretch of N stimulus choices
        %      that are identical to the current stimulus choice
        q_train_start = find(stim_choice(1:error_n) ~= stim_choice(error_n),1,'last') + 1;
        q_window = q_train_start:error_n;
        %   2) if the first choice in this window was a 'best_choice', 
        %      mark that index as 'last_best_choice'; do not continuously
        %      update the index even if subsequent choices still have the 
        %      highest probability of reward
        last_best_choice = find(best_choice(q_window),1,'first') + (q_train_start - 1);
        
        if(~isempty(last_best_choice)) % may not be any at first
            
            % are all stim since last best choice the same type?
            if(numel(unique(stim_choice(last_best_choice:error_n))) == 1)

                % is the probability of reward for chosing this
                % stimulus below the threshold? (i.e., there is
                % not sufficient reason for the subject to continue 
                % choosing this stimulus)
                stim_id = stimChar2Num(stim_choice(error_n));
                if(reward_prob_array(error_n,stim_id) < probability_threshold)

                    % 1) had the learning criterion been met upon making the 
                    %    last correct choice?
                    learn_crit_met = ( learn_crit_array(last_best_choice) );
                    % 2) has the subject chosen this stimulus despite receiving
                    %    no reward more than 'persev_criterion' times?
                    persev_crit_met = ( (error_n - last_best_choice) >= persever_criterion );

                    if(learn_crit_met && persev_crit_met)
                        % yes: that's a perseverative error
                        persev_errs(error_n) = true;
                    end
                end                
            end
        end
    end
end     

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
function nc = countTrialsToStim(stim_choice,stim_char)

nc = find(stim_choice == stim_char,1,'first');

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
function c = runningCount(best_choice)
% calculate running correct response count

count = 0; % initialize
c = zeros(size(best_choice)); % pre-allocate

for n = 1:length(c)
    if(best_choice(n))
        count = count+1;
        c(n) = count;
    else
        count = 0;
    end
end

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
function showGraphs(id,prob,n_prev_trials,b,persev_error)
% just plot some data

best_choice = b.best_choice;

fh = figure(1); set(fh,'position',[185 244 1372 710]);
h(1) = subplot(3,1,1); s = 6; 
plot(smooth(prob(:,1),s),'g'); hold on;
plot(smooth(prob(:,2),s),'r');
plot(smooth(prob(:,3),s),'b'); hold off; 

ylabel(sprintf('P(reward | last %d trials)',n_prev_trials));
title(sprintf('Probability of reward (subject n^o %d)',id));

sm = 10;
h(2) = subplot(3,1,2);
plot(smooth(b.achoice, sm),'g'); hold on;
plot(smooth(b.bchoice, sm),'r'); 
plot(smooth(b.cchoice, sm),'b'); 
plot(persev_error/2,'k');
hold off; 

ylabel('probability of chosing stimulus (?)');
title(sprintf('Stimulus choice (subject n^o %d)',id));
legend('stim A','stim B','stim C');

h(3) = subplot(3,1,3);
plot(smooth(best_choice,sm));
ylabel('Correct choices'); 
title(sprintf('Performance (subject n^o %d)',id));

set(h,'XLim',[1 300],'YLim',[0 1]);

return
