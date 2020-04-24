function q = bandit_proc( varargin )

%updates to account for hallquist subjects
%PArse out the optional arguments
p = inputParser;
defaultNonUPMCSubjs = false;
addParameter(p,'nonUPMC',defaultNonUPMCSubjs,@islogical)
parse(p,varargin{:})

[data_dir, numlist,alt_ids] = whichDataPathToUse(p);


% % processes 3-armed bandit data on all subjects
% out_path = [pathroot 'analysis/bandit/data/']; % set data path
% 
% % create list of subjects defined by directory names
% numlist = num_scan(dir([out_path 'raw/']));

% % run single-subject proc script on each
for sub=1:length(numlist)
    
    fprintf('processing id: %6d\t\t',numlist(sub));

	% load subject's data
    s = bandit_sub_proc(numlist(sub), varargin{:}, data_dir );
    
    % print some general error counts info
    fprintf('error counts: PS = %3d, SS = %3d, PE = %3d\n', ...
        sum(s.errors.prob_switch_err), ...
        sum(s.errors.spont_switch_err), ...
        sum(s.errors.perseverative) ...
    );


    ball.id(sub,1) = numlist(sub);
    % the [bellow/deleted] is redundant; we can get the same result using
    % the stored function handle and subject by subject data:
    %
    %     x = ball.fx.choice_to_stimID([ball.behav.choice]);
    %
    % this output is not in a logical/binary format as
    % ball.[a-c]choice were, but it is easity converted to such

	ball.behav(sub).choice     = s.stim_choice;
    ball.behav(sub).choice_numeric = s.stim_choice_numeric;
    if(eq(sub,1)) % only need to do this once
        % function handle converts ball.behav.choice from 'char' to 'int'
        ball.fx.choice_to_stimID = @(c) cast(c,'double')-64;
    end
    ball.behav(sub).bestchoice           = s.best_choice;
    ball.behav(sub).errors.spont         = s.errors.spont_switch_err;
    ball.behav(sub).errors.erratic_spont = s.errors.erratic_spont;
    ball.behav(sub).errors.prob          = s.errors.prob_switch_err;
    ball.behav(sub).errors.perseverative = s.errors.perseverative;
    ball.behav(sub).errors.explore_sw    = s.errors.explore_switch;
    
    % save when subject responded correctly
    ball.behav(sub).stim_ACC = s.showstim_ACC;

    % errors split into before and after reversal
    ball.behav(sub).errors.before = s.errors.before;
    ball.behav(sub).errors.after  = s.errors.after;
    
    % counts to first stim C choice after reversal
    ball.behav(sub).count_to_first_C = s.counts_to_first_C;
    
    % technical specs
    ball.behav(sub).RT              = s.showstim_RT;
    ball.behav(sub).chosen_position = s.chosen_position;
    
    % additional measures
    ball.behav(sub).above_chance_diff = s.above_chance_diff;
    ball.behav(sub).delta_index       = s.delta_index;

    % TODO: add routine for 'last_updated' vs. 'last_checked'
	ball.last_updated = datestr(now,'yyyy-mm-dd HH:MM:SS');

end

% enter descriptive field info
% -- not done yet --

%Change ids if needed
if ~isempty(alt_ids)
    ball.alt_ids = alt_ids;
end

% save it
save([data_dir 'bandit_data'],'ball');

% varargout
if(nargout), q = ball; end

return


function num_out = num_scan(data_in)

num_out = zeros(length(data_in),1);

for n = 1:length(data_in) %index_array %3:(length(A))-2
    num_out(n) = str2double(data_in(n).name); 
end

q_nan = isnan(num_out);
num_out = num_out(~q_nan);

return

function [out_path,numlist,alt_ids]=whichDataPathToUse(p)

%Default value
alt_ids = '';

%Use either default or custom data path
if p.Results.nonUPMC==1
    out_path=[uigetdir(pwd,'Please selcet where the bandit data is') filesep];
    %Grab the actual ePrime files by is in .txt file
    eprime_files=glob([out_path '\*\*.txt']);
    expression = '-([0-9]{2,5})-';
    numlist = cellfun(@(x) regexp(x,expression,'match'),eprime_files);
    numlist = str2double(cellfun(@(x) x(2:end-1), numlist,'UniformOutput',0));    
    
    %Grab the SPECC id, or subject specific id, make the strings part of vararagin 
    ids=glob([out_path '*_[a-z][a-z]']);
    expression = '([0-9]{3,5}_[aA-zZ]{2})';
    alt_ids = cellfun(@(x) regexp(x,expression,'match'),ids,'UniformOutput',0); 
    
else
    
    %processes 3-armed bandit data on all subjects
    out_path = [pathroot 'analysis/bandit/data/']; % set data path
    
    %create list of subjects defined by directory names
    numlist = num_scan(dir([out_path 'raw/']));
    
end