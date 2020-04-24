function [out_sub] = add_model_statistics(out_sub, b)
%accepts an out structure from VBA

bad_trials = find(isnan(out_sub.u(1,:))); %nan response on a trial indicates failure
winning_trials = find((b.stim_ACC==1)' & ~ismember(1:out_sub.dim.n_t,bad_trials-1));
losing_trials = find((b.stim_ACC==0)' & ~ismember(1:out_sub.dim.n_t,bad_trials-1));

%Separate the hidden states
Qvalues = out_sub.suffStat.muX(1:3,:); %also lives in posterior.muX (these are identical)
delta = out_sub.suffStat.muX(4,:);

%Left shift PE by 1 since delta is the PE of the *previous* trial, but for trial-level analysis, we want to enforce trial alignment
out_sub.additional.trial.delta = leftShift(delta);

%SHOULD THESE CHANGE?
%Values are shifted to the left add on as many zeros as needed to regain proper length
muX_diff = [diff(Qvalues,1,2) zeros(size(Qvalues,1),1)];
PEunsigned = max(abs(muX_diff));

%PEunsigned = [PEunsigned 0]; %Tack on zero to the end?
PEplus = zeros(1,length(b.stim_ACC));
PEminus = zeros(1,length(b.stim_ACC));

out_sub.additional.trial.PEchosen_pos = zeros(1,length(b.stim_ACC));
out_sub.additional.trial.PEchosen_neg = zeros(1,length(b.stim_ACC));
PEplus(winning_trials) = PEunsigned(winning_trials); %Good!
PEminus(losing_trials) = PEunsigned(losing_trials); %Out of bounds because losing trials contains index 300
out_sub.additional.trial.PEunsigned=PEunsigned;
out_sub.additional.trial.PEplus=PEplus;
out_sub.additional.trial.PEminus=PEminus;
out_sub.additional.trial.PEsigned=PEplus-PEminus;

%Grab the option they chose and remove any error codes
chosen_index = out_sub.y;
chosen_index = carryValueForward(chosen_index,out_sub.y); %If there are any Nan's replace them with the most recent decision made

%PEchosen is now the pe hidden state
out_sub.additional.trial.PEchosen = out_sub.additional.trial.delta; %PE of chosen choice
out_sub.additional.trial.PEchosen_pos(out_sub.additional.trial.PEchosen > 0) = out_sub.additional.trial.PEchosen(out_sub.additional.trial.PEchosen > 0);
out_sub.additional.trial.PEchosen_neg(out_sub.additional.trial.PEchosen < 0) = out_sub.additional.trial.PEchosen(out_sub.additional.trial.PEchosen < 0);

%Create Value regressors
out_sub.additional.trial.value = max(Qvalues); %Max value of each hidden state per trial
out_sub.additional.trial.value_diff = out_sub.additional.trial.value - mean(Qvalues);
out_sub.additional.trial.value_chosen = Qvalues(logical(chosen_index))';
out_sub.additional.trial.value_not_chosen=Qvalues(~logical(chosen_index))';
out_sub.additional.trial.value_not_chosen = reshape(out_sub.additional.trial.value_not_chosen,2,300);
%not_chosen_sum=sum(out_sub.additional.trial.value_not_chosen); %Keep this var on ice for now
out_sub.additional.trial.value_chosen_diff = out_sub.additional.trial.value_chosen - mean(out_sub.additional.trial.value_not_chosen); %Do not shift this one!

%Create different flavor of value difference regressor in which v(t+1) is
%indexed by chosen_index, (i.e. Vtplus1(chosen_index)) and subtracted by
%the mean(Vtplus1(~chosen_index)))
out_sub.additional.vtplus1 = Qvalues(:,2:end); %currently n-1 in size, so not promoting to .trial
out_sub.additional.vtplus1_not_chosen=out_sub.additional.vtplus1(~logical(chosen_index(:,1:end-1)))';
out_sub.additional.vtplus1_not_chosen = reshape(out_sub.additional.vtplus1_not_chosen,2,length(out_sub.additional.vtplus1));
out_sub.additional.vtplus1_chosen_diff=[(out_sub.additional.vtplus1(logical(chosen_index(:,1:end-1)))' - mean(out_sub.additional.vtplus1_not_chosen)) 0];

%Create a regrssor like vtplus1 but using the max subtracted by the median - V.B.
out_sub.additional.vtplus1_max=[(max(out_sub.additional.vtplus1) - median(out_sub.additional.vtplus1)) 0];

%Value chosen was normalized at one point?
%out_sub.additional.trial.value_chosen=out_sub.additional.trial.value_chosen./sum(Qvalues);

%Shift all the value regressors by 1
out_sub.additional.trial.value=leftShift(out_sub.additional.trial.value);
out_sub.additional.trial.value_diff=leftShift(out_sub.additional.trial.value_diff);
out_sub.additional.trial.value_chosen=leftShift(out_sub.additional.trial.value_chosen);
out_sub.additional.trial.value_not_chosen=leftShift(out_sub.additional.trial.value_not_chosen);
%out_sub.additional.trial.value_chosen=leftShift(out_sub.additional.trial.value_chosen);
%out_sub.additional.trial.value_chosen(1) = 0; %This is a NAN since 0/0

%Standardize the PEChosen and ValueChosenDiff regs

%zscore is only in stat toolbox?
try
    out_sub.additional.trial.value_chosen_diff_standardized = ...
        zscore(out_sub.additional.trial.value_chosen_diff);
    out_sub.additional.trial.PEchosen_standardized = ...
        zscore(out_sub.additional.trial.PEchosen );
catch
    fprintf('Stat toolbox not found!\n\n')
end

%Create reward stake and just stake align it with trialonset
out_sub.additional.reward_stake = b.rewardVec';
out_sub.additional.reward_stake(b.rewardVec==10)=1;
out_sub.additional.reward_stake(b.rewardVec==25)=2;
out_sub.additional.reward_stake(b.rewardVec==50)=3;

%mean corrected rew mag
out_sub.additional.reward_stake_mc = out_sub.additional.reward_stake - mean(out_sub.additional.reward_stake);

%This is what they could have won per trial <- what we want for createing the probabilities
out_sub.additional.trial.stake = b.stakeVec';
out_sub.additional.trial.stake(b.stakeVec==10)=1;
out_sub.additional.trial.stake(b.stakeVec==25)=2;
out_sub.additional.trial.stake(b.stakeVec==50)=3;

%Mean corrected stake regressor
out_sub.additional.trial.stake_mc = out_sub.additional.trial.stake - mean(out_sub.additional.trial.stake);


%Percentages of reward magnitude and staying
out_sub.additional.mag10_trials = find(b.stakeVec==10);
out_sub.additional.mag25_trials = find(b.stakeVec==25);
out_sub.additional.mag50_trials = find(b.stakeVec==50);

%Determine number of trials with specific magnitude that were win/loss
out_sub.additional.win_10_trials = intersect(winning_trials,out_sub.additional.mag10_trials);
out_sub.additional.win_25_trials = intersect(winning_trials,out_sub.additional.mag25_trials);
out_sub.additional.win_50_trials = intersect(winning_trials,out_sub.additional.mag50_trials);
out_sub.additional.loss_10_trials = intersect(losing_trials,out_sub.additional.mag10_trials);
out_sub.additional.loss_25_trials = intersect(losing_trials,out_sub.additional.mag25_trials);
out_sub.additional.loss_50_trials = intersect(losing_trials,out_sub.additional.mag50_trials);

%Find stay trials
error_code = 999; %If they missed a trial, ie don't want two zeros in a row to i...
out_sub.additional.stay_trials  = find(logical([0; b.chosen_stim(2:end)==b.chosen_stim(1:end-1)]) & b.chosen_stim~=error_code);

%These aren;t really nedded but its good to have a breakdown...
out_sub.additional.stay_10_trials = intersect(out_sub.additional.stay_trials,out_sub.additional.mag10_trials);
out_sub.additional.stay_25_trials = intersect(out_sub.additional.stay_trials,out_sub.additional.mag25_trials);
out_sub.additional.stay_50_trials = intersect(out_sub.additional.stay_trials,out_sub.additional.mag50_trials);

%Stay prob example (number of win 10 rew mag trials which subj stayed / number of win 10 rew mag trials)
out_sub.additional.global.win_stay_10_prob = length(intersect(out_sub.additional.win_10_trials,out_sub.additional.stay_trials))./length(out_sub.additional.win_10_trials);
out_sub.additional.global.win_stay_25_prob = length(intersect(out_sub.additional.win_25_trials,out_sub.additional.stay_trials))./length(out_sub.additional.win_25_trials);
out_sub.additional.global.win_stay_50_prob = length(intersect(out_sub.additional.win_50_trials,out_sub.additional.stay_trials))./length(out_sub.additional.win_50_trials);
out_sub.additional.global.loss_stay_10_prob = length(intersect(out_sub.additional.loss_10_trials,out_sub.additional.stay_trials))./length(out_sub.additional.loss_10_trials);
out_sub.additional.global.loss_stay_25_prob = length(intersect(out_sub.additional.loss_25_trials,out_sub.additional.stay_trials))./length(out_sub.additional.loss_25_trials);
out_sub.additional.global.loss_stay_50_prob = length(intersect(out_sub.additional.loss_50_trials,out_sub.additional.stay_trials))./length(out_sub.additional.loss_50_trials);

    function x = carryValueForward(x,y)
        %Remove all NANs and push chosen index forward
        
        %Pesky indexing, if nan at 1 set it to [1 0 0]'
        if sum(isnan(y(:,1)))>0
            x(:,1) = [1 0 0]';
            y(:,1) = [1 0 0]';
        end
        
        x(isnan(y)) = x(find(isnan(y))-size(y,1));
        if numel(find(isnan(x)))>0
            x = carryValueForward(x,y);
        end
    end


    function x=leftShift(x)
        %Shift the reg data by 1 to the left
        x = [x(:,2:end) zeros(size(x,1),1)];
    end

end

