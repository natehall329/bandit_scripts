% send some bandit data to a SPSS importable format file
%
% currently this only outputs error data from the bandit task
%
% Jan Kalkus
% 26 Dec 2012

load([pathroot 'analysis/bandit/data/bandit_data.mat']); %correct

%load(['C:\Users\wilsonj3\Desktop\Hallquist Bandit Data\Bandit Data\bandit_data.mat']); %hall
%load([pathroot 'analysis/bandit/data/PersevExp/bandit_data-ps_thr_0.3.mat']);
%load([pathroot 'analysis/bandit/data/PersevExp/bandit_data-ps_thr_0.5.mat']);
%load([pathroot 'analysis/bandit/data/PersevExp/bandit_data-ps_thr_0.8.mat']);

% open file pointer and print out headers
%fid = fopen([pathroot 'analysis/bandit/data/bandit2spss.dat'],'w');
%fid = fopen([pathroot 'analysis/bandit/data/bandit2spss_p0.3.dat'],'w');
%fid = fopen([pathroot 'analysis/bandit/data/bandit2spss_p0.5.dat'],'w');

fid = fopen([pathroot 'analysis/bandit/data/bandit2spss_p0.8.dat'],'w'); %correct

%fid = fopen('C:\Users\wilsonj3\Desktop\Hallquist Bandit Data\BanditData\bandit2spss_hallquist.dat','w'); %hall

fprintf(fid,'ID\tprob_switch_err\tspont_switch_err\tpersev_err\tpercent_corr\t');
fprintf(fid,'before_prob_sw\tbefore_spont_sw\tbefore_persev\tbefore_percent_corr\t');
fprintf(fid,'after_prob_sw\tafter_spont_sw\tafter_persev\tafter_percent_corr\t');
fprintf(fid,'sampling_index\t');
fprintf(fid,'exploratory_switch_err\t');
fprintf(fid,'erratic_spont\tdelta_index\n');

% percentage correct
f_percent_correct = @(p) 100*sum(ball.behav(p).bestchoice)/numel(ball.behav(p).bestchoice);

for nsubj = 1:numel(ball.id)
    
    % errors as usual
    pse  = sum(ball.behav(nsubj).errors.prob);
    sse  = sum(ball.behav(nsubj).errors.spont);
    per  = sum(ball.behav(nsubj).errors.perseverative);
    pcor = f_percent_correct(nsubj);
    
    % exploratory switch errors
    expl = nansum(ball.behav(nsubj).errors.explore_sw);
    erratic_spont = nansum(ball.behav(nsubj).errors.erratic_spont);
    
    % delta index
    delta_index = ball.behav(nsubj).delta_index;
    
    % split half errors (before and after reversal)
    before_pse = sum(ball.behav(nsubj).errors.before.prob_switch_err);
    before_sse = sum(ball.behav(nsubj).errors.before.spont_switch_err);
    before_per = sum(ball.behav(nsubj).errors.before.perseverative);
    
    after_pse  = sum(ball.behav(nsubj).errors.after.prob_switch_err);
    after_sse  = sum(ball.behav(nsubj).errors.after.spont_switch_err);
    after_per  = sum(ball.behav(nsubj).errors.after.perseverative);
    
    % percent correct
    before_pcor = 100*sum(ball.behav(nsubj).bestchoice(1:150)/numel(ball.behav(nsubj).bestchoice(1:150)));
    after_pcor  = 100*sum(ball.behav(nsubj).bestchoice(151:end)/numel(ball.behav(nsubj).bestchoice(151:end)));
    
    
    % write to file
    fprintf(fid,'%d\t%d\t%d\t%d\t%g\t',ball.id(nsubj),pse,sse,per,pcor); %Correct
    %fprintf(fid,'%s\t%d\t%d\t%d\t%g\t',upper(ball.id{nsubj}{:}),pse,sse,per,pcor); %hall
    fprintf(fid,'%d\t%d\t%d\t%g\t',before_pse,before_sse,before_per,before_pcor);
    fprintf(fid,'%d\t%d\t%d\t%g\t',after_pse,after_sse,after_per,after_pcor);
    fprintf(fid,'%d\t',ball.behav(nsubj).count_to_first_C);
    fprintf(fid,'%d\t',expl);
    fprintf(fid,'%d\t%g\n',erratic_spont,delta_index);
    
end

% kill the pointer
fclose(fid);
