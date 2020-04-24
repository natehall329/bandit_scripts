% Sometimes, even if the previous stimulus choice had the highest probability
% of reward, it might not appear to be a good choice. 
%
% Jan Kalkus
% 2014-02-01

%   - previous trial was different from current stimulus
%   - did the previous stimulus choice have the highest reward probability


function error_count_sanity_check(id)

if(nargin < 1), return; end

epoch_window = 150:250;


file_path = @(s) [pathroot 'analysis/bandit/data/' sprintf('%d.mat',s)];
load(file_path(id));

% set plot values
n = 8 + 1;
n=n-1; choice_A = n * b.achoice(epoch_window); choice_A(choice_A == 0) = nan;
n=n-1; choice_B = n * b.bchoice(epoch_window); choice_B(choice_B == 0) = nan;
n=n-1; choice_C = n * b.cchoice(epoch_window); choice_C(choice_C == 0) = nan;

n=n-1; rew = n * b.showstim_ACC(epoch_window); rew(rew == 0) = nan;

n=n-1; prob_switch    = n * b.errors.prob_switch_err(epoch_window); prob_switch(prob_switch == 0) = nan;
n=n-1; spont_switch   = n * b.errors.spont_switch_err(epoch_window); spont_switch(spont_switch == 0) = nan;
n=n-1; erratic_switch = n * b.errors.erratic_spont(epoch_window); erratic_switch(erratic_switch == 0) = nan;

n=n-1; persev_err = n * b.errors.perseverative(epoch_window); persev_err(persev_err == 0) = nan;





% alt plot data
figure('PaperPositionMode','auto','PaperOrientation','landscape', ...
    'Position',[120 231 1171 589]); %[120 350 1480 470]);

y1_data = [choice_A choice_B choice_C rew prob_switch spont_switch erratic_switch persev_err];

b.prob(isnan(b.prob)) = 0; % first trial has no sampling history
w = 5; % smoothing window
y2_data = filtfilt(ones(1,w)/w,1,b.prob(epoch_window,:)); % smooth with zero-phase filter
%y2_data = b.prob(epoch_window,:);
[ax,h1,~] = plotyy(epoch_window,y1_data,epoch_window,y2_data);

set(h1(1:3),'LineStyle','o','LineWidth',3);
set(ax(:),'TickDir','in','XLim',[min(epoch_window) max(epoch_window)]);

set(h1(4:end),'MarkerEdgeColor','k');
set(h1(4),'LineStyle','.','MarkerSize',18);
set(h1(5),'LineStyle','v');
set(h1(6),'LineStyle','>');
set(h1(7),'LineStyle','<');
set(h1(8),'LineStyle','+');

set(ax(1),'YTick',[],'YLim',[0 10]);
set(ax(2),'YTick',(0:0.2:1),'YLim',[0 1]);
xlabel('Trial number');
title(sprintf('Subject n^o: %d',id));

set(get(ax(2),'Ylabel'),'String','Probability of Reward')

legend('A choice','B choice','C choice','Reward received', ...
    'Prob. Sw. Err.','Spont. Sw. Err.','Erratic Sw. Err.','Persev. Err.'); %, ...
    %'p(rew|A)','p(rew|B)','p(rew|C)');


return

%%

%ids = [115105 207769 113961 207686 207351];
ids = [210591 215697 881071 881096 881137]; % p(rew) < 0.8

arrayfun(@error_count_sanity_check,ids);
