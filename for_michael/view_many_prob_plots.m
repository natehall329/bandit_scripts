%function view_many_prob_plots(n_of_plots)
% 2013-07-23 Jan Kalkus
%   A simply function for viewing many plots of stimulus choice (or the probability 
%   of reward for a given stimulus). 

% set number of plots
n_of_plots = 25;

% load to get all ids
load('data/bandit_data.mat');

square_dim = ceil(sqrt(n_of_plots));
dim_i = round(sqrt(n_of_plots));
dim_j = ceil(sqrt(n_of_plots));

% allocate mem. for axes handles
h = zeros(n_of_plots,1);

for n = 1:n_of_plots
    
    id = ball.id(26+n);
    
    x = bandit_sub_proc(id);
    h(n) = subplot(dim_i,dim_j,n);
    sf = 15; % smoothing factor
    schoice_mat = [smooth(x.achoice,sf) smooth(x.bchoice,sf) smooth(x.cchoice,sf)];
    plot(schoice_mat); hold on; plot(150,1,'ko'); hold off;
    
    %plot(x.prob); % plot reward probability
    title(sprintf('%g',id));
    
end

% apply axes parameters for aesthetic touches
set(h,'XLim',[0 300],'YLim',[0 1.1],'FontSize',8);
