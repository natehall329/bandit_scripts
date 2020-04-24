% load data
load('data/bandit_data.mat');

% preallocate mem. for error counts
ps_error_count = nan(length(ball),1);

% iterate through structure (still don't know how to do this quicker
for n = 1:lenght(ball)
    ps_error_count(n) = sum(ball.behav(n).errors.perseverative);
end