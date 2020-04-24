% load data
load('data/bandit_data.mat');

% preallocate mem. for error counts
ps_error_count = nan(length(ball.behav),1);
correct_count  = ps_error_count;

% iterate through structure (still don't know how to do this quicker
for n = 1:length(ball.behav)
    ps_error_count(n) = sum(ball.behav(n).errors.perseverative);
    correct_count(n)  = sum(ball.behav(n).bestchoice);
end

% plot hist(s)
subplot(1,2,1);
hist(correct_count,60)
title('Distribution of correct trials');
xlabel('Number of correct trials');
ylabel('Frequency');

subplot(1,2,2);
hist(ps_error_count,40);
title('Distribution of perseverative errors');
xlabel('Number of perseverative errors');
ylabel('Frequency');