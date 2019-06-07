function ball=bandit_rl_model(ball,params,plt)
%% This function is to calculate value from subjects choices given a set
%of parameters alpha lambda(win,loss) and inverse temperature constant Beta

%Take care of initial params
if  numel(params) < 5
    disp('Not enough parameters defaulting to generic values')
    alpha = 0.95;
    lambda_win = 0.3;
    lambda_loss = 0.1;
    beta = 10;
    epsilon = 0.4;
else
    alpha = params(1);
    lambda_win = params(2);
    lambda_loss = params(3);
    beta = params(4);
    epsilon = params(5);
end

str =sprintf('Parameters values Alpha = %.2f, Lambda Win = %.2f, Lambda Loss = %.2f, Beta = %.2f, Epsilon = %.2f',...
    alpha, lambda_win, lambda_loss, beta, epsilon);
disp(str);

%To plot or not to plot
plot_flag =plt;


%% Load  in data if ball doesn't exisit
% try
%     load('c:\kod\Neuropsych_preproc\matlab\analysis\bandit\data\bandit_data.mat')
% catch
%     disp('Can''t find bandit data, please locate the bandit data file')
%     [FileName,PathName,FilterIndex] = ...
%         uigetfile('*.mat','Choose a file');
%     load([PathName FileName]);
% end

%Load design file
design_struct = bandit_tablet_load_design;

%% Set up params
trial_length = length(ball.behav(1,1).choice);
vt_1=0.*ones(1,trial_length);
vt_2=0.*ones(1,trial_length);
vt_3=0.*ones(1,trial_length);

vt_1(1)=0;
vt_2(1)=0;
vt_2(1)=0;

Pr1=zeros(1,trial_length);
Pr2=zeros(1,trial_length);
Pr3=zeros(1,trial_length);
delta=zeros(1,trial_length);

gamma = 0.99; % decay param for expected value when not chosen
zeta = gamma; %increment param for uncertainty
min_u = -10; % how low uncertainty is allowed to go 

%%
%Grab subjects choices and run update equations
for i = 1:length(ball.id) %subject Loop
    choice = ball.behav(1,i).choice_numeric;
    r = ball.behav(1,i).stim_ACC;
    choice_hist(:,i) = choice;
    r_hist(:,i) = r;
    best_choice = nan(1,trial_length)';
    best_choice_empty = nan(1,trial_length)';
    best_choice_value = nan(1,trial_length)';
    best_choice_uvsum = nan(1,trial_length)';
    lose_switch = zeros(1,trial_length)';
    subj_model_predicted = zeros(1,trial_length)';
    
    %Init expected with init value set to 0
    vt_1=0.*ones(1,trial_length);
    vt_2=0.*ones(1,trial_length);
    vt_3=0.*ones(1,trial_length);
    
    %Init uncertainty with init value as 0
    ut_1 = zeros(1,trial_length)';
    ut_2 = zeros(1,trial_length)';
    ut_3 = zeros(1,trial_length)';
    ut_1(1) = 1;
    ut_2(1) = 1;
    ut_3(1) = 1;
    
        
    %Init uncertainty with init value as 0
    uv_sum1 = zeros(1,trial_length)';
    uv_sum2 = zeros(1,trial_length)';
    uv_sum3 = zeros(1,trial_length)';
    
    for j = 2:trial_length %Begin trial Loop
        
        
        %Did I win?
        if r(j)>0
            lambda = lambda_win;
        else
            lambda = lambda_loss;
        end
        %v_t(j) = alpha*v_t(j-1) + lambda*(r(j) - v_t(j-1));
        
        %Calculate expected value EV(t) = ALPHA*EV(t-1) + LAMBDA*DELTA
        if choice(j)==1
            %Calculate the value and uncertainties based on subj's choice
            [vt_1, vt_2, vt_3, ut_1, ut_2, ut_3]=calc_ev_and_uv(vt_1,...
                vt_2, vt_3, ut_1, ut_2, ut_3,delta,alpha,gamma,lambda,zeta,r,j);
        elseif choice(j) ==2
            [vt_2, vt_1, vt_3, ut_2, ut_1, ut_3]=calc_ev_and_uv(vt_2,...
                vt_1, vt_3, ut_2, ut_1, ut_3,delta,alpha,gamma,lambda,zeta,r,j);
        elseif choice(j) ==3
            [vt_3, vt_1, vt_2, ut_3, ut_1, ut_2]=calc_ev_and_uv(vt_3,...
                vt_1, vt_2, ut_3, ut_1, ut_2,delta,alpha,gamma,lambda,zeta,r,j);
        else
            %values
            vt_1(j) = vt_1(j-1);
            vt_2(j) = vt_2(j-1);
            vt_3(j) = vt_3(j-1);
            %uncertainity
            ut_1(j) = ut_1(j-1);
            ut_2(j) = ut_2(j-1);
            ut_3(j) = ut_3(j-1);
            %PE
            delta(j) = 0;
        end
        
        %% calculate the uvSums
        uv_sum1(j) = (1-epsilon)*vt_1(j) + epsilon*ut_1(j);
        uv_sum2(j) = (1-epsilon)*vt_2(j) + epsilon*ut_2(j);
        uv_sum3(j) = (1-epsilon)*vt_3(j) + epsilon*ut_3(j);
        
        %% calculate probability of chosing a given stimulus
        Pr1=exp(beta.*(vt_1))./(exp(beta.*(vt_1))+exp(beta.*(vt_2))+ exp(beta.*(vt_3)));
        Pr2=exp(beta.*(vt_2))./(exp(beta.*(vt_1))+exp(beta.*(vt_2))+ exp(beta.*(vt_3)));
        Pr3=exp(beta.*(vt_3))./(exp(beta.*(vt_1))+exp(beta.*(vt_2))+ exp(beta.*(vt_3)));
        
        
        
        %determine probablistically best choice
        best_choice_Pr = find_best_choice(best_choice_empty,Pr1,Pr2,Pr3,1);
        
        %determine value based best choice with 0.x cutoff
        best_choice_value = find_best_choice(best_choice_empty,vt_1, vt_2, vt_3,1);
        
        %determine UVsum based best choice
        best_choice_uvsum = find_best_choice(best_choice_empty, uv_sum1, uv_sum2, uv_sum3,1);
                
        %Temporary best choice until I add if statement or switch later
        best_choice = best_choice_uvsum;
        
        
        %Grab choices so they are easier to read
        current_choice = choice(j);
        prev_choice = choice(j-1);
        
        %So if at t-1 subj recieved an error while choice(-1) = the best choice
        %(according to the model) and the on the subsequent trial the subject
        %recieved an error again, r(t)=0, and switched their choice, while the
        %previous best choice was still the same, we have a lose switch error.
        if r(j-1)==0 && prev_choice == best_choice(j-1);
            if current_choice ~= prev_choice && best_choice(j) == best_choice(j-1) %&& best_choice_value(j)>0
                lose_switch(j) = 1;
            end
        end
        
        
    end %End trial Loop
    
    %     scaled_uv_sum1 = (uv_sum1-min(uv_sum1(:))) ./ (max(uv_sum1(:)-min(uv_sum1(:))));
    %     scaled_uv_sum2 = (uv_sum2-min(uv_sum2(:))) ./ (max(uv_sum2(:)-min(uv_sum2(:))));
    %     scaled_uv_sum3 = (uv_sum3-min(uv_sum3(:))) ./ (max(uv_sum3(:)-min(uv_sum3(:))));
    
    tmp = uv_sum1-min(uv_sum1);
    norm_uv_sum1 = tmp/norm(tmp,1);
    
    tmp = uv_sum2-min(uv_sum2);
    norm_uv_sum2 = tmp/norm(tmp,1);
    
    tmp = uv_sum3-min(uv_sum3);
    norm_uv_sum3 = tmp/norm(tmp,1);
    
    uv_sum = [uv_sum1 uv_sum2 uv_sum3];
    
 
    
    if plot_flag==1
        %plot uv_sums
        figure(99)
        clf
        plot(uv_sum1)
        %plot(scaled_uv_sum1)
        %plot(norm_uv_sum1)
        hold on
        plot(uv_sum2, 'r')
        %plot(scaled_uv_sum2, 'r')
        %plot(norm_uv_sum2, 'r')
        hold on
        plot(uv_sum3, 'g')
        %plot(scaled_uv_sum3, 'g')
        %plot(norm_uv_sum3, 'g')
        hold off
        legend('stim-1', 'stim-2', 'stim-3',...
            'Location','Best');
        title('UV sums')
        
        %plot u's
        figure(199)
        clf
        plot(ut_1)
        hold on
        plot(ut_2, 'r')
        hold on
        plot(ut_3, 'g')
        hold off
        legend('stim-1', 'stim-2', 'stim-3',...
            'Location','Best');
        title('Uncertainties')
    end
    
    %Pr1=exp(beta.*(v_t1))./(exp(beta.*(v_t1))+exp(beta.*(v_t2))+ exp(beta.*(v_t3)));
    %Pr2=exp(beta.*(v_t2))./(exp(beta.*(v_t1))+exp(beta.*(v_t2))+ exp(beta.*(v_t3)));
    %Pr3=exp(beta.*(v_t3))./(exp(beta.*(v_t1))+exp(beta.*(v_t2))+ exp(beta.*(v_t3)));
        
    %Best choice based on model probabilities, value, or uvSum compared to
    %subjects choices
    %subj_model_predicted = (best_choice_value==choice);    
    subj_model_predicted = (best_choice==choice);    
    
    %% calculate expected value for CHOSEN stimulus (not for model fitting)
    %find the trials where sub chose each stimulus
    chose1=(choice==1);
    chose2=(choice==2);
    chose3=(choice==3);
    echosen=[vt_1.*chose1'+vt_2.*chose2'+vt_3.*chose3']';
    etotal=vt_1+vt_2+vt_3;
    
        %% Did subject actually choose the highest probable stimulus
    smoothed_A = smooth(design_struct.Arew,20);
    smoothed_B = smooth(design_struct.Brew,20);
    smoothed_C = smooth(design_struct.Crew,20);
%     prA = smoothed_A./(smoothed_A + smoothed_B +smoothed_C);
%     prB = smoothed_B./(smoothed_A + smoothed_B +smoothed_C);
%     prC = smoothed_C./(smoothed_A + smoothed_B +smoothed_C);
best_choice_design = find_best_choice(best_choice_empty,smoothed_A,smoothed_B, smoothed_C,3);
%So I made these 'codes' to account for the situations in which the
%probability of choosing stimuli is equal
for o = 1:length(best_choice_design)
    if best_choice_design(o) == 997 && ismember(choice(o),[1,2])
        correct_choice(o,1) = 1;
    elseif best_choice_design(o) == 998 && ismember(choice(o),[2,3])
        correct_choice(o,1) = 1;
    elseif best_choice_design(o) == 9989 && ismember(choice(o),[1,3])
        correct_choice(o,1) = 1;
    elseif best_choice_design(o)==choice(o)
        correct_choice(o,1) = 1;
    else
        correct_choice(o,1) = 0;
    end
end


correct_choice_prereversal = sum(correct_choice(1:trial_length/2));
correct_choice_postreversal = sum(correct_choice((trial_length/2 +1):end));
correct_choice = sum(correct_choice);
    
    %% Save each meteric in subject's behavorial struct
    ball.behav(1,i).v_t = [vt_1' vt_2' vt_3'];
    ball.behav(1,i).delta = delta';
    ball.behav(1,i).Prs = [Pr1' Pr2' Pr3'];
    ball.behav(1,i).lose_switch = lose_switch;
    ball.behav(1,i).echosen = echosen;
    ball.behav(1,i).subj_model_predicted = subj_model_predicted;
    ball.behav(1,i).correct_choice = correct_choice;
    ball.behav(1,i).correct_choice_prereversal = correct_choice_prereversal;
    ball.behav(1,i).correct_choice_postreversal = correct_choice_postreversal;

    
    
    
    %% Plot some subjects QC check
%     figure(17)
%     clf
    if plot_flag == 1
        
        figure(55)
        clf
        plot(smooth(subj_model_predicted), 'LineWidth',10)
        title('Did subject pick the highest model predicted choice?')
        
        figure(70)
        clf
        smoothie = 20;
        subplot(3,1,1)
        plot(smooth(design_struct.Arew,smoothie), 'r--','LineWidth',2);
        hold on
        plot(smooth(chose1,smoothie), 'b', 'LineWidth',2);
        plot(smooth(vt_1,smoothie), 'k','LineWidth',2);
        %plot(smooth((best_choice==1),smoothie), 'm','LineWidth',2);
        plot(smooth(uv_sum1,smoothie), 'g','LineWidth',2);
        axis([0 300 0 1.1])
        title(['Arew vs EV A ' num2str(ball.id(i))]);
        subplot(3,1,2)
        plot(smooth(design_struct.Brew,smoothie), 'r--','LineWidth',2);
        hold on
        plot(smooth(chose2,smoothie), 'b', 'LineWidth',2);
        plot(smooth(vt_2,smoothie), 'k','LineWidth',2);
        %plot(smooth((best_choice==2),smoothie), 'm','LineWidth',2);
        plot(smooth(uv_sum2,smoothie), 'g','LineWidth',2);
        axis([0 300 0 1.1])
        title('Brew vs EV B');
        subplot(3,1,3)
        plot(smooth(design_struct.Crew,smoothie), 'r--','LineWidth',2);
        hold on
        plot(smooth(chose3,smoothie), 'b', 'LineWidth',2);
        plot(smooth(vt_3,smoothie), 'k','LineWidth',2);
        %plot(smooth((best_choice==3),smoothie), 'm','LineWidth',2);
        plot(smooth(uv_sum3,smoothie), 'g','LineWidth',2);
        axis([0 300 0 1.1])
        title('Crew vs EV C');
        
        input('Press ENTER to continue...');
        
    end
    
end %end Subject Loop
tmp=0;


function best_choice=find_best_choice(best_choice,x,y,z,gr)
if gr==1
    best_choice(x> y & x> z)=1;
    best_choice(y> x & y> z)=2;
    best_choice(z> y & z> x)=3;
elseif gr==3
    best_choice(x> y & x> z)=1;
    best_choice(y> x & y> z)=2;
    best_choice(z> y & z> x)=3;
    %Codes for the cases in which the probabilites are the same
    best_choice(x==y & y~=0) = 997;
    best_choice(y==z & y~=0) = 998;
    best_choice(x==z & z~=0) = 999;
else
    best_choice(x< y & x< z)=1;
    best_choice(y< x & y< z)=2;
    best_choice(z< y & z< x)=3;
end
return

function [a,b,c,x,y,z,dlt]=calc_ev_and_uv(a,b,c,x,y,z,dlt,al,gam,lmb,zta,r,j)
%Value
a(j) = al*a(j-1) + lmb*(r(j) - a(j-1));
b(j) = gam.*b(j-1);
c(j) = gam.*c(j-1);
%Uncertainty
x(j) = (al)*(x(j-1));
y(j) = (y(j-1));
z(j) = (z(j-1));
%PE
dlt(j) = r(j) - a(j-1);
return

%% Old code:
%       Old code might be useful if we nede to tweak anything later
%determine probablistically best choice
%         best_choice(Pr1> Pr2 & Pr1> Pr3)=1;
%         best_choice(Pr2> Pr1 & Pr2> Pr3)=2;
%         best_choice(Pr3> Pr2 & Pr3> Pr1)=3;
        
        %determine value based best choice with 0.x cutoff 
        %best_choice_value(vt_1> vt_2 & vt_1> vt_3 & vt_1> 0.5)=1;
%         best_choice_value(vt_1> vt_2 & vt_1> vt_3)=1;
%         best_choice_value(vt_2> vt_1 & vt_2> vt_3)=2;
%         best_choice_value(vt_3> vt_2 & vt_3> vt_1)=3;
        
        %determine UVsum based best choice
%         best_choice_uvsum(uv_sum1< uv_sum2 & uv_sum1< uv_sum3)=1;
%         best_choice_uvsum(uv_sum2< uv_sum1 & uv_sum2< uv_sum3)=2;
%         best_choice_uvsum(uv_sum3< uv_sum2 & uv_sum3< uv_sum1)=3;

%             v1(ct)=AL*v1(ct)+ AlphaWin.*(r(ct)-v1(ct-1));
%             v2(ct)=LossDecay.*v2(ct);
%             v3(ct)=LossDecay.*v3(ct);
%             delta(ct)=r(ct)-e1(ct);
%
%
%
%
%
