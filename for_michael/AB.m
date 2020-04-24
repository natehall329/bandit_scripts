%What is the influence of past_choices*current_reward on current choice?
%What is the influence of past_choices*past_reward on current choice?

%Assesses "spread of effect":
%current reward reinforcing earlier and future behaviors

function s=AB(id)
%cd M:\PRL\modelcode\f


b=revmakethreeregressors(id);
% goal: estimate: W, theta from reversal learning data

%BELOW IS OLD CODE FOR BEHAVIORAL DATA
%fname=sprintf('%d.rev',id);
%dat=dlmread(fname,' ',2);
%dat=textread(fname,'%d','delimiter',' ','whitespace',' '); % had whitespace be \n
%dat=subj;
%dat=load('209661.dat');
%s.trial=dat(:,1);
s.RT=b.RT;
s.onset=b.onset;
s.NumRight=b.NumRight;
s.Running=b.Running;
s.showstim_RESP=b.showstim_RESP;
s.showstim_ACC=b.showstim_ACC;
s.firstfive=b.firstfive;
s.lastfivegoodtrials=b.lastfivegoodtrials;
s.responsetrials=b.responsetrials;
s.switch=b.switch;
s.switchnum=b.switchnum;
s.pswitch=b.pswitch;
s.pswitchnum=b.pswitchnum;

s.pstay=b.pstay;
s.pstaynum=b.pstaynum;

s.sswitch=b.sswitch;
s.sswitchnum=b.sswitchnum;


s.percentcorrect=b.percentcorrect;
s.persev=b.persev;
s.persevnum=b.persevnum;
s.persevcum=b.persevcum;
s.blockspassed=b.blockspassed;
s.firstfive=b.firstfive;
s.lastfivegoodtrials=b.lastfivegoodtrials;
if exist('b.switchnum1','var')==1 && exist('b.switchnum2','var')==1 && exist('b.switchnum3','var')==1
    s.persevnum1=b.persevnum1;
    s.persevnum2=b.persevnum2;
    s.persevnum3=b.persevnum3;
    s.switchnum1=b.switchnum1;
    s.switchnum2=b.switchnum2;
    s.switchnum3=b.switchnum3;
    s.percentcorrect1=b.percentcorrect1;
    s.percentcorrect2=b.percentcorrect2;
    s.percentcorrect3=b.percentcorrect3;
    s.pswitchnum1=b.pswitchnum1;
    s.pswitchnum2=b.pswitchnum2;
    s.pswitchnum3=b.pswitchnum3;
    
    s.sswitchnum1=b.sswitchnum1;
    s.sswitchnum2=b.sswitchnum2;
    s.sswitchnum3=b.sswitchnum3;
    
    s.pstaynum1=b.pstaynum1;
    s.pstaynum2=b.pstaynum2;
    s.pstaynum3=b.pstaynum3;
    
    
elseif exist('b.switchnum1','var')==1 && exist('b.switchnum2','var')==1
    s.persevnum1=b.persevnum1;
    s.persevnum2=b.persevnum2;
    s.switchnum1=b.switchnum1;
    s.switchnum2=b.switchnum2;
    s.percentcorrect1=b.percentcorrect1;
    s.percentcorrect2=b.percentcorrect2;
    s.pswitchnum1=b.pswitchnum1;
    s.pswitchnum2=b.pswitchnum2;
    
    s.pstaynum1=b.pstaynum1;
    s.pstaynum2=b.pstaynum2;
    
    s.sswitchnum1=b.sswitchnum1;
    s.sswitchnum2=b.sswitchnum2;
    
    
elseif exist('b.switchnum2','var')==1 && exist('b.switchnum3','var')==1
    s.persevnum2=b.persevnum2;
    s.persevnum3=b.persevnum3;
    s.switchnum2=b.switchnum2;
    s.switchnum3=b.switchnum3;
    s.percentcorrect2=b.percentcorrect2;
    s.percentcorrect3=b.percentcorrect3;
    s.pswitchnum2=b.pswitchnum2;
    s.pswitchnum3=b.pswitchnum3;
    
    s.sswitchnum2=b.sswitchnum2;
    s.sswitchnum3=b.sswitchnum3;
    
    s.pstaynum2=b.pstaynum2;
    s.pstaynum3=b.pstaynum3;
    
    
else
    s.persevnum1=b.persevnum;
    s.switchnum1=b.switchnum;
    s.percentcorrect1=b.percentcorrect;
    s.pswitchnum1=b.pswitchnum;
    
    s.pstaynum1=b.pstaynum;
    
    s.sswitchnum1=b.sswitchnum;
end
s.choice=b.choice;%choice: 1=stim1, 2=stim2
s.ps=0; %ps=probabilistic switch - a switch to a bad stimulus after a probabilistic error
s.feed1=b.feed1;
s.feed2=b.feed2;
s.switch(1)=0;
for ct=1:length(s.choice)
    if s.choice(ct)==1,
        s.feed(ct)=s.feed1(ct);
    else
        s.feed(ct)=s.feed2(ct);
    end
end
s.feed=s.feed';
s.switchnum=0;
for ct=2:length(s.choice)
    if s.choice(ct)~=s.choice(ct-1)
        s.switch(ct)=1;
        s.switchnum=s.switchnum+1;
    else
        s.switch(ct)=0;
    end
end
s.switch=s.switch';
s.switch=s.switch';
s.stay=1-s.switch;

s.switchnext=[s.switch(2:end); 0];


%% shift choice
s.choice=s.choice';
shift=10;
a=zeros(shift,size(s.choice,2)+shift);
for ct=1:shift
    a(ct,ct:ct+size(s.choice,2)-1)=s.choice;
end

%%NOTES FOR JON
%% THIS CODE REPLICATES THE ANALYSES REPORTED IN WALTON NEURON 2010, FIGURE 5

%% find A*B sequences; for now, will only go up to [A*7 then B];
%% NOT DIRECTLY APPLICABLE TO BANDIT (BECAUSE REVERSAL HAD ONLY 2 OPTIONS) NEED TO RECODE!!!
s.a1b=find((a(2,:)==1) & (a(1,:)==2));
s.b1a=find((a(2,:)==2) & (a(1,:)==1));
s.a2b=find((a(3,:)==1) & (a(2,:)==1) & (a(1,:)==2));
s.b2a=find((a(3,:)==2) & (a(2,:)==2) & (a(1,:)==1));
s.a3b=find((a(4,:)==1) & (a(3,:)==1) & (a(2,:)==1) & (a(1,:)==2));
s.b3a=find((a(4,:)==2) & (a(3,:)==2) & (a(2,:)==2) & (a(1,:)==1));
s.a4b=find((a(5,:)==1) & (a(4,:)==1) & (a(3,:)==1) & (a(2,:)==1) & (a(1,:)==2));
s.b4a=find((a(5,:)==2) & (a(4,:)==2) & (a(3,:)==2) & (a(2,:)==2) & (a(1,:)==1));
s.a5b=find((a(6,:)==1) & (a(5,:)==1) & (a(4,:)==1) & (a(3,:)==1) & (a(2,:)==1) & (a(1,:)==2));
s.b5a=find((a(6,:)==2) & (a(5,:)==2) & (a(4,:)==2) & (a(3,:)==2) & (a(2,:)==2) & (a(1,:)==1));
s.a6b=find((a(7,:)==1) & (a(6,:)==1) & (a(5,:)==1) & (a(4,:)==1) & (a(3,:)==1) & (a(2,:)==1) & (a(1,:)==2));
s.b6a=find((a(7,:)==2) & (a(6,:)==2) & (a(5,:)==2) & (a(4,:)==2) & (a(3,:)==2) & (a(2,:)==2) & (a(1,:)==1));
s.a7plusb=find((a(8,:)==1) & (a(7,:)==1) & (a(6,:)==1) & (a(5,:)==1) & (a(4,:)==1) & (a(3,:)==1) & (a(2,:)==1) & (a(1,:)==2));
s.b7plusa=find((a(8,:)==2) & (a(7,:)==2) & (a(6,:)==2) & (a(5,:)==2) & (a(4,:)==2) & (a(3,:)==2) & (a(2,:)==2) & (a(1,:)==1));
%will make s.a40b, but won't use it -- just to clean s.a7b
%s.a39b=find((a(39,:)==1) & (a(8,:)==1) & (a(7,:)==1) & (a(6,:)==1) & (a(5,:)==1) & (a(4,:)==1) & (a(3,:)==1) & (a(2,:)==1) & (a(1,:)==2));

% shorter sequences should not include members of longer sequences
s.a1b=setdiff(s.a1b,s.a2b);
s.a2b=setdiff(s.a2b,s.a3b);
s.a3b=setdiff(s.a3b,s.a4b);
s.a4b=setdiff(s.a4b,s.a5b);
s.a5b=setdiff(s.a5b,s.a6b);
s.a6b=setdiff(s.a6b,s.a7plusb);
s.a23b=union(s.a2b,s.a3b);
s.a45b=union(s.a4b, s.a5b);
s.a67plusb=union(s.a6b, s.a7plusb);
s.a4plusb=union(s.a45b,s.a67plusb);

s.b1a=setdiff(s.b1a,s.b2a);
s.b2a=setdiff(s.b2a,s.b3a);
s.b3a=setdiff(s.b3a,s.b4a);
s.b4a=setdiff(s.b4a,s.b5a);
s.b5a=setdiff(s.b5a,s.b6a);
s.b6a=setdiff(s.b6a,s.b7plusa);
s.b23a=union(s.b2a,s.b3a);
s.b45a=union(s.b4a, s.b5a);
s.b67plusa=union(s.b6a, s.b7plusa);
s.b4plusa=union(s.b45a,s.b67plusa);

% find rewarded and punished trials
rew=find(s.feed==1);
pun=find(s.feed==2);

%% NOTE FOR JON: THIS REFERS TO FIGURE 5A
% find sequences where terminal B choice was reinforced/punished
s.a1brew=intersect(s.a1b,rew);
s.a1bpun=intersect(s.a1b,pun);
s.a2brew=intersect(s.a2b,rew);
s.a2bpun=intersect(s.a2b,pun);
s.a3brew=intersect(s.a3b,rew);
s.a3bpun=intersect(s.a3b,pun);
s.a4brew=intersect(s.a4b,rew);
s.a4bpun=intersect(s.a4b,pun);
s.a5brew=intersect(s.a5b,rew);
s.a5bpun=intersect(s.a5b,pun);
s.a6brew=intersect(s.a6b,rew);
s.a6bpun=intersect(s.a6b,pun);
s.a7plusbrew=intersect(s.a7plusb,rew);
s.a7plusbpun=intersect(s.a7plusb,pun);
s.a23brew=intersect(s.a23b,rew);
s.a23bpun=intersect(s.a23b,pun);
s.a4plusbrew=intersect(s.a4plusb,rew);
s.a4plusbpun=intersect(s.a4plusb,pun);

s.b1arew=intersect(s.b1a,rew);
s.b1apun=intersect(s.b1a,pun);
s.b23arew=intersect(s.b23a,rew);
s.b23apun=intersect(s.b23a,pun);
s.b4plusarew=intersect(s.b4plusa,rew);
s.b4plusapun=intersect(s.b4plusa,pun);
s.b7plusarew=intersect(s.b7plusa,rew);
s.b7plusapun=intersect(s.b7plusa,pun);



%% TO SEE WHAT THEY DID NEXT
% find sequences where terminal B was followed by B or A
s.a1bB=intersect(s.a1b,(find(s.choice==2)-1));
s.a1bA=intersect(s.a1b,(find(s.choice==1)-1));
s.a23bB=intersect(s.a23b,(find(s.choice==2)-1));
s.a23bA=intersect(s.a23b,(find(s.choice==1)-1));
s.a4plusbB=intersect(s.a4plusb,(find(s.choice==2)-1));
s.a4plusbA=intersect(s.a4plusb,(find(s.choice==1)-1));

s.b1aB=intersect(s.b1a,(find(s.choice==2)-1));
s.b1aA=intersect(s.b1a,(find(s.choice==1)-1));
s.b23aB=intersect(s.b23a,(find(s.choice==2)-1));
s.b23aA=intersect(s.b23a,(find(s.choice==1)-1));
s.b4plusaB=intersect(s.a4plusb,(find(s.choice==2)-1));
s.b4plusaA=intersect(s.a4plusb,(find(s.choice==1)-1));




%% BACKWARD SPREAD: find differential impact of reward/punishment on subsequent choice by
%% choice history
% for ab
if isempty(s.a1brew)==1 && isempty(s.b1arew)
    s.probAgivenA1BRew=NaN;
else
    s.probAgivenA1BRew=length(union((intersect(s.a1bA,s.a1brew)),(intersect(s.b1aA,s.b1arew))))./(length(s.a1brew)+length(s.b1arew));
end

if isempty(s.a1bpun)==1 && isempty(s.b1apun)==1
    s.probAgivenA1BPun=NaN;
else
    s.probAgivenA1BPun=length(union((intersect(s.a1bA,s.a1bpun)),(intersect(s.b1aA,s.b1apun))))./(length(s.a1bpun)+length(s.b1apun));
end
s.diffAgivenA1B=s.probAgivenA1BRew-s.probAgivenA1BPun;

% for a23b
if isempty(s.a23brew)==1 && isempty(s.b23arew)
    s.probAgivenA23BRew=NaN;
else
    s.probAgivenA23BRew=length(union((intersect(s.a23bA,s.a23brew)),(intersect(s.b23aA,s.b23arew))))./(length(s.a23brew)+length(s.b23arew));
end


if isempty(s.a23bpun)==1 && isempty(s.b23apun)==1
    s.probAgivenA23BPun=NaN;
else
    s.probAgivenA23BPun=length(union((intersect(s.a23bA,s.a23bpun)),(intersect(s.b23aA,s.b23apun))))./(length(s.a23bpun)+length(s.b23apun));
end

s.diffAgivenA23B=s.probAgivenA23BRew-s.probAgivenA23BPun;

% for a4plusb
if isempty(s.a4plusbrew)==1 && isempty(s.b4plusarew)
    s.probAgivenA4plusBRew=NaN;
else
    s.probAgivenA4plusBRew=length(union((intersect(s.a4plusbA,s.a4plusbrew)),(intersect(s.b4plusaA,s.b4plusarew))))./(length(s.a4plusbrew)+length(s.b4plusarew));
end

if isempty(s.a4plusbpun)==1 && isempty(s.b4plusapun)==1
    s.probAgivenA4plusBPun=NaN;
else
    s.probAgivenA4plusBPun=length(union((intersect(s.a4plusbA,s.a4plusbpun)),(intersect(s.b4plusaA,s.b4plusapun))))./(length(s.a4plusbpun)+length(s.b4plusapun));
end

s.diffAgivenA4plusB=s.probAgivenA4plusBRew-s.probAgivenA4plusBPun;
%% find sequences where previous A choice was reinforced/punished

%for initial a
%n-5
s.aRaaab=intersect(s.a4plusb,rew-4);
s.a0aaab=intersect(s.a4plusb,pun-4);
%n-4
s.aaRaab=intersect(s.a4plusb,rew-3);
s.aa0aab=intersect(s.a4plusb,pun-3);
%n-3
s.aaaRab=intersect(s.a4plusb,rew-2);
s.aaa0ab=intersect(s.a4plusb,pun-2);
%n-2
s.aaaaRb=intersect(s.a4plusb,rew-1);
s.aaaa0b=intersect(s.a4plusb,pun-1);
% find sequences where terminal B was followed by B or A
%n-5
s.aRaaabB=intersect(s.aRaaab,(find(s.choice==2)-1));
s.aRaaabA=intersect(s.aRaaab,(find(s.choice==1)-1));
s.a0aaabB=intersect(s.a0aaab,(find(s.choice==2)-1));
s.a0aaabA=intersect(s.a0aaab,(find(s.choice==1)-1));
%n-4
s.aaRaabB=intersect(s.aaRaab,(find(s.choice==2)-1));
s.aaRaabA=intersect(s.aaRaab,(find(s.choice==1)-1));
s.aa0aabB=intersect(s.aa0aab,(find(s.choice==2)-1));
s.aa0aabA=intersect(s.aa0aab,(find(s.choice==1)-1));
%n-3
s.aaaRabB=intersect(s.aaaRab,(find(s.choice==2)-1));
s.aaaRabA=intersect(s.aaaRab,(find(s.choice==1)-1));
s.aaa0abB=intersect(s.aaa0ab,(find(s.choice==2)-1));
s.aaa0abA=intersect(s.aaa0ab,(find(s.choice==1)-1));
%n-2
s.aaaaRbB=intersect(s.aaaaRb,(find(s.choice==2)-1));
s.aaaaRbA=intersect(s.aaaaRb,(find(s.choice==1)-1));
s.aaaa0bB=intersect(s.aaaa0b,(find(s.choice==2)-1));
s.aaaa0bA=intersect(s.aaaa0b,(find(s.choice==1)-1));

%same for initial b
%n-5
s.bRbbba=intersect(s.b4plusa,rew-4);
s.b0bbba=intersect(s.b4plusa,pun-4);
%n-4
s.bbRbba=intersect(s.b4plusa,rew-3);
s.bb0bba=intersect(s.b4plusa,pun-3);
%n-3
s.bbbRba=intersect(s.b4plusa,rew-2);
s.bbb0ba=intersect(s.b4plusa,pun-2);
%n-2
s.bbbbRa=intersect(s.b4plusa,rew-1);
s.bbbb0a=intersect(s.b4plusa,pun-1);
% find sequences where terminbl B wbs followed by B or A
%n-5
s.bRbbbaB=intersect(s.bRbbba,(find(s.choice==2)-1));
s.bRbbbaA=intersect(s.bRbbba,(find(s.choice==1)-1));
s.b0bbbaB=intersect(s.b0bbba,(find(s.choice==2)-1));
s.b0bbbaA=intersect(s.b0bbba,(find(s.choice==1)-1));
%n-4
s.bbRbbaB=intersect(s.bbRbba,(find(s.choice==2)-1));
s.bbRbbaA=intersect(s.bbRbba,(find(s.choice==1)-1));
s.bb0bbaB=intersect(s.bb0bba,(find(s.choice==2)-1));
s.bb0bbaA=intersect(s.bb0bba,(find(s.choice==1)-1));
%n-3
s.bbbRbaB=intersect(s.bbbRba,(find(s.choice==2)-1));
s.bbbRbaA=intersect(s.bbbRba,(find(s.choice==1)-1));
s.bbb0baB=intersect(s.bbb0ba,(find(s.choice==2)-1));
s.bbb0baA=intersect(s.bbb0ba,(find(s.choice==1)-1));
%n-2
s.bbbbRaB=intersect(s.bbbbRa,(find(s.choice==2)-1));
s.bbbbRaA=intersect(s.bbbbRa,(find(s.choice==1)-1));
s.bbbb0aB=intersect(s.bbbb0a,(find(s.choice==2)-1));
s.bbbb0aA=intersect(s.bbbb0a,(find(s.choice==1)-1));

%% FORWARD SPREAD: find differentibl impact of previous reward/punishment on choice by
% choice history
%n-5
if isempty(s.aRaaab)==1 && isempty(s.bRbbba)==1
    s.probAgivenaRaaab=NaN;
else
    s.probAgivenaRaaab=(length(s.aRaaabA)+length(s.bRbbbaB))./(length(s.aRaaab)+length(s.bRbbba));
end

if isempty(s.a0aaab)==1 && isempty(s.b0bbba)==1
    s.probAgivena0aaab=NaN;
else
    s.probAgivena0aaab=(length(s.a0aaabA)+length(s.b0bbbaB))./(length(s.a0aaab)+length(s.b0bbba));
end
s.diffAgivenaXaaab=s.probAgivenaRaaab-s.probAgivena0aaab;

%n-4
if isempty(s.aaRaab)==1 && isempty(s.bbRbba)==1
    s.probAgivenaaRaab=NaN;
else
    s.probAgivenaaRaab=(length(s.aaRaabA)+length(s.bbRbbaB))./(length(s.aaRaab)+length(s.bbRbba));
end

if isempty(s.aa0aab)==1 && isempty(s.bb0bba)==1
    s.probAgivenaa0aab=NaN;
else
    s.probAgivenaa0aab=(length(s.aa0aabA)+length(s.bb0bbaB))./(length(s.aa0aab)+length(s.bb0bba));
end
s.diffAgivenaaXaab=s.probAgivenaaRaab-s.probAgivenaa0aab;

%n-3
if isempty(s.aaaRab)==1 && isempty(s.bbbRba)==1
    s.probAgivenaaaRab=NaN;
else
    s.probAgivenaaaRab=(length(s.aaaRabA)+length(s.bbbRbaB))./(length(s.aaaRab)+length(s.bbbRba));
end

if isempty(s.aaa0ab)==1 && isempty(s.bbb0ba)==1
    s.probAgivenaaa0ab=NaN;
else
    s.probAgivenaaa0ab=(length(s.aaa0abA)+length(s.bbb0baB))./(length(s.aaa0ab)+length(s.bbb0ba));
end
s.diffAgivenaaaXab=s.probAgivenaaaRab-s.probAgivenaaa0ab;

%n-2
if isempty(s.aaaaRb)==1 && isempty(s.bbbbRa)==1
    s.probAgivenaaaaRb=NaN;
else
    s.probAgivenaaaaRb=(length(s.aaaaRbA)+length(s.bbbbRaB))./(length(s.aaaaRb)+length(s.bbbbRa));
end

if isempty(s.aaaa0b)==1 && isempty(s.bbbb0a)==1
    s.probAgivenaaaa0b=NaN;
else
    s.probAgivenaaaa0b=(length(s.aaaa0bA)+length(s.bbbb0aB))./(length(s.aaaa0b)+length(s.bbbb0a));
end
s.diffAgivenaaaaXb=s.probAgivenaaaaRb-s.probAgivenaaaa0b;

save mydata.mat s
