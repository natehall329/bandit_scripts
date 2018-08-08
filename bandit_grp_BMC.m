
function [posterior,out,p] = bandit_grp_BMC(DataDir,isMFX,savefile,doBetweenGroup,MFXFolderName,noMFXFolderName,MFXFileName,BetweenGroupFile)
debug=0;
if debug
    DataDir = '~/Box Sync/skinner/data/eprime/bandit';
    isMFX = 1;
    savefile = 1;
    doBetweenGroup = 1;
    MFXFolderName = 'vba_MFX_output';
    noMFXFolderName = 'vba_output';
    MFXFileName = "bandit_vba_MFX_output_";
    BetweenGroupFile = 'bandit_df3.csv';
    %Where is the root path for the processed data, should contain a model
    %folder & within it are subject vba files
    %DataDir='~/Box Sync/skinner/data/eprime/bandit';
    %
    %exampel:
    % bandit_grp_BMC('~/Box Sync/skinner/data/eprime/bandit',0,1) for noMFX
else
    
    if nargin > 8
        error('At most 8 inputs');
    end
    
    if nargin < 2
        error('Hey! Need at least the first two: DateDir(file path) and isMFX (logical)');
    end
    
    % Fill in unset optional values.
    switch nargin
        case 2
            savefile = 1;
            doBetweenGroup = 0;
            MFXFolderName = 'vba_MFX_output';
            noMFXFolderName = 'vba_output';
            MFXFileName = "bandit_vba_MFX_output_";
            BetweenGroupFile = 'bandit_df3.csv';
        case 3
            doBetweenGroup = 0;
            MFXFolderName = 'vba_MFX_output';
            noMFXFolderName = 'vba_output';
            MFXFileName = "bandit_vba_MFX_output_";
            BetweenGroupFile = 'bandit_df3.csv';
        case 4
            MFXFolderName = 'vba_MFX_output';
            noMFXFolderName = 'vba_output';
            MFXFileName = "bandit_vba_MFX_output_";
            BetweenGroupFile = 'bandit_df3.csv';
        case 5
            noMFXFolderName = 'vba_output';
            MFXFileName = "bandit_vba_MFX_output_";
            BetweenGroupFile = 'bandit_df3.csv';
        case 6
            MFXFileName = "bandit_vba_MFX_output_";
            BetweenGroupFile = 'bandit_df3.csv';
        case 7
            BetweenGroupFile = 'bandit_df3.csv';
    end
    
end

if isMFX 
   TypeN=MFXFolderName;
   Models=dir(fullfile(DataDir,TypeN));
   Models=Models(~[Models.isdir]);
   TypeK=fullfile(TypeN,'model_evidence');
else
   TypeN = noMFXFolderName;
   Models=dir(fullfile(DataDir,TypeN));
   Models=Models(3:length(Models));
   Models=Models([Models.isdir]);
   TypeK=TypeN;
end

if doBetweenGroup 
map = csvread(BetweenGroupFile,2);
%c1 is ID, c2 is G_5, c3 is G_7
[~,index]=unique(map(:,1));
map=map(index,:);
end 

Models=Models(~strcmp('.DS_Store',{Models.name}));
AllSubOutput = struct();
AllL=struct();
for x = 1:length(Models) 
    Model = Models(x);
    if isMFX
        load(fullfile(Model.folder,Model.name));
        Subs=out_sub;
        Model.name=char(extractBetween(Model.name,MFXFileName,".mat"));
        Models(x).name=Model.name;
    else
        Subs=dir(fullfile(Model.folder,Model.name));
        Subs=Subs(~[Subs.isdir]);
    end
    Models(x).name=strrep(Model.name,'_',' ');
    % write an L for BMC directly
    for j = 1:length(Subs)
        %clean up
        out=[];
        posterior=[];
        if isMFX
            out=out_sub{j,1};
            posterior=posterior_sub{j,1};
            Id=out.options.inF.b.id;
        else
            Sub=Subs(j);
            load(fullfile(Sub.folder,Sub.name));
            Id=str2double(regexp(Sub.name,'\d*','Match'));
            Id=Id(1);
        end
        AllSubOutput.(['model_' char(Model.name)]).(sprintf('id_%d',Id)).out=out;
        AllSubOutput.(['model_' char(Model.name)]).(sprintf('id_%d',Id)).posterior=posterior;
        AllL.(['L_' char(Model.name)]).F(j)=out.F;
        AllL.(['L_' char(Model.name)]).ID(j)=Id;
    end
    BanditL(x,:) = AllL.(['L_' char(Model.name)]).F;
    if doBetweenGroup
    GroupX(x,:) = map(find(ismember(map(:,1),AllL.(['L_' char(Model.name)]).ID)),2);
    end
end

%Made them Ls:d
%We at here assumed that all model has same subject number (the dim of
%BanditL is dependent on number of models and the last model's subject
%number, if the subj number is NOT the same; it will break subsequent code;
%BanditL = zeros(length(Models),length(Subs));

options.modelNames = {Models.name};
[posterior,out] = VBA_groupBMC(BanditL,options);

if doBetweenGroup
Fe=out.F(end);
for z = 1:length(unique(GroupX(1,:)))
    %Pos=findgroups(TempTable.Var1);
    zkl = unique(GroupX(1,:));
    zk = zkl(z);
    GroupL = BanditL(:,GroupX(1,:)==zk);
    options.modelNames = {Models.name};
    [posterior,out] = VBA_groupBMC(GroupL,options);
    GroupsL{z}=GroupL;
    GroupF(z)=out.F(end);
end
Fd = sum(GroupF);
p = 1/(1+exp(Fd-Fe)); 
else 
p = [];
end

if savefile 
file_path = fullfile(DataDir,TypeK);
mkdir(file_path)
file_name = sprintf('bandit_vba_model_evidence_%d_models',length(Models));
file_str = fullfile(file_path,file_name);
if ~doBetweenGroup
save(char(file_str),'posterior', 'out','p')
end
end 
