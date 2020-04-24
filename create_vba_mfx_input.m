function vba_df=create_vba_mfx_input()

fMRI_flag=0; %1 for fMRI sample 0 for behavioral

%Initialize the tbale
vba_df = table();

%Use glob to pull all the vba files
if fMRI_flag
    vba_files = glob('vba_output/*.mat');
else
%     vba_files = glob('/Users/nth7/Desktop/bandit_sandbox/bandit_scripts/Bandit_withrest/*.mat');
    vba_files = glob('vba_output/*.mat');
end


for vba_file = vba_files'
    load(vba_file{:}) %Load in the file should contain b,out,posterior
    stop=1;
    
    %Initialize temporay dataframe
    tmp_table = table();
    
    %Grab id
    tmp_table.ID = b.id;
    
    
%     expression = '\d+';
%     bla=regexp(vba_file, expression, 'match');
%     id = char(bla{1,1}(2));
%     
    
    %Grab y & u
    tmp_table.y = {out.y};
    tmp_table.u = {out.u};
    
    %Grab the options used or perhaps create a sub function to create this
    tmp_table.options = {out.options};
    
    vba_df = [vba_df; tmp_table];
    
end