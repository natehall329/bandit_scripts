%% test if different iterations of VBA_MFX look similar

load('vba_output/id_103_bandit_vba_output_valence_26-Feb-2020_')
bf = b;
outf = out;
posteriorf = posterior;

load('vba_output/id_103_bandit_vba_output_valence_22-Jul-2019_')
posterior.muTheta
posteriorf.muTheta
posterior.muPhi
posteriorf.muPhi

%excellent.

%% couple subjects. start with excellent fit

addpath(genpath('~/Documents/MATLAB/VBA-toolbox-master/'))
%addpath('~/Documents/MATLAB/VBA-toolbox-master/utils')
addpath('vba/')
addpath('behav_scripts/')
addpath('~/Box/temporal_instrumental_agent')
addpath('~/Box/spott_modeling/vba_timeseries/') %for toying with MH's options, priors etc

load('vba_output/id_25_bandit_vba_output_valence_26-Feb-2020_');
VBA_ReDisplay(posterior, out)

%% probably better to hone in on bad subs
load('vba_output/id_73_bandit_vba_output_valence_26-Feb-2020_');  % super erratic behavior. drop
VBA_ReDisplay(posterior, out)

load('vba_output/id_81_bandit_vba_output_valence_26-Feb-2020_'); % super erratic behavior. drop
VBA_ReDisplay(posterior, out)

load('vba_output/id_27_bandit_vba_output_valence_26-Feb-2020_');  % super erratic behavior. drop
VBA_ReDisplay(posterior, out)

load('vba_output/id_118_bandit_vba_output_valence_26-Feb-2020_'); % also pretty bad.
VBA_ReDisplay(posterior, out)

% okay, let's try someone out with an R2 of ~.3
load('vba_output/id_80_bandit_vba_output_valence_26-Feb-2020_'); % looking a little better, though temperature and alpha_win still seem correlated at ~r=.7
VBA_ReDisplay(posterior, out)

% look at first subject above R2 of .25. considering making this a cutpoint
load('vba_output/id_134_bandit_vba_output_valence_26-Feb-2020_'); 
VBA_ReDisplay(posterior, out)


