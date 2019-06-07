function b = eprimeread(fname,procname,fields,breaksevery,startoffset,endoffset,badprocname,altprocname)
% Unknown author
% sometime before 2011
%
%
% Reads an e-prime file into a matlab structure and returns data with the fields 
% you have specified.
%
% usage:    b = eprimeread(fname,procname,fields,breaksevery,startoffset,endoffset)
%
% For each occurrance of some procedure ("procname") finds values for all "fields"
% between each of the "offsets" and stores them in the return structure
%
%
% Input arguments:
%
%         fname:  Filename of the eprime .txt file (not the .edat!)
%
%      procname:  Procedure name (or subset of procedure name) in which to search 
%                 for desired fields.
%
%        fields:  a cell array of field names, i.e., {'field1','field2'...'fieldn'}
%
%	breaksevery:  if you have breaks between consecutive calls to the procedure 
%                 (i.e., branches from the procedure to another logframe) intervening 
%                 data should not be counted. This is a problem in the way eprime 
%                 handles both .txt and edat files - it duplicates the procedure name. 
%                 So if there are predictable breaks every n-trials let this routine 
%                 know. If not, breaksevery should be set to zero.
%
%	startoffset:
%     endoffset: 'startoffset' and 'endoffset' are the number of lines around a 
%                 procedure name to check for relevant data. This is important 
%                 because sometimes procedure data actually starts before the 
%                 procedure name (near as I can tell another bug in eprime).
%
%     note: a field is defined as a line with a ':' so lines specifying logframes 
%           should NOT count towards the total
%
%
% In English:
%
%    To use it you must specify a filename, the procedure in which your variables of 
%    interest occur, the variables of interest, how many trials there are between 
%    blocks (if the eprime code accounts for blocks by doing something wierd), as 
%    well as offsets specifying the window in which your variables of interest are 
%    likely to occur relative to the beginning of the trial.
%
%    If you want to read variables that occur in two different procedures (e.g., you 
%    have an alternating task design that alternates between cue and probe trials 
%    and want to read stuff about each of them), just call 'eprimeread' twice.
%
%
% example:
%
%    fname = 'c:/kod/matlab/test-file-23260.txt';
%    b = eprimeread(fname,'TrialProc1',{'Condition','Offer.RT', ...
%            'Offer.ACC','Offer'},0,-1,20);
%
%
%
% Jan Kalkus
% Sometime in 2012: cleaned up some of the code for visual clarity as well as 
%                   incorporation of quicker (e.g., cellfun) code
%
% Jan Kalkus
% 2013-12-03: fixed issue with UTF format files; instead of breaking, this function
%             will call 'uni2ascii' to convert the file fron UTF to 'ANSI'


% check input arguments
if(nargin < 4), breaksevery = 0; end
if(nargin < 5), startoffset = -4; end
if(nargin < 6), endoffset   = 17; end
if(nargin < 7), badprocname = 'bogusproc'; end
if(nargin < 8), altprocname = 'notaproc'; end

if (~exist(fname,'file')) 
    error('MATLAB:eprimeread:FileNotFound','File:\n\t%s\n was not found.\n',fname); 
else
	b.fname = fname; 
end

% what is this?
word_list = {'pretty','shower','gray','home','spring','cool','bench', ...
    'white','light','waterfall','true','today','tree','clock','summer', ...
    'winter','fall','down','write','float'};

% check for file format
if(checkFileBOM(fname)) % file format is UTF-16 IEEE-LE
    fprintf('\n\tfile: ''%s''\n\tis in UTF16-LE format\n',fname);
    fprintf('\t\tconverting to ASCII...');
    utf2ascii(fname,'fast'); % convert it (fast = no need to verify BOM)
    fprintf('\t done\n');
end

% read the raw text file
fid = fopen(fname,'r');

if(fid < 0)
    warning('eprimeread:open_file','Unable to open file ''%s'' ...moving on',fname);
    fclose(fid);
    return
else
    rawtxt = textscan(fid,'%s','delimiter',':\n','whitespace',''); % why '' for whitespace?
    rawtxt = rawtxt{:};
    fclose(fid);
end

% strip out non-fields
q_keep = ( cellfun(@numel,rawtxt) > 0 ); % set empty char. entries to 'false'
q_keep = ( q_keep & cellfun(@isempty,strfind(rawtxt,'***')) );
txt = cellfun(@strtrim,rawtxt(q_keep),'UniformOutput',false);

% get lines on which the correct procedure occurs
index=0;
proclines=0;
for ct=1:length(txt)-1 % these for-loops are killing me...
    % it looks like there are two large contingencies (A or B)
    % to satisfy, each with their own three contingencies (i, ..., iii)

        % THESE MUST ALL BE TRUE
        % A, i: 
        %   index contains 'Procedure' == true
        % A, ii:
        %   next index (after 'Procedure') matches _procname_
        % A, iii: 
        %   next index cannot be match with _badprocname_
        
        % OR...
        
        % THESE MUST ALSO BE TRUE
        % B, i:
        %   index contains 'Procedure' == true
        % B, ii:
        %   index+1 matches _altprocname_
        % B, iii:
        %   index+1 cannot match _badprocname_
        
    if ( ...
            strcmp('Procedure',txt{ct}) && ...
            strcmpi(procname,txt{ct+1}) && ...    % note use of 'strcmpi' here
            ~strcmpi(badprocname,txt{ct+1}) ...   % ...and here
        ) || ( ...
            strcmp('Procedure',txt{ct}) && ...
            strcmpi(altprocname,txt{ct+1}) && ... % ...and here
            ~strcmpi(badprocname,txt{ct+1}) ...   % ...and here
        )

        index = index + 1; 
		proclines(index) = ct;
    end
end

% % a cleaner attempt of above code... (not yet tested)
% n = 0;
% n=n+1; q{n} = ~cellfun(@isempty,strfind(txt,'Procedure'));
% n=n+1; q{n} = strfind(lower(txt),lower(procname)); % still need to incorp. ct+1 from above
% n=n+1; q{n} = isempty(strfind(lower(txt),lower(badprocname))); % see previous comment

if (breaksevery==0) 
	numblocks = 0;
else
	numblocks = floor(index./breaksevery);
end

data    = cell(index-numblocks,length(fields));
numdata = zeros(index-numblocks,length(fields))-999;
line    = 0;
for ind = 1:index
    if ((breaksevery==0) || (mod(ind,breaksevery+1)~=1))
		line=line+1;
    	for field=1:length(fields)
     		unfound=1;
     		for candidate=proclines(ind)+2.*startoffset:proclines(ind)+2.*endoffset;
      			if (unfound && strcmp(char(fields(field)),strtrim(char(txt(candidate)))))
					unfound=0;
        			data(line,field)=txt(candidate+1);

                    if(any(~cellfun(@isempty,strfind(word_list,char(data(line,field))))))
	  					numval=[];
        			else
  	  					numval=str2double(char(data(line,field)));
                        if isnan(numval) 
                            numval=[]; 
                        end
                    end

					%if field==1, fprintf('line=%d, data=%s\n',line,char(data(line,field))); end
					if (~isempty(numval))
	  					numdata(line,field)=numval;
	 					 %fprintf('line=%d,field=%d, data=%s, numval=%d\n',line,field,char(data(line,field)),numval);
					else
	  					numdata(line,field)=-999;
					end
      			end
     		end
    	end
  	end
end

if(isempty(data{end,1}))
	data=data(1:end-1,:);
	numdata=numdata(1:end-1,:);
end

for ct=1:length(fields)
    if max(numdata(:,ct))==-999
    	curdata=data(:,ct);
  	else
    	curdata=numdata(:,ct);
  	end
  	fldname=strrep(char(fields(ct)),'.','_');
  	b=setfield(b,fldname,curdata);
end

return