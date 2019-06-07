function utf2ascii(text_file_path,varargin)
% the 'utf2ascii' function converts a unicode text file to Window's 
% "ANSI" format. with this function, one does not have to manually 
% convert EVERY text file output by E-Prime.
%
% Jan Kalkus
% 2013-12-03: file inception
%
% Jan Kalkus
% 2013-12-13: added optional safety check to make sure file is UTF-16 
%             format instead of blindly reading in and converting the
%             file. this check is performed by default, though user 
%             can specify a 'fast' flag, which will skip the check, 
%             presumably skiping some file I/O, which might be more 
%             noticeable if 'utf2ascii' is run in a batch operation.


if(~any(strcmpi('fast',varargin)))
    if(~checkFileBOM(text_file_path)) % verify file is actually in UTF format
        warning('MATLAB:utf2ascii','file ''%s'' isn''t UTF format',text_file_path);
        return % if it isn't, simply exit this function (after alerting user)
    end
end

% open the file
fid_in = fopen(text_file_path,'rb','ieee-le');

% skip first 2 bytes (the BOM)
fseek(fid_in,2,0); 

% read in text in appropriate unicode format
all_the_bytes = fread(fid_in,'uint16');
fclose(fid_in); % close file pointer

% open file pointer for writing data back to file
fid_out = fopen(text_file_path,'w');
fwrite(fid_out,all_the_bytes); % write it

% close the file pointer
fclose(fid_out);

return