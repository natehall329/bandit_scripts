function UTF_BOM_bool = checkFileBOM(file_path)
% check file BOM for (checks the first two bytes of the file to see
% if it matches a text unicode file format)
%
% Jan Kalkus
% 2013-12-03: written a while ago, just now made a standalone file


file_pointer = fopen(file_path,'rb'); % open file pointer
first_two_bytes = fread(file_pointer,2); % read in first 2 bytes
fclose(file_pointer); % kill it (don't need it anymore)

% if the first two bytes are 0xff and 0xfe, that's a UTF BOM
if(all(eq(first_two_bytes(:),[255;254]))) % 0xff 0xfe 
    UTF_BOM_bool = true; % file is 16 bit Unicode, little endian format
else
    UTF_BOM_bool = false;
end

return