
curdir = cd;
datadir = [curdir '/sample data/'];

cd(datadir)

jsonfiles = dir('*.json');

fname = jsonfiles(5).name;
fid = fopen(fname); 
raw = fread(fid,inf); 
str = char(raw'); 
fclose(fid); 
val = jsondecode(str);
