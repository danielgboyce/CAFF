%IMPORTS SST PROJECTIONS FROM CMIP6, INTERPOLATES TO SAME RESOLUTION AS
%CRIB (REYNOLDS SST; 0.25deg) AND WRITES AS NETCDF FILES FOR USE IN R
lata=38;
latb=85;
lona=-83;
lonb=-41;

lata=38;
latb=88;
lona=-162;
lonb=-48;
lonq=[-179.875:0.25:179.875];
latq=[89.875:-0.25:-89.875]';
lonq=lonq(lonq> lona & lonq< lonb);
latq=latq(latq<latb & latq>lata);
[Xq,Yq] = meshgrid(lonq, latq);
%FIRST PART HERE SETS UP VALUES FROM PATHFINDER SST TO INTERPOLATE AT AND FORMATS POLYGON MASK FOR CANADIAN EEZ
latlenq=length(latq);
lonlenq=length(lonq);
latoutq = repmat(latq,lonlenq,1);
lonoutq = repmat(lonq,latlenq,1);
lonoutq = reshape(lonoutq,length(latoutq),1);
lnthq=length(lonoutq);




%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%    HADLEY
cd('C:\Users\danie\Downloads\cmip')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file='siconc_SImon_HadGEM3-GC31-MM_ssp585_r1i1p1f3_gn_201501-202912.nc'
ncdisp(file)

%PROJECTION RESPONSES
rsp={'siconc'};
sst = ncread(file,rsp{1});

%COORDINATES OF PROJECTION DATA
lon2 = ncread(file,'longitude');
%IF 0-360, CONVERT TO -180-180
ifelse = @(Condition,TrueVal,FalseVal) Condition .*TrueVal + ~Condition .*FalseVal;
lon2=ifelse(lon2< -180,lon2+360,lon2);
lonout=lon2(:);

lat2 = ncread(file,'latitude');
latout=lat2(:);

%GET TIME DIMENSION
times = size(sst,3);
times=5;

%%time = ncread(file,'time');
%times=10;

%SET UP EMPTY ARRAY (LAT, LON, TIME)
outt = NaN(length(latq),length(lonq),times);
%LOOP OVER TIME STEPS, PROCESS DATA, ACCUMULATE IN 'OUTT'
for i=1:1:times;
    
    display(i);
    %EXTRACT SST AT TIME I
    a=sst(:,:,i); 
    %ANY VALUE < -6 IS NA
    a(a<=-6) = NaN;   
    %image(a);
    %PUT IN PROPER FORMAT FOR INTERPOLATION
    out=[double(lonout) double(latout) double(a(:))];
    %%RESTRICT DATA TO SPEED UP INTERPOLATION; ADD SMALL BUFFER AROUND
    out=out(out(:,1)>= lona-5 & out(:,1)<= lonb+5,:);
    out=out(out(:,2)>= lata-5 & out(:,2)<= latb+5,:);
    %scatter(out(:,1),out(:,2),'.')
    %INTERPOLATE TO COMMON GRID ACROSS AOI
    gd = griddata(out(:,1),out(:,2),out(:,3),Xq,Yq,'linear');
    %image(fliplr(rot90(gd*100)));
    %image(gd*10000);
    %outt=cat(3,outt,gd);
    outt(:,:,i)=gd;

    clear a out gd;
 end;
   
%VALIDITY CHECK 
%a=outt(:,:,1);
%image(a*100)
b=[Xq(:), Yq(:), a(:)];
b(any(isnan(b),2),:) = [];
scatter(b(:,1),b(:,2),'.');


%%cd('N:\data\CC_vulnerability_reg\data\CMIP6_models\formatted\matlab\8.5')
%%save('HAD_SSP5_RCP8.5_SST_0.25deg_regrid.mat','datout')



%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%% IPSL 2.6
cd('N:\data\CMIP\CMIP6\Sea_ice_fraction\0.25deg\Monthly\2.6\ipsl')
file='siconc_SImon_CNRM-CM6-1-HR_ssp126_r1i1p1f2_gn_201501-210012.nc';
ncdisp(file)

%PROJECTION RESPONSES
rsp={'siconc'};
sic = ncread(file,rsp{1});

%COORDINATES OF PROJECTION DATA
lon2 = ncread(file,'lon');
lonout=lon2(:);
lat2 = ncread(file,'lat');
latout=lat2(:);
%lnth=length(lonout);
%t=t(1:10);
times = size(sic,3);
%%time = ncread(file,'time');
%times=10;

outt = NaN(length(latq),length(lonq),times);
for i=1:1:times;
    
    display(i);
    a=sic(:,:,i); 
    %image(a);
    a(a>=101) = NaN;   
    out=[double(lonout) double(latout) double(a(:))];
    %%RESTRICT DATA TO AREA OF INTEREST TO SPEED UP INTERPOLATION; ADD SMALL BUFFER AROUND
    out=out(out(:,1)>= lona-5 & out(:,1)<= lonb+5,:);
    out=out(out(:,2)>= lata-5 & out(:,2)<= latb+5,:);   
    %%outp=out;
    %%outp(any(isnan(outp),2),:) = [];
    %%scatter(outp(:,1),outp(:,2),'.');

    gd = griddata(out(:,1),out(:,2),out(:,3),Xq,Yq,'linear');
    %image(fliplr(rot90(gd*100)));
    %image(gd*100);
    %outt=cat(3,outt,gd);
    outt(:,:,i)=gd;
    %%a=gd(:,:,1);
    %image(a*5);          
    clear a out gd;
 end;
 
 
cd('N:\data\CAFF\data\CMIP6_models\SIC\formatted\matlab\2.6')
save('IPSL_SSP5_RCP2.6_SIC_0.25deg_regrid.mat','outt')
 
 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% IPSL 8.5
cd('N:\data\CMIP\CMIP6\Sea_ice_fraction\0.25deg\Monthly\8.5\ipsl')
file='siconc_SImon_CNRM-CM6-1-HR_ssp585_r1i1p1f2_gn_201501-210012.nc';
ncdisp(file)

%PROJECTION RESPONSES
rsp={'siconc'};
sic = ncread(file,rsp{1});

%COORDINATES OF PROJECTION DATA
lon2 = ncread(file,'lon');
lonout=lon2(:);
lat2 = ncread(file,'lat');
latout=lat2(:);
%lnth=length(lonout);
%t=t(1:10);
times = size(sic,3);
%%time = ncread(file,'time');
%times=10;

outt = NaN(length(latq),length(lonq),times);
for i=1:1:times;
    
    display(i);
    a=sic(:,:,i); 
    %image(a);
    a(a>=101) = NaN;   
    out=[double(lonout) double(latout) double(a(:))];
    %%RESTRICT DATA TO AREA OF INTEREST TO SPEED UP INTERPOLATION; ADD SMALL BUFFER AROUND
    out=out(out(:,1)>= lona-5 & out(:,1)<= lonb+5,:);
    out=out(out(:,2)>= lata-5 & out(:,2)<= latb+5,:);   
    %%outp=out;
    %%outp(any(isnan(outp),2),:) = [];
    %%scatter(outp(:,1),outp(:,2),'.');

    gd = griddata(out(:,1),out(:,2),out(:,3),Xq,Yq,'linear');
    %image(fliplr(rot90(gd*100)));
    %image(gd*100);
    %outt=cat(3,outt,gd);
    outt(:,:,i)=gd;
    %%a=gd(:,:,1);
    %image(a*5);          
    clear a out gd;
 end;
 
 
cd('N:\data\CAFF\data\CMIP6_models\SIC\formatted\matlab\8.5')
save('IPSL_SSP5_RCP8.5_SIC_0.25deg_regrid.mat','outt')


%%

%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%    HADLEY
cd('C:\Users\sailfish\Downloads\prac')
%all files in directory
fls = {dir 'siconc'};
fls = {fls{1}.name};
%fls=strrep(fls, 'tos','read');
fls=fls(cellfun('isempty',strfind(fls,'ssp'))==0);
%length of files and variables to loop through
lfile=length(fls);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%LOOPS THROUGH FILENAMES EXTRACTS; 
%RE-GRIDS DATA AND SUBSETS TO NW ATLANTIC AND NE PACIFIC 
datout = [];
for g = 1:lfile;
display(g)

file=fls{g}
%%file='tos_Omon_GFDL-CM4_ssp585_r1i1p1f1_gn_201501-203412.nc';
%%ncdisp(file)

%PROJECTION RESPONSES
rsp={'siconc'};
sst = ncread(file,rsp{1});

%COORDINATES OF PROJECTION DATA
lon2 = ncread(file,'longitude');
ifelse = @(Condition,TrueVal,FalseVal) Condition .*TrueVal + ~Condition .*FalseVal;
lon2=ifelse(lon2< -180,lon2+360,lon2);
lonout=lon2(:);

lat2 = ncread(file,'latitude');
latout=lat2(:);
%lnth=length(lonout);
%t=t(1:10);
times = size(sst,3);
%%time = ncread(file,'time');
%times=10;

outt = NaN(length(latq),length(lonq),times);
for i=1:1:times;
    
    display(i);
    a=sst(:,:,i); 
    a(a<=-6) = NaN;   
    %image(a);
    out=[double(lonout) double(latout) double(a(:))];
    %%RESTRICT DATA TO SPEED UP INTERPOLATION; ADD SMALL BUFFER AROUND
    out=out(out(:,1)>= lona-5 & out(:,1)<= lonb+5,:);
    out=out(out(:,2)>= lata-5 & out(:,2)<= latb+5,:);
    
    gd = griddata(out(:,1),out(:,2),out(:,3),Xq,Yq,'linear');
    %image(fliplr(rot90(gd*100)));
    %image(gd*10000);
    %outt=cat(3,outt,gd);
    outt(:,:,i)=gd;

    clear a out gd;
 end;
   
datout = cat(3, datout, outt);

end;

 
%a=datout(:,:,20);
%image(a*100)

%%cd('N:\data\CC_vulnerability_reg\data\CMIP6_models\formatted\matlab\8.5')
%%save('HAD_SSP5_RCP8.5_SST_0.25deg_regrid.mat','datout')








%%
%NOT DONE FOR THESE YET



%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%    GFDL
cd('N:\data\CC_vulnerability_reg\data\CMIP6_models\8.5\noaa')
%all files in directory
fls = {dir 'ssp'};
fls = {fls{1}.name};
%fls=strrep(fls, 'tos','read');
fls=fls(cellfun('isempty',strfind(fls,'ssp'))==0);
%ncdisp(fls{1})

%length of files and variables to loop through
lfile=length(fls);
lvar=length(rsp);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%LOOPS THROUGH FILENAMES EXTRACTS; 
%RE-GRIDS DATA AND SUBSETS TO NW ATLANTIC AND NE PACIFIC 
datout = [];
for g = 1:lfile;
display(g)

file=fls{g}
%%file='tos_Omon_GFDL-CM4_ssp585_r1i1p1f1_gn_201501-203412.nc';
%%ncdisp(file)

%PROJECTION RESPONSES
rsp={'tos'};
sst = ncread(file,rsp{1});

%COORDINATES OF PROJECTION DATA
lon2 = ncread(file,'lon');
ifelse = @(Condition,TrueVal,FalseVal) Condition .*TrueVal + ~Condition .*FalseVal;
lon2=ifelse(lon2< -180,lon2+360,lon2);
lonout=lon2(:);

lat2 = ncread(file,'lat');
latout=lat2(:);
%lnth=length(lonout);
%t=t(1:10);
times = size(sst,3);
%%time = ncread(file,'time');
%times=10;

outt = NaN(length(latq),length(lonq),times);
for i=1:1:times;
    
    display(i);
    a=sst(:,:,i); 
    a(a<=-6) = NaN;   
    %image(a);
    out=[double(lonout) double(latout) double(a(:))];
    %%RESTRICT DATA TO SPEED UP INTERPOLATION; ADD SMALL BUFFER AROUND
    out=out(out(:,1)>= lona-5 & out(:,1)<= lonb+5,:);
    out=out(out(:,2)>= lata-5 & out(:,2)<= latb+5,:);
    
    gd = griddata(out(:,1),out(:,2),out(:,3),Xq,Yq,'linear');
    %image(fliplr(rot90(gd*100)));
    %image(gd*100);
    %outt=cat(3,outt,gd);
    outt(:,:,i)=gd;

    clear a out gd;
 end;
   
datout = cat(3, datout, outt);

end;

%a=datout(:,:,20);
%image(a*100)

cd('N:\data\CC_vulnerability_reg\data\CMIP6_models\formatted\matlab\8.5')
save('GFDL_SSP5_RCP8.5_SST_0.25deg_regrid.mat','datout')


%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%    HADLEY
cd('N:\data\CC_vulnerability_reg\data\CMIP6_models\8.5\dkrz\Hadley')
%all files in directory
fls = {dir 'ssp'};
fls = {fls{1}.name};
%fls=strrep(fls, 'tos','read');
fls=fls(cellfun('isempty',strfind(fls,'ssp'))==0);
%length of files and variables to loop through
lfile=length(fls);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%LOOPS THROUGH FILENAMES EXTRACTS; 
%RE-GRIDS DATA AND SUBSETS TO NW ATLANTIC AND NE PACIFIC 
datout = [];
for g = 1:lfile;
display(g)

file=fls{g}
%%file='tos_Omon_GFDL-CM4_ssp585_r1i1p1f1_gn_201501-203412.nc';
%%ncdisp(file)

%PROJECTION RESPONSES
rsp={'tos'};
sst = ncread(file,rsp{1});

%COORDINATES OF PROJECTION DATA
lon2 = ncread(file,'longitude');
ifelse = @(Condition,TrueVal,FalseVal) Condition .*TrueVal + ~Condition .*FalseVal;
lon2=ifelse(lon2< -180,lon2+360,lon2);
lonout=lon2(:);

lat2 = ncread(file,'latitude');
latout=lat2(:);
%lnth=length(lonout);
%t=t(1:10);
times = size(sst,3);
%%time = ncread(file,'time');
%times=10;

outt = NaN(length(latq),length(lonq),times);
for i=1:1:times;
    
    display(i);
    a=sst(:,:,i); 
    a(a<=-6) = NaN;   
    %image(a);
    out=[double(lonout) double(latout) double(a(:))];
    %%RESTRICT DATA TO SPEED UP INTERPOLATION; ADD SMALL BUFFER AROUND
    out=out(out(:,1)>= lona-5 & out(:,1)<= lonb+5,:);
    out=out(out(:,2)>= lata-5 & out(:,2)<= latb+5,:);
    
    gd = griddata(out(:,1),out(:,2),out(:,3),Xq,Yq,'linear');
    %image(fliplr(rot90(gd*100)));
    %image(gd*10000);
    %outt=cat(3,outt,gd);
    outt(:,:,i)=gd;

    clear a out gd;
 end;
   
datout = cat(3, datout, outt);

end;

 
%a=datout(:,:,20);
%image(a*100)

cd('N:\data\CC_vulnerability_reg\data\CMIP6_models\formatted\matlab\8.5')
save('HAD_SSP5_RCP8.5_SST_0.25deg_regrid.mat','datout')










nanmin(lonoutq(:,1))
nanmax(lonoutq(:,1))
nanmin(latoutq(:,1))
nanmax(latoutq(:,1))    

a=sst(:,:,1);
image(a);
a2=a(:);

out=[lonout latout a(:)];
out(any(isnan(out),2),:) = [];
scatter(out(:,1),out(:,2),'.','green')

    

time = ncread(file,'time');
btime=char(ncreadatt(file,'time','units'));
byear=str2num(btime(12:15));


a=dat(:,:,1);
image(a);


