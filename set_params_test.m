% parameters for experiment
% AKR 19.10 edited from AW 9/7/12
% a function for setting parameters for drawsquares experiment

function p = set_params_test(subNum)

if nargin<1;
    subNum = 100;
end

%% -- trial information --
p.nruns = 10; %number of runs
p.nblocks = 2; %number of blocks
p.ntrials = 80;
p.ITI = 1000; % ITI in ms
p.waittime = 4000;
p.fixdur = 0.5; % seconds
p.stimdur = 0.5;

%% -- Screen info
p.ScreenNumber = max(Screen('Screens'));
[winWidth, winHeight] = Screen('DisplaySize', p.ScreenNumber); % width in mm
% winWidth = 710;
% winHeight =390;
[pixwidth, pixheight]=Screen('WindowSize', p.ScreenNumber); % width in pixels
xcenter=winWidth/2;
ycenter=winHeight/2;

Screen('Preference', 'SkipSyncTests', 0); %timing tests (value 1 to skip)
Screen('Preference', 'VisualDebugLevel', 4); %other screen tests (value determines level of debugging, higher is more debugging, 3 is recommended)

p.winWidth = winWidth;
p.winHeight = winHeight;
p.winpixx = pixwidth;
p.winpixy = pixheight;

%% -- image positions --

% details for converting visual angle to pixels
viewingdistance = 1120; % Distance between monitor and participant in mm
visang_rad = 2 * atan(winWidth/2/viewingdistance);
visang_deg = visang_rad * (180/pi); % number of degrees of monitor visual angle
% mm_pervisang = winWidth / visang_deg;
% pix_pervisang = pixwidth/winWidth*mm_pervisang; % pixels / visual angle
pix_pervisang = pixwidth/visang_deg; % number of pixels / visual angle

% size of images in degrees of arc
imdoa = 2; % image size in degrees of arc
raddoa = 4; % distance from centre of screen to object (radius)

% size of images in pixels
imsize = imdoa*pix_pervisang;
p.imgSize=imsize;

radsize = raddoa*pix_pervisang;

xs = [3*pix_pervisang 2.2*pix_pervisang]; % x distance to left or right (far, med)

% calculate Y distance from centre using pythag
ys(1) = sqrt(radsize^2-xs(1)^2); % lower
ys(2) = sqrt(radsize^2-xs(2)^2); % higher

screenRect = Screen('Rect', p.ScreenNumber);
centRect = CenterRect([0 0 imsize imsize], screenRect); %central rectangle

p.testRects = [OffsetRect(centRect,-xs(1),-ys(1)); ...
    OffsetRect(centRect,-xs(2),-ys(2)); ...
    OffsetRect(centRect,xs(2),-ys(2)); ...
    OffsetRect(centRect,xs(1),-ys(1)); ...
    ];

% move further apart for training
xs(1) = xs(1) + 50;
xs(2) = xs(2) - 15;


p.trainRects = [OffsetRect(centRect,-xs(1),-ys(1)); ...
    OffsetRect(centRect,-xs(2),-ys(2)); ...
    OffsetRect(centRect,xs(2),-ys(2)); ...
    OffsetRect(centRect,xs(1),-ys(1)); ...
    ];

%% fixation square
fixsize = 0.4*pix_pervisang;

p.fixRect = CenterRect([0 0 fixsize fixsize], screenRect); %central rectangle


%% photodiode stimulus

% photodiode rect
rectsize = 1*pix_pervisang;
pdlocs= [0, 0, rectsize,rectsize]; % coords for image (top right)
p.pdrect = OffsetRect(pdlocs,pixwidth-rectsize-150,pixheight-rectsize-150);



%% -- colours --
p.black = [0 0 0];
p.darkgrey = [30 30 30];

%p.grey = [170 170 170];
p.grey = [.6765 .6635 .6746]*255;

p.white = [204 204 204]; %[255 255 255];
p.blue = [    0.3669    0.6588    0.9843]*255;%[0 0 255];
    

% these are at 50% saturation and 70% brightness, separated by 90
% degrees in Hue
red = [204 122 122]; % 0 deg hue
green = [167 204 122];%[163 204 122]; %90
lblue = [122 204 204]; %180
purple = [163 122 204]; %270

isoblue =  [0.5407    0.6381    0.9316]*255;
isogreen=   [0.1375    0.7972    0.4220]*255;
isoorange=    [0.7924    0.6899    0.2940]*255;
isopink =    [0.9821    0.5095    0.8689]*255;

% green =  [0.0138    0.8286    0.1192]*255;


%% -- Background colours for each rule --
% 2 colours per rule
bgcols{1,1} = isoblue; % col 1, rule 1
bgcols{1,2} = isogreen; % col 1 rule 2
bgcols{2,1} = isoorange; % col 2 rule 1
bgcols{2,2} = isopink; % col 2 rule 2
p.cols= {'blue' 'green'; 'orange' 'pink'};
p.bgcols = bgcols; %pass back to main script



%% -- correct responses --
% corResp(blkType, rule, position) ie (rule, position)

set(1,:) = [4 1 3 2]; % rule 1
set(2,:) = [3 2 4 1]; % rule 2

%pass out (option for testing different rules on different people)
p.corResp = set;


% codes for button box and keyboard
% first row is keyboard, second row is button box
% KbName('UnifyKeyNames')

% p.kb_codes = KbName({'1' '2' '3' '4'}); %these (button box) may not be correct
p.kb_codes = [97:100]; %these (button box) may not be correct


%% ---- special params for learning and pratice version -----

% vary order or learning rules according to ptp

p.blockorder = [1 2 2 2]; % 1 = present rules; 2 = practice mixed rules
p.learnrulename = [0 1 2 3]; % what to call each rule (always 1-2 in order you learn them!)
p.learntrials = [24 32 32 32]; %how many trials do you want for each block of the practice (min 4)
p.prestime = [0 0 0 1]; % 0 = stay on until response, 1 = brief presentation
p.pos = [1 1 2 2]; % further apart positions, closer (test) positions

% response box cue rectangles
p.resRects = [OffsetRect(centRect,-225,200); ...
    OffsetRect(centRect,-75,200); ...
    OffsetRect(centRect,75,200); ...
    OffsetRect(centRect,225,200); ...
    ];

p.resRects2 = [OffsetRect(centRect,-450,200); ...
    OffsetRect(centRect,-150,200); ...
    OffsetRect(centRect,150,200); ...
    OffsetRect(centRect,450,200); ...
    ];

% ---- special params for test fingers ----
tf.ntrials = 40; %number of trials per block
tf.nblocks = 2;
tf.corResp(1,:) = [1 2 3 4]; %rule 1
tf.corResp(2,:) = [1 2 3 4]; %rule 2

p.tf = tf; %pass out



end