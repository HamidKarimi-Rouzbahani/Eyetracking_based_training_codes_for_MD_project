% parameters for experiment
% AKR 19.10 edited from AW 9/7/12
% a function for setting parameters for drawsquares experiment

function p = set_params_shortpredtrain(subNum)

if nargin<1
    subNum = 1;
end

%% -- trial information --
% p.nblocks = 10; %number of blocks
% p.ntrials = 80;
p.ITI = 500; %ITI in ms
p.waittime = 6000;
p.fixdur = 0.5; % seconds
p.stimdur = 0.5;
p.thresh = 60;

%% -- Screen info
p.ScreenNumber = max(Screen('Screens'));


[winWidth, winHeight] = Screen('DisplaySize', p.ScreenNumber); % width in mm
[pixwidth, pixheight]=Screen('WindowSize', p.ScreenNumber); % width in pixels


p.winWidth = winWidth;
p.winHeight = winHeight;
p.winpixx = pixwidth;
p.winpixy = pixheight;

Screen('Preference', 'SkipSyncTests', 1); %timing tests (value 1 to skip)
Screen('Preference', 'VisualDebugLevel', 0); %other screen tests (value determines level of debugging, higher is more debugging, 3 is recommended)

%% -- image positions --

% details for converting visual angle to pixels
viewingdistance = 600; % Distance between monitor and participant in mm
visang_rad = 2 * atan(winWidth/2/viewingdistance);
visang_deg = visang_rad * (180/pi);
mm_pervisang = winWidth / visang_deg;
%pix_pervisang = width/winWidth*mm_pervisang;
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
%xs(1) = xs(1) + 50;
%xs(2) = xs(2) - 15;
xs(1) = xs(1) + 30;
xs(2) = xs(2) - 15;

p.trainRects = [OffsetRect(centRect,-xs(1),-ys(1)); ...
    OffsetRect(centRect,-xs(2),-ys(2)); ...
    OffsetRect(centRect,xs(2),-ys(2)); ...
    OffsetRect(centRect,xs(1),-ys(1)); ...
    ];

%% fixation square
fixsize = 0.4*pix_pervisang;

p.fixRect = CenterRect([0 0 fixsize fixsize], screenRect); %central rectangle



%% -- colours --
p.black = [0 0 0];
%p.grey = [170 170 170];
p.grey = [.6765 .6635 .6746]*255;
p.darkgrey = [30 30 30];

p.white = [204 204 204]; %[255 255 255];
p.blue = [    0.3669    0.6588    0.9843]*255;%[0 0 255];
    

% these are at 50% saturation and 70% brightness, separated by 90
% degrees in Hue
% red = [204 122 122]; % 0 deg hue
% green = [163 204 122]; %90
% lblue = [122 204 204]; %180
% purple = [163 122 204]; %270

isoblue =  [0.5407    0.6381    0.95]*255;%[0.5407    0.6381    0.9316]*255;
isogreen=   [0.1375    0.85    0.4220]*255;%[0.1375    0.7972    0.4220]*255;
isoorange=    [0.7924    0.6899    0.2940]*255;
isopink =    [0.9821    0.5095    0.8689]*255;

% green =  [0.0138    0.8286    0.1192]*255;


%% -- Background colours for each rule --
% 2 colours per rule
% bgcols{1,1} = red; % col 1, rule 1
% bgcols{1,2} = lblue; % col 1 rule 2
% bgcols{2,1} = green; % col 2 rule 1
% bgcols{2,2} = purple; % col 2 rule 2
% p.cols= {'red' 'lblue'; 'green' 'purple'};

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
KbName('UnifyKeyNames')

p.kb_codes = KbName({'c' 'v' 'b' 'n'});%[67 86 66 78; ...%[6 25 5 17; ... %67 86 66 78; ... % 'c' v' 'b' 'n' (check with KnName(x))
 %   49 50 51 52]; %these (button box) may not be correct


%% ---- special params for learning and pratice version -----

% vary order or learning rules according to ptp

%     p.blockorder = [1 2 1 2 3]; % 1 = present rule; 2 = practice single rule; 3 = practice mixed rules
%
%     p.learnruleorder =  repmat( ...
%         [1 1 2 2 0; ...
%         2 2 1 1 0] ...
%         ,8,1); %index(subNum,block) which rule to use each block, ('1' or '2' in hard or easy block) 0 means mixed
%     p.learnrulename = [1 1 2 2 0]; % what to call each rule (always 1-2 in order you learn them!)
%
%     p.learntrials = [12 20 12 20 36]; %how many trials do you want for each block of the practice (min 4)
%

p.blockorder = [1 1 2 2 2]; % 1 = present rules; 2 = practice mixed rules
p.learnrulename = [0 0 1 2 3]; % what to call each rule (always 1-2 in order you learn them!)
p.learntrials = [16 16 32 32 32]; %how many trials do you want for each block of the practice (min 4)
p.prestime = [0 0 0 0 1]; % 0 = stay on until response, 1 = brief presentation
p.pos = [1 1 1 2 2]; % further apart positions, closer (test) positions
p.rules = [1 2 0 0 0];

% response box cue rectangles
p.resRects = [OffsetRect(centRect,-225,200); ...
    OffsetRect(centRect,-75,200); ...
    OffsetRect(centRect,75,200); ...
    OffsetRect(centRect,225,200); ...
    ];

% ---- special params for test fingers ----
tf.ntrials = 40; %number of trials per block
tf.nblocks = 2;
tf.corResp(1,:) = [1 2 3 4]; %rule 1
tf.corResp(2,:) = [1 2 3 4]; %rule 2

p.tf = tf; %pass out



end