% ARobinson 23 feb 2018 edited from AWoolgar July 2012
% changed from MEG original script: fewer trials!

% programme for teaching participants new set of rules in a fourlights
% framework


function train_res = trainsquares_shortpredtrain_cb()
eye_tracking=1;
% check for platform
if strcmp(computer, 'PCWIN')
    warning off MATLAB:DeprecatedLogicalAPI;
    warning off MATLAB:dispatcher:CaseInsensitiveFunctionPrecedesExactMatch;
    addpath(genpath('D:\BHPC_Files\Hamid\MD\Behavioural\'));
    outdir = 'D:\BHPC_Files\Hamid\MD\Behavioural\Data\';    
else
    addpath(genpath('D:\BHPC_Files\Hamid\MD\Behavioural\Data\'));
    addpath(genpath('D:\BHPC_Files\Hamid\MD\Behavioural\Data\'));
    outdir = 'D:\BHPC_Files\Hamid\MD\Behavioural\Data\';
end


disp('Welcome to the learn squares programme');
subNum = input('Pleave enter the participant Number (1-32): ');

clear Screen; % just in case
rng('shuffle'); %rand('twister',sum(100*clock)); % seed random number generator

% --- params ----
p = set_params_shortpredtrain(subNum); %collect parameters from set_params user script

% added for counterbalance
v_num = input('Version 1 or 2?: ');
if v_num == 2
    p.corResp = p.corResp([2 1],:);
end
p.version = v_num;

% save parameters
outmat = fullfile(outdir, ['trainsquares_p' sprintf('%0.3d', subNum) '_params.mat']);
save(outmat, 'subNum', 'p');

train_res = {};
train_res.p = p;
train_res.columns = {'Position' 'Rule' 'Cue colour' ...
    'Correct response' 'Response' 'Score' ...
    'Fixdisptime' 'Imageonset' 'Imageoffset' 'RT'};
save([outdir 'train_res_p' num2str(subNum) '.mat'], 'train_res');

% set up log file
logFID = fopen([outdir 'trainsquares_p' num2str(subNum) '.txt'],'at+');            % open a file as log file for everything (APPEND DATA)
fprintf(logFID,'SubjectID = %s\n',num2str(subNum));            % write name to logfile
fprintf(logFID,'Date = %s\n',datestr(clock));              % write date to logfile
fprintf(logFID,['Block\t'...
    'blocktype\t'...
    'trial\t'...
    'rule\t'...
    'bgcol\t' ...
    'cuecol\t'...
    'positioning\t'...
    'lengthoftime\t'...
    'stimpos\t'...
    'corresp\t'...
    'keypressed\t'...
    'response\t'...
    'fix_on\t'...
    'fixdur\t'...
    'picdur\t'...
    'RT\n']);

% keyboard version
practice = 1; % 1 if this is a practice

% welcome
disp('Welcome to the Learn Squares programme!');


if eye_tracking ==1
    % STEP 1
    % Added a dialog box to set your own EDF file name before opening
    % experiment graphics. Make sure the entered EDF file name is 1 to 8
    % characters in length and only numbers or letters are allowed.
    prompt = {'Enter tracker EDF file name (1 to 8 letters or numbers)'};
    dlg_title = 'Create EDF file';
    num_lines= 1;
    def     = {'DEMO'};
    answer  = inputdlg(prompt,dlg_title,num_lines,def);
    %edfFile= 'DEMO.EDF'
    edfFile = answer{1};
    fprintf('EDFFile: %s\n', edfFile );
    
    % STEP 2
    % Open a graphics window on the main screen
    % using the PsychToolbox's Screen function.
    screenNumber=max(Screen('Screens'));
%     screenNumber=2;
    [MainWindow, wRect]=Screen('OpenWindow', screenNumber, 0,[],32,2);
    Screen(MainWindow,'BlendFunction',GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    
    
    % STEP 3
    % Provide Eyelink with details about the graphics environment
    % and perform some initializations. The information is returned
    % in a structure that also contains useful defaults
    % and control codes (e.g. tracker state bit and Eyelink key values).
    el=EyelinkInitDefaults(MainWindow);
    dummymode=0;
    % STEP 4
    % Initialization of the connection with the Eyelink Gazetracker.
    % exit program if this fails.
    if ~EyelinkInit(dummymode)
        fprintf('Eyelink Init aborted.\n');
        cleanup;  % cleanup function
        return;
    end
    [v,vs]=Eyelink('GetTrackerVersion');
    fprintf('Running experiment on a ''%s'' tracker.\n', vs );
    
    % open file to record data to
    i = Eyelink('Openfile', edfFile);
    if i~=0
        fprintf('Cannot create EDF file ''%s'' ', edffilename);
        cleanup;
        %         Eyelink( 'Shutdown');
        return;
    end
    
    Eyelink('command', 'add_file_preamble_text ''Recorded by EyelinkToolbox demo-experiment''');
    [width, height]=Screen('WindowSize', screenNumber);
    
    
    % STEP 5
    % SET UP TRACKER CONFIGURATION
    % Setting the proper recording resolution, proper calibration type,
    % as well as the data file content;
    Eyelink('command','screen_pixel_coords = %ld %ld %ld %ld', 0, 0, width-1, height-1);
    Eyelink('message', 'DISPLAY_COORDS %ld %ld %ld %ld', 0, 0, width-1, height-1);
    % set calibration type.
    Eyelink('command', 'calibration_type = HV9');
    % set parser (conservative saccade thresholds)
    Eyelink('command', 'saccade_velocity_threshold = 35');
    Eyelink('command', 'saccade_acceleration_threshold = 9500');
    
    % remote mode possible add HTARGET ( head target)
    Eyelink('command', 'file_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON,INPUT');
    Eyelink('command', 'file_sample_data  = LEFT,RIGHT,GAZE,HREF,AREA,GAZERES,STATUS,INPUT,HTARGET');
    % set link data (used for gaze cursor)
    Eyelink('command', 'link_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON,INPUT,FIXUPDATE');
    Eyelink('command', 'link_sample_data  = LEFT,RIGHT,GAZE,GAZERES,AREA,STATUS,INPUT,HTARGET');
    
    % allow to use the big button on the eyelink gamepad to accept the
    % calibration/drift correction target
    Eyelink('command', 'button_function 5 "accept_target_fixation"');
    
    % Tell the Eyelink to send a fixation update every 50 ms
    Eyelink('command', 'fixation_update_interval = %d', 50);
    Eyelink('command', 'fixation_update_accumulate = %d', 50);
    
    % make sure we're still connected.
    if Eyelink('IsConnected')~=1
        cleanup;
        return;
    end
    
    % STEP 6
    % Calibrate the eye tracker
    % setup the proper calibration foreground and background colors
    el.backgroundcolour = 128;
    el.foregroundcolour = 0;
    % Hide the mouse cursor;
    Screen('HideCursorHelper', MainWindow);
    EyelinkDoTrackerSetup(el);
    
    
    %% STEP 7 Main expt
    % Now starts running individual trials;
    % You can keep the rest of the code except for the implementation
    % of graphics and event monitoring
    % Each trial should have a pair of "StartRecording" and "StopRecording"
    % calls as well integration messages to the data file (message to mark
    % the time of critical events and the image/interest area/condition
    % information for the trial)
    
end

% --- MAIN EXP LOOPS ----

% --- INIT! ----
% priorityLevel=MaxPriority(['GetSecs'],['KbCheck'],['KbWait'],['GetClicks']);

%initialise keyboard, allocate variables
KbCheck(-1);


%open screen window
[MainWindow,~] = Screen('OpenWindow', p.ScreenNumber, p.darkgrey); % (MainWindow var is like a file handle)
Screen('TextSize', MainWindow, 28);
HideCursor;
ListenChar(2) %suppress keyboard inputs to matlab

%warning
DrawFormattedText(MainWindow, 'Get ready...', 'center', 'center', p.black); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
Screen('Flip', MainWindow);
WaitSecs(2);

if eye_tracking==1
    % mark zero-plot time in data file
    Eyelink('Message', 'RunStartTime');
end

% -- BLOCK LOOP!! --
blk = 1;
blkstop = 0;
%numb4 = 0;
blkcount = 0;
trialcount = 0;
while blkstop == 0 % continue blocks until blk  > 4 and good enough performance
    blkcount = blkcount+1;
    
    if eye_tracking == 1
        % STEP 7.1
        % Sending a 'TRIALID' message to mark the start of a trial in Data
        % Viewer.  This is different than the start of recording message
        % START that is logged when the trial recording begins. The viewer
        % will not parse any messages, events, or samples, that exist in
        % the data file prior to this message.
        Eyelink('Message', 'BlockID %d', blkcount);
        
        % This supplies the title at the bottom of the eye_tracking display
        Eyelink('command', 'record_status_message "Block %d"', blkcount);
        % Before recording, we place reference graphics on the host display
        % Must be offline to draw to EyeLink screen
        Eyelink('Command', 'set_idle_mode');
        % clear tracker display and draw box at center
        Eyelink('Command', 'clear_screen 0')
        Eyelink('command', 'draw_box %d %d %d %d 15', width/2-50, height/2-50, width/2+50, height/2+50);
        
        
        % STEP 7.2
        % Do a drift correction at the beginning of each trial
        % Performing drift correction (checking) is optional for
        % EyeLink 1000 eye trackers.
        %       % EyelinkDoDriftCorrection(el);
        
        % STEP 7.3
        % start recording eye position (preceded by a short pause so that
        % the tracker can finish the mode transition)
        % The paramerters for the 'StartRecording' call controls the
        % file_samples, file_events, link_samples, link_events availability
        Eyelink('Command', 'set_idle_mode');
        WaitSecs(0.05);
        
        %         % tracker starts recording!
        Eyelink('StartRecording');
        % record a few samples before we actually start displaying
        % otherwise you may lose a few msec of data
        WaitSecs(0.1);
        
        % get eye that's tracked
        % returns 0 (LEFT_EYE), 1 (RIGHT_EYE) or 2 (BINOCULAR) depending on what data is
        eye_used = Eyelink('EyeAvailable');
        if eye_used == 2
            eye_used = 1; % use the right-eye data
        end
        eyelinkerror=Eyelink('CheckRecording');
        if(eyelinkerror~=0)
            break;
        end
    end
        
    % break out of block if esc is pressed
    [~, ~, keyCode]=KbCheck(-1); % can remove once collecting responses!
    if strcmp(KbName(find(keyCode)),'ESCAPE');
        if eye_tracking==1
            Eyelink('Message', 'Broke out of Block by experimenter');
        end
        break;
    end
    
    % display upcoming rules
    CueRules(MainWindow, p);
    if eye_tracking==1
        Eyelink('Message', 'CuedRules');
    end
    WaitSecs(2);
    KbWait(-1);

    % block parameters
    blkType = p.blockorder(blk);  % block type  1 = present rules; 2 = practice mixed rules
    tim = p.prestime(blk); % timing of presentation: 0 = until response, 1= brief square presentation
    

    positioning = p.pos(blk); % position of squares - 1: training, 2: testing
    switch positioning
        case 1
            rects = p.trainRects;
        case 2
            rects = p.testRects;
    end
    
    
    ntrials = p.learntrials(blk);
    

    
    % reset RT and score counters
    allRT = zeros(ntrials,1);
    allscore = zeros(ntrials,1);
    
    clear trials rules
    switch blkType
        case 1 % just showing the rules (first block)
            msg = ['You will now learn rule ' num2str(p.rules(blk))];
            clear trials
            clear rules
            trials = repmat(1:4,1,ntrials/4); % order of trials - POSITION
            rules = [repmat(p.rules(blk),1,ntrials); repmat([1 2],1,ntrials/2)]; % rule is rules(1,:), cue is rules(2,:)
            
        case 2
            tt=0; % make rule order
            for po = 1:4
                for ru = 1:2
                    for cu = 1:2
                        for nt =1:(ntrials/(4*2*2))
                            tt=tt+1;
                            trials(tt) = po;
                            rules(1,tt) = ru;
                            rules(2,tt) = cu;
                        end
                    end
                end
            end
            
            % now randomize
            randidx = randperm(length(trials));
            trials = trials(randidx); % randomize trials
            rules = rules(:,randidx); % rule 1/2, col 1/2
            if positioning == 1
                msg = 'Have a practice of the two rules together!';
            else
                if tim == 0
                    msg = 'The squares will be close together: have a practice!';
                else
                    msg = 'The squares will only appear for a short time: have a practice!';
                end
            end
    end
    
    % save details to mat
    train_res.blocktype(blkcount) = blkType;
    train_res.prestime(blkcount) = tim;
    train_res.positioning(blkcount) = positioning;
    train_res.ntrials(blkcount) = ntrials;
    train_res.pos.blkcount = trials;
    save([outdir 'train_res_p' num2str(subNum) '.mat'], 'train_res');
    
    
    
    DrawFormattedText(MainWindow, msg, 'center', 'center', p.black); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
    Screen('Flip', MainWindow); % show block start message
    if eye_tracking==1
        Eyelink('Message', 'BlockStartMessage');
    end
    WaitSecs(1.5);
    
    DrawFormattedText(MainWindow, 'Press space to start', 'center', 'center', p.black); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
    Screen('Flip', MainWindow); % show get ready message
    if eye_tracking==1
        Eyelink('Message', 'ShowReadyMessage');
    end
    KbWait(-1); % wait for key press
    
    % ITI before first stim
    Screen('Flip', MainWindow); % show nothing
    WaitSecs(p.ITI/1000);
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%
    % -- main trial loop ---
    %%%%%%%%%%%%%%%%%%%%%%%%
    
    endblock = 0;
    for t = 1:ntrials %for set number of trials
        trialcount = trialcount+1;
        % determine trial parameters
        pos = trials(t);
        rule = rules(1,t); %randomly select which rule to use on this trial
        col = rules(2,t);
        cuecol = p.cols{col,rule};
        bgcol = p.bgcols{col, rule}; %pick fixation cross colour for this trial based on rule
        rect = rects(pos,:); %pick the rectangle position you need to fill on the basis of trial type
        
        cres = p.corResp(rule, pos); %calculate correct response to expect
        
        
        % draw fixation cross
        Screen('TextSize', MainWindow, 50);
        %         DrawFormattedText(MainWindow, '+', 'center','center',p.black);
        Screen('FillRect', MainWindow, p.black, p.fixRect); % colour square
        
        fixdisptime = Screen('Flip', MainWindow); %% display fixation cross for fixdur duration
        if eye_tracking==1
            Eyelink('Message', 'FixationCross');
        end
        % prepare stimulus screen
        %         DrawFormattedText(MainWindow, '+', 'center','center',bgcol);
        Screen('FillRect', MainWindow, bgcol, p.fixRect); % colour square
        
        % display reponse box cues if learning block
        if blkType == 1
            Screen('FillRect', MainWindow, p.white, p.resRects'); % response box squares
            Screen('FillRect', MainWindow, p.white, p.trainRects'); % actual images (ARob 20/10)
        end
        
        % draw stimulus rectangle
        Screen('FillRect', MainWindow, p.black, rect); % colour square
        [VBLTimestamp, StimulusOnsetTime] = Screen('Flip', MainWindow,fixdisptime+p.fixdur-.01);
        if eye_tracking==1
            Eyelink('Message', 'Block_Stimulus %d %d', blkcount, t);
        end
        
        if blkType == 1 % if a learn block, also show the answer!
            ansRect = p.resRects(p.corResp(rule, pos),:); %correct answer rectangle
            ShowAnswer(MainWindow, rect, ansRect, rects,p,bgcol);
        end
        
        if tim==1 % if short image presentation (last block)
            % DrawFormattedText(MainWindow, '+', 'center','center',p.black);
            Screen('FillRect', MainWindow, p.black, p.fixRect); % colour square
            
            blanktime = Screen('Flip', MainWindow,VBLTimestamp+p.stimdur-.01); % show blank screen after p.stimdur
        end
        
        [keypress, RT, ~] = WaitforResponse(StimulusOnsetTime, p.waittime); %keypress is a funny thing. It's a number (eg 86, but if you print it to a string using sprintf, it's also a letter, e.g. 'V', according to which key was pressed, but num2str(resp) gives you the string of the number (ie '86')
        fliptime = Screen('Flip', MainWindow,VBLTimestamp+p.stimdur-.01); % show blank screen after p.stimdur
        if eye_tracking==1
            Eyelink('Message', 'Blank');
        end
        if tim == 0 % long image presentation
            blanktime = fliptime;
        end
        
        
        % check the response
        if keypress == 0 %didn't respond
            resp = 0; % didn't respond
            score = 0; % didn't respond
            RT = NaN;
        elseif keypress == 999 %responded but pressed 2 keys at once
            resp = 999; % pressed 2 buttons
            score = 0; % score as incorrect
        else
            if find(p.kb_codes(1,:)== keypress)
                
                resp = find(p.kb_codes(1,:)== keypress); % translate key press into response number 1-4
                score = resp == cres; % score 1 or 0, if response matches correct response
            else
                resp = keypress;
                score = 0;                
            end
            if eye_tracking==1
                Eyelink('Message', 'Response %d', keypress);
            end
        end
        
        % tally score and RT
        allscore(t) = score; %tally score
        if keypress ~=0; allRT(t)=RT; end %if they responded, tally RT
        
        if practice %if this is a practice, show feedback
            ShowFeedback(MainWindow, score, p.corResp(rule, pos));
            if score == 0
                
                ansRect = p.resRects(p.corResp(rule, pos),:); %correct answer rectangle
                %                DrawFormattedText(MainWindow, '+', 'center','center',bgcol);
                Screen('FillRect', MainWindow, bgcol, p.fixRect); % colour square
                
                ShowAnswer(MainWindow,rect,ansRect,rects,p,bgcol);
%                 [nkeypress, ~, ~] = WaitforResponse(GetSecs(), 2000);
                WaitforResponse(GetSecs(), 2000);
                
%                 % check the new response
%                 if nkeypress ~= 0
%                     nresp = find(p.kb_codes(buttonbox + 1,:)== nkeypress); % translate key press into response number 1-4
%                 else
%                     nresp = 0; % didn't respond (again!)
%                 end
%                 nRT = NaN;  % not timing second trys!
%                 nscore = NaN; % not scoring second trys!
            end
        end
        
        Screen('TextSize', MainWindow, 28);
        
        
        % ITI
        WaitSecs(p.ITI/1000);
        
        %%% print to logfile:
        fprintf(logFID,['%d\t%d\t%d\t%d\t' ...
            '%d\t%s\t%d\t%d\t'...
            '%d\t%d\t%d\t%d\t'...
            '%.3f\t%.3f\t%.3f\t%.3f\n'], ...
            blk,blkType,t,rule, ...
            col,cuecol,positioning,tim, ...
            pos,cres,resp,score, ...
            fixdisptime,VBLTimestamp-fixdisptime,blanktime-VBLTimestamp,RT);
        


        train_res.allresults(trialcount,:) = [pos rule col cres resp score fixdisptime VBLTimestamp blanktime RT];
        train_res.cuecol{trialcount} = cuecol;
        save([outdir 'train_res_p' num2str(subNum) '.mat'], 'train_res');
        
        if endblock == 1
            break
        end
        
    end
    
    WaitSecs(.5);
    DrawFormattedText(MainWindow, 'End of Block... calculating scores', 'center', 'center', p.black); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
    Screen('Flip', MainWindow);
    
    %end of block feedback
    corr = round(sum(allscore)/length(allscore)*100); % percent correct
    if  corr > p.thresh
        msg = sprintf('%s\n\n%s\n\n%s', ['You scored ' num2str(corr) ' %'], ...
            ['Mean reaction time: ' num2str(round(mean(allRT)*1000)) ' ms'], ...
            'Well done!');
        if blk == 5
%             numb4 = numb4 +1; % how many times done block 4
%             if numb4 == 3 % if done block 4 x 3 times above 70%, end experiment
                blkstop = 1;
%             end
        else
            blk = blk + 1; % move to next block
        end
    else
        msg = sprintf('%s\n\n%s\n\n%s', ['You scored ' num2str(corr) ' %'], ...
            ['Mean reaction time: ' num2str(round(mean(allRT)*1000)) ' ms'], ...
            'Let''s try again!');
    end
    if eye_tracking==1
        Eyelink('Message', 'EndOfBlock');
    end
    
    DrawFormattedText(MainWindow, msg, 'center', 'center', p.black); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
    DrawFormattedText(MainWindow, 'Wait for experimenter', 'center', 800, p.black); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
    
    Screen('Flip', MainWindow);
    %     WaitSecs(3);
    KbWait(-1);
    
    if blkstop == 0
        DrawFormattedText(MainWindow, 'press any key to continue', 'center', 'center', p.black); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
        Screen('Flip', MainWindow);
        if eye_tracking==1
            Eyelink('Message', 'PressKeyToContinue');
        end       
        KbWait(-1);% Wait for a key press
        [~, ~, keylist] = KbCheck(-1);
        key_code = find(keylist);
        if key_code == KbName('ESCAPE')
            ShowCursor;
            clear screen;
            Screen('CloseAll')
            ListenChar(0); %restore keyboard communication with matlab!
        end
    else
        sca
        fclose('all');
        ListenChar(0);
    end
    
    if eye_tracking==1
        % stop the recording of eye-movements for the current block.
        % recommended to put this right after Block_RESULT
        Eyelink('StopRecording');
        
        
        % STEP 7.7
        % Send out necessary integration messages for data analysis
        % Send out interest area information for the block
        % See "Protocol for EyeLink Data to Viewer Integration-> Interest
        % Area Commands" section of the EyeLink Data Viewer User Manual
        % IMPORTANT! Don't send too many messages in a very short period of
        % time or the EyeLink tracker may not be able to write them all
        % to the EDF file.
        % Consider adding a short delay every few messages.
        WaitSecs(0.001);
        Eyelink('Message', '!V IAREA ELLIPSE %d %d %d %d %d %s', 1, width/2-50, height/2-50, width/2+50, height/2+50,'center');
        Eyelink('Message', '!V IAREA RECTANGLE %d %d %d %d %d %s', 2, width/4-50, height/2-50, width/4+50, height/2+50,'left');
        Eyelink('Message', '!V IAREA RECTANGLE %d %d %d %d %d %s', 3, 3*width/4-50, height/2-50, 3*width/4+50, height/2+50,'right');
        Eyelink('Message', '!V IAREA RECTANGLE %d %d %d %d %d %s', 4, width/2-50, height/4-50, width/2+50, height/4+50,'up');
        Eyelink('Message', '!V IAREA RECTANGLE %d %d %d %d %d %s', 5, width/2-50, 3*height/4-50, width/2+50, 3*height/4+50,'down');
        
        % Send messages to report trial condition information
        % Each message may be a pair of trial condition variable and its
        % corresponding value follwing the '!V TRIAL_VAR' token message
        % See "Protocol for EyeLink Data to Viewer Integration-> Trial
        % Message Commands" section of the EyeLink Data Viewer User Manual
        WaitSecs(0.001);
        Eyelink('Message', '!V Block Number = %d' , blkcount)
        
        % STEP 7.8
        % Sending a 'BLOCK_RESULT' message to mark the end of a trial in
        % Data Viewer. This is different than the end of recording message
        % END that is logged when the trial recording ends. The viewer will
        % not parse any messages, events, or samples that exist in the data
        % file after this message.
        Eyelink('Message', 'BLOCK_RESULT 0');
    end
end %block loop

if eye_tracking
    % STEP 8
    % End of Experiment; close the file first
    % close graphics window, close data file and shut down tracker
    
    Eyelink('Command', 'set_idle_mode');
    WaitSecs(0.5);
    Eyelink('CloseFile');
    
    % download data file
    try
        fprintf('Receiving data file ''%s''\n', edfFile );
        status=Eyelink('ReceiveFile');
        if status > 0
            fprintf('ReceiveFile status %d\n', status);
        end
        if 2==exist(edfFile, 'file')
            fprintf('Data file ''%s'' can be found in ''%s''\n', edfFile, pwd );
        end
    catch
        fprintf('Problem receiving data file ''%s''\n', edfFile );
    end
    
    % STEP 9
    % run cleanup function (close the eye tracker and window).
    cleanup;
    Eyelink('ShutDown');
    Screen('CloseAll');
end

%clean up
Screen('CloseAll');
fclose('all');
ListenChar(0); %restore keyboard communication with matlab!

end

function cleanup
% Shutdown Eyelink:
Eyelink('Shutdown');

% Close window:
sca;
commandwindow;
end

function ShowFeedback(MainWindow, score, corResp)

Screen('TextSize', MainWindow, 28);

if score == 1
    msg = 'correct';
    %         Screen('FillRect', MainWindow, [204 204 204]); %bg white
    DrawFormattedText(MainWindow, msg, 'center', 'center', [0 0 0]); %msg in black
    Screen('Flip', MainWindow);
    WaitSecs(0.5);
else
    msg = ['INCORRECT! The correct answer was ' num2str(corResp)];
    %         Screen('FillRect', MainWindow, [204 204 204]); %bg white
    DrawFormattedText(MainWindow, msg, 'center', 'center', [0 0 0]); %msg in black
    Screen('Flip', MainWindow);
    WaitSecs(1.5);
end
Screen('TextSize', MainWindow, 50);
end

function ShowAnswer(MainWindow, rect, ansRect,rects, p,bgcol)
fromX = mean([rect(1) rect(3)]); % middle of stim box
fromY = rect(4) + 40; %below the box
toX = mean([ansRect(1) ansRect(3)]); %middle of answer box
toY = ansRect(2) - 40;

%DrawFormattedText(MainWindow, '+', 'center','center',bgcol);
Screen('FillRect', MainWindow, bgcol, p.fixRect); % colour square

Screen('FillRect', MainWindow, p.white, p.resRects'); % response box squares
Screen('FillRect', MainWindow, p.white, rects'); % actual box squares
Screen('FillRect', MainWindow, p.black, rect); % colour square
% DrawFormattedText(MainWindow, '+', 'center','center');
Screen('FillRect', MainWindow, bgcol, p.fixRect); % colour square

Screen('Flip', MainWindow);
WaitSecs(0.5);

% DrawFormattedText(MainWindow, '+', 'center','center',bgcol);
Screen('FillRect', MainWindow, bgcol, p.fixRect); % colour square

Screen('FillRect', MainWindow, p.white, p.resRects'); % response box squares
Screen('FillRect', MainWindow, p.white, rects'); % actual box squares
Screen('FillRect', MainWindow, p.black, rect); % colour square
Screen('DrawLine', MainWindow, p.black, fromX, fromY, toX, toY);
% DrawFormattedText(MainWindow, '+', 'center','center');
Screen('FillRect', MainWindow, bgcol, p.fixRect); % colour square

Screen('Flip', MainWindow);
WaitSecs(0.5);

% DrawFormattedText(MainWindow, '+', 'center','center',bgcol);
Screen('FillRect', MainWindow, bgcol, p.fixRect); % colour square

Screen('FillRect', MainWindow, p.white, p.resRects'); % response box squares
Screen('FillRect', MainWindow, p.white, rects'); % actual box squares
Screen('FillRect', MainWindow, p.black, rect); % colour square
Screen('DrawLine', MainWindow, p.black, fromX, fromY, toX, toY);
Screen('FillRect', MainWindow, p.black, ansRect); % colour correct response box
Screen('Flip', MainWindow);
end


function [resp, RT, ToR] = WaitforResponse(StimulusOnsetTime, delay)
% global buttonbox bbhandle
%keyCode bbresp scannerstart bbhandle StimulusOnsetTime

% oldresp = 0;
resp = 0;
RT = 0;


while (GetSecs - StimulusOnsetTime)<=delay/1000 % while loop until subject responds, up to max delay time
    
%     if buttonbox
%         data = IOPort('Read',bbhandle); %read button box's buffer
%         if data & data ~=53 & data~=oldresp %ignore TTL pulses % if there is any data
%             if length(data) > 1 %they have pressed more than one button at once, therefore score as incorrect
%                 ToR = GetSecs(); % Time of Response
%                 RT = ToR - StimulusOnsetTime;
%                 oldresp = data; % should not be needed (should never do this loop twice)
%                 resp = 999; % resp = 999 for answered but pressed 2 buttons (score as incorrect)
%                 break
%             elseif sum([67 86 66 78] == data); % else, if the data is drawn from the set of bb responses
%                 %if data & data ~=53 & data~=oldresp %ignore TTL pulses, ignore same button still held down
%                 ToR = GetSecs(); % Time of Response
%                 RT = ToR - StimulusOnsetTime;
%                 oldresp = data; % should not be needed (should never do this loop twice)
%                 resp = data;
%                 break
%             end
%         end
%     else %if keyboard
        [KeyIsDown, ~, keyCode]=KbCheck(-1); %check the keyboard
        if sum(keyCode)==1 %if keyCode is not empty (this happens if they press 2 buttons at once)
            if KeyIsDown% && sum([67 86 66 78] == find(keyCode))%(find(keyCode) ~= oldresp) && (find(keyCode) ~= 32) %ignore space bar responses
                ToR = GetSecs(); % Time of Response
                RT = ToR - StimulusOnsetTime;
%                 oldresp = find(keyCode); % again probably not needed
                resp = find(keyCode);
                break
            end
        end
%     end
    
    WaitSecs(0.001);
end %end while

if resp ==0; ToR = StimulusOnsetTime + delay; end %if you get to the end without a response, record current time instead of time of response

end


function CueRules(MainWindow, p)


bgd = ones(p.winpixx,p.winpixy);


rule1col1 = Screen('MakeTexture', MainWindow, bgd');
Screen('TextSize', rule1col1, 60);
Screen('FillRect', rule1col1, p.darkgrey,[100 100 p.winpixx-100 p.winpixy-100]); % background
Screen('FillRect', rule1col1, p.grey, p.testRects'); % stim squares
Screen('FillRect', rule1col1, p.grey, p.resRects'); % stim squares

% rule 1 colour 2
rule1col2 = Screen('MakeTexture', MainWindow, bgd');
Screen('TextSize', rule1col2, 60);
Screen('FillRect', rule1col2, p.darkgrey,[100 100 p.winpixx-100 p.winpixy-100]); % background
Screen('FillRect', rule1col2, p.grey, p.testRects'); % stim squares
Screen('FillRect', rule1col2, p.grey, p.resRects'); % stim squares

% rule 2 col 1
rule2col1 = Screen('MakeTexture', MainWindow, bgd');
Screen('TextSize', rule2col1, 60);
Screen('FillRect', rule2col1, p.darkgrey,[100 100 p.winpixx-100 p.winpixy-100]); % background
Screen('FillRect', rule2col1, p.grey, p.testRects'); % stim squares
Screen('FillRect', rule2col1, p.grey, p.resRects'); % stim squares

% rule 2 colour 2
rule2col2 = Screen('MakeTexture', MainWindow, bgd');
Screen('TextSize', rule2col2, 60);
Screen('FillRect', rule2col2, p.darkgrey,[100 100 p.winpixx-100 p.winpixy-100]); % background
Screen('FillRect', rule2col2, p.grey, p.testRects'); % stim squares
Screen('FillRect', rule2col2, p.grey, p.resRects'); % stim squares

for pos = 1:4 %for each stimulus
    stimRect =p.testRects(pos,:);
    fromX = mean([stimRect(1) stimRect(3)]); % middle of stim box
    fromY = stimRect(4) + 10; %below the box
    
    ansRect1 = p.resRects(p.corResp(1, pos),:); %(rule, pos)
    toX1 = mean([ansRect1(1) ansRect1(3)]); %middle of answer box
    toY1 = ansRect1(2) - 20;
    Screen('DrawLine', rule1col1, p.white, fromX, fromY, toX1, toY1,3);
    Screen('DrawLine', rule1col2, p.white, fromX, fromY, toX1, toY1,3);
    
    ansRect2 = p.resRects(p.corResp(2, pos),:); %(rule, pos)
    toX2 = mean([ansRect2(1) ansRect2(3)]); %middle of answer box
    toY2 = ansRect2(2) - 20;
    Screen('DrawLine', rule2col1, p.white, fromX, fromY, toX2, toY2,3);
    Screen('DrawLine', rule2col2, p.white, fromX, fromY, toX2, toY2,3);
end

% DrawFormattedText(rule1col2, '+', 'center','center',p.bgcols{2,1});
% DrawFormattedText(rule1col1, '+', 'center','center',p.bgcols{1,1});
% DrawFormattedText(rule2col2, '+', 'center','center',p.bgcols{2,2});
% DrawFormattedText(rule2col1, '+', 'center','center',p.bgcols{1,2});
Screen('FillRect', rule1col2, p.bgcols{2,1}, p.fixRect); % colour square
Screen('FillRect', rule1col1, p.bgcols{1,1}, p.fixRect); % colour square
Screen('FillRect', rule2col2, p.bgcols{2,2}, p.fixRect); % colour square
Screen('FillRect', rule2col1, p.bgcols{1,2}, p.fixRect); % colour square


Screen('DrawTexture',MainWindow,rule1col1,[],[0 0 p.winpixx/2 p.winpixy/2])
Screen('DrawTexture',MainWindow,rule1col2,[],[0 p.winpixy/2 p.winpixx/2 p.winpixy])
Screen('DrawTexture',MainWindow,rule2col1,[],[p.winpixx/2 0 p.winpixx p.winpixy/2])
Screen('DrawTexture',MainWindow,rule2col2,[],[p.winpixx/2 p.winpixy/2 p.winpixx p.winpixy])

DrawFormattedText(MainWindow, 'Here are the rules: press any key to start', 'center', 'center', p.grey); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
Screen('Flip', MainWindow);


end