% HKarimi Apr 2019 edited from ARob Oct 2017 edited from A Woolgar July 2012
% script for testing rules in MEG

% factors:
% two rules, each with different stimulus-response rules (rule 1 is
% opposite of rule 2)
% four stimulus positions: pos 1 and 2 overlap, and pos 3 and 4
% overlap: position uncertainty
% based on responses, can tell if participants made correct response, rule
% error, stimulus error or unspecified error



function testsquares_shortpredtraining_v2

global buttonbox bbhandle

trigger_from_scanner = 1;
isMegExp = 0;
buttonbox=1;
whichcomp = 1; % 1 is my laptop, 2 is MQ, 3 is MEG
eye_tracking= 1;

if whichcomp == 1
    addpath(genpath('/Applications/Psychtoolbox'));
    outdir = 'T:\BHPC_Files\fMRI_exp_Hamid\data\';
elseif whichcomp == 2
    addpath(genpath('~/Documents/Psychtoolbox'));
    outdir = 'T:\BHPC_Files\fMRI_exp_Hamid\data\';
elseif whichcomp == 3
    addpath('T:\BHPC_Files\fMRI_exp_Hamid\data\');
    outdir = 'T:\BHPC_Files\fMRI_exp_Hamid\data\';
end


RestrictKeysForKbCheck([]);


% input
disp('Welcome to the test rules programme');
subNum = input('Please enter the participant number: ');

% vars needed for multiple functions
if buttonbox
    IOPort('CloseAll');
    try
        [bbhandle, errmsg] = IOPort('OpenSerialPort', 'COM6');
    catch
        error('Couldn''t connect to serial port (bbhandle < 0)');
    end
end


clear Screen; % just in case
rng('shuffle'); %rand('twister',sum(100*clock)); % seed random number generator

% --- params ----
p = set_params_test(subNum); %collect parameters from set_params user script


% added for counterbalance
v_num = input('Version 1 or 2?: ');
if v_num == 2
    p.corResp = p.corResp([2 1],:);
end
p.version = v_num;


test_res = {};
test_res.p = p;
test_res.columns = {'Position' 'Rule' 'Cue colour' ...
    'Correct response' 'Response' 'Score' ...
    'Fixdisptime' 'Imageonset' 'RunStartTime' 'Imageoffset' 'RT'};
save([outdir 'test_res_p' num2str(subNum) '.mat'], 'test_res');

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


%ignore beam position errors
Screen('Preference', 'SkipSyncTests', 0);


% --- output file(s) ---
% set up log file
logFID = fopen(['testsquares_p' num2str(subNum) '.txt'],'at+');            % open a file as log file for everything (APPEND DATA)
fprintf(logFID,'SubjectID = %s\n',num2str(subNum));       % write name to logfile
fprintf(logFID,'Start time = %.3f\n',GetSecs);            % write name to logfile

fprintf(logFID,'Date = %s\n',datestr(clock));             % write date to logfile
fprintf(logFID,['Block\t'...
    'trial\t'...
    'rule\t'...
    'bgcol\t' ...
    'stimpos\t'...
    'corresp\t'...
    'keypressed\t'...
    'response\t'...
    'fix_on\t'...
    'fixdur\t'...
    'picdur\n']);


% --- MAIN EXP LOOPS ----

%initialise keyboard, allocate variables
[~, ~, keyCode]=KbCheck;

%open screen window
MainWindow= Screen('OpenWindow', p.ScreenNumber, p.darkgrey); % (MainWindow var is like a file handle)

Screen('TextSize', MainWindow, 28);
HideCursor;
% ListenChar(2) %suppress keyboard inputs to matlab

% %warning
% DrawFormattedText(MainWindow, 'Get ready...', 'center', 'center', p.grey); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
% Screen('Flip', MainWindow);
% WaitSecs(2);

RestrictKeysForKbCheck([KbName('space') p.kb_codes]);

if isMegExp
    %%% set up triggering
    trig = 176; %trigger to use
    resptrig = 180;
    triggerduration = 0.05; %in seconds
    
    % set up parallel port and trigger support
    [ioObj,address] = MQinitPP;
    
    %loop and send some triggers, while storing the timestamps and numbers
    triggertime = [];
    triggernumber = [];
    starttime = now();
    for t=1
        WaitSecs(.5+rand);
        triggertime(end+1) = now-starttime;
        triggernumber(end+1) = trig;
        fprintf('%.3fs Sending trigger %i\n',100000*triggertime(end),triggernumber(end));
        MQsendtrigger(ioObj,address,triggernumber(end),triggerduration);
    end
end

% -- RUN LOOP!! --
bc=0;
for run = 1:p.nruns
    
    DrawFormattedText(MainWindow, 'We will start the experiment,\n\n\n get ready!!!', 'center', 'center', p.grey); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
    Screen('Flip', MainWindow); % show block start message
    disp(['\n\n\n ###!!!   Please press the SPACE bar to start run ',num2str(run),'!!!###\n\n\n']);
    
    RestrictKeysForKbCheck(KbName('space'));
    KbWait(-1);
    WaitSecs(1);
    RestrictKeysForKbCheck([KbName('space') p.kb_codes]);
    Screen('FillRect',MainWindow,[0 255 0],[1 1 p.winpixx p.winpixy])
    Screen('Flip', MainWindow); % show block start message
    if trigger_from_scanner
        trigger = IOPort('Read',bbhandle); %read button box's buffer
        trigger = []; %read button box's buffer
        start_time=GetSecs;
        while (GetSecs-start_time)<=600
            trigger = IOPort('Read',bbhandle); %read button box's buffer
            if length(trigger)==1 & trigger ==84 %ignore TTL pulses % if there is any data
                Run_start_time=GetSecs;
                if eye_tracking==1
                    % mark zero-plot time in data file
                    Eyelink('Message', 'RunStartTimeByScannerTrigger');
                end
                break;
            end
            WaitSecs(0.001);
        end
        if trigger~=84
            error('No Scanner Pulse received !!!');
        end
    else
        KbWait(-1); % this is where I get the fMRI trigger
        Run_start_time=GetSecs;
        if eye_tracking==1
            % mark zero-plot time in data file
            Eyelink('Message', 'RunStartTime');
        end
    end
    WaitSecs(1);
    
            
    % -- BLOCK LOOP!! --
    for blk = 1:p.nblocks
        if eye_tracking == 1
            % STEP 7.1
            % Sending a 'TRIALID' message to mark the start of a trial in Data
            % Viewer.  This is different than the start of recording message
            % START that is logged when the trial recording begins. The viewer
            % will not parse any messages, events, or samples, that exist in
            % the data file prior to this message.
            Eyelink('Message', 'RunID_BlockID %d%d', run,blk);
            
            % This supplies the title at the bottom of the eye_tracking display
            Eyelink('command', 'record_status_message "Run %d/%d Block %d/%d"', run, p.nruns,blk, p.nblocks);
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
            
            %         %% fixation-dependent proceeding
            %         % fixation on cross for a certain time to proceed to search display
            %         %         proceedTime = GetSecs + 10;
            %         totalFixTime=0;
            %         Fixation_assurance_time = 3;
            %         radius=100;
            %
            %         % Draw fixation cross
            %         Screen('FillOval',w,[70 70 70], Obstacle_1);
            %         Screen('Flip', w);
            %         Eyelink('Message', 'FixationAppeared');
            %
            %         while totalFixTime < Fixation_assurance_time
            %
            %             % if using eye-tracker
            %             error=Eyelink('CheckRecording');
            %             if(error~=0)
            %                 disp('Error in Recording');
            %                 break;
            %             end
            %             % we need to loop over this a few times (30 is
            %             % randomly chosen) so that we do not miss any events
            %             % and to prevent any buffer overflow
            %             for j=1:30
            %                 evtype = Eyelink('GetNextDataType');
            %                 if evtype == el.FIXUPDATE
            %                     if Eyelink('isconnected') == el.connected % check if eye-tracker is connected
            %                         evt = Eyelink('getfloatdata', evtype);% get data
            %                         % only process if its the desired eye &&
            %                         % within ROI
            %                         if evt.eye == eye_used
            %                             if sqrt((evt.gavx-width/2)^2+(evt.gavy-height/2)^2)<radius
            %                                 totalFixTime = totalFixTime + 3; % make it consistent with fixupdate rate (1/50ms)
            %                                 if totalFixTime >= Fixation_assurance_time % if fixation beyond threshold
            %                                     disp('fixation done correctly!');
            %                                     break;
            %                                 else % reset if no fixation
            %                                     totalFixTime = 0;
            %                                 end
            %                             end
            %                         end
            %                     else
            %                         disp('Eyelink disconnected!');
            %                     end
            %                 end
            %             end
            %         end
            %     end
            eyelinkerror=Eyelink('CheckRecording');
            if(eyelinkerror~=0)
                break;
            end
        end
        %BREAK OUT OF BLOCK LOOP USING ESP:
        [~, ~, keyCode]=KbCheck;
        if strcmp(KbName(find(keyCode)),'esc');
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
        WaitSecs(10);
        %         KbWait(-1)
        
        DrawFormattedText(MainWindow, 'Start!', 'center', 'center', p.grey); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
        Screen('Flip', MainWindow); % show block start message
        if eye_tracking==1
            Eyelink('Message', 'Start Command');
        end
        WaitSecs(2);
        
        %         DrawFormattedText(MainWindow, 'Press any key to start', 'center', 'center', p.grey); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
        %         Screen('Flip',MainWindow); % show get ready message
        %         KbWait(-1) % wait for key press
        
        % ITI before first stim
        Screen('Flip', MainWindow);
        WaitSecs(p.ITI/1000);
        
        trials = repmat(1:4,1,p.ntrials/4); % order of trials
        rules = [repmat(1:2,1,p.ntrials/2); repmat([1 1 2 2],1,p.ntrials/4)]; % rule is rules(1,:), cue is rules(2,:)
        
        x=0;
        for t = 1:4
            for r = 1:2
                for c = 1:2
                    for nreps = 1:(p.ntrials/16)
                        x=x+1;
                        trials(x) = t;
                        rules(1,x) = r;
                        rules(2,x) = c;
                    end
                end
            end
        end
        
        
        ran = randperm(length(trials)); % randomize trials
        
        trials = trials(ran);
        rules = rules(:,ran); % rule 1/2, col 1/2
        
        
        % save details to mat
        test_res.pos(:,blk,run) = trials;
        test_res.rules(:,:,blk,run) = rules;
        save([outdir 'test_res_p' num2str(subNum) '.mat'], 'test_res');
        
        
        % reset RT and score counters
        allRT = zeros(p.ntrials,1);
        allscore = zeros(p.ntrials,1);
        

        %% -- main trial loop ---
        for t = 1:p.ntrials
            
            bc=bc+1;
            % determine trial parameters
            pos = trials(t);
            rule = rules(1,t); %randomly select which rule to use on this trial
            col = rules(2,t);
            cuecol = p.cols{col,rule};
            bgcol = p.bgcols{col, rule}; %pick fixation cross colour for this trial based on rule
            rect = p.testRects(pos,:); %pick the rectangle position you need to fill on the basis of trial type
            
            cres = p.corResp(rule, pos); %calculate correct response to expect
            
            
            % draw fixation cross
            Screen('TextSize', MainWindow, 50);
            %         DrawFormattedText(MainWindow, '+', 'center','center',p.grey);
            Screen('FillRect', MainWindow, p.grey, p.fixRect); % colour square
            
            fixdisptime = Screen('Flip', MainWindow); %% display fixation cross for fixdur duration
            
            if isMegExp % if MEG expt, draw our photodiode stim to the screen
                Screen('FillRect', MainWindow, p.white, p.pdrect); % colour square
            end
            
            % prepare stimulus screen
            Screen('FillRect', MainWindow, bgcol, p.fixRect); % colour square
            %         DrawFormattedText(MainWindow, '+', 'center','center',bgcol); % fixation
            Screen('FillRect', MainWindow, p.grey, rect); % colour square
            [VBLTimestamp, StimulusOnsetTime] = Screen('Flip', MainWindow,fixdisptime+p.fixdur-.01);
            if eye_tracking==1
                Eyelink('Message', 'Block_Stimulus %d %d', blk, t);
            end
            % send trigger
            if isMegExp
                MQsendtrigger(ioObj,address,trig,triggerduration);
            end
            
            % show blank fixation screen
            %         DrawFormattedText(MainWindow, '+', 'center','center',p.grey);
            Screen('FillRect', MainWindow, p.grey, p.fixRect); % colour square
            
            blanktime = Screen('Flip', MainWindow,VBLTimestamp+p.stimdur-.01); % show blank screen after p.stimdur
            
            % wait for response
            [keypress, RT, ToR] = WaitforResponse(StimulusOnsetTime, p.waittime,buttonbox,bbhandle); %keypress is a funny thing. It's a number (eg 86, but if you print it to a string using sprintf, it's also a letter, e.g. 'V', according to which key was pressed, but num2str(resp) gives you the string of the number (ie '86')
            if eye_tracking==1
                Eyelink('Message', 'Response %d', keypress);
            end
            
            % send trigger
            if isMegExp
                MQsendtrigger(ioObj,address,resptrig,triggerduration);
            end
            fliptime = Screen('Flip', MainWindow,VBLTimestamp+p.stimdur-.01); % show blank screen after p.stimdur
            fprintf('The %d key was pressed at time %.3f seconds\n', keypress, RT);
            
            % check the response
            if keypress == 0 %didn't respond
                resp = 0; % didn't respond
                score = 0; % didn't respond
                RT = NaN;
            elseif keypress == 999 %responded but pressed 2 keys at once
                resp = 999; % pressed 2 buttons
                score = 0; % score as incorrect
            else % if pressed one key
                if find(p.kb_codes(1,:)== keypress)                    
                    resp = find(p.kb_codes(1,:)== keypress); % translate key press into response number 1-4
                    score =  resp == cres; % score 1 or 0, if response matches correct response
                else
                    resp = keypress;
                    score = 0;
                end
            end
            
            
            allscore(t) = score; %tally score
            if keypress ~=0
                allRT(t)=RT;
            else
                allRT(t) = nan;
            end %if they responded at all (incl double presses), tally RT
            
            
            % ITI
            WaitSecs(p.ITI/1000);
            
            %%% print to logfile:
            fprintf(logFID,['%d\t%d\t%d\t' ...
                '%d\t%s\t'...
                '%d\t%d\t'...
                '%d\t%d\t%d\t'...
                '%.3f\t%.3f\t%.3f\t%.3f\t'...
                '%.3f\t%.3f\n'], ...
                blk,t,rule, ...
                col,cuecol, ...
                pos,cres,...
                resp,keypress,score, ...
                fixdisptime,VBLTimestamp-fixdisptime,blanktime-VBLTimestamp,RT,...
                VBLTimestamp,ToR);
            
            test_res.allresults(bc,:) = [pos rule col cres resp score fixdisptime VBLTimestamp Run_start_time blanktime RT];
            test_res.cuecol{bc} = cuecol;
            save([outdir 'test_res_p' num2str(subNum) '.mat'], 'test_res');
            
        end
        
        Screen('TextSize', MainWindow, 28);
        
        DrawFormattedText(MainWindow, 'End of Block', 'center', 'center', p.grey); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
        Screen('Flip', MainWindow);
        WaitSecs(2);
        if eye_tracking==1
            Eyelink('Message', 'End_of_Block_message');
        end
        
        if blk==1
            %end of block feedback
            msg = sprintf('%s\n\n%s\n\n%s', ['You scored ' num2str(round(sum(allscore)/length(allscore)*100)) ' %'], ...
                ['Mean reaction time: ' num2str(round(mean(allRT(~isnan(allRT)))*1000)) ' ms'], ...
                'Well done!',...
                ' ', ...
                ' ', ...
                'Next block starts after 30 seconds \n\n');
            DrawFormattedText(MainWindow, msg, 'center', 'center', p.grey); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
            Screen('Flip', MainWindow);
            WaitSecs(30);
        else
            msg = sprintf('%s\n\n%s\n\n%s', ['You scored ' num2str(round(sum(allscore)/length(allscore)*100)) ' %'], ...
                ['Mean reaction time: ' num2str(round(mean(allRT(~isnan(allRT)))*1000)) ' ms'], ...
                '\n\nWell done!',...
                ' ', ...
                ' ', ...
                'Next block starts after 1 minute \n\n'); % actually after the fMRI is triggerred
            DrawFormattedText(MainWindow, msg, 'center', 'center', p.grey); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
            Screen('Flip', MainWindow);
            WaitSecs(3);
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
            Eyelink('Message', '!V Run and Block Number =  %d %d', run , blk)
            
            % STEP 7.8
            % Sending a 'BLOCK_RESULT' message to mark the end of a trial in
            % Data Viewer. This is different than the end of recording message
            % END that is logged when the trial recording ends. The viewer will
            % not parse any messages, events, or samples that exist in the data
            % file after this message.
            Eyelink('Message', 'BLOCK_RESULT 0');
        end       
    end %block loop
end %run loop

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
% --- AUX FUNCTIONS ---



function [resp RT ToR] = WaitforResponse(StimulusOnsetTime, delay,buttonboxx,bbhandled)
% keyCode bbresp scannerstart bbhandle StimulusOnsetTime
% buttonbox=true;
oldresp = 0;
resp = 0;
RT = 0;
buttonbox=buttonboxx;
bbhandle=bbhandled;
if buttonbox
    data = IOPort('Read',bbhandle); %clear response box buffer (unnecc if recording throughout stim pres)
end

while (GetSecs - StimulusOnsetTime)<=delay/1000 % while loop until subject responds, up to max delay time
    
    if buttonbox
        data = IOPort('Read',bbhandle); %read button box's buffer
        if data & data ~=84 & data~=oldresp %ignore TTL pulses % if there is any data
            if length(data) > 1 %they have pressed more than one button at once, therefore score as incorrect
                ToR = GetSecs(); % Time of Response
                RT = ToR - StimulusOnsetTime;
                oldresp = data; % should not be needed (should never do this loop twice)
                resp = 999; % resp = 999 for answered but pressed 2 buttons (score as incorrect)
                break
            elseif sum([97 98 99 100] == data); % else, if the data is drawn from the set of bb responses
                %if data & data ~=53 & data~=oldresp %ignore TTL pulses, ignore same button still held down
                ToR = GetSecs(); % Time of Response
                RT = ToR - StimulusOnsetTime;
                oldresp = data; % should not be needed (should never do this loop twice)
                resp=data;               
                break
            end
        end
    else %if keyboard
        [KeyIsDown, ~, keyCode]=KbCheck; %check the keyboard
        if sum(keyCode)==1 %if keyCode is not empty (this happens if they press 2 buttons at once)
            if KeyIsDown% && sum([67 86 66 78] == find(keyCode))%(find(keyCode) ~= oldresp) && (find(keyCode) ~= 32) %ignore space bar responses
                ToR = GetSecs(); % Time of Response
                RT = ToR - StimulusOnsetTime;
                oldresp = find(keyCode); % again probably not needed
                resp = find(keyCode);
                break
            end
        end
    end
    
    WaitSecs(0.001);
end %end while

if resp ==0; ToR = StimulusOnsetTime + delay; end %if you get to the end without a response, record current time instead of time of response

end

function CueRules(MainWindow, p)


bgd = ones(p.winpixx,p.winpixy);


rule1col1 = Screen('MakeTexture', MainWindow, bgd');
Screen('TextSize', rule1col1, 60);
Screen('FillRect', rule1col1, p.darkgrey,[100 100 p.winpixx-100 p.winpixy-100]); % background
Screen('FillRect', rule1col1, p.grey, p.trainRects'); % stim squares
Screen('FillRect', rule1col1, p.grey, p.resRects2'); % stim squares

% rule 1 colour 2
rule1col2 = Screen('MakeTexture', MainWindow, bgd');
Screen('TextSize', rule1col2, 60);
Screen('FillRect', rule1col2, p.darkgrey,[100 100 p.winpixx-100 p.winpixy-100]); % background
Screen('FillRect', rule1col2, p.grey, p.trainRects'); % stim squares
Screen('FillRect', rule1col2, p.grey, p.resRects2'); % stim squares

% rule 2 col 1
rule2col1 = Screen('MakeTexture', MainWindow, bgd');
Screen('TextSize', rule2col1, 60);
Screen('FillRect', rule2col1, p.darkgrey,[100 100 p.winpixx-100 p.winpixy-100]); % background
Screen('FillRect', rule2col1, p.grey, p.trainRects'); % stim squares
Screen('FillRect', rule2col1, p.grey, p.resRects2'); % stim squares

% rule 2 colour 2
rule2col2 = Screen('MakeTexture', MainWindow, bgd');
Screen('TextSize', rule2col2, 60);
Screen('FillRect', rule2col2, p.darkgrey,[100 100 p.winpixx-100 p.winpixy-100]); % background
Screen('FillRect', rule2col2, p.grey, p.trainRects'); % stim squares
Screen('FillRect', rule2col2, p.grey, p.resRects2'); % stim squares

for pos = 1:4 %for each stimulus
    stimRect =p.trainRects(pos,:);
    fromX = mean([stimRect(1) stimRect(3)]); % middle of stim box
    fromY = stimRect(4) + 10; %below the box
    
    ansRect1 = p.resRects2(p.corResp(1, pos),:); %(rule, pos)
    toX1 = mean([ansRect1(1) ansRect1(3)]); %middle of answer box
    toY1 = ansRect1(2) - 20;
    Screen('DrawLine', rule1col1, p.white, fromX, fromY, toX1, toY1,3);
    Screen('DrawLine', rule1col2, p.white, fromX, fromY, toX1, toY1,3);
    
    ansRect2 = p.resRects2(p.corResp(2, pos),:); %(rule, pos)
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

DrawFormattedText(MainWindow, 'Here are the rules: they will disappear after 10 seconds', 'center', 'center', p.grey); %usage: [nx, ny, textbounds] = DrawFormattedText(win, tstring [, sx][, sy][, color][, wrapat][, flipHorizontal][, flipVertical][, vSpacing][, righttoleft])
Screen('Flip', MainWindow);


end

