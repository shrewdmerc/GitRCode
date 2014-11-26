try
global w ifi display expPar sti  mPar design faceList

KbName('UnifyKeyNames');
noisyImage = 0; 

design.separateRaces = 0; % whether we will be having different families for different races
design.separateGenders = 0; % whether we will be having deifferent families for different genders
design.numFamilies = 10; % the number of families (for each grouping) we are going to collect 
design.numMembers = 10; % the number of members in each family
%% set up default keys
Ykey = KbName('Y'); Skey = KbName('S'); Nkey = KbName('N'); Dkey = KbName('D'); Mkey = KbName('M');
Akey = KbName('A'); Kkey = KbName('K'); Lkey = KbName('L'); Spacekey = KbName('Space');
EscKey = KbName('Escape'); leftKey = KbName('LeftArrow'); rightKey = KbName('RightArrow');
upKey = KbName('UpArrow'); downKey = KbName('DownArrow'); yesKey = KbName('return');

zeroKey = KbName('0)'); oneKey = KbName('1!'); twoKey = KbName('2@'); threeKey = KbName('3#');
fourKey = KbName('4$'); fiveKey = KbName('5%'); sixKey = KbName('6^'); sevenKey = KbName('7&');
eightKey = KbName('8*'); nineKey = KbName('9('); 
deviceNumber = -1;

%% Standardize Colors
trueGray = [128 128 128]; red = [255 0 0]; green = [0 255 0]; blue = [0 0 255];
white = [255 255 255]; black = [0 0 0]; yellow = [255 255 0]; purple = [128 0 128];
orange = [255 128 0]; brown = [128 64 0]; azure = [0 255 255]; whitedark = [200 200 200];
pink = [255 0 128]; gray = trueGray; gold = [255 215 0]; green = [];
keyIsDown = 0;

%% Initiate random number generator.
rand('state',sum(100*clock)); 

%% Screen parameters
bcolor = white;
textColor = black;
blackFixation = {[0,0,0],[0,0,0]}; %defines outer and inner colors of the fixation square
greyFixation = {gray,gray}; % was 120 all the way
whiteFixation = {white,white};

%% Set up Screens
screens=Screen('Screens');
screenNumber=min(screens); % having problems on Alienware
Priority(MaxPriority(screenNumber)); % for priority I suppose - doesn't seem to work on mac mini though.
display.skipChecks = 1;
display.dist =50;	%viewing distance in cm
display.width = 34.09; % for Retina MacBook Pro
display.height = 19.18; % for Retina MacBook Pro
display.screenNum = screenNumber;
%set up text
display.text.color = black;
display.text.font = 'Trebuchet';
display.text.size = 14; % used to be 24
if IsOSX
    display.ptbPipeMode = kPsychNeedFastBackingStore;
end
display = OpenWindow(display);
w = display.windowPtr;
Screen('FillRect', display.windowPtr, bcolor);
display.center = [display.resolution(1)/2 display.resolution(2)/2];
display.windowRect = [0 0 display.resolution(1) display.resolution(2)];
display.fixation.color = blackFixation;
display.fixation.size = 0.2;
display.fixation.mask = 1;
display.bkColor = white;
% Center of monitor
display.midx=display.resolution(1)/2;
display.midy=display.resolution(2)/2;
display.dppx=atan(display.width*.5/display.dist)/pi*180/(display.resolution(1)/2); % distance between pixels

%% Set up stimulus parameters
stimPar.aperture = 20; % aperture size in deg
stimPar.boundary_wid=5; %in pixel
stimPar.boundary_length=1; 
stimPar.boundary_color=[0 0 0];
stimPar.probe_color=red; 
% to change size of the face square, change "14" to something else
stimPar.faceHeight = angle2pix(display,14); 
stimPar.faceSizeOrig = [450 450];

stimPar.duration = 1;
stimPar.category1 = 'Male';
stimPar.category2 = 'Female';
stimPar.minAge = 18;
stimPar.maxAge = 35;
stimPar.ageRange = stimPar.maxAge - stimPar.minAge + 1;
      
%% Get Race and Gender of Participant
display.fix_center = display.center;
 
GetConsent();
pause(0.5);
race = GetRace();
pause(0.5);
gender = GetGender();
pause(0.5);
%[age] = GetPositionEst(display); %not working

num_trials = 5;
completed_trials = 0
directory_end = '5';

while(completed_trials < num_trials)
completed_trials = completed_trials + 1;

loadName = 'exp_WMfaces.mat'
loadDir = '/Users/cgibson/Dropbox/Senior Year/Term 1/Directed Research/Data';
loadName = strcat(loadDir,loadName);
if exist(loadName,'file');
    load(loadName,'data');
    iMember = data.iMember;
    iFamily = data.iFamily;
else
    iFamily = 1;
    iMember = 1;
    
    data.familyMatrices{1} = zeros(1,9);
    data.lastFaceIndex = 0;
    
    % Column 1: Face index for first face shown
    % Column 2: Face index for initial face in face wheel
    % Column 3: Face index for chosen face
    % Column 4: Face angle for chosen face 
    % Column 5: RT
    % Column 6: Clicks
    % Column 7: Race
    % Column 8: Gender
    % Column 9: Age
end
faceTex = {};
my_directory = '/Users/cgibson/Dropbox/Senior Year/Term 1/Directed Research/00/';
directory_end = num2str(str2num(directory_end) + 1);
my_directory = strcat(my_directory, directory_end);
stimPar.directory = my_directory;


cd(stimPar.directory);
extension = '*.jpg'; 
stimPar.center = [0 0];
stimPar.faces = dir(extension);
stimPar.numFaces = length(stimPar.faces);
stimPar.degreesPerFace = 360/stimPar.numFaces; % was 360/stimPar.numFaces before
stimPar.faceAngles = kron((1:stimPar.numFaces)', ones(round(stimPar.degreesPerFace),1)); % the face numbers corresponding to each angle. Just round the angle and call the face at that index.
randomOffset = round(rand*stimPar.numFaces); % we're going to shift the circle by this much
stimPar.faceAngles_orig = stimPar.faceAngles;
stimPar.faceAngles = stimPar.faceAngles + randomOffset;
stimPar.faceAngles = mod(stimPar.faceAngles,stimPar.numFaces) +1;
stimPar.faceOrder = [1:stimPar.numFaces] + randomOffset;
stimPar.faceOrder = mod(stimPar.faceOrder,stimPar.numFaces) +1;
reverseFaceContinuum = round(rand);

stimPar.firstFaceIndex = round(length(stimPar.faces)/4)
stimPar.presentationSecs = stimPar.duration;
%stimPar.  = secs2frames(display,stimPar.presentationSecs);
if reverseFaceContinuum
    stimPar.faceAngles = flipdim(stimPar.faceAngles,1);
    stimPar.faceOrder = flipdim(stimPar.faceOrder,2);
end
faceTex = cell(stimPar.numFaces,1);

faceSizeOrig = [400 400];
normalizedWidth = faceSizeOrig(1)/faceSizeOrig(2); % height/width
ratio = [0 0 normalizedWidth 1];
faceRect = ratio*stimPar.faceHeight;
faceRect = CenterRectOnPoint(faceRect,display.center(1),display.center(2));

%% Generate  textures for each face
for i = stimPar.faceOrder
    faceName{i} = stimPar.faces(i).name;
    faceImage = rgb2gray(imread(faceName{i}));
    if noisyImage
        faceImage = imnoise(faceImage,'gaussian',meanNoise,meanVar);
    end
    faceTex{i} = Screen('MakeTexture', w, faceImage);
end

initialFaceAngle = round(rand*360); % choose the initial face which will be shown during the reproduction phase
initialFaceIndex = stimPar.faceAngles(initialFaceAngle);
stimPar.initialFaceName = stimPar.faces(initialFaceIndex).name; % save for posterity's sake
%% set up probe
probelength=1; %deg

%Draw the ring for the aperture
sz(1) = angle2pix(display,stimPar.aperture/2+.15);
sz(2) = angle2pix(display,stimPar.aperture/2);

for i=1:length(sz)
    rect{i}= [-sz(i)+display.center(1),-sz(i)+display.center(2), ...
        sz(i)+display.center(1),sz(i)+display.center(2)];
end

%Draw the probe for estimation
current_direction=initialFaceAngle*pi/180;
initial_direction = current_direction;
stimPar.initial_direction = initial_direction;
explorationAmount = pi*0.95;
stimPar.explorationAmount = explorationAmount; %how many angles the subject must traverse along the wheel before being allowed to record their choice
explorationCheck = 0;
clicks = 0;
clickAmount = 1;
stimPar.clickAmount = clickAmount; % how many discrete clicks the subject must make before being allowed to record their choice
stimPar.clickInterval = 0.15; % the amount of time in seconds between clicks that must past for a click to count towards clickAmount
clickInterval = stimPar.clickInterval;
lastClickTime = 0;
faceAngle = initialFaceAngle;
faceIndex = initialFaceIndex;
currentFace = faceTex{faceIndex};
aperturecenter=angle2pix(display,stimPar.center)+display.resolution./2;

%Give Instructions
Screen('TextSize',w, 35);
if (completed_trials == 1) %if first trial
    textWidth = RectWidth(Screen('TextBounds', display.windowPtr, 'To begin the task, press SPACE bar'));
    Screen('DrawText', display.windowPtr, 'To begin the task, press SPACE bar', display.center(1)-(textWidth/2), display.center(2)-300+(90*i), textColor, bcolor);
end
if (completed_trials > 1) %if not first trial
    textWidth = RectWidth(Screen('TextBounds', display.windowPtr, 'To continue the task, press SPACE bar'));
    Screen('DrawText', display.windowPtr, 'To continue the task, press SPACE bar', display.center(1)-(textWidth/2), display.center(2)-300+(90*i), textColor, bcolor);

end

Screen('Flip',w);
pause(0.5);

while(1)
    [keyIsDown,secs,keyCode] = KbCheck;
    if keyCode(KbName('Space'))==1
        break
    end
end

Screen('Flip',w);
pause(0.5);    

%% Draw test stimulus

while(1)
    %Outer ring (default is black)
    Screen('FillOval',w, stimPar.boundary_color,rect{1});
    
    % Get probe marker coordinates
    probe_cord=angle2pix(display,[
        stimPar.center(1)+(stimPar.aperture/2+probelength)*sin(current_direction) ...
        stimPar.center(2)-(stimPar.aperture/2+probelength)*cos(current_direction) ...
        0 ...
        0]);
    % add to the center of the screen...
    probe_cord=probe_cord+ ...
        [display.resolution(1)/2 display.resolution(2)/2 display.resolution(1)/2 display.resolution(2)/2];
    
    Screen('DrawLine',w,stimPar.probe_color, ...
        probe_cord(1), probe_cord(2), probe_cord(3), probe_cord(4), ...
        stimPar.boundary_wid);
    
    %Inner ring (default is gray)
    Screen('FillOval',w, display.bkColor,rect{2}); % draw this after probe cord, then draw face over all that.
    Screen('DrawTexture',w, currentFace, [], faceRect); %Display the center stimulus.
    Screen('Flip',w);
    
    t = GetSecs;
    [x,y,buttons] = GetMouse;
    
    while ~any(buttons) % wait for press
        [x,y,buttons] = GetMouse;
    end
    
    if buttons(1)==1
        clickTime = GetSecs;
        if (clickTime - lastClickTime >= clickInterval)
            clicks = clicks + 1;
        end
        lastClickTime=clickTime;
        current_direction=atan((x-aperturecenter(1))/-(y-aperturecenter(2))+eps);
        faceAngle = current_direction * 180/pi;
        if -(y-aperturecenter(2))<0
            current_direction=current_direction+pi;
            faceAngle = faceAngle+180;
        end
        
        while faceAngle<0 
            faceAngle = 360+faceAngle;
        end
        
        if (faceAngle < 1) & (faceAngle > 0) % ensures that it never tries to access index 0 below.
            faceAngle = ceil(faceAngle);
        end
        
        faceIndex = stimPar.faceAngles(round(faceAngle));
        currentFace = faceTex{faceIndex};
        Screen('DrawTexture', w, currentFace, [], faceRect); %Display the center stimulus.
       
        if explorationCheck == 0
            if abs( faceAngle - abs(initial_direction*180/pi) )>=(explorationAmount*180/pi)
                explorationCheck = 1;
            end
        end
        
        elseif ((buttons(2)==1 | KeyCode(KbName('Space'))==1) & (clicks >= clickAmount) & (explorationCheck==1)) % should be buttons(2) when not on PC
        break
    end
end

RT = GetSecs - t;

%% save data

lastFaceIndex = faceIndex;
lastFaceAngle = faceAngle;

data.lastFaceIndex = faceIndex;
data.lastFaceAngle = faceAngle;
currentData = [stimPar.firstFaceIndex initialFaceIndex faceIndex faceAngle ...
    RT clicks race gender 0];
if iMember > 1
    data.familyMatrices{iFamily} = [data.familyMatrices{iFamily}; currentData];
else
    data.familyMatrices{iFamily} = currentData;
end

if size(data.familyMatrices{iFamily},1) == design.numMembers
    iFamily = iFamily + 1;
    iMember = 1;
    data.lastFaceIndex = 0;
else
    iMember = iMember + 1;
end

data.iMember = iMember;
data.iFamily = iFamily;

saveName = 'exp_WMfaces.mat';
end
Screen('CloseAll');

catch
    % If an error occurs, the catch statements executed.  We restore as
    % best we can and then rethrow the error so user can see what it was.
    Screen('CloseAll');
    fprintf('We''ve hit an error.\n');
    psychrethrow(psychlasterror);
end 