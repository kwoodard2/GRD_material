import random
from psychopy import visual, core, event, gui
from useful_functions import loadFiles, calculateRectangularCoordinates, popupError, openOutputFile

#make these variables global
# a little hacky, but let's me update all pictures easily
global picNum
global selectedPicNum
global newList
global selectedStim
global positions
global mouse
global pics
global keys
global num
global conflictedStim
global conflictedPicNum
global newX
global newY

#name exp
expName='_EmotionGrid_v4_'

#get participant info
while True:
    popupName = 'Grid Experiment'
    subjInfo = {'participant': 'subjCode', 'seed':1, 'sort':['Choose','Practice','Sort1','Sort2']}
    popupDlg = gui.DlgFromDict(dictionary=subjInfo, title=popupName)
    if popupDlg.OK == False:
        popupError('check for errors')
    if subjInfo['participant']=='subjCode':  # or if ok_data is not None
        popupError('Subject code is blank')
    else:
        #naming and writing to output file
        outputFile = 'data/'+subjInfo['participant']+expName+subjInfo['sort']+'.csv'
        with open(outputFile,'w') as f:
            f.write('participant,seed,sort,image,final_img_pos,posX,posY' + '\n')
        break

#set random seed
random.seed(int(subjInfo['seed']))

#make a full screen window
win = visual.Window(fullscr=True,allowGUI=False, color="white", units='pix')

#load the pictures based on which sort
if subjInfo['sort'] == "Practice":
    pics = loadFiles('stimuli_practice', 'png', 'image', win)
elif subjInfo['sort'] == "Sort1":
    pics = loadFiles('stimuli_sort1', 'png', 'image', win)
elif subjInfo['sort'] == "Sort2":
    pics = loadFiles('stimuli_sort2', 'png', 'image', win)


picList = list(pics.keys()) #list of picture names
numPics = len(pics.keys()) #how many pictures (18)
random.shuffle(picList) #shuffles pictures into a random list
big = (315,315) #sets size for BIG pictures
small = (140,140) #sets size for SMALL pictures

#all pics appear in the middle initially
positions = numPics * [(0,0)]
#define initial variables
selectedPicNum = None #keeps track of what picture selected
selectedStim = None #keeps track of what picture selected
#setting it (below) makes first pic conflicted so you cant see new picture until its moved
conflictedStim = pics[picList[0]]['stim'] #keeps track of conflicted pic
conflictedPicNum = 0 #keeps track of conflicted pic
num = 1 #sets counter for cycling through picture list
newList = picList[0:num] #takes subset of list so pictures show 1 at a time
keys = event.getKeys() #waits for keyboard press to send program

#make mouse
mouse = event.Mouse(win=win)
mouse.setPos(newPos=(0,0)) #mouse appears in center of screen

       
#draw grid
def draw_grid():
    #here are the 60 boxes of the grid, all appear in center of screen
    #in hindsight, this is a little hacky, but leaving for now
    b1= visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b2 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b3 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b4 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b5 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b6 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b7 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b8 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b9 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b10 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b11=  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b12 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b13 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b14 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b15 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b16 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b17 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b18 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b19 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b20 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b21=  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b22 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b23 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b24 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b25 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b26 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b27 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b28 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b29 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b30 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b31=  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b32 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b33 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b34 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b35 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b36 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b37 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b38 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b39 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b40 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b41=  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b42 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b43 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b44 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b45 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b46 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b47 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b48 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b49 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b50 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b51=  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b52 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b53 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b54 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b55 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b56 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b57 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b58 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b59 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
    b60 =  visual.Rect(win,opacity =0.0, fillColor='blue',size=[280,280])
        
    #here are all of the coordinates for the grid
    vertical = calculateRectangularCoordinates(143, 0, 11, 1,yOffset=0,xOffset=0) #vertical bar positions
    horizontal = calculateRectangularCoordinates(0, 149, 1, 7,yOffset=0,xOffset=0) #horizontal bar positions
    boxes = calculateRectangularCoordinates(143, 149, 10, 6,yOffset=0,xOffset=0) #position of 60 boxes
    
    vbars = ['v1','v2','v3','v4','v5','v6','v7','v8','v9','v10','v11']
    hbars = ['h1','vh2','h3','h4','h5','h6','h7']
    
    #list of all 60 grid boxes
    global bbars
    bbars = [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,
         b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,
         b21,b22,b23,b24,b25,b26,b27,b28,b29,b30,
         b31,b32,b33,b34,b35,b36,b37,b38,b39,b40,
         b41,b42,b43,b44,b45,b46,b47,b48,b49,b50,
         b51,b52,b53,b54,b55,b56,b57,b58,b59,b60]
    
    #makes vertical bars
    vnum = 0
    for i in vbars:
        i = visual.Rect(win,lineWidth = 3.5, lineColor='black',fillColor='black',size=[1,1790],pos = vertical[vnum])
        vnum+=1
        i.draw()
    
    #makes horizontal bars
    hnum = 0
    for i in hbars:
        i = visual.Rect(win,lineWidth = 3.5, lineColor='black',fillColor='black',size=[2850,1],pos = horizontal[hnum])
        hnum+=1
        i.draw()
        
    #draws 60 invisible boxes
    bnum = 0
    for i in bbars:
        i.setPos(boxes[bnum])
        i.opacity = 0.0 #invisible, can make visible for debugging
        i.draw()
        bnum+=1

#draw the grid
draw_grid()

#make all pictures little
for picNum,curPic in enumerate(picList): #curPic is the name of the pic to be drawn
    # picNum is from 0 to 17 (inclusive)
    # curPic is the name of the image
    curStim = pics[curPic]['stim']
    curStim.setSize(small)


#function for drawing faces on the grid
def draw_pics():
    global picNum
    global selectedPicNum
    global newList
    global selectedStim
    global positions
    global mouse
    global pics
    global keys
    global num
    global bbars
    global conflictedStim
    global conflictedPicNum
    global newX
    global newY
    
    if selectedStim == None and conflictedStim != None:
        if mouse.isPressedIn(conflictedStim):
            selectedStim = conflictedStim
            selectedPicNum = conflictedPicNum
    #goes through list of pictures
    #newList increments 1 by 1 so one picture shows up at a time
    for picNum,curPic in enumerate(newList): 
        #grabs first picture
        curStim = pics[curPic]['stim']
        
        #if mouse pressed in picture, make it big, pick it up and move it
        if mouse.isPressedIn(curStim):
            if conflictedPicNum != None and picNum != conflictedPicNum:
                #does not allow new picture until conflict resolved
                pass
            else:
                if selectedPicNum == None:
                    selectedPicNum = picNum
                    selectedStim = curStim
                if selectedPicNum == picNum:
                    positions[picNum] = tuple(mouse.getPos()) #set image position to new mouse position
                    curStim.setSize(big)  
                
        #mouse is no longer pressed, make small again and finalize image position
        elif picNum == selectedPicNum:
            selectedPicNum = None
            selectedStim = None
            curStim.setSize(small) #make small
            stimPos = curStim.pos #get position of stim
            tuple_stimPos = tuple(stimPos) #make position a tuple to be readable
            
            for i in bbars:
                #check if any other picture has this position
                #if it does have this position, don't snap it
                if i.contains(tuple_stimPos):
                    #if tuple_stimPos in positions:
                    if tuple(i.pos) in positions:
                        (newX,newY) = tuple_stimPos
                        #move to center (50,50) based on positions
                        if tuple_stimPos[0] > 0 and tuple_stimPos[1] > 0:
                            newX = tuple_stimPos[0] - 50
                            newY = tuple_stimPos[1] - 50
                        elif tuple_stimPos[0] < 0 and tuple_stimPos[1] < 0:
                            newX = tuple_stimPos[0] + 50
                            newY = tuple_stimPos[1] + 50
                        elif tuple_stimPos[0] > 0 and tuple_stimPos[1] < 0:
                            newX = tuple_stimPos[0] - 50
                            newY = tuple_stimPos[1] + 50
                        elif tuple_stimPos[0] < 0 and tuple_stimPos[1] > 0:
                            newX = tuple_stimPos[0] + 50
                            newY = tuple_stimPos[1] - 50
                        #set new position
                        positions[picNum] = (newX,newY)
                        conflictedStim = curStim
                        conflictedPicNum = picNum
                    else:
                        #snap the picture in the center of the square
                        positions[picNum] = tuple(i.pos)
                        conflictedStim = None
                        conflictedPicNum = None
        #draw everything                
        curStim.setPos(positions[picNum])
        curStim.draw()
    
    if selectedStim != None:
        selectedStim.draw()
        
    win.flip()

#hide mouse since touch screen
mouse.setVisible(0) #hide mouse

#core task
while True:
    draw_grid() #draw 10x6 grid with rectangles
        
    draw_pics() #call function to draw pictures
    
    keys = event.getKeys() #watch out for keys to end task or add more pictures
    
    #press m to add another picture to the grid
    if 'm' in keys:
        if ((subjInfo['sort']=="Practice") and num >= 5):
            pass
        elif ((subjInfo['sort']=="Sort1" or subjInfo['sort']=="Sort2") and num >= 18):
            pass
        elif conflictedStim == None:
            num +=1
            newList = picList[0:num]
            conflictedStim = pics[picList[num-1]]['stim']
            conflictedPicNum = num-1
        
    #press qp to end the task
    if ('q'in keys and 'p' in keys):
        #save data!
        for picNum,curPic in enumerate(picList):
            with open(outputFile,'a') as f:
                 f.write(str(subjInfo['participant']) + ',' + str(subjInfo['seed']) + ',' + subjInfo['sort'] + ',' + curPic + ',' + str(pics[curPic]['stim'].pos) + ',' + str(pics[curPic]['stim'].pos[0])+ ',' + str(pics[curPic]['stim'].pos[1]) + '\n')
        break

