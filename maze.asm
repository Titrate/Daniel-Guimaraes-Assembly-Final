include macs.txt
include mazemacs.txt

.model small
.8086
.stack 100h

.data?
mazeNode struc								;This is a specific cell of the maze
	offsetNdx		db ? 					;position in array
	xPos 			db ?					;x offset of upper left corner
	yPos 			db ?					;y offset of upper right corner
	top 			db ?					;top wall, 0 means wall, 1 means path
	bottom 			db ?					;bottom wall
	right 			db ?					;Right wall
	left 			db ?					;Left wall
mazeNode ends

;Note on accessing
;[bx].offsetNdx = [bx]
;[bx].xPos		= [bx + 1]
;[bx].yPos		= [bx + 2]
;[bx].top		= [bx + 3]
;[bx].bottom	= [bx + 4]
;[bx].right		= [bx + 5]
;[bx].left		= [bx + 6]

.data
programHeading		db 'aMAZEing!$'			;The heading to be displayed.
instructionOne		db 'q - quit$'			;Instruction one heading
instructionTwo		db 'r - New Maze$'		;Instruction two heading
arraySize 			db 225					;This is a 2dim array, it is 2x2
rowLength 			db 15					;This is the length of a row
colLength 			db 15					;This is the number of columns
currentXOffset 		db ?					;Needed to pass data into draw macro
currentYOffset	 	db ?					;Same as above
exitNdx				db ?					;Used in maze building subroutines
entranceOffset		dw ?					;Stores address of entrance node
seed				dw 3749h				;Used to generate a random number
rndnum				dw 0					;stores the random number generated in rand subroutine
moveNdx				dw ?					;used by the move subroutines to move bx pointer
beginingPoint		dw ?					;First slot of array


;This is the maze in array form
mazeBoard mazeNode 225 dup (<>)

;old video mode
videoMode db ?

.code
main proc
	mov ax, @data
	mov ds,ax
	
	mov bx, offset mazeBoard
	mov beginingPoint, bx
	
	_storeCurrentVideoMode videoMode
	_changeVideoMode 13h
	
	makeRandomMaze:
	_clearScreen
	call setUpHeaders
	call initMazeArray						
	call genRandMaze						;get new maze
	
	mov ch, 0
	mov cl, arraySize						;set up draw loop
	mov bx, beginingPoint					;We use bx so we can use bx to pass in the draw values
	drawLoop:
		call drawMazeNode					;Draw first maze node
		add bx, Size mazeNode				;Goto next maze node
		loop drawLoop
	waitPlease:
	_waitForKeyToBePressed
	mov ah, 'q'								;If q, exit program
	cmp ah, al
	jz exitProgram
	
	mov ah, 'r'								;If r, make a new maze
	cmp ah, al
	jz makeRandomMaze
	
	jmp waitPlease							;Else, do wait for another keystroke
	
	
	exitProgram:
	_changeVideoMode videoMode
	;end program
	mov ah,4ch
	int 21h
main endp

setUpHeaders proc
	_moveCursor 1, 1
	_display programHeading
	_moveCursor 5, 1
	_display instructionOne
	_moveCursor 9, 1
	_display instructionTwo
	ret
setUpHeaders endp

;This is where the maze will get made.
genRandMaze proc
	mov bx, beginingPoint				;Start at the beginning of the maze
	call makeEnterance						
	call makeExit

	mov bx, entranceOffset
	
	beginWanderingPath:
		mov dl, 1							;Tells program to skip up moves for generating path
		call makeRandomMove					;this will handle how to move
		mov al, exitNdx						;load exit node for compare
		mov bx, moveNdx						;get move to new node
		cmp [bx], al						;Stop once you arrive at exitNdx
		jz doneWanderingPath				
		jmp beginWanderingPath				
	doneWanderingPath:
	
	call radomizeWalls						;Knock down a bunch of walls
	
	ret
genRandMaze endp

;This subroutine will knock down random walls of any node with more than 1 removable wall
;If the node is touching 4 walls, knock down two walls.
;It will add random walls if a node has 0 walls
radomizeWalls proc
	mov bx, beginingPoint
	mov cx, 0
	mov cl, arraySize
	
	randomizeLoop:
		call getDestroyableWallCount		;returned in dl
		cmp dl, 0
		jz startNextLoop
		cmp dl, 1
		jz startNextLoop
		cmp dl, 4							;if 4 walls, knock down two
		jnz otherDestrySubroutine
			mov dx, 0						;Dl = 1 tells program to skip going up. Make sure dl != 1
			call makeRandomMove				;Note, cx is super important to maintian here
			call makeRandomMove
			jmp StartNextLoop
		otherDestrySubroutine:
			mov dx, 0						;Dl = 1 tells program to skip going up. Make sure dl != 1
			call makeRandomMove				;Note, cx is super important to maintian here
		startNextLoop:
		
		mov al, Size mazeNode				;First go to next node
		mov ah, 0
		add bx, ax
		loop randomizeLoop
	ret
radomizeWalls endp

;This subroutine will count destroyable walls and store it in al
getDestroyableWallCount proc
	push cx									;Needs to be saved
	mov dl, 0								;wall count
	mov dh, 0								;nondestructable wall count
	
	;First count walls
	mov cl, [bx + 3]						;check top wall
	cmp cl, 0
	jnz nowCheckRightWall					
		inc dl
	nowCheckRightWall:
	mov cl, [bx + 5]						;check top wall
	cmp cl, 0
	jnz nowCheckBottomWall					
		inc dl
	nowCheckBottomWall:
	mov cl, [bx + 4]						;check top wall
	cmp cl, 0
	jnz nowCheckLeftWall					
		inc dl
	nowCheckLeftWall:
	mov cl, [bx + 6]						;check top wall
	cmp cl, 0
	jnz nowExitWallCount					
		inc dl
	nowExitWallCount:
	
	;Now count non-destructable Walls
	call isTopRow
	cmp al, 1
	jnz nowCheckRightColumn
		inc dh
	nowCheckRightColumn:
	call isRightColumn
	cmp al, 1
	jnz nowCheckBottomRow
		inc dh
	nowCheckBottomRow:
	call isBottomRow
	cmp al, 1
	jnz nowCheckLeftColumn
		inc dh
	nowCheckLeftColumn:
	call isLeftColumn
	cmp al, 1
	jnz finalizeCount
		inc dh
	finalizeCount:
	
	sub dl, dh
	
	pop cx									;retreive value
	ret
getDestroyableWallCount endp

;Make a random move. Only move in a direction that can be moved.
makeRandomMove proc
	push cx
	
	tryNewNum:
	call genRandNumber
	mov cl, 4								;We can travel 1 of 4 directions
	div cl
	mov al, ah								;We want the mod part

	;see if we are moving up
	cmp al, 0
	jnz notTop
		cmp dl, 1
		jz tryNewNum						;Skip up moves
		call moveTop
		jmp exitMakeRandMove
	notTop:									;Check Right
	cmp al, 1
	jnz notRight
		call moveRight
		jmp exitMakeRandMove
	notRight:								;check Bottom
	cmp al, 2
	jnz notBottom
		call moveBottom
		jmp exitMakeRandMove
	notBottom:								;Must be left then
	call moveLeft							
	exitMakeRandMove:
	
	pop cx
	ret
makeRandomMove endp

;This subroutine handles moving up
moveTop proc
	call isTopRow							;Skip this if no top
	cmp al, 1
	jz goDownInstead
		mov al, 1
		mov [bx + 3], al
		call updateTopNeighbor
		jmp exitMoveUp
	goDownInstead:
		call moveBottom
	exitMoveUp:
	ret
moveTop endp

;This subroutine handles moving up
moveRight proc
	call isRightColumn						;Skip this if no right
	cmp al, 1
	jz goLeftInstead		
		mov al, 1
		mov [bx + 5], al
		call updateRightNeighbor
		jmp exitMoveRight
	goLeftInstead:
		call moveLeft
	exitMoveRight:
	ret
moveRight endp

;This subroutine handles moving up
moveBottom proc
	call isBottomRow						;Skip this if no bottom
	cmp al, 1
	jz goUpInstead
		mov al, 1
		mov [bx + 4], al
		call updateBottomNeighbor
		jmp exitMoveBottom
	goUpInstead:
		cmp dl, 1							;Dl has 1 in it if this is path routine. Do not go up
		je exitMoveBottom
		call moveTop
	exitMoveBottom:
	ret
moveBottom endp

;This subroutine handles moving up
moveLeft proc
	call isleftColumn						;Skip this if no right
	cmp al, 1
	jz goRightInstead		
		mov al, 1
		mov [bx + 6], al
		call updateLeftNeighbor
		jmp exitMoveLeft
	goRightInstead:
		call moveRight
	exitMoveLeft:
	ret
moveLeft endp

;This subroutine will go to a random node in top row and knock down it's top wall as an entry point.
makeEnterance proc
	call genRandNumber
	mov ah, 0
	mov di, bx								;Save the start point of bx
	mov cl, rowLength
	mov ch, 0
	
	div cl									;quo in al, remainder in ah
	mov al, ah
	mov ah, 0
	mov cl, size mazeNode
	mul cl									;This will be the offset ndx of the node to make the enterance
	
	add bx, ax
	mov ah, 1								;To make a path in the top piece
	
	mov [bx + 3], ah						;update the wall var
	mov entranceOffset, bx					;Save offset of starting node
	mov bx, di								;Restore bx
	ret
makeEnterance endp

;This subroutine will go to a random node in bottom row and knock down it's bottom wall as an entry point.
makeExit proc
	call genRandNumber
	mov di, bx								;Save the start point of bx
	mov cl, rowLength
	mov ch, 0
	
	div cl									;quo in al, remainder in ah
	mov dl, ah								;Store offset
	mov al, colLength						;(colLength - 1) * rowLength = first slot in bottom row
	mov ah, 0
	dec al
	mov cl, rowLength
	mul cl									;This will be the offset ndx of the bottom row
	add al, dl								;This is the new ndx
	
	;Now get byte offset
	mov cl, size mazeNode					
	mul cl
	
	add bx, ax
	mov ah, 1								;To make a path in the top piece
	
	mov [bx + 4], ah						;update the bottom wall var
	mov al, [bx]
	mov exitNdx, al							;Save exit ndx
	mov bx, di								;Restore bx
	ret
makeExit endp

;This method will create a random number between 0 and 255. It will store it in dl
;This subroutine was borrowed from the internets
genRandNumber proc
	push bx									;Save
	push dx
	
	mov ah, 0
	int 1ah									;get system time
	add bx, dx								;add the pieces together for rand seed
	
	MOV	AX, seed							;; AX = seed
	add ax, bx
	MOV	DX, 8405h							;; DX = 8405h
	MUL	DX									;; MUL (8405h * SEED) into dword DX:AX

	CMP	AX, [SEED]
	JNZ	GOTSEED								;; if new SEED = old SEED, alter SEED
	MOV	AH, DL
	INC	AX
GOTSEED:
	MOV	SEED, AX							;; We have a new seed, so store it
	MOV	AX, DX								;; AL = random number
	MOV	RNDNUM, AX
	and ax, 00ffh
	
	pop dx
	pop bx

	ret
genRandNumber endp

;Sub Routine to draw the maze
drawMazeNode proc
	;Draw the walls
	push cx									;Save loop spot
	
	;Handle Top wall
	mov al, [bx + 3]						;0 if needs to be drawn
	mov ah, 0
	cmp al, ah
	jnz skipTopWall							;skip if it does not need to be drawn
		call drawTopWall					;First draw the wall
		call isTopRow						;check if it is in top row
		cmp al, 1
		jz noTopToUpdate
			call updateTopNeighbor				;Update the top neighbor of the node.
		noTopToUpdate:
	skipTopWall:
	
	;handle right wall
	mov al, [bx + 5]						;0 if needs to be drawn
	mov ah, 0
	cmp al, ah
	jnz skipRightWall						;Skip if drawn or not there
		call drawRightWall					;First draw the wall
		call isRightcolumn					;check if it is in the right column
		cmp al, 1
		jz noRightToUpdate
			call updateRightNeighbor		;Update the wall ref of right neighbor
		noRightToUpdate:
	skipRightWall:
	
	;Handle bottom wall
	mov al, [bx + 4]						;0 if needs to be drawn
	mov ah, 0
	cmp al, ah
	jnz skipBottomWall						;Skip wall if it is drawn or a path
		call drawBottomWall					;First draw the bottom wall
		call isBottomRow					;check if it is on bottom row
		cmp al, 1
		jz noBottomToUpdate
			call updateBottomNeighbor		;Update bottom neighor
		noBottomToUpdate:
	skipBottomWall:
	
;Skip wall if it is drawn or a path'
	mov al, [bx + 6]						;0 if needs to be drawn
	mov ah, 0
	cmp al, ah
	jnz skipLeftWall						;check if it is on bottom row	
		call drawLeftWall					;First draw the bottom wall
		call isLeftcolumn					;Handle left wall
		cmp al, 1
		jz noLeftToUpdate
			call updateLeftNeighbor			;Update bottom neighbor
		noLeftToUpdate:
	skipLeftWall:
	
	pop cx									;retrieve loop location
	ret
drawMazeNode endp

;Draw the top wall
drawTopWall proc
	mov dx, 0								;Make sure dx is empty
	mov dl, [bx + 1]	 					;this is the x offset
	mov si, dx								;use index si to pass in x offset
	mov dl, [bx + 2]						;this is the y offset
	mov di, dx								;Use index di to pass in y offset	
	_drawLeftRightWall si, di				;WORK!!!
	mov al, 0ffh
	mov [bx + 3], al						;Switch to -1 so it is not redrawn
	ret
drawTopWall endp

;This will just draw the right wall, turns the zero into a -1 so we know it has been drawn
drawRightWall proc
	mov dx, 0								;Make sure dx is empty
	mov dl, [bx + 1]	 					;this is the x offset
	add dl, 12								;This is the correct offset position		
	mov si, dx								;use index si to pass in x offset
	mov dl, [bx + 2]						;this is the y offset
	mov di, dx								;Use index di to pass in y offset	
	_drawUpDownWall si, di					;WORK!!!!
	mov al, 0ffh
	mov [bx + 5], al						;Switch to -1 so it is not redrawn
	ret
drawRightWall endp

;This subroutine will draw the bottom wall
drawBottomWall proc
	mov dx, 0								;Make sure dx is empty
	mov dl, [bx + 1]	 					;this is the x offset
	mov si, dx								;use index si to pass in x offset
	mov dl, [bx + 2]						;this is the y offset
	add dl, 12								;this is the correct offset
	mov di, dx								;Use index di to pass in y offset	
	_drawLeftRightWall si, di				;WORK!!!!
	mov al, 0ffh
	mov [bx + 4], al						;Switch to -1 so it is not redrawn
	ret
drawBottomWall endp

;This subroutine draws the left wall
drawLeftWall proc
	mov dx, 0								;Make sure dx is empty
	mov dl, [bx + 1]	 					;this is the x offset
	mov si, dx								;use index si to pass in x offset
	mov dl, [bx + 2]						;this is the y offset
	mov di, dx								;Use index di to pass in y offset	
	_drawUpDownWall si, di					;WORK!!!!
	mov al, 0ffh
	mov [bx + 6], al						;Switch to -1 so it is not redrawn
	ret
drawLeftWall endp

;This subroutine checks to see if node is in top row. yes -> al = 1, no -> al = 0
isTopRow proc
	push dx
	mov dx, 0								;force dx to be empty
	mov dl, [bx]							;if ndx is < row length then node is on top row and has no top neighbor
	cmp dl, rowLength
	jb topRow
		mov al, 0							;1 means node is in top row
		jmp exitIsTopRow
	topRow:
		mov al, 1							;0 means that node is not in top row
	exitIsTopRow:
	pop dx
	ret
isTopRow endp

;This subroutine checks to see if the node is in the right column
;Brace yourself, brain pain is coming. 
;The right side is all the pieces at ndx = (rowLength * n) - 1
;n is the set of ints from [1..colLength]
isRightcolumn proc
	push dx
	mov dx, 0
	mov cx, 0
	mov cl, colLength						
	
	whileMoreRightChecks:
		mov dl, rowLength
		mov ah, 0							
		mov al, dl							;al = rowLength
		mul cl								;cl = n stores product of al and cl in ax
		dec ax								;(rowLength * n) - 1
		mov dl, [bx]
		cmp dl, al							;if offset = al ndx is in right col
		jz inRightColumn 					;Not zero means not equal
		loop whileMoreRightChecks
	jmp notInRightColumn
	inRightColumn:
		mov al, 1							;1 means it is 
		jmp exitIsRightcolumn
	notInRightColumn:
		mov al, 0
	exitIsRightcolumn:
	pop dx
	ret
isRightcolumn endp

;This subroutine checks to see if node is in bottom row. yes -> al = 1, no -> al = 0
isBottomRow proc
	push dx
	;if the ndx is >= (rowLength) * (colLengh - 1)
	mov dl, colLength
	mov dh, 0
	mov al, rowLength
	mov ah, 0
	
	dec dl
	mul dl	

	;access the resuslts
	mov dl, [bx]
	
	;dx = ndx, ax =(rowLength) * (colLengh - 1)
	cmp dx, ax
	jb notBottomRow
		mov al, 1
		jmp exitIsBottomRow
	notBottomRow:
		mov al, 0
	exitIsBottomRow:
	pop dx
	ret
isBottomRow endp

;This subrutine puts a 1 in al if in right columnn or else a 0
;Brace yourself, the pain is returning. 
;The left side is all the pieces at ndx = (rowLength) * n
;n is the set of ints from [0..colLength - 1]
isLeftcolumn proc
	push dx
	mov dx, 0
	mov cx, 0
	mov cl, colLength
	dec cl									;cl = colLength - 1
	
	;if ndx is 0, it is in the left column
	mov dl, [bx]
	cmp dl, dh
	jz isInLeftColumn						;This check won't happen durning the loop. Force it to occur first.
	whileMoreLeftChecks:
		mov dl, rowLength
		mov ah, 0							
		mov al, dl							;al = rowLength
		mul cl								;cl = n, ax stores product of al and cl
		mov dl, [bx]
		cmp dl, al							;if offset = al ndx is in right col
		jz isInLeftColumn 					;Not zero means not equal
		loop whileMoreLeftChecks	
	mov al, 0								;if it falls out of loop, it is no in left column	
	jmp exitIsLeftColumn
	isInLeftColumn:
		mov al, 1
	exitIsLeftColumn:
	pop dx
	ret
isLeftcolumn endp

;Update top neighbor's bottom if top exists
updateTopNeighbor proc
	mov di, bx								;di is maintaining the reference to the current area
	
	;init counter
	mov ch, 0
	mov cl, rowLength						;This is the number of nodes that we need to traverse
	
	mov ax, 0
	mov al, size mazeNode
	mul cl									;node size * rowLength = offset distance to top node
	
	sub bx, ax
	
	mov ah, [di + 3]						;This is the value stored in current's top
	mov [bx + 4], ah						;Load current's top into top neighbor's bottom

	mov moveNdx, bx							;Save location of node for path traversal subroutines
	mov bx, di								;return bx to it's former state
	ret
updateTopNeighbor endp

;Update right neighbor
updateRightNeighbor proc
	mov di, bx
	add bx, size mazeNode
	
	mov ah, [di + 5]						;Store original's right
	mov [bx + 6], ah						;Move into right's left
	
	mov moveNdx, bx							;Save location of node for path traversal subroutines
	mov bx, di								;restore bx
	ret
updateRightNeighbor endp

;update bottom neighbor's top if bottom exists
updateBottomNeighbor proc
	mov di, bx								;di is maintaining the reference to the current area
	
	mov ax, 0
	mov al, SIZE mazeNode					;This is a byte to make our multiply easy
	mov ch, 0
	mov cl, rowLength						;This is the number of nodes that we need to traverse

	mul cl									;node size * rowLength = offset distance to bottom node
	add bx, ax
	
	mov ah, [di + 4]						;This is the value stored in current's bottom
	mov [bx + 3], ah						;Load current's bottom into top neighbor's top
	
	mov moveNdx, bx							;Save location of node for path traversal subroutines
	mov bx, di								;return bx to it's former state
	ret		
updateBottomNeighbor endp

;update left neighbor
updateLeftNeighbor proc
	mov di, bx
	sub bx, size mazeNode
	mov ah, [di + 6]						;Store original's left
	mov [bx + 5], ah						;Move into left's right
	
	mov moveNdx, bx							;Save location of node for path traversal subroutines
	mov bx, di								;restore bx
	ret
updateLeftNeighbor endp

;Sub Routine to initialize ndx, xPos, and yPos of the maze board array
initMazeArray proc
	mov bx, beginingPoint
	
	mov al, 0								;ax holds 0 because we have filled 0 rows so far
	mov ah, 0								;This is the initial x offset			
	mov dh, 0								;This is the initial y offset
	mov dl, 0								;This is the initial ndx
	
	whileMoreRows:
		mov ah, 0							;We always want the first x offset to be 0
		mov cl, rowLength					;This is the number of rows to fill
		mov ch, 0							;Make sure ch is empty
		rowFillLoop:
			mov [bx], dl					;Fill ndx, xPos, yPos
			mov [bx + 1], ah
			mov [bx + 2], dh
			push ax							;save value
				mov al, 0					;must use register to fill memory.
				mov [bx + 3], al			;fill top, bottom, left and right with 0
				mov [bx + 4], al
				mov [bx + 5], al
				mov [bx + 6], al
			pop ax							;retrieve ax
			add bx, SIZE mazeNode			;Move to next spot in array
			inc dl							;next ndx
			add ah, 12						;next x offset value
			loop rowFillLoop				;fill next row
		inc al								;Increment rows filled count
		add dh, 12							;This is the next y offset
		cmp al, rowLength					;Check to see all rows filled
		jb whileMoreRows					;go to next row if not done				
	ret
initMazeArray endp

end main