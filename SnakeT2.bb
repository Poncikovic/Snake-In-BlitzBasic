;REWRITE MAKEFOOD() FUNCTION
;future me: WHY THO??

Graphics(600,540);;

SetBuffer BackBuffer()

SeedRnd MilliSecs()

Const portalobj = 1, wallobj = 2, foodobj = 3 ; 0 is nothing

Global gameState = 0 ;0->MainMenu , 1->InLevel , 2->SuccessMenu , 3-> FailMenu , 4->ExitGame/CleanUp

Global lastUpdateTime = 0

Global updateTimer=45;;lower=faster

Global inputStackAmount = 3;;

Global ontoDifferentStack = 0

Global inputTimeTolerance = 2;;

Dim inputStack(inputStackAmount)

Dim lvl_objectmap(1,1,3)

For i = 0 To inputStackAmount

	inputStack(i)=-1

Next

Global lvl_width,lvl_height, lvl_IsGlobe

Global lvl_startingLength, lvl_startingX, lvl_startingY

Global player_part.SnakePart = Null

;check for walls and portals staticly, check for snakes bruteforce way

While Not KeyHit(1);MainLoop

	Flip

	Cls

	Select gameState

		Case 0

			;After Level Selected

			LoadLevel(1)

			;Dim 2d with level proportions

			gameState=1

		Case 1

			PlayerSnakeInput()

			If (lastUpdateTime=updateTimer) 

					UpdateSnakeParts()

					lastUpdateTime = 0

			End If

			lastUpdateTime=lastUpdateTime+1

			DrawLevel()

			Locate 0, 0 Color 120,120,250 Write inputStack(0)+" "+inputStack(1)+" "+inputStack(2)

		Case 2

		Case 3

			gameState=0

	End Select

Wend

Type SnakePart

	Field x, y

	Field direction ;0->right 1->up 2->left 3->down

	Field prev_direction

	Field attachedTo.SnakePart

	Field lastpart.SnakePart

	Field c_Red, c_Green, c_Blue

End Type

Function CreatePlayerSnakeHead(x,y,direction,c_red,c_green,c_blue)

	player_part = New SnakePart

	player_part\x=x player_part\y=y player_part\direction=direction player_part\c_Red=c_red player_part\c_Green=c_green player_part\c_Blue=c_blue

	player_part\lastpart=player_part

End Function

Function PlayerSnakeInput()

	If ontoDifferentStack=inputTimeTolerance And (KeyDown(205) Or KeyDown(203) Or KeyDown(208) Or KeyDown(200)) 

		For i = 0 To inputStackAmount

			inputStack(i)=-1

		Next

		ontoDifferentStack=0

	End If

	If KeyHit(205) And inputStack(0)<>0 And ((Not (inputStack(0)=-1 And player_part\prev_direction =0))And (Not inputStack(0)=2))

		For i=0 To inputStackAmount-1

			inputStack(inputStackAmount-i)=inputStack(inputStackAmount-i-1)

		Next

		inputStack(0)=0

	End If

	If KeyHit(203) And inputStack(0)<>2 And ((Not (inputStack(0)=-1 And player_part\prev_direction =2))And (Not inputStack(0)=0))

		For i=0 To inputStackAmount-1

			inputStack(inputStackAmount-i)=inputStack(inputStackAmount-i-1)

		Next

		inputStack(0)=2

	End If

	If KeyHit(208) And inputStack(0)<>3 And ((Not (inputStack(0)=-1 And player_part\prev_direction =3))And (Not inputStack(0)=1))

		For i=0 To inputStackAmount-1

			inputStack(inputStackAmount-i)=inputStack(inputStackAmount-i-1)

		Next

		inputStack(0)=3

	End If

	If KeyHit(200) And inputStack(0)<>1 And ((Not (inputStack(0)=-1 And player_part\prev_direction =1))And (Not inputStack(0)=3))

		For i=0 To inputStackAmount-1

			inputStack(inputStackAmount-i)=inputStack(inputStackAmount-i-1)

		Next

		inputStack(0)=1

	EndIf

End Function

Function CreateSnakePart.SnakePart(attachedTo.SnakePart, c_red, c_green, c_blue)

	a.SnakePart = New SnakePart

	a\attachedTo=attachedTo a\x=a\attachedTo\x a\y=a\attachedTo\y a\prev_direction = a\attachedTo\prev_direction a\direction=a\attachedTo\direction a\c_Red=c_red a\c_Green=c_green a\c_Blue = c_blue

	Local head.SnakePart

	If a\attachedTo <> Null Then head=a\attachedTo Else head=a

	While head\attachedTo <> Null

		head = head\attachedTo

	Wend

	head\lastpart=a

	Return a

End Function

Function UpdateSnakeParts(); 

	;;Setting Direction

	ontoDifferentStack=ontoDifferentStack+1

	For i=0 To inputStackAmount-1

		If inputStack(inputStackAmount-1-i)<>-1

			;player_part\prev_direction=player_part\direction

			player_part\direction=inputStack(inputStackAmount-1-i)

			inputStack(inputStackAmount-1-i)=-1

			Exit

		End If

	Next

	;;Update Each and EVERY snakepart ; yes, needs to be tweaked

	a.SnakePart = Last SnakePart;go from last to first

	While True

		If a = Null Then Exit ; if null, then probably out of list. End loop

		If a\attachedTo <> Null; if has parents, just switch position to it's parent

			parenta.SnakePart = a\attachedTo

			a\x=parenta\x a\y=parenta\y a\direction = parenta\direction

			a\prev_direction = parenta\prev_direction

		Else; if it's head

			Local new_x, new_y, newold_x

			Select a\direction

				Case 0 

					;First Calculate new possible position

					new_x=a\x+1 new_y=a\y

					a\prev_direction=2

				Case 1 

					;First Calculate new possible position

					new_x=a\x new_y=a\y-1

					a\prev_direction=3

				Case 2 

					;First Calculate new possible position

					new_x=a\x-1 new_y=a\y

					a\prev_direction=0

				Case 3 

					;First Calculate new possible position

					new_x=a\x new_y=a\y+1

					a\prev_direction=1

			End Select

			If new_x>=lvl_width 

				If lvl_IsGlobe Then new_x=0 Else gameState=3

			Else If new_x<0

				If lvl_IsGlobe Then new_x=lvl_width-1 Else gameState=3

			End If

			If new_y>=lvl_width 

				If lvl_IsGlobe Then new_y=0 Else gameState=3 

			Else If new_y<0 

				If lvl_IsGlobe Then new_y=lvl_height-1 Else gameState=3

			End If

			If(gameState=3) Then Goto skipIfCannotCheckPositions

			If(lvl_objectmap(new_x,new_y,0)=portalobj)

				newold_x = new_x

				new_x = lvl_objectmap(new_x,new_y,1)+new_x-a\x

				new_y = lvl_objectmap(newold_x,new_y,2)+new_y-a\y

			End If

			;Now check for collisions

			Local fatalCollide=False

			If(lvl_objectmap(new_x,new_y,0)=wallobj) Then fatalCollide=True

			For snakecollision.SnakePart = Each SnakePart

				If(new_x = snakecollision\x And new_y = snakecollision\y) 

					fatalCollide = True

					Exit

				End If

			Next

			If Not fatalCollide 

				a\x=new_x a\y=new_y

			Else ; If Deadly Collision Accured

				If player_part = a Then gameState = 3

			End If

			If(lvl_objectmap(new_x,new_y,0)=foodobj)

				lvl_objectmap(new_x,new_y,0)=0

				CreateSnakePart(a\lastpart,a\lastpart\c_Red,a\lastpart\c_Green,a\lastpart\c_Blue);snake grows ;manage created colours later

				MakeFood(-1,-1)

			End If

		End If
		
		.skipIfCannotCheckPositions

		a = Before a ;last to first update

	Wend

End Function

Function MakeFood(x,y)

	If x<0 Or y<0 

		

		Local searchi, searched, looped

		searchi = Rand(0,(lvl_width)*(lvl_height)-1)

		searched = 0

		notfoundOffset = 0

		looped = 0

		freeExists=False

		While (searched <= searchi)

			Local tempy = Floor((searched+notfoundOffset)/(lvl_width))

			Local tempx = (searched+notfoundOffset) Mod (lvl_width)

			While(tempy-looped*(lvl_height)>=lvl_height) 

				looped = looped+1

				If(freeExists = False) Then Goto out Else freeExists=False

			Wend

			Local nosnakes = True

			For a.SnakePart = Each SnakePart

				If(a\x=tempx And a\y=tempy-looped*(lvl_height)) Then nosnakes = False Exit

			Next

			If(lvl_objectmap(tempx,tempy-looped*(lvl_height),0)=0 And nosnakes=True);add another check for snakeparts l8r

				freeExists=True

				If searched=searchi 

					lvl_objectmap(tempx,tempy-looped*(lvl_height),0) = foodobj 

					Exit

				Else 

					searched=searched+1 

				End If

			Else

				notfoundOffset = notfoundOffset+1

			End If

		If freeExists=False Then Exit

		Wend

	Else

		lvl_objectmap(x,y,0)=foodobj

	End If

.out

End Function

Function DrawLevel()

	If lvl_width=0 Or lvl_height=0 Then Return

	Local ratiox# = Float GraphicsWidth()/lvl_width

	Local ratioy# = Float GraphicsHeight()/lvl_height

	Local spacex, spacey

	If ratiox >= ratioy 

		ratio#=ratioy

		spacex = (GraphicsWidth() - GraphicsHeight())/2

	Else

		ratio# = ratiox

		spacey = (GraphicsHeight()-GraphicsWidth())/2

	End If

	;Draw Grid

	For i=0 To lvl_width-1

		For j=0 To lvl_height-1

			If (((i+j) Mod 2) = 0) Then Color 100,100,100  Else Color 85,85,85

			Rect(i*ratio+spacex,j*ratio+spacey,ratio,ratio,1)

		Next

	Next

	

	;Draw Snakes

	snakeP.SnakePart = Last SnakePart

	While(True)

		If(snakeP=Null)Exit

		Local snakePosx#= snakeP\x*ratio+spacex

		Local snakePosy#= snakeP\y*ratio+spacey

		Local halfRatio# = ratio/2

		Local LinexOffset#,LineyOffset#

		Color snakeP\c_Red, snakeP\c_Green, snakeP\c_Blue

		Rect(snakePosx,snakePosy,ratio,ratio,1)

		Color 0, 0, 255;Front

		If snakeP <> player_part

			LineXOffset=((snakeP\direction = 0) - (snakeP\direction = 2))*halfRatio

			LineYOffset=((snakeP\direction = 3) - (snakeP\direction = 1))*halfRatio

		Else

			Local nextDirection

			For i=0 To inputStackAmount-1

				If inputStack(inputStackAmount-1-i)<>-1

					nextDirection=inputStack(inputStackAmount-1-i)

					Exit

				Else 

					nextDirection = snakeP\direction

				End If

			Next

			LineXOffset = ((nextDirection = 0) - (nextDirection = 2))*halfRatio

			LineYOffset = ((nextDirection = 3) - (nextDirection = 1))*halfRatio

		End If

		Local sx=snakePosx+halfRatio

		Local sy=snakePosy+halfRatio

		;Line(sx,sy,sx+LinexOffset,sy+LineyOffset);Slower than drawing rectangles

		If LinexOffset < 0 

			Rect(sx+LinexOffset,sy,-LinexOffset+1,1)

		Else If LinexOffset > 0

			Rect(sx,sy,LinexOffset+1,1)

		Else If LineyOffset < 0

			Rect(sx,sy+LineyOffset,1,-LineyOffset+1)

		Else If LineyOffset > 0

			Rect(sx,sy,1,LineyOffset+1)

		End If

		Color 255, 0, 0;Back

		LineXOffset=((snakeP\prev_direction = 0) - (snakeP\prev_direction = 2))*halfRatio

		LineYOffset=((snakeP\prev_direction = 3) - (snakeP\prev_direction = 1))*halfRatio

		;Line(sx,sy,sx+LineXOffset,sy+LineYOffset);Slower than drawing rectangles

		If LinexOffset < 0 

			Rect(sx+LinexOffset,sy,-LinexOffset+1,1)

		Else If LinexOffset > 0

			Rect(sx,sy,LinexOffset+1,1)

		Else If LineyOffset < 0

			Rect(sx,sy+LineyOffset,1,-LineyOffset+1)

		Else If LineyOffset > 0

			Rect(sx,sy,1,LineyOffset+1)

		End If

		snakeP = Before snakeP

	Wend 

	;Draw map objects

	For i=0 To lvl_width-1

		For j=0 To lvl_height-1

			Select(lvl_objectmap(i,j,0))

				Case portalobj

					Color 120,20,120

					Rect(i*ratio+spacex,j*ratio+spacey,ratio,ratio,1)

				Case wallobj

					Color 28,28,28

					Rect(i*ratio+spacex,j*ratio+spacey,ratio,ratio,1)

				Case foodobj

					Color 220,190,190

					Rect(i*ratio+spacex,j*ratio+spacey,ratio,ratio,1)

			End Select

		Next

	Next

End Function

Function LoadLevel(levelNumber)

	;Reset Things

	lastUpdateTime=0

	ontoDifferentStack=0

	For i = 0 To inputStackAmount

		inputStack(i)=-1

	Next

	Delete Each SnakePart

	;Read Level Data

	Select levelNumber

		Case 1

			Restore data_lvl1

	End Select

	Read lvl_width, lvl_height

	Dim lvl_objectmap(lvl_width,lvl_height,3);;0th for type, for portals, 1st for connectionx, 2nd for connectiony

	Read wallCount

	For i=0 To wallCount-1

		;Local tempx,tempy,tempr,tempg,tempb

		;Read tempx, tempy, tempr, tempg, tempb

		;a.Wall = MakeWall(tempx, tempy ,tempr, tempg, tempb)

		;lvl_walls(i) = a

		Read tempx, tempy

		lvl_objectmap(tempx, tempy,0) = wallobj

	Next

	Read portalCount

	For i=0 To portalCount-1

		;Local tempid, tempc

		;Read tempx, tempy, tempid, tempc

		;b.Portal = MakePortal(tempx, tempy ,tempid, tempc)

		;lvl_portals(i) = b

		Read tempx, tempy

		Read targetx, targety

		lvl_objectmap(tempx,tempy,0) = portalobj

		lvl_objectmap(tempx,tempy,1) = targetx

		lvl_objectmap(tempx,tempy,2) = targety

	Next

	Read lvl_IsGlobe

	Read lvl_startingLength,lvl_startingX,lvl_startingY

	CreatePlayerSnakeHead(lvl_startingX,lvl_startingY,1,20,200,20)

	For i=0 To lvl_startingLength-1

		CreateSnakePart(Last SnakePart,140,250,120)

	Next

	Read foodCount

	For i=0 To foodCount-1

		MakeFood(-1,-1)

	Next

End Function

;lvl datas-> lvl_width, lvl_height, lvl_walls, lvl_portals, lvl_IsGlobe, lvl_startingLength, lvl_startingX, lvl_startingY, starting_foodcount
.data_lvl1

Data 10, 10;lvl_width, lvl_height

Data 2 ; wallcount

Data 3,2 ; wallx, wally

Data 9,8 ; 

Data 2 ; number of portals , usually should be an even number

Data 2,2,8,8 ; x,y,targetx,targety

Data 8,8,2,2 ; 

Data True;Is Globe

Data 5,5,5;length, locationx, locationy

Data 5;Food Count