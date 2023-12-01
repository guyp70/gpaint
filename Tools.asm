; =================
; Paste Button
; Lets the user select a point to paste the rect to, should only be used after Copy_Button proc
; input: CP_Temp_File file and Vars in DATASEG - CP_Height,CP_Length,CP_Temp_File_Name_Var
; output: Screen Memory
; Special Exit Condition: Enter (interrupt's exit condition)
; =================
proc Paste_Button	 
	 
	 cmp [CP_Height],0 ; checks if either CP_Height or CP_Length are 0, because if even one is 0 - there is nothing to paste
	 jz @@Nothing_To_Paste
	 cmp [CP_Height],0
	 jNz @@Something_To_Paste
@@Nothing_To_Paste:
	 call Nothing_To_Paste_Error
	 jmp @@End_Proc
@@Something_To_Paste:
	 
@@Check_If_Clicked_Again:
	 call Get_Mouse_Position_and_Button_Status
	 test ax,1     ;   |F-8|7|6|5|4|3|2|1|0|  Button Status (bit fields)
	 jnz @@Set_XY;   |  | | | | | | | `---- left button (1 = pressed)
	 		       ;     |  | | | | | | `----- right button (1 = pressed)
		           ;     |  | | | | | `--- middle button  (1 = pressed)
				   ;     `------------------- unused
	 
	 test ax,2     ;if right mouse button is clicked, exit proc
	 jnz @@End_Proc
	 
	 jmp @@Check_If_Clicked_Again
	 
@@Set_XY:
	 mov [CP_X],cx
	 mov [CP_Y],dx
	 
	 call Read_Rect_From_CP_Temp_File_To_Screen
	 
@@End_Proc:
ret
endp Paste_Button

; =================
; Read Rect From CP Temp File To_Screen
; Pastes the rect to the location provided, should only be used after Copy_Button proc
; input: CP_Temp_File file, Vars in DATASEG - CP_X,CP_Y,CP_Height,CP_Length,CP_Temp_File_Name_Var
; output: Screen Memory
; =================
proc Read_Rect_From_CP_Temp_File_To_Screen
stop:
@@Temp_FileHandle equ [word ptr bp-2] ;will store [CP_Length] as it will be unreachable (ds will be altered)
@@Paste_Height equ [word ptr bp-4]
@@Paste_Length equ [word ptr bp-6]
@@Paste_Screen_Width_Exception equ [word ptr bp-8]
@@Num_Of_Vars equ 4
	 push bp
	 mov bp,sp
	 sub sp,@@Num_Of_Vars*2 ;allocates space for local var
	 
	 push ds
	 push ax
	 push bx
	 push cx
	 push dx
	 push si	 
	 
	 mov ah,03dh ; Open temp file
	 mov dx,offset CP_Temp_File_Name_Var
	 mov al,0
	 int 21h
	 mov @@Temp_FileHandle,ax ; copies Temp file handle to bx

	 
	 mov ax,[CP_Height] ;checks if the lowest line of the rect goes beyond the max allowed Y
	 mov @@Paste_Height,ax
	 add ax,[CP_Y]
	 cmp ax,Screen_Height
	 jb @@Does_Not_Go_Beyond_Max_Y
	 sub ax,Screen_Height; PressY+Height - Screen_Height = how much the rect exceeds the Screen_Height
	 sub @@Paste_Height,ax
@@Does_Not_Go_Beyond_Max_Y:

	 mov @@Paste_Screen_Width_Exception,0 ;becaues it is a place in stack we must zero it (there is a random word there before this line)
	 mov ax,[CP_Length] ;checks if the right most column of the rect goes beyond the max allowed x
	 mov @@Paste_Length,ax
	 add ax,[CP_X]
	 cmp ax,Screen_Width
	 jb @@Does_Not_Go_Beyond_Max_X
	 sub ax,Screen_Width
	 mov @@Paste_Screen_Width_Exception,ax; PressY+Height - Screen_Height = how much the rect exceeds the Screen_Height
	 sub @@Paste_Length,ax
@@Does_Not_Go_Beyond_Max_X:

	 mov cx,[CP_X] ; preps to read the rect
	 mov dx,[CP_Y]
	 call Calc_Loc_In_Screen_Memory
	 mov dx,ax
	 mov bx,@@Temp_FileHandle
	 mov cx,@@Paste_Length
	 mov ax,0a000h ; set ds to the start of screen memory
	 mov ds,ax
	 mov si, @@Paste_Height ; how many times to loop
	
	 call Hide_Mouse_Cursor
	 
	 cmp @@Paste_Screen_Width_Exception,0  ; Checks if to use more efficient loop or the one capable of handling width exception
	 jnz @@Check_If_To_Read_Another_Line_WITH_Width_Exception ; Loop capable of handling Paste_Length+CP_X that exceeds Screen_Width
	 
@@Check_If_To_Read_Another_Line_WITHOUT_Width_Exception: 
	 cmp si,0
	 jz @@ExitLoop
	 dec si
	 
	 mov ah,03fh ;read file 
	 mov cx,@@Paste_Length ; changes in LSEEK
	 int 21h
	 add dx,Screen_Width
	 push dx ; saving  to restore after LSEEK
	 
	 mov ah,42h ;LSEEK
	 mov al,1d
	 xor cx,cx
	 mov dx,@@Paste_Screen_Width_Exception
	 int 21h
	 
	 pop dx ;restore dx for Read_File int
	 jmp @@Check_If_To_Read_Another_Line_WITHOUT_Width_Exception
	 
@@Check_If_To_Read_Another_Line_WITH_Width_Exception:
	 cmp si,0
	 jz @@ExitLoop
	 dec si
	 
	 mov ah,03fh ;read file 
	 mov cx,@@Paste_Length ; changes in LSEEK
	 int 21h
	 add dx,Screen_Width
	 push dx ; saving  to restore after LSEEK
	 
	 mov ah,42h ;LSEEK
	 mov al,1d
	 xor cx,cx
	 mov dx,@@Paste_Screen_Width_Exception
	 int 21h
	 
	 pop dx ;restore dx for Read_File int
	 jmp @@Check_If_To_Read_Another_Line_WITH_Width_Exception
	 
@@ExitLoop:	 
	 mov ah,03eh ;Closes temp file 
	 mov bx,@@Temp_FileHandle
	 int 21h
	 
	 call Show_Mouse_Cursor
	 
@@End_Proc:
	 pop si
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 pop ds
	 
	 add sp,@@Num_Of_Vars*2;freeing space allocated to local var
	 pop bp
ret
endp Read_Rect_From_CP_Temp_File_To_Screen

; =================
; Prints CP_Error_Msg
; input: none
; output: none
; =================
proc Nothing_To_Paste_Error
	 
	 call Reset_Graphic_Cursor_Position
	 
	 mov ah,09
	 mov dx,offset CP_Error_Msg
	 int 21h
	 
ret
endp Nothing_To_Paste_Error

;Deletes Temp Files, should be done at exit 
; =================
; Delete Temps If Exits
; Deletes all Temporary files If they exist
; input: Vars in DATASEG - CP_Temp_File_Name_Var,CtrlZ_Temp_File_Name_Var
; output: none
; =================
proc Delete_Temps_If_Exits
	 push ax
	 push dx
	 
	 mov ah,41h
	 mov dx, offset CP_Temp_File_Name_Var
	 int 21h
	 
	 mov ah,41h
	 mov dx, offset CtrlZ_Temp_File_Name_Var
	 int 21h
	 
	 pop dx
	 pop ax
ret
endp Delete_Temps_If_Exits

;Lets the user select a rect to copy then copies data to Temp file
; =================
; Copy Button
; lets the user select a rectangular piece of the screen and copies it to CP_Temp_File
; input: none
; output: to CP_Temp_File (data is not stored as bmp or as any standard format)
; Special Exit Condition: Right Click
; =================
proc Copy_Button 
	 
	 call Select_Rect_To_Copy
	 
	 Call Store_Rect_In_TEMP
	 
ret
endp Copy_Button

; =================
; Store Rect In TEMP
; saves data in selected rect to file 
; input: vars in DATASEG - CP_Height,CP_Length,CP_X,CP_Y
; output: to CP_Temp_File (data is not stored as bmp or as any standard format)
; =================
proc Store_Rect_In_TEMP
@@Temp_FileHandle equ [bp-2] 
	 push bp
	 mov bp,sp
	 sub sp,2d ;allocates spce for local var
	 
	 push ds
	 push ax
	 push bx
	 push cx
	 push dx
	 
	 
	 mov ah,03ch ; Create Temp file or reduce to 0 if exits
	 xor cx,cx
	 mov dx, offset CP_Temp_File_Name_Var
	 int 21h
	 mov @@Temp_FileHandle,ax

	 call Hide_Mouse_Cursor
	 
	 mov bx,@@Temp_FileHandle ;preps for writing to file
	 mov dx,[CP_Y];calc from where to write in screen memory
	 mov cx,[CP_X]
	 call Calc_Loc_In_Screen_Memory
	 mov dx,ax 
	 mov di,[CP_Length] ;cx is reset every time we write so it will be restored from di	 
	 mov cx,[CP_Height] ;How many times to run loop
	 mov ax,0a000h; sets ds to start of the screen memory - until ds is restored, DATASEG vars won't be accessible
	 mov ds,ax
	 
@@Copy_Another_Line:
	 push cx
	 
	 mov ah,040h;wirte to file
	 mov cx,di ; di stores how many byte we need to store each line
	 int 21h
	 
	 add dx,Screen_Width 
	 
	 pop cx
	 loop @@Copy_Another_Line
	 
	 
	 mov ah,03eh ;CLOSE TEMP FILE (bx already loaded with [Temp_FileHandle]
	 int 21h
	 
	 call Show_Mouse_Cursor
	 
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 pop ds
	 
	 add sp,2;freeing space allocated to local var
	 pop bp
ret
endp Store_Rect_In_TEMP

; =================
; Select Rect To Copy
; the user draws a rect on screen using the mouse and the proc makes perparations for Store_Rect_In_TEMP proc activation
; input: none
; output: vars in DATASEG: CP_Height,CP_Length,CP_X,CP_Y
; Special Exit Condition: Right Click
; =================
proc Select_Rect_To_Copy
	 push ax
	 push bx
	 push cx
	 push dx
	 
@@Check_If_Clicked_Again:
	 call Get_Mouse_Position_and_Button_Status
	 test ax,1     ;   |F-8|7|6|5|4|3|2|1|0|  Button Status (bit fields)
	 jnz @@Set_XY;   |  | | | | | | | `---- left button (1 = pressed)
	 		       ;     |  | | | | | | `----- right button (1 = pressed)
		           ;     |  | | | | | `--- middle button  (1 = pressed)
				   ;     `------------------- unused
	 
	 test ax,2     ;if right mouse button is clicked, exit proc
	 jnz @@Right_Click
	 
	 jmp @@Check_If_Clicked_Again
	 
@@Set_XY:
	 mov [CP_X],cx
	 mov [CP_Y],dx
	 
@@Check_If_Released_Again:
	 call Get_Mouse_Position_and_Button_Status
	 test ax,1     				;   |F-8|7|6|5|4|3|2|1|0|  Button Status (bit fields)
	 jz @@Set_Height_And_Length ;   |  | | | | | | | `---- left button (1 = pressed)
								;     |  | | | | | | `----- right button (1 = pressed)
								;     |  | | | | | `--- middle button  (1 = pressed)
								;     `------------------- unused
	 
	 test ax,2     ;if right mouse button is clicked, exit proc
	 jnz @@Right_Click
	 
	 jmp @@Check_If_Released_Again
	 
@@Set_Height_And_Length:

	 sub cx,[CP_X]; ReleaseX - PressX = Length
	 cmp cx,0
	 jz @@End_Proc ; if either height or length is 0, there is nothing to copy
	 jg @@Skip_Change_X_And_abs_Length ;IF Positive 
	 
	 add [CP_X],cx;IF Negative
	 mov ax,cx
	 call Return_AX_Absolute
	 mov cx,ax
@@Skip_Change_X_And_abs_Length:
	 mov [CP_Length],cx 
	
	
	 sub dx,[CP_Y]; ReleaseY - PressY = Height
	 cmp dx,0
	 jz @@End_Proc ; if either height or length is 0, there is nothing to copy
	 jg @@Skip_Change_Y_And_abs_Height ;IF Positive 
	 
	 add [CP_Y],dx;IF Negative
	 mov ax,dx
	 call Return_AX_Absolute
	 mov dx,ax
@@Skip_Change_Y_And_abs_Height:
	 mov [CP_Height],dx
	 
	 
	 jmp @@End_Proc
@@Right_Click:
	 mov [CP_Length],0
	 mov [CP_Height],0
@@End_Proc: 
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 
ret
endp Select_Rect_To_Copy

; =================
; Clone Stamp Button
; Point to copy from is set first with a click of the middle mouse button
; Point to set Relativity is set with the first click from the left mouse button
; input: Var in DATASEG: Thickness
; output: Screen Memory
; Special Exit Condition: Right Click
; =================
proc Clone_Stamp_Button
@@Diffecrnce_Between_Point2_And_Point1 equ [word ptr bp-2]
	 push bp
	 mov bp,sp
	 sub sp,2
	 
	 push ax
	 push bx
	 push cx
	 push dx
	 
@@Wait_For_First_Click:
	 call Get_Mouse_Position_and_Button_Status
	 
	 test ax,4     ;   |F-8|7|6|5|4|3|2|1|0|  Button Status (bit fields)
	 jnz @@Set_Point1;   |  | | | | | | | `---- left button (1 = pressed)
	 		       ;     |  | | | | | | `----- right button (1 = pressed)
		           ;     |  | | | | | `--- middle button  (1 = pressed)
				   ;     `------------------- unused
	 
	 test ax,2     ;if right mouse button is clicked, exit proc
	 jnz @@End_Proc
	 
	 jmp @@Wait_For_First_Click 
	 
@@Set_Point1:

	 call Calc_Loc_In_Screen_Memory;calcs the relative address of the byte the was pressed
	 mov bx, ax                                                                                                            ;<|
	 mov ax, [Thickness]                                                                                                   ;<|
	 shr ax,1;split the thickness as the mouse should be in the middle                                                     ;<|
	 sub bx,ax ;so that the click will be in the middle horizontaly                                                        ;<|
	 mov dx,Screen_Width ;so that the click will be in the middle verticaly (Sceren_Width cause here we need to sub lines) ;<| calcs the address of the top left byte ot the rectangle the is printed 
	 mul dx                                                                                                                ;<|
	 sub bx, ax                                                                                                            ;<|
	 mov di,bx ;di contains Point1                                                                                         ;<|
	 
@@Wait_For_Second_Click:
	 call Get_Mouse_Position_and_Button_Status
	 
	 test ax,2     ;   |F-8|7|6|5|4|3|2|1|0|  Button Status (bit fields)
	 jnz @@End_Proc;     |  | | | | | | | `---- left button (1 = pressed)
	 		       ;     |  | | | | | | `----- right button (1 = pressed)
		           ;     |  | | | | | `--- middle button  (1 = pressed)
				   ;     `------------------- unused
	 test ax,1    
	 jnz @@Set_Point2 

	 jmp @@Wait_For_Second_Click 
	 
@@Set_Point2:

	 
	 call Calc_Loc_In_Screen_Memory ;calcs the relative address of the byte the was pressed
	 mov bx, ax                                                                                                            ;<|
	 mov ax, [Thickness]                                                                                                   ;<|
	 shr ax,1;split the thickness as the mouse should be in the middle                                                     ;<|
	 sub bx,ax ;so that the click will be in the middle horizontaly                                                        ;<|
	 mov dx,Screen_Width ;so that the click will be in the middle verticaly (Sceren_Width cause here we need to sub lines) ;<| calcs the address of the top left byte ot the rectangle the is printed 
	 mul dx                                                                                                                ;<|
	 sub bx, ax                                                                                                            ;<|
	 mov si,bx ;si contains Point2                                                                                         ;<|
	 
	 ;@@Diffecrnce_Between_Point2_And_Point1=Point2-Point1=si-di
	 mov @@Diffecrnce_Between_Point2_And_Point1,si
	 sub @@Diffecrnce_Between_Point2_And_Point1,di
	 
	 @@Check_Mouse_Status_Again:
	 call Get_Mouse_Position_and_Button_Status
	 
	 test ax,2     ;	|F-8|7|6|5|4|3|2|1|0|  Button Status (bit fields)
	 jnz @@End_Proc ;     |  | | | | | | | `---- left button (1 = pressed)
		           ;	  |  | | | | | | `----- right button (1 = pressed)
		           ;      |  | | | | | `--- middle button  (1 = pressed)
				   ;      `------------------- unused
	 test ax,1
	 jz @@Check_Mouse_Status_Again ;if the button isn't pressed
	 ;If it is pressed
	 call Hide_Mouse_Cursor
	 call Calc_Loc_In_Screen_Memory
	 mov bx, ax                                                                                                            ;<|
	 mov ax, [Thickness]                                                                                                   ;<|
	 shr ax,1;split the thickness as the mouse should be in the middle                                                     ;<|
	 sub bx,ax ;so that the click will be in the middle horizontaly                                                        ;<|
	 mov dx,Screen_Width ;so that the click will be in the middle verticaly (Sceren_Width cause here we need to sub lines) ;<| calcs the address of the top left byte ot the rectangle that is printed 
	 mul dx                                                                                                                ;<|
	 sub bx, ax                                                                                                            ;<|
	 mov ax,bx                                                                                                             ;<|
	 
	 mov si,ax
	 mov di,ax
	 sub di,@@Diffecrnce_Between_Point2_And_Point1
	 call Clone_Stamp
	 call Show_Mouse_Cursor
	 
	 jmp @@Check_Mouse_Status_Again ;Check againg, the only real exit condition from this loop is right click
	 
@@End_Proc:
	 
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 
	 add sp,2
	 pop bp
	 
ret
endp Clone_Stamp_Button

; =================
; Clone Stamp 
; copies a squere around Point1 to Point2
; input: recieves Point1 loacation in Screen Memory in di and Point2 loacation in Screen Memory in si
; output: Screen Memory, si and di are destroyed
; =================
proc Clone_Stamp
@@Pointer1 equ di
@@Pointer2 equ si

	 push es
	 push ax
	 push bx
	 push cx
	 push dx
	 
	 cmp si, Screen_Width*UI_Height ;checks if Point1 targets UI zone data
	 jbe @@SI_Exception
	 mov bx,si  ;calcs location where the Clone Stamp ends and checks also if it targets UI zone data
	 mov ax, [Thickness]
	 mov dx,Screen_Width
	 mul dx
	 add bx, ax
	 cmp bx, Screen_Width*Screen_Height
	 jae @@SI_Exception
	 cmp bx, Screen_Width*UI_Height
	 ja @@Skip_Handle_SI_Exception
@@SI_Exception:
	 jmp @@End_Proc
@@Skip_Handle_SI_Exception:
	 
	 cmp di, Screen_Width*UI_Height ;checks if Point1 targets UI zone data
	 jbe @@DI_Exception
	 mov bx,di  ;calcs location where the Clone Stamp ends and checks also if it targets UI zone data
	 mov ax, [Thickness]
	 mov dx,Screen_Width
	 mul dx
	 add bx, ax
	 cmp bx, Screen_Width*Screen_Height
	 jae @@DI_Exception
	 cmp bx, Screen_Width*UI_Height
	 ja @@Skip_Handle_DI_Exception 
@@DI_Exception:
	 jmp @@End_Proc
@@Skip_Handle_DI_Exception:

	 mov ax,0a000h
	 mov es,ax
	 
	 mov cx,[Thickness]
@@Print_Anoter_Line:
	 push cx
	 
	 mov cx,[Thickness]
	 @@Print_Another_Pixel:
		 mov al, [byte ptr es:@@Pointer1]
		 mov [byte ptr es:@@Pointer2],al
		 inc @@Pointer1
		 inc @@Pointer2
		 loop @@Print_Another_Pixel
	 
	 sub @@Pointer1,[Thickness] ;returns it to the start of the eraser line
	 sub @@Pointer2,[Thickness]
	 add @@Pointer1,Screen_Width;go down a line
	 add @@Pointer2,Screen_Width
	 
	 pop cx
	 loop @@Print_Anoter_Line
	 
	 
@@End_Proc:
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 pop es
	 
ret
endp Clone_Stamp

; =================
; Fill Button (RECURSIVE)
; color all adjacent pixels with the same color to [Color] 
; input: var in DATASEG: [Color]
; output: Screen Memory
; Special Exit Condition: Right Click
; =================
proc Fill_Button
	 
	 push ax
	 push bx
	 push cx
	 push dx
	 
	 
@@Check_Mouse_Status_Again:
	 call Get_Last_Left_Press_Coordinates
	 
	 test ax,2     ;	|F-8|7|6|5|4|3|2|1|0|  Button Status (bit fields)
	 jnz @@End_Proc ;     |  | | | | | | | `---- left button (1 = pressed)
		           ;	  |  | | | | | | `----- right button (1 = pressed)
		           ;      |  | | | | | `--- middle button  (1 = pressed)
				   ;      `------------------- unused
	 test ax,1
	 jz @@Check_Mouse_Status_Again ;if the button isn't pressed
	 ;If it is pressed
	 call Hide_Mouse_Cursor
	 call Fill
	 call Show_Mouse_Cursor
	 jmp @@Check_Mouse_Status_Again ;last resort
	 
@@End_Proc:
	 
	 
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 
ret
endp Fill_Button


; =================
; Fill - case proc (RECURSIVE)
; Handles input and calling to Fill_Core
; input: var in DATASEG: [Color], last left mouse click info
; output: Screen Memory
; =================
proc Fill
	 
	 call Get_Last_Left_Press_Coordinates
	 
	 mov ah,0dh ;checks what color
	 mov bh,0
	 int 10h
	 mov bl,al ;put to bl the color to fill
	 cmp al,[Color] ;checks if color to fill = [Color] 
	 je @@End_Proc ; skips if it is as it will cause Fill_Core to enter an endless loop
	 call Fill_Core
@@End_Proc:
ret
endp Fill

; =================
; Fill Core (TIP: Don't use, Use Fill)
; BE AWERE! You shouldn't use this proc as it requires to fill the input manually and lacks certein safeguareds, use Fill Instead
; A recursive proc that checks if the color is the color inputed in dh and if yes changes it to [Color]
; input: cx = X, dx = Y, bl = Color to Fill,
;       bh = page number (in dosbox, always 0),ah = 0dh
; output: Screen Memory
; Based on Yossi Zehavi's Flood in bmp program
; =================
proc Fill_Core
	 
	 
	 cmp cx,Screen_Width ;checks if fill reached the right edge of the screen
	 jae ExitFillNoPush
	 cmp cx,0 ;checks if fill reached the left edge of the screen
	 je ExitFillNoPush
	 cmp dl,Screen_Height ;checks if fill reached the lower esge of the screen
	 jae ExitFillNoPush 
	 cmp dl,UI_Height ;checks if the fill reached the UI zone
	 jbe ExitFillNoPush 
	 cmp sp, Min_Allowed_Value_Of_SP; checkes if the stack has reached a point where it is unsafe to push any more
	 jbe ExitFillNoPush
	 
	 push cx
	 push dx
	 
	 dec ah ;ah=0ch - Write Graphics Pixel at Coordinate
	 mov al,[Color]
	 int 10h
	 inc ah ;ah=0dh - Read Graphics Pixel at Coordinate

	 
	 dec dx
	 int 10h; get color of a single pixel (ah=0dh)
	 cmp bl,al ;check if to fill (compares colors)
	 jnz Skip_Up
	 call Fill_Core
	 Skip_Up:
	 
	 add dx,2
	 int 10h; get color of a single pixel (ah=0dh)
	 cmp bl,al;check if to fill (compares colors)
	 jnz Skip_Down
	 call Fill_Core
	 Skip_Down:
	 
	 dec dx
	 inc cx
	 int 10h; get color of a single pixel (ah=0dh)
	 cmp bl,al;check if to fill (compares colors)
	 jnz Skip_Right
	 call Fill_Core
	 Skip_Right:
	 
	 sub cx,2
	 int 10h; get color of a single pixel (ah=0dh)
	 cmp bl,al;check if to fill (compares colors)
	 jnz Skip_Left
	 call Fill_Core
	 Skip_Left:
	 
End_Fill_Core:
	 pop dx
	 pop cx
ExitFillNoPush:
ret
endp Fill_Core


; =================
; Sampler Button
; changes [Color] to the color of pixel the user presses with the mouse left button
; input: Screen Memory
; output: var in DATASEG: [Color]
; Special Exit Condition: Right Click
; =================
proc Sampler_Button
	 
	 push ax
	 push bx
	 push cx
	 push dx
	 
@@Check_Mouse_Status_Again:
	 call Get_Last_Left_Press_Coordinates
	 
	 test ax,2     ;	|F-8|7|6|5|4|3|2|1|0|  Button Status (bit fields)
	 jnz @@End_Proc ;     |  | | | | | | | `---- left button (1 = pressed)
		           ;	  |  | | | | | | `----- right button (1 = pressed)
		           ;      |  | | | | | `--- middle button  (1 = pressed)
				   ;      `------------------- unused
	 test ax,1
	 jz @@Check_Mouse_Status_Again ;if the button isn't pressed
	 ;If it is pressed
	 call Sampler
	 call Update_Color_Rect_In_UI
	 
	 jmp @@Check_Mouse_Status_Again ;last resort
	 
@@End_Proc:
	 
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 
ret
endp Sampler_Button

; =================
; Update Color Rect In UI
; Updates the Rect In UI that shows what color is chosen to the curren value of Color var
; input: var in DATASEG: [Color]
; output: Screen Memory
; =================
proc Update_Color_Rect_In_UI
	  
	 call Hide_Mouse_Cursor
	 
	 mov ax,23d ;height
	 push ax
	 
	 mov ax,16d ; length
	 push ax
	 
	 mov ax,119d ;x
	 push ax
	 
	 mov ax,18d ;y
	 push ax
	 
	 call Draw_Rect
	 
	 call Show_Mouse_Cursor
ret
endp Update_Color_Rect_In_UI

; =================
; Sampler
; changes [Color] to the color of pixel the user last pressed with the mouse left button
; input: Screen Memory
; output: var in DATASEG: [Color]
; =================
proc Sampler
	 
	 push es
	 push ax
	 push bx
	 push cx
	 push dx
	 
	 call Hide_Mouse_Cursor ; otherwize it'll take the black pixel from the mouse poiter's tip
	 
	 call Get_Mouse_Position_and_Button_Status	 
	 call Calc_Loc_In_Screen_Memory
	 mov bx,ax ;because Es:ax is illegal
	 
	 ;push ax ;debug
	 ;mov ax,cx
	 ;call ShowAXDecimal
	 ;mov ax,dx
	 ;call ShowAXDecimal
	 ;mov ax,bx 
	 ;call ShowAXDecimal
	 ;pop ax
	 
	 mov ax,0a000h
	 mov es,ax
	 
	 mov al,[byte ptr es:bx]
	 mov [byte ptr Color],al
	 
	 call Show_Mouse_Cursor
	 
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 pop es
ret
endp Sampler

; =================
; Print Eraser Button
; Paint where the user presses a squere the length of [Thickness] in White 
; input: none
; output: Screen Memory
; Special Exit Condition: Right Click
; =================
proc Print_Eraser_Button
	 push ax
	 
	 mov al,[Color] ;save [Color] for later
	 mov [Color],White
	 call Pencil_Button
	 mov [Color],al ; restore [Color]
	 
	 pop ax
ret
endp Print_Eraser_Button

; =================
; Pencil Button
; Paints where the user presses a squere the length of [Thickness] in [Color] 
; input: var in DATASEG: [Color] 
; output: Screen Memory
; Special Exit Condition: Right Click
; =================
proc Pencil_Button
	 
	 push ax
	 push bx
	 push cx
	 push dx
	 
@@Check_Mouse_Status_Again:
	 call Get_Mouse_Position_and_Button_Status
	 
	 test ax,2     ;	|F-8|7|6|5|4|3|2|1|0|  Button Status (bit fields)
	 jnz @@End_Proc ;     |  | | | | | | | `---- left button (1 = pressed)
		           ;	  |  | | | | | | `----- right button (1 = pressed)
		           ;      |  | | | | | `--- middle button  (1 = pressed)
				   ;      `------------------- unused
	 test ax,1
	 jz @@Check_Mouse_Status_Again ;if the button isn't pressed
	 ;If it is pressed
	 call Hide_Mouse_Cursor
	 call Print_Pencil 
	 call Show_Mouse_Cursor
	 jmp @@Check_Mouse_Status_Again ;last resort
	 
@@End_Proc:
	 
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 
ret
endp Pencil_Button

; =================
; Print_Pencil
; Paints a squere the length of [Thickness] in [Color] around the last left mouse button click location
; input: var in DATASEG: [Color] 
; output: Screen Memory
; =================
proc Print_Pencil
	 
	 push es
	 push ax
	 push bx
	 push cx
	 push dx
	 
	 call Get_Mouse_Position_and_Button_Status
	 
	 mov ax,0a000h
	 mov es,ax
	 
	 mov ax,dx; calcing current position in screen memory
	 mov dx,Screen_Width;muls Y val by Screen_Width
	 mul dx
	 mov bx,ax;stores the sum in bx
	 add bx,cx ; adds the X value
	 
	 mov ax, [Thickness]
	 shr ax,1;split the thickness as the mouse should be in the middle
	 sub bx,ax ;so that the click will be in the middle horizontaly
	 mov dx,Screen_Width ;so that the click will be in the middle verticaly (cause here we need to sub lines)
	 mul dx
	 sub bx, ax
	 
	 mov al,[byte ptr Color];preping the Color
	 
	 mov cx,[Thickness]
@@Print_Anoter_Line:
	 push cx
	 
	 mov cx,[Thickness]
@@Print_Another_Pixel:
	 mov [byte ptr es:bx],al
	 inc bx
	 loop @@Print_Another_Pixel
	 
	 sub bx,[Thickness] ;returns it to the start of the eraser line
	 add bx,Screen_Width;go down a line
	 
	 pop cx
	 loop @@Print_Anoter_Line
	 
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 pop es
	 
ret
endp Print_Pencil 

; =================
; Print Text Button
; allows the user to write using interrupt ah=9h of int 21h family at the location he presses the left mouse butoon  
; input: none
; output: Screen Memory
; Special Exit Condition: Right Click
; =================
proc Print_Text_Button 
	 push ax
	 push bx
	 push cx
	 push dx
	 
	 
@@Check_Mouse_Status_Again:
	 mov ax, 5;Get Left Mouse Button Press Information
	 mov bx,0
	 int 33h
	 
	 test ax,2     ;	|F-8|7|6|5|4|3|2|1|0|  Button Status (bit fields)
	 jnz @@End_Proc ;     |  | | | | | | | `---- left button (1 = pressed)
		           ;	  |  | | | | | | `----- right button (1 = pressed)
		           ;      |  | | | | | `--- middle button  (1 = pressed)
				   ;      `------------------- unused
	 cmp bx,0; bx contains the number of clicks on the left button since last time the int was called
	 je @@Check_Mouse_Status_Again ;if the button wasn't pressed
	 call Hide_Mouse_Cursor
	 call Print_Text ;If it was pressed
	 call Show_Mouse_Cursor
	 jmp @@Check_Mouse_Status_Again ;last resort
	 
@@End_Proc:
	 
	 pop dx
	 pop cx
	 pop bx
	 pop ax
ret
endp Print_Text_Button
 
; =================
; Print Text
; allows the user to write using interrupt ah=9h of int 21h family at the location he last pressed the left mouse butoon  
; input: none
; output: Screen Memory
; Special Exit Condition: Enter (interrupt's exit condition)
; =================
 proc Print_Text
	 push ax
	 push bx
	 push cx
	 push dx
	 
	 
	 call Get_Last_Left_Press_Coordinates
	 ;<-----------------------------------------------check why dividing cx by 8 works and not 2
	 shr cx,3 ;divide X value by 4 cause the change cursor interrupt recieves coordinates on a 80X25 grid
	 shr dx,3 ;divide Y value by 8 cause the change cursor interrupt recieves coordinates on a 80X25 grid
	 
	 mov ah,2 ;change cursor position
	 mov bh,0
	 mov dh,dl
	 mov dl,cl
	 int 10h
	 
	 mov ah,0ah ;recieves input to text while also shows it on screen (there is no real relevance to storing the info)
	 mov dx,offset Text
	 int 21h
	 
	 
     pop dx
	 pop cx
	 pop bx
	 pop ax
 ret
 endp Print_Text