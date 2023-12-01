; =================
; Open File By CommandLine
; Loads bmp file with name specified in CommandLine var at canvas  
; input: var in DATASEG - CommandLine
; output: Screen Memory
; =================
proc Open_File_By_CommandLine
	 
	 push ax
	 
	 call Read_Command_Line_From_PSP
	 cmp [CommandLine_Length],0
	 jz @@End_Proc
	 cmp [CommandLine_Length],12d ;dosbox filename length limit is 12d
	 ja @@CommandLine_Invalid
	 
	 push offset CommandLine
	 mov ax,0
	 push ax
	 mov ax,UI_Height
	 push ax
	 call Load_BMP_File
	 
	 jmp @@End_Proc
@@CommandLine_Invalid:
	 call Command_Line_Invalid_Error
@@End_Proc:
	 pop ax
ret
endp Open_File_By_CommandLine

; =================
; print Command Line Invalid Error Message
; input: Command_Line_Invalid_Error_Msg var at DATASEG
; output: Screen Memory(prints the Command_Line_Invalid_Error_Msg to screen)
; =================
proc Command_Line_Invalid_Error
	 
	 call Reset_Graphic_Cursor_Position
	 
	 mov ah,09
	 mov dx,offset Command_Line_Invalid_Error_Msg
	 int 21h
	 
ret
endp Command_Line_Invalid_Error

; =================
; Save Canvas bytes To CtrlZ_Temp
; input: Screen Memory 
; output: To CtrlZ_Temp
; =================
proc Save_Canvas_To_CtrlZ_Temp
@@Temp_FileHandle equ [bp-2] ;will store [CP_Length] as it will be unreachable (ds will be altered)
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
	 mov dx, offset CtrlZ_Temp_File_Name_Var
	 int 21h
	 mov @@Temp_FileHandle,ax
	 
	 mov bx,@@Temp_FileHandle ;preps for writing to file
	 mov dx,UI_Height*Screen_Width ;calc from where to write in screen memory - the start of canvas 
	 mov ax,0a000h; sets ds to start of the screen memory - until ds is restored, DATASEG vars won't be accessible
	 mov ds,ax
	 mov cx,Canvas_Height*Screen_Width ;How many bytes to copy - all of canvas
	 mov ah,040h;write to file
	 int 21h
	 
	 mov ah,03eh ;CLOSE TEMP FILE (bx already loaded with [Temp_FileHandle]
	 int 21h
	 
@@End_Proc:	
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 pop ds
	 
	 add sp,2;freeing space allocated to local var
	 pop bp
ret
endp Save_Canvas_To_CtrlZ_Temp


; =================
; Prints CtrlZ_Temp File Not Found Error
; input: none
; output: none
; =================
proc CtrlZ_Temp_File_Not_Found_Error
	 
	 call Reset_Graphic_Cursor_Position
	 
	 mov ah,09
	 mov dx,offset CtrlZ_Error_Msg
	 int 21h
	 
ret
endp CtrlZ_Temp_File_Not_Found_Error


; =================
; Restores Canvas From CtrlZ_Temp
; input: CtrlZ_Temp 
; output: Screen Memory
; =================
proc Restore_Canvas_From_CtrlZ_Temp
@@Temp_FileHandle equ [bp-2] ;will store [CP_Length] as it will be unreachable (ds will be altered)
	 push bp
	 mov bp,sp
	 sub sp,2d ;allocates spce for local var
	 
	 push ds
	 push ax
	 push bx
	 push cx
	 push dx
	 
	 mov ah,03dh ;open CtrlZ_Temp file
	 mov al,0;read only,
	 mov dx,offset CtrlZ_Temp_File_Name_Var
	 int 21h
	 mov @@Temp_FileHandle,ax
	 jnc @@CtrlZ_Temp_File_Not_Found ;checks if an error occurred while loading the Pallette
	 call CtrlZ_Temp_File_Not_Found_Error
	 jmp @@End_Proc
@@CtrlZ_Temp_File_Not_Found:


	 call Hide_Mouse_Cursor
	 
	 mov bx,@@Temp_FileHandle ;preps for writing to file
	 mov dx,UI_Height*Screen_Width ;calc from where to write in screen memory - the start of canvas 
	 mov ax,0a000h; sets ds to start of the screen memory - until ds is restored, DATASEG vars won't be accessible
	 mov ds,ax
	 mov cx,Canvas_Height*Screen_Width ;How many bytes to Read - all of canvas
	 mov ah,03fh;Read from file
	 int 21h	 
	 
	 call Show_Mouse_Cursor
	 
	 mov ah,03eh ;CLOSE TEMP FILE (bx already loaded with [Temp_FileHandle]
	 int 21h
	 
@@End_Proc:
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 pop ds
	 
	 add sp,2;freeing space allocated to local var
	 pop bp
ret
endp Restore_Canvas_From_CtrlZ_Temp

; =================
; Reset Graphic Cursor Position to 0,0 position
; input: none 
; output: none
; =================
proc Reset_Graphic_Cursor_Position
	 push ax
	 push bx
	 push dx
	 
	 mov ah,2
	 mov bh,0
	 mov dh,0
	 mov dl,0
	 int 10h
	 
	 pop dx
	 pop bx
	 pop ax
	 
ret
endp Reset_Graphic_Cursor_Position

; =================
; Load Image Button
; Recives file name and position in canvas from user  and Loads the selected bmp file at selected position  
; input: none (requires file name to be typed and a position to be selected by user) 
; output: Screen Memory
; Special Exit Condition: Right Click
; =================
proc Load_Image_Button	 
	 
	 call Recive_Input_To_File_Name_Buffer
	 
@@Check_If_Clicked_Again:
	 call Get_Mouse_Position_and_Button_Status
	 test ax,1     ;   |F-8|7|6|5|4|3|2|1|0|  Button Status (bit fields)
	 jnz @@Load_Image;   |  | | | | | | | `---- left button (1 = pressed)
	 		       ;     |  | | | | | | `----- right button (1 = pressed)
		           ;     |  | | | | | `--- middle button  (1 = pressed)
				   ;     `------------------- unused
	 
	 test ax,2     ;if right mouse button is clicked, exit proc
	 jnz @@End_Proc
	 
	 jmp @@Check_If_Clicked_Again
@@Load_Image:
	 
	 call Hide_Mouse_Cursor
	 
	 push offset File_Name+2
	 push cx
	 push dx
	 call Load_BMP_File
	 
	 call Show_Mouse_Cursor
@@End_Proc:
ret
endp Load_Image_Button

; =================
; Open File Button
; Receives  file name from user and Loads the selected bmp file at canvas   
; input: none (requires file name to be typed by user) 
; output: Screen Memory
; =================
proc Open_File_Button
	 
	 push ax
	 
	 call Recive_Input_To_File_Name_Buffer
	 
	 push offset File_Name+2
	 mov ax,0
	 push ax
	 mov ax,UI_Height
	 push ax
	 call Load_BMP_File
	 
	 pop ax
ret
endp Open_File_Button

; =================
; Save File Button
; Recives file name from user and Saves the canvas as a bmp file with the name typed 
; BeAware! The proc won't work properly if the Screen_Width isn't divisible by 4!  
; input: Canvas at Screen Memory (requires file name to be typed by user) 
; output: File
; =================
proc Save_File
	 
	 call Recive_Input_To_File_Name_Buffer
	 
	 call Save_Image_OnScreen_To_BMP
	 
ret
endp Save_File

; =================
; Save Canvas OnScreen To BMP
; Saves the canvas as a bmp file with the name starting at (File_Name + 2)
; BeAware! The proc won't work properly if the Screen_Width isn't divisible by 4!  
; input: File_Name Array at DATASEG
; output: File
; =================
proc Save_Image_OnScreen_To_BMP
	 push ds
	 push ax
	 push bx
	 push cx
	 push dx
	 
	 mov ah,03ch ;crating the file or truncating the one with the same name if exits
	 mov dx,offset File_Name + 2
	 mov cx,0
	 int 21h
	 mov [FileHandle],ax
	 
	 mov ah,40h ;saving the Header to file
	 mov bx,[FileHandle] 
	 mov cx,54d ;header's length
	 mov dx,offset Default_Header_To_Save
	 int 21h
	 
	 mov ah,40h ;saving the Pallette to file
	 mov bx,[FileHandle] 
	 mov cx, 1024d ;Pallette's Length
	 mov dx, offset Pallette
	 int 21h
	 
	 mov bx,[FileHandle] ;saving the Image Data to file
	 mov dx,Screen_Width*(Screen_Height-1) ;setd dx to point at the first byte of the last line
	 mov ax,0a000h ;setting ds to point to screen memory, attempts to reach vars in DATASEG until it is restered to its original state are unadvised 
	 mov ds,ax
	 
	 mov cx,Canvas_Height ;So that only canvas is saved
@@Save_Another_Line:
	 push cx
	 
	 mov ah,040h
	 mov cx,Screen_Width
	 int 21h
	 
	 sub dx,Screen_Width;going a line up
	 
	 pop cx
	 loop @@Save_Another_Line
	 
	 mov ah,03eh ;close file(the File Handle is already in bx) 
	 int 21h 
	 
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 pop ds
ret
endp Save_Image_OnScreen_To_BMP

; =================
; Receive Input To File Name Buffer 
; input: none (requires the user to type a string and press enter)
; output: File_Name array at DATASEG (Length of inputed string as byte at File_Name+1 and string start at File_Name+2)
; =================
proc Recive_Input_To_File_Name_Buffer
	 
	 push ax
	 push bx
	 push dx
	 
	 call Reset_Graphic_Cursor_Position
	 
	 mov ah,0ah
	 mov dx,offset File_Name
	 int 21h
	 
	 
	 mov bx,offset File_Name
	 xor ax,ax ;zeroing ax
	 mov al,[byte ptr File_Name+1];[File_Name+1] contains how many chars have been inputed
	 add bx,ax
	 add bx,2 ; for first two char 
	 mov [byte ptr bx],0 ;aims to put 0 in the enter slot
	 
	 pop dx
	 pop bx
	 pop ax
	 
ret
endp Recive_Input_To_File_Name_Buffer


; =================
; Load UI bmp 
; Loads the tools bar, recommended after every action that may paint over it
; input: UI_File_Name_Var Var at DATASEG
; output: Screen Memory(prints the UI to screen)
; =================
proc Load_UI
	 push ax
	 push bx
	 push cx
	 push dx
	 
	 
	 mov ah,03dh ;open UI file
	 mov al,0;read only,
	 mov dx,offset UI_File_Name_Var
	 int 21h
	 mov [UI_File_Handle],ax
	 
	 mov ah,42h ;Skip header, moves pointer in Ui_file to Pallette start
	 mov al,0
	 mov bx,[UI_File_Handle]
	 mov cx,0
	 mov dx, 54d
	 int 21h
	 
	 mov ah,3fh ;Reading Pallette from UI to Pallette var in DATASEG
	 mov bx,[UI_File_Handle] 
	 mov cx,1024d
	 mov dx,offset Pallette
	 int 21h
	 jnc @@No_Pallette_Reading_Error ;checks if an error occurred while loading the Pallette
	 call Pallette_Error
@@No_Pallette_Reading_Error:

	 call Put_Pallette_Var_To_Screen_Memory ;putting Pallette to screen
	 
	 push [UI_File_Handle]
	 mov ax,0
	 push ax
	 push ax
	 call Load_Bmp_Data_To_Screen
	 
	 mov ah,03eh ;Close UI file
	 mov bx, [UI_File_Handle]
	 int 21h
	 

	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 
ret 
endp Load_UI
; =================
; Load BMP File
; input:
; File_Name var at DATASEG
; param1 = Name Buffer Pointer
; param2 = X value
; param3 = Y value
; output: Screen Memory(prints the bmp file to screen)
; =================
proc Load_BMP_File
@@Name_Buffer_Pointer equ [word ptr bp+8]
@@X equ [word ptr bp+6]
@@Y equ [word ptr bp+4]

	 push bp
	 mov bp,sp
	 
	 push ax
	 push bx
	 push cx
	 push dx
	 
	 
	 mov ah,03dh ;open file
	 mov al,0;read only
	 mov dx,@@Name_Buffer_Pointer
	 int 21h
	 mov [FileHandle],ax
	 jnc @@File_Found ;checks if an error occurred while loading the Pallette
	 call File_Not_Found_Error
	 jmp @@End_Proc
@@File_Found:
	 
	 mov ah,42h ;Skip header, moves pointer in file to Pallette start
	 mov al,0
	 mov bx,[FileHandle]
	 mov cx,0
	 mov dx, 54d
	 int 21h
	 
	 mov ah,3fh ;Reading Pallette from UI to Pallette var in DATASEG
	 mov bx,[FileHandle] 
	 mov cx,1024d
	 mov dx,offset Pallette
	 int 21h
	 jnc @@No_Pallette_Reading_Error ;checks if an error occurred while loading the Pallette
	 call Pallette_Error
	 jmp @@Close_File
@@No_Pallette_Reading_Error:

	 call Put_Pallette_Var_To_Screen_Memory ;putting Pallette to screen
	 
	 push [FileHandle]
	 push @@X
	 push @@Y
	 call Load_Bmp_Data_To_Screen
	 
@@Close_File:
	 mov ah,03eh ;Close file
	 mov bx, [FileHandle]
	 int 21h
	 
@@End_Proc:
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 
	 pop bp
	 
ret 6
endp Load_BMP_File

; =================
; print Palette Error Message
; input: Pallette_Error_Msg var at DATASEG
; output: Screen Memory(prints the Pallette_Error_Msg to screen)
; =================
proc Pallette_Error
	 
	 call Reset_Graphic_Cursor_Position
	 
	 add [Pallette_Error_Msg],al
	 mov ah,09
	 mov dx,offset Pallette_Error_Msg
	 int 21h
	 
ret
endp Pallette_Error

; =================
; print File Not Found Error Message
; input: File_Not_Found_Error_Msg var at DATASEG
; output: Screen Memory(prints the File_Not_Found_Error_Msg to screen)
; =================
proc File_Not_Found_Error
	 
	 call Reset_Graphic_Cursor_Position
	 
	 mov ah,09
	 mov dx,offset File_Not_Found_Error_Msg
	 int 21h
	 
ret
endp File_Not_Found_Error

;;Reloads the tools bar, recommended after every action that may paint over it
;proc Refresh_Tools_Bar
;@@UI_File_Handle equ [bp-2]
;	 push bp
;	 mov bp,sp
;	 sub sp,2
;	 
;	 push ds
;	 push ax
;	 push bx
;	 push cx
;	 push dx
;	   
;	 
;	 mov ah,03dh ;open UI file
;	 mov al,0;read only,
;	 mov dx,offset UI_File_Name_Var
;	 int 21h
;	 mov @@UI_File_Handle,ax
;	 
;	 mov ah,42h; changes pointer position in the file to where image data is
;	 mov al,0
;	 mov bx,@@UI_File_Handle
;	 mov cx,0
;	 mov dx,1078+(Canvas_Height*320) ;go to tool bar last line (1024+54=1078 + Canvas_Height*320)
;	 int 21h
;	 
;	 mov ax,0A000h ;setting ds to point at screen memory
;	 mov ds,ax
;	 mov dx,320*UI_Height
;	 mov bx,@@UI_File_Handle
;	  
;	 mov cx,UI_Height
;@@Load_Another_Row:
;	 push cx
;	 
;	 mov ah,03fh;read from file int
;	 mov cx,Screen_Width ;read one bmp line
;	 int 21h
;	 
;	 sub dx,320;go a line up
;	 
;	 pop cx
;	 loop  @@Load_Another_Row
;	 
	 
;	 
;	 
;	 mov ah,03eh ;Close UI file
;	 mov bx, @@UI_File_Handle
;	 int 21h
;	 
;	 
;	 
;	 pop dx
;	 pop cx
;	 pop bx
;	 pop ax
;	 pop ds
;	 
;	 add sp,2
;	 pop bp
;ret
;endp Refresh_Tools_Bar


; =================
; Put Pallette Var To Screen Memory
; Will move out to screen memory the colors from Pallette var in DATASEG
; video ports are 3C8h for number of first color
; and 3C9h for all rest
; Wrritten by Yossi Zehavi, slightly changed by me
; input: var in DATASEG: Pallette
; output: none (to cpu,changes the palette of the screen)
; =================

proc Put_Pallette_Var_To_Screen_Memory							
										
	push si
	push ax
	push cx
	push dx
	
	mov si,offset Pallette
	mov cx,256
	mov dx,3C8h
	mov al,0  ; black first							
	out dx,al ;3C8h
	inc dx	  ;3C9h
@@CopyNextColor:
	mov al,[si+2] 		; Red				
	shr al,2 			; divide by 4 Max (cos max is 63 and we have here max 255 ) (losing color resolution).				
	out dx,al 						
	mov al,[si+1] 		; Green.				
	shr al,2            
	out dx,al 							
	mov al,[si] 		; Blue.				
	shr al,2            
	out dx,al 							
	add si,4 			; Point to next color.  (4 bytes for each color BGR + null)				
								
	loop @@CopyNextColor
	
	pop dx
	pop cx
	pop ax
	pop si
	
	ret
endp Put_Pallette_Var_To_Screen_Memory

; =================
; Load Bmp Data To Screen
; BE AWARE! After the proc the pointer inside the file will point to the last byte
; BE AWARE! bmp format saves the image so the last line is the first in the file (the order of byte inside the line is untouched) and first is the last - baisically upside down.
; input: (receives through stack)
;        param1 = File Handle
;        param2 = X value, where to start loading the image
; 		 param3 = Y value, where to start loading the image
; output: File
; =================
proc Load_Bmp_Data_To_Screen
@@File_Handle equ [bp+8]
@@X equ [word ptr bp+6]
@@Y equ [word ptr bp+4]
@@Height equ [word bp-4] ;both @@Height and @@Length are double words but because their value can't be more than 320d, I treat them as words (makes it easier to do calcs)
@@Width equ [word bp-8]
@@Screen_Width_Exception equ [word ptr bp-10d]
@@Screen_Height_Exception equ [word ptr bp-12d]
	 push bp
	 mov bp,sp
	 sub sp,12d ;allocating space to loacl vars
	 
	 push ds
	 push ax
	 push bx
	 push cx
	 push dx
	 
	 
	 mov ah,42h; changes pointer position in the file to where Width is stored in the header
	 mov al,0
	 mov bx,@@File_Handle
	 mov cx,0
	 mov dx,18d ;(=12h)
	 int 21h
	 
	 mov ah,03fh ;reading width and height from the bmp's header
	 mov bx,@@File_Handle
	 mov cx,8d ;(2 double words)
	 push ss;puts ss to ds
	 pop ds
	 mov dx, bp;moves the address of @@Width to dx
	 sub dx,8;---^
	 int 21h
	 
	 mov @@Screen_Width_Exception,0; so that if there are no exceptions they'll be 0
	 mov ax,@@X ; checks if the image exceeds max X
	 add ax,@@Width
	 cmp ax,Screen_Width
	 jbe @@No_Width_Exception
	 sub ax,Screen_Width
	 mov @@Screen_Width_Exception,ax
@@No_Width_Exception:
	 
	 mov @@Screen_Height_Exception,0; so that if there are no exceptions they'll be 0
	 mov ax,@@Y ; checks if the image exceeds max Y
	 add ax,@@Height
	 cmp ax,Screen_Height 
	 jbe @@No_Height_Exception
	 sub ax,Screen_Height
	 mov @@Screen_Height_Exception,ax
@@No_Height_Exception:
	 
	 mov ax,@@Width ; see bmp way of handling file with width that isn't divisible by 4 (for short: it adds padding bytes to complete the lines into lines divisible by 4, bytes we must skip to propely load the image)
	 and ax,11b 
	 cmp ax,0
	 jz @@Padding_Issue_resolved
	 mov bx,4d ;so that the padding bytes will be skipped - (4-(@@Width%4)) is the amount of padding bytes
	 sub bx,ax
	 add @@Width,bx ;width is now divisible by 4
	 add @@Screen_Width_Exception,bx ; skips the padding bytes
@@Padding_Issue_resolved:
	 
	 mov ah,42h; changes pointer position in the file to where image data is
	 mov al,0 ; mov cx:dx from the begining of the file
	 mov bx,@@File_Handle
	 mov cx,0
	 mov dx,1078d ;go to image data after Pallette and header (1024+54=1078)
	 int 21h
	 
	 mov ax,@@Screen_Height_Exception ; skips the data that won't be read in an event of Height exception
	 mul @@Width
	 mov dx,ax
	 mov ah,42h
	 mov al,1; mov cx:dx from the current position in the file
	 int 21h
	 
	 mov ax,0A000h ;setting ds to point at screen memory
	 mov ds,ax
	 
	 mov ax,Screen_Width;calcs the position where the begining of the last line of the bmp will be loaded to
	 mul @@Y
	 mov bx,@@X
	 add bx,ax ;temporerly storing it in bx ,the point in which the first line of the image will bo loaded
	 mov ax,Screen_Width
	 mov dx,@@Height
	 sub dx,@@Screen_Height_Exception
	 mul dx
	 mov dx,ax
	 add dx,bx
	 sub dx,Screen_Width ;calcs the 
	 
	 mov bx,@@File_Handle
	 
	 mov cx,@@Height ; calcs how many lines can be loaded
	 sub cx, @@Screen_Height_Exception
	 
	 cmp @@Screen_Width_Exception,0
	 jnz @@Load_BMP_With_Exception
	 
;NO WIDTH EXCEPTION:
@@Load_Another_Row:
	 push cx
	 
	 mov ah,03fh;read from file int
	 mov cx,@@Width ;read one bmp line
	 int 21h
	 
	 sub dx,Screen_Width;go a line up
	 
	 pop cx
	 loop  @@Load_Another_Row
	 
	 jmp @@End_Proc
	 
;WITH WIDTH EXCEPTION:
@@Load_BMP_With_Exception:
	 mov di, @@Width
	 sub di,@@Screen_Width_Exception
@@Load_Another_Row_With_Exception_Handling:
	 push cx
	 
	 
	 mov ah,03fh;read from file int
	 mov cx,di ;read one bmp line
	 int 21h
	 
	 sub dx,Screen_Width;go a line up
	 push dx; saves it so it can be restored after LSEEK
	 
	 mov ah,42h;LSEEK - Move File Pointer Using Handle
	 mov al,1;move to current location plus offset
	 xor cx,cx
	 mov dx,@@Screen_Width_Exception
	 int 21h
	 
	 pop dx; restored dx
	 
	 pop cx
	 loop  @@Load_Another_Row_With_Exception_Handling
	 
	 
	 
@@End_Proc:
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 pop ds
	 
	 add sp,12d
	 pop bp
ret 6
endp Load_Bmp_Data_To_Screen

; =================
; Copy A Full Line From Screen To Screen Line Var
; input: line's Y value at ax
; output: Screen_Line var at DATASEG
; =================
proc Copy_A_Full_Line_From_Screen_To_Screen_Line_Var
	 ; movsb (DS:SI-->ES:DI)
	 push ds 
	 push es
	 push ax
	 push cx
	 push dx
	 push si
	 push di
	 
	 mov si,Screen_Width;calculating the line's starting byte at the screen memory
	 mul si
	 mov si,ax
	 
	 push ds ;movsb reads to es and our var is in ds
	 pop es
	 mov di,offset Screen_Line

	 mov ax,0A000h ;screen memory starts at 0A0000h, since es serves as segment we divide it by 16 (=0A000h)
	 mov ds,ax
	 
	 mov cx,Screen_Width
@@Copy_Another_Byte:
	 movsb
	 loop @@Copy_Another_Byte
	
	 pop di
	 pop si
	 pop dx
	 pop cx
	 pop ax
	 pop es
	 pop ds 
ret
endp Copy_A_Full_Line_From_Screen_To_Screen_Line_Var


; =================
; Read Bmp Header
; Read bmp Header to Header var 
; Read 54 bytes the Header
; input: FileHandle var at DATASEG
; output: Header var at DATASEG
; =================
proc ReadBmpHeader	near
	push ax
	push bx
	push cx
	push dx
	
	mov ah,3fh
	mov bx, [FileHandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp ReadBmpHeader

; =================
; Turn Up Side Down
; Turns Canvas Up Side Down
; input: Screen Memory
; output: Screen Memory
; =================
proc Turn_Up_Side_Down
	 push es
	 push di
	 push si
	 push ax
	 push bx
	 
	 mov di, Screen_Width*UI_Height  ;move to si the start of the first line (simply put 0 instead if you want to turn ala of the screen)
	 mov si, Screen_Width*(Screen_Height-1) ;move to si the start of the last line
	 mov ax,0A000h ;setting ds to point at screen memory
	 mov es,ax
	 
	 
	 
	 mov cx,(200-UI_Height)/2; height is 200 lines and we switch 2 each time
@@Switch_Another_2_Lines:
	 push cx
	 
	 
	 mov cx,Screen_Width/2 ;(320 byte = (Screen_Width/2) words)
@@Switch_Next_2_Words:
	 mov ax,[word ptr es:si]
	 mov bx,[word ptr es:di]
	 mov [word ptr es:si],bx
	 mov [word ptr es:di],ax
	 add di,2
	 add si,2
	 loop @@Switch_Next_2_Words
	 
	 sub si,Screen_Width*2; subbing the line we just copied + putting to in the start of the on above it
	 
	 
	 pop cx
	 loop @@Switch_Another_2_Lines
	 
	 
	 pop bx
	 pop ax
	 pop si 
	 pop di
	 pop es
ret
endp Turn_Up_Side_Down

; =================
; Turn Left To Right
; Turns Canvas Left To Right
; input: Screen Memory
; output: Screen Memory
; =================
proc Turn_Left_To_Right
	 push es
	 push di
	 push si
	 push ax
	 
	 mov di, Screen_Width*UI_Height ;start of the first line after UI (simply put 0 instead if you want to turn all of the screen)
	 mov si,(Screen_Width-1)+Screen_Width*UI_Height ;end of the first line after UI(simply prem remove +Screen_Width*UI_Height instead if you want to turn all of the screen)
	 mov ax,0A000h ;setting es to point at screen memory
	 mov es,ax
	 
	 mov cx, Canvas_Height
@@Switch_Another_Line:
	 push cx
	 
	 mov cx, Screen_Width/2 ; /2 because aproching each other
@@Switch_Next_2_Bytes:
	 mov al,[byte ptr es:si]
	 mov ah,[byte ptr es:di]
	 mov [byte ptr es:si],ah
	 mov [byte ptr es:di],al
	 inc di
	 dec si
	 loop @@Switch_Next_2_Bytes
	 add di,Screen_Width-(Screen_Width/2) ;go a line down and go back to the start of the (next) line
	 add si,Screen_Width+(Screen_Width/2) ;go a line down and go back to the end of the (next) line
	 
	 pop cx
	 loop @@Switch_Another_Line
	 
	 
	 pop ax
	 pop si 
	 pop di
	 pop es
ret
endp Turn_Left_To_Right
