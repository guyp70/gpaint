.486
IDEAL
MODEL small
STACK 0f500h


DATASEG
;General EQUs
	Screen_Width equ 320d
	Screen_Height equ 200d
	UI_Height equ 60d
	Canvas_Height equ (Screen_Height-UI_Height)
;SaveLoad EQUs
	UI_File_Name equ "UI.bmp" ;"Witcher3.bmp"
	Max_Chars_That_Can_Be_Inputed equ 12 ;in the save file or open file procs; including ".bmp", the max amount of cahrs the dos box file handler can recievs
;Shapes EQUs
	;Guide for the shape (inputed to shp_Type var in DATASEG)
	Circle equ 0
	Diagonal_Line equ 1
	Rect equ 2
	Triangle_90_Deg equ 3
	Isosceles_Triangle equ 4
	Rhombus equ 5
;Tools EQUs
	CP_Temp_File_Name equ "CP_TEMP"
	CtrlZ_Temp_File_Name equ "CZ_TEMP"
	White equ 0ffh
	Min_Allowed_Value_Of_SP equ 50
	
;General Vars
	Color db 00 ; dictates the current color.
	CommandLine db 128 dup(0) ; buffer used receive the input from PSP in "Read_Command_Line_From_PSP" proc (from the command line entered after "PaintTop.exe ") 
	CommandLine_Length db 0 ; after "Read_Command_Line_From_PSP" proc activation will contain the number of chars put in CommandLine
;SaveLoad Vars
	Command_Line_Invalid_Error_Msg db "Error, Command Line Invalid (check if the number of letters exceeded 12)$"
	Screen_Line db Screen_Width dup (2)
	File_Name db Max_Chars_That_Can_Be_Inputed+1,014d dup (0);12 = max chars that can be inputed, 13 because 8 chars (max chars dosbox file handler can deal with)+".bmp"+a byte for the 'CR' you get from enter
	Header db 54d dup (0) ;buffer to store the loaded bmp's header.
	Pallette db 1024d dup (0) ; buffer to store the loaded bmp's palette.
	FileHandle dw ?
	UI_File_Name_Var db UI_File_Name,0 ;stores the name of the ui bmp file in asciiz
	UI_File_Handle dw ?
	Pallette_Error_Msg db '0'," An error occurred while loading the Pallette$"
	File_Not_Found_Error_Msg db "Error, File Not Found$"
	Default_Header_To_Save db 'B','M';                          ;beging of file header 
					   dd 1024+54+(Screen_Width*Canvas_Height) ;file size
					   dw 0,0
					   dd 1024d+54d ;when does the image data in the file begin (after the header and the Pallette)
                       dd 40d ;Length of image header        ;beging of image header 
					   dd Screen_Width
					   dd Canvas_Height
					   dw 1
					   dw 8 ;number of bits per pixel
					   dd 0,Canvas_Height*Screen_Width,0,0,0,0

;Shapes Vars
	shp_Type db 0
	shp_Height dw 0
	shp_Length dw 0
	shp_PressX dw 0
	shp_PressY dw 0
	shp_ReleaseX dw 0
	shp_ReleaseY dw 0
	Right_Click db 0
;Bresenhm Vars (Subclass in Shapes)
	TempW dw 1
    pointX dw ? 
	pointY dw ?
    point1X dw ? 
	point1Y dw ?
    point2X dw ?
	point2Y dw ?

;Tools Vars
	Thickness dw 1 ;dictates the thickness of the tools (except for shapes).
	;Print Text var
	Text db 0FFh,100h dup (0) ; used as buffer for int 09 of the 21h family in Text tool 
	;Copy Paste Vars
	CP_Temp_File_Name_Var db CP_Temp_File_Name,0
	CP_Length dw 0
	CP_Height dw 0
	CP_X dw 0
	CP_Y dw 0
	CP_Error_Msg db "Oops, There is nothing to paste!$"
	;CtrlZ
	CtrlZ_Temp_File_Name_Var db CtrlZ_Temp_File_Name,0
	CtrlZ_Error_Msg db "Oops, No Data Found!$"
CODESEG
    ORG 100h
start:
	 mov ax, @data
	 mov ds,ax
	    
	 call SetGrapicMode
	 call Set_Mouse_Routine
	 call Load_UI;refreshes UI
	 call Update_Color_Rect_In_UI
	 call Refresh_Canvas
	 
	 ;checks if Command Line is empty
	 call Open_File_By_CommandLine

	 
	 
	 call Show_Mouse_Cursor
Main_Loop:
	 xor ah,ah ;check key
	 int 16h
	 cmp ah,1;1 is 'esc' scancode
	 je End_Program

	 jmp Main_Loop
	 
End_Program:
	 call SetNormalMode
	 call Delete_Temps_If_Exits
EXIT:
    
	mov ax, 4C00h ; returns control to dos
  	int 21h
  
  
;---------------------------
; Procudures area
;---------------------------

; =================
; Sets the routine the program will follow if the left button is released 
; input: none
; output: none
; =================
proc Set_Mouse_Routine

	 mov ax, seg Mouse_Handler 
     mov es, ax
     mov dx, offset Mouse_Handler   ; ES:DX ->Far routine
     mov ax,0Ch             ; interrupt number
     mov cx,4d            ; 8d = Right Released, 2d =Left button pressed (http://www.ousob.com/ng/progref/ng3b053.php)
     int 33h  
ret
endp Set_Mouse_Routine

; =================
; Checks if the Button was pressed -  Restricts Mouse to canvas &  Saves canvas to CtrlZ_Temp_File before proc activation
; input: X value at cx and Y value at dx
; output: none
; =================
Macro Check_Button_Regular_CtrlZ_Embedded Button_X, Button_Y, Button_Length, Button_Height, Button_Proc
	local Not_Button
	 cmp cx, Button_X ; check x
	 jb Not_Button
	 cmp cx, Button_X+Button_Length
	 ja Not_Button
	 cmp dx, Button_Y ; check y
	 jb Not_Button
	 cmp dx, Button_Y+Button_Height
	 ja Not_Button
	 
	 ;If Button was pressed
	 call Hide_Mouse_Cursor ; Save for ctrlz restore
	 call Save_Canvas_To_CtrlZ_Temp
	 call Show_Mouse_Cursor
	 
	 call Restrict_Mouse_to_Canvas ; call proc
	 call Button_Proc
	 call Release_Mouse
	 jmp End_Button_Check
	 
	 Not_Button:
ENDM Check_Button_Regular_CtrlZ_Embedded

; =================
; Checks if the Button was pressed -  doesn't Restrict Mouse to canvas &  Saves canvas to CtrlZ_Temp_File before proc activation
; input: X value at cx and Y value at dx
; output: none
; =================
Macro Check_Button_No_Restrict_Mouse_CtrlZ_Embedded Button_X, Button_Y, Button_Length, Button_Height, Button_Proc
	local Not_Button
	 cmp cx, Button_X ; check x
	 jb Not_Button
	 cmp cx, Button_X+Button_Length
	 ja Not_Button
	 cmp dx, Button_Y ; check y
	 jb Not_Button
	 cmp dx, Button_Y+Button_Height
	 ja Not_Button
	 
	 ;If Button was pressed
	 call Hide_Mouse_Cursor
	 call Save_Canvas_To_CtrlZ_Temp
	 call Button_Proc
	 call Show_Mouse_Cursor
	 jmp End_Button_Check
	 Not_Button:
ENDM Check_Button_No_Restrict_Mouse_CtrlZ_Embedded

; =================
; Checks if the Button was pressed -  Restricts Mouse to canvas & doesn't save canvas to CtrlZ_Temp_File before proc activation
; input: X value at cx and Y value at dx
; output: none
; =================
Macro Check_Button_Regular_No_CtrlZ Button_X, Button_Y, Button_Length, Button_Height, Button_Proc
	local Not_Button
	 cmp cx, Button_X ; check x
	 jb Not_Button
	 cmp cx, Button_X+Button_Length
	 ja Not_Button
	 cmp dx, Button_Y ; check y
	 jb Not_Button
	 cmp dx, Button_Y+Button_Height
	 ja Not_Button
	 call Restrict_Mouse_to_Canvas
	 call Button_Proc	 ;If Button was pressed
	 call Release_Mouse
	 jmp End_Button_Check
	 Not_Button:
ENDM Check_Button_Regular_No_CtrlZ

; =================
; Checks if the Button was pressed - doesn't Restrict Mouse to canvas & doesn't save canvas to CtrlZ_Temp_File before proc activation
; input: X value at cx and Y value at dx
; output: none
; =================
Macro Check_Button_No_Restrict_Mouse_No_CtrlZ Button_X, Button_Y, Button_Length, Button_Height, Button_Proc
	local Not_Button
	 cmp cx, Button_X ; check x
	 jb Not_Button
	 cmp cx, Button_X+Button_Length
	 ja Not_Button
	 cmp dx, Button_Y ; check y
	 jb Not_Button
	 cmp dx, Button_Y+Button_Height
	 ja Not_Button
	 ;If Button was pressed
	 call Hide_Mouse_Cursor
	 call Button_Proc
	 call Show_Mouse_Cursor
	 jmp End_Button_Check
	 Not_Button:
ENDM Check_Button_No_Restrict_Mouse_No_CtrlZ

; =================
; Handles mouse clicks, checks if and what button was pressed 
; input: none
; output: none
; =================
proc Mouse_Handler far

	 push ax
	 push cx
	 push dx

	 call Hide_Mouse_Cursor ;refresh UI
	 call Load_UI
	 call Update_Color_Rect_In_UI
	 call Show_Mouse_Cursor
	 
	 call Get_Last_Left_Press_Coordinates 
	 
	 cmp dx,UI_Height ; so that it will skip all the checkes if the button was pressed outside of the UI
	 ja End_Button_Check
	 
	 _X = 5d ;Save_Button
	 _Y = 15d
	 _Length = 11d
	 _Height = 12d
	 Check_Button_No_Restrict_Mouse_No_CtrlZ _X, _Y, _Length, _Height, Save_File
	 
	 _X = 4d ;Open_File
	 _Y = 32d
	 _Length = 14d
	 _Height = 13d
	 Check_Button_No_Restrict_Mouse_CtrlZ_Embedded _X, _Y, _Length, _Height, Open_File_Button
	 
	 
	 _X = 22d ;Load_Image
	 _Y = 34d
	 _Length = 10d
	 _Height = 10d
	 Check_Button_Regular_CtrlZ_Embedded _X, _Y, _Length, _Height, Load_Image_Button
	 
	 _X = 22d ;Refresh_Canvas
	 _Y = 15d
	 _Length = 10d
	 _Height = 12d
	 Check_Button_No_Restrict_Mouse_CtrlZ_Embedded _X, _Y, _Length, _Height, Refresh_Canvas
	 
	 _X = 201d ;Text
	 _Y = 18d
	 _Length = 10d
	 _Height = 10d
	 Check_Button_Regular_CtrlZ_Embedded _X, _Y, _Length, _Height, Print_Text_Button
	 
	 _X = 212d ;Fill
	 _Y = 18d
	 _Length = 10d
	 _Height = 11d
	 Check_Button_Regular_CtrlZ_Embedded _X, _Y, _Length, _Height, Fill_Button
	 
	 _X = 224d ;Pencil
	 _Y = 18d
	 _Length = 10d
	 _Height = 10d
	 Check_Button_Regular_CtrlZ_Embedded _X, _Y, _Length, _Height, Pencil_Button
	 
	 _X = 207d ;Eraser
	 _Y = 33d
	 _Length = 10d
	 _Height = 9d
	 Check_Button_Regular_CtrlZ_Embedded _X, _Y, _Length, _Height, Print_Eraser_Button
	 
	 _X = 218d ;Sampler
	 _Y = 33d
	 _Length = 11d
	 _Height = 9d
	 Check_Button_Regular_No_CtrlZ _X, _Y, _Length, _Height, Sampler_Button
	 
	 _X = 241d ;Turn_Up_Side_Down
	 _Y = 14d
	 _Length = 10d
	 _Height = 18d
	 Check_Button_No_Restrict_Mouse_CtrlZ_Embedded _X, _Y, _Length, _Height, Turn_Up_Side_Down
	 
	 _X = 252d ;Turn_Left_To_Right
	 _Y = 18d
	 _Length = 18d
	 _Height = 10d
	 Check_Button_No_Restrict_Mouse_CtrlZ_Embedded _X, _Y, _Length, _Height, Turn_Left_To_Right
	 
	 _X = 278d ;Clone Stamp
	 _Y = 15d
	 _Length = 17d
	 _Height = 31d
	 Check_Button_Regular_CtrlZ_Embedded _X, _Y, _Length, _Height, Clone_Stamp_Button
	 
	 _X = 301d ;Copy
	 _Y = 15d
	 _Length = 11d
	 _Height = 18d
	 Check_Button_Regular_No_CtrlZ _X, _Y, _Length, _Height, Copy_Button
	 
	 _X = 301d ;Paste
	 _Y = 36d
	 _Length = 11d
	 _Height = 16d
	 Check_Button_Regular_CtrlZ_Embedded _X, _Y, _Length, _Height, Paste_Button
	 
	 ;checks if Pallette was pressed
	 Pallette_X equ 37d
	 Pallette_Y equ 13d
	 Pallette_Length equ 76d
	 Pallette_Height equ 33d
	 cmp cx, Pallette_X ; check x
	 jb @@Not_Pallette
	 cmp cx, Pallette_X+Pallette_Length
	 ja @@Not_Pallette
	 cmp dx, Pallette_Y ; check y
	 jb @@Not_Pallette
	 cmp dx, Pallette_Y+Pallette_Height
	 ja @@Not_Pallette
	 call Sampler
	 call Update_Color_Rect_In_UI
	 jmp End_Button_Check
	 @@Not_Pallette:
	 
	 
	 _X = 146d ;Change_Thickness
	 _Y = 19d
	 _Length = 17d
	 _Height = 23d
	 Check_Button_No_Restrict_Mouse_No_CtrlZ _X, _Y, _Length, _Height, Change_Thickness


	 ;Prints a Shape
	 Print_Shape_X equ 172d
	 Print_Shape_Y equ 15d
	 Print_Shape_Length equ 22d
	 Print_Shape_Height equ 30d
	 cmp cx, Print_Shape_X ; check x
	 jb @@Not_Print_Shape
	 cmp cx, Print_Shape_X+Print_Shape_Length
	 ja @@Not_Print_Shape
	 cmp dx, Print_Shape_Y ; check y
	 jb @@Not_Print_Shape
	 cmp dx, Print_Shape_Y+Print_Shape_Height
	 ja @@Not_Print_Shape
	 
	 call Hide_Mouse_Cursor ; Save for ctrlz restore
	 call Save_Canvas_To_CtrlZ_Temp
	 call Show_Mouse_Cursor
	 
	 call Find_What_Shape_Was_Clicked
	 call Restrict_Mouse_to_Canvas
	 call Print_Shape_Button_Rev
	 call Release_Mouse
	 jmp End_Button_Check
	 @@Not_Print_Shape:
	 
	 _X = 307d ;Restore_Canvas_From_CtrlZ_Temp
	 _Y = 0d
	 _Length = 13d
	 _Height = 10d
	 Check_Button_No_Restrict_Mouse_No_CtrlZ _X, _Y, _Length, _Height, Restore_Canvas_From_CtrlZ_Temp
	 
End_Button_Check:
@@End_Proc:
	 
	 pop dx
	 pop cx
	 pop ax
	 
retf
endp Mouse_Handler

; =================
; Checks if any of the change Thickness buttons were pressed and changes [Thickness] accordingly
; input: recievs Y value at dx
; output: [Thickness]
; =================
proc Change_Thickness
	 
	 cmp dx,Print_Shape_Y+5
	 jae @@Check_3
	 mov [Thickness],1d
	 jmp @@End_Proc
@@Check_3:
	 cmp dx,Print_Shape_Y+10d
	 jae @@Check_5
	 cmp dx,Print_Shape_Y+7d
	 jbe @@End_Proc
	 mov [Thickness],3d
	 jmp @@End_Proc
@@Check_5:
	 cmp dx,Print_Shape_Y+17d
	 jae @@Check_7
	 cmp dx,Print_Shape_Y+11d
	 jbe @@End_Proc
	 mov [Thickness],5d
	 jmp @@End_Proc
@@Check_7:
	 cmp dx,Print_Shape_Y+27d
	 jae @@Check_5
	 cmp dx,Print_Shape_Y+19d
	 jbe @@End_Proc
	 mov [Thickness],7d	 
@@End_Proc:
ret
endp Change_Thickness

; =================
; Checks what Shape button was pressed and calls the proper proc
; input: X value ac cx and Y value at dx
; output: none (indirectly: shp_Type)
; =================
proc Find_What_Shape_Was_Clicked
	 
	 cmp cx,Print_Shape_X+((Print_Shape_Length-2)/2)
	 ja @@Second_Column
	 ;First_Column:
	 cmp dx,Print_Shape_Y+(Print_Shape_Height/3)
	 ja @@Check_Rect
	 call Set_Shape_To_Circle
	 jmp @@End_Proc
@@Check_Rect:
	 cmp dx,Print_Shape_Y+2*(Print_Shape_Height/3)
	 ja @@Isosceles_Triangle
	 call Set_Shape_To_Rect
	 jmp @@End_Proc
@@Isosceles_Triangle:
	 call Set_Shape_To_Isosceles_Triangle
	 jmp @@End_Proc
	 
@@Second_Column:
	 cmp dx,Print_Shape_Y+(Print_Shape_Height/3)
	 ja @@Check_Triangle_90_Deg
	 call Set_Shape_To_Diagonal_Line
	 jmp @@End_Proc
@@Check_Triangle_90_Deg:
	 cmp dx,Print_Shape_Y+2*(Print_Shape_Height/3)
	 ja @@Rhombus
	 call Set_Shape_To_Triangle_90_Deg
	 jmp @@End_Proc
@@Rhombus:
	 call Set_Shape_To_Rhombus
	 jmp @@End_Proc

@@End_Proc:
ret
endp Find_What_Shape_Was_Clicked

 include "GenProcs.asm"
 include "SaveLoad.asm"
 include "Shapes.asm"
 include "Tools.asm"
 

END start