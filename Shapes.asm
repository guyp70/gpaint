;(guide to shapes in equs Section)
; =================
; Print Shape Button (Rev)
; Lets the user select a rectangular space and draws a shape in it 
; Tip: Use the Set... proc to change the shape.
; input: var in DATASEG: [shp_Type]
; output: Screen Memory
; =================
proc Print_Shape_Button_Rev
	 
	 push ax
	 push bx
	 push cx
	 push dx
	 
@@Loop_Until_Right_Click: ;Exit if the user press the right mouse button
	 
	 mov ax, 6 ;Get Left Button Release Info 
	 mov bx,0
	 int 33h
	 
	 test ax,2     ;if right mouse button is clicked, exit proc 
	 jnz @@End_Proc ;   |F-8|7|6|5|4|3|2|1|0|  Button Status (bit fields)
				    ;     |  | | | | | | | `---- left button (1 = pressed)
	 		        ;     |  | | | | | | `----- right button (1 = pressed)
		            ;     |  | | | | | `--- middle button  (1 = pressed)
				    ;     `------------------- unused
	 
	 cmp bx,0     ;BX = Count of Button Releases
	 jz @@Loop_Until_Right_Click
	 
	 call Get_Last_Left_Press_Coordinates ;<-- checks that the shape will be drawn fully in the canvas and prevents
	 cmp dx,UI_Height					  ;<-- a shpe from being drawn if the mouse was kept pressed after clicking the
	 jb @@Loop_Until_Right_Click		  ;<-- shape button and then only released in canvas (so the the ui was printed over)
	 
	 call Print_Shape
	 
	 jmp @@Loop_Until_Right_Click
	 
	 
@@End_Proc:
	 
	 pop dx
	 pop cx
	 pop bx
	 pop ax

	 ret 
endp Print_Shape_Button_Rev

;------------------------------------------------------------------

;A set of functions meant to be used by users by unfamiliar with the code, bond it with Print_Shape and you are set to go

proc Set_Shape_To_Circle
	 mov [shp_Type],circle
ret
endp Set_Shape_To_Circle


proc Set_Shape_To_Diagonal_Line
	 mov [shp_Type],Diagonal_Line
ret
endp Set_Shape_To_Diagonal_Line


proc Set_Shape_To_Rect
	 mov [shp_Type],Rect
ret
endp Set_Shape_To_Rect


proc Set_Shape_To_Triangle_90_Deg
	 mov [shp_Type],Triangle_90_Deg
ret
endp Set_Shape_To_Triangle_90_Deg


proc Set_Shape_To_Isosceles_Triangle
	 mov [shp_Type],Isosceles_Triangle
ret
endp Set_Shape_To_Isosceles_Triangle


proc Set_Shape_To_Rhombus
	 mov [shp_Type],Rhombus
ret
endp Set_Shape_To_Rhombus

;-------------------------------------------------------------------------------

; =================
; Print Shape
; takes the points of last release and press (of the left mouse button) and prints in the rectangular space between them the shape.
; Tip: Use the Set... proc to change the shape.
; input: var in DATASEG: [shp_Type], Last Left Press info and Last Left Release info
; output: Screen Memory
; =================
proc Print_Shape 
	 push ax
	 push cx
	 push dx
	 
	 
	 call Get_Last_Left_Press_Coordinates
 	 mov [shp_PressX],cx
	 mov [shp_PressY],dx

	 call Get_Last_Left_Release_Coordinates
	 mov [shp_ReleaseX],cx
	 mov [shp_ReleaseY],dx
	 
	 call Calc_Height_And_Length_Then_Print
	 
@@End_Proc:
	 
	 pop dx
	 pop cx
	 pop ax
	 ret
endp Print_Shape

; =================
; Calc Height_And Length Then Print
; calculates shp_Height and shp_Length based on Press and release coordinates and then chooses what shape to print based on shp_Type value
; input: vars in DATASEG: shp_Type, shp_ReleaseX, shp_ReleaseY, shp_PressX, shp_PressY
; output: Screen Memory
; =================
proc Calc_Height_And_Length_Then_Print
	 
	 call Calc_Height_And_Length
	 
	 cmp [shp_Height],0 ;Protection (I repeat checking if these values are zero a lot of times because 0 in one of them would yield hard to forsee and diagnose problems)
	 je @@End_Proc
	 cmp [shp_Length],0
	 je @@End_Proc
	 
	 
	 call Hide_Mouse_Cursor; so it wont intefere eith printing
	 
	 cmp [shp_Type],circle
	 je @@Prn_Circle
	 cmp [shp_Type],Diagonal_Line
	 je @@Prn_Diagonal_Line
	 cmp [shp_Type],Rect
	 je @@Prn_Rect
	 cmp [shp_Type],Triangle_90_Deg
	 je @@Prn_Triangle_90_Deg
	 cmp [shp_Type],Isosceles_Triangle
	 je @@Prn_Isosceles_Triangle
	 cmp [shp_Type],Rhombus
	 je @@Prn_Rhombus
	 jmp @@End_Proc
	 
	 @@Prn_Circle:
	 call Print_Circle
	 jmp @@End_Proc
@@Prn_Diagonal_Line:
	 call Print_Diagonal_Line
	 jmp @@End_Proc
@@Prn_Rect:
	 call Print_Rect
	 jmp @@End_Proc
@@Prn_Triangle_90_Deg:
	 call Print_Triangle_90_Deg 
	 jmp @@End_Proc
@@Prn_Isosceles_Triangle:
	 call Print_Isosceles_Triangle
	 jmp @@End_Proc
@@Prn_Rhombus:
	 call Print_Rhombus 
@@End_Proc: 
	 
	 call Show_Mouse_Cursor
	 
ret
endp Calc_Height_And_Length_Then_Print

; =================
; Calc Height_And Length 
; calculates shp_Height and shp_Length based on Press and release coordinates
; input: vars in DATASEG: shp_ReleaseX, shp_ReleaseY, shp_PressX, shp_PressY
; output: Screen Memory
; =================
proc Calc_Height_And_Length
	 push cx
	 push dx
	 
	 mov cx, [shp_ReleaseX]
	 mov dx, [shp_ReleaseY]
	 
	 sub cx,[shp_PressX]
	 sub dx,[shp_PressY]
	 
	 mov [shp_Length],cx
	 mov [shp_Height],dx
	 
	 pop dx
	 pop cx
	 ret
endp Calc_Height_And_Length





;-----------------------------------------------
;Checks if there is minus in the params for the shapes and adjusts them


;cmp @@Height, 0
;jae @@Check_X_And_Length
;mov ax,@@Height
;call Return_AX_Absolute
;sub @@Y,ax
;mov @@height,ax

;@@Check_X_And_Length:

;cmp @@Length, 0
;jae @@End_Proc
;mov ax,@@Length
;call Return_AX_Absolute
;sub @@X,ax
;mov @@Length,ax
;@@End_Check:
;pop ax




;-----------------------------------------------

; =================
; Draw Rect
; Draws rect, capable of handling negative length and height params.
; input: (through stack)
;	 	param1 = height
;		param2 = Length
;		param3 = X value 
;		param4 = Y value of the uppermost point
; output: Screen Memory
; =================
proc Draw_Rect

@@Y equ [word ptr word ptr bp+4]
@@X equ [word ptr word ptr bp+6]
@@Length equ [word ptr word ptr bp+8]
@@Height equ [word ptr bp+10]

	 push bp
	 mov bp,sp

	 push ax
	 push bx
	 push cx
	 push dx
	 push si



@@Check_Params_And_Fix_If_Necessary:
	 cmp @@Height, 0
	 jge @@Check_X_And_Length
	 mov ax,@@Height
	 call Return_AX_Absolute
	 sub @@Y,ax
	 mov @@height,ax

@@Check_X_And_Length:
	 cmp @@Length, 0
	 jge @@End_Check
	 mov ax,@@Length
	 call Return_AX_Absolute
	 sub @@X,ax
	 mov @@Length,ax
@@End_Check:
 
 
 
	 mov al,[Color]
	 mov bx,@@Height
	 mov si,@@Length
	 mov cx,@@X
	 mov dx,@@Y


@@Check_If_To_Print_Another_Line:
	 cmp bx,0
	 jz @@EndPrintingRectangle

@@Print_Anothr_Line:
	 call Draw_Horizontal_Line
	 inc dx
	 dec bx
     jmp @@Check_If_To_Print_Another_Line

@@EndPrintingRectangle:

	 pop si
	 pop dx
	 pop cx
	 pop bx
	 pop ax

	 pop bp
	 ret 8
endp Draw_Rect
 
 
 
 ; =================
; Print Rect (Case)
; Draws rect, capable of handling negative length and height params.
; input: vars in DATASEG: shp_Height,shp_Length,shp_PressX,shp_PressY
; output: Screen Memory
; =================
 proc Print_Rect
	 
	 
	 push [shp_Height]
	 push [shp_Length]
	 push [shp_PressX]
	 push [shp_PressY]
 
	 call Draw_Rect
 
 ret
 endp Print_Rect
 

; =================
; Print Isosceles Triangle (Case)
; Draws an isosceles triangle, capable of drawing to any direction (if shp_Height is neg it will be printed downwards).
; input: vars in DATASEG: shp_Height,shp_Length,shp_PressX,shp_PressY
; output: Screen Memory
; =================
proc Print_Isosceles_Triangle
	 push ax
	 
	 mov ax,[shp_Height]
	 push ax
	 mov ax,[shp_Length]
	 push ax
	 mov ax,[shp_PressX]
	 push ax
	 mov ax,[shp_PressY]
	 push ax
	 call Draw_Isosceles_Triangle_Rev
 
@@End_Proc:
	 pop ax
	 ret
 endp Print_Isosceles_Triangle

; =================
; Draw Isosceles Triangle (Rev)
; DESCRIPTION: draws an isosceles triangle upwards with base length eqhivelent to double the height
; input: (through stack)
;		Param1 = Height 
;		Param2 = Base Length 
;		Param3 = X value 
;		Param4 = Y value of the lowest point
; output: Screen Memory
; =================
proc Draw_Isosceles_Triangle_Rev

@@Height equ [word ptr bp+10]
@@Length equ [word ptr bp+8]
@@X equ [word ptr bp+6]
@@Y equ [word ptr bp+4]
@@Center_Point_X equ [word ptr bp-2]
@@Center_Point_Y equ [word ptr bp-4]


	 push bp
	 mov bp,sp
	 sub sp,4d
	 
	 push ax
	 push cx
	 push dx
	 push si

	 cmp @@Length,0
	 jz @@EndPrintingTriangle
	 jg @@Pos_Length
	 
	 mov ax,@@X
	 add ax,@@Length
	 mov @@X,ax
	 
	 mov ax,@@Length
	 call Return_AX_Absolute
	 mov @@Length,ax
	 @@Pos_Length:
	 
	 mov si,@@Length ;draws the base
	 mov al,[Color]
	 mov cx,@@X
	 mov dx,@@Y
	 call Draw_Horizontal_Line
	 
	 mov ax,@@Length ;calcing Center point coordinates
	 shr ax,1
	 add ax,@@X
	 mov @@Center_Point_X,ax
	 mov ax,@@Y
	 add ax,@@Height
	 mov @@Center_Point_Y,ax
	 
	 mov ax,@@Center_Point_X ;drawing second line  
	 mov [point1X],ax
	  mov ax,@@Center_Point_Y
	 mov [point1Y],ax
	 
	 mov ax,@@X 
	 mov [point2X],ax
	 mov ax, @@Y
	 mov [point2Y],ax
	 
	 call DrawLine2D
	 
	 mov ax,@@Center_Point_X ;drawing third line  
	 mov [point1X],ax
	  mov ax,@@Center_Point_Y
	 mov [point1Y],ax
	 
	 mov ax,@@X 
	 add ax, @@Length
	 mov [point2X],ax
	 mov ax, @@Y
	 mov [point2Y],ax
	 
	 call DrawLine2D

@@EndPrintingTriangle:
	 pop si
	 pop dx
	 pop cx
	 pop ax

	 add sp,4d
	 pop bp
	 ret 8
endp Draw_Isosceles_Triangle_Rev
 
 
 
 
 

; =================
; Draw Horizontal Line
; input: 
;		si = length
;		al = color
;		cx = X value 
;		dx = Y value 
; output: Screen Memory
; =================
proc Draw_Horizontal_Line
push ax 
push bx
push cx
push dx
push si
push es

push ax ;to save it for later

mov ax,0A000h
mov es,ax

mov ax,Screen_Width
mul dx
add ax,cx
mov bx,ax
;consequences: dx =0 
pop cx ;putting what was ax to cx

@@Check_If_To_Print_Another_Dot:
cmp si,0 ;to exit if the length is 0
jz @@End_Printing_HL

@@Draw_Another_Dot:
mov [byte ptr Es:bx],cl 
inc bx

dec si
jmp @@Check_If_To_Print_Another_Dot

@@End_Printing_HL:
Pop es
pop si
pop dx 
pop cx
pop bx
pop ax
ret
endp Draw_Horizontal_Line


; =================
; Draw Horizontal Line (Using INT10h)
; Tip: use Draw_Horizontal_Line instead, its more efficient
; input: 
;		si = length
;		al = color
;		cx = X value 
;		dx = Y value 
; output: Screen Memory
; =================
proc Draw_Horizontal_Line_Using_INT10h
push ax 
push bx
push cx
push dx
push si

mov ah,0ch

Check_If_To_Print_Another_Pixel:
cmp si,0
ja Print_Anothr_Pixel
jz EndPrinting

Print_Anothr_Pixel:
push ax
int 10h
pop ax
dec si
inc cx
jmp Check_If_To_Print_Another_Pixel
EndPrinting:
pop si
pop dx 
pop cx
pop bx
pop ax
ret
endp Draw_Horizontal_Line_Using_INT10h


 ; =================
; Draw Vertical Line
; input: 
;		si = length
;		al = color
;		cx = X value 
;		dx = Y value 
; output: Screen Memory
; =================
proc Draw_Vertical_Line
push ax 
push bx
push cx
push dx
push si
push es

push ax ;to save it for later

mov ax,0A000h
mov es,ax

mov ax,Screen_Width
mul dx
add ax,cx
mov bx,ax
;consequences: dx =0 
pop cx ;putting what was ax to cx

@@Check_If_To_Print_Another_Dot:
cmp si,0 ;to exit if the length is 0
jz @@End_Printing_HL

@@Draw_Another_Dot:
mov [byte ptr Es:bx],cl 
add bx,Screen_Width

dec si
jmp @@Check_If_To_Print_Another_Dot

@@End_Printing_HL:
Pop es
pop si
pop dx 
pop cx
pop bx
pop ax
ret
endp Draw_Vertical_Line

; =================
; Print Rhombus (Case)
; Draws a Rhombus, capable of drawing to any direction.
; input: vars in DATASEG: shp_Height,shp_Length,shp_PressX,shp_PressY
; output: Screen Memory
; =================
proc Print_Rhombus
	 
	 push ax
	 
	 mov ax,[shp_Height]
	 push ax
	 mov ax,[shp_Length]
	 push ax
	 mov ax,[shp_PressX]
	 push ax
	 mov ax, [shp_PressY]
	 push ax
	 call Draw_Rhombus_Rev
	 
	 pop ax
	 
ret
endp Print_Rhombus

; =================
; Draw Rhombus (Rev)
; draws a Rhombus.
; input: (through stack)
; 		Param1 = Height 
;		Param2 = Base Length 
;		param3 = X value 
;		param4 = Y value of the lowest point
; output: Screen Memory
; =================
proc Draw_Rhombus_Rev

@@Height equ [word ptr bp+10]
@@Length equ [word ptr bp+8]
@@X equ [word ptr bp+6]
@@Y equ [word ptr bp+4]
@@Center_Point_X equ [word ptr bp-2]
@@Center_Point_Y equ [word ptr bp-4]


	 push bp
	 mov bp,sp
	 sub sp,4d
	 
	 push ax
	 push cx
	 push dx
	 push si

	 cmp @@Length,0
	 jz @@EndPrintingRhombus
	 jg @@Pos_Length
	 
	 mov ax,@@X
	 add ax,@@Length
	 mov @@X,ax
	 
	 mov ax,@@Length
	 call Return_AX_Absolute
	 mov @@Length,ax
	 @@Pos_Length:
	 
	 cmp @@Height,0
	 jz @@EndPrintingRhombus
	 jg @@Pos_Height
	 
	 mov ax,@@Y
	 add ax,@@Height
	 mov @@Y,ax
	 
	 mov ax,@@Height
	 call Return_AX_Absolute
	 mov @@Height,ax
	 @@Pos_Height:
	 
	 
	 mov ax,@@Length ; calcing center coordinates
	 shr ax,1
	 add ax,@@X
	 mov @@Center_Point_X,ax
	 mov ax,@@Height
	 shr ax,1
	 add ax,@@Y
	 mov @@Center_Point_Y,ax
	 
	 mov ax,@@X ;drawing first line  
	 mov [point1X],ax
	  mov ax,@@Center_Point_Y
	 mov [point1Y],ax
	 
	 mov ax,@@Center_Point_X
	 mov [point2X],ax
	 mov ax, @@Y
	 mov [point2Y],ax
	 
	 call DrawLine2D
	 
	 mov ax,@@Center_Point_X ;drawing second line  
	 mov [point1X],ax
	  mov ax,@@Y
	 mov [point1Y],ax
	 
	 mov ax,@@X 
	 add ax, @@Length
	 mov [point2X],ax
	 mov ax, @@Center_Point_Y
	 mov [point2Y],ax
	 
	 call DrawLine2D
  
	 mov ax,@@X  ;drawing third line
	 add ax, @@Length
	 mov [point1X],ax
	 mov ax, @@Center_Point_Y
	 mov [point1Y],ax
	 
	 mov ax,@@Center_Point_X
	 mov [point2X],ax
	 mov ax, @@Y
	 add ax,@@Height
	 mov [point2Y],ax
	 
	 call DrawLine2D
	 
	 mov ax,@@Center_Point_X ;drawing fourth line  
	 mov [point1X],ax
	 mov ax, @@Y
	 add ax,@@Height
	 mov [point1Y],ax
	 
	 mov ax,@@X 
	 mov [point2X],ax
	 mov ax, @@Center_Point_Y
	 mov [point2Y],ax
	 
	 call DrawLine2D
@@EndPrintingRhombus:
	 pop si
	 pop dx
	 pop cx
	 pop ax

	 add sp,4d
	 pop bp
	 ret 8
endp Draw_Rhombus_Rev


; =================
; Print Circle (Case)
; Draws a Circle, capable of receiving negative shp_Height and shp_Length.
; BeAware! The circle's center is the center of the selected rectangular space
; input: vars in DATASEG: shp_Height,shp_Length,shp_PressX,shp_PressY
; output: Screen Memory
; =================
proc Print_Circle
@@Radius equ [word ptr bp-2]
	 
	 push bp
	 mov bp,sp
	 sub sp,2d
	 
	 push ax
	 push cx
	 push dx
	 push di
	 
	 cmp [shp_Height],0
	 jg @@Skip_Abs_Height
	 jz @@End_Proc
	 mov ax,[shp_Height] ; abs the height and updates PressY accordingly
	 add [shp_PressY],ax
	 call Return_AX_Absolute
	 mov [shp_Height],ax
@@Skip_Abs_Height:
	 
	 cmp [shp_Length],0
	 jg @@Skip_Abs_Length
	 jz @@End_Proc
	 mov ax,[shp_Length]; abs the length and updates PressX accordingly
	 add [shp_PressX],ax
	 call Return_AX_Absolute
	 mov [shp_Length],ax
@@Skip_Abs_Length:
	 
	 mov ax,[shp_Height] ;calcs radiuses from height and length then picks the smaller amongst the two
	 cmp ax,[shp_Length]
	 jbe @@AxIsSmaller
	 mov ax,[shp_Length]
@@AxIsSmaller:
	 shr ax,1 ; radius = length/2 or height/2
	 mov @@Radius,ax

	 mov cx,[shp_Length] ; calcing center point x value
	 shr cx,1
	 add cx,[shp_PressX]
	 mov dx,[shp_Height]; calcing center point y value
	 shr dx,1
	 add dx,[shp_PressY]
	 call Calc_Loc_In_Screen_Memory
	 mov di,ax
	 mov cx,@@Radius ;preps for calling circleV1
	 mov al,[Color]
	 
     call circleV3_Rev 
	 
	 
	 
@@End_Proc:

	 pop di
	 pop dx
	 pop cx
	 pop ax
	 
	 add sp,2d
	 pop bp
ret
endp Print_Circle

; =================
; Print Triangle 90 Deg (Case)
; Draws a 90 Degrees Triangle, capable of receiving negative shp_Height and shp_Length.
; BeAware! The two catheti always protrude from Press Point
; input: vars in DATASEG: shp_Height,shp_Length,shp_PressX,shp_PressY
; output: Screen Memory
; =================
proc Print_Triangle_90_Deg
	 push [shp_Height]
	 push [shp_Length]
	 push [shp_PressX]
	 push [shp_PressY]
	 call Draw_Triangle_90_Deg
ret
endp Print_Triangle_90_Deg

 
; =================
; Draw Triangle 90 Deg 
; draws a right angled triangle
; input: (through stack)
;		param1 = height
;		param2 = Length
;		param3 = X value 
;		param4 = Y value
; output: Screen Memory
; =================
proc Draw_Triangle_90_Deg

@@Y equ [word ptr word ptr bp+4]
@@X equ [word ptr word ptr bp+6]
@@Length equ [word ptr word ptr bp+8]
@@Height equ [word ptr bp+10]
@@XTemp equ [word ptr bp-2]
@@YTemp equ [word ptr bp-4]
	 
	 push bp
	 mov bp,sp
	 sub sp,4; allocating space to t word sized vars
	 
	 push ax
	 push cx
	 push dx
	 push si
	 
	 mov ax,@@Y ; saving the point's values for later
	 mov @@YTemp,ax
	 mov ax,@@X
	 mov @@XTemp,ax
	 
	 
	 cmp @@Length,0
	 jg @@Length_Is_Pos 
	 je @@End_Proc
	 
	 ; IF Length is negative
	 mov ax,@@length
	 call Return_AX_Absolute
	 mov @@Length,ax
	 sub @@XTemp,ax
	 
	 
	 mov cx,@@XTemp ; |  \<-- start writting from here, Draws the base (The function draws from left to right so the start X value must be different)
	 mov dx,@@Y 
	 mov al, [Color]
	 push @@Length
	 call Return_Absolute_Through_Stack
	 pop si
	 call Draw_Horizontal_Line
	 
	 jmp @@End_Length_Calc
@@Length_Is_Pos:
	 mov ax,@@Length 
	 add @@XTemp,ax
	 
	 mov cx,@@X ;  start writting from here -->|  \, Draws the base (The function draws from left to right so the start X value must be different)
	 mov dx,@@Y 
	 mov al, [Color]
	 mov si,@@Length
	 call Draw_Horizontal_Line
@@End_Length_Calc:



	 cmp @@Height,0
	 jg @@Height_Is_Pos
	 je @@End_Proc

	 mov ax,@@Height; IF Length is negative
	 call Return_AX_Absolute
	 mov @@Height,ax
	 sub @@YTemp,ax
	 
	 
	 mov cx, @@X
	 mov dx,@@YTemp ;  start writting from here -->|  \, Draws the non base leg of the triangle (The function draws downwards so the start Y value must be different)
	 mov al, [Color]
	 push @@Height
	 call Return_Absolute_Through_Stack
	 pop si
	 call Draw_Vertical_Line
	 
	 jmp @@End_Height_Calc
	 
@@Height_Is_Pos:
	 mov ax,@@Height
	 add @@YTemp,ax
	 
	 mov cx, @@X 
	 mov dx,@@Y  ;  start writting from here -->|  \, Draws the non base leg of the triangle (The function draws downwards so the start Y value must be different)
	 mov al, [Color]
	 mov si,@@Height
	 call Draw_Vertical_Line
@@End_Height_Calc:

	 mov cx,@@X ;Saving for the diagonal line
	 mov dx,@@Y
	 mov [point1X],cx
	 mov [point2Y],dx
	 mov ax,@@XTemp
	 mov [point2X],ax
	 mov ax,@@YTemp
	 mov [point1Y],ax

	 call DrawLine2D
	 
	 @@End_Proc:
	 pop si
	 pop dx
	 pop cx
	 pop ax
	 
	 add sp,4 
	 
	 pop bp
ret 8
endp Draw_Triangle_90_Deg

; =================
; Diagonal Line (Case)
; Draws a Diagonal Line
; input: vars in DATASEG: shp_PressX,shp_PressY,shp_ReleaseX,shp_ReleaseY
; output: Screen Memory
; =================
proc Print_Diagonal_Line
	 push cx
	 push dx

	 mov cx,[shp_PressX]
	 mov dx,[shp_PressY]
	 mov [point1X],cx
	 mov [point1Y],dx
	 
	 mov cx,[shp_ReleaseX]
	 mov dx,[shp_ReleaseY]
	 mov [point2X],cx
	 mov [point2Y],dx
		
	 call DrawLine2D

	 pop cx
	 pop dx
ret
endp Print_Diagonal_Line

; =================
; circleV3 (Rev)
; Draws a circle (non recursive)
; Eliminated dependency on es the original proc had (es can be whatever before activation and the proc would still work)
; input: (through stack)
; 		cx = radius 
; 		di = point on screen where to put the center of circle 
; 		al - color
; output: Screen Memory
; =================
proc circleV3_Rev  
	
	push es
	push ax
	push bx
    push dx
	push si
	push bp
	
	push 0a000h ; so that es would point to the start of the Screen Memory
	pop es

	push ax   ; store the color
   
    xor bx,bx
    mov ax,cx
    shl ax,1    ; radius * 2
    mov dx,3    
    sub dx,ax   ; 3 - r * 2
	
	pop ax     ; al=color
   

    ; calculate address of center in vidmem.
   
    mov bp,di   ; center point

@@loop1:
    ; plot 8 pixels using known centerpoint and x and y offsets.
    mov di,bp  ; back to center

    add di,bx 
    mov si,cx
    shl si,6
    add di,si
    shl si,2
    add di,si
    mov [es:di],al

    sub di,si
    sub di,si
    shr si,2
    sub di,si
    sub di,si
    mov [es:di],al


    sub di,bx
    sub di,bx
    mov [es:di],al

    add di,si
    add di,si
    shl si,2
    add di,si
    add di,si
    mov [es:di],al

    mov di,bp
    add di,cx
    mov si,bx
    shl si,6
    add di,si
    shl si,2
    add di,si
    mov [es:di],al


    sub di,si
    sub di,si
    shr si,2
    sub di,si
    sub di,si
    mov [es:di],al

    sub di,cx
    sub di,cx
    mov [es:di],al


    add di,si
    add di,si
    shl si,2
    add di,si
    add di,si
    mov [es:di],al

    ; is dx still under zero?
    cmp dx,0
    jge @@loop2

    ; if, add 4*xoffset+6 to dx
    mov si,bx
    shl si,2
    add si,6
    add dx,si
    jmp @@loop3


@@loop2:
    ; if not, add 4*(xoffset-yoffset)+10 to dx
    mov si,bx
    sub si,cx
    shl si,2
    add si,10
    add dx,si
    ; decrease y coordinate
    dec cx

@@loop3:
    ; increase x coordinate
    inc bx
    ; is x offset<y offset; if is, loop again.
    cmp bx,cx
    jnl @@endProc
	jmp @@loop1

@@endProc:   
	pop bp
	pop si
	pop dx
	pop bx
	pop ax
	pop es
	
	
	ret
endp circleV3_Rev

include "Bres_Rev.asm"

;------------------------------------------------------------------
;Non_Functional 
;recives in ax what shape to print (guide to shapes in equs above)
proc Print_Shape_Button

	 call shp_Set_Mouse_Routine

Loop_Until_Right_Click: ;Exit if the user press the right mouse button
	 
	 mov ax,5 ;int to recieve press info
	 mov bx,1 ;0-Returns left button info, 1- Right buttons info
	 int 33h
	 cmp cx,0 ; cx returns how many times the button was clicked since last checked 
	 
	 jz Loop_Until_Right_Click
	 
	 call Set_Mouse_Routine
	 
	 ret 
endp Print_Shape_Button


;Sets the routine the program will follow if the left button is released or the right one is pressed 
;recives in ax what shape to print
proc shp_Set_Mouse_Routine

	 mov ax, seg shp_Mouse_Routine 
     mov es, ax
     mov dx, offset shp_Mouse_Routine   ; ES:DX ->Far routine
     mov ax,0Ch             ; interrupt number
     mov cx,4d            ; 4d = Right Released(http://www.ousob.com/ng/progref/ng3b053.php)
     int 33h  
ret
endp shp_Set_Mouse_Routine

proc shp_Mouse_Routine far
	 call Print_Shape
	 retf
endp shp_Mouse_Routine

;------------------------------------------------------------------