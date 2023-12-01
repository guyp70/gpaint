; =================
; Calculates Location (address) In Screen Memory 
; input: Y value in dx and X Value in cx
; output: address in ax (Y*Screen_Width+X)
; =================
proc Calc_Loc_In_Screen_Memory
	 push bx
	 push cx
	 push dx
	 
	 mov ax,dx; calcing current position in screen memory
	 mov dx,Screen_Width;muls Y val by Screen_Width
	 mul dx
	 add ax,cx ; adds the X value
	 
	 pop dx
	 pop cx
	 pop bx
ret
endp Calc_Loc_In_Screen_Memory

; =================
; Gets Last Left Press info
; input: none
; output: x value in cx, y value at dx and mouse status in AX
; ( Mouse status guide:
;   bit 0 = 1 (1) - Left button pressed,
;   bit 1 = 1 (2) - Right button pressed, 
;   bit 2 = 1 (4) - Middle button pressed )
; =================
proc Get_Last_Left_Press_Coordinates
	 push bx

	 mov ax, 5
	 mov bx,0
	 int 33h

	 shr cx,1 ;the Mouse default is 640X200 So divide 640 by 2 to get 320 (DosBox default)

	 pop bx
	 ret
endp Get_Last_Left_Press_Coordinates

; =================
; Gets Last Left Release info
; input: none
; output: x value in cx and y value at dx 
; =================
proc Get_Last_Left_Release_Coordinates
	 push ax
	 push bx 

	 mov ax, 6
	 mov bx,0
	 int 33h

	 shr cx,1 ;the Mouse default is 640X200 So divide 640 by 2 to get 320 (DosBox default)

	 pop bx
	 pop ax
	 ret
endp Get_Last_Left_Release_Coordinates

; =================
; Get Mouse Position and Button Status
; input: none
; output: x value in cx, y value at dx and mouse status in AX
; ( Mouse status guide:
;   bit 0 = 1 (1) - Left button pressed,
;   bit 1 = 1 (2) - Right button pressed, 
;   bit 2 = 1 (4) - Middle button pressed )
; =================
proc Get_Mouse_Position_and_Button_Status
	 push bx

	 mov ax, 3
	 mov bx,0
	 int 33h

	 shr cx,1 ;the Mouse default is 640X200 So divide 640 by 2 to get 320 (DosBox default)
	 mov ax,bx; so that it will act like the Get_Last_Left_Press_Coordinates
	 
	 pop bx
	 ret
endp Get_Mouse_Position_and_Button_Status

; =================
; Sets the display moode to graphic mode
; input: none
; output: none
; =================
proc SetGrapicMode

mov ax,13h
int 10h
ret

endp SetGrapicMode
 
; =================
; Sets the display mode to normal mode
; input: none
; output: none
; =================
proc SetNormalMode

mov ax,02h
int 10h
ret

endp SetNormalMode

; =================
; Set mouse movement boundaries to within the painting ground
; input: none
; output: none
; =================
proc Restrict_Mouse_to_Canvas
	 push ax
	 push cx
	 push dx

	 mov ax,8
	 mov cx,UI_Height
	 mov dx,Screen_Height
	 int 33h
	 
	pop dx
     pop cx
	 pop ax
	 ret
endp Restrict_Mouse_to_Canvas

; =================
; Release mouse Release mouse movement, cancel mouse movement boundaries 
; input: none
; output: none
; =================
proc Release_Mouse
	 push ax
	 push cx
	 push dx

	 mov ax,8
	 mov cx,0d
	 mov dx,Screen_Height
	 int 33h

	 pop dx
	 pop cx
     pop ax 
	 ret
endp Release_Mouse

proc Show_Mouse_Cursor
	 push ax

	 mov ax,1
	 int 33h

	 pop ax
	 ret
endp Show_Mouse_Cursor

; =================
; Hide Mouse Cursor
; input: none
; output: none
; =================
proc Hide_Mouse_Cursor
	 push ax

	 mov ax,2
	 int 33h

	 pop ax
	 ret
endp Hide_Mouse_Cursor

; =================
; Refresh Canvas - set all canvas bytes to white value
; input: none (White equ)
; output: none
; =================
proc Refresh_Canvas
	 push es
	 push di
	 push ax
	 push cx
	 
	 mov ax,0a000h
	 mov es,ax
	 mov di,Screen_Width*UI_Height
	 mov al,White
	 mov ah,White
	 
	 mov cx,(Screen_Width*Canvas_Height)/2
	 Rep stosw
	 
	 pop cx
	 pop ax
	 pop di
	 pop es
ret
endp Refresh_Canvas

proc Return_AX_Absolute

	 test ax,8000h
	 jz @@End_Proc 
	 xor ax,0ffffh
	 inc ax

@@End_Proc:
	 ret
endp Return_AX_Absolute


; =================
; Receives a value through stack and overwrites it with its absolute value
; input: Parmr1 = Value (through stack)
; output: Parmr1 = abs Value (through stack)
; =================
proc Return_Absolute_Through_Stack
@@Value equ [bp+4]
	 push bp 
	 mov bp,sp
	 push ax 
	 
	 mov ax,@@Value
	 
	 test ax,8000h
	 jz @@End_Proc 
	 xor ax,0ffffh
	 inc ax 
	 
	 mov @@Value,ax
	 
@@End_Proc:
	 pop ax 
	 pop bp 
	 ret ;there is a parameter but it also returns a value so no "ret 2"
endp Return_Absolute_Through_Stack

; =================
; Show Ax Decimal
; input: ax (value to print)
; output: print ax value to screen (value with the Two's complement method)
; =================
 proc ShowAxDecimal
	   push ax
       push bx
	   push cx
	   push dx
	   
	   ; check if negative
	   test ax,08000h
	   jz PositiveAx
			
	   ;  put '-' on the screen
	   push ax
	   mov dl,'-'
	   mov ah,2
	   int 21h
	   pop ax

	   neg ax ; make it positive
PositiveAx:
       mov cx,0   ; will count how many time we did push 
       mov bx,10  ; the divider
   
put_mode_to_stack:
       xor dx,dx
       div bx
       add dl,30h
	   ; dl is the current LSB digit 
	   ; we cant push only dl so we push all dx
       push dx    
       inc cx
       cmp ax,9   ; check if it is the last time to div
       jg put_mode_to_stack

	   cmp ax,0
	   jz pop_next  ; jump if ax was totally 0
       add al,30h  
	   mov dl, al    
  	   mov ah, 2h
	   int 21h        ; show first digit MSB
	       
pop_next: 
       pop ax    ; remove all rest LIFO (reverse) (MSB to LSB)
	   mov dl, al
       mov ah, 2h
	   int 21h        ; show all rest digits
       loop pop_next
		
	   mov dl, ','
       mov ah, 2h
	   int 21h
   
	   pop dx
	   pop cx
	   pop bx
	   pop ax
	   
	   ret
endp ShowAxDecimal

; =================
; Read Command Line from PSP to var
; copies the command line from the buffer in the psp to CommandLine var at DATASEG in asciiz (without the 'cr' char at the end), also puts in CommandLine_Length var the length of the command line
; BeAware! Max length of Command Line is 127 and a 0 is needed at the end so CommandLine var length should be at least 128d bytes
; input: PSP
; output: vars in DATASEG: CommandLine_Length, CommandLine
; =================
proc Read_Command_Line_From_PSP

	 push es
	 push ax
	 push bx
	 push cx
	 push si
	 push di

	 mov ah,62h ;return PSP segment adress 
	 int 21h
	 mov es, bx ;es = PSP segment
	 
	 mov cl,[byte ptr es:80h] ;byte at 80h = Number of bytes on command-line (including enter)
	 dec cl ;cl now = Number of bytes on command line not including the 'space' (20h) char at the start
	 mov [byte ptr CommandLine_Length],cl
	 cmp cl,0 ; if command line is empty, skip copying
	 jz @@End_Proc
	 
	 xor ch,ch 
	 mov si,81h+1h ;  81h =Command Line Start, 1h for the space at the start (will serve as indexer in PSP)
	 xor di,di ; will serve as indexer in CommandLine var
@@Copy_Another_Char:
	 
	 mov ah,[byte ptr es:si] 
	 mov [byte ptr CommandLine+di],ah
	 inc si
	 inc di
	 
	 loop @@Copy_Another_Char
	 
	 mov [byte ptr CommandLine+di],0; puts zero after the last char
	 
@@End_Proc:
	 
	 pop di
	 pop si
	 pop cx
	 pop bx
	 pop ax
	 pop es
	 
ret
endp Read_Command_Line_From_PSP
