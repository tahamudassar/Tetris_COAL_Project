[org 0x0100]

jmp start

sxs: dw 4            ;screen-x-start
sxe: dw 40           ;screen-y-end
sys: dw 4            ;screen-y-start
sye: dw 21           ;scren-y-end
scolor: dw 0x7F      ;screen-color
oldisr: dd 0 ; space for saving old isr
; keyboard interrupt service routine
oldisrx:dd 0
tickcount: dw 0
xcord:dw 22
ycord:dw 5
xsize:dw 2 ;size of shape in horizontal
ysize:dw 2
startmessage1: db 'Press enter to start', 0

scoreString: db 'SCORE:'
scosLength: dw 6
scosX: dw 55
scosY: dw 4


scoreNumber: dw 0
sconX: dw 62
sconY: dw 4


nextShapeSt: db 'Incoming:'
nssl: dw 9
nssX: dw 55
nssY: dw 15


nsXs: dw 53
nsXe: dw 12
nsYs: dw 17
nsYe: dw 22
nsColor: dw 0x7F



time: db 'TIME: 01: 12'
timelength: dw 12
timeX: dw 55
timeY: dw 5

shapes: dw 1,2,4,3,2,3,1,2,3,1,1
shapesIndex: dw 0

endMessage1: dw ' G A M E  O V E R! ', 0
creditMessage1: dw 'S A A D  A L I', 0
creditMessage2: dw 'T A H A  M U D A S S A R', 0

delay:
push cx
mov cx,0xffff
l24:
loop l24
pop cx
ret


clearScreen:
	push es
	push ax
	push cx
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	xor di, di ; point di to top left column
	mov ax, 0x0720 ; space char in normal attribute
	mov cx, 2000 ; number of screen locations
	cld ; auto increment mode
	rep stosw ; clear the whole screen
	pop di
	pop cx
	pop ax
	pop es
	ret



printMainScreen:
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	
	mov ax, 0xb800
	mov es, ax ; point es to video base
	
	
	printNextRow:
	mov al, 80 ; load al with columns per row
	mul byte [bp+6] ; multiply with y position
	add ax, [bp+10] ; add x position
	shl ax, 1 ; turn into byte offset
	mov di,ax ; point di to required location
	
	
	mov cx, [bp+8] ; load length of string in cx
	mov ah, [bp+12] ; load attribute in ah 
	mov al, 0x20
	mov dx, [bp + 4]
	
	cld ; auto increment mode
	
	
	rep stosw ; print char/attribute pair
	inc word[bp+6]
	mov dx, [bp + 4]
    cmp dx, [bp + 6]
	jnz printNextRow
	
	
	pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 8
	
	
	






printScoreDisplay:
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov al, 80 ; load al with columns per row
	mul byte [bp+4] ; multiply with y position
	add ax, [bp+6] ; add x position
	shl ax, 1 ; turn into byte offset
	mov di,ax ; point di to required location
	mov si, [bp+8] ; point si to string
	mov cx, [bp+10] ; load length of string in cx
	mov ah, 0x09 ; load attribute in ah 
	cld ; auto increment mode
	nextchar: 
	lodsb ; load next char in al
	stosw ; print char/attribute pair
	loop nextchar ; repeat for the whole string
	pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 8
	
	
	
actualScore:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	
	mov ax, [bp+8] ; load number in ax
	mov bx, 10 ; use base 10 for division
	mov cx, 0 ; initialize count of digits
	nextdigit: 
	mov dx, 0 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30 ; convert digit into ascii value
	push dx ; save ascii value on stack
	inc cx ; increment count of values
	cmp ax, 0 ; is the quotient zero
	jnz nextdigit ; if no divide it again
	
	
	xor ax, ax
	mov al, 80 ; load al with columns per row
	mul byte [bp+4] ; multiply with y position
	add ax, [bp+6] ; add x position
	shl ax, 1 ; turn into byte offset
	mov di,ax ; point di to required location
	
	
	
	nextpos:
	pop dx ; remove a digit from the stack
	mov dh, 0x09 ; use normal attribute
	mov [es:di], dx ; print char on screen
	add di, 2 ; move to next screen location
	loop nextpos ; repeat for all digits on stack
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 6
	
	
	
printTime:
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov al, 80 ; load al with columns per row
	mul byte [bp+4] ; multiply with y position
	add ax, [bp+6] ; add x position
	shl ax, 1 ; turn into byte offset
	mov di,ax ; point di to required location
	mov si, [bp+8] ; point si to string
	mov cx, [bp+10] ; load length of string in cx
	mov ah, 0x09 ; load attribute in ah 
	cld ; auto increment mode
	nextchar2: 
	lodsb ; load next char in al
	stosw ; print char/attribute pair
	loop nextchar2 ; repeat for the whole string
	pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 8
	
	
	
printNextShapeString:
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov al, 80 ; load al with columns per row
	mul byte [bp+4] ; multiply with y position
	add ax, [bp+6] ; add x position
	shl ax, 1 ; turn into byte offset
	mov di,ax ; point di to required location
	mov si, [bp+10] ; point si to string
	mov cx, [bp+8] ; load length of string in cx
	mov ah, 0x09 ; load attribute in ah 
	cld ; auto increment mode
	nextchar1: 
	lodsb ; load next char in al
	stosw ; print char/attribute pair
	loop nextchar1 ; repeat for the whole string
	pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 8

;-------------------------------------------------v2-----------------------------------;

printnum:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov ax, [bp+4] ; load number in ax
	mov bx, 10 ; use base 10 for division
	mov cx, 0 ; initialize count of digits
nextdigits: 
	mov dx, 0 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30 ; convert digit into ascii value
	push dx ; save ascii value on stack
	inc cx ; increment count of values
	cmp ax, 0 ; is the quotient zero
	jnz nextdigits ; if no divide it again
	mov di, 140 ; point di to 70th column
nextposs: 
	pop dx ; remove a digit from the stack
	mov dh, 0x07 ; use normal attribute
	mov [es:di], dx ; print char on screen
	add di, 2 ; move to next screen location
	loop nextposs ; repeat for all digits on stack
	pop di
	pop dx
	pop cx
	pop bx
	pop ax 
	pop es
	pop bp
	ret 2
	
moveShapeR:
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push dx
	push ds
	
	mov ax, 0xb800
	mov es, ax ; point es to video base
	
	mov al, 80 ; load al with columns per row
	mul byte [ycord] ; multiply with y position
	add ax, [xcord] ; add x position
	shl ax, 1 ; turn into byte offset
	mov si,ax
	
	
	mov ax,[xsize]
	shl ax,1
	add ax,si
	mov di,ax
	sub ax,2
	mov si,ax
	cmp word[es:di],0x7F20
	jne skipsr
	;mov word[es:si],0x0921
	;jmp skipss
	mov cx,[xsize]
	mov dx,[ysize]
r1:
	mov cx,[xsize]
	;sub cx,1
	r2:
	mov ax,[es:si]
	mov [es:di],ax
	mov word[es:si],0x7F20;----------------------------change ascii here
	
	sub si,2
	sub di,2
	loop r2
	
	add si,160
	add di,160
	mov cx,[xsize]
	shl cx,1
	add di,cx
	add si,cx
	sub dx,1
	jnz r1
	
	mov ax,[xcord]
	add ax,1
	mov [xcord],ax ;updating current pos
	skipsr:
	pop ds
	pop dx
	pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 
	
moveShapeL:;parameter order bp+4=y,bp+6=x,bp+8=vertical size,bp+10=horizontal
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push dx
	push ds
	
	mov ax, 0xb800
	mov es, ax ; point es to video base
	
	mov al, 80 ; load al with columns per row
	mul byte [ycord] ; multiply with y position
	add ax, [xcord] ; add x position
	shl ax, 1 ; turn into byte offset
	mov si,ax
	
	sub ax,2
	mov di,ax
	cmp word[es:di],0x7F20
	jne skipsl
	
	mov cx,[xsize]
	mov dx,[ysize]
left1:
	mov cx,[xsize]
	
	left2:
	mov ax,[es:si]
	mov [es:di],ax
	mov word[es:si],0x7F20;----------------------------change ascii here
	add si,2
	add di,2
	loop left2
	
	add si,160
	add di,160
	mov cx,[xsize]
	shl cx,1
	sub di,cx
	sub si,cx
	sub dx,1
	jnz left1
	
	mov ax,[xcord]
	sub ax,1
	mov [xcord],ax ;updating current pos
	skipsl:
	pop ds
	pop dx
	pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 
shape1a:
	mov word[xsize],3
	mov word[ysize],2
	mov ax,22
	push ax
	mov ax,5
	push ax
	call shape1
	jmp shapeRet
	
shape2a:
	mov word[xsize],2
	mov word[ysize],2
	mov ax,22
	push ax
	mov ax,5
	push ax
	call shape2
	jmp shapeRet
	
shape3a:
	mov word[xsize],3
	mov word[ysize],3
	mov ax,22
	push ax
	mov ax,5
	push ax
	call shape3
	jmp shapeRet
	
shape4a:
	mov word[xsize],3
	mov word[ysize],2
	mov ax,22
	push ax
	mov ax,5
	push ax
	call shape4
	jmp shapeRet
	

	
shape1b:
	mov ax,58
	push ax
	mov ax,18
	push ax
	call shape1
	jmp shapeRetb
	
shape2b:
	
	mov ax,58
	push ax
	mov ax,18
	push ax
	call shape2
	jmp shapeRetb
	
shape3b:
	
	mov ax,58
	push ax
	mov ax,18
	push ax
	call shape3
	jmp shapeRetb
	
shape4b:
	
	mov ax,58
	push ax
	mov ax,18
	push ax
	call shape4
	jmp shapeRetb

resetIndex:
	mov word[shapesIndex],0
	jmp resetIndexRet
	
shapeCaller:
	push ax
	push bx
	push cx
	mov word[xcord],22
	mov word[ycord],5
	
	
	mov bx,[shapesIndex]
	shl bx,1
	mov ax,[shapes+bx]
	add word[shapesIndex],1
	cmp word[shapesIndex],10
	je resetIndex
	resetIndexRet:
	cmp ax,1
	je shape1a
	cmp ax,2
	je shape2a
	cmp ax,3
	je shape3a
	cmp ax,4
	je shape4a
	shapeRet:
	
	push ax
	push bx
	push si
	push cx
	
	mov si,2826
	mov ax,5
	loops:
	mov cx,12
	loops1:
	mov word[es:si],0x0920 ;change here
	add si,2
	loop loops1
	add si,160-24
	sub ax,1
	jnz loops
	;mov word[es:3328],0x0921
	;mov word[es:3306],0x0921
	;
	;mov word[es:2826],0x0921
	pop cx
	pop si
	pop bx
	pop ax
	
	add bx,2
	mov ax,[shapes+bx] ;for next shape graphic
	
	
	cmp ax,1
	je shape1b
	cmp ax,2 
	je shape2b
	cmp ax,3 
	je shape3b
	cmp ax,4 
	je shape4b
	shapeRetb:
	pop cx
	pop bx
	pop ax
	ret
	
collison:
	pop di
	call endCheck
	call checkRow
	;mov word[xcord],22
	;mov word[ycord],5
	;mov word[xsize],2
	;mov word[ysize],2
	;push ax
	;mov ax,22
	;push ax
	;mov ax,5
	;push ax
	call shapeCaller
	;pop ax
	jmp skipsd
moveShapeD:;parameter order bp+4=y,bp+6=x,bp+8=ysize,bp+10=xsize
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push dx
	push ds
	
	mov ax, 0xb800
	mov es, ax ; point es to video base
	
	mov al, 80 ; load al with columns per row
	mul byte [ycord] ; multiply with y position
	add ax, [xcord] ; add x position
	shl ax, 1 ; turn into byte offset
	mov si,ax
	
	
	mov al,160
	mul byte [ysize]
	add ax,si
	mov di,ax
	sub ax,160
	mov si,ax
	mov cx,[xsize]
	push di
l69:
	cmp word[es:di],0x7F20
	jne collison
	add di,2
loop l69
	pop di
	;jmp skipss
	mov cx,[xsize]
	mov dx,[ysize]
down1:
	mov cx,[xsize]
	;---------------------------------------may be sub cx,1
down2:
	mov ax,[es:si]
	mov [es:di],ax
	mov word[es:si],0x7F20;----------------------------change ascii here
	add si,2
	add di,2
	loop down2
	
	sub si,160
	sub di,160
	mov cx,[xsize]
	shl cx,1
	sub di,cx
	sub si,cx
	sub dx,1
	jnz down1
	
	
	
	mov ax,[ycord]
	add ax,1
	mov [ycord],ax ;updating current pos
	
	skipsd:
	;------------------------------------check row
	pop ds
	pop dx
	pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 
temp69:
	jmp printEndScreen
endCheck: ;this checks if the top row has been reached
	push es
	push ax
	push cx
	push si
	push di
	push dx
	;push ds
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov di,800 ;first row
	mov cx,80
	cmp word[ycord],5
	je temp69
;checkLoop:
;	cmp word[es:di],0x7F20
;	jne clearScreen
;	loop checkLoop
	;pop ds
	pop dx
	pop di
	pop si
	pop cx
	pop ax
	pop es
	ret	
	
temp:
	mov ax,0
	jmp returns

scrollDown:
	push di
	push cx
	push ax
	push si
	push dx
	sub di,80
	mov cx,80 ;loading size of columns
	mov si,di
	sub si,160 ;pointing si to one row before di
	sub di,8  ;subtracting 8 to make it so it is a factor o 160
	mov dl,160
	mov ax,di
	div dl
	mov dl,al
	
	sub dl,4  ;dl will have number od rows to be shifted
	add di,8 ;returning it back
	
scrollLoop1:
	
	mov cx,40
	scroollLoop2:
	mov ax,word[es:si]
	mov word[es:di],ax
	add si,2
	add di,2
	loop scroollLoop2
	sub di,160+80   ;moving back one row and going back to start of column
	sub si,160+80
	sub dl,1
	jnz scrollLoop1
	pop dx
	pop si
	pop ax
	pop cx
	pop di
	ret
scrollDownHelp:
	call scrollDown
	
	mov ax, [scoreNumber]    ;bp + 8 
	add ax,10
	push ax
	mov ax, [sconX]          ;bp+6
	push ax
	mov ax, [sconY]          ;bp + 4
	push ax
	call actualScore

	jmp scrollRet

checkRow:
	push es
	push ax
	push cx
	push si
	push di
	push dx
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov di,3208 ;difference in first and last pixel (horizontaly) is 78
	;mov word[es:648],0x0921
	mov dx,16
	mov ax,1
	checkFullLine1:;outer loop
	mov cx,40
	
	checkFullLine:
	cmp word[es:di],0x7F20
	je temp   
	returns:
	;mov word[es:di],0x0921	;for debugging
	add di,2
	loop checkFullLine
	cmp ax,1
	mov ax,1
	je scrollDownHelp ;;;;;;;;;;;;;;----------------------------------------------
	scrollRet:
	sub di,160+80
	sub dx,1
	jnz checkFullLine1;outer loop
	;mov word[es:di],0x0921
	
	
	pop dx
	pop di
	pop si
	pop cx
	pop ax
	pop es
	ret

	
	
timer: 
	push ax
	
	inc word [cs:tickcount]; increment tick count
	cmp word[tickcount],250
	je temp69
	push word [cs:tickcount]
	;push xcord
	call printnum ; print tick count
	
	
;-----------------------------------------------------------------------------;	check for boundary
	;call moveShapeD
	;mov si,[xcord]
	;mov word[es:si],0x0921
	push ax
	push bx
	mov al, 80 ; load al with columns per row
	mul byte [ycord] ; multiply with y position
	add ax, [xcord] ; add x position
	shl ax, 1 ; turn into byte offset
	mov bx,ax
	
	mov ax,0
	mov al,160
	mul byte [ysize]
	add ax,bx
	cmp ax,0x0720
	pop bx
	pop ax
	je skipd
	call delay
	call delay
	call moveShapeD
	call checkRow
	
	
skipd: ;skip down movement
	mov al, 0x20
	out 0x20, al ; end of interrupt
	pop ax
	iret ; return from interrupt
	
	
	
kbisr: 
    push ax
    push es
    mov ax, 0xb800
    mov es, ax ; point es to video memory
    in al, 0x60 ; read a char from keyboard port
    cmp al, 0x4B ; is the key left shift
    jne nextcmp ; no, try next comparison
	call moveShapeL
    mov byte [es:0], 'L' ; yes, print L at top left
	push dx
	
	;mov dx,10
	;push dx
	;push dx
	;call shape1
	pop dx
    jmp nomatch ; leave interrupt routine
nextcmp: 
	cmp al, 0x4D ; is the key right shift
    jne nomatch ; no, leave interrupt routine
	call moveShapeR
    mov byte [es:0], 'R' ; yes, print R at top left
nomatch: ; mov al, 0x20
 ; out 0x20, al
    pop es
    pop ax
    jmp far [cs:oldisr] ; call the original ISR
 ; iret 
shape1: ; the origin is the top left block; reverse T
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov al, 80 ; load al with columns per row
	mul byte [bp+4] ; multiply with y position
	add ax, [bp+6] ; add x position
	shl ax, 1 ; turn into byte offset
	mov di,ax ; point di to required location
	mov al, 0x00
	mov ah, 0x2A
	;mov word[es:di], ax
	mov word[es:di+2], ax
	mov word[es:di+160], ax
	mov word[es:di+162], ax;top block
	mov word[es:di+164], ax;top block
	pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 4

	
	
shape2: ; the origin is the top left block; square
push bp
 mov bp, sp
 push es
 push ax
 push cx
 push si
 push di
 mov ax, 0xb800
 mov es, ax ; point es to video base
 mov al, 80 ; load al with columns per row
 mul byte [bp+4] ; multiply with y position
 add ax, [bp+6] ; add x position
 shl ax, 1 ; turn into byte offset
 mov di,ax ; point di to required location
 
 mov al, 0x00
 mov ah, 0x19
 mov word[es:di], ax
 mov word[es:di+2], ax
 mov word[es:di+160], ax;top block 1
 mov word[es:di+2+160], ax;top block 2
 pop di
 pop si
 pop cx
 pop ax
 pop es
 pop bp
 ret 4





shape3: ; the origin is the top left block; L
push bp
 mov bp, sp
 push es
 push ax
 push cx
 push si
 push di
 mov ax, 0xb800
 mov es, ax ; point es to video base
 mov al, 80 ; load al with columns per row
 mul byte [bp+4] ; multiply with y position
 add ax, [bp+6] ; add x position
 shl ax, 1 ; turn into byte offset
 mov di,ax ; point di to required location
 
 mov ah, 0x4C
 mov al, 0x00
 mov word[es:di], ax
 mov word[es:di+160], ax
 mov word[es:di+320], ax
 ;mov word[es:di+480], 0x0741
 mov word[es:di+320+2], ax
 mov word[es:di+320+4], ax
 
 pop di
 pop si
 pop cx
 pop ax
 pop es
 pop bp
 ret 4
 
 
 
 shape4: ;origin is top left;T
 push bp
 mov bp, sp
 push es
 push ax
 push cx
 push si
 push di
 mov ax, 0xb800
 mov es, ax ; point es to video base
 mov al, 80 ; load al with columns per row
 mul byte [bp+4] ; multiply with y position
 add ax, [bp+6] ; add x position
 shl ax, 1 ; turn into byte offset
 mov di,ax ; point di to required location
 
 mov ah, 0x3B
 mov al, 0x00
 mov word[es:di], ax
 mov word[es:di+2], ax
 mov word[es:di+4], ax
 mov word[es:di+2+160], ax
 
 pop di
 pop si
 pop cx
 pop ax
 pop es
 pop bp
 ret 4





PrintNextShapeScreen:
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	
	mov ax, 0xb800
	mov es, ax ; point es to video base
	
	
	printNextRow1:
	mov al, 80 ; load al with columns per row
	mul byte [bp+6] ; multiply with y position
	add ax, [bp+10] ; add x position
	shl ax, 1 ; turn into byte offset
	mov di,ax ; point di to required location
	
	
	mov cx, [bp+8] ; load length of string in cx
	mov ah, [bp+12] ; load attribute in ah 
	mov al, 0x20
	mov dx, [bp + 4]
	
	cld ; auto increment mode
	
	
	rep stosw ; print char/attribute pair
	inc word[bp+6]
	mov dx, [bp + 4]
    cmp dx, [bp + 6]
	jnz printNextRow1
	
	
	pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 8






;--------------------------------------startscreen-----------------------

setgraybg:
	push es
	push ax
	push cx
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	xor di, di ; point di to top left column
	mov ax, 0x7720 ; space char in normal attribute
	mov cx, 2000 ; number of screen locations
	cld ; auto increment mode
	rep stosw ; clear the whole screen
	pop di 
	pop cx
	pop ax
	pop es
	ret
	

pblackbox: 
	push bp
    mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov al, 80 ; load al with columns per row
	mov dl, 16
	mul dl  ; multiply with y position
	add ax, 6 ; add x position
	shl ax, 1 ; turn into byte offset
	mov di,ax ; point di to required location
	mov cx, 22 
	mov ah, 0x00 ; load attribute in ah 
	mov al, ' '
	mov bx, 8
	cld ; auto increment mode
	
	outerloop:
	mov cx, 30
	nextchar10: 
	stosw ; print char/attribute pair
	loop nextchar10
	add di, 158
	sub di, 58
	dec bx
	jnz outerloop

	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	
	ret 8



pvl:
	push bp
    mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov al, 80 ; load al with columns per row
	mul byte [bp+8] ; multiply with y position
	add ax, [bp+10] ; add x position
	shl ax, 1 ; turn into byte offset
	mov di,ax ; point di to required location
	mov cx, [bp+6] ; load length of string in cx
	mov ah, [bp+4] ; load attribute in ah 
	mov al, ' '
	
	cld ; auto increment mode
	nextchar20: 
	stosw ; print char/attribute pair
	add di, 158
	loop nextchar20

	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	
	ret 8
	
	
	

phl:
	push bp
    mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov al, 80 ; load al with columns per row
	mul byte [bp+8] ; multiply with y position
	add ax, [bp+10] ; add x position
	shl ax, 1 ; turn into byte offset
	mov di,ax ; point di to required location
	mov cx, [bp+6] ; load length of string in cx
	mov ah, [bp+4] ; load attribute in ah 
	mov al, ' '
	
	cld ; auto increment mode
	nextchar5: 
	stosw ; print char/attribute pair
	loop nextchar5

	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	
	ret 8
	
	
	
	
	
	
	
	
prdl:
	push bp
    mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov al, 80 ; load al with columns per row
	mul byte [bp+8] ; multiply with y position
	add ax, [bp+10] ; add x position
	shl ax, 1 ; turn into byte offset
	mov di,ax ; point di to required location
	mov cx, [bp+6] ; load length of string in cx
	mov ah, [bp+4] ; load attribute in ah 
	mov al, ' '
	
	cld ; auto increment mode
	nextchar25: 
	stosw ; print char/attribute pair
	add di, 160
	loop nextchar25

	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	
	ret 8
	
	
	
pldl:
	push bp
    mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov al, 80 ; load al with columns per row
	mul byte [bp+8] ; multiply with y position
	add ax, [bp+10] ; add x position
	shl ax, 1 ; turn into byte offset
	mov di,ax ; point di to required location
	mov cx, [bp+6] ; load length of string in cx
	mov ah, [bp+4] ; load attribute in ah 
	mov al, ' '
	
	cld ; auto increment mode
	nextchar3: 
	stosw ; print char/attribute pair
	sub di, 160
	loop nextchar3

	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	
	ret 8
	
	
printB:
	push bp
    mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	

	mov ax, 10          ;push x coord
	push ax
	mov ax, 4			;push y coord
	push ax
	mov ax, 6 			;push length
	push ax
	mov ax, 0x17        ;color attribute
	push ax
	call pvl            ;print vertical line
	
	
	mov ax, 10          ;push x coord
	push ax
	mov ax, 4			;push y coord
	push ax
	mov ax, 8 			;push length
	push ax
	mov ax, 0x17        ;color attribute
	push ax
	call phl            ;print horiz line
	
	
	mov ax, 10          ;push x coord
	push ax
	mov ax, 7			;push y coord
	push ax
	mov ax, 8 			;push length
	push ax
	mov ax, 0x17        ;color attribute
	push ax
	call phl            ;print horiz line
	
	
	mov ax, 10          ;push x coord
	push ax
	mov ax, 10			;push y coord
	push ax
	mov ax, 8 			;push length
	push ax
	mov ax, 0x17        ;color attribute
	push ax
	call phl            ;print horiz line
	
	
	mov ax, 18          ;push x coord
	push ax
	mov ax, 4			;push y coord
	push ax
	mov ax, 4 			;push length
	push ax
	mov ax, 0x17        ;color attribute
	push ax
	call pvl            ;print horiz line
	
	
	mov ax, 18          ;push x coord
	push ax
	mov ax, 7			;push y coord
	push ax
	mov ax, 4 			;push length
	push ax
	mov ax, 0x17        ;color attribute
	push ax
	call pvl            ;print horiz line
	
	
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	
	ret
	
	
printL:
	push bp
    mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	
	
	mov ax, 24          ;push x coord
	push ax
	mov ax, 4			;push y coord
	push ax
	mov ax, 7 			;push length
	push ax
	mov ax, 0x17        ;color attribute
	push ax
	call pvl
	
	mov ax, 24          ;push x coord
	push ax
	mov ax, 10			;push y coord
	push ax
	mov ax, 8 			;push length
	push ax
	mov ax, 0x17        ;color attribute
	push ax
	call phl
	

	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	
	ret
	
	
printO:
	push bp
    mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	
	
	mov ax, 36          ;push x coord
	push ax
	mov ax, 4			;push y coord
	push ax
	mov ax, 7 			;push length
	push ax
	mov ax, 0x17        ;color attribute
	push ax
	call pvl            ;print horiz line
	
	mov ax, 36          ;push x coord
	push ax
	mov ax, 4			;push y coord
	push ax
	mov ax, 10 			;push length
	push ax
	mov ax, 0x17        ;color attribute
	push ax
	call phl
	
	mov ax, 36          ;push x coord
	push ax
	mov ax, 10			;push y coord
	push ax
	mov ax, 10 			;push length
	push ax
	mov ax, 0x17        ;color attribute
	push ax
	call phl
	
	mov ax, 46          ;push x coord
	push ax
	mov ax, 4			;push y coord
	push ax
	mov ax, 7 			;push length
	push ax
	mov ax, 0x17        ;color attribute
	push ax
	call pvl
	
	

	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	
	ret
	
	
	
	
printX:
	push bp
    mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	
	
	mov ax, 52          ;push x coord
	push ax
	mov ax, 4			;push y coord
	push ax
	mov ax, 7 			;push length
	push ax
	mov ax, 0x17        ;color attribute
	push ax
	call prdl            ;print horiz line
	
	
	mov ax, 52          ;push x coord
	push ax
	mov ax, 10			;push y coord
	push ax
	mov ax, 7 			;push length
	push ax
	mov ax, 0x17        ;color attribute
	push ax
	call pldl            ;print horiz line
	
	
	
	

	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	
	ret
	
	
printstartMessage: 
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push ds
	pop es ; load ds in es
	mov di, [bp+4] ; point di to string
	mov cx, 0xffff ; load maximum number in cx
	xor al, al ; load a zero in al
	repne scasb ; find zero in the string
	mov ax, 0xffff ; load maximum number in ax
	sub ax, cx ; find change in cx
	dec ax ; exclude null from length
	jz exit ; no printing if string is empty
	mov cx, ax ; load string length in cx
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov al, 80 ; load al with columns per row
	mul byte [bp+8] ; multiply with y position
	add ax, [bp+10] ; add x position
	shl ax, 1 ; turn into byte offset
	mov di,ax ; point di to required location
	mov si, [bp+4] ; point si to string
	mov ah, [bp+6] ; load attribute in ah
	cld ; auto increment mode
	nextchar7: lodsb ; load next char in al
	stosw ; print char/attribute pair
	loop nextchar7 ; repeat for the whole string
	exit: pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 8
	
	
	
printEndScreen:
	push bp
	mov bp, sp
	push ax
	
	
	call clearScreen
	
	mov ax, 30
	push ax ; push x position
	mov ax, 10
	push ax ; push y position
	mov ax, 0x71 ; blue on black attribute
	push ax ; push attribute
	mov ax, endMessage1
	push ax ; push address of message
	call printstartMessage
	
	
	mov ax, 50
	push ax ; push x position
	mov ax, 20
	push ax ; push y position
	mov ax, 0x1 ; blue on black attribute
	push ax ; push attribute
	mov ax, creditMessage2
	push ax ; push address of message
	call printstartMessage
	
	
	mov ax, 50
	push ax ; push x position
	mov ax, 22
	push ax ; push y position
	mov ax, 0x1 ; blue on black attribute
	push ax ; push attribute
	mov ax, creditMessage1
	push ax ; push address of message
	call printstartMessage
	
	

	pop ax
	pop bp
	
	ret











start:

	call clearScreen
	call setgraybg
	
	call printB
	call printL
	call printO
	call printX
	
	mov ax, 0          ;push x coord
	push ax
	mov ax, 14			;push y coord
	push ax
	mov ax, 80 			;push length
	push ax
	mov ax, 0x94        ;color attribute
	push ax
	call phl            ;print horiz line
	
	
	call pblackbox
	
	
	
	mov ax, 10
	push ax ; push x position
	mov ax, 19
	push ax ; push y position
	mov ax, 1 ; blue on black attribute
	push ax ; push attribute
	mov ax, startmessage1
	push ax ; push address of message
	call printstartMessage





	mov ah, 0 
	int 0x16















	call clearScreen


mov ax, [scolor]  ;screen color bp+12
push ax
mov ax, [sxs]    ;bp+10
push ax
mov ax, [sxe]     ;bp+8
push ax
mov ax, [sys]   ;bp+6
push ax
mov ax, [sye]     ;bp+4
push ax
call printMainScreen




mov ax, [scosLength]   ;bp + 10
push ax
mov ax, scoreString        ;bp+8
push ax
mov ax, [scosX]       ;bp+6
push ax
mov ax, [scosY]       ;bp+4
push ax
call printScoreDisplay




mov ax, [scoreNumber]    ;bp + 8 
push ax
mov ax, [sconX]          ;bp+6
push ax
mov ax, [sconY]          ;bp + 4
push ax
call actualScore






mov ax, nextShapeSt    ;bp + 10
push ax
mov ax, [nssl]        ;bp + 8
push ax
mov ax, [nssX]        ;bp + 6
push ax
mov ax, [nssY]       ;bp + 4
push ax
call printNextShapeString




mov ax, [nsColor]  ;screen color bp+12
push ax
mov ax, [nsXs]    ;bp+10
push ax
mov ax, [nsXe]     ;bp+8
push ax
mov ax, [nsYs]   ;bp+6
push ax
mov ax, [nsYe]     ;bp+4
push ax
call PrintNextShapeScreen

;------------------------------------v2-----------------------------------;
	mov ax,22
	push ax
	mov ax,5
	push ax
	call shape2
	xor ax, ax
	mov es, ax ; point es to IVT base
	mov ax, [es:9*4]
	mov [oldisr], ax ; save offset of old routine (for keyboar)
	mov ax, [es:9*4+2]
	mov [oldisr+2], ax ; save segment of old routine
	;for clock
	xor ax, ax
	mov es, ax ; point es to IVT base
	mov ax, [es:8*4]
	mov [oldisrx], ax ; save offset of old routine
	mov ax, [es:8*4+2]
	mov [oldisrx+2], ax ; save segment of old routine
	
	cli ; disable interrupts
	mov word [es:9*4], kbisr ; store offset at n*4
	mov [es:9*4+2], cs ; store segment at n*4+2
	
	mov word [es:8*4], timer ; store offset at n*4
	mov [es:8*4+2], cs 
		
	sti ; enable interrupts
	mainloop: 
	

	mov ah, 0 ; service 0 – get keystroke
	int 0x16 ; call BIOS keyboard service
	
	cmp al, 27 ; is the Esc key pressed
	jne mainloop ; if no, check for next key
	
	skipss:
	mov ax, [oldisr] ; read old offset in ax
	mov bx, [oldisr+2] ; read old segment in bx
	cli ; disable interrupts
	
	mov [es:9*4], ax ; restore old offset from ax
	mov [es:9*4+2], bx ; restore old segment from bx
	
	mov ax,[oldisrx]
	mov bx,[oldisrx+2]
	mov [es:8*4], ax ; restore old offset from ax
    mov [es:8*4+2], bx ; 
	
	sti ; enable interrupts
	
	mov dx, start ; end of resident portion
	add dx, 15 ; round up to next para
	mov cl, 4
	shr dx, cl ; number of paras
	mov ax, 0x3100
	
	
	call printEndScreen  ;here you go nigga, end screen just like you wished, nice working with u
	
	mov ax, 0x4c00 ; terminate program
	int 0x21 





