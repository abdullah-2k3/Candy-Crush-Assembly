org 0x0100
	jmp start
	
	
clrscr:
	push es 
	push ax 
	push di 
	mov ax, 0xb800
	mov es, ax 
	mov di, 0 

	mov ax, 0x0720
	mov cx, 2000
	cld
	rep stosw

	pop di 
	pop ax 
	pop es 
	
	ret
	
endscreen:
	pusha	
	mov ax, 0xb800
	mov es, ax
	mov al, ' '
	mov ah, 0x22

	mov di, 1142
	
	mov cx, 7	
.l1:	
	mov [es:di], ax
	add di, 160
	call delay
	loop .l1
	
	mov di, 1134
	mov cx, 9
.l2:	
	mov [es:di], ax
	add di, 2
	call delay
	loop .l2
	
	mov di, 1156
	mov cx, 7
	mov ah, 0x00
.l3: 
	mov [es:di], ax
	add di, 160
	call delay
	loop .l3
	
	mov di, 1170
	mov cx, 7
.l4:
	mov [es:di], ax
	add di, 160
	call delay
	loop .l4
	
	
	mov di, 1636
	mov cx, 7
.l5:
	mov [es:di], ax
	add di, 2
	call delay
	loop .l5
	
	mov di, 1190
	mov cx, 7
	mov ah, 0x44
.l6:
	mov [es:di], ax
	sub di, 2
	call delay
	loop .l6
	
	mov cx, 6
.l7:
	mov [es:di], ax
	add di, 160
	call delay
	loop .l7
	
	mov cx, 8	
.l8:
	mov [es:di], ax
	add di, 2
	call delay
	loop .l8
	
	mov di, 1656
	mov cx, 8
.l9:
	mov [es:di], ax
	add di, 2
	call delay
	loop .l9
	
	
mov di, 1218
	mov cx, 7
	mov ah, 0x66
.l10:
	mov [es:di], ax
	sub di, 2
	call delay
	loop .l10
	
	mov cx, 6
.l11:
	mov [es:di], ax
	add di, 160
	call delay
	loop .l11
	
	mov cx, 8	
.l12:
	mov [es:di], ax
	add di, 2
	call delay
	loop .l12
	
	mov di, 1684
	mov cx, 8
.l13:
	mov [es:di], ax
	add di, 2
	call delay
	loop .l13
	
	
	mov di, 2184
	mov ah, 0x22
	mov cx, 7	
.l14:	
	mov [es:di], ax
	sub di, 160
	call delay
	loop .l14
	
	mov cx, 7
.l15:	
	add di, 162
	mov [es:di], ax
	call delay
	loop .l15
	
	add di, 2
	mov cx, 7	
.l16:	
	mov [es:di], ax
	sub di, 160
	call delay
	loop .l16
	
	add di, 166
	mov ah, 0x11
	mov cx, 7	
.l17:	
	mov [es:di], ax
	add di, 160
	call delay
	loop .l17
	
	mov di, 1248
	mov cx, 7
.l18:
	mov [es:di], ax
	add di, 2
	call delay
	loop .l18
	
	mov cx, 7	
.l19:	
	mov [es:di], ax
	add di, 160
	call delay
	loop .l19
	
	sub di, 160
	mov cx, 8	
.l20:	
	mov [es:di], ax
	sub di, 2
	call delay
	loop .l20
	
	
	mov di, 2570
	mov cx, 70
	mov ah, 0x77
.l21:	
	mov [es:di], ax
	add di, 2
	call delay
	loop .l21	
	
	popa
	ret

	
printbox:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push di
	
	mov ax, 0xb800
	mov es, ax
	mov di, [bp+4]
	
	mov ah, 0x11 ;blue color
	mov al, ' '

	mov bx, 2
.L1:
	mov cx, 4
.l1:
	mov [es:di], ax
	add di, 2
	loop .l1
	add di, 152
	dec bx
	cmp bx, 0
	jg .L1
	
	pop di
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2
	
	
printboard:
	pusha

	mov di, 356
	mov bx, 8
.L1:
	mov cx, 8      	
.l1:
	push di
	call printbox
	add di, 12	; next col
	loop .l1 
	add di, 384  ; next row
	dec bx
	cmp bx, 0
	jg .L1
	
	popa
	ret




printnum: push bp 
	 mov bp, sp 
	 push es 
	 push ax 
	 push bx 
	 push cx 
	 push dx 
	 push di 
	 push si
	 mov si, 0
	 mov ax, 0xb800 
	 mov es, ax ; point es to video base 
	 mov ax, [bp+4] ; load number in ax 
	 cmp ax, 0
	 je _pexit
	 
	 mov cx, 0 ; initialize count of digits 
nextdigit:
	 mov bx, 10
	 mov dx, 0 ; zero upper half of dividend 
	 div bx ; divide by 10 
	 add dl, 0x30 ; convert digit into ascii value 
	 push dx ; save ascii value on stack 
	 inc cx ; increment count of values 
	 cmp ax, 0 ; is the quotient zero 
	 jnz nextdigit ; if no divide it again 
	 mov di,  [bp+6]; 
nextpos:
	 pop dx ; remove a digit from the stack 
	 mov dh, 0x04 ; use normal attribute 
	 mov [es:di], dx ; print char on screen 
	 add di, 2 ; move to next screen location 
	 ;inc si
	 loop nextpos ; repeat for all digits on stack
_pexit:
	 pop si
	 pop di 
	 pop dx 
	 pop cx 
	 pop bx 
	 pop ax 
	 pop es 
	 pop bp 
	 ret 4
	 
	 
print_index_values:
	pusha
	;row values:
	
	mov di, 198
	mov ax, 1
	mov cx, 8
.l1:	
	push di
	push ax
	call printnum
	add di, 12
	inc ax
	loop .l1
	
	;col values:
	mov di, 514
	mov ax, 1
	mov cx, 8
.l2:	
	push di
	push ax
	call printnum
	add di, 480
	inc ax
	loop .l2
	
	
	
	popa
	ret
	
delay:
	push cx
	mov cx, 0xffff
.d:  nop
	loop .d
	pop cx
	ret
	
GenRandNum:
	push bp
	mov bp,sp;
	push cx
	push ax
	push dx;

	MOV AH, 00h ; interrupts to get system time
	INT 1AH ; CX:DX now hold number of clock ticks since midnight
	mov ax, dx
	xor dx, dx
	mov cx, 5;
	div cx ; here dx contains the remainder of the division - from 0 to 9
	inc dx
	mov word [randNum],dx;

	pop dx;
	pop ax;
	pop cx;
	pop bp;
	ret
	
initialize_board_values:
	pusha
	
	mov si, 0
	mov cx, 64
.l1:
	call delay
	call GenRandNum
	mov word ax, [randNum]
	cmp word ax, [board+si-2]
	jne .safeX
	cmp word ax, [board+si-4]
	jne .safeX
	jmp .l1
.safeX:
	cmp word ax, [board+si-16]
	jne .safeY
	cmp word ax, [board+si-32]
	jne .safeY
	jmp .l1
.safeY:
	mov word [board+si], ax
	
	add si, 2
	loop .l1
	
	popa
	ret
	
	
print_score:
	pusha
	mov ax, 0xb800
	mov es, ax
	
	mov ax, moves
	push word 456
	push ax
	call printstr
	mov di, 470
	push di
	mov ax, [move_count]
	push ax
	call clrtext
	mov di, 470
	push di
	mov ax, [move_count]
	push ax
	call printnum
	
	mov ax, score
	push word 776
	push ax
	call printstr
	mov di, 790
	push di
	mov ax, [score_count]
	push ax
	call clrtext
	mov di, 790
	push di
	mov ax, [score_count]
	push ax
	call printnum
	
	
	popa
	ret
	
print_candies:
	pusha
	mov ax, 0xb800
	mov es, ax
	
	
	mov bx, 0
	mov al, '@'
	mov di, 518	
	mov dx, 0
.L1:
	mov cx, 8
.l1:	
	mov si, [board+bx]
	mov ah, [color+si]
	mov [es:di], ax
	add bx, 2
	add di, 12
	loop .l1
	add di, 384
	inc dx
	cmp dx, 8
	jl .L1
	
	popa
	ret
	

printstr:
	push bp
	mov bp, sp
	pusha
	
	push ds
	pop es
	mov di,[bp+4]
	mov cx, 0xffff
	xor al,al
	repne scasb
	mov ax, 0xffff
	sub ax, cx
	dec ax
	jz d
	
	mov cx, ax
	mov ax, 0xb800
	mov es, ax
	mov di, [bp+6]
	mov si, [bp+4]
	mov ah, 0x0e
	cld
	nextchar:
		lodsb
		stosw
		loop nextchar
d:
		popa
		pop bp
		ret 4
	

clrtext:
	push bp
	mov bp,sp
	pusha
	    push ds
		pop es
		mov di,[bp+4]
		mov cx, 0xffff
		xor al,al
		repne scasb
		mov ax, 0xffff
		sub ax, cx
		dec ax
		jz exitclr
	mov cx, ax
	mov ax, 0xb800
	mov es, ax
	mov di, [bp+6]
	mov ah,0x0e
	mov al,20h
	cld
	rep stosw
	exitclr:popa
	pop bp
	ret 4

;------SOUND---------
sound:
	push bp
	mov bp, sp
	push ax

	mov al, 182
	out 0x43, al
	mov ax, [bp + 4]   ; frequency
	out 0x42, al
	mov al, ah
	out 0x42, al
	in al, 0x61
	or al, 0x03
	out 0x61, al
call delay
call delay
call delay
call delay
call delay
call delay
	in al, 0x61

	and al, 0xFC
	out 0x61, al

	pop ax
	pop bp
    ret 2



;-------------MOVE CODE-----------------------------

is_valid_move:
	push ax
	mov ax, [src_index]
	sub ax, [dst_index]
	cmp ax, 2
	je .true
	cmp ax, 16
	je .true
	mov ax, [dst_index]
	sub ax, [src_index]
	cmp ax, 2
	je .true
	cmp ax, 16
	je .true
	
.false:
	mov word [valid_flag], 0
	jmp .done
	
.true:
	mov word [valid_flag], 1
.done:
	pop ax
	ret

make_move:
	pusha
.m:
	call is_valid_move
	mov ax, [valid_flag]
	cmp ax, 0
	je .invalid
	
	mov bx, [src_index]
	mov si, [dst_index]
	mov word ax, [board+si]
	mov word dx, [board+bx]
	cmp ax, 6
	je .bomb_blast
	cmp dx, 6
	je .bomb_blast
	mov word [board+si], dx
	mov word [board+bx], ax
	jmp .done
	
.invalid:
	mov ax, error
	push word 1762
	push ax
	call printstr
	call delay 
	call delay
	call delay
	mov ax, error
	push word 1762
	push ax
	call clrtext	
	push word [src_index]
	call get_input
	pop word [src_index]
	push word [dst_index]
	call get_input
	pop word [dst_index]
	jmp .m
	
.bomb_blast:
	add word [score_count], 15
	push 2000
	call sound
	mov cx, 64
	mov word [board+bx], 0
	mov word [board+si], 0
	mov bx, 0
.l1:
	cmp [board+bx], ax
	jne .next
	mov word [board+bx], 0
.next:
	add bx, 2
	loop .l1

.done:
	call crush_candies
	sub word [move_count], 1
	add word [score_count], 10
	push 2500
	call sound
	popa
	ret
	

;---------DESTROY CANDIES------------------

crush_candies:
	pusha
	
	mov cx, 62
	mov bx, 0
.checkX:
	mov ax, [board+bx]
	cmp ax, [board+bx+2]
	jne .nextX
	cmp ax, [board+bx+4]
	jne .nextX
	cmp ax, [board+bx+6]
	jne .l2
	add word [score_count], 5
	mov word [board+bx+6], 0
	cmp ax, [board+bx+8]
	jne .l2
	mov word [board+bx+8], 0
	mov word [board+bx], 6	; value to identify a color bomb
	add bx, 2
.l2:
	mov si, bx
	mov word [board+si], 0
	mov word [board+si+2], 0
	mov word [board+si+4], 0
	.l21:
		cmp si, 16
		jl .nextX
		push si
		call candy_fall_X
		sub si, 16
		jmp .l21

.nextX:
	add bx, 2
	loop  .checkX
	

	
.checkY:
	mov bx, 0
	mov cx, 80
.checkloop:
	mov ax, [board+bx]
	cmp ax, [board+bx+16]
	jne .nextY
	cmp ax, [board+bx+32]
	jne .nextY
	cmp ax, [board+bx+48]
	jne .l3
	add word [score_count], 5
	mov word [board+bx+48], 0
	cmp ax, [board+bx+64]
	jne .l3
	mov word [board+bx], 6; set bomb
	add bx, 16
.l3	:
	mov si, bx
	mov word [board+si], 0
	mov word [board+si+16], 0
	mov word [board+si+32], 0
	.l31:	
	cmp si, 80
	jl .nextY
	push si
	call candy_fall_Y
	sub si, 16
	jmp .l31
.nextY:
	add bx, 2
	loop .checkloop
	
	
.crushed:
	mov si, 0
	mov cx, 64
.l4:
	cmp word [board+si], 0
	je .set_new_value
	jmp .next_val
	mov ax,0
.set_new_value:
		inc ax
		cmp ax, 6
		jl .safe
		mov ax, 1
	.safe
		cmp [board+si+2], ax
		jne .prev
		cmp [board+si+4], ax
		je .set_new_value
	.prev:
		push si
		sub si, 2 
		cmp [board+si], ax
		jne .up
		sub si, 2
		cmp [board+si], ax
		jne .up
		pop si
		jmp .set_new_value
	.up:
		pop si
		push si
		sub si, 16
		cmp [board+si], ax
		jne .down
		sub si, 16
		cmp [board+si], ax
		jne .down
		pop si
		jmp .set_new_value
	.down:
		pop si
		cmp [board+si+16], ax
		jne .set
		cmp [board+si+32], ax
		jne .set
		jmp .set_new_value				
	.set:
		mov [board+si], ax
.next_val:	
	add si, 2
	loop .l4
	
	popa
	ret
	
	
candy_fall_X:
	push bp
	mov bp, sp
	push ax
	push bx
		
	mov bx, [bp+4]
	sub bx, 16
	mov ax, [board+bx]
	add bx, 16
	mov [board+bx], ax
	sub bx, 14
	mov ax, [board+bx]
	add bx, 14	
	mov [board+bx+2], ax
	sub bx, 12	
	mov ax, [board+bx]
	add bx, 12
	mov [board+bx+4], ax
	
	pop bx
	pop ax
	pop bp
	ret 2
	
candy_fall_Y:
	push bp
	mov bp, sp
	push ax
	push bx

	mov bx, [bp+4]
	sub bx, 48
	mov ax, [board+bx]
	add bx, 48
	mov [board+bx], ax
	sub bx, 64
	mov ax, [board+bx]
	add bx, 64	
	sub bx, 16
	mov [board+bx], ax
	add bx, 16
	sub bx, 80	
	mov ax, [board+bx]
	add bx, 80
	sub bx, 32
	mov [board+bx], ax
	add bx, 32
	
	pop bx
	pop ax
	pop bp
	ret 2
	
;--------------------------INPUT CODE-----------------------------------
get_input:
	push bp
	mov bp, sp
	push ax
	push bx
	push es
	push di

	push word 642
	mov ax, str1
	push ax
	call printstr
	
row_input:
	xor ax, ax
	mov ah, 0
    int 0x16
	cmp ah, 0x1                 ; Esc key
    je near exit
    cmp al, 0x31                
    jl row_input
	cmp al, 0x38
	jg row_input
	
    sub al, 0x30
	dec al
	mov byte [row_value], al
	inc al
	mov ah, 0
	push word 662
	push ax
	call printnum
	
	push word 1282
	mov ax, str2
	push ax
	call printstr

col_input:
	xor ax, ax
	mov ah, 0
    int 0x16
	cmp ah, 0x1                 ; Esc key
    je exit
    cmp al, 0x31                
    jl col_input
	cmp al, 0x38
	jg col_input
    
	sub al, 0x30
	dec al	
	mov byte [col_value], al
	inc al
	mov ah, 0
	push word 1302
	push ax
	call printnum
	
	;---Calculate index value---
	xor ax, ax	
	mov al, [row_value]
	shl ax, 4 ;multiply by 16
	xor bx, bx
	mov bl, [col_value]
	shl bx, 1 ; word offset
	add ax, bx
	
	mov [bp+4], ax	
	
	pop di
	pop es
	pop bx
	pop ax
	pop bp
	ret
		

;--------------------------------------MAIN PROGRAM-----------------------------------------------	
start:
	call initialize_board_values
	call clrscr
	call printboard
	call print_index_values
	call print_candies
	call print_score
	
	mov cx, 21
game_loop:
	call print_candies
	call print_score
	push word [src_index]
	call get_input
	pop word [src_index]
	
	push word [dst_index]
	call get_input
	pop word [dst_index]
	
	call make_move
	
	loop game_loop	

exit:	
	mov cx, 3
.bye:
	push 2000
	call sound
	push 2500
	call sound
	push 2700
	call sound
	loop .bye
	
	mov ax, 0x4c00
	int 0x21
	

moves: db "Moves: ", 0

str1: db "Enter Row: ", 0
str2: db "Enter Col: ", 0
error: db "Wrong Move", 0
randNum: dw 0
score: db "Score: ", 0


row_value: db 0
col_value: db 0

src_index: dw 0
dst_index: dw 0
move_count: dw 20

valid_flag: dw 0
score_count: dw 0


candy: dw 1, 2, 3, 4, 5, 6
color: db 0x77, 0x22, 0x33, 0x44, 0x55, 0x66, 0x8e

board: dw 0, 0, 0, 0, 0, 0, 0, 0,
	   dw 0, 0, 0, 0, 0, 0, 0, 0,
	   dw 0, 0, 0, 0, 0, 0, 0, 0,
	   dw 0, 0, 0, 0, 0, 0, 0, 0,
	   dw 0, 0, 0, 0, 0, 0, 0, 0,
	   dw 0, 0, 0, 0, 0, 0, 0, 0,
	   dw 0, 0, 0, 0, 0, 0, 0, 0,
	   dw 0, 0, 0, 0, 0, 0, 0, 0