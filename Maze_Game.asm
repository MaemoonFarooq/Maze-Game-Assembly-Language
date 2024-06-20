;Assignment 3
;Name: Muhammad Maemoon Farooq
;roll number:21i1680
;COAL 


[org 0x0100]
 jmp start
 loc: dw 0
 bee: db 'B'
 block: db 'A'
 target: db 'T'
 tickcount: dw 00
 resultmessage: db 'Congratulations you win!!!'
 


printnum: push bp
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
nextdigit: mov dx, 0 ; zero upper half of dividend
 div bx ; divide by 10
 add dl, 0x30 ; convert digit into ascii value
 push dx ; save ascii value on stack
 inc cx ; increment count of values
 cmp ax, 0 ; is the quotient zero
 jnz nextdigit ; if no divide it again
 mov di, 140 ; point di to 70th column
nextpos: pop dx ; remove a digit from the stack
 mov dh, 0x07 ; use normal attribute
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
 ret 2
; timer interrupt service routine

timer: push ax
 inc word [cs:tickcount]; 
 push word [cs:tickcount]
 call printnum ; print tick count
 mov al, 0x20
 out 0x20, al ; end of interrupt
 pop ax
 iret 


clrscr:
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

background:
 mov word [es:di], 0x06020 ; clear next char on screen
 add di, 2 ; move to next screen location
 cmp di, 4000 ; has the whole screen cleared
 jne background ; if no clear next position
 ret 


printstr:
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
 mul byte [bp+10] ; multiply with y position
 add ax, [bp+12] ; add x position
 shl ax, 1 ; turn into byte offset
 mov di,ax ; point di to required location
 mov [loc],ax
 mov si, [bp+6] ; point si to string
 mov cx, [bp+4] ; load length of string in cx
 mov ah, [bp+8] ; load attribute in ah
 nextchar: mov al, [si] ; load next char of string
 mov [es:di], ax ; show this char on screen
 add di, 2 ; move to next screen location 
 add si, 1 ; move to next char in string
 loop nextchar ; repeat the operation cx times
 pop di
 pop si
 pop cx
 pop ax
 pop es
 pop bp
 ret 10


 
;Horizontal line subrotine
horizontalline:
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
 mul byte [bp+10] ; multiply with y position
 add ax, [bp+12] ; add x position
 shl ax, 1 ; turn into byte offset
 mov di,ax ; point di to required location
 mov si, [bp+6] ; point si to string
 mov cx, [bp+4] ; load length of string in cx
 mov al,[block]
 mov ah, [bp+8] ; load attribute in ah 

nextchar_h:
 mov [es:di], ax ; show this char on screen
 add di, 2 ; move to next screen location
 loop nextchar_h ; repeat the operation cx times
 pop di
 pop si
 pop cx
 pop ax
 pop es
 pop bp
 ret 10 

;Vertical line subrotine
verticalline:
    push bp
    mov bp, sp
    push es
    push ax
    push cx
    push si
    push di

    mov ax, 0xB800
    mov es, ax               ; Point es to video base
    mov al, 80               ; Load al with columns per row
    mul byte [bp+10]         ; Multiply with y position
    add ax, [bp+12]          ; Add x position
    shl ax, 1                ; Turn into byte offset
    mov di, ax               ; Point di to required location
    mov si, [bp+6]           ; Point si to string
    mov cx, [bp+4]           ; Load length of string in cx
    mov al, [block]
    mov ah, [bp+8]           ; Load attribute in ah

nextchar_v:
    mov [es:di], ax          ; Show this char on screen
    add di, 160              ; Move to next screen location (80 columns * 2 bytes per column)
    loop nextchar_v          ; Repeat the operation cx times

    pop di
    pop si
    pop cx
    pop ax
    pop es
    pop bp
    ret 10



;Horizontal line subrotine
targetline:
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
 mul byte [bp+10] ; multiply with y position
 add ax, [bp+12] ; add x position
 shl ax, 1 ; turn into byte offset
 mov di,ax ; point di to required location
 mov si, [bp+6] ; point si to string
 mov cx, [bp+4] ; load length of string in cx
 mov al,[target]
 mov ah, [bp+8] ; load attribute in ah 

nextchar_t:
 mov [es:di], ax ; show this char on screen
 add di, 2 ; move to next screen location
 loop nextchar_t ; repeat the operation cx times
 pop di
 pop si
 pop cx
 pop ax
 pop es
 pop bp
 ret 10 








;keyboard interrupt service routine
 kbisr: push ax
 push es
 mov ax, 0xb800
 mov es, ax ; point es to video memory
 

;read from keyboard
 in al, 0x60 ; read a char from keyboard port
 cmp al, 0x4B ; is the key left  key
 jne upkey ; no, try next comparison
 mov byte [es:0], 'L' ; yes, print L at top left

;functionality
 sub di,2
 mov ah,byte [es:di]
 add di,2
 cmp ah,[block]
 je nomatch_t  
 

mov di,[loc]
 mov byte [es:di], ' ' ; yes, print L at top left
 sub di,2
 mov [loc],di
 mov byte [es:di], 'B' ; yes, print L at top left
 
jmp nomatch ; leave interrupt routine

nomatch_t: mov al, 0x20
  out 0x20, al ; send EOI to PIC
  pop es
  pop ax
  iret 
  
upkey:
 cmp al, 0x48 ; is the key up  key
 jne downkey ; no, try next comparison
 mov byte [es:0], 'U' ; yes, print L at top left
 ;functionality
 
 sub di,160
 mov ah,byte [es:di]
 add di,160
 cmp ah,[block]
 je nomatch_t 
 mov di,[loc]
 mov byte [es:di], ' ' ; yes, print L at top left
 sub di,160
 mov [loc],di
 mov byte [es:di], 'B' ; yes, print L at top left
 jmp nomatch ; leave interrupt routine
 

downkey:
 cmp al, 0x50 ; is the key up  key
 jne rightkey ; no, try next comparison
 mov byte [es:0], 'D' ; yes, print L at top left
 ;functionality
 add di,160
 mov ah,byte [es:di]
 sub di,160
 cmp ah,[block]
 je nomatch
 cmp ah ,[target]
 je resultscr

 mov di,[loc]
 mov byte [es:di], ' ' ; yes, print L at top left
 add di,160
 mov [loc],di
 mov byte [es:di], 'B' ; yes, print L at top left



 jmp nomatch ; leave interrupt routine


rightkey: 
 cmp al, 0x4D ; is the key right key
 jne exitkey ; no, leave interrupt routine
 
;functionality
 add di,2
 mov ah,byte [es:di]
 sub di,2
 cmp ah,[block]
 je nomatch 




 mov di,[loc]
 mov byte [es:di], ' ' ; yes, print L at top left
 add di,2
 mov [loc],di
 mov byte [es:di], 'B' ; yes, print L at top left
 



 mov byte [es:0], 'R' ; yes, print R at top left
 
exitkey:
 cmp al, 0x81
 jmp nomatch ; leave interrupt routine
 
 
nomatch: mov al, 0x20
  out 0x20, al ; send EOI to PIC
  pop es
  pop ax
  iret 


resultscr:
call clrscr
 mov ax, 20
 push ax ; push x position
 mov ax, 10
 push ax ; push y position
 mov ax, 0x06020 ; blue on black attribute
 push ax ; push attribute
 mov ax, resultmessage
 push ax ; push address of message
 push word 26 ; push message length
 call printstr ; call the printstr subroutine




mov ax,0x4c00
int 0x21



start: 
 call clrscr
 mov ax, 0xb800 ; load video base in ax
 mov es, ax ; point es to video base
 mov di, 0 ; point di to top left column
 call background


 ;print the bee pixel
 mov ax, 10
 push ax ; push x position
 mov ax, 20
 push ax ; push y position
 mov ax, 0x06020 ; blue on black attribute
 push ax ; push attribute
 mov ax, bee
 push ax ; push address of message
 push word 1 ; push message length
 call printstr ; call the printstr subroutine


;topbox
 mov ax, 2
 push ax ; push x position
 mov ax, 2
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 75 ;length
 call horizontalline 


;botbox
 mov ax, 2
 push ax ; push x position
 mov ax, 22
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 75 ;length
 call horizontalline 


;leftbox
 mov ax, 2
 push ax ; push x position
 mov ax, 2
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 20 ;length
 call verticalline
 
 mov ax, 3
 push ax ; push x position
 mov ax, 2
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 20 ;length
 call verticalline




;rightbox
 mov ax, 77
 push ax ; push x position
 mov ax, 2
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 21 ;length
 call verticalline

 mov ax, 76
 push ax ; push x position
 mov ax, 2
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 21 ;length
 call verticalline
;end of the border 


;inner lines
 mov ax, 35
 push ax ; push x position
 mov ax, 17
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 34;length
 call horizontalline 

 mov ax, 15
 push ax ; push x position
 mov ax, 17
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 13;length
 call horizontalline 

 mov ax, 15
 push ax ; push x position
 mov ax, 6
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 11 ;length
 call verticalline
  mov ax, 16
 push ax ; push x position
 mov ax, 6
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 11 ;length
 call verticalline

 mov ax, 26
 push ax ; push x position
 mov ax, 3
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 5 ;length
 call verticalline
 mov ax, 25
 push ax ; push x position
 mov ax, 3
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 5 ;length
 call verticalline

 mov ax, 25
 push ax ; push x position
 mov ax, 13
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 5 ;length
 call verticalline
 mov ax, 26
 push ax ; push x position
 mov ax, 13
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 5 ;length
 call verticalline


 mov ax, 26
 push ax ; push x position
 mov ax, 13
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 40;length
 call horizontalline 

 mov ax, 64
 push ax ; push x position
 mov ax, 7
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 6 ;length
 call verticalline
 mov ax, 65
 push ax ; push x position
 mov ax, 7
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 6 ;length
 call verticalline

 mov ax, 43
 push ax ; push x position
 mov ax, 6
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 7 ;length
 call verticalline
 mov ax, 42
 push ax ; push x position
 mov ax, 6
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 7 ;length
 call verticalline

 mov ax, 53
 push ax ; push x position
 mov ax, 5
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 3 ;length
 call verticalline
 mov ax, 54
 push ax ; push x position
 mov ax, 5
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 3 ;length
 call verticalline


 mov ax, 65
 push ax ; push x position
 mov ax, 11
 push ax ; push y position
 mov ax, 20 ; blue on black attribute
 push ax ; push attribute
 mov ax, block
 push ax ; push address of message
 push 12;length
 call horizontalline
;end of innerlines ;vericallines code written twice for one line to add width



 mov ax, 66
 push ax ; push x position
 mov ax, 8
 push ax ; push y position
 mov ax, 62 ; 
 push ax ; push attribute
 mov ax, target
 push ax ; push address of message
 push 10;length
 call targetline

 mov ax, 66
 push ax ; push x position
 mov ax, 9
 push ax ; push y position
 mov ax, 62 ; 
 push ax ; push attribute
 mov ax, target
 push ax ; push address of message
 push 10;length
 call targetline


 mov ax, 66
 push ax ; push x position
 mov ax, 10
 push ax ; push y position
 mov ax, 62 ; 
 push ax ; push attribute
 mov ax, target
 push ax ; push address of message
 push 10;length
 call targetline



 xor ax, ax
 mov es, ax ; point es to IVT base
 cli ; disable interrupts
 mov word [es:9*4], kbisr ; store offset at n*4
 mov [es:9*4+2], cs ; store segment at n*4+2
 sti ; enable interrupts 


 mov word [es:8*4], timer; store offset at n*4
 mov [es:8*4+2], cs ; store segment at n*4+2
 sti 

 mov dx, start ; end of resident portion
 add dx, 15 ; round up to next para
 mov cl, 4
 shr dx, cl ; number of paras

 l1: jmp l1 ; infinite loop 

 exit:
 mov ax,0x4c00
 int 0x21