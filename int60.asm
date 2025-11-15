
; Programa para instalar ISR en el vector de interrupciones 60h
;comandos:
;tasm int60
;tlink /t int60
; y ejecutar el programa para instalar la interrupcion

; limpia la pantalla

.8086
.model tiny		
.code
   org 100h		
start:
   jmp main		

Funcion PROC FAR
    push bx
    push cx
    push dx

    mov bh, al
    mov ax, 0600h
    xor cx, cx
    mov dx, 184Fh
    int 10h

    pop dx
    pop cx
    pop bx   
    iret
endp

DespIntXX dw 0
SegIntXX  dw 0

FinResidente LABEL BYTE		
Cartel    DB "Int60h instalado exitosamente",0dh, 0ah, '$'

main:

    mov ax,CS
    mov DS,ax
    mov ES,ax

InstalarInt:
    mov AX,3560h        
    int 21h    
         
    mov DespIntXX,BX    
    mov SegIntXX,ES

    mov AX,2560h	
    mov DX,Offset Funcion 
    int 21h

MostrarCartel:
    mov dx, offset Cartel
    mov ah,9
    int 21h

DejarResidente:		
    Mov     AX,(15+offset FinResidente) 
    Shr     AX,1            
    Shr     AX,1        
    Shr     AX,1
    Shr     AX,1	
    Mov     DX,AX           
    Mov     AX,3100h    
    Int     21h         
end start