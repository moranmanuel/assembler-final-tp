
; Programa para instalar ISR en el vector de interrupciones 80h
; ejecutar el programa para instalar la interrupcion
;comandos:
;tasm int80
;tlink /t int80
; y ejecutar el programa para instalar la interrupcion
; calcula la posiicon central segun la cantidad de letras de la palabra


.8086
.model tiny		
.code
   org 100h		
start:
   jmp main		

Funcion PROC FAR
    ; Calcula la columna centrada para los recuadros
    ; Cada recuadro ocupa 5 caracteres + 1 espacio = 6 caracteres
    ; 5 recuadros × 6 = 30, pero el último no tiene espacio = 29 caracteres totales
    ; Retorna en AL la columna inicial centrada
    push bx
    push cx
    
    ; Ancho total: 5 recuadros × 6 caracteres - 1 espacio del último = 29
    mov al, 5           ; Número de recuadros
    mov bl, 6           ; Ancho por recuadro (5 caracteres + 1 espacio)
    mul bl              ; AX = 5 × 6 = 30
    sub ax, 1           ; Restar 1 espacio del último recuadro = 29
    
    ; Calcular columna centrada: (80 - 29) / 2
    mov bl, 50h         ; 80 columnas (50h)
    sub bl, al          ; 80 - 29 = 51
    mov al, bl
    shr al, 1           ; Dividir por 2 = 25 (redondeado hacia abajo)
    
    pop cx
    pop bx    
    iret
endp

DespIntXX dw 0
SegIntXX  dw 0

FinResidente LABEL BYTE		
Cartel    DB "Int80h instalado exitosamente",0dh, 0ah, '$'

main:

    mov ax,CS
    mov DS,ax
    mov ES,ax

InstalarInt:
    mov AX,3580h        
    int 21h    
         
    mov DespIntXX,BX    
    mov SegIntXX,ES

    mov AX,2580h	
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