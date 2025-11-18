.model small
.stack

.data
tempTarget          db 5 dup (0)
rw_base_column      db 0
rw_base_row         db 0
rw_letter_attr      db 0
render_base_column  db 0
big_letter_attr     db 0        ; Atributo temporal para letras grandes
box_char_top_l      db 218      ; ┌ (esquina superior izquierda redondeada)
box_char_top_r      db 191      ; ┐ (esquina superior derecha redondeada)
box_char_bot_l      db 192      ; └ (esquina inferior izquierda redondeada)
box_char_bot_r      db 217      ; ┘ (esquina inferior derecha redondeada)
box_char_horiz      db 196      ; ─ (línea horizontal simple)
box_char_vert       db 179      ; │ (línea vertical simple)
dataDiv             db 10, 1
general             db 'general.txt',0 ;los txt tienen que tener un espacio al principio y al final
paises              db 'paises.txt',0
comidas             db 'comidas.txt',0
bytesRead           dw 0
buffer              db 1000 dup(0) ;guarda el txt


; Arte ASCII para letras grandes estilo contorno (5x11 cada una, 55 bytes)
; Usando _, -, | para crear solo los bordes/contornos
; W - 11 caracteres de ancho     
bigW db 177,177,' ',' ',' ',' ',' ',177,177,' ',' ',177,177,177,177,' ',' ',177,177,177,177,177,177,' ',' ',177,177,177,177,' ',' ',' ',' ',177,177,' ',' ',' ',177,177,' ',' ',' ',' ',177,177,' ',' ',177,177,0dh,0ah 
     db 177,177,' ',' ',' ',' ',' ',177,177,' ',177,177,' ',' ',177,177,' ',177,177,' ',' ',' ',177,177,' ',177,177,' ',' ',177,177,' ',' ',177,177,' ',' ',' ',' ',177,177,' ',' ',177,177,' ',' ',' ',177,177,0dh,0ah 
     db 177,177,' ',' ',177,' ',' ',177,177,' ',177,177,' ',' ',177,177,' ',177,177,177,177,177,177,' ',' ',177,177,' ',' ',' ',177,177,' ',177,177,' ',' ',' ',' ',' ',177,177,177,177,' ',' ',' ',' ',177,177,0dh,0ah 
     db 177,177,' ',177,' ',177,' ',177,177,' ',177,177,' ',' ',177,177,' ',177,177,' ',' ',' ',177,177,' ',177,177,' ',' ',177,177,' ',' ',177,177,' ',' ',' ',' ',' ',' ',177,177,' ',' ',' ',' ',' ',' ',' ',0dh,0ah 
     db ' ',177,177,' ',' ',' ',177,177,' ',' ',' ',177,177,177,177,' ',' ',177,177,' ',' ',' ',177,177,' ',177,177,177,177,' ',' ',' ',' ',177,177,177,177,177,' ',' ',' ',177,177,' ',' ',' ',' ',' ',177,177,0dh,0ah 

.code

GREEN_ATTR  EQU 2Fh
YELLOW_ATTR EQU 6Fh
BASE_ATTR   EQU 0Fh

public ClearStringAt
public SetVideoModeText
public PrintDollarStringAt
public PrintCenteredDollarString
public ClearCenteredDollarString
public DrawGuessSlots
public ReadWord
public EvaluateGuess
public RenderGuessRow
public DrawBigText
public r2a
public PickRandomWord
public general
public paises
public comidas
public drawRedFooter


drawRedFooter proc
    ; Guarda los registros que se van a utilizar
    push cx
    push dx
    push ax
    push es
    push di

    ; Cargar la dirección base de la VRAM para modo texto B800h
    mov ax, 0b800h
    mov es, ax          ; es ahora apunta al segmento de memoria de video

    mov dx, 20          ; dx = fila actual (empieza en la fila 20)

bucle_fila:
    ; Calcular el offset inicial de la fila: Fila * Ancho * 2 bytes/carácter
    ; Offset = dx * 160
    mov ax, dx          ; ax = Fila actual
    mov bl, 2           ; Multiplicar por 2 (carácter + atributo)
    mul bl              ; ax = Fila * 2
    mov bl, 80          ; bl = Ancho de pantalla
    mul bl              ; ax = Offset lineal de la fila
    mov di, ax          ; di = Offset en es:[di]

    mov cx, 80          ; cx = contador de columnas (80 iteraciones por fila)

bucle_columna:
    ; El primer byte es el caracter (lo dejamos como espacio ' ')
    mov al, ' '         
    mov es:[di], al     
    inc di              

    ; Escribir el atributo de color rojo
    mov al, 4fh         
    mov es:[di], al     
    inc di              

    loop bucle_columna  ; Repite para las 80 columnas

    inc dx              ; Siguiente fila
    
    cmp dx, 25          
    jl bucle_fila       ; Si no, continuamos con la siguiente fila

fin_dibujo_texto:
    ; Restaura los registros originales antes de salir
    pop di
    pop es
    pop ax
    pop dx
    pop cx
    ret                 ; Retorna de la función

drawRedFooter endp


readFile proc near

    push ax
    push bx
    push cx
    push dx
    push di
    push si

    ;recibe en dx offset filename

    ; --- Abrir archivo para lectura ---
    mov ah, 3Dh      ; Función DOS: abrir archivo
    mov al, 0        ; Modo: lectura
    int 21h
    jc  file_error   
    mov bx, ax       ; BX = handle del archivo

    ; --- Leer archivo ---
    mov ah, 3Fh      ; Función DOS: leer archivo
    mov cx, 1000      ; Cantidad de bytes a leer
    lea dx, buffer   ; Dirección del buffer
    int 21h
    jc file_error
    mov bytesRead, ax ; Guardar cantidad de bytes leídos
    mov si, ax
    mov buffer[si], 0

    ; --- Cerrar archivo ---
    mov ah, 3Eh      ; Función DOS: cerrar archivo
    int 21h

file_error:
    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret
    ; Manejo de error
readFile endp

PickRandomWord proc near
    ; Entrada: BX = offset de la categoría (general, paises, o comidas)
    ;          DI = offset de targetWord
    ;          SI = offset de targetWordDisplay
    ; Salida:  CX = cantidad de caracteres de la palabra
    push ax
    push dx
    push di
    push si

    ;limpiar buffer

    mov dx, bx
    call readFile

    ;ahora las palabras estan guardadas en buffer
    ;funcion que cuenta las palabras
    mov si, offset buffer
    call CountWords
    

    mov bx, ax
    call Random1ToN

    ;en ax tengo el num del 1 al 30 (cada categoría tiene 30 palabras)
    ;bucle que recorre palabras y cada vez q encuentra un espacio aumenta en 1
    ;recibe en di offset palabra a adivinar
    ;recibe en si offset variable targetWordDisplay
    ;devuelve en cx cantidad de caracteres de la palabra
    
    mov bx, offset buffer      ; Usar la categoría pasada como parámetro
    mov cx, 0
    pop si
    pop di
recorrerWords:
    cmp byte ptr[bx], 20h
    je isSpace
    inc bx
    jmp recorrerWords
    isSpace:
    inc cx
    cmp cx, ax
    je isWord
    inc bx
    jmp recorrerWords

isWord: ;copiar la palabra en la variable de destino
    mov cx, 0
isWordLoop:
    inc bx
    cmp byte ptr[bx], 20h
    je endCopy
    mov al, [bx]    ;muevo a al el caracter
    mov [di], al  ;lo guardo en la variable   
    mov [si], al
    inc cx
    inc di
    inc si
    jmp isWordLoop

endCopy:

    pop dx
    pop ax
    ret

PickRandomWord endp

CountWords proc near
    push bx
    push cx
    ;pasar por si offset cadena
    ;devuelve natidad de palabras en ax
    ;las palabras tienen q estar separadas por un espacio y el primer caracter tiene que ser un espacio tambien
    xor ax, ax        ; AX = contador de palabras
    mov bx, si        ; BX = puntero a cadena
CountLoopp:
    mov cl, [bx]      ; leer un caracter
    cmp cl, 0         ; si es fin de cadena (NULL), salir
    je Done
    cmp cl, ' '       ; si es un espacio, aumentar contador
    jne NextCharr
    inc ax            ; contar palabra
NextCharr:
    inc bx
    jmp CountLoopp
Done:

    dec ax ;cuenta una de mas porque al final de la cadena hay un espacio
    pop cx
    pop bx
    ret
CountWords endp

Random1ToN proc
    ;pasar en bx max num
    ;devuelve en ax num random
    push dx
    push cx

    ; --- Semilla simple: segundos del reloj ---
    mov ah, 2Ch
    int 21h
    mov al, dl        ; usar segundos (0..59)
    mov ah, 0
    mov dx, ax        ; DX = semilla

    ; --- Generar pseudoaleatorio simple ---
    ; AX = (semilla * 3 + 7) mod 65536
    mov ax, dx
    mov cx, 3
    mul cx            ; AX * 3
    add ax, 7         ; sumamos constante
    ; ahora AX = pseudoaleatorio 0..65535

    ; --- Ajustar al rango 1..N ---
    mov cx, bx
    xor dx, dx
    div cx            ; AX / N -> DX = residuo 0..N-1
    mov ax, dx
    inc ax            ; 1..N

    pop cx
    pop dx
    ret
Random1ToN endp

ClearCenteredDollarString proc near
    push ax
    push bx
    push cx
    push dx
    push si

    ; Guardar BH y AH en DL/DH
    mov dh, ah
    mov dl, bh

    ; Guardar puntero original en BX y contar longitud en CX
    mov bx, si       ; BX <- puntero original
    xor cx, cx

CountLooop:
    mov al, [si]     ; leer byte en SI sin usar LODSB (más controlable)
    cmp al, '$'
    je CountDone
    inc cx
    inc si
    jmp CountLooop

CountDone:
    mov si, bx       ; restaurar puntero original en SI

    ; Calcular columna inicial: (80 - longitud) / 2
    mov ax, 50h      ; 0x50 = 80 columnas
    sub ax, cx
    shr ax, 1
    mov bl, al       ; BL = columna inicial

    ; Restaurar BH y AH desde DL/DH
    mov bh, dl
    mov ah, dh

    ; Llamar a la rutina que imprime espacios por cada carácter ($-terminated)
    call ClearStringAt

    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
ClearCenteredDollarString endp


ClearStringAt proc near
    ; entrada:
    ;  SI = dirección del string con '$'
    ;  BH = fila
    ;  BL = columna
    ;  AH = atributo
NextClear:
    lodsb
    cmp al, '$'
    je EndClear
    mov al, ' '        ; convertir cualquier carácter en espacio
    call WriteCharAttr
    inc bl
    jmp NextClear
EndClear:
    ret
ClearStringAt endp

SetVideoModeText proc near
    mov ax, 0003h
    int 10h
    ret
SetVideoModeText endp

WriteCharAttr proc near
    push bx
    push cx
    push dx
    push di

    mov dx, ax
    mov al, bh
    xor ah, ah
    mov cl, 50h          ; 80 decimal
    mul cl
    mov di, ax
    mov al, bl
    xor ah, ah
    add di, ax
    shl di, 1
    mov ax, 0B800h
    mov es, ax
    mov ax, dx
    mov es:[di], ax

    pop di
    pop dx
    pop cx
    pop bx
    ret
WriteCharAttr endp

PrintDollarStringAt proc near
    push ax
    push si

NextChar:
    lodsb
    cmp al, '$'
    je EndPrint
    call WriteCharAttr
    inc bl
    jmp NextChar

EndPrint:
    pop si
    pop ax
    ret
PrintDollarStringAt endp

PrintCenteredDollarString proc near
    ; Entrada: SI = puntero a string, BH = fila, AH = atributo
    push ax
    push bx
    push cx
    push dx
    push si

    mov dh, ah          ; Guardar AH (atributo) en DH
    mov dl, bh          ; Guardar BH (fila) en DL
    mov bx, si          ; BX = puntero al string para contar
    xor cx, cx

CountLoop:
    lodsb
    cmp al, '$'
    je LengthReady
    inc cx
    jmp CountLoop

LengthReady:
    mov si, bx          ; Restaurar puntero al string
    mov ax, 50h
    sub ax, cx
    shr ax, 1
    mov bl, al          ; BL = columna centrada
    mov bh, dl          ; Restaurar BH (fila) desde DL
    mov ah, dh          ; Restaurar AH (atributo) desde DH
    call PrintDollarStringAt

    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
PrintCenteredDollarString endp

DrawBox proc near
    ; Dibuja un recuadro con líneas dobles
    ; Entrada: BH = fila, BL = columna, AL = letra (o '_' si vacío), AH = atributo
    push ax
    push bx
    push cx
    push dx

    mov dl, al          ; Guardar letra
    mov dh, ah          ; Guardar atributo
    mov cl, bl          ; Guardar columna base

    ; Línea superior: ╔═══╗ (5 caracteres: ╔ + 3 ═ + ╗)
    mov al, [box_char_top_l]
    mov ah, dh
    call WriteCharAttr
    inc bl
    mov al, [box_char_horiz]
    call WriteCharAttr
    inc bl
    mov al, [box_char_horiz]
    call WriteCharAttr
    inc bl
    mov al, [box_char_horiz]
    call WriteCharAttr
    inc bl
    mov al, [box_char_top_r]
    call WriteCharAttr

    ; Línea media: ║ L ║ (centrada: espacio, letra, espacio)
    inc bh
    mov bl, cl          ; Restaurar columna base
    mov al, [box_char_vert]
    call WriteCharAttr
    inc bl
    mov al, ' '         ; Espacio antes de la letra
    call WriteCharAttr
    inc bl
    mov al, dl          ; Letra o '_' (centrada)
    call WriteCharAttr
    inc bl
    mov al, ' '         ; Espacio después de la letra
    call WriteCharAttr
    inc bl
    mov al, [box_char_vert]
    call WriteCharAttr

    ; Línea inferior: ╚═══╝ (5 caracteres: ╚ + 3 ═ + ╝)
    inc bh
    mov bl, cl          ; Restaurar columna base
    mov al, [box_char_bot_l]
    call WriteCharAttr
    inc bl
    mov al, [box_char_horiz]
    call WriteCharAttr
    inc bl
    mov al, [box_char_horiz]
    call WriteCharAttr
    inc bl
    mov al, [box_char_horiz]
    call WriteCharAttr
    inc bl
    mov al, [box_char_bot_r]
    call WriteCharAttr

    pop dx
    pop cx
    pop bx
    pop ax
    ret
DrawBox endp

DrawGuessSlots proc near
    ; Entrada: BH = fila, BL = columna, AH = atributo
    push ax
    push bx
    push cx
    push dx

    mov [render_base_column], bl
    mov cx, 5
    mov dh, bh          ; Guardar fila base
    mov dl, ah          ; Guardar atributo

DrawLoop:
    mov bh, dh
    mov al, ' '
    mov ah, dl          ; Usar atributo pasado como parámetro
    call DrawBox

    ; Avanzar a la siguiente posición (cada recuadro ocupa 5 columnas + 1 espacio)
    mov bl, [render_base_column]
    add bl, 6
    mov [render_base_column], bl
    loop DrawLoop

    pop dx
    pop cx
    pop bx
    pop ax
    ret
DrawGuessSlots endp

ReadWord proc near
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    mov [rw_base_column], dl
    mov [rw_letter_attr], ah
    mov [rw_base_row], dh
    mov di, bx

    mov cx, 5
    mov si, di
    xor al, al

ClearBuffer:
    mov [si], al
    inc si
    loop ClearBuffer

    xor cx, cx

ReadLoop:
    xor ah, ah
    int 16h
    cmp al, 08h
    je HandleBackspace
    cmp al, 0Dh
    je ReadLoop
    cmp al, ' '
    jnb ReadLoopContinue1
    jmp InvalidKey
ReadLoopContinue1:
    cmp cx, 5
    jae ReadLoop

    cmp al, 'a'
    jb CheckUpper
    cmp al, 'z'
    ja CheckUpper
    sub al, 20h

CheckUpper:
    cmp al, 'A'
    jnb CheckUpperValidLow
    jmp InvalidKey
CheckUpperValidLow:
    cmp al, 'Z'
    jbe CheckUpperDone
    jmp InvalidKey
CheckUpperDone:

    mov si, di
    add si, cx
    mov [si], al

    ; Calcular posición del recuadro (cada recuadro ocupa 6 columnas: 5 del recuadro + 1 espacio)
    mov dl, al          ; Guardar letra
    mov al, cl
    xor ah, ah          ; Limpiar AH para multiplicación
    mov bl, 6
    mul bl              ; Multiplicar índice por 6 (resultado en AX)
    mov bl, [rw_base_column]
    add bl, al
    mov bh, [rw_base_row]
    mov al, dl          ; Restaurar letra
    mov ah, [rw_letter_attr]
    call DrawBox

    inc cx
    cmp cx, 5
    je ReadDone
    jmp ReadLoop

HandleBackspace:
    cmp cx, 0
    je ReadLoop
    dec cx
    mov si, di
    add si, cx
    mov byte ptr [si], 0

    ; Calcular posición del recuadro y dibujar recuadro vacío
    mov al, cl
    xor ah, ah          ; Limpiar AH para multiplicación
    mov bl, 6
    mul bl              ; Multiplicar índice por 6 (resultado en AX)
    mov bl, [rw_base_column]
    add bl, al
    mov bh, [rw_base_row]
    mov al, ' '
    mov ah, [rw_letter_attr]
    call DrawBox
    jmp ReadLoop

InvalidKey:
    jmp ReadLoop

ReadDone:
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
ReadWord endp

EvaluateGuess proc near
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    mov bp, bx
    mov dx, si

    mov si, di
    mov di, offset tempTarget
    mov cx, 5
CopyTarget:
    mov al, [si]
    mov [di], al
    inc si
    inc di
    loop CopyTarget

    mov cx, 5
    mov di, bp
    xor al, al
ZeroStatuses:
    mov [di], al
    inc di
    loop ZeroStatuses

    mov si, dx
    mov di, offset tempTarget
    mov bx, bp
    mov cx, 5
FirstPass:
    mov al, [si]
    mov ah, [di]
    cmp al, ah
    jne FirstNext
    mov byte ptr [bx], 2
    mov byte ptr [di], 0
FirstNext:
    inc si
    inc di
    inc bx
    loop FirstPass

    mov si, dx
    mov bx, bp
    mov cx, 5
SecondPass:
    mov al, [si]
    cmp byte ptr [bx], 0
    jne SkipSecond
    mov di, offset tempTarget
    mov dx, 5
SearchLoop:
    cmp al, [di]
    jne ContinueSearch
    mov byte ptr [bx], 1
    mov byte ptr [di], 0
    jmp FoundLetter
ContinueSearch:
    inc di
    dec dx
    jnz SearchLoop
FoundLetter:
SkipSecond:
    inc si
    inc bx
    loop SecondPass

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
EvaluateGuess endp

RenderGuessRow proc near
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    mov [render_base_column], dl
    mov cx, 5

RenderLoop:
    mov al, [si]
    mov dl, [di]
    mov ah, BASE_ATTR
    cmp dl, 2
    je UseGreen
    cmp dl, 1
    je UseYellow
    jmp AttrReady

UseGreen:
    mov ah, GREEN_ATTR
    jmp AttrReady

UseYellow:
    mov ah, YELLOW_ATTR

AttrReady:
    mov bl, [render_base_column]
    mov bh, dh
    call DrawBox

    ; Avanzar a la siguiente posición (cada recuadro ocupa 5 columnas + 1 espacio)
    mov bl, [render_base_column]
    add bl, 6
    mov [render_base_column], bl

    inc si
    inc di
    loop RenderLoop

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
RenderGuessRow endp

DrawBigText proc near
    ; Dibuja el título completo desde bigW de una vez
    ; bigW contiene el título completo con 0dh,0ah al final de cada fila
    ; Entrada: BH = fila base, AH = atributo
    push ax
    push bx
    push cx
    push dx
    push si
    
    mov dh, ah          ; Guardar atributo en DH
    mov dl, bh          ; Guardar fila base en DL
    lea si, bigW        ; SI apunta al inicio del título
    
    mov ch, 5           ; 5 filas en total
DrawTitleRow:
    push cx             ; Guardar contador de filas
    mov bx, si          ; BX = inicio de fila (para restaurar después)
    
    ; Contar caracteres en esta fila (hasta encontrar 0dh o 0ah)
    xor cx, cx          ; Contador de caracteres (limpiar CX)
CountRowChars:
    lodsb               ; Leer carácter
    cmp al, 0Dh         ; ¿Es retorno de carro?
    je RowLengthFound
    cmp al, 0Ah         ; ¿Es nueva línea?
    je RowLengthFound
    inc cx
    jmp CountRowChars
    
RowLengthFound:
    mov si, bx          ; Restaurar SI al inicio de la fila
    
    ; Calcular columna centrada para esta fila
    mov ax, 80          ; Ancho de pantalla
    sub ax, cx          ; 80 - longitud de fila
    shr ax, 1           ; Dividir por 2
    mov bl, al          ; BL = columna inicial centrada
    mov bh, dl          ; BH = fila actual
    
    ; Dibujar la fila completa
    mov ah, dh          ; Restaurar atributo
DrawRowChar:
    lodsb               ; Leer carácter
    cmp al, 0Dh         ; ¿Es retorno de carro?
    je SkipNewline
    cmp al, 0Ah         ; ¿Es nueva línea?
    je RowDone
    call WriteCharAttr
    inc bl
    jmp DrawRowChar
    
SkipNewline:
    lodsb               ; Saltar el 0Ah también (SI ahora apunta a la siguiente fila)
RowDone:
    inc dl              ; Siguiente fila
    pop cx              ; Restaurar contador de filas
    ; SI ya apunta al inicio de la siguiente fila (después del 0dh,0ah)
    dec ch
    jnz DrawTitleRow
    
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
DrawBigText endp

DrawBigLetter proc near
    ; Dibuja una letra grande de 5x11
    ; Entrada: SI = puntero al patrón de la letra (55 bytes: 5 filas × 11 columnas)
    ;          BL = columna base, BH = fila base, AH = atributo
    push ax
    push bx
    push cx
    push dx
    push si
    
    mov dl, bl          ; Guardar columna base en DL
    mov dh, bh          ; Guardar fila base en DH
    mov [big_letter_attr], ah  ; Guardar atributo en variable
    
    mov ch, 5           ; Contador de filas (5 filas)
DrawLetterRow:
    push cx             ; Guardar contador de filas
    mov bl, dl          ; Restaurar columna base
    mov bh, dh          ; Restaurar fila base
    mov cl, 11          ; Contador de columnas (11 columnas)
DrawLetterCol:
    lodsb               ; Cargar carácter del patrón
    mov ah, [big_letter_attr]  ; Restaurar atributo desde variable
    call WriteCharAttr
    inc bl
    dec cl
    jnz DrawLetterCol
    inc dh              ; Siguiente fila
    pop cx              ; Restaurar contador de filas
    dec ch
    jnz DrawLetterRow
    
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
DrawBigLetter endp

r2a proc
    ;recibe en al el numero a convertir
    ;recibe en dx el offset de la variable en la que escribe el ascii
    ;en dx queda guardado el offset de la variable para despues llamar al serivio 9
    push ax
    push cx
    push bx
    push dx
    push si

    ;limpiar variable ascii
    mov cx, 2
    mov bx, dx
limpiar:
    mov byte ptr[bx], 30h
    inc bx
    loop limpiar


    mov ah, 0
    mov cx, 2
    mov bx, dx
    mov si, 0
reg2ascc:
    mov dl, dataDiv[si]
    div dl
    add [bx], al
    mov al, ah
    mov ah, 0
    inc bx
    inc si
loop reg2ascc

    pop si
    pop dx
    pop bx
    pop cx
    pop ax
    


ret
r2a endp

end


