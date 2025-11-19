.model small
.stack 100h

GREEN_ATTR       EQU 2Fh
RED_ATTR         EQU 4Fh
MAX_ATTEMPTS     EQU 15
WORD_LEN         EQU 9
PROMPT_ROW       EQU 3        ; Movido más arriba para dar más espacio
INPUT_ROW        EQU 5        ; Movido más abajo para dar más espacio al historial
HISTORY_BASE_ROW EQU INPUT_ROW + 3       ; Fila más baja del historial (palabras nuevas aquí)
PROMPT_HINT_ROW  EQU PROMPT_ROW + 2

;int 80h -> calcular columna central (CalculateCenteredColumn) (en al -> largo de la palabra)
;int 60h -> limpiar pantalla (ClearScreenAttr) (al -> 07h)

extrn SetVideoModeText:near
extrn PrintDollarStringAt:near
extrn PrintCenteredDollarString:near
extrn DrawGuessSlots:near
extrn ReadWord:near
extrn EvaluateGuess:near
extrn RenderGuessRow:near
extrn DrawBigText:near
extrn r2a:near
extrn ClearStringAt:near
extrn ClearCenteredDollarString:near
extrn PickRandomWord:near
extrn drawFooter:near
extrn cleanVar:near
extrn clearTemp:near
extrn general:byte
extrn lugares:byte
extrn comidas:byte

.data
welcomeTitle        db 'Wordly$'
welcomePrompt       db 'Selecciona la categoria que quieras jugar:$'
continuePrompt      db 'Presiona Enter para continuar$'
categoryText        db 'Categoria:         $' 
categoriesTable     dw categoryLugares, categoryComidas, categoryGeneral
attemptsPrompt      db 'Intentos restantes: $' 
promptText          db 'Ingresa tu palabra para adivinar la escondida:$'
promptHint          db '         $'
successMsg          db 'Felicitaciones! Adivinaste la palabra.$'
failMsg             db 'No acertaste. La palabra era: $'
categoryLugares      db 'Lugares$', 0
categoryComidas     db 'Comidas$', 0
categoryGeneral     db 'General$', 0
arrow               db '->$'
spaceArrow          db '  $'
targetWord          db 10 dup (24h)
targetWordDisplay   db 10 dup (24h)
guessBuffer         db WORD_LEN dup (0)
statusBuffer        db WORD_LEN dup (0)
historyWords        db MAX_ATTEMPTS * WORD_LEN dup (0)
historyStatuses     db MAX_ATTEMPTS * WORD_LEN dup (0)
attemptsLeft        db '00$'
attemptCount        db 0
selectedCategory    db 0              ; 0=Lugares, 1=Comidas, 2=General
categoryOffset      dw 0              ; Offset de la categoría seleccionada
wordLen             db 0

.code
start:
    mov ax, @data
    mov ds, ax
    cld

    call SetVideoModeText
    mov al, 07h
    int 60h

WelcomeMenu:
    mov al, 07h
    int 60h
    ;aca habria q limpiar las variables
    ; Mostrar pantalla de bienvenida con título grande
    mov bh, 8               ; Fila para el título grande (5 filas de altura)
    mov ah, 0Eh             ; Atributo: amarillo sobre negro
    call DrawBigText

    lea si, welcomePrompt
    mov bh, 14              ; Dos filas más abajo del título
    mov ah, 0Fh
    call PrintCenteredDollarString

    ; Inicializar categoría seleccionada (0 = Lugares)
    mov byte ptr [selectedCategory], 0

CategoryMenu:
    ; Dibujar el menú de categorías
    call DrawCategoryMenu
    
CategoryMenuLoop:
    ; Leer tecla del teclado
    xor ah, ah
    int 16h
    
    ; Verificar si es Enter
    cmp al, 0Dh
    je CategorySelected
    
    ; Verificar si es flecha arriba (código 48h en AH)
    cmp ah, 48h
    je MoveUp
    
    ; Verificar si es flecha abajo (código 50h en AH)
    cmp ah, 50h
    je MoveDown
    
    ; Si no es ninguna tecla válida, volver a leer
    jmp CategoryMenuLoop

MoveUp:
    ; Mover hacia arriba (decrementar selectedCategory)
    cmp byte ptr [selectedCategory], 0
    je CategoryMenuLoop     ; Ya está en la primera categoría
    dec byte ptr [selectedCategory]
    call DrawCategoryMenu
    jmp CategoryMenuLoop

MoveDown:
    ; Mover hacia abajo (incrementar selectedCategory)
    cmp byte ptr [selectedCategory], 2
    je CategoryMenuLoop     ; Ya está en la última categoría
    inc byte ptr [selectedCategory]
    call DrawCategoryMenu
    jmp CategoryMenuLoop

CategorySelected:
    ; Establecer el offset de la categoría seleccionada
    mov al, [selectedCategory]
    cmp al, 0
    je SetLugares
    cmp al, 1
    je SetComidas
    jmp SetGeneral

SetLugares:
    lea bx, lugares
    mov [categoryOffset], bx
    jmp StartGame

SetComidas:
    lea bx, comidas
    mov [categoryOffset], bx
    jmp StartGame

SetGeneral:
    lea bx, general
    mov [categoryOffset], bx
    jmp StartGame

StartGame:
    ; Limpiar pantalla y comenzar el juego
    mov al, 07h
    int 60h

    ; Imprimo categoria:
    lea si, categoryText
    mov bh, 1
    mov ah, 0Fh
    call PrintCenteredDollarString

    ; Imprimo la categoria del juego actual:
    xor bx, bx
    mov bl, selectedCategory
    shl bx, 1                       ; BX = BX * 2 porque la tabla de categorias es un dw
    mov si, [categoriesTable + bx]  ; SI = puntero a la categoria
    mov bh, 1
    mov bl, 41
    mov ah, 0Fh
    call PrintDollarStringAt

    lea si, promptText
    mov bh, PROMPT_ROW
    mov ah, 0Fh
    call PrintCenteredDollarString

    lea si, promptHint
    mov bh, PROMPT_HINT_ROW
    mov ah, 0Fh
    call PrintCenteredDollarString

    ; Seleccionar palabra aleatoria de la categoría elegida
    mov di, offset targetWord
    mov si, offset targetWordDisplay
    mov bx, [categoryOffset]
    call PickRandomWord
    ;guardo largo de la palabra
    mov wordLen, cl
    xor ch, ch
GameLoop:
    ;calcular intentos restantes
    mov al, MAX_ATTEMPTS
    sub al, attemptCount
    mov dx, offset attemptsLeft
    call r2a
    ;imprimir contador de intentos restantes
    lea si, attemptsPrompt
    mov bh, 2
    mov ah, 0Fh
    call PrintCenteredDollarString

    lea si, attemptsLeft
    mov bh, 2
    mov bl, 50
    mov ah, 0Fh
    call PrintDollarStringAt

    mov al, wordLen
    int 80h
    mov bl, al              ; Columna centrada en BL
    mov bh, INPUT_ROW
    mov ah, 0Fh
    mov cl, wordLen
    call DrawGuessSlots     ;cambiar pasar largo de palabra

    mov al, wordLen
    int 80h
    mov dl, al              ; Columna centrada en DL
    lea bx, guessBuffer
    mov dh, INPUT_ROW
    mov ah, 1Fh
    mov cl, wordLen
    xor ch, ch
    call ReadWord       ;cambiar pasar wordLen

    lea si, guessBuffer
    lea di, targetWord
    lea bx, statusBuffer
    mov cl, wordLen
    xor ch, ch
    call EvaluateGuess

    mov al, attemptCount
    xor ah, ah
    mov bl, wordLen
    mul bl
    mov dx, ax

    push ds
    pop es
    lea si, guessBuffer
    lea di, historyWords
    add di, dx
    mov cl, wordLen
    xor ch, ch
    rep movsb

    lea si, statusBuffer
    lea di, historyStatuses
    add di, dx
    mov cl, wordLen
    xor ch, ch
    rep movsb

    ; Redibujar todo el historial para simular el scroll
    ; La palabra más nueva (attemptCount) va en la fila más baja
    ; Las palabras anteriores van 3 filas más arriba cada una
    mov cl, attemptCount
    xor ch, ch          ; Limpiar CH para tener CX = attemptCount
    inc cx              ; Incluir la palabra que acabamos de agregar
    xor bx, bx          ; BX será el índice del historial (0 = más nueva)
    
RenderHistoryLoop:
    push cx
    push bx
    
    ; Calcular offset en el historial (las más nuevas están al final)
    mov al, attemptCount
    sub al, bl          ; attemptCount - índice = posición en el historial
    xor ah, ah
    push bx             ; Guardar índice
    mov bl, wordLen
    mul bl
    mov dx, ax
    pop bx              ; Restaurar índice
    
    ; Obtener la palabra y estado del historial
    lea si, historyWords
    add si, dx
    lea di, historyStatuses
    add di, dx
    
    ; Calcular la fila: la más nueva (índice 0) va en HISTORY_BASE_ROW
    ; Las anteriores van 3 filas más arriba
    mov al, bl          ; bl contiene el índice (0 = más nueva)
    push bx             ; Guardar índice
    mov bl, 3
    mul bl              ; Cada recuadro ocupa 3 filas
    mov dh, HISTORY_BASE_ROW
    add dh, al          ; Restar para que las nuevas estén abajo
    pop bx              ; Restaurar índice
    
    mov al, wordLen
    int 80h
    mov dl, al
    mov cl, wordLen       ;cambiar
    xor ch, ch
    call RenderGuessRow
    
    pop bx
    pop cx
    inc bx              ; Siguiente palabra (más vieja)
    loop RenderHistoryLoop

    mov cl, wordLen
    xor ch, ch
    lea si, statusBuffer
CheckWinLoop:
    cmp byte ptr [si], 2
    jne NotWin
    inc si
    loop CheckWinLoop
    jmp HandleWin

NotWin:
    inc attemptCount
    cmp attemptCount, MAX_ATTEMPTS
    jae GameOver            ; Si attemptCount >= MAX_ATTEMPTS, ir a GameOver
    mov ax, offset GameLoop
    jmp ax                  ; Salto indirecto para evitar "out of range"

GameOver:
    mov al, RED_ATTR
    call drawFooter

    lea si, failMsg
    mov bh, 21
    mov ah, 0Fh
    call PrintCenteredDollarString

    lea si, targetWord
    mov bh, 22
    mov ah, 2Fh
    call PrintCenteredDollarString

    lea si, continuePrompt
    mov bh, 23
    mov ah, 0Fh
    call PrintCenteredDollarString

    ;limpio prompt
    lea si, promptText
    mov bh, PROMPT_ROW
    mov ah, 07h
    call ClearCenteredDollarString

    ;limpio la cadena de intentos restantes
    mov bh, 2
    mov ah, 07h
    lea si, attemptsPrompt
    call ClearCenteredDollarString

    mov bh, 2
    mov bl, 50
    mov ah, 07h
    lea si, attemptsLeft
    call ClearStringAt

    ;limpio la cadena de categorias
    mov bh, 1
    mov ah, 07h
    lea si, categoryText
    call ClearCenteredDollarString

    ; --- Resetear estado del juego ---
    ; Poner attemptCount = 0
    mov byte ptr [attemptCount], 0

    ;resetear historial
    push ds
    pop es

    ; Borrar historyWords 
    lea di, historyWords
    mov cx, MAX_ATTEMPTS * WORD_LEN
    xor al, al
    rep stosb

    ; Borrar historyStatuses 
    lea di, historyStatuses
    mov cx, MAX_ATTEMPTS * WORD_LEN
    xor al, al
    rep stosb

    ; Limpiar buffer de entrada (guessBuffer)
    push ds
    pop es
    lea di, guessBuffer
    mov cx, WORD_LEN
    xor al, al
    rep stosb

    ; Limpiar statusBuffer (por las dudas)
    lea di, statusBuffer
    mov cx, WORD_LEN
    xor al, al
    rep stosb

    ;limpio vars
    mov al, 24h
    mov bx, offset targetWord
    call cleanVar

    call clearTemp

    WaitForEnteer:
    xor ah, ah
    int 16h                 ; Leer tecla del teclado
    cmp al, 0Dh             ; Verificar si es Enter (código 0Dh)
    jne WaitForEnteer        ; Si no es Enter, seguir esperando


    jmp WelcomeMenu

HandleWin:
    mov al, GREEN_ATTR
    call drawFooter

    lea si, successMsg
    mov bh, 22
    mov ah, 0Fh
    call PrintCenteredDollarString

    lea si, continuePrompt
    mov bh, 23
    mov ah, 0Fh
    call PrintCenteredDollarString

    ;limpio la cadena de categorias
    mov bh, 1
    mov ah, 07h
    lea si, categoryText
    call ClearCenteredDollarString

    ;limpio la cadena de intentos restantes
    mov bh, 2
    mov ah, 07h
    lea si, attemptsPrompt
    call ClearCenteredDollarString

    mov bh, 2
    mov bl, 50
    mov ah, 07h
    lea si, attemptsLeft
    call ClearStringAt

    ;limpio prompt
    lea si, promptText
    mov bh, PROMPT_ROW
    mov ah, 07h
    call ClearCenteredDollarString

    mov bh, 1
    mov bl, 50
    mov ah, 07h
    lea si, attemptsLeft
    call ClearStringAt

    ;resetear estado del juego
    mov byte ptr [attemptCount], 0

    ;resetear historial
    push ds
    pop es

    ; Borrar historyWords 
    lea di, historyWords
    mov cx, MAX_ATTEMPTS * WORD_LEN
    xor al, al
    rep stosb

    ; Borrar historyStatuses 
    lea di, historyStatuses
    mov cx, MAX_ATTEMPTS * WORD_LEN
    xor al, al
    rep stosb

    ; Limpiar buffer de entrada (guessBuffer)
    push ds
    pop es
    lea di, guessBuffer
    mov cx, WORD_LEN
    xor al, al
    rep stosb

    ; Limpiar statusBuffer (por las dudas)
    lea di, statusBuffer
    mov cx, WORD_LEN
    xor al, al
    rep stosb

    ;limpio vars
    mov al, 24h
    mov bx, offset targetWord
    call cleanVar

    call clearTemp

WaitForEnterr:
    xor ah, ah
    int 16h                 ; Leer tecla del teclado
    cmp al, 0Dh             ; Verificar si es Enter (código 0Dh)
    jne WaitForEnterr        ; Si no es Enter, seguir esperando


    jmp WelcomeMenu

DrawCategoryMenu proc near
    ; Dibuja el menú de categorías con la flecha en la seleccionada
    push ax
    push bx
    push cx
    push dx
    push si
    
    ; Fila base para las categorías (después del welcomePrompt que está en fila 14)
    ; Usar un registro temporal para la fila para no perder el valor
    mov ch, 16              ; Primera categoría en fila 16
    
    ; Dibujar "Lugares" (índice 0)
    mov bh, ch             ; Establecer fila desde CH
    mov dl, 0              ; Flag para saber si mostrar flecha
    mov al, [selectedCategory]
    cmp al, 0
    jne DrawLugaresNoArrow
    mov dl, 1              ; Mostrar flecha
DrawLugaresNoArrow:
    lea si, categoryLugares
    mov ah, 0Fh
    call DrawCategoryItem
    
    ; Dibujar "Comidas" (índice 1)
    inc ch                 ; Siguiente fila
    mov bh, ch             ; Establecer fila desde CH
    mov dl, 0
    mov al, [selectedCategory]
    cmp al, 1
    jne DrawComidasNoArrow
    mov dl, 1
DrawComidasNoArrow:
    lea si, categoryComidas
    mov ah, 0Fh
    call DrawCategoryItem
    
    ; Dibujar "General" (índice 2)
    inc ch                 ; Siguiente fila
    mov bh, ch             ; Establecer fila desde CH
    mov dl, 0
    mov al, [selectedCategory]
    cmp al, 2
    jne DrawGeneralNoArrow
    mov dl, 1
DrawGeneralNoArrow:
    lea si, categoryGeneral
    mov ah, 0Fh
    call DrawCategoryItem
    
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
DrawCategoryMenu endp

DrawCategoryItem proc near
    ; Entrada: SI = offset del texto de la categoría
    ;          BH = fila (DEBE preservarse)
    ;          DL = 1 si mostrar flecha, 0 si no
    ;          AH = atributo
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    
    ; Guardar valores críticos ANTES de cualquier modificación
    mov di, si             ; DI = offset del texto
    mov ch, bh             ; CH = fila (CRÍTICO - preservar)
    mov cl, ah             ; CL = atributo (preservar)
    push dx                ; Guardar flag de flecha (DL) en la pila
    push cx                ; Guardar CX completo (CH y CL) en la pila
    
    ; Contar longitud del texto
    mov si, di
    xor cx, cx
    
CountCategoryLen:
    lodsb
    cmp al, '$'
    je CategoryLenDone
    inc cx
    jmp CountCategoryLen
    
CategoryLenDone:
    ; Guardar longitud en BX antes de restaurar CX
    mov bx, cx             ; BX = longitud del texto (preservar)
    
    ; Restaurar valores críticos desde la pila (orden inverso al push)
    pop cx                 ; Restaurar CX (CH = fila, CL = atributo)
    pop dx                 ; Restaurar flag de flecha (DL)
    
    ; Calcular columna centrada: (80 - (longitud_texto + 3)) / 2
    push dx                ; Guardar flag de flecha temporalmente
    mov ax, bx             ; AX = longitud del texto (desde BX)
    add ax, 3              ; Agregar "-> "
    mov dx, 74
    sub dx, ax
    shr dx, 1              ; DX = columna inicial
    mov bl, dl             ; BL = columna inicial
    mov bh, ch             ; BH = fila (desde CH - CRÍTICO)
    mov ah, cl             ; AH = atributo (desde CL)
    pop dx                 ; Restaurar flag de flecha (DL)
    
    ; Dibujar flecha o espacios
    cmp dl, 1
    jne NoArrow
    lea si, arrow
    mov bh, ch             ; Asegurar que BH tiene la fila correcta
    call PrintDollarStringAt
    inc bl                 ; Espacio después de la flecha
    jmp DrawCategoryText
    
NoArrow:
    lea si, spaceArrow
    mov bh, ch             ; Asegurar que BH tiene la fila correcta
    call PrintDollarStringAt
    inc bl                 ; Espacio después de los espacios
    
DrawCategoryText:
    ; Dibujar el texto de la categoría
    mov si, di             ; Restaurar SI desde DI
    mov bh, ch             ; CRÍTICO: restaurar BH desde CH antes de llamar
    call PrintDollarStringAt
    
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
DrawCategoryItem endp

GameEnd:
    xor ax, ax
    int 16h
    mov ax, 4C00h
    int 21h

end start

