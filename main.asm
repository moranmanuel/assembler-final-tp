.model small
.stack 100h

MAX_ATTEMPTS     EQU 15
WORD_LEN         EQU 5
PROMPT_ROW       EQU 2        ; Movido más arriba para dar más espacio
INPUT_ROW        EQU 4       ; Movido más abajo para dar más espacio al historial
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

.data
welcomeTitle        db 'Wordly$'
welcomePrompt       db 'Presiona Enter para comenzar$'
continuePrompt      db 'Presiona Enter para continuar$'
attemptsPrompt      db 'Intentos restantes: $' 
promptText          db 'Ingresa tu palabra para adivinar la escondida:$'
promptHint          db '         $'
successMsg          db 'Felicitaciones! Adivinaste la palabra.$'
failMsg             db 'No acertaste. La palabra era: $'
targetWord          db 10 dup (24h)
targetWordDisplay   db 10 dup (24h)
guessBuffer         db WORD_LEN dup (0)
statusBuffer        db WORD_LEN dup (0)
historyWords        db MAX_ATTEMPTS * WORD_LEN dup (0)
historyStatuses     db MAX_ATTEMPTS * WORD_LEN dup (0)
attemptsLeft        db '00$'
attemptCount        db 0

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
    mov bh, 13              ; Dos filas más abajo del título (10 + 2 + 1 = 13)
    mov ah, 0Fh
    call PrintCenteredDollarString

pickWord:
    mov di, offset targetWord
    mov si, offset targetWordDisplay
    call PickRandomWord
    ; Esperar a que presione Enter
WaitForEnter:
    xor ah, ah
    int 16h                 ; Leer tecla del teclado
    cmp al, 0Dh             ; Verificar si es Enter (código 0Dh)
    jne WaitForEnter        ; Si no es Enter, seguir esperando

    ; Limpiar pantalla y comenzar el juego
    mov al, 07h
    int 60h

    lea si, promptText
    mov bh, PROMPT_ROW
    mov ah, 0Fh
    call PrintCenteredDollarString

    lea si, promptHint
    mov bh, PROMPT_HINT_ROW
    mov ah, 0Fh
    call PrintCenteredDollarString

GameLoop:
    ;calcular intentos restantes
    mov al, MAX_ATTEMPTS
    sub al, attemptCount
    mov dx, offset attemptsLeft
    call r2a
    ;imprimir contador de intentos restantes
    lea si, attemptsPrompt
    mov bh, 1
    mov ah, 0Fh
    call PrintCenteredDollarString

    lea si, attemptsLeft
    mov bh, 1
    mov bl, 50
    mov ah, 0Fh
    call PrintDollarStringAt
    ;lea si, failMsg
    ;mov bh, 21
    ;mov bl columna
    ;mov ah, 0Fh
    ;call PrintCenteredDollarString


    int 80h
    mov bl, al              ; Columna centrada en BL
    mov bh, INPUT_ROW
    mov ah, 0Fh
    call DrawGuessSlots

    int 80h
    mov dl, al              ; Columna centrada en DL
    lea bx, guessBuffer
    mov dh, INPUT_ROW
    mov ah, 1Fh
    call ReadWord

    lea si, guessBuffer
    lea di, targetWord
    lea bx, statusBuffer
    call EvaluateGuess

    mov al, attemptCount
    xor ah, ah
    mov bl, WORD_LEN
    mul bl
    mov dx, ax

    push ds
    pop es
    lea si, guessBuffer
    lea di, historyWords
    add di, dx
    mov cx, WORD_LEN
    rep movsb

    lea si, statusBuffer
    lea di, historyStatuses
    add di, dx
    mov cx, WORD_LEN
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
    mov bl, WORD_LEN
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
    
    int 80h
    mov dl, al
    call RenderGuessRow
    
    pop bx
    pop cx
    inc bx              ; Siguiente palabra (más vieja)
    loop RenderHistoryLoop

    mov cx, WORD_LEN
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

    ;limpio la cadena de intentos restantes
    mov bh, 1
    mov ah, 07h
    lea si, attemptsPrompt
    call ClearCenteredDollarString

    mov bh, 1
    mov bl, 50
    mov ah, 07h
    lea si, attemptsLeft
    call ClearStringAt


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

    WaitForEnteer:
    xor ah, ah
    int 16h                 ; Leer tecla del teclado
    cmp al, 0Dh             ; Verificar si es Enter (código 0Dh)
    jne WaitForEnteer        ; Si no es Enter, seguir esperando

    ;llamar a funcion que elige una nueva palabra random

    jmp WelcomeMenu

HandleWin:
    lea si, successMsg
    mov bh, 22
    mov ah, 0Fh
    call PrintCenteredDollarString

    lea si, continuePrompt
    mov bh, 23
    mov ah, 0Fh
    call PrintCenteredDollarString

    ;limpio la cadena de intentos restantes
    mov bh, 1
    mov ah, 07h
    lea si, attemptsPrompt
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

WaitForEnterr:
    xor ah, ah
    int 16h                 ; Leer tecla del teclado
    cmp al, 0Dh             ; Verificar si es Enter (código 0Dh)
    jne WaitForEnterr        ; Si no es Enter, seguir esperando

    ;llamar a funcion que elige una nueva palabra random

    jmp WelcomeMenu

GameEnd:
    xor ax, ax
    int 16h
    mov ax, 4C00h
    int 21h

end start

