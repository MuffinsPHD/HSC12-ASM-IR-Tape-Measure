#include "hcs12.inc"
lcd_dat         equ     portk
lcd_dir         equ     ddrk
lcd_E           equ     $02
lcd_RS          equ     $01

                org     $10ff            ;loads RMB safely away from stack
inint           rmb     3                ;reserved for inch integer value
period          fcb     $2E              ;forces ASCII value for "."
indeci          rmb     2                ;reserved for inch decimal value
                fcb     $69              ;"i" ascii
                fcb     $6e              ;"n" ascii
lastm           rmb     1                ; =0 for puts2LCD, ends at null char
cmint           rmb     3                ;reserved for cm interger value
                fcb     $2E              ;"." ascii
cmdeci          rmb     2                ;reserved for cm decimal value
                fcb     $63              ;"c" ascii
                fcb     $6d              ;"m" ascii
lastn           rmb     1                ; =0 for puts2LCD, ends at null char

                org     $1150            ;else it would display jargon
range1          rmb     2                ;8-20in range
range2          rmb     2                ;20-30in range
range3          rmb     2                ;30-40in range
range4          rmb     2                ;40-100in range
ATDreg          rmb     2
cmconst         rmb     2
                                         
                org     $2000             ;start by typing g 2000 into term
                lds     #$1500            ; stack
                jsr     openLCD
                ldab    #0                ; load A w/ 0
                stab    lastm             ;stores 0 so putslcd stops (inches)
                stab    lastn             ;stores 0 so putslcd stops (cm)
                
                ldaa    #115              ;Highest ATD Value for Range1
                std    $1151              ;range1
                ldaa    #40               ;Highest ATD Value for Range2
                std    $1153              ;range2
                ldaa    #25               ;Highest ATD Value for Range3
                std    $1155              ;range3
                ldaa    #20               ;Highest ATD Value for Range4
                std    $1157              ;range4
                
                movb    #$C0,atd0ctl2     ; power up,fast flags clear
                jsr     delay20us         ; delay to initialize

start           movb    #$00,atd0ctl3        ; 8 conversions
                movb    #$8B,atd0ctl4        ; 8 bit 500khz
                movb    #$A3,atd0ctl5        ; right justified, unsigned; input AN3
load            brclr   atd0stat0,$80,load      ; branch if msb  = 0
                ldx     ATD0DR2
                stx     ATDreg

;;;chooses constant so IR is accurate to at most (confidently) 65 inches
                ldx     ATDreg         ;loads ATD register into acc X
                cpx     range4          ;compare ATD value to range4
                blt     k4              ;breaks if ATD value is less than range4
                ldx     ATDreg         ;;;checks all the ranges
                cpx     range3          ;;;if ATD value is less than range
                blt     k3              ;;;it uses the constant tied to the range
                ldx     ATDreg         ;;;contant is specified in k
                cpx     range2          ;;;ex: k4
                blt     k2
                ldx     ATDreg
                cpx     range1
                blt     k1
                bra     big             ;used as failsafe, prob wont be used

;loads inch/cm constants

k4              ldx     ATDreg         ;loads ATD value
                ldd     #3020          ;loads CM constant for range
                std     cmconst        ;stores in rmb cmconst
                ldd     #1200          ;loads inch constant
                bra     inchequ        ;breaks to inchequ
k3              ldx     ATDreg         ;; same for the following
                ldd     #3060          ;;;the cm does not equal inch precisely
                std     cmconst        ;;;i assume its the byproduct of using
                ldd     #1222          ;;;8-bit and 16-bit math with
                bra     inchequ        ;;;8-bit ATD readings
k2              ldx     ATDreg
                ldd     #3200
                std     cmconst
                ldd     #1290
                bra     inchequ
k1              ldx     ATDreg
                ldd     #3310
                std     cmconst
                ldd     #1350
                bra     inchequ
big             ldx     ATDreg
                ldd     #3333
                std     cmconst
                ldd     #1350
                bra     inchequ
                
;((1/km)/ATD) - 1 = R = int.dec
inchequ         ldy     #0              ;ASM division is weird
                ediv                    ;((1/km)/ATD))
                dey                     ;y-1
                sty     $1200           ;stores value
                lslb                    ;shifts B to get rid of leading '0'
                std     $1202           ;stores value
                ldy     #inint          ;loads integer space
                ldd     $1200           ;loads interger value previously calc
                jsr     ascint          ;converts from binary to ASCII
                ldy     #indeci         ;loads decimal space
                ldd     $1202           ;loads decimal value previously calc
                jsr     ascdec          ;converts binary value to ASCII
                bra     centequ

;((1/km)/ATD) - 1 = R = int.dec
centequ         ldx     ATDreg
                ldd     cmconst
		ldy     #0              ;ASM division is weird
                ediv                    ;((1/km)/ATD))
                dey                     ;y-1
                sty     $1204           ;stores value
                lslb                    ;shifts B to get rid of leading '0'
                std     $1206           ;stores value
                ldy     #cmint          ;loads integer space
                ldd     $1204           ;loads interger value previously calc
                jsr     ascint          ;converts from binary to ASCII
                ldy     #cmdeci         ;loads decimal space
                ldd     $1206           ;loads decimal value previously calc
                jsr     ascdec          ;converts binary value to ASCII
                bra     toLCD           ;else breaks to cm lcd command

;calculated values to LCD
toLCD           ldaa    #$81            ;value specifies top line of lcd
                jsr     cmd2LCD         ;uses value in LCD cmd
                ldx     #inint          ;loads inch ASCII value
                jsr     putsLCD         ;uses value and displays it on LCD
                ldaa    #$C1            ;value specifies bottom line of lcd
                jsr     cmd2LCD         ;uses value in lcd cmd
                ldx     #cmint          ;loads cm ASCII value
                jsr     putsLCD         ;uses value and displays it on lcd
                ldy     #$30            ;delays $28 * 10ms
                jsr     delayby10ms
                jmp     start           ;jumps to beginning

;converts numbers left of decimal to ASCII
ascint          ldx     #10
                idiv
                ldaa    #$30
                aba
                staa    2,Y
                xgdx
                ldx     #10
                idiv
                ldaa    #$30
                aba
                staa    1,Y
                xgdx
                ldx     #10
                idiv
                ldaa    #$30
                aba
                staa    0,Y
                rts

;converts decimal to ASCII
ascdec          ldx       #10
                idiv
                ldaa      #$30
                aba
                staa      1,y
                xgdx
                ldx       #10
                idiv
                ldaa      #$30
                aba
                staa      0,Y
                rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cmd2LCD     psha
            bclr    lcd_dat,lcd_RS
            bset    lcd_dat,lcd_E
            anda    #$F0
            lsra
            lsra
            oraa    #lcd_E
            staa    lcd_dat
            nop
            nop
            nop
            nop
            bclr    lcd_dat,lcd_E
            pula
            anda    #$0F
            lsla
            lsla
            bset    lcd_dat,lcd_E
            oraa    #lcd_e
            staa    lcd_dat
            nop
            nop
            nop
            bclr    lcd_dat,lcd_E
            ldy     #2
            jsr     delayby1ms
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
openLCD     movb    #$FF,lcd_dir
            ldy     #10
            jsr     delayby1ms
            ldaa    #$28
            jsr     cmd2lcd
            ldaa    #$01
            jsr     cmd2lcd
            ldaa    #$06
            jsr     cmd2lcd
            ldaa    #$01
            jsr     cmd2lcd
            ldy     #2
            jsr     delayby1ms
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
putcLCD     psha
            bset    lcd_dat,lcd_RS
            bset    lcd_dat,lcd_E
            anda    #$F0
            lsra
            lsra
            oraa    #$03
            staa    lcd_dat
            nop
            nop
            nop
            bclr    lcd_dat,lcd_E
            pula
            anda    #$0F
            lsla
            lsla
            bset    lcd_dat,lcd_E
            oraa    #$03
            staa    lcd_dat
            nop
            nop
            nop
            bclr    lcd_dat,lcd_E
            ldy     #1
            jsr     delayby1ms
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
putsLCD     ldaa    1,x+                    ; get one char from string
            beq     donePS                  ; reach NULL character
            jsr     putcLCD
            bra     putsLCD
donePS      rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
delayby1ms  movb    #$90,TSCR1              ; enable TCNT & ffclr
            movb    #$06,TSCR2              ; config prs factor to 64
            movb    #$01,TIOS               ; enable OC0
            ldd     TCNT
again0      addd    #375                    ; start an o/c operation
            std     TC0                     ; with 50 ms time delay
wait_lp0    brclr   TFLG1,$01,wait_lp0
            ldd     TC0
            dbne    y,again0
            rts

delayby10ms movb    #$90,TSCR1              ; enable TCNT & ffclr
            movb    #$06,TSCR2              ; config prs factor to 64
            movb    #$01,TIOS               ; enable OC0
            ldd     TCNT
again2      addd    #3750                   ; start an output compare operation
            std     TC0                     ; with 50 ms time delay
wait_lp     brclr   TFLG1,$01,wait_lp
            ldd     TC0
            dbne    y,again2
            rts

delay20us   movb    #$90,tscr1              ; enable timer and ffclr
            movb    #$00,tscr2              ; prescale set to 1
            movb    #$01,TIOS               ; enable OC0
            ldd     tcnt                    ; enable time counter
            addd    #480                    ; add 480 to register D
            std     tc0                     ; enable ch.0 for o/c
loop        brclr   tflg1,$01,loop          ; branch clear
            rts