
;
; This code is a subset of the original c64 kernal code.
; It has been modified to implement the load and save IEC routines
; to be run on a KIM-1 single board computer.
; The code assumes a ROM expansion on the KIM-1 at $2000.
;
; Modified June, 2022 by Dave McMurtrie <dave@commodore.international>
; Modified 23. June 2022 by Netzherpes, Just to start Code at $A000 and compile with CC65
; <webdoktor@netzherpes.de>
; Modified 30 Jan 2024 by Eduardo Casino <mail@eduardocasino.es>:
;     General cleanup
;     Implement dir listing, full file names, send commands and msg printing
;           
;   Dir listing and send command adapted from https://codebase64.org/
;

;  TODO:
;    Recover original cassette-like fiunctions by Dave McMurtrie
;
RVS_TEST        = 1

                .include "xkim1541.inc"

                ; THE C64 KERNAL USED DC00 AND DD00 6526 ADDRESSES FOR THESE.
                ; CONVERTED TO USE 6530 LOCATIONS FOR TIMER AND IO TO RUN ON
                ; A KIM-1.
                ;
PAD             := $1700        ; PERIPHERAL DATA REGISTER A ON KIM-1
PADD            := $1701        ; PERIPHERAL DATA DIR REGISTER A ON KIM-1
D2PRA           := $1700        ; PERIPHERAL DATA REGISTER A ON C64
D2DDRA          := $1701        ; PERIPHERAL DATA DIR REGISTER A ON C64
D1T2H           := $1704        ; 1T INTERVAL TIMER ON THE 6530
D64TH           := $1706        ; 64T INTERVAL TIMER ON THE 6530
D1ICR           := $1707        ; 6530 INTERVAL TIMER STATUS REGISTER

                ; KIM-1 Zero Page locations
                ;
PREG            := $f1          ; Current Status Register

                ; KIM-1 routines in ROM
                ;
CRLF            := $1e2f        ; print CR/LF
PRTBYT          := $1e3b        ; print A as two hex digits
GETCH           := $1e5a        ; get a key from tty into A
OUTSP           := $1e9e        ; print a space
OUTCH           := $1ea0        ; print A to TTY

                ; ZERO PAGE ADDRESSES FOR IEC ROUTINES
                ;
                ; NOTE: Keep zp usage low to allow integration with KB9
                ;
                .segment        "zp_iec" : zeropage

        .if     RVS_TEST = 1
FLNFLG:         .res    1       ; (byte) First line flag
PRNL:           .res    1       ; (byte) Address of string to print (low)
PRNH:           .res    1       ; (byte) Address of string to print (high)
        .endif

STATUS:         .res    1       ; (byte) I/O OPERATION STATUS BYTE
C3P0:           .res    1       ; (byte) IEEE BUFFERED CHAR FLAG
BSOUR:          .res    1       ; (byte) CHAR BUFFER FOR IEEE
R2D2:           .res    1       ; (byte) SERIAL BUS USAGE
BSOUR1:         .res    1       ; (byte) TEMP USED BY SERIAL ROUTINE
COUNT:          .res    1       ; (byte) TEMP USED BY SERIAL ROUTINE
FNADR:          .res    2       ; (word) FILENAME ADDRESS
DSAL:           .res    1       ; (byte) START ADDR LOW BYTE FOR SAVE ROUTINE
DSAH:           .res    1       ; (byte) START ADDR HIGH BYTE FOR SAVE ROUTINE
DEAL:           .res    1       ; (byte) END ADDRESS LOW BYTE
DEAH:           .res    1       ; (byte) END ADDRESS HIGH BYTE
VERCK:          .res    1       ; (byte) LOAD OR VERIFY FLAG

                .assert * <= $EF, error, "Page zero overflow!"

                ; Locations used by xkim1541 for variables
                ;
                .segment        "bss"

MSGFLG:         .res    1       ; (byte) OS MESSAGE FLAG
FA:             .res    1       ; (byte) FILE PRIMARY ADDRESS
SA:             .res    1       ; (byte) FILE SECONDARY ADDRESS
FNLEN:          .res    1       ; (byte) FILENAME LENGTH
MEMUSS:         .res    2       ; (word) USER SPECIFIED FILE LOAD ADDRESS
SAVEY:          .res    1       ; (byte) Save Y register
PRNUM:          .res    2       ; (word) BCDPRN: 16-bit number to print 

                .assert * <= $17E7, error, "BSS segment overflow!"

                ;LOCATIONS USED BY THE NATIVE KIM-1 TAPE LOAD AND SAVE ROUTINES.
                ;I'LL USE THESE FOR DISK LOAD AND SAVE FOR CONSISTENCY OF USER
                ;EXPERIENCE.
                ;
                .segment        "kim_ram"

SAL:            .res    1       ;  (byte) $17F5 KIM-1 START ADDRESS LOW BYTE FOR TAPE ROUTINES
SAH:            .res    1       ;  (byte) $17F6 KIM-1 START ADDRESS HIGH BYTE FOR TAPE ROUTINES
EAL:            .res    1       ;  (byte) $17F7 KIM-1 END ADDRESS LOW FOR TAPE SAVE
EAH:            .res    1       ;  (byte) $17F8 KIM-1 END ADDRESS HIGH FOR TAPE SAVE
ID:             .res    1       ;  (byte) $17F9 KIM-1 TAPE ID. USE AS FILENAME

                .segment        "code"

                ; Jump table. Add entries to new functions at the end
                ;
SEINIT:         jmp     RSEINIT         ; Init serial bus
SETSAD:         jmp     RSETSAD         ; Set Start ADdress
SETEAD:         jmp     RSETEAD         ; Set End ADdress
SETMUSS:        jmp     RSETMUSS        ; Set User Specified Load Address
SETVRCK:        jmp     RSETVRCK        ; Set Verify Flag
SETFA:          jmp     RSETFA          ; Set Frive Number
SETSA:          jmp     RSETSA          ; Set Secondary Address
GETSTAT:        jmp     RGETSTAT        ; Get Status Byte
SETMSGF:        jmp     RSETMSGF        ; Set Message Flag
GETMSGF:        jmp     RGETMSGF        ; Get message Flag
DSKCMD:         jmp     RDSKCMD         ; Send a command to the disk
DIRLIST:        jmp     RDIRLIST        ; Directory listing
FREAD:          jmp     RFREAD          ; Read from file
FWRITE:         jmp     RFWRITE         ; Write to file

BCDPRN:         jmp     RBCDPRN         ; 16-bit BCD print

.export         SEINIT, SETSAD, SETEAD, SETMUSS, SETVRCK, SETFA, SETSA
.export         GETSTAT, SETMSGF, GETMSGF, DSKCMD, DIRLIST, FREAD, FWRITE


                ; Set Reverse On and Reverse Off mode
                ;
        .if     RVS_TEST = 1   
RVSON:          lda     #<ONSEQ
                sta     PRNL
                lda     #>ONSEQ
                sta     PRNH
                jmp     PUTS

RVSOFF:         lda     #<OFFSEQ
                sta     PRNL
                lda     #>OFFSEQ
                sta     PRNH

PUTS:           php
                clc
                ldy     0
@LOOP:          lda     (PRNL), y
                beq     @DONE
                sty     SAVEY
                jsr     OUTCH
                ldy     SAVEY
                iny
                jmp     @LOOP
@DONE:          plp
                rts

ONSEQ:           .byte $1b, "[7m", 0
OFFSEQ:          .byte $1b, "[27m", 0
        .endif

                ; Init serial bus
                ;
                ; Parameters: None
                ;
RSEINIT:        cld
                sei
                lda     #%00000111
                sta     PAD
                lda     #%00111111 
                sta     PADD

                and     #0
                sta     PREG
                sta     FA
                sta     SA
                sta     STATUS
                sta     MSGFLG
                sta     C3P0
                sta     R2D2

                clc
                rts

RSETSAD:        ; Set Start ADdress
                ;
                ; Parameters:
                ;       A : Address low
                ;       Y : Address high
                ;
                sta     DSAL
                sty     DSAH
                rts

RSETEAD:        ; Set End ADdress
                ;
                ; Parameters:
                ;       A : Address low
                ;       Y : Address high
                ;
                sta     DEAL
                sty     DEAH
                rts

RSETMUSS:        ; Set User Specified Load Address
                ;
                ; Parameters:
                ;       A : Address low
                ;       Y : Address high
                ;
                sta     MEMUSS
                sty     MEMUSS+1
                rts

RSETVRCK:       ; Set Verify Flag
                ;
                ; Parameters:
                ;       A : Value
                ;
                sta     VERCK
                rts

RSETFA:         ; Set Drive Number 
                ;
                ; Parameters:
                ;       A : Drive number
                ;
                sta     FA
                rts

RSETSA:         ; Set Secondary ADdress 
                ;
                ; Parameters:
                ;       A : Secondary Address
                ;
                sta     SA
                rts

RGETSTAT:       ; Get Status Byte
                ;
                ; Parameters: None
                ;
                ; Returns status byte in A
                ; 
                lda     STATUS
                rts

RSETMSGF:       ; Set Message Flag
                ;
                ; Parameters:
                ;       A : Value
                ;
                sta     MSGFLG
                rts

RGETMSGF:       ; Get Message Flag
                ;
                ; Parameters: None
                ;
                ; Returns flag in A
                ; 
                lda     MSGFLG
                rts

                ; Send a command to the disk
                ;
                ; Parameters:
                ;       A : Command string length
                ;       X : Address of command string (low)
                ;       Y : Address of command string (high)
                ;
                ; Expected zero page variables:
                ;       FA:     Drive number. If 0, set DEFDRIVE
                ; 
RDSKCMD:        jsr     SETNAM
                jsr     SETDRV

                lda     #$6f            ; Secondary address 6f (required for disk command)
                sta     SA

                jsr     OPENI           ; Send command
                jsr     CLSEI

                rts              

                ; Directory listing
                ;
                ; Parameters:
                ;
                ;       FA:     Drive number. If 0, set DEFDRIVE
                ; 
RDIRLIST:       lda     #1              ; Filename length
        .if     RVS_TEST = 1
                sta     FLNFLG          ; While here, set first line flag
        .endif
                ldx     #<DIRNAME       ; Pointer to file name ("$")
                ldy     #>DIRNAME
                jsr     SETNAM
                jsr     SETDRV

                lda     #$00            ; Secondary address 0 (required for dir reading)
                sta     SA

                jsr     OPENI           ; Open the directory
                ; bcs     @ERROR        ; quit if OPEN failed
                                        ; OPENI foes not return on error
                ; Open channel for input
                ;
                lda     FA
                jsr     TALK
                jsr     TKATN           ; SA is 0 for dir reading, no need to send it
                bit     STATUS          ; Did he listen?
                bpl     @CONT
                jmp     ERROR5          ; Nope, device not present

@CONT:          ldy     #$04            ; Skip 4 bytes on the first dir line
                bne     @SKIP2          ; Always jump
@NEXT:          ldy     #$02            ; Skip 2 bytes on all other lines
@SKIP2:         jsr     @GETBYTE        ; get a byte from dir and ignore it
                dey
                bne     @SKIP2

                jsr     @GETBYTE        ; get low byte of basic line number
                sta     PRNUM
                jsr     @GETBYTE        ; get high byte of basic line number
                sta     PRNUM+1
                jsr     BCDPRN          ; print basic line number
                lda     #' '            ; print a space first 
        .if     RVS_TEST = 1               
                jsr     OUTCH
                lda     FLNFLG          ; First line?
                beq     @CHAR2          ; No, continue
                jsr     RVSON           ; Yes, set reverse video on
                jmp     @CHAR2          ; Skip next line
        .endif
@CHAR:          jsr     OUTCH           ; print character
@CHAR2:         jsr     @GETBYTE
                bne     @CHAR           ; continue until end of line

                jsr     CRLF

        .if     RVS_TEST = 1
                lda     FLNFLG          ; First line?
                beq     @NEXT           ; No, continue
                jsr     RVSOFF          ; Yes, set reverse video off
                dec     FLNFLG          ; And unset flag
        .endif
                ; FIXME: Check for esc/space to interrupt/pause listing
                ;

                jmp     @NEXT           ; next line

@EXIT:          jsr     UNTLK           ; Untalk channel
                jsr     CLSEI           ; Close file
                rts

@GETBYTE:       jsr     READST          ; Read status byte
                bne     @END            ; read error or end of file
                jmp     CHRIN           ; Read byte from directory
@END:           pla                     ; don't return to dir reading loop
                pla
                jmp     @EXIT

DIRNAME:        .byte   "$"

                ; Read from file
                ;
                ; Parameters:
                ;       A : File name length
                ;       X : Address of file name (low)
                ;       Y : Address of file name (high)
                ;
                ;       MEMUSS: User load address for SA<>0
                ;       VERCK:  Verify flag
                ;       FA:     Drive number. If 0, set DEFDRIVE
                ;       SA:     Secondary address
                ; 
RFREAD:         jsr     SETNAM
                jsr     SETDRV

                ldy     FNLEN           ; Must have file name
                bne     @OPEN           ; Yes...OK
                ;
                jmp     ERROR8          ; Missing file name
                ;
@OPEN:          ldx     SA              ; Save SA in .X
                jsr     LUKING          ; Tell user looking
                lda     #$60            ; Special LOAD command
                sta     SA
                jsr     OPENI           ; Open the file
                ;
                lda     FA
                jsr     TALK            ; Establish the channel
                lda     SA
                jsr     TKSA            ; Tell it to load
                ;
                jsr     ACPTR           ; Get first byte
                sta     DEAL            ; Store as low byte load address
                ;
                lda     STATUS          ; Test STATUS for error
                lsr
                lsr
                bcs     @NOTFND         ; File not found
                ;
                jsr     ACPTR
                sta     DEAH            ; Store as high byte load address
                ;
                txa                     ; Find out old SA
                bne     @LOAD           ; SA<>0 use disk address
                lda     MEMUSS          ; Else load where user wants
                sta     DEAL
                lda     MEMUSS+1
                sta     DEAH
@LOAD:          jsr     LODING          ; Tell user loading
                lda     DEAH
                jsr     PRTBYT
                lda     DEAL
                jsr     PRTBYT
                
                ;
@TMLOOP:        lda     #$FD            ; Mask off timeout
                and     STATUS
                sta     STATUS
                ;
                ; TODO: check "ESC" key (and PAUSE if TYPE)
                ;
                ; jsr   STOP            ; STOP KEY?
                ; bne   @CONT           ; NO...
                ;
                ; jmp   BREAK           ; STOP KEY PRESSED
                ;
@CONT:          jsr     ACPTR           ; Get byte off IEEE
                tax
                lda     STATUS          ; Was there a timeout?
                lsr 
                lsr 
                bcs     @TMLOOP         ; Yes...Try again
                txa
                ldy     VERCK           ; Performing verify?
                beq     @DOLOAD         ; No...LOAD
                ldy     #0
                cmp     (DEAL),Y        ; Verify it
                beq     @NEXT           ; O.K....
                lda     #SPERR          ; No good...Verify error
                jsr     UDST            ; Update status
                .byte   $2C             ; Skip next store
                ;
@DOLOAD:        sta     (DEAL),Y
@NEXT:          inc     DEAL            ; Increment store addr
                bne     @SKIP3
                inc     DEAH
@SKIP3:         bit     STATUS          ; EOI?
                bvc     @TMLOOP         ; No...Continue load
                
                jsr     UNTLK           ; Close channel
                jsr     CLSEI           ; Close the file
                ;
                ; Set up end load address
                ;
                ldx     DEAL
                ldy     DEAH
                ;
                clc                     ; Good exit
                rts

@NOTFND:        jmp     ERROR4          ; File not found


                ; Write to file
                ;
                ; Parameters:
                ;       A : File name length
                ;       X : Address of file name (low)
                ;       Y : Address of file name (high)
                ;
                ;       FA:     Drive number. If 0, set DEFDRIVE
                ;       DSAL-DSAH : Start address
                ;       DEAL-DEAH : End address
                ; 
RFWRITE:        jsr     SETNAM
                jsr     SETDRV

                lda     #$61
                sta     SA
                ldy     FNLEN           ; Must have file name
                bne     @OPEN           ; Yes...OK
                ;
                jmp     ERROR8          ; Missing file name

@OPEN:          jsr     OPENI
                jsr     SAVING
                lda     FA
                jsr     LISTN
                lda     SA
                jsr     SECND
                ldy     #0
                lda     DSAL
                jsr     CIOUT
                lda     DSAH
                jsr     CIOUT
SV30:           jsr     CMPSTE          ; Compare start to end
                bcs     SV50            ; Have reached end
                lda     (DSAL),Y
                jsr     CIOUT
                ; JSR  STOP
                ; BNE  SV40
                ;
                ; BREAK  JSR CLSEI
                ;        LDA    #0
                ;        SEC
                ;        RTS
                ;
SV40:           jsr     INCSAL          ; Increment current addr.
                bne     SV30
SV50:           jsr     UNLSN
CLSEI:          bit     SA
                bmi     CLSEI2
                lda     FA
                jsr     LISTN
                lda     SA
                and     #$EF
                ora     #$E0
                jsr     SECND
                ;
CUNLSN:         jsr     UNLSN           ; ENTRY FOR OPENI
                ;
CLSEI2:         clc
                rts
                ;
                ; Compare start and end load/save
                ; addresses.  Subroutine called by
                ; TAPE READ, SAVE, TAPE WRITE
                ;
CMPSTE:         sec
                lda     DSAL
                sbc     DEAL
                lda     DSAH
                sbc     DEAH
                rts
        ;
INCSAL:         inc     DSAL
                bne     INCR
                inc     DSAH
INCR:           rts


                ; Print the 16 bit number stored at PRNUM
                ;
RBCDPRN:        lda     #0
                clv
                ldy     #16
DIV10:          cmp     #10/2
                bcc     UNDER10
                sbc     #(10/2)+$80
                sec
UNDER10:
                rol     PRNUM
                rol     PRNUM+1
                rol
                dey
                bne     DIV10
                ora     #'0'
                pha
                lda     #>(PRNIBBLE-1)
                pha
                lda     #<(PRNIBBLE-1)
                pha
                bvs     RBCDPRN
                rts
PRNIBBLE:       pla
                jmp     OUTCH                

                ; *************************
                ; * IEC SUPPORT FUNCTIONS *
                ; *************************

                ; Establish filename
                ;
SETNAM:         sta     FNLEN
                stx     FNADR
                sty     FNADR+1
                rts

                ; Establish FA and SA
                ;
SETDRV:         lda     FA              ; Get drive number
                bne     SETDRV1         ; Set default if unset
                lda     #DEFDRIVE
SETDRV1:        sta     FA
                rts

                ; CHRIN - Input character
                ;
CHRIN:
                lda     STATUS          ; STATUS FROM LAST
                beq     CHI01           ; WAS GOOD
                lda     #$D             ; BAD...ALL DONE
                clc                     ; VALID DATA
                rts
CHI01:          jmp     ACPTR
                ;

OPENI:          lda     SA
                bmi     OP175           ; NO SA...DONE
                ldy     FNLEN
                beq     OP175           ; NO FILE NAME...DONE
                ;
                lda     #0              ; CLEAR THE SERIAL STATUS
                sta     STATUS
                ;
                lda     FA
                jsr     LISTN           ; DEVICE LA TO LISTEN
                ;
                lda     SA
                ora     #$F0
                jsr     SECND
                ;
                lda     STATUS          ; ANYBODY HOME?
                bpl     OP35            ; YES...CONTINUE
                ;
                ; THIS ROUTINE IS CALLED BY OTHER
                ; KERNAL ROUTINES WHICH ARE CALLED
                ; DIRECTLY BY OS.  KILL RETURN
                ; ADDRESS TO RETURN TO OS.
                ;
                pla
                pla
                jmp     ERROR5          ; DEVICE NOT PRESENT
                ;
OP35:           lda     FNLEN
                beq     OP45            ; NO NAME...DONE SEQUENCE
                ;
                ; SEND FILE NAME OVER SERIAL
                ;
                ldy     #0
OP40:           lda     (FNADR),Y
                jsr     CIOUT
                iny
                cpy     FNLEN
                bne     OP40
                ;
OP45:           jmp     CUNLSN          ; jsr UNLSN: clc: rts

LDEXIT:         clc                     ; GOOD EXIT
                ;
                ; SET UP END LOAD ADDRESS
                ;
                ldx     DEAL
                ldy     DEAH
                ;
OP175:          clc                     ; FLAG GOOD OPEN
OP180:          rts                     ; EXIT IN PEACE

                ; Subroutine to print to console:
                ;
                ; 'SEARCHING [FOR NAME]''
                ;
LUKING:         lda     MSGFLG          ; Supposed to print?
                bpl     LD115           ; ...No
                ldy     #MS5-MS1        ; "SEARCHING"
                jsr     MSG
                lda     FNLEN
                beq     LD115
                ldy     #MS6-MS1        ; "FOR"
                jsr     MSG
                ; Subroutine to output file name
                ;
OUTFN:          ldy     FNLEN           ; Is there a name?
                beq     LD115           ; No...Done
                ldy     #0
LD110:          lda     (FNADR),Y
                sty     SAVEY
                jsr     OUTCH
                ldy     SAVEY
                iny
                cpy     FNLEN
                bne     LD110
                ;
LD115:          rts
                ; Subroutine to print:
                ;
                ; 'LOADING'/'VERIFING'
                ;
LODING:         ldy     #MS10-MS1       ; Assume 'LOADING'
                lda     VERCK           ; Check flag
                beq     LD410           ; Are doing load
                ldy     #MS21-MS1       ; Are 'VERIFYING'
LD410:          jmp     SPMSG
                ; Subroutine to print:
                ;
                ; 'SAVING <FILE NAME>'
                ;
SAVING:         lda     MSGFLG
                bpl     SV115           ; No print
                ldy     #MS11-MS1       ; 'SAVING'
                jsr     MSG
                jmp     OUTFN           ; <FILE NAME>                ;

READST:         lda     STATUS
UDST:           ora     STATUS
                sta     STATUS
SV115:          rts

                ; COMMAND SERIAL BUS DEVICE TO TALK
                ;
TALK:           ora     #$40            ; MAKE A TALK ADR
                .byte   $2C             ; SKIP TWO BYTES
                ;
                ;COMMAND SERIAL BUS DEVICE TO LISTEN
                ;
LISTN:          ora     #$20            ; MAKE A LISTEN ADR
                ; XXX THE CALL TO RSP232 IS NOT NECESSARY ON THE KIM-1
                ; jsr RSP232            ; PROTECT SELF FROM RS232 NMI'S
LIST1:          pha
                ;
                bit     C3P0            ; CHARACTER LEFT IN BUF?
                bpl     LIST2           ; NO...
                ;
                ; SEND BUFFERED CHARACTER
                ;
                sec                     ; SET EOI FLAG
                ror     R2D2
                ;
                jsr     ISOUR           ; SEND LAST CHARACTER
                ;
                lsr     C3P0            ; BUFFER CLEAR FLAG
                lsr     R2D2            ; CLEAR EOI FLAG
        ;
LIST2:          pla                     ; TALK/LISTEN ADDRESS
                sta     BSOUR
                ; sei
                jsr     DATAHI
                cmp     #$3F            ;CLKHI ONLY ON UNLISTEN
                bne     LIST5
                jsr     CLKHI
                ;
LIST5:          lda     D2PRA           ; ASSERT ATTENTION
                ora     #$08
                sta     D2PRA
                ;
                ; ISOURA SEI
ISOURA:         nop
                jsr     CLKLO           ; SET CLOCK LINE LOW
                jsr     DATAHI
                jsr     W1MS            ; DELAY 1 MS
                ; ISOUR SEI             ; NO IRQS ALLOWED
ISOUR:          nop
                jsr     DATAHI          ; MAKE SURE DATA IS RELEASED
                jsr     DEBPIA          ; DATA SHOULD BE LOW
                bcs     NODEV
                jsr     CLKHI           ; CLOCK LINE HIGH
                bit     R2D2            ; EOI FLAG TEST
                bpl     NOEOI
                ; DO THE EOI
ISR02:          jsr     DEBPIA          ; WAIT FOR DATA TO GO HIGH
                bcc     ISR02
                ;
ISR03:          jsr     DEBPIA          ; WAIT FOR DATA TO GO LOW
                bcs     ISR03
                ;
NOEOI:          jsr     DEBPIA          ; WAIT FOR DATA HIGH
                bcc     NOEOI
                jsr     CLKLO           ; SET CLOCK LOW
                ;
                ; SET TO SEND DATA
                ;
                lda     #$08            ; COUNT 8 BITS
                sta     COUNT
                ;
ISR01:
                lda     D2PRA           ; DEBOUNCE THE BUS
                cmp     D2PRA
                bne     ISR01
                asl                     ; SET THE FLAGS
                bcc     FRMERR          ; DATA MUST BE HI
                ;
                ror     BSOUR           ; NEXT bit INTO CARRY
                bcs     ISRHI
                jsr     DATALO
                bne     ISRCLK
ISRHI:          jsr     DATAHI
ISRCLK:         jsr     CLKHI           ; CLOCK HI
                nop
                nop
                nop
                nop
                lda     D2PRA
                and     #$FF-$20        ; DATA HIGH
                ora     #$10            ; CLOCK LOW
                sta     D2PRA
                dec     COUNT
                bne     ISR01
                lda     #$04            ; SET TIMER FOR 1MS
                sta     D1T2H
                ; lda #TIMRB            ; TRIGGER TIMER ; XXX NOT NEEDED ON KIM-1
                ; sta D1CRB
                ; lda D1ICR             ; CLEAR THE TIMER FLAGS<<<<<<<<<<<<<
ISR04:          lda     D1ICR
                and     #$02
                bne     FRMERR
                jsr     DEBPIA
                bcs     ISR04
                ; cli                   ; LET IRQS CONTINUE
                rts
                ;
NODEV:                                  ; DEVICE NOT PRESENT ERROR
                lda     #$80
                .byte   $2C
FRMERR:                                 ; FRAMING ERROR
                lda     #$03
CSBERR:         jsr     UDST            ; COMMODORE SERIAL BUSS ERROR ENTRY
                ; cli                   ; IRQ'S WERE OFF...TURN ON
                clc                     ; MAKE SURE NO KERNAL ERR

                bcc     DLABYE          ; TURN ATN OFF ,RELEASE ALL LINES
                ;
                ; SEND SECONDARY ADDRESS AFTER LISTEN
                ;
SECND:          sta     BSOUR           ; BUFFER CHARACTER
                jsr     ISOURA          ; SEND IT
                ;RELEASE ATTENTION AFTER LISTEN
                ;
SCATN:          lda     D2PRA
                and     #$FF-$08
                sta     D2PRA           ; RELEASE ATTENTION
                rts
                ; TALK SECOND ADDRESS
                ;
TKSA:           sta     BSOUR           ; BUFFER CHARACTER
                jsr     ISOURA          ; SEND SECOND ADDR
TKATN:                                  ; SHIFT OVER TO LISTENER
                ; sei                   ; NO IRQ'S HERE
                jsr     DATALO          ; DATA LINE LOW
                jsr     SCATN
                jsr     CLKHI           ; CLOCK LINE HIGH JSR/RTS
TKATN1:         jsr     DEBPIA          ; WAIT FOR CLOCK TO GO LOW
                bmi     TKATN1
                ; cli                   ; IRQ'S OKAY NOW
                rts
                ; BUFFERED OUTPUT TO SERIAL BUS
                ;
CIOUT:          bit     C3P0            ; BUFFERED CHAR?
                bmi     CI2             ; YES...SEND LAST
                ;
                sec                     ; NO...
                ror     C3P0            ; SET BUFFERED CHAR FLAG
                bne     CI4             ; BRANCH ALWAYS
                ;
CI2:            pha                     ; SAVE CURRENT CHAR
                jsr     ISOUR           ; SEND LAST CHAR
                pla                     ; RESTORE CURRENT CHAR
CI4:            sta     BSOUR           ; BUFFER CURRENT CHAR
                clc                     ; CARRY-GOOD EXIT
                rts
                ;SEND UNTALK COMMAND ON SERIAL BUS
                ;
                ; UNTLK  SEI 
UNTLK:          nop
                jsr     CLKLO
                lda     D2PRA           ; PULL ATN
                ora     #$08
                sta     D2PRA
                lda     #$5F            ; UNTALK COMMAND
                .byte   $2C             ; SKIP TWO BYTES
                ;SEND UNLISTEN COMMAND ON SERIAL BUS
                ;
UNLSN:          lda     #$3F            ; UNLISTEN COMMAND
                jsr     LIST1           ; SEND IT
                ;
                ; RELEASE ALL LINES
DLABYE:         jsr     SCATN           ; ALWAYS RELEASE ATN
                                        ; DELAY THEN RELEASE CLOCK AND DATA
                                        ;
DLADLH:         txa                     ; DELAY APPROX 60 US
                ldx     #10
DLAD00:         dex
                bne     DLAD00
                tax
                jsr     CLKHI
                jmp     DATAHI

                ; INPUT A BYTE FROM SERIAL BUS
                ;
ACPTR:
                sei                     ; NO IRQ ALLOWED
                lda     #$00            ; SET EOI/ERROR FLAG
                sta     COUNT
                jsr     CLKHI           ; MAKE SURE CLOCK LINE IS RELEASED
ACP00A:         jsr     DEBPIA          ; WAIT FOR CLOCK HIGH
                bpl     ACP00A
                ;
EOIACP:
                lda     #$05            ; SET TIMER 2 FOR 256+64US
                sta     D64TH
                ; lda   #TIMRB          ; XXX NOT NEEDED ON KIM-1
                ; sta   D1CRB
                jsr     DATAHI          ; DATA LINE HIGH (MAKES TIMMING MORE LIKE VIC-20
                ; lda   D1ICR           ; CLEAR THE TIMER FLAGS<<<<<<<<<<<<
ACP00:          bit     D1ICR
                bmi     ACP00B          ; RAN OUT.....
                jsr     DEBPIA          ; CHECK THE CLOCK LINE
                bmi     ACP00           ; NO NOT YET
                bpl     ACP01           ; YES.....
                ;
ACP00B:         lda     COUNT           ; CHECK FOR ERROR (TWICE THRU TIMEOUTS)
                beq     ACP00C
                lda     #2
                jmp     CSBERR          ; ST = 2 READ TIMEOUT
                ;
                ; TIMER RAN OUT DO AN EOI THING
                ;
ACP00C:         jsr     DATALO          ; DATA LINE LOW
                jsr     CLKHI           ; DELAY AND THEN SET DATAHI (FIX FOR 40US C64)
                lda     #$40
                jsr     UDST            ; OR AN EOI bit INTO STATUS
                inc     COUNT           ; GO AROUND AGAIN FOR ERROR CHECK ON EOI
                bne     EOIACP
                ;
                ; DO THE BYTE TRANSFER
                ;
ACP01:          lda     #08             ; SET UP COUNTER
                sta     COUNT
                ;
ACP03:          lda     D2PRA           ; WAIT FOR CLOCK HIGH
                cmp     D2PRA           ; DEBOUNCE
                bne     ACP03
                asl                     ; SHIFT DATA INTO CARRY
                bpl     ACP03           ; CLOCK STILL LOW...
                ror     BSOUR1          ; ROTATE DATA IN
                ;
ACP03A:         lda     D2PRA           ; WAIT FOR CLOCK LOW
                cmp     D2PRA           ; DEBOUNCE
                bne     ACP03A
                asl
                bmi     ACP03A
                dec     COUNT
                bne     ACP03           ; MORE BITS.....
                ;...EXIT...
                jsr     DATALO          ; DATA LOW
                bit     STATUS          ; CHECK FOR EOI
                bvc     ACP04           ; NONE...
                ;
                jsr     DLADLH          ; DELAY THEN SET DATA HIGH
                ;
ACP04:          lda     BSOUR1
                ; cli                   ; IRQ IS OK
                clc                     ; GOOD EXIT
                rts
                ;
CLKHI:                                  ; SET CLOCK LINE HIGH (INVERTED)
                lda     D2PRA
                and     #$FF-$10
                sta     D2PRA
                rts
                ;
CLKLO:                                  ;SET CLOCK LINE LOW  (INVERTED)
                lda     D2PRA
                ora     #$10
                sta     D2PRA
                rts
                ;
DATAHI:                                 ; SET DATA LINE HIGH (INVERTED)
                lda     D2PRA
                and     #$FF-$20
                sta     D2PRA
                rts
                ;
DATALO:                                 ; SET DATA LINE LOW  (INVERTED)
                lda     D2PRA
                ora     #$20
                sta     D2PRA
                rts
                ;
DEBPIA:         lda     D2PRA           ; DEBOUNCE THE PIA
                cmp     D2PRA
                bne     DEBPIA
                asl                     ; SHIFT THE DATA bit INTO THE CARRY...
                rts                     ; ...AND THE CLOCK INTO NEG FLAG
                ;
W1MS:                                   ; DELAY 1MS USING LOOP
                txa                     ; SAVE .X
                ldx     #200-16         ; 1000US-(1000/500*8=#40US HOLDS)
W1MS1:          dex                     ; 5US LOOP
                bne     W1MS1
                tax                     ; RESTORE .X
                rts

                ;************************************
                ;*                                  *
                ;* ERROR HANDLER                    *
                ;*                                  *
                ;* PRINTS KERNAL ERROR MESSAGE IF   *
                ;* BIT 6 OF MSGFLG SET.  RETURNS    *
                ;* WITH ERROR # IN .A AND CARRY.    *
                ;*                                  *
                ;************************************
                ;
ERROR4:         lda     #4              ; File not found
                .byte   $2c
ERROR5:         lda     #5              ; Device not present
                .byte   $2c
ERROR8:         lda     #8              ; Missing file name
                ;
                pha                     ; Error number on stack
                jsr     UNLSN           ; Restore I/O Channels
                jsr     UNTLK           ;
                ;
                ldy     #MS1-MS1
                bit     MSGFLG          ; Are we printing error?
                bvc     EREXIT          ; No...
                ;
                jsr     MSG             ; Print "CBM I/O ERROR #"
                pla
                pha
                ora     #$30            ; Make error # ASCII
                jsr     OUTCH           ; Print it
                ;
EREXIT:         pla
                sec
                rts

                ; Print message to screen only if
                ; output enabled
                ;
SPMSG:          bit     MSGFLG          ; Printing messages?
                bpl     MSG10           ; No...
MSG:            lda     MS1,y
                ; php
                ; and   #$7F
                ; jsr   BSOUT
                ; iny
                ; plp
                ; bpl   MSG
                beq     MSG10           ; End of message
                sty     SAVEY
                jsr     OUTCH           ; Nope, print char
                ldy     SAVEY
                iny
                jmp     MSG
MSG10:          clc
                rts

MS1:            .byte   $0d, $0a, "I/O ERROR #", 0
MS5:            .byte   $0d, $0a, "SEARCHING ", 0
MS6:            .byte   "FOR ", 0
MS10:           .byte   $0d, $0a, "LOADING AT 0x", 0
MS11:           .byte   $0d, $0a, "SAVING ", 0
MS21:           .byte   $0d, $0a, "VERIFYING AT 0x", 0
MS17:           .byte   $0d, $0a, "FOUND ", 0
MS18:           .byte   $0d, $0a, "OK", $0d, $0a, 0

                .end
