DEFDRIVE        = $08           ; Default drive number

SPERR           = $10           ; Second Pass error (verify error)
EOI             = $40

FPRNMSG         = %10000000
FPRNERR         = %01000000

FO_TYPE         = %10000000     ; 1xxx xxxx - Type to screen
FO_BIN          = 0             ; 0000 0000 - Binary read/write (prg)
FO_HEX          = %00000001     ; xxxx xxx1 - Intel Hex read/write
