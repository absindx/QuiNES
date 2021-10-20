;-------------------;
;  QuiNES           ;
;     Quine of NES  ;
;-------------------;
;version:           ;
;  1.00 - 2021/10/20;
;build:             ;
;  nesasm QuiNES.asm;
;-------------------;

; iNES header
  .inesprg 1
  .ineschr 1
  .inesmir 0
  .inesmap 0

; I/O registers
PPUCTRL     = $2000
PPUMASK     = $2001
PPUSTATUS   = $2002
OAMADDR     = $2003
OAMDATA     = $2004
PPUSCROLL   = $2005
PPUADDR     = $2006
PPUDATA     = $2007
OAMDMA      = $4014
JOY1        = $4016
JOY2        = $4017

; Memory map
ScreenSel   = $10
Joypad1     = $11
YPos        = $12
ScrollY     = $14
YRow        = $16
YRowDown    = $18
YRowUp      = $1A
VramDst     = $1C

; Define
PalBlack    = $0F
PalWhite    = $30
SrcRow      = 751 - 1
IdxRow      = 939 - 1
TxtRow      = 1329- 1
ChrRow      = 1590
ScrStop     = 1609

; Macro
Cs .func (\1-*%\1)
Align .macro
  .org *+Cs(\1)%\1
  .endm

; PRG
;---------------

; Interrupt
  .bank 1
  .org $FFFA
  .dw NMI, RST, IRQ

; Reset
  .bank 0
  .org $C000
RST:
  SEI
  CLD
  LDA #$00
  TAX
  TAY
.InitMem
  STA <$00, X
  PHA
  STA $0200, X
  STA $0300, X
  STA $0400, X
  STA $0500, X
  STA $0600, X
  STA $0700, X
  INX
  BNE .InitMem
  DEX
  TXS

.WVBL1
  BIT PPUSTATUS
  BPL .WVBL1
.WVBL2
  BIT PPUSTATUS
  BPL .WVBL2

  STY PPUCTRL
  STY PPUMASK

  LDA #$3F
  STA PPUADDR
  STY PPUADDR
  LDA #PalBlack
  LDX #PalWhite
  STA PPUDATA
  STX PPUDATA
  LDX #$1E
.InitPal
  STA PPUDATA
  DEX
  BNE .InitPal

  STY OAMADDR
  STY OAMDMA

  INC <$00
  LDA #$20
  .db $2C ; BIT abs
.InitNtBtm
  LDA #$28
  STA PPUADDR
  STY PPUADDR
.InitNt
  STY PPUDATA
  STY PPUDATA
  STY PPUDATA
  STY PPUDATA
  DEX
  BNE .InitNt
  DEC <$00
  BEQ .InitNtBtm

  LDA #$00
  STA <YRowDown
  STA <YRowDown + 1
  LDA #$C4
  STA <YRowUp
  LDA #$FF
  STA <YRowUp + 1
  LDA #$20
  STA <VramDst + 1
  LDA #$42
  STA <VramDst

  LDA #$2D
  STA <$0F
  LDA #$00
.InitDrawCode
  JSR DrawCode
  JSR RowDown
  DEC <$0F
  BNE .InitDrawCode

  LDA #%10001000
  STA PPUCTRL
  LDA #%01000000
  STA JOY2

.InfLoop
  JMP .InfLoop

; Interrupt
NMI:
  LDA PPUSTATUS
  LDA #$00
  STA PPUCTRL
  STA PPUMASK

  JSR GetJoypad
  LDA <Joypad1
  AND #%00000100
  BEQ .SkipDown
  LDA <YRowDown
  CMP #LOW(ScrStop)
  LDA <YRowDown + 1
  SBC #HIGH(ScrStop)
  BCS .SkipDown
  INC <YPos
  BNE .SkipDownHigh
  INC <YPos + 1
.SkipDownHigh
  LDX <ScrollY
  INX
  CPX #$F0
  BCC .SkipDownFlip
  LDA #$02
  EOR <ScreenSel
  STA <ScreenSel
  LDX #$00
.SkipDownFlip
  STX <ScrollY
  LDX #YRowDown
  JSR RowSet
  JSR DrawCode
  JSR RowDown
  JSR DrawCode
.SkipDown
  LDA <Joypad1
  AND #%00001000
  BEQ .SkipUp
  LDA <YPos
  ORA <YPos + 1
  BEQ .SkipUp
  LDA <YPos
  BNE .SkipUpHigh
  DEC <YPos + 1
.SkipUpHigh
  DEC <YPos
  LDX <ScrollY
  DEX
  CPX #$EF
  BCC .SkipUpFlip
  LDA #$02
  EOR <ScreenSel
  STA <ScreenSel
  LDX #$EF
.SkipUpFlip
  STX <ScrollY
  LDX #YRowUp
  JSR RowSet
  JSR DrawCode
  JSR RowUp
  JSR DrawCode
.SkipUp

  LDA #$00
  STA PPUSCROLL
  LDA <ScrollY
  STA PPUSCROLL
  LDA #%10000000
  ORA <ScreenSel
  STA PPUCTRL
  LDA #%00001010
  STA PPUMASK
IRQ:
  RTI

GetJoypad:
  LDX #$01
  STX JOY1
  DEX
  STX JOY1
  STX <Joypad1
  LDX #$08
.Read
  LDA JOY1
  LSR A
  ROL <Joypad1
  DEX
  BNE .Read
  LDA <Joypad1
  AND #%00001100
  CMP #%00001100
  BNE .Return
  EOR <Joypad1
  STA <Joypad1
.Return
  RTS

IncDw:
  INC <$00, X
  BNE .SkipHigh
  INC <$01, X
.SkipHigh
  RTS
DecDw:
  LDA <$00, X
  BNE .SkipHigh
  DEC <$01, X
.SkipHigh
  DEC <$00, X
  RTS

RowSet:
  LDA <$00, X
  STA <YRow
  LDA <$01, X
  STA <YRow + 1
  RTS

RowDown:
  LDA <ScrollY
  AND #$07
  BNE .Return
  LDX #YRowDown
  JSR IncDw
  JSR RowSet
  LDX #YRowUp
  JSR IncDw
  CLC
  LDA <VramDst
  ADC #$20
  STA <VramDst
  LDA <VramDst + 1
  ADC #$00
  STA <VramDst + 1
  AND #$03
  CMP #$03
  BNE .Return
  LDA <VramDst
  CMP #$C0
  BCC .Return
  LDA #$02
  STA <VramDst
  LDA <VramDst + 1
  EOR #$0B
  STA <VramDst + 1
.Return
  RTS

RowUp:
  LDA <ScrollY
  AND #$07
  CMP #$07
  BNE .Return
  LDX #YRowDown
  JSR DecDw
  LDX #YRowUp
  JSR DecDw
  JSR RowSet
  SEC
  LDA <VramDst
  SBC #$20
  STA <VramDst
  LDA <VramDst + 1
  SBC #$00
  STA <VramDst + 1
  AND #$03
  CMP #$03
  BNE .Return
  LDA <VramDst
  CMP #$C0
  BCC .Return
  LDA #$A2
  STA <VramDst
  LDA <VramDst + 1
  ADC #$00
  EOR #$0B
  STA <VramDst + 1
.Return
  RTS

DrawCode:
  LDA <VramDst + 1
  STA PPUADDR
  LDA <VramDst
  STA PPUADDR
  LDA <YRow + 1
  BMI .DrawCode
.CheckBtm
  SEC
  LDA <YRow
  SBC #LOW(ChrRow)
  LDA <YRow + 1
  SBC #HIGH(ChrRow)
  BCC .CheckChr
  JMP DrawBottom
.CheckChr
  LDA <YRow
  SBC #LOW(TxtRow)
  LDA <YRow + 1
  SBC #HIGH(TxtRow)
  BCC .CheckText
  JMP DrawChr
.CheckText
  LDA <YRow
  SBC #LOW(IdxRow)
  LDA <YRow + 1
  SBC #HIGH(IdxRow)
  BCC .CheckIndex
  JMP DrawText
.CheckIndex
  LDA <YRow
  SBC #LOW(SrcRow)
  LDA <YRow + 1
  SBC #HIGH(SrcRow)
  BCC .DrawCode
  JMP DrawIndex
.DrawCode
  LDA #LOW(SrcIndex)
  STA <$00
  LDA #HIGH(SrcIndex)
  STA <$01
  LDA <YRow
  ASL A
  TAX
  LDA <YRow + 1
  ROL A
  TAY
  TXA
  CLC
  ADC <$00
  STA <$00
  TYA
  ADC <$01
  STA <$01
  LDY #$00
  LDA [$00], Y
  ASL A
  STA <$02
  INY
  LDA [$00], Y
  ROL A
  STA <$03
  LDA #LOW(SrcData)
  STA <$00
  LDA #HIGH(SrcData)
  STA <$01
  LDA <$02
  ASL A
  ROL <$03
  ASL A
  ROL <$03
  ASL A
  ROL <$03
  ASL A
  ROL <$03
  ADC <$00
  STA <$00
  LDA <$03
  ADC <$01
  STA <$01
  LDX #$1C
  LDY #$FF
.Draw
  DEX
  INY
  LDA [$00], Y
  STA PPUDATA
  BNE .Draw
  LDA #$00
.Clear
  STA PPUDATA
  DEX
  BNE .Clear
  RTS

DrawIndex:
  LDA #LOW(SrcIndex)
  STA <$00
  LDA #HIGH(SrcIndex)
  STA <$01
  LDA #$00
  STA <$02
  CLC
  LDA <YRow
  SBC #LOW(SrcRow)
  ASL A
  ROL <$02
  ASL A
  ROL <$02
  ASL A
  ROL <$02
  ADC <$00
  STA <$00
  LDA <$02
  ADC <$01
  STA <$01
  LDA #' '
  STA PPUDATA
  STA PPUDATA
  LDX #'.'
  STX PPUDATA
  LDX #'d'
  STX PPUDATA
  LDX #'w'
  STX PPUDATA
  STA PPUDATA
  LDA #$03
  STA <$03
  LDY #$00
.Loop
  LDA #'$'
  STA PPUDATA
  LDA [$00], Y
  INY
  STA <$04
  LDA [$00], Y
  INY
  ORA #$30
  STA PPUDATA
  LDA <$04
  LSR A
  LSR A
  LSR A
  LSR A
  TAX
  LDA HexChar, X
  STA PPUDATA
  LDA <$04
  AND #$0F
  TAX
  LDA HexChar, X
  STA PPUDATA
  DEC <$03
  BPL .Continue
  LDA #$00
  STA PPUDATA
  STA PPUDATA
  STA PPUDATA
  RTS
.Continue
  LDA #','
  STA PPUDATA
  BNE .Loop

DrawText:
  CLC
  LDA <YRow
  SBC #LOW(IdxRow+3)
  STA <$02
  LDA <YRow + 1
  SBC #HIGH(IdxRow+3)
  BCS .DrawText
  LDA <$02
  SBC #$FC
  JMP DrawEx
.DrawText
  STA <$03
  LDA #LOW(SrcData)
  STA <$00
  LDA #HIGH(SrcData)
  STA <$01
  LDA <$02
  ASL A
  ROL <$03
  ASL A
  ROL <$03
  ASL A
  ROL <$03
  ASL A
  ROL <$03
  ASL A
  ROL <$03
  ADC <$00
  STA <$00
  LDA <$03
  ADC <$01
  STA <$01

  LDA #' '
  STA PPUDATA
  STA PPUDATA
  LDX #'S'
  STX PPUDATA
  LDX #'D'
  STX PPUDATA
  STA PPUDATA
  LDX #$22
  STX PPUDATA
  LDA #$15
  STA <$04
  LDY #$00
.Loop
  LDA [$00], Y
  BEQ .Exit
  INY
  STA PPUDATA
  CMP #$5C
  BNE .Next
  STA PPUDATA
  DEC <$04
.Next
  DEC <$04
  BNE .Loop
.Exit
  LDA #$22
  STA PPUDATA
  LDX <$04
  BEQ .Return
  LDA #$00
.Clear
  STA PPUDATA
  DEX
  BNE .Clear
.Return
  RTS

DrawChr:
  CLC
  LDA <YRow
  SBC #LOW(TxtRow+5)
  STA <$02
  LDA <YRow + 1
  SBC #HIGH(TxtRow+5)
  BCS .DrawChr
  LDA <$02
  SBC #$F7
  JMP DrawEx
.DrawChr
  ASL <$02
  ROL A
  ASL <$02
  ROL A
  ASL <$02
  ROL A
  STA PPUADDR
  LDA <$02
  STA PPUADDR
  LDA PPUDATA ; dummy
  LDX #$00
  LDY #$08
.Read
  LDA PPUDATA
  STA <$00, X
  INX
  DEY
  BNE .Read
  LDA <VramDst + 1
  STA PPUADDR
  LDA <VramDst
  STA PPUADDR
  LDA #$20
  STA PPUDATA
  STA PPUDATA
  LDX #'C'
  STX PPUDATA
  INX
  STX PPUDATA
  LDX #$08
  STX <$09
  LDX #$00
.Write
  STA PPUDATA
  LDA <$00, X
  STA <$0A
  LSR A
  LSR A
  LSR A
  LSR A
  TAY
  LDA HexChar, Y
  STA PPUDATA
  LDA <$0A
  AND #$0F
  TAY
  LDA HexChar, Y
  STA PPUDATA
  LDA #','
  INX
  DEC <$09
  BNE .Write
  RTS

DrawBottom:
  LDA #$00
  LDX #$1C
.Loop
  STA PPUDATA
  DEX
  BNE .Loop
  RTS

DrawEx:
  ASL A
  ASL A
  ASL A
  ASL A
  ADC #LOW(.Exdata)
  STA <$00
  LDA #$00
  TAY
  ADC #HIGH(.Exdata)
  STA <$01
  LDX #$10
.Loop
  LDA [$00], Y
  INY
  STA PPUDATA
  DEX
  BNE .Loop
  LDA #$00
  LDX #$0C
.Clear
  STA PPUDATA
  DEX
  BNE .Clear
  RTS

  Align 16
.Exdata
  ; Blank line
  .db $00,$00,$00,$00
  .db $00,$00,$00,$00
  .db $00,$00,$00,$00
  .db $00,$00,$00,$00
  ;   Align 32
  .db $20,$20,$41,$6C
  .db $69,$67,$6E,$20
  .db $33,$32,$00,$00
  .db $00,$00,$00,$00
  ; SrcData:
  .db $53,$72,$63,$44
  .db $61,$74,$61,$3A
  .db $00,$00,$00,$00
  .db $00,$00,$00,$00
  ; Blank line
  .db $00,$00,$00,$00
  .db $00,$00,$00,$00
  .db $00,$00,$00,$00
  .db $00,$00,$00,$00
  ; ; CHR
  .db $3B,$20,$43,$48
  .db $52,$00,$00,$00
  .db $00,$00,$00,$00
  .db $00,$00,$00,$00
  ; ;---------------
  .db $3B,$2D,$2D,$2D
  .db $2D,$2D,$2D,$2D
  .db $2D,$2D,$2D,$2D
  .db $2D,$2D,$2D,$2D
  ;   .bank 2
  .db $20,$20,$2E,$62
  .db $61,$6E,$6B,$20
  .db $32,$00,$00,$00
  .db $00,$00,$00,$00
  ;   .org $0000
  .db $20,$20,$2E,$6F
  .db $72,$67,$20,$24
  .db $30,$30,$30,$30
  .db $00,$00,$00,$00

HexChar:
  .db $30,$31,$32,$33
  .db $34,$35,$36,$37
  .db $38,$39,$41,$42
  .db $43,$44,$45,$46

;--------------------

SD .macro
  .if $DFE0<=*
  .if *<$E000
  .bank 1
  .org $E000
  .endif
  .endif
  Align 32
  .db \1,0
  .endm

CD .macro
  .db $\1,$\2
  .db $\3,$\4
  .db $\5,$\6
  .db $\7,$\8
  .endm

  Align 2
  .dw 0,0,0,0,0,0,0,0
  .dw 0,0,0,0,0,0,0,0
  .dw 0,0,0,0,0,0,0,0
  .dw 0,0,0,0,0,0,0,0

SrcIndex:
  .dw $001,$002,$003,$001
  .dw $004,$005,$006,$007
  .dw $001,$000,$008,$009
  .dw $00A,$00B,$00C,$000
  .dw $00D,$00E,$00F,$010
  .dw $011,$012,$013,$014
  .dw $015,$016,$017,$018
  .dw $000,$019,$01A,$01B
  .dw $01C,$01D,$01E,$01F
  .dw $020,$021,$000,$022
  .dw $023,$024,$025,$026
  .dw $027,$028,$029,$000
  .dw $02A,$02B,$02C,$02D
  .dw $02E,$000,$02F,$030
  .dw $000,$031,$032,$033
  .dw $034,$000,$035,$036
  .dw $037,$038,$039,$03A
  .dw $03B,$03C,$03D,$03E
  .dw $03F,$040,$041,$042
  .dw $043,$044,$045,$046
  .dw $047,$048,$049,$04A
  .dw $000,$04B,$04C,$04D
  .dw $04E,$04C,$04F,$000
  .dw $050,$051,$000,$052
  .dw $053,$054,$055,$056
  .dw $057,$058,$059,$05A
  .dw $057,$049,$05B,$000
  .dw $05C,$05D,$000,$05E
  .dw $05F,$060,$061,$062
  .dw $053,$054,$063,$064
  .dw $064,$064,$064,$049
  .dw $065,$066,$067,$000
  .dw $03B,$068,$069,$06A
  .dw $06B,$06C,$06D,$05F
  .dw $06E,$06F,$070,$000
  .dw $071,$072,$03B,$073
  .dw $074,$075,$076,$077
  .dw $000,$078,$079,$07A
  .dw $07B,$000,$07C,$07D
  .dw $000,$031,$07E,$07F
  .dw $03B,$079,$080,$000
  .dw $081,$082,$083,$084
  .dw $085,$086,$087,$088
  .dw $089,$08A,$08B,$08C
  .dw $08D,$08E,$047,$08F
  .dw $090,$091,$092,$093
  .dw $094,$095,$096,$097
  .dw $098,$074,$075,$074
  .dw $099,$082,$09A,$09B
  .dw $09C,$09D,$09B,$09C
  .dw $09E,$09F,$0A0,$0A1
  .dw $08E,$049,$0A2,$0A3
  .dw $091,$092,$093,$0A4
  .dw $0A5,$096,$0A6,$098
  .dw $074,$0A7,$074,$0A8
  .dw $000,$03B,$0A9,$0AA
  .dw $0A9,$0AB,$0AC,$079
  .dw $0AD,$080,$0AE,$0AF
  .dw $000,$0B0,$0B1,$0B2
  .dw $049,$0B2,$0B3,$0B4
  .dw $0B5,$0B6,$0B7,$0B8
  .dw $049,$0B9,$082,$0BA
  .dw $0BB,$0BC,$0BD,$0BE
  .dw $0BF,$0C0,$000,$0C1
  .dw $0C2,$0C3,$0C4,$0C5
  .dw $0C0,$0C6,$0C7,$0C3
  .dw $0C8,$0C5,$0C9,$0C0
  .dw $000,$0CA,$0C7,$0CB
  .dw $0CC,$0CD,$0C0,$000
  .dw $0CE,$0AA,$0CF,$0BC
  .dw $097,$0D0,$098,$0A6
  .dw $0D0,$0D1,$0D2,$0D3
  .dw $070,$0D4,$0D5,$06E
  .dw $0D6,$0D7,$0BC,$0D2
  .dw $0D8,$0D9,$091,$070
  .dw $0D4,$0DA,$06E,$0BF
  .dw $0C0,$000,$0DB,$0AA
  .dw $0CF,$0DC,$0BC,$097
  .dw $0DD,$0A6,$0DD,$098
  .dw $0DE,$0D2,$0DF,$070
  .dw $0D4,$0E0,$06E,$0D6
  .dw $0D7,$0BC,$0D2,$0D8
  .dw $0D9,$0E1,$070,$0D4
  .dw $0D5,$0DA,$06E,$0BF
  .dw $0C0,$000,$0E2,$0D4
  .dw $053,$0D2,$053,$0E3
  .dw $0E4,$0E5,$0DE,$0E6
  .dw $0E7,$0E3,$0E8,$0E9
  .dw $0EA,$0EB,$0E6,$0EC
  .dw $0E3,$0ED,$0EE,$0EF
  .dw $0F0,$0E6,$0F1,$0E3
  .dw $0F2,$0F3,$0F4,$0F5
  .dw $0E6,$0F6,$0E3,$0F7
  .dw $0F8,$0F9,$0FA,$0FB
  .dw $0FC,$0FD,$0FE,$0E6
  .dw $0FF,$03C,$0E3,$100
  .dw $03D,$101,$0D1,$102
  .dw $0FC,$103,$104,$0FE
  .dw $105,$106,$0FF,$107
  .dw $108,$106,$100,$109
  .dw $10A,$0FC,$10B,$0FE
  .dw $10C,$0FF,$10D,$0FF
  .dw $10D,$0FF,$10D,$0FF
  .dw $10D,$102,$0FC,$10E
  .dw $104,$0FE,$10F,$110
  .dw $111,$049,$108,$106
  .dw $057,$112,$03B,$113
  .dw $057,$049,$114,$0C0
  .dw $000,$115,$0FB,$0FC
  .dw $0FD,$0FE,$03B,$107
  .dw $0D1,$0E6,$0F6,$0FF
  .dw $116,$0FF,$116,$0FF
  .dw $116,$102,$0FC,$10C
  .dw $104,$0FE,$117,$057
  .dw $057,$118,$058,$119
  .dw $058,$11A,$058,$057
  .dw $11B,$109,$105,$11C
  .dw $11D,$057,$106,$108
  .dw $11E,$106,$108,$11F
  .dw $057,$120,$0B7,$0B7
  .dw $0B7,$0B7,$03C,$121
  .dw $057,$120,$122,$03C
  .dw $121,$057,$123,$124
  .dw $03B,$057,$057,$057
  .dw $0C0,$125,$126,$057
  .dw $127,$000,$128,$0D1
  .dw $0E6,$129,$107,$0E3
  .dw $12A,$12B,$10C,$12C
  .dw $12D,$12E,$109,$10A
  .dw $0FC,$10B,$0FE,$10C
  .dw $0FF,$10D,$0FF,$10D
  .dw $0FF,$10D,$0FF,$10D
  .dw $0FF,$10D,$102,$0FC
  .dw $10E,$104,$0FE,$000
  .dw $117,$057,$057,$12F
  .dw $058,$130,$058,$057
  .dw $131,$058,$132,$11E
  .dw $105,$11C,$106,$133
  .dw $108,$057,$134,$135
  .dw $057,$136,$137,$136
  .dw $127,$138,$139,$057
  .dw $13A,$13B,$03B,$113
  .dw $057,$049,$114,$0BF
  .dw $0C0,$000,$13C,$0D1
  .dw $0E6,$13D,$107,$0E3
  .dw $13E,$13F,$10C,$140
  .dw $12D,$141,$142,$100
  .dw $142,$100,$142,$100
  .dw $053,$10C,$053,$143
  .dw $094,$144,$0B5,$145
  .dw $03F,$047,$146,$0B9
  .dw $0D4,$053,$0D2,$053
  .dw $05F,$057,$057,$147
  .dw $058,$047,$058,$0B4
  .dw $148,$094,$149,$057
  .dw $0C7,$14A,$0B7,$0B7
  .dw $0B7,$0B7,$03D,$14B
  .dw $057,$14C,$122,$03D
  .dw $14B,$057,$126,$047
  .dw $14D,$14E,$0C0,$000
  .dw $14F,$03B,$10F,$11C
  .dw $057,$049,$127,$0C0
  .dw $000,$150,$0FF,$0FF
  .dw $0FF,$0FF,$151,$0FC
  .dw $03B,$03D,$152,$0FE
  .dw $153,$11C,$106,$108
  .dw $057,$049,$127,$03B
  .dw $154,$113,$057,$049
  .dw $114,$0C0,$000,$155
  .dw $156,$157,$158,$158
  .dw $158,$158,$159,$15A
  .dw $15B,$15C,$158,$15D
  .dw $15E,$15F,$158,$158
  .dw $157,$158,$158,$158
  .dw $158,$160,$161,$162
  .dw $158,$158,$163,$164
  .dw $165,$165,$165,$166
  .dw $167,$168,$169,$158
  .dw $16A,$16B,$16C,$16D
  .dw $158,$000,$16E,$16F
  .dw $170,$171,$172,$000
  .dw $173,$000,$174,$175
  .dw $176,$032,$177,$178
  .dw $178,$179,$17A,$02E
  .dw $000,$17B,$17C,$17D
  .dw $17E,$17F,$02E,$000
  .dw $180,$181,$181,$181
  .dw $181,$000,$182,$000

  Align 32
SrcData:
  SD ""
  SD ";-------------------;"
  SD ";  QuiNES           ;"
  SD ";     Quine of NES  ;"
  SD ";version:           ;"
  SD ";  1.00 - 2021/10/20;"
  SD ";build:             ;"
  SD ";  nesasm QuiNES.asm;"
  SD "; iNES header"
  SD "  .inesprg 1"
  SD "  .ineschr 1"
  SD "  .inesmir 0"
  SD "  .inesmap 0"
  SD "; I/O registers"
  SD "PPUCTRL     = $2000"
  SD "PPUMASK     = $2001"
  SD "PPUSTATUS   = $2002"
  SD "OAMADDR     = $2003"
  SD "OAMDATA     = $2004"
  SD "PPUSCROLL   = $2005"
  SD "PPUADDR     = $2006"
  SD "PPUDATA     = $2007"
  SD "OAMDMA      = $4014"
  SD "JOY1        = $4016"
  SD "JOY2        = $4017"
  SD "; Memory map"
  SD "ScreenSel   = $10"
  SD "Joypad1     = $11"
  SD "YPos        = $12"
  SD "ScrollY     = $14"
  SD "YRow        = $16"
  SD "YRowDown    = $18"
  SD "YRowUp      = $1A"
  SD "VramDst     = $1C"
  SD "; Define"
  SD "PalBlack    = $0F"
  SD "PalWhite    = $30"
  SD "SrcRow      = 751 - 1"
  SD "IdxRow      = 939 - 1"
  SD "TxtRow      = 1329- 1"
  SD "ChrRow      = 1590"
  SD "ScrStop     = 1609"
  SD "; Macro"
  SD "Cs .func (\\1-*%\\1)"
  SD "Align .macro"
  SD "  .org *+Cs(\\1)%\\1"
  SD "  .endm"
  SD "; PRG"
  SD ";---------------"
  SD "; Interrupt"
  SD "  .bank 1"
  SD "  .org $FFFA"
  SD "  .dw NMI, RST, IRQ"
  SD "; Reset"
  SD "  .bank 0"
  SD "  .org $C000"
  SD "RST:"
  SD "  SEI"
  SD "  CLD"
  SD "  LDA #$00"
  SD "  TAX"
  SD "  TAY"
  SD ".InitMem"
  SD "  STA <$00, X"
  SD "  PHA"
  SD "  STA $0200, X"
  SD "  STA $0300, X"
  SD "  STA $0400, X"
  SD "  STA $0500, X"
  SD "  STA $0600, X"
  SD "  STA $0700, X"
  SD "  INX"
  SD "  BNE .InitMem"
  SD "  DEX"
  SD "  TXS"
  SD ".WVBL1"
  SD "  BIT PPUSTATUS"
  SD "  BPL .WVBL1"
  SD ".WVBL2"
  SD "  BPL .WVBL2"
  SD "  STY PPUCTRL"
  SD "  STY PPUMASK"
  SD "  LDA #$3F"
  SD "  STA PPUADDR"
  SD "  STY PPUADDR"
  SD "  LDA #PalBlack"
  SD "  LDX #PalWhite"
  SD "  STA PPUDATA"
  SD "  STX PPUDATA"
  SD "  LDX #$1E"
  SD ".InitPal"
  SD "  BNE .InitPal"
  SD "  STY OAMADDR"
  SD "  STY OAMDMA"
  SD "  INC <$00"
  SD "  LDA #$20"
  SD "  .db $2C ; BIT abs"
  SD ".InitNtBtm"
  SD "  LDA #$28"
  SD ".InitNt"
  SD "  STY PPUDATA"
  SD "  BNE .InitNt"
  SD "  DEC <$00"
  SD "  BEQ .InitNtBtm"
  SD "  STA <YRowDown"
  SD "  STA <YRowDown + 1"
  SD "  LDA #$C4"
  SD "  STA <YRowUp"
  SD "  LDA #$FF"
  SD "  STA <YRowUp + 1"
  SD "  STA <VramDst + 1"
  SD "  LDA #$42"
  SD "  STA <VramDst"
  SD "  LDA #$2D"
  SD "  STA <$0F"
  SD ".InitDrawCode"
  SD "  JSR DrawCode"
  SD "  JSR RowDown"
  SD "  DEC <$0F"
  SD "  BNE .InitDrawCode"
  SD "  LDA #%10001000"
  SD "  STA PPUCTRL"
  SD "  LDA #%01000000"
  SD "  STA JOY2"
  SD ".InfLoop"
  SD "  JMP .InfLoop"
  SD "NMI:"
  SD "  LDA PPUSTATUS"
  SD "  STA PPUMASK"
  SD "  JSR GetJoypad"
  SD "  LDA <Joypad1"
  SD "  AND #%00000100"
  SD "  BEQ .SkipDown"
  SD "  LDA <YRowDown"
  SD "  CMP #LOW(ScrStop)"
  SD "  LDA <YRowDown + 1"
  SD "  SBC #HIGH(ScrStop)"
  SD "  BCS .SkipDown"
  SD "  INC <YPos"
  SD "  BNE .SkipDownHigh"
  SD "  INC <YPos + 1"
  SD ".SkipDownHigh"
  SD "  LDX <ScrollY"
  SD "  CPX #$F0"
  SD "  BCC .SkipDownFlip"
  SD "  LDA #$02"
  SD "  EOR <ScreenSel"
  SD "  STA <ScreenSel"
  SD "  LDX #$00"
  SD ".SkipDownFlip"
  SD "  STX <ScrollY"
  SD "  LDX #YRowDown"
  SD "  JSR RowSet"
  SD ".SkipDown"
  SD "  AND #%00001000"
  SD "  BEQ .SkipUp"
  SD "  LDA <YPos"
  SD "  ORA <YPos + 1"
  SD "  BNE .SkipUpHigh"
  SD "  DEC <YPos + 1"
  SD ".SkipUpHigh"
  SD "  DEC <YPos"
  SD "  CPX #$EF"
  SD "  BCC .SkipUpFlip"
  SD "  LDX #$EF"
  SD ".SkipUpFlip"
  SD "  LDX #YRowUp"
  SD "  JSR RowUp"
  SD ".SkipUp"
  SD "  STA PPUSCROLL"
  SD "  LDA <ScrollY"
  SD "  LDA #%10000000"
  SD "  ORA <ScreenSel"
  SD "  LDA #%00001010"
  SD "IRQ:"
  SD "  RTI"
  SD "GetJoypad:"
  SD "  LDX #$01"
  SD "  STX JOY1"
  SD "  STX <Joypad1"
  SD "  LDX #$08"
  SD ".Read"
  SD "  LDA JOY1"
  SD "  LSR A"
  SD "  ROL <Joypad1"
  SD "  BNE .Read"
  SD "  AND #%00001100"
  SD "  CMP #%00001100"
  SD "  BNE .Return"
  SD "  EOR <Joypad1"
  SD "  STA <Joypad1"
  SD ".Return"
  SD "  RTS"
  SD "IncDw:"
  SD "  INC <$00, X"
  SD "  BNE .SkipHigh"
  SD "  INC <$01, X"
  SD ".SkipHigh"
  SD "DecDw:"
  SD "  LDA <$00, X"
  SD "  DEC <$01, X"
  SD "  DEC <$00, X"
  SD "RowSet:"
  SD "  STA <YRow"
  SD "  LDA <$01, X"
  SD "  STA <YRow + 1"
  SD "RowDown:"
  SD "  AND #$07"
  SD "  JSR IncDw"
  SD "  CLC"
  SD "  LDA <VramDst"
  SD "  ADC #$20"
  SD "  LDA <VramDst + 1"
  SD "  ADC #$00"
  SD "  AND #$03"
  SD "  CMP #$03"
  SD "  CMP #$C0"
  SD "  BCC .Return"
  SD "  EOR #$0B"
  SD "RowUp:"
  SD "  CMP #$07"
  SD "  JSR DecDw"
  SD "  SEC"
  SD "  SBC #$20"
  SD "  SBC #$00"
  SD "  LDA #$A2"
  SD "DrawCode:"
  SD "  LDA <YRow + 1"
  SD "  BMI .DrawCode"
  SD ".CheckBtm"
  SD "  LDA <YRow"
  SD "  SBC #LOW(ChrRow)"
  SD "  SBC #HIGH(ChrRow)"
  SD "  BCC .CheckChr"
  SD "  JMP DrawBottom"
  SD ".CheckChr"
  SD "  SBC #LOW(TxtRow)"
  SD "  SBC #HIGH(TxtRow)"
  SD "  BCC .CheckText"
  SD "  JMP DrawChr"
  SD ".CheckText"
  SD "  SBC #LOW(IdxRow)"
  SD "  SBC #HIGH(IdxRow)"
  SD "  BCC .CheckIndex"
  SD "  JMP DrawText"
  SD ".CheckIndex"
  SD "  SBC #LOW(SrcRow)"
  SD "  SBC #HIGH(SrcRow)"
  SD "  BCC .DrawCode"
  SD "  JMP DrawIndex"
  SD ".DrawCode"
  SD "  LDA #LOW(SrcIndex)"
  SD "  STA <$00"
  SD "  LDA #HIGH(SrcIndex)"
  SD "  STA <$01"
  SD "  ASL A"
  SD "  ROL A"
  SD "  TXA"
  SD "  ADC <$00"
  SD "  TYA"
  SD "  ADC <$01"
  SD "  LDY #$00"
  SD "  LDA [$00], Y"
  SD "  STA <$02"
  SD "  INY"
  SD "  STA <$03"
  SD "  LDA #LOW(SrcData)"
  SD "  LDA #HIGH(SrcData)"
  SD "  LDA <$02"
  SD "  ROL <$03"
  SD "  LDA <$03"
  SD "  LDX #$1C"
  SD "  LDY #$FF"
  SD ".Draw"
  SD "  BNE .Draw"
  SD ".Clear"
  SD "  BNE .Clear"
  SD "DrawIndex:"
  SD "  ROL <$02"
  SD "  LDA #' '"
  SD "  LDX #'.'"
  SD "  LDX #'d'"
  SD "  LDX #'w'"
  SD "  LDA #$03"
  SD ".Loop"
  SD "  LDA #'$'"
  SD "  STA <$04"
  SD "  ORA #$30"
  SD "  LDA <$04"
  SD "  LDA HexChar, X"
  SD "  AND #$0F"
  SD "  DEC <$03"
  SD "  BPL .Continue"
  SD ".Continue"
  SD "  LDA #','"
  SD "  BNE .Loop"
  SD "DrawText:"
  SD "  SBC #LOW(IdxRow+3)"
  SD "  SBC #HIGH(IdxRow+3)"
  SD "  BCS .DrawText"
  SD "  SBC #$FC"
  SD "  JMP DrawEx"
  SD ".DrawText"
  SD "  LDX #'S'"
  SD "  LDX #'D'"
  SD "  LDX #$22"
  SD "  LDA #$15"
  SD "  BEQ .Exit"
  SD "  CMP #$5C"
  SD "  BNE .Next"
  SD "  DEC <$04"
  SD ".Next"
  SD ".Exit"
  SD "  LDA #$22"
  SD "  LDX <$04"
  SD "  BEQ .Return"
  SD "DrawChr:"
  SD "  SBC #LOW(TxtRow+5)"
  SD "  SBC #HIGH(TxtRow+5)"
  SD "  BCS .DrawChr"
  SD "  SBC #$F7"
  SD ".DrawChr"
  SD "  ASL <$02"
  SD "  LDA PPUDATA ; dummy"
  SD "  LDY #$08"
  SD "  LDA PPUDATA"
  SD "  DEY"
  SD "  LDX #'C'"
  SD "  STX <$09"
  SD ".Write"
  SD "  STA <$0A"
  SD "  LDA HexChar, Y"
  SD "  LDA <$0A"
  SD "  DEC <$09"
  SD "  BNE .Write"
  SD "DrawBottom:"
  SD "DrawEx:"
  SD "  ADC #LOW(.Exdata)"
  SD "  ADC #HIGH(.Exdata)"
  SD "  LDX #$10"
  SD "  LDX #$0C"
  SD "  Align 16"
  SD ".Exdata"
  SD "  ; Blank line"
  SD "  .db $00,$00,$00,$00"
  SD "  ;   Align 32"
  SD "  .db $20,$20,$41,$6C"
  SD "  .db $69,$67,$6E,$20"
  SD "  .db $33,$32,$00,$00"
  SD "  ; SrcData:"
  SD "  .db $53,$72,$63,$44"
  SD "  .db $61,$74,$61,$3A"
  SD "  ; ; CHR"
  SD "  .db $3B,$20,$43,$48"
  SD "  .db $52,$00,$00,$00"
  SD "  ; ;---------------"
  SD "  .db $3B,$2D,$2D,$2D"
  SD "  .db $2D,$2D,$2D,$2D"
  SD "  ;   .bank 2"
  SD "  .db $20,$20,$2E,$62"
  SD "  .db $61,$6E,$6B,$20"
  SD "  .db $32,$00,$00,$00"
  SD "  ;   .org $0000"
  SD "  .db $20,$20,$2E,$6F"
  SD "  .db $72,$67,$20,$24"
  SD "  .db $30,$30,$30,$30"
  SD "HexChar:"
  SD "  .db $30,$31,$32,$33"
  SD "  .db $34,$35,$36,$37"
  SD "  .db $38,$39,$41,$42"
  SD "  .db $43,$44,$45,$46"
  SD ";--------------------"
  SD "SD .macro"
  SD "  .if $DFE0<=*"
  SD "  .if *<$E000"
  SD "  .org $E000"
  SD "  .endif"
  SD "  Align 32"
  SD "  .db \\1,0"
  SD "CD .macro"
  SD "  .db $\\1,$\\2"
  SD "  .db $\\3,$\\4"
  SD "  .db $\\5,$\\6"
  SD "  .db $\\7,$\\8"
  SD "  Align 2"
  SD "  .dw 0,0,0,0,0,0,0,0"
  SD "SrcIndex:"

; CHR
;---------------
  .bank 2
  .org $0000
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 18,3C,3C,3C,18,18,00,18
  CD 00,00,00,00,00,00,00,00
  CD 14,14,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 28,28,7C,28,7C,28,28,00
  CD 00,00,00,00,00,00,00,00
  CD 10,38,50,38,14,38,10,00
  CD 00,00,00,00,00,00,00,00
  CD 42,A4,A8,54,2A,4A,84,00
  CD 00,00,00,00,00,00,00,00
  CD 30,48,50,20,54,48,34,00
  CD 00,00,00,00,00,00,00,00
  CD 10,10,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 08,10,20,20,20,10,08,00
  CD 00,00,00,00,00,00,00,00
  CD 20,10,08,08,08,10,20,00
  CD 00,00,00,00,00,00,00,00
  CD 00,54,38,10,38,54,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,10,10,7C,10,10,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,18,18,08
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,7E,7E,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,18,18,00
  CD 00,00,00,00,00,00,00,00
  CD 04,04,08,08,10,10,20,20
  CD 00,00,00,00,00,00,00,00
  CD 38,4C,C6,C6,C6,64,38,00
  CD 00,00,00,00,00,00,00,00
  CD 18,38,18,18,18,18,7E,00
  CD 00,00,00,00,00,00,00,00
  CD 7C,C6,0E,3C,78,E0,FE,00
  CD 00,00,00,00,00,00,00,00
  CD 7E,0C,18,3C,06,C6,7C,00
  CD 00,00,00,00,00,00,00,00
  CD 1C,3C,6C,CC,FE,0C,0C,00
  CD 00,00,00,00,00,00,00,00
  CD FC,C0,FC,06,06,C6,7C,00
  CD 00,00,00,00,00,00,00,00
  CD 3C,60,C0,FC,C6,C6,7C,00
  CD 00,00,00,00,00,00,00,00
  CD FE,C6,0C,18,30,30,30,00
  CD 00,00,00,00,00,00,00,00
  CD 7C,C6,C6,7C,C6,C6,7C,00
  CD 00,00,00,00,00,00,00,00
  CD 7C,C6,C6,7E,06,0C,78,00
  CD 00,00,00,00,00,00,00,00
  CD 00,10,00,00,10,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,10,00,00,10,10,20,00
  CD 00,00,00,00,00,00,00,00
  CD 04,08,10,20,10,08,04,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,3C,00,3C,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 20,10,08,04,08,10,20,00
  CD 00,00,00,00,00,00,00,00
  CD 00,7C,C6,06,1E,18,00,18
  CD 00,00,00,00,00,00,00,00
  CD 3C,42,9A,AA,94,40,3E,00
  CD 00,00,00,00,00,00,00,00
  CD 38,6C,C6,C6,FE,C6,C6,00
  CD 00,00,00,00,00,00,00,00
  CD FC,C6,C6,FC,C6,C6,FC,00
  CD 00,00,00,00,00,00,00,00
  CD 3C,66,C0,C0,C0,66,3C,00
  CD 00,00,00,00,00,00,00,00
  CD F8,CC,C6,C6,C6,CC,F8,00
  CD 00,00,00,00,00,00,00,00
  CD FE,C0,C0,FC,C0,C0,FE,00
  CD 00,00,00,00,00,00,00,00
  CD FE,C0,C0,FC,C0,C0,C0,00
  CD 00,00,00,00,00,00,00,00
  CD 3E,60,C0,CE,C6,66,3E,00
  CD 00,00,00,00,00,00,00,00
  CD C6,C6,C6,FE,C6,C6,C6,00
  CD 00,00,00,00,00,00,00,00
  CD 7E,18,18,18,18,18,7E,00
  CD 00,00,00,00,00,00,00,00
  CD 1E,06,06,06,C6,C6,7C,00
  CD 00,00,00,00,00,00,00,00
  CD C6,CC,D8,F0,F8,DC,CE,00
  CD 00,00,00,00,00,00,00,00
  CD 60,60,60,60,60,60,7E,00
  CD 00,00,00,00,00,00,00,00
  CD C6,EE,FE,FE,D6,C6,C6,00
  CD 00,00,00,00,00,00,00,00
  CD C6,E6,F6,FE,DE,CE,C6,00
  CD 00,00,00,00,00,00,00,00
  CD 7C,C6,C6,C6,C6,C6,7C,00
  CD 00,00,00,00,00,00,00,00
  CD FC,C6,C6,C6,FC,C0,C0,00
  CD 00,00,00,00,00,00,00,00
  CD 7C,C6,C6,C6,DE,CC,7A,00
  CD 00,00,00,00,00,00,00,00
  CD FC,C6,C6,CE,F8,DC,CE,00
  CD 00,00,00,00,00,00,00,00
  CD 78,CC,C0,7C,06,C6,7C,00
  CD 00,00,00,00,00,00,00,00
  CD 7E,18,18,18,18,18,18,00
  CD 00,00,00,00,00,00,00,00
  CD C6,C6,C6,C6,C6,C6,7C,00
  CD 00,00,00,00,00,00,00,00
  CD C6,C6,C6,EE,7C,38,10,00
  CD 00,00,00,00,00,00,00,00
  CD C6,C6,D6,FE,FE,EE,C6,00
  CD 00,00,00,00,00,00,00,00
  CD C6,EE,7C,38,7C,EE,C6,00
  CD 00,00,00,00,00,00,00,00
  CD 66,66,66,3C,18,18,18,00
  CD 00,00,00,00,00,00,00,00
  CD FE,0E,1C,38,70,E0,FE,00
  CD 00,00,00,00,00,00,00,00
  CD 18,10,10,10,10,10,18,00
  CD 00,00,00,00,00,00,00,00
  CD 20,20,10,10,08,08,04,04
  CD 00,00,00,00,00,00,00,00
  CD 18,08,08,08,08,08,18,00
  CD 00,00,00,00,00,00,00,00
  CD 08,14,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,38,00
  CD 00,00,00,00,00,00,00,00
  CD 10,08,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,7C,0C,7C,CC,7E,00
  CD 00,00,00,00,00,00,00,00
  CD 00,60,60,7C,66,66,7C,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,3C,66,60,66,3C,00
  CD 00,00,00,00,00,00,00,00
  CD 00,06,06,3E,66,66,3E,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,3C,66,7E,60,3C,00
  CD 00,00,00,00,00,00,00,00
  CD 00,3C,30,7E,30,30,30,00
  CD 00,00,00,00,00,00,00,00
  CD 00,03,3E,66,7E,06,66,3C
  CD 00,00,00,00,00,00,00,00
  CD 00,60,60,7C,66,66,66,00
  CD 00,00,00,00,00,00,00,00
  CD 00,18,00,18,18,18,18,00
  CD 00,00,00,00,00,00,00,00
  CD 00,0C,00,0C,0C,0C,6C,38
  CD 00,00,00,00,00,00,00,00
  CD 00,60,6C,68,78,7C,6C,00
  CD 00,00,00,00,00,00,00,00
  CD 00,18,18,18,18,18,18,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,7C,7E,56,56,56,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,7C,66,66,66,66,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,3C,66,66,66,3C,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,7C,66,66,7C,60,60
  CD 00,00,00,00,00,00,00,00
  CD 00,00,3E,66,66,3E,06,06
  CD 00,00,00,00,00,00,00,00
  CD 00,00,60,7C,76,60,60,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,3C,60,3C,0E,3C,00
  CD 00,00,00,00,00,00,00,00
  CD 00,30,30,7C,30,34,18,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,66,66,66,66,3E,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,66,66,24,3C,18,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,6A,6A,6A,7E,34,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,74,38,1C,2E,46,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,66,66,3C,18,38,30
  CD 00,00,00,00,00,00,00,00
  CD 00,00,7E,0C,18,30,7E,00
  CD 00,00,00,00,00,00,00,00
  CD 18,10,10,20,10,10,18,00
  CD 00,00,00,00,00,00,00,00
  CD 10,10,10,10,10,10,10,00
  CD 00,00,00,00,00,00,00,00
  CD 18,08,08,04,08,08,18,00
  CD 00,00,00,00,00,00,00,00
  CD 32,4C,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
  CD 00,00,00,00,00,00,00,00
