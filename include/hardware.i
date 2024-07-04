; Amiga hardware registers OCS-ECS-AGA
; Still incomplete! I add new registers/bits when I use them.

CIAB      EQU $bfd000 ;*NSTD
CIAA      EQU $bfe000 ;*NSTD
CIAAPRA   EQU $001 ;CIA port A

;-------------------- BEGIN CIAAPRA bits --------------------------------------
FIR1      EQU (7) ;Right mouse button / Fire button 2
FIR0      EQU (6) ;Left mouse button / Fire button 1
;-------------------- END CIAAPRA bits ----------------------------------------

CUSTOM    EQU $dff000 ;***** NO STANDARD (maybe)
BLTDDAT   EQU $000 ;ER - Blitter destination early read (dummy address)
DMACONR   EQU $002 ;R - DMA control and blitter status read
;-------------------- BEGIN DMACONR bits --------------------------------------
CUSTOMSET EQU $8000 ;Bit 15 set: subsequent set bits will be set *NSTD
CUSTOMCLR EQU $0000 ;Bit 15 clear: subsequent set bits will be cleared *NSTD
BBUSY     EQU $4000 ;Bit 14: Blitter busy status bit (read only)
BZERO     EQU $2000 ;Bit 13: Blitter logic zero status bit (read only)
BLTPRI    EQU $0400 ;Bit 10: Blitter DMA priority (over CPU micro)
                      ;(also called "blitter nasty") (disables /BLS pin,
                      ;preventing micro from stealing any bus cycles while
                      ;blitter DMA is running)
DMAEN     EQU $0200 ;Bit 9: Enable all DMA below (also UHRES DMA)
BPLEN     EQU $0100 ;Bit 8: Bit plane DMA enable
COPEN     EQU $0080 ;Bit 7: Coprocessor DMA enable
BLTEN     EQU $0040 ;Bit 6: Blitter DMA enable
SPREN     EQU $0020 ;Bit 5: Sprite DMA enable
DSKEN     EQU $0010 ;Bit 4: Disk DMA enable
AUD3EN    EQU $0008 ;Bit 3: Audio channel 3 DMA enable
AUD2EN    EQU $0004 ;Bit 2: Audio channel 2 DMA enable
AUD1EN    EQU $0002 ;Bit 1: Audio channel 1 DMA enable
AUD0EN    EQU $0001 ;Bit 0: Audio channel 4 DMA enable
AUDxEN    EQU $0007 ;All audio channels ***** NSTD
;-------------------- END DMACONR bits ----------------------------------------
VPOSR     EQU $004 ;R - Read vertical most sig. bits (and frame flop)
VPOSRH    EQU $004 ; ***** NOT STANDARD (maybe)
VPOSRL    EQU $005 ; ***** NOT STANDARD (maybe)
VHPOSR    EQU $006 ;R - Read vertical and horizontal position of beam
ADKCONR   EQU $010 ;R - Audio, disk control and UART register read
POT0DAT   EQU $012 ;R - Pot counter data left pair (vert, horiz)
POT1DAT   EQU $014 ;R - Pot counter data right pair (vert, horiz)
POTINP    EQU $016 ;R - Pot pin data read
;------------------- BEGIN POTINP bits ---------------------------------------
OUTRY     EQU 15 ;Output enable for Paula pin 33
DATRY     EQU 14 ;I/O data Paula pin 33
OUTRX     EQU 13 ;Output enable for Paula pin 32
DATRX     EQU 12 ;I/O data Paula pin 32
OUTLY     EQU 11 ;Out put enable for Paula pin 36
DATLY     EQU 10 ;I/O data Paula pin 36
OUTLX     EQU 9 ;Output enable for Paula pin 35
DATLX     EQU 8 ;I/O data Paula pin 35
START     EQU 1 ;Start pots (dump capacitors, start counters)
;-------------------- END POTINP bits ----------------------------------------
INTENAR   EQU $01c ;R - Interrupt enable bits read
;------------------- BEGIN INTENAR bits ---------------------------------------
INTEN     EQU $4000 ;Master interrupt (enable only, no request)
INTDIS    EQU $3fff ;Disable all interrupts (INTEN cant be cleared) *NSTD
EXTER     EQU $2000 ;Level 6: External interrupt
DSKSYN    EQU $1000 ;Level 5: Disk sync register (DSKSYNC) matches disk
RBF       EQU $0800 ;Level 5: Serial port receive buffer full
AUD3      EQU $0400 ;Level 4: Audio channel 3 block finished
AUD2      EQU $0200 ;Level 4: Audio channel 2 block finished
AUD1      EQU $0100 ;Level 4: Audio channel 1 block finished
AUD0      EQU $0080 ;Level 4:	Audio channel 0 block finished
BLIT      EQU $0040 ;Level 3: Blitter has finished
VERTB     EQU $0020 ;Level 3: Start of vertical blank
COPER     EQU $0010 ;Level 3: Copper
PORTS     EQU $0008 ;Level 2: I/O Ports and timers
SOFT      EQU $0004 ;Level 1: Reserved for software initiated interrupt
DSKBLK    EQU $0002 ;Level 1: Disk block finished
TBE       EQU $0001 ;Level 1: Serial port transmit buffer empty
;------------------- END INTENAR bits -----------------------------------------
INTREQR   EQU $01e ;R - Interrupt request bits read
                     ;--- bits: see INTENAR
INTREQRH  EQU $01e ;R - Interrupt request bits read, high byte
INTREQRL  EQU $01f ;R - Interrupt request bits read, low byte
REFPTR    EQU $028 ;W - Refresh pointer
VPOSW     EQU $02a ;W - Write vert most sig. bits (and frame flop)
VHPOSW    EQU $02c ;W - Write vert and horiz pos of beam
COPCON    EQU $02e ;W - Coprocessor control
POTGO     EQU $034 ;W - Pot count start, pot pin drive enable data
STREQ     EQU $038 ;S - Strobe for horiz sync with VB and EQU
STRVBL    EQU $03a ;S - Strobe for horiz sync with VB (vert blank)
STRHOR    EQU $03c ;S - Strobe for horiz sync
STRLONG   EQU $03e ;S - Strobe for identification of long horiz line
BLTCON0   EQU $040 ;W - Blitter control register 0
;------------------- BEGIN BLTCON0 bits ---------------------------------------
;                    Area mode (BLTCON1 bit 0 = 0)
ASH3      EQU $8000 ;Shift value of A source
ASH2      EQU $4000
ASH1      EQU $2000
ASH0      EQU $1000
USEA      EQU $0800 ;Mode control bit to use source A
USEB      EQU $0400 ;Mode control bit to use source B
USEC      EQU $0200 ;Mode control bit to use source C
USED      EQU $0100 ;Mode control bit to use source D
LF7       EQU $0080 ;Logic function minterm select lines
LF6       EQU $0040
LF5       EQU $0020
LF4       EQU $0010
LF3       EQU $0008
LF2       EQU $0004
LF1       EQU $0002
LF0       EQU $0001
;                    Line mode (BLTCON0 bit 0 = 1)
START3    EQU $8000 ;Starting point of line (0 thru 15 hex)
START2    EQU $4000
START1    EQU $2000
START0    EQU $1000
                      ;LFx apply here, choose $4a in line mode
;------------------- END BLTCON0 bits -----------------------------------------
BLTCON1   EQU $042 ;W - Blitter control register 1
;------------------- BEGIN BLTCON1 bits ---------------------------------------
;                    Area mode (BLTCON1 bit 0 = 0)
BSH3      EQU $8000 ;Shift value of B source
BSH2      EQU $4000
BSH1      EQU $2000
BSH0      EQU $1000
DOFF      EQU $0080 ;?
EFE       EQU $0010 ;Exclusive fill enable
IFE       EQU $0008 ;Inclusive fill enable
FCI       EQU $0004 ;Fill carry input
DESC      EQU $0002 ;Descending (decreasing address) control bit
LINE      EQU $0001 ;Line mode control bit (set to 0)
;                    Line mode (BLTCON0 bit 0 = 1)
TEXTURE3  EQU $8000
TEXTURE2  EQU $4000
TEXTURE1  EQU $2000
TEXTURE0  EQU $1000
SIGN      EQU $0040 ;Sign flag
SUD       EQU $0010 ;Sometimes up or down (=AUD*)
SUL       EQU $0008 ;Sometimes up or left
AUL       EQU $0004 ;Always up or left
SING      EQU $0002 ;Single bit per horizontal line for use with subsequent
                      ;area fill
;------------------- END BLTCON1 bits -----------------------------------------
BLTAFWM   EQU $044 ;W - Blitter first word mask for source A
BLTALWM   EQU $046 ;W - Blitter last word mask for source A
BLTCPTH   EQU $048 ;W - Blitter pointer to source C (high 3 bits)
BLTCPTL   EQU $04A ;W - Blitter pointer to source C (low 15 bits)
BLTBPTH   EQU $04C ;W - Blitter pointer to source B (high 3 bits)
BLTBPTL   EQU $04E ;W - Blitter pointer to source B (low 15 bits)
BLTAPTH   EQU $050 ;W - Blitter pointer to source A (high 3 bits)
BLTAPTL   EQU $052 ;W - Blitter pointer to source A (low 15 bits)
BLTDPTH   EQU $054 ;W - Blitter pointer to destination D (high 3 bits)
BLTDPTL   EQU $056 ;W - Blitter pointer to destination D (low 15 bits)
BLTSIZE   EQU $058 ;W - Blitter start and size (window width, height)
BLTCON0L  EQU $05A ;W - Blitter control 0, lower 8 bits (minterms)
BLTSIZV   EQU $05C ;W - Blitter V size (for 15 bit vertical size)
BLTSIZH   EQU $05E ;W - Blitter H size and start (for 11 bit H size)
BLTCMOD   EQU $060 ;W - Blitter modulo for source C
BLTBMOD   EQU $062 ;W - Blitter modulo for source B
BLTAMOD   EQU $064 ;W - Blitter modulo for source A
BLTDMOD   EQU $066 ;W - Blitter modulo for destination D
BLTCDAT   EQU $070 ;W - Blitter source C data register
BLTBDAT   EQU $072 ;W - Blitter source B data register
BLTADAT   EQU $074 ;W - Blitter source A data register
COP1LC    EQU $080 ;W - Copper 1st location
COP1LCH   EQU $080 ;W - Copper 1st location (high 5 bits, was 3 bits)
COP1LCL   EQU $082 ;W - Copper 1st location (low 15 bits)
COP2LC    EQU $084 ;W - Copper 2nd location
COP2LCH   EQU $084 ;W - Copper 2nd location (high 5 bits, was 3 bits)
COP2LCL   EQU $086 ;W - Copper 2nd location (low 15 bits)
COPJMP1   EQU $088 ;S - Coprocessor restart at 1st location
COPJMP2   EQU $08a ;S - Coprocessor restart at 2nd location
DIWSTRT   EQU $08e ;W - Display window start (upper left vert, horiz pos)
DIWSTOP   EQU $090 ;W - Display window stop (lower right vert, horiz pos)
DDFSTRT   EQU $092 ;W - Display bit plane data fetch start, horiz pos
DDFSTOP   EQU $094 ;W - Display bit plane data fetch stop, horiz pos
DMACON    EQU $096 ;W - DMA control write (clear or set bits)
                     ;--- Bits: see DMACON
INTENA    EQU $09a ;W - Interrupt enable bits (clear or set bits)
                     ;--- Bits: see INTENAR
INTREQ    EQU $09c ;W - Interrupt request bits (clear or set bits)
ADKCON    EQU $09e ;W - Audio, disk and UART control
BPL1PTH   EQU $0e0 ;W - Bitplane pointer 1 (high 5 bits, was 3 bits)
BPL1PTL   EQU $0e2 ;W - Bitplane pointer 1 (low 15 bits)
BPL2PTH   EQU $0e4 ;W - Bitplane pointer 2 (high 5 bits, was 3 bits)
BPL2PTL   EQU $0e6 ;W - Bitplane pointer 2 (low 15 bits)
BPL3PTH   EQU $0e8 ;W - Bitplane pointer 3 (high 5 bits, was 3 bits)
BPL3PTL   EQU $0ea ;W - Bitplane pointer 3 (low 15 bits)
BPL4PTH   EQU $0ec ;W - Bitplane pointer 4 (high 5 bits, was 3 bits)
BPL4PTL   EQU $0ee ;W - Bitplane pointer 4 (low 15 bits)
BPL5PTH   EQU $0f0 ;W - Bitplane pointer 5 (high 5 bits, was 3 bits)
BPL5PTL   EQU $0f2 ;W - Bitplane pointer 5 (low 15 bits)
BPL6PTH   EQU $0f4 ;W - Bitplane pointer 6 (high 5 bits, was 3 bits)
BPL6PTL   EQU $0f6 ;W - Bitplane pointer 6 (low 15 bits)
BPL7PTH   EQU $0f8 ;W - Bitplane pointer 7 (high 5 bits, was 3 bits)
BPL7PTL   EQU $0fa ;W - Bitplane pointer 7 (low 15 bits)
BPL8PTH   EQU $0fc ;W - Bitplane pointer 8 (high 5 bits, was 3 bits)
BPL8PTL   EQU $0fe ;W - Bitplane pointer 8 (low 15 bits)
BPLCON0   EQU $100 ;W - Bitplane control (miscellaneous control bits)
;-------------------- BEGIN BPLCON0 bits --------------------------------------
HIRES     EQU $8000 ;HIRES = High resolution (640*200/640*400 interlace) mode
BPU2      EQU $4000 ;3rd bit of bit planes use
BPU1      EQU $2000 ;2nd bit of bit planes use
BPU0      EQU $1000 ;1st bit of bit planes use
HAM       EQU $0800 ;Hold and modify mode, now using either 6 or 8 bit planes
DPF       EQU $0400 ;Double playfield (PF1 = odd and PF2 = even bit planes)
                      ;now available in all resolutions (if BPU=6 and HAM=0 and
                      ;DPF=0 a special mode is defined that allows bitplane 6
                      ;to cause an intensity reduction of the other 5 btplanes.
                      ;The color register output selected by 5 bitplanes is
                      ;shifted to half intensity by the 6th bit plane. This is
                      ;called EXTRA-HALFBRITE Mode.
COLOR     EQU $0200 ;Enables color burst output signal
GAUD      EQU $0100 ;Genlock audio enable. This level appears on the ZD pin
                      ;on Denise during all blanking periods, unless ZDCLK bit
                      ;is set.
UHRES     EQU $0080 ;Enables the UHRES pointers (for 1k*1k) also needs bits
                      ;in DMACON (hires chips only). Disables hard stops for
                      ;vert,horiz display windows.
SHRES     EQU $0040 ;Super hi-res mode (35ns pixel width)
BYPASS    EQU $0020 ;=0. Bit planes are scrolled and prioritized normally,
                      ;but bypass color table and 8 bit wide data appear on
                      ;R(7:0).
BPU3      EQU $0010 ;=0. See above (BPUx)
LPEN      EQU $0008 ;Light pen enable (reset on power up)
LACE      EQU $0004 ;Interlace enable (reset on power up)
ERSY      EQU $0002 ;External resync (HSYNC, VSYNC pads become inputs)
                      ;(reset on power up)
ECSENA    EQU $0001 ;=0. When low (default), the following bits in BPLCON3
                      ;are disabled: BRDRBLNK,BRDNTRAN,ZDCLKEN,BRDSPRT,
                      ;EXTBLKEN.
                      ;These 5 bits can always be set by writing to BPLCON3,
                      ;however there effects are inhibited until ECSENA goes
                      ;high. This allows rapid context switching between
                      ;pre-ECS viewports and new ones.
;-------------------- END BPLCON0 bits ----------------------------------------
BPLCON1   EQU $102 ;W - Bitplane control (scroll value)
;-------------------- BEGIN BPLCON1 bits --------------------------------------
PF2H7     EQU $8000 ;(PF2Hx =) Playfield 2 horizontal scroll code, x=0-7
PF2H6     EQU $4000
PF2H1     EQU $2000
PF2H0     EQU $1000
PF1H7     EQU $0800 ;(PF1Hx =) Playfield 1 horizontal scroll code, x=0-7
                      ;where PFyH0=LSB=35ns SHRES pixel (bits have been
                      ;renamed,old PFyH0 now PFyH2, etc). Now that the scroll
                      ;range has been quadrupled to allow for wider (32 or 64
                      ;bits) bitplanes.
PF1H6     EQU $0400
PF1H1     EQU $0200
PF1H0     EQU $0100
PF2H5     EQU $0080 ;OCS/ECS
PF2H4     EQU $0040
PF2H3     EQU $0020
PF2H2     EQU $0010
PF1H5     EQU $0008
PF1H4     EQU $0004
PF1H3     EQU $0002
PF1H2     EQU $0001
;-------------------- END BPLCON1 bits ----------------------------------------
BPLCON2   EQU $104 ;W - Bitplane control (video priority control)
BPLCON3   EQU $106 ;W - Bitplane control (enhanced features)
;-------------------- BEGIN BPLCON3 bits --------------------------------------
BANK2     EQU $8000 ;W - Selects one of eight color banks
BANK1     EQU $4000
BANK0     EQU $2000
PF2OF2    EQU $1000 ;W - Determine bit plane color table offset when
PF2OF1    EQU $0800 ; playfield 2 has priority in dual playfield mode:
PF2OF0    EQU $0400 ; 000: none; 001: 2 (plane 2 affected); 010: 4 (p.3 af.)
                      ; 011: 8 (p. 4 af., default); 100: 16 (plane 5 af.)
                      ; 101: 32 (p. 6 af.); 110: (p. 7 af.); 111: 128 (p.8 af.)
LOCT      EQU $0200 ;=0 Dictates that subsequent color palette values will be
                      ;written to a second 12-bit color palette, constituting
                      ;the RGB low minus order bits. Writes to the normal hi
                      ;minus order color palette automattically copied to the
                      ;low order for backwards compatibility.
SPRES1    EQU $0080 ;=0 Determine resolution of all 8 sprites:
SPRES0    EQU $0040 ; 00: ECS defaults (LORES, HIRES=140ns, SHRES=70ns)
                      ; 01: LORES (140ns); 10: HIRES (70ns); 11: SHRES (35ns)
BRDRBLNK  EQU $0020 ;=0 "Border area" is blanked instead of color (0).
                      ; Disabled when ECSENA low.
BRDNTRAN  EQU $0010 ;=0 "Border area" is non minus transparant (ZD pin is low
                      ; when border is displayed). Disabled when ECSENA low.
ZDCLKEN   EQU $0004 ;=0 ZD pin outputs a 14MHz clock whose falling edge
                      ; coincides with hires (7MHz) video data. This bit when
                      ; set disables all other ZD functions.
                      ; Disabled when ESCENA low.
BRDSPRT   EQU $0002 ;=0 Enables sprites outside the display window.
                      ; Disabled when ESCENA low.
EXTBLKEN  EQU $0001 ;=0 Causes BLANK output to be programmable instead of
                      ; reflecting internal fixed decodes.
                      ; Disabled when ESCENA low.
;-------------------- END BPLCON3 bits ----------------------------------------
BPL1MOD   EQU $108 ;W - Bitplane modulo (odd planes)
BPL2MOD   EQU $10a ;W - Bitplane modulo (even planes)
BPLCON4   EQU $10c ;W - Bitplane control register (bitplane & sprite masks)
SPR0PTH   EQU $120 ;W - Sprite 0 pointer (high 5 bits, was 3 bits)
SPR0PTL   EQU $122 ;W - Sprite 0 pointer (low 15 bits)
SPR1PTH   EQU $124 ;W - Sprite 1 pointer (high 5 bits, was 3 bits)
SPR1PTL   EQU $126 ;W - Sprite 1 pointer (low 15 bits)
SPR2PTH   EQU $128 ;W - Sprite 2 pointer (high 5 bits, was 3 bits)
SPR2PTL   EQU $12a ;W - Sprite 2 pointer (low 15 bits)
SPR3PTH   EQU $12c ;W - Sprite 3 pointer (high 5 bits, was 3 bits)
SPR3PTL   EQU $12e ;W - Sprite 3 pointer (low 15 bits)
SPR4PTH   EQU $130 ;W - Sprite 4 pointer (high 5 bits, was 3 bits)
SPR4PTL   EQU $132 ;W - Sprite 4 pointer (low 15 bits)
SPR5PTH   EQU $134 ;W - Sprite 5 pointer (high 5 bits, was 3 bits)
SPR5PTL   EQU $136 ;W - Sprite 5 pointer (low 15 bits)
SPR6PTH   EQU $138 ;W - Sprite 6 pointer (high 5 bits, was 3 bits)
SPR6PTL   EQU $13a ;W - Sprite 6 pointer (low 15 bits)
SPR7PTH   EQU $13c ;W - Sprite 7 pointer (high 5 bits, was 3 bits)
SPR7PTL   EQU $13e ;W - Sprite 7 pointer (low 15 bits)
COLOR00   EQU $180 ;W - Color table #0
COLOR01   EQU $182 ;W - Color table #1
COLOR02   EQU $184 ;W - Color table #2
COLOR03   EQU $186 ;W - Color table #3
COLOR04   EQU $188 ;W - Color table #4
COLOR05   EQU $18a ;W - Color table #5
COLOR06   EQU $18c ;W - Color table #6
COLOR07   EQU $18e ;W - Color table #7
COLOR08   EQU $190 ;W - Color table #8
COLOR09   EQU $192 ;W - Color table #9
COLOR10   EQU $194 ;W - Color table #10
COLOR11   EQU $196 ;W - Color table #11
COLOR12   EQU $198 ;W - Color table #12
COLOR13   EQU $19a ;W - Color table #13
COLOR14   EQU $19c ;W - Color table #14
COLOR15   EQU $19e ;W - Color table #15
COLOR16   EQU $1a0 ;W - Color table #16
COLOR17   EQU $1a2 ;W - Color table #17
COLOR18   EQU $1a4 ;W - Color table #18
COLOR19   EQU $1a6 ;W - Color table #19
COLOR20   EQU $1a8 ;W - Color table #20
COLOR21   EQU $1aa ;W - Color table #21
COLOR22   EQU $1ac ;W - Color table #22
COLOR23   EQU $1ae ;W - Color table #23
COLOR24   EQU $1b0 ;W - Color table #24
COLOR25   EQU $1b2 ;W - Color table #25
COLOR26   EQU $1b4 ;W - Color table #26
COLOR27   EQU $1b6 ;W - Color table #27
COLOR28   EQU $1b8 ;W - Color table #28
COLOR29   EQU $1ba ;W - Color table #29
COLOR30   EQU $1bc ;W - Color table #30
COLOR31   EQU $1be ;W - Color table #31
FMODE     EQU $1fc ;W - Fetch mode register (AGA only)
;------------------- BEGIN FMODE bits ----------------------------------------
SPAGEM    EQU $0008 ;sprite page mode (double CAS)
SPR32     EQU $0004 ;sprite 32 bits wide mode
BPAGEM    EQU $0002 ;bitplane page mode (double CAS)
BPL32     EQU $0001 ;bitplane 32 bits wide mode
;------------------- END FMODE bits ------------------------------------------
