;================================================================================;
;                       Driver pro hw seriovou komunikaci                        ;
;--------------------------------------------------------------------------------;
;                                   By THS                                       ;
;--------------------------------------------------------------------------------;
;  Pro prijem znaku je pouzito preruseni serialu (4), prijate znaky jsou         ;
;  zapisovany do 256-bytoveho bufferu READBUF.                                   ;
;  Pri preteceni bufferu je nastaven bit FLOW_ERR, nuluje se v hlavnim           ;
;  programu                                                                      ;
;  Pro odesilani znaku je vyuzivan buffer SENDBUF. Jeho velikost je dana         ;
;  konst. SENDBUFSIZE.                                                           ;
;                                                                                ;
;  MCOM_CLEAR_BUFFERS() ... Inicializuje ukazatele a priznaku pro buffery.       ;
;                                                                                ;
;  MCOM_READBYTE()  ... Vraci jeden byte z bufferu                               ;
;  bit MCOM_RIBUF   ... 1 = V prijimacim bufferu je min. 1 znak,                 ;
;                  0 = Prazdny buffer. Nulovani provadi funkce ReadByte().       ;
;  bit MCOM_FLOW_ERR  ... Signalizuje preteceni prijimaciho bufferu, pokud       ;
;                    k tomu dojde, jsou jiz data ztracena !                      ;
;                                                                                ;
;  bit MCOM_SFLOW_ERR ... Signalizuje preteceni vysilaciho bufferu, pokud        ;
;                    k tomu dojde, jsou jiz data ztracena !                      ;
;                                                                                ;
;  bit MCOM_SENDBUF_ENABLE ... 1 = Povoleni vysilaciho bufferu a zpozdeneho      ;
;                         vysilani                                               ;
;                                                                                ;
;  _MCOM_SEND_CHAR(char) ... Zaradi do vysilaciho bufferu 1 znak                 ;
;  _MCOM_SEND_STRING(void *)  ... Zaradi do vysilaciho bufferu retezec           ;
;                            na pozici R2:R1 v CODE oblasti.                     ;
;                            Retezec musi byt ukoncen 0 !                        ;
;  _MCOM_SEND_BYTE(char) ... Zaradi do vys. buf. byte v hex. tvaru               ;
;  _MCOM_SEND_INT(int)   ... Zaradi do vys. buf. hodnotu typu int v hex. tvaru   ;
;  _MCOM_SEND_LONG(long) ... Zaradi do vys. buf. hodnotu typu long v hex. tvaru  ;
;                                                                                ;
;  _MCOM_DELAY(char) ... zpozdeni                                                ;
;                                                                                ;
;  MCOM_PROC()  ... Rutina pro vysilani vys. buf. musi byt cyklicky volana       ;
;                   z hlavniho programu                                          ;
;                                                                                ;
;                                                                                ;
;--------------------------------------------------------------------------------;
; 15/05/02 ... Zmena: Testovani prazdneho prijimaciho bufferu presunuto          ;
;                     z rutiny READBYTE na zacatek MCOM_PROC                     ;
;                     (Z duvodu jednodussiho usporadani ciloveho programu)       ;
;================================================================================;
; N * 8 + 3

NAME DRV232

;;;$INCLUDE(REG52.l)

PUBLIC  MCOM_READBYTE
PUBLIC  MCOM_RIBUF
PUBLIC  MCOM_TIBUF
PUBLIC  MCOM_RFLOW_ERR
PUBLIC  MCOM_SFULL_BUF
PUBLIC  _MCOM_SEND_CHAR
PUBLIC  _MCOM_SEND_STRING
PUBLIC  _MCOM_SEND_XDATA_STRING
PUBLIC  _MCOM_SEND_BYTE
PUBLIC  _MCOM_SEND_INT
PUBLIC  _MCOM_SEND_LONG
PUBLIC  _MCOM_DELAY
PUBLIC  MCOM_PROC
PUBLIC  MCOM_RESET_REC_BUF
PUBLIC  MCOM_RESET_SND_BUF
PUBLIC  MCOM_RESET_BUFFERS
PUBLIC  MCOM_FREE_REC_BUF
PUBLIC  MCOM_FREE_SND_BUF

PUBLIC  MCOM_SEND_DELAY_VALUE

; ------------------------------ KONSTANTY ----------------------------------
RECBUFSIZE   EQU     64     ; Velikost bufferu pro prijem dat
SNDBUFSIZE   EQU     64     ; Velikost bufferu pro odesilani dat
; Pozor: velikosti bufferu musi byt udavany v bitovem zarovnani! (8,16,32...)
; ------------------------------ Zasobnik -----------------------------------
;STACK        SEGMENT IDATA    ; Pokud je tento driver pouzivan
;             RSEG    STACK    ; jako driver, STACKu netreba...
;
;             DS     020h


; ------------------------ BUFFER PRO PRIJEM DAT ----------------------------
READBUF_XDATA SEGMENT   XDATA  PAGE  ; Segment bude zacinat od adresy xx00h
              RSEG      READBUF_XDATA

MCOM_READBUF: DS        RECBUFSIZE     ; Buffer pro prijem dat


; ---------------------- BUFFER PRO ODESILANI DAT ----------------------------
SENDBUF_XDATA SEGMENT   XDATA  PAGE  ; Segment bude zacinat od adresy xx00h
              RSEG      SENDBUF_XDATA

MCOM_SENDBUF: DS        SNDBUFSIZE     ; Buffer pro odesilani dat



; --------------------------- BITOVE PROMENNE -------------------------------
DRV232_BIT   SEGMENT BIT
             RSEG    DRV232_BIT

MCOM_RIBUF:      DBIT    1     ; Priznak naplneni prijimaciho bufferu,
                               ; 1 = v bufferu je minimalne 1 znak

MCOM_TIBUF:      DBIT    1     ; Priznak naplneni vysilaciho bufferu,
                               ; 1 = v bufferu je minimalne 1 znak

MCOM_RFLOW_ERR:  DBIT    1     ; Priznak preteceni prijimaciho bufferu
                               ; Nuluje se v hlavnim programu

MCOM_SFULL_BUF:  DBIT    1     ; Priznak plneho vysilaciho bufferu
                               ; Nuluje se v hlavnim programu

CY_SAVE:         DBIT    1     ; Ulozeni puv. hodnoty priznaku prenosu



; --------------------------- BYTOVE PROMENNE -------------------------------
DRV232_DVAR  SEGMENT DATA  ; Primo adresovatelne promenne
             RSEG    DRV232_DVAR

; Promenne pro prijimaci buffer
MCOM_RR_BEG_BUF:   DS  01h       ; Virtualni zacatek bufferu pro cteni z bufferu
MCOM_RW_END_BUF:   DS  01h       ; Virtualni konec bufferu pro zapis do bufferu

; Promenne pro vysilaci buffer
MCOM_SR_BEG_BUF:  DS  01h        ; Virtualni zacatek bufferu pro vysilani bufferu
MCOM_SW_END_BUF:  DS  01h        ; Virtualni konec bufferu pro vysilani bufferu

; Citac zpozdeni pro vysilani
MCOM_SEND_DELAY_VALUE: DS   01h    ; Zpozdeni pro vysilani (=Pocet volani SEND_PROC)
SP_DELAY_CT:           DS   01h



; ---------------------------------------------------------------------------
  CSEG    AT    23h      ; Nastaveni preruseni serialu
  ORG     23h            ; Preruseni serialu
  LJMP    INTR_SBUFTOBUF      ; Skok na prerusovaci rutinu
; ---------------------------------------------------------------------------



;===========================================================================;
;                         Zacatek CODE segmentu                             ;
;===========================================================================;
DRV232_CODE   SEGMENT CODE
              RSEG    DRV232_CODE


USING 0                  ; Vyber sady registru


;===========================================================================;
;         Serial interrupt 4:  Zapis prichoziho bytu do bufferu             ;
;---------------------------------------------------------------------------;
INTR_SBUFTOBUF:
          ;CLR   ES               ; Zakaz preruseni serialu
          MOV   CY_SAVE,C        ; Ulozeni hodnoty priznaku prenosu
          PUSH  Acc
          PUSH  DPH              ; Uschovani registru
          PUSH  DPL

          JBC   RI,SB_RECEIVEBYTE   ; Pri prichozim znaku
          JBC   TI,SB_SENDBYTE      ; Pri pozadavku na odeslani

          JMP   EXIT_STB            ; Skok na konec preruseni


; --------------------- Interrupt: Prijem znaku -----------------------
SB_RECEIVEBYTE:
           ; Priznak RI je nyni vynulovan po skoku JBC na zacatku
           ; preruseni.

          MOV   A,SBUF             ; Nacti znak ze serialu
          MOV   DPH,#HIGH MCOM_READBUF  ; Nastav horni byte adresy bufferu
          MOV   DPL,MCOM_RW_END_BUF     ; Nastav ukazatel na konec bufferu
          MOVX  @DPTR,A            ; Zapis znak do bufferu
          SETB  MCOM_RIBUF         ; Nastav priznak neprazdneho bufferu
          INC   MCOM_RW_END_BUF    ; Posun ukazatel konce bufferu
          ANL   MCOM_RW_END_BUF,#RECBUFSIZE-1

          MOV   A,MCOM_RW_END_BUF   ; Do Acc ukazatel na konec bufferu
          XRL   A,MCOM_RR_BEG_BUF   ; Acc=SW_END_BUF xor SR_BEG_BUF
          JNZ   NO_FLOW_ERROR      ; Zacatek = Konec buf. -> Preteceni buf.

          SETB  MCOM_RFLOW_ERR     ; Nastav priznak preteceni bufferu

NO_FLOW_ERROR:
          JMP   EXIT_STB           ; Konec preruseni



; ------------------- Interrupt: Vysilani bufferu ---------------------
SB_SENDBYTE:                       ; Vysilani bufferu
          ; TI je nyni vynulovan po skoku JBC na zacatku preruseni
          ; TI se sam znovu nastavi po odeslani znaku,

          CLR   MCOM_TIBUF

;---------------------- Konec preruseni -----------------------
EXIT_STB:
          POP   DPL
          POP   DPH                ; Obnoveni registru
          POP   Acc
          MOV   C,CY_SAVE          ; Obnoveni puv. hodnoty priznaku prenosu
          ;SETB  ES                 ; Povoleni preruseni serialu

          RETI




;===========================================================================;
;    Vysilani obsahu bufferu a nulovani priznaku znaku v prijm. bufferu     ;
;   --------------------------------------------------------------------    ;
; Neni-li nastaven priznak TIBUF, vysila postupne obsah vys. bufferu        ;
;---------------------------------------------------------------------------;
MCOM_PROC:
          JNB   MCOM_RIBUF,MP_TEST_TIBUF

          PUSH  Acc
          MOV   A,MCOM_RW_END_BUF
          XRL   A,MCOM_RR_BEG_BUF    ; Je-li zacatek=konec buffer je prazdny
          JNZ   MP_TEST_RIBUF_EXIT
          CLR   MCOM_RIBUF          ; Nuluj priznak znaku v bufferu

MP_TEST_RIBUF_EXIT:
          POP   Acc

MP_TEST_TIBUF:
          JB    MCOM_TIBUF,MP_EXIT2

          PUSH  Acc
          PUSH  DPH                ; Obnoveni registru
          PUSH  DPL

MP_CT:    DEC   SP_DELAY_CT
          MOV   A,SP_DELAY_CT
          JNZ   MP_EXIT

          ANL   MCOM_SR_BEG_BUF,#SNDBUFSIZE-1

           ; Je-li zacatek=konec buffer je prazdny -> konec preruseni
          MOV   A,MCOM_SW_END_BUF   ; Do Acc ukazatel na konec bufferu
          XRL   A,MCOM_SR_BEG_BUF   ; Acc=SW_END_BUF xor SR_BEG_BUF
          JZ    MP_EXIT             ; Zacatek = Konec buf. -> Konec

          MOV   DPH,#HIGH MCOM_SENDBUF ; Nastav horni byte adresy bufferu
          MOV   DPL,MCOM_SR_BEG_BUF    ; Nastav ukazatel na zacatek bufferu

          MOVX  A,@DPTR             ; Nacti byte z bufferu
          MOV   SBUF,A              ; Odesli byte

          MOV   SP_DELAY_CT,MCOM_SEND_DELAY_VALUE ; Nastaveni citace zpozdeni pro vysilani

          INC   MCOM_SR_BEG_BUF          ; Posun ukazatel v buf na dalsi znak
          ANL   MCOM_SR_BEG_BUF,#SNDBUFSIZE-1

          SETB  MCOM_TIBUF

MP_EXIT:
          POP   DPL
          POP   DPH                 ; Obnoveni registru
          POP   Acc
MP_EXIT2:
          RET


;===========================================================================;
;                        Nacte jeden byte z bufferu                         ;
;                       ----------------------------                        ;
;   Vraci jeden byte nacteny z prijimaciho bufferu v R7                     ;
;---------------------------------------------------------------------------;
MCOM_READBYTE:
          CLR   ES                  ; Zakaz preruseni serialu
          MOV   CY_SAVE,C           ; Ulozeni hodnoty priznaku prenosu
          PUSH  DPH
          PUSH  DPL
          PUSH  Acc

          MOV   DPH,#HIGH MCOM_READBUF   ; Nastav horni byte adresy bufferu
          MOV   DPL,MCOM_RR_BEG_BUF       ; Nastav ukazatel na zacatek bufferu

          MOVX  A,@DPTR             ; Nacti znak z bufferu
          MOV   R7,A                ; Zapis znak do R7

          INC   MCOM_RR_BEG_BUF      ; Posun ukazatel v bufferu na dalsi znak
          ANL   MCOM_RR_BEG_BUF,#RECBUFSIZE-1

EXIT_RB:
          POP   Acc
          POP   DPL
          POP   DPH
          MOV   C,CY_SAVE          ; Obnoveni puv. hodnoty priznaku prenosu
          SETB  ES                 ; Povoleni preruseni serialu
          RET



;===========================================================================;
;                     Zapis znaku do vysilaciho bufferu                     ;
;                    -----------------------------------                    ;
; Zapise do vysilaciho bufferu znak v R7                                    ;
;---------------------------------------------------------------------------;
_MCOM_SEND_CHAR:
          PUSH  Acc
          PUSH  DPH
          PUSH  DPL

          CLR   ES
          ANL   MCOM_SW_END_BUF,#SNDBUFSIZE-1

          ;-------------------------------------------------------
          ; Nastaveni priznaku pri plnem vysilacim bufferu
          MOV   A,MCOM_SW_END_BUF
          INC   A
          ANL   A,#SNDBUFSIZE-1
          XRL   A,MCOM_SR_BEG_BUF
          JNZ   SCH_BUFFER_OK

          SETB  MCOM_SFULL_BUF      ; Nastaveni priznaku plneho vysilaciho bufferu
          JMP   SCH_BUFFER_FULL

SCH_BUFFER_OK:
          ;-------------------------------------------------------
          MOV   DPH,#HIGH MCOM_SENDBUF ; Nastav horni byte adresy bufferu
          MOV   DPL,MCOM_SW_END_BUF    ; Nastav ukazatel na virtualni konec bufferu

          MOV   A,R7                ; Zapis znak z R7 do Acc
          MOVX  @DPTR,A             ; Zapis znak do bufferu
          INC   MCOM_SW_END_BUF     ; Posun ukazatel v bufferu na dalsi znak
          ANL   MCOM_SW_END_BUF,#SNDBUFSIZE-1

SCH_BUFFER_FULL:
          SETB  ES

          POP   DPL
          POP   DPH
          POP   Acc
          RET



;===========================================================================;
;             Zapis retezec z CODE pameti do vysilaciho bufferu             ;
;            ---------------------------------------------------            ;
; Zapise do vysilaciho bufferu retezec z CODE, na ktery ukazuje R1,R2       ;
;---------------------------------------------------------------------------;
_MCOM_SEND_STRING:
          PUSH  Acc
          PUSH  DPH
          PUSH  DPL
          PUSH  AR7

          MOV   DPH,R2           ; Nastav horni byte adresy retezce
          MOV   DPL,R1           ; Nastav dolni byte adresy retezce

SS_LOOP:
          MOV   A,#0
          MOVC  A,@A + DPTR      ; Nacti znak z code pameti
          JZ    EXIT_SS          ; Je-li znak #0, konec retezce...

          MOV   R7,A             ; Nastav znak jako parametr pro _SendChar
          CALL  _MCOM_SEND_CHAR  ; Zapis znak do bufferu
          INC   DPTR             ; Posun ukazatel retezce na dalsi znak

          JMP   SS_LOOP

EXIT_SS:
          POP   AR7
          POP   DPL
          POP   DPH
          POP   Acc
          RET



;===========================================================================;
;            Zapis retezec z XDATA pameti do vysilaciho bufferu             ;
;           ----------------------------------------------------            ;
; Zapise do vysilaciho bufferu retezec z CODE, na ktery ukazuje R1,R2       ;
;---------------------------------------------------------------------------;
_MCOM_SEND_XDATA_STRING:
          PUSH  Acc
          PUSH  DPH
          PUSH  DPL
          PUSH  AR7

          MOV   DPH,R2           ; Nastav horni byte adresy retezce
          MOV   DPL,R1           ; Nastav dolni byte adresy retezce

SXS_LOOP:
          MOVX  A,@DPTR          ; Nacti znak z xdata pameti
          JZ    EXIT_SS          ; Je-li znak #0, konec retezce...

          MOV   R7,A             ; Nastav znak jako parametr pro _SendChar
          CALL  _MCOM_SEND_CHAR  ; Zapis znak do bufferu
          INC   DPTR             ; Posun ukazatel retezce na dalsi znak

          JMP   SXS_LOOP

EXIT_SXS:
          POP   AR7
          POP   DPL
          POP   DPH
          POP   Acc
          RET



;===========================================================================;
;      Zapise znak na adrese v DPTR do R7 a zavola funkci Send_Char         ;
;     컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         ;
;---------------------------------------------------------------------------;
CONV_SEND:
          MOVC A,@A+DPTR          ; Nacti znak z prev. tab.
          XCH  A,R7               ; Prohozeni Acc <-> R7
          CALL _MCOM_SEND_CHAR    ; Zapis znaku do vysilaciho retezce
          XCH  A,R7               ; Prohozeni Acc <-> R7
          RET


;===========================================================================;
;         Zobrazi byte v hexadec. tvaru na aktualni pozici (1 BYTE)         ;
;        컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        ;
;Vstup:                                                                     ;
;  R7 ... Cislo                                                             ;
;---------------------------------------------------------------------------;
_MCOM_SEND_BYTE:
          PUSH Acc
          PUSH DPL
          PUSH DPH

          MOV  DPTR,#NUM_CHAR     ; Nastav adresu prev. tab.
          MOV  A,R7
          ANL  A,#0F0h            ; Horni 1/2 byte
          SWAP A
          CALL CONV_SEND

          MOV  A,R7
          ANL  A,#00Fh            ; Dolni 1/2 byte
          CALL CONV_SEND

          POP  DPH
          POP  DPL
          POP  Acc
          RET



;===========================================================================;
;         Zobrazi byte v hexadec. tvaru na aktualni pozici (2 BYTE)         ;
;        컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        ;
;Vstup:                                                                     ;
;  R7 ... Nizsi byte cisla                                                  ;
;  R6 ... Vyssi byte cisla                                                  ;
;---------------------------------------------------------------------------;
_MCOM_SEND_INT:
          PUSH Acc
          PUSH DPL
          PUSH DPH

          MOV  DPTR,#NUM_CHAR     ; Nastav adresu prev. tab.
          MOV  A,R6
          ANL  A,#0F0h            ; Horni 1/2 byte
          SWAP A
          CALL CONV_SEND

          MOV  DPTR,#NUM_CHAR     ; Nastav adresu prev. tab.
          MOV  A,R6
          ANL  A,#00Fh            ; Dolni 1/2 byte
          CALL CONV_SEND


          MOV  DPTR,#NUM_CHAR     ; Nastav adresu prev. tab.
          MOV  A,R7
          ANL  A,#0F0h            ; Horni 1/2 byte
          SWAP A
          CALL CONV_SEND

          MOV  DPTR,#NUM_CHAR     ; Nastav adresu prev. tab.
          MOV  A,R7
          ANL  A,#00Fh            ; Dolni 1/2 byte
          CALL CONV_SEND

          POP  DPH
          POP  DPL
          POP  Acc
          RET



;===========================================================================;
;         Zobrazi byte v hexadec. tvaru na aktualni pozici (4 BYTE)         ;
;        컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        ;
;Vstup:                                                                     ;
;  R7 ... Nizsi byte cisla                                                  ;
;  R6 ... Vyssi byte cisla                                                  ;
;  R5 ... Nizsi byte cisla                                                  ;
;  R4 ... Vyssi byte cisla                                                  ;
;---------------------------------------------------------------------------;
_MCOM_SEND_LONG:
          PUSH Acc
          PUSH DPL
          PUSH DPH

          MOV  DPTR,#NUM_CHAR     ; Nastav adresu prev. tab.
          MOV  A,R4
          ANL  A,#0F0h            ; Horni 1/2 byte
          SWAP A
          CALL CONV_SEND

          MOV  DPTR,#NUM_CHAR     ; Nastav adresu prev. tab.
          MOV  A,R4
          ANL  A,#00Fh            ; Dolni 1/2 byte
          CALL CONV_SEND

          MOV  DPTR,#NUM_CHAR     ; Nastav adresu prev. tab.
          MOV  A,R5
          ANL  A,#0F0h            ; Horni 1/2 byte
          SWAP A
          CALL CONV_SEND

          MOV  DPTR,#NUM_CHAR     ; Nastav adresu prev. tab.
          MOV  A,R5
          ANL  A,#00Fh            ; Dolni 1/2 byte
          CALL CONV_SEND

          MOV  DPTR,#NUM_CHAR     ; Nastav adresu prev. tab.
          MOV  A,R6
          ANL  A,#0F0h            ; Horni 1/2 byte
          SWAP A
          CALL CONV_SEND

          MOV  DPTR,#NUM_CHAR     ; Nastav adresu prev. tab.
          MOV  A,R6
          ANL  A,#00Fh            ; Dolni 1/2 byte
          CALL CONV_SEND

          MOV  DPTR,#NUM_CHAR     ; Nastav adresu prev. tab.
          MOV  A,R7
          ANL  A,#0F0h            ; Horni 1/2 byte
          SWAP A
          CALL CONV_SEND

          MOV  DPTR,#NUM_CHAR     ; Nastav adresu prev. tab.
          MOV  A,R7
          ANL  A,#00Fh            ; Dolni 1/2 byte
          CALL CONV_SEND

          POP  DPH
          POP  DPL
          POP  Acc
          RET



;===========================================================================;
;           Inicializace/Zahozeni obsahu prijimaciho bufferu                ;
;          ---------------------------------------------------              ;
; Nastavi priznaky a ukazatele v bufferu na pocatecni hodnotu               ;
;---------------------------------------------------------------------------;
MCOM_RESET_REC_BUF:
          ;CLR  ES            ; Zakaz preruseni serialu
          MOV  MCOM_RR_BEG_BUF, #0 ; Nulovani ukazatele virtualniho zacatku bufferu (prijem)
          MOV  MCOM_RW_END_BUF, #0 ; Nulovani ukazatele virtualniho konce bufferu (prijem)
          CLR  MCOM_RIBUF     ; Nulovani priznaku znaku v bufferu
          CLR  MCOM_RFLOW_ERR ; Nulovani priznaku preteceni prijimaciho bufferu
          ;SETB ES            ; Povoleni preruseni serialu
          RET


;===========================================================================;
;                      Vyprazdneni prijimaciho bufferu                      ;
;                     ---------------------------------                     ;
; Nastavi ukazatel konce bufferu na zacatek                                 ;
;---------------------------------------------------------------------------;
MCOM_FREE_REC_BUF:
          ;CLR  ES                ; Povoleni preruseni serialu
          MOV  MCOM_RR_BEG_BUF, MCOM_RW_END_BUF
          CLR  MCOM_RIBUF         ; Nulovani priznaku znaku v bufferu
          CLR  MCOM_RFLOW_ERR     ; Nulovani priznaku preteceni prijimaciho bufferu
          ;SETB ES                ; Povoleni preruseni serialu
          RET


;===========================================================================;
;           Inicializace/Zahozeni obsahu vysilaciho bufferu                 ;
;          -------------------------------------------------                ;
; Nastavi priznaky a ukazatele v bufferu na pocatecni hodnotu               ;
;---------------------------------------------------------------------------;
MCOM_RESET_SND_BUF:
          ;CLR  ES            ; Povoleni preruseni serialu
          MOV  MCOM_SR_BEG_BUF,#0 ; Nulovani ukazatele virtualniho zacatku bufferu (vysilani)
          MOV  MCOM_SW_END_BUF,#0 ; Nulovani ukazatele virtualniho konce bufferu (vysilani)
          CLR  MCOM_TIBUF         ; Nulovani priznaku znaku v bufferu
          CLR  MCOM_SFULL_BUF     ; Nulovani priznaku preteceni vysilaciho bufferu

          MOV  MCOM_SEND_DELAY_VALUE,#1  ; Zpozdeni pro vysilani (=Pocet volani SEND_PROC)
          MOV  SP_DELAY_CT,#0       ; Nulovani citace zpozdeneho vysil.

          ;SETB ES            ; Povoleni preruseni serialu
          RET


;===========================================================================;
;                      Vyprazdneni prijimaciho bufferu                      ;
;                     ---------------------------------                     ;
; Nastavi ukazatel konce bufferu na zacatek                                 ;
;---------------------------------------------------------------------------;
MCOM_FREE_SND_BUF:
          ;CLR  ES            ; Povoleni preruseni serialu
          MOV  MCOM_SR_BEG_BUF, MCOM_SW_END_BUF
          CLR  MCOM_TIBUF         ; Nulovani priznaku znaku v bufferu
          CLR  MCOM_SFULL_BUF     ; Nulovani priznaku preteceni vysilaciho bufferu
          ;SETB ES            ; Povoleni preruseni serialu
          RET




;===========================================================================;
;                    Inicializace/Zahozeni obsahu bufferu                   ;
;                   --------------------------------------                  ;
; Nastavi priznaky a ukazatele v bufferu na pocatecni hodnotu               ;
;---------------------------------------------------------------------------;
MCOM_RESET_BUFFERS:
          CALL MCOM_RESET_REC_BUF   ; Reset prijimaciho bufferu
          CALL MCOM_RESET_SND_BUF   ; Reset odesilaciho bufferu
          RET




;===========================================================================;
;                                 Zpozdeni                                  ;
;                                ----------                                 ;
;  Parametr R7 ... zpozdeni                                                 ;
;---------------------------------------------------------------------------;
_MCOM_DELAY:
          MOV  R6,#02Bh        ; Zpozdeni  (2Bh)
          DJNZ R6,$            ; Vnitrni smycka
          DJNZ R7,_MCOM_DELAY   ; Vnejsi smycka
          RET




;===========================================================================;
;                   Prevodni tabulka cisla na znak (HEX)                    ;
;                  --------------------------------------                   ;
;---------------------------------------------------------------------------;
NUM_CHAR:
; Prevodni tabulka pro zobrazovani DEC a HEX cisel
  DB '0123456789ABCDEF'


END
