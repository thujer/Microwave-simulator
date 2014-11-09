extern bit  MCOM_RIBUF;
extern bit  MCOM_TIBUF;

extern bit  MCOM_RFLOW_ERR;
extern bit  MCOM_SFLOW_ERR;

extern bit  MCOM_SFULL_BUF;

extern char MCOM_READBYTE();
extern void MCOM_PROC();

extern void MCOM_SEND_CHAR         ( unsigned char );
extern void MCOM_SEND_XDATA_STRING ( void *        );
extern void MCOM_SEND_STRING       ( void *        );
extern void MCOM_SEND_BYTE         ( unsigned char );
extern void MCOM_SEND_INT          ( unsigned int  );
extern void MCOM_SEND_LONG         ( unsigned long );

extern void MCOM_DELAY( unsigned char );

extern unsigned char MCOM_SEND_DELAY_VALUE;

extern void MCOM_RESET_REC_BUF();
extern void MCOM_RESET_SND_BUF();
extern void MCOM_RESET_BUFFERS();
extern void MCOM_FREE_REC_BUF();
extern void MCOM_FREE_SND_BUF();
