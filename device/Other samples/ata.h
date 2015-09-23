/*---------------------------------------------------------------------- 
 * 
 * Filename: ata.h 
 * 
 * Contents: class ATA 
 * 
 * Authors: Whing, Anderson, Sean, ... 
 * 
 * Notes: 
 *   1. 
 *   2. 
 * 
 * Copyright (c) 2005 SAMSUNG Electronics. 
 *---------------------------------------------------------------------- 
 */ 
 
#ifndef __ATA_H__ 
#define __ATA_H__ 
 
 
//#include "fat.h" 
#include "Def.h" 
#define CFCON_MODE 
 
enum PIOMODE 
{ 
	PIO0, PIO1, PIO2, PIO3, PIO4 
}; 
 
enum UDMAMODE 
{ 
	UDMA0, UDMA1, UDMA2, UDMA3, UDMA4 
}; 
 
enum ATA_TRANSFER_CMD  
{ 
	ATA_CMD_STOP, ATA_CMD_START, ATA_CMD_ABORT, ATA_CMD_CONTINUE 
}; 
 
enum ATA_INT_SRC  
{ 
	ATA_INT_XFER_DONE, ATA_INT_UDMA_HOLD, ATA_INT_IRQ, ATA_INT_TBUF_FULL, ATA_INT_SBUF_EMPTY 
}; 
 
enum ATA_MODE  
{ 
	PIO_CPU, PIO_DMA, UDMA 
}; 
 
#define SRC_INT_NUM 5  
 
class ATA  
{ 
public : 
	bool OpenMedia(void); 
	bool OpenMedia(ATA_MODE mode); 
	bool CloseMedia(void); 
	virtual bool ReadBlocks(U32 uStBlock, U32 uBlocks, U32 uBufAddr); 
	virtual bool WriteBlocks(U32 uStBlock, U32 uBlocks, U32 uBufAddr); 
	virtual bool StartReadingBlocks(U32 uStBlock, U32 uBlocks, U32 uBufAddr); 
	virtual bool StartWritingBlocks(U32 uStBlock, U32 uBlocks, U32 uBufAddr); 
	bool IsReadingBlocksDone(void); 
	bool IsWritingBlocksDone(void); 
	void ContinueReadingBlocks(void); 
	void ContinueWritingBlocks(void); 
	void ChangeMode(void); 
 
	void Init(void); 
	void IdentifyDevice(void); 
	void GetMaxSectors(U32& maxSec); 
	void GetMaxMultiple(U32& maxMulti); 
	void ResetAll(void ); 
	void Clear_Pending(U32 srcInt); 
	bool FindInterruptRequest(U32& nthBit); 
	void ClearAllInterrupt(void);	 
	void SetTransferCommand(ATA_TRANSFER_CMD command); 
	void IsTBufFullContinue(bool& status); 
	void IsSBufEmptyContinue(bool& status); 
	void SetMultiple(U32 uSector); 
 
	bool ReadSectors_Pio(U32 nLBA,U32 nSectorCount,U32 nDesAddress); 
	bool ReadSector_PioDma(U32 uLBA,U32 uDesAddress); 
	bool ReadSectors_MultiplePioDma(U32 uLBA, U32 uDesAddress); /// 
	bool ReadSectors_Udma(U32 uLBA,U32 uSectorCount,U32 uSrcAddress); 
 
	bool WriteSectors_Pio(U32 nLBA,U32 nSectorCount,U32 nSrcAddress); 
	bool WriteSector_PioDma(U32 uLBA, U32 uSrcAddress); 
	bool WriteSectors_MultiplePioDma(U32 uLBA, U32 uSrcAddress); /// 
	bool WriteSectors_Udma(U32 uLBA, U32 uSectorCount, U32 uSrcAddress); 
 
	void SetAtaMode(ATA_MODE mode); 
	bool IsDmaDone(void); 
	 
protected: 
	 
	bool WaitForDeviceReady(void); 
	void WaitForHostReady(void); 
	 
	void SetPioMode(PIOMODE pmode); 
	void SetUdmaMode(UDMAMODE umode); 
	void SetLittleEndian(void); 
	void SetConfigMode(ATA_MODE mode, bool isWriteMode);	 
 
	void ReadDeviceReg(U32 nRegister, U8& data); 
	void GetDataFromDevice(U16& data); 
 
	void WaitForTransferDone(void); 
	void WriteOnTaskFileReg(U32 nRegister,U32 nValue); 
	void PutDataToDevice(U16 data);	 
 
	void SetAtaOnOff(U8 OnOff); 
 
	void SetAtaDevice(U32 nLBA, U32 nSectorCount); 
 
 
	U32 m_uCfgReg; 
	ATA_MODE m_eMode; 
 
	U32 m_uMaxMultiple; 
	U32 m_uCurrentMultiple; 
	U32 m_uMaxSectors; 
	PIOMODE m_eMaxPioMode; 
	PIOMODE m_eCurrentPioMode; 
	U32 m_uMaxUdmaMode; 
	U32 m_uCurrentUdmaMode; 
 
}; 
 
inline void ATA::SetAtaMode(ATA_MODE mode) { m_eMode = mode; } 
 
 
 
#endif // __ATA_H__ 
 