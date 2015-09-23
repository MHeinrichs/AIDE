

www.pudn.com > S3C2443_Test_program.zip > ata.cpp, change:2006-08-21,size:31429b
 


    #include <stdio.h>   
    #include "2443addr.h"   
    #include "system.h"   
    #include "ata.h"   
    #include "etc.h"   
       
       
    #ifdef DBG_ATA   
    #define DbgAta(x) Dbg x   
    #else   
    #define DbgAta(x) 0;   
    #endif   
       
    #define CFCON_BASE 0x4B800000   
       
    #ifdef CFCON_MODE   
    #define ATA_MUX           (CFCON_BASE+0x1800)   
    #define ATA_BASE            (CFCON_BASE+0x1900)   
    #endif   
       
       
    /*--------------------------------------------------------------*/   
    /*  ATA Register                                                */   
    /*--------------------------------------------------------------*/   
    #define ATA_CONTROL     (ATA_BASE + 0x00)   //ATA0 enable and clock down status   
    #define ATA_STATUS      (ATA_BASE + 0x04)   //ATA0 status   
    #define ATA_COMMAND     (ATA_BASE + 0x08)   //ATA0 command   
    #define ATA_SWRST           (ATA_BASE + 0x0C)   //ATA0 software reset                     
    #define ATA_IRQ         (ATA_BASE + 0x10)   //ATA0 interrupt sources   
    #define ATA_IRQ_MASK        (ATA_BASE + 0x14)   //ATA0 interrupt mask   
    #define ATA_CFG         (ATA_BASE + 0x18)   //ATA0 configuration for ATA interface                  
       
    #define ATA_PIO_TIME        (ATA_BASE + 0x2C)   //ATA0 PIO timing                                       
    #define ATA_UDMA_TIME       (ATA_BASE + 0x30)   //ATA0 UDMA timing                                      
    #define ATA_XFR_NUM     (ATA_BASE + 0x34)   //ATA0 transfer number                                  
    #define ATA_XFR_CNT     (ATA_BASE + 0x38)   //ATA0 current transfer count                           
    #define ATA_TBUF_START  (ATA_BASE + 0x3C)   //ATA0 start address of track buffer                    
    #define ATA_TBUF_SIZE       (ATA_BASE + 0x40)   //ATA0 size of track buffer                             
    #define ATA_SBUF_START  (ATA_BASE + 0x44)   //ATA0 start address of Source buffer1                  
    #define ATA_SBUF_SIZE       (ATA_BASE + 0x48)   //ATA0 size of source buffer1                           
    #define ATA_CADR_TBUF       (ATA_BASE + 0x4C)   //ATA0 current write address of track buffer            
    #define ATA_CADR_SBUF       (ATA_BASE + 0x50)   //ATA0 current read address of source buffer            
    #define ATA_PIO_DTR     (ATA_BASE + 0x54)   //ATA0 PIO device data register                         
    #define ATA_PIO_FED     (ATA_BASE + 0x58)   //ATA0 PIO device Feature/Error register                
    #define ATA_PIO_SCR     (ATA_BASE + 0x5C)   //ATA0 PIO sector count register                        
    #define ATA_PIO_LLR     (ATA_BASE + 0x60)   //ATA0 PIO device LBA low register                      
    #define ATA_PIO_LMR     (ATA_BASE + 0x64)   //ATA0 PIO device LBA middle register                   
    #define ATA_PIO_LHR     (ATA_BASE + 0x68)   //ATA0 PIO device LBA high register                     
    #define ATA_PIO_DVR     (ATA_BASE + 0x6C)   //ATA0 PIO device register                              
    #define ATA_PIO_CSD     (ATA_BASE + 0x70)   //ATA0 PIO device command/status register               
    #define ATA_PIO_DAD     (ATA_BASE + 0x74)   //ATA0 PIO device control/alternate status register     
    #define ATA_PIO_READY       (ATA_BASE + 0x78)   //ATA0 PIO data read/write ready                        
    #define ATA_PIO_RDATA       (ATA_BASE + 0x7C)   //ATA0 PIO read data from device data register          
    #define BUS_FIFO_STATUS (ATA_BASE + 0x90)   //Internal AHBP fifo status                     
    #define ATA_FIFO_STATUS (ATA_BASE + 0x94)   //Internal ATA0  fifo status                    
       
    /*=========================================================================  
     *                        ata Register Address  
     *=========================================================================  
     */   
       
    #define DEV_ERROR           (ATA_BASE + 0x58)   
    #define DEV_FEATURE     (ATA_BASE + 0x58)   
    #define DEV_SECTOR          (ATA_BASE + 0x5c)   
    #define DEV_LOWLBA      (ATA_BASE + 0x60)   
    #define DEV_MIDLBA          (ATA_BASE + 0x64)   
    #define DEV_HIGHLBA     (ATA_BASE + 0x68)   
    #define DEV_DEVICE          (ATA_BASE + 0x6c)   
    #define DEV_STATUS          (ATA_BASE + 0x70)   
    #define DEV_COMMAND     (ATA_BASE + 0x70)   
    #define DEV_ALTANATE        (ATA_BASE + 0x74)   
    #define DEV_CONTROL     (ATA_BASE + 0x74)   
       
    /*=========================================================================  
     *                             ata Command  
     *=========================================================================  
     */   
    #define IDENTIFYDEVICE      0xec   
    #define READSECTOR          0x20   
    #define READMULTIPLE        0xc4   
    #define READDMA         0xc8   
    #define WRITESECTOR     0x30   
    #define WRITEMULTIPLE       0xc5   
    #define WRITEDMA            0xca   
    #define SETFEATURES     0xEF   
       
    //-------------------------------------------------------------------------   
       
    #define ATAPI_MASK                  0xffffffe0   
    #define ATA_SECTORSIZE              512   
       
    #define STATUS_DEVICE_BUSY  0x80   
    #define STATUS_DATA_REQUEST 0x58   
    #define STATUS_ERR              0x1   
       
       
    void SetBufferDirection(U32 dir); // only for SMDK b'd   
       
       
    /*=========================================================================  
     *                  ata controller register functions  
     *=========================================================================  
     */   
       
    void ATA::SetAtaOnOff(U8 OnOff)   
    {   
        U32 temp;   
        Inp32(ATA_CONTROL, temp);   
       
        if(OnOff==1)   
            Outp32(ATA_CONTROL, temp | 0x1);   
        else if(OnOff == 0)   
            Outp32(ATA_CONTROL, temp &0xfffffffe);   
    }   
       
       
    void ATA::SetLittleEndian(void)   
    {   
        // set Little endian           
        m_uCfgReg &= (~0x40);    
        Outp32(ATA_CFG, m_uCfgReg);   
    }   
       
       
    void ATA::SetTransferCommand(ATA_TRANSFER_CMD command)   
    {   
        U8 cmd = (command == ATA_CMD_STOP) ? 0 :   
                (command == ATA_CMD_START) ? 1 :   
                (command == ATA_CMD_ABORT) ? 2 : 3;   
        WaitForHostReady(); /// needed   
        Outp32(ATA_COMMAND, cmd);   
    }   
       
    void ATA::IsTBufFullContinue(bool& status)   
    {   
        U32 temp;   
        Inp32(ATA_CFG, temp);   
        status = (temp&0x80) ? true : false;   
    }   
       
    void ATA::IsSBufEmptyContinue(bool& status)   
    {   
        U32 temp;   
        Inp32(ATA_CFG, temp);   
        status = (temp&0x100) ? true : false;    
    }   
       
       
       
       
    /*=========================================================================  
     *                     ata controller register I/O fuctions  
     *=========================================================================  
     */   
       
       
    void ATA::WaitForHostReady(void)   
    {   
        U32 tempRead;   
       
        do {   
            Inp32(ATA_FIFO_STATUS, tempRead); // modified by Bryan W. Lee (Oct.19th, 2005)   
        } while((tempRead>>28)!=0);   
    }   
       
       
       
    void ATA::ReadDeviceReg(U32 nRegister, U8& data)    
    {   
        U32 tempRead;   
       
        WaitForHostReady();   
        Inp32(nRegister, tempRead);   
        WaitForHostReady();   
        Inp32(ATA_PIO_RDATA, tempRead);   
        data = (U8 )(tempRead&0xFF);   
    }   
       
    void ATA::GetDataFromDevice(U16& data)    
    {   
        U32 tempRead;   
       
        WaitForHostReady();   
        Inp32(ATA_PIO_DTR, tempRead);   
        WaitForHostReady();   
        Inp32(ATA_PIO_RDATA, tempRead);   
        data = (U16)(tempRead&0xFFFF);   
    }   
       
    void ATA::WriteOnTaskFileReg(U32 nRegister,U32 nValue)    
    {   
       
        WaitForHostReady();   
        Outp32(nRegister, nValue);   
    }   
       
    void ATA::PutDataToDevice(U16 data)    
    {   
       
        WaitForHostReady();   
        Outp32(ATA_PIO_DTR, data);   
    }   
       
    void ATA::WaitForTransferDone(void)   
    {   
        U32 x;   
       
        do {   
            WaitForHostReady(); /// needed   
            Inp32(ATA_STATUS, x);   
        } while((x & 3)!=0);   
    }   
       
       
    bool ATA::WaitForDeviceReady(void)   
    {   
        U8 tempRead;   
       
       
        while(1)    
        {   
            ReadDeviceReg(DEV_ALTANATE, tempRead);   
            ReadDeviceReg(DEV_STATUS, tempRead);   
            if((tempRead&STATUS_DEVICE_BUSY) == 0)    
                break;   
        }   
       
        return true;   
    }   
       
       
    void ATA::Clear_Pending(U32 srcInt)   
    {   
    //  Assert(srcInt < SRC_INT_NUM);   
           
        Outp32(ATA_IRQ, (1<<srcInt));   
    }   
       
       
    bool ATA::FindInterruptRequest(U32& nthBit)   
    {   
        U32 i, temp1, temp2;   
       
        for(i=0;i<SRC_INT_NUM;i++) {   
            Inp32(ATA_IRQ, temp1);   
            Inp32(ATA_IRQ_MASK, temp2);   
            if(((temp1&(~temp2))>>i)&0x01)    
                break;   
        }   
       
        nthBit = i;   
           
        if (i == SRC_INT_NUM)   
            return false;   
        else   
            return true;   
           
           
    }   
       
    void ATA::ClearAllInterrupt(void)   
    {   
        Outp32(ATA_IRQ, 0xff);   
        Outp32(ATA_IRQ_MASK, ATAPI_MASK|(1<<ATA_INT_IRQ));   
    }   
       
       
    /*=========================================================================  
     *                          FIU BASIC routine  
     *=========================================================================  
     */   
       
    void ATA::ResetAll(void )   
    {   
        U32 i;   
        U32 temp1, temp2;   
       
        // TDelay timer setting   
        SetResTDelay(10); // TDelay 1 unit = 10us   
        InitTDelayFunc();   
       
        TDelay(203); // 2ms+25us   
           
        Outp32(ATA_SWRST, 0x1);  // CF controller reset   
        TDelay(1); // need 5us   
       
        Outp32(ATA_SWRST, 0x0);   
        TDelay(200); // need 2ms   
       
        Outp32(ATA_CFG, 0x1); // ata device reset.     
        TDelay(1);   
       
        Outp32(ATA_CFG, 0x0); // ata device no reset.   
        TDelay(25000); // need 200ms   
    }   
           
       
       
    void ATA::Init(void)   
    {   
        // TDelay timer setting   
        SetResTDelay(10); // TDelay 1 unit = 10us   
        InitTDelayFunc();   
       
        // GPIO, EBI setting   
        rEBICON |= (1<<10)|(1<<9); // bank3_cfg->CF,bank2_cfg->CF    
        rGPGCON |= (3<<30)|(3<<28)|(3<<26)|(3<<24)|(3<<22); //nCARD_PWREN, RESET_CF,nRE3G_CF,nINPACK,nIREQ_CF   
        rGPACON |= (1<<27)|(1<<11)|(1<<14)|(1<<13);// nWE_CF,nOE_CF,nRCS3,nRCS2 enable   
        rMISCCR &=(~(1<<30)); // card detect when card is detected ,the bit should be '0'.   
       
        // CF controller - True IDE mode setting   
        Outp32(ATA_MUX, 0x07); // Output pad disable, Card power off, ATA mode   
        TDelay(100);   
        Outp32(ATA_MUX, 0x03); // Output pad enable, Card power off, ATA mode   
        TDelay(100);   
        Outp32(ATA_MUX, 0x01); // Output pad enable, Card power on, ATA mode   
        TDelay(40000); // wait for 500ms, be positively necessary   
    //  ChangeMode();   
       
        // Card configuration   
        Outp32(ATA_PIO_TIME, 0x1C238);   
        Outp32(ATA_UDMA_TIME, 0x20B1362);   
        SetLittleEndian();     
        SetAtaOnOff(1);   
        TDelay(20000); // wait for 200ms, be positively necessary   
           
    //  ResetAll(); //x. why?   
        m_uCfgReg = 0;  // configuration register value   
    }   
       
       
    void ATA::IdentifyDevice(void)    
    {   
        U16 readBuffer[ATA_SECTORSIZE/2];   
        U8 tempBuffer;   
        volatile U8 *tempString;   
        U32 tBuf[4];   
        int i;   
       
        for (i=0;i<ATA_SECTORSIZE/2;i++)   
            readBuffer[i] = 1;   
       
           
        m_uCfgReg |= 0x40; // set Big endian (must be)   
        Outp32(ATA_CFG, m_uCfgReg);   
       
        Outp32(ATA_IRQ_MASK, 0xffffffff);   
        WriteOnTaskFileReg(DEV_DEVICE, 0x40);   
        WriteOnTaskFileReg(DEV_COMMAND, IDENTIFYDEVICE);   
       
        WaitForDeviceReady();   
    //  TDelay(100);   
           
        for(i=0; i<ATA_SECTORSIZE/2; i++) {   
            GetDataFromDevice(readBuffer[i]);   
            printf("DATA : 0x%04X\n",readBuffer[i]);   
        }   
        WaitForDeviceReady();   
       
        //   
        //verify identify data~~~~~~~~~~~~~~~~~~~~~~~~~~   
        //   
        tempString = (U8 *)&readBuffer[10];   
        printf("\nSerial Number :");   
        for(i=0;i<20;i++) printf("%c",*(tempString+i));   
       
        tempString = (U8 *)&readBuffer[27];   
        printf("\nModel Number :");   
        for(i=0;i<40;i++) printf("%c",*(tempString+i));   
       
        tBuf[0] = (U8)(readBuffer[61]&0xff);   
        tBuf[1] = (U8)((readBuffer[61]&0xff00)>>8);   
        tBuf[2] = (U8)(readBuffer[60]&0xff);   
        tBuf[3] = (U8)((readBuffer[60]&0xff00)>>8);   
        m_uMaxSectors = (U32)((tBuf[0]<<24)|(tBuf[1]<<16)|(tBuf[2]<<8)|tBuf[3]);   
        printf("\nMax Sectors : %d\n",m_uMaxSectors);   
       
        // Caution: readBuffer[x] - Big Endian, so upper byte means LSB..   
        m_uMaxMultiple= (readBuffer[47]>>8)&0xFF;   
        printf("\nMax Multiple : %02X\n",m_uMaxMultiple);   
        if (readBuffer[59]&0x1) { //multiple sector setting is valid   
            m_uCurrentMultiple= (readBuffer[59]>>8)&0xFF;   
            printf("Current Multiple : %03X\n",m_uCurrentMultiple);   
        }   
           
        if ((readBuffer[64]>>8)&0x3 == 1) m_eMaxPioMode = PIO3;   
        else if ((readBuffer[64]>>8)&0x3 == 3) m_eMaxPioMode = PIO4;   
        else m_eMaxPioMode = PIO2;   
        printf("Max PIO Mode : %d\n",m_eMaxPioMode);   
       
        m_uMaxUdmaMode =0;   
        tempBuffer = readBuffer[88]>>8;   
        for(i=4;i>=0;i--) {   
            if(tempBuffer&(0x01<<i)) {   
                m_uMaxUdmaMode = i;   
                break;    
            }   
        }   
       
        m_uCurrentUdmaMode =0;   
        tempBuffer = readBuffer[88]&0x00ff;   
        for(i=0;i<5;i++) {   
            if(tempBuffer&(0x01<<i)) {   
                m_uCurrentUdmaMode = i;   
                break; ///   
            }   
        }   
       
        printf("Max UDMA Mode : %d\n", m_uMaxUdmaMode);   
        printf("Current UDMA Mode : %d\n", m_uCurrentUdmaMode);   
        //   
        //verify identify data~~~~~~~~~~~~~~~~~~~~~~~END   
        //   
       
        SetLittleEndian();   
    }   
       
    void ATA::GetMaxSectors(U32& maxSec)   
    {   
        maxSec = m_uMaxSectors;   
    }   
       
    void ATA::GetMaxMultiple(U32& maxMulti)   
    {   
        maxMulti = m_uMaxMultiple;   
    }   
       
    /**********************************  
     * PIO mode maximum transfer rate  
     * PIO0 : 3.3MB/s  
     * PIO1 : 5.2MB/s  
     * PIO2 : 8.3MB/s  
     * PIO3 : 11.1MB/s  
     * PIO4 : 16.7MB/s  
     *  
    ***********************************/   
        
    void ATA::SetPioMode(PIOMODE  pmode)    
    {   
        U8 nMode = (pmode == PIO0) ? 0 :   
                    (pmode == PIO1) ? 1 :   
                    (pmode == PIO2) ? 2 :   
                    (pmode == PIO3) ? 3 :   
                    (pmode == PIO4) ? 4 : 0;   
           
        U32 uT1;   
        U32 uT2;   
        U32 uTeoc;   
        U32 i;   
           
        U32 uPioTime[5];   
        U32 m_uPioT1[5] = {200,50,30,50,30};    // min = {70,50,30,30,25};   
        U32 m_uPioT2[5] = {300,300,290,100,70}; // min = {290,290,290,80,70};   
        U32 m_uPioTeoc[5] = {100,40,20,30,20};  // min = {20,15,10,10,10};   
           
        U32 uCycleTime = (U32)(1000000000/HCLK);   
       
       
        for (i=0; i<5; i++)   
        {   
            uT1   = (m_uPioT1[i]  /uCycleTime + 1)&0xff;   
            uT2   = (m_uPioT2[i]  /uCycleTime + 1)&0xff;   
            uTeoc = (m_uPioTeoc[i]/uCycleTime + 1)&0x0f;   
            uPioTime[i] = (uTeoc<<12)|(uT2<<4)|uT1;   
    //      DbgAta(("PIO%dTIME = %x\n", i, uPioTime[i]));   
        }   
           
        Outp32(ATA_IRQ, 0xff);   
        Outp32(ATA_IRQ_MASK, ATAPI_MASK);   
       
       
        WriteOnTaskFileReg(DEV_CONTROL,0);   
        WriteOnTaskFileReg(DEV_FEATURE,0x03); //set transfer mode based on value in Sector Count register   
        WriteOnTaskFileReg(DEV_SECTOR,0x08|(nMode&0x7)); // PIO flow control transfer mode   
        WriteOnTaskFileReg(DEV_LOWLBA,0x00);   
        WriteOnTaskFileReg(DEV_MIDLBA,0x00);   
        WriteOnTaskFileReg(DEV_HIGHLBA,0x00);   
        WriteOnTaskFileReg(DEV_DEVICE,0x40);   
           
        WriteOnTaskFileReg(DEV_COMMAND,SETFEATURES); //set feature command   
       
        WaitForDeviceReady();   
       
        switch(pmode) { // modified by Bryan W. Lee (Oct. 19th, 2005)   
            case PIO1:   
                m_uCfgReg &= (~0x2); //IORDY disable   
                Outp32(ATA_PIO_TIME, uPioTime[1]);   
                break;   
            case PIO2:   
                m_uCfgReg &= (~0x2); //IORDY disable   
                Outp32(ATA_PIO_TIME, uPioTime[2]);   
                break;   
            case PIO3:   
                m_uCfgReg |= 0x2; //IORDY enable   
                Outp32(ATA_PIO_TIME, uPioTime[3]);   
                break;   
            case PIO4:   
                m_uCfgReg |= 0x2; //IORDY enable   
                Outp32(ATA_PIO_TIME, uPioTime[4]);   
                break;   
            default:   
                m_uCfgReg &= (~0x2); //IORDY disable   
                Outp32(ATA_PIO_TIME, uPioTime[0]);   
                break;   
            }   
        Outp32(ATA_CFG, m_uCfgReg);   
        m_eCurrentPioMode = pmode;   
       
    }   
       
    //uSector: the number of sectors per block   
    // if the block count is not supported, an Abort Command error is posted,   
    // and the Read Multiple and Write Multiple commands are disabled.   
    void ATA::SetMultiple(U32 uSector)    
    {   
        m_uCurrentMultiple = uSector;   
       
        WriteOnTaskFileReg(DEV_CONTROL,0);   
        WriteOnTaskFileReg(DEV_SECTOR,uSector&0xff);   
        WriteOnTaskFileReg(DEV_LOWLBA,0x00);   
        WriteOnTaskFileReg(DEV_MIDLBA,0x00);   
        WriteOnTaskFileReg(DEV_HIGHLBA,0x00);   
        WriteOnTaskFileReg(DEV_DEVICE,0x40);   
        WriteOnTaskFileReg(DEV_COMMAND,0xC6); //Set Multiple mode (implemented in ATA IP ??)   
       
        WaitForDeviceReady();   
    }   
       
    /**********************************  
     * UDMA mode maximum transfer rate  
     * UDMA0 : 16.7MB/s  
     * UDMA1 : 25.0MB/s  
     * UDMA2 : 33.3MB/s  
     * UDMA3 : 44.4MB/s  
     * UDMA4 : 66.7MB/s  
     * UDMA5 : 100 MB/s   
     *  
    ***********************************/   
    void ATA::SetUdmaMode(UDMAMODE umode)    
    {   
        U8 nMode;   
        U32 uTdvh1;   
        U32 uTdvs;   
        U32 uTrp;   
        U32 uTss;   
        U32 uTackenv;   
        U32 i;   
       
        U32 uUdmaTime[5];   
        U32 uUdmaTdvh[5] = {20,20,10,10,10}; //{7,7,7,7,7};   
        U32 uUdmaTdvs[5] = {100,60,50,35,20}; //{70,48,31,20,7};   
        U32 uUdmaTrp[5] = {160,125,100,100,100};   
        U32 uUdmaTss[5] = {50,50,50,50,50};   
        U32 uUdmaTackenvMin[5] = {20,20,20,20,20};   
        U32 uUdmaTackenvMax[5] = {70,70,70,55,55};     
        U32 uCycleTime = (U32)(1000000000/HCLK);   
       
        // added GPB setting for SMDK buffer mechanism 060812   
        rGPBDAT |= (3<<5); // GPB5,6 -> H   
        rGPBCON = rGPBCON & ~(0xf<<10) | (5<<10); // GPB5,6 output mode   
           
        for (i=0; i<5; i++)   
        {   
            uTdvh1  = (uUdmaTdvh[i] / uCycleTime + 1)&0x0f;   
            uTdvs   = (uUdmaTdvs[i] / uCycleTime + 1)&0xff;   
            uTrp    = (uUdmaTrp[i]  / uCycleTime + 1)&0xff;   
            uTss    = (uUdmaTss[i]  / uCycleTime + 1)&0x0f;   
            uTackenv= (uUdmaTackenvMin[i]/uCycleTime + 1)&0x0f;   
            uUdmaTime[i] = (uTdvh1<<24)|(uTdvs<<16)|(uTrp<<8)|(uTss<<4)|uTackenv;   
    //      DbgAta(("UDMA%dTIME = %x\n", i, uUdmaTime[i]));   
        }      
       
        Outp32(ATA_IRQ, 0xff);   
        Outp32(ATA_IRQ_MASK, ATAPI_MASK);   
       
        WriteOnTaskFileReg(DEV_CONTROL,0);   
        WriteOnTaskFileReg(DEV_FEATURE,0x03);   
        WriteOnTaskFileReg(DEV_SECTOR,0x40|(nMode&0x7));   
        WriteOnTaskFileReg(DEV_LOWLBA,0x00);   
        WriteOnTaskFileReg(DEV_MIDLBA,0x00);   
        WriteOnTaskFileReg(DEV_HIGHLBA,0x00);   
        WriteOnTaskFileReg(DEV_DEVICE,0x40);   
        WriteOnTaskFileReg(DEV_COMMAND,SETFEATURES);   
       
        WaitForDeviceReady();   
       
           
        switch(umode) {   
            case UDMA0:   
                nMode = 0;   
                Outp32(ATA_UDMA_TIME, uUdmaTime[0]);   
                break;         
            case UDMA1:   
                nMode = 1;   
                Outp32(ATA_UDMA_TIME, uUdmaTime[1]);   
                break;   
            case UDMA2:   
                nMode = 2;   
                Outp32(ATA_UDMA_TIME, uUdmaTime[2]);   
                break;   
            case UDMA3:   
                nMode = 3;   
                Outp32(ATA_UDMA_TIME, uUdmaTime[3]);   
                break;   
            case UDMA4:   
                nMode = 4;   
                Outp32(ATA_UDMA_TIME, uUdmaTime[4]);   
                break;   
            default:   
                DbgAta(("Wrong UDMA mode in SetUdmaMode()\n"));   
                break;   
            }   
        m_uCurrentUdmaMode = nMode;   
           
       
    }   
       
    bool ATA::OpenMedia(void)   
    {   
        ATA_MODE mode;   
           
        Init();   
       
        IdentifyDevice();   
       
        // Decide what mode is best choice in this media.   
        if (m_uMaxUdmaMode == 0)   
        {   
            mode = PIO_DMA;   
        }   
        else   
        {   
            mode = UDMA;   
        }   
       
        SetAtaMode(mode);   
        if (mode == PIO_CPU || mode == PIO_DMA)   
        {   
            SetPioMode(m_eMaxPioMode);                 
        }   
        else    
        {   
            UDMAMODE udmaMode =    
                (m_uMaxUdmaMode == 0) ? UDMA0 :   
                (m_uMaxUdmaMode == 1) ? UDMA1 :   
                (m_uMaxUdmaMode == 2) ? UDMA2 :   
                (m_uMaxUdmaMode == 3) ? UDMA3 : UDMA4;   
            SetPioMode(PIO0);   
            SetUdmaMode(udmaMode);         
        }   
       
        return true;   
    }   
       
    bool ATA::OpenMedia(ATA_MODE mode)   
    {      
        Init();   
       
        IdentifyDevice();   
       
        SetAtaMode(mode);   
        if (mode == PIO_CPU || mode == PIO_DMA)   
        {   
            SetPioMode(m_eMaxPioMode);                 
        }   
        else    
        {   
            UDMAMODE udmaMode =    
                (m_uMaxUdmaMode == 0) ? UDMA0 :   
                (m_uMaxUdmaMode == 1) ? UDMA1 :   
                (m_uMaxUdmaMode == 2) ? UDMA2 :   
                (m_uMaxUdmaMode == 3) ? UDMA3 : UDMA4;   
            SetPioMode(PIO0);   
            SetUdmaMode(udmaMode);         
        }   
       
        return true;   
    }   
       
    bool ATA::CloseMedia(void)   
    {   
        SetAtaOnOff(0);   
       
        // Output pad disable, Card power off, PC card mode   
        Outp32(ATA_MUX, 0x06);   
       
        return true;   
    }   
       
    bool ATA::ReadBlocks(U32 uStBlock, U32 uBlocks, U32 uBufAddr)   
    {   
    //  Assert(uStBlock+uBlocks <= m_uMaxSectors);   
           
        U32 k;   
        ATA_MODE mode = m_eMode;   
        bool status;   
           
        switch(mode)    
        {   
            case PIO_CPU:   
                status = ReadSectors_Pio( uStBlock, uBlocks, uBufAddr);   
                break;   
                   
            case PIO_DMA:   
                for(k=0; k<uBlocks; k++)    
                {   
                    ReadSector_PioDma(uStBlock+k, uBufAddr+ATA_SECTORSIZE*k);   
                    WaitForTransferDone();   
                }   
                status = true; // need to move..   
                break;   
                   
            case UDMA:   
                status = ReadSectors_Udma( uStBlock, uBlocks, uBufAddr);   
                break;   
                   
            default:   
                DbgAta(("Not supported mode in ReadBlocks()\n"));   
                break;   
        }   
        return status;   
    }   
       
    bool ATA::WriteBlocks(U32 uStBlock, U32 uBlocks, U32 uBufAddr)   
    {   
    //  Assert(uStBlock+uBlocks <= m_uMaxSectors);   
           
        U32 k;   
        ATA_MODE mode = m_eMode;   
        bool status;   
           
        switch(mode)    
        {   
            case PIO_CPU:   
                status = WriteSectors_Pio( uStBlock, uBlocks, uBufAddr);   
                break;   
                   
            case PIO_DMA:   
                for(k=0; k<uBlocks; k++)   
                {   
                    WriteSector_PioDma(uStBlock+k, uBufAddr+ATA_SECTORSIZE*k);   
                    WaitForTransferDone();   
                }   
                status = true;   
                break;   
            case UDMA:             
                status = WriteSectors_Udma( uStBlock, uBlocks, uBufAddr);   
                break;   
            default:   
                DbgAta(("Not supported mode in WriteBlocks()\n"));   
                break;   
        }   
           
        return status;   
    }   
       
       
    void SetBufferDirection(U32 dir) // only for SMDK b'd 060812   
    {   
        switch(dir)   
        {   
            case READDMA :   
                rGPBDAT &= ~(3<<5); // GPB5 -> L, buffer direction - read setting    
                                    // GPB6 -> L, buffer Output enable    
                break;   
            case WRITEDMA :   
                rGPBDAT |= (1<<5);    // GPB5 -> H, buffer direction - write setting    
                rGPBDAT &= ~(1<<6); // GPB6 -> L, buffer Output enable 060812   
                break;   
            default : // clear   
                rGPBDAT |= (3<<5);    // GPB5 -> H, buffer direction - write setting    
                                    // GPB6 -> H, buffer Output disable    
                break;   
        }          
    }   
       
       
    bool ATA::StartReadingBlocks(U32 uStBlock, U32 uBlocks, U32 uBufAddr)   
    {   
    //  Assert(!(m_eMode == PIO_CPU));   
           
        U32 deviceCmd = (m_eMode == UDMA) ? READDMA : READSECTOR;   
           
        /*Track Buffer 1 Setting*/   
        Outp32(ATA_TBUF_START, uBufAddr);   
        Outp32(ATA_TBUF_SIZE, uBlocks*ATA_SECTORSIZE);   
        Outp32(ATA_XFR_NUM, uBlocks*ATA_SECTORSIZE);   
       
        SetAtaDevice(uStBlock, uBlocks);   
        WriteOnTaskFileReg(DEV_COMMAND, deviceCmd);   
       
        WaitForDeviceReady();   
           
        SetConfigMode(m_eMode, false);   
        if (m_eMode == UDMA) // apply at specific set using buffer control   
        {   
            printf("Ready for reading data in UDMA. Change dip SW & Press Enter..\n");   
            getchar();   
            SetBufferDirection(READDMA);   
        }   
           
        SetTransferCommand(ATA_CMD_START);   
           
        return true;   
    }   
       
    bool ATA::IsReadingBlocksDone(void)   
    {   
        return IsWritingBlocksDone();   
    }   
       
    void ATA::ContinueReadingBlocks(void)   
    {   
       
    }   
       
       
    bool ATA::StartWritingBlocks(U32 uStBlock, U32 uBlocks, U32 uBufAddr)   
    {   
    //  Assert(!(m_eMode == PIO_CPU));   
       
        U32 deviceCmd = (m_eMode == UDMA) ? WRITEDMA : WRITESECTOR;   
       
           
        /*Source Buffer Setting*/   
        Outp32(ATA_SBUF_START, uBufAddr);   
        Outp32(ATA_SBUF_SIZE, uBlocks*ATA_SECTORSIZE);   
        Outp32(ATA_XFR_NUM,   uBlocks*ATA_SECTORSIZE);   
           
        SetAtaDevice(uStBlock, uBlocks);   
        WriteOnTaskFileReg(DEV_COMMAND, deviceCmd);   
       
        WaitForDeviceReady();   
       
        SetConfigMode(m_eMode, true);      
        if (m_eMode == UDMA) // apply at specific set using buffer control   
        {   
            printf("Ready for writing data in UDMA. Change dip SW & Press Enter..\n");   
            getchar();   
            SetBufferDirection(WRITEDMA); // for SMDK buffer direction only   
        }   
       
        SetTransferCommand(ATA_CMD_START);   
       
        return true;   
    }   
       
    bool ATA::IsWritingBlocksDone(void)   
    {   
        WaitForTransferDone();   
           
        return IsDmaDone();   
    }   
       
    void ATA::ContinueWritingBlocks(void)   
    {   
       
    }   
       
    bool ATA::IsDmaDone(void)   
    {   
        SetTransferCommand(ATA_CMD_ABORT);   
        if (m_eMode == UDMA) // apply at specific set using buffer control   
        {   
            SetBufferDirection(0); // clear   
            printf("Return to PIO mode. Change dip SW & Press Enter..\n");   
            getchar();   
        }   
       
        SetConfigMode(PIO_CPU, true);   
        WaitForDeviceReady();   
       
        if (m_eMode == UDMA)   
        {   
            m_uCfgReg &= (~0x200); // disable ATA DMA auto mode   
            Outp32(ATA_CFG, m_uCfgReg);    
        }   
        return true;   
    }   
       
    void ATA::ChangeMode(void)   
    {   
    #ifdef CFCON_MODE   
        SetResTDelay(10); // TDelay 1 unit = 10us   
        InitTDelayFunc();   
           
        // CF controller - True IDE mode setting   
        Outp32(ATA_MUX, 0x07); // Output pad disable, Card power off, ATA mode   
        TDelay(100);   
        Outp32(ATA_MUX, 0x03); // Output pad enable, Card power off, ATA mode   
        TDelay(100);   
        Outp32(ATA_MUX, 0x01); // Output pad enable, Card power on, ATA mode   
        TDelay(50000); // wait for 500ms   
    #endif   
    }   
       
    void ATA::SetAtaDevice(U32 uLBA, U32 uSectorCount)   
    {   
        WriteOnTaskFileReg(DEV_SECTOR,uSectorCount&0xFF);   
        WriteOnTaskFileReg(DEV_LOWLBA,uLBA&0xFF);   
        WriteOnTaskFileReg(DEV_MIDLBA,(uLBA>>8)&0xFF);   
        WriteOnTaskFileReg(DEV_HIGHLBA,(uLBA>>16)&0xFF);   
        WriteOnTaskFileReg(DEV_DEVICE,((uLBA>>24)&0xF)|0x40);   
    }   
       
       
    void ATA::SetConfigMode(ATA_MODE mode, bool isWriteMode)   
    {   
        switch(mode)   
        {   
            case PIO_CPU:   
                m_uCfgReg = ((m_uCfgReg&0x1F3) | (0<<2)); // set PIO_CPU class   
                break;   
            case PIO_DMA:   
                m_uCfgReg = ((m_uCfgReg&0x1F3) | (1<<2)); // set PDMA class   
                if (isWriteMode == true)   
                    m_uCfgReg |= 0x10; // DMA write mode   
                else   
                    m_uCfgReg &= (~0x10); // DMA read mode   
                break;   
            case UDMA:   
                m_uCfgReg = ((m_uCfgReg&0x1F3) | (2<<2)); // set UDMA class   
                m_uCfgReg |= 0x200; // set ATA DMA auto mode (enable multi block transfer)   
                if (isWriteMode == true)   
                    m_uCfgReg |= 0x10; // DMA write mode   
                else   
                    m_uCfgReg &= (~0x10); // DMA read mode   
                break;   
            default:   
                break;   
        }   
        Outp32(ATA_CFG, m_uCfgReg);   
    }   
       
    bool ATA::WriteSectors_Pio(U32 nLBA,U32 nSectorCount,U32 nSrcAddress)    
    {   
        U32 uCurrentCount;   
        U32 uRemainderCount;   
        U32 uCurrentLba;   
        U32 uCurrentSrcAddr;   
        U32 uRound;   
        U16* uAtaHostAddr;   
        U32 i;   
           
        uRemainderCount = nSectorCount;   
        uCurrentLba = nLBA;   
        uRound = 0;    
       
           
        while(uRemainderCount != 0) {   
            if(uRemainderCount>256) {   
                uCurrentCount = 256; //0 means 256   
                uRemainderCount -= 256;   
            } else {   
                uCurrentCount = uRemainderCount;   
                uRemainderCount = 0;   
            }   
            uCurrentLba = nLBA + uRound*256;   
            uCurrentSrcAddr = nSrcAddress + uRound*256*ATA_SECTORSIZE;   
            uAtaHostAddr = (U16*)uCurrentSrcAddr;   
               
            SetAtaDevice(uCurrentLba, uCurrentCount);   
            WriteOnTaskFileReg(DEV_COMMAND, WRITESECTOR);   
       
            while(uCurrentCount-- ) {      
                WaitForDeviceReady();   
                for (i=0;i<ATA_SECTORSIZE/2;i++) {              
                    PutDataToDevice(*uAtaHostAddr);   
                    uAtaHostAddr++;   
                }   
            }          
            WaitForDeviceReady();   
       
            uRound++;   
        }   
        return true;   
    }   
       
       
    bool ATA::ReadSectors_Pio(U32 uLBA, U32 uSectorCount, U32 uDesAddress)    
    {   
        U32 uCurrentCount;   
        U32 uRemainderCount;   
        U32 uCurrentLba;   
        U32 uCurrentDstAddr;   
        U32 uRound;   
        U16* uAtaHostAddr;   
        U32 i;   
           
        uRemainderCount = uSectorCount;   
        uCurrentLba = uLBA;   
        uRound = 0;    
       
           
        while(uRemainderCount != 0) {   
            if(uRemainderCount>256) {   
                uCurrentCount = 256; //0 means 256   
                uRemainderCount -= 256;   
            } else {   
                uCurrentCount = uRemainderCount;   
                uRemainderCount = 0;   
            }   
            uCurrentLba = uLBA + uRound*256;   
            uCurrentDstAddr = uDesAddress + uRound*256*ATA_SECTORSIZE;   
            uAtaHostAddr = (U16*)uCurrentDstAddr;   
       
            SetAtaDevice(uCurrentLba, uCurrentCount);   
            WriteOnTaskFileReg(DEV_COMMAND, READSECTOR);   
       
            while(uCurrentCount-- ) {   
                WaitForDeviceReady();   
                for (i=0;i<ATA_SECTORSIZE/2;i++) {   
                     GetDataFromDevice(*uAtaHostAddr);   
                     uAtaHostAddr++;   
                }   
            }   
            WaitForDeviceReady();   
       
            uRound++;   
        }   
        return true;   
    }   
       
       
       
       
    bool ATA::WriteSector_PioDma(U32 uLBA, U32 uSrcAddress)    
    {   
    #if 1   
        StartWritingBlocks(uLBA, 1, uSrcAddress);   
        return IsWritingBlocksDone();   
    #else      
        /*Source Buffer 1 Setting*/   
        Outp32(ATA_SBUF_START, uSrcAddress);   
        Outp32(ATA_SBUF_SIZE, 1*ATA_SECTORSIZE);   
        Outp32(ATA_XFR_NUM,   1*ATA_SECTORSIZE);   
           
        SetAtaDevice(uLBA, 1);   
        WriteOnTaskFileReg(DEV_COMMAND, WRITESECTOR);   
       
        WaitForDeviceReady();   
       
        SetConfigMode(PIO_DMA, true);   
           
        SetTransferCommand(ATA_CMD_START);   
        WaitForTransferDone();   
        SetTransferCommand(ATA_CMD_ABORT);   
       
        SetConfigMode(PIO_CPU, true);   
       
        WaitForDeviceReady();   
           
        return true;   
    #endif     
    }   
       
    bool ATA::ReadSector_PioDma(U32 uLBA, U32 uDesAddress)    
    {   
    #if 1   
        StartReadingBlocks(uLBA, 1, uDesAddress);   
        return IsReadingBlocksDone();   
    #else   
        /*Track Buffer 1 Setting*/   
        Outp32(ATA_TBUF_START, uDesAddress);   
        Outp32(ATA_TBUF_SIZE, 1*ATA_SECTORSIZE);   
        Outp32(ATA_XFR_NUM, 1*ATA_SECTORSIZE);   
       
        SetAtaDevice(uLBA, 1);   
        WriteOnTaskFileReg(DEV_COMMAND, READSECTOR);   
           
        WaitForDeviceReady();   
               
        SetConfigMode(PIO_DMA, false);   
               
        SetTransferCommand(ATA_CMD_START);   
        WaitForTransferDone();   
        SetTransferCommand(ATA_CMD_ABORT);   
       
        SetConfigMode(PIO_CPU, false);   
       
        WaitForDeviceReady();   
                   
        return true;   
    #endif     
    }   
       
       
       
    bool ATA::WriteSectors_MultiplePioDma(U32 uLBA, U32 uSrcAddress)    
    {      
           
        /*Source Buffer 1 Setting*/   
        Outp32(ATA_SBUF_START, uSrcAddress);   
        Outp32(ATA_SBUF_SIZE, m_uCurrentMultiple*ATA_SECTORSIZE);   
        Outp32(ATA_XFR_NUM,   m_uCurrentMultiple*ATA_SECTORSIZE);   
           
        SetAtaDevice(uLBA, m_uCurrentMultiple);   
        WriteOnTaskFileReg(DEV_COMMAND, WRITEMULTIPLE);   
       
        WaitForDeviceReady();   
        SetConfigMode(PIO_DMA, true);   
               
        /*ATA Transfer Command */   
        SetTransferCommand(ATA_CMD_START);   
        WaitForTransferDone();   
        SetTransferCommand(ATA_CMD_ABORT);   
       
        SetConfigMode(PIO_CPU, true);   
        WaitForDeviceReady();   
           
        return true;   
    }   
       
    bool ATA::ReadSectors_MultiplePioDma(U32 uLBA, U32 uDesAddress)    
    {   
        /*Track Buffer 1 Setting*/   
        Outp32(ATA_TBUF_START, uDesAddress);   
        Outp32(ATA_TBUF_SIZE, (m_uCurrentMultiple*ATA_SECTORSIZE));   
        Outp32(ATA_XFR_NUM,  (m_uCurrentMultiple*ATA_SECTORSIZE));   
       
        SetAtaDevice(uLBA, m_uCurrentMultiple);   
        WriteOnTaskFileReg(DEV_COMMAND, READMULTIPLE);   
           
        WaitForDeviceReady();   
        SetConfigMode(PIO_DMA, false);   
       
        SetTransferCommand(ATA_CMD_START);   
        WaitForTransferDone();   
        SetTransferCommand(ATA_CMD_ABORT);   
       
        SetConfigMode(PIO_CPU, false);   
        WaitForDeviceReady();   
       
        return true;   
    }   
       
    bool ATA::WriteSectors_Udma(U32 uLBA, U32 uSectorCount, U32 uSrcAddress)    
    {   
        U32 uCurrentCount;   
        U32 uRemainderCount;   
        U32 uCurrentLba;   
        U32 uCurrentSrcAddr;   
        U32 uRound;   
           
        uRemainderCount = uSectorCount;   
        uRound = 0;    
       
           
        while(uRemainderCount != 0) {   
            if(uRemainderCount>256) {   
                uCurrentCount = 256; //0 means 256   
                uRemainderCount -= 256;   
            } else {   
                uCurrentCount = uRemainderCount;   
                uRemainderCount = 0;   
            }   
            uCurrentLba = uLBA + uRound*256;   
            uCurrentSrcAddr = uSrcAddress + uRound*256*ATA_SECTORSIZE;   
    #if 1   
            StartWritingBlocks(uCurrentLba, uCurrentCount, uCurrentSrcAddr);   
            IsWritingBlocksDone();   
    #else   
            /*Source Buffer 1 Setting*/   
            Outp32(ATA_SBUF_START, uCurrentSrcAddr);   
            Outp32(ATA_SBUF_SIZE, uCurrentCount*ATA_SECTORSIZE);   
            Outp32(ATA_XFR_NUM,   uCurrentCount*ATA_SECTORSIZE);       
       
            SetAtaDevice(uCurrentLba, uCurrentCount);   
            WriteOnTaskFileReg(DEV_COMMAND,WRITEDMA);   
       
            WaitForDeviceReady();   
            SetConfigMode(UDMA, true);   
           
            printf("Ready for writing in UDMA. Change dip SW & Press Enter..\n");   
            getchar();   
       
            /*ATA Transfer Command */   
            SetBufferDirection(WRITEDMA);   
            SetTransferCommand(ATA_CMD_START);   
       
    #if 1   
            WaitForTransferDone();   
    #else  // abort operation   
            while(1){   
                Inp32(ATA_XFR_CNT, temp);   
                if(temp <= uCurrentCount*ATA_SECTORSIZE) break;   
            }   
    #endif   
            SetTransferCommand(ATA_CMD_ABORT);   
            SetBufferDirection(0); // clear to H   
       
            printf("Return to PIO mode. Change dip SW & Press Enter..\n");   
            getchar();   
       
            SetConfigMode(PIO_CPU, true);   
            WaitForDeviceReady();   
               
            m_uCfgReg &= (~0x200); // disable ATA DMA auto mode   
            Outp32(ATA_CFG, m_uCfgReg);    
    #endif   
            uRound++;   
        }   
       
        return true;   
       
    }   
       
       
    bool ATA::ReadSectors_Udma(U32 uLBA, U32 uSectorCount, U32 uDstAddress)    
    {   
        U32 uCurrentCount;   
        U32 uRemainderCount;   
        U32 uCurrentLba;   
        U32 uCurrentDstAddr;   
        U32 uRound;   
           
        uRemainderCount = uSectorCount;   
        uRound = 0;    
       
           
        while(uRemainderCount != 0) {   
            if(uRemainderCount>256) {   
                uCurrentCount = 256; //0 means 256   
                uRemainderCount -= 256;   
            } else {   
                uCurrentCount = uRemainderCount;   
                uRemainderCount = 0;   
            }   
            uCurrentLba = uLBA + uRound*256;   
            uCurrentDstAddr = uDstAddress + uRound*256*ATA_SECTORSIZE;   
    #if 1   
            StartReadingBlocks(uCurrentLba, uCurrentCount, uCurrentDstAddr);   
            IsReadingBlocksDone();   
    #else   
            /*Track Buffer 1 Setting*/   
            Outp32(ATA_TBUF_START, uCurrentDstAddr);   
            Outp32(ATA_TBUF_SIZE, uCurrentCount*ATA_SECTORSIZE);   
            Outp32(ATA_XFR_NUM, uCurrentCount*ATA_SECTORSIZE);   
       
            SetAtaDevice(uCurrentLba, uCurrentCount);   
            WriteOnTaskFileReg(DEV_COMMAND,READDMA);   
       
            WaitForDeviceReady();   
            SetConfigMode(UDMA, false);   
       
            printf("Ready for reading in UDMA. Change dip SW & Press Enter..\n");   
            getchar();   
       
            /*ATA Transfer Command */          
            SetBufferDirection(READDMA);   
            SetTransferCommand(ATA_CMD_START);   
       
            WaitForTransferDone(); // Host   
       
            SetTransferCommand(ATA_CMD_ABORT);     
            SetBufferDirection(0); // clear to H   
       
            printf("Return to PIO mode. Change dip SW & Press Enter..\n");   
            getchar();   
       
            SetConfigMode(PIO_CPU, false);   
            WaitForDeviceReady();   
       
            m_uCfgReg &= (~0x200); // disable ATA DMA auto mode   
            Outp32(ATA_CFG, m_uCfgReg);   
    #endif         
            uRound++;   
        }   
       
        return true;   
    }   
       
       


