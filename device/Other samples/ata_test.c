

www.pudn.com > S3C2443_Test_program.zip > ata_test.cpp, change:2006-08-21,size:15167b
 


    #include <stdio.h>   
    #include "2443addr.h"   
    #include "console.h"   
    #include "system.h"   
    #include "etc.h"   
    #include "ata.h"   
    #include "cf.h"   
       
       
    #define CODEC_MEM_ST 0x31000000   
       
    static ATA  oAta;   
    //static INTC oIntc;   
       
    static bool bIsDone;   
    static bool bIsXferDone;   
       
       
    void __irq Isr_Ata(void)    
    {   
        U32 nthBit;   
        bool status;   
       
    printf("[I]");         
        if (!oAta.FindInterruptRequest(nthBit)) {   
            ClearPending(BIT_CFCON);   
            //Assert(0);   
        }   
       
        oAta.Clear_Pending(nthBit);   
       
        if (nthBit == 0 || nthBit == 1)   
        {   
            oAta.IsDmaDone();   
            bIsXferDone = true;   
    //      Dbg("x");   
        }   
        else if (nthBit == 2)   
        {   
            bIsDone = true;   
    //      Dbg("b");   
        }   
       
        ClearPending(BIT_CFCON);       
       
        if (nthBit == 3)   
        {   
    //      Dbg("t");   
            oAta.IsTBufFullContinue(status);   
            if (status)    
                oAta.SetTransferCommand(ATA_CMD_CONTINUE);   
        }   
        else if (nthBit == 4)   
        {   
    //      Dbg("s");   
            oAta.IsSBufEmptyContinue(status);   
            if (status)    
                oAta.SetTransferCommand(ATA_CMD_CONTINUE);   
        }   
       
    }   
       
       
    void TestReset(void)   
    {   
        oAta.ResetAll();   
    }   
       
       
    void TestChangeModeToAta(void)   
    {   
        oAta.ChangeMode();   
    }   
       
       
    // No need to select mode in OpenMedia()   
    void TestBasicWriteRead(void)   
    {   
        U32 uWriteBuf, uReadBuf;   
        U32 deviceLBA;   
        U32 uSector;   
        U32 uDeviceMaxSectors;   
       
        uWriteBuf = CODEC_MEM_ST;   
        uReadBuf = CODEC_MEM_ST + 0x400000;   
           
        oAta.OpenMedia();   
       
        oAta.GetMaxSectors(uDeviceMaxSectors);   
       
        printf("\nInput device sector address[max: 0x%x]\n",uDeviceMaxSectors);   
        deviceLBA = (U32)GetIntNum();   
       
        printf("Input sector count[max: 0x%x]\n",uDeviceMaxSectors-deviceLBA);   
        uSector = (U32)GetIntNum();   
       
        for (U32 i=0; i<uSector*512; i++)   
        {   
            Outp8(uWriteBuf+i, (i+0)%256);   
            Outp8(uReadBuf+i, 0);   
        }   
       
        printf("Press Enter \n");      
        getchar();   
           
        oAta.WriteBlocks(deviceLBA, uSector, uWriteBuf);   
        oAta.ReadBlocks( deviceLBA, uSector, uReadBuf);   
       
        if (Compare32(uWriteBuf, uReadBuf, uSector*128) == false)   
        {   
            printf("Error detected\n");   
            Dump32(uReadBuf, uSector*128);   
        }   
        else   
        {   
            printf("Write/Read operation is OK\n");   
        }   
        oAta.CloseMedia();   
    }   
       
       
    void TestPioCpuMode(void)   
    {   
        U32 uWriteBuf, uReadBuf;   
        U32 deviceLBA;   
        U32 uSector;   
        U32 uDeviceMaxSectors;   
       
        uWriteBuf = CODEC_MEM_ST;   
        uReadBuf = CODEC_MEM_ST + 0x400000;   
           
        oAta.OpenMedia(PIO_CPU);   
       
        oAta.GetMaxSectors(uDeviceMaxSectors);   
       
        printf("\nInput device sector address[max: 0x%x]\n",uDeviceMaxSectors);   
        deviceLBA = (U32)GetIntNum();   
       
        printf("Input sector count[max: 0x%x]\n",uDeviceMaxSectors-deviceLBA);   
        uSector = (U32)GetIntNum();   
       
        for (U32 i=0; i<uSector*512; i++)   
        {   
            Outp8(uWriteBuf+i, (i+1)%256);   
            Outp8(uReadBuf+i, 0);   
        }   
           
        oAta.WriteBlocks(deviceLBA, uSector, uWriteBuf);   
        oAta.ReadBlocks( deviceLBA, uSector, uReadBuf);   
       
        if (Compare32(uWriteBuf, uReadBuf, uSector*128) == false)   
        {   
            printf("Error detected\n");   
            Dump32(uReadBuf, uSector*128);   
        }   
        else   
        {   
            printf("Write/Read in PIO_CPU mode is OK\n");   
        }   
           
        oAta.CloseMedia();   
    }   
       
       
    void TestPioCpuModeRead(void)   
    {   
        U32 uReadBuf;   
        U32 deviceLBA;   
        U32 uSector;   
        U32 uDeviceMaxSectors;   
       
        uReadBuf = CODEC_MEM_ST + 0x400000;   
           
        oAta.OpenMedia(PIO_CPU);   
        oAta.GetMaxSectors(uDeviceMaxSectors);   
       
        printf("\nInput device sector address[max: 0x%x]\n",uDeviceMaxSectors);   
        deviceLBA = (U32)GetIntNum();   
       
        printf("Input sector count[max: 0x%x]\n",uDeviceMaxSectors-deviceLBA);   
        uSector = (U32)GetIntNum();   
       
        for (U32 i=0; i<uSector*512; i=i+4)   
        {   
            Outp32(uReadBuf+i, 0);   
        }   
           
        oAta.ReadBlocks( deviceLBA, uSector, uReadBuf);   
           
        Dump32(uReadBuf, uSector*128);   
       
        oAta.CloseMedia();   
    }   
       
       
    void TestPioDmaMode(void)   
    {   
        U32 uWriteBuf, uReadBuf;   
        U32 deviceLBA;   
        U32 uSector;   
        U32 uDeviceMaxSectors;   
       
        uWriteBuf = CODEC_MEM_ST;   
        uReadBuf = CODEC_MEM_ST + 0x400000;   
       
        oAta.OpenMedia(PIO_DMA);   
        oAta.GetMaxSectors(uDeviceMaxSectors);   
           
        printf("\nInput device sector address[max: 0x%x]\n",uDeviceMaxSectors);   
        deviceLBA = (U32)GetIntNum();   
       
        printf("Input sector count[max: 0x%x]\n",uDeviceMaxSectors-deviceLBA);   
        uSector = (U32)GetIntNum();   
       
        for (U32 i=0; i<uSector*512; i++)   
        {   
            Outp8(uWriteBuf+i, (i+2)%256);   
            Outp8(uReadBuf+i, 0);   
        }   
       
        printf("Get Ready\n");   
        getchar();   
       
        oAta.WriteBlocks(deviceLBA, uSector, uWriteBuf);   
    //  printf("Write Done\n");   
       
        oAta.ReadBlocks( deviceLBA, uSector, uReadBuf);   
    //  printf("Read Done\n");   
           
        if (Compare32(uWriteBuf, uReadBuf, uSector*128) == false)   
        {   
            printf("Error detected\n");   
            Dump32(uReadBuf, uSector*128);   
        }   
        else   
        {   
            printf("Write/Read in PIO DMA mode is OK\n");   
        }      
       
        oAta.CloseMedia();   
    }   
       
    void TestPioDmaModeRead(void)   
    {   
        U32 uReadBuf;   
        U32 deviceLBA;   
        U32 uSector;   
        U32 uDeviceMaxSectors;   
       
        uReadBuf = CODEC_MEM_ST + 0x400000;   
       
        oAta.OpenMedia(PIO_DMA);   
        oAta.GetMaxSectors(uDeviceMaxSectors);   
           
        printf("\nInput device sector address[max: 0x%x]\n",uDeviceMaxSectors);   
        deviceLBA = (U32)GetIntNum();   
       
        printf("Input sector count[max: 0x%x]\n",uDeviceMaxSectors-deviceLBA);   
        uSector = (U32)GetIntNum();   
       
        for (U32 i=0; i<uSector*512; i=i+4)   
        {   
            Outp32(uReadBuf+i, 0);   
        }   
       
        oAta.ReadBlocks( deviceLBA, uSector, uReadBuf);   
       
        Dump32(uReadBuf, uSector*128);   
       
        oAta.CloseMedia();   
    }   
       
    void TestUdmaMode(void)   
    {   
        U32 uWriteBuf, uReadBuf;   
        U32 deviceLBA;   
        U32 uSector;   
        U32 uDeviceMaxSectors;   
       
        uWriteBuf = CODEC_MEM_ST;   
        uReadBuf = CODEC_MEM_ST + 0x400000;   
       
        oAta.OpenMedia(UDMA);   
        oAta.GetMaxSectors(uDeviceMaxSectors);   
       
        printf("\nInput device sector address[max: 0x%x]\n",uDeviceMaxSectors);   
        deviceLBA = (U32)GetIntNum();   
       
        printf("Input sector count[max: 0x%x]\n",uDeviceMaxSectors-deviceLBA);   
        uSector = (U32)GetIntNum();   
       
        for (U32 i=0; i<uSector*512; i++)   
        {   
            Outp8(uWriteBuf+i, (i+3)%256);   
            Outp8(uReadBuf+i, 0);   
        }   
       
        oAta.WriteBlocks(deviceLBA, uSector, uWriteBuf);   
       
        printf("Write Done\n");   
        getchar();   
        oAta.ReadBlocks( deviceLBA, uSector, uReadBuf);   
       
        printf("Read Done\n");   
           
        if (Compare32(uWriteBuf, uReadBuf, uSector*128) == false)   
        {   
            printf("Error detected\n");   
            Dump32(uReadBuf, uSector*128);   
        }   
        else   
        {   
            printf("Write/Read in UDMA mode is OK\n");   
        }      
       
        printf("Reset added dip SW for general SMC mode! Press Enter\n");   
        getchar();   
       
        oAta.CloseMedia();   
    }   
       
    void TestUdmaModeRead(void)   
    {   
        U32 uReadBuf;   
        U32 deviceLBA;   
        U32 uSector;   
        U32 uDeviceMaxSectors;   
       
        uReadBuf = CODEC_MEM_ST + 0x400000;        
       
        oAta.OpenMedia(UDMA);   
        oAta.GetMaxSectors(uDeviceMaxSectors);   
       
        printf("\nInput device sector address[max: 0x%x]\n",uDeviceMaxSectors);   
        deviceLBA = (U32)GetIntNum();   
       
        printf("Input sector count[max: 0x%x]\n",uDeviceMaxSectors-deviceLBA);   
        uSector = (U32)GetIntNum();   
       
        for (U32 i=0; i<uSector*512; i=i+4)   
        {   
            Outp32(uReadBuf+i, 0);   
        }   
           
        oAta.ReadBlocks( deviceLBA, uSector, uReadBuf);   
       
        Dump32(uReadBuf, uSector*128);   
       
        oAta.CloseMedia();   
    }   
       
    void TestPioDmaMode_Int(void)   
    {   
        U32 uWriteBuf, uReadBuf;   
        U32 deviceLBA;   
        U32 uSector;   
        U32 uDeviceMaxSectors;   
       
        uWriteBuf = CODEC_MEM_ST;   
        uReadBuf = CODEC_MEM_ST + 0x400000;   
       
        oAta.OpenMedia(PIO_DMA);   
        oAta.GetMaxSectors(uDeviceMaxSectors);   
           
        printf("\nInput device sector address[max: 0x%x]\n",uDeviceMaxSectors);   
        deviceLBA = (U32)GetIntNum();   
       
        printf("Input sector count[max: 0x%x]\n",uDeviceMaxSectors-deviceLBA);   
        uSector = (U32)GetIntNum();   
       
        for (U32 i=0; i<uSector*512; i++)   
        {   
            Outp8(uWriteBuf+i, (i+4)%256);   
            Outp8(uReadBuf+i, 0);   
        }   
       
        bIsDone = false;   
        bIsXferDone = false;   
       
    //  oIntc.Init();          
        oAta.ClearAllInterrupt();   
        pISR_CFCON = (unsigned)Isr_Ata;     
        rINTMSK &= (~BIT_CFCON); // unmask cfcon    
           
    //    oIntc.SetHandlerAndUnmask(BIT_CFCON, Isr_Ata);   
           
        for (U32 i=0; i<uSector; i++)   
        {   
            oAta.StartWritingBlocks(deviceLBA+i, 1, uWriteBuf+i*512);   
            while(bIsXferDone != true) ;   
            bIsXferDone = false;       
        }   
       
        for (U32 i=0; i<uSector; i++)   
        {   
            oAta.StartReadingBlocks(deviceLBA+i, 1, uReadBuf+i*512);   
            while(bIsXferDone != true) ;   
            bIsXferDone = false;       
        }   
           
        if (Compare32(uWriteBuf, uReadBuf, uSector*128) == false)   
        {   
            printf("Error detected\n");   
            Dump32(uReadBuf, uSector*128);   
        }   
        else   
        {   
            printf("Write/Read in PIO DMA mode is OK\n");   
        }   
           
        rINTMSK |= BIT_CFCON; // mask cfcon    
           
        oAta.CloseMedia();   
    //  oIntc.Mask(BIT_CFCON);   
    }   
       
    void TestUDmaMode_Int(void)   
    {   
        U32 uWriteBuf, uReadBuf;   
        U32 deviceLBA;   
        U32 uSector;   
        U32 uDeviceMaxSectors;   
       
        U32 uCurrentCount;   
        U32 uRemainderCount;   
        U32 uCurrentLba;   
        U32 uCurrentAddr;   
        U32 uRound;   
       
        uWriteBuf = CODEC_MEM_ST;   
        uReadBuf = CODEC_MEM_ST + 0x400000;   
       
        oAta.OpenMedia(UDMA);   
        oAta.GetMaxSectors(uDeviceMaxSectors);   
       
        printf("\nInput device sector address[max: 0x%x]\n", uDeviceMaxSectors);   
        deviceLBA = (U32)GetIntNum();   
       
        printf("Input sector count[max: 0x%x]\n", uDeviceMaxSectors-deviceLBA);   
        uSector = (U32)GetIntNum();   
       
        for (U32 i=0; i<uSector*512; i++)   
        {   
            Outp8(uWriteBuf+i, (i+4)%256);   
            Outp8(uReadBuf+i, 0);   
        }   
       
        bIsDone = false;   
        bIsXferDone = false;   
       
    //  oIntc.Init();          
        oAta.ClearAllInterrupt();   
        ClearPending(BIT_CFCON);     
        rINTMSK &= (~BIT_CFCON); // unmask cfcon    
        pISR_CFCON = (unsigned)Isr_Ata;   
    //    oIntc.SetHandlerAndUnmask(BIT_CFCON, Isr_Ata);   
       
    //  printf("Get Ready\n");   
    //  getchar();   
           
        // Max transfer block count per command is 256.   
        uRemainderCount = uSector;   
        uRound = 0;    
           
        while(uRemainderCount != 0) {   
            if(uRemainderCount>256) {   
                uCurrentCount = 256; //0 means 256   
                uRemainderCount -= 256;   
            } else {   
                uCurrentCount = uRemainderCount;   
                uRemainderCount = 0;   
            }   
            uCurrentLba = deviceLBA + uRound*256;   
            uCurrentAddr = uWriteBuf + uRound*256*512;   
               
            oAta.StartWritingBlocks(uCurrentLba, uCurrentCount, uCurrentAddr);   
            while(bIsXferDone != true) ;   
            bIsXferDone = false;   
       
            uRound++;   
        }   
       
        printf("Write Done\n");   
        getchar();   
       
        uRemainderCount = uSector;   
        uRound = 0;    
           
        while(uRemainderCount != 0) {   
            if(uRemainderCount>256) {   
                uCurrentCount = 256; //0 means 256   
                uRemainderCount -= 256;   
            } else {   
                uCurrentCount = uRemainderCount;   
                uRemainderCount = 0;   
            }   
            uCurrentLba = deviceLBA + uRound*256;   
            uCurrentAddr = uReadBuf + uRound*256*512;   
       
            oAta.StartReadingBlocks(uCurrentLba, uCurrentCount, uCurrentAddr);   
            while(bIsXferDone != true) ;   
            bIsXferDone = false;       
       
            uRound++;   
        }   
           
        printf("Read Done\n");   
        getchar();   
           
        if (Compare32(uWriteBuf, uReadBuf, uSector*128) == false)   
        {   
            printf("Error detected\n");   
            Dump32(uReadBuf, uSector*128);   
        }   
        else   
        {   
            printf("Write/Read in UDMA mode is OK\n");   
        }   
           
        rINTMSK |= BIT_CFCON; // mask cfcon    
       
        oAta.CloseMedia();   
    //  oIntc.Mask(BIT_CFCON);   
    }   
       
    void TestMultiPioDmaModeWrite(void)   
    {   
        U32 k;   
        U32 uWriteBuf, uReadBuf;   
        U32 deviceLBA;   
        U32 uSector;   
        U32 uDeviceMaxSectors;   
        U32 uDeviceMaxMultiple;   
        U32 uQuotient, uRemainder;   
        U32 uCurrentMultiple;   
        U32 uCurrentLba, uCurrentBufAddr;   
       
        uWriteBuf = CODEC_MEM_ST;   
        uReadBuf = CODEC_MEM_ST + 0x400000;   
       
        oAta.OpenMedia(PIO_DMA);   
        oAta.GetMaxSectors(uDeviceMaxSectors);   
       
        printf("\nInput device sector address[max: 0x%x]\n",uDeviceMaxSectors);   
        deviceLBA = (U32)GetIntNum();   
       
        printf("Input sector count[max: 0x%x]\n",uDeviceMaxSectors-deviceLBA);   
        uSector = (U32)GetIntNum();   
       
        for (U32 i=0; i<uSector*512; i++)   
        {   
            Outp8(uWriteBuf+i, (i+6)%256);   
            Outp8(uReadBuf+i, 0);   
        }   
       
        oAta.SetAtaMode(PIO_DMA);   
       
        oAta.GetMaxMultiple(uDeviceMaxMultiple);   
        //Assert(uDeviceMaxMultiple > 1);  // not support for CF card   
    /*  
        if (uDeviceMaxMultiple > 6)   
            uDeviceMaxMultiple = 6;  // may be bug     -> hang  
    */   
        if (uDeviceMaxMultiple > 5)    
            uDeviceMaxMultiple = 5;  // may be bug.   
        uQuotient = uSector/uDeviceMaxMultiple;   
        uRemainder = uSector%uDeviceMaxMultiple;   
        uCurrentLba = deviceLBA;   
        uCurrentBufAddr = uWriteBuf;   
           
        do   
        {   
            uCurrentMultiple = (uQuotient > 0) ? uDeviceMaxMultiple : uRemainder;   
            if (uCurrentLba == deviceLBA || uQuotient == 0)   
            {   
                oAta.SetMultiple(uCurrentMultiple);   
            }   
           
            oAta.WriteSectors_MultiplePioDma(uCurrentLba, uCurrentBufAddr);   
            uCurrentLba += uCurrentMultiple;   
            uCurrentBufAddr += uCurrentMultiple*512;   
        } while (uQuotient--);   
           
        oAta.SetAtaMode(PIO_CPU);   
        oAta.ReadBlocks( deviceLBA, uSector, uReadBuf);   
           
        if (Compare32(uWriteBuf, uReadBuf, uSector*128) == false)   
        {   
            printf("Error detected\n");   
            Dump32(uReadBuf, uSector*128);   
        }   
        else   
        {   
            printf("Write/Read in Multiple PIO DMA mode is OK\n");   
        }      
       
        oAta.CloseMedia();   
    }   
       
    void TestMultiPioDmaModeRead(void)   
    {   
        U32 uReadBuf;   
        U32 deviceLBA;   
        U32 uSector;   
        U32 uDeviceMaxSectors;   
        U32 uDeviceMaxMultiple;   
        U32 uQuotient, uRemainder;   
        U32 uCurrentMultiple;   
        U32 uCurrentLba, uCurrentBufAddr;   
       
        uReadBuf = CODEC_MEM_ST + 0x400000;   
       
        oAta.OpenMedia(PIO_DMA);   
        oAta.GetMaxSectors(uDeviceMaxSectors);   
       
        printf("\nInput device sector address[max: 0x%x]\n",uDeviceMaxSectors);   
        deviceLBA = (U32)GetIntNum();   
       
        printf("Input sector count[max: 0x%x]\n",uDeviceMaxSectors-deviceLBA);   
        uSector = (U32)GetIntNum();   
       
        for (U32 i=0; i<uSector*512; i=i+4)   
        {   
            Outp32(uReadBuf+i, 0);   
        }   
       
        oAta.GetMaxMultiple(uDeviceMaxMultiple);   
        //Assert(uDeviceMaxMultiple > 1);  // not support for CF card   
        if (uDeviceMaxMultiple > 4)    
        {   
            uDeviceMaxMultiple = 4;  // may be bug   
        }   
        uQuotient = uSector/uDeviceMaxMultiple;   
        uRemainder = uSector%uDeviceMaxMultiple;   
        uCurrentLba = deviceLBA;   
        uCurrentBufAddr = uReadBuf;   
           
        do   
        {   
            uCurrentMultiple = (uQuotient > 0) ? uDeviceMaxMultiple : uRemainder;   
            if (uCurrentLba == deviceLBA || uQuotient == 0)   
            {   
                oAta.SetMultiple(uCurrentMultiple);   
            }   
           
            oAta.ReadSectors_MultiplePioDma(uCurrentLba, uCurrentBufAddr);   
            uCurrentLba += uCurrentMultiple;   
            uCurrentBufAddr += uCurrentMultiple*512;   
        } while (uQuotient--);   
           
        Dump32(uReadBuf, uSector*128);     
       
        oAta.CloseMedia();   
    }   
       
       
    void Test_ATA(void)   
    {   
        int i, sel;   
       
        const FUNC_MENU menu[]=   
        {   
            NULL,                           "Exit",   
            TestChangeModeToAta,            "Change mode to ATA",      
            TestReset,                      "Reset ATA device\n",   
               
            TestBasicWriteRead,             "Basic Write/Read test\n",   
               
            TestPioCpuMode,                 "PIO_CPU mode Write/Read test",   
            TestPioCpuModeRead,             "PIO_CPU mode Read test",          
            TestPioDmaMode,                 "PIO_DMA mode Write/Read test [Polling mode]",   
            TestPioDmaModeRead,             "PIO_DMA mode Read test [Polling mode]",   
            TestUdmaMode,                   "UDMA mode Write/Read test [Polling mode]",   
            TestUdmaModeRead,               "UDMA mode Read test [Polling mode]\n",   
       
            TestPioDmaMode_Int,             "PIO_DMA Write/Read test [Interrupt mode]",   
            TestUDmaMode_Int,               "UDMA Write/Read test [Interrupt mode]\n",   
    // Additional test   
            TestMultiPioDmaModeWrite,       "PIO DMA Multiple Write/Read test [MicroDrive only]",   
            TestMultiPioDmaModeRead,        "PIO DMA Multiple Read test [MicroDrive only]",   
       
            0,0   
        };   
       
        while(1)   
        {   
            printf("\n");   
            for (i=0; (int)(menu[i].desc)!=0; i++)   
                printf("%2d: %s\n", i, menu[i].desc);   
       
            printf("\nSelect the function to test : ");   
            sel = GetIntNum();   
            printf("\n");   
       
            if (sel == 0)   
                break;   
            else if (sel>0 && sel<(sizeof(menu)/8-1))   
                (menu[sel].func)();   
        }      
    }   
       


