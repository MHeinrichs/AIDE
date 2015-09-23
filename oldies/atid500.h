/* TF = Task File base address + 3000hex */
#define TF (IDE_BASE_ADDRESS + 0x3000)
#define WORD_WIDE unsigned short int
#define BYTE_WIDE unsigned char
#define REG_INC 4
#define CS0 (-0x1000)
#define CS1 (-0x2000)
#define BYTESWAP 1
#ifdef BYTESWAP
#define Y 0
#endif
#ifndef BYTESWAP
#define Y 1
#endif


#define TF_DATA     (*(volatile WORD_WIDE *)(TF + 0x0 * REG_INC + CS0))
#define TF_DATA_8BIT (*(volatile BYTE_WIDE *)(Y+TF + 0x0 * REG_INC + CS0))
#define TF_ERROR    (*(volatile BYTE_WIDE *)(Y+ TF + 0x1 * REG_INC + CS0))
#define TF_FEATURES (*(volatile BYTE_WIDE *)(Y+ TF + 0x1 * REG_INC + CS0))
#define TF_SECTOR_COUNT  (*(BYTE_WIDE *)(Y+TF + 0x2 * REG_INC + CS0))
#define TF_SECTOR_NUMBER (*(BYTE_WIDE *)(Y+TF + 0x3 * REG_INC + CS0))
#define TF_CYLINDER_LOW  (*(BYTE_WIDE *)(Y+TF + 0x4 * REG_INC + CS0))
#define TF_CYLINDER_HIGH (*(BYTE_WIDE *)(Y+TF + 0x5 * REG_INC + CS0))
#define TF_DRIVE_HEAD    (*(BYTE_WIDE *)(Y+TF + 0x6 * REG_INC + CS0))
#define TF_STATUS  (*(volatile BYTE_WIDE *)(Y+TF + 0x7 * REG_INC + CS0))
#define TF_COMMAND (*(volatile BYTE_WIDE *)(Y+TF + 0x7 * REG_INC + CS0))
#define TF_ALTERNATE_STATUS\
                   (*(volatile BYTE_WIDE *)(Y+TF + 0x6 * REG_INC + CS1))
#define TF_DEVICE_CONTROL TF_ALTERNATE_STATUS
#define TF_DRIVE_ADDRESS\
           (*(volatile BYTE_WIDE *)(TF_BASE+ 0x7 * REG_INC + CS1))

