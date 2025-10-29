---
icon: material/file-code
---
# CVTRA04Y

## Description
This copybook defines a data structure for transaction category types with a record length of 60 bytes. It contains a key composed of a 2-character transaction type code and a 4-digit transaction category code, followed by a 50-character description field. The structure is likely used throughout the CardDemo application to categorize different types of card transactions, providing standardized transaction classification for reporting, processing, or display purposes.

## Copybook Code
```cobol
      *****************************************************************         
      *    Data-structure for transaction category type (RECLN = 60)            
      *****************************************************************         
       01  TRAN-CAT-RECORD.                                                     
           05  TRAN-CAT-KEY.                                                    
              10  TRAN-TYPE-CD                         PIC X(02).               
              10  TRAN-CAT-CD                          PIC 9(04).               
           05  TRAN-CAT-TYPE-DESC                      PIC X(50).               
           05  FILLER                                  PIC X(04).               
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:01 CDT
      *

```