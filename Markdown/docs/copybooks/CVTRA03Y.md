---
icon: material/file-code
---
# CVTRA03Y

## Description
This copybook defines a data structure for transaction type records with a fixed length of 60 bytes. It contains fields for storing a 2-character transaction type code, a 50-character description of the transaction type, and an 8-character filler field. This structure is likely used throughout the CardDemo application to standardize the representation of transaction types and their descriptions in various programs that process card transactions.

## Copybook Code
```cobol
      *****************************************************************         
      *    Data-structure for transaction type (RECLN = 60)                     
      *****************************************************************         
       01  TRAN-TYPE-RECORD.                                                    
           05  TRAN-TYPE                               PIC X(02).               
           05  TRAN-TYPE-DESC                          PIC X(50).               
           05  FILLER                                  PIC X(08).               
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:00 CDT
      *

```