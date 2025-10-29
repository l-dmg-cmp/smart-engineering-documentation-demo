---
icon: material/file-code
---
# CVTRA05Y

## Description
This copybook defines the data structure for transaction records with a record length of 350 bytes. It contains fields for storing transaction details including identification, type, amount, merchant information, and timestamps. Key fields include transaction ID, type code, category code, amount, merchant details (ID, name, city, ZIP), card number, and timestamps for both transaction origin and processing. This structure is likely used throughout the CardDemo application for processing and storing credit card transaction data in various programs that handle transaction processing, reporting, or analysis.

## Copybook Code
```cobol
      *****************************************************************         
      *    Data-structure for TRANsaction record (RECLN = 350)                  
      *****************************************************************         
       01  TRAN-RECORD.                                                         
           05  TRAN-ID                                 PIC X(16).               
           05  TRAN-TYPE-CD                            PIC X(02).               
           05  TRAN-CAT-CD                             PIC 9(04).               
           05  TRAN-SOURCE                             PIC X(10).               
           05  TRAN-DESC                               PIC X(100).              
           05  TRAN-AMT                                PIC S9(09)V99.           
           05  TRAN-MERCHANT-ID                        PIC 9(09).               
           05  TRAN-MERCHANT-NAME                      PIC X(50).               
           05  TRAN-MERCHANT-CITY                      PIC X(50).               
           05  TRAN-MERCHANT-ZIP                       PIC X(10).               
           05  TRAN-CARD-NUM                           PIC X(16).               
           05  TRAN-ORIG-TS                            PIC X(26).               
           05  TRAN-PROC-TS                            PIC X(26).               
           05  FILLER                                  PIC X(20).               
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:01 CDT
      *

```