---
icon: material/file-code
---
# CVTRA01Y

## Description
This copybook defines a data structure for tracking transaction category balances. It contains a record layout with a key composed of an account ID, transaction type code, and category code, followed by a signed balance field with two decimal places. The record has a fixed length of 50 bytes, with 22 bytes of filler space reserved for potential future expansion. This structure is likely used in the CardDemo application to maintain and report on transaction balances categorized by different transaction types.

## Copybook Code
```cobol
      *****************************************************************         
      *    Data-structure for transaction category balance (RECLN = 50)         
      *****************************************************************         
       01  TRAN-CAT-BAL-RECORD.                                                 
           05  TRAN-CAT-KEY.                                                    
              10 TRANCAT-ACCT-ID                       PIC 9(11).               
              10 TRANCAT-TYPE-CD                       PIC X(02).               
              10 TRANCAT-CD                            PIC 9(04).               
           05  TRAN-CAT-BAL                            PIC S9(09)V99.           
           05  FILLER                                  PIC X(22).               
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:00 CDT
      *

```