---
icon: material/file-code
---
# CVTRA02Y

## Description
This copybook defines a data structure for a disclosure group record with a fixed length of 50 bytes. It contains fields for account group identification, transaction type and category codes, and an interest rate. The structure is likely used in the CardDemo application for processing or storing transaction-related disclosure information, possibly for regulatory compliance or customer notification purposes. The key fields form a composite identifier for different types of disclosures, while the interest rate field allows for signed values with four digits before and two digits after the decimal point.

## Copybook Code
```cobol
      *****************************************************************         
      *    Data-structure for disclosure group (RECLN = 50)                     
      *****************************************************************         
       01  DIS-GROUP-RECORD.                                                    
           05  DIS-GROUP-KEY.                                                   
              10 DIS-ACCT-GROUP-ID                     PIC X(10).               
              10 DIS-TRAN-TYPE-CD                      PIC X(02).               
              10 DIS-TRAN-CAT-CD                       PIC 9(04).               
           05  DIS-INT-RATE                            PIC S9(04)V99.           
           05  FILLER                                  PIC X(28).               
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:00 CDT
      *

```