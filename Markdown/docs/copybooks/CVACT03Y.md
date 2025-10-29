---
icon: material/file-code
---
# CVACT03Y

## Description
This copybook defines the data structure for a card cross-reference record with a record length of 50 bytes. It establishes the relationship between a payment card, customer, and account within the CardDemo application. The structure contains three key fields: a 16-character card number, a 9-digit customer ID, and an 11-digit account ID, followed by 14 bytes of unused filler space. This cross-reference structure enables the application to link card information with the appropriate customer and account records.

## Copybook Code
```cobol
      *****************************************************************         
      *    Data-structure for card xref (RECLN 50)                              
      *****************************************************************         
       01 CARD-XREF-RECORD.                                                     
           05  XREF-CARD-NUM                     PIC X(16).                     
           05  XREF-CUST-ID                      PIC 9(09).                     
           05  XREF-ACCT-ID                      PIC 9(11).                     
           05  FILLER                            PIC X(14).                     
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:00 CDT
      *

```