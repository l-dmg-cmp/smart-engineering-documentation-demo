---
icon: material/file-code
---
# CVACT02Y

## Description
This copybook defines the data structure for a card entity with a record length of 150 bytes. It contains essential credit/debit card information fields including the card number, associated account ID, CVV code, embossed name, expiration date, and active status indicator. The structure allows for standardized card data representation across the CardDemo application, which is used to showcase AWS and partner technologies for mainframe migration scenarios. The remaining 59 bytes are reserved as filler space for potential future expansion of the card record structure.

## Copybook Code
```cobol
      *****************************************************************
      *    Data-structure for card entity (RECLN 150)
      *****************************************************************
       01  CARD-RECORD.
           05  CARD-NUM                          PIC X(16).
           05  CARD-ACCT-ID                      PIC 9(11).
           05  CARD-CVV-CD                       PIC 9(03).
           05  CARD-EMBOSSED-NAME                PIC X(50).
           05  CARD-EXPIRAION-DATE               PIC X(10).
           05  CARD-ACTIVE-STATUS                PIC X(01).
           05  FILLER                            PIC X(59).
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:00 CDT
      *

```