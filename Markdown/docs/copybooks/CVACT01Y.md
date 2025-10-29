---
icon: material/file-code
---
# CVACT01Y

## Description
This copybook defines the data structure for an account entity with a record length of 300 bytes. It contains essential account information fields including account identification, status, financial limits, balances, and important dates. Key fields include the account ID, active status indicator, current balance, credit limits (both regular and cash), various date fields (open, expiration, and reissue dates), cycle credits and debits, ZIP code, and a group identifier. This structure is likely used across multiple programs in the CardDemoAWS application for consistent account data representation and processing.

## Copybook Code
```cobol
      *****************************************************************
      *    Data-structure for  account entity (RECLN 300)
      *****************************************************************
       01  ACCOUNT-RECORD.
           05  ACCT-ID                           PIC 9(11).
           05  ACCT-ACTIVE-STATUS                PIC X(01).
           05  ACCT-CURR-BAL                     PIC S9(10)V99.
           05  ACCT-CREDIT-LIMIT                 PIC S9(10)V99.
           05  ACCT-CASH-CREDIT-LIMIT            PIC S9(10)V99.
           05  ACCT-OPEN-DATE                    PIC X(10).
           05  ACCT-EXPIRAION-DATE               PIC X(10). 
           05  ACCT-REISSUE-DATE                 PIC X(10).
           05  ACCT-CURR-CYC-CREDIT              PIC S9(10)V99.
           05  ACCT-CURR-CYC-DEBIT               PIC S9(10)V99.
           05  ACCT-ADDR-ZIP                     PIC X(10).
           05  ACCT-GROUP-ID                     PIC X(10).
           05  FILLER                            PIC X(178).      
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:15:59 CDT
      *

```