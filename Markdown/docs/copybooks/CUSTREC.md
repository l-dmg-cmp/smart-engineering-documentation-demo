---
icon: material/file-code
---
# CUSTREC

## Description
This copybook defines the data structure for a Customer entity with a record length of 500 bytes. It contains comprehensive customer information including personal identification (customer ID, SSN, government ID), contact details (name, address, phone numbers), financial information (EFT account ID, FICO credit score), and status indicators (primary cardholder indicator). The structure is designed to store complete customer profiles for the CardDemo application, which is used to showcase AWS and partner technologies for mainframe migration and modernization scenarios. The fields are organized logically to represent customer demographic and account relationship data needed for credit card processing operations.

## Copybook Code
```cobol
      *****************************************************************
      *    Data-structure for Customer entity (RECLN 500)
      *****************************************************************
       01  CUSTOMER-RECORD.
           05  CUST-ID                                 PIC 9(09).
		     05  CUST-FIRST-NAME                         PIC X(25).
		     05  CUST-MIDDLE-NAME                        PIC X(25).
		     05  CUST-LAST-NAME                          PIC X(25).
		     05  CUST-ADDR-LINE-1                        PIC X(50).
		     05  CUST-ADDR-LINE-2                        PIC X(50).
		     05  CUST-ADDR-LINE-3                        PIC X(50).		   
		     05  CUST-ADDR-STATE-CD                      PIC X(02).
		     05  CUST-ADDR-COUNTRY-CD                    PIC X(03).
		     05  CUST-ADDR-ZIP                           PIC X(10).
		     05  CUST-PHONE-NUM-1                        PIC X(15).
		     05  CUST-PHONE-NUM-2                        PIC X(15).
		     05  CUST-SSN                                PIC 9(09).
		     05  CUST-GOVT-ISSUED-ID                     PIC X(20).
		     05  CUST-DOB-YYYYMMDD                       PIC X(10).
		     05  CUST-EFT-ACCOUNT-ID                     PIC X(10).
		     05  CUST-PRI-CARD-HOLDER-IND                PIC X(01).
		     05  CUST-FICO-CREDIT-SCORE                  PIC 9(03).
             05  FILLER                                  PIC X(168).      
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:15:59 CDT
      *

```