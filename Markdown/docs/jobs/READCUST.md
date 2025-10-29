---
icon: material/file-cog
---
# READCUST

## Description
This JCL job executes program CBCUS01C which reads data from the customer master VSAM file. The job is part of the CardDemo application used for AWS mainframe migration demonstrations. It allocates the necessary datasets including the customer data KSDS file and standard output destinations. The job serves as a utility to access and potentially report on customer information stored in the VSAM file, supporting the application's data access capabilities.

## Referenced Programs
- [`CBCUS01C`](/CBCUS01C.md)

## JCL Code
```jcl
//READCUST JOB 'Read Customer Data file',CLASS=A,MSGCLASS=0,
// NOTIFY=&SYUID
//* *******************************************************************
//* RUN THE PROGRAM THAT READS THE CUSTOMER MASTER VSAM FILE
//* *******************************************************************
//STEP05 EXEC PGM=CBCUS01C
//STEPLIB  DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.LOADLIB
//CUSTFILE DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:07 CDT
//*

```