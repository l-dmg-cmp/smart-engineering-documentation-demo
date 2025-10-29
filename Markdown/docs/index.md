# Project Documentation
## Project Description
CardDemo is a comprehensive mainframe application designed to showcase AWS and partner technologies for mainframe migration and modernization use-cases.

## Summary
CardDemo simulates a credit card processing system with complete functionality for account management, transaction processing, user administration, and reporting. The application demonstrates various mainframe technologies and patterns to serve as a test bed for migration strategies, modernization approaches, service enablement, and performance testing. It includes both online (CICS) and batch components that work together to provide a complete business solution.

## Technologies
- COBOL
- CICS (Customer Information Control System)
- JCL (Job Control Language)
- VSAM (Virtual Storage Access Method)
- IBM Language Environment
- Batch processing
- Transient Data Queues (TDQ)

## Dependencies
- VSAM Files:
  - CUSTFILE (Customer information)
  - ACCTFILE/ACCTDAT (Account data)
  - CARDFILE/CARDDAT (Credit card information)
  - TRANSACT (Transaction records)
  - CXACAIX (Account cross-reference)
  - USRSEC (User security information)
  - XREFFILE (Card-to-account cross-reference)
  - TRANTYPE (Transaction type definitions)
  - TRANCATG (Transaction category definitions)
  - DISCGRP (Disclosure group/interest rate information)
- Batch Jobs:
  - POSTTRAN (Transaction posting)
  - INTCALC (Interest calculation)
  - TRANREPT (Transaction reporting)
  - COMBTRAN (Transaction processing)
- Utilities:
  - CSUTLDTC (Date validation utility)

## Starting Points
- Online Entry Points:
  - COSGN00C: Sign-on screen and authentication
  - COMEN01C: Main menu for regular users
  - COADM01C: Administrative menu
- Key Batch Processes:
  - CBTRN01C: Daily transaction validation
  - CBTRN02C: Transaction posting to accounts
  - CBACT04C: Interest calculation
  - CBTRN03C: Transaction reporting
- Core Business Functions:
  - COTRN00C/COTRN01C: Transaction viewing
  - COTRN02C: Transaction entry
  - COBIL00C: Bill payment
  - COACTVWC: Account viewing
  - COCRDLIC: Credit card listing

## Architecture
CardDemo follows a traditional mainframe architecture with separation between online and batch components. The online system is built on CICS with screen-based navigation between related functions, while batch processes handle periodic tasks like transaction posting and interest calculation.

The application uses a modular design with clear separation of concerns:
- User interface components (CO* programs) handle screen interactions
- Batch programs (CB* programs) process data in bulk
- Utility components (CS* programs) provide shared functionality
- Data is stored in VSAM files with indexed access for online lookups

The system maintains data integrity through proper file handling, transaction management, and validation routines. It demonstrates typical mainframe patterns including master file updates, transaction processing, reporting, and administrative functions. The application uses a communication area (commarea) to maintain context between screens and implements role-based access control for administrative functions.