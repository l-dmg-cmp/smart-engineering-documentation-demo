---
icon: material/code-tags
---
# CBACT03C

## Overview
This batch COBOL program (CBACT03C) reads and displays account cross-reference data from an indexed VSAM file. It sequentially processes records from the XREFFILE, displaying each record's content until the end of file is reached. The program includes comprehensive error handling for file operations (open, read, close) with detailed status code reporting and program termination via CEE3ABD in case of errors. It's part of the CardDemo application, which demonstrates AWS and partner technologies for mainframe migration and modernization use cases.

## Metadata
**Program ID**: `CBACT03C`

**Author**: `AWS`

## Referenced Copybooks
- [`CVACT03Y`](copybooks/CVACT03Y.md)

## Environment Division

### INPUT-OUTPUT SECTION
This section defines the file control parameters for the XREFFILE-FILE, which is an indexed VSAM file containing account cross-reference data. The file is configured for sequential access mode with FD-XREF-CARD-NUM as the record key. The program tracks file operation status through the XREFFILE-STATUS field, which is used for error handling during file operations.
<details><summary>Code</summary>
```cobol
FILE-CONTROL.                                                            
           SELECT XREFFILE-FILE ASSIGN TO   XREFFILE                            
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS SEQUENTIAL                                    
                  RECORD KEY   IS FD-XREF-CARD-NUM                              
                  FILE STATUS  IS XREFFILE-STATUS.                              
      *
```
</details>


## Data Division

### FILE SECTION
This section defines the file structure for the XREFFILE, which contains account cross-reference data. The record layout consists of two fields: a 16-character card number field (FD-XREF-CARD-NUM) and a 34-character data field (FD-XREF-DATA). This structure is used by the program to read and process account cross-reference information from the indexed VSAM file.
<details><summary>Code</summary>
```cobol
FD  XREFFILE-FILE.                                                       
       01  FD-XREFFILE-REC.                                                     
           05 FD-XREF-CARD-NUM                  PIC X(16).                      
           05 FD-XREF-DATA                      PIC X(34).
```
</details>


### WORKING-STORAGE SECTION
This Working Storage Section defines the data structures used for file status tracking and error handling in the CBACT03C program. It includes the CVACT03Y copybook, file status fields for the XREFFILE (XREFFILE-STATUS), general I/O status tracking variables, and binary-to-character conversion fields. The section also contains program control variables including APPL-RESULT with condition names for normal processing (APPL-AOK) and end-of-file conditions (APPL-EOF), an END-OF-FILE flag, an ABCODE field for abnormal termination, and a TIMING field. These structures support the program's primary function of reading and displaying account cross-reference data with robust error handling.
<details><summary>Code</summary>
```cobol
*****************************************************************         
       COPY CVACT03Y.                                                           
       01  XREFFILE-STATUS.                                                     
           05  XREFFILE-STAT1      PIC X.                                       
           05  XREFFILE-STAT2      PIC X.                                       
                                                                                
       01  IO-STATUS.                                                           
           05  IO-STAT1            PIC X.                                       
           05  IO-STAT2            PIC X.                                       
       01  TWO-BYTES-BINARY        PIC 9(4) BINARY.                             
       01  TWO-BYTES-ALPHA         REDEFINES TWO-BYTES-BINARY.                  
           05  TWO-BYTES-LEFT      PIC X.                                       
           05  TWO-BYTES-RIGHT     PIC X.                                       
       01  IO-STATUS-04.                                                        
           05  IO-STATUS-0401      PIC 9   VALUE 0.                             
           05  IO-STATUS-0403      PIC 999 VALUE 0.                             
                                                                                
       01  APPL-RESULT             PIC S9(9)   COMP.                            
           88  APPL-AOK            VALUE 0.                                     
           88  APPL-EOF            VALUE 16.                                    
                                                                                
       01  END-OF-FILE             PIC X(01)    VALUE 'N'.                      
       01  ABCODE                  PIC S9(9) BINARY.                            
       01  TIMING                  PIC S9(9) BINARY.                            
                                                                                
      *****************************************************************
```
</details>


## Procedure Division

### 1000-XREFFILE-GET-NEXT
This paragraph handles the sequential reading of records from the XREFFILE. It reads a record into CARD-XREF-RECORD and processes the result based on the file status. When a record is successfully read (status '00'), it sets APPL-RESULT to 0 and displays the record. For end-of-file condition (status '10'), it sets APPL-RESULT to 16. Any other status is treated as an error with APPL-RESULT set to 12. The paragraph then evaluates APPL-RESULT: if processing was successful, it continues; if end-of-file was reached, it sets END-OF-FILE flag to 'Y'; for any other error, it displays an error message, captures the file status, calls routines to display the status details and abnormally terminate the program.
<details><summary>Code</summary>
```cobol
READ XREFFILE-FILE INTO CARD-XREF-RECORD.                            
           IF  XREFFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
               DISPLAY CARD-XREF-RECORD                                         
           ELSE                                                                 
               IF  XREFFILE-STATUS = '10'                                       
                   MOVE 16 TO APPL-RESULT                                       
               ELSE                                                             
                   MOVE 12 TO APPL-RESULT                                       
               END-IF                                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               IF  APPL-EOF                                                     
                   MOVE 'Y' TO END-OF-FILE                                      
               ELSE                                                             
                   DISPLAY 'ERROR READING XREFFILE'                             
                   MOVE XREFFILE-STATUS TO IO-STATUS                            
                   PERFORM 9910-DISPLAY-IO-STATUS                               
                   PERFORM 9999-ABEND-PROGRAM                                   
               END-IF                                                           
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*
```
</details>


### 0000-XREFFILE-OPEN
This paragraph handles the opening of the XREFFILE-FILE for input processing. It initializes APPL-RESULT to 8, attempts to open the file, and then sets APPL-RESULT to 0 if successful or 12 if unsuccessful. If the operation fails (APPL-AOK is false), it displays an error message, captures the file status code, calls procedures to display the I/O status details (9910-DISPLAY-IO-STATUS) and then terminates the program abnormally (9999-ABEND-PROGRAM). This represents standard error handling for file opening operations in the CardDemo application.
<details><summary>Code</summary>
```cobol
MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT XREFFILE-FILE                                             
           IF  XREFFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING XREFFILE'                                 
               MOVE XREFFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*
```
</details>


### 9000-XREFFILE-CLOSE
This paragraph handles the closing of the XREFFILE-FILE and implements error handling for this operation. It initially sets APPL-RESULT to 8, then attempts to close the file. If the operation is successful (status code '00'), it resets APPL-RESULT to zero; otherwise, it sets APPL-RESULT to 12 to indicate an error. When an error occurs, the paragraph displays an error message, captures the file status code, calls a routine to display the I/O status details, and then terminates the program abnormally. The EXIT statement marks the end of the paragraph.
<details><summary>Code</summary>
```cobol
ADD 8 TO ZERO GIVING APPL-RESULT.                                    
           CLOSE XREFFILE-FILE                                                  
           IF  XREFFILE-STATUS = '00'                                           
               SUBTRACT APPL-RESULT FROM APPL-RESULT                            
           ELSE                                                                 
               ADD 12 TO ZERO GIVING APPL-RESULT                                
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING XREFFILE'                                 
               MOVE XREFFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.
```
</details>


### 9999-ABEND-PROGRAM
This paragraph handles abnormal program termination by displaying an "ABENDING PROGRAM" message, resetting the TIMING variable to zero, setting the abend code to 999, and then calling the Language Environment service 'CEE3ABD' to force an immediate program termination. This is part of the error handling mechanism that ensures the program terminates in a controlled manner when critical errors are encountered.
<details><summary>Code</summary>
```cobol
DISPLAY 'ABENDING PROGRAM'                                           
           MOVE 0 TO TIMING                                                     
           MOVE 999 TO ABCODE                                                   
           CALL 'CEE3ABD'.                                                      
                                                                                
      *****************************************************************
```
</details>


### 9910-DISPLAY-IO-STATUS
This paragraph formats and displays file status codes for error reporting. It handles two types of status codes: extended file status codes (when the first byte is '9' or non-numeric) and standard file status codes. For extended status codes, it preserves the first byte and converts the second byte to a binary value for proper display. For standard codes, it formats them with leading zeros. The formatted status code is then displayed with the prefix "FILE STATUS IS: NNNN" where NNNN is the actual status code. This provides consistent error reporting throughout the program, helping with debugging file operation issues.
<details><summary>Code</summary>
```cobol
IF  IO-STATUS NOT NUMERIC                                            
           OR  IO-STAT1 = '9'                                                   
               MOVE IO-STAT1 TO IO-STATUS-04(1:1)                               
               MOVE 0        TO TWO-BYTES-BINARY                                
               MOVE IO-STAT2 TO TWO-BYTES-RIGHT                                 
               MOVE TWO-BYTES-BINARY TO IO-STATUS-0403                          
               DISPLAY 'FILE STATUS IS: NNNN' IO-STATUS-04                      
           ELSE                                                                 
               MOVE '0000' TO IO-STATUS-04                                      
               MOVE IO-STATUS TO IO-STATUS-04(3:2)                              
               DISPLAY 'FILE STATUS IS: NNNN' IO-STATUS-04                      
           END-IF                                                               
           EXIT.                                                                
                                                                                
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:31 CDT
      *
```
</details>
