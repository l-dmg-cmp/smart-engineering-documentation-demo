---
icon: material/file-cog
---
# PRTCATBL

## Description
This JCL job extracts and formats transaction category balance data from the CardDemo application. It first deletes any existing report file, then executes a processing procedure to unload data from a VSAM KSDS file to a backup dataset. The job then uses SORT to filter and organize the transaction category balances by account ID, transaction type code, and category code. The output is formatted with appropriate decimal editing for balance amounts and written to a report file. This job is part of the CardDemo application's reporting functionality, providing visibility into transaction category balances across accounts.

## Referenced Programs
- `IEFBR14`
- `SORT`

## JCL Code
```jcl
//PRTCATBL JOB 'Print Trasaction Category Balance File',                        
// CLASS=A,MSGCLASS=0,NOTIFY=&SYSUID           
//******************************************************************
//* Copyright Amazon.com, Inc. or its affiliates.                   
//* All Rights Reserved.                                            
//*                                                                 
//* Licensed under the Apache License, Version 2.0 (the "License"). 
//* You may not use this file except in compliance with the License.
//* You may obtain a copy of the License at                         
//*                                                                 
//*    http://www.apache.org/licenses/LICENSE-2.0                   
//*                                                                 
//* Unless required by applicable law or agreed to in writing,      
//* software distributed under the License is distributed on an     
//* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    
//* either express or implied. See the License for the specific     
//* language governing permissions and limitations under the License
//******************************************************************            
//JOBLIB JCLLIB ORDER=('AWS.M2.CARDDEMO.PROC')                                  
//*
//DELDEF   EXEC PGM=IEFBR14
//THEFILE  DD DISP=(MOD,DELETE),
//         UNIT=SYSDA,
//         SPACE=(TRK,(1,1),RLSE),
//         DSN=AWS.M2.CARDDEMO.TCATBALF.REPT
//* ********************************************************`***********        
//* Unload the processed transaction category balance file                      
//* *******************************************************************         
//STEP05R EXEC PROC=REPROC,                                                     
// CNTLLIB=AWS.M2.CARDDEMO.CNTL                                                 
//*                                                                             
//PRC001.FILEIN  DD DISP=SHR,                                                   
//        DSN=AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS                                
//*                                                                             
//PRC001.FILEOUT DD DISP=(NEW,CATLG,DELETE),                                    
//        UNIT=SYSDA,                                                           
//        DCB=(LRECL=50,RECFM=FB,BLKSIZE=0),                                   
//        SPACE=(CYL,(1,1),RLSE),                                               
//        DSN=AWS.M2.CARDDEMO.TCATBALF.BKUP(+1)                                 
//* *******************************************************************         
//* Filter the TCATBALFions for a the parm date and sort by card num            
//* *******************************************************************         
//STEP10R  EXEC PGM=SORT                                                        
//SORTIN   DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.TCATBALF.BKUP(+1)                                
//SYMNAMES DD *                                                                 
TRANCAT-ACCT-ID,1,11,ZD                                                         
TRANCAT-TYPE-CD,12,2,CH                                                         
TRANCAT-CD,14,4,ZD
TRAN-CAT-BAL,18,11,ZD
//SYSIN    DD *                                                                 
 SORT FIELDS=(TRANCAT-ACCT-ID,A,TRANCAT-TYPE-CD,A,TRANCAT-CD,A)                 
 OUTREC FIELDS=(TRANCAT-ACCT-ID,X,
     TRANCAT-TYPE-CD,X,
     TRANCAT-CD,X,
     TRAN-CAT-BAL,EDIT=(TTTTTTTTT.TT),9X)
/*                                                                              
//SYSOUT   DD SYSOUT=*                                                          
//SORTOUT  DD DISP=(NEW,CATLG,DELETE),                                          
//         UNIT=SYSDA,                                                          
//         DCB=(LRECL=40,RECFM=FB,BLKSIZE=0),                                   
//         SPACE=(CYL,(1,1),RLSE),                                              
//         DSN=AWS.M2.CARDDEMO.TCATBALF.REPT                              
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:06 CDT
//*

```