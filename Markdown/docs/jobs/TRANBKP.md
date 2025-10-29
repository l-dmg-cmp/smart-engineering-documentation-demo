---
icon: material/file-cog
---
# TRANBKP

## Description
This JCL job performs a backup and reset operation for the transaction master file in the CardDemo application. It first backs up the current transaction data from the VSAM KSDS file to a sequential backup dataset with a generation number, then deletes and redefines the transaction master VSAM file with its associated alternate index. The job consists of three main steps: STEP05R uses a custom REPROC procedure to copy the transaction data to a backup file, STEP05 deletes the existing VSAM cluster and alternate index, and STEP10 redefines the empty transaction master VSAM file with specific allocation parameters. This job is likely run periodically to archive processed transactions and reset the transaction master file for new processing cycles.

## Referenced Programs
- `IDCAMS`

## JCL Code
```jcl
//TRANBKP JOB 'REPRO and Delete Transaction Master',CLASS=A,MSGCLASS=0,         
//  NOTIFY=&SYSUID                    
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
//* *******************************************************************        
//* Repro the processed transaction file                                       
//* *******************************************************************         
//STEP05R EXEC PROC=REPROC,                                                     
// CNTLLIB=AWS.M2.CARDDEMO.CNTL                                                 
//*                                                                             
//PRC001.FILEIN  DD DISP=SHR,                                                   
//        DSN=AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS                                
//*                                                                             
//PRC001.FILEOUT DD DISP=(NEW,CATLG,DELETE),                                    
//        UNIT=SYSDA,                                                           
//        DCB=(LRECL=350,RECFM=FB,BLKSIZE=0),                                   
//        SPACE=(CYL,(1,1),RLSE),                                               
//        DSN=AWS.M2.CARDDEMO.TRANSACT.BKUP(+1)                                 
//* *******************************************************************         
//* DELETE TRANSACATION MASTER VSAM FILE IF ONE ALREADY EXISTS                  
//* *******************************************************************         
//STEP05 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//SYSIN    DD   *                                                               
   DELETE AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS -                                  
          CLUSTER                                                               
   IF MAXCC LE 08 THEN SET MAXCC = 0                                            
   DELETE AWS.M2.CARDDEMO.TRANSACT.VSAM.AIX -                                   
          ALTERNATEINDEX                                                        
   IF MAXCC LE 08 THEN SET MAXCC = 0                                            
/*                                                                              
//*                                                                             
//* *******************************************************************         
//* DEFINE TRANSACATION MASTER VSAM FILE                                        
//* *******************************************************************         
//STEP10 EXEC PGM=IDCAMS,COND=(4,LT)                                            
//SYSPRINT DD   SYSOUT=*                                                        
//SYSIN    DD   *                                                               
   DEFINE CLUSTER (NAME(AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS) -                   
          CYLINDERS(1 5) -                                                      
          VOLUMES(AWSHJ1 -                                                      
          ) -                                                                   
          KEYS(16 0) -                                                          
          RECORDSIZE(350 350) -                                                 
          SHAREOPTIONS(2 3) -                                 
          ERASE -                                                               
          INDEXED -                                                             
          ) -                                                                   
          DATA (NAME(AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS.DATA) -                 
          ) -                                                                   
          INDEX (NAME(AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS.INDEX) -               
          )                                                                     
/*                                                                              
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:08 CDT
//*

```