---
icon: material/file-cog
---
# COMBTRAN

## Description
This JCL job combines transaction records from two sources and loads them into a transaction master file. It first merges and sorts transactions from the regular transaction backup file and system-generated transactions, sorting them by transaction ID in ascending order. The sorted output is stored in a new generation of the combined transaction dataset. In the second step, the job uses IDCAMS REPRO to load these combined transactions into a VSAM KSDS file that serves as the transaction master. This job is part of the CardDemo application's transaction processing workflow, ensuring that both user-initiated and system-generated transactions are properly consolidated and made available for further processing.

## Referenced Programs
- `SORT`
- `IDCAMS`

## JCL Code
```jcl
//COMBTRAN JOB 'COMBINE TRANSACTIONS',CLASS=A,MSGCLASS=0,                  
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
//* *******************************************************************         
//* Sort current transaction file and system generated transactions
//* *******************************************************************         
//STEP05R  EXEC PGM=SORT                                                        
//SORTIN   DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.TRANSACT.BKUP(0)                                
//         DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.SYSTRAN(0)                                
//SYMNAMES DD *                                                                 
TRAN-ID,1,16,CH                                                         
//SYSIN    DD *                                                                 
 SORT FIELDS=(TRAN-ID,A)                                                  
/*                                                                              
//SYSOUT   DD SYSOUT=*                                                          
//SORTOUT  DD DISP=(NEW,CATLG,DELETE),                                          
//         UNIT=SYSDA,                                                          
//         DCB=(*.SORTIN),                                                      
//         SPACE=(CYL,(1,1),RLSE),                                              
//         DSN=AWS.M2.CARDDEMO.TRANSACT.COMBINED(+1)                            
//* *******************************************************************         
//* Load combined file to transaction master
//* *******************************************************************         
//STEP10 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//TRANSACT DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.TRANSACT.COMBINED(+1)                            
//TRANVSAM DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS                               
//SYSIN    DD   *                                                               
   REPRO INFILE(TRANSACT) OUTFILE(TRANVSAM)                                     
/*                                                                              
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:05 CDT
//*

```