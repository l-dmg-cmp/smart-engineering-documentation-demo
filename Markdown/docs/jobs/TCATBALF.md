---
icon: material/file-cog
---
# TCATBALF

## Description
This JCL job creates and populates the Transaction Category Balance VSAM file for the CardDemo application. The job executes in three steps: first deleting any existing VSAM file with the same name (STEP05), then defining a new VSAM Key Sequenced Data Set with 17-byte keys and 50-byte fixed-length records (STEP10), and finally copying data from a flat file source into the newly created VSAM file (STEP15). The VSAM file is configured with primary allocation of 1 cylinder and secondary allocation of 5 cylinders on volume AWSHJ1, with appropriate sharing options for concurrent access.

## Referenced Programs
- `IDCAMS`

## JCL Code
```jcl
//TCATBALF JOB 'DEFINE TRANCAT BAL',CLASS=A,MSGCLASS=0,                         
// NOTIFY=&SYSUID          
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
//* DELETE TRANSACTION CATEGORY BALANCE VSAM FILE IF ONE ALREADY EXISTS         
//* *******************************************************************         
//STEP05 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//SYSIN    DD   *                                                               
   DELETE AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS -                                  
          CLUSTER                                                               
   SET    MAXCC = 0                                                             
/*                                                                              
//*                                                                             
//* *******************************************************************         
//* DEFINE TRANSACTION CATEGORY BALANCE VSAM FILE                               
//* *******************************************************************         
//STEP10 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//SYSIN    DD   *                                                               
   DEFINE CLUSTER (NAME(AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS) -                   
          CYLINDERS(1 5) -                                                      
          VOLUMES(AWSHJ1 -                                                      
          ) -                                                                   
          KEYS(17 0) -                                                          
          RECORDSIZE(50 50) -                                                   
          SHAREOPTIONS(2 3) -                                                   
          ERASE -                                                               
          INDEXED -                                                             
          ) -                                                                   
          DATA (NAME(AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS.DATA) -                 
          ) -                                                                   
          INDEX (NAME(AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS.INDEX) -               
          )                                                                     
/*                                                                              
//* *******************************************************************         
//* COPY DATA FROM FLAT FILE TO VSAM FILE                                       
//* *******************************************************************         
//STEP15 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//TCATBAL DD DISP=SHR,                                                          
//         DSN=AWS.M2.CARDDEMO.TCATBALF.PS                                      
//TCATBALV DD DISP=OLD,                                                         
//         DSN=AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS                               
//SYSIN    DD   *                                                               
   REPRO INFILE(TCATBAL) OUTFILE(TCATBALV)                                      
/*                                                                              
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:07 CDT
//*

```