---
icon: material/file-cog
---
# BMSCMP

## Description
This JCL job compiles a BMS (Basic Mapping Support) map for CICS applications within the CardDemo AWS mainframe migration showcase project. It performs a two-step process: first compiling the BMS map to generate the necessary copybook and load modules, then issuing a CICS NEWCOPY command to make the updated map immediately available in the running CICS region. The job uses a custom procedure (BUILDBMS) stored in the project's procedure library and includes parameters for customization, allowing users to specify their high-level qualifier and map name. The job is designed to be reusable by replacing "CICSMAP" with the actual map name needed for compilation.

## Referenced Programs
- `SDSF`

## JCL Code
```jcl
//CBLDBMS  JOB 'Compile BMS Map',CLASS=A,MSGCLASS=H,        
//             MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID,TIME=1440
//*********************************************************************
//*  Change CICSMAP to your map name everywhere
//*----->   C CICSMAP xyz all <--------
//*  set    HLQ      to your high level qualifier
//*********************************************************************
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
//*********************************************************************    
//****  Sample Assembler BMS Compile JCL                         ******         
//****  Check with your Administrator for                        ******         
//****  JCL suitable to your environment                         ******
//*********************************************************************
//****  Compile CICS BMS to generate Copybook                    ******
//*********************************************************************
//*  ---------------------------
//*  Set Parms for this compile:
//*  ---------------------------
//   SET HLQ=AWS.M2
//*
//*********************************************************************
//*  Add Proclib Reference
//*********************************************************************
//CCLIBS  JCLLIB ORDER=&HLQ..CARDDEMO.PRC.UTIL
//STEP1 EXEC BUILDBMS,MAPNAME=CICSMAP,HLQ=&HLQ
//*********************************************************************
//****  CICS commands in batch to Execute NEWCOPY                ******
//*********************************************************************
//SDSF1 EXEC PGM=SDSF
//ISFOUT DD SYSOUT=*
//CMDOUT DD SYSOUT=*
//ISFIN  DD *
 /MODIFY CICSAWSA,'CEMT SET PROG(CICSMAP) NEWCOPY'
/*

```