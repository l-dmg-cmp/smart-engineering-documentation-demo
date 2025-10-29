---
icon: material/file-cog
---
# DEFGDGB

## Description
This job defines Generation Data Group (GDG) bases required by the CardDemo application. It executes the IDCAMS utility to create six GDG bases with a limit of 5 generations each and the SCRATCH option enabled (which automatically removes entries when they exceed the limit). The defined GDGs are used for various data management purposes within the CardDemo system, including transaction backups, daily transactions, transaction reports, account balance backups, system transactions, and combined transaction data. Error handling is implemented to continue processing even if a GDG already exists (by resetting the condition code when LASTCC=12).

## Referenced Programs
- `IDCAMS`

## JCL Code
```jcl
//DEFGDGB JOB 'DEF GDG BASES',CLASS=A,MSGCLASS=0,NOTIFY=&SYSUID      
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
//*  DEFINE GDG BASES NEEDED BY CARDDEMO PROJECT                                
//* *******************************************************************         
//STEP05 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//SYSIN    DD   *                                                               
   DEFINE GENERATIONDATAGROUP -                                                 
   (NAME(AWS.M2.CARDDEMO.TRANSACT.BKUP) -                                       
    LIMIT(5) -                                                                  
    SCRATCH -                                                                   
   )                                                                            
   IF LASTCC=12 THEN SET MAXCC=0                                                
   DEFINE GENERATIONDATAGROUP -                                                 
   (NAME(AWS.M2.CARDDEMO.TRANSACT.DALY) -                                       
    LIMIT(5) -                                                                  
    SCRATCH -                                                                   
   )                                                                            
   IF LASTCC=12 THEN SET MAXCC=0                                                
   DEFINE GENERATIONDATAGROUP -                                                 
   (NAME(AWS.M2.CARDDEMO.TRANREPT) -                                            
    LIMIT(5) -                                                                  
    SCRATCH -                                                                   
   )                                                                            
   IF LASTCC=12 THEN SET MAXCC=0                                                
   DEFINE GENERATIONDATAGROUP -                                                 
   (NAME(AWS.M2.CARDDEMO.TCATBALF.BKUP) -                                       
    LIMIT(5) -                                                                  
    SCRATCH -                                                                   
   )                                                                            
   IF LASTCC=12 THEN SET MAXCC=0                                                
   DEFINE GENERATIONDATAGROUP -                                                 
   (NAME(AWS.M2.CARDDEMO.SYSTRAN) -                                             
    LIMIT(5) -                                                                  
    SCRATCH -                                                                   
   )                                                                            
   IF LASTCC=12 THEN SET MAXCC=0                                                
   DEFINE GENERATIONDATAGROUP -                                                 
   (NAME(AWS.M2.CARDDEMO.TRANSACT.COMBINED) -                                   
    LIMIT(5) -                                                                  
    SCRATCH -                                                                   
   )                                                                            
   IF LASTCC=12 THEN SET MAXCC=0                                                
/*                                                                              
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:05 CDT
//*

```