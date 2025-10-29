---
icon: material/file-cog
---
# REPTFILE

## Description
This JCL job defines a Generation Data Group (GDG) for storing transaction reports in the CardDemo application. The job creates a GDG named AWS.M2.CARDDEMO.TRANREPT with a limit of 10 generations, meaning it will maintain up to 10 historical versions of the report file. The GDG structure allows the application to manage multiple iterations of transaction reports while automatically handling version control and retention policies.

## Referenced Programs
- `IDCAMS`

## JCL Code
```jcl
//REPTFILE JOB 'DEF GDG FOR REPORT FILE',                                       
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
//* *******************************************************************         
//* DELETE TRANSACATION MASTER VSAM FILE IF ONE ALREADY EXISTS                  
//* *******************************************************************         
//STEP05 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//SYSIN    DD   *                                                               
   DEFINE GENERATIONDATAGROUP -                                                 
   (NAME(AWS.M2.CARDDEMO.TRANREPT) -                                            
    LIMIT(10) -                                                                 
   )                                                                            
/*                                                                              
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:07 CDT
//*

```