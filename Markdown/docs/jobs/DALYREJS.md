---
icon: material/file-cog
---
# DALYREJS

## Description
This JCL job defines a Generation Data Group (GDG) for daily rejection records in the CardDemo application. It creates a GDG named AWS.M2.CARDDEMO.DALYREJS with a limit of 5 generations, meaning the system will maintain up to 5 historical versions of the dataset. The SCRATCH parameter indicates that expired generations will be automatically deleted. This GDG is likely used to store daily transaction rejection records that need to be maintained for a limited historical period for analysis or reporting purposes.

## Referenced Programs
- `IDCAMS`

## JCL Code
```jcl
//DALYREJS JOB 'DEF GDG FOR REJS',CLASS=A,MSGCLASS=0,NOTIFY=&SYSUID     
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
   (NAME(AWS.M2.CARDDEMO.DALYREJS) -                                            
    LIMIT(5) -                                                                  
    SCRATCH -                                                                   
   )                                                                            
/*                                                                              
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:05 CDT
//*

```