---
icon: material/file-code
---
# CSDAT01Y

## Description
This copybook defines a comprehensive date and time data structure used throughout the CardDemo application. It provides multiple formats for representing date and time information, including numeric components, formatted display strings, and timestamp formats. The structure includes fields for year, month, day, hours, minutes, seconds, and milliseconds, with both individual components and combined representations. The copybook also includes redefinitions to allow accessing date and time values as numeric fields. This standardized date-time structure enables consistent handling of temporal data across the CardDemo application, supporting various formatting needs for display, calculations, and database operations.

## Copybook Code
```cobol
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.                   
      * All Rights Reserved.                                            
      *                                                                 
      * Licensed under the Apache License, Version 2.0 (the "License"). 
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at                         
      *                                                                 
      *    http://www.apache.org/licenses/LICENSE-2.0                   
      *                                                                 
      * Unless required by applicable law or agreed to in writing,      
      * software distributed under the License is distributed on an     
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    
      * either express or implied. See the License for the specific     
      * language governing permissions and limitations under the License
      ****************************************************************** 
       01 WS-DATE-TIME.
         05 WS-CURDATE-DATA.
           10  WS-CURDATE.
             15  WS-CURDATE-YEAR         PIC 9(04).
             15  WS-CURDATE-MONTH        PIC 9(02).
             15  WS-CURDATE-DAY          PIC 9(02).
           10 WS-CURDATE-N REDEFINES WS-CURDATE PIC 9(08).
           10  WS-CURTIME.
             15  WS-CURTIME-HOURS        PIC 9(02).
             15  WS-CURTIME-MINUTE       PIC 9(02).
             15  WS-CURTIME-SECOND       PIC 9(02).
             15  WS-CURTIME-MILSEC       PIC 9(02).
           10 WS-CURTIME-N REDEFINES WS-CURTIME PIC 9(08).
         05 WS-CURDATE-MM-DD-YY.
           10  WS-CURDATE-MM             PIC 9(02).
           10  FILLER                    PIC X(01) VALUE '/'.
           10  WS-CURDATE-DD             PIC 9(02).
           10  FILLER                    PIC X(01) VALUE '/'.
           10  WS-CURDATE-YY             PIC 9(02).
         05 WS-CURTIME-HH-MM-SS.
           10  WS-CURTIME-HH             PIC 9(02).
           10  FILLER                    PIC X(01) VALUE ':'.
           10  WS-CURTIME-MM             PIC 9(02).
           10  FILLER                    PIC X(01) VALUE ':'.
           10  WS-CURTIME-SS             PIC 9(02).
         05 WS-TIMESTAMP.
           10  WS-TIMESTAMP-DT-YYYY      PIC 9(04).
           10  FILLER                    PIC X(01) VALUE '-'.
           10  WS-TIMESTAMP-DT-MM        PIC 9(02).
           10  FILLER                    PIC X(01) VALUE '-'.
           10  WS-TIMESTAMP-DT-DD        PIC 9(02).
           10  FILLER                    PIC X(01) VALUE ' '.
           10  WS-TIMESTAMP-TM-HH        PIC 9(02).
           10  FILLER                    PIC X(01) VALUE ':'.
           10  WS-TIMESTAMP-TM-MM        PIC 9(02).
           10  FILLER                    PIC X(01) VALUE ':'.
           10  WS-TIMESTAMP-TM-SS        PIC 9(02).
           10  FILLER                    PIC X(01) VALUE '.'.
           10  WS-TIMESTAMP-TM-MS6       PIC 9(06).
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:15:58 CDT
      *

```