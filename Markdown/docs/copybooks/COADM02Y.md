---
icon: material/file-code
---
# COADM02Y

## Description
This copybook defines the structure for the CardDemo application's administration menu options, specifically focusing on user security management. It contains a data structure that holds information about four administrative functions: listing users, adding users, updating users, and deleting users. Each menu option is defined with a numeric identifier, descriptive text, and the associated program name to be called when selected. The structure is organized to allow for up to 9 menu options (though only 4 are currently defined) and includes a count field to track the actual number of options available. This copybook is likely used across multiple programs that need to display or process the administration menu within the CardDemo application.

## Copybook Code
```cobol
      ******************************************************************
      * CardDemo - Admin Menu Options
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
       01 CARDDEMO-ADMIN-MENU-OPTIONS.
         05 CDEMO-ADMIN-OPT-COUNT           PIC 9(02) VALUE 4.

         05 CDEMO-ADMIN-OPTIONS-DATA.

           10 FILLER                        PIC 9(02) VALUE 1.
           10 FILLER                        PIC X(35) VALUE
               'User List (Security)               '.
           10 FILLER                        PIC X(08) VALUE 'COUSR00C'.

           10 FILLER                        PIC 9(02) VALUE 2.
           10 FILLER                        PIC X(35) VALUE
               'User Add (Security)                '.
           10 FILLER                        PIC X(08) VALUE 'COUSR01C'.

           10 FILLER                        PIC 9(02) VALUE 3.
           10 FILLER                        PIC X(35) VALUE
               'User Update (Security)             '.
           10 FILLER                        PIC X(08) VALUE 'COUSR02C'.

           10 FILLER                        PIC 9(02) VALUE 4.
           10 FILLER                        PIC X(35) VALUE
               'User Delete (Security)             '.
           10 FILLER                        PIC X(08) VALUE 'COUSR03C'.

         05 CDEMO-ADMIN-OPTIONS REDEFINES CDEMO-ADMIN-OPTIONS-DATA.
           10 CDEMO-ADMIN-OPT OCCURS 9 TIMES.
             15 CDEMO-ADMIN-OPT-NUM           PIC 9(02).
             15 CDEMO-ADMIN-OPT-NAME          PIC X(35).
             15 CDEMO-ADMIN-OPT-PGMNAME       PIC X(08).
      *
      * Ver: CardDemo_v1.0-26-g42273c1-79 Date: 2022-07-20 16:59:12 CDT
      *

```