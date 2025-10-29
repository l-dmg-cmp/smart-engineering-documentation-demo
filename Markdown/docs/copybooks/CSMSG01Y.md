---
icon: material/file-code
---
# CSMSG01Y

## Description
This copybook defines a set of common message constants used throughout the CardDemo application. It contains predefined text messages for standard user interactions, including a thank you message and an invalid key notification. These standardized messages help maintain consistency in user communication across different components of the CardDemo application.

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
       01 CCDA-COMMON-MESSAGES.
         05 CCDA-MSG-THANK-YOU         PIC X(50) VALUE
              'Thank you for using CardDemo application...      '.
         05 CCDA-MSG-INVALID-KEY       PIC X(50) VALUE
              'Invalid key pressed. Please see below...         '.
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:15:58 CDT
      *

```