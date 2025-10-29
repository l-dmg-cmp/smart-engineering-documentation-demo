---
icon: material/file-code
---
# COTTL01Y

## Description
This copybook defines the screen title elements used in the CardDemo application for AWS Mainframe Modernization. It contains three 40-character fields that make up the application's header display: a main title identifying it as an AWS Mainframe Modernization application, a secondary title showing "CardDemo" (with a commented-out alternative that would have shown "Credit Card Demo Application (CCDA)"), and a thank you message displayed to users. This copybook is likely included in various screens throughout the application to maintain consistent header formatting and branding.

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
       01 CCDA-SCREEN-TITLE.
         05 CCDA-TITLE01    PIC X(40) VALUE
            '      AWS Mainframe Modernization       '.
         05 CCDA-TITLE02    PIC X(40) VALUE
      *     '  Credit Card Demo Application (CCDA)   '.
            '              CardDemo                  '.
         05 CCDA-THANK-YOU  PIC X(40) VALUE
            'Thank you for using CCDA application... '.
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:15:58 CDT
      *

```