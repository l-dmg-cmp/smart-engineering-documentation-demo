---
icon: material/file-code
---
# UNUSED1Y

## Description
This copybook defines a data structure named UNUSED-DATA that appears to be a placeholder or legacy structure not actively used in the current application. It contains fields for storing user identification information including an ID, first name, last name, password, user type, and filler space. The structure is likely maintained for compatibility with older code or for potential future use within the CardDemo application. The comment at the bottom indicates the version control information for the CardDemo project.

## Copybook Code
```cobol
       01 UNUSED-DATA.
         05 UNUSED-ID                 PIC X(08).
         05 UNUSED-FNAME              PIC X(20).
         05 UNUSED-LNAME              PIC X(20).
         05 UNUSED-PWD                PIC X(08).
         05 UNUSED-TYPE               PIC X(01).
         05 UNUSED-FILLER             PIC X(23).
      *
      * Ver: CardDemo_v1.0-56-gd8e5ebf-109 Date: 2022-08-19 17:55:18 CDT
      *

```