# Abstract Machine

## Parameter

|| CONSTRAINTS | INCLUDES/EXTENDS | PROPERTIES | INVARIANT | OPERATIONS
---------|-----|-----|-----|-----|---
Machine  | yes | yes | no  | yes | yes
Seen     | no  | no  | no  | no  | no 
Used     | no  | no  | no  | yes | yes
Included | no  | no  | no  | no  | no 

## Abstract Set/Concrete Set/Enumerate/Concrete Constant/Abstract Constant

|| CONSTRAINTS | INCLUDES/EXTENDS | PROPERTIES | INVARIANT | OPERATIONS
---------|-----|-----|-----|-----|---
Machine  | no  | yes | yes | yes | yes
Seen     | no  | yes | yes | yes | yes
Used     | no  | no  | yes | yes | yes
Included | no  | no  | yes | yes | yes

## Concrete Variable/Abstract Variable

|| CONSTRAINTS | INCLUDES/EXTENDS | PROPERTIES | INVARIANT | OPERATIONS
---------|-----|-----|-----|-----|---
Machine  | no  | no  | no  | yes   | yes (rw)
Seen     | no  | no  | no  | no (*)| yes (ro)
Used     | no  | no  | no  | yes   | yes (ro)
Included | no  | no  | no  | yes   | yes (ro)

# Refinement

## Parameter

|| INCLUDES/EXTENDS | PROPERTIES | INVARIANT | ASSERT | OPERATIONS 
---------------------|-----|-----|-----------|--------|-----------
Refinement | yes | no  | yes | yes | yes
Seen       | no  | no  | no  | no  | no 
Included   | no  | no  | no  | no  | no 

## Abstract Set/Concrete Set/Enum/Concrete Constant

|| INCLUDES/EXTENDS | PROPERTIES | INVARIANT | ASSERT | OPERATIONS 
---------------------|-----|-----|-----------|--------|-----------
Refinememt           | yes | yes | yes | yes | yes
Refined              | yes | yes | yes | yes | yes
Seen                 | yes | yes | yes | yes | yes
Included             | no  | yes | yes | yes | yes
Refined and Seen     | yes | yes | yes | yes | yes
Refined and Included | no  | yes | yes | yes | yes

## Abstract Constant

|| INCLUDES/EXTENDS | PROPERTIES | INVARIANT | ASSERT | OPERATIONS 
---------------------|-----|-----|-----------|--------|-----------
Refinement | yes | yes | yes | yes | yes
Refined    | no  | yes | yes | yes | no 
Seen       | yes | yes | yes | yes | yes
Included   | no  | yes | yes | yes | yes
Refined and Refinement | yes(?) | yes | yes | yes | yes
Refined and Seen       | yes    | yes | yes | yes | yes
Refined and Included   | non(?) | yes | yes | yes | yes

## Concrete Variable

|| INCLUDES/EXTENDS | PROPERTIES | INVARIANT | ASSERT | OPERATIONS 
---------------------|-----|-----|-----------|--------|-----------
Refinement           | no  | no  | yes    | yes | yes (rw)
Refined              | no  | no  | yes    | yes | yes (rw)
Seen                 | no  | no  | no (*) | yes | yes (ro)
Included             | no  | no  | yes    | yes | yes (ro)
Refined and Included | no  | no  | yes    | yes | yes (ro)

## Abstract Variable

|| INCLUDES/EXTENDS | PROPERTIES | INVARIANT | ASSERT | OPERATIONS 
---------------------|-----|-----|-----------|--------|-----------
Refinement | no  | no  | yes    | yes | yes (rw)
Refined    | no  | no  | yes    | yes | no 
Seen       | no  | no  | non(?) | yes | yes (ro)
Included   | no  | no  | yes    | yes | non (ro)
Refined and Refinement | no  | no  | yes | yes | yes (rw)
Refined and Included   | no  | no  | yes | yes | yes (ro)
 
# Implementation

## Parameter

|| IMPORTS/EXTENDS | PROPERTIES | VALUES | INVARIANT | ASSERT/WHILE | LOCAL\_OPERATIONS | OPERATIONS 
-------------------|------------|--------|-----------|--------------|-------------------|-----------|---
Implementation | yes | no  | no  | yes | yes | yes | yes
Seen           | no  | no  | no  | no  | no  | no  | no 
Imported       | no  | no  | no  | no  | no  | no  | no 

## Abstract Set/Concrete Set/Enum/Concrete Constant

|| IMPORTS/EXTENDS | PROPERTIES | VALUES | INVARIANT | ASSERT/WHILE | LOCAL\_OPERATIONS | OPERATIONS 
-------------------|------------|--------|-----------|--------------|-------------------|-----------|---
Implementation | yes | yes | yes | yes | yes | yes | yes
Refined        | yes | yes | yes | yes | yes | yes | yes
Seen           | yes | yes | yes | yes | yes | yes | yes
Imported       | no  | yes | yes | yes | yes | yes | yes
Refined and Seen     | yes | yes | yes | yes | yes | yes | yes
Refined and Imported | no (?) | yes | yes | yes | yes | yes | yes

## Abstract Constant

|| IMPORTS/EXTENDS | PROPERTIES | VALUES | INVARIANT | ASSERT/WHILE | LOCAL\_OPERATIONS | OPERATIONS 
-------------------|------------|--------|-----------|--------------|-------------------|-----------|---
Refined              | no  | yes | no  | yes | yes | no     | no 
Seen                 | no  | yes | no  | yes | yes | yes    | no 
Imported             | no  | yes | no  | yes | yes | yes    | no 
Refined and Seen     | no  | yes | no  | yes | yes | yes(?) | no 
Refined and Imported | no  | yes | no  | yes | yes | yes(?) | no 

## Concrete Variable

|| IMPORTS/EXTENDS | PROPERTIES | VALUES | INVARIANT | ASSERT/WHILE | LOCAL\_OPERATIONS | OPERATIONS 
-------------------|------------|--------|-----------|--------------|-------------------|-----------|---
Implementation       | no  | no  | no  | yes    | yes | yes (rw) | yes (rw)
Refined              | no  | no  | no  | yes    | yes | yes (rw) | yes (rw)
Seen                 | no  | no  | no  | no (*) | yes | yes (ro) | yes (ro)
Imported             | no  | no  | no  | yes    | yes | yes (rw) | yes (ro)
Refined and Imported | no  | no  | no  | yes    | yes | yes (rw) | yes (ro)

## Abstract Variable

|| IMPORTS/EXTENDS | PROPERTIES | VALUES | INVARIANT | ASSERT/WHILE | LOCAL\_OPERATIONS | OPERATIONS 
-------------------|------------|--------|-----------|--------------|-------------------|-----------|---
Refined              | no  | no  | no  | yes   | yes | no     | no 
Seen                 | no  | no  | no  | no (*)| yes | yes (ro) | no 
Imported             | no  | no  | no  | yes   | yes | yes (rw) | no 
Refined and Imported | no  | no  | no  | yes   | yes | yes (rw) | no 
