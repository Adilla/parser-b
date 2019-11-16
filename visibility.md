# Abstract Machine

## Parameter

|| CONSTRAINTS | INCLUDES/EXTENDS | PROPERTIES | INVARIANT | OPERATIONS
---------|-----|-----|-----|-----|---
Machine (D) | yes | yes | no  | yes | yes
Used (D)    | no  | no  | no  | yes | yes
Seen (D)    | no  | no  | no  | no  | no

## Abstract Set/Concrete Set/Enumerate/Concrete Constant

|| CONSTRAINTS | INCLUDES/EXTENDS | PROPERTIES | INVARIANT | OPERATIONS
---------|-----|-----|-----|-----|---
Machine (D) | no  | yes | yes | yes | yes
Seen    (E) | no  | yes | yes | yes | yes
Used    (E) | no  | no  | yes | yes | yes
Included (E) | no  | no  | yes | yes | yes

## Abstract Constant

|| CONSTRAINTS | INCLUDES/EXTENDS | PROPERTIES | INVARIANT | OPERATIONS
---------|-----|-----|-----|-----|---
Machine (D) | no  | no | yes | yes | yes
Seen    (E) | no  | no | yes | yes | yes
Used    (E) | no  | no  | yes | yes | yes
Included (E) | no  | no  | yes | yes | yes

## Concrete Variable/Abstract Variable

|| CONSTRAINTS | INCLUDES/EXTENDS | PROPERTIES | INVARIANT | OPERATIONS
---------|-----|-----|-----|-----|---
Machine  (D) | no  | no  | no  | yes   | yes (rw)
Seen     (E) | no  | no  | no  | no (1)| yes (ro)
Used     (E) | no  | no  | no  | yes   | yes (ro)
Included (E) | no  | no  | no  | yes   | yes (ro)

# Refinement

## Parameter

|| INCLUDES/EXTENDS | PROPERTIES | INVARIANT | ASSERT | OPERATIONS 
---------------------|-----|-----|-----------|--------|-----------
Refinement (D) | yes | no  | yes | yes | yes
Seen (D)       | no  | no  | no  | no  | no

## Abstract Set/Concrete Set/Enumerate

|| INCLUDES/EXTENDS | PROPERTIES | INVARIANT | ASSERT | OPERATIONS 
---------------------|-----|-----|-----------|--------|-----------
Refinememt (D)       | yes | yes | yes | yes | yes
Refined (E)          | yes | yes | yes | yes | yes
Seen (E)             | yes | yes | yes | yes | yes
Included (E) (3)     | no  | yes | yes | yes | yes

## Concrete Constant

|| INCLUDES/EXTENDS | PROPERTIES | INVARIANT | ASSERT | OPERATIONS 
---------------------|-----|-----|-----------|--------|-----------
Refinememt (D)       | yes | yes | yes | yes | yes
Refined (E)          | yes | yes | yes | yes | yes
Seen (E)             | yes | yes | yes | yes | yes
Included (E) (3)     | no  | yes | yes | yes | yes
Refined (E) and Refinement (D) (2) | yes  | yes | yes | yes | yes
Refined (E) and Included (E) (2) | no  | yes | yes | yes | yes

## Abstract Constant

|| INCLUDES/EXTENDS | PROPERTIES | INVARIANT | ASSERT | OPERATIONS 
---------------------|-----|-----|-----------|--------|-----------
Refinement (D) | no  | yes | yes | yes | yes
Refined (E)    | no  | yes | yes | yes | no 
Seen (E)       | no  | yes | yes | yes | yes
Included (E)   | no  | yes | yes | yes | yes
Refined (E) and Refinement (D) | no  | yes | yes | yes | yes
Refined (E) and Included (E) | no  | yes | yes | yes | yes

## Concrete Variable

|| INCLUDES/EXTENDS | PROPERTIES | INVARIANT | ASSERT | OPERATIONS 
---------------------|-----|-----|-----------|--------|-----------
Refinement (D)          | no  | no  | yes    | yes | yes (rw)
Refined (E)             | no  | no  | yes    | yes | yes (rw)
Seen (E)                | no  | no  | no (1) | yes | yes (ro)
Included (3) (E)        | no  | no  | yes    | yes | yes (ro)
Refined (E) and Refinement (D) (2) | no  | no  | yes | yes | yes (rw)
Refined (E) and Included (E) (2) | no  | no  | yes | yes | yes (ro)

## Abstract Variable

|| INCLUDES/EXTENDS | PROPERTIES | INVARIANT | ASSERT | OPERATIONS 
---------------------|-----|-----|-----------|--------|-----------
Refinement (D) | no  | no  | yes    | yes | yes (rw)
Refined    (E) | no  | no  | yes    | yes | no 
Seen       (E) | no  | no  | no (1) | yes | yes (ro)
Included   (E) | no  | no  | yes    | yes | yes (ro)
Refined (E) and Refinement (D) | no  | no  | yes | yes | yes (rw)
Refined (E) and Included (E)   | no  | no  | yes | yes | yes (ro)
 
# Implementation

## Parameter

|| IMPORTS/EXTENDS | PROPERTIES | VALUES | INVARIANT | ASSERT/WHILE | LOCAL\_OPERATIONS | OPERATIONS 
-------------------|------------|--------|-----------|--------------|-------------------|-----------|---
Implementation (D) | yes | no  | no  | yes | yes | yes | yes
Seen (D)           | no  | no  | no  | no  | no  | no  | no

## Abstract Set/Concrete Set/Enumerate

|| IMPORTS/EXTENDS | PROPERTIES | VALUES | INVARIANT | ASSERT/WHILE | LOCAL\_OPERATIONS | OPERATIONS 
-------------------|------------|--------|-----------|--------------|-------------------|-----------|---
Implementation (D) | yes | yes | yes | yes | yes | yes | yes
Refined (E)        | yes | yes | yes | yes | yes | yes | yes
Seen (E)           | yes | yes | yes | yes | yes | yes | yes
Imported (E)       | no  | yes | yes | yes | yes | yes | yes
Refined (E) and Seen (E) | yes | yes | yes | yes | yes | yes | yes
Refined (E) and Imported (E) | no | yes | yes | yes | yes | yes | yes

## Concrete Constant

|| IMPORTS/EXTENDS | PROPERTIES | VALUES | INVARIANT | ASSERT/WHILE | LOCAL\_OPERATIONS | OPERATIONS 
-------------------|------------|--------|-----------|--------------|-------------------|-----------|---
Implementation (D) | yes | yes | yes | yes | yes | yes | yes
Refined (E)        | yes | yes | yes | yes | yes | yes | yes
Seen (E)           | yes | yes | yes | yes | yes | yes | yes
Imported (E)       | no  | yes | yes | yes | yes | yes | yes
Refined (E) and Implementation (E) (2) | yes | yes | yes | yes | yes | yes | yes
Refined (E) and Imported (E) | no | yes | yes | yes | yes | yes | yes

## Abstract Constant

|| IMPORTS/EXTENDS | PROPERTIES | VALUES | INVARIANT | ASSERT/WHILE | LOCAL\_OPERATIONS | OPERATIONS 
-------------------|------------|--------|-----------|--------------|-------------------|-----------|---
Refined (E)             | no  | yes | no  | yes | yes | no  | no 
Seen (E)                | no  | yes | no  | yes | yes | yes | no 
Imported (E)            | no  | yes | no  | yes | yes | yes | no 
Refined (E) and Imported (E) | no  | yes | no  | yes | yes | yes | no 

## Concrete Variable

|| IMPORTS/EXTENDS | PROPERTIES | VALUES | INVARIANT | ASSERT/WHILE | LOCAL\_OPERATIONS | OPERATIONS 
-------------------|------------|--------|-----------|--------------|-------------------|-----------|---
Implementation (D)       | no  | no  | no  | yes    | yes | yes (rw) | yes (rw)
Refined (E)              | no  | no  | no  | yes    | yes | yes (rw) | yes (rw)
Seen (E)                 | no  | no  | no  | no (1) | yes | yes (ro) | yes (ro)
Imported (E)             | no  | no  | no  | yes    | yes | yes (rw) | yes (ro)
Refined (E) and Imported (E) (2) | no  | no  | no  | yes    | yes | yes (rw) | yes (ro)

## Abstract Variable

|| IMPORTS/EXTENDS | PROPERTIES | VALUES | INVARIANT | ASSERT/WHILE | LOCAL\_OPERATIONS | OPERATIONS 
-------------------|------------|--------|-----------|--------------|-------------------|-----------|---
Refined (E)             | no  | no  | no  | yes   | yes | no     | no 
Seen (E)                | no  | no  | no  | no (1)| yes | yes (ro) | no 
Imported (E)            | no  | no  | no  | yes   | yes | yes (rw) | no 
Refined (E) and Imported (E) | no  | no  | no  | yes   | yes | yes (rw) | no 

 - (1) yes if option Extended\_Sees is on
 - (2) only if the concrete variable/constant is abstract in the refined machine.
 - (3) This line also applies when the data is declared in a machine included both by the refinement and the refined machine
 - (D) Data declared in the component
 - (E) Data exported by the component. A data is exported by a component if it is declared in the component or exported by an included/extended/imported component.
