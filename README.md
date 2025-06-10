for ACAD, DWCAD, BRICSCAD - multi-profile-area-2-file-v1.lsp
extract poliline area , left / right and make a rectangle for processed profiles

create folder C:\tmp\  , to save txt file 

layer 0 must exist and unfrozen
 
profiles must be vertical arranged at offset 20m
the text 0.00 in each profile is used to sort area for left / right 
 
change at line 200 layers to extract from
(8 . "frezare")    ; layer art1
(8 . "umplutura")  ; layer art2
