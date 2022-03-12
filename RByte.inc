.286
ReadByte MACRO Destination, source, varstr 
;;Source = where i want to read the string from
;;Destination = the hexa number (coresponding to the string)
  pusha
  pushf
    lea si, source
    lea di, varstr
    MOVSW         ;copy the string into a variable string
    Str_Hex varstr, Destination   ;convert the string to hexa number and save it in the destination
  popf
  popa  
ENDM