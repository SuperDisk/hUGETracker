note: MACRO ;; (note, instr, effect)
    db \1
    db ((\2 << 4) | (\3 >> 8))
    db LOW(\3)
ENDM

; ;; unreal superhero
    note F4, 01, $A01

db F4
db G#4
db C5
db F5
db G#5
db F5
db C5
db G#4
db F4
db G#4
db C5
db F5
db G#5
db F5
db C5
db G#4
db F4
db C5
db D#5
db G#5
db C6
db G#5
db D#5
db C5
db G#4
db C5
db D#5
db G#5
db C6
db G#5
db D#5
db C5
db A#4
db D5
db F5
db A#5
db D6
db A#5
db F5
db D5
db A#4
db D5
db F5
db A#5
db D6
db A#5
db F5
db D5
db F4
db C5
db F5
db C6
db F6
db C6
db F5
db C5
db F4
db C5
db F5
db C6
db F6
db C6
db F5
db C5