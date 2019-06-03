dn: MACRO ;; (note, instr, effect)
    db \1
    db ((\2 << 4) | (\3 >> 8))
    db LOW(\3)
ENDM

; ;; unreal superhero
    dn F6, 11, $000
    dn G#6,00, $332
    dn C7, 00, $300
    dn F7, 00, $300
    dn G#7,00, $300
    dn F7, 00, $300
    dn C7, 00, $300
    dn G#6,00, $300
    dn F6, 00, $300
    dn G#6,00, $300
    dn C7, 00, $300
    dn F7, 00, $300
    dn G#7,00, $300
    dn F7, 00, $300
    dn C7, 00, $300
    dn G#6,00, $300
    dn F6, 00, $300
    dn C7, 00, $300
    dn D#7,00, $300
    dn G#7,00, $300
    dn C8, 00, $300
    dn G#7,00, $300
    dn D#7,00, $300
    dn C7, 00, $300
    dn G#6,00, $300
    dn C7, 00, $300
    dn D#7,00, $300
    dn G#7,00, $300
    dn C8, 00, $300
    dn G#7,00, $300
    dn D#7,00, $300
    dn C7, 00, $300
    dn A#6,00, $300
    dn D7, 00, $300
    dn F7, 00, $300
    dn A#7,00, $300
    dn D8, 00, $300
    dn A#7,00, $300
    dn F7, 00, $300
    dn D7, 00, $300
    dn A#6,00, $300
    dn D7, 00, $300
    dn F7, 00, $300
    dn A#7,00, $300
    dn D8, 00, $300
    dn A#7,00, $300
    dn F7, 00, $300
    dn D7, 00, $300
    dn F6, 00, $300
    dn C7, 00, $300
    dn F7, 00, $300
    dn C8, 00, $300
    dn F8, 00, $300
    dn C8, 00, $300
    dn F7, 00, $300
    dn C7, 00, $300
    dn F6, 00, $300
    dn C7, 00, $300
    dn F7, 00, $300
    dn C8, 00, $300
    dn F8, 00, $300
    dn C8, 00, $300
    dn F7, 00, $300
    dn C7, 00, $300

; db F4
; db G#4
; db C5
; db F5
; db G#5
; db F5
; db C5
; db G#4
; db F4
; db G#4
; db C5
; db F5
; db G#5
; db F5
; db C5
; db G#4
; db F4
; db C5
; db D#5
; db G#5
; db C6
; db G#5
; db D#5
; db C5
; db G#4
; db C5
; db D#5
; db G#5
; db C6
; db G#5
; db D#5
; db C5
; db A#4
; db D5
; db F5
; db A#5
; db D6
; db A#5
; db F5
; db D5
; db A#4
; db D5
; db F5
; db A#5
; db D6
; db A#5
; db F5
; db D5
; db F4
; db C5
; db F5
; db C6
; db F6
; db C6
; db F5
; db C5
; db F4
; db C5
; db F5
; db C6
; db F6
; db C6
; db F5
; db C5