;; hUGETracker note table.
;; Written by SuperDisk 2019

;; Gameboy sound frequences are represented as 11 byte periods.
;; this note table was generated from http://www.devrs.com/gb/files/sndtab.html

;; Since they're 11 bytes, and to avoid doing a bunch of anding and shuffling stuff
;; through HL at playtime, we store them here for a total of 142 bytes of rom.

db 44, 0 	 ;; C_3 = 44
db 156, 0 	 ;; C#_3 = 156
db 6, 1 	 ;; D_3 = 7
db 107, 1 	 ;; D#_3 = 107
db 201, 1 	 ;; E_3 = 201
db 35, 2 	 ;; F_3 = 35
db 119, 2 	 ;; F#_3 = 119
db 198, 2 	 ;; G_3 = 198
db 18, 3 	 ;; G#_3 = 19
db 86, 3 	 ;; A_3 = 87
db 155, 3 	 ;; A#_3 = 155
db 218, 3 	 ;; B_3 = 219
db 22, 4 	 ;; C_4 = 22
db 78, 4 	 ;; C#_4 = 78
db 131, 4 	 ;; D_4 = 135
db 181, 4 	 ;; D#_4 = 181
db 229, 4 	 ;; E_4 = 229
db 17, 5 	 ;; F_4 = 21
db 59, 5 	 ;; F#_4 = 63
db 99, 5 	 ;; G_4 = 103
db 137, 5 	 ;; G#_4 = 141
db 172, 5 	 ;; A_4 = 173
db 206, 5 	 ;; A#_4 = 207
db 237, 5 	 ;; B_4 = 237
db 10, 6 	 ;; C_5 = 14
db 39, 6 	 ;; C#_5 = 39
db 66, 6 	 ;; D_5 = 70
db 91, 6 	 ;; D#_5 = 95
db 114, 6 	 ;; E_5 = 118
db 137, 6 	 ;; F_5 = 143
db 158, 6 	 ;; F#_5 = 158
db 178, 6 	 ;; G_5 = 182
db 196, 6 	 ;; G#_5 = 198
db 214, 6 	 ;; A_5 = 214
db 231, 6 	 ;; A#_5 = 231
db 247, 6 	 ;; B_5 = 247
db 6, 7 	 ;; C_6 = 7
db 20, 7 	 ;; C#_6 = 23
db 33, 7 	 ;; D_6 = 39
db 45, 7 	 ;; D#_6 = 47
db 57, 7 	 ;; E_6 = 63
db 68, 7 	 ;; F_6 = 71
db 79, 7 	 ;; F#_6 = 79
db 89, 7 	 ;; G_6 = 95
db 98, 7 	 ;; G#_6 = 103
db 107, 7 	 ;; A_6 = 111
db 115, 7 	 ;; A#_6 = 119
db 123, 7 	 ;; B_6 = 127
db 131, 7 	 ;; C_7 = 135
db 138, 7 	 ;; C#_7 = 143
db 144, 7 	 ;; D_7 = 151
db 151, 7 	 ;; D#_7 = 151
db 157, 7 	 ;; E_7 = 159
db 162, 7 	 ;; F_7 = 167
db 167, 7 	 ;; F#_7 = 167
db 172, 7 	 ;; G_7 = 175
db 177, 7 	 ;; G#_7 = 183
db 182, 7 	 ;; A_7 = 183
db 186, 7 	 ;; A#_7 = 191
db 190, 7 	 ;; B_7 = 191
db 193, 7 	 ;; C_8 = 199
db 196, 7 	 ;; C#_8 = 199
db 200, 7 	 ;; D_8 = 207
db 203, 7 	 ;; D#_8 = 207
db 206, 7 	 ;; E_8 = 207
db 209, 7 	 ;; F_8 = 215
db 212, 7 	 ;; F#_8 = 215
db 214, 7 	 ;; G_8 = 215
db 217, 7 	 ;; G#_8 = 223
db 219, 7 	 ;; A_8 = 223
db 221, 7 	 ;; A#_8 = 223
db 223, 7 	 ;; B_8 = 223