extensions [table]

globals [freq-tbl code-tbl *LEFT *RIGHT *WEIGHT *LETTER]

to encode
  set text-to-decode ""
  set binary-encoding ""
  set header ""
  ; set constant values needed later
  set *WEIGHT 0
  set *LETTER 1
  set *LEFT 2
  set *RIGHT 3
  
  ;; build a table of the
  ;; frequency of each symbol
  ;; in the text-to-encode
  build-freq-table
  
  ;; build huffman encoding
  ;; tree using the frequency 
  ;; table
  let #huff-tree build-huff-tree
  
  ;; then build a code table
  ;; for use during the encryption
  ;; process
  build-code-table #huff-tree
  
  ;; iterate through and encode
  ;; each symbol in the 
  ;; text-to-encode
  let #n 0
  while [#n < length text-to-encode] [
    let #l substring text-to-encode #n (#n + 1)
    ;; first in the huffman code
    set text-to-decode (word text-to-decode encoded-letter #l)
    ;; then in binary
    set binary-encoding (word binary-encoding binary-encoded-letter #l)
    set #n #n + 1
  ]
  
  ;; create the header needed to
  ;; decode the huffman encoded
  ;; text
  let #huff-header huffman-header
  set header #huff-header
  
  show code-tbl
end

to-report encoded-letter [$ltr]
  report table:get code-tbl $ltr
end

to-report node-vis [$node]
  let #has-right (is-list? (item *RIGHT $node))
  let #has-left (is-list? (item *LEFT $node))
  let #vis (list (item *WEIGHT $node) (item *LETTER $node) #has-left #has-right)
  report #vis
end

to walk-tree [$tree]
  let #head node-vis $tree
end

to build-code-table [$huff-tree]
  ;; procedure to initialize
  ;; code table and then 
  ;; call recursive procedure
  ;; to build it
  set code-tbl table:make
  code-tree "" $huff-tree
end

to code-tree [binary-code $node]
  ;; recursive procedure to walk tree
  ;; to leaf nodes and then record letter
  ;; and binary code in the code-tbl
  
  ;; if the node does not have a letter
  ;; then recursively continue the walk
  ;; by calling code-tree on left and
  ;; right nodes held by the current node
  ifelse item *LETTER $node = "" [
    code-tree (word binary-code "0") (item *LEFT $node)
    code-tree (word binary-code "1") (item *RIGHT $node) 
  ]
  [ ;; else
    ;; the node has a letter in it
    ;; then it is a leaf node -- add
    ;; the letter and code to the 
    ;; code-tbl 
    ifelse binary-code = ""
      ;; found leaf node immediately so put in a 0
      ;; in the code-tbl
      [table:put code-tbl (item *LETTER $node) "0"]
      ;; found leaf node, record letter and code
      ;; in the code-tbl
      [table:put code-tbl (item *LETTER $node) binary-code]
  ] ;; end if
end
    
to build-freq-table
  set freq-tbl table:make
  let n 0
  while [n < (length text-to-encode)] [
    let ltr (substring text-to-encode n (n + 1))
    ifelse table:has-key? freq-tbl ltr
      [ table:put freq-tbl ltr ( (table:get freq-tbl ltr) + 1) ]
      [ table:put freq-tbl ltr 1]
    set n (n + 1)
  ]
end

to-report build-huff-tree
  ;; get sorted keys and put them in a list, 
  ;; the list that will become our encoding tree
  let #tree sort-by [table:get freq-tbl ?1 < table:get freq-tbl ?2] table:keys freq-tbl
  
  while [(length #tree) > 1] [
    let #left first #tree
    set #tree butfirst #tree
    let #right first #tree
    set #tree butfirst #tree
    let #subtree make-subtree #left #right
    set #tree insert-into-tree #tree #subtree
  ]
  ;; the build process left us with a
  ;; list wrapped around the first node,
  ;; so just return the first node
  report item 0 #tree
end 

to-report make-subtree [$k1 $k2]
  ;; set #left-node to $k1
  let #left-node $k1
  ;; replace it with a new node if
  ;; $k1 isn't a node but is a key
  if not is-list? $k1 
    [ set #left-node new-node (get-weight $k1) $k1 false false ]
    
  ;; same goes for $k2
  let #right-node $k2 
  if not is-list? $k2
    [ set #right-node new-node (get-weight $k2) $k2 false false]  
  let #weight ((get-weight $k1) + (get-weight $k2))
  let #subtree new-node #weight "" #left-node #right-node
  report #subtree
end
  
to-report new-node [$weight $letter $left $right]
  let #node (list $weight $letter $left $right)
  report #node
end

to-report get-weight [$node]
  let #result -1
  ifelse is-list? $node
    ;; if the item is a tree, report weight
    [ set #result (item *WEIGHT $node) ]
    ;; else item is a key-value, so go to
    ;; freq-tbl to get the freq --> weight
    [ set #result (table:get freq-tbl $node) ]
    
    report #result
end

to-report insert-into-tree [$list $node]
  ;; find the node in the tree that has the
  ;; greatest value
  let #n 0
  let #continue? true
  while [#n < (length $list) and #continue?] [
    let $list-item (item #n $list)
    if get-weight $list-item > get-weight $node [set #continue? false]
    if #continue? [set #n (#n + 1)]
  ]
  
  let #new-list []  
  ifelse #continue? [
    ;; we fell out of list without finding
    ;; a node with greater weight; the
    ;; $node has the greatest weight so
    ;; put it at the end of the list
    set #new-list lput $node $list
  ] 
  [ ;; else
    ;; loop through to build a new list
    ;; but put $node in before the item
    ;; that was of greater weight
    let #m 0
    foreach $list [
      if (#m = #n) [set #new-list lput $node #new-list]
      set #new-list lput ? #new-list
      set #m (#m + 1)
    ] ;; end foreach
  ] ;; end if
  report #new-list
end

to-report read-header
  ;; read the first byte to 
  ;; find out how many bits
  ;; encode the frequency
  report ""
end

to-report binary-encoded-letter [$ltr]
  let #ascii ascii-code $ltr
  let #bin-code decimal-to-binary #ascii
  report #bin-code
end

to-report decimal-to-binary [$dec]
  let #bin ""
  while [$dec > 0] [
    let #n int ($dec / 2)
    let #m ($dec mod 2)
    ifelse #m = 0
      [ set #bin word "0" #bin]
      [ set #bin word "1" #bin]
    set $dec #n
  ]
  report #bin
end

to-report binary-to-decimal [$bin]
  let #dec 0
  let #b $bin
  let #bit read-from-string last #b
  set #dec (#dec + #bit)
  set #b but-last #b
  let #m 1
  while [length #b > 0] [
    set #bit read-from-string last #b
    set #dec (#dec + (#bit * 2 ^ #m))
    set #m (#m + 1)
    set #b but-last #b
  ]
  report #dec
end

to-report ascii-code [$ltr]
  ;; function to report the 
  ;; ascii code for a given 
  ;; symbol; does not include
  ;; any of the non-printable
  ;; symbols
  let #ascii "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
  ifelse member? $ltr #ascii
    [report ((position $ltr #ascii) + 32)]
    [report 0]
end

to-report huffman-header
  ;; function to create the
  ;; binary header that can
  ;; be used to decode the
  ;; huffman encoded text
  
  ;; first find out how many
  ;; are needed to encode symbol
  ;; frequencies
  ;; then create a byte that holds
  ;; that information
  let #bin-code-freq-len binary-code-for-freq-length
  ;; TODO clean this up so we're not translating back to decimal
  let #freq-bits binary-to-decimal #bin-code-freq-len
  let #header bits-to-byte #bin-code-freq-len
  
  ;; then find out how many symbols
  ;; there are and encode that in 
  ;; a byte so the reader knows when
  ;; header ends and data starts
  set #header (word #header binary-code-for-symbol-count)
  
  ;; now loop over freq-tbl
  foreach table:keys freq-tbl [
    ;; now encode first the symbol as
    ;; a byte
    let #symbol-bin-code bits-to-byte decimal-to-binary ascii-code ?
    set #header (word #header #symbol-bin-code)
    
    ;; then encode the frequency of the
    ;; symbol in the determined number
    ;; of bits
    let #freq-bin-code pad-bits (decimal-to-binary (table:get freq-tbl ?)) #freq-bits
    set #header (word #header #freq-bin-code)
  ]
  
  ;; TODO:
  ;; we could try to encode the 
  ;; tree by figuring out how man bits
  ;; are necessary for the value portion
  ;; of the tree encoding that in
  ;; binary
  
  ;; then encode a 0 for a node
  ;; encode 1 followed by 
  ;; the value in bits for 
  ;; a leaf
  
  ;; the walk should be similar
  ;; to code-tree
  
  report #header
end

to-report binary-code-for-freq-length
  let #max-freq get-max-freq
  let #bin-max-freq decimal-to-binary #max-freq
  let #len length #bin-max-freq
  let #bin-code-for-freq-len decimal-to-binary #len
  report #bin-code-for-freq-len 
end

to-report binary-code-for-symbol-count
  let #sym-count (length (table:keys freq-tbl))
  report bits-to-byte decimal-to-binary #sym-count
end

to-report get-max-freq 
  ;; function to find the 
  ;; maximum frequency 
  ;; in the frequency table
  let #max-freq 0
  foreach table:keys freq-tbl [
    if table:get freq-tbl ? > #max-freq [set #max-freq table:get freq-tbl ?]
  ]
  report int #max-freq
end

to-report pad-bits [$bits $pad-length]
  let #padding $pad-length - (length $bits)
  let #n 0
  let #byte $bits
  while [#n < #padding] [
    set #byte (word "0" #byte)
    set #n (#n + 1)
  ]
  report #byte
end

to-report bits-to-byte [$bits]
  report pad-bits $bits 8
end
@#$#@#$#@
GRAPHICS-WINDOW
790
10
1035
214
16
16
5.242424242424242
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
590
11
667
44
NIL
encode\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
33
10
579
123
text-to-encode
Magnus Carlsen’s meteoric rise to the top ranked player in the world (at age 19), the highest chess rating in history (age 22), and as of a few days ago, the title of World Chess Champion (age 22) has brought with it a renewed interest in chess. This is exciting, because Carlsen represents the first real hope of renewing chess’s mass appeal since the days of Bobby Fischer1.
1
0
String

INPUTBOX
32
133
582
246
text-to-decode
001000101010001010001100010111001111011111101000111101101100010000100001101100111101100001010010101101000110111110111111001101111100010111100111010111100101100101111001110101011011110011101000010010001101010111011110110111011010101000000100011111011100011111001011001011110000111010001111011010111011110110011010100111110100010101011100001110010010010111100000001111001011001011101100111001010110010110010011111101110110010110011001110011101010010111000100101111011100011110110011111001001110100011100000111101100110100010101011100000100000110111100000001111010000110111011110101100111110101000111111010111100011010100001111101110101010000011001111010001011101000000011110010110010111100101111001110110010111110101000111110010010111010001111011010111011110111110110010110011001111011111011010101011000101101011111010000111110110011010001010101110000010000011011110111011010101100111000010001111010100010100101011010011111000010111100101101110111100111110101110011010000101010000101010111011101110001100101000110101100100111101110001111110111011001011001100001000011100100110011001111100111011111001110100010011111011101111001011100010010100000011100001001011011110101000101110001011110111111010001111011011000100001111001101010110100110101100010000110011100111100101100101111000110111001111001001111001101010101101101110110110101011010101111101010001111100110100001010100001011100010010111111011101100101100110000001101100111101100010101100110011110101011011011010101010110110111110001110001110111010111100101100101111011101010100000110011111010100011111100010001101000001000001010000011110001001011111001101110110010001100001110010000
1
0
String

BUTTON
588
258
665
291
NIL
decode\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
30
256
580
363
header
0000011100100101010010110000001010111110011000011001010001010011011000010001011100110000011011100010011111000000001000011010000010000100011100000010011011010100001000011000110101001000000000000010011010110000011011100100010111011011010001111011001110010101011000010001000011001100010100011011100000111011010010000001011000100000111011101110000101011101010000101001001100000011001011110000010001101110000001001001110000011001010100000100001100000000100011001000000110010101010000001011000000000100001011000000010010100100000001011101100000001010000000000001010001000000001
1
0
String

INPUTBOX
30
370
585
482
binary-encoding
100101110111111100101110110011100111110001100000110111111110000110101011100011100011110110011100011101011110001111100101100011110110111100001100111110000111100001100111111000111000111110010110110111100101100110110001111100101101101110111011100001011111110110011010011100011110001011011101101010101111111101111100011111000011001111101100111001011001101100011111010111011011110000110101011000101001101011111111001010111111100101110001110111111011110011110101011100101100110110001111001101100111110010111001101100011111000111100101100001110011011000111110001111000111100001011111111001011001111101100110010111001111101100110011011001111110001111001011011011110000111011110011010111111100101110001111000011000010011110101010111111101100110001010111111110001110110111001001011111110010011000111110101110001010111111110111111000110111111100101110110110101011100101100110110001111100101100111111001011010101100011110110111001001010101110110111100001101010110001010000011100110110001111100011110001100000111001101011111110101111011101100111110110111011001001101011111110010111000111100001100001001111100110101111111100011100000111000011011011110011110010111001101110010111010111001111110010110011011001111110010101111111100001100011110110011000111110101110001111000101100111110110011100101100011111000011000111110001111001011001111101100110000111001101100011111000111100011011001010010110011011001111110001110011111100011100011111011011000011100111111001011001111101100110010110101011000001100011110000110111111110011111000111000111000001101111111100001101010111000111000111101100111000011000111101110111000011000111110001110001111011001110010111000111100101100110110001111001001100111111000011100011110010111000011000111011111110101011001101101101110111011000111101101110010011100001100011110110011000111110101110011111011001100101110000111001101100011111000111100011110001110101110111111110001111000110111111101110110111011000111011111110101011100011100111110110011000011100011111001011001101100011110001010111111110111111000111011011100100100000011011011100000110000011101111000100110011111100011100001110011011000111110000101111101100
1
0
String

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
