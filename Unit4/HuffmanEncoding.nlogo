extensions [table]

globals [freq-tbl code-tbl *LEFT *RIGHT *WEIGHT *LETTER]

to reset
  set huffman-encoded-text ""
  set binary-encoded-text ""
  set header ""
  set text-to-encode ""
  set decoded-text ""
end

to encode
  set huffman-encoded-text ""
  set binary-encoded-text ""
  set header ""
  ; set constant values needed later
  set *WEIGHT 0
  set *LETTER 1
  set *LEFT 2
  set *RIGHT 3
  
  ;; build a table of the
  ;; frequency of each symbol
  ;; in the text-to-encode
  set freq-tbl (build-freq-table text-to-encode)
  
  ;; build huffman encoding
  ;; tree using the frequency 
  ;; table
  let #huff-tree build-huff-tree freq-tbl
  
  ;; then build a code table
  ;; for use during the encryption
  ;; process
  set code-tbl build-code-table #huff-tree
  
  ;; iterate through and encode
  ;; each symbol in the 
  ;; text-to-encode
  let #n 0
  while [#n < length text-to-encode] [
    let #l substring text-to-encode #n (#n + 1)
    ;; first in the huffman code
    set huffman-encoded-text (word huffman-encoded-text encoded-letter #l)
    ;; then in binary
    set binary-encoded-text (word binary-encoded-text binary-encoded-letter #l)
    set #n #n + 1
  ]
  
  ;; create the header needed to
  ;; decode the huffman encoded
  ;; text
  let #huff-header huffman-header
  set header #huff-header
end

to decode
  let #header header
  
  ;; read first bit of header 
  ;; to find out what kind of 
  ;; header will be read, 
  ;; tree (0) or frequency table (1)
  let #header-type-bit first #header
  set #header but-first #header
  
  let #freq-tbl table:make
  ifelse #header-type-bit = "0"
    [ read-tree-header #header ]
    [ ;; else
      ;; it is a frequency table header
      ;; so we need to read in the frequency
      ;; table
      set #freq-tbl read-frequency-table-header #header 
      ;; now we have build our huffman tree
      ;; table
      let #h-tree build-huff-tree #freq-tbl
      
      ;; walk tree with bit stream and put
      ;; decoded text in the monitor
      let #bit-stream huffman-encoded-text
      decode-bit-stream #bit-stream #h-tree
    ]
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

to-report build-code-table [$huff-tree]
  ;; procedure to initialize
  ;; code table and then 
  ;; call recursive procedure
  ;; to build it
  let #code-tbl table:make
  walk-tree-to-build-code-table "" $huff-tree #code-tbl
  report #code-tbl
end

to walk-tree-to-build-code-table [binary-code $node $code-tbl]
  ;; recursive procedure to walk tree
  ;; to leaf nodes and then record letter
  ;; and binary code in a code-tbl
  
  ;; if the node does not have a letter
  ;; then recursively continue the walk
  ;; by calling code-tree on left and
  ;; right nodes held by the current node
  ifelse item *LETTER $node = "" [
    walk-tree-to-build-code-table (word binary-code "0") (item *LEFT $node) $code-tbl
    walk-tree-to-build-code-table (word binary-code "1") (item *RIGHT $node) $code-tbl
  ]
  [ ;; else
    ;; the node has a letter in it
    ;; then it is a leaf node -- add
    ;; the letter and code to the 
    ;; code-tbl 
    ifelse binary-code = ""
      ;; found leaf node immediately so put in a 0
      ;; in the code-tbl
      [table:put $code-tbl (item *LETTER $node) "0"]
      ;; found leaf node, record letter and code
      ;; in the code-tbl
      [table:put $code-tbl (item *LETTER $node) binary-code]
  ] ;; end if
end

to decode-bit-stream [$bit-stream $node] 
  let #plain-text ""
  while [length $bit-stream > 0] [
    let #result (walk-tree-to-decode-bit-stream $bit-stream $node)
    set $bit-stream (item 1 #result)
    set #plain-text (word #plain-text (item 0 #result))
  ]
  set decoded-text #plain-text
end

to-report walk-tree-to-decode-bit-stream [$bit-stream $node]
  ;; recursive procedure to walk tree 
  ;; based on bit-stream until a node
  ;; is reached
  ifelse item *LETTER $node = ""
    [ ;; read next bit
      let #next-bit first $bit-stream
      set $bit-stream but-first $bit-stream
      ;; go left if next bit is 0
      ;; go right if next bit is 1
      ifelse #next-bit = "0"
      [report walk-tree-to-decode-bit-stream $bit-stream (item *LEFT $node)]
      [report walk-tree-to-decode-bit-stream $bit-stream (item *RIGHT $node)]
    ]
    [ ;; else
      ;; this is a leaf-node so
      ;; append the stored symbol
      ;; to the decoded-text
      report list (item *LETTER $node) $bit-stream
    ] ;; end if
end
  
to-report build-freq-table [$text]
  let #freq-tbl table:make
  let n 0
  while [n < (length $text)] [
    let ltr (substring $text n (n + 1))
    ifelse table:has-key? #freq-tbl ltr
      [ table:put #freq-tbl ltr ( (table:get #freq-tbl ltr) + 1) ]
      [ table:put #freq-tbl ltr 1]
    set n (n + 1)
  ]
  report #freq-tbl
end

to-report build-huff-tree [$freq-tbl]
  ;; get sorted keys and put them in a list, 
  ;; the list that will become our encoding tree
  let #tree sort-by [table:get $freq-tbl ?1 < table:get $freq-tbl ?2] table:keys $freq-tbl
  
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

to read-tree-header [$header]
end

to-report read-frequency-table-header [$header]
  let #freq-tbl table:make
  
  ;; read first byte to find out how many
  ;; bits encode the frequency
  let #frequency-length-byte substring $header 0 8
  let #frequency-length binary-to-decimal #frequency-length-byte
  
  ;; read second byte to find out how many
  ;; entries there are in the frequency table
  let #entry-count-byte substring $header 8 16
  let #entry-count binary-to-decimal #entry-count-byte
  
  ;; check header
  let #header-length 16 + (7 + #frequency-length) * #entry-count
  if length $header != #header-length
    [show "Incorrect header length" stop]
  
  
  let #n 16
  while [#n < #header-length] [
    let #symbol-bits substring $header #n (#n + 7)
    let #frequency-bits substring $header (#n + 7) (#n + 7 + #frequency-length)
    let #symbol from-ascii binary-to-decimal #symbol-bits
    let #frequency binary-to-decimal #frequency-bits
    table:put #freq-tbl #symbol #frequency
    set #n (#n + 7 + #frequency-length)
  ]
  report #freq-tbl
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
  ;; all of the non-printable
  ;; symbols
  
  if $ltr = "\t" [report 9]
  if $ltr = "\n" [report 10]
  if $ltr = "\r" [report 13]
  if $ltr = " " [report 32]

  let #ascii "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
  ifelse member? $ltr #ascii
    [report ((position $ltr #ascii) + 32)]
    [report 0]

end

to-report from-ascii [$ascii-code]
  ;; function to report the 
  ;; ascii code for a given 
  ;; symbol; does not include
  ;; all of the non-printable
  ;; symbols
  if $ascii-code = 9 [report "\t"]
  if $ascii-code = 10 [report "\n"]
  if $ascii-code = 13 [report "\r"]
  if $ascii-code = 32 [report " "]
  
  let #ascii "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
  ifelse $ascii-code > 31 and $ascii-code < (32 + (length #ascii))
    [report substring #ascii ($ascii-code - 32) ($ascii-code - 32 + 1)]
    [report ""]

end

to-report huffman-header
  ;; function to create the
  ;; binary header that can
  ;; be used to decode the
  ;; huffman encoded text
  let #header ""
  ifelse huffman-header-type = "frequency table header"
    [ set #header build-frequency-table-header ]
    [ set #header tree-header ]
  report #header
end

to-report build-frequency-table-header
  ;; first put a single bit to indicate
  ;; the header type
  ;; 1 for the frequency table header
  let #header "1"
  ;; find out how many
  ;; are needed to encode symbol
  ;; frequencies
  ;; then create a byte that holds
  ;; that information
  let #bin-code-freq-len binary-code-for-freq-length
  ;; TODO clean this up so we're not translating back to decimal
  let #freq-bits binary-to-decimal #bin-code-freq-len
  set #header (word #header bits-to-byte #bin-code-freq-len)
  
  ;; then find out how many symbols
  ;; there are and encode that in 
  ;; a byte so the reader knows when
  ;; header ends and data starts
  set #header (word #header binary-code-for-symbol-count)
  
  ;; now loop over freq-tbl
  foreach table:keys freq-tbl [
    ;; now encode first the symbol as
    ;; 7 bits since ascii is less 
    ;; than 128 decimal
    let #symbol-bin-code pad-bits (decimal-to-binary ascii-code ?) 7
    set #header (word #header #symbol-bin-code)
    
    ;; then encode the frequency of the
    ;; symbol in the determined number
    ;; of bits
    let #freq-bin-code pad-bits (decimal-to-binary (table:get freq-tbl ?)) #freq-bits
    set #header (word #header #freq-bin-code)
  ]
  report #header
end

to-report tree-header
  let #header ""
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
212
352
457
556
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
8
126
85
159
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
7
10
553
123
text-to-encode
NIL
1
0
String

INPUTBOX
675
121
1225
234
huffman-encoded-text
NIL
1
0
String

BUTTON
1133
291
1210
324
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
674
10
1224
117
header
NIL
1
0
String

INPUTBOX
2
231
557
343
binary-encoded-text
NIL
1
0
String

CHOOSER
93
127
283
172
huffman-header-type
huffman-header-type
"tree header" "frequency table header"
1

MONITOR
674
239
868
284
Length of Huffman Encoded Text
length huffman-encoded-text
17
1
11

MONITOR
290
126
431
171
Length of Text
length text-to-encode
17
1
11

MONITOR
873
239
992
284
Length of Header
length header
17
1
11

MONITOR
3
349
183
394
Length of Binary Encoded Text
length binary-encoded-text
17
1
11

MONITOR
999
239
1210
284
Length of Header + H. Encoded Text
length header + length huffman-encoded-text
17
1
11

INPUTBOX
673
368
1224
525
decoded-text
NIL
1
0
String

BUTTON
447
128
510
161
NIL
reset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

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
