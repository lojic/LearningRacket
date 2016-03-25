#lang racket

(define text #<<HERE
Haul wind red ensign mizzenmast poop deck hearties transom lee
Brethren of the Coast hornswaggle Sea Legs. Spanish Main jolly
boat bilge reef grog hail-shot jury mast carouser rope's end
scourge of the seven seas. Nipperkin wherry hempen halter Shiver
me timbers sloop landlubber or just lubber quarterdeck crow's
nest yard squiffy.

Come about man-of-war measured fer yer chains topsail prow
Spanish Main Jack Tar boatswain holystone galleon. Ahoy grog
blossom walk the plank jolly boat ye haul wind topmast gunwalls
parrel list. Brethren of the Coast hulk grog blossom doubloon
gally Admiral of the Black maroon schooner yard plunder.

Scallywag main sheet draft rigging crow's nest jib gangplank
keelhaul Brethren of the Coast spirits. Privateer chase guns warp
quarter heave to galleon spirits lee hang the jib
topmast. Mizzenmast nipper spyglass fluke sutler rigging port
mizzen hornswaggle bilged on her anchor.
HERE
)

; Accept a string and return a list of words in decreasing
; order of their frequency with the count.
(define (word-frequency str)
  (sort (hash->list
         (foldl (λ (w hsh) (hash-update hsh w add1 0))
                (hash)
                (string-split (string-downcase str))))
        > #:key cdr))

(word-frequency text)

