#lang wraith racket

;; Example based on "Programmable Publishing: Digital Humanities for Everyone!"
;;   https://dustycloud.org/misc/digital-humanities/
;;   https://github.com/mlemmer/DigitalHumanities
;; tutorial on "Building a snowman with Racket"
;;   https://dustycloud.org/misc/digital-humanities/Snowman.html
;;   https://github.com/mlemmer/DigitalHumanities/blob/master/Snowman.scrbl
;; by Morgan Lemmer-Webber and Christopher Lemmer Webber

require pict

define left-arm
  colorize
    text "Y" '(bold) 30
         * pi .5
    "brown"

define right-arm
  colorize
    text "Y" '(bold) 30
         * pi 1.5
    "brown"

define head
  cc-superimpose
    disk 50 #:color "white"
    text ":^D" '(bold) 20
         * pi 1.5

define body
  hc-append left-arm
            cc-superimpose
              disk 65 #:color "white"
              text "* * *" '(bold) 15
                   * pi .5
            right-arm

define butt
  disk 80 #:color "white"

define top-hat
  vc-append
    filled-rectangle 30 30
                     #:color "black"
    filled-rectangle 50 10
                     #:color "black"

define snowman
  vc-append top-hat head body butt

define snow-scene
  vc-append
    cbl-superimpose
      filled-rectangle 400 300
                       #:color "aqua"
      snowman
    filled-rectangle 400 100
                     #:color "white"
