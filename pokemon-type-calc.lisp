#! /usr/bin/sbcl --script
;; Pokémon Generation II through V type matchup calculator
;; Written as my first Common Lisp program. There are definitely things I'd
;; change/rearrange/put into additional functions, but overall I'm pretty happy
;; with it!
;;
;; Tested in SBCL (Steel Bank Common Lisp), but should run in any Common Lisp
;; compliant Lisp.
;;
;; I wrote it so I'd have a calculator on my laptop and Android phone that
;; didn't depend on internet connectivity, and chose Common Lisp because I
;; thought it looked fun. And it was!
;;
;; (C) Wyatt Ward, 06 November 2018. Released under the BSD 3-clause license.
;; See https://opensource.org/licenses/BSD-3-Clause for full text. It's a
;; pretty lax license, so I don't forsee any issues.

;; make sure we're at least consistent. At one point I convert a string
;; into a symbol, so we need to get casing consistent.
(setf (readtable-case *readtable*) :upcase)

;; Generation II-V Pokemon type matchup calculator
;; association lists! Yay!
;; association list so that each type has an ID.
;; coincidentally, these ID's are from the order they appear in the pokemon
;; crystal disassembly, but they are not 1:1 mapped, as the 'bird' type and
;; several dummy type slots (all labeled "Normal") are skipped, as is the
;; 'curse' (???) type.
(defconstant super-effective 2.0)
(defconstant not-very-effective 0.5)
(defconstant no-effect 0)

;; the following ID's don't really matter numerically, except in that they
;; are consistent.
(defconstant normal    0)
(defconstant fighting  1)
(defconstant flying    2)
(defconstant poison    3)
(defconstant ground    4)
(defconstant rock      5)
(defconstant bug       6)
(defconstant ghost     7)
(defconstant steel     8)
(defconstant fire      9)
(defconstant water    10)
(defconstant grass    11)
(defconstant electric 12)
(defconstant psychic  13)
(defconstant ice      14)
(defconstant dragon   15)
(defconstant dark     16)

(let* (
       ;; string to id matching for user input purposes
       (typematch `( ("NORMAL" . ,normal)
                     ("FIGHTING" . ,fighting)
                     ("FLYING" . ,flying)
                     ("POISON" . ,poison)
                     ("GROUND" . ,ground)
                     ("ROCK" . ,rock)
                     ("BUG" . ,bug)
                     ("GHOST" . ,ghost)
                     ("STEEL" . ,steel)
                     ("FIRE" . ,fire)
                     ("WATER" . ,water)
                     ("GRASS" . ,grass)
                     ("ELECTRIC" . ,electric)
                     ("PSYCHIC" . ,psychic)
                     ("ICE" . ,ice)
                     ("DRAGON" . ,dragon)
                     ("DARK" . ,dark)
                     ))
       ;; every single type matchup in generation 2 in association list form.
       (effectiveness `(
                        (,`(,normal   ,rock    ) . ,not-very-effective)
                        (,`(,normal   ,steel   ) . ,not-very-effective)
                        (,`(,fire     ,fire    ) . ,not-very-effective)
                        (,`(,fire     ,water   ) . ,not-very-effective)
                        (,`(,fire     ,grass   ) . ,super-effective   )
                        (,`(,fire     ,ice     ) . ,super-effective   )
                        (,`(,fire     ,bug     ) . ,super-effective   )
                        (,`(,fire     ,rock    ) . ,not-very-effective)
                        (,`(,fire     ,dragon  ) . ,not-very-effective)
                        (,`(,fire     ,steel   ) . ,super-effective   )
                        (,`(,water    ,fire    ) . ,super-effective   )
                        (,`(,water    ,water   ) . ,not-very-effective)
                        (,`(,water    ,grass   ) . ,not-very-effective)
                        (,`(,water    ,ground  ) . ,super-effective   )
                        (,`(,water    ,rock    ) . ,super-effective   )
                        (,`(,water    ,dragon  ) . ,not-very-effective)
                        (,`(,electric ,water   ) . ,super-effective   )
                        (,`(,electric ,electric) . ,not-very-effective)
                        (,`(,electric ,grass   ) . ,not-very-effective)
                        (,`(,electric ,ground  ) . ,no-effect         )
                        (,`(,electric ,flying  ) . ,super-effective   )
                        (,`(,electric ,dragon  ) . ,not-very-effective)
                        (,`(,grass    ,fire    ) . ,not-very-effective)
                        (,`(,grass    ,water   ) . ,super-effective   )
                        (,`(,grass    ,grass   ) . ,not-very-effective)
                        (,`(,grass    ,poison  ) . ,not-very-effective)
                        (,`(,grass    ,ground  ) . ,super-effective   )
                        (,`(,grass    ,flying  ) . ,not-very-effective)
                        (,`(,grass    ,bug     ) . ,not-very-effective)
                        (,`(,grass    ,rock    ) . ,super-effective   )
                        (,`(,grass    ,dragon  ) . ,not-very-effective)
                        (,`(,grass    ,steel   ) . ,not-very-effective)
                        (,`(,ice      ,water   ) . ,not-very-effective)
                        (,`(,ice      ,grass   ) . ,super-effective   )
                        (,`(,ice      ,ice     ) . ,not-very-effective)
                        (,`(,ice      ,ground  ) . ,super-effective   )
                        (,`(,ice      ,flying  ) . ,super-effective   )
                        (,`(,ice      ,dragon  ) . ,super-effective   )
                        (,`(,ice      ,steel   ) . ,not-very-effective)
                        (,`(,ice      ,fire    ) . ,not-very-effective)
                        (,`(,fighting ,normal  ) . ,super-effective   )
                        (,`(,fighting ,ice     ) . ,super-effective   )
                        (,`(,fighting ,poison  ) . ,not-very-effective)
                        (,`(,fighting ,flying  ) . ,not-very-effective)
                        (,`(,fighting ,psychic ) . ,not-very-effective)
                        (,`(,fighting ,bug     ) . ,not-very-effective)
                        (,`(,fighting ,rock    ) . ,super-effective   )
                        (,`(,fighting ,dark    ) . ,super-effective   )
                        (,`(,fighting ,steel   ) . ,super-effective   )
                        (,`(,poison   ,grass   ) . ,super-effective   )
                        (,`(,poison   ,poison  ) . ,not-very-effective)
                        (,`(,poison   ,ground  ) . ,not-very-effective)
                        (,`(,poison   ,rock    ) . ,not-very-effective)
                        (,`(,poison   ,ghost   ) . ,not-very-effective)
                        (,`(,poison   ,steel   ) . ,no-effect         )
                        (,`(,ground   ,fire    ) . ,super-effective   )
                        (,`(,ground   ,electric) . ,super-effective   )
                        (,`(,ground   ,grass   ) . ,not-very-effective)
                        (,`(,ground   ,poison  ) . ,super-effective   )
                        (,`(,ground   ,flying  ) . ,no-effect         )
                        (,`(,ground   ,bug     ) . ,not-very-effective)
                        (,`(,ground   ,rock    ) . ,super-effective   )
                        (,`(,ground   ,steel   ) . ,super-effective   )
                        (,`(,flying   ,electric) . ,not-very-effective)
                        (,`(,flying   ,grass   ) . ,super-effective   )
                        (,`(,flying   ,fighting) . ,super-effective   )
                        (,`(,flying   ,bug     ) . ,super-effective   )
                        (,`(,flying   ,rock    ) . ,not-very-effective)
                        (,`(,flying   ,steel   ) . ,not-very-effective)
                        (,`(,psychic  ,fighting) . ,super-effective   )
                        (,`(,psychic  ,poison  ) . ,super-effective   )
                        (,`(,psychic  ,psychic ) . ,not-very-effective)
                        (,`(,psychic  ,dark    ) . ,no-effect         )
                        (,`(,psychic  ,steel   ) . ,not-very-effective)
                        (,`(,bug      ,fire    ) . ,not-very-effective)
                        (,`(,bug      ,grass   ) . ,super-effective   )
                        (,`(,bug      ,fighting) . ,not-very-effective)
                        (,`(,bug      ,poison  ) . ,not-very-effective)
                        (,`(,bug      ,flying  ) . ,not-very-effective)
                        (,`(,bug      ,psychic ) . ,super-effective   )
                        (,`(,bug      ,ghost   ) . ,not-very-effective)
                        (,`(,bug      ,dark    ) . ,super-effective   )
                        (,`(,bug      ,steel   ) . ,not-very-effective)
                        (,`(,rock     ,fire    ) . ,super-effective   )
                        (,`(,rock     ,ice     ) . ,super-effective   )
                        (,`(,rock     ,fighting) . ,not-very-effective)
                        (,`(,rock     ,ground  ) . ,not-very-effective)
                        (,`(,rock     ,flying  ) . ,super-effective   )
                        (,`(,rock     ,bug     ) . ,super-effective   )
                        (,`(,rock     ,steel   ) . ,not-very-effective)
                        (,`(,ghost    ,normal  ) . ,no-effect         )
                        (,`(,ghost    ,psychic ) . ,super-effective   )
                        (,`(,ghost    ,dark    ) . ,not-very-effective)
                        (,`(,ghost    ,steel   ) . ,not-very-effective)
                        (,`(,ghost    ,ghost   ) . ,super-effective   )
                        (,`(,dragon   ,dragon  ) . ,super-effective   )
                        (,`(,dragon   ,steel   ) . ,not-very-effective)
                        (,`(,dark     ,fighting) . ,not-very-effective)
                        (,`(,dark     ,psychic ) . ,super-effective   )
                        (,`(,dark     ,ghost   ) . ,super-effective   )
                        (,`(,dark     ,dark    ) . ,not-very-effective)
                        (,`(,dark     ,steel   ) . ,not-very-effective)
                        (,`(,steel    ,fire    ) . ,not-very-effective)
                        (,`(,steel    ,water   ) . ,not-very-effective)
                        (,`(,steel    ,electric) . ,not-very-effective)
                        (,`(,steel    ,ice     ) . ,super-effective   )
                        (,`(,steel    ,rock    ) . ,super-effective   )
                        (,`(,steel    ,steel   ) . ,not-very-effective)
                        )
         ) ;; effectiveness/eval
       )

  (defun type-get-match (typelist)
    ;; handles getting type weaknesses/resistances/immunities
    (let ((test (assoc typelist effectiveness :test #'equal)))
      (if (not test) 1 (cdr test))) ;; return 1 if no match, else the value
    )
  ;; (format t "~A~%" effectiveness) ;; print raw chart of matchups


  ;; Get user inputs. What do we want to match up?
  (defun user-input-type-name (&optional query option)
    (clear-input)
    (write-string query)
    (finish-output)
    (string-upcase (read-line)) ;; return uppercased input
    )
  (defun against-two-types (move target1 target2)
    (let* ( ;; needs more function breakout
           (move-type (cdr (assoc move typematch :test #'equal)))
           (target-type-1 (cdr (assoc target1 typematch :test #'equal)))
           (target-type-2 (cdr (assoc target2 typematch :test #'equal)))
           (against-type-1 (type-get-match (list move-type target-type-1)))
           (against-type-2 (type-get-match (list move-type target-type-2)))
           )
      (* against-type-1 against-type-2) ;; return combined result
      )
    )
  (let* (
         (move-type (user-input-type-name "Enter the move's type: "))
         (target-type1 (user-input-type-name "Enter the target's primary type: "))
         (target-type2 (user-input-type-name "Enter the target's secondary type (optional): "))
         )
    
    (if (> (length target-type2) 0)  ;;if type 2 is not an empty string
        (format t "~A-type move on ~A/~A-type target:~%~2$x damage.~%" (string-downcase move-type) (string-downcase target-type1) (string-downcase target-type2) (against-two-types move-type target-type1 target-type2))
        ;; else, it's easier. do one type comparison and print results
        (format t "~A-type move on ~A-type target:~%~2$x damage.~%" (string-downcase move-type) (string-downcase target-type1) (type-get-match (list (cdr(assoc move-type typematch :test #'equal)) (cdr(assoc target-type1 typematch :test #'equal)))))
        ) ;; end if
    )
  ;;(format t "fire vs water ~2$~%" (type-get-match `(,fire ,water)))
  ;;(format t "water vs fire ~2$~%" (type-get-match `(,water ,fire)))
  ) ;; let definitions end
;;(quit)
;;)

;;(EXT:SAVEINITMEM "types-clisp" :QUIET t :EXECUTABLE t :init-function #'main)
;;(sb-ext:save-lisp-and-die "types" :toplevel #'main :executable t)
