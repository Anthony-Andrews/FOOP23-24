#lang simply-scheme

; http://www.eecs.berkeley.edu/~bh/ssch9/bridge.html

; Bridge Hands Scoring Project
; Authors: Josh Paley, Takeshi Kaneko
; Last updated: 10/03/22 [tk]
; Student Coders:
; Anthony Andrews
;  with help from Peter Jiang.

; Remember to generously comment your code

; When dealing with bridge hands, we really want to be talking about
; cards and their ranks and suits. If you asked a friend what the
; butfirst of a card was, they would think you were crazy. So we will
; create an abstraction for cards that allows us to think in terms of
; cards, ranks, and suits, and we will then use those functions.

; function to take in a card and determine it's individual value.
(define (card-val card)
    (cond
        ((equal? (bf card) 'a)  4) ; ace is worth 4
        ((equal? (bf card) 'k)  3) ; king is worth 3
        ((equal? (bf card) 'q)  2) ; queen is worth 2
        ((equal? (bf card) 'j)  1) ; jack is worth 1
        (else 0)                    ; otherwise, it's 2-10 and it's worth 0
    )
)

(define (high-card-points hand)
    (accumulate + (every card-val hand)) ; recursively add the value of each card in a given hand
)

(define (count-suit suit hand) ; return the number of cards with a given suit in a given hand.
    (length (keep 
                (lambda(card) (equal? suit (first card)))
            hand)
    )
)

(define (suit-counts hand) ; return how many cards of each suit is in a given hand in the order spade, heart, club, diamond
    (sentence
        (count-suit 's hand)
        (count-suit 'h hand)
        (count-suit 'c hand)
        (count-suit 'd hand)
    )
)

(define (suit-dist-points num) ; calculate the distribution points given the number of any one given suit
    (cond
        ((equal? num 0) 3)
        ((equal? num 1) 2)
        ((equal? num 2) 1)
        (else 0)
    )
)

(define (hand-dist-points hand) ; calculate the total number of distribution points in a hand
    (accumulate +
        (every suit-dist-points
            (sentence
                (count-suit 's hand)
                (count-suit 'h hand)
                (count-suit 'c hand)
                (count-suit 'd hand)
            )
        )
    )
)

(define (bridge-val bridge) ; calculate the total brdige hand valule based on distribution points and card points.
(+ (hand-dist-points bridge) (high-card-points bridge) )
)

;(define (in-hand? card hand)
;  (cond
;  ((equal? (length hand) 1) (equal? (sentence card) hand))
;  (else (sentence (equal? card (first hand) (in-hand? card (bf hand)))))
;  )
;)

;(define (in-hand? card hand)
;    (keep (lambda(x)(equal? card x)) hand)
;)

(define (duplicates? hand)
    (if (equal? 1 (length hand)) 
        #f
        (or (member? (first hand) (bf hand)) (duplicates? (bf hand)))
    )
)

(define (duplicates hand)
    (if (equal? 1 (length hand)) 
        #f
        (if (or (member? (first hand) (bf hand)) (duplicates? (bf hand)))
            (first hand)
            'none ; this condition should never be reached under normal circumstances.
        )
    )
)

(define (is-card? card)
    (if (member? (first card) '(c d h s)) 
        (if (member? (bf card) '(a 2 3 4 5 6 7 8 9 10 j q k))
            #t 
            #f
        )
        #f
    )
)

(define (card-suit card)
    (cond
        ((equal? (first card) 'c ) '(of clubs) )
        ((equal? (first card) 'd ) '(of diamonds) )
        ((equal? (first card) 'h ) '(of houses) )
        ((equal? (first card) 's ) '(of spades) )
    )
)

(define (card-name card)
    (cond
        ((equal? (bf card) 'a ) (sentence 'ace (card-suit card) ))
        ((equal? (bf card) '2 ) (sentence '2 (card-suit card) ))
        ((equal? (bf card) '3 ) (sentence '3 (card-suit card) ))
        ((equal? (bf card) '4 ) (sentence '4 (card-suit card) ))
        ((equal? (bf card) '5 ) (sentence '5 (card-suit card) ))
        ((equal? (bf card) '6 ) (sentence '6 (card-suit card) ))
        ((equal? (bf card) '7 ) (sentence '7 (card-suit card) ))
        ((equal? (bf card) '8 ) (sentence '8 (card-suit card) ))
        ((equal? (bf card) '9 ) (sentence '9 (card-suit card) ))
        ((equal? (bf card) '10 ) (sentence '10 (card-suit card) ))
        ((equal? (bf card) 'j ) (sentence 'jack (card-suit card) ))
        ((equal? (bf card) 'q ) (sentence 'queen (card-suit card) ))
        ((equal? (bf card) 'k ) (sentence 'king (card-suit card) ))
    )
)

(define (invalid-card hand)
    (if (is-card? (first hand))
        (invalid-card (bf hand))
        (first hand)
    )
)

(define (hand-error hand)
    (if (equal? (length (keep is-card? hand) )13 )
        (if (duplicates? hand)
            (sentence '(Invalid hand. There are more than one) (card-name (duplicates hand)))
            '(Hand is valid.)
        )
        (if (equal? (length hand) 13 )
            (sentence '(Invalid hand.) (invalid-card hand) '(is not a valid card.))
            '(Invalid hand. A valid bridge hand has 13 cards.)
    ))
)
