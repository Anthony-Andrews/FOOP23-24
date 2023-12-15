#lang simply-scheme

; Scoring Poker Hands

; Your name: Anthony Andrews 

; Starter code last updated: 11/30/22 by Takeshi Kaneko

; INSTRUCTIONS:
; (1) Read through the entire collection of starter code.
; (2) Insert your code everywhere you find YOUR_CODE_HERE.
; (3) Add comments where additional clarity is needed.

; ************************************************************
; USEFUL CONSTANTS
; ************************************************************

(define ranks '(2 3 4 5 6 7 8 9 10 j q k a))
(define suits '(s h d c)) ; spades, hearts, diamonds, clubs
(define ranknames '(ace two three four five six seven
                        eight nine ten jack queen king ace)) ; this is important (ace can be 1 and 14 ARGH)

; ************************************************************
; UTILITY HELPER FUNCTIONS
; ************************************************************

; suit-name takes in a suit as a member of suits, and
;           returns the name of the suit
(define (suit-name suit)
  (cond ((equal? suit 's) 'spades)
        ((equal? suit 'h) 'hearts)
        ((equal? suit 'd) 'diamonds)
        ((equal? suit 'c) 'clubs)
        (else 'not-a-suit)))                 ; added for debugging

; rank-name takes in a rank as a member of ranks, and
;           returns the name of the rank
(define (rank-name rank)
  (item (numeric-rank rank) ranknames))

; pluralize takes in a rank name, and
;           returns the plural of the name
(define (pluralize r)
  (word r (cond ((member? r '(six)) 'es) ; six is the only time 's' (sixs) is not the correct plural ending, instead add 'es' (sixes).
                (else 's))))

; numeric-rank takes in a rank (or card), and
;              returns the corresponding number rank
(define (numeric-rank rank-or-card)
  (let ((rank (if (member? rank-or-card ranks)
                  rank-or-card
                  (rank rank-or-card)))) ; extracts the rank if it's a card using a helper function
    (cond ((equal? rank 'a) 14)
          ((equal? rank 'k) 13)
          ((equal? rank 'q) 12)
          ((equal? rank 'j) 11)
          (else rank))))

; rank extracts the rank of a single card using bf (helper function for the afformentioned function above ^^)
(define (rank card) (bf card))

; ranks-only takes in a hand, and
;            returns the ranks of all cards in a hand
(define (ranks-only hand)
    (every (lambda(x) (rank x)) hand) ; lambda is redundant here but allows debugging and changes.
)

; suit extracts the suit of a single card using first
(define (suit card) (first card))

; second takes in a word or sentence, and
;        returns the second part of the input when possible and
;        the input otherwise
(define (second thing)
  (if (> (count thing) 1)
      (first (bf thing))
      thing))

; third takes in a word or sentence, and
;       returns the third part of the input when possible and
;       the last of the input otherwise
(define (third thing)
  (if (> (count thing) 2)
      (first ((repeated bf 2) thing))
      (last thing)))

; fourth takes in a word or sentence, and
;        returns the fourth part of the input when possible and
;        the last of the input otherwise
(define (fourth thing)
  (if (> (count thing) 3)
      (first ((repeated bf 3) thing))
      (last thing)))

; sort takes in a hand, and
;      returns a hand completely sorted by rank
(define (sort hand)
;    (show 'sort)
  ( (repeated sort-once (- (count hand) 1)) hand))

; sort-once takes in a hand, and
;           returns the hand sorted once
(define (sort-once hand)
  (cond ((empty? hand) hand)
        ((= (count hand) 1) hand)
        ((> (numeric-rank (first hand))
            (numeric-rank (second hand)))
         (se (first hand) (sort-once (bf hand))))
        (else (se (second hand)
                  (sort-once (se (first hand) (bf (bf hand))))))))

; ************************************************************
; PREDICATES TO DETERMINE THE TYPE OF HAND
; ************************************************************

;equal-next-rank? is a helper function that takes in a hand and a place in the hand and checks if the card there is equal to the following +1 card.
(define (equal-next-rank? hand x)
;    (show (se 'en? hand '- x))
;    (show (equal? (rank (item x hand)) (rank (item (+ 1 x) hand)))) ; these two (show) lines were very helpful for debugging
    (equal? (rank (item x hand)) (rank (item (+ 1 x) hand)))
)

;same as (equal-next-rank) but check suit rather than rank
(define (equal-next-suit? hand x)
    (equal? (suit (item x hand)) (suit (item (+ 1 x) hand)))
)

; NOTE: ALL the following functions assume the hand is sorted. <<<< would have been nice to know they are sorted in descending hour.
; flush? returns #t when all suits are the same and
;                #f otherwise

;flush? will recursively check if the first card has the same suit as the second card until it either is false, or reaches the base case of 1 card. (whichever comes first)
(define (flush? hand)
;    (show 'flush?) ; verbose debugging
    (if (equal? (count hand) 1 )
        #t
        (if (equal-next-suit? hand 1)
            (flush? (bf hand))
            #f
        )
    )
)

; straight? returns #t when all ranks are in sequence and
;                   #f otherwise
; NOTE: suit is not considered
;       assumes a 5-card hand
; IDEA: include a check for (not (flush? hand))

; straight? has one base cases and a special case. It will recursively check if the second card is one greator than the first until the base case is reached at which point it is known the hand is straight.
(define (straight? hand)
;    (show 'straight?) ;verbose debugging
    (cond 
        ((equal? '(a 5 4 3 2) (every rank hand)) #t) ; special case where (once sorted) hand can be "ace five four three two" due to ace straight.
        ((equal? (count hand) 1 ) #t)
        ((equal? (numeric-rank (item 1 hand)) (+ 1 (numeric-rank (item 2 hand)))) (straight? (bf hand)))
        (else #f)
    )
)

; straight-flush? returns #t when the hand is a straight,
;                                             a flush, and
;                                             second-high card is not king
;                         #f otherwise

; checks if all three conditionals listed above are met.
(define (straight-flush? hand)
;    (show 'straight-flush?) ; debugging you know the drill by now.
    (if (and
            (straight? hand)
            (flush? hand)
            (not (equal? 'k (rank (item 2 hand))))
        )
        #t
        #f
    )
)

; royal-flush? returns #t when the hand is a straight,
;                                          a flush, and
;                                          second-high card is king
;                      #f otherwise

; checks if all three conditionals listed above are met.
(define (royal-flush? hand)
;    (show 'royal-flush?) ; debugging
    (if (and 
            (straight? hand) 
            (flush? hand) 
            (equal? 'k (rank (item 2 hand)))
        )
        #t
        #f
    )
)

; pair? takes a sorted hand as its input and
;       returns #t if there exists exactly one pair and
;               #f otherwise
; NOTE: must assume higher-value hands have been eliminated

; recursively checks if a the first card in the hand is equal to the second. shortens the hand until it is either true or false.
(define (pair? hand)
;    (show (se 'pair? hand))
    (if (equal? (count hand) 1 ) 
        #f
        (if (equal-next-rank? hand 1)
            #t 
            (pair? (bf hand))
        )
    )
)

; two-pair? takes a sorted hand as its input and
;           returns #t if there exists exactly two pairs and
;                   #f otherwise
; NOTE: must assume higher-value hands have been eliminated

; two-pair? first checks if the card is 3 cards long; if it is, it cannot contain two-pairs
; secondly, much like pair?, it seraches for a pair using recursion. If a pair is found it can pass the hand (minus the first pair) to the pair? function to find the second pair.
(define (two-pair? hand)
;    (show 'two-pair)
    (if (equal? (count hand) 3 ) 
        #f
        (if (equal-next-rank? hand 1)
            (pair? (bf (bf hand)))
            (two-pair? (bf hand))
        )
    )
)

; three-of-a-kind? takes a sorted hand as its input and
;                  returns #t if there exists exactly one trio and
;                          #f otherwise
; NOTE: must assume higher-value hands have been eliminated

; three-of-a-kind? checks if the hand is two cards long as the base case. It then checks for 3 like cards in a row and recursively shortens the hand until the base condition is met.
(define (three-of-a-kind? hand)
;    (show 'three-of-a-kind?)
    (if (equal? (count hand) 2 ) 
        #f
        (if (equal-next-rank? hand 1)
            (if (equal-next-rank? hand 2)
                #t
                (three-of-a-kind? (bf hand))
            )
            (three-of-a-kind? (bf hand))
        )
    )
)

; full-house? takes a sorted hand as its input and
;             returns #t if there exists exactly one trio and and one pair and
;                     #f otherwise
; NOTE: must assume higher-value hands have been eliminated

; This is where things got full out lazy and since there are only two possible five card hands that can be a full-house we check for either.
; The two possible cases are XXXxx and xxXXX.
(define (full-house? hand)
;    (show 'full-house?)
    (or
        (and 
            (equal-next-rank? hand 1)
            (equal-next-rank? hand 2)
            (equal-next-rank? hand 4)
            (not (equal-next-rank? hand 3)) ; this is not needed to pass the test code but conforms to official definitions of a full house.
        )

        (and 
            (equal-next-rank? hand 1)
            (equal-next-rank? hand 3)
            (equal-next-rank? hand 4)
            (not (equal-next-rank? hand 2)) ; this is not needed to pass the test code but conforms to official definitions of a full house.
        )
    )
)

; four-of-a-kind? takes a sorted hand as its input and
;                 returns #t if there exists four of a kind and
;                        #f otherwise

; Much like full-house? there are only two possible combinations. As such we check for either.
; The two possible cases are XXXXx and xXXXX.
(define (four-of-a-kind? hand)
;   (show 'four-of-a-kind?)
    (or
        (and 
            (equal-next-rank? hand 1)
            (equal-next-rank? hand 2)
            (equal-next-rank? hand 3)
            (not (equal-next-rank? hand 4)) ; this is not needed to pass the test code but conforms to official definitions of four of a kind.
        )

        (and 
            (equal-next-rank? hand 2)
            (equal-next-rank? hand 3)
            (equal-next-rank? hand 4)
            (not (equal-next-rank? hand 1)) ; this is not needed to pass the test code but conforms to official definitions of four of a kind.
        )
    )
)

; ************************************************************
; OUTPUT FUNCTIONS THAT DESCRIBE THE TYPE OF HAND
; ************************************************************

; All of these non predicate functions follow the descriptions above. Comments are given to noteworthy code.

; plural-card is a helper function that takes a rank and give the pluralized version of it's rank name. Useful.
(define (plural-card card)
    (pluralize (rank-name (rank card)))
)

; royal-flush takes a royal flush as an input and returns
;             the type of royal-flush in the form
;             "royal flush - [suit]"
; NOTE: royal-flush is a flush, a straight, and high card is an ace

; Takes the suit name from the first item in a hand then concatenates it into a sentence.
(define (royal-flush hand)
    (se
        '(royal flush -) 
        (suit-name (suit (item 1 hand)))
    )
)

; straight-flush takes a straight flush as an input and returns
;                the type of straight flush in the form
;                "[rank]-high straight flush - [suit]"
; NOTE: straight-flush is a flush, a straight, and
;       high card is not an ace
(define (straight-flush hand)
    (se
        (if (equal? (rank (item 1 hand)) 'a)     ; NOTE: this checks for the exception where ace is first, I spent far to long realizing this is needed.
            (word (rank-name (item 2 hand)) '-high) ; If ace is the first card in the hand, take the rank of the second highest card.
            (word (rank-name (item 1 hand)) '-high) ; Likewise, if it's not, take the rank of the first (highest) card.
        ) 
        '(straight flush -) (suit-name (suit (item 1 hand))) ; Put it into a sentence with the suit taken from the first card.
    )
)

; flush takes a flush as an input and returns
;       the type of flush in the form
;       "flush - [suit]"

; Takes the suit from the first card in a hand; concatenates it into a sentence.
(define (flush hand)
;    (show (sentence 'flush hand)) ; debugging
    (se '(flush -) (suit-name (suit (item 1 hand))))
)

; straight takes a straight as an input and returns
;          the type of straight in the form
;          "[rank]-high straight"
(define (straight hand)
    (se
        (if (equal? (rank (item 1 hand)) 'a)        ; Once again, much like straight-flush we are checking if the first card in the hand is an ace.
            (word (rank-name (item 2 hand)) '-high) 
            (word (rank-name (item 1 hand)) '-high) 
        ) 
        'straight
    )
)

; pair takes a pair as an input and returns
;      the type of pair in the form
;      "pair of [ranks]"
; NOTE: returns #f if no pair exists

(define (pair hand)
;    (show (se 'pair hand)) ; Only place where the debugging is left uncommented because I was banging my head against a desk for hours on this one... This honestly might be an oversight in the testing program im not sure.
    (if (equal? (item 1 hand) (item 2 hand)) ; IMPORTANT NOTE : (equal-next-rank?) cannot be used here as it expects card format not number format.
            (se '(pair of ) (pluralize (rank-name (item 1 hand)))) ; same goes for not using (plural-card) since it expects card format.
            (pair (bf hand))
    )
)

; three-of-a-kind takes three-of-a-kind as an input and returns
;                 the type of three-of-a-kind in the form
;                 "three [ranks]"
; NOTE: returns #f if three-of-a-kind does not exist

; For three-of-a-kind , there are only 3 possible cases: xxXXX xXXXx or XXXxx. In all of them the middle card (item 3) is part of the three of a kind. As such we take the rank from it.
(define (three-of-a-kind rhand hand)
        (se 'three (plural-card (item 3 hand)))
)

; full-house takes a full house as an input and returns
;            the type of full house in the form
;            "full house - [ranks] full of [ranks]"
;            "full house - [bigger rank] full of [smaller rank]" - better comment

; There are only two possible hands in which full-house is true, they are XXXxx or xxXXX. 
; As such we check if the second card is equal to the third. This tells us which of the two possibilities it is, then we know which card is the smaller group of ranks and larger.
(define (full-house rhand hand)
    (if (equal-next-rank? hand 2)
        (se '(full house -) (plural-card (item 1 hand)) '(full of) (plural-card (item 5 hand))) ; XXXxx condition
        (se '(full house -) (plural-card (item 5 hand)) '(full of) (plural-card (item 1 hand))) ; xxXXX condition
    )
)

; four-of-a-kind takes four-of-a-kind as an input and returns
;                the type of four-of-a-kind in the form
;                "four [ranks]"
; NOTE: returns #f if no four-of-a-kind exists

; In all two cases of four-of-a-kind (xXXXX & XXXXx) the second card will contain the rank of the kind.
(define (four-of-a-kind rhand hand)
    (se 'four (plural-card (item 2 hand)))
)

; two-pair takes two-pair as an input and returns
;          the type of two-pair in the form
;          "two pair - [ranks] and [ranks]"
; NOTE: returns #f if two-pair does not exist

; Two-pair has three total possible combinations such are:
; XXYYx XXxYY xXXYY
; As such we just check for the first two and get the corresponding ranks. 
;This could all be acomplished in recusion, but it would use more lines and be more computationally intensive in both processing and memory.
(define (two-pair rhand hand) ; rhand is called by the test code here but not used ¯\_(ツ)_/¯
    (cond 
        ((and (equal-next-rank? hand 1) (equal-next-rank? hand 3))
            (se '(two pair - ) (plural-card (item 1 hand)) 'and (plural-card (item 3 hand)))
        )
        ((and (equal-next-rank? hand 1) (equal-next-rank? hand 4))
            (se '(two pair - ) (plural-card (item 1 hand)) 'and (plural-card (item 4 hand)))
        )
        (else 
            (se '(two pair - ) (plural-card (item 2 hand)) 'and (plural-card (item 5 hand)))
        )
    )
)
; high-card takes in a hand and returns
;           the high card rank in the form
;           "[rank] high"

; Only 1 case, we get the rank from the first card.
(define (high-card hand)
        (se (rank-name (item 1 hand)) 'high)
)
; Hooray it's over and works! Thanks for taking the time to read these comments :D

; ************************************************************
; EXTENSION CODE
; ************************************************************

; " In some versions of poker, each player gets seven cards and can choose any five of the seven to make a hand. With seven cards, you have to pick the best category that can be made from your cards.  
; Write a procedure poker-hand-7 that takes in a hand of seven cards and returns the highest-value five-card hand with the hand's value."

(define r 5) ; number of items in final combination - this represents a five card hand. For a 5 card hand out of a 7 card pool, there are 21 possible permutations.

(define checks (list royal-flush? straight-flush? four-of-a-kind? full-house? flush? straight? three-of-a-kind? two-pair? pair?)) ; lists all the predicates that must be checked in order from highest to lowest to ensure that the highest hand is being found.

; this function is a bit of a doosy. be iterrating over r and acc it can generate all possible combinations of a given list. The possible permutations is derived using the combination formula. Here we are doing something similar.
(define (generate-combinations base-list) ; this behaves the same as a seperate caller function except its just wrapped around the actual function.
    (define (combinations-helper lst r acc)
        (cond
            ((= r 0) (list acc))                  ; Base case: return a list containing the accumulated combination
            ((null? lst) '())                    ; Base case: return an empty list if the input list is empty
            (else
                (append (combinations-helper (bf lst) (- r 1) (cons (car lst) acc)) ; here append and cons are behaving like sentence, and car is behvaing like first. The reason which they are required in this instance is because we are using nested lists and sentence and first do not allow nested sentences.
                (combinations-helper (bf lst) r acc))
            )
        )
    )
    (combinations-helper base-list r '()) ; This invocates the start of the recusive function using starting parameters.
)

; This function is suprisingly similar in structure to the previous. It recusively checks each of the previous generated possible permutations for royal flush, then straight flush, etc, etc as stated by (define checks)
; Although it's meant for 7 card hands, this function will work for any number of cards!

(define (best-hand unsorted)
    (let ((sorted-combos  (map sort (generate-combinations unsorted))  )) ; its very important that (map) is used here instead of every since every will flatten the nested sentences passed in. (map) does not do this.
        (define (rank-search check combos)
            ;(show combos)
            ;(show (se '(failed predicate check) (- 10 (length check)))) ; very useful verbose debugging to see which predicate check the function fails on.
            (cond

                ((null? check) (car combos)) ; all predicate test failed, this is the high card.
                ((null? combos) (rank-search (cdr check) sorted-combos) ) ; case where predicate (first being royal-flush?) is checked against all hands and all are false.

                ((apply (car check) (list (car combos))) ; first check with first hand NOTE: apply and list are needed here for syntactical reasons. Otherwise it would be impossible to apply a list of functions to a singular list.
                    ;(show combos) debugging verbosity.
                    ;(show check)
                    (car combos) ; found best hand case:
                )

                (else (rank-search check (cdr combos)))
            )
            ;(show check-hand-helper)
        )
        (rank-search checks sorted-combos)
    )
)

(define (poker-hand-7 hand)
    (let ((best (best-hand hand)))
        (se '(The best possible hand is:) best '- (poker-value best)) ; helper function to just add some text.
    )
)
; (poker-hand-7 '(sa s5 sk cj c4 c5 c6 ha h4 h6 h3 h4 h8 h2 h2))
; '(The best possible hand is: sa ha h4 c4 h4 - full house - fours full of aces)

; pretty proud of this one :D

; ************************************************************
; MAIN PROGRAM
; ************************************************************

; poker-value takes in a hand, sorts the hand, and returns
;             the type of the hand
; NOTE:  the types with all unique ranks are tested first,
;        then the types with duplicate ranks are tested
(define (poker-value hand)
  (let ((sortedhand (sort hand)))
    (cond ((royal-flush? sortedhand) (royal-flush sortedhand))
          ((straight-flush? sortedhand) (straight-flush sortedhand))
          ((flush? sortedhand) (flush sortedhand))
          ((straight? sortedhand) (straight sortedhand))
          (else (let ((handranks (ranks-only sortedhand)))
                  (cond ((four-of-a-kind? sortedhand) (four-of-a-kind handranks
                                                                      sortedhand))
                        ((full-house? sortedhand) (full-house handranks
                                                              sortedhand))
                        ((three-of-a-kind? sortedhand) (three-of-a-kind handranks
                                                                        sortedhand))
                        ((two-pair? sortedhand) (two-pair handranks
                                                          sortedhand))
                        ((pair? sortedhand) (pair handranks))
                        (else (high-card sortedhand))))))))

; ************************************************************
; TESTING CODE
; ************************************************************

; CAUTION: The following code utilizes lists (Ch. 17), so it is
;          fully acceptable if you do not yet understand what
;          it is doing.  After Ch. 17, revisting this section
;          can serve as a good gauge of how well you understand
;          lists.

; The following is a collection of 21 test-hand pairs in the form
; '[hand hand-description])
(define rf '[(ha hk h10 hj hq) (royal flush - hearts)]) ; royal flush
(define sf '[(h9 hk h10 hj hq) (king-high straight flush - hearts)]) ; straight flush
(define sf5 '[(d2 d5 d3 da d4) (five-high straight flush - diamonds)]) ; 5-high straight flush
(define fourXxxxx '[(d3 h3 h10 s3 c3) (four threes)]) ; four of a kind
(define fourxxxxX '[(hj dj c2 sj cj) (four jacks)]) ; four of a kind
(define fhxxxyy '[(c3 ha sa h3 ca) (full house - aces full of threes)]) ; full house
(define fhxxyyy '[(ca ha s3 h3 c3) (full house - threes full of aces)]) ; full house
(define f '[(d2 d5 d10 dj da) (flush - diamonds)]) ; flush
(define str '[(dq h9 h10 hj h8) (queen-high straight)]) ; straight
(define str5 '[(d4 c5 ca c2 c3) (five-high straight)]) ; 5-high straight
(define threexxxXX '[(c6 d6 h5 h6 h3) (three sixes)]) ; three of a kind
(define threeXxxxX '[(c5 d5 h5 h6 h3) (three fives)]) ; three of a kind
(define threeXXxxx '[(c3 d3 h5 h6 h3) (three threes)]) ; three of a kind
(define twopxxyyX '[(d10 h3 h10 h2 d3) (two pair - tens and threes)]) ; two pair
(define twopxxXyy '[(d10 h3 h10 h2 d2) (two pair - tens and twos)]) ; two pair
(define twopXxxyy '[(d3 h3 h10 h2 d2) (two pair - threes and twos)]) ; two pair
(define pxxXXX '[(d3 h9 h10 hj dj) (pair of jacks)]) ; pair
(define pXxxXX '[(d3 h9 h10 hj d10) (pair of tens)]) ; pair
(define pXXxxX '[(d3 h9 h10 hj d9) (pair of nines)]) ; pair
(define pXXXxx '[(d3 h9 h10 h3 dj) (pair of threes)]) ; pair
(define high '[(ca h2 d3 s4 sk) (ace high)]) ; high card

; all-test-hands is a list containing the 21 test-hand pairs
(define all-test-hands (list rf sf sf5 fourXxxxx fourxxxxX fhxxxyy fhxxyyy
                             f str str5 threexxxXX threeXxxxX threeXXxxx
                             twopxxyyX twopxxXyy twopXxxyy
                             pxxXXX pXxxXX pXXxxX pXXXxx high))

; Special testing code for a royal flush
(let [(hand (sort (car rf)))]
; NOTE: Each of the following five lines is designed to work
;       independently.  Uncomment these lines ONE-at-a-time.
  (append '(royal flush test hand is) (list hand)))
;  (cons hand (append '(is) (if (straight? hand) '() '(not)) '(a straight))))
;  (cons hand (append '(is) (if (flush? hand) '() '(not)) '(a flush))))
;  (cons hand (append '(is) (if (straight-flush? hand) '() '(not)) '(a straight flush))))
;  (cons hand (append '(is) (if (royal-flush? hand) '() '(not)) '(a royal flush))))
; NOTE: Once all five tests have passed, comment all Special
;       testing code for a royal flush and proceed to the
;       last line below.

; test takes in a hand and a description as a list, and returns
;      a list of the form
;      '(hand #t) when the hand and description match, and
;      '(hand [error] #f) when the hand and description do not match.
; NOTE: The actual sentence describing the hand MUST match what
;       you chose to use in your code.
(define (test testhand)
  (let* [(hand (car testhand))
         (val (poker-value hand))
         (exp-val (car (cdr testhand)))]
    (cons hand
          (if (equal? val exp-val)
              '(#t)
              (list (list 'actual: val 'expected: exp-val)
                    #f)))))

; tests stores the results of the above tests in a list.
; EXAMPLE: if poker-value is successful, tests will look similar to
;          '(([hand] #t) (([hand]) #t))
;          if poker-value is not working, tests will contain #f's for
;          each hand value that is not detected properly, similar to
;          '(([hand] #f) ([hand] #f))
(define tests (map test all-test-hands))

; test-results returns the contents of tests with 'FAIL when
;                      at least one test fails
;                      and 'PASS when all tests are successful!
(define (test-results)
  (let [(errors (filter (lambda (x) (not (last x)))
                        tests))]
    (if (null? errors)
        'PASS
        (list tests 'FAIL))))

; THIS INVOCATION CONTROLS ALL TESTS
; NOTE: Uncomment this ONLY when you are done testing for
;       a royal flush above.
(test-results)



; ============= MORE TEST CODE ============
;
; input:
;   (poker-value '(h4 s4 c6 s6 c4))
; expected output:
;   '(full house - fours full of sixes)
;
;
;
; input:
;   (poker-value '(h7 s3 c5 c4 d6))
; expected otuput:
;   '(seven-high straight)
;
;
;
; input:
;   (poker-value '(dq d10 dj da dk))
; expected output:
;   '(royal flush - diamonds)
;
;
;
; input:
;   (poker-value '(da d6 d3 c9 h6))
; expected output:
;   '(pair of sixes)
;
; input:
;   (poker-hand-7 '(sa h3 d7 sk sq sj s10))
; expected output:
;   '(The best possible hand is: sa sk sq sj s10 - royal flush - spades)
;
; input:
;   (poker-hand-7 '(sa s5 sk cj c4 c5 c6 ha h4 h6 h3 h4 h8 h2 h2))
; expected output:
;   '(The best possible hand is: sa ha h4 c4 h4 - full house - fours full of aces)
;=============================================
