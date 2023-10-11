#lang simply-scheme

(define (ascii obj)
    (cond
        ( (equal? obj 'rock)
            (string-append
"
██████╗  ██████╗  ██████╗██╗  ██╗
██╔══██╗██╔═══██╗██╔════╝██║ ██╔╝
██████╔╝██║   ██║██║     █████╔╝ 
██╔══██╗██║   ██║██║     ██╔═██╗ 
██║  ██║╚██████╔╝╚██████╗██║  ██╗
╚═╝  ╚═╝ ╚═════╝  ╚═════╝╚═╝  ╚═╝
")
        )

        ( (equal? obj 'paper)
            (string-append
"
█▀█ ▄▀█ █▀█ █▀▀ █▀█
█▀▀ █▀█ █▀▀ ██▄ █▀▄
")
        )
            
        ( (equal? obj 'scissors)
            (string-append
"
 _______  _______  ___   _______  _______  _______  ______    _______ 
|       ||       ||   | |       ||       ||       ||    _ |  |       |
|  _____||       ||   | |  _____||  _____||   _   ||   | ||  |  _____|
| |_____ |       ||   | | |_____ | |_____ |  | |  ||   |_||_ | |_____ 
|_____  ||      _||   | |_____  ||_____  ||  |_|  ||    __  ||_____  |
 _____| ||     |_ |   |  _____| | _____| ||       ||   |  | | _____| |
|_______||_______||___| |_______||_______||_______||___|  |_||_______|
")
        )

        ( (equal? obj 'lizzard)
            (string-append
"
 ██▓     ██▓▒███████▒▒███████▒ ▄▄▄       ██▀███  ▓█████▄ 
▓██▒    ▓██▒▒ ▒ ▒ ▄▀░▒ ▒ ▒ ▄▀░▒████▄    ▓██ ▒ ██▒▒██▀ ██▌
▒██░    ▒██▒░ ▒ ▄▀▒░ ░ ▒ ▄▀▒░ ▒██  ▀█▄  ▓██ ░▄█ ▒░██   █▌
▒██░    ░██░  ▄▀▒   ░  ▄▀▒   ░░██▄▄▄▄██ ▒██▀▀█▄  ░▓█▄   ▌
░██████▒░██░▒███████▒▒███████▒ ▓█   ▓██▒░██▓ ▒██▒░▒████▓ 
░ ▒░▓  ░░▓  ░▒▒ ▓░▒░▒░▒▒ ▓░▒░▒ ▒▒   ▓▒█░░ ▒▓ ░▒▓░ ▒▒▓  ▒ 
░ ░ ▒  ░ ▒ ░░░▒ ▒ ░ ▒░░▒ ▒ ░ ▒  ▒   ▒▒ ░  ░▒ ░ ▒░ ░ ▒  ▒ 
  ░ ░    ▒ ░░ ░ ░ ░ ░░ ░ ░ ░ ░  ░   ▒     ░░   ░  ░ ░  ░ 
    ░  ░ ░    ░ ░      ░ ░          ░  ░   ░        ░    
            ░        ░                            ░      
")
        )

        ( (equal? obj 'spock)
            (string-append
"
.d8888. d8888b.  .d88b.   .o88b. db   dD 
88'  YP 88  `8D .8P  Y8. d8P  Y8 88 ,8P' 
`8bo.   88oodD' 88    88 8P      88,8P   
  `Y8b. 88~~~   88    88 8b      88`8b   
db   8D 88      `8b  d8' Y8b  d8 88 `88. 
`8888Y' 88       `Y88P'   `Y88P' YP   YD                                                      
")
        )
    )
)

; rpsls takes in a human_throw in the form of an argument and prints whether or not the human wins or loses.
(define (rpsls human_throw comp_throw)
    (cond 

        ( (not (member? human_throw '(rock paper scissors lizzard spock ))) ; check for invalid input
            '(That was not a valid input!)
        )

        ( (equal? human_throw comp_throw) ; check for tie
            (sentence human_throw 'tied comp_throw)
        )

        ( (equal? (and human_throw 'scissors) (equal? comp_throw 'paper)) ; sciccors beats paper
            (sentence '(You won!....) human_throw 'beat comp_throw)
        )

        ( (and (equal? human_throw 'rock) (equal? comp_throw 'scissors)) ; rock beats scissors
            (sentence '(You won!....) human_throw 'beat comp_throw)
        )

        ( (and (equal? human_throw 'paper) (equal? comp_throw 'rock)) ; paper beats rock
            (sentence '(You won!....) human_throw 'beat comp_throw)
        )

        ( (equal? (and human_throw 'rock) (equal? comp_throw 'lizzard)) ; rock beats lizzard
            (sentence '(You won!....) human_throw 'beat comp_throw)
        )

        ( (and (equal? human_throw 'lizzard) (equal? comp_throw 'spock)) ; lizzard beats spock
            (sentence '(You won!....) human_throw 'beat comp_throw)
        )

        ( (and (equal? human_throw 'spock) (equal? comp_throw 'scissors)) ; spock beats scissors
            (sentence '(You won!....) human_throw 'beat comp_throw)
        )

        ( (equal? (and human_throw 'paper) (equal? comp_throw 'spock)) ; paper beats spock
            (sentence '(You won!....) human_throw 'beat comp_throw)
        )

        ( (and (equal? human_throw 'lizzard) (equal? comp_throw 'paper)) ; lizzard beats paper
            (sentence '(You won!....) human_throw 'beat comp_throw)
        )

        ( (and (equal? human_throw 'spock) (equal? comp_throw 'rock)) ; spock beats rock
            (sentence '(You won!....) human_throw 'beat comp_throw)
        )

        ( (and (equal? human_throw 'scissors) (equal? comp_throw 'lizzard)) ; scissors beats lizzard
            (sentence '(You won!....) human_throw 'beat comp_throw)
        )

        (else 
            (se '(You lost....) human_throw '(loses to) comp_throw) ; otherwise loose
        )
    )
)


; comp does not take an input, and outputs a psedorandom computer throw.

; this is bugged in coding rooms when called by console for whatever reason.
(define (randThrow)
    (let ((x (random 5)))
        (cond ((equal? x 0) 'rock )
            ( (equal? x 1) 'paper )
            ( (equal? x 2) 'scissors )
            ( (equal? x 3) 'lizzard )
            ( (equal? x 4) 'spock )
        )
    )
)

; TESTING_CODE - Add some tests that will efficiently demonstrate that your code works.

(define (randPlay)
    (rpsls (randThrow) (randThrow) ) ; play with two random throws
)

; play randomly once
;(randPlay)

; Anthony Andrews Rock-Paper-Scissors-Lizzard-Spock using modulo
; run this code ten times to get each possible outcome (on average) or comment out the last line to play against the computer

;(define (randThrow) ; create a function to return a psuedo random integer from 1 through 5
;    (let ((x (random 5) ))
;        (cond 
;            ( (equal? x 0) 1 )
;            ( (equal? x 1) 2 )
;            ( (equal? x 2) 3 )
;            ( (equal? x 3) 4 )
;            ( (equal? x 4) 5 )
;        )
;    )
;)

;(define (throwName y) ; translates the values generated by the randThrow computer into their respective throw names.
;    (cond
;        ( (equal? y 1) 'rock)
;        ( (equal? y 2) 'spock)
;        ( (equal? y 3) 'paper)
;        ( (equal? y 4) 'lizzard)
;        ( (equal? y 5) 'scissors)
;    )
;)

;(define (rpsls humThrow)
;    (let ((compThrow (randThrow))) ; assigns the variable x to the integer created by the randThrow function.
;        (let ((x (remainder (+ (- humThrow compThrow ) 5 ) 5 ) )) ; do the maths and assign the variable x to the remainder.
;            (cond ; check if the remainder (x) corresponds to a tie, loss, or win.
;                ( (equal? x 0) ; tie
;                    (sentence (throwName humThrow) 'tied (throwName compThrow )) ; call the throwName function passing in the; throws given to get their respective throw names, then concatenate them into a sentance.
;                )
;                ( (equal? x 1) ; win
;                    (sentence (throwName humThrow) 'beat (throwName compThrow ))
;                )
;                ( (equal? x 2) ; win
;                    (sentence (throwName humThrow) 'beat (throwName compThrow ))
;                )
;                ( (equal? x 3) ;loss
;                    (sentence (throwName humThrow) '(lost to) (throwName compThrow ))
;                )
;                ( (equal? x 4) ; loss
;                    (sentence (throwName humThrow) '(lost to) (throwName compThrow ))
;                )
;            )
;        )
;    )
;)

;(rpsls (randThrow)) ; play using a random throw as the human input.
