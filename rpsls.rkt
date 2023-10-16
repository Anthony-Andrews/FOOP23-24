#lang simply-scheme

(define (ascii obj)
    (cond
        ( (equal? obj 'rock)
            (display
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
            (display
"
 █▀█ ▄▀█ █▀█ █▀▀ █▀█
 █▀▀ █▀█ █▀▀ ██▄ █▀▄
")
        )
            
        ( (equal? obj 'scissors)
            (display
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
            (display
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
            (display
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
            (display "\nIt's a tie!\n")
            (ascii human_throw)
            (display "\ntied,\n")
            (ascii comp_throw)
        )

        ( (equal? (and human_throw 'scissors) (equal? comp_throw 'paper)) ; sciccors beats paper
            (display "\nYou won!\n")
            (ascii human_throw)
            (display "\ncut,\n")
            (ascii comp_throw)
        )

        ( (and (equal? human_throw 'rock) (equal? comp_throw 'scissors)) ; rock beats scissors
            (display "\nYou won!\n")
            (ascii human_throw)
            (display "\nsmashed,\n")
            (ascii comp_throw)
        )

        ( (and (equal? human_throw 'paper) (equal? comp_throw 'rock)) ; paper beats rock
            (display "\nYou won!\n")
            (ascii human_throw)
            (display "\nbeat,\n")
            (ascii comp_throw)
            (display "\n(somehow)...\n")
        )

        ( (equal? (and human_throw 'rock) (equal? comp_throw 'lizzard)) ; rock beats lizzard
            (display "\nYou won!\n")
            (ascii human_throw)
            (display "\ncrushes,\n")
            (ascii comp_throw)
        )

        ( (and (equal? human_throw 'lizzard) (equal? comp_throw 'spock)) ; lizzard beats spock
            (display "\nYou won!\n")
            (ascii human_throw)
            (display "\nate,\n")
            (ascii comp_throw)
        )

        ( (and (equal? human_throw 'spock) (equal? comp_throw 'scissors)) ; spock beats scissors
            (display "\nYou won!\n")
            (ascii human_throw)
            (display "\nsmashed,\n")
            (ascii comp_throw)
        )

        ( (equal? (and human_throw 'paper) (equal? comp_throw 'spock)) ; paper beats spock
            (display "\nYou won!\n")
            (ascii human_throw)
            (display "\ndisproves,\n")
            (ascii comp_throw)
        )

        ( (and (equal? human_throw 'lizzard) (equal? comp_throw 'paper)) ; lizzard beats paper
            (display "\nYou won!\n")
            (ascii human_throw)
            (display "\nate,\n")
            (ascii comp_throw)
        )

        ( (and (equal? human_throw 'spock) (equal? comp_throw 'rock)) ; spock beats rock
            (display "\nYou won!\n")
            (ascii human_throw)
            (display "\nvaporizes,\n")
            (ascii comp_throw)
        )

        ( (and (equal? human_throw 'scissors) (equal? comp_throw 'lizzard)) ; scissors beats lizzard
            (display "\nYou won!\n")
            (ascii human_throw)
            (display "\ndecapitates,\n")
            (ascii comp_throw)
        )

        (else 
            (display "\nYou lost...\n")
            (ascii human_throw)
            (display "\nlost to,\n")
            (ascii comp_throw) ; otherwise loose
        )
    )
)


; comp does not take an input, and outputs a psedorandom computer throw.

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
