;sorts a sentence low to high alphabetically or numerically
(define (sorts sent)
  (if (empty? sent)
      '()
      (se (earliest-word sent) (sorts (remove-once (earliest-word sent) sent))))) ; Line A

;returns the earliest word in a sentence (alphabetical or numerical)
(define (earliest-word sent)
  (earliest-helper (first sent) (bf sent)))

;so-far = earliest word at current point, rest = the rest of the sentence
(define (earliest-helper so-far rest) 
  (cond ((empty? rest) so-far)  
        ((before? so-far (first rest))
         (earliest-helper so-far (bf rest)))  
        (else (earliest-helper (first rest) (bf rest))))) 

;removes a word once from a sentence
(define (remove-once wd sent)
  (cond ((empty? sent) '()) 
        ((equal? wd (first sent)) (bf sent)) 
        (else (se (first sent) (remove-once wd (bf sent)))))) 

;trace through (sorts '(c b a))

; (sorts '(c b a))  ;does the following starting at line A
; 
; (se (earliest-word '(c b a) (sorts (remove-once (earliest-word '(c b a)) '(c b a)))))
; 
;    (earliest-word '(c b a))
; 
;       (earliest-helper 'c '(b a))  
;       (earliest-helper 'b '(a))  
;       (earliest-helper 'a '())
;       'a
; 
; ;So back to building the sentence recursively... we have
; 
; (se 'a (sorts (remove-once 'a '(c b a))))
; (se 'a (sorts '(c b)))
; (se 'a (se (earliest-word '(c b) (sorts (remove-once (earliest-word '(c b))) '(c b)))))
; (se 'a (se 'b (sorts (remove-once 'b '(c b)))))
; 
; (se 'a (se 'b (sorts '(c))))
; (se 'a (se 'b (se (earliest-word '(c) (sorts (remove-once (earliest-word '(c)) '(c)))))))
; (se 'a (se 'b (se 'c (sorts (remove-once 'c '(c))))))
; (se 'a (se 'b (se 'c (sorts '()))))
; (se 'a (se 'b (se 'c '())))
; '(a b c)
; 
; ;DONE! UGG!
; 
; 
; 
;               
