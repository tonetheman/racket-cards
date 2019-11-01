#lang racket


(struct card (rank suit) #:transparent )

(define (suit->string c)
    (cond
    [(= 0 (card-suit c)) "D"]
    [(= 1 (card-suit c)) "H"]
    [(= 2 (card-suit c)) "C"]
    [(= 3 (card-suit c)) "S"]
    )
)
(define (rank->string c)
    (define r (card-rank c))
    (cond
        [(= 0 r) "A"]
        [(= 1 r) "2"]
        [(= 2 r) "3"]
        [(= 3 r) "4"]
        [(= 4 r) "5"]
        [(= 5 r) "6"]
        [(= 6 r) "7"]
        [(= 7 r) "8"]
        [(= 8 r) "9"]
        [(= 9 r) "T"]
        [(= 10 r) "J"]
        [(= 11 r) "Q"]
        [(= 12 r) "K"]
    )
)

(define (make-card num)
    (define rank (modulo num 13))
    (define suit (truncate (/ num 13)))
    (card rank suit)
)   
(define (print-card c)
    (display (rank->string c))
    (display (suit->string c))
    (display "\n")
)
(define tmp (list))
(for ([i 52])

    (let ([c (make-card i)])
        (set! tmp (append tmp (list c)))
        (print-card c)
    )
)

(display tmp)