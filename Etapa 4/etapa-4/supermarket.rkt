#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

; structura definita de mine -> daca close este 1 autnci casa este INCHISA
; daca close este o, atunci casa este DESCHISA
(define-struct counter (index tt et close queue) #:transparent)

(define (empty-counter index)
  (make-counter index 0 0 0 empty-queue))
  
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei

; functie de update de la etapa 3
(define (update f counters index)
  (map (lambda (x) (if (= (counter-index x) index) (f x) x)) counters))

; functie pentru a extrage casele deschise
(define (get-open-counters counters)
  (foldl (lambda (x acc) (if (= 0 (counter-close x))
                             (append acc (list x))
                             acc)) '() counters)
  )

(define get-min-counter
  (lambda (f)
    (lambda (y_aux)
      (let ((y (get-open-counters y_aux)))
      (if
       (= (length y) 1)
       (cons (counter-index (car y)) (f (car y)))
       (if
        (and (<= (f (car y)) (cdr ((get-min-counter f) (cdr y)))) (= 0 (counter-close (car y))))
        (cons (counter-index (car y)) (f (car y)))
        ((get-min-counter f) (cdr y))))))))

(define min-tt (get-min-counter counter-tt))
(define min-et (get-min-counter counter-et))

; functie care determina prima structura care are tt-ul minim dintr-o lista de counters
; si este casa DESCHISA
(define (min-counter-tt counters)
      (if (and (= (counter-tt (car counters)) (cdr (min-tt counters))) (= (counter-close (car counters)) 0))
          (car counters)
          (min-counter-tt (cdr counters)))
;      (empty-counter -1))
  )

; functie preluata de la etapa 3
(define (add-to-counter name items)
  (λ (C)
    (match C
         [(counter index tt et close queue)
          (struct-copy counter C [tt (+ tt items)] [et (+ (if (queue-empty? queue) items 0) et)] [queue (enqueue (cons name items) queue)])])))


(define (add-customer requests name n-items fast-counters slow-counters list3)
  (if (<= n-items ITEMS)
      (if (and (not (null? (get-open-counters fast-counters))) (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters))))
          (auxiliary-serve requests
                 (foldl (lambda (x acc) (if (= (counter-index x) (counter-index (min-counter-tt fast-counters)))
                                            (append acc (list ((add-to-counter name n-items) x)))
                                            (append acc (list x)))) '() fast-counters) slow-counters list3)
          (auxiliary-serve requests fast-counters
                 (foldl (lambda (x acc) (if (= (counter-index x) (counter-index (min-counter-tt slow-counters)))
                                            (append acc (list ((add-to-counter name n-items) x)))
                                            (append acc (list x)))) '() slow-counters) list3)
      )
      (auxiliary-serve requests fast-counters
                 (foldl (lambda (x acc) (if (= (counter-index x) (counter-index (min-counter-tt slow-counters)))
                                            (append acc (list ((add-to-counter name n-items) x)))
                                            (append acc (list x)))) '() slow-counters) list3)
  )
  )

;; urmatoarele 7 functii sunt preluate in principiu de la etapa3
;; si adaptate la cerintele actuale

; functia care calculeaza suma tt-urilor dintr-o coada
(define (get-sum-tt q)
  (let loop ((size (queue-size-l q)) (s (queue-left q)))
    (if (= size 1)
        (cdr (stream-first s))
        (+ (cdr (stream-first s)) (loop (sub1 size) (stream-rest s)))
    )))

(define (sum-tt queue)
  (+ (get-sum-tt queue) (foldl (lambda (x acc) (+ acc (cdr x))) 0 (queue-right queue))))

(define (remove-first-from-counter C)
  (match C
         [(counter index tt et close queue)
          (if (queue-empty? queue)
              (struct-copy counter C)
              (let ((aux_queue (dequeue queue)))
              (struct-copy counter C [tt (if (queue-empty? aux_queue) 0 (sum-tt aux_queue))] [et (if (queue-empty? aux_queue) 0 (cdr (top aux_queue)))] [queue aux_queue])))]))

(define (pass-time-through-counter minutes)
  (λ (C)
    (match C
         [(counter index tt et close queue)
          (struct-copy counter C [tt (if (< (- tt minutes) 0) 0 (- tt minutes))] [et (if (< (- et minutes) 0) 0 (- et minutes))])])
    ))

; functie care modifica o lista de case - la trecerea unui minut
(define (modify-list counters)
  (foldl (lambda (x acc)
           (cond
             ((= (counter-et x) 0) (append acc (list x)))
             ((and (= (counter-et x) 1) (not (and (= (queue-size-l (counter-queue x)) 0) (= (queue-size-r (counter-queue x)) 0)))) (append acc (list (remove-first-from-counter x))))
             (else (append acc (list ((pass-time-through-counter 1) x))))
             )
           )
         '() counters)
  )

; functie care imi selecteaza oamenii care au plecat de la case
(define (get-removed counters)
  (foldl (lambda (x acc)
           (cond
             ((and (= (counter-et x) 1) (not (and (= (queue-size-l (counter-queue x)) 0) (= (queue-size-r (counter-queue x)) 0)))) (append acc (list (cons (counter-index x) (car (top (counter-queue x)))))))
             (else (append acc '()))
             )
           )
         '() counters)
  )

; functia care modifica casele odata cu trecerea minutelor - cea care apeleaza functia serve
(define (trec minutes requests fast-counters slow-counters list_out)
  (if (= minutes 0)
      (auxiliary-serve requests fast-counters slow-counters list_out)
      (trec (- minutes 1) requests (modify-list fast-counters) (modify-list slow-counters) (append list_out (get-removed (append fast-counters slow-counters))))
  ))

; functie care inchide o casa
(define (close-the-counter index requests fast-counters slow-counters list_out)
  (if (> index (length fast-counters))
      (auxiliary-serve requests fast-counters
                       (foldl (lambda (x acc) (if (= index (counter-index x))
                                                  (append acc (list (struct-copy counter x [close 1])))
                                                  (append acc (list x)))) '() slow-counters) list_out)
      (auxiliary-serve requests (foldl (lambda (x acc) (if (= index (counter-index x))
                                                  (append acc (list (struct-copy counter x [close 1])))
                                                  (append acc (list x)))) '() fast-counters) slow-counters list_out)
  ))

; functia friend-out transforma in output-ul dorit listele de counters
; TODO -> trebuie afisate doar casele care mai au clienti la coada
(define (friend-out counters)
  (foldl (lambda (C acc)
           (if (queue-empty? (counter-queue C))
               acc
               (append acc (list (cons (counter-index C) (counter-queue C))))))
         '() counters)
  )

; urmatoarele 3 functii le folosesc pentru ensure - similar ca in etapa precedenta
(define (get-tt-med counters)
  (/ (foldl (lambda (x acc) (if (= 0 (counter-close x)) (+ acc (counter-tt x)) acc)) 0 counters) (length (get-open-counters counters))
     ))

(define (ensure-counters requests average fast-counters slow-counters)
  (if (> (get-tt-med (append fast-counters slow-counters)) average)
      (ensure-counters requests average fast-counters (append slow-counters (list (empty-counter (+ (length slow-counters) (length fast-counters) 1)))))
      slow-counters))

(define (auxiliary-ensure-counters requests average fast-counters slow-counters list_out)
  (auxiliary-serve requests fast-counters (ensure-counters requests average fast-counters slow-counters) list_out)
  )

; functie pentru delay
(define (delay requests index minutes fast-counters slow-counters list3)
      (if (null? (filter (lambda (C) (= (counter-index C) index)) fast-counters))
          (auxiliary-serve requests fast-counters (update (lambda (x) (struct-copy counter x [tt (+ minutes (counter-tt x))] [et (+ minutes (counter-et x))])) slow-counters index) list3)
          (auxiliary-serve requests (update (lambda (x) (struct-copy counter x [tt (+ minutes (counter-tt x))] [et (+ minutes (counter-et x))])) fast-counters index) slow-counters list3)
          ))

; functia auxiliara pentru serve -> unde se fac match-urile
(define (auxiliary-serve requests fast-counters slow-counters list_out)
  (if (null? requests)
      (append (list list_out) (append (friend-out fast-counters) (friend-out slow-counters)))
      (match (car requests)
        [(list name n-items) (if (equal? (car (car requests)) 'ensure)
                                 (auxiliary-ensure-counters (cdr requests) n-items fast-counters slow-counters list_out)
                                 (if (equal? (car (car requests)) 'close)
                                     (close-the-counter n-items (cdr requests) fast-counters slow-counters list_out)
                                     (add-customer (cdr requests) name n-items fast-counters slow-counters list_out)))]
        [(list 'delay index minutes) (delay (cdr requests) index minutes fast-counters slow-counters list_out)]
        [number (trec (car requests) (cdr requests) fast-counters slow-counters list_out)]
        )))

; functia originala de serve
(define (serve requests fast-counters slow-counters)
  (auxiliary-serve requests fast-counters slow-counters '()))