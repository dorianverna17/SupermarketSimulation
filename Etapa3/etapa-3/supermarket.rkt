#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (map (lambda (x) (if (= (counter-index x) index) (f x) x)) counters))

(define tt+
  (lambda (x)
    (lambda (y)
      (match y
         [(counter index tt et queue)
          (struct-copy counter y [tt (+ tt x)])]))))

(define et+
  (lambda (x)
    (lambda (y)
      (match y
         [(counter index tt et queue)
          (struct-copy counter y [et (+ et x)])]))))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (match C
         [(counter index tt et queue)
          (struct-copy counter C [tt (+ tt items)] [et (+ (if (queue-empty? queue) items 0) et)] [queue (enqueue (cons name items) queue)])])))

(define get-min-counter
  (lambda (f)
    (lambda (y)
      (if
       (= (length y) 1)
       (cons (counter-index (car y)) (f (car y)))
       (if
        (<= (f (car y)) (cdr ((get-min-counter f) (cdr y))))
        (cons (counter-index (car y)) (f (car y)))
        ((get-min-counter f) (cdr y)))))))

(define min-tt (get-min-counter counter-tt))
(define min-et (get-min-counter counter-et))

; functia care calculeaza suma tt-urilor dintr-o coada
(define (sum-tt queue)
  (+ (foldl (lambda (x acc) (+ acc (cdr x))) 0 (queue-left queue)) (foldl (lambda (x acc) (+ acc (cdr x))) 0 (queue-right queue))))

(define (remove-first-from-counter C)   ; testată de checker
  (match C
         [(counter index tt et queue)
          (if (queue-empty? queue)
              (struct-copy counter C)
              (let ((aux_queue (dequeue queue)))
              (struct-copy counter C [tt (if (queue-empty? aux_queue) 0 (sum-tt aux_queue))] [et (if (queue-empty? aux_queue) 0 (cdr (top aux_queue)))] [queue aux_queue])))]))


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (match C
         [(counter index tt et queue)
          (struct-copy counter C [tt (if (< (- tt minutes) 0) 0 (- tt minutes))] [et (if (< (- et minutes) 0) 0 (- et minutes))])])
    ))
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.

; functie care determina prima structura care are tt-ul minim dintr-o lista de counters
(define (min-counter-tt counters)
  (if (= (counter-tt (car counters)) (cdr (min-tt counters)))
      (car counters)
      (min-counter-tt (cdr counters))
   ))

; functie care determina prima structura care are et-ul minim dintr-o lista de counters
(define (min-counter-et counters)
  (if (= (counter-et (car counters)) (cdr (min-et counters)))
      (car counters)
      (min-counter-et (cdr counters))
   ))

; functie similara pentru adaugarea unui customer la o casa - cea la care trebuie sa fie repartizat
(define (add-customer requests name n-items fast-counters slow-counters list3)
  (if (<= n-items ITEMS)
      (if (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
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

; functia prin care se face delay
(define (delay requests index minutes fast-counters slow-counters list3)
      (if (null? (filter (lambda (C) (= (counter-index C) index)) fast-counters))
          (auxiliary-serve requests fast-counters (update (lambda (x) (struct-copy counter x [tt (+ minutes (counter-tt x))] [et (+ minutes (counter-et x))])) slow-counters index) list3)
          (auxiliary-serve requests (update (lambda (x) (struct-copy counter x [tt (+ minutes (counter-tt x))] [et (+ minutes (counter-et x))])) fast-counters index) slow-counters list3)
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

; urmatoarele 3 functii le folosesc pentru ensure - similar ca in etapa precedenta
(define (get-tt-med counters)
  (/ (foldl (lambda (x acc) (+ acc (counter-tt x))) 0 counters) (length counters)
     ))

(define (ensure-counters requests average fast-counters slow-counters)
  (if (> (get-tt-med (append fast-counters slow-counters)) average)
      (ensure-counters requests average fast-counters (append slow-counters (list (empty-counter (+ (length slow-counters) (length fast-counters) 1)))))
      slow-counters))

(define (auxiliary-ensure-counters requests average fast-counters slow-counters list_out)
  (auxiliary-serve requests fast-counters (ensure-counters requests average fast-counters slow-counters) list_out)
  )

; functie auxiliara de serve - pentru a retine in ordinea ceruta oamenii care pleaca de la casa
(define (auxiliary-serve requests fast-counters slow-counters list_out)
  (if (null? requests)
      (append (list list_out) (append fast-counters slow-counters))
      (match (car requests)
        [(list 'delay index minutes) (delay (cdr requests) index minutes fast-counters slow-counters list_out)]
        [(list name n-items) (if (equal? (car (car requests)) 'ensure)
                                 (auxiliary-ensure-counters (cdr requests) n-items fast-counters slow-counters list_out)
                                 (add-customer (cdr requests) name n-items fast-counters slow-counters list_out))]
        [number (trec number (cdr requests) fast-counters slow-counters list_out)])))

(define (serve requests fast-counters slow-counters)
  (auxiliary-serve requests fast-counters slow-counters '()))
        
