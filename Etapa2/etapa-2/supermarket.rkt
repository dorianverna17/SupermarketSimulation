#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat întâi Ana cu 3 produse și apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute => et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (make-counter index 0 0 '()))


; TODO
; Implementați o funcție care aplică o transformare casei cu un anumit index.
; f = funcție unară care se aplică pe un argument de tip casă, counters = listă de case,
; index = indexul casei din counters căreia îi veți aplica transformarea f
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index, veți întoarce lista nemodificată.
(define (update f counters index)
  (map (lambda (x) (if (= (counter-index x) index) (f x) x)) counters))


; TODO
; Memento: tt+ crește tt-ul unei case cu un număr dat de minute.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument funcției update în cel mai facil mod
; Obs: Pentru compatibilitatea cu primul argument al lui update, trebuie să obținem ușor din tt+ 
; o funcție unară care se aplică pe un argument de tip casă (v. funcții curry).
; Am eliminat parametrii lui tt+ din define-ul de mai jos astfel încât voi să determinați
; cum este cel mai bine ca tt+ să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție, dar asistentul va verifica
; faptul că ați implementat-o conform cerințelor.
(define tt+
  (lambda (x)
    (lambda (y)
      (match y
         [(counter index tt et queue)
          (struct-copy counter y [tt (+ tt x)])]))))


; TODO
; Implementați o funcție care crește et-ul unei case cu un număr dat de minute.
; Păstrați formatul folosit pentru tt+.
; Checker-ul nu testează separat această funcție.
(define et+
  (lambda (x)
    (lambda (y)
      (match y
         [(counter index tt et queue)
          (struct-copy counter y [et (+ et x)])]))))


; TODO
; Memento: add-to-counter adaugă o persoană (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter pentru aceleași rațiuni pentru care am modificat tt+.
; Atenție la cum se modifică tt și et!
; Checker-ul nu testează separat această funcție.
(define add-to-counter
  (lambda (a b)
    (lambda (x)
      (match x
         [(counter index tt et queue)
          (struct-copy counter x [tt (+ tt b)] [et (+ (if (null? queue) b 0) et)] [queue (append queue (list (cons a b)))])]))))


; TODO
; Întrucât vom folosi atât min-tt (implementat în etapa 1) cât și min-et (funcție nouă),
; definiți-vă o funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (când mai multe case au același et, este preferată casa cu indexul cel mai mic)
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

(define min-tt (get-min-counter counter-tt)) ; folosind funcția de mai sus
(define min-et (get-min-counter counter-et)) ; folosind funcția de mai sus


; TODO
; Implementați o funcție care scoate prima persoană din coada unei case.
; Funcția presupune, fără să verifice, că există minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva, înseamnă că ea nu mai are întârzieri.

; Functie auxiliara definita de mine pentru a calcula tt total pentru un queue
(define (sum-tt queue)
  (foldl (lambda (x acc) (+ acc (cdr x))) 0 queue))

(define (remove-first-from-counter C)
  (match C
         [(counter index tt et queue)
          (if (null? queue)
              (struct-copy counter C)
              (struct-copy counter C [tt (if (null? (cdr queue)) 0 (sum-tt (cdr queue)))] [et (if (null? (cdr queue)) 0 (cdr (car (cdr queue))))] [queue (cdr queue)]))]))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția serve operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a obține comportamentul pe liste de case)
; - requests conține 4 tipuri de cereri (cele 2 din etapa anterioară plus 2 noi):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care se află
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
;   (ca înainte, dar acum folosiți fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți)
;   (puțin diferit față de etapa 1, și acum folosiți fast-counters și slow-counters)
; - persoana care părăsește supermarketul este persoana fruntașă la casa cu et-ul minim
;   (dintre casele care au clienți); dacă nicio casă nu are clienți, cererea e ignorată
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>
; Considerați că indecșii caselor încep de la 1 și sunt mereu în ordine.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie veți adăuga întâi casa 16, apoi casa 17, etc.
; (puteți determina matematic de câte case noi este nevoie sau
;  puteți adăuga recursiv una câte una până când condiția este îndeplinită)

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

; functia auxiliara add-customer care adauga un customer la o casa
(define (add-customer requests name n-items fast-counters slow-counters)
  (if (<= n-items ITEMS)
      (if (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
          (serve requests
                 (foldl (lambda (x acc) (if (= (counter-index x) (counter-index (min-counter-tt fast-counters)))
                                            (append acc (list ((add-to-counter name n-items) x)))
                                            (append acc (list x)))) '() fast-counters) slow-counters)
          (serve requests fast-counters
                 (foldl (lambda (x acc) (if (= (counter-index x) (counter-index (min-counter-tt slow-counters)))
                                            (append acc (list ((add-to-counter name n-items) x)))
                                            (append acc (list x)))) '() slow-counters))
      )
      (serve requests fast-counters
                 (foldl (lambda (x acc) (if (= (counter-index x) (counter-index (min-counter-tt slow-counters)))
                                            (append acc (list ((add-to-counter name n-items) x)))
                                            (append acc (list x)))) '() slow-counters))
  )
  )

; functie pentru delay - ma folosesc de functia update
(define (delay requests index minutes fast-counters slow-counters)
      (if (null? (filter (lambda (C) (= (counter-index C) index)) fast-counters))
          (serve requests fast-counters (update (lambda (x) (struct-copy counter x [tt (+ minutes (counter-tt x))] [et (+ minutes (counter-et x))])) slow-counters index))
          (serve requests (update (lambda (x) (struct-copy counter x [tt (+ minutes (counter-tt x))] [et (+ minutes (counter-et x))])) fast-counters index) slow-counters)
          ))

; functie care returneaza casa la care se afla cea mai avansata persoana
(define (find-most-advanced counters)
  (if (null? counters)
      null
      (if (= (length counters) 1)
          (car counters)
          (if (> (counter-et (car counters)) 0)
              (if (<= (counter-et (car counters)) (counter-et (find-most-advanced (cdr counters))))
                  (car counters)
                  (find-most-advanced (cdr counters)))
              (find-most-advanced (cdr counters))))))

; functie care este apelata in serve pentru a sterge
(define (remove-most-advanced requests fast-counters slow-counters)
  (cond
    ((and (null? (find-most-advanced (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) fast-counters))) (null? (find-most-advanced (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) slow-counters))))
     (serve requests fast-counters slow-counters))
    ((and (null? (find-most-advanced (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) fast-counters))) (not (null? (find-most-advanced (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) slow-counters)))))
     (serve requests fast-counters (update (lambda (x) (remove-first-from-counter x)) slow-counters (counter-index (find-most-advanced (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) slow-counters))))))
    ((and (null? (find-most-advanced (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) slow-counters))) (not (null? (find-most-advanced (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) fast-counters)))))
     (serve requests (update (lambda (x) (remove-first-from-counter x)) fast-counters (counter-index (find-most-advanced (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) fast-counters)))) slow-counters))
    (else (if (<= (counter-et (find-most-advanced (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) fast-counters))) (counter-et (find-most-advanced (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) slow-counters))))
          (serve requests (update (lambda (x) (remove-first-from-counter x)) fast-counters (counter-index (find-most-advanced (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) fast-counters)))) slow-counters)
          (serve requests fast-counters (update (lambda (x) (remove-first-from-counter x)) slow-counters (counter-index (find-most-advanced (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) slow-counters)))))))
    )
  )

(define (get-tt-med counters)
  (/ (foldl (lambda (x acc) (+ acc (counter-tt x))) 0 counters) (length counters)
     ))

(define (ensure-counters requests average fast-counters slow-counters)
  (if (> (get-tt-med (append fast-counters slow-counters)) average)
      (ensure-counters requests average fast-counters (append slow-counters (list (empty-counter (+ (length slow-counters) (length fast-counters) 1)))))
      slow-counters))

(define (auxiliary-ensure-counters requests average fast-counters slow-counters)
  (serve requests fast-counters (ensure-counters requests average fast-counters slow-counters))
  )

(define (serve requests fast-counters slow-counters)

  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)
        [(list 'delay index minutes) (delay (cdr requests) index minutes fast-counters slow-counters)]
        [(list name n-items) (if (equal? (car (car requests)) 'ensure)
                                 (auxiliary-ensure-counters (cdr requests) n-items fast-counters slow-counters)
                                 (add-customer (cdr requests) name n-items fast-counters slow-counters))]
        [(list 'remove-first) (remove-most-advanced (cdr requests) fast-counters slow-counters)])))

