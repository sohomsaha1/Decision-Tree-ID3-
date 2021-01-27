;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname DecisionTreeAnimals) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Sohom Saha
;; 11/04/2020
;; Decision Tree (ID3 Algo) - Classify Animals


;; An Example is a (cons Sym (listof Sym))
;; Requires: each attribute in the rest is unique

;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)

(define seen
  (list
   (list 'squirrel 'small 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'crow 'medium 'flies 'angry)))

(define seen2
  (list
   (list 'animal 'small 'angry)
   (list 'goose 'large 'swims 'flies 'angry 'happy)
   (list 'crow 'medium 'flies 'angry 'small)))

(define seen3
  (list
   (list 'squirrel 'small)
   (list 'herring 'large 'swims 'flies 'angry)
   (list 'crow 'flies 'angry)))

(define seen4
  (list
   (list 'bird)
   (list 'human)
   (list 'banana)
   (list 'parrot)))

(define empty-examples
  (list ))




;; (collect-attributes examples) produces
;; a list of attributes with no duplicates from
;; examples

;; Examples:

(check-expect (collect-attributes seen)
              (list 'medium 'flies 'swims 'large 'angry 'small))

(check-expect (collect-attributes seen2)
              (list 'medium 'happy 'flies 'swims 'large 'angry 'small))

(check-expect (collect-attributes seen3)
              (list
                'angry
                'flies
                'swims
                'large
                'small))

;; collect-attributes: (listof Example) -> (listof Sym)
(define (collect-attributes lst)
  (local [(define (d-remove lst empt)
  (cond[(empty? lst) empt]
       [(not(member? (first lst) empt))
        (d-remove (rest lst) (cons (first lst)empt))] 
       [ else (d-remove (rest lst) empt)]))]
   (local [(define (f-list lst)
  (cond[ (empty? lst) empty]
       [ else (append (rest(first lst)) (f-list (rest lst)))]))]
     (d-remove(f-list lst) empty))))

;; Tests:

(check-expect (collect-attributes seen4)
                            (list ))
(check-expect (collect-attributes empty-examples)
                            (list ))




  
;; (split-examples: examples symbol) produces a list with 2 lists,
;; one containing all the values with symbol inside and another without


;; Examples:
(check-expect (split-examples seen 'goose)
(list
 (list
  (list 'goose 'large 'swims 'flies 'angry)
  (list 'goose 'large 'swims 'flies 'angry))
 (list
  (list 'squirrel 'small 'angry)
  (list 'crow 'medium 'flies 'angry))))


(check-expect (split-examples seen2 'squirrel) (list
 '()
 (list
  (list 'animal 'small 'angry)
  (list 'goose 'large 'swims 'flies 'angry 'happy)
  (list 'crow 'medium 'flies 'angry 'small))))

(check-expect (split-examples seen2 'angry)
              (list
               (list
                (list 'animal 'small 'angry)
                (list 'goose 'large 'swims 'flies 'angry 'happy)
                (list 'crow 'medium 'flies 'angry 'small))
               '()))
 
;; split-examples: (listof Example) Sym -> (listof (listof Example))
(define (split-examples lst sym)
  (local [(define (split-1 lst sym)
  (cond[(empty? lst) empty]
       [(member? sym (first lst))
        (cons (first lst) (split-1 (rest lst) sym))]
       [else (split-1 (rest lst) sym)]))]
   (local [(define (split-2 lst sym)
  (cond[(empty? lst) empty]
       [(not(member? sym (first lst)))
        (cons (first lst) (split-2 (rest lst) sym))]
       [else (split-2 (rest lst) sym)])) ]
  (local [(define (combine lst sym)
  (cond[(empty? lst) empty]
       [else (list (split-1 lst sym)(split-2 lst sym))]))]
    (combine lst sym)))))

;; Tests:
(check-expect (split-examples empty-examples 'hello)
              '())


(check-expect (split-examples seen 'large)
              (list
               (list
                (list
                 'goose
                 'large
                 'swims
                 'flies
                 'angry)
                (list
                 'goose
                 'large
                 'swims
                 'flies
                 'angry))
               (list
                (list 'squirrel 'small 'angry)
                (list 'crow 'medium 'flies 'angry))))




;; (histogram examples) produces a list
;; of attribute/count pairs with the pair showing how
;; many times that specific attribute appears in examples.



;; Examples:

(check-expect (histogram seen)
              (list
               (list 'medium 1) (list 'flies 3) (list 'swims 2)
               (list 'large 2) (list 'angry 4) (list 'small 1)))

(check-expect (histogram seen2)
              (list
               (list 'medium 1)
               (list 'happy 1)
               (list 'flies 2)
               (list 'swims 1)
               (list 'large 1)
               (list 'angry 3)
               (list 'small 2)))

;; histogram: (listof Example) -> Histogram
(define (histogram examples)
  (local
    [(define (counter lst sym count)
       (cond[(empty? lst) count]
            [(symbol=? (first lst) sym) (counter (rest lst) sym (+ 1 count))]
            [ else (counter (rest lst) sym count)]))]
  (local
   [(define (big-count lst sym count)
      (cond[(empty? lst) (list sym count)]
           [ else (big-count (rest lst) sym(+ (counter (first lst) sym 0) count))]))]
   
  (local
    [(define (hist-help lst1 lst)
       (cond[(empty? lst) empty]
            [ else (cons (big-count lst1 (first lst) 0) (hist-help lst1 (rest lst)))]))]

    (hist-help examples (collect-attributes examples))))))


;; Tests:


(check-expect (histogram (list (list 'hello 'world)(list 'how 'are) (list 'u)))
              (list (list 'are 1) (list 'world 1)))

(check-expect (histogram (list (list 'hello)))'())





;; (augment-histogram histogram attributes total)
;; outputs a list of attributes with the number of
;; occurences as well as the number of non-occurences
;; Examples:

(check-expect
 (augment-histogram
  (list (list 'a 100) (list 'c 50))
  (list 'a 'b 'c)
  200)
 (list(list 'a 100 100)(list 'b 0 200)(list 'c 50 150)))



(check-expect
 (augment-histogram empty (list 'x 'y) 10)
    (list (list 'x 0 10) (list 'y 0 10)))


;; augment-histogram: Histogram (listof Sym) Nat -> AH
(define (augment-histogram histogram attributes total)
  (local
    [(define (search lst sym total)
       (cond[(empty? lst) empty]
            [ (symbol=? sym (first(first lst)))
              (list sym (second(first lst))
              (- total (second(first lst))))]
            [ else (search (rest lst) sym total)]))]
   (local
     [(define (search-empt attributes total)
        (cond[(empty? attributes) empty]
             [else (cons (list (first attributes) 0 total)
                         (search-empt (rest attributes) total))]))]
  (cond[ (empty? histogram) (search-empt attributes total)]
       [ (empty? attributes) empty]
       [ else (cons (cond[(equal?(search histogram
                                         (first attributes) total) empty)
                          (list (first attributes) 0 total)]
                         [else (search histogram (first attributes) total)])
                    (augment-histogram histogram (rest attributes) total))]))))

   
 

;; Tests

(check-expect (augment-histogram (list (list 'a 1)) (list 'a 'b 'c) 200)
              (list
               (list 'a 1 199)
               (list 'b 0 200)
               (list 'c 0 200)))

(check-expect (augment-histogram empty (list 'a) 100)
              (list (list 'a 0 100)))
  
  




;; (entropy positive-counts negative-counts)
;; outputs the entropy of positive-counts
;; and negative-counts.

;; Examples:

(check-within
 (entropy (list 'large 126 59) (list 'large 146 669))
 #i0.5663948489858 0.001)

(check-within
 (entropy (list 'small 17 168) (list 'small 454 361))
 #i0.5825593868115 0.001)

(check-within
 (entropy (list 'a 0 100) (list 'b 100 0))
 0.0 0.001)

;; entropy:(list Sym Nat Nat) (list Sym Nat Nat) -> Num
(define (entropy positive-counts negative-counts)
  (local
    [(define (p-helper x y)
       (cond[(> (+ x y) 0) (/ x(+ x y))]
            [(= (+ x y) 0) 0.5]))]
  (local
    [(define (e-helper p)
       (cond[(and(> p 0)(<= p 1))(* (* -1 (log p 2)p))]
            [ (= p 0) 0]))]
  (+ (*
      (p-helper (+ (second positive-counts)
                    (second negative-counts))
                 (+ (third positive-counts)
                    (third negative-counts)))
       (+ (e-helper (p-helper (second positive-counts)
                                (second negative-counts)))
     (e-helper (p-helper (second negative-counts)
                         (second positive-counts)))))
       
       (* (p-helper (+ (third positive-counts)
                    (third negative-counts))
                 (+ (second positive-counts)
                    (second negative-counts)))
       (+ (e-helper (p-helper (third positive-counts)
                                (third negative-counts)))
     (e-helper (p-helper (third negative-counts)
                         (third positive-counts)))))))))

 

;; Tests:
(check-within (entropy (list 'large 126 59) (list 'large 146 669))
              0.5663948489858 0.001)

(check-within (entropy (list 'small 17 168) (list 'small 454 361))
              0.5825593868115 0.001)

(check-within (entropy (list 'a 0 100) (list 'b 100 0)) 0.0 0.01)






;; (entropy-attributes positive negative)
;; produces the entropy of every attribute
;; within positive and negative and produces
;; a list of attribute/entropy pairings.

;; Examples:

(check-within (entropy-attributes
               (list
                (list 'large 126 59) (list 'angry 161 24)
                (list 'small 17 168) (list 'flies 170 15)
                (list 'swims 162 23) (list 'medium 42 143))
               (list
                (list 'large 146 669) (list 'angry 469 346)
                (list 'small 454 361) (list 'flies 615 200)
                (list 'swims 365 450) (list 'medium 215 600)))
              (list
               (list 'large #i0.5663948489858)
               (list 'angry #i0.6447688190492)
               (list 'small #i0.5825593868115)
               (list 'flies #i0.6702490498564)
               (list 'swims #i0.6017998773730)
               (list 'medium #i0.6901071708677)) 0.001)

;; entropy-attributes: AH AH -> EAL
(define (entropy-attributes positive negative)
  (cond[(empty? positive) empty]
       [(empty? negative) empty]
       [ else (cons (list  (first(first positive))
                    (entropy (first positive)(first negative)))
                    (entropy-attributes (rest positive) (rest negative)))]))



;; Tests
(check-within (entropy-attributes
               (list
                (list 'large 126 59) (list 'angry 161 24)
                (list 'small 17 168) (list 'flies 170 15)
                (list 'swims 162 23) (list 'medium 42 143))
               (list
                (list 'large 146 669) (list 'angry 469 346)
                (list 'small 454 361) (list 'flies 615 200)
                (list 'swims 365 450) (list 'medium 215 600)))
              (list
               (list 'large #i0.5663948489858)
               (list 'angry #i0.6447688190492)
               (list 'small #i0.5825593868115)
               (list 'flies #i0.6702490498564)
               (list 'swims #i0.6017998773730)
               (list 'medium #i0.6901071708677)) 0.001)
 





;; (best-attribute entropies) returns
;; the attribute with the smallest
;; entropy in entropies

;; Examples:

(check-expect
 (best-attribute
  (list
   (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
   (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
   (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)))
               'large)

;; entropy-attributes: EAL -> Sym
(define (best-attribute entropies)
  (local
    [(define (min-finder lst x)
       (cond[(empty? lst) x]
            [ (< (second(first lst)) x) (min-finder (rest lst) (second(first lst)))]
            [ else (min-finder (rest lst) x)]))]
   (local
     [(define (find-x lst x)
        (cond[(empty? lst) empty]
             [ (=(second(first lst))x) (first(first lst))]
       [else (find-x (rest lst) x)]))]
       
  (find-x entropies (min-finder entropies 10000)))))


;; Tests
(check-expect (best-attribute
(list
 (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
 (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
 (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)))
              'large)



;; (build-dt examples label): produces a decision tree based on examples and label.

;; Examples:
;; -->  no examples to test.

;; build-dt: (listof Example) Sym -> DT
(define (build-dt examples label)
  (local[(define attributes (collect-attributes examples))
    (define positive-examples
      (cond[(empty?
             (split-examples examples label)) empty]
                                 [else (first(split-examples examples label))]))
    (define negative-examples
      (cond[ (empty? (split-examples examples label)) empty]
                                 [else(second(split-examples examples label))]))
      (define root-attribute (best-attribute
                        (entropy-attributes
                         (augment-histogram
                          (histogram positive-examples) attributes
                          (length positive-examples))
                         (augment-histogram
                          (histogram negative-examples) attributes
                          (length negative-examples)))))
                  
                    (define (remover lst sym)
                      (cond[(empty? lst) empty]
                           [else (cons (sing-remove
                                        (first lst) sym)
                                       (remover (rest lst) sym))]))
                    
                    (define (sing-remove lst sym)
                      (cond[(empty? lst) empty]
                           [(not(equal? (first lst) sym))
                            (cons (first lst) (sing-remove(rest lst) sym))]
                           [else (sing-remove (rest lst) sym)]))
                    (define (splitter lst sym) (split-examples lst root-attribute))
    
 
      (define (tree-maker examples attr)
         (cond[(empty? positive-examples) false]
              [(empty? negative-examples) true]
              [(empty? attr)
               (cond[(> (length positive-examples)
                        (length negative-examples)) true]
                    [else false])]
              [else
               (local
                 [
                  (define r-s (second (splitter examples root-attribute)))
                  
                   (define l-s (remover (first(splitter examples root-attribute))
                                        root-attribute))
                  
                  (define right-subtree (build-dt r-s label))
                  
                  (define left-subtree (build-dt l-s label))]
                  
                (cond[(equal? left-subtree right-subtree) left-subtree]
                [else (list root-attribute left-subtree right-subtree)]))]))]
    (tree-maker examples attributes)))
 
 




