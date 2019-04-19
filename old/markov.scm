(module markov mzscheme 
  
  (provide
   markov<%> markov%)
  (require (lib "class.ss"))
  (require (prefix srfi- (lib "1.ss" "srfi")))
  (require (prefix srfi- (lib "13.ss" "srfi")))
  (require (lib "list.ss"))
  (require (lib "misc.ss" "swindle"))
  (define markov<%> (interface () add serialise insert-list similarity markov-table insert-list-pairs next-link popular-link deserialise insert-text normalised-similarity serialise-to-file load-from-file get-table))
;=item markov% object
;
;This object manages a markov table (implemented as a hash of hashes).  It includes a handy set of functions for accessing the table, or you can play with the table yourself by calling markov-table.
;
;The table currently expects that you will pass it strings.
;(new markov%)
(define markov% 
  (class* object% (markov<%>)
    ; Declare public methods that can be overridden:
    (public add serialise insert-list similarity markov-table insert-list-pairs next-link popular-link deserialise insert-text normalised-similarity serialise-to-file load-from-file get-table)
    
    (define table (make-hash-table 'equal))        ; A private field 
    [define [get-table ] table]
    
    ; Method implementations:
    ;=item (add state1 state2)
    ;
    ;The fundamental call for adding a transition.  If the states didn't exist before, they'll be added to the table, otherwise the transition probability will be increased.
    (define (add state1 state2)
;      (display (format "Markov-add called with key: ~a and value: ~a~n" state1 state2))
      (let ((transition-hash (hash-table-get table state1 (lambda () (make-hash-table 'equal)))))
        (let ((current-freq (hash-table-get transition-hash state2 (lambda () 0))))
          (hash-table-put! transition-hash state2 (add1 current-freq))
          (hash-table-put! table state1 transition-hash)
          )))

    ;=item (insert-list-pairs a-list)
    ;
    ;Inserts a list (usually made by breaking a text document up into words), but unlike insert-list, this function joins pairs of words together before inserting.  It is mostly useless.
    (define insert-list-pairs (lambda (a-list)
                     (if (> 5 (length a-list))
                         #f
                         
                             (begin 
                               (add (srfi-string-join (list (first a-list) (second a-list)) " ") (srfi-string-join (list (third a-list) (fourth a-list)) " "))
                                    (insert-list-pairs (cdr a-list))))))
    ;=item (insert-list a-list)
    ;
    ;Inserts a list (usually made by breaking a text document up into words).  Each item is assumed to transition to the following item, meaning there is always a chain that goes from one end of the list to the other.
    (define insert-list (lambda (a-list)
                     (if (not (pair? a-list))
                         #f
                         (if (not (pair? (cdr a-list)))
                             #f
                             (begin (add (car a-list) (cadr a-list))
                                    (insert-list (cdr a-list)))))))
    
    ;=item (insert-text a-string)
    ;
    ;insert-text will break up a document (on whitespace boundaries) and insert each word into the table using insert-list.
    (define insert-text (lambda ( a-text )
                          (insert-list (regexp-split  " +|\r\n|\r|\t|\n" a-text))))
                          
    

    ;=item (serialise)
    ;
    ;Writes the table out in a format that can be read back in with deserialise.
    (define serialise (lambda ()
                        (hash-table-map table 
                                        (lambda (key1 val1)
                                          (begin 
                                            ;(display (format "Transition table for ~a~n" key1))
                                            (cons key1
                                          (hash-table-map val1
                                                          (lambda (key2 val2)
                                                            (begin 
                                                              ;(display (format "key: ~a  freq: ~a~n" key2 val2))
                                                                   (cons key2 val2))))))))))
   
    ;=item (serialise-to-disk file-path)
    ;
    ;Writes the table out in a format that can be read back in with deserialise.
    (define serialise-to-file (lambda (file-path)
                        [call-with-output-file file-path [λ [out-port] [write [ serialise] out-port]] 'replace]))
   
    
    ;=item (deserialse a-serialisation)
    ;
    ;Reads in a structure written out by serialise.  It will destroy the existing table.
    (define deserialise (lambda (a-list)
                          (let ((master-table (make-hash-table 'equal)))
                          (map
                           (lambda (chain-list)
                             (let ((new-table (make-hash-table 'equal)))
                             (map
                              (lambda (a-pair)
                                (hash-table-put! new-table(car a-pair) (cdr a-pair)))
                              (cdr chain-list))
                               (hash-table-put! master-table (car chain-list) new-table)))
                           a-list)
                            (set! table master-table))))
                              
    
    [define load-from-file [λ [filename]
                             [call-with-input-file filename
                              [λ [in-port] [deserialise [read in-port]]]]]]
                              
                               
    ;=item (next-link a-string)
    ;
    ;Will pick a random transition for a-string from the table, respecting the probabilities of the transactions.  Returns the next string in a (any) chain or #f if there are no further links to be followed.
    (define next-link (lambda ( a-key )
                        (let ((forwards-table (hash-table-get table a-key (lambda () #f))))
                          (if (not forwards-table)
                              #f
                              (let ((keys (srfi-fold append '() (hash-table-map forwards-table (lambda (key val) (srfi-make-list val key)) ))))
                                
                        (nth keys (random (length keys))))))))
    ;=item (popular-link a-string)
    ;
    ;Returns the next state with the highest probability.
    (define popular-link (lambda ( a-key )
                        (let ((forwards-table (hash-table-get table a-key (lambda () #f))))
                          (if (not forwards-table)
                              #f
                              (let ((best-key #f) (val 0))
                                (hash-table-map forwards-table (lambda (key nval)
                                                  (if (> nval val)
                                                      (set! best-key key))))
                                best-key)))))
                                
                                
                        
    
    (define score 0)
    ;=item (similarity a-markov%-object)
    ;
    ;Implements a slightly deranged comparison of two markov objects.  It returns a number indicating how closely related two markov tables are.  The number isn't normalised, so you'll have to either play around a bit or normalise it, however it is usually the case that a negative number means the documents are NOT related, and a positive number means they are somewhat related.  The more positive the number, the more they are related.
    (define joint-similarity (lambda (other-markov-table our-table)
                       (set! score 0)
                       (hash-table-map our-table 
                                        (lambda (key1 val1)
                                          (begin 
                                            ;(display (format "Transition table for ~a~n" key1))
                                            (if (not (hash-table-get other-markov-table key1 (lambda () #f)))
                                                (set! score (- score 2))
                                                (begin
                                                  (hash-table-map val1
                                                          (lambda (key2 val2)
                                                            (begin 
                                                              ;(display (format "key: ~a  freq: ~a~n" key2 val2))
                                                                   (let ((other-val (hash-table-get
                                                                                     (hash-table-get other-markov-table key1 (lambda () (error "Impossible error")))
                                                                                     key2 (lambda () 0))))
                                                                     (if (equal? val2 other-val)
                                                                         (set! score (+ score 10))
                                                                     (set! score (+ score (abs (- val2 other-val) )))))))))))))
                       score))
    (define similarity (lambda (a-markov) (joint-similarity (send a-markov markov-table) table)))
    (define normalised-similarity (lambda (a-markov)
                                    (/ (joint-similarity (send a-markov markov-table) table) (joint-similarity table table))))
    (define markov-table (lambda () table))
    (super-new)
    )))