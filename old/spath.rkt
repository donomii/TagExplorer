(module spath racket 
  ;This module provides spath, the simple scheme data accessor.  Spath provides a single, unified interface to all data types
  
  (provide spath spath-fail   s = sl =λ sf =f) 
  (require (lib "class.ss"))
  [require mzlib/defmacro]
  ;(require (prefix srfi- (lib "1.ss" "srfi")))
  ;(require (prefix srfi- (lib "13.ss" "srfi")))
  [require srfi/1 srfi/13]
  ;(require (lib "list.ss"))
  ;(require (lib "misc.ss" "swindle"))
  
  
  
  ;[require mzlib/defmacro]
  
  ;=item [spath '[path list] data]
  ; spath walks through the nested data structure, choosing the correct accessor function for each data type encountered.
  
  
  [define spath [lambda [path-list a-struct] [hh path-list a-struct]]]
  [define spath-fail  [lambda [path-list a-struct a-thunk] [with-handlers [[[λ args #t] [λ args [a-thunk]]]] [hh path-list a-struct]]]]
  
  [define hh [lambda [path-list a-struct]
               [if [> [length path-list] 0]
                   [letrec [[a-key [car path-list]]
                            [a-value 
                             [if [vector? a-struct]
                                 [vector-ref a-struct [string->number a-key]]
                                 [if [dict? a-struct]
                                     
                                     [dict-ref a-struct a-key]
                                     [if [struct? a-struct]
                                         [if [string->number a-key]
                                             [vector-ref [struct->vector a-struct] [add1 a-key]]
                                             [let-values [[[type dont-give-a-fuck]  [struct-info a-struct]]]
                                               ;why god why would anyone use values?
                                               [write type]
                                               [eval-syntax  [datum->syntax [quote-syntax here] [list [string->symbol [format "~a-~a" [object-name type] a-key]] a-struct]] ]
                                               ;[call-with-values [lambda [] [struct-type-info type]] [lambda args [[cadddr args] a-struct a-key]]]
                                               ]]
                                         [list-ref a-struct a-key]]
                                     ]
                                 ]]]
                     [hh [cdr path-list] a-value]
                     ]
                   a-struct]]]
  
  [define spath-recurse-permissive [lambda [path-list a-struct]
                                     [if [> [length path-list] 0]
                                         [letrec [[a-key [car path-list]]
                                                  [a-value 
                                                   [if [vector? a-struct]
                                                       [vector-ref a-struct [string->number a-key]]
                                                       [if [dict? a-struct]
                                                           [with-handlers [[[λ args #t] [lambda [e] [dict-ref a-struct [string->symbol a-key]]]]]
                                                           [with-handlers [[[λ args #t] [lambda [e] [dict-ref a-struct [symbol->string a-key]]]]]
                                                             [dict-ref a-struct a-key]]]
                                                           
                                                           [if [struct? a-struct]
                                                               [if [string->number a-key]
                                                                   [vector-ref [struct->vector a-struct] [add1 [string->number a-key]]]
                                                                   [let-values [[[type dont-give-a-fuck]  [struct-info a-struct]]]
                                                                     ;why god why would anyone use values?
                                                                     ;[write type]
                                                                     [eval-syntax  [datum->syntax [quote-syntax here] [list [string->symbol [format "~a-~a" [object-name type] a-key]] a-struct]] ]
                                                                     ;[call-with-values [lambda [] [struct-type-info type]] [lambda args [[cadddr args] a-struct a-key]]]
                                                                     ]]
                                                               [if [bytes? a-struct]
                                                                   [bytes-ref a-struct [car path-list] ]
                                                               [list-ref a-struct [string->number a-key]]]]
                                                           ]]]]
                                           [spath-recurse-permissive [cdr path-list] a-value]
                                           ]
                                         a-struct]]]
  [define h [lambda [a-path a-struct]
              ;[write a-path][newline]
              [if [list? a-path] [hh a-path a-struct]
                  [letrec [[string-path [if [symbol? a-path] [symbol->string a-path] a-path]]
                           [path-list  [regexp-split "/" string-path]]]
                    [spath-recurse-permissive  path-list a-struct]
                    ]
                  ]]]
  
  [define s [lambda [a-path a-struct] [h [symbol->string a-path] a-struct] ]]
  [define sl [λ [a-path a-struct a-thunk] [with-handlers [[[λ args #t] [λ args [a-thunk]]]] [s a-path a-struct]]]]
  [define sf [λ [a-path a-struct a-val] [with-handlers [[[λ args #t] [λ args a-val]]] [s a-path a-struct]]]]
  
  [define-macro [= aaa bbb] `[s [quote ,aaa] ,bbb]]
  [define-macro [=λ aaa bbb ccc] `[sl [quote ,aaa] ,bbb ,ccc]]
  [define-macro [=f aaa bbb ccc] `[sf [quote ,aaa] ,bbb ,ccc]]
  
  
  [spath '[2 "bananas"] '[ "b" "c" [[apples . "yes"] ["bananas" "no"]] ]]
  (struct posn (x y [z #:auto])
    #:auto-value 0
    #:transparent)
  [h 'y/1 [posn 1 '[a b c]]]
  
  [= y/2 [posn 1 '[a b c]]]
  
  )