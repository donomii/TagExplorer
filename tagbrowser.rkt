#lang racket
(require web-server/servlet
         web-server/servlet-env
         racket/path srfi/1
         xml)
(require net/uri-codec)
;[require unstable/port]
[require file/md5]
[require "builders.rkt"]
[require "spath.rkt"]
[require "markov.scm"]
(require mzlib/pregexp mzlib/string)
[require "stop-words.rkt"]
(require web-server/managers/lru)
[define introduction #t]
[define page-length  15]

[define base-dir  #f]
(require web-server/safety-limits)
(define-for-syntax args (current-command-line-arguments))
(define-for-syntax GRAPHICS (or
                            (= (vector-length args) 0)
                            (equal? (vector-ref args 0) "--graphics")))

(define-syntax (if-graphics stx)
  (syntax-case stx ()
    ((_ debug-expr non-debug-expr)
     (if GRAPHICS
         #'debug-expr
         #'non-debug-expr))))

(if-graphics
   [begin
     [define graphics #t]
     [set! base-dir [path->string [get-directory]]]
     [require racket/gui/base]
     ]
   [begin [define graphics #f]
   [set! base-dir  (current-directory)]
]     )



[define mime-types '[[flv  . #"video/x-flv"]
                     [mp4  . #"video/mp4"]
                     [m3u8 . #"application/x-mpeg"]
                     [ts   . #"video/MP2T"]
                     [3gp  . #"video/3gpp"]
                     [mov  . #"video/quicktime"]
                     [avi  . #"video/x-msvideo"]
                     [wmv  . #"video/x-ms-wmv"]
                     [mp3  . #"audio/mpeg3"]
                     [m4v  . #"video/mp4"]]]

[define favicon.ico #"\377\330\377\340\0\20JFIF\0\1\1\0\0\1\0\1\0\0\377\376\0<CREATOR: gd-jpeg v1.0 (using IJG JPEG v62), quality = 100\n\377\333\0C\0\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\377\333\0C\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\377\300\0\21\b\0\20\0\20\3\1\"\0\2\21\1\3\21\1\377\304\0\37\0\0\1\5\1\1\1\1\1\1\0\0\0\0\0\0\0\0\1\2\3\4\5\6\a\b\t\n\v\377\304\0\265\20\0\2\1\3\3\2\4\3\5\5\4\4\0\0\1}\1\2\3\0\4\21\5\22!1A\6\23Qa\a\"q\0242\201\221\241\b#B\261\301\25R\321\360$3br\202\t\n\26\27\30\31\32%&'()*456789:CDEFGHIJSTUVWXYZcdefghijstuvwxyz\203\204\205\206\207\210\211\212\222\223\224\225\226\227\230\231\232\242\243\244\245\246\247\250\251\252\262\263\264\265\266\267\270\271\272\302\303\304\305\306\307\310\311\312\322\323\324\325\326\327\330\331\332\341\342\343\344\345\346\347\350\351\352\361\362\363\364\365\366\367\370\371\372\377\304\0\37\1\0\3\1\1\1\1\1\1\1\1\1\0\0\0\0\0\0\1\2\3\4\5\6\a\b\t\n\v\377\304\0\265\21\0\2\1\2\4\4\3\4\a\5\4\4\0\1\2w\0\1\2\3\21\4\5!1\6\22AQ\aaq\23\"2\201\b\24B\221\241\261\301\t#3R\360\25br\321\n\26$4\341%\361\27\30\31\32&'()*56789:CDEFGHIJSTUVWXYZcdefghijstuvwxyz\202\203\204\205\206\207\210\211\212\222\223\224\225\226\227\230\231\232\242\243\244\245\246\247\250\251\252\262\263\264\265\266\267\270\271\272\302\303\304\305\306\307\310\311\312\322\323\324\325\326\327\330\331\332\342\343\344\345\346\347\350\351\352\362\363\364\365\366\367\370\371\372\377\332\0\f\3\1\0\2\21\3\21\0?\0\303\370\305\250'\355\a\342=C\3427\307\2153\342W\215\3741\342\235+\302Z\257\301\237\6x$x\255\274?\341=3S7\367\232\244\257m\341U\232\305\374iok.\210\326\263x\326#\341k\320\372\202\336|\251\2\307\344Z_\305\337\24~\307:\237\204\274u\360\307\301~<\323 \360\354\177\360\223~\321\326:\353\370\233O\360\227\210l\254t]N\363\307\26\326\0267\221\301\360\374]E\251[\332\305\360\356O\1\35\213g<\255\255\304Z\25-\364\247\304\235\n\317\366y\265O\204\177\31<ow\360\223M\265\261\261\320\274!\361Xj\232'\2074\315cG\322\36\336=>\347D\361/\212\254\257\3743\6\265.\237i\25\266\257\243\337A=\344\36e\323Ao\3455\275\342\374\301\256\3746\277\375\252\357\346\375\234\277g\337\213\332\357\306\324\361d\261\177\302y\361\eU\327|%\342\257\v\3747\360e\312=\206\261w{\257x\eB\3214o\264\334Z\3132\350\372\23\311u\253j\232\247\226\261}\236\312;\271\355\377\0\312>\27\343\0361\305\346\330^\30\314\362*q\303\323\305W\302W\314k\345\371\254a\375\236\253\322\366\271\272\307Q\240\262\312ui\321\346\255V\23\305\306\265|\306\225,G/2\205\n\237\335y\227\tp\235:u3\\7\20b\245\223O\v\34\306\236\36\226#\6\362\364\243F\\\231l\250T\304}jn\244\371h'\f;\234\25YS\3765\352C\377\331"]
[define guess-mime-type [lambda [a-string]
                          [letrec [[a-suffix [substring a-string [- (string-length a-string) 3](string-length a-string)]]
                                   [result [assoc a-suffix mime-types]]]
                            [if result
                                result
                                #"application/octet-stream"
                                ]]]
  ]


[define [open-in-explorer a-path-string]
  [log [format "Shell command: explorer /select,~a" a-path-string]]
  [displayln [format "Shell command: explorer /select,\"~a\"" a-path-string]]
  [system [format "explorer /select,\"~a\"" a-path-string]]]

[define [open-with-default a-path-string]
  [log [format "Shell command: ~a" a-path-string]]
  [displayln [format "Shell command: \"~a\"" a-path-string]]
  [system [format "\"~a\"" a-path-string]]]


[define wrap-with-box [lambda [content]
[wrap-content `(div ((class "box")) "\r\n        "
                    ,content)
              ""
              
                        ]]]


[define identity [λ [x] x]]
;[define resources-dir "e:/programming/" ]
[define resources-dir [current-directory] ]
[define log-history ""]
[define log [λ args [write args][newline]
              [set! log-history [string-join [list log-history [format "~a~n"  args]] " "]]]]
[log "Chose base directory" base-dir]
[define [recurse-dir dir]
  [displayln dir]
  [append [directory-list dir] 
          
          [append-map 
           [λ [x] 
             [map [λ [y] [build-path x y]]
                  [with-handlers [[[λ args #t] [λ args '[]]]]
                    
                    [recurse-dir [build-path  dir x]]]]]
           [directory-list dir]]]]

[define [tags-from-filename rawname]  [letrec [[fname [string-downcase rawname]]
                                               [filename  [last [regexp-split "[\\/?]" fname]]]
                                          [with-spaces [regexp-replace "\\.|_" filename " "]]] 
                                      [delete-duplicates [append
                                                                                                      [regexp-split " |\\.|-|_|/|\\\\|\\(|\\)|&" filename]
                                                                                                      [pregexp-split "\\\\|/| " fname]
                                                                                                      [regexp-split "\\.|/|\\\\" filename]
                                                                                                      [regexp-split "\\-+|\\[+|\\]+" with-spaces]
                                                                                                      [regexp-split "\\-+| +|_+" with-spaces]
                                                                                                      ;[regexp-split "\\/+" filename]
                                                                                                      ]]]] 
[define tagcounts [make-hasheq]]
[define tags-to-files [new markov%]]
[define file-to-tags-cache [make-hasheq]]
[define total-selected-files 1]
[define selected-scored-tags [make-hasheq]]
[define page-number 0]
[define [hash-increment a-hash a-key] [hash-set! a-hash a-key [add1 [hash-ref a-hash a-key 0]]]]
[define full-search-results '[]]
[log  [format "Scanning ~a" base-dir]]
[define all-files-list '[]]
[define quiet
  [begin
    [map [λ [fname]
           ;[log fname]
           [let [[tags  [map string->symbol [tags-from-filename fname]]]]
             [hash-set! file-to-tags-cache [string->symbol fname] tags]
             ;[log [format "Storing ~s" [string->symbol fname]]]
             [map [λ [tag] 
                    [unless [is-stop-word? tag ]
                      [begin [hash-increment tagcounts tag]
                             [send tags-to-files add tag [string->symbol fname]]]]] tags]]]  [map path->string  [recurse-dir base-dir]]]
    [map [lambda [x] [hash-remove! tagcounts x]
           [hash-remove! [send tags-to-files get-table] x]] [map string->symbol all-stop-words]]
    [set! all-files-list [hash-keys file-to-tags-cache] ]]]
[set! quiet #f]
;[write tagcounts]
[define selected-tags '[]]
[define rejected-tags '[]]
[define pre-selected-tags '[gif png]]
;[put-preferences [list 'clipbook:archive-directory] [list [get-preference 'clipbook:archive-directory [lambda [] [path->string [get-directory]]]]]]
[define archive-directory [get-preference 'clipbook:archive-directory ]]

[define [limit-list a-list]
  [if [< [length a-list] 30000]
      a-list
      [begin
        
        [append [take-at-most  a-list 30] [list
                                           '[br]`[div [[style "clear: both"]] [a [[href ,[format "/displayfilelist/blah?selected=~s&or=~s" selected-tags pre-selected-tags]]] ,[format "And ~a more ..." [- [length a-list] 30]]]]]]]]]
[define [limit60 a-list]
  [if [< [length a-list] 60]
      a-list
      [take-at-most  a-list 60]]
  ]

[define [clip-to-page page-number a-list ] 
  [letrec [
           ;[total-pages [mod [length a-list] page-length]]
           [left-trimmed-list [drop a-list [* page-number page-length] ]]
           ]
    [take-at-most  left-trimmed-list page-length]]
    ]


;[define [build-tags-box files] 
;  [cons 'ul [cons '((style "margin-top:10px;")) [map 
;              [λ [a-clip]
;                `[li []  ,[build-href a-clip a-clip] ", "]] files]]]]
[define [presets]
  '[div 
    [table
     [tr 
      [td [a [[href [format "/display/?selected=~s" `(jpg gif png tiff)]]]  [img [[src "/resources/piccies.png"] [width "64"][height "64"]] ] [br] "Pictures"]]
      [td [a [[href "/display/?selected=~s" `(mp3 ogg flac)]]  [img [[src "/resources/piccies.png"] [width "64"][height "64"]] ] [br] "Music"]]
      [td 
       [a [[href "/display/?selected=~s" `(mpg mpeg mp4 flv)]]  [img [[src "/resources/movies.png"] [width "64"][height "64"]] ] [br]"Movies"]]
      [td
       [a [[href "/display/?selected=(txt%20pdf)"]]  [img [[src "/resources/docs.png"] [width "64"][height "64"]] ] [br] "Documents"]]
      
      [td[a [[href "/display/?selected=~s" `(ebook azw aeh lrf lrx cbr cbz cb7 cbt cba djvu epub fb2 pdf tr2 tr3)]]  [img [[src "/resources/books.png"] [width "64"][height "64"]] ] [br] "Books"]]
      [td[a [[href "/display/?selected=~s" `(ebook azw aeh lrf lrx cbr cbz cb7 cbt cba djvu epub fb2 pdf tr2 tr3)]]  [img [[src "/resources/books.png"] [width "64"][height "64"]] ] [br] "Clear Presets"]]
      ]]]
  ]


[define [select-files selected-tags rejected-tags]
  [filter 
   [λ [a-file] 
     [equal? [length selected-tags ] 
             ;[length [delete-duplicates [lset-intersection eq?  [hash-ref file-to-tags-cache a-file [const '[]]] selected-tags]]]]]
             [length  [lset-intersection eq?  [hash-ref file-to-tags-cache a-file [const '[]]] selected-tags]]]]
    [filter
     [λ [a-file]
       [equal? 0 [length  [lset-intersection eq?  [hash-ref file-to-tags-cache a-file [const '[]]] rejected-tags]]]]
   ;[λ [X] [lset<= equal? selected-tags [tags-from-filename X] ]]
   [if [empty? pre-selected-tags]
       [begin
         [displayln "Pre-selected all files"]
         ;[displayln [hash-keys file-to-tags-cache]]
         all-files-list]
       [begin
         [displayln [format "Preselecting: ~a" pre-selected-tags]]
         ;rewrite this to update a hash rather than a list
         ;the count could be part of the recommendations - the number of tags that match that file might be a good indicator of relevence
         [letrec [[tagshash [make-hasheq]]]
           [map [λ [x] 
                  [map [lambda [atag] [hash-set! tagshash atag [hash-ref tagshash atag 0]]]
                       [hash-keys [spath-fail 
                                   [list x]                                                                                      ;[list [format "~a" x]]
                                   [send tags-to-files get-table] 
                                   [λ args [make-hash]]]]]] pre-selected-tags]
           [hash-keys tagshash]]]]]]]
[define build-export-response [lambda []
                              (response/full
                          200
                          #"OK"
                          (current-seconds)
                          #"text/plain"
                          (list ) ; (make-header a b )
                           [list [string->bytes/utf-8 [string-join [map symbol->string [select-files selected-tags]] [format "~n"]]]])]]
[define [remove-tags bad-tags tag-list]
  [remove [lambda [a-tag] [member a-tag bad-tags]] tag-list]]

[define [suggest-tags files]
  [let [[tags [make-hasheq]]]
    [map [lambda [a-file]
           [map [lambda [a-tag] [hash-increment tags a-tag]]  [hash-ref file-to-tags-cache a-file identity]]]
         files]
    [take-at-most  [sort [hash-keys tags] > #:key [λ [a-key] [hash-ref tags a-key]]] 100]]]

[define [tags-for-file a-file]
  [hash-ref file-to-tags-cache a-file [const [list a-file]]]]

; The main request hander

[require mzlib/defmacro]

(define (start request)
  [log "Received request" [url->string [request-uri request]]]
  
  [letrec [
           [split-path [regexp-split "[\\/?]" [uri-decode [url->string [request-uri request]]]] ]
           [action [cadr split-path]]]
    [log "Decoded path " split-path]
    [set! selected-tags [map [lambda [x] [string->symbol [if [symbol? x][symbol->string x][format "~a" x]]] ]
                             [read-from-string [=f selected  [request-bindings request] "[]"]]]]
    [set! rejected-tags [map [lambda [x] [string->symbol [if [symbol? x][symbol->string x][format "~a" x]]] ]
                             [read-from-string [=f rejected  [request-bindings request] "[]"]]]]
    [log [format "Selected tags: ~s" selected-tags]]
    [set! pre-selected-tags [map [lambda [x] [string->symbol [if [symbol? x][symbol->string x][format "~a" x]]] ]
                                 [read-from-string [=f or  [request-bindings request] "[]"]]]]
    [set! page-number [string->number
                                  [=f page  [request-bindings request] "0"]]]
    
    [when [equal? action "search"]
      [log "Searching for tag " [caddr split-path]]
      [log "Selected tags: " selected-tags]
      [log "Union tags: " pre-selected-tags]
      [set! selected-tags [cons  [read-from-string [=f q  [request-bindings request] "[]"]]  selected-tags]]]
    [when [equal? action "addtag"]
      [log "Adding tag " [caddr split-path]]
      [log "Selected tags: " selected-tags]
      [log "Union tags: " pre-selected-tags]
      [set! selected-tags [cons [string->symbol [caddr split-path]]  selected-tags]]]
    [set! full-search-results [select-files   selected-tags rejected-tags]]
    [let [[selected-files [clip-to-page page-number full-search-results]]]
      [set! total-selected-files [length selected-files]]
      [log [format "Selected ~a files" total-selected-files ]]
      [set! selected-scored-tags [make-hasheq]]
      [map [λ [fname] 
             [let [[tags  [hash-ref file-to-tags-cache fname]]]
               [map [λ [tag] [hash-increment selected-scored-tags tag]] tags]]]  selected-files]
      [case action
        [[ "export"]
          [build-export-response]]
      [[ "info"]
          [build-info-response]]
      [[ "explorer"]
              [begin [log "Open in explorer: " [path->string [apply build-path [cons base-dir  [cddr split-path]]]]]
                     [open-in-explorer [path->string [apply build-path [cons base-dir [cddr split-path]]]]]
                     (response/xexpr [wrap-content `[span []  "Your file has been opened in explorer.  Please press the back button to return to your search." ] "" [make-context selected-files presets tagcounts remove-tags selected-tags rejected-tags suggest-tags limit-list page-number full-search-results]])]]
         [[ "start"]
              [begin [log "Start: " [path->string [apply build-path [cons base-dir  [cddr split-path]]]]]
                     [open-with-default [path->string [apply build-path [cons base-dir [cddr split-path]]]]]
                     (response/xexpr [wrap-content `[span []  "Your file has been ;aunched in the default app.  Please press the back button to return to your search." ] "" [make-context selected-files presets tagcounts remove-tags selected-tags rejected-tags suggest-tags limit-list page-number full-search-results]])]]
      [["tagexplorer"]
              [begin [log "Preparing tagexplorer response"]
                     [build-tagexplorer-response]]]
      [["favicon.ico"]
                  [begin [log "sending favicon"]
                         (response
                          200 #"favicon"
                          (current-seconds) #"image/vnd.microsoft.icon"
                          (list ) ; (make-header a b )
                          (λ (op) (copy-port [open-input-bytes favicon.ico ] op)))]]
                  [["displayfilelist"]
                      [begin
                        [log "Display File List" selected-files]
                        (response/xexpr
                         
                         [wrap-content `[span (h1 ,[last split-path])
                                              ,[build-download-box selected-files [λ [a]a]]
                                              [div [[style "border:3px solid red"]]
                                                   "Related Tags"
                                                   ,[build-tags-box [suggest-tags selected-files]]]
                                              [div [[style "border:3px solid red"]]
                                                   ,[build-download-box selected-files limit-list]]
                                              
                                              ,[build-footer]]
                                       ""
                                       [make-context selected-files presets tagcounts remove-tags selected-tags rejected-tags suggest-tags limit-list page-number full-search-results]])]]
                      [["displayfile"]
                          [begin 
                            [log "displayfile" [caddr split-path]]
                            [let [[full-path [path->string [apply build-path [cons base-dir [cddr split-path]] ]]]]
                              [log "full path " full-path]
                              (response/xexpr
                               
                               `(html
                                 (head (title ,[last split-path] ))
                                 (body (h1 ,[last split-path])
                                       [form [[method "post"]]
                                             ,[build-file-info split-path base-dir remove-tags tagcounts tags-for-file]
                                             [div [[style "border:3px solid red"]] 
                                                  ,[presets]]
                                             [div [[style "border:3px solid red"]] 
                                                  ,[build-tags-box [take [sort [hash-keys tagcounts] > #:key [λ [a-key] [hash-ref tagcounts a-key]]] 50]]]
                                             [div [[style "border:3px solid red"]]  "Suggested tags"
                                                  ,[build-tags-box [take-at-most [remove-tags [take-at-most [sort [hash-keys tagcounts] > #:key [lambda [a-key] [hash-ref tagcounts a-key]]] 50] [remove-tags selected-tags [suggest-tags selected-files]]]10 ]]]
                                             [div [[style "border:3px solid red"]]  "Tags for file"
                                                  ,[build-tags-box [take-at-most [remove-tags [take-at-most [sort [hash-keys tagcounts] > #:key [lambda [a-key] [hash-ref tagcounts a-key]]] 50] [remove-tags selected-tags [tags-for-file [string->symbol [path->string [apply build-path [cddr split-path] ]]]]]]10 ]]]
                                             
                                             [div [[style "border:3px solid red"]]
                                                  ,[build-download-box selected-files limit-list]]]
                                       ,[build-footer])))]
                            ]]
                          [["resources"]
                              [begin 
                                [log "Resources file: " [path->string [apply build-path [cons resources-dir [cddr split-path]]]]]
                                
                                (response
                                 200 #"Here it is"
                                 (current-seconds) [guess-mime-type [last [cddr split-path]] ]
                                 (list [make-header #"Content-Disposition"
                                                    [string->bytes/utf-8 [format "attachment; filename=\"~a\"" [last [cddr split-path]] ]]] ) ; (make-header a b )
                                 (λ (op) (copy-port 
                                          [with-handlers [[[λ args #t] [λ args [write args]]]]
                                            [open-input-file [apply build-path [cons resources-dir [cddr split-path]]] #:mode 'binary ]] op)))]]
                              [["getfile"]
                                  [begin 
                                    [log "getfile: " [path->string [apply build-path [cons base-dir [cddr split-path]]]]]
                                    
                                    (response
                                     200 #"Here it is"
                                     (current-seconds) #"unknown/unknown"
                                     (list (make-header (string->bytes/utf-8 "Content-Length") (string->bytes/utf-8 (format "~a" (file-size [apply build-path [cons base-dir [cddr split-path]]])))) ) ; (make-header a b )
                                     (λ (op)
 [with-handlers [[[λ args #t] [λ args [write args]]]]
   [call-with-input-file [apply build-path [cons base-dir [cddr split-path]]]

     [lambda [inport] (copy-port inport op)]
     #:mode 'binary ]
                                       ]))]]
                                 [else  
                                  ;The main page drawing routine
                                  
                                  [begin ;[log [xexpr->string [build-results-page selected-files]]]
                                         (response/xexpr
                                                                            
                                          [build-results-page (make-context selected-files presets tagcounts remove-tags selected-tags rejected-tags suggest-tags limit-list page-number full-search-results )]
                                          )]]]]]
    ;[log "Call complete"]
    )

[if graphics
(serve/servlet start
               #:launch-browser? #t #:servlet-regexp #rx"" #:listen-ip "0.0.0.0" #:port 61120 #:quit? #t  #:stateless?   #t #:safety-limits (make-unlimited-safety-limits) 	;#:manager (make-threshold-LRU-manager #f (* 512 1024 1024))
               )
(serve/servlet start
               #:launch-browser? #f #:servlet-regexp #rx"" #:listen-ip "0.0.0.0" #:port 61120 #:quit? #t  #:stateless?   #t #:safety-limits (make-unlimited-safety-limits)	;#:manager (make-threshold-LRU-manager #f (* 512 1024 1024))
               )]



;(div ((id "main-image")) (img ((alt "I love Pets") (height "222") (src "/resources/images/lady.jpg") (width "153"))))
