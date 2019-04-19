#lang racket/gui
(require web-server/servlet
         web-server/servlet-env
         racket/path srfi/1)
 (require net/uri-codec)
[require unstable/port]
[require file/md5]
[require "markov.scm"]
(require mzlib/pregexp mzlib/string)
[require "stop-words.rkt"]


[define identity [λ [x] x]]
[define base-dir  [path->string [get-directory]] ];"z:/torrents/Completed Torrents"];
[define resources-dir "c:/users/user/dropbox/" ]
[define log [λ args [write args][newline]]]
[define [recurse-dir dir]
  
  [append [directory-list dir] 
          
          [append-map 
           [λ [x] 
             [map [λ [y] [build-path x y]]
                  [with-handlers [[[λ args #t] [λ args '[]]]]
                    
                    [recurse-dir [build-path  dir x]]]]]
           [directory-list dir]]]]

[define [tags-from-filename fname] [let [[a [regexp-replace "\\.|_" fname " "]]] [delete-duplicates [append
                                    ;[regexp-split " |\\.|-|_|/|\\\\|\\(|\\)|&" fname]
                                    ;[regexp-split "\\.|/|\\\\" fname]
                                    [regexp-split "\\-+|\\[+|\\]+" a]
                                    [regexp-split "\\-+| +|_+" a]
                                    [regexp-split "\\/+" fname]
                                    ]]]] 
[define tagcounts [make-hasheq]]
[define tags-to-files [new markov%]]
[define file-to-tags-cache [make-hasheq]]
[define total-selected-files 1]
[define selected-scored-tags [make-hasheq]]
[define [hash-increment a-hash a-key] [hash-set! a-hash a-key [add1 [hash-ref a-hash a-key 0]]]]
[log  [format "Scanning ~a" base-dir]]
[define quiet
  [begin
  [map [λ [fname] 
         [let [[tags  [map string->symbol [tags-from-filename fname]]]]
           [hash-set! file-to-tags-cache [string->symbol fname] tags]
           [map [λ [tag] 
                         [hash-increment tagcounts tag]
                         [send tags-to-files add tag [string->symbol fname]]] tags]]]  [map path->string  [recurse-dir base-dir]]]
  [map [lambda [x] [hash-remove! tagcounts x]
         [hash-remove! [send tags-to-files get-table] x]] [map string->symbol all-stop-words]]]]
[set! quiet #f]
;[write tagcounts]
[define selected-tags '[]]
;[put-preferences [list 'clipbook:archive-directory] [list [get-preference 'clipbook:archive-directory [lambda [] [path->string [get-directory]]]]]]
[define archive-directory [get-preference 'clipbook:archive-directory ]]

; HTML builders
; return XHTML structures that can be put together in a response/xhtml 
[define[ build-href link text]
  `[a [[href ,[format "/addtag/~a?selected=~a" link selected-tags]]] ,[format "~a(~a%)" text [round [* 100 [/ [hash-ref selected-scored-tags link [const 0]] [add1 total-selected-files]]]]]]]
[define[ build-download-link link text]
  `[a [[href ,[format "/getfile/~a" link ]]] ,text]]
[define [build-remove-href a-string text]
  `[a [[href ,[format "/removetag/~a?selected=~a" a-string [remove-tags [list a-string ] selected-tags]]] [title ,[format "Tag ~a adds ~a entries.  Click to remove." a-string 10]]] ,text]]
[define take-at-most [λ [a-num a-list] [if [< [length a-list] a-num]
                                           a-list
                                           [take a-list a-num]]]]
[define [build-selected-box]
  [cons 'div [append-map [λ [a-clip]
                           [list 
                            
                            [build-remove-href a-clip a-clip] ", "]] selected-tags]]]
[define [limit-list a-list]
  [if [< [length a-list] 30]
      a-list
      [begin
        
        [append [take-at-most 30 a-list] [list
                                          '[br]`[a [[href ,[format "/displayfilelist/blah?selected=~a" selected-tags]]] ,[format "And ~a more ..." [- [length a-list] 30]]]]]]]]

[define [build-download-box files limiter] 
  [cons 'div [limiter [append-map [λ [a-clip]
                                    [let [[ a-clip [symbol->string a-clip]]]
                                    [list
                                     [if [regexp-match "mp3" a-clip]
                                         `[audio [
                                                  [controls "controls"]
                                                  [preload "none"]
                                                  [autobuffer "true"]] 
                                                 
                                                 [source [ [ src ,[string-join [list "/getfile" [path->string [build-path a-clip]]] "/"]] [type "audio/mp3"]]
                                                         
                                         [object [[type "application/x-shockwave-flash"] [data "/resources/player_mp3_maxi.swf"] [width "200"]  [height "20"]]
                                                  [param [[name "movie"] [value "/resources/player_mp3_maxi.swf"]]]
                                                  [param [[name "FlashVars"] [value ,[format "showloading=always&mp3=~a"  [string-join [list "/getfile" [path->string [build-path a-clip]]] "/"]]]]]
                                                  ]]]
                                     [if [regexp-match "flv" a-clip]
                                         `[object [[type "application/x-shockwave-flash"] [data "/resources/flvmaxi.swf"] [width "320"]  [height "240"]]
                                                  [param [[name "movie"] [value "/resources/flvmaxi.swf"]]]
                                                  [param [[name "FlashVars"] [value ,[format "flv=~a"  [string-join [list "/getfile" [path->string [build-path a-clip]]] "/"]]]]]
                                                  ]
                                         [if [regexp-match "png|gif|jpg|jpeg" a-clip]
                                             `[img [[src  ,[format "/getfile/~a" a-clip ]] [width "64"][height "64"]] ]
                                             '[span]]]]
                                     [build-download-link a-clip [path->string[file-name-from-path a-clip]]] ", " '[br]]]]  files]]]]
[define build-info-response [lambda []
(response/xexpr
                 
                 `(html
                   (head (title "Internal details" ))
                   (body (h1 "Internal details")
                         
                         [div [[style "border:3px solid red"]]
                              [p [] "Scanned directory:" ,resources-dir]
                              ;[p [] "Number of items:" ,[format "~a" [length  files]]]
                              [p [] "Number of tags:" ,[format "~a" [length [hash-keys tagcounts]]]]])))]]

[define [build-tags-box files] 
  [cons 'div [append-map 
              [λ [a-clip]
                [list  [build-href a-clip a-clip] ", "]] files]]]



[define [build-footer]
  '[div []
        [a [[href "/info"]] "Info"]]]

[define [presets]
  '[div 
    [table
     [tr 
      [td [a [[href "/addtag/jpg?selected=(gif%20png)"]]  [img [[src "/resources/piccies.png"] [width "64"][height "64"]] ] [br] "Pictures"]]
      [td [a [[href "/addtag/mp?selected=(mp3)"]]  [img [[src "/resources/piccies.png"] [width "64"][height "64"]] ] [br] "Music"]]
      [td 
       [a [[href "/addtag/avi?selected=(mpg%20mpeg%20mp4%20flv)"]]  [img [[src "/resources/movies.png"] [width "64"][height "64"]] ] [br]"Movies"]]
      [td
       [a [[href "/addtag/doc?selected=(txt%20pdf)"]]  [img [[src "/resources/docs.png"] [width "64"][height "64"]] ] [br] "Documents"]]
      
      [td[a [[href "/addtag/mobi?selected=(ebook%20azw%20aeh%20lrf%20lrx%20cbr%20cbz%20cb7%20cbt%20cba%20djvu%20epub%20fb2%20pdf%20tr2%20tr3)"]]  [img [[src "/resources/books.png"] [width "64"][height "64"]] ] [br] "Books"]]]]]
  ]


[define [select-files selected-tags][filter 
                                     [λ [a-file] 
                     [equal? [length selected-tags ] 
                             [length [delete-duplicates [lset-intersection equal?  [hash-ref file-to-tags-cache a-file [const '[]]] selected-tags]]]]]
                                     ;[λ [X] [lset<= equal? selected-tags [tags-from-filename X] ]]
                   [delete-duplicates [append-map [λ [x] 
                                                                                                                               [hash-keys [spath-fail 
                                                     [list x]                                                                                      ;[list [format "~a" x]]
                                                                                                                                                      [send tags-to-files get-table] 
                                                                                                                                                      [λ args [make-hash]]]]] selected-tags]]]]
[define [remove-tags bad-tags tag-list]
  [remove [lambda [a-tag] [member a-tag bad-tags]] tag-list]]

[define [suggest-tags files]
  [let [[tags [make-hasheq]]]
  [map [lambda [a-file]
         [map [lambda [a-tag] [hash-increment tags a-tag]]  [hash-ref file-to-tags-cache a-file identity]]]
   files]
  [take-at-most 100 [sort [hash-keys tags] > #:key [λ [a-key] [hash-ref tags a-key]]]]]]

; The main request hander

[require mzlib/defmacro]
[require "spath.rkt"]
(define (start request)
  [log "Received request"]
  
  [letrec [
           [split-path [regexp-split "[/?]" [uri-decode [url->string [request-uri request]]]] ]
           [action [cadr split-path]]]
    [log "Url path is " split-path]
    [set! selected-tags [map [lambda [x] [string->symbol [if [symbol? x][symbol->string x][format "~a" x]]] ]
                             [read-from-string [=f selected  [request-bindings request] "[]"]]]]
    [when [equal? action "addtag"]
      [log "Adding tag " [caddr split-path]]
      [log "Selected tags: " selected-tags]
      [set! selected-tags [cons [string->symbol [caddr split-path]]  selected-tags]]]
    [log "Selected tags: " selected-tags]
    [let [[selected-files [select-files selected-tags]]]
      [set! total-selected-files [length selected-files]]
      [set! selected-scored-tags [make-hasheq]]
      [map [λ [fname] 
         [let [[tags  [hash-ref file-to-tags-cache fname]]]
           [map [λ [tag] [hash-increment selected-scored-tags tag]] tags]]]  selected-files]
      [if [equal? action "info"]
          [build-info-response]
           [if [equal? action "favicon.ico"]
          [begin [log "sending favicon"]
                 (response
                  200 #"favicon"
                  (current-seconds) #"image/vnd.microsoft.icon"
                  (list ) ; (make-header a b )
                  (λ (op) (copy-port [open-input-bytes favicon.ico ] op)))]
          [if [equal? action "displayfilelist"]
              [begin
                [log "Display File List" selected-files]
                (response/xexpr
                 
                 `(html
                   (head (title ,[last split-path] ))
                   (body (h1 ,[last split-path])
                         ,[build-download-box selected-files [λ [a]a]]
                         [div [[style "border:3px solid red"]]
                              "Related Tags"
                              ,[build-tags-box [suggest-tags selected-files]]]
                         [div [[style "border:3px solid red"]]
                              ,[build-download-box selected-files limit-list]]
                         
                         ,[build-footer])))]
              [if [equal? action "displayfile"]
                  [begin 
                    [log "displayfile" [caddr split-path]]
                    [let [[full-path [path->string [apply build-path [cons base-dir [cddr split-path]] ]]]]
                      [log "full path " full-path]
                      (response/xexpr
                       
                       `(html
                         (head (title ,[last split-path] ))
                         (body (h1 ,[last split-path])
                               [form [[method "post"]]
                                     [div [[style "border:3px solid red"]]
                                          "File Size: " ,[format "~a Mib" [exact->inexact [/ [file-size full-path] 1000000]]] [br]
                                          "Directory: " ,[string-join [reverse [drop [reverse split-path] 1]] "/"] [br]
                                          ,[build-download-link [string-join [cddr split-path] "/"] [last split-path] ]
                                          ]
                                     [div [[style "border:3px solid red"]] 
                                          ,[presets]]
                                     [div [[style "border:3px solid red"]] 
                                          ,[build-tags-box [take [sort [hash-keys tagcounts] > #:key [λ [a-key] [hash-ref tagcounts a-key]]] 50]]]
                                     [div [[style "border:3px solid red"]]
                                          ,[build-download-box selected-files limit-list]]]
                               ,[build-footer])))]
                    ]
                  [if [equal? action "resources"]
                      [begin 
                        [log "Resources file: " [path->string [apply build-path [cons resources-dir [cddr split-path]]]]]
                        
                        (response
                         200 #"Here it is"
                         (current-seconds) #"unknown/unknown"
                         (list ) ; (make-header a b )
                         (λ (op) (copy-port 
                                  [with-handlers [[[λ args #t] [λ args [write args]]]]
                                    [open-input-file [apply build-path [cons resources-dir [cddr split-path]]] #:mode 'binary ]] op)))]
                      [if [equal? action "getfile"]
                          [begin 
                            [log "getfile: " [path->string [apply build-path [cons base-dir [cddr split-path]]]]]
                            
                            (response
                             200 #"Here it is"
                             (current-seconds) #"unknown/unknown"
                             (list ) ; (make-header a b )
                             (λ (op) (copy-port 
                                      [with-handlers [[[λ args #t] [λ args [write args]]]]
                                        [open-input-file [apply build-path [cons base-dir [cddr split-path]]] #:mode 'binary ]] op)))]
                          
                          (response/xexpr
                           
                           `(html
                             (head (title "Tag Browser"))
                             (body (h1 "All tags")
                                   [form [[method "post"]]
                                         [div [[style "border:3px solid red"]] 
                                              ,[build-selected-box]
                                              ]
                                         [div [[style "border:3px solid red"]] 
                                              ,[presets]]
                                         [div [[style "border:3px solid red"]] "Global Tags"
                                              ,[build-tags-box [take [sort [hash-keys tagcounts] > #:key [λ [a-key] [hash-ref tagcounts a-key]]] 50]]]
                                         [div [[style "border:3px solid red"]]
                              "Related Tags"
                              ,[build-tags-box [take-at-most 10 [remove-tags selected-tags [suggest-tags selected-files]]]]]
                                         [div [[style "border:3px solid red"]]
                              "Related Tags Without Globals"
                              ,[build-tags-box [take-at-most 10 [remove-tags [take [sort [hash-keys tagcounts] > #:key [λ [a-key] [hash-ref tagcounts a-key]]] 50] [remove-tags selected-tags [suggest-tags selected-files]]]]]]
                                         
                                         [div [[style "border:3px solid red"]]
                                              ,[build-download-box selected-files limit-list]]]
                                   ,[build-footer])))]]]]]]]])

(serve/servlet start
               #:servlet-regexp #rx"" #:listen-ip #f #:port 61121 #:quit? #t)

[define favicon.ico #"\377\330\377\340\0\20JFIF\0\1\1\0\0\1\0\1\0\0\377\376\0<CREATOR: gd-jpeg v1.0 (using IJG JPEG v62), quality = 100\n\377\333\0C\0\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\377\333\0C\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\377\300\0\21\b\0\20\0\20\3\1\"\0\2\21\1\3\21\1\377\304\0\37\0\0\1\5\1\1\1\1\1\1\0\0\0\0\0\0\0\0\1\2\3\4\5\6\a\b\t\n\v\377\304\0\265\20\0\2\1\3\3\2\4\3\5\5\4\4\0\0\1}\1\2\3\0\4\21\5\22!1A\6\23Qa\a\"q\0242\201\221\241\b#B\261\301\25R\321\360$3br\202\t\n\26\27\30\31\32%&'()*456789:CDEFGHIJSTUVWXYZcdefghijstuvwxyz\203\204\205\206\207\210\211\212\222\223\224\225\226\227\230\231\232\242\243\244\245\246\247\250\251\252\262\263\264\265\266\267\270\271\272\302\303\304\305\306\307\310\311\312\322\323\324\325\326\327\330\331\332\341\342\343\344\345\346\347\350\351\352\361\362\363\364\365\366\367\370\371\372\377\304\0\37\1\0\3\1\1\1\1\1\1\1\1\1\0\0\0\0\0\0\1\2\3\4\5\6\a\b\t\n\v\377\304\0\265\21\0\2\1\2\4\4\3\4\a\5\4\4\0\1\2w\0\1\2\3\21\4\5!1\6\22AQ\aaq\23\"2\201\b\24B\221\241\261\301\t#3R\360\25br\321\n\26$4\341%\361\27\30\31\32&'()*56789:CDEFGHIJSTUVWXYZcdefghijstuvwxyz\202\203\204\205\206\207\210\211\212\222\223\224\225\226\227\230\231\232\242\243\244\245\246\247\250\251\252\262\263\264\265\266\267\270\271\272\302\303\304\305\306\307\310\311\312\322\323\324\325\326\327\330\331\332\342\343\344\345\346\347\350\351\352\362\363\364\365\366\367\370\371\372\377\332\0\f\3\1\0\2\21\3\21\0?\0\303\370\305\250'\355\a\342=C\3427\307\2153\342W\215\3741\342\235+\302Z\257\301\237\6x$x\255\274?\341=3S7\367\232\244\257m\341U\232\305\374iok.\210\326\263x\326#\341k\320\372\202\336|\251\2\307\344Z_\305\337\24~\307:\237\204\274u\360\307\301~<\323 \360\354\177\360\223~\321\326:\353\370\233O\360\227\210l\254t]N\363\307\26\326\0267\221\301\360\374]E\251[\332\305\360\356O\1\35\213g<\255\255\304Z\25-\364\247\304\235\n\317\366y\265O\204\177\31<ow\360\223M\265\261\261\320\274!\361Xj\232'\2074\315cG\322\36\336=>\347D\361/\212\254\257\3743\6\265.\237i\25\266\257\243\337A=\344\36e\323Ao\3455\275\342\374\301\256\3746\277\375\252\357\346\375\234\277g\337\213\332\357\306\324\361d\261\177\302y\361\eU\327|%\342\257\v\3747\360e\312=\206\261w{\257x\eB\3214o\264\334Z\3132\350\372\23\311u\253j\232\247\226\261}\236\312;\271\355\377\0\312>\27\343\0361\305\346\330^\30\314\362*q\303\323\305W\302W\314k\345\371\254a\375\236\253\322\366\271\272\307Q\240\262\312ui\321\346\255V\23\305\306\265|\306\225,G/2\205\n\237\335y\227\tp\235:u3\\7\20b\245\223O\v\34\306\236\36\226#\6\362\364\243F\\\231l\250T\304}jn\244\371h'\f;\234\25YS\3765\352C\377\331"]
