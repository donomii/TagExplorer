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
(require web-server/managers/lru)
[define introduction #t]

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
[define guess-mime-type [lambda [a-string]
                          [letrec [[a-suffix [substring a-string [- (string-length a-string) 3](string-length a-string)]]
                                   [result [assoc a-suffix mime-types]]]
                            [if result
                                result
                                #"application/octet-stream"
                                ]]]
  ]

[define build-results-page-old [lambda [selected-files ]
                                 `(html
                                   (head (title "Tag Browser"))
                                   (body (h1 "All tags")
                                         [form [[method "post"]]
                                               [div [[style "border:3px solid red"]] 
                                                    ,[build-selected-box]
                                                    ]
                                               [div [[style "border:3px solid red"]] 
                                                    ,[presets]]
                                               [div [[style "border:3px solid red"]] "Common Tags"
                                                    ,[build-tags-box [take [sort [hash-keys tagcounts] > #:key [lambda [a-key] [hash-ref tagcounts a-key]]] 50]]
                                                    ]
                                               [div [[style "border:3px solid red"]]
                                                    "Related Tags"
                                                    ,[build-tags-box [take-at-most  [remove-tags selected-tags [suggest-tags selected-files]]10]]]
                                               [div [[style "border:3px solid red"]]
                                                    "Related Tags Without Globals"
                                                    ,[build-tags-box [take-at-most [remove-tags [take [sort [hash-keys tagcounts] > #:key [lambda [a-key] [hash-ref tagcounts a-key]]] 50] [remove-tags selected-tags [suggest-tags selected-files]]]10 ]]]
                                               
                                               [div [[style "border:3px solid red"]]
                                                    ,[build-download-box selected-files limit-list]]]
                                         ,[build-footer]))]]

[define build-results-page [lambda [selected-files]
                             
                             [wrap-content `(div ((class "box")) "\r\n        "
                                  
                                  ,[if [empty? selected-files] `[br]                                                                                       `(h2 () "Your search results") ] ,[if [empty? selected-files] `[br]                                                   `(p () (br ()) ,[if [not [empty? selected-files]] [build-download-box selected-files limit-list]`[br]])]
                                  ,[if [not [empty? selected-files]] `(div ((class "box")) "\r\n        "(h1 () "Related Tags") "\r\n        " (p () ,[build-tags-box [take-at-most [remove-tags [take-at-most [sort [hash-keys tagcounts] > #:key [lambda [a-key] [hash-ref tagcounts a-key]]] 50] [remove-tags selected-tags [suggest-tags selected-files]]]10 ]] ) )`[br]]
                                  
                                  ,[if  [not [empty? selected-files]]  `(div ((class "box")) "\r\n        "(h1 () "Extra") "\r\n        " (p () ,[build-tags-box [take-at-most  [remove-tags selected-tags [suggest-tags selected-files]]10]] )) `[br]]
                                  
                                  )]]]
[define wrap-content [lambda [content]
                       `(html ((xmlns "http://www.w3.org/1999/xhtml")) "\r\n" 
                              (head () "\r\n" 
                                    (meta ((content "text/html; charset=iso-8859-1") (http-equiv "Content-Type"))) "\r\n" 
                                    (title () "TagBrowser") "\r\n" 
                                    (link ((href "/resources/style.css") (rel "stylesheet") (type "text/css"))) "\r\n") "\r\n" 
                                    (body () "\r\n" 
                                          (div ((id "wrapper")) "\r\n  " 
                                               (div ((id "header")) "\r\n    " 
                                                    (div ((id "nav")) 
                                                         (a ((href "index.html")) "Home") " " nbsp "|" nbsp " " 
                                                         (a ((href "/resources/")) "Resources") " " nbsp "|" nbsp " " 
                                                         (a ((href "#")) "About us") " " nbsp "|" nbsp " " 
                                                         (a ((href "/info")) "Info") " " nbsp "|" nbsp " " 
                                                         (a ((href "#")) "Latest News") " " nbsp "|" nbsp " " 
                                                         (a ((href "#")) "Contact us") nbsp "|" nbsp " " 
                                                         (a ((href "#")) [span [[class ""]]"Search" 
                                                                               [form [
                                                                                      [action "/search/"] 
                                                                                      [method "get"]] 
                                                                                     [input [[type "text"] 
                                                                                             [name "q"] 
                                                                                             [autofocus "autofocus"] 
                                                                                             [placeholder "Search"]]] 
                                                                                     [input [[type "hidden"] 
                                                                                             [name "selected"] 
                                                                                             [value ,[format "~a" selected-tags]] 
                                                                                             [style "visibility: hidden;display: none;"]]]
                                                                                     [input [[type "hidden"] 
                                                                                             [name "or"] 
                                                                                             [value ,[format "~a" pre-selected-tags]] 
                                                                                             [style "visibility: hidden;display: none;"]]] 
                                                                                     [input [[type "submit"] [style "visibility: hidden;display: none;"]]] ]])) "\r\n    "
                                                                                     (div ((id "bg"))) "\r\n  ") "\r\n  " 
                                                                                     (div ((id "main-content")) "\r\n    " 
                                                                                          (div ((id "left-column")) "\r\n      "
                                                                                               ,[if introduction [begin [set! introduction #f] 
                                                     `(div ((class "box")) "\r\n        "
                                                           (h1 () "How to use TagBrowser") "\r\n        " 
                                                           (p ()  "Tagbrowser is a quick and convenient way to organise your files.  Tagbrowser lets you select keywords to quickly refine your search to find the right files." ) [p [] "To start, select a keyword from the lists below, or a type of file, from the list to right."]    "\r\n      ")] `[br]]
                                                                                           ,content "\r\n      "
                                                                                               "\r\n    ") "\r\n    " 
                                                                                           (div ((id "right-column")) "\r\n      " 
                                                                                                ;(div ((id "main-image")) (img ((alt "I love Pets") (height "222") (src "/resources/images/lady.jpg") (width "153"))))
                                                                                                
                                                                                                "\r\n      " (div ((class "sidebar")) "\r\n        "  "\r\n        " (h3 () "Presets") 
                                                                                                                  "\r\n        " 
                                                                                                                  (div ((class "box")) "\r\n          " 
                                                                                                                       (ul () "\r\n            " 
                                                                                                                           (li () [a [[href "/display/?selected=()&or=(gif%20png%20jpg%20jpeg)"]]  "Pictures"]) "\r\n            " 
                                                                                                                           (li () [a [[href "/display/?selected=()&or=(mp3%20ogg%20wav%20flac)"]]  "Music"]) "\r\n            " 
                                                                                                                           (li () [a [[href "/display/?selected=()&or=(mpg%20mpeg%20mp4%20flv%20avi%20avi)"]]  "Video"]) "\r\n            " 
                                                                                                                           (li () [a [[href "/display/?selected=()&or=(ebook%20azw%20aeh%20lrf%20lrx%20cbr%20cbz%20cb7%20cbt%20cba%20djvu%20epub%20fb2%20pdf%20tr2%20tr3)"]]  "Books"] " ") "\r\n            "
                                                                                                                           (li () [a [[href ,[format "/display/?selected=~s&or=()" selected-tags]]]  "Clear Presets"]) "\r\n            "
                                                                                                                           "\r\n          ") "\r\n        ")
                                                                                                                  (h3 () "About this Site") "\r\n        " (p () "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. In egestas nisl sit amet odio. Duis iaculis metus eu nulla. Donec venenatis sapien sed urna. Donec et felis ut elit elementum pellentesque. Praesent bibendum turpis semper lacus.")
                                                                                                                  
                                                                                                                  (h1 () "Global Tags") "\r\n        "(p () ,[build-tags-box [sort [take-at-most [sort [hash-keys tagcounts] > #:key [lambda [a-key] [hash-ref tagcounts a-key]]] 50] string<? #:key [lambda [x][symbol->string x]]]] (span ((class "crd")) (a ((href "http://www.web-designers-directory.org/") (target "_blank")) " web site design")) " credit links  in the footer of this template intact. Here is how a list looks like: ") "\r\n        " (ul ((style "margin-top:10px;")) "\r\n          " (li () "Lorem ipsum dolor sit amet, consetetur sadipscing.") "\r\n          " (li () "Tempor invidunt ut labore et dolore.") "\r\n          " (li () "At vero eos et accusam et justo duo dolores et ea rebum.") "\r\n          " (li () "Stet clita kasd gubergren, no sea takimata sanctus.") "\r\n          " (li () "Stet clita kasd gubergren") "\r\n        ") "\r\n      "                                                                                                                                                                                                                                                                                             (a ((href "http://www.web-designers-directory.org/"))) (a ((href "http://www.medicine-pet.com/"))) "\r\n      ") "\r\n    ") "\r\n  ") "\r\n  " (div ((id "footer")) "Copyright " copy " 2006 Your Company Name, All rights reserved." (br ()) "\r\n    " (a ((href "http://validator.w3.org/check?uri=referer") (target "_blank")) "XHTML") "  |  " (a ((href "http://jigsaw.w3.org/css-validator/check/referer?warning=no&profile=css2") (target "_blank")) "CSS") "  - Thanks to: " (a ((href "http://www.medicine-pet.com/") (target "_blank")) "Pet Medicine") " | " (span ((class "crd")) (a ((href "http://www.web-designers-directory.org/")) "Web site Design")) " by : " (a ((href "http://www.web-designers-directory.org/") (target "_blank")) "WDD")) "\r\n") "\r\n\r\n") "\r\n")]]

[define identity [λ [x] x]]
[define base-dir  [path->string [get-directory]] ];"z:/torrents/Completed Torrents"];
;[define resources-dir "e:/programming/" ]
[define resources-dir [current-directory] ]
[define log-history ""]
[define log [λ args [write args][newline]
              [set! log-history [string-join [list log-history [format "~a~n"  args]] " "]]]]
[define [recurse-dir dir]
  
  [append [directory-list dir] 
          
          [append-map 
           [λ [x] 
             [map [λ [y] [build-path x y]]
                  [with-handlers [[[λ args #t] [λ args '[]]]]
                    
                    [recurse-dir [build-path  dir x]]]]]
           [directory-list dir]]]]

[define [tags-from-filename fname]  [let [[a [regexp-replace "\\.|_" fname " "]]] [delete-duplicates [append
                                                                                                      [regexp-split " |\\.|-|_|/|\\\\|\\(|\\)|&" fname]
                                                                                                      [regexp-split "\\.|/|\\\\" fname]
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
             ;[log [format "Storing ~s" [string->symbol fname]]]
             [map [λ [tag] 
                    [unless [is-stop-word? tag ]
                      [begin [hash-increment tagcounts tag]
                             [send tags-to-files add tag [string->symbol fname]]]]] tags]]]  [map path->string  [recurse-dir base-dir]]]
    [map [lambda [x] [hash-remove! tagcounts x]
           [hash-remove! [send tags-to-files get-table] x]] [map string->symbol all-stop-words]]]]
[set! quiet #f]
;[write tagcounts]
[define selected-tags '[]]
[define pre-selected-tags '[gif png]]
;[put-preferences [list 'clipbook:archive-directory] [list [get-preference 'clipbook:archive-directory [lambda [] [path->string [get-directory]]]]]]
[define archive-directory [get-preference 'clipbook:archive-directory ]]

; HTML builders
; return XHTML structures that can be put together in a response/xhtml 
[define[ build-href link text]
  ;  `[a [[href ,[format "/addtag/~a?selected=~a&or=~a" link selected-tags pre-selected-tags]]] ,[format "~a(~a%)" text [round [* 100 [/ [hash-ref selected-scored-tags link [const 0]] [add1 total-selected-files]]]]]]
  `[a [[href ,[format "/display/?selected=~s&or=~s" [cons link selected-tags ] pre-selected-tags]]] ,[format "~a" text ]]
  
  ]
[define[ build-download-link link text]
  `[a [[href ,[format "/getfile/~a" link ]]] ,text]]
[define[ build-download-link-string link text]
  [format "/getfile/~a" link ]]
[define [build-remove-href a-string text]
  `[a [[href ,[format "/removetag/~a?selected=~a&or=~a" a-string [remove-tags [list a-string ] selected-tags] pre-selected-tags]] [title ,[format "Tag ~a adds ~a entries.  Click to remove." a-string 10]]] ,text]]
[define take-at-most [λ [ a-list a-num] [if [< [length a-list] a-num]
                                            a-list
                                            [take a-list a-num]]]]
[define [build-selected-box]
  [cons 'span [append-map [λ [a-clip]
                            [list 
                             
                             [build-remove-href a-clip a-clip] " "]] selected-tags]]]
[define [limit-list a-list]
  [if [< [length a-list] 30]
      a-list
      [begin
        
        [append [take-at-most  a-list 30] [list
                                           '[br]`[div [[style "clear: both"]] [a [[href ,[format "/displayfilelist/blah?selected=~s&or=~s" selected-tags pre-selected-tags]]] ,[format "And ~a more ..." [- [length a-list] 30]]]]]]]]]
[define [limit60 a-list]
  [if [< [length a-list] 60]
      a-list
      [take-at-most  a-list 60]]
  ]

[define [build-download-box files limiter] 
  [cons 'div [limiter [append-map [λ [a-clip]
                                    [let [[ a-clip [symbol->string a-clip]]]
                                      [list
                                       [if [regexp-match "mp3" a-clip]
                                           `[div [p [ ]
                                                    ,[build-download-link a-clip [path->string[file-name-from-path a-clip]]]
                                                    [audio [
                                                            [controls "controls"]
                                                            [preload "none"]
                                                            [autobuffer "true"]] 
                                                           
                                                           [source [ [ src ,[string-join [list "/getfile" [path->string [build-path a-clip]]] "/"]] [type "audio/mp3"]]
                                                                   
                                                                   [object [[type "application/x-shockwave-flash"] [data "/resources/player_mp3_maxi.swf"] [width "200"]  [height "20"]]
                                                                           [param [[name "movie"] [value "/resources/player_mp3_maxi.swf"]]]
                                                                           [param [[name "FlashVars"] [value ,[format "showloading=always&mp3=~a"  [string-join [list "/getfile" [path->string [build-path a-clip]]] "/"]]]]]
                                                                           ]]] ]
                                                 [p [],[build-file-info [append [list "a" "b" ][regexp-split #rx"/" a-clip]]]]]
                                           [if [regexp-match "flv" a-clip]
                                               `[object [[type "application/x-shockwave-flash"] [data "/resources/flvmaxi.swf"] [width "320"]  [height "240"]]
                                                        [param [[name "movie"] [value "/resources/flvmaxi.swf"]]]
                                                        [param [[name "FlashVars"] [value ,[format "flv=~a"  [string-join [list "/getfile" [path->string [build-path a-clip]]] "/"]]]]]
                                                        ]
                                               [if [regexp-match "png|gif|jpg|jpeg" a-clip]
                                                   `[div [[style "clear: both"]]
                                                         (p () [a ([href ,[build-download-link-string a-clip [path->string[file-name-from-path a-clip]]]]) (img ((align "left") (alt ,[format "~a" a-clip ]) (height "129") (src ,[format "/getfile/~a" a-clip ]) (style "margin-right:10px;margin-bottom:10px;") (width "92")))]
                                                            ,[build-file-info [append [list "a" "b" ][regexp-split #rx"/" a-clip]]]
                                                            ) ]
                                                   
                                                   [build-download-link a-clip [path->string[file-name-from-path a-clip]]]]]]
                                       ", " '[br]]]]  [limit60 files]]]]]
[define build-info-response [lambda []
                              (response/xexpr
                               
                               (wrap-content `(span (h1 "Internal details")
                                       
                                       [div [[style "border:3px solid red"]]
                                            [p [] "Resources directory:" ,[format "~a" resources-dir]]
                                            [p [] "Scanned directory:" ,[format "~a" base-dir]]
                                            ;[p [] "Number of items:" ,[format "~a" [length  files]]]
                                            [p [] "Number of tags:" ,[format "~a" [length [hash-keys tagcounts]]]]
                                            [pre [] "Log:" ,[format "~a" log-history]]])))]]

[define [build-file-info split-path]
  [let [[full-path [path->string [apply build-path [cons base-dir [cddr split-path]] ]]]
        [directory [path->string [apply build-path [cons base-dir [reverse [cdr [reverse [cddr split-path]]]]]]]]
        [filename [last split-path]]]
    `[div [[style "background:white;padding:3em"]]
          
          
          "Tags:" ,[build-tags-box [take-at-most [remove-tags [take-at-most [sort [hash-keys tagcounts] > #:key [lambda [a-key] [hash-ref tagcounts a-key]]] 50] [remove-tags selected-tags [tags-for-file [string->symbol [path->string [apply build-path [cddr split-path] ]]]]]]10 ]]
          "Directory: " ,filename [br] ;,[string-join  [reverse [drop [reverse [cddr split-path]] 1]] "/"] [br]
          "File Size: " ,[format "~a Mib" [exact->inexact [round [/ [file-size full-path] 1000000]]]] [br]
          [a [[href ,[build-download-link-string [string-join [cddr split-path] "/"] [last split-path] ]]] [format "Download" ]]
          ]]]
;[define [build-tags-box files] 
;  [cons 'ul [cons '((style "margin-top:10px;")) [map 
;              [λ [a-clip]
;                `[li []  ,[build-href a-clip a-clip] ", "]] files]]]]
[define [build-tags-box files] 
  [cons 'div [cons '((style "margin-top:10px;")) [map 
                                                  [λ [a-clip]
                                                    `[span [] " • - " ,[build-href a-clip a-clip] ", "]] files]]]]


[define [build-footer]
  '[div []
        [a [[href "/info"]] "Info"]]]

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


[define [select-files selected-tags][filter 
                                     [λ [a-file] 
                                       [equal? [length selected-tags ] 
                                               [length [delete-duplicates [lset-intersection equal?  [hash-ref file-to-tags-cache a-file [const '[]]] selected-tags]]]]]
                                     ;[λ [X] [lset<= equal? selected-tags [tags-from-filename X] ]]
                                     [if [empty? pre-selected-tags]
                                         [begin
                                           [displayln "Pre-selected all files"]
                                           ;[displayln [hash-keys file-to-tags-cache]]
                                           [hash-keys file-to-tags-cache]]
                                         [begin
                                           [displayln [format "Preselecting: ~a" pre-selected-tags]]
                                           [delete-duplicates [append-map [λ [x] 
                                                                            [hash-keys [spath-fail 
                                                                                        [list x]                                                                                      ;[list [format "~a" x]]
                                                                                        [send tags-to-files get-table] 
                                                                                        [λ args [make-hash]]]]] pre-selected-tags]]]]]]
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
[require "spath.rkt"]
(define (start request)
  [log "Received request" [url->string [request-uri request]]]
  
  [letrec [
           [split-path [regexp-split "[/?]" [uri-decode [url->string [request-uri request]]]] ]
           [action [cadr split-path]]]
    [log "Decoded path " split-path]
    [set! selected-tags [map [lambda [x] [string->symbol [if [symbol? x][symbol->string x][format "~a" x]]] ]
                             [read-from-string [=f selected  [request-bindings request] "[]"]]]]
    [set! pre-selected-tags [map [lambda [x] [string->symbol [if [symbol? x][symbol->string x][format "~a" x]]] ]
                                 [read-from-string [=f or  [request-bindings request] "[]"]]]]
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
                     
                     [wrap-content `[span (h1 ,[last split-path])
                             ,[build-download-box selected-files [λ [a]a]]
                             [div [[style "border:3px solid red"]]
                                  "Related Tags"
                                  ,[build-tags-box [suggest-tags selected-files]]]
                             [div [[style "border:3px solid red"]]
                                  ,[build-download-box selected-files limit-list]]
                             
                             ,[build-footer]]])]
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
                                         ,[build-file-info split-path]
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
                        ]
                      [if [equal? action "resources"]
                          [begin 
                            [log "Resources file: " [path->string [apply build-path [cons resources-dir [cddr split-path]]]]]
                            
                            (response
                             200 #"Here it is"
                             (current-seconds) [guess-mime-type [last [cddr split-path]] ]
                             (list [make-header #"Content-Disposition"
                                                [string->bytes/utf-8 [format "attachment; filename=\"~a\"" [last [cddr split-path]] ]]] ) ; (make-header a b )
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
                               [build-results-page selected-files]
                               )]]]]]]]
    ;[log "Call complete"]
    ])

(serve/servlet start
               #:servlet-regexp #rx"" #:listen-ip "0.0.0.0" #:port 61120 #:quit? #t  #:stateless?   #t 	;#:manager (make-threshold-LRU-manager #f (* 512 1024 1024))
               )



[define favicon.ico #"\377\330\377\340\0\20JFIF\0\1\1\0\0\1\0\1\0\0\377\376\0<CREATOR: gd-jpeg v1.0 (using IJG JPEG v62), quality = 100\n\377\333\0C\0\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\377\333\0C\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\377\300\0\21\b\0\20\0\20\3\1\"\0\2\21\1\3\21\1\377\304\0\37\0\0\1\5\1\1\1\1\1\1\0\0\0\0\0\0\0\0\1\2\3\4\5\6\a\b\t\n\v\377\304\0\265\20\0\2\1\3\3\2\4\3\5\5\4\4\0\0\1}\1\2\3\0\4\21\5\22!1A\6\23Qa\a\"q\0242\201\221\241\b#B\261\301\25R\321\360$3br\202\t\n\26\27\30\31\32%&'()*456789:CDEFGHIJSTUVWXYZcdefghijstuvwxyz\203\204\205\206\207\210\211\212\222\223\224\225\226\227\230\231\232\242\243\244\245\246\247\250\251\252\262\263\264\265\266\267\270\271\272\302\303\304\305\306\307\310\311\312\322\323\324\325\326\327\330\331\332\341\342\343\344\345\346\347\350\351\352\361\362\363\364\365\366\367\370\371\372\377\304\0\37\1\0\3\1\1\1\1\1\1\1\1\1\0\0\0\0\0\0\1\2\3\4\5\6\a\b\t\n\v\377\304\0\265\21\0\2\1\2\4\4\3\4\a\5\4\4\0\1\2w\0\1\2\3\21\4\5!1\6\22AQ\aaq\23\"2\201\b\24B\221\241\261\301\t#3R\360\25br\321\n\26$4\341%\361\27\30\31\32&'()*56789:CDEFGHIJSTUVWXYZcdefghijstuvwxyz\202\203\204\205\206\207\210\211\212\222\223\224\225\226\227\230\231\232\242\243\244\245\246\247\250\251\252\262\263\264\265\266\267\270\271\272\302\303\304\305\306\307\310\311\312\322\323\324\325\326\327\330\331\332\342\343\344\345\346\347\350\351\352\362\363\364\365\366\367\370\371\372\377\332\0\f\3\1\0\2\21\3\21\0?\0\303\370\305\250'\355\a\342=C\3427\307\2153\342W\215\3741\342\235+\302Z\257\301\237\6x$x\255\274?\341=3S7\367\232\244\257m\341U\232\305\374iok.\210\326\263x\326#\341k\320\372\202\336|\251\2\307\344Z_\305\337\24~\307:\237\204\274u\360\307\301~<\323 \360\354\177\360\223~\321\326:\353\370\233O\360\227\210l\254t]N\363\307\26\326\0267\221\301\360\374]E\251[\332\305\360\356O\1\35\213g<\255\255\304Z\25-\364\247\304\235\n\317\366y\265O\204\177\31<ow\360\223M\265\261\261\320\274!\361Xj\232'\2074\315cG\322\36\336=>\347D\361/\212\254\257\3743\6\265.\237i\25\266\257\243\337A=\344\36e\323Ao\3455\275\342\374\301\256\3746\277\375\252\357\346\375\234\277g\337\213\332\357\306\324\361d\261\177\302y\361\eU\327|%\342\257\v\3747\360e\312=\206\261w{\257x\eB\3214o\264\334Z\3132\350\372\23\311u\253j\232\247\226\261}\236\312;\271\355\377\0\312>\27\343\0361\305\346\330^\30\314\362*q\303\323\305W\302W\314k\345\371\254a\375\236\253\322\366\271\272\307Q\240\262\312ui\321\346\255V\23\305\306\265|\306\225,G/2\205\n\237\335y\227\tp\235:u3\\7\20b\245\223O\v\34\306\236\36\226#\6\362\364\243F\\\231l\250T\304}jn\244\371h'\f;\234\25YS\3765\352C\377\331"]
