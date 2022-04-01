(module builders racket
  (provide (all-defined-out) )
  (require xml)
  (require web-server/servlet)
  (require srfi/1)
  (require net/uri-codec)
  [define introduction #t]
  (define [make-context selected-files presets tagcounts remove-tags selected-tags rejected-tags suggest-tags limit-list page-number full-search-results split-path base-dir tags-for-file page-length]
    (let [[x
           (zip `(      selected-files presets tagcounts remove-tags selected-tags rejected-tags suggest-tags limit-list page-number full-search-results split-path base-dir tags-for-file page-length)
                (list   selected-files presets tagcounts remove-tags selected-tags rejected-tags suggest-tags limit-list page-number full-search-results split-path base-dir tags-for-file page-length))]]
      (lambda (key)
        [let [[res [assoc key x]]]
          [if res
              [second res]
              [error "Cannot find in assoc list" key]]
          ]
        )
      )
    )
  
  [define build-results-page-old
    [lambda [selected-files presets tagcounts remove-tags selected-tags rejected-tags suggest-tags limit-list page-number full-search-results]
      `(html
        (head (title "Tag Browser"))
        (body (h1 "All tags")
              [form [[method "post"]]
                    [div [[style "border:3px solid red"]] 
                         ;,[build-selected-box c]
                         ]
                    [div [[style "border:3px solid red"]] 
                         ,presets]
                    [div [[style "border:3px solid red"]] "Common Tags"
                         ,[build-tags-box [take [sort [hash-keys tagcounts] > #:key [lambda [a-key] [hash-ref tagcounts a-key]]] 50] selected-tags rejected-tags presets]]
                    [div [[style "border:3px solid red"]]
                         "Related Tags"
                         ,[build-tags-box [take-at-most  [remove-tags selected-tags [suggest-tags selected-files]]10] selected-tags rejected-tags presets]]
                    [div [[style "border:3px solid red"]]
                         "Related Tags Without Globals"
                         ,[build-tags-box [take-at-most [remove-tags [take [sort [hash-keys tagcounts] > #:key [lambda [a-key] [hash-ref tagcounts a-key]]] 50] [remove-tags selected-tags [suggest-tags selected-files]]]10 ] selected-tags rejected-tags presets]]
                                               
                    [div [[style "border:3px solid red"]]
                         ,[build-download-box selected-files limit-list page-number full-search-results]]]
              ,[build-footer]))]]


  [define build-results-page
    [lambda [c]
      [wrap-content
       `(div  "\r\n        "
              ,[if [empty?  [c 'selected-tags]]
                   `[br]
                   `[div ((class "box"))  (h1 () "Selected Tags") ,[build-selected-box c]]]
              ,[if [empty? [c 'rejected-tags]]
                   `[br]
                   `[div [[style "border:3px solid green"]]  (h1 () "Rejected Tags") ,[build-rejected-box c ]]]
                                                    
              ,[if [empty? [c 'selected-files]]
                   `[br]

                   `[div ((class "box"))
                         (h2 () "Your search results") 
                         (p () (br ())
                            ,[if [not [empty? [c 'selected-files]]] [build-download-box [c 'selected-files] [c 'limit-list] [c 'page-number] c]`[br]])]]
              `(br)
              ,[if [empty? [c 'selected-files]]
                   `[br]
                   `(div ((class "box"))
                         "\r\n        "(h1 () "Related Tags") "\r\n        "
                         (p () ,[build-tags-box
                                 [take-at-most [[c 'remove-tags]
                                                [take-at-most
                                                 [sort [hash-keys [c 'tagcounts]] > #:key [lambda [a-key] [hash-ref [c 'tagcounts] a-key]]] 50] [[c 'remove-tags] [c 'selected-tags] [[c 'suggest-tags] [c 'selected-files]]]]10 ]
                                 [c 'selected-tags]
                                 [c 'rejected-tags]
                                 [c 'presets]]
                            ) )]
              `[br]
                                  
              ,[if  [and
                     [not [empty? [c 'selected-files]]]
                     [not [empty? [take-at-most [sort [hash-keys [c 'tagcounts]] > #:key [lambda [a-key] [hash-ref [c 'tagcounts] a-key]]] 50]]]]  `(div ((class "box")) "\r\n        "(h1 () "Extra Tags") "\r\n        " (p ()
,[build-tags-box [take-at-most  [[c 'remove-tags] [c 'selected-tags] [[c 'suggest-tags] [c 'selected-files]]]10] [c 'selected-tags] [c 'rejected-tags] [c 'presets]] )) `[br]]
              [div [[style "border:3px solid red"]] "Common Tags"
                   ,[build-tags-box [take-at-most [sort [hash-keys [c 'tagcounts]] > #:key [lambda [a-key] [hash-ref [c 'tagcounts] a-key]]] 50]  [c 'selected-tags] [c 'rejected-tags] [c 'presets]]
                   ]
              )
       ""
       c]]]


  [define wrap-content
    [lambda [content x c]
      `(html ((xmlns "http://www.w3.org/1999/xhtml")) "\r\n" 
             (head () "\r\n" 
                   (meta ((content "text/html; charset=iso-8859-1") (http-equiv "Content-Type"))) "\r\n" 
                   (title () "TagBrowser") "\r\n" 
                   (link
                    ((href "/resources/style.css")
                     (rel "stylesheet")
                     (type "text/css")))
                   "\r\n"
                   )
             "\r\n" 
             (body () "\r\n" 
                   (div ((id "wrapper")) "\r\n  " 
                        (div ((id "header")) "\r\n    " 
                             (div ((id "nav")) 
                                  (a ((href "index.html")) "Reset") " " nbsp "|" nbsp " " 
                                  (a ((href "/export")) "Export List") " " nbsp "|" nbsp " " 
                                  (a ((href "#")) "About TagExplorer") " " nbsp "|" nbsp " " 
                                  (a ((href "/info")) "Info") " " nbsp "|" nbsp " "
                                  (a ((href "/exit")) "Exit") " " nbsp "|" nbsp " " 
                                  (a ((href "/tagexplorer")) "Tag Explorer") " " nbsp "|" nbsp " " 
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
                                                                      [value ,[format "~a" [c 'selected-tags]]] 
                                                                      [style "visibility: hidden;display: none;"]]]
                                                              [input [[type "hidden"] 
                                                                      [name "or"] 
                                                                      [value ,[format "~a" [c 'presets]]] 
                                                                      [style "visibility: hidden;display: none;"]]] 
                                                              [input [[type "submit"] [style "visibility: hidden;display: none;"]]] ]])) "\r\n    "
                                                                                                                                         (div ((id "bg"))) "\r\n  ") "\r\n  " 
                                                                                                                                                                     (div ((id "main-content")) "\r\n    " 
                                                                                                                                                                          (div ((id "left-column")) "\r\n      "
                                                                                                                                                                               ,[if introduction [begin [set! introduction #f] 
                                                                                                                                                                                                        `(div ((class "box")) "\r\n        "
                                                                                                                                                                                                              (h1 () "Using TagBrowser") "\r\n        " 
                                                                                                                                                                                                              (p ()  "Tagbrowser is a quick and convenient way to explore your files." ) [p [] "To start, search for a keyword, or select a preset from the list to right."]    "\r\n      ")] `[br]]
                                                                                                                                                                               ,content "\r\n      "           "\r\n    ")     (div ((id "right-column")) "\r\n      " 
                                                                                                                                            
                                                                                                                                            
                                                                                                                                                                                                                                    "\r\n      " (div ((class "sidebar")) "\r\n        "  "\r\n        "
                                                                                                                                                                                                                                                      (h3 () "Explore your files") "\r\n        " (p () "Select tags to refine your search, select [X] to forbid that tag.")(p())
                                                                                                                                                                                                                                                      (h3 () "Presets") 
                                                                                                                                                                                                                                                      "\r\n        " 
                                                                                                                                                                                                                                                      (div ((class "box")) "\r\n          " 
                                                                                                                                                                                                                                                           (ul () "\r\n            " 
                                                                                                                                                                                                                                                               (li () [a [[href "/display/?selected=()&or=(gif%20png%20jpg%20jpeg)"]]  "Pictures"]) "\r\n            " 
                                                                                                                                                                                                                                                               (li () [a [[href "/display/?selected=()&or=(mp3%20ogg%20wav%20flac)"]]  "Music"]) "\r\n            " 
                                                                                                                                                                                                                                                               (li () [a [[href "/display/?selected=()&or=(mpg%20mpeg%20mp4%20flv%20avi%20avi)"]]  "Video"]) "\r\n            " 
                                                                                                                                                                                                                                                               (li () [a [[href "/display/?selected=()&or=(ebook%20azw%20aeh%20lrf%20lrx%20cbr%20cbz%20cb7%20cbt%20cba%20djvu%20epub%20fb2%20pdf%20tr2%20tr3)"]]  "Books"] " ") "\r\n            "
                                                                                                                                                                                                                                                               (li () [a [[href ,[format "/display/?selected=~s&or=()" [c 'selected-tags]]]]  "Clear Presets"]) "\r\n            "
                                                                                                                                                                                                                                                               "\r\n          ") "\r\n        ")
                                                                                                                                                              
                                                                                                                                                              
                                                                                                                                                                                                                                                      (h1 () "Global Tags") "\r\n        "(p () ,[build-tags-box [sort [take-at-most [sort [hash-keys [c 'tagcounts]] > #:key [lambda [a-key] [hash-ref [c 'tagcounts] a-key]]] 50] string<? #:key [lambda [x][symbol->string x]]] [c 'selected-tags] [c 'rejected-tags] [c 'presets]] )                                                                                                                                                                                                                                                                                               (a ((href "http://www.web-designers-directory.org/"))) (a ((href "http://www.medicine-pet.com/"))) "\r\n      ") "\r\n    ") "\r\n  ") "\r\n  " (div ((id "footer")) "Copyright " copy " 2016 PraeceptaMachinae.com." (br ()) "\r\n    " (a ((href "http://validator.w3.org/check?uri=referer") (target "_blank")) "XHTML") "  |  " (a ((href "http://jigsaw.w3.org/css-validator/check/referer?warning=no&profile=css2") (target "_blank")) "CSS") "  - Thanks to: " (a ((href "http://www.medicine-pet.com/") (target "_blank")) "Pet Medicine") " | " (span ((class "crd")) (a ((href "http://www.web-designers-directory.org/")) "Web site Design")) " by : " (a ((href "http://www.web-designers-directory.org/") (target "_blank")) "WDD")) "\r\n") "\r\n\r\n") "\r\n")]]



  ; HTML builders
  ; return XHTML structures that can be put together in a response/xhtml 
  [define[ build-href selected text rejected-tags pre-selected-tags [pn 0] ]
    ;  `[a [[href ,[format "/addtag/~a?selected=~a&or=~a" link selected-tags pre-selected-tags]]] ,[format "~a(~a%)" text [round [* 100 [/ [hash-ref selected-scored-tags link [const 0]] [add1 total-selected-files]]]]]]
    `[a [[href ,[format "/display/?selected=~s&rejected=~s&or=~s&page=~s" selected  rejected-tags  pre-selected-tags pn]]] ,[format "~a" text ]]
    ]

  [define[ build-better-href some-tags  text rejected-tags pre-selected-tags [pn 0] ]
    ;  `[a [[href ,[format "/addtag/~a?selected=~a&or=~a" link selected-tags pre-selected-tags]]] ,[format "~a(~a%)" text [round [* 100 [/ [hash-ref selected-scored-tags link [const 0]] [add1 total-selected-files]]]]]]
    `[a [[href ,[format "/display/?selected=~s&rejected=~s&or=~s&page=~s" some-tags rejected-tags pre-selected-tags pn]]] ,[format "~a" text ]]
    ]

  [define[ build-download-link link text]
    `[a [[href ,[format "/getfile/~a" link ]]] ,text]]
  [define[ build-download-link-string link text]
    [format "/getfile/~a" link ]]
  [define [build-remove-href a-string text c]
    `[a [[href ,[format "/removetag/~a?selected=~a&rejected=~s&or=~a" a-string  [[c 'remove-tags] [list a-string ] [c 'selected-tags]] [c 'rejected-tags] [c 'presets]]] [title ,[format "Tag ~a adds ~a entries.  Click to remove." a-string 10]]] ,text]]

  [define [build-remove-rejected-href a-string text c]
    `[a [[href ,[format "/removetag/~a?selected=~a&rejected=~s&or=~a" a-string  [c 'selected-tags]  [[c 'remove-tags] [list a-string ] [c 'rejected-tags]] [c 'presets]]] [title ,[format "Tag ~a adds ~a entries.  Click to remove." a-string 10]]] ,text]]
  [define take-at-most [λ [ a-list a-num] [if [< [length a-list] a-num]
                                              a-list
                                              [take a-list a-num]]]]
  [define [build-selected-box c]
    [cons 'span [append-map [λ [a-clip]
                              [list 
                             
                               [build-remove-href a-clip a-clip c] " "]] [c 'selected-tags]]]]

  [define [build-rejected-box c]
    [cons 'span [append-map [λ [a-clip]
                              [list 
                             
                               [build-remove-rejected-href a-clip a-clip c] " "]] [c' rejected-tags]]]]


  [define [build-pages-bar  c ] 
    [letrec [[page-length  [c 'page-length]]
             [total-pages [quotient [length [c 'full-search-results]] page-length]]]
    
      [append
       `[div ]
       [map [lambda [p]
              `[span [[elem "debug"]]
                     ,[build-better-href  [c 'selected-tags] [format "~a" p] [c 'rejected-tags] [c 'presets] p] " "]] [iota  total-pages]]]]
    ]

  [define [build-file-info c split-path]
    [let [
          [base-dir [c 'base-dir]]]
                      
    [let [[full-path [path->string [apply build-path [cons base-dir [cddr split-path]] ]]]
          [directory [path->string [apply build-path [cons base-dir [reverse [cdr [reverse [cddr split-path]]]]]]]]
          [filename [last split-path]]]
      `[div [[style "background:white;padding:3em"]]
          
          
            "Tags:" ,[build-tags-box 
                      [map uri-encode 
                           [map symbol->string 
                                [take-at-most 
                                 [[c 'remove-tags] 
                                  [take-at-most 
                                   [sort [hash-keys [c 'tagcounts]] > #:key [lambda [a-key] [hash-ref [c 'tagcounts] a-key]]] 
                                   50]
                                  [[c 'tags-for-file] [string->symbol [path->string [apply build-path [cddr split-path] ]]]]] 10 ]]] [c 'selected-tags] [c 'rejected-tags] [c 'presets]]
            ;"Directory: " ,directory [br] ;,[string-join  [reverse [drop [reverse [cddr split-path]] 1]] "/"] [br]
            "File Size: " ,[format "~a Mib" [exact->inexact [round [/ [file-size full-path] 1000000]]]] [br]
            [a [[href ,[build-download-link-string [string-join [cddr split-path] "/"] [last split-path] ]]] ,[format "Download" ]]
            ]]]]

  
  [define [build-download-box files limiter page-number c] 
    [append '[div []]  [append-map [λ [a-clip]
                                     [let [[ a-clip [symbol->string a-clip]]]
                                       [list
                                        [if [regexp-match "mp3" a-clip]
                                            `[div [p [ ]
                                                     ,[build-download-link a-clip [path->string [file-name-from-path a-clip]]]
                                                     [audio [
                                                             [controls "controls"]
                                                             [preload "none"]
                                                             [autobuffer "true"]] 
                             
                                                            [source [ [ src ,[string-join [list "/getfile" [path->string [build-path a-clip]]] "/"]] [type "audio/mp3"]]
                                     
                                                                    [object [[type "application/x-shockwave-flash"] [data "/resources/player_mp3_maxi.swf"] [width "200"]  [height "20"]]
                                                                            [param [[name "movie"] [value "/resources/player_mp3_maxi.swf"]]]
                                                                            [param [[name "FlashVars"] [value ,[format "showloading=always&mp3=~a"  [string-join [list "/getfile" [path->string [build-path a-clip]]] "/"]]]]]
                                                                            ]]] ]
                                                  [p [],[build-file-info c [append [list "a" "b" ][regexp-split #rx"/|\\" a-clip]]]]]
                                            [if [regexp-match "flv" a-clip]
                                                `[object [[type "application/x-shockwave-flash"] [data "/resources/flvmaxi.swf"] [width "320"]  [height "240"]]
                                                         [param [[name "movie"] [value "/resources/flvmaxi.swf"]]]
                                                         [param [[name "FlashVars"] [value ,[format "flv=~a"  [string-join [list "/getfile" [path->string [build-path a-clip]]] "/"]]]]]
                                                         ]
                                                [if [regexp-match "(?i:\\.png|\\.gif|\\.jpg|\\.jpeg)" a-clip]
                                                    `[div  [[style "clear: both"]] 
                                                           (p () [a ([href ,[build-download-link-string a-clip [path->string[file-name-from-path a-clip]]]]) 
                                                                    (img ((align "left") (alt ,[format "~a" a-clip ]) (height "129") (src ,[format "/getfile/~a" a-clip ]) 
                                                                                         (style "margin-right:10px;margin-bottom:10px;") (width "92")))
                                                                    ]
                                                              ,[build-file-info c [append [list "a" "b" ][regexp-split #rx"/|\\" a-clip]]]
                                                              ) 
                                                           ]
                     
                                                    `[div
                                                      
                       
                                                      [a [[href 

                                                           ,[string-join [list "/explorer" [uri-encode [path->string [build-path a-clip]]]] "/"]]] (img ((alt "Open in Explorer") (height "32") (src "/resources/images/revealfinder.jpg") (width "32")))]

                                                      [a [[href 

                                                           ,[string-join [list "/start" [uri-encode [path->string [build-path a-clip]]]] "/"]]] (img ((alt "Launch") (height "32") (src "/resources/images/launch.png") (width "32")))]

                                                      ,[build-download-link a-clip `(span (img ((alt ,[path->string[file-name-from-path a-clip]]) (height "32") (src "/resources/images/download-arrow.jpg") (width "32"))) ,[path->string[file-name-from-path a-clip]] ) ]


                                                      ]]]]
                                        " " '[br]]]]   files]
            [build-pages-bar c]]]
  [define build-info-response [lambda [resources-dir base-dir tagcounts log-history]
                                (response/xexpr
                               
                                 (wrap-content `(span (h1 "Internal details")
                                       
                                                      [div [[style "border:3px solid red"]]
                                                           [p [] "Resources directory:" ,[format "~a" resources-dir]]
                                                           [p [] "Scanned directory:" ,[format "~a" base-dir]]
                                                           ;[p [] "Number of items:" ,[format "~a" [length  files]]]
                                                           [p [] "Number of tags:" ,[format "~a" [length [hash-keys tagcounts]]]]
                                                           [pre [] "Log:" ,[format "~a" log-history]]])))]]


  [define build-tagexplorer-response [lambda [tagcounts  selected-tags rejected-tags]
                                       (response/xexpr
                               
                                        (wrap-content `(span (h1 "Tag Explorer")
                                       
                                                             [div [[style "border:3px solid red"]]
                                                                  (h2 "Top 50 Tags")
                                                                  ,[build-tags-box [take-at-most 
                                                                                    [sort [hash-keys tagcounts] > #:key [lambda [a-key] [hash-ref tagcounts a-key]]] 
                                                                                    50] selected-tags rejected-tags]]
                                                             [div [[style "border:3px solid red"]]
                                                                  (h2 "Top 1000 Tags")
                                                                  ,[build-tags-box [take-at-most 
                                                                                    [sort [hash-keys tagcounts] > #:key [lambda [a-key] [hash-ref tagcounts a-key]]] 
                                                                                    1000] selected-tags rejected-tags]])))]]

  [define [build-tags-box files selected-tags rejected-tags  pre-selected-tags] 
    [append '[ div  ((style "margin-top:10px;"))] [apply append [ zip [map 
                                                                       [λ [a-clip]  
                                                                         `[span ,[build-href [cons a-clip selected-tags ] [uri-decode [format "~a" a-clip]] rejected-tags pre-selected-tags]
                                                                                ,[build-href selected-tags   "[X]" [cons a-clip rejected-tags] pre-selected-tags]
                                                                                ]
                                                                         ] files]
                                                                      [make-list [length files] ", "]]]]

    ]


  [define [build-footer]
    '[div []
          [a [[href "/info"]] "Info"]]]

  )