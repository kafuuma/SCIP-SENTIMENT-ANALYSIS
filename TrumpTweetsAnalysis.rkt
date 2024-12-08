#lang racket

(require data-science-master)
(require plot)
(require math)
(require json)
;(require pict)
(require racket/date)

;;; This function reads line-oriented JSON (as output by massmine),
;;; and packages it into an array. For very large data sets, loading
;;; everything into memory like this is heavy handed. For data this small,
;;; working in memory is simpler

(define (json-lines->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()]
             [record (read-json(current-input-port))])
    (if (or (eof-object? record)
            (and head (>= num head)))
        (jsexpr->string json-array)
        (loop (add1 num) (cons record json-array)
              (read-json (current-input-port))))))



;;; Read in the entire tweet database (3200 tweets from Trump's timeline)
(define tweets (string->jsexpr
                (with-input-from-file "/Users/henry/projets/code_snippets/tweets.json" (lambda () (json-lines->json-array)))))


;;; Remove just the tweet text and source from each tweet
;;; hash. Finally, remove retweets.
;;; Remove just the tweet text, source, and timestamp from each tweet
;;; hash. Finally, remove retweets.

(define t
  (let ([tmp (map (lambda (x) (list (hash-ref x 'text) (hash-ref x 'source)
                               (hash-ref x 'created_at))) tweets)])
    (filter (lambda (x) (not (string-prefix? (first x) "RT"))) tmp)))

(define t-text
  (map (lambda (x) (hash-ref x 'text))  tweets))
   


;;; Label each tweet as coming from iphone, android, or other.
(define tweet-by-type
  (map (lambda (x) (list (first x)
                    (cond [(string-contains? (second x) "Android") "android"]
                          [(string-contains? (second x) "iPhone") "iphone"]
                          [else "other"])))
       t))


;;; Separate tweets by their source (created on iphone vs
;;; android... we aren't interested in tweets from other sources)

(define android (filter (lambda (x) (string=? (second x) "android")) tweet-by-type))
(define iphone (filter (lambda (x) (string=? (second x) "iphone")) tweet-by-type))

;; plot by type of device used


;; Function to plot tweet count by type
(define (plot-tweet-count-by-type android iphone)
  (define counts (list (list  "Android" (length android))
                      (list "iPhone" (length iphone))))
    (parameterize ((plot-width 400)  ; Width
                   (plot-height 4000)) ; Height
    (plot (list
           (tick-grid)
           (discrete-histogram
            (sort counts (lambda (x y) (> (second x) (second y))))
            #:color "MediumSlateBlue"
            #:line-color "MediumSlateBlue"))
          #:x-label "Source"
          #:y-label "Count"
           #:y-min 0
          #:y-max 4000)))

;; Call the function with sample data
(plot-tweet-count-by-type android iphone)





;;; Normalize case, remove URLs, remove punctuation, and remove spaces
;;; from each tweet. 
(define (preprocess-text str)
  (string-normalize-spaces
   (remove-punctuation
    (remove-urls
     (string-downcase str)) #:websafe? #t)))


(define (remove-stopwords lst #:lexicon [lexicon 'SMART])
  (let ([stopwords (cond [(equal? lexicon 'SMART) SMART]
                         [(equal? lexicon 'snowball) snowball]
                         [(equal? lexicon 'onix) onix]
                         [else (error "Unknown lexicon")])])
    (filter (λ (word) (not (member word stopwords))) lst)))




(define a (map (lambda (x)
                 (remove-stopwords x))
               (map (lambda (y)
                      (string-split (preprocess-text y)))
                    ($ android 0))))


(define i (map (lambda (x)
                 (remove-stopwords x))
               (map (lambda (y)
                      (string-split (preprocess-text y)))
                    ($ iphone 0))))


;;; Remove empty strings and flatten tweets into a single list of
;;; words 
(define ad (filter (lambda (x) (not (equal? x ""))) (flatten a)))
(define ip (filter (lambda (x) (not (equal? x ""))) (flatten i)))
;;; All words from both sources of tweets
(define b (append ad ip))
;;; Only words that are used by both devices
(define c (set-intersect ad ip))


;; ;;; Word list from android and iphone tweets
(define awords (sort (sorted-counts ad)
                     (λ (x y) (> (second x) (second y))))) 
(define iwords (sort (sorted-counts ip)
                     (λ (x y) (> (second x) (second y)))))
(define bwords (sort (sorted-counts b)
                     (λ (x y) (> (second x) (second y)))))



;;; Plot the top 20 words from both devices combined
(parameterize ([plot-width 600]
               [plot-height 600])
    (plot (list
        (tick-grid)
        (discrete-histogram (reverse (take bwords 20))
                            #:invert? #t
                            #:color "DimGray"
                            #:line-color "DimGray"
                            #:y-max 450))
       #:x-label "Occurrences"
       #:y-label "word"))




;;; To begin our sentiment analysis, we extract each unique word
;;; and the number of times it occurred in the document

(define (list-to-string lst)
  (string-join lst " ")) ; Uses a space as the delimiter

(define words (document->tokens (list-to-string  t-text) #:sort? #t))

;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label. 
(define sentiment (list->sentiment words #:lexicon 'nrc))

;;; We can take a sneak peak at the data...
(take sentiment 5)
;;; --> '(("word" "sentiment" "freq")
;;;       ("ship" "anticipation" 367)
;;;       ("sea" "positive" 364)
;;;       ("time" "anticipation" 318)
;;;       ("long" "anticipation" 311))

;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))
;;; --> '(("anticipation" 4739)
;;;       ("positive" 9206)
;;;       ("joy" 3196)
;;;       ("trust" 5095)
;;;       ("surprise" 2157)
;;;       ("negative" 7090)
;;;       ("fear" 4136)
;;;       ("sadness" 3317)
;;;       ("anger" 2765)
;;;       ("disgust" 1958))

;;; Better yet, we can visualize this result as a barplot (discrete-histogram)
(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (lambda (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))










