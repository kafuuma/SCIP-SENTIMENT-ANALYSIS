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
                (with-input-from-file "/Users/henry/projets/code_snippets/uganda_tweets.json" (lambda () (json-lines->json-array)))))

(define text-tweets
  (map (lambda (x) (hash-ref x 'text))  tweets))

;;; To begin our sentiment analysis, we extract each unique word
;;; and the number of times it occurred in the document

(define (list-to-string lst)
  (string-join lst " ")) ; Uses a space as the delimiter


;;; Normalize case, remove URLs, remove punctuation, and remove spaces
;;; from each tweet. This function takes a list of words and returns a
;;; preprocessed subset of words/tokens as a list
(define (preprocess-text lst)
  (map (λ (x)
         (string-normalize-spaces
          (remove-punctuation
           (remove-urls
            (string-downcase x)))))
       lst))

;;; Normalize case, remove URLs, remove punctuation, and remove spaces
;;; from each tweet. 
(define (preprocess-str-text str)
  (string-normalize-spaces
   (remove-punctuation
    (remove-urls
     (string-downcase str)) #:websafe? #t)))


(define words (document->tokens
               (list-to-string
                (preprocess-text text-tweets)) #:sort? #t))




;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label. 
(define sentiment (list->sentiment words #:lexicon 'nrc))


;;; We can take a sneak peak at the data...
;(take sentiment 10)
;;;-->'(("word" "sentiment" "freq")
;;;  ("police" "fear" 686)
;;;  ("police" "positive" 686)
;;;  ("police" "trust" 686)
;;;  ("baby" "joy" 599)
;;;  ("baby" "positive" 599)
;;;  ("burial" "anger" 585)
;;;  ("burial" "fear" 585)
;;;  ("burial" "negative" 585)
;;;  ("burial" "sadness" 585))


;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))
;; --> '(("fear" 2421) ("positive" 5020) ("trust" 3313) ("joy" 1860) ("anger" 1405) ("negative" 2041) ("sadness" 1360) ("anticipation" 1712) ("surprise" 640) ("disgust" 462))


;;; Determine total sentiment score
(define counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq)))
   




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

