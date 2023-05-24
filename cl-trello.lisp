;;;; cl-trello.lisp

(in-package #:cl-trello)

;;; "cl-trello" goes here. Hacks and glory await!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   Trello Interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;  Visit https://trello.com/1/connect?key=KEY&name=MyApp&response_type=token&scope=read,write&expiration=never
;;;  in your browser while logged in to trello (in this case, as the success-automaton trello user
;;;  allow the app access
;;;  copy the token and use it below:
;;;

(defparameter *cl-trello-version* "0.9.0")
(defvar *trello-key* nil  )
(defvar *trello-token* nil)
(defvar *trello-api-url* "https://api.trello.com/1/" "Trello API version 1")

(let ((rate 0)
      (rate-lock (bt:make-lock "rate-lock"))
      (time-last-call 0)
      (start-time 0)
      (initialize nil)
      ;; Trello rate limit 100 per 10 seconds
      ;; or 10 per second
      (trello-rate-limit 99)
      (rate-limit-seconds 10)
      (counts (vector 0 0 0 0 0 0 0 0 0 0)))
 (flet  ((trello-current-rate (increment-p)
	   "Calculates the current rate"
	   (unless initialize
	     (setf start-time (get-universal-time))
	     (setf time-last-call start-time)
	     (setf initialize t))
	   (let* ((time-current-call (get-universal-time))
		  (index (mod (- time-current-call start-time) rate-limit-seconds))
		  (last-index (mod (- time-last-call start-time) rate-limit-seconds))
		  (delta (- time-current-call time-last-call )))
	     (log:trace increment-p  index delta rate)
	     (log:trace start-time time-last-call time-current-call )

	     (cond
	       ;; If it's been more than 10 seconds since
	       ;;  the time-last-call, reset
	       ((>= (- time-current-call time-last-call)
		    rate-limit-seconds)
		(log:trace "resetting counts start-time time-last-call")
		(fill counts 0)
		(when increment-p (incf (elt counts 0)))
		(setf start-time (get-universal-time))
		(setf time-last-call start-time))
	       ;;   0  1  2  3  4  5  6  7  8  9
	       ;;  10 11 12 13 14 15 16 17 18 19
	       ;;  start = 0
	       ;;  time last call = 8
	       ;;  time-current = 12
	       ;;  have to 0 out 9, 0, 1 and 2
	       ;;  and incr 2
	       (t
		(dotimes (i (-  delta 1) )
		  (log:trace "zeroing stuff" i  (- delta 1)  )
		  (log:trace (mod (+ last-index i 1) rate-limit-seconds))
		  (setf (elt counts (mod (+ last-index i 1) rate-limit-seconds))
			0)
		  (log:trace counts)
		  )
		;;  When the index moves forward, zero the count at index
		(when (not (= 0 (- index last-index)))
		  (setf (elt counts index) 0))
		(when increment-p (incf (elt counts index)))))
	     (setf rate (reduce #'+ counts)))))
    
   (defun trello-rate-limit-p ()
     "Returns rate after rate limit has not been exceeded"
     (bt:acquire-lock rate-lock t)
    
     (do ((current-rate (trello-current-rate t)
			(trello-current-rate nil)))
	 ((< current-rate trello-rate-limit))
       (log:trace "Sleeping 1" )
       (sleep 1))
      
     (setf time-last-call (get-universal-time))
     (bt:release-lock rate-lock)
     (values rate counts))))


(define-condition other-return-code (error) 
  ((status-code   :initarg :status-code
		  :initform nil :reader status-code)
   (reason-phrase :initarg :reason-phrase
		  :initform nil :reader reason-phrase))
  (:report (lambda (condition stream)
	     (format stream
		     "Unknown Return Code~%    Status-code: ~a~%     Reason-Phrase: ~a~%"
		     (when (slot-boundp condition 'status-code)
		       (status-code condition))
		     (when (slot-boundp condition 'reason-phrase)
		       (reason-phrase condition))))))

(define-condition rate-limit-exceeded (other-return-code)
  ((rate :initarg :rate :reader rate))
  (:report (lambda (condition stream)
	     (format stream
		     "Rate Limit Exceeded~%    Rate: ~a~%     Status-code: ~a~%    Reason-phrase: ~a~%"
		     (status-code condition) (rate condition) (reason-phrase condition)))))


(define-condition invalid-value-for-value (other-return-code)
  ()
  (:report (lambda (condition stream)
	     (format stream
		     "Invalid Value for Value~%    Status-code: ~a~%    Reason-phrase: ~a~%"
		     (status-code condition)  (reason-phrase condition)))))

(defmacro make-http-request (name rate-function)
  "Makes a request named NAME-http-request that uses the function FUNCTION"
  `(let* ((macro-name (intern (format nil  "~:@(~a-HTTP-REQUEST~)" ,name) *package*))
	 )
    `(defmacro ,macro-name (&rest args)
       `(progn
	 (multiple-value-bind (rate counts)
	     (,,,rate-function) 
	   (log:trace counts rate)
	   (multiple-value-bind
		 (body-or-stream status-code headers uri
				 stream must-close reason-phrase)
	       (drakma:http-request ,@args)
	     (declare (ignore stream must-close))
	     (cond
	       ((= 200 status-code) body-or-stream)
	       ((= 429 status-code)
		(log:error headers)
		(log:error "Rate Limit Exceeded" reason-phrase uri status-code )
		(error 'rate-limit-exceeded :status-code status-code
		       :rate rate :reason-phrase reason-phrase))
	       ((and (= 400 status-code)
		     (equalp reason-phrase "invalid value for value"))
		(log:error headers)
		(log:error "Invalid value for value" reason-phrase uri status-code )
		(error 'invalid-value-for-value :status-code status-code
		       :reason-phrase reason-phrase))
	       (status-code
		(error 'other-return-code :status-code status-code
		       :reason-phrase reason-phrase))
	       (t ))))))))

(make-http-request 'trello 'trello-rate-limit-p)
;; creates trello-http-request
(defmacro trello-http-request (&rest args)
   `(progn
      (multiple-value-bind (rate counts)

	  (trello-rate-limit-p )
        (log:trace counts rate))
      (drakma:http-request ,@args)))

(defmacro trello-get (&rest api-calls)
  "retrieves from trello API of form
     /1/lists/[idList]/cards
     e.g. (trello-get 1 lists list-id cards)"
  
  `(let* ((uri (concatenate 'string *trello-api-url* ,@api-calls
			   "?key=" *trello-key* "&token=" *trello-token*))
	 (trello-return    (trello-http-request uri)))
     (typecase trello-return
       (string (error trello-return))
       (t (with-input-from-string
	      (in (flexi-streams:octets-to-string trello-return))
	    (cl-json:decode-json in))))))

(defmacro trello-update (params method &rest api-calls)
  "posts from trello API of form
     POST /1/cards - Create a new card on a List
     e.g. (trello-update ((\"idList\" . \"1234sf\")
                        (\"name\" . \"bob\")) cards )"
  
  `(let* ((uri (concatenate 'string *trello-api-url* ,@api-calls))
	 (trello-return (trello-http-request uri :method ,method
				  :parameters (cons (cons "key" *trello-key*)
						    (cons (cons "token" *trello-token*)
							  ,params)))))
     (typecase trello-return
       (string (error trello-return))
       (t 
	(with-input-from-string
	    (in (flexi-streams:octets-to-string trello-return))
	  (cl-json:decode-json in))))))

;;;;  Boards

(defun board-id (board)
  "if BOARD is a STRING, assume its the board-id
   if BOARD is a CONS assume is a board list and get the ID"
  (typecase board
    (integer (format nil "~a" board))
    (string board)
    (cons (cdr (assoc :id board)))))


(defun retrieve-boards () 
  "retrieves a list of boards
     GET /1/members/me/boards - Get an array of the Boards of a user"
  (trello-get "/members/me/boards" ))

(defun retrieve-lists (board)
  "retrieves  lists from a  board
     GET /1/boards/[board_id]/lists - Get an array of Lists on a board"
  (let ((board-id (board-id board)))
    (trello-get "board/" board-id "/lists")))

(defun retrieve-cards-from-board (board)
  "retrieves cards from a a board
     GET /1/boards/[board_id]/cards - Get an array of Cards on a board"
  (trello-get "boards/" (board-id board) "/cards"))

(defun add-label-board (board labelname color)
  "Adds a label to a board with the name LABELNAME
    and color COLOR
      POST /1/boards/[board_id]/labels

      Required permissions: write
      Arguments 
         name (required)
           Valid Values: a string with a length from 0 to 16384
         color (required)
           Valid Values: A valid label color or null "
  (trello-update (pairlis (list "name" "color") (list labelname color))
		 :post "boards/" (board-id board) "/labels"))




;;;; lists

(defun retrieve-cards-from-list (list-id) 
  "retrieves cards from a list
      /1/lists/[idList]/cards"
  (trello-get "lists/" list-id "/cards"))




;;  Add a card to a list on a board
(defun add-card (list-id name )
  "Add a card to a list.
     POST /1/cards - Create a new card on a List "
  (trello-update (pairlis '("name" "idList"  "urlSource")
			(list name list-id   ()))
	       :post 
	       "cards"))


(defun delete-card (card-id)
  "Delete a card"
  (trello-update nil :delete "cards/" card-id ))

;;;;; Cards

(defun retrieve-card (card)
  "Retrieves the card from trello
      GET /1/cards/[card id or shortlink]"
  (trello-get "cards/" (board-id card) ))

(defun retrieve-list (list)
  "Retrieves the card from trello
      GET /1/cards/[card id or shortlink]"
  (trello-get "lists/" (board-id list) ))

;;;;;; labels sort order
;;;;;;   1: Green
;;;;;;   2: Yellow
;;;;;;   3: Orange
;;;;;;   4: Red
;;;;;;   5: Purple
;;;;;;   6: Blue
;;;;;;   7: Turquoise
;;;;;;   8: Light green
;;;;;;   9: Pink
;;;;;;   0: Black

(defun add-label-card (label-id card-id)
    (unless (member label-id
		    (cdr (assoc :id-labels (retrieve-card card-id)))
		    :test #'equalp)
      (trello-update (list (cons "value" (format nil "~a" label-id)))
		     :post "cards/" card-id "/idLabels")))


(defun retrieve-search-results (query &rest query-params  )
  "Retrieves all cards with the given JIRA-KEY in the card name"
  (cl-trello:trello-update (cons (cons "query" query)
		       query-params)
		 :get "search"))


(defun date-created (trello-id)
  "Returns the date represented by the trello-id"
  (local-time:unix-to-timestamp
   (parse-integer (subseq trello-id 0 8) :radix 16)))
