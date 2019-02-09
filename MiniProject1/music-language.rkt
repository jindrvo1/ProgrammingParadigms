#lang racket

;;; Name: Vojtěch Jindra
;;; Email: vjindr18@student.aau.dk

(require "music-base.rkt")

;;; Supportive functions for classes
(define (new-instance class . parameters)
  (apply class parameters))

(define (send message object . args)
  (let ((method (object message)))
    (cond ((procedure? method) (apply method args))
          (else (error "Error in method lookup " method)))))


;;; Supportive functions for using symbols for instruments

; find-instr-val finds an instrument's symbol based on its integer key
; key -- an integer aassociated with given instrument
; lst -- association list to search
; returns instrument's symbol if given key is found, false otherwise
(define (find-instr-val key lst)
  (cond
    ((null? lst) #f)
    ((eqv? (caar lst) key) (cdar lst))
    (else (find-instr-val key (cdr lst)))
    ))

; find-instr-key finds an instrument's key based on its symbol
; val -- a symbol associated with given instrument's key
; lst -- association list to search
; returns instrument's key if given symbol is found, false otherwise
(define (find-instr-key val lst)
  (cond
    ((null? lst) #f)
    ((eqv? (cdar lst) val) (caar lst))
    (else (find-instr-key val (cdr lst)))
    ))

; find-instrument determines whether to find the key of desired instrument, or its symbol
; key -- can be either symbol, in which case the find-instr-key function is called and the key of an instrument is returned,
;        or can be an integer key, in which case the find-instr-val function is called and the symbol of the instrument is returned
; returns either the key or the symbol of given instrument if succesful, false otherwise
(define (find-instrument key)
  (cond
    ((number? key) (find-instr-val key instruments))
    ((symbol? key) (find-instr-key key instruments))
    (else #f)
    ))

; Association list of instruments
; Keys are integers, values are symbols
(define instruments (list (cons 0 'None)
                          (cons 1 'Piano)
                          (cons 2 'Organ)
                          (cons 3 'Guitar)
                          (cons 4 'Violin)
                          (cons 5 'Flute)
                          (cons 6 'Trumpet)
                          (cons 7 'Helicopter)
                          (cons 8 'Telephone)))

; Velocity is set to a constant
(define velocity 80)

;;; Supportive functions for easier manupilation of objects

; music-element checks whether given object is a music element (i.e. either a Note, Pause, SequentialMusicElement or ParallelMusicElement)
; obj -- object to check
; returns true if obj is a music element, false otherwise
(define (music-element? obj)
  (cond
    ((or (is-note? obj)
         (is-pause? obj)
         (is-sequential? obj)
         (is-parallel? obj))
     #t)
    (else #f)))

; error-music-elemnt prints an error message
(define (error-music-element)
  (error "Given object is not a music element"))

; is-note? checks whether given object is a Note
; obj -- object to check
; returns true of object is a Note, false otherwise
(define (is-note? obj) (cond ((not (procedure? obj)) #f) (else (cond ((eqv? (type-of obj) 'Note) #t) (else #f)))))

; is-pause? checks whether given object is a Pause
; obj -- object to check
; returns true of object is a Pause, false otherwise
(define (is-pause? obj) (cond ((not (procedure? obj)) #f) (else (cond ((eqv? (type-of obj) 'Pause) #t) (else #f)))))

; is-sequential? checks whether given object is a SequentialMusicElement
; obj -- object to check
; returns true of object is a SequentialMusicElement, false otherwise
(define (is-sequential? obj) (cond ((not (procedure? obj)) #f) (else (cond ((eqv? (type-of obj) 'SequentialMusicElement) #t) (else #f)))))

; is-parallel? checks whether given object is a ParallelMusicElement
; obj -- object to check
; returns true of object is a ParallelMusicElement, false otherwise
(define (is-parallel? obj) (cond ((not (procedure? obj)) #f) (else (cond ((eqv? (type-of obj) 'ParallelMusicElement) #t) (else #f)))))

; get-duration calls a getter of variable duration of given object
; obj -- object to call the getter on
; returns object's duration if it is a music element, prints error otherwise
(define (get-duration obj) (cond ((music-element? obj) (send 'get-duration obj)) (else (error-music-element))))

; get-pitch calls a getter of variable pitch of given object
; obj -- object to call the getter on
; returns object's pitch if it is a music element, prints error otherwise
(define (get-pitch obj) (cond ((music-element? obj) (send 'get-pitch obj)) (else (error-music-element))))

; get-instrument calls a getter of variable instrument of given object
; obj -- object to call getter on
; returns object's instrument if it is a music element, prints error otherwise
(define (get-instrument obj) (cond ((music-element? obj) (send 'get-instrument obj)) (else (error-music-element))))

; get-elements calls a getter of variable elements of given object
; obj -- object to call getter on
; returns object's elements if it is a music element, prints error otherwise
(define (get-elements obj) (cond ((music-element? obj) (send 'get-elements obj)) (else (error-music-element))))

(define (get-abs-time obj) (cond ((music-element? obj) (send 'get-abs-time obj)) (else (error-music-element))))

; type-of determines given object's type
; obj -- object which type is to be determined
; returns given object's type as a symbol
(define (type-of obj) (send 'type-of obj))

; scale scales given object's durations by a scaling factor
; obj -- object to scale
; scaling-factor -- a number the durations of given objects are to be multiplied by
; returns new object with scaled durations if object is a music element, prints error otherwise
(define (scale obj scaling-factor) (cond ((music-element? obj) (send 'scale obj scaling-factor)) (else (error-music-element))))

; transpose transposes given object's pitches by a given number
; obj -- object to transpose
; add-pitch -- the number to be added to object's current pitches
; returns new object with transposed pitches if object is a music element, prints error otherwise
(define (transpose obj add-pitch) (cond ((music-element? obj) (send 'transpose obj add-pitch)) (else (error-music-element))))

; reinstrument changes the instruments of a given object
; obj -- object to reinstrument
; new-instrument -- instrument to be assigned to given object
; returns new object with new instrument if object is a music element, prints error otherwise
(define (reinstrument obj new-instrument) (cond ((music-element? obj) (send 'reinstrument obj new-instrument)) (else (error-music-element))))

; duration-total calculated the total duration of a given object
; obj -- object to calculate duration on
; returns a total duration of given object if it is a music element, prints error otherwise
(define (duration-total obj) (cond ((music-element? obj) (send 'duration obj)) (else (error-music-element))))

;;; Other function used to fulfuill the assignment

; monophonic? checks whether a music element is monophonic
; obj -- object to be checked
; returns a boolean if given object is a music element, prints error otherwise
(define (monophonic? obj) (cond ((> (degree-of-polyphony obj) 1) #f) (else #t)))

; degree-of-polyphony calculates the total degree of polyphony of a music element
; obj -- object to calculate degree of polyphony on
; return the degree of polyphony if given object is a music element, prints error otherwise
(define (degree-of-polyphony obj) (cond ((music-element? obj) (degree-of-polyphony-elements (get-abs-start-and-duration-list obj) (get-abs-start-and-duration-list obj) 0)) (else (error-music-element))))

; linearize makes a flat list of note elements with assigned absolute starting times
; obj -- object to linearize
; returns a flat list of notes with absolute starting times assigned
(define (linearize obj) (cond ((music-element? obj) (remove-empty-and-pauses (denestify (calc-abs-times obj 0)))) (else (error-music-element))))

; calc-abs-times is a helping function that assigns absolute starting times to note elements
; obj -- object to calculate absolute times on
; time -- starting time of the object obj
; returns an accordingly nested list of notes
(define (calc-abs-times obj time) (cond ((music-element? obj) (send 'calc-abs-times obj time)) (else (error-music-element))))

; assign-abs-time is a helping function that assigns an absolute starting time to a music element
; obj -- object to have absolute time assigned
; time -- time to be asssigned
; returns a new object of the same type with assigned absolute starting value
(define (assign-abs-time obj time) (cond ((music-element? obj) (send 'assign-abs-time obj time)) (else (error-music-element))))

; convert-to-abs-times converts a list of note objects to list of note-abs-time-with-duration
; obj -- object to be converted
; returns a list of notes represented by note-abs-time-with-duration
(define (convert-to-abs-times obj) (cond ((music-element? obj) (convert-to-abs-times-elements (linearize obj))) (else (error-music-element))))

; convert-to-abs-times-elements is a helping function that recursively converts a list of note objects to list of note-abs-time-with-duration
; lst -- elements of an object to be converted
; returns a list of notes represented by note-abs-time-with-duration
(define (convert-to-abs-times-elements lst)
  (cond
    ((null? lst) '())
    (else (cons
           (note-abs-time-with-duration (get-abs-time (car lst)) (find-instrument (get-instrument (car lst))) (get-pitch (car lst)) velocity (get-duration (car lst)))
           (convert-to-abs-times-elements (cdr lst))))))

; denestify converts a nested list into a flat list
; lst -- list to be converted to a flat list
; returns a flat list
(define (denestify lst)
  (cond
    ((null? lst) '())
    ((pair? (car lst)) (append (denestify (car lst)) (denestify (cdr lst))))
    (else (cons (car lst) (denestify (cdr lst))))))

; remove-empty-and-pauses removes empty lists and pauses from a list (except for the last empty list in order to keep the list proper)
; lst -- list to be altered
; returns a list without pauses and empty lists
(define (remove-empty-and-pauses lst)
  (cond
    ((null? lst) '())
    ((or (empty? (car lst)) (is-pause? (car lst))) (remove-empty-and-pauses (cdr lst)))
    (else (cons (car lst) (remove-empty-and-pauses (cdr lst))))))

; elements-valid? checks a list if all of its elements are music elements
; lst -- list to be checked
; returns a boolean
(define (elements-valid? lst)
  (cond
    ((null? lst) #t)
    ((not (music-element? (car lst))) #f)
    (else (elements-valid? (cdr lst)))))

; get-abs-start-and-duration-list creates a sorted list of pairs of absolute starting time and its duration of each element
; obj -- object to be converted to a list of absolute starting times and durations
; returns a list
(define (get-abs-start-and-duration-list obj) (sort-by-abs-start (get-abs-start-and-duration-list-elements (linearize obj))))
(define (get-abs-start-and-duration-list-elements lst)
  (cond
    ((null? lst) '())
    (else (cons (cons (get-abs-time (car lst)) (get-duration (car lst))) (get-abs-start-and-duration-list-elements (cdr lst))))))

; sort-by-abs-start sorts a list by its absolute starting time (ascendingly)
; lst -- list of pairs to be sorted by the first element in the each pair
; returns a sorted list of the same elements as accepted list
(define (sort-by-abs-start lst)
  (sort lst (lambda (x y) (< (car x) (car y)))))

; degree-of-polyphony-elements calculates calculates the degree of polyphony of elements of given list and takes the biggest one
; lst -- list to determine the biggest degree-of-polyphony on
; orig-lst -- backup of the original list
; max-degree -- maximum degree parameter for tail recursion
; returns a maximum polyphony degree of given list
(define (degree-of-polyphony-elements lst orig-lst max-degree)
  (cond
    ((null? lst) max-degree)
    ((> (check-against-others (car (car lst)) (+ (cdr (car lst)) (car (car lst))) (remove-elem orig-lst (car lst)) 0) max-degree)
     (degree-of-polyphony-elements (cdr lst) orig-lst (check-against-others (car (car lst)) (+ (cdr (car lst)) (car (car lst))) (remove-elem orig-lst (car lst)) 0)))
    (else (degree-of-polyphony-elements (cdr lst) orig-lst max-degree))))

; check-against-others whether given element intersects with other elements of a list
; start -- starting time of given element
; end -- ending time of given element
; lst -- list to check the element against
; res -- number of intersections; used for tail recursion
; returns number of intersections of given element against given list
(define (check-against-others start end lst res)
  (cond
    ((null? lst) (+ 1 res))
    ((and (< (car (car lst)) end) (> (+ (car (car lst)) (cdr (car lst))) start))
     (check-against-others (if (> (car (car lst)) start) (car (car lst)) start)
                           (if (< (+ (car (car lst)) (cdr (car lst))) end) (+ (car (car lst)) (cdr (car lst))) end)
                           (cdr lst)
                           (+ 1 res)))
    (else (check-against-others start end (cdr lst) res))))

; remove-elem removes an element from a list
; lst -- list to remove the element from
; elem -- element to be removed
; returns a list of the same elements as given list except for the elem element
(define (remove-elem lst elem)
  (cond
    ((null? lst) '())
    ((equal? (car lst) elem) (remove-elem (cdr lst) elem))
    (else (cons (car lst) (remove-elem (cdr lst) elem)))))

;;; Definition of the Note class
; pitch -- pitch of the note to be constructed; 0 <= pitch <= 127
; duration -- duration of the note to be constructed; duration >= 0
; instrument -- instrument of the not e to be constructed in the form of symbol.
;               Valid values are: 'Piano, 'Organ, 'Guitar, 'Violin, 'Flute, 'Trumpet, 'Helicopter, 'Telephone
; abs-time -- flexible parameter abs-time is used for assigning an absolute starting time of the Note; only the (car abs-time) is used
(define (Note pitch duration instrument . abs-time)

  ; Check whether given arguments are valid
  ; Raises an error in case of an invalid argument
  (when (or (< pitch 0) (> pitch 127)) (error "Invalid pitch value " pitch))
  (when (< duration 0) (error "Invalid duration value" duration))
  (when (or (< (find-instrument instrument) 1) (eqv? (find-instrument instrument) #f)) (error "Invalid instrument value" instrument))
  
  (let ((pitch pitch)
        (duration duration)
        (instrument (find-instrument instrument))
        (abs-time abs-time))

    ; type-of determines the type of the Note object
    ; returns symbol 'Note
    (define (type-of) 'Note)

    ; get-elements is a getter function for the elements of Note
    ; returns an instance of Note in a list
    (define (get-elements) (list (new-instance Note pitch duration (find-instrument instrument))))

    ; get-pitch is a getter function for the pitch of Note
    ; returns the value of pitch
    (define (get-pitch) pitch)

    ; get-duration is a getter function for the duration of Note
    ; returns the value of duration
    (define (get-duration) duration)

    ; get-instrument is a getter function for the instrument of Note
    ; returns the value of instrument
    (define (get-instrument) (find-instrument instrument))

    ; get-abs-time is a getter function for the flexible abs-time argument of Note
    ; returns the value of abs-time
    (define (get-abs-time) (car abs-time))

    ; scale multiplies the duration of the Note by a scaling factor
    ; scaling-factor -- the value to multiply the duration by
    ; returns a new instance of Note with scaled duration
    (define (scale scaling-factor) (new-instance Note pitch (* duration scaling-factor) instrument))

    ; transpose alters the pitch of the Note by a number
    ; add-pitch -- the value to be added to current pitch
    ; returns a new instance of Note with transposed pitch
    (define (transpose add-pitch) (new-instance Note (+ pitch add-pitch) duration instrument))

    ; reinstrument changes the instrument of the Note
    ; new-instrument -- the instrument to be assigned to the Note object
    ; returns a new instance of Note with changed instrument
    (define (reinstrument new-instrument) (new-instance Note pitch duration new-instrument))

    ; duration-total calculates the total duration of the Note, which is the same as the value of the duration variable
    ; returns the value of duration
    (define (duration-total) duration)

    ; calc-abs-time adds the duration of the note element to the absolute starting time passsed as an argument
    ; time -- starting time passed from previous music elements
    ; returns a new instance of Note with set absolute starting time in a list
    (define (calc-abs-time time) (list (new-instance Note pitch duration (find-instrument instrument) (+ duration time))))

    ; assign-abs-time assigns the absolute starting time to the note object
    ; time -- absolute starting time to be assigned
    ; returns a new instance of Note with assigned absolute starting time
    (define (assign-abs-time time) (new-instance Note pitch duration (find-instrument instrument) time))

    ; self catches messages sent to the object and calls appropriate functions
    ; messaage -- the message determining which function should be called
    ; returns the function to be called if the message is defined, raises an error otherwise
    (define (self message)
      (cond ((eqv? message 'get-elements) get-elements)
            ((eqv? message 'get-pitch) get-pitch)
            ((eqv? message 'get-duration) get-duration)
            ((eqv? message 'get-instrument) get-instrument)
            ((eqv? message 'get-abs-time) get-abs-time)
            ((eqv? message 'type-of) type-of)
            ((eqv? message 'scale) scale)
            ((eqv? message 'transpose) transpose)
            ((eqv? message 'reinstrument) reinstrument)
            ((eqv? message 'duration) duration-total)
            ((eqv? message 'linearize) linearize)
            ((eqv? message 'calc-abs-times) calc-abs-time)
            ((eqv? message 'assign-abs-time) assign-abs-time)
            (else (error "Undefined message" message))))
     
    self))

;;; Definition of the Pause class
; duration -- duration of the pause to be constructed; duration >= 0
; abs-time -- flexible parameter abs-time is used for assigning an absolute starting time of the Pause; only the (car abs-time) is used
(define (Pause duration . abs-time)

  ; Check whether given arguments are valid
  ; Raises an error in case of an invalid argument
  (when (< duration 0) (error "Invalid duration value" duration))

  (let ((pitch 0)
        (duration duration)
        (instrument (find-instrument 'None))
        (abs-time abs-time))

    ; type-of determines the type of the Pause object
    ; returns symbol 'Pause
    (define (type-of) 'Pause)

    ; get-elements is a getter function for the elements of Pause
    ; returns an instance of Pause in a list
    (define (get-elements) (list (new-instance Pause duration)))

    ; get-pitch is a getter function for the pitch of Pause
    ; returns the value of pitch, which is 0 for all pauses
    (define (get-pitch) pitch)

    ; get-duration is a getter function for the duration of Pause
    ; returns the value of duration
    (define (get-duration) duration)

    ; get-instrument is a getter function for the instrument of Pause
    ; returns the value of instrument, which is 'None for all pauses
    (define (get-instrument) (find-instrument instrument))

    ; get-abs-time is a getter function for the flexible abs-time argument of Pause
    ; returns the value of abs-time
    (define (get-abs-time) (car abs-time))

    ; scale multiplies the duration of the Pause by a scaling factor
    ; scaling-factor -- the value to multiply the duration by
    ; returns a new instance of Pause with scaled duration
    (define (scale scaling-factor) (new-instance Pause (* duration scaling-factor)))

    ; transpose secures that an error will not be raised when attempting to transpose pitch of a pause,
    ;           however the pitch will not be altered, since pause has no pitch
    ; add-pitch -- the value to be added to current pitch
    ; returns a new instance of Pause with the same pitch as it had (i.e. 0)
    (define (transpose scaling-factor) (new-instance Pause duration))

    ; reinstrument secures that an error will not be raised when attempting to assign an instrument to a pause,
    ;              however the instrument will not be altered, since pause has no instrument
    ; new-instrument -- the instrument to be assigned to the Pause object
    ; returns a new instance of Pause with the same instrument as it had (i.e. 'None)
    (define (reinstrument new-instrument) (new-instance Pause duration))

    ; duration-total calculates the total duration of the Pause, which is the same as the value of the duration variable
    ; returns the value of duration
    (define (duration-total) duration)

    ; calc-abs-time adds the duration of the pause element to the absolute starting time passsed as an argument
    ; time -- starting time passed from previous music elements
    ; returns a new instance of Pause with set absolute starting time in a list
    (define (calc-abs-time time) (list (new-instance Pause duration (+ duration time))))

    ; assign-abs-time assigns the absolute starting time to the pause object
    ; time -- absolute starting time to be assigned
    ; returns a new instance of Pause with assigned absolute starting time
    (define (assign-abs-time time) (new-instance Pause duration time))
   
    ; self catches messages sent to the object and calls appropriate functions
    ; messaage -- the message determining which function should be called
    ; returns the function to be called if the message is defined, raises an error otherwise
    (define (self message)
      (cond ((eqv? message 'get-elements) get-elements)
            ((eqv? message 'get-duration) get-duration)
            ((eqv? message 'get-pitch) get-pitch)
            ((eqv? message 'get-instrument) get-instrument)
            ((eqv? message 'get-abs-time) get-abs-time)
            ((eqv? message 'type-of) type-of)
            ((eqv? message 'scale) scale)
            ((eqv? message 'transpose) transpose)
            ((eqv? message 'reinstrument) reinstrument)
            ((eqv? message 'duration) duration-total)
            ((eqv? message 'linearize) linearize)
            ((eqv? message 'calc-abs-times) calc-abs-time)
            ((eqv? message 'assign-abs-time) assign-abs-time)
            (else (error "Undefined message" message))))
   
    self))

;;; Definition of the SequentialMusicElement class
; elements -- list of music elements to be played sequentially
(define (SequentialMusicElement elements)

  ; Check whether given arguments are valid
  ; Raises an error in case of an invalid argument
  (when (null? elements) (error "Invalid argument"))
  
  (let ((elements elements))

    ; type-of determines the type of the SequentialMusicElement object
    ; returns symbol 'SequentialMusicElement
    (define (type-of) 'SequentialMusicElement)

    ; get-elements is a getter function for the elements of SequentialMusicElement
    ; returns the variable elements
    (define (get-elements) elements)

    ; get-pitch is a getter function for the pitches of SequentialMusicElement
    ;           calls a supportive function get-pitch-elements to determine the pitches
    ; returns the values of pitches of elements in a list
    (define (get-pitches) (get-pitch-elements elements))

    ; get-pitch-elements recursively constructs the list of pitches of elements in given lisst
    ; lst -- list of music elements
    ; returns a list of pitches of given music elements
    (define (get-pitch-elements lst)
      (cond
        ((null? lst) '())
        (else (cons (get-pitch (car lst)) (get-pitch-elements (cdr lst))))))

    ; get-durations is a getter function for the durations of SequentialMusicElement
    ;               calls a supportive function get-duration-elements to determine the durations
    ; returns the values of the durations of elements in a list
    (define (get-durations) (get-duration-elements elements))

    ; get-duration-elements recursively constructs the list of durations of elements in given list
    ; lst -- list of music elements
    ; returns a list of durations of given music elements
    (define (get-duration-elements lst)
      (cond
        ((null? lst) '())
        (else (cons (get-duration (car lst)) (get-duration-elements (cdr lst))))))

    ; get-instruments is a getter function for the instruments of SequentialMusicElement
    ;                 calls a supportive function get-instrument-elements to determine the instruments
    ; returns the instruments of elements in a list
    (define (get-instruments) (get-instrument-elements elements))
 
    ; get-instrument-elements recursively constructs the list of instruments of elements in given list
    ; lst -- list of music elements
    ; returns a list of instruments of given music elements
    (define (get-instrument-elements lst)
      (cond
        ((null? lst) '())
        (else (cons (get-instrument (car lst)) (get-instrument-elements (cdr lst))))))

    ; scale-seq multiplies the duration of every element in the elements variable by a scaling factor
    ;           calls a supportive function scale-elements to scale the elements
    ; scaling-factor -- the value the durations are to be multiplied by
    ; returns a new instance of SequentialMusicElement with multiplied durations
    (define (scale-seq scaling-factor) (new-instance SequentialMusicElement (scale-elements scaling-factor elements)))

    ; scale-elements recursively calls scale on elements of given list
    ; lst -- list of music elements
    ; returns a list of music elements with scaled durations
    (define (scale-elements scaling-factor lst)
      (cond
        ((null? lst) '())
        (else (cons (scale (car lst) scaling-factor) (scale-elements scaling-factor (cdr lst))))))

    ; transpose-seq adds a value to pitch of every element in the elements variable
    ;               calls a supportive function transpose-elements to alter pitches of the elements
    ; add-pitch -- the value to be added to pitches
    ; returns a new instance of SequentialMusicElement with altered pitches
    (define (transpose-seq add-pitch) (new-instance SequentialMusicElement (transpose-elements add-pitch elements)))

    ; transpose-elements recursively calls transpose on elements of given list
    ; lst -- list of music elements
    ; returns a list of music elements with altered pitches
    (define (transpose-elements add-pitch lst)
      (cond
        ((null? lst) '())
        (else (cons (transpose (car lst) add-pitch) (transpose-elements add-pitch (cdr lst))))))

    ; reinstrument-seq changes the instrument of every element in the elements variable
    ;                  calls a supportive function reinstrument-elements to change the instruments of the elements
    ; new-instrument -- a symbol representing the new instrument
    ; returns a new instrance of SequentialMusicElement with altered instruments
    (define (reinstrument-seq new-instrument) (new-instance SequentialMusicElement (reinstrument-elements new-instrument elements)))

    ; reinstrument-elements recursively calls reinstrument on elements of given list
    ; lst -- list of music elements
    ; returns a list of music elements with altered instruments
    (define (reinstrument-elements new-instrument lst)
      (cond
        ((null? lst) '())
        (else (cons (reinstrument (car lst) new-instrument) (reinstrument-elements new-instrument (cdr lst))))))

    ; duration-total calculated the total duration of the SequentialMusicElement
    ;                calls a supportive to function calculate-duration to determine the duration of the elements in the elements variable
    ; returns a value of the total duration of SequentialMusicElement
    (define (duration-total-seq) (calculate-duration elements 0))

    ; calculate-duration recursively calls duration-total on every element of given list
    ; lst -- list of music elements
    ; res -- saves the current result troughout recursions; initially should be 0
    ; returns the sum of durations of every element in given list
    (define (calculate-duration lst res) 
      (cond
        ((null? lst) res)
        (else (calculate-duration (cdr lst) (+ (duration-total (car lst)) res)))))


    ; calc-abs-times-seq recursively calls calc-abs-times-elements on every element of given list
    ; lst -- list of music elements
    ; time -- absolute time of currently calculated music element
    ; returns a list of music element with assigned absolute starting times
    (define (calc-abs-times-seq time) (calc-abs-times-elements elements time))

    ; calc-abs-times-elements assigns absolute starting times to every element of given list
    ; lst -- list of music elements
    ; time -- absolute starting time
    ; returns a list of music element with assigned absolute starting times
    (define (calc-abs-times-elements lst time)
      (cond
        ((null? lst) '())
        ((or (is-note? (car lst)) (is-pause? (car lst))) (cons (assign-abs-time (car lst) time) (calc-abs-times-elements (cdr lst) (+ (get-duration (car lst)) time))))
        (else (cons (calc-abs-times (car lst) time) (calc-abs-times-elements (cdr lst) (+ (duration-total (car lst)) time))))))
    
    ; self catches messages sent to the object and calls appropriate functions
    ; messaage -- the message determining which function should be called
    ; returns the function to be called if the message is defined, raises an error otherwise
    (define (self message)
      (cond ((eqv? message 'get-elements) get-elements)
            ((eqv? message 'get-duration) get-durations)
            ((eqv? message 'get-pitch) get-pitches)
            ((eqv? message 'get-instrument) get-instruments)
            ((eqv? message 'type-of) type-of)
            ((eqv? message 'scale) scale-seq)
            ((eqv? message 'transpose) transpose-seq)
            ((eqv? message 'reinstrument) reinstrument-seq)
            ((eqv? message 'duration) duration-total-seq)
            ((eqv? message 'calc-abs-times) calc-abs-times-seq)
            (else (error "Undefined message" message))))

    self))

;;; Definition of the ParallelMusicElement class
; elements -- list of music elements to be played parallely
(define (ParallelMusicElement elements)

  ; Check whether given arguments are valid
  ; Raises an error in case of an invalid argument
  (when (null? elements) (error "Invalid argument"))
  (when (not (list? elements)) (error "Invalid argument"))
  (when (not (elements-valid? elements)) (error "Invalid argument"))
  
  (let ((elements elements))

    ; type-of determines the type of the ParallelMusicElement object
    ; returns symbol 'ParallelMusicElement
    (define (type-of) 'ParallelMusicElement)

    ; get-elements is a getter function for the elements of ParallelMusicElement
    ; returns the variable elements
    (define (get-elements) elements)

    ; get-pitches is a getter function for the pitches of ParallelMusicElement
    ;           calls a supportive function get-pitch-elements to determine the pitches
    ; returns the values of pitches of elements in a list
    (define (get-pitches) (get-pitch-elements elements))

    ; get-pitch-elements recursively constructs the list of pitches of elements in given lisst
    ; lst -- list of music elements
    ; returns a list of pitches of given music elements
    (define (get-pitch-elements lst)
      (cond
        ((null? lst) '())
        (else (cons (get-pitch (car lst)) (get-pitch-elements (cdr lst))))))

    ; get-durations is a getter function for the durations of ParallelMusicElement
    ;               calls a supportive function get-duration-elements to determine the durations
    ; returns the values of the durations of elements in a list
    (define (get-durations) (get-duration-elements elements))

    ; get-duration-elements recursively constructs the list of durations of elements in given list
    ; lst -- list of music elements
    ; returns a list of durations of given music elements
    (define (get-duration-elements lst)
      (cond
        ((null? lst) '())
        (else (cons (get-duration (car lst)) (get-duration-elements (cdr lst))))))

    ; get-instruments is a getter function for the instruments of ParallelMusicElement
    ;                 calls a supportive function get-instrument-elements to determine the instruments
    ; returns the instruments of elements in a list
    (define (get-instruments) (get-instrument-elements elements))

    ; get-instrument-elements recursively constructs the list of instruments of elements in given list
    ; lst -- list of music elements
    ; returns a list of instruments of given music elements
    (define (get-instrument-elements lst)
      (cond
        ((null? lst) '())
        (else (cons (get-instrument (car lst)) (get-instrument-elements (cdr lst)))))) 

    ; scale-par multiplies the duration of every element in the elements variable by a scaling factor
    ;           calls a supportive function scale-elements to scale the elements
    ; scaling-factor -- the value the durations are to be multiplied by
    ; returns a new instance of ParallelMusicElement with multiplied durations
    (define (scale-par scaling-factor) (new-instance ParallelMusicElement (scale-elements scaling-factor elements)))

    ; scale-elements recursively calls scale on elements of given list
    ; lst -- list of music elements
    ; returns a list of music elements with scaled durations
    (define (scale-elements scaling-factor lst)
      (cond
        ((null? lst) '())
        (else (cons (scale (car lst) scaling-factor) (scale-elements scaling-factor (cdr lst))))))

    ; transpose-par adds a value to pitch of every element in the elements variable
    ;               calls a supportive function transpose-elements to alter pitches of the elements
    ; add-pitch -- the value to be added to pitches
    ; returns a new instance of ParallelMusicElement with altered pitches
    (define (transpose-par add-pitch) (new-instance ParallelMusicElement (transpose-elements add-pitch elements)))

    ; transpose-elements recursively calls transpose on elements of given list
    ; lst -- list of music elements
    ; returns a list of music elements with altered pitches
    (define (transpose-elements add-pitch lst)
      (cond
        ((null? lst) '())
        (else (cons (transpose (car lst) add-pitch) (transpose-elements add-pitch (cdr lst))))))

    ; reinstrument-par changes the instrument of every element in the elements variable
    ;                  calls a supportive function reinstrument-elements to change the instruments of the elements
    ; new-instrument -- a symbol representing the new instrument
    ; returns a new instrance of ParallelMusicElement with altered instruments
    (define (reinstrument-par new-instrument) (new-instance ParallelMusicElement (reinstrument-elements new-instrument elements)))

    ; reinstrument-elements recursively calls reinstrument on elements of given list
    ; lst -- list of music elements
    ; returns a list of music elements with altered instruments
    (define (reinstrument-elements new-instrument lst)
      (cond
        ((null? lst) '())
        (else (cons (reinstrument (car lst) new-instrument) (reinstrument-elements new-instrument (cdr lst))))))

    ; duration-total calculated the total duration of the ParallelMusicElement
    ;                calls a supportive to function find-longest-duration to determine the duration of the elements in the elements variable
    ; returns a value of the total duration of SequentialMusicElement
    (define (duration-total-par) (find-longest-duration elements 0))

    ; find-longest-duration recursively calls duration-total on every element of given list and determines the longest duration
    ; lst -- list of music elements
    ; res -- saves the current result troughout recursions; initially should be 0
    ; returns the longest duration in given list
    (define (find-longest-duration lst res)
      (cond
        ((null? lst) res)
        ((> (duration-total (car lst)) res) (find-longest-duration (cdr lst) (duration-total (car lst))))
        (else (find-longest-duration (cdr lst) res))))

    ; calc-abs-times-par recursively calls calc-abs-times-elements on every element of given list
    ; lst -- list of music elements
    ; time -- absolute time of currently calculated music element
    ; returns a list of music element with assigned absolute starting times
    (define (calc-abs-times-par time) (calc-abs-times-elements elements time))

    ; calc-abs-times-elements assigns absolute starting times to every element of given list
    ; lst -- list of music elements
    ; time -- absolute starting time
    ; returns a list of music element with assigned absolute starting times
    (define (calc-abs-times-elements lst time)
      (cond
        ((null? lst) '())
        ((or (is-note? (car lst)) (is-pause? (car lst))) (cons (assign-abs-time (car lst) time) (calc-abs-times-elements (cdr lst) time)))
        (else (cons (calc-abs-times (car lst) time) (calc-abs-times-elements (cdr lst) time)))))
    
    ; self catches messages sent to the object and calls appropriate functions
    ; messaage -- the message determining which function should be called
    ; returns the function to be called if the message is defined, raises an error otherwise
    (define (self message)
      (cond ((eqv? message 'get-elements) get-elements)
            ((eqv? message 'get-duration) get-durations)
            ((eqv? message 'get-pitch) get-pitches)
            ((eqv? message 'get-instrument) get-instruments)
            ((eqv? message 'type-of) type-of)
            ((eqv? message 'scale) scale-par)
            ((eqv? message 'transpose) transpose-par)
            ((eqv? message 'reinstrument) reinstrument-par)
            ((eqv? message 'duration) duration-total-par)
            ((eqv? message 'calc-abs-times) calc-abs-times-par)
            (else (error "Undefined message" message))))
    
    self))                                                  

;;; Polyphonic song 
(define canon
  (new-instance SequentialMusicElement
                (list (new-instance ParallelMusicElement
                                    (list (new-instance Note 44 1875 'Piano)
                                          (new-instance Note 56 1875 'Piano)
                                          (new-instance Note 60 1875 'Piano)
                                          (new-instance Note 63 1875 'Piano)))
                      (new-instance Pause 150)
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 46 1500 'Piano)
                                          (new-instance Note 53 1500 'Piano)
                                          (new-instance Note 58 1500 'Piano)
                                          (new-instance Note 62 1500 'Piano)))
                      (new-instance Pause 150)
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 39 4350 'Piano)
                                          (new-instance Note 51 4350 'Piano)
                                          (new-instance Note 55 900 'Piano)
                                          (new-instance Note 58 900 'Piano)
                                          (new-instance Note 63 900 'Piano)
                                          (new-instance SequentialMusicElement
                                                        (list (new-instance Pause 750)
                                                              (new-instance Note 65 225 'Piano)
                                                              (new-instance Note 67 450 'Piano)
                                                              (new-instance Pause 75)
                                                              (new-instance Note 65 450 'Piano)
                                                              (new-instance Pause 75)
                                                              (new-instance Note 63 450 'Piano)
                                                              (new-instance Pause 75)
                                                              (new-instance Note 60 450 'Piano)
                                                              (new-instance Pause 75)
                                                              (new-instance Note 58 450 'Piano)
                                                              (new-instance Pause 75)
                                                              (new-instance Note 55 450 'Piano)
                                                              (new-instance Pause 75)
                                                              (new-instance Note 53 450 'Piano)))))
                      (new-instance Pause 75)
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 43 600 'Piano)
                                          (new-instance Note 55 600 'Piano)
                                          (new-instance Note 58 600 'Piano)
                                          (new-instance Note 63 600 'Piano)))
                      (new-instance Pause 75)
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 43 600 'Piano)
                                          (new-instance Note 55 600 'Piano)
                                          (new-instance Note 58 600 'Piano)
                                          (new-instance Note 63 600 'Piano)))
                      (new-instance Pause 75)
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 44 2100 'Piano)
                                          (new-instance Note 56 2100 'Piano)
                                          (new-instance Note 60 2100 'Piano)
                                          (new-instance Note 63 2100 'Piano)))
                      (new-instance Pause 225)
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 43 600 'Piano)
                                          (new-instance Note 55 600 'Piano)
                                          (new-instance Note 58 600 'Piano)
                                          (new-instance Note 63 600 'Piano)))
                      (new-instance Pause 75)
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 43 600 'Piano)
                                          (new-instance Note 55 600 'Piano)
                                          (new-instance Note 58 600 'Piano)
                                          (new-instance Note 63 600 'Piano)))
                      (new-instance Pause 75)
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 46 2400 'Piano)
                                          (new-instance Note 56 2400 'Piano)
                                          (new-instance Note 60 2400 'Piano)
                                          (new-instance Note 63 2400 'Piano)
                                          (new-instance Note 67 2400 'Piano)))
                      (new-instance Pause 225)
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 44 1800 'Piano)
                                          (new-instance Note 56 1800 'Piano)
                                          (new-instance Note 60 1800 'Piano)
                                          (new-instance Note 63 1800 'Piano)))
                      (new-instance Pause 225)
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 46 1350 'Piano)
                                          (new-instance Note 53 1350 'Piano)
                                          (new-instance Note 58 1350 'Piano)
                                          (new-instance Note 62 1350 'Piano)))
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 39 4350 'Piano)
                                          (new-instance Note 51 4350 'Piano)
                                          (new-instance Note 55 900 'Piano)
                                          (new-instance Note 58 900 'Piano)
                                          (new-instance Note 63 900 'Piano)
                                          (new-instance SequentialMusicElement
                                                        (list (new-instance Pause 750)
                                                              (new-instance Note 65 225 'Piano)
                                                              (new-instance Note 67 450 'Piano)
                                                              (new-instance Pause 75)
                                                              (new-instance Note 65 450 'Piano)
                                                              (new-instance Pause 75)
                                                              (new-instance Note 63 450 'Piano)
                                                              (new-instance Pause 75)
                                                              (new-instance Note 60 450 'Piano)
                                                              (new-instance Pause 75)
                                                              (new-instance Note 58 450 'Piano)
                                                              (new-instance Pause 75)
                                                              (new-instance Note 55 450 'Piano)
                                                              (new-instance Pause 75)
                                                              (new-instance Note 53 450 'Piano)))))
                      (new-instance Pause 75)
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 43 600 'Piano)
                                          (new-instance Note 55 600 'Piano)
                                          (new-instance Note 58 600 'Piano)
                                          (new-instance Note 63 600 'Piano)))
                      (new-instance Pause 75)
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 43 600 'Piano)
                                          (new-instance Note 55 600 'Piano)
                                          (new-instance Note 58 600 'Piano)
                                          (new-instance Note 63 600 'Piano)))
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 44 2100 'Piano)
                                          (new-instance Note 56 2100 'Piano)
                                          (new-instance Note 60 2100 'Piano)
                                          (new-instance Note 63 2100 'Piano)))
                      (new-instance Pause 225)
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 43 600 'Piano)
                                          (new-instance Note 55 600 'Piano)
                                          (new-instance Note 58 600 'Piano)
                                          (new-instance Note 63 600 'Piano)))
                      (new-instance Pause 75)
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 43 600 'Piano)
                                          (new-instance Note 55 600 'Piano)
                                          (new-instance Note 58 600 'Piano)
                                          (new-instance Note 63 600 'Piano)))
                      (new-instance Pause 75)
                      (new-instance ParallelMusicElement
                                    (list (new-instance Note 46 2250 'Piano)
                                          (new-instance Note 56 1800 'Piano)
                                          (new-instance Note 60 1800 'Piano)
                                          (new-instance Note 63 1800 'Piano)
                                          (new-instance Note 67 1800 'Piano))))))
                      
                     
;;; Output of song to a midi file "vjindr18.mid"
(transform-to-midi-file-and-write-to-file! (convert-to-abs-times canon) "vjindr18.mid")
