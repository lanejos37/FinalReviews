;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname into-the-woodlands-adventure) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "adventure-define-struct.rkt")
(require "macros.rkt")
(require "utilities.rkt")

;;;
;;; OBJECT
;;; Base type for all in-game objects
;;;

(define-struct object
  ;; adjectives: (listof string)
  ;; List of adjectives to be printed in the description of this object
  (adjectives)
  
  #:methods
  ;; noun: object -> string
  ;; Returns the noun to use to describe this object.
  (define (noun o)
    (type-name-string o))

  ;; description-word-list: object -> (listof string)
  ;; The description of the object as a list of individual
  ;; words, e.g. '("a" "red" "door").
  (define (description-word-list o)
    (add-a-or-an (append (object-adjectives o)
                         (list (noun o)))))
  ;; description: object -> string
  ;; Generates a description of the object as a noun phrase, e.g. "a red door".
  (define (description o)
    (words->string (description-word-list o)))
  
  ;; print-description: object -> void
  ;; EFFECT: Prints the description of the object.
  (define (print-description o)
    (begin (printf (description o))
           (newline)
           (void))))

;;;
;;; CONTAINER
;;; Base type for all game objects that can hold things
;;;

(define-struct (container object)
  ;; contents: (listof thing)
  ;; List of things presently in this container
  (contents)
  
  #:methods
  ;; container-accessible-contents: container -> (listof thing)
  ;; Returns the objects from the container that would be accessible to the player.
  ;; By default, this is all the objects.  But if you want to implement locked boxes,
  ;; rooms without light, etc., you can redefine this to withhold the contents under
  ;; whatever conditions you like.
  (define (container-accessible-contents c)
    (container-contents c))
  
  ;; prepare-to-remove!: container thing -> void
  ;; Called by move when preparing to move thing out of
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-remove! container thing)
    (void))
  
  ;; prepare-to-add!: container thing -> void
  ;; Called by move when preparing to move thing into
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-add! container thing)
    (void))
  
  ;; remove!: container thing -> void
  ;; EFFECT: removes the thing from the container
  (define (remove! container thing)
    (set-container-contents! container
                             (remove thing
                                     (container-contents container))))
  
  ;; add!: container thing -> void
  ;; EFFECT: adds the thing to the container.  Does not update the thing's location.
  (define (add! container thing)
    (set-container-contents! container
                             (cons thing
                                   (container-contents container))))

  ;; describe-contents: container -> void
  ;; EFFECT: prints the contents of the container,excluding the player
  ;; ADDED EFFECT: excludes the key from accessible contents to make it harder for the player to find the key
  (define (describe-contents container)
    (begin (local [(define other-stuff (remove bomb1 (remove key1 (remove me (container-accessible-contents container)))))]
             (if (empty? other-stuff)
                 (printf "There's nothing here.~%")
                 (begin (printf "You see:~%")
                        (for-each print-description other-stuff))))
           (void))))

;; move!: thing container -> void
;; Moves thing from its previous location to container.
;; EFFECT: updates location field of thing and contents
;; fields of both the new and old containers.
(define (move! thing new-container)
  (begin
    (prepare-to-remove! (thing-location thing)
                        thing)
    (prepare-to-add! new-container thing)
    (prepare-to-move! thing new-container)
    (remove! (thing-location thing)
             thing)
    (add! new-container thing)
    (set-thing-location! thing new-container)))

;; destroy!: thing -> void
;; EFFECT: removes thing from the game completely.
(define (destroy! thing)
  ; We just remove it from its current location
  ; without adding it anyplace else.
  (remove! (thing-location thing)
           thing))

;;;
;;; ROOM
;;; Base type for rooms and outdoor areas
;;;

(define-struct (room container)
  ())

;; new-room: string -> room
;; Makes a new room with the specified adjectives
(define (new-room adjectives)
  (make-room (string->words adjectives)
             '()))

;;;
;;; THING
;;; Base type for all physical objects that can be inside other objects such as rooms
;;;

(define-struct (thing container)
  ;; location: container
  ;; What room or other container this thing is presently located in.
  (location)
  
  #:methods
  (define (examine thing)
    (print-description thing))

  ;; prepare-to-move!: thing container -> void
  ;; Called by move when preparing to move thing into
  ;; container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-move! container thing)
    (void)))

;; initialize-thing!: thing -> void
;; EFFECT: adds thing to its initial location
(define (initialize-thing! thing)
  (add! (thing-location thing)
        thing))

;; new-thing: string container -> thing
;; Makes a new thing with the specified adjectives, in the specified location,
;; and initializes it.
(define (new-thing adjectives location)
  (local [(define thing (make-thing (string->words adjectives)
                                    '() location))]
    (begin (initialize-thing! thing)
           thing)))

;;;
;;; DOOR
;;; A portal from one room to another
;;; To join two rooms, you need two door objects, one in each room
;;;

(define-struct (door thing)
  ;; destination: container
  ;; The place this door leads to
  ;; ADDED FIELD
  ;; locked? : Whether the door is locked or not
  (destination locked?)
  
  #:methods
  ;; go: door -> void
  ;; EFFECT: Moves the player to the door's location and (look)s around.
  ;; ADDED EFFECT: Checks if the door is locked. If it is, it checks if the player has found the key yet.
  ;; One cannot proceed without finding the key to the next room
  (define (go door)
    (if (door-locked? door)
        (if (member? key1 (my-inventory))
            (begin
              (set-door-locked?! door
                                 false)
              (move! me (door-destination door))
              (look)
              (display-line "Great job getting this far. Ride your horse away to freedom"))
            (error "The room is locked. Stop being lazy and go look for the key!!!"))
        (begin (move! me (door-destination door))
               (set-door-locked?! door
                                  true)
               (look)))))

;; join: room string room string
;; EFFECT: makes a pair of doors with the specified adjectives
;; connecting the specified rooms.
(define (join! room1 adjectives1 room2 adjectives2 locked?)
  (local [(define r1->r2 (make-door (string->words adjectives1)
                                    '() room1 room2 locked?))
          (define r2->r1 (make-door (string->words adjectives2)
                                    '() room2 room1 locked?))]
    (begin (initialize-thing! r1->r2)
           (initialize-thing! r2->r1)
           (void))))

;;;
;;; PERSON
;;; A character in the game.  The player character is a person.
;;;

(define-struct (person thing)
  ())

;; initialize-person: person -> void
;; EFFECT: do whatever initializations are necessary for persons.
(define (initialize-person! p)
  (initialize-thing! p))

;; new-person: string container -> person
;; Makes a new person object and initializes it.
(define (new-person adjectives location)
  (local [(define person
            (make-person (string->words adjectives)
                         '()
                         location))]
    (begin (initialize-person! person)
           person)))

;; This is the global variable that holds the person object representing
;; the player.  This gets reset by (start-game)
(define me empty)

;; This is the global variable that holds the key object. Gets reset by (start-game)
;; so that it is hidden in different places every time (start-game) is run
(define key1 empty)

(define bomb1 empty)
;;;
;;; PROP
;;; A thing in the game that doesn't serve any purpose other than to be there.
;;;

(define-struct (prop thing)
  (;; noun-to-print: string
   ;; The user can set the noun to print in the description so it doesn't just say "prop"
   noun-to-print
   ;; examine-text: string
   ;; Text to print if the player examines this object
   examine-text
   )
  
  #:methods
  (define (noun prop)
    (prop-noun-to-print prop))

  (define (examine prop)
    (display-line (prop-examine-text prop))))

;; new-prop: string container -> prop
;; Makes a new prop with the specified description.
(define (new-prop description examine-text location)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define prop (make-prop adjectives '() location noun examine-text))]
    (begin (initialize-thing! prop)
           prop)))

;;;

;;random-element: (listofT)->T
;;Return a randomly chosen element of list.
(define (random-element list)
  (list-ref list
            (random (length list))))

;;; ADD YOUR TYPES HERE!
;;;
;;GUN
(define-struct (gun prop)
  (loaded?)
  #:methods
  (define (load gun)
    (if (member? gun (my-inventory))
        (if (not (gun-loaded? gun))
            (begin
              (set-gun-loaded?! gun true)
              (display-line "Nice. Now you stand a chance against the witch"))
            (error "The gun is already loaded")
            )
        (error "You haven't picked the gun yet!!"))))
  
(define (new-gun description examine-text location loaded?)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define gun (make-gun adjectives '() location noun examine-text loaded?))]
    (begin (initialize-thing! gun)
           gun)))
;;HORSE
(define-struct (horse prop)
  ()
  #:methods
  (define (ride horse)
    (display-line "Well-done. Another journey awaits you. Nice hunting. Don't get lost again")))

(define (new-horse description examine-text location)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define horse (make-horse adjectives '() location noun examine-text))]
    (begin (initialize-thing! horse)
           horse)))

;;BOMB
(define-struct (bomb thing)
  (hidden-location))

(define (new-bomb location hidden-location)
  (local[(define bomb
           (make-bomb '() '() location hidden-location))]
    (begin (initialize-thing! bomb)
                              bomb)))
;; KEY- For accessing locked rooms
(define-struct (key thing)
  ;;hidden-location - the place where the key is hidden
  (hidden-location))
  ;;#:methods
  ;;(define (use key door)
  ;;  (begin
  ;;    (if (member? key (my-inventory))
  ;;        (go (the door))
  ;;        (error "You've not found the key yet")))))

(define (new-key location hidden-location)
  (local[(define key
           (make-key '("key") '() location hidden-location))]
    (begin (initialize-thing! key)
                              key)))
;;FOOD
(define-struct (food prop)
  ()
  #:methods
  ;;eat
  (define (eat food)
    (if (member? food (my-inventory))
        (begin
          (destroy! food)
          (display-line "Yum Yum"))
        (error "There's nothing like that in your pocket"))))

(define (new-food description examine-text location)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define food (make-food adjectives '() location noun examine-text))]
    (begin (initialize-thing! food)
           food)))

;;GRAMOPHONE
(define-struct (gramophone prop)
  ;;on? - whether the gramophone is on or off
  (on?)
  #:methods
  (define (prepare-to-move! gramophone c)
    (error "You can't move the gramophone. It's too delicate"))
  (define (switch-on gramophone)
    (if (not (gramophone-on? gramophone))
        (begin
          (set-gramophone-on?! gramophone
                               true)
          (display-line "Enjoy but remember noise is going to alert the witch. Quiet down!"))
        (error "The gramophone is already switched on")))
  (define (switch-off gramophone)
    (if (gramophone-on? gramophone)
        (begin
          (set-gramophone-on?! gramophone
                               false)
          (display-line "Great, now look for the key!"))
        (error "The gramophone is already switched off")))
  )

(define (new-gramophone description examine-text location on?)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define gramophone (make-gramophone adjectives '() location noun examine-text on?))]
    (begin (initialize-thing! gramophone)
           gramophone)))

;;ROCKING CHAIR
(define-struct (rocking-chair prop)
  ;; seated-down? - whether the player has sat down on the chair
  ;; leaned-back? - whether the chair is leaned back
  (seated-down? leaned-back?)
  #:methods
  (define (prepare-to-move! rocking-chair c)
    (error "You can't move the rocking-chair. It's too heavy"))
  (define (sit-down rocking-chair)
    (if (not (rocking-chair-seated-down? rocking-chair))
        (begin
          (set-rocking-chair-seated-down?! rocking-chair
                                   true)
          (display-line "Relax at your own peril. The witch is almost here!!!"))
        (error "You've already sat down")))
  (define (stand-up rocking-chair)
    (if (rocking-chair-seated-down? rocking-chair)
        (begin
          (set-rocking-chair-seated-down?! rocking-chair
                                   false)
          (display-line "Glad you realized what's at stake. Nice key-searching"))
        (error "You're standing")))
  (define (lean-back rocking-chair)
    (if (rocking-chair-seated-down? rocking-chair)
        (if (not (rocking-chair-leaned-back? rocking-chair)) 
            (begin
              (set-rocking-chair-leaned-back?! rocking-chair
                                       true)
              (display-line "Hmmm, do you like that? Perfect for the witch to strangle you!"))
            (error "The chair is leaned back to the maximum"))
        (error "Consider sitting down first!"))
        ))
    
  (define (lean-up rocking-chair)
    (if (rocking-chair-seated-down? rocking-chair)
        (if (rocking-chair-leaned-back? rocking-chair) 
            (begin
              (set-rocking-chair-leaned-back?! rocking-chair
                                       false)
              (display-line "The chair is back to its initial leaning position"))
            (error "The chair is not leaned back"))
        (error "Consider sitting down first!"))
    )
  
(define (new-rocking-chair description examine-text location seated-down? leaned-back?)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define rocking-chair (make-rocking-chair adjectives '() location noun examine-text seated-down? leaned-back?))]
    (begin (initialize-thing! rocking-chair)
           rocking-chair)))

;;CABINET
(define-struct (cabinet thing)
  (opened?)
  #:methods
  (define (prepare-to-move! cabinet c)
    (error "You can't move the cabinet. It's too heavy"))
  (define (open cabinet)
    (if (not (cabinet-opened? cabinet))
        (begin
           (set-cabinet-opened?! cabinet
                            true)
           (describe-contents cabinet)
           (display-line "Take anything and eat or drink. The witch likes her prey full anyway"))
        (error "The cabinet is not closed")))

  (define (close cabinet)
    (if (cabinet-opened? cabinet)
        (begin
          (set-cabinet-opened?! cabinet
                             false)
          (display-line "Goodluck"))
        (error "The cabinet is closed"))))

(define (new-cabinet adjectives location opened?)
  (local [(define cabinet
            (make-cabinet (string->words adjectives)
                         '()
                         location
                         opened?))]
    (begin (initialize-thing! cabinet)
           cabinet)))

;;;
;;; USER COMMANDS

(define (look)
  (begin (printf "You are in ~A.~%"
                 (description (here)))
         (describe-contents (here))
         (void)))

(define-user-command (look) "Prints what you can see in the room")

(define (inventory)
  (if (empty? (my-inventory))
      (printf "You don't have anything.~%")
      (begin (printf "You have:~%")
             (for-each print-description (my-inventory)))))

(define-user-command (inventory)
  "Prints the things you are carrying with you.")

(define-user-command (examine thing)
  "Takes a closer look at the thing")

(define (take thing)
  (move! thing me))

(define-user-command (take thing)
  "Moves thing to your inventory")

(define (drop thing)
  (if (member? thing (my-inventory))
      (move! thing (here))
  (display-line "You haven't picked it yet"))
  )

(define-user-command (drop thing)
  "Removes thing from your inventory and places it in the room
")

(define (put thing container)
  (move! thing container))

(define-user-command (put thing container)
  "Moves the thing from its current location and puts it in the container.")

(define (help)
  (for-each (λ (command-info)
              (begin (display (first command-info))
                     (newline)
                     (display (second command-info))
                     (newline)
                     (newline)))
            (all-user-commands)))

(define-user-command (help)
  "Displays this help information")

(define-user-command (go door)
  "Go through the door to its destination")

(define (check condition)
  (if condition
      (display-line "Check succeeded")
      (error "Check failed!!!")))

(define-user-command (check condition)
  "Throws an exception if condition is false.")

;; display-line: object -> void
;; EFFECT: prints object using display, and then starts a new line.
(define (display-line what)
  (begin (display what)
         (newline)
         (void)))
;;;
;;; ADD YOUR COMMANDS HERE!
;;;
;;; checks whether a key is hidden under a certain prop
(define (check-under key prop)
  (if (string=? (noun prop) (noun (bomb-hidden-location bomb1)))
      (error "Ooops. Game over. You defused a bomb instead. You're dead")
      (if (string=? (noun prop) (noun(key-hidden-location key1)))
          (begin
            (take key)
            (display-line "Great job!!!Proceed to the next room before the witch catches you trespassing"))
          (error "The key isn't here"))))
;;;
;;; THE GAME WORLD - FILL ME IN
;;;
(define start-text
  (begin
    (display-line "During a hunting escapade deep in woods, you come across a empty cabin.")
    (display-line "Curious to find out who lives there, you stumble inside and the door behind you quickly closes.")
    (display-line "You're faced with the harsh reality that you are trapped. Luckily, a key is hidden in the second room. However, so is a bomb.")
    (display-line "If you check under the wrong place,you might find yourself blown to pieces. Hurry up before the old witch finds you.")
    (display-line "Time is running out!!!!!!")))
;; start-game: -> void
;; Recreate the player object and all the rooms and things.
(define (start-game)
  ;; Fill this in with the rooms you want
  (local [(define room1 (new-room "old and withered"))
          (define room2 (new-room "eerie and gloomy"))
          (define room3 (new-room "white and promising"))
          (define cabinet1 (new-cabinet "wooden and dusty" room2 false))
          (define gramophone1 (new-gramophone "gramophone" "classic" room2 false))
          (define rocking-chair1 (new-rocking-chair "wooden rocking-chair" "big" room2 false false))
          (define loc (random-element (list cabinet1 gramophone1 rocking-chair1)))
          (define bomb-hidden-loc empty)
          ;; Uses recursion to set the bomb location by checking the key location and bomb location
          ;; If they are the same, the bomb location is changed to a new place
          (define (set-loc loc)
            (local [(define temp-loc loc)]
              (if (not (string=? (noun loc) (noun (key-hidden-location key1))))
                  loc
                  (begin
                    (set! temp-loc (random-element (list cabinet1 gramophone1 rocking-chair1)))
                    (set-loc temp-loc)))
              ))
          ]
    (begin 
           (set! me (new-person "" room1))
           ;; Add join commands to connect your rooms with doors
           (join! room1 "eerie and gloomy"
                  room2 "old and withered"
                  false)
           (join! room2 "white and promising"
                  room3 "eerie and gloomy"
                  true)
           ;; Add code here to add things to your rooms
           (new-food "banana" "ripe" cabinet1)
           (new-food "cheese" "sweet" cabinet1)
           (new-gun "shot-gun" "black" room2 false)
           (new-prop "pair of boots" "brown" room3)
           (new-horse "white horse" "Appaloosa" room3)
           (set! key1 (new-key room2 (random-element (list cabinet1 gramophone1 rocking-chair1))))
           (set! bomb-hidden-loc (set-loc loc))
           (set! bomb1 (new-bomb room2 bomb-hidden-loc))
            start-text
           (check-containers!)
           (void))))

;;;
;;; PUT YOUR WALKTHROUGHS HERE
;;;
(define-walkthrough win
  (go (the door))
  (switch-on (the gramophone))
  (sit-down (the rocking-chair))
  (lean-back (the rocking-chair))
  (lean-up (the rocking-chair))
  (switch-off (the gramophone))
  (stand-up (the rocking-chair))
  (open (the cabinet))
  (take (within (the cabinet) "cheese"))
  (inventory)
  (eat (the cheese))
  (close (the cabinet))
  (take (the gun))
  (load (the gun))
  (check-under (the key) (key-hidden-location key1))
  (check (have? (the key)))
  (go (the white and promising door))
  (ride (the horse)))

(define-walkthrough fail
  (go (the door))
  (switch-on (the gramophone))
  (sit-down (the rocking-chair))
  (lean-back (the rocking-chair))
  (lean-up (the rocking-chair))
  (switch-off (the gramophone))
  (stand-up (the rocking-chair))
  (open (the cabinet))
  (take (within (the cabinet) "cheese"))
  (inventory)
  (eat (the cheese))
  (close (the cabinet))
  (take (the gun))
  (load (the gun))
  (check-under (the key) (bomb-hidden-location bomb1)))



;;;
;;; UTILITIES
;;;

;; here: -> container
;; The current room the player is in
(define (here)
  (thing-location me))

;; stuff-here: -> (listof thing)
;; All the stuff in the room the player is in
(define (stuff-here)
  (container-accessible-contents (here)))

;; stuff-here-except-me: -> (listof thing)
;; All the stuff in the room the player is in except the player.
(define (stuff-here-except-me)
  (remove me (stuff-here)))

;; my-inventory: -> (listof thing)
;; List of things in the player's pockets.
(define (my-inventory)
  (container-accessible-contents me))

;; accessible-objects -> (listof thing)
;; All the objects that should be searched by find and the.
(define (accessible-objects)
  (append (stuff-here-except-me)
          (my-inventory)))

;; have?: thing -> boolean
;; True if the thing is in the player's pocket.
(define (have? thing)
  (eq? (thing-location thing)
       me))

;; have-a?: predicate -> boolean
;; True if the player as something satisfying predicate in their pocket.
(define (have-a? predicate)
  (ormap predicate
         (container-accessible-contents me)))

;; find-the: (listof string) -> object
;; Returns the object from (accessible-objects)
;; whose name contains the specified words.
(define (find-the words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (accessible-objects)))

;; find-within: container (listof string) -> object
;; Like find-the, but searches the contents of the container
;; whose name contains the specified words.
(define (find-within container words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (container-accessible-contents container)))

;; find: (object->boolean) (listof thing) -> object
;; Search list for an object matching predicate.
(define (find predicate? list)
  (local [(define matches
            (filter predicate? list))]
    (case (length matches)
      [(0) (error "There's nothing like that here")]
      [(1) (first matches)]
      [else (error "Which one?")])))

;; everything: -> (listof container)
;; Returns all the objects reachable from the player in the game
;; world.  So if you create an object that's in a room the player
;; has no door to, it won't appear in this list.
(define (everything)
  (local [(define all-containers '())
          ; Add container, and then recursively add its contents
          ; and location and/or destination, as appropriate.
          (define (walk container)
            ; Ignore the container if its already in our list
            (unless (member container all-containers)
              (begin (set! all-containers
                           (cons container all-containers))
                     ; Add its contents
                     (for-each walk (container-contents container))
                     ; If it's a door, include its destination
                     (when (door? container)
                       (walk (door-destination container)))
                     ; If  it's a thing, include its location.
                     (when (thing? container)
                       (walk (thing-location container))))))]
    ; Start the recursion with the player
    (begin (walk me)
           all-containers)))

;; print-everything: -> void
;; Prints all the objects in the game.
(define (print-everything)
  (begin (display-line "All objects in the game:")
         (for-each print-description (everything))))

;; every: (container -> boolean) -> (listof container)
;; A list of all the objects from (everything) that satisfy
;; the predicate.
(define (every predicate?)
  (filter predicate? (everything)))

;; print-every: (container -> boolean) -> void
;; Prints all the objects satisfying predicate.
(define (print-every predicate?)
  (for-each print-description (every predicate?)))

;; check-containers: -> void
;; Throw an exception if there is an thing whose location and
;; container disagree with one another.
(define (check-containers!)
  (for-each (λ (container)
              (for-each (λ (thing)
                          (unless (eq? (thing-location thing)
                                       container)
                            (error (description container)
                                   " has "
                                   (description thing)
                                   " in its contents list but "
                                   (description thing)
                                   " has a different location.")))
                        (container-contents container)))
            (everything)))

;; is-a?: object word -> boolean
;; True if word appears in the description of the object
;; or is the name of one of its types
(define (is-a? obj word)
  (let* ((str (if (symbol? word)
                  (symbol->string word)
                  word))
         (probe (name->type-predicate str)))
    (if (eq? probe #f)
        (member str (description-word-list obj))
        (probe obj))))



;; words->string: (listof string) -> string
;; Converts a list of one-word strings into a single string,
;; e.g. '("a" "red" "door") -> "a red door"
(define (words->string word-list)
  (string-append (first word-list)
                 (apply string-append
                        (map (λ (word)
                               (string-append " " word))
                             (rest word-list)))))

;; string->words: string -> (listof string)
;; Converts a string containing words to a list of the individual
;; words.  Inverse of words->string.
(define (string->words string)
  (string-split string))

;; add-a-or-an: (listof string) -> (listof string)
;; Prefixes a list of words with "a" or "an", depending
;; on whether the first word in the list begins with a
;; vowel.
(define (add-a-or-an word-list)
  (local [(define first-word (first word-list))
          (define first-char (substring first-word 0 1))
          (define starts-with-vowel? (string-contains? first-char "aeiou"))]
    (cons (if starts-with-vowel?
              "an"
              "a")
          word-list)))

;;
;; The following calls are filling in blanks in the other files.
;; This is needed because this file is in a different langauge than
;; the others.
;;
(set-find-the! find-the)
(set-find-within! find-within)
(set-restart-game! (λ () (start-game)))
(define (game-print object)
  (cond [(void? object)
         (void)]
        [(object? object)
         (print-description object)]
        [else (write object)]))

(current-print game-print)
   
;;;
;;; Start it up
;;;

(start-game)
(look)

