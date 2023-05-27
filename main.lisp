;;;; TITLE: Garden Project
;;;;
;;;; PROGRAMMER: Ethan Smith
;;;;
;;;; DATE: May, 2023
;;;;
;;;; DESCRIPTION: RPG dungeon game. The player enters a room, which may have
;;;; monsters, loot, doors, or a stairwell. More rooms on a level are navigated
;;;; through the doors. Deeper levels can be accessed through the stairwells.
;;;; The player can buff their stats by picking up loot. Higher stats allow the
;;;; player to take on tougher monsters.
;;;;
;;;; There will be three stats
;;;; - Defense Level
;;;; - Attack Level
;;;; - Health Points
;;;;
;;;; The defense level sums up the armor levels of all the equipment donned by
;;;; the player. The attack level is simply the level of the weapon held in the
;;;; players hand.
;;;;
;;;; --COMBAT--
;;;; When a monster is encountered, combat is initiated. Combat works in this
;;;; manner:
;;;;
;;;; - base attack = (E. Defense - P. Attack)
;;;; - damage done to opponent = base-attack + (uniformly distributed random
;;;;   value from -(base_attack/2) to (base_attack/4))
;;;;
;;;; The same concept works in reverse from the monster to the player. Combat
;;;; ends when either the player or the monster loses all health.
;;;;
;;;; --SCORING--
;;;; the players score is the sum of all the damage dealt throughout his
;;;; life. When the player dies, their final score is displayed
;;;;
;;;; --MAP--
;;;; the maps are randomly generated, one staircase for each level.
;;;;
;;;;

(defun room@pos (this-room x y &optional checked-rooms)
  (cond
    ;; return nil when the room doesn't exist, or if it has already been
    ;; checked
    ((and (not this-room)
          (find (list (getf this-room :x) (getf this-room :y))
                checked-rooms :test #'equal))
     nil)

    ;; since this room does need to be checked, see if it is at x / y. If it is,
    ;; return true.
    ((and (eql x (getf this-room :x))
          (eql y (getf this-room :y)))
     ;; this room is at x and y! return this-room
     this-room)

    ;; Since this room is not at x / y, return true if any of the rooms at the
    ;; doors are at x / y.
    (t
     ;; first add this room to the checked-rooms list
     (setf checked-rooms
           (push (list (getf this-room :x)
                       (getf this-room :y))
                 checked-rooms))

     ;; return the results of this function on sub-rooms.
     ;; If a sub room doesn't exist (is nil, then the function will return nil.
     (or (room@pos (getf this-room :f) x y checked-rooms)
         (room@pos (getf this-room :b) x y checked-rooms)
         (room@pos (getf this-room :l) x y checked-rooms)
         (room@pos (getf this-room :r) x y checked-rooms)))))

(defun make-room (x y rooms-left last-room level)
  ;; one less room to create, since this one has been created.
  (decf rooms-left)

  (let ((this-room (list :x x :y y))
        (doors '(:f :b :l :r)))
    ;; --CREATE THIS ROOM--
    ;; (setf (getf this-room :stairs) (spawn-stairs level))

    ;; (setf (getf this-room :monster) (spawn-monster level
    ;;                                                (getf this-room :stairs)))

    (setf (getf this-room :loot) (spawn-loot level))

    ;; --CREATE SUB-ROOMS--
    ;;
    ;; determine whether last room was l, r, b, or f
    ;; set last-room to the respective room key, if there was a last room.
    ;; creates a circular link to the last room.
    ;;
    (if last-room
        (setf (getf this-room
                    (cond ((> x (getf last-room :x)) :l)
                          ((< x (getf last-room :x)) :r)
                          ((> y (getf last-room :y)) :f)
                          ((< y (getf last-room :y)) :b)))
              (list (cons (car last-room)
                          (cdr last-room)))))

    ;; remove doors that lead to the last room
    (setf doors (remove (get-properties this-room '(:l :r :f :b)) doors))

    ;; distribute the rooms-left between the available rooms and then create new
    ;; rooms.
    ;;
    ;; The arguments for the next call of this function are gradually being
    ;; built up through an alist. This function will be called again once for
    ;; every entry in the alist.
    ;;
    ;; the room tracker subtracts one from rooms-left because the current room
    ;; counts toward the number for rooms created.
    (let ((room-tracker (- rooms-left 1))
          new-doors-alist)
      (cond
        ;; return this room if no rooms need to be created
        ((= room-tracker 0) this-room)

        ;; there are no rooms left, then return nil.
        ((< room-tracker 0) nil)

        ;; otherwise, add sub-rooms to fill rooms-left, and return this-room.
        (t
         ;; remove doors that lead to a pre-existing room.
         ;; creates an alist which contains rooms which are avaible to create.
         ;; every entry will have the x and y parameters set.
         (dolist (dir doors nil)
           (let ((x (cond ((eql dir :l) (+ (getf this-room :x) -1))
                          ((eql dir :r) (+ (getf this-room :x) 1))
                          (t (getf this-room :x))))
                 (y (cond ((eql dir :f) (+ (getf this-room :y) 1))
                          ((eql dir :b) (+ (getf this-room :y) -1))
                          (t (getf this-room :y)))))

             (unless (room@pos this-room x y)
               (setf new-doors-alist (push (cons dir (list x y))
                                           new-doors-alist)))))

         ;; determine how many subrooms each room will have. when calling this
         ;; function, there should be at least 1 room. otherwise, no room would
         ;; be created.
         ;;
         ;; after this map, all parameters will be in each alist element.
         (setf new-doors-alist
               (map
                'list
                (lambda (door)
                  (let ((new-rooms (if (/= room-tracker 0)
                                       (+ 1 (random room-tracker))
                                       0)))
                    (decf room-tracker new-rooms)

                    ;; reconstruct the alist cell
                    (list ) (cons (car door)
                          (concatenate 'list
                                       ;; x y
                                       (cdr door)
                                       ;; rooms-left last-room level
                                       (list new-rooms this-room level)))))

                ;; mapping over new-doors-alist
                new-doors-alist))

         ;; dish out remaining rooms to first element
         ;; we are incrementing the third parameter, rooms-left.
         (incf (third (cdr (first new-doors-alist))) room-tracker)

         ;; call this function again with the arguments built up in each element
         ;; of new-doors-alist. Link each new room with the appropriate door in
         ;; this-room
         ;;
         ;; returns this-room
         (dolist (door new-doors-alist this-room)
           (setf (getf this-room (car door))
                 (list (apply #'make-room (cdr door))))))))))

(defun weapon-alist ()
  '(("Longsword" . "Versatile and balanced sword with a straight double-edged blade.")
    ("Warhammer" . "Heavy weapon with a large hammerhead on one side and a piercing spike on the other.")
    ("Battleaxe" . "Powerful weapon featuring a broad, curved blade and a spiked or blunt poll.")
    ("Dagger" . "Short, lightweight weapon ideal for quick and precise strikes.")
    ("Mace" . "Blunt weapon with a heavy head, often spiked, designed to crush opponents.")
    ("Greatsword" . "Massive two-handed sword with a long blade for devastating slashing attacks.")
    ("Rapier" . "Slender and agile weapon with a sharp-pointed blade for thrusting attacks.")
    ("Flail" . "Weapon consisting of a spiked ball attached to a handle by a chain.")
    ("Morningstar" . "Club-like weapon with a spiked ball at the end, causing crushing and piercing damage.")
    ("Halberd" . "Polearm with an axe-like blade and a spike, combining the features of an axe and a spear.")
    ("Scimitar" . "Curved sword with a single-edged blade, known for its slashing attacks.")
    ("Whip" . "Flexible weapon with a long, thin lash used for striking or entangling enemies.")
    ("Glaive" . "Long polearm with a large blade on the end, ideal for sweeping attacks.")
    ("Claymore" . "Massive two-handed sword with a double-edged blade, favored by heavy-hitting warriors.")
    ("Spear" . "Long, thrusting weapon with a pointed blade on one end and a handle on the other.")
    ("Trident" . "Three-pronged spear often associated with aquatic or sea-themed warriors.")
    ("Katana" . "Traditional Japanese sword with a curved, single-edged blade, known for its precision and deadly cuts.")
    ("Morning Glory" . "Blunt weapon with a spiked ball covered in sharp spikes, causing extensive damage.")
    ("Throwing Axes" . "Lightweight axes designed for throwing at enemies from a distance.")
    ("Nunchaku" . "Pair of wooden sticks connected by a chain or rope, known for quick and unpredictable strikes.")
    ("Wakizashi" . "Shorter companion sword to the katana, used for close-quarters combat and as a backup weapon.")
    ("Flamberge" . "Two-handed sword with a wavy, flame-like blade, causing disorientation and increased cutting potential.")
    ("Pike" . "Long polearm with a pointed spearhead, commonly used by infantry formations for thrusting attacks.")
    ("Sickle" . "Curved blade attached to a short handle, used for slashing and hooking attacks.")
    ("Giant Hammer" . "Massive two-handed hammer capable of devastating blows, often used against armored opponents.")
    ("Giant Club" . "Enormous wooden or metal club designed for brute force, causing immense damage.")))

(last )
(defun id->weapon-name (id)
  (car (nth id (weapon-alist))))

(defun id->weapon-desc (id)
  (cdr (nth id (weapon-alist))))

(defun spawn-loot (level)
  (let ((id (random (length (weapon-alist)))))
    (list :weapon-id id
          :name (id->weapon-name id)
          :desc (id->weapon-desc id)
          :power (+ 1 (random (* 10 level))))))

;; takes a loot property list and returns a string representation
(defun loot->str (loot-prop)
  (let* ((adjvs (list "feeble"
                      "weak"
                      "frail"
                      "subpar"
                      "inferior"
                      "mediocre"
                      "average"
                      "moderate"
                      "standard"
                      "adequate"
                      "solid"
                      "reliable"
                      "robust"
                      "excellent"
                      "formidable"
                      "powerful"
                      "potent"
                      "strong"
                      "fierce"
                      "mighty"
                      "legendary"
                      "mythical"
                      "godlike"
                      "unstoppable"
                      "divine"
                      "supreme"
                      "transcendent"
                      "apocalyptic"
                      "omnipotent"
                      "eternal"
                      "ultimate"))
         (index (floor (/ (getf loot-prop :power)
                          (/ 150 (length adjvs)))))
         (adjv (nth (min index (- (length adjvs) 1)) adjvs))
         (sentences (list "This weapon feels feeble in my hands."
                          "I wield a weak weapon that lacks power."
                          "The frail weapon seems fragile and delicate."
                          "The weapon's performance is subpar at best."
                          "An inferior weapon that fails to impress."
                          "The weapon's performance is mediocre, neither great nor terrible."
                          "An average weapon that gets the job done."
                          "A moderate weapon with decent power."
                          "I hold a standard weapon that performs adequately."
                          "This weapon is adequate, neither outstanding nor disappointing."
                          "The solid weapon feels reliable and sturdy."
                          "A reliable weapon that won't let me down in battle."
                          "A robust weapon that can withstand tough challenges."
                          "The excellent weapon surpasses my expectations."
                          "A formidable weapon that inspires fear in my foes."
                          "This powerful weapon gives me an edge in combat."
                          "A potent weapon capable of delivering devastating blows."
                          "The strong weapon exudes strength and force."
                          "The weapon's fierce strikes instill terror in my enemies."
                          "I wield a mighty weapon that dominates the battlefield."
                          "This legendary weapon is renowned and revered."
                          "A mythical weapon with extraordinary abilities."
                          "The weapon's godlike power is beyond comprehension."
                          "An unstoppable weapon that obliterates all opposition."
                          "This divine weapon feels blessed by the gods."
                          "A supreme weapon that reigns supreme in combat."
                          "The transcendent weapon transcends mortal limitations."
                          "An apocalyptic weapon capable of cataclysmic destruction."
                          "This omnipotent weapon possesses limitless power."
                          "An eternal weapon that stands the test of time."
                          "The ultimate weapon, unmatched in its power and prowess."))
         (s-index (floor (/ (getf loot-prop :power)
                          (/ 150 (length adjvs)))))
         (sentence (nth (min s-index (- (length sentences) 1)) sentences)))

    (format nil "'~a'~%you take the ~a ~a (~d)"
            sentence adjv (getf loot-prop :name) (getf loot-prop :power))))

(defun command-mov-p (cmd)
  "determines whether cmd is a call to mov"
  (and (string-equal "mov " cmd :end2 4)
       (or (string-equal "f" cmd :start2 4)
           (string-equal "b" cmd :start2 4)
           (string-equal "l" cmd :start2 4)
           (string-equal "r" cmd :start2 4))))

(defun command-mov (cmd room)
  "assuming cmd is mov, return the players selected room or nil"
  (getf room
        (cond ((char-equal #\f (char cmd 4)) :f)
              ((char-equal #\b (char cmd 4)) :b)
              ((char-equal #\l (char cmd 4)) :l)
              ((char-equal #\r (char cmd 4)) :r))))

(defun command-see-p (cmd) (equalp "see" cmd))
(defun command-see (cmd room)
  (format t "coords: ~d ~d~%" (getf room :x) (getf room :y))
  (write-line "MONSTERS")
  (write-line "LOOT")
  (write-line (loot->str (getf room :loot)))

  (write-string "DOORS: ")
  (dolist (d '(:f :b :l :r))
    (when (getf room d) (format t "~a " d)))
  (write-line ""))

(defun start-level (level &optional armor weapons inventory)
  (let* ((map (make-room 0 0 50 nil level))
         (room (room@pos map 0 0)))

    (format t "You shiver as you enter level ~d.~%" level)

    (do (cmd) ((equalp cmd "exit"))
      (format t "> ")
      (finish-output)
      (setf cmd (read-line))

      (cond ((command-see-p cmd) (command-see cmd room))
            ((command-mov-p cmd) (setf room (or (command-mov cmd room)
                                                room)))))))
