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
    ((or (not this-room)
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

(defun link-adjacent-rooms (room1 room2 door)
  "room2 becomes the value of door 1, in room 1."
  (setf (getf room1 door) room2))

(defun split-n-m-ways (n m)
  "returns a list of length m which sums to n. The distrobution is random.
m >= 1, n >= 0

The algorithm for this is as follows:
  - m-1 separators are generated, each on a range from 0 to n+1
  - these separators are sorted smallest to largest
  - add a separator at the beginning and end of the sequence, to ensure m ways
    are generated
  - the size way m_i is given by m_i - m_(i-1)"

  ;; set initial value to n+1, because it will be passed to random in map
  (let ((ms (make-list (1- m) :initial-element (1+ n))))
    (map-into ms #'random ms)
    (setf ms (sort ms #'<))
    (setf ms (concatenate 'list '(0) ms (list n)))
    (map 'list #'- (cdr ms) ms)))

(defun flatten-rooms (room &optional return-list)
  "returns a list of positions that contain rooms in the format of ((x y) (x y)
...)"
  (let ((pos (list (getf room :x) (getf room :y)))
        (sym (gensym "NEW-ROOM")))
    (cond
      ;; condition that room is nil or already checked.
      ((or (not room)
           (find pos return-list :test #'equal))
       nil)

      ;; otherwise, append this rooms position to the return list, and then
      ;; recursively append the positions of the sub-rooms
      (t

       (push pos return-list)

       (sort
        (append (list pos)
                (flatten-rooms (getf room :f) return-list)
                (flatten-rooms (getf room :b) return-list)
                (flatten-rooms (getf room :l) return-list)
                (flatten-rooms (getf room :r) return-list))
        #'< :key 'car)))))

(defun draw-points (points &optional character-pos)
  (let* ((sorted-by-x (sort points #'< :key 'first))
         (minx (first (first sorted-by-x)))
         (maxx (first (first (last sorted-by-x))))
         ;; note, sort destructively merges points.
         (sorted-by-y (sort points #'< :key 'second))
         (miny (second (first sorted-by-y)))
         (maxy (second (first (last sorted-by-y))))

         ;; each dimension is min-max + 1.
         (map (loop repeat (- maxy miny -1)
                    collect (make-list (- maxx minx -1) :initial-element #\#))))

    (dolist (p points)
      (let ((x (- (first p) minx))
            (y (- (second p) miny)))
        (setf (nth x (nth y map)) #\M)))

    (when character-pos
      (let ((x (- (first character-pos) minx))
            (y (- (second character-pos) miny)))
        (setf (nth x (nth y map)) #\@)))


    ;; flip along x axis so that y's appear right direction when printed
    (setf map (nreverse map))
    (dolist (line map)
      (format t "~A~%" line))))

(defun draw-map (room &optional character)
  (draw-points (flatten-rooms room) character))

(defun print-room (room)
  (format t "x,y: ~D,~D~%Loot: ~A~&F: ~A ~D~%B: ~A ~D~%L: ~A ~D~%R: ~A ~D~%"
          (getf room :x)
          (getf room :y)
          (getf (getf room :loot) :name)
          (when (getf room :f) t) (getf (getf room :f) :sub-rooms)
          (when (getf room :b) t) (getf (getf room :b) :sub-rooms)
          (when (getf room :l) t) (getf (getf room :l) :sub-rooms)
          (when (getf room :r) t) (getf (getf room :r) :sub-rooms)))

;; TODO fix problem with players getting stuck in rooms (calls where
;; rooms-left=1 are probably the culprit)
(defun make-room (x y rooms-left last-room level)
  "creates a new room with rooms-leve - 1 sub rooms"
  (declare (optimize debug))
  ;; dir-args are the arguments that will be called on this function in each
  ;; sub-direction
  (let ((this-room (list :x x :y y :f nil :b nil :l nil :r nil))
        (dir-args (copy-tree '((:f) (:b) (:l) (:r)))))

    ;;
    ;; --CREATE THIS ROOM--
    ;;

    ;; (setf (getf this-room :stairs) (spawn-stairs level))

    ;; (setf (getf this-room :monster) (spawn-monster level
    ;;                                                (getf this-room :stairs)))

    (setf (getf this-room :loot) (spawn-loot level))
    (setf (getf this-room :sub-rooms) (1- rooms-left))

    ;;
    ;; --LINK ADJACENT ROOMS--
    ;;

    ;; determine whether last room was l, r, b, or f, and set last room to it.
    (link-adjacent-rooms this-room last-room
                         (cond ((not last-room) nil) ; guard math against nil
                               ((> x (getf last-room :x)) :l)
                               ((< x (getf last-room :x)) :r)
                               ((> y (getf last-room :y)) :f)
                               ((< y (getf last-room :y)) :b)))


    ;; remove doors that lead to the last room
    (setf dir-args
          (remove (get-properties this-room '(:l :r :f :b)) dir-args :key 'car))

    ;; BUG for some reason, dir-arg and dir-args aren't being bound lexically
    ;; link adjacent rooms, and remove them from the arguments alist
    (dolist (dir-arg dir-args nil)
      (let* ((dir (car dir-arg))
             (x (cond ((eql dir :l) (+ (getf this-room :x) -1))
                      ((eql dir :r) (+ (getf this-room :x) 1))
                      (t (getf this-room :x))))
             (y (cond ((eql dir :f) (+ (getf this-room :y) 1))
                      ((eql dir :b) (+ (getf this-room :y) -1))
                      (t (getf this-room :y)))))

        ;; if there is a room at x/y, then it will be linked into this-room at
        ;; dir otherwise, that direction in this room will be nil
        (link-adjacent-rooms this-room (room@pos this-room x y) dir)

        (if (getf this-room dir)
            ;; if room@pos returned a room, delete this entry, so this direction
            ;; isn't recursed into.
            (setf dir-args
                  (remove dir dir-args :key 'car))

            ;; otherwise, put x and y in the arguments list
            (setf (cdr dir-arg) (list x y)))))

    ;; return from this function early if there aren't enough rooms left to make
    ;; more.
    (cond ((eql rooms-left 1)
           (return-from make-room this-room))
          ((<= rooms-left 0)
           (return-from make-room nil)))

    ;;
    ;; --CREATE SUB-ROOMS--
    ;;

    ;; add sub-rooms to dir-arguments
    (map 'list #'(lambda (dir-arg n) (setf dir-arg
                                      (nconc dir-arg (list n this-room level))))
         ;; first argument of above lambda functions
         dir-args

         ;; generate number of subrooms for each remaining door
         (split-n-m-ways (1- rooms-left) (length dir-args)))

    ;; (break "dir-args: ~S~%debug-test: ~S~%" dir-args debug-test)

    ;; recursively create subrooms, and link them to their respective doors.
    (dolist (dir-arg dir-args this-room)
      ;; (break "dir-args: ~S~%dir-arg: ~S~%debug-test: ~S~%" dir-args dir-arg debug-test)

      ;; NOTE: dir-args is getting modified, because you are setting something to
      ;; NIL whenever a room is given zero rooms.
      ;;
      ;; All elements are getting modified because link-adjacent-rooms is setting
      ;; the element NIL in this-room to NIL
      (link-adjacent-rooms this-room (apply #'make-room (cdr dir-arg))
                           (car dir-arg)))))

;; test function to figure out why SLY isn't showing me local variables defined
;; in let
(defun test-func (bob)
  (declare (optimize debug))
  (let ((x 4)
        (y 10)
        (z 40))
    (break)
    (+ bob x y z)))


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
