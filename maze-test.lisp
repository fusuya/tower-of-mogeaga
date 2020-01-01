(defparameter *map100*
  #2A((30 30 30 30 30 30 30 30 30 30 30)
      (30 30 30 30 30 30 30 30 30 30 30)
      (30 30 30  0  0  5  0 30 30 30 30)
      (30 30 30  0  0  0  0 30 30 30 30)
      (30 30 30  0  0  0  0 30 30 30 30)
      (30 30 30  0  0  0  0 30 30 30 30)
      (30 30 30  0  0  0  0 30 30 30 30)
      (30 30 30  0  0  0  0 30 30 30 30)
      (30  3  0  0  0  0  0  0  0  6 30)
      (30 30 30 30 30  1 30 30 30 30 30)
      (30 30 30 30 30 30 30 30 30 30 30)))


;;マップデータをクリア
(defun clear-mapdate (map)
  (setf (donjon-blocks map) nil
	(donjon-enemies map) nil
	(donjon-map map) nil
	(donjon-objects map) nil
	(donjon-path map) nil
	(donjon-yuka map) nil
	(donjon-stop-list map) nil))

;;マップを壁で埋める
(defun full-block-map (map tate yoko) 
  (loop for i from 0 below tate do
       (loop for j from 0 below yoko do
	    (if (or (= i 0) (= j 0) (= i (1- tate)) (= j (1- yoko)))
		(setf (aref map i j) 40) ;;壊せない壁
		(setf (aref map i j) 30))))) ;;壊せる壁

;;ボスフロア
(defun create-boss-stage (map tate yoko) 
  (loop for i from 0 below tate do
       (loop for j from 0 below yoko do
	    (if (or (= i 0) (= j 0) (= i (1- tate)) (= j (1- yoko)))
		(setf (aref map i j) 40) ;;壊せない壁
		(setf (aref map i j) 0))))) ;;壊せる壁

(defun rand1234 (lst lst1)
  (if (null lst1)
      lst
      (let* ((n (random (length lst1)))
             (m (nth n lst1)))
       (push m lst)
       (rand1234 lst (remove m lst1)))))

(defun rem-blocks (x y map dir)
  (case dir
    (1
     (if (= 30 (aref (donjon-map map) (- y 1) x))
      (setf (aref (donjon-map map) (- y 1) x) 0)))
    (2
     (if (= 30 (aref (donjon-map map) (+ y 1) x))
      (setf (aref (donjon-map map) (+ y 1) x) 0)))
    (3
     (if (= 30 (aref (donjon-map map) y (+ x 1)))
      (setf (aref (donjon-map map) y (+ x 1)) 0)))
    (4
     (if (= 30 (aref (donjon-map map) y (1- x)))
      (setf (aref (donjon-map map) y (1- x)) 0)))))


(defun recursion (y x map dir)
  (let ((lst (rand1234 '() '(1 2 3 4)))
        (stop? t))
     (loop for i in lst do
      (case i
         (1 ;;上
           (if (< 0 (- y 2)) ;;2マス先が迷路の外か
            (cond
                ((= (aref (donjon-map map) (- y 2) x) 30) ;;2マス先が壁
                 (setf (aref (donjon-map map) (- y 2) x) 0)
                 (setf (aref (donjon-map map) (- y 1) x) 0)
                 (push (list x (- y 2)) (donjon-path map))
                 (push (list x (- y 1)) (donjon-path map))
                 (setf stop? nil)
                 (recursion (- y 2) x map 1)))))
      ;;(return))
         (2 ;;下
           (if (> (donjon-tate map) (+ y 2)) ;;2マス先が迷路の外か
            (cond
                ((= (aref (donjon-map map) (+ y 2) x) 30)
                 (setf (aref (donjon-map map) (+ y 2) x) 0)
                 (setf (aref (donjon-map map) (+ y 1) x) 0)
                 (push (list x (+ y 2)) (donjon-path map))
                 (push (list x (+ y 1)) (donjon-path map))
                 (setf stop? nil)
                 (recursion (+ y 2) x map 2)))))
      ;;(return))
         (3 ;;右
           (if (> (donjon-yoko map) (+ x 2)) ;;2マス先が迷路の外か
            (cond
                ((= (aref (donjon-map map) y (+ x 2)) 30)
                 (setf (aref (donjon-map map) y (+ x 2)) 0)
                 (setf (aref (donjon-map map) y (+ x 1)) 0)
                 (push (list (+ x 2) y) (donjon-path map))
                 (push (list (+ x 1) y) (donjon-path map))
                 (setf stop? nil)
                 (recursion y (+ x 2) map 3)))))
      ;;(return))
         (4 ;;左
           (if (< 0 (- x 2)) ;;2マス先が迷路の外か
            (cond
                ((= (aref (donjon-map map) y (- x 2)) 30)
                 (setf (aref (donjon-map map) y (- x 2)) 0)
                 (setf (aref (donjon-map map) y (- x 1)) 0)
                 (push (list (- x 2) y) (donjon-path map))
                 (push (list (- x 1) y) (donjon-path map))
                 (setf stop? nil)
                 (recursion y (- x 2) map 4)))))))
    (if stop? ;;行き止まりだったら
     (progn
    ;;(scr-format "y=~d x=~d~%" y x);;テスト用
        (rem-blocks x y map dir)
        (push (list y x) (donjon-stop-list map)) ;;行き止まりの座標リスト
        ;;(setf (aref (donjon-map map) y x) 3)
	))))

;;numとは異なるlen内の乱数を返す((diff-num 0 1)だと無限ループになる)
(defun diff-num (num len)
  (let ((hoge (random len)))
    (if (= hoge num)
     (diff-num num len)
     hoge)))

;;マップに鍵とドアをセット
(defun set-key-door (map)
  (let* ((len (length (donjon-stop-list map)))
         (k (random len)) (b (diff-num k len))
         (door (nth k (donjon-stop-list map)))
         (key (nth b (donjon-stop-list map))))
    (setf (aref (donjon-map map) (car door) (cadr door)) 2
	  (aref (donjon-map map) (car key) (cadr key)) 3)))


;;出現する敵 階層によって出現率を変える
(defun appear-enemy ()
  (let* ((m (random 101))
	 (slime-rate-max (- 70 (* (stage *p*) 3)))
	 (orc-rate-min   (1+ slime-rate-max))
	 (orc-rate-max   (+ (- 30 (stage *p*)) orc-rate-min))
	 (bri-rate-min (1+ orc-rate-max))
	 (bri-rate-max (+ 10 (stage *p*) bri-rate-min))
	 (hydra-rate-min (1+ bri-rate-max))
	 (hydra-rate-max (+ 10 (stage *p*) hydra-rate-min))
	 (dragon-rate-min (1+ hydra-rate-max))
	 (dragon-rate-max (+ 10 (stage *p*) dragon-rate-min)))
    ;;(format t "slime:~d~%orc~d~%bri:~d~%hydra:~d~%dragon:~d~%"
	;;    slime-rate-max orc-rate-max bri-rate-max hydra-rate-max dragon-rate-max)
    (cond
      ((>= 1 m 0) :yote1)
      ((>= slime-rate-max m 2) :slime)
      ((>= orc-rate-max m orc-rate-min) :orc)
      ((>= bri-rate-max m bri-rate-min) :brigand)
      ((>= hydra-rate-max m hydra-rate-min) :hydra)
      ((>= dragon-rate-max m dragon-rate-min) :dragon)
      (t :slime))))

;;
(defun random-enemy ()
  (case (random 6)
    (1 :slime)
    (2 :orc)
    (3 :brigand)
    (4 :hydra)
    (5 :dragon)
    (0 :yote1)))

;;ドロップアイテムを返す 
(defun drop-item-type (map)
  (car (donjon-drop-item map)))

;;敵生成
(defun create-enemy (e-type e-pos hp str def expe ido-spd)
  (make-instance 'enemy :x (* (car e-pos) *blo-w46*)
		 :y (* (cadr e-pos) *blo-h46*)
		 :moto-w *obj-w* :moto-h *obj-h*
		 :str str :def def :hp hp :maxhp hp
		 :ido-spd ido-spd :expe expe
		 :w *obj-w* :h *obj-h*
		 :w/2 (floor *obj-w* 2) :h/2 (floor *obj-h* 2)
		 :obj-type e-type
		 :img 1))

;;プレイヤーのいる階層で敵の強さが変わる
(defun create-enemies (e-pos e-type)
  (case e-type
    (:slime   (create-enemy e-type e-pos
			    (+ 6 (floor (random (stage *p*)) 2))
			    (+ 1 (floor (random (stage *p*)) 3))
			    (+ 1 (floor (random (stage *p*)) 3))
			    (+ 3 (floor (random (stage *p*)) 3))
			    1))
    (:orc     (create-enemy e-type e-pos
			    (+ 10 (floor (random (stage *p*)) 2))
			    (+ 4 (floor (random (stage *p*)) 2))
			    (+ 1 (floor (random (stage *p*)) 3))
			    (+ 5 (floor (random (stage *p*)) 2))
			    1))
    (:brigand (create-enemy e-type e-pos
			    (+ 6 (floor (random (stage *p*)) 2))
			    (+ 2 (floor (random (stage *p*)) 3))
			    (+ 2 (floor (random (stage *p*)) 3))
			    (+ 7 (floor (random (stage *p*)) 2))
			    2))
    (:hydra   (create-enemy e-type e-pos
			    (+ 12 (floor (random (stage *p*)) 1))
			    (+ 2 (floor (random (stage *p*)) 3))
			    (+ 5 (floor (random (stage *p*)) 2))
			    (+ 10 (floor (random (stage *p*)) 2))
			    1))
    (:dragon  (create-enemy e-type e-pos
			    (+ 20 (floor (random (stage *p*)) 2))
			    (+ 5 (floor (random (stage *p*)) 2))
			    (+ 6 (floor (random (stage *p*)) 2))
			    (+ 20 (floor (random (stage *p*)) 2))
			    2))
    (:yote1   (create-enemy e-type e-pos 3 3 50 300 20))))

;;敵を配置する
(defun set-enemies (map)
  (let ((enemy-num (+ 3 (random (+ 3 (floor (stage *p*) 5)))))) ;;1フロアに出る敵の数
    (loop for i from 0 to enemy-num do
	 (let* ((e-pos (nth (random (length (donjon-path map))) (donjon-path map)))
		(e-type (appear-enemy))
		(e (create-enemies e-pos e-type)))
	   (when (= i 0)
	     (setf (drop e) (drop-item-type map)))
	   (push e (donjon-enemies map))
	   (setf (donjon-path *map*)
		 (remove e-pos (donjon-path *map*) :test #'equal))))))

;;ボス配置
(defun set-boss (map)
  (let* ((e-pos (list (floor (donjon-yoko map) 2) 1))
	 (boss (make-instance 'enemy :x (* (car e-pos) *blo-w46*)
			      :y (* (cadr e-pos) *blo-h46*)
			      :moto-w 64 :moto-h 64
			      :str 50 :def 20 :hp 100 :maxhp 100
			      :ido-spd 2 :expe 0
			      :w 64 :h 64
			      :w/2 32 :h/2 32
			      :obj-type :boss
			      :img 1)))
    (push boss (donjon-enemies map))))

;;マップ設定
(defun set-map (map moto)
  (setf (donjon-tate map) 11
        (donjon-yoko map) 11)
  (loop for i from 0 below (donjon-tate map) do
       (loop for j from 0 below (donjon-yoko map) do
	    (setf (aref (donjon-map map) i j) (aref moto i j)))))


;;オブジェクトの画像
(defun map-obj-img (num)
  (case num
    (30 +soft-block+) ;; 壁
    (40 +hard-block+) ;;壊せない壁
    (0  +yuka+) ;;床
    (4  +soft-block+) ;; 薬
    (5  +soft-block+) ;;ボス
    (3  +key+) ;; 鍵
    (2  +door+) ;; 下り階段
    (6  +soft-block+) ;; イベント
    (7  +soft-block+))) ;; 中ボス ハツネツエリア

;;オブジェクトの画像のタイプ
(defun map-obj-type (num)
  (case num
    (30 :soft-block) ;; 壁
    (40 :hard-block) ;;壊せない壁
    (0  :yuka)
    (4  :potion) ;; 薬
    (5  :boss) ;;ボス
    (3  :key) ;; 鍵
    (2  :door) ;; ドア
    (6  :event) ;; イベント
    (7  :ha2ne2))) ;; 中ボス ハツネツエリア

;;おbジェクトの位置設定
(defun set-obj-info (map)
  (loop for y from 0 below (donjon-tate map) do
       (loop for x from 0 below (donjon-yoko map) do
	    (let* ((obj-num (aref (donjon-map map) y x))
		   (img (map-obj-img obj-num))
		   (obj-type (map-obj-type obj-num))
		   (obj-w *blo-w46*) (obj-h *blo-h46*)
		   (posx (* x obj-w)) (posy (* y obj-h))
		   (obj (make-instance 'obj :x posx :y posy
				   :x2 (+ posx obj-w) :y2 (+ posy obj-h)
				   :w obj-w :h obj-h :w/2 (floor obj-w 2)
				   :moto-w *obj-w* :moto-h *obj-h*
				   :h/2 (floor obj-h 2)
				   :obj-type obj-type :img img)))
	      (case obj-num
		(0
		 (push obj (donjon-yuka map)))
		(40
		 (push obj (donjon-blocks map)))
		((30)
		 (let ((blo (make-instance 'obj :x posx :y posy
				   :x2 (+ posx *blo-w*) :y2 (+ posy *blo-w*)
				   :w *blo-w* :h *blo-h* :w/2 (floor *blo-w* 2)
				   :moto-w *blo-w* :moto-h *blo-h*
				   :h/2 (floor *blo-w* 2)
				   :obj-type obj-type :img img))
		       (yuka (make-instance 'obj :x posx :y posy
				   :x2 (+ posx *blo-w46*) :y2 (+ posy *blo-w46*)
				   :w *blo-w46* :h *blo-h46* :w/2 (floor *blo-w46* 2)
				   :moto-w *obj-w* :moto-h *obj-h*
				   :h/2 (floor *blo-w46* 2)
				   :obj-type :yuka :img (map-obj-img 0))))
		   (push blo (donjon-blocks map))
		   (push yuka (donjon-yuka map))))
		((2 3)
		 (let ((yuka (make-instance 'obj :x posx :y posy
				   :x2 (+ posx *blo-w46*) :y2 (+ posy *blo-w46*)
				   :w *blo-w46* :h *blo-h46* :w/2 (floor *blo-w46* 2)
				   :moto-w *obj-w* :moto-h *obj-h*
				   :h/2 (floor *blo-w46* 2)
				   :obj-type :yuka :img (map-obj-img 0))))
		   (push yuka (donjon-yuka map))
		   (push obj (donjon-objects map)))))))))
		  

;;迷路マップ生成
(defun maze (map p)
  (let* ((x 0)
         (startx 0)
         (y 0)
         (starty 0))
    (clear-mapdate map)
    (setf (donjon-map map) (make-array (list (donjon-tate map) (donjon-yoko map))));;マップ配列作成
    (full-block-map (donjon-map map) (donjon-tate map) (donjon-yoko map)) ;;マップをブロックで埋める
    (cond
      ((= (stage p) 30) ;; 100階は固定マップ
       (create-boss-stage (donjon-map map) (donjon-tate map) (donjon-yoko map))
       (setf (y p) (* (- (donjon-tate map) 2) *blo-w46*)
	     (x p) (* (floor (donjon-yoko map) 2) *blo-h46*));;プレイヤー位置
       (set-boss map))
      (t
       ;;奇数座標を初期位置にする
       (setf x (random (floor (donjon-yoko map) 2))
	     y (random (floor (donjon-tate map) 2))
	     startx (+ (* x 2) 1)
	     starty (+ (* y 2) 1))
       (setf (aref (donjon-map map) starty startx) 0) ;;初期位置を通路にする
       (recursion starty startx map 0) ;;迷路生成
       (loop until (<= 2 (length (donjon-stop-list map)))
             do
             ;; 行き止まりが 1 つしか無かったのでやりなおし
             (full-block-map (donjon-map map) (donjon-tate map) (donjon-yoko map))
             (setf (donjon-stop-list map) nil)
             (setf (aref (donjon-map map) starty startx) 0)
             (recursion starty startx map 0))
       ;;(setf (aref (donjon-map map) starty startx) 1) ;;主人公の位置
       (setf (y p) (* starty *blo-w46*)
	     (x p) (* startx *blo-h46*)) ;;初期位置
       ;;パーティーの位置に敵を配置しないようにする
       (setf (donjon-path map) (remove (list (x p) (y p)) (donjon-path map) :test #'equal))
       (set-enemies map) ;;敵を配置
       (set-key-door map))) ;;鍵とドアを配置
    (set-obj-info map)))
          ;;(d-map-map mapn)))
          ;;(test-show-map (d-map-map mapn))))
