(defparameter *map100*
  #2A((30 30 30 30 30 30 30 30 30 30 30)
      (30 30 30 30 30 30 30 30 30 30 30)
      (30 30 30 30  0  5  0 30 30 30 30)
      (30 30 30 30  0  0  0 30 30 30 30)
      (30 30 30 30 30  0 30 30 30 30 30)
      (30 30 30 30 30  0 30 30 30 30 30)
      (30 30 30 30 30  0 30 30 30 30 30)
      (30 30 30 30 30  0 30 30 30 30 30)
      (30  3  0  0  0  0  0  0  0  6 30)
      (30 30 30 30 30  1 30 30 30 30 30)
      (30 30 30 30 30 30 30 30 30 30 30)))


(defun init-map (map tate yoko) ;;マップを壁で埋める
  (loop for i from 0 below tate do
       (loop for j from 0 below yoko do
	    (if (or (= i 0) (= j 0) (= i (1- tate)) (= j (1- yoko)))
		(setf (aref map i j) 40) ;;壊せない壁
		(setf (aref map i j) 30))))) ;;壊せる壁

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


;;敵の種類ランダム
(defun random-enemy ()
  (case (random 5)
    (0 :slime)
    (1 :orc)
    (2 :brigand)
    (3 :hydra)
    (4 :dragon)))

;;階層ごとのドロップアイテム
(defun drop-item-type ()
  (nth (stage *p*) *drop-item*))
    

;;敵を配置する
(defun set-enemies (map)
  (let ((len (length (donjon-path map)))
        (enemy-num (+ 3 (random 6))))
    (loop for i from 0 to enemy-num do
	 (let* ((e-pos (nth (random len) (donjon-path map)))
		(e (make-instance 'player :x (* (car e-pos) *r-blo-w*)
				  :y (* (cadr e-pos) *r-blo-h*)
				  :moto-w 32 :moto-h 32
				  :str 10 :def 3
				  :w 40 :h 40 ::w/2 20 :h/2 20
				  :obj-type (random-enemy)
				  :img 1)))
	   (when (= i 0)
	     (setf (drop e) (drop-item-type)))
	   (push e (donjon-enemies map))))))

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
		   (posx (* x *r-blo-w*)) (posy (* y *r-blo-h*))
		   (obj (make-instance 'obj :x posx :y posy
				   :x2 (+ posx *r-blo-w*) :y2 (+ posy *r-blo-h*)
				   :w *r-blo-w* :h *r-blo-h* :w/2 (floor *r-blo-w* 2)
				   :h/2 (floor *r-blo-h* 2)
				   :obj-type obj-type :img img)))
	      (case obj-num
		(0
		 (push obj (donjon-yuka map)))
		((30 40)
		 (push obj (donjon-blocks map)))
		((2 3)
		 (let ((yuka (make-instance 'obj :x posx :y posy
				   :x2 (+ posx *r-blo-w*) :y2 (+ posy *r-blo-h*)
				   :w *r-blo-w* :h *r-blo-h* :w/2 (floor *r-blo-w* 2)
				   :h/2 (floor *r-blo-h* 2)
				   :obj-type :yuka :img (map-obj-img 0))))
		   (push yuka (donjon-yuka map))
		   (push obj (donjon-objects map)))))))))
		  

	    
(defun maze (map p)
  (let* ((x 0)
         (startx 0)
         (y 0)
         (starty 0))
    (setf (donjon-map map) (make-array (list (donjon-tate map) (donjon-yoko map))));;マップ配列作成
    (init-map (donjon-map map) (donjon-tate map) (donjon-yoko map)) ;;マップ初期化
    (setf (donjon-stop-list map) nil
          (donjon-enemies map) nil
          (donjon-path map) nil)
    (cond
      ((= (tower-lv p) 100) ;; 100階は固定マップ
       (set-map map *map100*))
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
             (init-map (donjon-map map) (donjon-tate map) (donjon-yoko map))
             (setf (donjon-stop-list map) nil)
             (setf (aref (donjon-map map) starty startx) 0)
             (recursion starty startx map 0))
       ;;(setf (aref (donjon-map map) starty startx) 1) ;;主人公の位置
       (setf (y p) (* starty *r-blo-w*)
	     (x p) (* startx *r-blo-h*)) ;;初期位置
       ;;パーティーの位置に敵を配置しないようにする
       (setf (donjon-path map) (remove (list (x p) (y p)) (donjon-path map) :test #'equal))
       (set-enemies map) ;;敵を配置
       (cond
        ((= (tower-lv p) 50)
         (set-key-door map))
        (t (set-key-door map)))))
    (set-obj-info map)))
          ;;(d-map-map mapn)))
          ;;(test-show-map (d-map-map mapn))))
