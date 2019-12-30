;;TODO  アイテムの種類を増やす
;;ボス登場
;;音つける

;;ブラシ生成
(defun set-brush ()
  (setf *brush* (make-array 7 :initial-contents
                              (list
                                (create-solid-brush (encode-rgb 128 0 255))
                                (create-solid-brush (encode-rgb 255 0 0))
                                (create-solid-brush (encode-rgb 1 255 0))
                                (create-solid-brush (encode-rgb 0 0 255))
                                (create-solid-brush (encode-rgb 255 255 0))
                                (create-solid-brush (encode-rgb 0 255 255))
                                (create-solid-brush (encode-rgb 255 0 255))))))
りせき
(defun set-font ()
  (setf *font140* (create-font "MSゴシック" :height 140)
        *font40* (create-font "MSゴシック" :height 40)
	*font30* (create-font "MSゴシック" :height 32)
        *font20* (create-font "MSゴシック" :height 25 :width 12 :weight (const +fw-bold+))))

(defun delete-font ()
  (delete-object *font140*)
  (delete-object *font40*)
  (delete-object *font30*)
  (delete-object *font20*))

(defun delete-object-array (arr)
  (loop for i across arr
     do (delete-object i)))

(defun delete-brush ()
  (delete-object-array *brush*))

(defun delete-images ()
  (delete-object-array *images*)
  (delete-object-array *p-imgs*)
  (delete-object-array *hammer-imgs*)
  (delete-object-array *monster-anime*)
  (delete-object-array *buki-imgs*))

(defun load-images ()
  (setf *images* (make-imgs-array "./img/img-*.*")
	*p-imgs* (make-imgs-array "./img/p*.*")
	*hammer-imgs* (make-imgs-array "./img/ham*.*")
	*monster-anime* (make-imgs-array "./img/anime-*.*")
	*buki-imgs* (make-imgs-array "./img/buki*.*")))

;;ゲーム初期化
(defun init-game ()
  (setf *battle?* nil
	*screen-w* (+ *map-w* *status-w*)
	*screen-h* (+ *map-h* *status-h*)
	*mag-w* 1
	*mag-h* 1
        *ido?* t
        *monster-num* 6
        *monster-level* 1
        *boss?* 0
        *end* 0
        *start-time* (get-internal-real-time)
        *ha2ne2* nil
        *copy-buki* (copy-tree *buki-d*)
        *p* (make-instance 'player :w *obj-w* :h *obj-h* :str 5 :def 2
			   :moto-w *obj-w* :moto-h *obj-h* :atk-now nil :ido-spd 2
			   :w/2 (floor *obj-w* 2) :h/2 (floor *obj-h* 2)
			   :name "もげぞう" :img +down+ :buki (make-instance 'buki :name "こん棒" :atk 3))
        *map* (make-donjon :tate *tate-block-num* :yoko *yoko-block-num*))
  (maze *map* *p*))


;;キー押したとき
(defun moge-keydown (hwnd wparam)
  (with-slots (left right down up z x c enter shift) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
        (:left (setf left t))
	(:shift (setf shift t))
        (:right (setf right t))
        (:down (setf down t))
        (:up (setf up t))
        (:return (setf enter t))
        (:keyz (setf z t))
        (:keyx (setf x t))
	(:keyc (setf c t))
        (:keyq ;; quit
          (send-message hwnd (const +wm-close+) nil nil))))))

;;キー話したとき
(defun moge-keyup (wparam)
  (with-slots (left right down up z x c enter shift) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
        (:left (setf left nil))
	(:shift (setf shift nil))
        (:right (setf right nil))
        (:down (setf down nil))
        (:up (setf up nil))
        (:return (setf enter nil))
        (:keyx (setf x nil))
	(:keyc (setf c nil))
        (:keyz (setf z nil))))))


(defun randval (n)
  (1+ (random n)))

;;'(:up :down :right :left)
(defun rand-dir (lst new-lst)
  (if (null lst)
      new-lst
      (let ((hoge (nth (random (length lst)) lst)))
	(rand-dir (remove hoge lst) (cons hoge new-lst)))))





;;判定
(defun obj-hit-p (obj1 obj2)
  (let ((obj1-px (+ (x obj1) (w/2 obj1)))
	(obj1-py (+ (y obj1) (h/2 obj1)))
	(obj2-px (+ (x obj2) (w/2 obj2)))
	(obj2-py (+ (y obj2) (h/2 obj2))))
    (and (< (abs (- obj1-px obj2-px)) (+ (w/2 obj1) (w/2 obj2)))
	 (< (abs (- obj1-py obj2-py)) (+ (h/2 obj1) (h/2 obj2))))))


;;ブロックとプレイヤーの当たり判定
(defun block-hit-p (p)
  (let ((hoge nil))
    (loop for obj in (donjon-blocks *map*)
       do (when (and (or (eq (obj-type obj) :hard-block)
			 (eq (obj-type obj) :soft-block))
		     (obj-hit-p p obj))
	    (setf hoge obj)
	    (return)))
    hoge))

;;プレイヤーとフロアにあるアイテムの当たり判定
(defun player-hit-item ()
  (loop for obj in (donjon-objects *map*)
     do
       (when (obj-hit-p *p* obj)
	 (case (obj-type obj)
	   (:key (setf (key? *p*) t)
		 (setf (img obj) +yuka+
		       (obj-type obj) :yuka))
	   (:potion
	    (setf (hp *p*) (maxhp *p*))
	    (setf (donjon-objects *map*)
		  (remove obj (donjon-objects *map*) :test #'equal)))
	   (:boots
	    (setf (ido-spd *p*) 3) ;;移動速度アップ
	    (push obj (item *p*))
	    (setf (donjon-objects *map*)
		  (remove obj (donjon-objects *map*) :test #'equal)))
	   (:door
	    (when (key? *p*)
	      (incf (stage *p*))
	      (setf (key? *p*) nil)
	      ;;(setf *map* (make-donjon :tate *tate-block-num* :yoko *yoko-block-num*))
	      (maze *map* *p*)))))))



;;ダメージ計算
(defmethod damage-calc ((atker player) defender)
  (with-slots (buki) atker
    (let* ((a1 (+ (str atker) (atk buki))))
      (max 1 (floor (* (- a1 (/ (def defender) 2)) (/ (+ 99 (random 55)) 256)))))))

;;ダメージ計算
(defmethod damage-calc ((atker enemy) defender)
  (let* ((a1 (str atker)))
    (max 1 (floor (* (- a1 (/ (def defender) 2)) (/ (+ 99 (random 55)) 256))))))

;;レヴェルアップ時ステータス上昇
(defun status-up (atker)
  (incf (maxhp atker) (1+ (random 3)))
  (incf (str atker) (random 3))
  (incf (def atker) (random 3)))

;;経験値取得
(defun player-get-exp (atker defender)
  (when (eq 'player (type-of atker))
    (incf (expe atker) (expe defender))
    (loop while (>= (expe atker) (lvup-exp atker))
       do
	 (status-up atker)
	 (incf (level atker))
	 (setf (expe atker) (- (expe atker) (lvup-exp atker)))
	 (incf (lvup-exp atker) 20))))

;;ダメージ計算して　表示する位置とか設定
(defun set-damage (atker defender)
  (with-slots (x y) defender
    (let* ((dmg-x (+ x 10))
	   (dmg-y (+ y 20))
	   (dmg-num (damage-calc atker defender))
	   (x-dir (if (eq (dir atker) :left) :left :right))
	   (dmg (make-instance 'dmg-font :x dmg-x :y dmg-y :dmg-num  dmg-num
			       :y-dir :up :x-dir x-dir
			       :maxy dmg-y :miny (- dmg-y 15))))
      (decf (hp defender) dmg-num) ;;hpを減らす
      (when (>= 0 (hp defender)) ;; hpが0以下になったら死亡
	(setf (dead defender) t)
	(player-get-exp atker defender))
      (setf (dmg defender) dmg) ;;ダメージを表示するためのもの
      )))


;;敵と武器の当たり判定
(defun buki-hit-enemy (p)
  (when (atk-now p)
    (with-slots (buki) p
      (loop for e in (donjon-enemies *map*)
	 do (case (obj-type e)
	      ((:slime :orc :hydra :dragon :brigand :briball :yote1)
	       (when (and (obj-hit-p buki e)
			  (null (dead e)))
		 (setf (atkhit p) t) ;;攻撃があたりました
		 (set-damage p e)))))))) ;;ダメージ処理
	    ;; (when (>= 0 (hp e))
	    ;;   (setf (donjon-enemies *map*)
	    ;; 	    (remove e (donjon-enemies *map*) :test #'equal))))
	 ;; (text-out *hmemdc* (format nil "x:~d y:~d w:~d h:~d" (x buki) (y buki) (w buki) (h buki))
	 ;; 	   100 600))))


;;通常時画像設定
(defun set-normal-img (p)
  (case (dir p)
    (:down  (setf (img p) +down+
		  (w p) *tate-w* (w/2 p) *tate-w/2*
		  (h p) *tate-h* (h/2 p) *tate-h/2*
		  (moto-w p) *tate-w* (moto-h p) *tate-h*))
    (:up    (setf (img p) +up+
		  (w p) *tate-w* (w/2 p) *tate-w/2*
		  (h p) *tate-h* (h/2 p) *tate-h/2*
		  (moto-w p) *tate-w* (moto-h p) *tate-h*))
    (:left  (setf (img p) +left+
		  (w p) *yoko-w* (w/2 p) *yoko-w/2*
		  (h p) *yoko-h* (h/2 p) *yoko-h/2*
		  (moto-w p) *yoko-w* (moto-h p) *yoko-h*))
    (:right (setf (img p) +right+
		  (w p) *yoko-w* (w/2 p) *yoko-w/2*
		  (h p) *yoko-h* (h/2 p) *yoko-h/2*
		  (moto-w p) *yoko-w* (moto-h p) *yoko-h*))))


;;武器の画像を設定
(defun set-buki-img (p)
  (with-slots (buki x y dir) p
    (case dir
      (:down  (setf (moto-w buki) 32 (moto-h buki) 24
		    (w buki) 32 (h buki) 34
		    (w/2 buki) 16 (h/2 buki) 17
		    (x buki) x
		    (y buki) (+ y 20)))
      (:up    (setf (moto-w buki) 32 (moto-h buki) 28
		    (w buki) 32 (h buki) 34
		    (w/2 buki) 16 (h/2 buki) 17
		    (x buki) x
		    (y buki) (- y 16)))
      (:left  (setf (moto-w buki) 18 (moto-h buki) 32
		    (w buki) 24 (h buki) 32
		    (w/2 buki) 12 (h/2 buki) 16
		    (x buki) (- x 18)
		    (y buki) y))
      (:right (setf (moto-w buki) 18 (moto-h buki) 32
		    (w buki) 24 (h buki) 32
		    (w/2 buki) 12 (h/2 buki) 16
		    (x buki) (+ x 18)
		    (y buki) y)))))

;;方向　キャラの攻撃画像
(defun set-atk-img (p)
  (case (dir p)
    (:down  (setf (img p) (atk-img p)))
    (:up    (setf (img p) (+ (atk-img p) +atk-u+)))
    (:left  (setf (img p) (+ (atk-img p) +atk-l+)))
    (:right (setf (img p) (+ (atk-img p) +atk-r+)))))
	    
;;攻撃画像更新
(defun update-atk-img (p)
  (incf (atk-c p))
  (when (zerop (mod (atk-c p) (atk-spd p)))
    (incf (atk-img p))
    (set-atk-img p)
    (when (and (>= 2 (atk-img p))
	       (null (atkhit p)))
      (buki-hit-enemy p)))
  ;;(set-buki-img p)
  (when (> (atk-img p) 2)
    (set-normal-img p)
    (setf (atk-now p) nil
	  (hammer-now p) nil
	  (atk-c p) 0
	  (atkhit p) nil
	  (atk-img p) 0)))


;;壁壊す
(defun hammer-hit-kabe (p)
  (with-slots (buki) p
    (loop for kabe in (donjon-blocks *map*)
       do (when (and (obj-hit-p buki kabe)
		     (eq (obj-type kabe) :soft-block))
	    (setf (obj-type kabe) :yuka
		  (img kabe) +yuka+)
	    ))))

;;画像右側めりこみ判定
(defun merikomi-hantei (p)
  (let ((blo  (block-hit-p p)))
    (when blo
      (setf (x p) (- (x blo) (w p))))))


;;キー入力処理
(defun update-input-key (p)
  (with-slots (left right down up z c shift) *keystate*
    (cond
      (z
       (set-atk-img p)
       (set-buki-img p)
       (setf (atk-now p) t)
       (buki-hit-enemy p))
      (c
       (set-atk-img p)
       (set-buki-img p)
       (setf (hammer-now p) t)
       (hammer-hit-kabe p))
      (left
       (when (null shift)
	 (setf (dir p) :left)
	 (set-normal-img p))
       (decf (x p) (ido-spd p))
       (when (block-hit-p p)
	 (incf (x p) (ido-spd p))))
      (right
       (when (null shift)
	 (setf (dir p) :right)
	 (set-normal-img p))
       (incf (x p) (ido-spd p))
       (when (block-hit-p p)
	 (decf (x p) (ido-spd p))))
      (up
       (when (null shift)
	 (setf (dir p) :up)
	 (set-normal-img p))
       (decf (y p) (ido-spd p))
       (let ((blo  (block-hit-p p)))
	 (when blo
	   (incf (y p) (ido-spd p))
	   (merikomi-hantei p))))
      (down
       (when (null shift)
	 (setf (dir p) :down)
	 (set-normal-img p))
       (incf (y p) (ido-spd p))
       (let ((blo  (block-hit-p p)))
	 (when blo
	   (decf (y p) (ido-spd p))
	   (merikomi-hantei p)))))))


;;プレイヤー被ダメージ処理
(defun hit-enemies-player (p)
  (loop for e in (donjon-enemies *map*)
     do (when (and (obj-hit-p p e)
		   (null (dead e)))
	  (set-damage e p) 
	  (setf (dmg-c p) 50)
	  (case (obj-type e)
	    (:briball
	     (setf (donjon-enemies *map*)
		   (remove e (donjon-enemies *map*) :test #'equal)))))))

	 

;;プレイヤーの色々更新
(defun update-player (p)
  (when (> (dmg-c p) 0)
    (decf (dmg-c p)))
  (when (zerop (dmg-c p)) ;;dmg-cが0の時
    (hit-enemies-player p)) ;;ダメージ処理
  (cond
    ((atk-now p)
     (update-atk-img p))
    ((hammer-now p)
     (update-atk-img p))
    (t
     (update-input-key p)
     (player-hit-item))))
    
;;ランダム方向へ移動 '(:up :down :right :left :stop)
(defun set-rand-dir (e)
  (loop for d in (rand-dir '(:up :down :right :left :stop) nil)
     do (case d
	  (:stop (setf (dir e) :stop)
		 (return))
	  (:up
	   (decf (y e))
	   (if (block-hit-p e)
	       (incf (y e))
	       (progn (setf (dir e) :up)
		      (return))))
	  (:down
	   (incf (y e))
	   (if (block-hit-p e)
	       (decf (y e))
	       (progn (setf (dir e) :down)
		      (return))))
	  (:right
	   (incf (x e))
	   (if (block-hit-p e)
	       (decf (x e))
	       (progn (setf (dir e) :right)
		      (return))))
	  (:left
	   (decf (x e))
	   (if (block-hit-p e)
	       (incf (x e))
	       (progn (setf (dir e) :left)
		      (return)))))))

;;スライムの移動
(defun update-slime-pos (e)
  (case (dir e)
    (:stop )
    (:up    (decf (y e) (ido-spd e))
	    (when (block-hit-p e)
	      (incf (y e) (ido-spd e))
	      (set-rand-dir e)))
    (:down  (incf (y e) (ido-spd e))
	    (when (block-hit-p e)
	      (decf (y e) (ido-spd e))
	      (set-rand-dir e)))
    (:right (incf (x e) (ido-spd e))
	    (when (block-hit-p e)
	      (decf (x e) (ido-spd e))
	      (set-rand-dir e)))
    (:left  (decf (x e) (ido-spd e))
	    (when (block-hit-p e)
	      (incf (x e) (ido-spd e))
	      (set-rand-dir e)))))

;;敵の移動
(defun update-enemy-pos (e)
  (case (dir e)
    (:stop )
    (:up    (decf (y e) (ido-spd e))
	    (let ((kabe  (block-hit-p e)))
	      (when kabe
		(setf (y e) (+ (y kabe) (h kabe)))
		(set-rand-dir e))))
    (:down  (incf (y e) (ido-spd e))
	    (let ((kabe  (block-hit-p e)))
	      (when kabe
		(setf (y e) (- (y kabe) (h e)))
		(set-rand-dir e))))
    (:right (incf (x e) (ido-spd e))
	    (let ((kabe  (block-hit-p e)))
	      (when kabe
		(setf (x e) (- (x kabe) (w e)))
		(set-rand-dir e))))
    (:left  (decf (x e) (ido-spd e))
	    (let ((kabe  (block-hit-p e)))
	      (when kabe
		(setf (x e) (+ (x kabe) (w kabe)))
		(set-rand-dir e))))))
    



;;dx dy以内のeから見たプレイヤーの方向を返す
(defun enemy-can-atk? (e dx dy)
  (let* ((diffx (- (x e) (x *p*)))
	 (diffy (- (y e) (y *p*)))
	 (absx (abs diffx))
	 (absy (abs diffy)))
    (if (and (>= dx absx)
	     (>= dy absy))
	(cond
	  ((and (>= diffx 0) (>= diffy 0))
	   (if (>= absx absy)
	       :left :up))
	  ((and (>= diffx 0) (> 0 diffy))
	   (if (>= absx absy)
	       :left :down))
	  ((and (> 0 diffx) (>= diffy 0))
	   (if (>= absx absy)
	       :right :up))
	  ((and (> 0 diffx) (> 0 diffy))
	   (if (>= absx absy)
	       :right :down))
	  (t nil))
	nil)))



;;プレイヤーにeの攻撃があたる方向 atk-x = 攻撃距離
(defun set-can-atk-dir (e atk-x atk-y)
  (let ((f-dir (enemy-can-atk? e atk-x atk-y)))
    (if f-dir
	(setf (dir e) f-dir)
	nil)))
      

;;歩行グラフィック更新
(defun update-enemy-anime-img (e)
  (with-slots (walk-c walk-func img) e
    ;;walk-counterが10を超えたら画像更新
    (when (> walk-c 15)
      (cond ;;walk-stateが 0 1 2 1 0 1 2 ってなるようにする
	((= img 0)   (setf walk-func #'+))
	((= img 1))
	((= img 2) (setf walk-func #'-)))
      (setf img (+ (funcall walk-func img 1)))
      (setf walk-c 0))))

;;スライムの行動
(defun update-slime (e change-dir-time)
  (incf (dir-c e)) ;;移動カウンター更新
  (incf (walk-c e))
  (update-enemy-anime-img e)
  (if (> (dir-c e) change-dir-time)
      (progn (set-rand-dir e)
	     (setf (dir-c e) 0))
      (update-enemy-pos e)))


;;オークの攻撃エフェクト追加
(defun add-orc-atk-effect (e dx dy)
  (let ((atk (make-instance 'enemy :img 0 :obj-type :orc-atk
			    :x (- (x e) dx) :y (- (y e) dy)
			    :str (str e)
			    :moto-w (moto-w e) :moto-h (moto-h e)
			    :w (w e) :h (h e) :w/2 (w/2 e) :h/2 (h/2 e))))
    (if (null (block-hit-p atk)) ;;ブロックにぶつかるなら追加しない
	(push atk (donjon-enemies *map*)))))

;;攻撃エフェクト出す方向
(defun check-orc-atk-effect-dir (e)
  (case (dir e)
    (:up (add-orc-atk-effect e 0 (h e)))
    (:down (add-orc-atk-effect e 0 (- (h e))))
    (:left (add-orc-atk-effect e (w e) 0))
    (:right (add-orc-atk-effect e (- (w e)) 0))))

;;攻撃エフェクト消えるまで待つ
(defun wait-atk-effect (e wait-time)
  (incf (atk-c e))
  (when (>= (atk-c e) wait-time)
    (setf (atk-now e) nil
	  (atk-c e) 0)))
			    
;;オークの行動
(defun update-orc (e)
  (cond
    ((atk-now e)
     (wait-atk-effect e *orc-atk-effect-time*))
    (t
     (incf (dir-c e)) ;;移動カウンター更新
     (incf (walk-c e))
     (when (and (= 1 (random 50)) ;;攻撃
		(set-can-atk-dir e (w e) (h e)))
       (check-orc-atk-effect-dir e)
       (setf (atk-now e) t))
     (update-enemy-anime-img e)
     (if (> (dir-c e) 40)
	 (progn (set-rand-dir e)
		(setf (dir-c e) 0))
	 (update-enemy-pos e)))))


;;ヒドラの攻撃エフェクトを敵として追加
(defun add-hydra-atk (e)
  (let ((atk (make-instance 'enemy :img 0 :obj-type :hydra-atk
			    :x (- (x e) 32) :y (y e) :str (str e)
			    :moto-w 32 :moto-h 32 :dir (dir e)
			    :w 32 :h 32 :w/2 16 :h/2 16)))
    (setf (centerx atk) (+ (x e) (w/2 e))
	  (centery atk) (+ (y e) (h/2 e)))
    (push atk (donjon-enemies *map*))))

;;ヒドラの攻撃更新 ヒドラの周りを一周させる
(defun update-hydra-atk (e)
  (incf (atk-c e))
  (let* ((radian  (/ (* (deg e) pi) 180))
	 (addx (floor (* (cos radian) 30)))
	 (addy (floor (* (sin radian) 30))))
    ;;(centerx (+ (x e) (w/2 e)))
    ;;(centery (+ (y e) (h/2 e))))
    (setf (x e) (- (+ (centerx e) addx) (w/2 e))
	  (y e) (- (+ (centery e) addy) (h/2 e)))
    (incf (deg e) 10)
    (when (>= (atk-c e) *hydra-atk-effect-time*)
      (setf (donjon-enemies *map*)
	    (remove e (donjon-enemies *map*) :test #'equal)))))


;;ヒドラの行動
(defun update-hydra (e)
  (cond
    ((atk-now e)
     (wait-atk-effect e *hydra-atk-effect-time*)) ;;TODO
    (t
     (incf (dir-c e)) ;;移動カウンター更新
     (incf (walk-c e))
     (when (and (= 1 (random 40)) ;;攻撃
		(set-can-atk-dir e (w e) (h e) ))
       (add-hydra-atk e)
       (setf (atk-now e) t))
     (update-enemy-anime-img e)
     (if (> (dir-c e) 40)
	 (progn (set-rand-dir e)
		(setf (dir-c e) 0))
	 (update-enemy-pos e)))))

;;ブリガンドのボール追加
(defun add-bri-ball (e dx dy)
  (let ((ball (make-instance 'enemy :img 0 :obj-type :briball
			     :moto-w 20 :moto-h 20 :dir (dir e)
			     :str (str e) :maxhp 1 :hp 1 :def 0
			     :w 16 :h 16 :w/2 8 :h/2 8)))
    (setf (x ball) (- (x e) dx)
	  (y ball) (- (y e) dy))
    (if (null (block-hit-p ball))
	(push ball (donjon-enemies *map*)))))
  

;;ブリガンドボールの方向
(defun add-bri-ball-dir (e)
  (case (dir e)
    (:up (add-bri-ball e 0 20))
    (:down (add-bri-ball e 0 -20))
    (:left (add-bri-ball e 20 0))
    (:right (add-bri-ball e -20 0))))
  

;;ブリガンドの行動
(defun update-brigand (e)
  (incf (dir-c e)) ;;移動カウンター更新
  (incf (walk-c e))
  (update-enemy-anime-img e)
  (when (and (= 1 (random 90)) ;;攻撃
	     (set-can-atk-dir e 600 600))
    (add-bri-ball-dir e))
  (if (> (dir-c e) 40)
      (progn (set-rand-dir e)
	     (setf (dir-c e) 0))
      (update-enemy-pos e)))

;;火追加 
(defun add-fire (e dx dy fire-n)
  (let ((fire (make-instance 'enemy :img 0 :obj-type :fire
			     :str (str e)
			     :moto-w (moto-w e) :moto-h (moto-h e)
			     :w (w e) :h (h e) :w/2 (w/2 e) :h/2 (h/2 e))))
    (setf (x fire) (- (x e) (* dx fire-n))
	  (y fire) (- (y e) (* dy fire-n)))
    (if (null (block-hit-p fire)) ;;ブロックにぶつかるなら追加しない
	(push fire (donjon-enemies *map*))
	(setf (atk-now e) nil ;;初期化
	      (atk-c e) 0))))

;;火が追加できるか
(defun check-add-fire (e)
  (incf (atk-c e)) ;;火を追加する間隔
  (when (zerop (mod (atk-c e) 30))
    (let ((fire-n (floor (atk-c e) 30)))
      (case (dir e)
	(:up (add-fire e 0 (h e) fire-n))
	(:down (add-fire e 0 (- (h e)) fire-n))
	(:left (add-fire e (w e) 0 fire-n))
	(:right (add-fire e (- (w e)) 0 fire-n)))
      (when (= fire-n 3)
	(setf (atk-now e) nil
	      (atk-c e) 0)))))





;;ドラゴンの行動
(defun update-dragon (e)
  (cond
    ((atk-now e)
     (check-add-fire e))
    (t
     (incf (dir-c e)) ;;移動カウンター更新
     (incf (walk-c e))
     (when (and (= 1 (random 50)) ;;攻撃
		(set-can-atk-dir e (* (w e) 3) (* (h e) 3)))
       (setf (atk-now e) t))
        ;;(set-enemy-atk e))
     (update-enemy-anime-img e)
     (if (> (dir-c e) 40)
	 (progn (set-rand-dir e)
		(setf (dir-c e) 0))
	 (update-enemy-pos e)))))

;;火の更新
(defun update-fire (e)
  (incf (atk-c e))
  (when (>= (atk-c e) 50) ;;火を消す
    (setf (donjon-enemies *map*)
	  (remove e (donjon-enemies *map*) :test #'equal))))

;;ぶりボールの更新
(defun update-briball (e)
  (case (dir e)
    (:up (decf (y e)))
    (:down (incf (y e)))
    (:left (decf (x e)))
    (:right (incf (x e))))
  (when (block-hit-p e)
    (setf (donjon-enemies *map*)
	  (remove e (donjon-enemies *map*) :test #'equal))))

(defun update-orc-atk-effect (e)
  (incf (atk-c e))
  (when (>= (atk-c e) *orc-atk-effect-time*)
    (setf (donjon-enemies *map*)
	  (remove e (donjon-enemies *map*) :test #'equal))))
    

;;敵の位置更新
(defun update-enemies ()
  (loop for e in (donjon-enemies *map*)
     do (when (null (dead e))
	  (case (obj-type e)
	    (:slime   (update-slime e 40))
	    (:dragon  (update-dragon e))
	    (:brigand (update-brigand e))
	    (:hydra   (update-hydra e))
	    (:hydra-atk (update-hydra-atk e))
	    (:fire    (update-fire e))
	    (:briball (update-briball e))
	    (:orc     (update-orc e))
	    (:yote1   (update-slime e 10))
	    (:orc-atk (update-orc-atk-effect e))))))




;;transparent-blt
(defun trans-blt (x y w-src h-src w-dest h-dest)
  (transparent-blt *hmemdc* x y *hogememdc* 0 0 :width-source w-src
		   :height-source h-src
		   :width-dest w-dest :height-dest h-dest
		   :transparent-color (encode-rgb 0 255 0)))




;;アニメ表示
(defun render-enemy (e anime-num)
  (when (null (dead e)) ;;死んでなかったら表示
    (select-object *hogememdc* (aref *monster-anime* (+ (img e) anime-num)))
    (trans-blt (x e) (y e) (moto-w e) (moto-h e) (w e) (h e))))

;;敵表示
(defun render-enemies ()
  (loop for e in (donjon-enemies *map*)
     do (case (obj-type e)
	  (:slime (render-enemy e +slime-anime+))
	  (:hydra (render-enemy e +hydra-anime+))
	  (:hydra-atk (render-enemy e +hydra-atk+))
	  (:dragon (render-enemy e +dragon-anime+))
	  (:brigand (render-enemy e +brigand-anime+))
	  (:fire  (render-enemy e +dragon-fire+))
	  (:briball (render-enemy e +brigand-ball+))
	  (:yote1 (render-enemy e +yote-anime+))
	  (:orc   (render-enemy e +orc-anime+))
	  (:orc-atk (render-enemy e +orc-atk+)))))



;;攻撃時の武器表示
(defun render-buki ()
  (with-slots (buki) *p*
    (select-object *hogememdc* (aref *buki-imgs* (img *p*)))
    (transparent-blt *hmemdc* (x buki) (y buki) *hogememdc* 0 0
		     :width-source (moto-w buki) :height-source (moto-h buki)
		     :width-dest (w buki) :height-dest (h buki)
		     :transparent-color (encode-rgb 0 255 0))))

;;ハンマー表示
(defun render-bukiorhammer (imgs)
  (with-slots (buki) *p*
    (select-object *hogememdc* (aref imgs (img *p*)))
    (trans-blt (x buki) (y buki) (moto-w buki) (moto-h buki) (w buki) (h buki))))

;;プレイヤー表示
(defun render-player ()
  (when (atk-now *p*)
    (render-bukiorhammer *buki-imgs*))
  (when (hammer-now *p*)
    (render-bukiorhammer *hammer-imgs*))
  (select-object *hogememdc* (aref *p-imgs* (img *p*)))
  (trans-blt (x *p*) (y *p*) (moto-w *p*) (moto-h *p*)
	     (w *p*) (h *p*)))

;;ブロック描画
(defun render-block ()
  (loop for obj in (donjon-blocks *map*)
     do
       (select-object *hogememdc* (aref *images* (img obj)))
       (trans-blt (x obj) (y obj) (moto-w obj) (moto-h obj)
		  (w obj) (h obj))))

;;床描画
(defun render-yuka ()
  (loop for obj in (donjon-yuka *map*)
     do
       (select-object *hogememdc* (aref *images* (img obj)))
       (trans-blt (x obj) (y obj) (moto-w obj) (moto-h obj)
		  (w obj) (h obj))))

;;鍵とか描画
(defun render-item ()
  (loop for obj in (donjon-objects *map*)
     do
       (select-object *hogememdc* (aref *images* (img obj)))
       (trans-blt (x obj) (y obj) (moto-w obj) (moto-h obj)
		  (w obj) (h obj))))

;;プレイヤーのステータス表示
(defun render-p-status ()
  (select-object *hmemdc* *font30*)
  (set-text-color *hmemdc* (encode-rgb 255 255 255))
  (set-bk-mode *hmemdc* :transparent)
  (text-out *hmemdc* (format nil "~a" (name *p*)) (+ *map-w* 10) 10)
  (text-out *hmemdc* (format nil "Lv:~2d" (level *p*)) (+ *map-w* 10) 50)
  (text-out *hmemdc* (format nil "HP:~2d/~2d" (hp *p*) (maxhp *p*)) (+ *map-w* 10) 90)
  (text-out *hmemdc* (format nil "攻:~2d" (str *p*)) (+ *map-w* 10) 130)
  (text-out *hmemdc* (format nil "防:~2d" (def *p*)) (+ *map-w* 10) 170)
  (text-out *hmemdc* (format nil "exp") (+ *map-w* 10) 210)
  (text-out *hmemdc* (format nil "~3d/~3d" (expe *p*) (lvup-exp *p*)) (+ *map-w* 10) 250)
  (text-out *hmemdc* (format nil "ハンマー") (+ *map-w* 10) 290)
  (text-out *hmemdc* (format nil "残り:~d回" (hammer *p*)) (+ *map-w* 10) 330)
  
  ;;(text-out *hmemdc* (format nil "w:~2d" *change-screen-w*) (+ *map-w* 10) 250)
  ;;(text-out *hmemdc* (format nil "h:~2d" *change-screen-h*) (+ *map-w* 10) 290)
  (text-out *hmemdc* (format nil "モゲアーガの塔 ~2,'0d階" (stage *p*)) 10 (+ *map-h* 10))
  (text-out *hmemdc* (format nil "持ち物") 10 (+ *map-h* 40))
  (loop for item in (item *p*)
     for i from 0 do
       (select-object *hogememdc* (aref *images* (img item)))
       (trans-blt (+ 10 (* i 36)) (+ *map-h* 80) 32 32 32 32))
  (when (key? *p*)
    (select-object *hogememdc* (aref *images* +key+))
    (trans-blt 10 (+ *map-h* 120) *obj-w* *obj-h* *obj-w* *obj-h*)))


;;バックグラウンド
(defun render-background ()
  (select-object *hmemdc* (get-stock-object :black-brush))
  (rectangle *hmemdc* 0 0 *change-screen-w* *change-screen-h*))

;;マップを表示
(defun render-map ()
  (render-background)
  (render-yuka)
  (render-block)
  (render-item))


;;HPバー表示
(defun render-hpbar (e)
  (let* ((len (floor (* (/ (hp e) (maxhp e)) *hpbar-max*)))
	 (hp-w (+ (x e) len)))
    ;;残りHP
    (select-object *hmemdc* (aref *brush* +green+))
    (rectangle *hmemdc* (x e) (- (y e) 15) hp-w (y e))
    ;;減ったHP
    (select-object *hmemdc* (aref *brush* +red+))
    (rectangle *hmemdc* hp-w (- (y e) 15) (+ hp-w (- *hpbar-max* len)) (y e))))

;;ダメージ表示
(defun render-damage (e color)
  (with-slots (dmg) e
    (when dmg
      (select-object *hmemdc* *font20*)
      
      (set-bk-mode *hmemdc* :transparent)
      ;;縁取り
      (set-text-color *hmemdc* (encode-rgb 0 0 0))
      (text-out *hmemdc* (format nil "~d" (dmg-num dmg)) (- (x dmg) 2) (y dmg))
      (text-out *hmemdc* (format nil "~d" (dmg-num dmg)) (+ (x dmg) 2) (y dmg))
      (text-out *hmemdc* (format nil "~d" (dmg-num dmg)) (x dmg) (- (y dmg) 2))
      (text-out *hmemdc* (format nil "~d" (dmg-num dmg)) (x dmg) (+ (y dmg) 2))
      ;;
      (set-text-color *hmemdc* color)
      (text-out *hmemdc* (format nil "~d" (dmg-num dmg)) (x dmg) (y dmg))
      )))

;;全てのダメージ表示
(defun render-all-damage ()
  (render-damage *p*  (encode-rgb 255 147 122))
  ;;(render-hpbar *p*)
  (loop for e in (donjon-enemies *map*)
     do (render-damage e  (encode-rgb 255 255 255))
       (when (and (/= (maxhp e) (hp e))
		  (null (dead e)))
	 (render-hpbar e))))

;;ゲーム全体描画
(defun render-game (hdc)
  (render-map)
  (render-player)
  (render-p-status)
  (render-enemies)
  (render-all-damage)
  (transparent-blt hdc 0 0 *hmemdc* 0 0
          :width-dest *change-screen-w* :height-dest *change-screen-h*
          :width-source (rect-right *c-rect*) :height-source (rect-bottom *c-rect*) 
          :transparent-color (encode-rgb 0 255 0)))

;;ダメージフォントの位置更新
(defun update-damage-font (e)
  (with-slots (dmg) e
    (when dmg
      (cond
	((eq :up (y-dir dmg))
	 (if (eq (x-dir dmg) :right)
	     (incf (x dmg))
	     (decf (x dmg)))
	 (decf (y dmg))
	 (when (= (y dmg) (miny dmg))
	   (setf (y-dir dmg) :down)))
	((eq :down (y-dir dmg))
	 (if (eq (x-dir dmg) :right)
	     (incf (x dmg))
	     (decf (x dmg)))
	 (incf (y dmg))
	 (when (= (y dmg) (maxy dmg))
	   (setf dmg nil)))))))

;;ダメージフォントの位置更新
(defun update-damage-fonts ()
  (update-damage-font *p*)
  (loop for e in (donjon-enemies *map*)
     do (update-damage-font e)))

;;アイテムの画像
(defun item-img (item)
  (case item
    (:boots +boots+)
    (:potion +potion+)
    (:hammer1 +hammer+)))

;;アイテムの靴を落とす
(defun enemy-drop-item (e)
  (let ((item (drop e)))
    (when item
      (let ((drop-item (make-instance 'obj :img (item-img item)
				      :x (x e) :y (y e) :w 32 :h 32 :w/2 16 :h/2 16
				      :moto-w 32 :moto-h 32 :obj-type item)))
	(push drop-item (donjon-objects *map*))
	(setf (donjon-drop-item *map*) (cdr (donjon-drop-item *map*)))))))
	;; (when (>= (random 5) 3)
	;;   (let ((drop-item (make-instance 'obj :img +potion+ 
	;; 				:x (x e) :y (y e) :w 32 :h 32 :w/2 16 :h/2 16
	;; 				:moto-w 32 :moto-h 32 :obj-type :potion)))
	;;     (push drop-item (donjon-objects *map*)))))))

;;死んだ敵の情報を消す
(defun delete-enemies ()
  (loop for e in (donjon-enemies *map*)
     do (when (and (null (dmg e))
		   (dead e))
	  (enemy-drop-item e)
	  (setf (donjon-enemies *map*)
		(remove e (donjon-enemies *map*) :test #'equal)))))

;;ゲームループ
(defun main-game-loop (hwnd)
  (update-player *p*)
  (update-enemies)
  (update-damage-fonts)
  (delete-enemies)
  (invalidate-rect hwnd nil nil))

;;ウィンドウサイズ変更時に画像拡大縮小する
(defun change-screen-size (lp)
  (let* ((change-w (loword lp))
	 (change-h (hiword lp)))
    (setf *change-screen-w* change-w
	  *change-screen-h* change-h)))

(defun remake-obj ()
  (loop for blo in (donjon-yuka *map*)
     do (setf (x blo) (floor (* (x blo) *mag-w*))
	      (y blo) (floor (* (y blo) *mag-h*))
	      (w blo) (floor (* (w blo) *mag-w*))
	      (h blo) (floor (* (h blo) *mag-h*))))
  (loop for blo in (donjon-blocks *map*)
     do (setf (x blo) (floor (* (x blo) *mag-w*))
	      (y blo) (floor (* (y blo) *mag-h*))
	      (w blo) (floor (* (w blo) *mag-w*))
	      (h blo) (floor (* (h blo) *mag-h*)))))


;;クライアント領域を*client-w* *client-h*に設定
(defun set-client-size (hwnd)
  (let* ((rc (get-client-rect hwnd))
         (rw (get-window-rect hwnd))
         (new-w (+ *client-w* (- (- (rect-right rw) (rect-left rw))
                               (- (rect-right rc) (rect-left rc)))))
         (new-h (+ *client-h* (- (- (rect-bottom rw) (rect-top rw))
                               (- (rect-bottom rc) (rect-top rc))))))
    (set-window-pos hwnd nil 0 0 new-w new-h '(:no-move :no-zorder))))

;;proc
(defwndproc moge-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (set-brush)
     (set-font)
     (load-images)
     (init-game)
     ;;(set-client-size hwnd)
     (setf *c-rect* (get-client-rect hwnd))
     ;;(setf *screen-w* (rect-right *c-rect*)
	;;   *screen-h* (rect-bottom *c-rect*))
     ;;(setf *screen-center-x* (+ (rect-right *c-rect*)
     ;;                         (floor (- (rect-left *c-rect*) (rect-right *c-rect*)) 2)))

     ;;(set-layered-window-attributes hwnd (encode-rgb 255 0 0) 0 (const +lwa-colorkey+))
     (with-dc (hdc hwnd)
       (setf *hmemdc* (create-compatible-dc hdc)
             *hbitmap* (create-compatible-bitmap hdc (rect-right *c-rect*) (rect-bottom *c-rect*))
	     *hogememdc* (create-compatible-dc hdc)
             *hogebitmap* (create-compatible-bitmap hdc (rect-right *c-rect*) (rect-bottom *c-rect*)))
       (select-object *hmemdc* *hbitmap*)
       (select-object *hogememdc* *hogebitmap*)))
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (render-game hdc)))
    ((const +wm-size+)
     (change-screen-size lparam))
     ;;(remake-obj))
    ((const +wm-close+)
     (destroy-window hwnd))
    ;;((const +wm-timer+)
    ;; (invalidate-rect hwnd nil nil))
    ((const +wm-keydown+)
     (moge-keydown hwnd wparam))
    ((const +wm-keyup+)
     (moge-keyup wparam))
    ((const +wm-destroy+)
     (delete-dc *hmemdc*)
     (delete-object *hbitmap*)
     (delete-dc *hogememdc*)
     (delete-object *hogebitmap*)
     (delete-brush)
     (delete-images)
     (delete-font)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


;;メイン
(defun moge ()
  (setf *random-state* (make-random-state t))
  (register-class "MOGE" (callback moge-wndproc)
		  :styles (logior-consts +cs-hredraw+ +cs-vredraw+)
                  :cursor (load-cursor :arrow)
                  :background (create-solid-brush (encode-rgb 0 255 0)))
  (let ((hwnd (create-window "MOGE"
                             :window-name "もげぞうの塔"
                             ;;:ex-styles  (logior-consts +WS-EX-LAYERED+ +ws-ex-composited+) ;;透明
                             :styles (logior-consts +ws-overlappedwindow+ +ws-visible+)
                             :x 400 :y 100 :width *screen-w* :height *screen-h*))
        (msg (make-msg)))
    ;;(init-game)
    (show-window hwnd) 
    (update-window hwnd)
    (do ((done nil))
        (done)
      (let ((m (ftw:peek-message msg :remove-msg :remove :error-p nil)))
        (cond
          (m
            ;;(let ((r (get-message msg)))
            (cond
              ((= (msg-message msg) (const +wm-quit+))
               (setf done t))
              (t
               (translate-message msg)
               (dispatch-message msg))))
          (t
            (sleep 0.01)
            (main-game-loop hwnd)))))
    (msg-wparam msg)))
