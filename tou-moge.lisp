;;TODO 
;;ボスの攻撃

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

(defun set-font ()
  (setf *font140* (create-font "MSゴシック" :height 140)
	*font90* (create-font "MSゴシック" :height 90)
	*font70* (create-font "MSゴシック" :height 70)
        *font40* (create-font "MSゴシック" :height 40)
	*font30* (create-font "MSゴシック" :height 22 :width 9)
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
  (delete-object *p-img*)
  (delete-object *objs-img*)
  (delete-object *p-atk-img*)
  (delete-object *hammer-img*)
  (delete-object *anime-monsters-img*)
  (delete-object *buki-img*))

(defun load-images ()
  (setf *objs-img* (load-image "./img/objs-img2.bmp" :type :bitmap
			     :flags '(:load-from-file :create-dib-section))
	*p-img* (load-image "./img/p-ido-anime.bmp" :type :bitmap
			     :flags '(:load-from-file :create-dib-section))
	*p-atk-img* (load-image "./img/p-atk-anime.bmp" :type :bitmap
			     :flags '(:load-from-file :create-dib-section))
	*hammer-img* (load-image "./img/hammer-anime.bmp" :type :bitmap
			     :flags '(:load-from-file :create-dib-section))
	*anime-monsters-img* (load-image "./img/monsters.bmp" :type :bitmap
					 :flags '(:load-from-file :create-dib-section))
	*buki-img* (load-image "./img/buki-anime.bmp" :type :bitmap
			     :flags '(:load-from-file :create-dib-section))))


(defun init-keystate ()
  (with-slots (left right down up z x c enter shift) *keystate*
    (setf left nil right nil down nil up nil z nil x nil c nil enter nil shift nil)))


;;ゲーム初期化
(defun init-game ()
  (setf *p* (make-instance 'player :w *p-w* :h *p-h* :str 5 :def 2 :stage 1 :state :title
			   :hp 30 :maxhp 30 :name *name*
			   :moto-w *p-w* :moto-h *p-h* :atk-now nil :ido-spd 2 :level 1
			   :w/2 (floor *obj-w* 2) :h/2 (floor *obj-h* 2) :hammer 3
			   :img +down+ :buki (make-instance 'buki :name "こん棒" :atk 1 :w *p-w* :h *p-h* :moto-w *p-w* :moto-h *p-h* :w/2 *p-w/2* :h/2 *p-h/2* :img 0))
        *map* (make-donjon :tate *tate-block-num* :yoko *yoko-block-num*))
  (init-keystate)
  (maze *map* *p*))

;;効果音ならす
(defun sound-play (path)
  (play-sound path '(:filename :async)))


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




;;時間変換
(defun get-hms (n)
  (multiple-value-bind (h m1) (floor n 3600000)
    (multiple-value-bind (m s1) (floor m1 60000)
      (multiple-value-bind (s ms1) (floor s1 1000)
	(multiple-value-bind (ms) (floor ms1 10)
	  (values h m s ms))))))

;;1以上のランダムな数
(defun randval (n)
  (1+ (random n)))

(defun rand+- (n)
  (let ((a (1+ (random n))))
    (if (= (random 2) 1)
	a
	(- a))))

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
	   (:key
	    (sound-play *get-item-wav*)
	    (setf (key? *p*) t
		  (img obj) +yuka+
		  (obj-type obj) :yuka))
	   (:potion
	    (sound-play *get-potion-wav*)
	    (setf (hp *p*) (maxhp *p*)
		  (donjon-objects *map*)
		  (remove obj (donjon-objects *map*) :test #'equal)))
	   (:sword
	    (with-slots (buki) *p*
	      (sound-play *get-item-wav*)
	      (setf (donjon-objects *map*)
		    (remove obj (donjon-objects *map*) :test #'equal))
	      (when (> *buki-list-len* (atk buki))
		(setf (name buki) (nth (atk buki) *buki-list*))
		(incf (atk buki))))) ;;武器の攻撃力は１づつ上がる))
	   (:hammer
	    (sound-play *get-item-wav*)
	    (incf (hammer *p*))
	    (setf (donjon-objects *map*)
		  (remove obj (donjon-objects *map*) :test #'equal)))
	   (:boots
	    (sound-play *get-item-wav*)
	    (push obj (item *p*))
	    (setf (ido-spd *p*) 3 ;;移動速度アップ
		  (boots? *p*) t
		  (donjon-objects *map*)
		  (remove obj (donjon-objects *map*) :test #'equal)))
	   (:door
	    (when (key? *p*)
	      (sound-play *door-wav*)
	      (incf (stage *p*))
	      (setf (key? *p*) nil)
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
	 (sound-play *lvup-wav*)
	 (status-up atker)
	 (incf (level atker))
	 (setf (expe atker) (- (expe atker) (lvup-exp atker)))
	 (incf (lvup-exp atker) 20))))

;;ダメージ計算して　表示する位置とか設定
(defun set-damage (atker defender)
  (with-slots (x y obj-type hp atk-spd) defender
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
      (when (and (eq obj-type :boss) ;;ボス発狂モード
		 (>= 50 hp))
	(setf (atk-spd defender) 40))
      )))


;;敵と武器の当たり判定
(defun buki-hit-enemy (p)
  (when (atk-now p)
    (with-slots (buki) p
      (loop for e in (donjon-enemies *map*)
	 do (case (obj-type e)
	      ((:slime :orc :hydra :dragon :brigand :briball :yote1 :toge :boss)
	       (when (and (obj-hit-p buki e)
			  (null (dead e)))
		 (sound-play *atk-enemy-wav*)
		 (setf (atkhit p) t) ;;攻撃があたりました
		 (set-damage p e)))))))) ;;ダメージ処理



;;武器の画像を設定
(defun set-buki-pos (p)
  (with-slots (buki x y dir) p
    (case dir
      (:down  (setf (x buki) x
		    (y buki) (+ y 20)))
      (:up    (setf (x buki) x
		    (y buki) (- y 20)))
      (:left  (setf (x buki) (- x 18)
		    (y buki) y))
      (:right (setf (x buki) (+ x 18)
		    (y buki) y)))))

;;方向　キャラの攻撃画像
(defun set-atk-img (p)
  (setf (img p) 0))
	    
;;攻撃画像更新
(defun update-atk-img (p)
  (incf (atk-c p))
  (when (zerop (mod (atk-c p) (atk-spd p)))
    (incf (img p))
    (when (and (>= 2 (img p))
	       (null (atkhit p)))
      (buki-hit-enemy p)))
  (when (> (img p) 2)
    (setf (atk-now p) nil
	  (hammer-now p) nil
	  (atk-c p) 0
	  (atkhit p) nil
	  (img p) 0)))


;;壁壊す
(defun hammer-hit-kabe (p)
  (with-slots (buki) p
    (let ((hit? nil))
      (loop for kabe in (donjon-blocks *map*)
	 do (when (and (obj-hit-p buki kabe)
		       (eq (obj-type kabe) :soft-block))
	      (setf hit? t)
	      (setf (donjon-blocks *map*)
		    (remove kabe (donjon-blocks *map*) :test #'equal))))
	      ;;(setf (obj-type kabe) :yuka
		;;    (img kabe) +yuka+)))
      (when hit?
	(sound-play *atk-block-wav*)
	(decf (hammer *p*))))))

;;画像右側めりこみ判定
(defun merikomi-hantei (p)
  (let ((blo  (block-hit-p p)))
    (when blo
      (setf (x p) (- (x blo) (w p))))))


;;歩行グラフィック更新
(defun update-ido-anime-img (e)
  (with-slots (walk-c walk-func img) e
    (incf (walk-c e))
    ;;walk-counterが10を超えたら画像更新
    (when (> walk-c 15)
      (cond ;;walk-stateが 0 1 2 1 0 1 2 ってなるようにする
	((= img 0)   (setf walk-func #'+))
	((= img 1))
	((= img 2) (setf walk-func #'-)))
      (setf img (+ (funcall walk-func img 1)))
      (setf walk-c 0))))

;;キー入力処理
(defun update-input-key (p)
  (with-slots (left right down up z c shift) *keystate*
    (cond
      (z
       (set-atk-img p)
       (set-buki-pos p)
       (setf (atk-now p) t)
       (buki-hit-enemy p))
      ((and c (> (hammer *p*) 0))
       (set-atk-img p)
       (set-buki-pos p)
       (setf (hammer-now p) t)
       (hammer-hit-kabe p))
      (left
       (when (and (null shift)
		  (not (eq (dir p) :left)))
	 (setf (dir p) :left))
       (update-ido-anime-img p)
       (decf (x p) (ido-spd p))
       (when (block-hit-p p)
	 (incf (x p) (ido-spd p))))
      (right
       (when (and (null shift)
		  (not (eq (dir p) :right)))
	 (setf (dir p) :right))
       (update-ido-anime-img p)
       (incf (x p) (ido-spd p))
       (when (block-hit-p p)
	 (decf (x p) (ido-spd p))))
      (up
       (when (and (null shift)
		  (not (eq (dir p) :up)))
	 (setf (dir p) :up))
       (update-ido-anime-img p)
       (decf (y p) (ido-spd p))
       (let ((blo  (block-hit-p p)))
	 (when blo
	   (incf (y p) (ido-spd p))
	   (merikomi-hantei p))))
      (down
       (when (and (null shift)
		  (not (eq (dir p) :down)))
	 (setf (dir p) :down))
       (update-ido-anime-img p)
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
	  (sound-play *damage-wav*)
	  (set-damage e p) 
	  (setf (dmg-c p) 50)
	  (case (obj-type e)
	    (:briball
	     (setf (donjon-enemies *map*)
		   (remove e (donjon-enemies *map*) :test #'equal)))))))





;;プレイヤーの色々更新
(defun update-player (p)
  (when (> (dmg-c p) 0) ;;敵からの攻撃食らう間隔
    (decf (dmg-c p)))
  (when (zerop (dmg-c p)) ;;dmg-cが0の時
    (hit-enemies-player p)) ;;ダメージ処理
    ;;(when (dead p) ;;ゲームオーバー
      ;;(setf (state p) :dead)))
  ;;(when (null (dead p))
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
      


;;スライムの行動
(defun update-slime (e change-dir-time)
  (incf (dir-c e)) ;;移動カウンター更新
  (update-ido-anime-img e)
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
     (when (and (= 1 (random 50)) ;;攻撃
		(set-can-atk-dir e (w e) (h e)))
       (check-orc-atk-effect-dir e)
       (setf (atk-now e) t))
     (update-ido-anime-img e)
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
     (when (and (= 1 (random 40)) ;;攻撃
		(set-can-atk-dir e (w e) (h e) ))
       (add-hydra-atk e)
       (setf (atk-now e) t))
     (update-ido-anime-img e)
     (if (> (dir-c e) 40)
	 (progn (set-rand-dir e)
		(setf (dir-c e) 0))
	 (update-enemy-pos e)))))

;;ブリガンドのボール追加
(defun add-bri-ball (e dx dy)
  (let ((ball (make-instance 'enemy :img 0 :obj-type :briball
			     :moto-w 32 :moto-h 32 :dir (dir e)
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
  (incf (atk-c e)) ;;攻撃カウンター
  (update-ido-anime-img e)
  (when (and (>= (atk-c e) 100) ;;攻撃
	     (set-can-atk-dir e 600 600))
    (setf (atk-c e) 0)
    (add-bri-ball-dir e))
  (if (> (dir-c e) 40)
      (progn (set-rand-dir e)
	     (setf (dir-c e) 0))
      (update-enemy-pos e)))

;;火追加 
(defun add-fire (e fire-n)
  (let ((fire (make-instance 'enemy :img 0 :obj-type :fire
			     :str (str e)
			     :moto-w *fire-w* :moto-h *fire-h*
			     :w *fire-w* :h *fire-h* :w/2 *fire-w/2* :h/2 *fire-h/2*)))
    (case (dir e)
      (:up (setf (x fire) (x e)
		 (y fire) (- (y e) (* *fire-h* fire-n))))
      (:down (setf (x fire) (x e)
		   (y fire) (+ (y e) (h e) (- (* *fire-h* fire-n) *fire-h*))))
      (:right (setf (x fire) (+ (x e) (w e) (- (* *fire-w* fire-n) *fire-w*))
		    (y fire) (y e)))
      (:left (setf (x fire) (- (x e) (* *fire-w* fire-n))
		   (y fire) (y e))))
      
    (if (null (block-hit-p fire)) ;;ブロックにぶつかるなら追加しない
	(push fire (donjon-enemies *map*))
	(setf (atk-now e) nil ;;初期化
	      (atk-c e) 0))))

;;火追加ボス 
(defun boss-add-fire (e fire-n)
  (let ((fire (make-instance 'enemy :img 0 :obj-type :fire
			     :str (str e)
			     :moto-w *fire-w* :moto-h *fire-h*
			     :w *fire-w* :h *fire-h* :w/2 *fire-w/2* :h/2 *fire-h/2*))
	(fire2 (make-instance 'enemy :img 0 :obj-type :fire
			      :str (str e)
			      :moto-w *fire-w* :moto-h *fire-h*
			      :w *fire-w* :h *fire-h* :w/2 *fire-w/2* :h/2 *fire-h/2*)))
    (case (dir e)
      (:up (setf (x fire) (x e)
		 (y fire) (- (y e) (* *fire-h* fire-n))
		 (x fire2) (+ (x e) *fire-w*)
		 (y fire2) (- (y e) (* *fire-h* fire-n))))
      (:down (setf (x fire) (x e)
		   (x fire2) (+ (x e) *fire-w*)
		   (y fire) (+ (y e) (h e) (- (* *fire-h* fire-n) *fire-h*))
		   (y fire2) (+ (y e) (h e) (- (* *fire-h* fire-n) *fire-h*))))
      (:right (setf (x fire) (+ (x e) (w e) (- (* *fire-w* fire-n) *fire-w*))
		    (x fire2) (+ (x e) (w e) (- (* *fire-w* fire-n) *fire-w*))
		    (y fire) (y e)
		    (y fire2) (+ (y e) *fire-h*)))
      (:left (setf (x fire) (- (x e) (* *fire-w* fire-n))
		   (x fire2) (- (x e) (* *fire-w* fire-n))
		   (y fire) (y e)
		   (y fire2) (+ (y e) *fire-h*))))
      
    (if (null (block-hit-p fire)) ;;ブロックにぶつかるなら追加しない
	(progn (push fire (donjon-enemies *map*))
	       (push fire2 (donjon-enemies *map*)))
	(setf (atk-now e) nil ;;初期化
	      (atk-c e) 0))))

;;火が追加できるか
(defun check-add-fire (e max-fire fire-time)
  (incf (atk-c e)) ;;火を追加する間隔
  (when (zerop (mod (atk-c e) fire-time))
    (let ((fire-n (floor (atk-c e) fire-time)))
      (case (obj-type e)
	(:dragon (add-fire e fire-n))
	(:boss (boss-add-fire e fire-n)))
      (when (= fire-n max-fire)
	(setf (atk-now e) nil
	      (atk-c e) 0)))))




;;ドラゴンの行動
(defun update-dragon (e)
  (cond
    ((atk-now e)
     (check-add-fire e 3 30))
    (t
     (incf (dir-c e)) ;;移動カウンター更新
     (when (and (= 1 (random 50)) ;;攻撃
		(set-can-atk-dir e (* (w e) 3) (* (h e) 3)))
       (setf (atk-now e) t))
        ;;(set-enemy-atk e))
     (update-ido-anime-img e)
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

;;ボスのとげ攻撃
(defun boss-toge-atk (e)
  (let ((toge (make-instance 'enemy :img 0 :obj-type :toge :hp 1 :maxhp 1
			     :str (str e) :x (x e) :y (+ (y e) (floor (h e) 2))
			     :moto-w *fire-w* :moto-h *fire-h* :vx (rand+- 3) :vy (rand+- 3)
			     :w *fire-w* :h *fire-h* :w/2 *fire-w/2* :h/2 *fire-h/2*)))
    (push toge (donjon-enemies *map*))
    (setf (atk-now e) nil
	  (atk-c e) 0)))

;;ボスの行動
(defun update-boss (e)
  (cond
    ((eq (atk-now e) :fire)
     (check-add-fire e 5 20))
    ((eq (atk-now e) :toge)
     (boss-toge-atk e))
    (t
     (incf (dir-c e)) ;;移動カウンター更新
     (incf (atk-c e))
     (when (zerop (mod (atk-c e) (atk-spd e)))
       (let ((ran (random 2)))
	 (cond
	   ((and (= 0 ran) ;;攻撃
		 (set-can-atk-dir e (* (w e) 30) (* (h e) 30)))
	    (setf (atk-now e) :fire))
	   ((= ran 1)
	    (setf (atk-now e) :toge)))
	 (setf (atk-c e) 0)))
     (update-ido-anime-img e)
     (if (> (dir-c e) 40)
	 (progn (set-rand-dir e)
		(setf (dir-c e) 0))
	 (update-enemy-pos e)))))

;;ボスのとげ攻撃の更新
(defun update-toge (e)
  (incf (x e) (vx e))
  (incf (y e) (vy e))
  (incf (atk-c e))
  (if (>= (atk-c e) 500)
      (setf (donjon-enemies *map*)
	    (remove e (donjon-enemies *map*) :test #'equal))
      (when (block-hit-p e)
	(let ((vx0 (vx e))
	      (vy0 (vy e)))
	  (cond
	    ((>= *blo-w46* (x e))
	     (setf (vx e) (- (vx e))))
	    ((>= (+ (x e) (w e)) (- *map-w* *blo-w46*))
	     (setf (vx e) (- (vx e))))
	    ((<= (y e) *blo-h46*)
	     (setf (vy e) (- (vy e))))
	    ((>= (+ (y e) (h e)) (- *map-h* *blo-h46*))
	     (setf (vy e) (- (vy e)))))
	  (decf (x e) vx0)
	  (decf (y e) vy0)))))
	      

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
	    (:toge    (update-toge e))
	    (:boss    (update-boss e))
	    (:orc-atk (update-orc-atk-effect e))))))




;;transparent-blt
(defun trans-blt (x y w-src h-src w-dest h-dest)
  (transparent-blt *hmemdc* x y *hogememdc* 0 0 :width-source w-src
		   :height-source h-src
		   :width-dest w-dest :height-dest h-dest
		   :transparent-color (encode-rgb 0 255 0)))

(defun new-trans-blt (x y x-src y-src w-src h-src w-dest h-dest)
  (transparent-blt *hmemdc* x y *hogememdc* x-src y-src :width-source w-src
		   :height-source h-src
		   :width-dest w-dest :height-dest h-dest
		   :transparent-color (encode-rgb 0 255 0)))




;;アニメ表示
(defun render-enemy (e anime-num)
  (when (null (dead e)) ;;死んでなかったら表示
    (select-object *hogememdc* *anime-monsters-img*)
    (new-trans-blt (x e) (y e) (* (moto-w e) (img e)) (* *obj-h* anime-num)
		   (moto-w e) (moto-h e) (w e) (h e))))

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
	  (:orc-atk (render-enemy e +orc-atk+))
	  (:toge (render-enemy e +boss-atk1+))
	  (:boss (render-enemy e +boss-anime+)))))




;;現在の方向
(defun p-dir-num ()
  (case (dir *p*)
    (:up +up+)
    (:down +down+)
    (:left +left+)
    (:right +right+)))

;;攻撃時の描画
(defun render-p-atk (atk-img)
  (with-slots (buki) *p*
    (let ((dir (p-dir-num)))
      (cond
	((eq dir +down+)
	 (select-object *hogememdc* *p-atk-img*)
	 (new-trans-blt (x *p*) (y *p*) (* *p-w* (img *p*)) (* *p-h* dir)
			(moto-w *p*) (moto-h *p*) (w *p*) (h *p*))
	 (select-object *hogememdc* atk-img)
	 (new-trans-blt (x buki) (y buki) (* *p-w* (img *p*)) (* *p-h* dir)
			(moto-w buki) (moto-h buki) (w buki) (h buki)))
	(t
	 (select-object *hogememdc* atk-img)
	 (new-trans-blt (x buki) (y buki) (* *p-w* (img *p*)) (* *p-h* dir)
			(moto-w buki) (moto-h buki) (w buki) (h buki))
	 (select-object *hogememdc* *p-atk-img*)
	 (new-trans-blt (x *p*) (y *p*) (* *p-w* (img *p*)) (* *p-h* dir)
			(moto-w *p*) (moto-h *p*) (w *p*) (h *p*)))))))
 
;;プレイヤー表示
(defun render-player ()
  (cond
    ((atk-now *p*)
     (render-p-atk *buki-img*))
    ((hammer-now *p*)
     (render-p-atk *hammer-img*))
    (t
     (select-object *hogememdc* *p-img*)
     (new-trans-blt (x *p*) (y *p*) (* *p-w* (img *p*)) (* *p-h* (p-dir-num))
		    (moto-w *p*) (moto-h *p*) (w *p*) (h *p*)))))

;;*objs-img*の描画
(defun render-objs-img (obj)
  (select-object *hogememdc* *objs-img*)
  (new-trans-blt (x obj) (y obj) (* (moto-w obj) (img obj)) 0
		 (moto-w obj) (moto-h obj) (w obj) (h obj)))



;;ブロック描画
(defun render-block ()
  (loop for obj in (donjon-blocks *map*)
     do (render-objs-img obj)))

;;床描画
(defun render-yuka ()
  (loop for obj in (donjon-yuka *map*)
     do (render-objs-img obj)))

;;鍵とか描画
(defun render-item ()
  (loop for obj in (donjon-objects *map*)
     do (render-objs-img obj)))

;;プレイヤーのステータス表示
(defun render-p-status ()
  (let* ((num 10)
	(time-now (get-internal-real-time))
	(time1 (- time-now *start-time*)))
    (multiple-value-bind (h m s ms) (get-hms time1)
      
    (macrolet ((hoge (n)
		 `(incf ,n 25)))
      (select-object *hmemdc* *font30*)
      (set-text-color *hmemdc* (encode-rgb 255 255 255))
      (set-bk-mode *hmemdc* :transparent)
      (text-out *hmemdc* (format nil "~a" (name *p*)) (+ *map-w* 10) num)
      (text-out *hmemdc* (format nil "Lv:~2d" (level *p*)) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "HP:~2d/~2d" (hp *p*) (maxhp *p*)) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "攻:~2d" (str *p*)) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "防:~2d" (def *p*)) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "exp") (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "~3d/~3d" (expe *p*) (lvup-exp *p*)) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "ハンマー") (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "残り:~d回" (hammer *p*)) (+ *map-w* 10) (hoge num))
      (hoge num)
      (text-out *hmemdc* (format nil "武器") (+ *map-w* 10) (hoge num))
      (with-slots (buki) *p*
	(text-out *hmemdc* (format nil "~a" (name buki)) (+ *map-w* 10) (hoge num))
	(text-out *hmemdc* (format nil "攻:~d" (atk buki)) (+ *map-w* 10) (hoge num)))
      (hoge num)
      (text-out *hmemdc* (format nil "持ち物") (+ *map-w* 10) (hoge num))
      (loop for item in (item *p*)
	 for i from 0 do
	   (select-object *hogememdc* *objs-img*)
	   (new-trans-blt (+ *map-w* 10) (hoge num) (* (moto-w item) (img item)) 0
			  (moto-w item) (moto-h item) (w item) (h item)))
      (when (key? *p*)
	(select-object *hogememdc* *objs-img*)
	(new-trans-blt (+ *map-w* 10) (hoge num) (* *obj-w* +key+) 0
		       32 32 32 32))
      ;;(text-out *hmemdc* (format nil "w:~2d" *change-screen-w*) (+ *map-w* 10) 250)
      ;;(text-out *hmemdc* (format nil "h:~2d" *change-screen-h*) (+ *map-w* 10) 290)
      (select-object *hmemdc* *font30*)
      (set-text-color *hmemdc* (encode-rgb 255 255 255))
      (text-out *hmemdc* (format nil "モゲアーガの塔 ~2,'0d階" (stage *p*)) 10 (+ *map-h* 10))
      (text-out *hmemdc* (format nil "~2,'0d:~2,'0d:~2,'0d:~2,'0d" h m s ms) 200 (+ *map-h* 10))

     ))))


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


;;test
(defun render-test ()
  (select-object *hogememdc*  *anime-monsters-img*)
  (transparent-blt *hmemdc* 0 0 *hogememdc* 0 32 :width-source 32
		   :height-source 32
		   :width-dest 32 :height-dest 32
		   :transparent-color (encode-rgb 0 255 0)))

;;タイトル画面
(defun render-title-gamen (mes1 mes2)
  (render-background)
  (select-object *hmemdc* *font140*)
  (set-bk-mode *hmemdc* :transparent)
  (set-text-color *hmemdc* (encode-rgb 0 155 255))
  (text-out *hmemdc* (format nil "~a" mes1) 130 100)
  (when (eq (state *p*) :dead)
    (text-out *hmemdc* (format nil "~d階で力尽きた" (stage *p*)) 130 250))
  (select-object *hogememdc* *objs-img*)
  (if (= (cursor *p*) 0)
      (new-trans-blt 380 400 (* 32 +cursor+) 0 32 32 32 32)
      (new-trans-blt 380 450 (* 32 +cursor+) 0 32 32 32 32))
  (select-object *hmemdc* *font40*)
  (set-text-color *hmemdc* (encode-rgb 255 255 255))
  (text-out *hmemdc* (format nil "~a" mes2) 430 400)
  (text-out *hmemdc* (format nil "おわる") 430 450))



;;エンディング画面
(defun render-ending-gamen ()
  (let ((time1 (- (endtime *p*) *start-time*)))
    (multiple-value-bind (h m s ms) (get-hms time1)
      (render-background)
      (select-object *hmemdc* *font70*)
      (set-bk-mode *hmemdc* :transparent)
      (set-text-color *hmemdc* (encode-rgb 0 155 255))
      (text-out *hmemdc* (format nil "~a は" (name *p*)) 10 10)
      (text-out *hmemdc* (format nil "モゲアーガの塔を制覇した！") 170 100)
      (select-object *hmemdc* *font70*)
      (set-text-color *hmemdc* (encode-rgb 255 255 255))
      (text-out *hmemdc* (format nil "クリアタイム") 360 200)
      (text-out *hmemdc* (format nil  "~2,'0d 時間 ~2,'0d 分 ~2,'0d 秒 ~2,'0d" h m s ms) 210 280)
      (select-object *hogememdc* *objs-img*)
      (if (= (cursor *p*) 0)
	  (new-trans-blt 380 400 (* 32 +cursor+) 0 32 32 32 32)
	  (new-trans-blt 380 450 (* 32 +cursor+) 0 32 32 32 32))
      (select-object *hmemdc* *font40*)
      (set-text-color *hmemdc* (encode-rgb 255 255 255))
      (text-out *hmemdc* (format nil "もう一度やる") 430 400)
      (text-out *hmemdc* (format nil "おわる") 430 450))))

;;名前入力画面
(defun set-name-gamen ()
  (render-background)
  (select-object *hmemdc* *font40*)
  (set-bk-mode *hmemdc* :transparent)
  (set-text-color *hmemdc* (encode-rgb 0 155 255))
  (text-out *hmemdc* "※名前を入力してください （6文字まで）" 100 10)
  (text-out *hmemdc* "決 or Enterキーでゲームスタート！" 500 400)
  (let ((x 0) (y 0) (xx 50))
    (loop :for i :across *aiueo*
	 :for cursor from 0
       :with m = 0
       :do
	 (when (equal i #\が)
	   (setf xx 300
		 y 0
		 m 0))
	 (cond
	   ((zerop (mod m 5))
	    (setf x xx
		  y (+ y 50)
		  m 0))
	   
	   (t (setf x (+ x 40))))
	 (incf m)
	 (when (= cursor (cursor *p*))
	   (select-object *hmemdc* (get-stock-object :white-brush))
	   (rectangle *hmemdc* x y (+ x 40) (+ y 40)))
	 (select-object *hmemdc* *font40*)
	 (text-out *hmemdc* (format nil "~a" i) x y))
    (text-out *hmemdc* (format nil "名前：~a" *name*) 400 500)))

;;ゲーム全体描画
(defun render-game (hdc)
  (cond
    ((eq (state *p*) :title) ;;タイトル画面
     (render-title-gamen "モゲアーガの塔" "はじめる"))
    ((eq (state *p*) :name)
     (set-name-gamen))
    ((eq (state *p*) :playing) ;;ゲーム
     (render-map)
     (render-player)
     (render-p-status)
     (render-enemies)
     ;;(render-test)
     (render-all-damage))
    ((eq (state *p*) :dead)
     (render-title-gamen "ゲームオーバー" "もう一度やる"))
    ((eq (state *p*) :ending) ;;エンディング画面
     (render-ending-gamen)
     ))
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
    (:hammer +hammer+)
    (:sword +sword+)))

;;通常のドロップアイテム
(defun normal-drop-item ()
  (let* ((n (random 100)))
    (cond
      ((>= 5 n 0)   :boots)
      ((>= 25 n 10)  :potion)
      ((>= 45 n 26) :hammer)
      ((>= 51  n 46) :sword)
      (t nil))))

;;靴持ってる時のドロップアイテム
(defun no-boots-drop-item ()
  (let* ((n (random 100)))
    (cond
      ((>= 25 n 10)  :potion)
      ((>= 45 n 26) :hammer)
      ((>= 51  n 46) :sword)
      (t nil))))

;;アイテムの靴を落とす
(defun enemy-drop-item (e)
  (let* ((item (if (boots? *p*)
		   (no-boots-drop-item)
		   (normal-drop-item))))
    (when item
      (let ((drop-item (make-instance 'obj :img (item-img item)
				      :x (x e) :y (y e) :w 32 :h 32 :w/2 16 :h/2 16
				      :moto-w 32 :moto-h 32 :obj-type item)))
	(push drop-item (donjon-objects *map*))))))
	;;(setf (donjon-drop-item *map*) (cdr (donjon-drop-item *map*)))))))
;; (when (>= (random 5) 3)
	;;   (let ((drop-item (make-instance 'obj :img +potion+ 
	;; 				:x (x e) :y (y e) :w 32 :h 32 :w/2 16 :h/2 16
	;; 				:moto-w 32 :moto-h 32 :obj-type :potion)))
	;;     (push drop-item (donjon-objects *map*)))))))

;;ボスを倒したら
(defun go-ending ()
  (setf (state *p*) :ending
	(endtime *p*) (get-internal-real-time)))

;;死んだ敵の情報を消す
(defun delete-enemies ()
  (loop for e in (donjon-enemies *map*)
     do (when (and (null (dmg e))
		   (dead e))
	  (when (eq (obj-type e) :boss)
	    (go-ending))
	  (enemy-drop-item e)
	  (setf (donjon-enemies *map*)
		(remove e (donjon-enemies *map*) :test #'equal)))))


(defun start-name ()
  (setf (state *p*) :name
	(atk-c *p*) 0
	(cursor *p*) 0))


;;ゲームを開始する
(defun start-game ()
  (init-game)
  (setf (state *p*) :playing
	*start-time* (get-internal-real-time)))

;;タイトル画面での操作
(defun update-title-and-ending-gamen (hwnd)
  (with-slots (up down z enter) *keystate*
    (incf (atk-c *p*))
    (when (zerop (mod (atk-c *p*) 9))
      (cond
	(up
	 (cond
	   ((= (cursor *p*) 1)
	    (setf (cursor *p*) 0))
	   ((= (cursor *p*) 0)
	    (setf (cursor *p*) 1))))
	(down
	 (cond
	   ((= (cursor *p*) 0)
	    (setf (cursor *p*) 1))
	   ((= (cursor *p*) 1)
	    (setf (cursor *p*) 0))))
	((or z enter)
	 (if (= (cursor *p*) 0)
	     (if (or (eq :dead (state *p*)) (eq :ending (state *p*)))
		 (start-game)
		 (start-name))
	     (send-message hwnd (const +wm-close+) nil nil)))))))

;;ゲームオーバー判定
(defun game-over? ()
  (when (dead *p*)
    (setf (state *p*) :dead)))

;;名前決め
(defun update-name-cursor ()
  (incf (atk-c *p*))
  (when (zerop (mod (atk-c *p*) 6))
    (with-slots (up down right left z enter) *keystate*
      (cond
	(enter
	 (start-game))
	(z 
	 (cond
	   ((= (cursor *p*) 77)
	    (start-game))
	   ((= (cursor *p*) 76)
	    (when (> (length (name *p*)) 0)
	      (setf *name*
		    (subseq *name* 0 (1- (length *name*))))))
	   ((> 6 (length *name*))
	    (setf *name*
		  (concatenate 'string *name* (format nil "~a" (aref *aiueo* (cursor *p*))))))))
	(up
	 (cond
	   ((> 5 (cursor *p*))
	    (setf (cursor *p*) (+ (cursor *p*) 45)))
	   ((>= 52 (cursor *p*) 50)
	    (setf (cursor *p*) (+ (cursor *p*) 25)))
	   ((>= 54 (cursor *p*) 53)
	    (setf (cursor *p*) (+ (cursor *p*) 20)))
	   (t
	    (decf (cursor *p*) 5))))
	(down
	 (cond
	   ((>= 49 (cursor *p*) 45)
	    (setf (cursor *p*) (- (cursor *p*) 45)))
	   ((>= 77 (cursor *p*) 75)
	    (setf (cursor *p*) (- (cursor *p*) 25)))
	   ((>= 74 (cursor *p*) 73)
	    (setf (cursor *p*) (- (cursor *p*) 20)))
	   (t
	    (incf (cursor *p*) 5))))
	(right
	 (cond
	   ((and  (> 30 (cursor *p*))
		  (= (mod (cursor *p*) 5) 4))
	    (setf (cursor *p*) (+ (cursor *p*) 46)))
	   ((and  (> 50 (cursor *p*) 30)
		  (= (mod (cursor *p*) 5) 4))
	    (setf (cursor *p*) (- (cursor *p*) 4)))
	   ((and  (>  75 (cursor *p*) 50)
		  (= (mod (cursor *p*) 5) 4))
	    (setf (cursor *p*) (- (cursor *p*) 54)))
	   ((= 77 (cursor *p*))
	    (setf (cursor *p*) 25))
	   (t
	    (incf (cursor *p*)))))
	(left
	 (cond
	   ((and  (> 25 (cursor *p*))
		  (= (mod (cursor *p*) 5) 0))
	    (setf (cursor *p*) (+ (cursor *p*) 54)))
	   ((and  (> 50 (cursor *p*) 29)
		  (= (mod (cursor *p*) 5) 0))
	    (setf (cursor *p*) (+ (cursor *p*) 4)))
	   ((and  (>=  74 (cursor *p*) 50)
		  (= (mod (cursor *p*) 5) 0))
	    (setf (cursor *p*) (- (cursor *p*) 46)))
	   ((= 75 (cursor *p*))
	    (setf (cursor *p*) 29))
	   ((= 25 (cursor *p*))
	    (setf (cursor *p*) 77))
	   (t
	    (decf (cursor *p*)))))))))

;;ゲームループ
(defun main-game-loop (hwnd)
  (cond
    ((eq (state *p*) :title)
     (update-title-and-ending-gamen hwnd))
    ((eq (state *p*) :name)
     (update-name-cursor))
    ((eq (state *p*) :playing)
     (update-player *p*)
     (update-enemies)
     (update-damage-fonts)
     (delete-enemies)
     (game-over?))
    ((eq (state *p*) :dead)
     (update-title-and-ending-gamen hwnd))
    ((eq (state *p*) :ending)
     (update-title-and-ending-gamen hwnd)))
  (invalidate-rect hwnd nil nil))

;;ウィンドウサイズ変更時に画像拡大縮小する
(defun change-screen-size (lp)
  (let* ((change-w (loword lp))
	 (change-h (hiword lp)))
    (setf *change-screen-w* change-w
	  *change-screen-h* change-h)))


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
