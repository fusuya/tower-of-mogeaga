;;TODO 敵の攻撃どうする　階段の画像
;;ダメージフォントの色どうする　拝啓どうする　
;; 敵の種類ふやす
;;オークの攻撃

;;ブラシ生成
(defun set-brush ()
  (setf *brush* (make-array 7 :initial-contents
                              (list
                                (create-solid-brush (encode-rgb 128 0 255))
                                (create-solid-brush (encode-rgb 255 0 0))
                                (create-solid-brush (encode-rgb 0 255 0))
                                (create-solid-brush (encode-rgb 0 0 255))
                                (create-solid-brush (encode-rgb 255 255 0))
                                (create-solid-brush (encode-rgb 0 255 255))
                                (create-solid-brush (encode-rgb 255 0 255))))))

(defun set-font ()
  (setf *font140* (create-font "MSゴシック" :height 140)
        *font40* (create-font "MSゴシック" :height 40)
        *font20* (create-font "MSゴシック" :height 25 :width 12 :weight (const +fw-bold+))))

(defun delete-font ()
  (delete-object *font140*)
  (delete-object *font40*)
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
  (delete-object-array *slime-anime*)
  (delete-object-array *orc-anime*)
  (delete-object-array *brigand-anime*)
  (delete-object-array *buki-imgs*))

(defun load-images ()
  (setf *images* (make-imgs-array "./img/img-*.*")
	*p-imgs* (make-imgs-array "./img/p*.*")
	*hammer-imgs* (make-imgs-array "./img/ham*.*")
	*slime-anime* (make-imgs-array "./img/slime*.*")
	*orc-anime* (make-imgs-array "./img/orc*.*")
	*brigand-anime* (make-imgs-array "./img/brigand*.*")
	*buki-imgs* (make-imgs-array "./img/buki*.*")))

;;ゲーム初期化
(defun init-game ()
  (setf *battle?* nil
        *ido?* t
        *monster-num* 6
        *monster-level* 1
        *boss?* 0
        *end* 0
        *start-time* (get-internal-real-time)
        *ha2ne2* nil
        *copy-buki* (copy-tree *buki-d*)
        *p* (make-instance 'player :w *r-tate-w* :h *r-tate-h*
			   :moto-w *tate-w* :moto-h *tate-h* :atk-now nil :ido-spd 2
			   :w/2 (floor *r-tate-w* 2) :h/2 (floor *r-tate-h* 2)
			   :name "もげぞう" :img +down+ :buki (make-instance 'buki :name "こん棒" :atk 5))
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

;;プレイヤーの生死判定
(defun player-dead (p)
  (<= (player-hp p) 0))

;;プレイヤーたちが死んでないかチェック
(defun players-dead (pt)
  (every #'player-dead (party-players pt)))


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
	    (setf hoge t)
	    (return)))
    hoge))

;;プレイヤーと鍵とドアの当たり判定
(defun player-hit-key-door ()
  (loop for obj in (donjon-objects *map*)
     do
       (when (obj-hit-p *p* obj)
	 (case (obj-type obj)
	   (:key (setf (key? *p*) t)
		 (setf (img obj) (aref *images* +yuka+)
		       (obj-type obj) :yuka))
	   (:door
	    (when (key? *p*)
	      (setf (key? *p*) nil)
	      (setf *map* (make-donjon :tate *tate-block-num* :yoko *yoko-block-num*))
	      (maze *map* *p*)))))))

;;ダメージ計算
(defun damage-keisan (p e)
  (with-slots (buki) p
    (if buki
	(let* ((a1 (+ (str p) (atk buki)))
	       (a2 (random (max 1 (- a1 (def e))))))
	  (max 0 (- (+ a1 a2) (def e))))
	(let* ((a1 (str p))
	       (a2 (random (max 1 (- a1 (def e))))))
	  (max 0 (- (+ a1 a2) (def e)))))))


;;ダメージ計算して　表示する位置とか設定
(defun set-damage (p e)
  (with-slots (x y) e
    (let* ((dmg-x (+ x 10))
	   (dmg-y (+ y 20))
	   (dmg-num (damage-keisan p e))
	   (x-dir (if (eq (dir p) :left) :left :right))
	   (dmg (make-instance 'dmg-obj :x dmg-x :y dmg-y :dmg-num  dmg-num
			       :y-dir :up :x-dir x-dir
			       :maxy dmg-y :miny (- dmg-y 15))))
      (decf (hp e) dmg-num) ;;hpを減らす
      (when (>= 0 (hp e)) ;; hpが0以下になったら死亡
	(setf (dead e) t))
      (setf (dmg e) dmg) ;;ダメージを表示するためのもの
      )))


;;敵と武器の当たり判定
(defun buki-hit-enemy (p)
  (with-slots (buki) p
    (loop for e in (donjon-enemies *map*)
       do (when (and (obj-hit-p buki e)
		     (null (dead e)))
	    (set-damage p e))))) ;;ダメージ処理
	    ;; (when (>= 0 (hp e))
	    ;;   (setf (donjon-enemies *map*)
	    ;; 	    (remove e (donjon-enemies *map*) :test #'equal))))
	 ;; (text-out *hmemdc* (format nil "x:~d y:~d w:~d h:~d" (x buki) (y buki) (w buki) (h buki))
	 ;; 	   100 600))))


;;通常時画像設定
(defun set-normal-img (p)
  (case (dir p)
    (:down  (setf (img p) +down+
		  (w p) *r-tate-w* (w/2 p) *r-tate-w/2*
		  (h p) *r-tate-h*
		  (moto-w p) *tate-w* (moto-h p) *tate-h*))
    (:up    (setf (img p) +up+
		  (w p) *r-tate-w* (w/2 p) *r-tate-w/2*
		  (h p) *r-tate-h*
		  (moto-w p) *tate-w* (moto-h p) *tate-h*))
    (:left  (setf (img p) +left+
		  (w p) *r-yoko-w* (w/2 p) *r-yoko-w/2*
		  (h p) *r-yoko-h*
		  (moto-w p) *yoko-w* (moto-h p) *yoko-h*))
    (:right (setf (img p) +right+
		  (w p) *r-yoko-w* (w/2 p) *r-yoko-w/2*
		  (h p) *r-yoko-h*
		  (moto-w p) *yoko-w* (moto-h p) *yoko-h*))))


;;武器の画像を設定
(defun set-buki-img (p)
  (with-slots (buki x y dir) p
    (case dir
      (:down  (setf (moto-w buki) 32 (moto-h buki) 24
		    (w buki) 40 (h buki) 32
		    (w/2 buki) 20 (h/2 buki) 16
		    (x buki) x
		    (y buki) (+ y 22)))
      (:up    (setf (moto-w buki) 32 (moto-h buki) 28
		    (w buki) 40 (h buki) 36
		    (w/2 buki) 20 (h/2 buki) 18
		    (x buki) x
		    (y buki) (- y 16)))
      (:left  (setf (moto-w buki) 18 (moto-h buki) 32
		    (w buki) 26 (h buki) 40
		    (w/2 buki) 13 (h/2 buki) 20
		    (x buki) (- x 14)
		    (y buki) y))
      (:right (setf (moto-w buki) 18 (moto-h buki) 32
		    (w buki) 26 (h buki) 40
		    (w/2 buki) 13 (h/2 buki) 20
		    (x buki) (+ x 21)
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
    (incf (atk-img p)))
  (set-atk-img p)
  (set-buki-img p)
  (when (> (atk-img p) 2)
    (set-normal-img p)
    (setf (atk-now p) nil
	  (hammer-now p) nil
	  (atk-c p) 0
	  (atk-img p) 0)))



;;壁壊す
(defun hammer-hit-kabe (p)
  (with-slots (buki) p
    (loop for kabe in (donjon-blocks *map*)
       do (when (and (obj-hit-p buki kabe)
		     (eq (obj-type kabe) :soft-block))
	    (setf (obj-type kabe) :yuka
		  (img kabe) (aref *images* +yuka+))
	    ))))

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
	 (setf (img p) +left+
	       (w p) *r-yoko-w*
	       (moto-w p) *yoko-w*
	       (dir p) :left))
       (decf (x p) (ido-spd p))
       (when (block-hit-p p)
	 (incf (x p) (ido-spd p))))
      (right
       (when (null shift)
	 (setf (img p) +right+
	       (w p) *r-yoko-w*
	       (moto-w p) *yoko-w*
	       (dir p) :right))
       (incf (x p) (ido-spd p))
       (when (block-hit-p p)
	 (decf (x p) (ido-spd p))))
      (up
       (when (null shift)
	 (setf (img p) +up+
	       (w p) *r-tate-w*
	       (moto-w p) *tate-w*
	       (dir p) :up))
       (decf (y p) (ido-spd p))
       (when (block-hit-p p)
	 (incf (y p) (ido-spd p))))
      (down
       (when (null shift)
	 (setf (img p) +down+
	       (w p) *r-tate-w*
	       (moto-w p) *tate-w*
	       (dir p) :down))
       (incf (y p) (ido-spd p))
       (when (block-hit-p p)
	 (decf (y p) (ido-spd p)))))))


;;プレイヤー被ダメージ処理
(defun hit-enemies-player (p)
  (loop for e in (donjon-enemies *map*)
     do (when (and (obj-hit-p p e)
		   (null (dead e)))
	  (set-damage e p) 
	  (setf (dmg-c p) 50))))

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
     (player-hit-key-door))))
    
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
    (:up    (decf (y e))
	    (when (block-hit-p e)
	      (incf (y e))
	      (set-rand-dir e)))
    (:down  (incf (y e))
	    (when (block-hit-p e)
	      (decf (y e))
	      (set-rand-dir e)))
    (:right (incf (x e))
	    (when (block-hit-p e)
	      (decf (x e))
	      (set-rand-dir e)))
    (:left  (decf (x e))
	    (when (block-hit-p e)
	      (incf (x e))
	      (set-rand-dir e)))))


;;敵の攻撃の画像を設定
(defun set-enemy-atk-img (e)
  (with-slots (buki x y dir) e
    (case dir
      (:down  (setf (moto-w buki) 32 (moto-h buki) 32
		    (w buki) 40 (h buki) 40
		    (w/2 buki) 20 (h/2 buki) 20
		    (x buki) x
		    (y buki) (+ y 40)))
      (:up    (setf (moto-w buki) 32 (moto-h buki) 32
		    (w buki) 40 (h buki) 40
		    (w/2 buki) 20 (h/2 buki) 20
		    (x buki) x
		    (y buki) (- y 40)))
      (:left  (setf (moto-w buki) 32 (moto-h buki) 32
		    (w buki) 40 (h buki) 40
		    (w/2 buki) 20 (h/2 buki) 20
		    (x buki) (- x 40)
		    (y buki) y))
      (:right (setf (moto-w buki) 32 (moto-h buki) 32
		    (w buki) 40 (h buki) 40
		    (w/2 buki) 20 (h/2 buki) 20
		    (x buki) (+ x 40)
		    (y buki) y)))))

;;敵とプレイヤーの距離判定
(defun enemy-can-atk? (e)
  (let* ((diffx (- (x e) (x *p*)))
	 (diffy (- (y e) (y *p*)))
	 (absx (abs diffx))
	 (absy (abs diffy)))
    (if (and (>= 32 absx)
	     (>= 32 absy))
	(cond
	  ((and (>= diffx 0) (>= diffy 0))
	   (if (>= absx absy)
	       :right :up))
	  ((and (>= diffx 0) (> 0 diffy))
	   (if (>= absx absy)
	       :right :down))
	  ((and (> 0 diffx) (>= diffy 0))
	   (if (>= absx absy)
	       :left :up))
	  ((and (> 0 diffx) (> 0 diffy))
	   (if (>= absx absy)
	       :left :down))
	  (t nil))
	nil)))

;;敵の攻撃設定
(defun set-enemy-atk (e)
  (let ((atk? (enemy-can-atk? e)))
    (if atk?
	(progn (setf (dir e) atk?
		     (atk-now e) t)
	       (set-enemy-atk-img e))
        nil)))

(defun update-enemy-atk (e)
  (incf (atk-c e))
  (when (zerop (mod (atk-c e) (atk-spd e)))
    (incf (atk-img e)))
  ;;(set-atk-img p)
  ;;(set-buki-img p)
  (when (> (atk-img e) 2)
    ;;(set-normal-img p)
    (setf (atk-now e) nil
	  (atk-c e) 0)))
	  ;;(atk-img e) 0)))
      

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

;;スライムの移動
(defun update-slime (e)
  (incf (dir-c e)) ;;移動カウンター更新
  (incf (walk-c e))
  (update-enemy-anime-img e)
  (if (> (dir-c e) 40)
      (progn (set-rand-dir e)
	     (setf (dir-c e) 0))
      (update-slime-pos e)))

;;オークの移動
(defun update-orc (e)
  (cond
    ((atk-now e)
     (update-enemy-atk e))
    (t
     (incf (dir-c e)) ;;移動カウンター更新
     (incf (walk-c e))
     (when (= 1 (random 2))
       (set-enemy-atk e))
     (update-enemy-anime-img e)
     (if (> (dir-c e) 40)
	 (progn (set-rand-dir e)
		(setf (dir-c e) 0))
	 (update-slime-pos e)))))

;;ブリガンドの移動
(defun update-brigand (e)
  (incf (dir-c e)) ;;移動カウンター更新
  (incf (walk-c e))
  (update-enemy-anime-img e)
  (if (> (dir-c e) 40)
      (progn (set-rand-dir e)
	     (setf (dir-c e) 0))
      (update-slime-pos e)))

;;敵の位置更新
(defun update-enemies ()
  (loop for e in (donjon-enemies *map*)
     do (case (obj-type e)
	  (:slime (update-slime e))
	  (:brigand (update-brigand e))
	  (:orc (update-orc e)))))


;;移動後のマップ更新
(defun update-map (map pt y x)
  (case (aref (donjon-map map) (+ (party-posy pt) y) (+ (party-posx pt) x))
    (30 ;;壁
     (if (and (> (party-hammer pt) 0)
              (> (- (donjon-tate map) 1) (+ (party-posy pt) y) 0)
              (> (- (donjon-yoko map) 1) (+ (party-posx pt) x) 0))
         (setf *kabe-break* t
               *ido?* nil)))
         ;;(kabe-break (donjon-map map) pt y x)))
        ;;(scr-format "「そっちには移動できません！！」~%")))
    (40 ;;壁
     nil)
    (2 ;;くだり階段
     (incf (party-map pt))
     (maze map pt)
     ;;２階降りるごとにハンマーもらえる
     ;; (if (= (mod (party-map p) 2) 0)
     ;; 	 (incf (party-hammer p)))
     ;;５階降りるごとに宝箱の確率変わる
     ;;(if (= (mod (party-map pt) 5) 0)
     ;;    (setf *copy-buki* (omomin-zurashi *copy-buki*)))
         ;;７階降りるごとに敵のレベル上がる
     (if (= (mod (party-map pt) 7) 0)
         (incf *monster-level*)))
    ;;(3 ;;宝箱
     ;;(item-get2 pt)
     ;;(update-player-pos pt x y))
    (5 ;;ボス
     (update-player-pos pt x y)
     (setf *battle?* t
           *boss?* 1))
    ;;(6 ;;イベント
     ;;(update-player-pos pt x y)
     ;;(moge-event pt))
    (7 ;;中ボス
     (update-player-pos pt x y)
     (setf *battle?* t
           *boss?* 2))
    (otherwise
     (update-player-pos pt x y))))


;;transparent-blt
(defun trans-blt (x y w-src h-src w-dest h-dest)
  (transparent-blt *hmemdc* x y *hogememdc* 0 0 :width-source w-src
		   :height-source h-src
		   :width-dest w-dest :height-dest h-dest
		   :transparent-color (encode-rgb 0 255 0)))


(defun render-enemy-atk (e)
  (with-slots (buki) e
    (select-object *hogememdc* (aref *images* +e-atk+))
    (trans-blt (x buki) (y buki) (moto-w buki) (moto-h buki)
	       (w buki) (h buki))))

;;アニメ表示
(defun render-enemy (e anime-arr)
  (when (null (dead e)) ;;死んでなかったら表示
    (when (atk-now e)
      (render-enemy-atk e))
    (select-object *hogememdc* (aref anime-arr (img e)))
    (transparent-blt *hmemdc* (x e) (y e) *hogememdc* 0 0
		     :width-source (moto-w e) :height-source (moto-h e)
		     :width-dest (w e) :height-dest (h e)
		     :transparent-color (encode-rgb 0 255 0))))
;;敵表示
(defun render-enemies ()
  (loop for e in (donjon-enemies *map*)
     do (case (obj-type e)
	  (:slime (render-enemy e *slime-anime*))
	  (:brigand (render-enemy e *brigand-anime*))
	  (:orc   (render-enemy e *orc-anime*)))))



;;スライムアニメ表示
(defun render-slime (e)
  (when (null (dead e)) ;;死んでなかったら表示
    (select-object *hogememdc* (aref *slime-anime* (img e)))
    (transparent-blt *hmemdc* (x e) (y e) *hogememdc* 0 0
		     :width-source (moto-w e) :height-source (moto-h e)
		     :width-dest (w e) :height-dest (h e)
		     :transparent-color (encode-rgb 0 255 0))))

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
  (transparent-blt *hmemdc* (x *p*) (y *p*) *hogememdc* 0 0
		   :width-source (moto-w *p*) :height-source (moto-h *p*)
		   :width-dest (w *p*) :height-dest (h *p*)
		   :transparent-color (encode-rgb 0 255 0)))

;;ブロック描画
(defun render-block ()
  (loop for obj in (donjon-blocks *map*)
     do
       (select-object *hogememdc* (img obj))
       (transparent-blt *hmemdc* (x obj) (y obj) *hogememdc* 0 0 :width-source *obj-w*
			:height-source *obj-h*
			:width-dest (w obj) :height-dest (h obj)
			:transparent-color (encode-rgb 0 255 0))))

;;床描画
(defun render-yuka ()
  (loop for obj in (donjon-yuka *map*)
     do
       (select-object *hogememdc* (img obj))
       (trans-blt (x obj) (y obj) *obj-w* *obj-h* (w obj) (h obj))))

;;鍵とか描画
(defun render-item ()
  (loop for obj in (donjon-objects *map*)
     do
       (select-object *hogememdc* (img obj))
       (trans-blt (x obj) (y obj) *obj-w* *obj-h* (w obj) (h obj))))

;;プレイヤーのステータス表示
(defun render-p-status ()
  (select-object *hmemdc* *font40*)
  (set-text-color *hmemdc* (encode-rgb 255 255 255))
  (set-bk-mode *hmemdc* :transparent)
  (text-out *hmemdc* (format nil "~a" (name *p*)) (+ *map-w* 10) 10)
  (text-out *hmemdc* (format nil "Lv:~2d" (level *p*)) (+ *map-w* 10) 50)
  (text-out *hmemdc* (format nil "HP:~2d" (hp *p*)) (+ *map-w* 10) 90)
  (when (key? *p*)
    (select-object *hogememdc* (aref *images* +key+))
    (trans-blt (+ *map-w* 10) 140 *obj-w* *obj-h* *r-blo-w* *r-blo-h*)))


;;バックグラウンド
(defun render-background ()
  (select-object *hmemdc* (get-stock-object :black-brush))
  (rectangle *hmemdc* 0 0 (rect-right *c-rect*) (rect-bottom *c-rect*)))

;;マップを表示
(defun render-map ()
  (render-background)
  (render-yuka)
  (render-block)
  (render-item))



;;
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

;;ダメージ表示
(defun render-all-damage ()
  (render-damage *p*  (encode-rgb 255 147 122))
  (loop for e in (donjon-enemies *map*)
     do (render-damage e  (encode-rgb 255 255 255))))

;;ゲーム全体描画
(defun render-game (hdc)
  (render-map)
  (render-player)
  (render-p-status)
  (render-enemies)
  (render-all-damage)
  (transparent-blt hdc 0 0 *hmemdc* 0 0
          :width-dest (rect-right *c-rect*) :height-dest (rect-bottom *c-rect*)
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

;;死んだ敵の情報を消す
(defun delete-enemies ()
  (loop for e in (donjon-enemies *map*)
     do (when (and (null (dmg e))
		   (dead e))
	  (setf (donjon-enemies *map*)
		(remove e (donjon-enemies *map*) :test #'equal)))))

;;ゲームループ
(defun main-game-loop (hwnd)
  (update-player *p*)
  (update-enemies)
  (update-damage-fonts)
  (delete-enemies)
  (invalidate-rect hwnd nil nil))

;;win32api
;;".\\images\\*.*" ロードした画像の配列を作る


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
     (set-client-size hwnd)
     (setf *c-rect* (get-client-rect hwnd))
     (setf *screen-center-x* (+ (rect-right *c-rect*)
                              (floor (- (rect-left *c-rect*) (rect-right *c-rect*)) 2)))

     (set-layered-window-attributes hwnd (encode-rgb 0 255 0) 0 (const +lwa-colorkey+))
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
                  :cursor (load-cursor :arrow)
                  :background (create-solid-brush (encode-rgb 0 255 0)))
  (let ((hwnd (create-window "MOGE"
                             :window-name "もげぞうの塔"
                             :ex-styles  (logior-consts +WS-EX-LAYERED+ +ws-ex-composited+) ;;透明
                             :styles (logior-consts +ws-overlappedwindow+ +ws-visible+)
                             :x 400 :y 100 :width 960 :height 720))
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
