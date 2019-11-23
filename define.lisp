
(defmacro my-enum (&rest names)
  `(progn
     ,@(loop for i from 0
             for name in names
        collect `(defconstant ,name ,i))))

;;".\\images\\*.*" ロードした画像の配列を作る
(defun make-imgs-array (img-path)
  (let* ((img-list (mapcar #'namestring (directory img-path)))
         (imgs (make-array (length img-list))))
    (loop for str in img-list
          for i from 0
          do (setf (aref imgs i)
                   (load-image str :type :bitmap
                               :flags '(:load-from-file :create-dib-section))))
    imgs))

(defparameter *p-imgs* nil)
(defparameter *buki-imgs* nil)
(defparameter *hammer-imgs* nil)
(defparameter *slime-anime* nil)
(defparameter *orc-anime* nil)
(defparameter *brigand-anime* nil)
(defconstant +atk-d+ 0)
(defconstant +atk-l+ 3)
(defconstant +atk-r+ 6)
(defconstant +atk-u+ 9)

(defconstant +down+ 12)
(defconstant +left+ 13)
(defconstant +right+ 14)
(defconstant +up+ 15)

;;透過用
(defcfun (%set-layered-window-attributes "SetLayeredWindowAttributes" :convention :stdcall)
         :boolean
  (hwnd :pointer)
  (crkey :int32)
  (balpha :uint8)
  (dwflags :uint32))

(defun set-layered-window-attributes (hwnd crkey balpha dwflags)
  (%set-layered-window-attributes hwnd crkey balpha dwflags))

(defparameter *font140* nil)
(defparameter *font40* nil)
(defparameter *font20* nil)



;;(defparameter *tate* 11) ;;マップサイズ
;;(defparameter *yoko* 11)
(defparameter *monsters* nil)
(defparameter *monster-builders* nil)

(defparameter *map* nil)
(defparameter *ido?* nil)
(defparameter *p* nil)
(defparameter *pt* nil)

(defparameter *battle?* nil)
(defparameter *monster-num* 10)
(defparameter *monster-level* 1) ;;階数によるモンスターのレベル
(defparameter *boss?* 0)
(defparameter *end* 0)
(defparameter *lv-exp* 100)
(defparameter *start-time* 0)
(defparameter *ha2ne2* nil)
(defparameter *copy-buki* (copy-tree *buki-d*))
(defparameter *urawaza* nil)
(defparameter *images* nil)


;;拡大
(defparameter *kakudai* 1.3)

;;基本サイズ 元の画像サイズ
(defparameter *obj-w* 32)
(defparameter *obj-h* 32)


;;元のブロック画像のサイズ
(defparameter *blo-w* 32)
(defparameter *blo-h* 32)
;;表示するブロック画像のサイズ
(defparameter *r-blo-w* 46)
(defparameter *r-blo-h* 46)

;;元の上下向きプレイヤー画像のサイズ
(defparameter *tate-w* 32)
(defparameter *tate-h* 32)
;;表示する上下向きプレイヤー画像サイズ
(defparameter *r-tate-w* 40)
(defparameter *r-tate-h* 40)
(defparameter *r-tate-w/2* (floor *r-tate-w* 2))
(defparameter *r-tate-h/2* (floor *r-tate-h* 2))

;;元の横向きのプレイヤー画像のサイズ
(defparameter *yoko-w* 24)
(defparameter *yoko-h* 32)
;;表示する横向きプレイヤー画像サイズ
(defparameter *r-yoko-w* 32)
(defparameter *r-yoko-h* 40)
(defparameter *r-yoko-w/2* (floor *r-yoko-w* 2))
(defparameter *r-yoko-h/2* (floor *r-yoko-h* 2))

;;元の横向きのプレイヤー攻撃画像のサイズ
(defparameter *atk-yoko-w* 36)
(defparameter *atk-yoko-h* 32)
;;表示する横向きのプレイヤー攻撃画像のサイズ
(defparameter *r-atk-yoko-w* 44)
(defparameter *r-atk-yoko-h* 40)

;;元の縦向きのプレイヤー攻撃画像のサイズ
(defparameter *atk-tate-w* 32)
(defparameter *atk-tate-h* 46)
;;表示する縦のプレイヤー攻撃画像のサイズ
(defparameter *r-atk-tate-w* 40)
(defparameter *r-atk-tate-h* 54)

(defparameter *w/2* (floor *obj-w* 2))
(defparameter *h/2* (floor *obj-h* 2))

;;オブジェクト画像表示サイズ
(defparameter *w-test* 36)
(defparameter *h-test* 36)

;;
(defparameter *tate* 20)
(defparameter *yoko* 30)

(defparameter *yoko-block-num* 21)
(defparameter *tate-block-num* 13)

;;マップ領域
(defparameter *map-w* (* *yoko-block-num* *r-blo-w*))
(defparameter *map-h* (* *tate-block-num* *r-blo-h*))

(defparameter *screen-w* 960)
(defparameter *screen-h* 720)
(defparameter *waku-size* 10) ;;ゲームフィールドの周りの枠太さ
(defparameter *c-rect* nil) ;;クライアント領域
(defparameter *p* nil)
(defparameter *e* nil)

;;画面領域
(defparameter *client-w* (+ *map-w* 150))
(defparameter *client-h* (* *r-tate-h* *tate*))
(defparameter *screen-center-x* nil)

(defparameter *brush* nil)
(defparameter *start* nil)
(defparameter *hmemDC* nil)
(defparameter *hbitmap* nil)


(defparameter *hogememDC* nil)
(defparameter *hogebitmap* nil)

(defparameter *kabe-break* nil)

(my-enum +brigand+ +door+ +dragon+ +hard-block+ +e-atk+ +hydra+ +key+ +yote1+ +orc+ +slime+ +soft-block+ +yuka+ +yusha+)

(defclass keystate ()
  ((right :accessor right :initform nil :initarg :right)
   (left  :accessor left  :initform nil :initarg :left)
   (down  :accessor down  :initform nil :initarg :down)
   (up    :accessor up    :initform nil :initarg :up)
   (enter :accessor enter :initform nil :initarg :enter)
   (shift :accessor shift :initform nil :initarg :shift)
   (z     :accessor z     :initform nil :initarg :z)
   (x     :accessor x     :initform nil :initarg :x)
   (c     :accessor c     :initform nil :initarg :c)))

(defparameter *keystate* (make-instance 'keystate))

(defparameter *orc-name*
  '("オークピー" "トンヌラ" "ブタピー" "オークタン" "コブタ" "ほげぞう" "オオオーク"
    "最終皇帝" "ブヒブヒ" "たけし" "オークデビル" "イノシシ" "茨城オーク"))

(defparameter *slime-name*
  '("スラリソ" "ヌルリン" "ベチョリン" "ベトリン" "デロデロ" "ヌルイム" "ズラリン"
    "ケロリン" "群馬スライム" "まさし" "スラぞう"))

(defparameter *hydra-name*
  '("ヒードラ" "よしひこ" "ヘビリン" "へびぞう" "スゴイヘビ" "キングギドラ" "ヘビックス"
    "ヤマダノ" "オロチ" "ハイドラ" "ドラタロウ" "にしきのくん" "パイソン"))

(defparameter *brigand-name*
  '("サンゾク" "ムナゲ" "ヒザゲ" "ワキゲ" "アラサー" "ぶりぶり" "ガンドー" "ケツゲ"
    "やわ毛" "バリカタ" "親方" "ヒゲガンド" "うす毛" "カンダタ" "ブリトニー"))

(defparameter *yote1-name*
  '("ヨテイチ" "メタイチ" "うなぎ" "ナニモシナイ"))

;; (defstruct party
;;   (posx 0)
;;   (posy 0)
;;   (players nil)
;;   (heal 2)
;;   (hammer 5)
;;   (map 1)
;;   (msg nil)
;;   (mode nil) ;;バトルモードや移動モードその他
;;   (item nil) ;;持ち物リスト
;;   (drop nil) ;;敵からのドロップ品一時保管場所
;;   (new-nakama nil)
;;   (monster-num 0)) ;;戦闘時の敵の総数

;; (defstruct player
;;   (hp 30)
;;   (maxhp 30)
;;   (agi 30)
;;   (maxagi 30)
;;   (str 30)
;;   (maxstr 30)
;;   (level 1)
;;   (level-exp 100)
;;   (exp 0)
;;   (name nil)
;;   (type 0) ;; 0:プレイヤー 1:オーク 2:スライム 3:ヒドラ 4:ブリガンド 5 メテルヨテイチ
;;   (buki '("なし" 0 0 0)))

(defstruct donjon
  (map nil)  ;;マップ
  (tate *tate*)  ;;縦幅
  (yoko *yoko*)  ;;横幅
  (enemies nil)
  (path nil)
  (yuka nil) ;;床
  (blocks nil) ;;ブロック
  (objects nil) ;;鍵とドア
  (stop-list nil)) ;;行き止まりリスト


;;ブロックとか
(defclass obj ()
  ((x        :accessor x        :initform 0      :initarg :x)
   (y        :accessor y        :initform 0      :initarg :y)
   (x2       :accessor x2       :initform 0      :initarg :x2)
   (y2       :accessor y2       :initform 0      :initarg :y2)
   (w        :accessor w        :initform 0      :initarg :w)
   (h        :accessor h        :initform 0      :initarg :h)
   (moto-w   :accessor moto-w   :initform 0      :initarg :moto-w)
   (moto-h   :accessor moto-h   :initform 0      :initarg :moto-h)
   (w/2      :accessor w/2      :initform 0      :initarg :w/2)
   (h/2      :accessor h/2      :initform 0      :initarg :h/2)
   (obj-type :accessor obj-type :initform 0      :initarg :obj-type)
   (img      :accessor img      :initform nil    :initarg :img)))

(defclass dmg-obj (obj)
  ((dmg-num  :accessor dmg-num   :initform 0   :initarg :dmg-num)
   (miny     :accessor miny      :initform 0   :initarg :miny)
   (maxy     :accessor maxy      :initform 0   :initarg :maxy)
   (y-dir    :accessor y-dir     :initform :up :initarg :y-dir)
   (x-dir    :accessor x-dir     :initform :left :initarg :x-dir)
   ))

(defclass buki (obj)
  ((atk  :accessor atk       :initform 0   :initarg :atk)
   (name :accessor name      :initform nil :initarg :name)))

(defclass player (obj)
  ((hp       :accessor hp       :initform 30  :initarg :hp)
   (maxhp    :accessor maxhp    :initform 30  :initarg :maxhp)
   (agi      :accessor agi      :initform 30  :initarg :agi)
   (def      :accessor def      :initform 30  :initarg :def)
   (str      :accessor str      :initform 30  :initarg :str)
   (dead     :accessor dead     :initform nil :initarg :dead) ;;死亡判定
   (atk-spd  :accessor atk-spd  :initform 8   :initarg :atk-spd) ;;攻撃速度
   (ido-spd  :accessor ido-spd  :initform 2   :initarg :ido-spd) ;;移動速度
   (key?     :accessor key?     :initform nil :initarg :key?) ;;鍵所持
   (level    :accessor level    :initform 1   :initarg :level)
   (dmg      :accessor dmg      :initform nil :initarg :dmg)
   (dmg-c    :accessor dmg-c    :initform 0   :initarg :dmg-c) ;;ダメージを受ける間隔
   (lvup-exp :accessor lvup-exp :initform 100 :initarg :lvup-exp) ;;次のレベルアップに必要な経験値
   (expe     :accessor expe     :initform 0   :initarg :expe)
   (name     :accessor name     :initform nil :initarg :name)
   (hammer   :accessor hammer   :initform 2   :initarg :hammer)
   (race     :accessor race     :initform nil :initarg :race) ;; 0:プレイヤー 1:オーク 2:スライム 3:ヒドラ 4:ブリガンド 5 メテルヨテイチ
   (buki     :accessor buki     :initform nil :initarg :buki)
   (walk-c   :accessor walk-c   :initform 0   :initarg :walk-c) ;;歩行アニメカウンター
   (walk-func :accessor walk-func :initform #'+   :initarg :walk-func)
   (tower-lv :accessor tower-lv :initform 1   :initarg :tower-lv)
   (dir      :accessor dir      :initform :down :initarg :dir)
   (dir-c    :accessor dir-c    :initform 0   :initarg :dir-c) ;;方向転換用カウンター
   (atk-now  :accessor atk-now  :initform nil :initarg :atk-now)
   (hammer-now :accessor hammer-now  :initform nil :initarg :hammer-now)
   (atk-c    :accessor atk-c    :initform 0   :initarg :atk-c) ;;攻撃モーション更新用
   (atk-img  :accessor atk-img  :initform 0   :initarg :atk-img) ;;攻撃画像番号 ０～２  
   ))
