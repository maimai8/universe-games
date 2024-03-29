(* TODO: plate と ball の 当たり判定 *)
open UniverseJs
open World
open Image
open Color

;; Random.self_init ()
  
(* block_t : ブロックの座標, ブロックの硬さ の組の型 *)
type block_t = (float * float) * int
(* ball_t : ボールの座標, ボールの進むベクトル, ボールの種類(コメント) の組の型 *)
type ball_t = (float * float) * (float * float) * string
(* item_t : アイテムの座標, アイテムの種類 の型 *)
type item_t = (float * float) * int
(* plate_t : 反射板の座標, (横の長さ, 縦の長さ） の型 *)
type plate_t = (float * float) * (float * float)
(* world_t : ブロックリ, ボールリ, アイテムリ, 反射板, スコア レベル の組の型 *)
type world_t = block_t list * ball_t list * item_t list * plate_t * int * int

(* -----画像関係----- *)

let width	= 550	(* 画面の幅 *)
let height	= 540	(* 画面の高さ *)

let ball_color  = darkSlateBlue
let plate_color = gray20

let br = 5.    (* ball radius *)
let ir = 10.   (* item radius *)
let bl_w = 50. (* block width *)
let bl_h = 20. (* block height *)
let pl_w = 60. (* plate width *)
let pl_h = 10. (* plate height *)

let background = empty_scene (float_of_int width) (float_of_int height)
let block c    = rectangle bl_w bl_h c (* tough color *) 
let ball       = circle br ball_color
let plate w h  = rectangle w h plate_color

let item1 = circle ir ~fill:true  ~outline_size:2. darkSlateBlue (* ボールを増やす *)
let item2 = circle ir ~fill:false ~outline_size:2. lightGray (* plateを短くする *)
let item3 = circle ir ~fill:true  ~outline_size:2. tomato (* plateを長くする *)
let item4 = circle ir ~fill:true  ~outline_size:2. gold (* ちょっと得点がもらえる *)
    
(* -----画像関係ここまで----- *)

(* -----初期値関係----- *)

let rec make_block_posns n m =
  if n = 0 && m = 0 then []
  else if m = 0 then make_block_posns (n-1) (width / int_of_float bl_w)
  else (float_of_int n, float_of_int m) :: make_block_posns n (m-1)

(* (float * float) list -> block_t list *)
let rec init_blocks block_posn_lst =
  match block_posn_lst with
  | [] -> []
  | (y, x) :: rest ->
    (((x-.1.)*.bl_w, (y-.1.)*.bl_h), (Random.int 4)+1) (* block num = 1~4 *)
    :: init_blocks rest

let rec init_balls n =
  if n = 0 then []
  else ((float_of_int (width / 2) +. pl_w /. 2., float_of_int (height - 200)), 
        ((Random.float 2.) -. 1., (Random.float 2.) -. 1.),
        "") :: init_balls (n-1)

let init_item n =
  let x = Random.float (float_of_int width) in
  let y = -.(Random.float (float_of_int (50*n))) in
  ((x, y), (Random.int 4)+1) (* item num = 1~4 *)
  
let rec init_items n = 
  let x = Random.float (float_of_int width) in
  let y = -.(Random.float (float_of_int (10*n))) in
  if n = 0 then []
  else ((x, y), (Random.int 4)+1)  (* item num = 1~4 *)
       :: init_items (n-1)
         
(* worldの初期値 *)
let initial_world =
  (init_blocks (make_block_posns 8 (width / int_of_float bl_w)), (* ブロックのリスト *)
   init_balls 2 , (* ボールのリスト *)
   init_items 20, (* アイテムのリスト *)
   ((float_of_int (width / 2), float_of_int (height - 30)), (pl_w, pl_h)), (* 反射板 *)
   0, 1)
  
(* -----初期値関係ここまで----- *)

(* -----スコアの判定関係-----*)

let _score = 1

(* block と ball のデータをもらう *)
(* block が ball とぶつかっていたら true, いなかったら false を返す *)
(* check : block_t -> ball_t -> (string * bool) *)
let check block ball = match (block, ball) with
  | ((lx, ly), tough), ((ax, ay), (vx, vy), k) -> (* x,y は ball の四角の左上座標 *)
    let br2 = br *. 2. in
    if ly-.br <= ay && ay <= ly+.bl_h+.br && lx+.bl_w-.br <= ax && ax <= lx+.bl_w 
    then ("right", true)
    else if ly-.br <= ay && ay <= ly+.bl_h+.br && lx-.br2 <= ax && ax <= lx-.br
    then ("left", true)
    else if ly-.br2 <= ay && ay <= ly-.br && lx-.br <= ax && ax <= lx+.bl_w
    then ("top", true)
    else if ly+.bl_h-.br <= ay && ay <= ly+.bl_h && lx-.br <= ax && ax <= lx+.bl_w
    then ("bottom", true)
    else ("fail", false)

(* 各 block が 各 ball とぶつかっているか判定してスコアを出す *)
(* block_t list -> ball_t list -> int *)
let check_score block_lst score =
  List.fold_left (fun s block -> match block with
      | ((_, _), tough) -> if tough <= 0 then s + _score else s) score block_lst
    
(* -----スコアの判定関係ここまで-----*)
  
(* -----キー操作関係-----*)

(* key_draw : world_t -> (world_t, 'a) World.t *)  
let key_draw world key =
  match world with
    (blocklst, balllst, itemlst, ((x, y), (pw, ph)), score, level) ->
    if key = "left" && x > 0. then
      (blocklst, balllst, itemlst, ((x-.7., y), (pw, ph)), score, level)
    else if key = "right" && x +. pw < float_of_int width then
      (blocklst, balllst, itemlst, ((x+.7., y), (pw, ph)), score, level)
    else if key = "\\190" && x +. pw < (float_of_int width)-.30. then
      (blocklst, balllst, itemlst, ((x+.30., y), (pw, ph)), score, level)
    else if key = "\\188" && x > 30. then
      (blocklst, balllst, itemlst, ((x-.30., y), (pw, ph)), score, level)
    else
      (blocklst, balllst, itemlst, ((x, y), (pw, ph)), score, level)
      
(* -----キー操作関係ここまで-----*)

(* -----on_tick関係----- *)

(* ある item が plate とぶつかったかどうか調べ、
   ぶつかっていたら true , ぶつかっていなかったら false を返す *)
let check_i plate item =
  match (plate, item) with
  | ((px, py), (pw, ph)), ((ix, iy), kind) ->
    let ir2 = ir *. 2. in
    if px-.ir2 <= ix && ix <= px+.pw && py-.ir <= iy && iy <= py
    then true else false

(* plate_t -> item_t list -> item_t list * item_t list *)
let rec remove_itemlst_or_remain_itemlst plate itemlst =
  List.partition (fun item -> check_i plate item) itemlst
    
(* world と ぶつかった item の 種類をもらってきたら、 種類に応じてworldを変更する *)
(* world_t -> item_t list -> world_t *)
let rec change_by_items world remove_itemlst =
  match world with
    (blocklst, balllst, itemlst, plate, score, level) ->
    match remove_itemlst with
    | [] -> world
    | item :: rest -> match item with ((ix, iy), k) ->
      if check_i plate item then (* この item は plate とぶつかっている *)
        match plate with ((x, y), (w, h)) ->
          if k = 1 then
            let (balllst : ball_t list) = (init_balls 1) @ balllst in
            change_by_items (blocklst, balllst, itemlst, ((x, y), (w, h)), score, level) rest
            (* ball を増やす *)
          else if k = 2 then
            let w = w /. 1.2 in
            change_by_items (blocklst, balllst, itemlst, ((x, y), (w, h)), score, level) rest
            (* plate の長さを半分にする *)
          else if k = 3 then
            let w = w +. 4. in
            change_by_items (blocklst, balllst, itemlst, ((x, y), (w, h)), score, level) rest
            (* plate の長さを倍にする *)
          else if k = 4 then
          change_by_items (blocklst, balllst, itemlst, ((x, y), (w, h)), score+10, level) rest
          (* 得点+10 *)
          else change_by_items (blocklst, balllst, itemlst, ((x, y), (w, h)), score, level) rest
          (* それ以外のアイテムなら何もしない *)
      else change_by_items (blocklst, balllst, itemlst, plate, score, level) rest
      
(* -----反射関係----- *)

(* 各 ball が　plate とぶつかっているか判定して新たな ball list を返す *)
let reflect_plate ball_lst plate =
  (* plate が ball とぶつかっていたら 新たな方向ベクトル をセットし, 
     ぶつかっていなかったら そのままの方向ベクトル をセット *)
  (* reflect_p : plate_t -> ball_t -> ball_t *)
  let reflect_p plate ball = match (plate, ball) with
    | ((px, py), (pw, ph)), ((bx, by), (ix, iy), k) ->
      let br2 = br *. 2. in
      if px-.br2 <= bx && bx <= px+.pw && py-.br2 <= by && by <= py
      then ((bx, by), (ix, -.iy), "reflect plate!")
      else ball
  in List.map (fun ball -> reflect_p plate ball) ball_lst
  
(* 各 ball が 壁 とぶつかっているかどうか判定して ball list を返す *)
let reflect_wall ball_lst =
  (* 壁 と ball がぶつかっていたら 新たな方向ベクトルをセット *)
  (* reflect_w : ball_t -> ball_t *)
  let reflect_w ball = match ball with
    | ((x, y), (ix, iy), kind) ->
      let br2 = br *. 2. in
      if x+.br2 >= (float_of_int width) then ((x, y), (-.ix, iy), "reflect right wall")
      else if x <= 0. then ((x, y), (-.ix, iy), "reflect left wall")
      else if y <= 0. then ((x,  y), (ix, -.iy), "reflect top wall")
      else ball
  in List.map reflect_w ball_lst

(* 全ての block に対して、全ての ball が反応したとき、 block list はどうなるか *)
(* block_t list -> ball_t list -> block_t list *)
let change_blocks (block_lst : block_t list) (ball_lst : ball_t list) =
  (* ある block に対して、 全ての ball が反応したとき、block がどうなるか *)
  (* block_t -> ball_t list -> block_t *)
  let change_block (block : block_t) (ball_lst : ball_t list) =
    (* block と ball がぶつかっていたら、硬さを-1する *)
    (* block_t -> ball_t -> block_t *)
    let crash block ball =
      if snd (check block ball) then
        match block with
        | ((x, y), h) -> ((x, y), (h-1))
      else block
    in List.fold_left (fun block ball -> crash block ball) block ball_lst
  in List.map (fun block -> change_block block ball_lst) block_lst

(* 全ての block に対して、 全ての ball が反応したとき、 ball list がどうなるか *)
let react_balls blocklst balllst =
  (* 全ての block に対して、ある ball が反応したとき、 ball がどうなるか *)
  (* block_t list -> ball_t -> ball_t *)
  let react_ball blocklst ball =
    (* block と ball がぶつかっていたら、ball の方向ベクトルを変更する *)
    (* block_t -> ball_t -> ball_t *)
    let reflect_b block ball =
      match ball with
        ((x,  y), (ix, iy), kind) ->
        match fst (check block ball) with
        | "right"  -> ((x, y), (-.ix, iy), "reflect right block")
        | "left"   -> ((x, y), (-.ix, iy), "reflect left block")
        | "top"    -> ((x, y), (ix, -.iy), "reflect top block")
        | "bottom" -> ((x, y), (ix, -.iy), "reflect bottom block")
        | _ -> ((x, y), (ix, iy), "not reflect block")           
    in List.fold_left (fun ball block -> reflect_b block ball ) ball blocklst
  in List.map (fun ball -> react_ball blocklst ball) balllst

(* 全ての block に対して 全ての ball が どう reflect したか、さらに 完全に crash した block は除去する *)
(* block_t list -> block_t list *)
let remove_blocks block_lst =
  List.filter (fun block -> match block with ((x, y), tough) -> tough > 0) block_lst
    
(* ball の y 座標が一番下についていたら ball を消す *)
(* remove_ball : int -> bool *)
let lower_y y =
  if y > float_of_int height then false else true

let remove_balls ball_lst =
  List.filter
    (fun b -> match b with ((x, y), (ix, iy), k) -> lower_y y) ball_lst 

let remove_items item_lst =
  List.map
    (fun i -> match i with ((x, y), k) ->
        if lower_y y then i else ((x, -.(Random.float 400.)), (Random.int 4)+1)) item_lst
    
(* ball を方向ベクトル分動かす *)
 (* move_ball : ball_t -> ball_t *) 
let move_ball ((x, y), (ix, iy), kind) =
  if 0.2 > (abs_float iy) then
    let iy = iy +. (Random.float 1.) in ((x+.ix, y+.iy), (ix, iy), kind) 
  else ((x +. ix*.2., y +. iy*.2.), (ix, iy), kind)

(* item を下に動かす *)
(* move_item : item_t -> item_t *)
let move_item ((x, y), k) =
  if k mod 2 <> 0 then ((x, y +. 0.2), k)
  else ((x, y +. 0.1), k)
       
(* ball を 現在の方向ベクトルに応じて動かす *)
(* ball が 壁 あるいは plate あるいは block と衝突したら 新たな方向ベクトルをセット *)
(* ball と block が衝突したら score 加算 *)
(* 新たな block list をセット *)
(* 新たな ball list をセット *)
(* move_on_tick : world_t -> (world_t, 'a) World.t *)
let move_on_tick world =
  match world with
    (blocklst, balllst, itemlst, plate, score, level) ->
    let balllst : ball_t list = List.map move_ball balllst in
    let itemlst : item_t list = List.map move_item itemlst in
    let balllst : ball_t list = reflect_wall balllst in
    let balllst : ball_t list = reflect_plate balllst plate in
    let balllst : ball_t list = react_balls blocklst balllst in
    let blocklst : block_t list = change_blocks blocklst balllst in
    let newscore : int = check_score blocklst score in
    let (remove, remain) = remove_itemlst_or_remain_itemlst plate itemlst in 
    let blocklst : block_t list = remove_blocks blocklst in
    let balllst : ball_t list = remove_balls balllst in
    let (level, newblocklst) =
      match blocklst with
      | [] -> (level+1, init_blocks (make_block_posns ((Random.int 15)+1) (width / int_of_float bl_w)))
      | _  -> (level, blocklst) in
    let itemlst = remain @ (init_items (List.length remove)) in
    change_by_items (newblocklst, balllst, itemlst, plate, newscore, level) remove
    
(* ----- on_tick関係ここまで ----- *)

(* ----- item 関係 ----- *)

let rec make_imageit_lst itemlst =
  match itemlst with
  | [] -> []
  | item :: rest ->
    match item with
      ((x, y), kind) ->
      if kind = 1 then item1 :: make_imageit_lst rest
      else if kind = 2 then item2 :: make_imageit_lst rest
      else if kind = 3 then item3 :: make_imageit_lst rest
      else item4 :: make_imageit_lst rest
             
(* ----- item 関係ここまで ----- *)
(* ----- 描画関係 ----- *)

(* y座標を表示用に変換 *)
(* change_y : (int * int) -> int *)
let change_y y = (float_of_int height) -. y

(* block_t list から block の座標を取り出す *)
(* block_t list -> float * float *)
let block_posns block_lst =
  List.map
    (fun blocks -> match blocks with ((x, y), h) -> (x, y))
    block_lst
    
(* ball_t list から ball の座標を取り出す *)
(* ball_t list -> float * float *)
let ball_posns ball_lst =
  List.map
    (fun ball -> match ball with ((x, y), (ix, iy), k) -> (x, y))
    ball_lst

(* item_t list から ball の座標を取り出す *)
(* item_t list -> float * float *)
let item_posns item_lst =
  List.map
    (fun item -> match item with ((x, y), k) -> (x, y))
    item_lst

let rec make_image_lst image n =
  if n = 0 then []
  else image :: make_image_lst image (n-1)

let rec make_imagebl_lst imagef blocklst =
  match blocklst with
  | [] -> []
  | block :: rest ->
    match block with
      ((x, y), tough) ->
      if tough = 1 then imagef mistyRose2 :: make_imagebl_lst imagef rest
      else if tough = 2 then imagef honeydew2 :: make_imagebl_lst imagef rest
      else if tough = 3 then imagef honeydew3 :: make_imagebl_lst imagef rest
      else imagef honeydew4 :: make_imagebl_lst imagef rest

let ball_reflect_num balllst =
  match balllst with
  | [] -> "no ball..."
  | ((_, _), (_, _), k) :: r -> k

let last world = match world with
    (blocklst, balllst, itemlst, ((px, py), (pw, ph)), score, level) ->
    (List.length balllst = 0) || pw <= 0. || level >= 10

(* 各座標から画像を作成 *)
(* draw : world_t -> Image.t *)
let draw world = match world with
    (blocklst, balllst, itemlst, ((px, py), (pw, ph)), score, level) ->
    let block_pos = block_posns blocklst in
    let ball_pos = ball_posns balllst in
    let item_pos = item_posns itemlst in
    let ball_len = List.length balllst in
    (* let item_len = List.length itemlst in *) 
    (place_image (text (string_of_int score) ~size:50 bisque4) (20., 20.)
       (place_image (text ("Lv. " ^ (string_of_int level)) ~size:30 bisque4) (0., 500.)
          (place_images (make_imageit_lst itemlst) item_pos
             (place_images (make_image_lst ball ball_len) ball_pos
                (place_images (make_imagebl_lst block blocklst) block_pos
                   (place_image (plate pw ph) (px, py) background))))))

let draw_last world = match world with
    (blocklst, balllst, itemlst, ((px, py), (pw, ph)), score, level) ->
    (place_image (text ("Your score is " ^ (string_of_int score) ^ "\n"
                        ^ "Your level is " ^ (string_of_int level) ) ~size:30 bisque4)
       (20., float_of_int (height/2))
       background)
    
(* -----描画関係ここまで----- *)

(* ゲーム開始 *)
let _ =
  big_bang initial_world
	   ~name:"blocks"
           ~width:width
	   ~height:height
	   ~to_draw:draw
	   ~on_key_press:key_draw 
           ~on_tick:move_on_tick
           ~stop_when:last
           ~to_draw_last:draw_last
	   ~rate:0.005			(* ゲームの動く速さ *)
