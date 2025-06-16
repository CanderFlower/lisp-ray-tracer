#lang racket
(require racket/string
         racket/port
         racket/pretty
         racket/json
         "ast.rkt"
         "utils.rkt"
         "render.rkt")
(provide handle-command eval-expr 
         make-default-scene
         builtin-funcs)

;; ----------------------------------------
;; DSL 执行环境：handle-command, eval-expr, 支持 let, repeat, 各种命令
;; ----------------------------------------

;; global-scene 在 ast.rkt 定义并在 interpreter.rkt 初始化

;; environment: alist mapping symbol->value，用于 DSL 表达式求值（let、repeat 中变量绑定）
;; 顶层 env 通常空

;; eval-expr: 递归评估表达式 expr，在 env 中查找变量或应用内置函数
;; expr: Racket datum, 可以是 number, string, symbol, list
;; 支持 built-in functions: +, -, *, /, sin, cos, tan, sqrt, abs, number->string, string-append, string-pad-left, etc.
;; 支持 access to scene/query? 只做表达式计算，不修改 scene.
;; 内置函数映射:
(define builtin-funcs
  ;; symbol -> procedure accepting Racket values, returning Racket value
  (hash
   '+ (lambda args (apply + args))
   '- (lambda args (match args
                    [(list x) (- x)]
                    [(list x y) (- x y)]
                    [(list x y rest ...) (apply - x y rest)]))
   '* (lambda args (apply * args))
   '/ (lambda args (match args
                    [(list x y) (/ x y)]
                    [(list x y rest ...) (apply / x y rest)]))
   'sin (lambda (x) (sin x))
   'cos (lambda (x) (cos x))
   'tan (lambda (x) (tan x))
   'sqrt (lambda (x) (sqrt x))
   'abs (lambda (x) (abs x))
   'number->string (lambda (x) (number->string x))
   'string-append (lambda args (apply string-append args))
   'string-pad-left (lambda (s total pad)
                     (string-pad-left s (inexact->exact total) pad))))

;; eval-expr: expr: datum; env: list of pairs (symbol . value)
(define (eval-expr expr env)
  (cond
    [(number? expr) expr]
    [(string? expr) expr]
    [(symbol? expr)
     (let ([binding (assoc expr env)])
       (if binding
           (cdr binding)
           (error "Undefined variable in expression:" expr)))]
    [(list? expr)
     (let ([op (first expr)] [args (rest expr)])
       (cond
         [(hash-has-key? builtin-funcs op)
          (let ([f (hash-ref builtin-funcs op)])
            (apply f (map (λ (sub) (eval-expr sub env)) args)))]
         [else
          (error "Unknown function in expression:" op)]))]
    [else (error "Cannot eval expression of type" expr)]))


(define (make-default-scene)
  ;; Scene(camera objects lights groups background settings counter)
  (let ([cam (Camera (Vec3 0 0 5) (Vec3 0 0 0) (Vec3 0 1 0) 60 300 200)]
        [objs (make-hash)]
        [lights (make-hash)]
        [groups (make-hash)]
        [background '(gradient (color 0.5 0.7 1.0) (color 1.0 1.0 1.0))]
        [rc 0])
    ;; 初始化 settings 为可变哈希，然后插入键值对
    (let ([settings (make-hash)])
      (hash-set! settings 'samples 50)
      (hash-set! settings 'max-depth 5)
      (Scene cam objs lights groups background settings rc))))

;; ----------------------------------------
;; handle-command: 解析并执行一条 DSL 命令（S-Expr）
;; expr: list, first element is symbol: 命令名称或控制结构(let, repeat)
;; env: environment alist for eval-expr
;; 返回 void
;; ----------------------------------------
(define (handle-command expr env)
  (cond
    ;; 空或非 list 不处理
    [(not (list? expr)) (void)]
    [else
     (match expr
       ;; let: (let var expr body...)
       ;; 绑定 var 到 expr 的值，然后依次执行 body forms，再返回 void
       [`(let ,var ,val-expr . ,bodies)
        (unless (symbol? var) (error "let: first argument must be symbol"))
        (let ([v (eval-expr val-expr env)])
          (for ([bd bodies])
            (handle-command bd (cons (cons var v) env)))
          (void))]
       ;; repeat: (repeat n body...)
       [`(repeat ,n-expr . ,bodies)
        (let ([n-val (eval-expr n-expr env)])
          (unless (and (number? n-val) (integer? n-val) (>= n-val 0))
            (error "repeat: count must be non-negative integer:" n-val))
          (for ([i (in-range n-val)])
            (for ([bd bodies])
              (handle-command bd (cons (cons 'i i) env))))
          (void))]
       ;; clear-scene
       [`(clear-scene)
        (global-scene (make-default-scene))
        (displayln "Scene cleared.")
        (void)]
       ;; set-camera: (set-camera (camera (pos x y z) ...))
       [`(set-camera ,camera-spec)
        (let ([cam (parse-camera-spec camera-spec env)])
          (let ([C (Scene-camera (global-scene))])
            (set-Camera-pos! C (Camera-pos cam))
            (set-Camera-look-at! C (Camera-look-at cam))
            (set-Camera-up! C (Camera-up cam))
            (set-Camera-fov! C (Camera-fov cam))
            (set-Camera-width! C (Camera-width cam))
            (set-Camera-height! C (Camera-height cam)))
          (displayln "Camera updated.")
          (void))]
       ;; add-object: (add-object name (sphere ...)|(plane ...)|...)
       [`(add-object ,name . ,rest)
        (unless (symbol? name) (error "add-object: name must be symbol"))
        (when (hash-has-key? (Scene-objects (global-scene)) name)
          (error "add-object: object already exists:" name))
        (let ([spec (if (= (length rest) 1) (first rest) (cons 'begin rest))])
          (let ([obj (parse-object-spec spec env)])
            (hash-set! (Scene-objects (global-scene)) name obj)
            (displayln (format "Added object ~a." name))))
        (void)]
       ;; update-object: (update-object name (center x y z)) 或 (update-object name (material ...)), 只能一个更新 spec
       [`(update-object ,name ,update-spec)
        (unless (symbol? name) (error "update-object: name must be symbol"))
        (let ([tbl (Scene-objects (global-scene))])
          (unless (hash-has-key? tbl name)
            (error "update-object: object not found:" name))
          (let ([old (hash-ref tbl name)])
            (apply-object-update! old update-spec env)
            (displayln (format "Updated object ~a." name))))
        (void)]
       ;; transform: (transform name spec)
       [`(transform ,name ,transform-spec)
        (unless (symbol? name) (error "transform: name must be symbol"))
        ;; 可能 name 是 object, light, group, 或 'camera
        (cond
          [(hash-has-key? (Scene-objects (global-scene)) name)
           (let* ([obj (hash-ref (Scene-objects (global-scene)) name)]
                  [M (parse-transform-spec transform-spec env)]
                  [old (object-transform-get obj)]
                  [newM (mat4-mul old M)])
              [displayln (format "old: ~a." old)]
              [displayln (format "M: ~a." M)]
              [displayln (format "newM: ~a." newM)]
             (object-transform-set! obj newM)
             (displayln (format "Applied transform to object ~a." name)))]
          [(hash-has-key? (Scene-groups (global-scene)) name)
           (let* ([grp (hash-ref (Scene-groups (global-scene)) name)]
                  [M (parse-transform-spec transform-spec env)])
             ;; 对 group，立即将 transform 应用于所有成员对象，并保留 group transform 以便 future transforms
             (set-Group-transform! grp (mat4-mul (Group-transform grp) M))
             ;; 同时对成员即时更新其 transform
             (for ([mem (Group-members grp)])
               (when (hash-has-key? (Scene-objects (global-scene)) mem)
                 (let ([obj (hash-ref (Scene-objects (global-scene)) mem)])
                   (object-transform-set! obj (mat4-mul (object-transform-get obj) M)))))
             (displayln (format "Applied transform to group ~a." name)))]
          [(eq? name 'camera)
           ;; transform camera: apply to pos, look-at as point; up as dir
           (let* ([cam (Scene-camera (global-scene))]
                  [M (parse-transform-spec transform-spec env)]
                  ;; pos and look-at as points
                  [new-pos (mat4-transform-point M (Camera-pos cam))]
                  [new-look (mat4-transform-point M (Camera-look-at cam))]
                  ;; up as direction: transform-dir
                  [new-up (mat4-transform-dir M (Camera-up cam))]
                  ;; 更新
                 )
             (set-Camera-pos! cam new-pos)
             (set-Camera-look-at! cam new-look)
             (set-Camera-up! cam new-up)
             (displayln "Transformed camera."))
           ]
          [(hash-has-key? (Scene-lights (global-scene)) name)
           ;; transform light: 对 light.pos (if positional) 或 dir (if directional)
           (let ([light (hash-ref (Scene-lights (global-scene)) name)]
                 [M (parse-transform-spec transform-spec env)])
             (cond
               [(PointLight? light)
                (set-PointLight-pos! light (mat4-transform-point M (PointLight-pos light)))
                (displayln (format "Transformed point-light ~a." name))]
               [(SpotLight? light)
                (set-SpotLight-pos! light (mat4-transform-point M (SpotLight-pos light)))
                (set-SpotLight-dir! light (mat4-transform-dir M (SpotLight-dir light)))
                (displayln (format "Transformed spot-light ~a." name))]
               [(DirectionalLight? light)
                ;; dir: transform as direction; position irrelevant
                (set-DirectionalLight-dir! light (mat4-transform-dir M (DirectionalLight-dir light)))
                (displayln (format "Transformed directional-light ~a." name))]
               [else
                (displayln (format "transform: light type not supported for ~a" name))])
           )
          ]
          [else
           (error "transform: name not found:" name)])
        (void)]
       ;; group: (group group-name name1 name2 ...)
       [`(group ,group-name ,@members)
        (unless (symbol? group-name) (error "group: group-name must be symbol"))
        ;; ensure members exist
        (for ([m members])
          (unless (symbol? m) (error "group: member name must be symbol")))
        ;; 创建 group struct，初始 transform identity
        (let ([grp (Group members mat4-identity)])
          (hash-set! (Scene-groups (global-scene)) group-name grp)
          (displayln (format "Group ~a created with members ~a." group-name members)))
        (void)]
       ;; ungroup: remove group entry
       [`(ungroup ,group-name)
        (unless (symbol? group-name) (error "ungroup: group-name must be symbol"))
        (when (hash-has-key? (Scene-groups (global-scene)) group-name)
          (hash-remove! (Scene-groups (global-scene)) group-name)
          (displayln (format "Group ~a removed." group-name)))
        (void)]
       ;; list-objects
       [`(list-objects)
        (displayln "Objects:")
        (for ([(name obj) (in-hash (Scene-objects (global-scene)))])
          (let ([summary (object-summary obj)])
            (displayln (format "  ~a: ~a" name summary))))
        (void)]
       ;; list-lights
       [`(list-lights)
        (displayln "Lights:")
        (for ([(name light) (in-hash (Scene-lights (global-scene)))])
          (let ([summary (light-summary light)])
            (displayln (format "  ~a: ~a" name summary))))
        (void)]
       ;; list-groups
       [`(list-groups)
        (displayln "Groups:")
        (for ([(name grp) (in-hash (Scene-groups (global-scene)))])
          (displayln (format "  ~a: members=~a" name (Group-members grp))))
        (void)]
       ;; add-light: (add-light name (point-light ...)|...)
       [`(add-light ,name . ,rest)
        (unless (symbol? name) (error "add-light: name must be symbol"))
        (when (hash-has-key? (Scene-lights (global-scene)) name)
          (error "add-light: light already exists:" name))
        (let ([spec (if (= (length rest) 1) (first rest) (cons 'begin rest))])
          (let ([light (parse-light-spec spec env)])
            (hash-set! (Scene-lights (global-scene)) name light)
            (displayln (format "Added light ~a." name))))
        (void)]
       ;; update-light: (update-light name (pos x y z)) or (update-light name (intensity r g b)), or both?
       [`(update-light ,name ,update-spec)
        (unless (symbol? name) (error "update-light: name must be symbol"))
        (let ([tbl (Scene-lights (global-scene))])
          (unless (hash-has-key? tbl name)
            (error "update-light: light not found:" name))
          (let ([light (hash-ref tbl name)])
            (apply-light-update! light update-spec env)
            (displayln (format "Updated light ~a." name))))
        (void)]
       ;; remove-object: (remove-object name)
       [`(remove-object ,name)
        (unless (symbol? name) (error "remove-object: name must be symbol"))
        (when (hash-has-key? (Scene-objects (global-scene)) name)
          (hash-remove! (Scene-objects (global-scene)) name)
          (displayln (format "Removed object ~a." name)))
        (void)]
      ;; remove-light
      [`(remove-light ,name)
       (unless (symbol? name) (error "remove-light: name must be symbol"))
       (when (hash-has-key? (Scene-lights (global-scene)) name)
         (hash-remove! (Scene-lights (global-scene)) name)
         (displayln (format "Removed light ~a." name)))
       (void)]
      ;; remove-group
      [`(remove-group ,name)
       (unless (symbol? name) (error "remove-group: name must be symbol"))
       (when (hash-has-key? (Scene-groups (global-scene)) name)
         (hash-remove! (Scene-groups (global-scene)) name)
         (displayln (format "Removed group ~a." name)))
       (void)]
      ;; set-background: (set-background (color r g b)) or (set-background (gradient (color ...) (color ...)))
      [`(set-background ,bg-spec)
       (let ([b (parse-background-spec bg-spec env)])
         (let ([scene (global-scene)]) 
            (set-Scene-background! scene b))
         (displayln "Background updated."))
       (void)]
      ;; set-global: (set-global (samples n)) or (set-global (max-depth d))
      [`(set-global ,setting-spec)
       (match setting-spec
         [`(samples ,n-expr)
          (let ([n (eval-expr n-expr env)])
            (unless (and (number? n) (integer? n) (>= n 1))
         (error "set-global samples: must be positive integer"))
            (hash-set! (Scene-settings (global-scene)) 'samples n)
            (displayln (format "Global samples set to ~a." n)))]
         [`(max-depth ,d-expr)
          (let ([d (eval-expr d-expr env)])
            (unless (and (number? d) (integer? d) (>= d 1))
         (error "set-global max-depth: must be positive integer"))
            (hash-set! (Scene-settings (global-scene)) 'max-depth d)
            (displayln (format "Global max-depth set to ~a." d)))]
         [else (error "set-global: unknown setting spec" setting-spec)])
       (void)]
      ;; save-scene: (save-scene "file.json")
      [`(save-scene ,file-expr)
       (let ([fname (eval-expr file-expr env)])
         (unless (string? fname) (error "save-scene: filename must be string"))
         (call-with-output-file fname
           (lambda (out)
        (write-json (scene->json (global-scene)) out))
           #:exists 'replace)
         (displayln (format "Scene saved to ~a." fname)))
       (void)]
      ;; load-scene: (load-scene "file.json")
      [`(load-scene ,file-expr)
       (let ([fname (eval-expr file-expr env)])
         (unless (string? fname) (error "load-scene: filename must be string"))
         (call-with-input-file fname
           (lambda (in)
        (let ([j (read-json in)])
          (json->scene! (global-scene) j)))
           )
         (displayln (format "Scene loaded from ~a." fname)))
       (void)]
      ;; render: (render (width w)? (height h)? (samples n)? (max-depth d)? (filename "f.ppm")?)
      [`(render . ,opts)
        ;; 调试打印 expr 和 opts
        (displayln (format "DEBUG: render expr = ~a" expr))
        (displayln (format "DEBUG: render opts = ~a" opts))
        ;; 获取当前场景 parameter
        (define scene (global-scene))
        ;; 解析选项列表
        (define opt-map (parse-render-opts opts env))
        (displayln (format "DEBUG: render options map = ~a" opt-map))
        ;; 提取参数或使用默认
        (define width
          (or (hash-ref opt-map 'width #f)
              (Camera-width (Scene-camera scene))))
        (define height
          (or (hash-ref opt-map 'height #f)
              (Camera-height (Scene-camera scene))))
        (define samples
          (or (hash-ref opt-map 'samples #f)
              (hash-ref (Scene-settings scene) 'samples)))
        (define max-depth
          (or (hash-ref opt-map 'max-depth #f)
              (hash-ref (Scene-settings scene) 'max-depth)))
        (define filename
          (or (hash-ref opt-map 'filename #f)
              (generate-filename! scene)))
        (displayln (format "DEBUG: render parameters: width=~a, height=~a, samples=~a, max-depth=~a, filename=~a"
                            width height samples max-depth filename))
        ;; 调用渲染函数
        (render-scene-to-ppm scene width height samples max-depth filename)
        (displayln
          (format "Rendered to ~a (resolution ~ax~a, samples=~a, max-depth=~a)"
                  filename width height samples max-depth))
        ;; 返回 void
        (void)]
      ;; help
      [`(help)
       (display-help)
       (void)]
      ;; exit
       [`(exit)
        (displayln "Bye.")
        (exit 0)]
       ;; unknown
       [_
        (error "Unknown or invalid command:" expr)])]))

;; ----------------------------------------
;; 以下为 parse-* 辅助函数
;; ----------------------------------------
;; parse-camera-spec: expr: '(camera (pos x y z) (look-at x y z) (up x y z) (fov deg) (width w) (height h))
;; 返回 Camera struct
(define (parse-camera-spec sexpr env)
  (match sexpr
    [`(camera . ,fields)
     (define pos (parse-vec3-field 'pos fields env))
     (define look (parse-vec3-field 'look-at fields env))
     (define up  (parse-vec3-field 'up fields env))
     (define fov (eval-expr (second (assoc 'fov fields)) env))
     (define width (eval-expr (second (assoc 'width fields)) env))
     (define height (eval-expr (second (assoc 'height fields)) env))
     (unless (and (number? fov) (number? width) (number? height))
       (error "parse-camera-spec: invalid fov/width/height"))
     (Camera (Vec3 (first pos) (second pos) (third pos))
             (Vec3 (first look) (second look) (third look))
             (Vec3 (first up) (second up) (third up))
             fov width height)]
    [_ (error "Invalid camera spec:" sexpr)]))

;; parse-vec3-field: key symbol, fields: list of lists, env: environment
;; 返回 list of 3 numbers
(define (parse-vec3-field key fields env)
  (let ([item (assoc key fields)])
    (unless item (error "Missing field" key "in camera spec"))
    (match item
      [`(,key ,x ,y ,z)
       (list (eval-expr x env) (eval-expr y env) (eval-expr z env))]
      [_ (error "Invalid "~a" field" key)])))
;; parse-object-spec: expr like '(sphere (center ...) (radius ...) (material diffuse (color ...))) 等
;; 返回 object struct with fields filled; transform 初始为 identity
(define (parse-object-spec sexpr env)
  (match sexpr
    ;; sphere
    [`(sphere . ,props)
     ;; 解析 center
     (define center (parse-vec3-prop 'center props env))
     ;; 解析 radius
     (define radius-entry (assoc 'radius props))
     (unless radius-entry (error "parse-object-spec: missing radius"))
     (define radius (eval-expr (second radius-entry) env))
     ;; 解析 material: 使用 cdr 拿到 '(diffuse (color ...))
     (define mat-entry (assoc 'material props))
     (unless mat-entry (error "parse-object-spec: missing material"))
     (define mat-rest (cdr mat-entry)) ; '(diffuse (color ...))
     (unless (and (list? mat-rest) (not (null? mat-rest)))
       (error "parse-object-spec: invalid material spec" mat-entry))
     (define mat (parse-material-spec mat-rest env))
     ;; 构造 Sphere
     (Sphere (Vec3 (first center) (second center) (third center))
             radius
             mat
             mat4-identity)]
    ;; plane
    [`(plane . ,props)
     ;; 解析 normal
     (define normal (parse-vec3-prop 'normal props env))
     ;; 解析 dist
     (define dist-entry (assoc 'dist props))
     (unless dist-entry (error "parse-object-spec: missing dist"))
     (define dist (eval-expr (second dist-entry) env))
     ;; 解析 material
     (define mat-entry (assoc 'material props))
     (unless mat-entry (error "parse-object-spec: missing material"))
     (define mat-rest (cdr mat-entry))
     (unless (and (list? mat-rest) (not (null? mat-rest)))
       (error "parse-object-spec: invalid material spec" mat-entry))
     (define mat (parse-material-spec mat-rest env))
     ;; 构造 Plane
     (Plane (Vec3 (first normal) (second normal) (third normal))
            dist
            mat
            mat4-identity)]
    ;; box
    [`(box . ,props)
     ;; 解析 min/max
     (define vmin (parse-vec3-prop 'min props env))
     (define vmax (parse-vec3-prop 'max props env))
     ;; 解析 material
     (define mat-entry (assoc 'material props))
     (unless mat-entry (error "parse-object-spec: missing material"))
     (define mat-rest (cdr mat-entry))
     (unless (and (list? mat-rest) (not (null? mat-rest)))
       (error "parse-object-spec: invalid material spec" mat-entry))
     (define mat (parse-material-spec mat-rest env))
     ;; 构造 Box
     (Box (Vec3 (first vmin) (second vmin) (third vmin))
          (Vec3 (first vmax) (second vmax) (third vmax))
          mat
          mat4-identity)]
    ;; triangle 如果需要
    [`(triangle . ,props)
     ;; 解析三顶点
     (define p1 (parse-vec3-prop 'p1 props env))
     (define p2 (parse-vec3-prop 'p2 props env))
     (define p3 (parse-vec3-prop 'p3 props env))
     ;; 解析 material
     (define mat-entry (assoc 'material props))
     (unless mat-entry (error "parse-object-spec: missing material"))
     (define mat-rest (cdr mat-entry))
     (unless (and (list? mat-rest) (not (null? mat-rest)))
       (error "parse-object-spec: invalid material spec" mat-entry))
     (define mat (parse-material-spec mat-rest env))
     ;; 构造 Triangle
     (Triangle (Vec3 (first p1) (second p1) (third p1))
               (Vec3 (first p2) (second p2) (third p2))
               (Vec3 (first p3) (second p3) (third p3))
               mat
               mat4-identity)]
    [else (error "Unknown object type in parse-object-spec:" sexpr)]))

;; parse-vec3-prop: 类似 parse-vec3-field，但 props 是 list of lists
(define (parse-vec3-prop key props env)
  (let ([item (assoc key props)])
    (unless item (error "Missing property" key "in object spec"))
    (match item
      [`(,key ,x ,y ,z)
       (list (eval-expr x env) (eval-expr y env) (eval-expr z env))]
      [_ (error "Invalid vector property ~a" key)])))

;; parse-material-spec: expr like '(diffuse (color x y z)) etc.
(define (parse-material-spec sexpr env)
  (match sexpr
    [`(diffuse (color ,r ,g ,b))
     (Material 'diffuse 
               (Vec3 (eval-expr r env) (eval-expr g env) (eval-expr b env)) 
               #f #f #f)]
    [`(metal (albedo ,r ,g ,b) (fuzz ,f))
     (Material 'metal 
               #f
               (Vec3 (eval-expr r env) (eval-expr g env) (eval-expr b env))
               (eval-expr f env)
               #f)]
    [`(dielectric (ref_idx ,ri))
     (Material 'dielectric #f #f #f (eval-expr ri env))]
    [`(emissive (color ,r ,g ,b))
     (Material 'emissive
               (Vec3 (eval-expr r env) (eval-expr g env) (eval-expr b env))
               #f #f #f)]
    [else (error "Unknown material spec:" sexpr)]))

;; parse-light-spec: '(point-light (pos ...) (intensity ...)) etc.
(define (parse-light-spec sexpr env)
  (match sexpr
    [`(point-light (pos ,x ,y ,z) (intensity ,r ,g ,b))
     (PointLight (Vec3 (eval-expr x env) (eval-expr y env) (eval-expr z env))
                 (Vec3 (eval-expr r env) (eval-expr g env) (eval-expr b env)))]
    [`(directional-light (dir ,x ,y ,z) (intensity ,r ,g ,b))
     (DirectionalLight (vec-normalize (Vec3 (eval-expr x env) (eval-expr y env) (eval-expr z env)))
                       (Vec3 (eval-expr r env) (eval-expr g env) (eval-expr b env)))]
    [`(ambient-light (intensity ,r ,g ,b))
     (AmbientLight (Vec3 (eval-expr r env) (eval-expr g env) (eval-expr b env)))]
    [`(spot-light (pos ,px ,py ,pz) (dir ,dx ,dy ,dz) (angle ,ang) (intensity ,r ,g ,b))
     (SpotLight (Vec3 (eval-expr px env) (eval-expr py env) (eval-expr pz env))
                (vec-normalize (Vec3 (eval-expr dx env) (eval-expr dy env) (eval-expr dz env)))
                (eval-expr ang env)
                (Vec3 (eval-expr r env) (eval-expr g env) (eval-expr b env)))]
    [else (error "Unknown light spec:" sexpr)]))

;; parse-transform-spec: '(translate x y z)', '(scale x y z)', '(rotate-axis-angle x y z angle)', '(set-transform (matrix m1 ... m16))', '(reset-transform)', '(compose ...)'
(define (parse-transform-spec sexpr env)
  (displayln (format "DEBUG: parse-transform-spec input = ~a" sexpr)) ; Debug input
  (let ([result
         (match sexpr
           [`(translate ,x ,y ,z)
            (make-translation-mat4 (eval-expr x env) (eval-expr y env) (eval-expr z env))]
           [`(scale ,sx ,sy ,sz)
            (make-scale-mat4 (eval-expr sx env) (eval-expr sy env) (eval-expr sz env))]
           [`(rotate-axis-angle ,ax ,ay ,az ,angle)
            (make-rotation-mat4 (eval-expr ax env) (eval-expr ay env) (eval-expr az env) (eval-expr angle env))]
           [`(set-transform (matrix ,@ms))
            (let ([vals (map (λ (e) (eval-expr e env)) ms)])
              (unless (= (length vals) 16) (error "set-transform matrix requires 16 numbers"))
              (Mat4 (list->vector vals)))]
           [`(reset-transform) mat4-identity]
           [`(compose ,@subs)
            (foldl (λ (acc t) (mat4-mul acc (parse-transform-spec t env))) mat4-identity subs)]
           [_ (error "Unknown transform spec:" sexpr)])])
    (displayln (format "DEBUG: parse-transform-spec output = ~a" result)) ; Debug output
    result))

;; parse-render-opts: list of S-expr: '(width w)', '(height h)', '(samples n)', '(max-depth d)', '(filename "f")'
(define (parse-render-opts opts env)
  (define m (make-hash))
  (for ([opt opts])
    (match opt
      [`(width ,w) (hash-set! m 'width (eval-expr w env))]
      [`(height ,h) (hash-set! m 'height (eval-expr h env))]
      [`(samples ,s) (hash-set! m 'samples (eval-expr s env))]
      [`(max-depth ,d) (hash-set! m 'max-depth (eval-expr d env))]
      [`(filename ,fexpr)
       (let ([f (eval-expr fexpr env)])
         (unless (string? f) (error "render filename must be string"))
         (hash-set! m 'filename f))]
      [_ (error "Unknown render option:" opt)]))
  m)

;; parse-background-spec: '(color r g b)' or '(gradient (color ...) (color ...))'
(define (parse-background-spec sexpr env)
  (match sexpr
    [`(color ,r ,g ,b)
     `(color ,(eval-expr r env) ,(eval-expr g env) ,(eval-expr b env))]
    [`(gradient (color ,r1 ,g1 ,b1) (color ,r2 ,g2 ,b2))
     `(gradient (color ,(eval-expr r1 env) ,(eval-expr g1 env) ,(eval-expr b1 env))
                (color ,(eval-expr r2 env) ,(eval-expr g2 env) ,(eval-expr b2 env)))]
    [_ (error "Unknown background spec:" sexpr)]))

;; object-transform-get and set: for Sphere, Plane, Box, Triangle
(define (object-transform-get obj)
  (cond
    [(Sphere? obj) (Sphere-transform obj)]
    [(Plane? obj) (Plane-transform obj)]
    [(Box? obj) (Box-transform obj)]
    [(Triangle? obj) (Triangle-transform obj)]
    [else mat4-identity]))
(define (object-transform-set! obj M)
  (cond
    [(Sphere? obj) (set-Sphere-transform! obj M)]
    [(Plane? obj) (set-Plane-transform! obj M)]
    [(Box? obj) (set-Box-transform! obj M)]
    [(Triangle? obj) (set-Triangle-transform! obj M)]
    [else (void)]))

;; object-summary
(define (object-summary obj)
  (cond
    [(Sphere? obj)
      (format "type=sphere, center=~a, radius=~a, material=~a, \n\ttransform=~a"
              (Sphere-center obj) (Sphere-radius obj) (Material-type (Sphere-material obj)) (Sphere-transform obj))]
    [(Plane? obj)
      (format "type=plane, normal=~a, dist=~a, material=~a, \n\ttransform=~a"
              (Plane-normal obj) (Plane-dist obj) (Material-type (Plane-material obj)) (Plane-transform obj))]
    [(Box? obj)
      (format "type=box, min=~a, max=~a, material=~a, \n\ttransform=~a"
              (Box-min obj) (Box-max obj) (Material-type (Box-material obj)) (Box-transform obj))]
    [(Triangle? obj)
      (format "type=triangle, p1=~a, p2=~a, p3=~a, material=~a, \n\ttransform=~a"
              (Triangle-p1 obj) (Triangle-p2 obj) (Triangle-p3 obj) (Material-type (Triangle-material obj)) (Triangle-transform obj))]
    [else (format "unknown object type")]))

;; light-summary
(define (light-summary light)
  (cond
    [(PointLight? light)
     (format "point-light, pos=~a, intensity=~a" (PointLight-pos light) (PointLight-intensity light))]
    [(DirectionalLight? light)
     (format "directional-light, dir=~a, intensity=~a" (DirectionalLight-dir light) (DirectionalLight-intensity light))]
    [(AmbientLight? light)
     (format "ambient-light, intensity=~a" (AmbientLight-intensity light))]
    [(SpotLight? light)
     (format "spot-light, pos=~a, dir=~a, angle=~a, intensity=~a"
             (SpotLight-pos light) (SpotLight-dir light) (SpotLight-angle light) (SpotLight-intensity light))]
    [else "unknown light type"]))

;; apply-object-update!: 修改 object 根据 update-spec '(center x y z)' 或 '(material ...)'
(define (apply-object-update! obj update-spec env)
  (match update-spec
    [`(center ,x ,y ,z)
     (unless (Sphere? obj) (error "update-object: center only for sphere"))
     (set-Sphere-center! obj (Vec3 (eval-expr x env) (eval-expr y env) (eval-expr z env)))]
    [`(radius ,r)
     (unless (Sphere? obj) (error "update-object: radius only for sphere"))
     (set-Sphere-radius! obj (eval-expr r env))]
    [`(normal ,x ,y ,z)
     (unless (Plane? obj) (error "update-object: normal only for plane"))
     (set-Plane-normal! obj (Vec3 (eval-expr x env) (eval-expr y env) (eval-expr z env)))]
    [`(dist ,d)
     (unless (Plane? obj) (error "update-object: dist only for plane"))
     (set-Plane-dist! obj (eval-expr d env))]
    [`(min ,x ,y ,z)
     (unless (Box? obj) (error "update-object: min only for box"))
     (set-Box-min! obj (Vec3 (eval-expr x env) (eval-expr y env) (eval-expr z env)))]
    [`(max ,x ,y ,z)
     (unless (Box? obj) (error "update-object: max only for box"))
     (set-Box-max! obj (Vec3 (eval-expr x env) (eval-expr y env) (eval-expr z env)))]
    [`(material . ,mat-spec)
     (let ([mat (parse-material-spec mat-spec env)])
       (cond
         [(Sphere? obj) (set-Sphere-material! obj mat)]
         [(Plane? obj) (set-Plane-material! obj mat)]
         [(Box? obj) (set-Box-material! obj mat)]
         [(Triangle? obj) (set-Triangle-material! obj mat)]
         [else (error "update-object: material not supported for this object")]))]
    [else (error "Unknown update-object spec:" update-spec)]))

;; apply-light-update!: 修改 light
(define (apply-light-update! light update-spec env)
  (match update-spec
    [`(pos ,x ,y ,z)
     (unless (or (PointLight? light) (SpotLight? light)) (error "update-light: pos only for point/spot light"))
     (if (PointLight? light)
         (set-PointLight-pos! light (Vec3 (eval-expr x env) (eval-expr y env) (eval-expr z env)))
         (set-SpotLight-pos! light (Vec3 (eval-expr x env) (eval-expr y env) (eval-expr z env))))]
    [`(intensity ,r ,g ,b)
     (let ([v (Vec3 (eval-expr r env) (eval-expr g env) (eval-expr b env))])
       (cond
         [(PointLight? light) (set-PointLight-intensity! light v)]
         [(DirectionalLight? light) (set-DirectionalLight-intensity! light v)]
         [(AmbientLight? light) (set-AmbientLight-intensity! light v)]
         [(SpotLight? light) (set-SpotLight-intensity! light v)]
         [else (error "update-light: intensity not supported for this light")]))]
    [`(dir ,x ,y ,z)
     (unless (or (DirectionalLight? light) (SpotLight? light)) (error "update-light: dir only for directional/spot light"))
     (let ([v (vec-normalize (Vec3 (eval-expr x env) (eval-expr y env) (eval-expr z env)))])
       (if (DirectionalLight? light)
           (set-DirectionalLight-dir! light v)
           (set-SpotLight-dir! light v)))]
    [`(angle ,ang)
     (unless (SpotLight? light) (error "update-light: angle only for spot light"))
     (set-SpotLight-angle! light (eval-expr ang env))]
    [else (error "Unknown update-light spec:" update-spec)]))

;; parse JSON <-> Scene
;; scene->json: 将 Scene 序列化为 JSON-able Racket 对象: hash, lists, symbols -> strings
(define (vec3->list v) (list (Vec3-x v) (Vec3-y v) (Vec3-z v)))
(define (mat4->list m) (let ([v (Mat4 m)]) (vector->list v)))

(define (scene->json scene)
  ;; 返回一 Racket hash 表示 JSON，字段: camera, objects, lights, background, settings, counter
  (let* ([cam (Scene-camera scene)]
         [cam-hash (hash 'pos (vec3->list (Camera-pos cam))
                         'look-at (vec3->list (Camera-look-at cam))
                         'up (vec3->list (Camera-up cam))
                         'fov (Camera-fov cam)
                         'width (Camera-width cam)
                         'height (Camera-height cam))]
         [objs (for/list ([kv (in-hash (Scene-objects scene))])
                 (let ([name (symbol->string (car kv))] [obj (cdr kv)])
                   (match obj
                     [(Sphere center radius material transform)
                      (hash 'type "sphere"
                            'name name
                            'center (vec3->list center)
                            'radius radius
                            'material (material->json material)
                            'transform (mat4->list transform))]
                     [(Plane normal dist material transform)
                      (hash 'type "plane"
                            'name name
                            'normal (vec3->list normal)
                            'dist dist
                            'material (material->json material)
                            'transform (mat4->list transform))]
                     [(Box vmin vmax material transform)
                      (hash 'type "box"
                            'name name
                            'min (vec3->list vmin)
                            'max (vec3->list vmax)
                            'material (material->json material)
                            'transform (mat4->list transform))]
                     [(Triangle p1 p2 p3 material transform)
                      (hash 'type "triangle"
                            'name name
                            'p1 (vec3->list p1)
                            'p2 (vec3->list p2)
                            'p3 (vec3->list p3)
                            'material (material->json material)
                            'transform (mat4->list transform))]
                     [else (error "scene->json: unknown object type" obj)])))]
         [lights (for/list ([kv (in-hash (Scene-lights scene))])
                   (let ([name (symbol->string (car kv))] [light (cdr kv)])
                     (match light
                       [(PointLight pos intens)
                        (hash 'type "point-light" 'name name
                              'pos (vec3->list pos)
                              'intensity (vec3->list intens))]
                       [(DirectionalLight dir intens)
                        (hash 'type "directional-light" 'name name
                              'dir (vec3->list dir)
                              'intensity (vec3->list intens))]
                       [(AmbientLight intens)
                        (hash 'type "ambient-light" 'name name
                              'intensity (vec3->list intens))]
                       [(SpotLight pos dir angle intens)
                        (hash 'type "spot-light" 'name name
                              'pos (vec3->list pos) 'dir (vec3->list dir)
                              'angle angle 'intensity (vec3->list intens))]
                       [else (error "scene->json: unknown light type" light)])))]
         [bg (Scene-background scene)]
         [bg-json (match bg
                    [`(color ,r ,g ,b) (hash 'type "color" 'value (list r g b))]
                    [`(gradient (color ,r1 ,g1 ,b1) (color ,r2 ,g2 ,b2))
                     (hash 'type "gradient" 'value (list (list r1 g1 b1) (list r2 g2 b2)))]
                    [else (error "scene->json: unknown background" bg)])]
         [settings (Scene-settings scene)]
         [settings-json (hash 'samples (hash-ref settings 'samples)
                              'max-depth (hash-ref settings 'max-depth))]
         [rc (Scene-counter scene)])
    (hash 'camera cam-hash
          'objects objs
          'lights lights
          'background bg-json
          'settings settings-json
          'counter rc)))

;; material->json
(define (material->json mat)
  (match mat
    [(Material 'diffuse color _ _ _)
     (hash 'type "diffuse" 'color (vec3->list color))]
    [(Material 'metal _ albedo fuzz _)
     (hash 'type "metal" 'albedo (vec3->list albedo) 'fuzz fuzz)]
    [(Material 'dielectric _ _ ref-idx _)
     (hash 'type "dielectric" 'ref_idx ref-idx)]
    [(Material 'emissive color _ _ _)
     (hash 'type "emissive" 'color (vec3->list color))]
    [else (error "material->json: unknown material type" mat)]))

;; json->scene!: 将 Racket JSON 数据填入已有 global-scene，可重置 scene
(define (json->scene! scene j)
  ;; j is a hash with keys 'camera, 'objects, 'lights, 'background, 'settings, 'counter
  (define cam-h (hash-ref j 'camera))
  (define pos-list (hash-ref cam-h 'pos))
  (define look-list (hash-ref cam-h 'look-at))
  (define up-list (hash-ref cam-h 'up))
  (define fov (hash-ref cam-h 'fov))
  (define width (hash-ref cam-h 'width))
  (define height (hash-ref cam-h 'height))
  (let ([C (Scene-camera scene)])
    (set-Camera-pos! C (Vec3 (list-ref pos-list 0) (list-ref pos-list 1) (list-ref pos-list 2)))
    (set-Camera-look-at! C (Vec3 (list-ref look-list 0) (list-ref look-list 1) (list-ref look-list 2)))
    (set-Camera-up! C (Vec3 (list-ref up-list 0) (list-ref up-list 1) (list-ref up-list 2)))
    (set-Camera-fov! C fov)
    (set-Camera-width! C width)
    (set-Camera-height! C height))
  ;; objects
  (hash-clear! (Scene-objects scene))
  (for ([obj-h (in-list (hash-ref j 'objects))])
    (define t (hash-ref obj-h 'type))
    (define name (string->symbol (hash-ref obj-h 'name)))
    (match t
      ["sphere"
       (define center (hash-ref obj-h 'center))
       (define radius (hash-ref obj-h 'radius))
       (define mat-h (hash-ref obj-h 'material))
       (define mat (json->material mat-h))
       (define trans-list (hash-ref obj-h 'transform))
       (define transform (Mat4 (list->vector trans-list)))
       (hash-set! (Scene-objects scene) name
                  (Sphere (Vec3 (list-ref center 0) (list-ref center 1) (list-ref center 2))
                          radius mat transform))]
      ["plane"
       (define normal (hash-ref obj-h 'normal))
       (define dist (hash-ref obj-h 'dist))
       (define mat-h (hash-ref obj-h 'material))
       (define mat (json->material mat-h))
       (define trans-list (hash-ref obj-h 'transform))
       (define transform (Mat4 (list->vector trans-list)))
       (hash-set! (Scene-objects scene) name
                  (Plane (Vec3 (list-ref normal 0) (list-ref normal 1) (list-ref normal 2))
                         dist mat transform))]
      ["box"
       (define vmin (hash-ref obj-h 'min))
       (define vmax (hash-ref obj-h 'max))
       (define mat-h (hash-ref obj-h 'material))
       (define mat (json->material mat-h))
       (define trans-list (hash-ref obj-h 'transform))
       (define transform (Mat4 (list->vector trans-list)))
       (hash-set! (Scene-objects scene) name
                  (Box (Vec3 (list-ref vmin 0) (list-ref vmin 1) (list-ref vmin 2))
                       (Vec3 (list-ref vmax 0) (list-ref vmax 1) (list-ref vmax 2))
                       mat transform))]
      ["triangle"
       (define p1 (hash-ref obj-h 'p1))
       (define p2 (hash-ref obj-h 'p2))
       (define p3 (hash-ref obj-h 'p3))
       (define mat-h (hash-ref obj-h 'material))
       (define mat (json->material mat-h))
       (define trans-list (hash-ref obj-h 'transform))
       (define transform (Mat4 (list->vector trans-list)))
       (hash-set! (Scene-objects scene) name
                  (Triangle (Vec3 (list-ref p1 0) (list-ref p1 1) (list-ref p1 2))
                            (Vec3 (list-ref p2 0) (list-ref p2 1) (list-ref p2 2))
                            (Vec3 (list-ref p3 0) (list-ref p3 1) (list-ref p3 2))
                            mat transform))]
      [else (error "json->scene!: unknown object type" t)]))
  ;; lights
  (hash-clear! (Scene-lights scene))
  (for ([light-h (in-list (hash-ref j 'lights))])
    (define t (hash-ref light-h 'type))
    (define name (string->symbol (hash-ref light-h 'name)))
    (match t
      ["point-light"
       (define pos (hash-ref light-h 'pos))
       (define intens (hash-ref light-h 'intensity))
       (hash-set! (Scene-lights scene) name
                  (PointLight (Vec3 (list-ref pos 0) (list-ref pos 1) (list-ref pos 2))
                              (Vec3 (list-ref intens 0) (list-ref intens 1) (list-ref intens 2))))]
      ["directional-light"
       (define dir (hash-ref light-h 'dir))
       (define intens (hash-ref light-h 'intensity))
       (hash-set! (Scene-lights scene) name
                  (DirectionalLight (vec-normalize (Vec3 (list-ref dir 0) (list-ref dir 1) (list-ref dir 2)))
                                    (Vec3 (list-ref intens 0) (list-ref intens 1) (list-ref intens 2))))]
      ["ambient-light"
       (define intens (hash-ref light-h 'intensity))
       (hash-set! (Scene-lights scene) name
                  (AmbientLight (Vec3 (list-ref intens 0) (list-ref intens 1) (list-ref intens 2))))]
      ["spot-light"
       (define pos (hash-ref light-h 'pos))
       (define dir (hash-ref light-h 'dir))
       (define angle (hash-ref light-h 'angle))
       (define intens (hash-ref light-h 'intensity))
       (hash-set! (Scene-lights scene) name
                  (SpotLight (Vec3 (list-ref pos 0) (list-ref pos 1) (list-ref pos 2))
                             (vec-normalize (Vec3 (list-ref dir 0) (list-ref dir 1) (list-ref dir 2)))
                             angle
                             (Vec3 (list-ref intens 0) (list-ref intens 1) (list-ref intens 2))))]
      [else (error "json->scene!: unknown light type" t)]))
  ;; background
  (define bg-h (hash-ref j 'background))
  (match (hash-ref bg-h 'type)
    ["color"
     (define v (hash-ref bg-h 'value))
     (set-Scene-background! scene `(color ,@(v)))]
    ["gradient"
     (define vs (hash-ref bg-h 'value)) ; list of two lists
     (set-Scene-background! scene
                            `(gradient (color ,@(first vs)) (color ,@(second vs))))]
    [else (error "json->scene!: unknown background type" (hash-ref bg-h 'type))])
  ;; settings
  (define settings-h (hash-ref j 'settings))
  (hash-set! (Scene-settings scene) 'samples (hash-ref settings-h 'samples))
  (hash-set! (Scene-settings scene) 'max-depth (hash-ref settings-h 'max-depth))
  ;; counter
  (set-Scene-counter! scene (hash-ref j 'counter))
  )

;; json->material
(define (json->material mat-h)
  (define t (hash-ref mat-h 'type))
  (match t
    ["diffuse"
     (define c (hash-ref mat-h 'color))
     (Material 'diffuse (Vec3 (list-ref c 0) (list-ref c 1) (list-ref c 2)) #f #f)]
    ["metal"
     (define alb (hash-ref mat-h 'albedo))
     (define fuzz (hash-ref mat-h 'fuzz))
     (Material 'metal #f (Vec3 (list-ref alb 0) (list-ref alb 1) (list-ref alb 2)) fuzz)]
    ["dielectric"
     (define ri (hash-ref mat-h 'ref_idx))
     (Material 'dielectric #f #f ri)]
    ["emissive"
     (define c (hash-ref mat-h 'color))
     (Material 'emissive (Vec3 (list-ref c 0) (list-ref c 1) (list-ref c 2)) #f #f)]
    [else (error "json->material: unknown material type" t)]))

;; ----------------------------------------
;; display-help: 打印 DSL 帮助
;; ----------------------------------------
(define (display-help)
  (displayln "Available commands:")
  (displayln "  (clear-scene)")
  (displayln "  (set-camera (camera (pos x y z) (look-at x y z) (up x y z) (fov deg) (width w) (height h)))")
  (displayln "  (add-object name (sphere (center x y z) (radius r) (material diffuse (color r g b))))")
  (displayln "  (add-object name (plane (normal x y z) (dist d) (material diffuse (color r g b))))")
  (displayln "  (add-object name (box (min x y z) (max x y z) (material ...)))")
  (displayln "  (update-object name (center x y z))")
  (displayln "  (update-object name (radius r))")
  (displayln "  (update-object name (material ...))")
  (displayln "  (transform name (translate x y z))")
  (displayln "  (transform name (rotate-axis-angle x y z angle))")
  (displayln "  (transform name (scale x y z))")
  (displayln "  (group group-name obj1 obj2 ...)")
  (displayln "  (ungroup group-name)")
  (displayln "  (list-objects)")
  (displayln "  (list-lights)")
  (displayln "  (list-groups)")
  (displayln "  (add-light name (point-light (pos x y z) (intensity r g b)))")
  (displayln "  (add-light name (directional-light (dir x y z) (intensity r g b)))")
  (displayln "  (update-light name (pos x y z))")
  (displayln "  (update-light name (intensity r g b))")
  (displayln "  (remove-object name)")
  (displayln "  (remove-light name)")
  (displayln "  (remove-group name)")
  (displayln "  (set-background (color r g b))")
  (displayln "  (set-background (gradient (color r1 g1 b1) (color r2 g2 b2)))")
  (displayln "  (set-global (samples n))")
  (displayln "  (set-global (max-depth d))")
  (displayln "  (save-scene \"file.json\")")
  (displayln "  (load-scene \"file.json\")")
  (displayln "  (render (width w)? (height h)? (samples n)? (max-depth d)? (filename \"f.ppm\")?)")
  (displayln "  (let var expr body...)  ; bind var for body")
  (displayln "  (repeat n body...)      ; loop i from 0 to n-1, i bound in env")
  (displayln "  (exit)")
  (displayln "")
  (displayln "In expressions for let/repeat, supported builtins: +, -, *, /, sin, cos, tan, sqrt, abs, number->string, string-append, string-pad-left. Use symbol i in repeat as loop index."))

