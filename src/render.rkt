#lang racket
(require racket/random
         racket/math
         "ast.rkt"
         "utils.rkt")
(provide render-scene-to-ppm)

;; ----------------------------------------
;; 光线结构
(struct Ray (origin dir) #:transparent)
;; origin: Vec3, dir: Vec3 normalized

;; ----------------------------------------
;; 射线生成（Camera）
;; ----------------------------------------
;; generate-ray: 根据 camera 与像素 (i,j) 及随机偏移，生成抗锯齿采样射线
;; jitter-u, jitter-v: number in [0,1), for subpixel采样；如果 samples=1，可传 0.5,0.5
(define (generate-ray camera i j width height jitter-u jitter-v)
  ;; camera fields
  (let* ([pos (Camera-pos camera)]
         [look (Camera-look-at camera)]
         [up (Camera-up camera)]
         [fov (Camera-fov camera)]
         ;; 1. camera 坐标系：w_dir 从 pos 指向 look? 通常 w = normalize(pos - look)
         [w-dir (vec-normalize (vec-sub pos look))]
         [u-dir (vec-normalize (vec-cross up w-dir))]
         [v-dir (vec-cross w-dir u-dir)]
         [aspect (/ width height)]
         [theta (* pi (/ fov 180))]
         [half-height (tan (/ theta 2))]
         [half-width (* aspect half-height)]
         ;; 像素位置到 NDC: jitter 用于抗锯齿
         [u (+ (* (- (* 2 (/ (+ i jitter-u) width)) 1) half-width))]
         [v (+ (* (- 1 (* 2 (/ (+ j jitter-v) height))) half-height))]
         ;; 图像平面中心: pos - w_dir
         [lower-left (vec-sub (vec-sub (vec-sub pos (vec-scale u-dir half-width))
                                       (vec-scale v-dir half-height))
                              w-dir)]
         [pixel-point (vec-add (vec-add lower-left (vec-scale u-dir u))
                               (vec-scale v-dir v))]
         [dir (vec-normalize (vec-sub pixel-point pos))])
    (Ray pos dir)))

;; ----------------------------------------
;; 相交测试：Sphere, Plane, Box
;; ----------------------------------------
;; transform ray to local space of object: 使用 object.transform 的逆矩阵
(define (transform-ray-to-local ray transform)
  (let* ([inv (mat4-inverse transform)]
         [o (Ray-origin ray)]
         [d (Ray-dir ray)]
         [o-loc (mat4-transform-point inv o)]
         [d-loc (mat4-transform-dir inv d)]
         [d-norm (vec-normalize d-loc)])
    (Ray o-loc d-norm)))

;; intersect sphere in local coordinates; 返回 HitInfo 或 #f
(define (intersect-ray-sphere ray sphere)
  (let* ([radius (Sphere-radius sphere)]
         [center (Sphere-center sphere)]
         [oc (vec-sub (Ray-origin ray) center)]
         [a (vec-dot (Ray-dir ray) (Ray-dir ray))]
         [b (* 2 (vec-dot oc (Ray-dir ray)))]
         [c (- (vec-dot oc oc) (* radius radius))]
         [disc (- (* b b) (* 4 a c))])
    (if (< disc 0)
        #f
        (let* ([sqrt-d (sqrt disc)]
               [t1 (/ (+ (- b) (- sqrt-d)) (* 2 a))]
               [t2 (/ (+ (- b) sqrt-d) (* 2 a))]
               ;; 选最小正 t
               [t (cond [(and (> t1 1e-4) (< t1 t2)) t1]
                        [(> t2 1e-4) t2]
                        [else #f])])
          (if (not t) #f
              ;; world-space: 先在 local 计算 point, normal; 然后 transform 回 world
              (let* ([p-loc (vec-add (Ray-origin ray) (vec-scale (Ray-dir ray) t))]
                     [n-loc (vec-normalize (vec-sub p-loc center))]
                     ;; transform back:
                     [transform (Sphere-transform sphere)]
                     [p-world (if transform (mat4-transform-point transform p-loc) p-loc)]
                     ;; normal-world: 使用逆转置:
                     [n-world (if transform
                                  (let* ([inv (mat4-inverse transform)]
                                         ;; 对 normal 作为向量应用逆转置：即 (transpose inv) * normal
                                         [m (Mat4-m inv)]
                                         ;; compute transpose(inv) * n-loc
                                         [nx (+ (* (vector-ref m 0) (Vec3-x n-loc))
                                                (* (vector-ref m 4) (Vec3-y n-loc))
                                                (* (vector-ref m 8) (Vec3-z n-loc)))]
                                         [ny (+ (* (vector-ref m 1) (Vec3-x n-loc))
                                                (* (vector-ref m 5) (Vec3-y n-loc))
                                                (* (vector-ref m 9) (Vec3-z n-loc)))]
                                         [nz (+ (* (vector-ref m 2) (Vec3-x n-loc))
                                                (* (vector-ref m 6) (Vec3-y n-loc))
                                                (* (vector-ref m 10) (Vec3-z n-loc)))])
                                    (vec-normalize (Vec3 nx ny nz)))
                                  n-loc)])
                (HitInfo t p-world n-world (Sphere-material sphere))))))))

;; intersect plane in local: plane equation n·P + dist = 0
(define (intersect-ray-plane ray plane)
  (let* ([normal (Plane-normal plane)]
         [dist (Plane-dist plane)]
         ;; Ray-origin and dir in local? But transform: first transform ray to local
         [den (vec-dot normal (Ray-dir ray))])
    (if (zero? den)
        #f
        (let* ([t (/ (- (vec-dot normal (Ray-origin ray)) dist) den)]) ; careful: want solve n·(O + tD) + dist =0? Depending sign; assume plane stored as n·P = dist
          (if (<= t 1e-4) #f
              (let* ([p-loc (vec-add (Ray-origin ray) (vec-scale (Ray-dir ray) t))]
                     ;; normal in local is constant; transform to world:
                     [transform (Plane-transform plane)]
                     [p-world (if transform (mat4-transform-point transform p-loc) p-loc)]
                     [n-world (if transform
                                  (let* ([inv (mat4-inverse transform)]
                                         [m (Mat4-m inv)]
                                         [nx (+ (* (vector-ref m 0) (Vec3-x normal))
                                                (* (vector-ref m 4) (Vec3-y normal))
                                                (* (vector-ref m 8) (Vec3-z normal)))]
                                         [ny (+ (* (vector-ref m 1) (Vec3-x normal))
                                                (* (vector-ref m 5) (Vec3-y normal))
                                                (* (vector-ref m 9) (Vec3-z normal)))]
                                         [nz (+ (* (vector-ref m 2) (Vec3-x normal))
                                                (* (vector-ref m 6) (Vec3-y normal))
                                                (* (vector-ref m 10) (Vec3-z normal)))])
                                    (vec-normalize (Vec3 nx ny nz)))
                                  normal)])
                (HitInfo t p-world n-world (Plane-material plane))))))))

;; intersect-ray-box: 在 local 空间中，使用 slab 法求 axis-aligned box 相交
;; 返回 HitInfo 或 #f
(define (intersect-ray-box ray box)
  (let* ([bmin (Box-min box)]
         [bmax (Box-max box)]
         [orig (Ray-origin ray)]
         [dir  (Ray-dir ray)])
    ;; 使用 for/fold 计算每个轴的 tmin, tmax 累积
    (let-values ([(tmin tmax)
                  (for/fold ([tmin -inf.0] [tmax +inf.0]) ([axis '(x y z)])
                    (let* ([o    (case axis
                                  [(x) (Vec3-x orig)]
                                  [(y) (Vec3-y orig)]
                                  [(z) (Vec3-z orig)])]
                           [d    (case axis
                                  [(x) (Vec3-x dir)]
                                  [(y) (Vec3-y dir)]
                                  [(z) (Vec3-z dir)])]
                           [amin (case axis
                                  [(x) (Vec3-x bmin)]
                                  [(y) (Vec3-y bmin)]
                                  [(z) (Vec3-z bmin)])]
                           [amax (case axis
                                  [(x) (Vec3-x bmax)]
                                  [(y) (Vec3-y bmax)]
                                  [(z) (Vec3-z bmax)])])
                      (if (zero? d)
                          ;; 平行于该轴面
                          (if (or (< o amin) (> o amax))
                              ;; 射线在该轴方向与 slab 无交集
                              (values +inf.0 -inf.0)
                              ;; 平行且在 slab 内，不改变 tmin,tmax
                              (values tmin tmax))
                          ;; 非平行，计算切片交点
                          (let* ([t1 (/ (- amin o) d)]
                                 [t2 (/ (- amax o) d)]
                                 [ta (min t1 t2)]
                                 [tb (max t1 t2)])
                            (values (max tmin ta) (min tmax tb))))))])
      ;; 现在得到了 tmin, tmax
      ;; 判断是否有正向交点：若 tmin>ε 则用 tmin；否则若 tmax>ε 则用 tmax；否则无有效交点
      (let ([valid-t
             (cond
               [(> tmin 1e-4) tmin]
               [(> tmax 1e-4) tmax]
               [else #f])])
        (if (and valid-t (<= tmin tmax))
            ;; 有交点，计算交点位置和法线
            (let* ([t      valid-t]
                   ;; 交点在 local 空间
                   [p-loc  (vec-add (Ray-origin ray)
                                    (vec-scale (Ray-dir ray) t))]
                   [eps    1e-6]
                   [nx     0]
                   [ny     0]
                   [nz     0])
              ;; 判断交点属于哪一面：比较 p-loc 与 bmin/bmax 的距离
              (cond
                [(<= (abs (- (Vec3-x p-loc) (Vec3-x bmin))) eps) (set! nx -1)]
                [(<= (abs (- (Vec3-x p-loc) (Vec3-x bmax))) eps) (set! nx 1)]
                [(<= (abs (- (Vec3-y p-loc) (Vec3-y bmin))) eps) (set! ny -1)]
                [(<= (abs (- (Vec3-y p-loc) (Vec3-y bmax))) eps) (set! ny 1)]
                [(<= (abs (- (Vec3-z p-loc) (Vec3-z bmin))) eps) (set! nz -1)]
                [(<= (abs (- (Vec3-z p-loc) (Vec3-z bmax))) eps) (set! nz 1)]
                [else (void)])
              ;; 局部法线
              (let* ([normal-local (Vec3 nx ny nz)]
                     [transform    (Box-transform box)]
                     ;; 变换到 world 空间
                     [p-world (if transform
                                  (mat4-transform-point transform p-loc)
                                  p-loc)]
                     [n-world (if transform
                                  ;; 逆转置矩阵作用于局部法线
                                  (let* ([inv (mat4-inverse transform)]
                                         [m   (Mat4-m inv)]
                                         [nx2 (+ (* (vector-ref m 0) (Vec3-x normal-local))
                                                 (* (vector-ref m 4) (Vec3-y normal-local))
                                                 (* (vector-ref m 8) (Vec3-z normal-local)))]
                                         [ny2 (+ (* (vector-ref m 1) (Vec3-x normal-local))
                                                 (* (vector-ref m 5) (Vec3-y normal-local))
                                                 (* (vector-ref m 9) (Vec3-z normal-local)))]
                                         [nz2 (+ (* (vector-ref m 2) (Vec3-x normal-local))
                                                 (* (vector-ref m 6) (Vec3-y normal-local))
                                                 (* (vector-ref m 10) (Vec3-z normal-local)))])
                                    (vec-normalize (Vec3 nx2 ny2 nz2)))
                                  normal-local)])
                ;; 返回 HitInfo：t, 交点世界位置, 世界法线, 以及材质
                (HitInfo t p-world n-world (Box-material box))))
            ;; 无交点
            #f)))))

;; intersect-ray-triangle: 暂未实现
(define (intersect-ray-triangle ray triangle)
  ;; 使用 Möller–Trumbore 算法或其他方法
  ;; 返回 HitInfo 或 #f
  #f) ; placeholder

;; ----------------------------------------
;; Scene 相交：遍历所有 objects 和 groups
;; ----------------------------------------
;; compute full transform for object: 累积组 transform
;; 为简化，这里忽略 group 层级：在 handle-command 添加 object 时，若在 group 中注册成员时并未自动调整 transform；
;; 为准确，应维护 group 层次并在相交时累积 transforms：实现略，假设 group.transform 已合并到 member.transform。
;; 若需要 group 变换、递归，可在这里实现：对每个 object，若存在 group 包含它，则累积 group transforms.
;; 为本实现简化：在 group command 时，立即更新成员对象的 transform: 当 (group g a b) + (transform g M) 时，将 M 应用于成员对象 transform，并清空 group; 这样避免层级追踪。
;; 以下 assume Sphere-transform 等总是 world-transform。
;; 如果要保留 group 关系并动态变换，需要更复杂管理；此处采取简单即时更新策略：在 handle-command 里，transform group 时，对成员立即更新其 transform，并清除 group 或保持 group 仅作逻辑分组用途。
;; 因此相交仅需遍历 Scene-objects。

(define (find-nearest-intersection ray scene)
  (let ([best-hit #f] [best-t +inf])
    ;; 遍历 objects
    (for ([(name obj) (in-hash (Scene-objects scene))])
      (let* (;; transform ray to local
             [local-ray (cond [(Sphere? obj) (transform-ray-to-local ray (Sphere-transform obj))]
                              [(Plane? obj) (transform-ray-to-local ray (Plane-transform obj))]
                              [(Box? obj) (transform-ray-to-local ray (Box-transform obj))]
                              [(Triangle? obj) (transform-ray-to-local ray (Triangle-transform obj))]
                              [else ray])]
             [hit (cond [(Sphere? obj) (intersect-ray-sphere local-ray obj)]
                        [(Plane? obj) (intersect-ray-plane local-ray obj)]
                        [(Box? obj) (intersect-ray-box local-ray obj)]
                        [(Triangle? obj) (intersect-ray-triangle local-ray obj)]
                        [else #f])])
        ;; 输出 ray 和当前 object 以及是否命中
        ; (printf "Ray: ~a, \nObject: ~a, \nHit?: ~a~%\n\n" ray obj (not (not hit)))
        (when hit
          (let ([t (HitInfo-t hit)])
            (when (< t best-t)
              (set! best-t t)
              (set! best-hit hit))))))
    best-hit))

;; ----------------------------------------
;; 光照计算与 trace-ray
;; ----------------------------------------
;; reflect: 反射向量 R = D - 2*(D·N)*N
(define (reflect v n)
  (vec-sub v (vec-scale n (* 2 (vec-dot v n)))))

;; refract: 计算折射向量，输入 v 入射方向（已归一，指向表面? 需要约定），n 法线，ni_over_nt: 比率
;; 返回向量或 #f
(define (refract v n ni_over_nt)
  (let* ([uv (vec-normalize v)]
         [dt (vec-dot uv n)]
         [discriminant (- 1 (* ni_over_nt ni_over_nt) (+ (* dt dt) (* - (* ni_over_nt ni_over_nt) (* dt dt))))])
    ;; Actually use formula: discriminant = 1 - (ni_over_nt^2)*(1 - dt^2)
    (define disc (- 1 (* ni_over_nt ni_over_nt) (* -1 (* ni_over_nt ni_over_nt) (* dt dt)))) ; simplified wrong
    ;; Better formula:
    (define disc2 (- 1 (* ni_over_nt ni_over_nt) (* (- 1 (* dt dt)) ni_over_nt ni_over_nt)))
    ;; But correct: discriminant = 1 - ni_over_nt^2 * (1 - dt^2)
    (define disc3 (- 1 (* ni_over_nt ni_over_nt) (* (- 1 (* dt dt)) (* ni_over_nt ni_over_nt))))
    ;; But we do:
    (define discriminant-correct (- 1 (* ni_over_nt ni_over_nt) (* (- 1 (* dt dt)) ni_over_nt ni_over_nt)))
    (if (< discriminant-correct 0)
        #f
        (let ([refr (vec-sub (vec-scale (vec-sub uv (vec-scale n dt)) ni_over_nt)
                             (vec-scale n (sqrt discriminant-correct)))])
          refr))))

;; schlick 近似
(define (schlick cosine ref_idx)
  (let ([r0 (expt (/ (- 1 ref_idx) (+ 1 ref_idx)) 2)])
    (+ r0 (* (- 1 r0) (expt (- 1 cosine) 5)))))

;; compute background color: 若背景纯色或渐变
(define (background-color scene ray)
  (match (Scene-background scene)
    [`(color ,r ,g ,b) (Vec3 r g b)]
    [`(gradient (color ,r1 ,g1 ,b1) (color ,r2 ,g2 ,b2))
     (let* ([camera (Scene-camera scene)]
            [up (vec-normalize (Camera-up camera))]
            [dir (vec-normalize (Ray-dir ray))]
            [dot-up (vec-dot dir up)]
            [t (clamp (* 0.5 (+ 1 dot-up)) 0 1)]
            [c1 (Vec3 r1 g1 b1)]
            [c2 (Vec3 r2 g2 b2)]
            [c (vec-add (vec-scale c1 (- 1 t)) (vec-scale c2 t))])
       c)]
    [else ; 未知设置，返回黑
     (Vec3 0 0 0)]))

;; offset point 避免自交：p + epsilon * normal
(define (offset-point p n)
  (vec-add p (vec-scale n 1e-4)))

;; trace-ray: 返回 Vec3 color，depth 控制递归
(define (trace-ray ray scene depth)
  (if (>= depth (hash-ref (Scene-settings scene) 'max-depth))
      (background-color scene ray)
      (let ([hit (find-nearest-intersection ray scene)])
        (if (not hit)
            (background-color scene ray)
            (let* ([p (HitInfo-point hit)]
                   [n (HitInfo-normal hit)]
                   [mat (HitInfo-material hit)]
                   [col (Vec3 0 0 0)]
                   [emit (if (eq? (Material-type mat) 'emissive)
                             (Material-color mat)
                             (Vec3 0 0 0))])
              ;; 如果 emissive，直接返回 emission，不再考虑其他光照。（可根据需求调整）
              (if (eq? (Material-type mat) 'emissive)
                  emit
                  (begin
                    ;; 1. ambient light contribution
                    (for ([(lname light) (in-hash (Scene-lights scene))])
                      (when (AmbientLight? light)
                        (let* ([int (AmbientLight-intensity light)]
                               [diffuse-col (if (eq? (Material-type mat) 'diffuse)
                                                (Material-color mat)
                                                (if (eq? (Material-type mat) 'metal)
                                                    (Material-albedo mat)
                                                    (Vec3 0 0 0)))]
                               [c-part (Vec3 (* (Vec3-x diffuse-col) (Vec3-x int))
                                            (* (Vec3-y diffuse-col) (Vec3-y int))
                                            (* (Vec3-z diffuse-col) (Vec3-z int)))])
                          (set! col (vec-add col c-part)))))
                    ;; 2. other lights: point, directional, spot
                    (for ([(lname light) (in-hash (Scene-lights scene))])
                      (cond
                          [(PointLight? light)
                           (let* ([to-light (vec-sub (PointLight-pos light) p)]
                                  [dist2 (vec-dot to-light to-light)]
                                  [L (vec-normalize to-light)]
                                  [shadow-ray (Ray (offset-point p n) L)]
                                  [in-shadow? (let ([hit2 (find-nearest-intersection shadow-ray scene)])
                                                (and hit2
                                                     (< (HitInfo-t hit2) (sqrt dist2))))]
                                  [dotNL (max 0 (vec-dot n L))])
                             (unless in-shadow?
                               (let ([intens (PointLight-intensity light)]
                                     [diffuse-col (if (eq? (Material-type mat) 'diffuse)
                                                      (Material-color mat)
                                                      (if (eq? (Material-type mat) 'metal)
                                                          (Material-albedo mat)
                                                          (Vec3 0 0 0)))])
                                 (let ([atten (/ 1 dist2)])
                                   (let ([c-part (Vec3 (* (Vec3-x diffuse-col) (Vec3-x intens) dotNL atten)
                                                      (* (Vec3-y diffuse-col) (Vec3-y intens) dotNL atten)
                                                      (* (Vec3-z diffuse-col) (Vec3-z intens) dotNL atten))])
                                     (set! col (vec-add col c-part)))))))]
                          [(DirectionalLight? light)
                           (let* ([dirL (vec-normalize (DirectionalLight-dir light))]
                                  [shadow-ray (Ray (offset-point p n) (vec-scale dirL -1))] ; from point toward light (light at infinity)
                                  [in-shadow? (find-nearest-intersection shadow-ray scene)] ; any hit blocks
                                  [dotNL (max 0 (vec-dot n (vec-scale dirL -1)))])
                             (unless in-shadow?
                               (let ([intens (DirectionalLight-intensity light)]
                                     [diffuse-col (if (eq? (Material-type mat) 'diffuse)
                                                      (Material-color mat)
                                                      (if (eq? (Material-type mat) 'metal)
                                                          (Material-albedo mat)
                                                          (Vec3 0 0 0)))])
                                 (let ([c-part (Vec3 (* (Vec3-x diffuse-col) (Vec3-x intens) dotNL)
                                                    (* (Vec3-y diffuse-col) (Vec3-y intens) dotNL)
                                                    (* (Vec3-z diffuse-col) (Vec3-z intens) dotNL))])
                                   (set! col (vec-add col c-part))))))]
                          [(SpotLight? light)
                           (let* ([to-light (vec-sub (SpotLight-pos light) p)]
                                  [dist2 (vec-dot to-light to-light)]
                                  [L (vec-normalize to-light)]
                                  [angle-to (acos (max -1 (min 1 (vec-dot (vec-scale L -1) (vec-normalize (SpotLight-dir light))))))] ; angle between direction and ray
                                  [angle-rad (* pi (/ (SpotLight-angle light) 180))]
                                  [in-cone? (<= angle-to (/ angle-rad 2))]
                                  [shadow-ray (Ray (offset-point p n) L)]
                                  [in-shadow? (and in-cone?
                                                  (let ([hit2 (find-nearest-intersection shadow-ray scene)])
                                                    (and hit2 (< (HitInfo-t hit2) (sqrt dist2)))))]
                                  [dotNL (max 0 (vec-dot n L))])
                             (when (and in-cone? (not in-shadow?))
                               (let ([intens (SpotLight-intensity light)]
                                     [diffuse-col (if (eq? (Material-type mat) 'diffuse)
                                                      (Material-color mat)
                                                      (if (eq? (Material-type mat) 'metal)
                                                          (Material-albedo mat)
                                                          (Vec3 0 0 0)))])
                                 (let ([atten (/ 1 dist2)])
                                   (let ([c-part (Vec3 (* (Vec3-x diffuse-col) (Vec3-x intens) dotNL atten)
                                                      (* (Vec3-y diffuse-col) (Vec3-y intens) dotNL atten)
                                                      (* (Vec3-z diffuse-col) (Vec3-z intens) dotNL atten))])
                                     (set! col (vec-add col c-part)))))))]
                          [else (void)]))
                    ;; 3. 反射/折射
                    (cond
                      [(eq? (Material-type mat) 'metal)
                       ;; 镜面反射，带 fuzz：反射方向 + 随机扰动
                       (let* ([albedo (Material-albedo mat)]
                              [fuzz (Material-fuzz mat)]
                              [ref-dir (reflect (Ray-dir ray) n)]
                              [rand-v (vec-sub (vec-scale (Vec3 (random) (random) (random)) 2) (Vec3 1 1 1))] ; 随机向量 in [-1,1]
                              [scattered-dir (vec-normalize (vec-add ref-dir (vec-scale rand-v fuzz)))]
                              [scattered-ray (Ray (offset-point p n) scattered-dir)])
                         ;; 递归
                         (let ([rcol (trace-ray scattered-ray scene (add1 depth))])
                           (set! col (vec-add col (vec-scale rcol (Vec3-x albedo)))) ; approximate: use albedo.x for all? Better multiply componentwise:
                           (set! col (Vec3 (* (Vec3-x col) (Vec3-x albedo))
                                          (* (Vec3-y col) (Vec3-y albedo))
                                          (* (Vec3-z col) (Vec3-z albedo)))))
                       col)]
                      [(eq? (Material-type mat) 'dielectric)
                       ;; 折射或反射，根据 Schlick
                       (let* ([ref-idx (Material-ref-idx mat)]
                              [etai-over-etat (if (< (vec-dot (Ray-dir ray) n) 0) ; outside
                                                 (/ 1 ref-idx)
                                                 ref-idx)]
                              [unit-dir (vec-normalize (Ray-dir ray))]
                              [cosine (abs (vec-dot unit-dir n))]
                              [refr-opt (let ([r (refract unit-dir (if (< (vec-dot unit-dir n) 0) n (vec-scale n -1)) etai-over-etat)])
                                          r)]
                              [reflect-prob (schlick cosine ref-idx)]
                              [rand (random)])
                         (if (and refr-opt (< rand reflect-prob))
                             (let ([scattered-ray (Ray (offset-point p (if (< (vec-dot unit-dir n) 0) n (vec-scale n -1))) refr-opt)])
                               (trace-ray scattered-ray scene (add1 depth)))
                             ;; 反射
                             (let ([reflected (reflect unit-dir n)]
                                   [scattered-ray (Ray (offset-point p n) (reflect unit-dir n))])
                               (trace-ray scattered-ray scene (add1 depth)))))]
                      [else
                       col])
                    ;; 最终返回 col
                    )))))))

;; ----------------------------------------
;; 渲染主函数
;; ----------------------------------------
;; render-scene-to-ppm: scene: Scene；width,height,samples,max-depth,filename
(define (render-scene-to-ppm scene width height samples max-depth filename)
  ;; 更新全局 settings
  (hash-set! (Scene-settings scene) 'samples samples)
  (hash-set! (Scene-settings scene) 'max-depth max-depth)
  ;; 更新 camera 分辨率
  (set-Camera-width! (Scene-camera scene) width)
  (set-Camera-height! (Scene-camera scene) height)
  ;; 打开文件
  (define out (open-output-file filename #:mode 'text))
  ;; 写 PPM 头 (P3)
  (fprintf out "P3~%~a ~a~%255~%" width height)
  ;; 主循环
  (for ([j (in-range height)])
    (for ([i (in-range width)])
      (let ([acc (Vec3 0 0 0)])
        (for ([s (in-range samples)])
          ;; jitter in [0,1)
          (let ([jitter-u (random)] [jitter-v (random)])
            (let ([ray (generate-ray (Scene-camera scene) i j width height jitter-u jitter-v)])
              ;; Print ray direction
              ; (printf "Ray direction: ~a~%" (Ray-dir ray))
              (let ([col (trace-ray ray scene 0)])
              ; (printf "Color from trace-ray: ~a~%" col)
          (set! acc (vec-add acc col))))))
        ;; average and gamma correction
        (let* ([avg (vec-scale acc (/ 1.0 samples))]
               ;; gamma 校正 γ=2: sqrt
               [r (sqrt (Vec3-x avg))] 
               [g (sqrt (Vec3-y avg))] 
               [b (sqrt (Vec3-z avg))]
               [ir (inexact->exact (exact-round (* 255 (clamp r 0 1))))]
               [ig (inexact->exact (exact-round (* 255 (clamp g 0 1))))]
               [ib (inexact->exact (exact-round (* 255 (clamp b 0 1))))])
          (fprintf out "~a ~a ~a " ir ig ib)))))
    (fprintf out "~%") ; 每行结束换行
  (close-output-port out))