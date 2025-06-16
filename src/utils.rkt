#lang racket
(require racket/math
         racket/dict
         racket/string)
(require "ast.rkt")

;; Define infinity constants
(define +inf +inf.0)
(define -inf -inf.0)

(provide vec3 vec-add vec-sub vec-scale vec-dot vec-length vec-normalize
         vec-cross  ; Add cross product to provides
         mat4-identity mat4-mul mat4-transform-point mat4-transform-dir
         mat4-inverse make-translation-mat4 make-scale-mat4 make-rotation-mat4
         clamp
         generate-filename!
         string-pad-left
         +inf -inf  ; Add infinity constants to provides
         )

;; ----------------------------------------
;; 向量运算
;; ----------------------------------------

;; 构造 Vec3
(define (vec3 x y z) (Vec3 x y z))

(define (vec-add a b)
  (Vec3 (+ (Vec3-x a) (Vec3-x b))
        (+ (Vec3-y a) (Vec3-y b))
        (+ (Vec3-z a) (Vec3-z b))))

(define (vec-sub a b)
  (Vec3 (- (Vec3-x a) (Vec3-x b))
        (- (Vec3-y a) (Vec3-y b))
        (- (Vec3-z a) (Vec3-z b))))

(define (vec-scale a s)
  (Vec3 (* (Vec3-x a) s)
        (* (Vec3-y a) s)
        (* (Vec3-z a) s)))

(define (vec-dot a b)
  (+ (* (Vec3-x a) (Vec3-x b))
     (* (Vec3-y a) (Vec3-y b))
     (* (Vec3-z a) (Vec3-z b))))

(define (vec-length a)
  (sqrt (vec-dot a a)))

(define (vec-normalize a)
  (let ([len (vec-length a)])
    (if (zero? len) a (vec-scale a (/ 1 len)))))

;; Vector cross product
(define (vec-cross a b)
  (Vec3 (- (* (Vec3-y a) (Vec3-z b))
           (* (Vec3-z a) (Vec3-y b)))
        (- (* (Vec3-z a) (Vec3-x b))
           (* (Vec3-x a) (Vec3-z b)))
        (- (* (Vec3-x a) (Vec3-y b))
           (* (Vec3-y a) (Vec3-x b)))))

;; clamp between min and max
(define (clamp x lo hi)
  (cond [(< x lo) lo]
        [(> x hi) hi]
        [else x]))

;; ----------------------------------------
;; 矩阵运算
;; ----------------------------------------
;; Mat4 from ast.rkt is struct Mat4 holding a vector-of 16 numbers row-major.
;; 定义 identity
(define mat4-identity
  (Mat4 (vector 
         1 0 0 0
         0 1 0 0
         0 0 1 0
         0 0 0 1)))

;; 矩阵乘法: a * b
(define (mat4-mul a b)
  (let* ([av (Mat4-m a)]
         [bv (Mat4-m b)]
         [rv (make-vector 16 0)])
    ;; row i, col j: sum over k: a[i,k] * b[k,j]
    (for ([i (in-range 4)] [j (in-range 4)])
      (let ([sum (for/fold ([s 0]) ([k (in-range 4)]) 
                   (+ s (* (vector-ref av (+ (* i 4) k))
                          (vector-ref bv (+ (* k 4) j)))))])
        (vector-set! rv (+ (* i 4) j) sum)))
    (Mat4 rv)))

;; transform-point: 对 Vec3 应用矩阵，包括平移：把 Vec3 当作 (x,y,z,1)
(define (mat4-transform-point m v)
  (let* ([mv (Mat4-m m)]
         [x (Vec3-x v)] [y (Vec3-y v)] [z (Vec3-z v)]
         [m0 (vector-ref mv 0)]  [m1 (vector-ref mv 1)]  [m2 (vector-ref mv 2)]  [m3 (vector-ref mv 3)]
         [m4 (vector-ref mv 4)]  [m5 (vector-ref mv 5)]  [m6 (vector-ref mv 6)]  [m7 (vector-ref mv 7)]
         [m8 (vector-ref mv 8)]  [m9 (vector-ref mv 9)]  [m10 (vector-ref mv 10)] [m11 (vector-ref mv 11)]
         [m12 (vector-ref mv 12)][m13 (vector-ref mv 13)][m14 (vector-ref mv 14)][m15 (vector-ref mv 15)]
         ;; result = M * [x y z 1]^T
         [rx (+ (* m0 x) (* m1 y) (* m2 z) (* m3 1))]
         [ry (+ (* m4 x) (* m5 y) (* m6 z) (* m7 1))]
         [rz (+ (* m8 x) (* m9 y) (* m10 z) (* m11 1))]
         [rw (+ (* m12 x)(* m13 y)(* m14 z)(* m15 1))])
    (if (and rw (not (= rw 0)))
        (Vec3 (/ rx rw) (/ ry rw) (/ rz rw))
        (Vec3 rx ry rz))))

;; transform-dir: 对 Vec3 方向向量应用矩阵，仅旋转和缩放部分，不加平移：把 Vec3 当作 (x,y,z,0)
(define (mat4-transform-dir m v)
  (let* ([mv (Mat4-m m)]
         [x (Vec3-x v)] [y (Vec3-y v)] [z (Vec3-z v)]
         [m0 (vector-ref mv 0)]  [m1 (vector-ref mv 1)]  [m2 (vector-ref mv 2)]
         [m4 (vector-ref mv 4)]  [m5 (vector-ref mv 5)]  [m6 (vector-ref mv 6)]
         [m8 (vector-ref mv 8)]  [m9 (vector-ref mv 9)]  [m10 (vector-ref mv 10)]
         ;; 忽略最后列
         [rx (+ (* m0 x) (* m1 y) (* m2 z))]
         [ry (+ (* m4 x) (* m5 y) (* m6 z))]
         [rz (+ (* m8 x) (* m9 y) (* m10 z))])
    (Vec3 rx ry rz)))

;; 矩阵逆：实现通用 4x4 逆矩阵算法
;; 参考常见 GLU 逆矩阵实现翻译
(define (mat4-inverse m)
  (let ([src (Mat4-m m)]
        [dst (make-vector 16 0)])
    ;; 使用 local copies for readability
    (define a00 (vector-ref src 0))  (define a01 (vector-ref src 1))
    (define a02 (vector-ref src 2))  (define a03 (vector-ref src 3))
    (define a10 (vector-ref src 4))  (define a11 (vector-ref src 5))
    (define a12 (vector-ref src 6))  (define a13 (vector-ref src 7))
    (define a20 (vector-ref src 8))  (define a21 (vector-ref src 9))
    (define a22 (vector-ref src 10)) (define a23 (vector-ref src 11))
    (define a30 (vector-ref src 12)) (define a31 (vector-ref src 13))
    (define a32 (vector-ref src 14)) (define a33 (vector-ref src 15))
    ;; 计算临时变量
    ;; Actually for det and cofactors we follow known code:
    ;; We'll follow GLU code pattern (slightly adapted).
    ;; Compute cofactors:
    (define b00 (- (* a00 a11) (* a01 a10)))
    (define b01 (- (* a00 a12) (* a02 a10)))
    (define b02 (- (* a00 a13) (* a03 a10)))
    (define b03 (- (* a01 a12) (* a02 a11)))
    (define b04 (- (* a01 a13) (* a03 a11)))
    (define b05 (- (* a02 a13) (* a03 a12)))
    (define b06 (- (* a20 a31) (* a21 a30)))
    (define b07 (- (* a20 a32) (* a22 a30)))
    (define b08 (- (* a20 a33) (* a23 a30)))
    (define b09 (- (* a21 a32) (* a22 a31)))
    (define b10 (- (* a21 a33) (* a23 a31)))
    (define b11 (- (* a22 a33) (* a23 a32)))
    ;; Compute determinant
    (define det (+ (* a00 b11) (* -1 (* a01 b10)) (+ (* a02 b09) (* -1 (* a03 b08)))))
    (when (zero? det)
      (error "Matrix is singular, cannot invert"))
    (define inv-det (/ 1 det))
    ;; Compute inverse elements, using formula for inverse of 4x4:
    ;; Following formula from GLU:
    (vector-set! dst 0  (* inv-det (+ (* a11 b11) (* -1 (* a12 b10)) (+ (* a13 b09)))))
    (vector-set! dst 1  (* inv-det (+ (* -1 a01 b11) (* a02 b10) (* -1 (* a03 b09)))))
    (vector-set! dst 2  (* inv-det (+ (* a31 b05) (* -1 (* a32 b04)) (+ (* a33 b03)))))
    (vector-set! dst 3  (* inv-det (+ (* -1 a21 b05) (* a22 b04) (* -1 (* a23 b03)))))
    (vector-set! dst 4  (* inv-det (+ (* -1 a10 b11) (* a12 b08) (* -1 (* a13 b07)))))
    (vector-set! dst 5  (* inv-det (+ (* a00 b11) (* -1 (* a02 b08)) (+ (* a03 b07)))))
    (vector-set! dst 6  (* inv-det (+ (* -1 a30 b05) (* a32 b02) (* -1 (* a33 b01)))))
    (vector-set! dst 7  (* inv-det (+ (* a20 b05) (* -1 (* a22 b02)) (+ (* a23 b01)))))
    (vector-set! dst 8  (* inv-det (+ (* a10 b10) (* -1 (* a11 b08)) (+ (* a13 b06)))))
    (vector-set! dst 9  (* inv-det (+ (* -1 a00 b10) (* a01 b08) (* -1 (* a03 b06)))))
    (vector-set! dst 10 (* inv-det (+ (* a30 b04) (* -1 (* a31 b02)) (+ (* a33 b00)))))
    (vector-set! dst 11 (* inv-det (+ (* -1 a20 b04) (* a21 b02) (* -1 (* a23 b00)))))
    (vector-set! dst 12 (* inv-det (+ (* -1 a10 b09) (* a11 b07) (* -1 (* a12 b06)))))
    (vector-set! dst 13 (* inv-det (+ (* a00 b09) (* -1 (* a01 b07)) (+ (* a02 b06)))))
    (vector-set! dst 14 (* inv-det (+ (* -1 a30 b03) (* a31 b01) (* -1 (* a32 b00)))))
    (vector-set! dst 15 (* inv-det (+ (* a20 b03) (* -1 (* a21 b01)) (+ (* a22 b00)))))
    (Mat4 dst)))

;; 构造基本变换矩阵
;; translation
(define (make-translation-mat4 dx dy dz)
  (let ([v (vector 1 0 0 0
                   0 1 0 0
                   0 0 1 0
                   dx dy dz 1)])
    (Mat4 v)))

;; scale
(define (make-scale-mat4 sx sy sz)
  (let ([v (vector sx 0 0 0
                   0 sy 0 0
                   0 0 sz 0
                   0 0 0 1)])
    (Mat4 v)))

;; rotation about arbitrary axis (x,y,z) by angle degrees (convert to radians internally)
(define (make-rotation-mat4 ax ay az angle-deg)
  (let* ([theta ( * pi (/ angle-deg 180) )]
         [u (vec-normalize (Vec3 ax ay az))]
         [ux (Vec3-x u)] [uy (Vec3-y u)] [uz (Vec3-z u)]
         [c (cos theta)] [s (sin theta)] [t (- 1 c)]
         ;; rotation matrix row-major:
         ;; [ t*ux*ux + c,   t*ux*uy - s*uz, t*ux*uz + s*uy, 0 ]
         ;; [ t*ux*uy + s*uz, t*uy*uy + c,   t*uy*uz - s*ux, 0 ]
         ;; [ t*ux*uz - s*uy, t*uy*uz + s*ux, t*uz*uz + c,   0 ]
         ;; [ 0, 0, 0, 1 ]
         [m00 (+ (* t ux ux) c)]
         [m01 (+ (* t ux uy) (* -1 (* s uz)))] ; careful signs
         [m02 (+ (* t ux uz) (* s uy))]
         [m10 (+ (* t ux uy) (* s uz))]
         [m11 (+ (* t uy uy) c)]
         [m12 (+ (* t uy uz) (* -1 (* s ux)))]
         [m20 (+ (* t ux uz) (* -1 (* s uy)))]
         [m21 (+ (* t uy uz) (* s ux))]
         [m22 (+ (* t uz uz) c)]
         [v (vector m00 m01 m02 0
                    m10 m11 m12 0
                    m20 m21 m22 0
                    0   0   0   1)])
    (Mat4 v)))

;; ----------------------------------------
;; 文件名生成
;; ----------------------------------------
;; 从 Scene.counter 自增并返回 filename "scene-0001.ppm"
(define output-dir "output") 
(define (generate-filename! scene)
  (let ([cnt (add1 (Scene-counter scene))])
    (set-Scene-counter! scene cnt)
    (define fname (format "scene-~a.ppm" cnt))
    ;; 确保 output-dir 存在
    (when (not (directory-exists? output-dir))
      (make-directory output-dir))
    ;; 返回完整路径，可以是 path 对象或字符串
    (build-path output-dir fname)))


;; ----------------------------------------
;; 字符串填充
;; ----------------------------------------
;; string-pad-left: 将 s 填充到 total-length 长度，使用 pad-char（单字符字符串）在左侧填充
(define (string-pad-left s total-length pad-char)
  (let* ([curr (string-length s)]
         [needed (- total-length curr)])
    (if (<= needed 0)
        s
        (apply string-append (make-list needed pad-char) s)))

  )

