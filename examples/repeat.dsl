; to-run
; racket src\main.rkt examples\repeat.dsl

(clear-scene)

(set-camera
  (camera
    (pos -0.1 0.5 -3.0)
    (look-at -1 1 0.7)
    (up 0 1 0)
    (fov 35)
    (width 640)
    (height 480)))

(set-background (color 0.1 0.1 0.1))

; 加入五个长方体，其中第i个长方体：
; name: "box{i}"
; min: (x - 0.1, -0.1 - 1.2x, 2.4 - 0.1x)
; max: (1.2x + 0.1, 0.1 - x, 2.6 + 0.1x)
; color: (0.8 - 0.1x, 0.3, 0.3 + 0.15x)

(repeat 5
  (let x
    (+ -0.5 (* i (/ 1 (- 5 1))))
    (let name
      (string->symbol (string-append "box" (number->string i)))
      (add-object name
        (box
            (min (- x 0.1) (- -0.1 (* 1.2 x)) (- 2.4 (* 0.1 x)))
            (max (+ (* 1.2 x) 0.1) (- 0.1 x) (+ 2.6 (* 0.1 x)))
          (material diffuse (color (- 0.8 (* x 0.15)) 0.3 (+ 0.3 (* x 0.15)))))))))

(add-light lamp1
  (point-light
    (pos 2 5 1)
    (intensity 6 6 6)))

(add-light lamp2
  (point-light
    (pos -2 3 0.5)
    (intensity 6 6 6)))

(add-light dir
  (directional-light
    (dir 0 0 1)
    (intensity 0.1 0.1 0.1)))

(add-light ambient
  (ambient-light (intensity 0.1 0.1 0.1)))

(render (samples 5))

; 把第 i 个长方体绕 (1,1,1) 旋转 10*i 度
(repeat 5
  (let name
      (string->symbol (string-append "box" (number->string i)))
      (transform name
        (rotate-axis-angle 1 1 1 (* 10 i)))))

(render (samples 5))