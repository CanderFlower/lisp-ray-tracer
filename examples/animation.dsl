; 示例3：动画脚本 animation.dsl
; 假设解释器支持 (repeat <count> <command1> <command2> ...)

; 1. 清空并初始化基础场景
(clear-scene)
(set-global (samples 50))
(set-global (max-depth 5))

; 2. 基础摄像机设置：位置稍远
(set-camera
  (camera
    (pos 0 2 10)
    (look-at 0 1 0)
    (up 0 1 0)
    (fov 50)
    (width 400)
    (height 300)))

; 3. 添加地面
(add-object ground
  (plane (normal 0 1 0) (dist 1) (material diffuse (color 0.6 0.6 0.6))))

; 4. 添加一个组 group_balls，用于放多个球，后面做旋转动画
(group group_balls)
; 生成 8 个球均匀分布在圆周上
; 如果 DSL 支持 repeat 和简单算术及 string 拼接；否则可手动写多个 add-object
; 假设有 (repeat <n> (<body>...))，并且可以访问内置变量 i (0..n-1)
; 以下伪示例演示思想。若解释器暂时不支持，可手动复制 8 行 add-object：
(repeat 8
  ; i 从 0 到 7
  (let name (string-append "ball" (number->string i)))
    ; 计算位置：半径 3，在 XZ 平面分布
    ; 角度 θ = 2π * i / 8 （单位度或弧度，根据 parse 需实现简单表达式或在 REPL 中由 Racket 做），此处假定 DSL 内支持 sin/cos，或预先计算好常数
    ; 这里示意：center (3*cosθ, 1, 3*sinθ)
    (add-object name
      (sphere
        (center ( * 3 (cos (* 360 (/ i 8))) ) 1 ( * 3 (sin (* 360 (/ i 8))) ))
        (radius 0.5)
        (material diffuse (color (abs (cos (* 45 i))) (abs (sin (* 30 i))) 0.5))))
))

; 5. 添加中心发光体：小发光球，模拟中心光源
(add-object center_light_sphere
  (sphere (center 0 1 0) (radius 0.3) (material emissive (color 3 3 2))))

; 6. 添加额外光源（辅助点光）
(add-light lamp1 (point-light (pos 5 5 5) (intensity 0.8 0.8 1.0)))
(add-light ambient (ambient-light (intensity 0.2 0.2 0.2)))

; 7. 设置背景渐变
(set-background (gradient (color 0.1 0.1 0.2) (color 0.8 0.9 1.0)))

; 8. 首次渲染静态帧，文件名 animation-0000.ppm（假设解释器提供前缀功能或用户手动指定）
(render (filename "animation-0000.ppm"))

; 9. 动画循环示例：让 group_balls 及 camera 绕 Y 轴旋转
; 假设 DSL 支持 repeat，并且每次循环 i 从 1 到 N，可在循环体内使用 i。
; 如果 DSL 不直接支持表达式运算，可手动在脚本中预先生成命令；此处示例演示思路。
(repeat 36
  ; 每次旋转 10 度，共 36 帧
  (transform group_balls (rotate-axis-angle 0 1 0 10))
  (transform center_light_sphere (rotate-axis-angle 0 1 0 10))
  ; 可选：camera 绕场景旋转，先把 camera 平移到原点旋转再平移回来；示例简化：
  (transform camera (rotate-axis-angle 0 1 0 10)) ; 需要解释器支持对 camera 的 transform；若不支持，可用 update-camera 的高级命令
  ; 渲染当前帧，指定文件名带帧号
  (let fname (string-append "animation-" (string-pad-left (number->string i) 3 "0") ".ppm"))
    (render (filename fname)))
)

; 10. 动画结束后，可在脚本末尾保存场景状态
(save-scene "animation_scene.json")

; 11. 退出
(exit)
