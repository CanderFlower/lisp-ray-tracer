; 示例1：基础单球场景
; 假设文件名为 simple_sphere.dsl
; racket src\main.rkt examples\simple_sphere.dsl

; 1. 清空场景（重置到默认）
(clear-scene)

; 2. 设置摄像机
(set-camera
  (camera
    (pos -0.8 0.3 -1.4)
    (look-at -0.9 1 0.7)
    (up 0 1 0)
    (fov 60)
    (width 320)
    (height 240)))

; 3. 设置背景为渐变：上方浅蓝，下方白
(set-background (gradient (color 0.5 0.7 1.0) (color 1.0 1.0 1.0)))
; (set-background (gradient (color 1 0 0) (color 0 1.0 1.0)))
; 4. 添加物体
(add-object box
  (box
    (min -0.4 -0.4 2.6)
    (max 0.4 0.4 3.4)
    (material diffuse (color 0.7 0.5 0.0))))

(add-object sphere2
  (sphere
    (center 1 0.4 2)
    (radius 0.5)
    (material metal (albedo 0.9 0.9 0.9) (fuzz 0.0))))

(add-object sphere3
  (sphere
    (center 1.3 0 1)
    (radius 0.4)
    (material diffuse (color 0.2 0.5 0.7))))



(add-object sphere4
  (sphere
    (center 1.3 0.7 1)
    (radius 0.2)
    (material diffuse (color 0.2 0.9 0.2))))


; 5. 添加一个水平平面，作为地面：法线 (0,1,0)，距离 1（y= -1? 视约定；这里假定 plane.dist 表示从原点到平面的距离沿法线方向）
;    例如 plane 方程 n·P + dist = 0; 若想 y = -1, 可用 normal (0,1,0) dist 1。（根据实现细节）
(add-object floor
  (plane
    (normal 0 1 0)
    (dist 1)
    (material diffuse (color 0.5 0.2 0.2))))

; 6. 添加一个点光源 lamp1，在 (2,5,1)，强度 (1,1,1)
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

; 7. 可选：查看当前对象列表
(list-objects)
; 输出示例：
; sphere1: type=sphere, center=(0 0 -5), radius=1, material=diffuse(color=(1 0 0)), transform=Identity
; floor: type=plane, normal=(0 1 0), dist=1, material=diffuse(color=(0.8 0.8 0.8)), transform=Identity

(set-global (samples 1))

; 8. 渲染：使用 50 样本抗锯齿，最大递归深度使用默认（如 5）；自动文件名 scene-0001.ppm
(render)



(transform sphere2
  (translate 0 -0.6 0))
(transform sphere4
  (scale 0.5 1 0.5))
  
(update-object sphere4
  (material diffuse (color 0.2 0.2 0.9)))
(list-objects)
(render)

; 9. 渲染完成后，解释器会提示 “Rendered to scene-0001.ppm (resolution 300x200, samples=50, max-depth=5)”
;    可以在 Python 脚本或图像查看器中将其转换/查看。 python scripts\convert_ppm.py output\scene-1.ppm

; 10. 退出（若在交互式 REPL，则可用 (exit)；在脚本中可省略）
(exit)
