; to-run
; racket src\main.rkt examples\check.dsl

; 清空场景
(clear-scene)

; 设置摄像机、宽高（低质量版）
(set-camera
  (camera
    (pos -0.8 0.3 -1.4)
    (look-at -1 1 0.7)
    (up 0 1 0)
    (fov 60)
    (width 160)
    (height 120)))

; 设置背景为渐变
(set-background (gradient (color 0.5 0.7 1.0) (color 1.0 1.0 1.0)))

; 添加物体

; box-使用对角坐标表示
; diffuse-漫反射材质
(add-object box
  (box
    (min -0.4 -0.4 2.6)
    (max 0.4 0.4 3.4)
    (material diffuse (color 0.7 0.5 0.0))))

; sphere-用球心、半径表示
; metal-金属材质，白色基础色，非常光滑
(add-object sphere2
  (sphere
    (center 1 0.4 2)
    (radius 0.5)
    (material metal (albedo 1 1 1) (fuzz 0.0))))

; 同上
(add-object sphere3
  (sphere
    (center 1.3 0 1)
    (radius 0.4)
    (material diffuse (color 0.2 0.5 0.7))))

; 同上
(add-object sphere4
  (sphere
    (center 1.3 0.7 1)
    (radius 0.2)
    (material diffuse (color 0.2 0.9 0.2))))


; 添加一个水平平面，作为地面：法线 (0,1,0)，距离 1（从原点到平面的距离沿法线方向）
(add-object floor
  (plane
    (normal 0 1 0)
    (dist 1)
    (material diffuse (color 0.5 0.2 0.2))))

; 添加光源
(add-light lamp1
  (point-light
    (pos 2 5 1)
    (intensity 6 6 6)))

(add-light lamp2
  (point-light
    (pos -2 3 0.5)
    (intensity 6 6 6)))

; 方向光源不受衰减
(add-light dir
  (directional-light
    (dir 0 0 1)
    (intensity 0.1 0.1 0.1)))

; 查看当前对象列表
(list-objects)

; 设置全局设置：16样本（低质量）
(set-global (samples 16))

; 渲染：使用16样本抗锯齿（设置的），最大递归深度使用默认（5）；自动文件名 scene-0001.ppm
(render)

; 对物体进行变换
; 平移
(transform sphere2
  (translate 0 -0.6 0))
; 缩放
(transform sphere4
  (scale 0.5 1 0.5))

; 直接重新设置物体材质
(update-object sphere4
  (material diffuse (color 0.2 0.2 0.9)))

(list-objects)

(render)

; 加入新的灯光：环境光（永远受此光照）
(add-light ambient
  (ambient-light (intensity 0.1 0.1 0.1)))

; 创建group：将这两个球编组
(group group1 sphere2 sphere3)

; 整体变换
(transform group1 (translate 0 0.2 0.2))

; 更改物体材质
; dielectric，可理解为透明材质，ref_idx是折射率
(update-object sphere3
  (material dielectric (ref_idx 1.5)))


(list-objects)
(render)


; 退出
(exit)

; 运行以下命令可以查看图片
; python scripts\convert_ppm.py output\scene-1.ppm
; python scripts\convert_ppm.py output\scene-2.ppm
; python scripts\convert_ppm.py output\scene-3.ppm