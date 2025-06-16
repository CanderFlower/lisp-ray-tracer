; 示例1：基础单球场景
; 假设文件名为 simple_sphere.dsl

; 1. 清空场景（重置到默认）
(clear-scene)

; 2. 设置摄像机：位置在 (0,1,5)，朝向原点，up=(0,1,0)，视野60度，分辨率 300x200
(set-camera
  (camera
    (pos 0 1 5)
    (look-at 0 0 0)
    (up 0 1 0)
    (fov 60)
    (width 300)
    (height 200)))

; 3. 设置背景为渐变：上方浅蓝，下方白
(set-background (gradient (color 0.5 0.7 1.0) (color 1.0 1.0 1.0)))

; 4. 添加一个红色漫反射球，名称 sphere1，中心 (0,0,-5)，半径 1
(add-object sphere1
  (sphere
    (center 0 0 -5)
    (radius 1)
    (material diffuse (color 1 0 0))))

; 5. 添加一个水平平面，作为地面：法线 (0,1,0)，距离 1（y= -1? 视约定；这里假定 plane.dist 表示从原点到平面的距离沿法线方向）
;    例如 plane 方程 n·P + dist = 0; 若想 y = -1, 可用 normal (0,1,0) dist 1。（根据实现细节）
(add-object floor
  (plane
    (normal 0 1 0)
    (dist 1)
    (material diffuse (color 0.8 0.8 0.8))))

; 6. 添加一个点光源 lamp1，在 (2,5,1)，强度 (1,1,1)
(add-light lamp1
  (point-light
    (pos 2 5 1)
    (intensity 1 1 1)))

; 7. 可选：查看当前对象列表
(list-objects)
; 输出示例：
; sphere1: type=sphere, center=(0 0 -5), radius=1, material=diffuse(color=(1 0 0)), transform=Identity
; floor: type=plane, normal=(0 1 0), dist=1, material=diffuse(color=(0.8 0.8 0.8)), transform=Identity

; 8. 渲染：使用 50 样本抗锯齿，最大递归深度使用默认（如 5）；自动文件名 scene-0001.ppm
(render (samples 1))

; 9. 渲染完成后，解释器会提示 “Rendered to scene-0001.ppm (resolution 300x200, samples=50, max-depth=5)”
;    可以在 Python 脚本或图像查看器中将其转换/查看。

; 10. 退出（若在交互式 REPL，则可用 (exit)；在脚本中可省略）
(exit)
