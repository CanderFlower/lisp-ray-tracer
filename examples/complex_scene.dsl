; 示例2：复杂场景 demo
; 假设文件名 complex_scene.dsl

; 1. 清空场景
(clear-scene)

; 2. 设置全局渲染默认：采样 100、最大递归深度 8
(set-global (samples 100))
(set-global (max-depth 8))

; 3. 设置摄像机：位置稍偏，分辨率 600x400
(set-camera
  (camera
    (pos 0 2 10)
    (look-at 0 1 0)
    (up 0 1 0)
    (fov 45)
    (width 600)
    (height 400)))

; 4. 设置背景为纯色深蓝
(set-background (color 0.1 0.1 0.2))

; 5. 添加地面：一个大盒子或平面
(add-object ground
  (plane
    (normal 0 1 0)
    (dist 1)
    (material diffuse (color 0.5 0.5 0.5))))

; 6. 添加一个金属球 sphere_metal，镜面反射
(add-object sphere_metal
  (sphere
    (center -2 1 -5)
    (radius 1)
    (material metal (albedo 0.8 0.8 0.9) (fuzz 0.1))))

; 7. 添加一个玻璃球 sphere_glass，折射
(add-object sphere_glass
  (sphere
    (center 2 1 -5)
    (radius 1)
    (material dielectric (ref_idx 1.5))))

; 8. 添加一个漫反射彩色球 sphere_diffuse
(add-object sphere_diffuse
  (sphere
    (center 0 1 -3)
    (radius 1)
    (material diffuse (color 0.2 0.8 0.3))))

; 9. 添加一个小盒子 box1，金属材质
(add-object box1
  (box
    (min -1 0 -2)
    (max 1 2 -1)
    (material metal (albedo 0.7 0.6 0.5) (fuzz 0.2))))

; 10. 添加一个发光平面 emitter，模拟自发光面
(add-object light_panel
  (plane
    (normal 0 -1 0)
    (dist -4)  ; 假设 plane 在 y=4 向下发光
    (material emissive (color 4 4 4)))) ; 强度较大，或在光照计算中 treat emissive specially

; 11. 添加光源：环境光 + 方向光 + 点光
(add-light ambient
  (ambient-light (intensity 0.1 0.1 0.1)))

(add-light sun
  (directional-light (dir -1 -1 -0.5) (intensity 0.5 0.5 0.5)))

(add-light lamp
  (point-light (pos 0 5 0) (intensity 1 0.9 0.8)))

; 12. 列出当前对象/光源
(list-objects)
(list-lights)

; 13. 分组示例：将两个球放到一个组 group1，便于统一旋转或平移
(group group1 sphere_metal sphere_glass)

; 14. 对 group1 应用一个旋转和平移：先绕 Y 轴旋转 30 度，再向上平移 0.5
(transform group1 (rotate-axis-angle 0 1 0 30))
(transform group1 (translate 0 0.5 0))

; 15. 更新已有对象：将 sphere_diffuse 调整到新位置或修改材质
(update-object sphere_diffuse
  (center 0 1.5 -4))  ; 仅修改中心位置

(update-object box1
  (material diffuse (color 0.9 0.7 0.4)))  ; 将 box1 改为漫反射色

; 16. 保存当前场景状态到文件
(save-scene "saved_complex_scene.json")

; 17. 进一步修改：载入刚才保存的场景以恢复，演示 load-scene
(load-scene "saved_complex_scene.json")

; 18. 再次列出，确保恢复成功
(list-objects)
(list-groups)

; 19. 渲染：显式指定文件名
(render (filename "complex1.ppm") (width 600) (height 400))
; 渲染完成后会生成 complex1.ppm

; 20. 修改全局设置：将采样提高，用于更高质量渲染
(set-global (samples 200))
(set-global (max-depth 10))
; 更新背景渐变
(set-background (gradient (color 0.2 0.3 0.5) (color 1.0 1.0 1.0)))

; 21. 再次渲染，使用自动文件名（scene-0002.ppm）
(render)

; 22. 如果想查看组内成员变换：可 list-groups，再对 group 再 transform，例如：
(transform group1 (rotate-axis-angle 0 1 0 45))
(list-objects)

; 23. 渲染新的视角
(render (filename "complex_rotated.ppm"))

; 24. 退出
(exit)
