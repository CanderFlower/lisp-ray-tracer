#lang racket
(provide 
 ;; Structs
 Vec3 Mat4 Camera Sphere Plane Box Triangle Material HitInfo
 PointLight DirectionalLight AmbientLight SpotLight Group Scene
 global-scene
(struct-out Vec3)
(struct-out Mat4)
(struct-out Camera)
(struct-out Sphere)
(struct-out Plane)
(struct-out Box)
(struct-out Triangle)
(struct-out Material)
(struct-out HitInfo)
(struct-out PointLight)
(struct-out DirectionalLight)
(struct-out AmbientLight)
(struct-out SpotLight)
(struct-out Group)
(struct-out Scene)
)  

;; ----------------------------------------
;; 核心数据结构定义
;; ----------------------------------------

;; 向量 3D
(struct Vec3 (x y z) #:transparent)
;; 示例: (Vec3 1 2 3)

;; 4x4 矩阵，存储为 Racket Vector 长度16，按行主序 row-major: index = 4*row + col
(struct Mat4 (m) #:transparent)
;; Mat4 的 m 字段是一个 vector-of 16 numbers.

;; Camera: pos, look-at, up: Vec3；fov: number（度）；width,height: integer 分辨率
(struct Camera (pos look-at up fov width height) #:mutable #:transparent)
;; e.g. (Camera (Vec3 0 1 5) (Vec3 0 0 0) (Vec3 0 1 0) 60 300 200)

;; Material 类型枚举字段放在 Material struct 中：
;; type: symbol 'diffuse, 'metal, 'dielectric, 'emissive
;; params: vary per type; 为简单起见，把必要字段存在 struct 中
(struct Material (type color albedo fuzz ref-idx) #:mutable #:transparent)
;; 使用约定：
;; - 如果 type = 'diffuse: color: Vec3； albedo, fuzz, ref-idx ignored
;; - 如果 type = 'metal: albedo: Vec3； fuzz: number； color, ref-idx ignored
;; - 如果 type = 'dielectric: ref-idx: number； color, albedo, fuzz ignored
;; - 如果 type = 'emissive: color: Vec3； albedo, fuzz, ref-idx ignored

;; HitInfo: 用于存储一次相交的结果
(struct HitInfo (t point normal material) #:transparent)
;; t: number；point, normal: Vec3；material: Material

;; Sphere: center: Vec3；radius: number；material: Material；transform: Mat4
(struct Sphere (center radius material transform) #:mutable #:transparent)
;; Plane: 存储平面方程 n·P + d = 0，normal: Vec3，dist: number；material: Material；transform: Mat4
(struct Plane (normal dist material transform) #:mutable #:transparent)
;; Box: axis-aligned box in local space, min & max as Vec3；material; transform: Mat4
(struct Box (min max material transform) #:mutable #:transparent)
;; Triangle: p1,p2,p3: Vec3 in local space；material; transform: Mat4
(struct Triangle (p1 p2 p3 material transform) #:mutable #:transparent)

;; 光源类型
(struct PointLight (pos intensity) #:mutable #:transparent)
(struct DirectionalLight (dir intensity) #:mutable #:transparent)
(struct AmbientLight (intensity) #:mutable #:transparent)
(struct SpotLight (pos dir angle intensity) #:mutable #:transparent)
;; SpotLight: pos Vec3, dir Vec3(normalized), angle number（度或弧度，内部用 radians）, intensity Vec3

;; Group: 存储成员名称列表及局部 transform 矩阵
;; members: list of symbols (object names or group names)
;; transform: Mat4
(struct Group (members transform) #:mutable #:transparent)

;; Scene: camera: Camera；objects: hash mapping symbol->object struct；lights: hash mapping symbol->light struct
;; groups: hash mapping symbol->Group； background: a struct或list描述； settings: hash 'samples 'max-depth； counter: integer
(struct Scene (camera 
              objects 
              lights 
              groups 
              background 
              settings 
              counter) 
  #:mutable 
  #:transparent)

;; 全局 Scene 变量
;; 初始化在 interpreter.rkt 中
(define global-scene (make-parameter #f))

(provide global-scene)