"""
convert_ppm.py

用法：
    python convert_ppm.py input.ppm output.png

功能：
    读取 PPM 格式图像文件（支持 PIL 可识别的 PPM，如 P3/P6），将其保存为 PNG，
    并在执行环境中显示（若环境支持图形界面或 notebook）。
"""

import sys
import os

def convert_ppm_to_png(ppm_path, png_path):
    try:
        from PIL import Image
    except ImportError:
        print("需要安装 Pillow 库: pip install pillow")
        sys.exit(1)

    # 确保输入文件存在
    if not os.path.isfile(ppm_path):
        print(f"输入文件不存在: {ppm_path}")
        sys.exit(1)

    # 读取 PPM
    try:
        img = Image.open(ppm_path)
    except Exception as e:
        print(f"无法读取 PPM 文件: {e}")
        sys.exit(1)

    # 保存为 PNG
    try:
        img.save(png_path)
    except Exception as e:
        print(f"无法保存 PNG 文件: {e}")
        sys.exit(1)

    print(f"已将 {ppm_path} 转换为 {png_path}")

    # 尝试显示图片（在有 GUI 的环境下会打开一个窗口；在 notebook 环境下会被忽略）
    try:
        img.show()
    except Exception as e:
        # 在某些环境 img.show() 可能不工作
        pass

def main():
    if len(sys.argv) < 2:
        print("用法: python convert_ppm.py input.ppm [output.png]")
        sys.exit(1)
    ppm_path = sys.argv[1]
    if len(sys.argv) >= 3:
        png_path = sys.argv[2]
    else:
        # 默认把扩展名替换为 .png
        base, _ = os.path.splitext(ppm_path)
        png_path = base + ".png"
    convert_ppm_to_png(ppm_path, png_path)

if __name__ == "__main__":
    main()
