del /q output\*
racket src\main.rkt examples\simple_sphere.dsl
python scripts\convert_ppm.py output\scene-1.ppm
python scripts\convert_ppm.py output\scene-2.ppm