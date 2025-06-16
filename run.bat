del /q output\*
set filename=%1
racket src\main.rkt examples\%filename%.dsl
python scripts\convert_ppm.py output\scene-1.ppm
python scripts\convert_ppm.py output\scene-2.ppm
python scripts\convert_ppm.py output\scene-3.ppm