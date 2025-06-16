@echo off
if exist output\scene-*.ppm del /q output\scene-*.ppm
if exist output\scene-*.png del /q output\scene-*.png

if "%~1"=="" (
    echo Usage: run example_name
    exit /b 1
)

set filename=%1
echo === Running example: %filename% ===

powershell -Command "$start = Get-Date; racket src\main.rkt examples\%filename%.dsl; $end = Get-Date; $elapsed = $end - $start; Write-Host ('>>> Elapsed Time: ' + $elapsed.TotalSeconds + ' s')"

for %%f in (output\*.ppm) do (
    python scripts\convert_ppm.py %%f
)
