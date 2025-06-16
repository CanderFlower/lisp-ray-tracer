# Lisp Ray Tracer

This project is a simple ray tracer implemented in Lisp. It demonstrates basic concepts of computer graphics, including ray tracing, lighting, and rendering.

Final project for the course "Principes of Programming Languages" at Fudan University.

## Features

- Ray-sphere intersection
- Basic lighting (ambient, diffuse, and specular)
- Scene rendering to an image file
- Configurable scene setup

## Requirements

- Racket
- Python

## Usage

There are two ways to run the ray tracer:
### Using Racket
Run the `main.rkt` file directly in Racket. It is an interactive program that allows you to set up the scene and render it.
### Write scripts with `.dsl`
You can write scripts in the `.dsl` language to define scenes and render them. 

Use `run.bat`: it will take the first argument as the script file and run it. For example:
```bash
run check
```
will run the `examples/check.dsl` script. **It will delete all the images in the `output` directory, then generate new images.**

## Examples

There are several example scripts in the `examples` directory. You can run them using the provided `run.bat` script. Here are some examples:
- `check.dsl`: A simple scene with different spheres and lighting. 
- `render.dsl`: Same as `check.dsl`, but with higher resolution and quality settings. (Time to render: ~45min)
- `repeat.dsl`: A scene to show how to use loops in the DSL.