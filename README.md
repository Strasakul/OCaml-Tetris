# OCaml-Tetris

A fully functional Tetris clone

The game state is completely immutable—every frame, collision, and line-clear is calculated by passing data through mathematical pipelines.

## Controls

* **`A`** - Move Left
* **`D`** - Move Right
* **`S`** - Soft Drop (Speed up!)
* **`Q`** - Rotate Left
* **`E`** - Rotate Right
* **Mouse Click** - Click the "Restart" button on the Game Over screen

## How to Play

You'll need OCaml and Dune installed on your machine. Because this uses the classic OCaml `Graphics` module, Linux users will also need standard X11 headers (e.g., `sudo apt install libx11-dev`).

**1. Install the dependencies:**
```bash
opam install dune graphics
```

**2. Build and run the game:**
```bash
dune exec src/main.exe
```

*(Note: If you are playing on WSL2 and it crashes complaining about the display, make sure your WSL environment is updated with `wsl --update` so it can forward the graphical window to Windows!)*