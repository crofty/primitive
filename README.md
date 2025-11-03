# Primitive Pictures (Clojure Edition)

This repository contains a Clojure reimplementation of Michael Fogleman's
**Primitive** project. The program recreates an input image using layered
semi-transparent geometric primitives (currently triangles) discovered via a
hill-climbing search.

## Requirements

* Java 11+
* [Clojure CLI tools](https://clojure.org/guides/getting_started)

## Command-line Usage

```bash
clojure -M:run -- -i input.png -o output.png -n 100
```

### Notable options

| Flag | Description |
| ---- | ----------- |
| `-i`, `--input` | Input image path (required) |
| `-o`, `--output` | Output image path (required, may be specified multiple times) |
| `-n`, `--count` | Number of shapes to add |
| `-a`, `--alpha` | Alpha value (0-255) used for each triangle |
| `-r`, `--resize` | Resize large input images so the longest edge matches this size (default `256`) |
| `-s`, `--size` | Resize the final image to this square size |
| `--samples` | Number of random candidate triangles evaluated per iteration (default `20`) |
| `--mutations` | Number of mutations applied to the best candidate each iteration (default `40`) |
| `--bg` / `--background` | Override the starting background color (hex RGB) |
| `--seed` | Provide a seed for deterministic runs |
| `-v`, `--verbose` | Print progress information |

The Clojure port currently supports triangle mode (`-m 1`). Other shape modes
from the Go version are not yet implemented.

Output images are written in PNG, JPEG, or SVG format based on the file
extension. SVG output saves the discovered triangles as vector polygons and
honors the `--size` option by scaling the coordinates. If the output path
contains a printf-style placeholder (e.g. `frame-%03d.png`), the final frame is
saved using the provided iteration count.

## Development

Run the automated tests with:

```bash
clojure -M:test
```

The implementation focuses on clarity and testability rather than raw
performance. Small source images (e.g. 256×256) provide the best balance of
speed and quality.

