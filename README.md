# doku

An `ncurses`-based Sudoku game.

## building

- `stack build`

## controls

- `q` :: quit
- Arrow keys or `h`/`j`/`k`/`l` (vim-style) :: navigation
- number keys :: enter a value
- `backspace` or `delete` to clear a value
- `shift` + number keys :: toggle marks
- `enter` :: check solution

## TODO

- [ ] `--help` documentation
- [ ] improve visuals a bit
- [ ] mouse support
- [ ] colors
- [ ] save/load puzzles
- [ ] prevent interacting with "givens"
- [ ] generate puzzles
- [ ] variants
    - [ ] killer sudoku
    - [ ] sandwich sudoku
    - [ ] others?
