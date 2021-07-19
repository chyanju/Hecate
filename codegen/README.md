# Codegen


## Requirements
- [SML/NJ](https://www.smlnj.org/) 110.99
- `clang-format`
  

## Usage
Compile with
    
    ml-build sources.cm Main.main codegen

Run with

    sml @SMLload codegen.[platform] GRAMMAR_FILE CONCRETE_SCHEDULE

During development/debugging, run with

    ./run GRAMMAR_FILE CONCRETE_SCHEDULE


## Tests

### FMM
    # Generate concrete schedule using fmm.grammar
    (cd ..; ./run.rkt --root VirtualRoot fuse codegen/tests/fmm/fmm.grammar)
    # Please manually copy the concrete schedule into codegen/tests/fmm/fmm.sch
    # Run code-gen with augmented grammar and concrete schedule
    ./run tests/fmm/fmm.grammar-aug tests/fmm/fmm.sch > tests/fmm/fmm.h
