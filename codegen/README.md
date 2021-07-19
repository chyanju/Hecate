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

    ./run GRAMMAR_FILE CONCRETE_SCHEDULE OUTPUT_FILE

to force recompilation.