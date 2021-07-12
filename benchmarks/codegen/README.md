# Codegen

## Requirements
- [SML/NJ](https://www.smlnj.org/) 110.99
  
## Usage
Compile with
    
    ml-build sources.cm Main.main codegen

Run with

    sml @SMLload codegen.[platform] GRAMMAR_FILE CONCRETE_SCHEDULE

During debugging, run with

    sml test.sml GRAMMAR_FILE CONCRETE_SCHEDULE

to avoid recompilation.