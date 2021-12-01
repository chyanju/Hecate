## Hecate: Tree Traversal Synthesis Using Domain-Specific Symbolic Compilation

This branch (`checking`) is the implementation of general-purpose encoding.

### Prerequisites

- Racket 7.7 ([https://download.racket-lang.org/releases/7.7/](https://download.racket-lang.org/releases/7.7/))

- Rosette 3.2 ([https://github.com/emina/rosette](https://github.com/emina/rosette))

  - To install Rosette 3.2, you need to install from source:

    ```bash
    git clone https://github.com/emina/rosette.git
    raco pkg remove rosette
    cd rosette/
    git checkout c092b65
    raco pkg install
    ```

- Z3 Theorem Prover ([https://github.com/Z3Prover/z3](https://github.com/Z3Prover/z3))

Note: The general-purpose encoding version doesn't need IBM CPLEX.

### Commands for ASPLOS Artifact Evaluation

This reproduces the `HECATE-G` column of Table 1:

```bash
# binary-tree.grammar
racket ./run.rkt --interface Root --traversal fuse --grammar benchmarks/grafter/binary-tree.grammar

# fmm.grammar (this needs full examples)
racket ./run.rkt --interface VirtualRoot --traversal fuse --grammar benchmarks/grafter/fmm.grammar

# piecewise series
racket ./run.rkt --interface VirtualRoot --traversal fuse --grammar benchmarks/grafter/piecewise-exp1.grammar
racket ./run.rkt --interface VirtualRoot --traversal fuse --grammar benchmarks/grafter/piecewise-exp2.grammar
racket ./run.rkt --interface VirtualRoot --traversal fuse --grammar benchmarks/grafter/piecewise-exp3.grammar

# ast (this needs full examples, which takes a long time)
racket ./run.rkt --interface Program --traversal fuse --grammar benchmarks/grafter/ast.grammar

# render (this needs full examples)
racket ./run.rkt --interface Document --traversal fuse --grammar benchmarks/grafter/render.grammar
```

Note: add timing commands to record time usage, e.g., `time` in Linux.

### Testing Commands

This includes all commands for testing.

```bash
# molly3.grammar: accessing list children without dependencies
racket ./run.rkt --interface Node --traversal fusion --grammar ./benchmarks/molly/molly3.grammar

# molly4.grammar: accessing list children with dependencies
racket ./run.rkt --interface Node --traversal fusion --grammar ./benchmarks/molly/molly4.grammar

# molly5.grammar: parent depends on children
racket ./run.rkt --interface Node --traversal fusion --grammar ./benchmarks/molly/molly5.grammar

# molly6.grammar: chain dependency
racket ./run.rkt --interface VirtualRoot --traversal fusion --grammar ./benchmarks/molly/molly6.grammar

# molly7.grammar: slightly more attributes
racket ./run.rkt --interface VirtualRoot --traversal fusion --grammar ./benchmarks/molly/molly7.grammar

# molly8.grammar: MFE for oopsla-example (denote-ite bug)
racket ./run.rkt --interface Root --traversal fuse --grammar ./benchmarks/molly/molly8.grammar

# molly9.grammar: testing input keyword
racket ./run.rkt --interface Node --traversal fusion --grammar ./benchmarks/molly/molly9.grammar

# molly10.grammar: testing uninterpreted function (faked with concrete values)
racket ./run.rkt --interface Node --traversal fusion --grammar ./benchmarks/molly/molly10.grammar

# molly11.grammar: testing uninterpreted function (faked with new symbolic variables)
racket ./run.rkt --interface Node --traversal fusion --grammar ./benchmarks/molly/molly11.grammar

# molly12.grammar: many many attributes
racket ./run.rkt --interface VirtualRoot --traversal fusion --grammar ./benchmarks/molly/molly12.grammar
```

### Benchmark Commands

This includes all commands for both evaluation and debugging.

```bash
# toy.grammar
racket ./run.rkt --interface Tree --traversal layout --grammar benchmarks/toy.grammar

# hv-toy.grammar
racket ./run.rkt --interface HVBox --traversal fuse --grammar benchmarks/grafter/hv-toy.grammar

# grafter/oopsla-example.grammar: this takes forever to solve
racket ./run.rkt --interface Root --traversal fuse --grammar ./benchmarks/grafter/oopsla-example.grammar

# binary-tree.grammar
racket ./run.rkt --interface Root --traversal fuse --grammar benchmarks/grafter/binary-tree.grammar

# fmm.grammar (this needs full examples)
racket ./run.rkt --interface VirtualRoot --traversal fuse --grammar benchmarks/grafter/fmm.grammar

# piecewise series
racket ./run.rkt --interface VirtualRoot --traversal fuse --grammar benchmarks/grafter/piecewise-exp1.grammar
racket ./run.rkt --interface VirtualRoot --traversal fuse --grammar benchmarks/grafter/piecewise-exp2.grammar
racket ./run.rkt --interface VirtualRoot --traversal fuse --grammar benchmarks/grafter/piecewise-exp3.grammar

# render (this needs full examples)
racket ./run.rkt --interface Document --traversal fuse --grammar benchmarks/grafter/render.grammar

# ast (this needs full examples, which takes a long time)
racket ./run.rkt --interface Program --traversal fuse --grammar benchmarks/grafter/ast.grammar
```

