## initialH: Experimental Tree Traversal Synthesis

### General-Purpose Encoding with Verifier Support / CEGIS

(The `checking` branch: general-purpose encoding.)

### Known Issues

- ~~Enabling full examples makes `ast` very slow.~~

- ~~Uninterpreted function parsing/interpretation is not yet supported.~~
  - ~~This affects (including): `hv-toy.grammar`,  `fmm.grammar`.~~
- ~~Attributes marked as `input` are currently not set.~~
  - ~~This affects (including): `piecewise`.~~
- ~~Switch to matrix encoding of a hole to improve performance.~~
  - ~~This affects all complex benchmarks.~~

### Testing Commands

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

```bash
# toy.grammar
racket ./run.rkt --interface Tree --traversal layout --grammar benchmarks/toy.grammar

# hv-toy.grammar
racket ./run.rkt --interface HVBox --traversal fuse --grammar benchmarks/grafter/hv-toy.grammar

# hv-toy-list.grammar
racket ./run.rkt --interface HVBox --traversal fuse --grammar benchmarks/grafter/hv-toy-list.grammar

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

