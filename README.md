## initialH: Experimental Tree Traversal Synthesis

### Domain-Specific Encoding

(The `tracing`/`master` branch: domain-specific encoding.)

### Benchmark Commands

```bash
# render-toy-vector.grammar
racket ./run.rkt --root Box layout benchmarks/grafter/render-toy-vector.grammar

# render-toy-linked.grammar
racket ./run.rkt --root Box layout benchmarks/grafter/render-toy-linked.grammar

# toy.grammar for layout engine
racket ./run.rkt --root Tree layout benchmarks/toy.grammar

# memory-bug.grammar
racket ./run.rkt --root Function fuse benchmarks/testing/memory-bug.grammar

# hv-toy.grammar
racket ./run.rkt --root HVBox fuse benchmarks/grafter/hv-toy.grammar

# oopsla-example.grammar
racket ./run.rkt --root Root fuse benchmarks/grafter/oopsla-example.grammar

# binary-tree.grammar
racket ./run.rkt --root Root fuse benchmarks/grafter/binary-tree.grammar

# fmm.grammar (this needs full examples)
racket ./run.rkt --root VirtualRoot fuse benchmarks/grafter/fmm.grammar

# piecewise series
racket ./run.rkt --root VirtualRoot fuse benchmarks/grafter/piecewise-exp1.grammar
racket ./run.rkt --root VirtualRoot fuse benchmarks/grafter/piecewise-exp2.grammar
racket ./run.rkt --root VirtualRoot fuse benchmarks/grafter/piecewise-exp3.grammar

# render (this needs full examples)
racket ./run.rkt --root Document fuse benchmarks/grafter/render.grammar

# ast (this needs full examples, which takes a long time)
racket ./run.rkt --root Program fuse benchmarks/grafter/ast.grammar

# ast-list (this needs full examples, which takes a long time)
racket ./run.rkt --root Program fuse benchmarks/grafter/ast-list.grammar
```

