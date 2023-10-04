# Cranelift ISLE Verification Prototype

This crate is a prototype for verifying Cranelift's ISLE lowering rules using an SMT solver.

Currently, term semantics are specified manually in `src/isle_annotations.rs`. These should be replaces by annotations on ISLE terms that are then parsed to our Verification IR.  

## Running on a file

To run on a `.isle` file, run:

```bash
cargo run -- <path-to-file>
```

Right now, this will check equivalence of all rules that start with `lower` on the left hand side. 
The engine will also include ISLE definitions from these files:
- `cranelift/codegen/src/prelude.isle`
- `cranelift/codegen/src/prelude_lower.isle`
- `cranelift/codegen/src/inst_specs.isle`


## Testing

To see an examples of our current output, run tests without capturing standard out:
```bash
cargo test -- --nocapture
```

To run a specific rule, you can provide the test name (most rules are tested in `cranelift/isle/veri/veri_engine/tests/veri.rs`):

```bash
cargo test test_named_band_64 -- --nocapture  
```

To see the x86-64 CVE repro, run:

```bash
cargo run -- --noprelude -t amode_add -i examples/x86/amode_add_uextend_shl.isle
```

To see the x86-64 CVE variant with a 32-bit address, run:
```bash
cargo run -- --noprelude -t amode_add -i examples/x86/amode_add_shl.isle
```