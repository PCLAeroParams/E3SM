# PartMCSL source_partition MPI exchange — handoff

Continues the work from `partmcsl_readme.md`.  Goal of this slice: ghost-exchange
the `source_partition_t` so each rank knows the foreign records targeting its
own elements.  Particle-data exchange is deferred (separate task).

## What changed

Single file edited: `src/partmcsl/partmcsl_advection.F90`.

Added:

- `arrival_partition_t` — receiver-side mirror of `source_partition_t`.  Indexed
  by `(d, k, cj, je)` for the *receiving* element je owned by this rank.
  Stores `nsrc`, `src_gid`, `src_subcell`, `src_frac`.  Foreign-only — local
  self/neighbor contributions are read directly from `src_partition`.
- Module-level `partmcsl_ghostbuf : GhostBuffer3D_t` and `ghostbuf_initialized`
  flag.  `initGhostBuffer3D` in `partmcsl_init`, `FreeGhostBuffer3D` in
  `partmcsl_finalize`.
- Buffer-sizing parameters `pmcsl_payload_words`, `pmcsl_ghost_np=21`,
  `pmcsl_ghost_nhc=20` → `pmcsl_ghost_slot=441` ≥ payload (436).
- Pack/unpack helpers and the public driver
  `partmcsl_exchange_source_partition(par, ithr, elem, nets, nete)` =
  pack → `ghost_exchangeVfull` → unpack.
- Three tests (`test_identity_exchange`, `test_topology_coverage`,
  `test_sum_to_one`) called from `partmcsl_test`.

## Design summary (so future-you doesn't re-derive it)

**Key insight:** no global→local rank map is needed.  The pack-side rank A
knows the GlobalIDs of its element's neighbors (`fv_mesh%elem_global_id`), and
the receive-side rank B knows the GlobalIDs of *its* element's neighbors
(same field).  Records carry `(gid_dest, ci_dest, frac)` and B filters by
`gid_dest == elem(je)%GlobalID` at unpack.

**Payload layout per (ie, k), 1D, 436 reals (zero-padded to 441):**
- `payload(1..4)` — `ndest(k, ci=1..4, ie)` cast to real (trusted count at unpack)
- `payload(5..436)` — for each `ci=1..4`, max_ndest=36 record slots of
  `(gid_dest, ci_dest, frac)` triples.  Block stride per `ci` is `max_ndest*3`.

**Ints packed as reals.**  Asserted in `pack_payload` that `gid_dest < 2**52`.
`max_ndest=36` and `ci_dest∈[1,4]` are trivially safe.

**`dest_cell_idxs` decoding** — the C++ side stores a flat 0-based cell index
(see `partmcsl.hpp:init_local_mesh_if_needed`):
`cell_idx = nbr_idx * 4 + subcell_idx`.  `decode_local_dest_idx` reverses this
to 1-based (`in_dest`, `ci_dest`); `gid_dest = elem_global_id(1, in_dest, ie)`.

**Ghost-exchange primitive used:** `ghost_exchangeVfull` from `bndry_mod`
(theta-l).  We do NOT use `ghostVpack3d`/`ghostVunpack3d` because those bake
in GLL halo packing.  Instead the slot `partmcsl_ghostbuf%buf(:,:,k,is)` is
treated as a flat scratchpad (reshape from/to 1D `payload`).  Packing copies
the same payload to every neighbor slot via `loc2buf`/`putmapP_ghost`; unpack
reads via `getmapP_ghost` and gets the source GID via `desc%globalID(l)`.

**Batching:** all `nlev` levels exchanged in one call (`nlyr=nlev` in the
ghost buffer).  Comment in code mentions we may want per-level when the
particle exchange lands (larger payloads).

## What the three tests assert

1. **identity** (`test_identity_exchange`) — synthesize identity src_partition
   (each cell→itself, frac=1.0).  Every record has
   `gid_dest == source_elem%GlobalID`, never matches a foreign je's gid.
   Assert: `arrival_partition%nsrc` is zero everywhere on every rank.

2. **topology coverage** (`test_topology_coverage`) — synthesize uniform
   src_partition (each ie sends 1/nneighbors(ie) to each neighbor at same
   subcell).  Per owned je, expected `nsrc(k, cj, je) ==` count of *foreign*
   neighbors of je.  Each arrival record's `src_subcell == cj`, `src_gid` is
   in je's neighbor list (and not je itself), `src_frac` ∈ {1/8, 1/9}.

3. **sum-to-one** (`test_sum_to_one`) — same uniform setup.  Per (je, cj, k),
   `sum(local_src_partition contributions targeting (je,cj,k))` plus
   `sum(arrival_partition fracs)` equals
   `sum over n in je's neighbors of 1/nneighbors(n)`.  For owned n we know
   nneighbors(n) directly; for foreign n we read frac from arrival_partition
   at `(cj=1, k=1, src_gid=n)` (uniform setup guarantees a record exists).

Tolerance for sum check: `1e-12`.

## How to build / run

Convenience script at repo root: `run_homme_partmcsl.sh -c -b -r`.
Manually:

```
cmake -Wno-dev -C <homme_machine_file> -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=DEBUG \
      -DHOMME_USE_MKL=FALSE <e3sm-pclap>/components/homme
make -j 24 theta-l-nlev20-native
mpirun --map-by ppr:56:socket:PE=1 --bind-to hwthread --n 56 \
      <wdir>/test_execs/theta-l-nlev20-native/theta-l-nlev20-native \
      < <e3sm-pclap>/components/homme/partmcsl_dcmip12_transport.nl 2>&1 \
      | tee homme-out.txt
```

Tests run inside `partmcsl_test` (gated by `do_checks=.true.`).  Look for
log lines `partmcsl_test: identity exchange passed.` /
`partmcsl_test: topology coverage passed.` / `partmcsl_test: sum-to-one passed.`

## Open issues / things to verify after build

1. **Compile** — first thing.  New `use` clauses for `bndry_mod`,
   `edge_mod` (initGhostBuffer3D, FreeGhostBuffer3D), `edgetype_mod`
   (GhostBuffer3D_t).  These are routed through theta-l's `bndry_mod.F90` and
   `edge_mod.F90` shims.  If a circular dep shows up, that's the suspect.
2. **`partmcsl_test` runs at init** (called from `prim_main.F90:234`).  At
   that point partmcsl_init has run, so `fv_mesh` and the ghost buffer are
   ready.  The synthetic tests don't need any prior advection step.
3. **Single-rank run** — tests should still pass (no foreign neighbors →
   `nsrc` stays zero, sum-to-one degenerates to sum over owned neighbors only).
4. **Pre-existing bug noticed but NOT fixed:** `partmcsl.cpp:112` does
   `(elem_self_idx - 1) * n_subcells_per_elem` even though `elem_self_idx` is
   already 0-based (was `-1`'d at `partmcsl.cpp:199`).  Also the FA4 dim order
   for `dest_idx`/`dest_frac` at `partmcsl.cpp:77-78` lists `(nlev,
   max_ndest_cell, ...)` while Fortran allocates `(max_ndest, nlev, ...)`.
   These look like the failing assertions called out in `partmcsl_readme.md`.
   The MPI work I added is independent of those — but the synthetic tests
   bypass `calc_src_partition` entirely (they fill `src_partition` directly
   in Fortran), so they should pass even if the C++ indexing bugs remain.
5. **If sum-to-one fails at corner cells**: probable cause is foreign
   nneighbors mis-inferred.  Quick fix: add a 5th word to the payload header
   carrying `nneighbors(ie)` and use it directly instead of inferring from
   `src_frac`.  Bumps payload to 437 words; still fits in 441 slot.

## Where particle-exchange work picks up (deferred)

`partmcsl_step_forward` step 3 — the variable-length particle MPI is *not*
going through the ghost buffer.  The plan is to walk `Schedule(1)%SendCycle`
/ `RecvCycle` directly, do a sizes-handshake, then variable-length payload.
Per-level exchanges (not batched) likely needed because particle counts × cells
will dwarf the 441-real slot.  The arrival_partition built here is the
correct driving structure for that (each record says how many particles to
move from a foreign source cell to a local destination cell).
