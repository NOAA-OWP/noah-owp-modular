# fortran-messagepack (vendored)

Third-party MessagePack serialization library for Fortran, used to encode and decode model state
snapshots.

| | |
|---|---|
| Upstream | https://github.com/synthfi/fortran-messagepack |
| Commit | `2b2c9fc78bf3f7212ec9e35c675febdd508b7830` (2025-02-10) |
| License | MIT, Copyright (c) 2025 Kelly Schultz — see [LICENSE](LICENSE) |

## Where the sources are, and why

The four `.f90` files are **in `src/`**, not in this directory:

    src/byte_utilities.f90
    src/messagepack_value.f90
    src/messagepack_user.f90
    src/messagepack.f90

They have to be. ngen builds this repository through its own listfile
(`extern/noah-owp-modular/CMakeLists.txt` in the ngen tree), which globs `src/`, `bmi/` and
`driver/` only. Sources anywhere else are never compiled, and `src/StateSerialization.f90` then
fails to find `messagepack.mod`. This directory holds the license and provenance so the
third-party boundary is still recorded somewhere deliberate.

They are vendored unmodified **except for the edits listed under "Local modifications"**. Keep it
that way: local edits are lost at the next re-vendor, and near byte-identity is what makes the
check below meaningful. Behavior changes belong in the calling code. Upstream's build files,
tests, and example app are not vendored.

`CMakeLists.txt` keeps them in their own `NOAHOWP_MESSAGEPACK_SOURCES` list rather than folding
them into the model sources, so the boundary stays visible in the build too.

## Local modifications

`src/messagepack_user.f90` differs from upstream at five places.

Four are the `class default` arms of the `select type (mpv)` constructs in `unpack_array`,
`unpack_map` (twice) and `unpack_ext`. Upstream has `deallocate(mpv)` inside each arm. Here the
`deallocate` is moved to just after `end select`, guarded by `if (.not. successful)`, and the
three sites inside `do` loops also `return`.

The fifth is one added line at the top of `unpack_value`: `byteadvance = 0` next to the
`successful = .true.` default. Upstream returns from the two early error paths just below
(`'buffer is empty'`, `'insufficient size'`) without ever assigning `byteadvance`, which is
`intent(out)`, so its value on return is undefined. `unpack_buf` then compares that value against
the buffer size. Whatever happens to be in that memory decides the outcome: a small leftover
value skips the comparison harmlessly, while a large one sends the code into an error-reporting
branch that writes into `error_message` — a deferred-length string of length zero at that point —
and an internal write cannot grow such a string, so the program aborts with an end-of-record
error. Which of those a given build sees depends on the compiler, optimisation level and what
the stack held beforehand, which is why the malformed-payload tests can pass with one toolchain
and abort with another. `check_size` in the same module already sets `byteadvance = 1` as its
default; this just makes `unpack_value` do the same.

Why the first four: inside `select type (mpv)` the name `mpv` is the associate name, which the standard says
"does not have the ALLOCATABLE or POINTER attributes" (Fortran 2023 §11.1.3.3; the same rule is
in every standard since Fortran 2003, §16.4.1.5). Deallocating it is a constraint violation.
Intel `ifx` rejects it with `error #6724: An allocate/deallocate object must have the
ALLOCATABLE or POINTER attribute. [MPV]`, so the library does not build under Intel. gfortran
does not diagnose it. After `end select` the name refers to the declared allocatable dummy
again, where `deallocate` is conforming. The `return` matches the sibling error path a few lines
above each site; without it the loop would continue and `select type` on the deallocated
variable. All four arms are unreachable in practice (`mpv` is assigned a concrete type on the
line before each `select type`), so no payload changes.

The failure was first observed when an Intel compiler job was added to CI, and diagnosed and
fixed in PR #135, which also has the full write-up. Upstream had no activity after the vendored
commit when this was applied, so the edit was kept local. If it is re-vendored, re-apply or
confirm upstream has fixed it. The `diff` below reports `DIFFERS: messagepack_user`; that is
expected, and the diff hunks should be exactly these five.

## Verifying or updating

```sh
git clone https://github.com/synthfi/fortran-messagepack.git /tmp/fmp
git -C /tmp/fmp checkout 2b2c9fc78bf3f7212ec9e35c675febdd508b7830
for f in byte_utilities messagepack_value messagepack_user messagepack; do
    diff "/tmp/fmp/src/$f.f90" "src/$f.f90" || echo "DIFFERS: $f"
done
```

To update, repeat against the new commit, copy the four files across, re-apply the local
modifications above if upstream still needs them, refresh the commit SHA above, and re-run the
test suite — the state payload layout depends on how this library packs values.