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

They are vendored **unmodified**. Keep them that way: local edits are lost at the next re-vendor,
and byte-identity is what makes the check below meaningful. Behavior changes belong in the calling
code. Upstream's build files, tests, and example app are not vendored.

`CMakeLists.txt` keeps them in their own `NOAHOWP_MESSAGEPACK_SOURCES` list rather than folding
them into the model sources, so the boundary stays visible in the build too.

## Verifying or updating

```sh
git clone https://github.com/synthfi/fortran-messagepack.git /tmp/fmp
git -C /tmp/fmp checkout 2b2c9fc78bf3f7212ec9e35c675febdd508b7830
for f in byte_utilities messagepack_value messagepack_user messagepack; do
    diff "/tmp/fmp/src/$f.f90" "src/$f.f90" || echo "DIFFERS: $f"
done
```

To update, repeat against the new commit, copy the four files across, refresh the commit SHA
above, and re-run the test suite — the state payload layout depends on how this library packs
values.