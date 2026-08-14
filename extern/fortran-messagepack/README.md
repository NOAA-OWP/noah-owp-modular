# fortran-messagepack (vendored)

Third-party MessagePack serialization library for Fortran, used to encode and decode model state
snapshots.

| | |
|---|---|
| Upstream | https://github.com/synthfi/fortran-messagepack |
| Commit | `2b2c9fc78bf3f7212ec9e35c675febdd508b7830` (2025-02-10) |
| License | MIT, Copyright (c) 2025 Kelly Schultz — see [LICENSE](LICENSE) |

The sources in `src/` are vendored unmodified. Keep them that way: local edits here are lost at the
next re-vendor, and byte-identity is what makes the check below meaningful. Behavior changes belong
in the calling code.

Upstream's build files, tests, and example app are not vendored — these sources are compiled
directly by this repository's build.

## Verifying or updating

```sh
git clone https://github.com/synthfi/fortran-messagepack.git /tmp/fmp
git -C /tmp/fmp checkout 2b2c9fc78bf3f7212ec9e35c675febdd508b7830
diff -r /tmp/fmp/src extern/fortran-messagepack/src   # expect: only upstream's CMakeLists.txt
```

To update, repeat against the new commit, copy the four `.f90` files across, refresh the commit SHA
above, and re-run the test suite — the state payload layout depends on how this library packs values.

## Divergence from NGWPC's copy

NGWPC/noah-owp-modular vendors these same files into `src/`, with one undocumented change:

```diff
 ! messagepack_value.f90, mp_float_type
-        logical :: is_64 = .true.
+        logical :: is_64 = .false.
```

Not carried here because it is inert. `is_64` is a default initializer reached only by a
default-constructed `mp_float_type`; construction goes through the `mp_float_type` generic
interface, whose `new_real32` and `new_real64` specifics both assign it explicitly.
