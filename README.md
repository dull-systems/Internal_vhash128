# .Internal vhash128 [![status](http://assets.dull.systems:8080/status?id=Internal_vhash128/README.md)](https://dull.systems/Internal_vhash128 "")

Dependency-free R hash function. Produces 128-bit hashes of raw(n) data.

# Problem
You need a non-cryptographic hash function to create unique identifiers. You don't expect to feed it adversarial data.
You are willing to sacrifice speed to avoid depending on yet another R package.

# Solution
Drop `IVH` somewhere in your codebase. Hash with `IVH$hash(raw_serialized_data)`.

# Alternatives
If you're not willing to take an extra dependency, serialize your data, write it to a temporary file and hash it
with `tools::md5sum()`.

If you can live with an extra dependency, maybe pick one implementation of the popular XXH128:
- xxhashlite: `xxhashlite::xxhash_raw(input, algo = 'xxh128')`. The fastest of the three.
- digest: `digest::digest(input, algo = 'xxh3_128', serialize = FALSE)`. Maintained by Dirk Eddelbuettel. His packages
have an excellent track record of backwards compatibility.
- rlang: `rlang::hash(input)`. Many R packages depend on `rlang`. Maybe one of your dependencies already depends on it.
It does _not_ guarantee consistent hashes across R versions.
