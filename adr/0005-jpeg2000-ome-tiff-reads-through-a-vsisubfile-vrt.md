# 0005. JPEG-2000 OME-TIFF reads through a `/vsisubfile` VRT, not a conversion

- **Status:** Accepted
- **Date:** 2026-08-28
- **Supersedes:** —
- **Superseded by:** —

## Context

Loading Xenium morphology images required a Python environment. `to_simple_tif()`
sourced `inst/python/tif_convert.py` through reticulate to decode one page with
`tifffile` and rewrite it as a flat TIFF terra could open, and `tif_metadata()`
imported `tifffile` purely to reach the OME-XML string.

The obvious fix — probe `terra::rast()` first and only convert when it fails —
rests on a premise that does not hold. A survey of the morphology images on hand
found that the only file terra could open was the *cropped* Xenium fixture, whose
crop tool had re-encoded it to Deflate (recorded as a declared deviation in its
`crop_manifest.json`). Every vendor-original file is compression 34712,
JPEG-2000-in-TIFF:

| file | dims | comp | tiles | pyramid |
|---|---|---|---|---|
| cropped mini focus | 2000×2000 | 8 Deflate | 16 | – |
| lung cancer `morphology_focus_0000` | 51187×17098 | 34712 JP2K | 850 | 7 |
| lung cancer `morphology.ome.tif` | 51187×17098 | 34712 JP2K | 850 × 11 pages | 7 |
| Atera `morphology.ome.tif` | 46543×28048 | 34712 JP2K | 1288 × 10+ pages | 7 |
| lung cancer H&E | 11580×45087 ×3 | 8 Deflate | 540 | 5 |

libtiff has no JPEG-2000 codec, so GDAL 3.8.5 rejects those outright
(`Cannot open TIFF file due to missing codec JP2000`). A probe alone would
therefore fall through to Python on exactly the files that matter.

Two facts make a pure-R route possible anyway. Each tile of a JPEG-2000 TIFF is
a **complete image stream in its own right** -- a JP2 box in Xenium files
(`00 00 00 0c 6a 50 20 20`), a bare J2K codestream in Aperio ones
(`ff 4f ff 51`) -- and GDAL's `/vsisubfile/<offset>_<len>,<path>` VFS can
present a byte range as a file. The `JP2OpenJPEG` driver is already compiled into the GDAL that ships with
terra. Separately, the OME-XML is plain TIFF tag 270, reachable with `readBin`
without decoding a pixel.

Measurements, single 414 MB JP2K page, 16 CPUs, runs serialized under
`caffeinate -dimsu`, peak RSS from `/usr/bin/time -l`. R's ~0.2 GB floor is the
interpreter:

| operation | R (terra + GDAL) | Python (tifffile) |
|---|---|---|
| read OME-XML metadata | 0.015 s / 0.22 GB | 0.001 s / 0.03 GB |
| open image lazily | 0.057 s / 0.25 GB | no lazy equivalent |
| plot-sized read (pyramid lvl 4) | 0.104 s / 0.30 GB | 0.263 s / 0.11 GB |
| 2048² full-res ROI | 0.264 s / 0.47 GB | 3.699 s / 2.41 GB |
| full page → Deflate TIFF | 6.6 s / 1.01 GB | 5.4 s / 2.41 GB |

The ROI row is the reason for the decision: Python must decode all 875M pixels
to reach any window. Conversion is the one case Python still wins on time, and
it is the step this design removes from the default path.

Correctness was established against `tifffile` on the real 51187×17098 file: a
single tile matched exactly, full-raster stats matched exactly
(min 0, max 10866, mean 459.5687), and the VRT compared against the flat TIFF the
old pipeline produced — read the same way, `terra::rast(noflip = TRUE)` — differed
in **0 pixels** at three ROIs, with identical extents.

## Decision

`.create_terra_spatraster()` becomes a three-rung ladder: read directly with
`terra::rast()`; failing that, build a VRT mosaicking the page's JPEG-2000 tiles
through `/vsisubfile/` with SubIFD levels wired in as `<Overview>`; failing that,
fall back to `to_simple_tif()`. `tif_metadata()` reads tag 270 in pure R and only
falls back to `tifffile` for formats whose metadata lives in private binary tags.

VRTs are written under `tempdir()` and cached per session. `@file_path` keeps the
original `.ome.tif`, never the VRT.

The routing predicate accepts compression 34712 (Xenium) and 33003/33005
(Aperio), any sample count, tiled only.

## Consequences

- No Python for any Xenium image, or any Deflate/LZW OME-TIFF. `to_simple_tif()`
  and `tif_convert.py` stay for qptiff and codecs GDAL cannot reach.
- Nothing is written next to the user's data any more. `tif_exports/` is still
  honoured when a previous run left one behind.
- **`@file_path` must stay the original file.** `reconnect()` routes through
  `.create_terra_spatraster()`, so a session that has lost its tempdir rebuilds
  the VRT transparently. Storing the VRT path there would strand the object.
  `saveGiotto()` is unaffected — it materialises images with `writeRaster()`.
- Full-resolution conversion, if ever wanted, must pass
  `-co NUM_THREADS=ALL_CPUS`; without it GDAL single-threads Deflate and takes
  26 s instead of 6.6 s. Route it through `sf::gdal_utils("translate")` rather
  than `terra::writeRaster()`, which peaked at 9.7 GB against GDAL's 1.0 GB.
- Aperio SVS whole-slide images come along for free, which was not the goal.
- Revisit if GDAL gains a JPEG-2000 codec inside its GTiff driver, at which
  point rung 2 becomes dead code and should be deleted rather than maintained.

## Scope, and why it is wider than Xenium

Nothing in the reader in `R/tif.R` names a vendor; routing is driven
by the compression and tile tags alone. The first draft was narrower -- 34712 and
single-sample only -- purely because no other JPEG-2000 file was available to
test against, and a guess about tile-to-band mapping is the kind of thing that
fails silently.

An Aperio SVS then turned up and removed the excuse. Its tiles are bare J2K
codestreams under compression 33005 with `SamplesPerPixel = 3`, and a per-band
VRT reproduced page 5 (2305x915, 40 tiles) **byte-identically to tifffile** on
all three bands, position-weighted checksums included. Both guards were
therefore relaxed rather than documented as limitations. `inst/extdata/
toy_jp2k_rgb.tif` (8 KB, two real Aperio tiles) pins this in CI.

What is still declined: strip-based JPEG-2000, which no file in the wild has
shown; and qptiff, for which there is still no test file. Both fall through to
`to_simple_tif()`.

## Alternatives considered

- **Capability probe alone, convert on failure** — what the investigation set out
  to do. Fails on every vendor-original file, because they are all JP2K.
- **Decode tiles in R and reassemble** — works (verified), but materialises the
  whole page in memory and gives up lazy windowed access, which is where the
  win is.
- **Write the VRT next to the data, in `tif_exports/`** — survives the session, so
  `loadGiotto` would need no regeneration. Rejected: it writes into the vendor
  output directory for no benefit, since `saveGiotto()` already materialises
  images and `reconnect()` rebuilds in 0.06 s.
- **Rust** — the only hard part is JPEG-2000 decoding, and there is no mature
  pure-Rust decoder; `jpeg2k`/`openjpeg-sys` bind the same OpenJPEG C library
  GDAL already ships. It would add a cargo/extendr toolchain and a
  cross-platform build burden for no decode advantage. The one thing it could
  help with, parallel tile orchestration during conversion, is already covered
  by `NUM_THREADS=ALL_CPUS`.
- **`RBioFormats`** — handles JP2K-in-TIFF, but trades a Python dependency for a
  Java one.
- **Requiring a newer GDAL** — outside our control, and GDAL's GTiff driver still
  has no JPEG-2000 codec regardless of version.

## References

- `R/tif.R` (directory reader, VRT builder, converters), and
  `.create_terra_spatraster()` in `R/images.R`, which consumes them
- `inst/scripts/make_toy_jp2k.R` — how the JPEG-2000 test fixture was built, and
  why the Deflate mini dataset could not serve as one
