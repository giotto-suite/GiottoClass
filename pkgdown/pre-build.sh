#!/usr/bin/env bash
# Repo-specific preparation, run by the shared pkgdown workflow
# (giotto-suite/.github) from the directory pkgdown will build from.
#
# DEV ONLY. This file lives in the repo, so the shared workflow runs it for
# every mode and hands it SITE_MODE to tell them apart. Everything below would
# corrupt the release build, which documents v0.5.1.
set -euo pipefail

if [ "${SITE_MODE:-release}" != "dev" ]; then
  echo "pre-build: release mode, nothing to do"
  exit 0
fi

# 1. Reference entries for gsource-only topics.
#
# These cannot live in _pkgdown.yml: the release build reads the same file
# against v0.5.1, where the topics do not exist, and pkgdown hard-errors on a
# listed topic it cannot find. Spliced ahead of the `internal` section.
awk '/^- title: internal$/ && !done {
       while ((getline line < "pkgdown/reference-gsource.yml") > 0)
         if (line !~ /^#/) print line
       done = 1
     } {print}' _pkgdown.yml > _pkgdown.tmp
mv _pkgdown.tmp _pkgdown.yml
echo "pre-build: spliced gsource reference entries ($(grep -c 'New on the gsource line' _pkgdown.yml) marker, expect 1)"

# 2. spatial_geometries.Rmd does not survive gsource. Its subsetting chunk fails in
#     [ -> subset(x, cell_ids = i) -> .subset_giotto_polygon_object()
# which returns a zero-length object, so plot() has nothing to draw. The same
# vignette renders fine against the released 0.5.1, and R/subset.R was rewritten
# on gsource -- a package behaviour change, not a documentation problem.
#
# Remove this block once polygon subsetting by cell ID is settled.
rm -f vignettes/spatial_geometries.Rmd

# Drop its navbar entry too, or the menu links to a page that will not exist.
# One-line delay so the `- text:` line above the href goes with it. Uses a
# `have` flag rather than testing prev for emptiness, which would silently
# swallow every blank line in the file.
awk '{ if ($0 ~ /articles\/spatial_geometries\.html/) { have=0; next }
       if (have) print prev
       prev=$0; have=1 }
     END { if (have) print prev }' _pkgdown.yml > _pkgdown.tmp
mv _pkgdown.tmp _pkgdown.yml
echo "pre-build: excluded spatial_geometries.Rmd (gsource subsetting change)"
