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

# 1b. spat_net_to_igraph() was removed on gsource (replaced by as.igraph()), but
# the release index still lists it, and pkgdown errors on a listed topic it
# cannot find. Drop the entry for dev only.
sed -i.bak '/^ *- spat_net_to_igraph$/d' _pkgdown.yml && rm -f _pkgdown.yml.bak
echo "pre-build: dropped spat_net_to_igraph (removed on gsource)"
