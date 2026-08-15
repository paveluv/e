#!/bin/sh
# Download the Scheme reference corpus and build describe.sdata from it.
#
# Sources: The Scheme Programming Language, 4th edition (TSPL4), which
# documents R6RS, and the Chez Scheme User's Guide (CSUG), which
# documents the Chez extensions.  The downloaded pages and the
# extracted database stay in this directory and are gitignored; run
# this script once to enable M-x (describe ...) in the editor.
set -e
cd "$(dirname "$0")"

TSPL="binding control exceptions io libraries objects records syntax"
CSUG="binding compat control debug expeditor foreign io libraries
      numeric objects smgmt syntax system threads"

mkdir -p tspl4 csug
for p in $TSPL; do
  echo "tspl4/$p.html"
  curl -fsS "https://www.scheme.com/tspl4/$p.html" -o "tspl4/$p.html"
done
for p in $CSUG; do
  echo "csug/$p.html"
  curl -fsS "https://cisco.github.io/ChezScheme/csug10.0/$p.html" -o "csug/$p.html"
done

scheme --script extract.ss
