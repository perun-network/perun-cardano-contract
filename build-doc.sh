#!/bin/bash

cabal haddock \
  --haddock-quickjump \
  --haddock-internal \
  --haddock-all \
  --haddock-hyperlink-source \
  --enable-documentation \
  perun-plutus

echo "Use the above directory to serve a static HTTP server (required if dynamic search with <S> hotkey is wanted)."
echo "If the quickjump utility is not required simply copy and pasting the above directory to your browser will suffice."
