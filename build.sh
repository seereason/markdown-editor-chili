# git clone git@github.com:seereason/markdown-editor-chili.git
set -x

( cd editor-client ;
  cabal sandbox init ;
  cabal sandbox add-source ../editor-common ;
  cabal install )
( cd editor-server ;
  cabal sandbox init ;
  cabal sandbox add-source ../editor-common ;
  cabal install ;
  echo Connect to http://localhost:8000/editor/index.html ;
  echo starting server ;
  ./.cabal-sandbox/bin/editor-server
  )



