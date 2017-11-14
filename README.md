$  git clone git@github.com:seereason/markdown-editor-chili.git
$  cd markdown-editor-chili/editor-client/
$  cabal sandbox init
$  cabal sandbox add-source ../editor-common
$  cabal install
$  cd ../editor-server/
$  cabal sandbox init
$  cabal sandbox add-source ../editor-common
$  cabal install
$  ./.cabal-sandbox/bin/editor-server 

http://localhost:8000/editor/index.html

