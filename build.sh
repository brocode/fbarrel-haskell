#!/bin/bash

docker run --rm -v $(pwd):/data samdoshi/haskell-stack  bash -c "cd /data && stack build --allow-different-user"
