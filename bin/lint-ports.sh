#!/usr/bin/bash

for folder in */ ; do
  echo "Checking ${folder}"

  ( cd "${folder}" || exit ; duniverse-lint )
done
