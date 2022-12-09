#!/bin/bash
NAME=${1:-dayX}

cargo new "$NAME"

cp ./day1/.gitignore "./$NAME/." || true

cp -r ./template.rs "./$NAME/src/main.rs" || true
