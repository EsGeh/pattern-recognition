#!/bin/bash

INPUT="$(echo *.md)"
OUTPUT="$(basename $INPUT .md)".pdf

pandoc --number-offset=5 --number-sections -o "$OUTPUT" "createPDF/header" "$INPUT"
# pandoc --filter pandoc-citeproc --include-in-header="./createPDF/header" --number-sections -o "$OUTPUT" $(echo "$INPUT")
