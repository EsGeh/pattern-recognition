#!/bin/bash

OUTPUT=Uebung3.pdf
INPUT=Uebung3.md

pandoc --number-offset=3 --number-sections -o "$OUTPUT" "createPDF/header" "$INPUT"
# pandoc --filter pandoc-citeproc --include-in-header="./createPDF/header" --number-sections -o "$OUTPUT" $(echo "$INPUT")
