#!/bin/sh

dotnet run -- ./examples/24-entities-normal.yaml -o output.dot
dot -Tpdf output.dot -o output.pdf
evince output.pdf &

