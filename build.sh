#!/bin/sh

dotnet run -- ./examples/24-hand.yaml -o output.dot
dot -Tpdf output.dot -o output.pdf
evince output.pdf &

