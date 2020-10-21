#!/bin/sh

dotnet run
dot -Tpdf output.dot -o output.pdf
evince output.pdf &

