#!/bin/sh

dotnet run -- ./yagisan.yml -o output.dot
dot -Tpdf output.dot -o output.pdf
evince output.pdf &

