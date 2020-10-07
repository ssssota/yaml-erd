#!/bin/sh

dotnet run
dot -Kfdp -n -Tpdf output.dot -o output.pdf
evince output.pdf &

