#!/bin/sh

dotnet run
dot -T pdf output.dot -o output.pdf
evince output.pdf &

