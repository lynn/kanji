#!/bin/sh

FREQ=frequency-2501.txt
JOYO=joyo.txt

OUTPUT=
OUTPUT2=

# get frequent joyo in order
for f in `cat "$FREQ"`; do
	if grep "$f" "$JOYO" >/dev/null; then
		OUTPUT="$OUTPUT$f"
	fi
done

# get rare joyo in 適当 order
for j in `cat "$JOYO"`; do
	if ! echo "$OUTPUT" | grep "$j" >/dev/null; then
		OUTPUT2="$OUTPUT2$j"
	fi
done

echo "$OUTPUT"
echo "$OUTPUT2"
