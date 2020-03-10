if gnatmake $1 -o output; then
  ./output
else
  echo "Compilation failed"
fi