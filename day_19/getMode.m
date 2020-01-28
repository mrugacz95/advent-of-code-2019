function mode = getMode(offset, pointer, mem)
  mode = idivide(mem(pointer + 1), 100);
  for i = 1:offset-1
    mode = idivide(mode,10);
  endfor
  mode = mod(mode, 10);
endfunction