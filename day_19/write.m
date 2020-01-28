function memory = write(offset, pointer, mem, relativeBase, value)
   memory = mem;
  mode = getMode(offset, pointer, memory);
  switch(mode)
    case 0
      memory(read(pointer + offset, memory) + 1 ) = value;
    case 1
      memory(pointer + offset + 1) = value;
    case 2
      memory(relativeBase+ read(pointer + offset, memory) + 1) = value;
    otherwise
      disp(["wrong write mode:",num2str(mode)]);
  endswitch
endfunction