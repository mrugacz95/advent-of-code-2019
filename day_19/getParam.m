function param = getParam(offset, pointer, mem, relativeBase)
    mode = getMode(offset, pointer, mem);
    switch(mode)
        case 0
            param = read(read(pointer + offset, mem), mem);
        case 1
            param = read(pointer + offset, mem);
        case 2
            param = read(relativeBase + read(pointer + offset, mem), mem);
        otherwise
            disp(["Unknown read mode", num2str(mode)]);
    endswitch
endfunction