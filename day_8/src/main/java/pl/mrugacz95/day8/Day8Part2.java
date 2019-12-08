package pl.mrugacz95.day8;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Day8Part2 {
    private static final int WIDTH = 25;
    private static final int HEIGHT = 6;

    public static void main(String[] args) {
        String input;
        try {
            BufferedReader reader = new BufferedReader(new FileReader("day_8.in"));
            input = reader.readLine();
            reader.close();
        } catch (IOException e) {
            System.out.println("Couldn't read input file");
            return;
        }
        long minZeros = Long.MAX_VALUE;
        StringBuilder result = new StringBuilder();
        for (int y = 0; y < HEIGHT; y++) {
            for (int x = 0; x < WIDTH; x++) {
                colorFind:
                for (int layerIdx = 0; layerIdx * WIDTH * HEIGHT < input.length(); layerIdx++) {
                    String layer = input.substring(layerIdx * WIDTH * HEIGHT, (layerIdx + 1) * WIDTH * HEIGHT);
                    char color = layer.charAt(WIDTH * y + x);
                    switch (color) {
                        case '0':
                            result.append(' ');
                            break colorFind;
                        case '1':
                            result.append('\u2588');
                            break colorFind;
                    }
                }
            }
            result.append("\n");
        }

        System.out.println(result.toString());
    }
}