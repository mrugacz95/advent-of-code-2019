package pl.mrugacz95.day8;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Day8 {
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
        long result = -1;
        for (int layerIdx = 0; layerIdx * WIDTH * HEIGHT < input.length(); layerIdx++) {
            String layer = input.substring(layerIdx * WIDTH * HEIGHT, (layerIdx + 1) * WIDTH * HEIGHT);
            long zeros = layer.chars().filter(it -> it == '0').count();
            if (zeros < minZeros) {
                minZeros = zeros;
                long ones = layer.chars().filter(it -> it == '1').count();
                long twos = layer.chars().filter(it -> it == '2').count();
                result = ones * twos;
            }
        }
        System.out.println(result);
    }
}
