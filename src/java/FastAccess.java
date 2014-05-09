import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.nio.FloatBuffer;

import org.lwjgl.Sys;
import org.lwjgl.BufferUtils;
import org.lwjgl.LWJGLException;
import org.lwjgl.input.Keyboard;
import org.lwjgl.opengl.ContextAttribs;
import org.lwjgl.opengl.Display;
import org.lwjgl.opengl.DisplayMode;
import org.lwjgl.opengl.GL11;
import org.lwjgl.opengl.GL13;
import org.lwjgl.opengl.GL15;
import org.lwjgl.opengl.GL20;
import org.lwjgl.opengl.GL30;
import org.lwjgl.opengl.PixelFormat;

public class FastAccess {
    // Entry point for the application
    public static void main(String[] args) {
        new FastAccess();
    }
    
    public FastAccess() {
    }

    public static void setTexture(float[] data, int dim0, int dim1, int x, int y, int layer, int t) {
        int offset = layer * (dim0 * dim1 * 40) + (x * dim1 + y) * 40;
        int tx = t % 10; 
        int ty = t / 10;
        float frac = 10f/512f;

        data[offset +  8] = tx * frac;
        data[offset +  9] = ty * frac;

        data[offset + 18] = tx * frac;
        data[offset + 19] = (ty + 1) * frac;

        data[offset + 28] = (tx + 1) * frac;
        data[offset + 29] = (ty + 1) * frac;

        data[offset + 38] = (tx + 1) * frac;
        data[offset + 39] = ty * frac;
    }

    public static double calcSine(double x, double y, double elapsed) {
        return Math.sin(Math.sqrt(x*x + y*y)/3.0 + elapsed/1000.0);
    }

    public static void setColor(float[] data, int dim0, int dim1, int x, int y, int layer, float r, float g, float b, float a) {
        int offset = layer * (dim0 * dim1 * 40) + (x * dim1 + y) * 40;
        data[offset +  4] = data[offset + 14] = data[offset + 24] = data[offset + 34] = r; 
        data[offset +  5] = data[offset + 15] = data[offset + 25] = data[offset + 35] = g; 
        data[offset +  6] = data[offset + 16] = data[offset + 26] = data[offset + 36] = b; 
        data[offset +  7] = data[offset + 17] = data[offset + 27] = data[offset + 37] = a; 
    }
}

