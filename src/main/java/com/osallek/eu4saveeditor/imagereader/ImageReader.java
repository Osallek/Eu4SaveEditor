package com.osallek.eu4saveeditor.imagereader;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

public class ImageReader {

    private ImageReader() {}

    public static BufferedImage convertFileToImage(File file) throws IOException {
        if (file == null ||!file.exists()) {
            return null;
        }

        if (file.toString().endsWith(".tga")) {
            return ImageIO.read(file);
        } else if (file.toString().endsWith(".dds")) {
            try (FileInputStream fis = new FileInputStream(file)) {
                byte[] buffer = new byte[fis.available()];
                fis.read(buffer);

                int[] pixels = DDSReader.read(buffer, DDSReader.ARGB, 0);
                int width = DDSReader.getWidth(buffer);
                int height = DDSReader.getHeight(buffer);
                BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
                image.setRGB(0, 0, width, height, pixels, 0, width);

                return image;
            }
        } else {
            return null;
        }
    }
}
