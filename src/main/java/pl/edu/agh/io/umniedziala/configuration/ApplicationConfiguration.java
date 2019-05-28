package pl.edu.agh.io.umniedziala.configuration;

import com.moandjiezana.toml.Toml;
import com.moandjiezana.toml.TomlWriter;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Objects;

public class ApplicationConfiguration {
    public final static File CONFIGURATION_FILE = Paths.get("./config.toml").toAbsolutePath().toFile();
    public final static String DEFAULT_CONFIGURATION_PATH = "/resources/config.toml";

    private Toml configuration = new Toml();

    private static ApplicationConfiguration instance;

    public static void initialize() throws ConfigurationException {
        instance = new ApplicationConfiguration();
        instance.read();
    }

    public static ApplicationConfiguration getInstance() {
        return instance;
    }

    public void read() throws ConfigurationException {
        File defaultConfigFile = new File(
                Objects.requireNonNull(
                        getClass().getClassLoader().getResource(DEFAULT_CONFIGURATION_PATH)
                ).toString()
        );

        if (!defaultConfigFile.exists()) {
            throw new ConfigurationException("Default Configuration File does not exists!");
        }

        Toml defaultConfig = new Toml().read(defaultConfigFile);

        configuration = new Toml(defaultConfig).read(CONFIGURATION_FILE);

        this.save();
    }

    public void save() {
        TomlWriter tw = new TomlWriter();
        try {
            tw.write(configuration, CONFIGURATION_FILE);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public final Toml config() {
        return this.configuration;
    }

    public class ConfigurationException extends RuntimeException {
        public ConfigurationException(String s) {
            super(s);
        }
    }
}
