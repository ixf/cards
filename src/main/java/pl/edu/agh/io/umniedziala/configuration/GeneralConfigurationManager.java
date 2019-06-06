package pl.edu.agh.io.umniedziala.configuration;

import com.moandjiezana.toml.Toml;
import com.moandjiezana.toml.TomlWriter;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Objects;

public class GeneralConfigurationManager {
    private final static File CONFIGURATION_FILE = Paths.get("config.toml").toAbsolutePath().toFile();
    private final static String DEFAULT_CONFIGURATION_PATH = "config.toml";

    private Toml configuration = new Toml();

    private static GeneralConfigurationManager instance;

    public static GeneralConfigurationManager getInstance() {
        if (instance == null) {
            instance = new GeneralConfigurationManager();
            instance.read();
        }
        return instance;
    }

    private synchronized void read() throws ConfigurationException {

        Toml defaultConfig = getDefaultConfiguration();

        if (!CONFIGURATION_FILE.exists()) {
            configuration = new Toml(defaultConfig);
        } else {
            configuration = new Toml(defaultConfig).read(CONFIGURATION_FILE);
        }

        this.save();
    }

    private Toml getDefaultConfiguration() {
        return new Toml().read(getClass().getClassLoader().getResourceAsStream(DEFAULT_CONFIGURATION_PATH));
    }

    public void resetToDefaults() {
        configuration = getDefaultConfiguration();

        this.save();
    }

    public synchronized void save() {
        TomlWriter tw = new TomlWriter();
        try {
            tw.write(configuration.toMap(), CONFIGURATION_FILE);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public synchronized void setConfigurationFromMap(Map<String, Object> map) {
        TomlWriter tw = new TomlWriter();
        try {
            tw.write(map, CONFIGURATION_FILE);
        } catch (IOException e) {
            e.printStackTrace();
        }

        this.read();
    }

    public Toml config() {
        return this.configuration;
    }

    public class ConfigurationException extends RuntimeException {
        ConfigurationException(String s) {
            super(s);
        }
    }
}
