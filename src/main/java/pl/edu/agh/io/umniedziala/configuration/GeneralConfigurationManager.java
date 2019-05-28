package pl.edu.agh.io.umniedziala.configuration;

import com.moandjiezana.toml.Toml;
import com.moandjiezana.toml.TomlWriter;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Paths;
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

    private void read() throws ConfigurationException {

        URL defaultConfigFileUrl = Objects.requireNonNull(
                getClass().getClassLoader().getResource(DEFAULT_CONFIGURATION_PATH)
        );

        File defaultConfigFile = new File(defaultConfigFileUrl.getFile());

        if (!defaultConfigFile.exists()) {
            throw new ConfigurationException("Default Configuration File does not exists!");
        }

        Toml defaultConfig = new Toml().read(defaultConfigFile);

        if (!CONFIGURATION_FILE.exists()) {
            configuration = new Toml(defaultConfig);
        } else {
            configuration = new Toml(defaultConfig).read(CONFIGURATION_FILE);
        }

        this.save();
    }

    private void save() {
        TomlWriter tw = new TomlWriter();
        try {
            tw.write(configuration.toMap(), CONFIGURATION_FILE);
        } catch (IOException e) {
            e.printStackTrace();
        }
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
