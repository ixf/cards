package pl.edu.agh.io.umniedziala.configuration;

import com.moandjiezana.toml.Toml;

public class Configuration {
    private Long refreshInterval = 0L;

    private static Configuration instance;

    private Configuration() {
        GeneralConfigurationManager.getInstance();
    }

    public static Configuration getInstance() {
        if(instance == null) {
            instance = new Configuration();
            instance.read();
        }
        return instance;
    }

    private void read() {
        Toml config = GeneralConfigurationManager.getInstance().config();

        refreshInterval = config.getLong("monitor.refresh_interval");
    }

    public Long getRefreshInterval() {
        return refreshInterval;
    }
}
