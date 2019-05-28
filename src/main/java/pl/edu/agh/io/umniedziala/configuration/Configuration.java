package pl.edu.agh.io.umniedziala.configuration;

import com.moandjiezana.toml.Toml;

public class Configuration {
    private static Configuration instance;

    public static Configuration getInstance() {
        if (instance == null) {
            instance = new Configuration();
            instance.read();
        }
        return instance;
    }

    private Long checkInterval = 0L;
    private Long activityTime = 0L;
    private Long chartStart = 0L;
    private Long chartEnd = 0L;

    private Configuration() {
        GeneralConfigurationManager.getInstance();
    }

    private void read() {
        Toml config = GeneralConfigurationManager.getInstance().config();

        checkInterval = config.getLong("monitor.check_interval", 500L);
        activityTime = config.getLong("monitor.inactivity_period", 15L);
        chartStart = config.getLong("monitor.chart.start", 0L);
        chartEnd = config.getLong("monitor.chart.end", 23L);
    }

    public Long getCheckInterval() {
        return checkInterval;
    }

    public Long getInactivityPeriod() {
        return activityTime;
    }

    public Long getChartStart() {
        return chartStart;
    }

    public Long getChartEnd() {
        return chartEnd;
    }
}
