package pl.edu.agh.io.umniedziala.configuration;

import com.moandjiezana.toml.Toml;

import java.util.HashMap;
import java.util.Map;

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
    private Long inactivityPeriod = 0L;
    private Long chartStart = 0L;
    private Long chartEnd = 0L;

    private Configuration() {
        GeneralConfigurationManager.getInstance();
    }

    private void read() {
        Toml config = GeneralConfigurationManager.getInstance().config();

        checkInterval = config.getLong("monitor.check_interval", 500L);
        inactivityPeriod = config.getLong("monitor.inactivity_period", 15L);
        chartStart = config.getLong("monitor.chart.start", 0L);
        chartEnd = config.getLong("monitor.chart.end", 23L);
    }

    public void resetToDefaults() {
        GeneralConfigurationManager.getInstance().resetToDefaults();
        this.read();
    }

    public Long getCheckInterval() {
        return checkInterval;
    }

    public Long getInactivityPeriod() {
        return inactivityPeriod;
    }

    public Long getChartStart() {
        return chartStart;
    }

    public Long getChartEnd() {
        return chartEnd;
    }

    public void setCheckInterval(Long checkInterval) {
        this.checkInterval = checkInterval;

        Toml config = GeneralConfigurationManager.getInstance().config();

        Map<String, Object> map = config.toMap();

        ((HashMap<String, Object>)map.get("monitor")).put("check_interval", checkInterval);

        GeneralConfigurationManager.getInstance().setConfigurationFromMap(map);

        this.read();
    }

    public void setInactivityPeriod(Long inactivityPeriod) {
        this.inactivityPeriod = inactivityPeriod;

        Toml config = GeneralConfigurationManager.getInstance().config();

        Map<String, Object> map = config.toMap();

        ((HashMap<String, Object>)map.get("monitor")).put("inactivity_period", inactivityPeriod);


        GeneralConfigurationManager.getInstance().setConfigurationFromMap(map);

        this.read();
    }

    public void setChartStart(Long chartStart) {
        this.chartStart = chartStart;

        Toml config = GeneralConfigurationManager.getInstance().config();

        Map<String, Object> map = config.toMap();

        ((HashMap<String, Object>)((HashMap<String, Object>)map.get("monitor")).get("chart")).put("start", chartStart);

        GeneralConfigurationManager.getInstance().setConfigurationFromMap(map);

        this.read();
    }

    public void setChartEnd(Long chartEnd) {
        this.chartEnd = chartEnd;

        Toml config = GeneralConfigurationManager.getInstance().config();

        Map<String, Object> map = config.toMap();
        ((HashMap<String, Object>)((HashMap<String, Object>)map.get("monitor")).get("chart")).put("end", chartEnd);

        GeneralConfigurationManager.getInstance().setConfigurationFromMap(map);

        this.read();
    }
}
