package configuration;

import org.junit.Before;
import org.junit.Test;
import pl.edu.agh.io.umniedziala.configuration.Configuration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class ConfigurationTests {

    private Configuration config;

    @Test
    @Before
    public void instanceTest() {
        config = Configuration.getInstance();
    }

    @Test
    public void valueGettingTest() {
        assertNotNull(config.getCheckInterval());
    }

    @Test
    public void valueSettingTest() {
        config.setChartEnd(123L);

        Long end = config.getChartEnd();

        assertEquals(end, Long.valueOf(123L));

        config.resetToDefaults();

        end = config.getChartEnd();

        assertEquals(Long.valueOf(23L), end);
    }
}
