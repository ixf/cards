package configuration;

import org.junit.Before;
import org.junit.Test;
import pl.edu.agh.io.umniedziala.configuration.Configuration;

import static org.junit.Assert.assertNotNull;

public class ConfigurationTests {

    Configuration config;

    @Test
    @Before
    public void instanceTest() {
        config = Configuration.getInstance();
    }

    @Test
    public void valueGettingTest() {
        assertNotNull(config.getRefreshInterval());
    }
}
