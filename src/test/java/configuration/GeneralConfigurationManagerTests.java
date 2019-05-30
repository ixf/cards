package configuration;

import org.junit.Test;
import pl.edu.agh.io.umniedziala.configuration.GeneralConfigurationManager;

import static org.junit.Assert.assertNotNull;

public class GeneralConfigurationManagerTests {

    @Test
    public void keyAndValuesTest() {
        assertNotNull(GeneralConfigurationManager.getInstance().config().getString("meta.config_version"));
    }
}
